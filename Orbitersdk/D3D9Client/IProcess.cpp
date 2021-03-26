
// =================================================================================================================================
//
// Copyright (C) 2016-2018 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation
// files (the "Software"), to use, copy, modify, merge, publish, distribute, interact with the Software and sublicense copies
// of the Software, subject to the following conditions:
//
// a) You do not sell, rent or auction the Software.
// b) You do not collect distribution fees.
// c) If the Software is distributed in an object code form, it must inform that the source code is available and how to obtain it.
// d) You do not remove or alter any copyright notices contained within the Software.
// e) This copyright notice must be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================


#include "IProcess.h"
#include "D3D9Util.h"
#include "D3D9Surface.h"
#include <sstream>


// ================================================================================================
//
ImageProcessing::ImageProcessing(LPDIRECT3DDEVICE9 pDev, const char *_file, const char *_entry, const char *_ppf)
	: pDevice(pDev)
	, pVSConst(NULL)
	, pPSConst(NULL)
	, hVP(NULL)
	, desc()
	, iVP()
{
	for (int i=0;i<16;i++) pTextures[i].hTex = NULL;
	for (int i=0;i<4;i++) pRtg[i] = pRtgBak[i] = NULL;

	pVertex  = CompileVertexShader(pDevice, "Modules/D3D9Client/IPI.hlsl", "VSMain", NULL, &pVSConst);
	pPixel   = CompilePixelShader(pDevice, _file, _entry, _ppf, &pPSConst);
	pOcta	 = new SMVERTEX[10];

	Shaders[string(_entry)].pPixel = pPixel;
	Shaders[string(_entry)].pPSConst = pPSConst;

	if (pVSConst) {
		hVP = pVSConst->GetConstantByName(NULL, "mVP");
		hPos = pVSConst->GetConstantByName(NULL, "vPos");
		hSiz = pVSConst->GetConstantByName(NULL, "vTgtSize");
		SetTemplate();
	}

	if (!hVP || !hPos) LogErr("Failed to get ImageProcessing::hVP handle");

	double w = 22.5 * RAD;
	double q = w;
	double r = 1.0 / cos(w);
	
	pOcta[0].x = 0.0f;
	pOcta[0].y = 0.0f;
	pOcta[0].z = 0.0f;
	pOcta[0].tu = 0.0f;
	pOcta[0].tv = 0.0f;
	
	for (int i = 1; i < 10; i++) {
		pOcta[i].x = float(cos(q) * r);
		pOcta[i].y = float(sin(q) * r);
		pOcta[i].z = 0.0f;
		pOcta[i].tu = pOcta[i].x;
		pOcta[i].tv = pOcta[i].y;
		q += w*2.0;
	}

	strcpy_s(file, 256, _file);
	strcpy_s(entry, 32, _entry);
	if (_ppf) strcpy_s(ppf, 256, _ppf);
	else strcpy_s(ppf, 32, "");

	// Create a database of defines ----------------------------------------------------------------
	std::string line;
	std::ifstream fs(_file);
	while (std::getline(fs, line)) {
		if (!line.length() || line.find("//") == 0) continue;
		if (line.find("#define") == 0) def.push_front(line.substr(line.find("#define") + 8));
	}
	fs.close();
}


// ================================================================================================
//
ImageProcessing::~ImageProcessing()
{
	SAFE_RELEASE(pVSConst);
	SAFE_RELEASE(pVertex);
	SAFE_DELETEA(pOcta);

	for (auto x : Shaders) {
		SAFE_RELEASE(x.second.pPixel);
		SAFE_RELEASE(x.second.pPSConst);
	}
	Shaders.clear();
}


// ================================================================================================
//
bool ImageProcessing::CompileShader(const char *Entry)
{
	string name(Entry);
	LPD3DXCONSTANTTABLE pPSC = NULL;
	Shaders[name].pPixel = CompilePixelShader(pDevice, file, Entry, ppf, &pPSC);
	Shaders[name].pPSConst = pPSC;
	return ((Shaders[name].pPixel != NULL) && (Shaders[name].pPSConst != NULL));
}


// ================================================================================================
//
bool ImageProcessing::Activate(const char *Entry)
{
	SetTemplate();
	if (!Entry) return Activate(entry);
	string name(Entry);
	if (Shaders.count(name) == 0) {
		LogErr("ImageProcessing::Activate() FAILED Entry=%s", Entry);
		return false;
	}
	pPixel = Shaders[name].pPixel;
	pPSConst = Shaders[name].pPSConst;
	return true;
}


// ================================================================================================
//
int ImageProcessing::FindDefine(const char *_key)
{
	int retval;
	std::string key;
	auto it = def.begin();
	while (it!=def.end()) {
		std::istringstream iss(*it);
		iss >> key >> retval;
		if (key.compare(_key) == 0) return retval;
		it++;
	}
	return 0;
}

// ================================================================================================
//
bool ImageProcessing::SetupViewPort()
{

	// Check that the first render target is valid
	//
	if (pRtg[0]) pRtg[0]->GetDesc(&desc);
	else {
		LogErr("ImageProcessing(0x%X): No render target is set", this);
		return false;
	}

	D3DSURFACE_DESC ds;

	// Check that all additional render targets have the same size
	//
	for (int i=1;i<4;i++) {
		if (pRtg[i]) {
			pRtg[i]->GetDesc(&ds);
			if ((ds.Height!=desc.Height) || (ds.Width!=desc.Width)) {
				LogErr("ImageProcessing(0x%X): All render targets must be same the size", this);
				return false;
			}
		}
		else break;
	}

	// Setup view-projection matrix and viewport
	//
	D3DXMatrixOrthoOffCenterLH(&mVP, 0.0f, (float)desc.Width, (float)desc.Height, 0.0f, 0.0f, 1.0f);

	iVP.X = 0;
	iVP.Y = 0;
	iVP.Width  = desc.Width;
	iVP.Height = desc.Height;
	iVP.MinZ = 0.0f;
	iVP.MaxZ = 1.0f;

	HR(pDevice->SetViewport(&iVP));
	HR(pVSConst->SetMatrix(pDevice, hVP, &mVP));
	HR(pVSConst->SetVector(pDevice, hSiz, &D3DXVECTOR4(float(desc.Width), float(desc.Height), 1.0f/float(desc.Width), 1.0f/float(desc.Height))));
	HR(pVSConst->SetVector(pDevice, hPos, &vTemplate));
	return true;
}


// ================================================================================================
//
void ImageProcessing::SetTemplate(float w, float h, float x, float y)
{
	vTemplate = D3DXVECTOR4(w, h, x, y);
}


// ================================================================================================
//
bool ImageProcessing::Execute(bool bInScene)
{
	return Execute(0, 0, 0, bInScene, Rect);
}


// ================================================================================================
//
bool ImageProcessing::ExecuteTemplate(bool bInScene, ipitemplate mode)
{
	return Execute(0, 0, 0, bInScene, mode);
}


// ================================================================================================
//
bool ImageProcessing::Execute(DWORD blendop, DWORD src, DWORD dest, bool bInScene, ipitemplate mode)
{
	if (!IsOK()) return false;
	if (!SetupViewPort()) return false;

	// Set device state -------------------------------------------------------
	//
	HR(pDevice->SetVertexShader(pVertex));
	HR(pDevice->SetPixelShader(pPixel));
	HR(pDevice->SetVertexDeclaration(pPosTexDecl));

	HR(pDevice->SetRenderState(D3DRS_ZENABLE, false));
	HR(pDevice->SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID));
	HR(pDevice->SetRenderState(D3DRS_ZWRITEENABLE, false));
	HR(pDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE));
	HR(pDevice->SetRenderState(D3DRS_ALPHABLENDENABLE, (blendop!=0)));
	HR(pDevice->SetRenderState(D3DRS_ALPHATESTENABLE, false));
	HR(pDevice->SetRenderState(D3DRS_STENCILENABLE, false));

	if (blendop) {
		HR(pDevice->SetRenderState(D3DRS_BLENDOP, blendop));
		HR(pDevice->SetRenderState(D3DRS_SRCBLEND, src));
		HR(pDevice->SetRenderState(D3DRS_DESTBLEND, dest));
	}

	// Define vertices --------------------------------------------------------
	//
	SMVERTEX Vertex[4] = {
		{0, 0, 0, 0, 0},
		{0, 1, 0, 0, 1},
		{1, 1, 0, 1, 1},
		{1, 0, 0, 1, 0}
	};

	static WORD cIndex[6] = {0, 2, 1, 0, 3, 2};

	// Set render targets -----------------------------------------------------
	//
	for (int i=0;i<4;i++) {
		pDevice->GetRenderTarget(i, &pRtgBak[i]);
		pDevice->SetRenderTarget(i, pRtg[i]);
	}

	// Set textures and samplers -----------------------------------------------
	//
	for (int idx=0;idx<16;idx++) {

		if (pTextures[idx].hTex==NULL) continue;

		DWORD flags = pTextures[idx].flags;

		if (flags&IPF_CLAMP_U)			pDevice->SetSamplerState(idx, D3DSAMP_ADDRESSU, D3DTADDRESS_CLAMP);
		else if (flags&IPF_MIRROR_U)	pDevice->SetSamplerState(idx, D3DSAMP_ADDRESSU, D3DTADDRESS_MIRROR);
		else							pDevice->SetSamplerState(idx, D3DSAMP_ADDRESSU, D3DTADDRESS_WRAP);

		if (flags&IPF_CLAMP_V)			pDevice->SetSamplerState(idx, D3DSAMP_ADDRESSV, D3DTADDRESS_CLAMP);
		else if (flags&IPF_MIRROR_V)	pDevice->SetSamplerState(idx, D3DSAMP_ADDRESSV, D3DTADDRESS_MIRROR);
		else							pDevice->SetSamplerState(idx, D3DSAMP_ADDRESSV, D3DTADDRESS_WRAP);

		if (flags&IPF_CLAMP_W)			pDevice->SetSamplerState(idx, D3DSAMP_ADDRESSW, D3DTADDRESS_CLAMP);
		else if (flags&IPF_MIRROR_W)	pDevice->SetSamplerState(idx, D3DSAMP_ADDRESSW, D3DTADDRESS_MIRROR);
		else							pDevice->SetSamplerState(idx, D3DSAMP_ADDRESSW, D3DTADDRESS_WRAP);

		DWORD filter = D3DTEXF_POINT;

		if (flags&IPF_LINEAR) filter = D3DTEXF_LINEAR;
		if (flags&IPF_PYRAMIDAL) filter = D3DTEXF_PYRAMIDALQUAD;
		if (flags&IPF_GAUSSIAN) filter = D3DTEXF_GAUSSIANQUAD;

		HR(pDevice->SetSamplerState(idx, D3DSAMP_MAGFILTER, filter));
		HR(pDevice->SetSamplerState(idx, D3DSAMP_MINFILTER, filter));
		HR(pDevice->SetSamplerState(idx, D3DSAMP_MIPFILTER, D3DTEXF_NONE));

		HR(pDevice->SetTexture(idx, pTextures[idx].hTex));
	}

	// Execute ----------------------------------------------------------------
	//
	if (!bInScene) HR(pDevice->BeginScene());

	if (mode == ImageProcessing::Rect) {
		HR(pDevice->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 4, 2, &cIndex, D3DFMT_INDEX16, &Vertex, sizeof(SMVERTEX)));
	}

	if (mode == ImageProcessing::Octagon) {
		HR(pDevice->DrawPrimitiveUP(D3DPT_TRIANGLEFAN, 8, pOcta, sizeof(SMVERTEX)));
	}

	if (!bInScene) HR(pDevice->EndScene());

	// Disconnect render targets ----------------------------------------------
	//
	for (int i=0;i<4;i++) {
		HR(pDevice->SetRenderTarget(i, pRtgBak[i]));
		SAFE_RELEASE(pRtgBak[i]);
	}

	// Disconnect textures -----------------------------------------------------
	//
	for (int idx=0;idx<16;idx++) {
		if (pTextures[idx].hTex==NULL) continue;
		HR(pDevice->SetTexture(idx, NULL));
	}

	return true;
}


// ================================================================================================
//
void ImageProcessing::SetFloat(const char *var, const void *val, int bytes)
{
	D3DXHANDLE hVar = pPSConst->GetConstantByName(NULL, var);

	if (!hVar) {
		//LogErr("IPInterface::SetFloat() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
		return;
	}

	if (pPSConst->SetFloatArray(pDevice, hVar, (const FLOAT *)val, bytes>>2)!=S_OK) {
		LogErr("IPInterface::SetFloat() Failed. Variable[%s], File[%s], Entrypoint[%s]", var, file, entry);
	}
}


// ================================================================================================
//
void ImageProcessing::SetInt(const char *var, const int *val, int bytes)
{
	D3DXHANDLE hVar = pPSConst->GetConstantByName(NULL, var);

	if (!hVar) {
		//LogErr("IPInterface::SetInt() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
		return;
	}

	if (pPSConst->SetIntArray(pDevice, hVar, val, bytes>>2)!=S_OK) {
		LogErr("IPInterface::SetInt() Failed. Variable[%s], File[%s], Entrypoint[%s]", var, file, entry);
	}
}


// ================================================================================================
//
void ImageProcessing::SetBool(const char *var, const bool *val, int bytes)
{
	D3DXHANDLE hVar = pPSConst->GetConstantByName(NULL, var);

	if (!hVar) {
		//LogErr("IPInterface::SetBool() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
		return;
	}

	int *data = new int[bytes];
	for (int i=0;i<bytes;i++) data[i] = val[i];

	if (pPSConst->SetBoolArray(pDevice, hVar, (const BOOL *)data, bytes)!=S_OK) {
		LogErr("IPInterface::SetBool() Failed. Variable[%s], File[%s], Entrypoint[%s]", var, file, entry);
	}

	delete []data;
}


// ================================================================================================
//
void ImageProcessing::SetStruct(const char *var, const void *val, int bytes)
{
	D3DXHANDLE hVar = pPSConst->GetConstantByName(NULL, var);

	if (!hVar) {
		//LogErr("IPInterface::SetStruct() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
		return;
	}

	if (pPSConst->SetValue(pDevice, hVar, val, bytes)!=S_OK) {
		LogErr("IPInterface::SetStruct() Failed. Variable[%s], File[%s], Entrypoint[%s]", var, file, entry);
	}
}


// ================================================================================================
//
void ImageProcessing::SetFloat(const char *var, float val)
{
	SetFloat(var, (const float*)&val, sizeof(float));
}


// ================================================================================================
//
void ImageProcessing::SetInt(const char *var, int val)
{
	SetInt(var, (const int*)&val, sizeof(int));
}


// ================================================================================================
//
void ImageProcessing::SetBool(const char *var, bool val)
{
	SetBool(var, (const bool*)&val, sizeof(bool));
}


// ================================================================================================
//
void ImageProcessing::SetTexture(const char *var, SURFHANDLE hTex, DWORD flags)
{
	D3DXHANDLE hVar = pPSConst->GetConstantByName(NULL, var);

	if (!hVar) {
		//LogErr("IPInterface::SetTexture() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
		return;
	}

	DWORD idx = pPSConst->GetSamplerIndex(hVar);

	if (!hTex) {
		pTextures[idx].hTex = NULL;
		pTextures[idx].flags = 0;
		return;
	}

	pTextures[idx].hTex = SURFACE(hTex)->GetTexture();
	pTextures[idx].flags = flags;
}


// ================================================================================================
//
void ImageProcessing::SetTextureNative(const char *var, LPDIRECT3DBASETEXTURE9 hTex, DWORD flags)
{
	D3DXHANDLE hVar = pPSConst->GetConstantByName(NULL, var);

	if (!hVar) {
		//LogErr("IPInterface::SetTextureNative() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
		return;
	}

	DWORD idx = pPSConst->GetSamplerIndex(hVar);

	if (!hTex) {
		pTextures[idx].hTex = NULL;
		pTextures[idx].flags = 0;
		return;
	}

	pTextures[idx].hTex = hTex;
	pTextures[idx].flags = flags;
}


// ================================================================================================
//
void ImageProcessing::SetOutput(int id, SURFHANDLE hTex)
{
	if (id<0) id=0;
	if (id>3) id=3;

	if (hTex) pRtg[id] = SURFACE(hTex)->GetSurface();
	else 	  pRtg[id] = NULL;
}


// ================================================================================================
//
void ImageProcessing::SetOutputNative(int id, LPDIRECT3DSURFACE9 hSrf)
{
	if (id<0) id=0;
	if (id>3) id=3;

	if (hSrf) pRtg[id] = hSrf;
	else 	  pRtg[id] = NULL;
}


// ================================================================================================
//
bool ImageProcessing::IsOK()
{
	for (auto x : Shaders) {
		if (x.second.pPixel == NULL) return false;
		if (x.second.pPSConst == NULL) return false;
	}
	return (pVertex && pVSConst && pDevice && hVP && hPos && hSiz);
}
