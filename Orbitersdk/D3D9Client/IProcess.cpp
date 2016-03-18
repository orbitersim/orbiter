
// ================================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under LGPL v3
// Copyright (C) 2016 Jarmo Nikkanen
// ================================================================================================


#include "IProcess.h"
#include "D3D9Util.h"
#include "D3D9Surface.h"


// ================================================================================================
//
ImageProcessing::ImageProcessing(LPDIRECT3DDEVICE9 pDev, const char *_file, const char *_entry, const char *ppf)
{
	pVSConst = NULL;
	pPSConst = NULL;
	pDevice  = pDev;
	
	hVP = NULL;
	for (int i=0;i<16;i++) pTextures[i].hTex = NULL;
	for (int i=0;i<4;i++) pRtg[i] = pRtgBak[i] = NULL;

	pVertex  = CompileVertexShader(pDevice, "Modules/D3D9Client/IPI.hlsl", "VSMain", NULL, &pVSConst);
	pPixel   = CompilePixelShader(pDevice, _file, _entry, ppf, &pPSConst);

	if (pVSConst) hVP = pVSConst->GetConstantByName(NULL, "mVP");
	
	if (!hVP) LogErr("Failed to get ImageProcessing::hVP handle");

	strcpy_s(file, 256, _file);
	strcpy_s(entry, 64, _entry);
}


// ================================================================================================
//
ImageProcessing::~ImageProcessing()
{
	SAFE_RELEASE(pVSConst);
	SAFE_RELEASE(pPSConst);
	SAFE_RELEASE(pVertex);
	SAFE_RELEASE(pPixel);
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

	return true;
}


// ================================================================================================
//
bool ImageProcessing::Execute()
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
	HR(pDevice->SetRenderState(D3DRS_ALPHABLENDENABLE, false));
	HR(pDevice->SetRenderState(D3DRS_ALPHATESTENABLE, false));
	HR(pDevice->SetRenderState(D3DRS_STENCILENABLE, false));


	// Define vertices --------------------------------------------------------
	//
	SMVERTEX Vertex[4] = {
		{float(0),  float(0), 0, 0, 0},
		{float(0),  float(desc.Height), 0, 0, 1},
		{float(desc.Width), float(desc.Height), 0, 1, 1},
		{float(desc.Width), float(0), 0, 1, 0}
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
	HR(pDevice->BeginScene());
	HR(pDevice->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 4, 2, &cIndex, D3DFMT_INDEX16, &Vertex, sizeof(SMVERTEX)));	
	HR(pDevice->EndScene());

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
		LogErr("IPInterface::SetFloat() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
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
		LogErr("IPInterface::SetInt() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
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
		LogErr("IPInterface::SetBool() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
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
		LogErr("IPInterface::SetStruct() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
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
		LogErr("IPInterface::SetTexture() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
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
void ImageProcessing::SetTextureNative(const char *var, LPDIRECT3DTEXTURE9 hTex, DWORD flags)
{
	D3DXHANDLE hVar = pPSConst->GetConstantByName(NULL, var);

	if (!hVar) {
		LogErr("IPInterface::SetTextureNative() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
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
	return (pPixel && pVertex && pPSConst && pVSConst && pDevice && hVP);
}
