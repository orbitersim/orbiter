

// ===================================================
// Copyright (C) 2021-2026 Jarmo Nikkanen
// licensed under LGPL v2
// ===================================================

#include <string.h>
#include "IProcess.h"
#include "D3D9Util.h"
#include "D3D9Surface.h"
#include <sstream>
#include <fstream> // std::ifstream (MSVC had it through <sstream>)


// ================================================================================================
//
ImageProcessing::ImageProcessing(VkDev *pDev, const char *_file, const char *_psentry, const char *_ppf, const char *_vsentry)
	: pDevice(pDev)
	, pVSConst(NULL)
	, pPSConst(NULL)
	, pCB(NULL)
	, pDepth(NULL)
	, pDepthBak(NULL)
	, pMesh(NULL)
	, hPos(NULL)
	, hSiz(NULL)
	, hVP(NULL)
	, mesh_cull(gcIPInterface::ipicull::None)
	, desc()
	, iVP()
	, mesh_tex_idx(-1)
{
	for (int i=0;i<(int)std::size(pTextures);i++) pTextures[i].hTex = NULL;
	for (int i=0;i<4;i++) pRtg[i] = pRtgBak[i] = NULL;

	if (_vsentry) pVertex = CompileVertexShader(pDevice, _file, _vsentry, "IPIVS", NULL, &pVSConst);
	else pVertex = CompileVertexShader(pDevice, "Modules/VulkanClient/IPI.glsl", "VSMain", "IPIVS", NULL, &pVSConst);

	pPixel   = CompilePixelShader(pDevice, _file, _psentry, "IPIPS", _ppf, &pPSConst);
	pOcta	 = new SMVERTEX[10];

	pCB = new VkConstBuffer(pDevice); // not upstream: holds the blocks of both tables (constant registers in D3D9)
	pCB->SetTable(pVSConst);
	pCB->SetTable(pPSConst);

	Shaders[string(_psentry)].pPixel = pPixel;
	Shaders[string(_psentry)].pPSConst = pPSConst;

	if (pVSConst) {
		hVP = pVSConst->GetConstantByName("mVP");
		hPos = pVSConst->GetConstantByName("vPos");
		hSiz = pVSConst->GetConstantByName("vTgtSize");
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

	snprintf(file, 256, "%s", _file);
	snprintf(entry, 32, "%s", _psentry);
	if (_ppf) snprintf(ppf, 256, "%s", _ppf);
	else snprintf(ppf, 32, "%s", "");

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
	if (pDevice->GetConstantSource() == pCB) pDevice->SetConstantSource(NULL, NULL); // not upstream: the device must not push a deleted buffer
	VkDevice d = pDevice->dev;
	SAFE_DELETE(pVSConst);
	VkShaderEXT vs = pVertex;
	pDevice->Defer([d, vs]() { if (vs) vkx.DestroyShaderEXT(d, vs, NULL); }); // SAFE_RELEASE(pVertex)
	pVertex = VK_NULL_HANDLE;
	SAFE_DELETEA(pOcta);

	for (auto x : Shaders) {
		VkShaderEXT ps = x.second.pPixel;
		pDevice->Defer([d, ps]() { if (ps) vkx.DestroyShaderEXT(d, ps, NULL); }); // SAFE_RELEASE(x.second.pPixel)
		SAFE_DELETE(x.second.pPSConst);
	}
	Shaders.clear();
	SAFE_DELETE(pCB);
}


// ================================================================================================
//
bool ImageProcessing::CompileShader(const char *Entry)
{
	string name(Entry);
	VkConstTable *pPSC = NULL;
	Shaders[name].pPixel = CompilePixelShader(pDevice, file, Entry, "IPIPS2", ppf, &pPSC);
	Shaders[name].pPSConst = pPSC;
	if (pPSC) pCB->SetTable(pPSC); // not upstream: the constant buffer takes the new shader's blocks
	return ((Shaders[name].pPixel != VK_NULL_HANDLE) && (Shaders[name].pPSConst != NULL));
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
	snprintf(entry, 31, "%s", Entry);
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
	if (pRtg[0]) { desc.Width = pRtg[0]->w; desc.Height = pRtg[0]->h; } // GetDesc
	else {
		LogErr("ImageProcessing(%s): No render target is set", _PTR(this));
		return false;
	}

	struct { UINT Width, Height; } ds; // D3DSURFACE_DESC

	// Check that all additional render targets have the same size
	//
	for (int i=1;i<4;i++) {
		if (pRtg[i]) {
			ds.Width = pRtg[i]->w; ds.Height = pRtg[i]->h; // GetDesc
			if ((ds.Height!=desc.Height) || (ds.Width!=desc.Width)) {
				LogErr("ImageProcessing(%s): All render targets must be same the size", _PTR(this));
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

	pDevice->SetViewport((float)iVP.X, (float)iVP.Y, (float)iVP.Width, (float)iVP.Height, iVP.MinZ, iVP.MaxZ);
	pCB->SetValue(hVP, &mVP, sizeof(D3DXMATRIX)); // pVSConst->SetMatrix
	pCB->SetValue(hSiz, ptr(D3DXVECTOR4(float(desc.Width), float(desc.Height), 1.0f/float(desc.Width), 1.0f/float(desc.Height))), sizeof(D3DXVECTOR4)); // SetVector
	pCB->SetValue(hPos, &vTemplate, sizeof(D3DXVECTOR4)); // SetVector
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
void ImageProcessing::SetMesh(const MESHHANDLE hMesh, const char *tex, gcIPInterface::ipicull cull)
{
	pMesh = GetSketchMesh(hMesh);

	mesh_cull = cull;

	if (tex) {
		VkConstHandle hVar = pPSConst->GetConstantByName(tex);
		if (!hVar) {
			LogErr("IPInterface::SetSketchMesh() Invalid variable name [%s]", tex);
			return;
		}
		mesh_tex_idx = hVar->binding - VkDev::NUBOS; // GetSamplerIndex
	}
	else mesh_tex_idx = -1;
}


// ================================================================================================
//
bool ImageProcessing::Execute(bool bInScene)
{
	return Execute(0, bInScene, gcIPInterface::ipitemplate::Rect);
}


// ================================================================================================
//
bool ImageProcessing::Execute(const char *shader, bool bInScene, DWORD blendop)
{
	Activate(shader);
	return Execute(blendop, bInScene, gcIPInterface::ipitemplate::Rect);
}


// ================================================================================================
//
bool ImageProcessing::Execute(DWORD blendop, bool bInScene, gcIPInterface::ipitemplate mode, int grp)
{
	if (!IsOK()) return false;
	if (!SetupViewPort()) return false;

	// Set device state -------------------------------------------------------
	//
	pDevice->BindShaders(pVertex, pPixel); // SetVertexShader, SetPixelShader
	smpSlots.clear(); // not upstream: the sampler bindings the pair reads
	for (VkConstTable *t : { pPSConst, pVSConst })
		for (auto &e : t->entries) {
			if (!e.sampler) continue;
			bool have = false;
			for (auto &s : smpSlots) if (s.binding == e.binding) have = true;
			if (!have) smpSlots.push_back({ e.binding, e.view });
		}
	pDevice->SetConstantSource(pCB, &smpSlots);
	pDevice->SetVertexDecl(pPosTexDecl);

	VkPolygonMode BakFill = pDevice->GetState().fill; // GetRenderState(D3DRS_FILLMODE)

	pDevice->SetFillMode(VK_POLYGON_MODE_FILL);
	pDevice->SetCullMode(VK_CULL_MODE_NONE);
	pDevice->SetBlend((blendop!=0));
	// D3DRS_ALPHATESTENABLE false: alpha tests are shader discards
	VkDev::State st = pDevice->GetState(); st.stencil = false; pDevice->SetState(st); // D3DRS_STENCILENABLE false
	pDevice->SetColorWrite(0xF);

	if (blendop == 1) {
		pDevice->SetBlendFunc(VK_BLEND_FACTOR_SRC_ALPHA, VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA, VK_BLEND_OP_ADD); // D3DRS_BLENDOP, SRCBLEND, DESTBLEND
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
		pRtgBak[i] = pDevice->GetRenderTargetN(i); // GetRenderTarget(i)
		pDevice->SetRenderTargetN(i, pRtg[i]);
		// D3DRS_COLORWRITEENABLE1-3 = 0xF for the extra targets: SetColorWrite(0xF) above covers every bound target
	}

	// Set Depth-Stencil surface ----------------------------------------------
	//
	pDepthBak = pDevice->GetDepthStencil(); // GetDepthStencilSurface (not upstream: saved in both cases)
	if (pDepth) {	
		pDevice->SetDepthTest(true);
		pDevice->SetDepthWrite(true);

		pDevice->SetRenderTarget(pRtg[0], pDepth); // SetDepthStencilSurface
	}
	else {
		pDevice->SetDepthTest(false);
		pDevice->SetDepthWrite(false);
		pDevice->SetRenderTarget(pRtg[0], NULL); // not upstream: no depth attachment while Z is off (Vulkan needs it as large as the target)
	}

	// Set textures and samplers -----------------------------------------------
	//
	VkSamplerDesc smp[std::size(pTextures)]; // not upstream: sampler state per stage, D3D9's default for stages not set here
	for (auto &s : smp) s = { VK_FILTER_NEAREST, VK_FILTER_NEAREST, VK_SAMPLER_MIPMAP_MODE_NEAREST, VK_SAMPLER_ADDRESS_MODE_REPEAT, VK_SAMPLER_ADDRESS_MODE_REPEAT, VK_SAMPLER_ADDRESS_MODE_REPEAT, 0.0f, 0.0f, true };

	for (int idx=0;idx<(int)std::size(pTextures);idx++) {

		if (pTextures[idx].hTex==NULL) continue;

		DWORD flags = pTextures[idx].flags;
		VkSamplerDesc &sd = smp[idx]; // SetSamplerState(idx, ...)

		if (flags&IPF_CLAMP_U)			sd.u = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
		else if (flags&IPF_MIRROR_U)	sd.u = VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT;
		else							sd.u = VK_SAMPLER_ADDRESS_MODE_REPEAT;

		if (flags&IPF_CLAMP_V)			sd.v = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
		else if (flags&IPF_MIRROR_V)	sd.v = VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT;
		else							sd.v = VK_SAMPLER_ADDRESS_MODE_REPEAT;

		if (flags&IPF_CLAMP_W)			sd.w = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
		else if (flags&IPF_MIRROR_W)	sd.w = VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT;
		else							sd.w = VK_SAMPLER_ADDRESS_MODE_REPEAT;

		VkFilter filter = VK_FILTER_NEAREST;

		if (flags&IPF_LINEAR) filter = VK_FILTER_LINEAR;
		if (flags&IPF_PYRAMIDAL) filter = VK_FILTER_LINEAR; // D3DTEXF_PYRAMIDALQUAD: no Vulkan filter, linear
		if (flags&IPF_GAUSSIAN) filter = VK_FILTER_LINEAR; // D3DTEXF_GAUSSIANQUAD: no Vulkan filter, linear

		sd.mag = filter;
		sd.min = filter;
		sd.noMip = true; // D3DSAMP_MIPFILTER D3DTEXF_NONE

		pCB->SetTexture(idx + VkDev::NUBOS, pTextures[idx].hTex, sd); // SetTexture(idx, ...)
	}

	// Execute ----------------------------------------------------------------
	//
	// BeginScene: nothing, the device's frame is always open (rendering starts at the first draw)


	if (mode == gcIPInterface::ipitemplate::Rect)
	{
		pDevice->DrawIndexedPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 4, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 2), &cIndex, VK_INDEX_TYPE_UINT16, &Vertex, sizeof(SMVERTEX));
	}


	if (mode == gcIPInterface::ipitemplate::Octagon)
	{
		pDevice->DrawPrimitiveUP(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN, VkPrimVerts(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN, 8), pOcta, sizeof(SMVERTEX));
	}


	if (mode == gcIPInterface::ipitemplate::Mesh)
	{

		pDevice->SetCullMode(VK_CULL_MODE_BACK_BIT);

		pMesh->Init();

		DWORD nGrp = pMesh->GroupCount();
		if (grp < 0) for (DWORD i=0;i<nGrp;i++) {
			if (mesh_tex_idx >= 0) {
				SURFHANDLE hTex = pMesh->GetTexture(i);
				pCB->SetTexture(mesh_tex_idx + VkDev::NUBOS, SURFACE(hTex)->GetTexture(), smp[mesh_tex_idx]); // SetTexture(mesh_tex_idx, ...)
			}
			pMesh->RenderGroup(i);
		}
		else {
			if (mesh_tex_idx >= 0) {
				SURFHANDLE hTex = pMesh->GetTexture(grp);
				pCB->SetTexture(mesh_tex_idx + VkDev::NUBOS, SURFACE(hTex)->GetTexture(), smp[mesh_tex_idx]); // SetTexture(mesh_tex_idx, ...)
			}
			pMesh->RenderGroup(grp);
		}
	}

	if (!bInScene) pDevice->EndRendering(); // EndScene

	// Disconnect render targets ----------------------------------------------
	//
	pDevice->SetRenderTarget(pRtg[0], pDepthBak); // SetDepthStencilSurface (not upstream: in both cases)

	// Disconnect render targets ----------------------------------------------
	//
	for (int i=0;i<4;i++) {
		pDevice->SetRenderTargetN(i, pRtgBak[i]);
		pRtgBak[i] = NULL; // SAFE_RELEASE: GetRenderTargetN doesn't add a reference
	}

	// Disconnect textures -----------------------------------------------------
	//
	pCB->ClearTextures(); // SetTexture(idx, NULL) of the stages set above

	pDevice->SetFillMode(BakFill);

	return true;
}


// ================================================================================================
//
void ImageProcessing::SetFloat(const char *var, const void *val, int bytes)
{
	VkConstHandle hVar = pPSConst->GetConstantByName(var);

	if (!hVar) {
		LogErr("IPInterface::SetFloat() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
		return;
	}

	pCB->SetValue(hVar, val, bytes); // SetFloatArray (bytes>>2 floats); its "Failed" log left out: SetValue has no error
}


// ================================================================================================
//
void ImageProcessing::SetInt(const char *var, const int *val, int bytes)
{
	VkConstHandle hVar = pPSConst->GetConstantByName(var);

	if (!hVar) {
		LogErr("IPInterface::SetInt() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
		return;
	}

	pCB->SetValue(hVar, val, bytes); // SetIntArray (bytes>>2 ints); its "Failed" log left out: SetValue has no error
}


// ================================================================================================
//
void ImageProcessing::SetBool(const char *var, const bool *val, int bytes)
{
	VkConstHandle hVar = pPSConst->GetConstantByName(var);

	if (!hVar) {
		LogErr("IPInterface::SetBool() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
		return;
	}

	int *data = new int[bytes];
	for (int i=0;i<bytes;i++) data[i] = val[i];

	pCB->SetValue(hVar, data, bytes * sizeof(int)); // SetBoolArray (32-bit BOOLs); its "Failed" log left out: SetValue has no error

	delete []data;
	data = NULL;
}


// ================================================================================================
//
void ImageProcessing::SetStruct(const char *var, const void *val, int bytes)
{
	VkConstHandle hVar = pPSConst->GetConstantByName(var);

	if (!hVar) {
		LogErr("IPInterface::SetStruct() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
		return;
	}

	pCB->SetValue(hVar, val, bytes); // its "Failed" log left out: SetValue has no error
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
void ImageProcessing::SetVSFloat(const char *var, const void *val, int bytes)
{
	VkConstHandle hVar = pVSConst->GetConstantByName(var);

	pCB->SetValue(hVar, val, bytes); // (upstream went through pPSConst here; the handle's block is what counts)
}


// ================================================================================================
//
void ImageProcessing::SetVSInt(const char *var, const int *val, int bytes)
{
	VkConstHandle hVar = pVSConst->GetConstantByName(var);

	pCB->SetValue(hVar, val, bytes); // (upstream went through pPSConst here; the handle's block is what counts)
}


// ================================================================================================
//
void ImageProcessing::SetVSBool(const char *var, const bool *val, int bytes)
{
	VkConstHandle hVar = pVSConst->GetConstantByName(var);

	if (!hVar) return;
	int *data = new int[bytes];
	for (int i = 0; i<bytes; i++) data[i] = val[i];

	pCB->SetValue(hVar, data, bytes * sizeof(int)); // (upstream went through pPSConst here; the handle's block is what counts)

	delete[]data;
}


// ================================================================================================
//
void ImageProcessing::SetVSStruct(const char *var, const void *val, int bytes)
{
	VkConstHandle hVar = pVSConst->GetConstantByName(var);
	if (!hVar) return;
	pCB->SetValue(hVar, val, bytes); // (upstream went through pPSConst here; the handle's block is what counts)
}


// ================================================================================================
//
void ImageProcessing::SetVSFloat(const char *var, float val)
{
	SetVSFloat(var, (const float*)&val, sizeof(float));
}


// ================================================================================================
//
void ImageProcessing::SetVSInt(const char *var, int val)
{
	SetVSInt(var, (const int*)&val, sizeof(int));
}


// ================================================================================================
//
void ImageProcessing::SetVSBool(const char *var, bool val)
{
	SetVSBool(var, (const bool*)&val, sizeof(bool));
}


// ================================================================================================
//
void ImageProcessing::SetTexture(const char *var, SURFHANDLE hTex, DWORD flags)
{
	VkConstHandle hVar = pPSConst->GetConstantByName(var);

	if (!hVar) {
		LogErr("IPInterface::SetTexture() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
		return;
	}

	DWORD idx = hVar->binding - VkDev::NUBOS; // GetSamplerIndex

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
void ImageProcessing::SetTextureNative(const char *var, VkTex *hTex, DWORD flags)
{
	VkConstHandle hVar = pPSConst->GetConstantByName(var);

	if (!hVar) {
		LogErr("IPInterface::SetTextureNative() Invalid variable name [%s]. File[%s], Entrypoint[%s]", var, file, entry);
		return;
	}

	DWORD idx = hVar->binding - VkDev::NUBOS; // GetSamplerIndex

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
void ImageProcessing::SetDepthStencil(VkSurf *hSrf)
{
	pDepth = hSrf;
}


// ================================================================================================
//
void ImageProcessing::SetOutputNative(int id, VkSurf *hSrf)
{
	if (id<0) id=0;
	if (id>3) id=3;
	pRtg[id] = hSrf;
}


// ================================================================================================
//
bool ImageProcessing::IsOK()
{
	for (auto x : Shaders) {
		if (x.second.pPixel == VK_NULL_HANDLE) return false;
		if (x.second.pPSConst == NULL) return false;
	}
	return (pVertex && pVSConst && pDevice && hVP && hPos && hSiz);
}









// ================================================================================================
// PUBLIC INTERFACE
// ================================================================================================
//

gcIPInterface::~gcIPInterface()
{

}

bool gcIPInterface::CompileShader(const char *Entry)
{
	return pIPI->CompileShader(Entry);
}

bool gcIPInterface::Activate(const char *Shader)
{
	return pIPI->Activate(Shader);
}
	
void gcIPInterface::SetFloat(const char *var, float val)
{
	pIPI->SetFloat(var, val);
}

void gcIPInterface::SetInt(const char *var, int val)
{
	pIPI->SetInt(var, val);
}

void gcIPInterface::SetBool(const char *var, bool val)
{
	pIPI->SetBool(var, val);
}

void gcIPInterface::SetFloat(const char *var, const void *val, int bytes)
{
	pIPI->SetFloat(var, val, bytes);
}

void gcIPInterface::SetInt(const char *var, const int *val, int bytes)
{
	pIPI->SetInt(var, val, bytes);
}

void gcIPInterface::SetBool(const char *var, const bool *val, int bytes)
{
	pIPI->SetBool(var, val, bytes);
}

void gcIPInterface::SetStruct(const char *var, const void *val, int bytes)
{
	pIPI->SetStruct(var, val, bytes);
}

void gcIPInterface::SetVSFloat(const char *var, float val)
{
	pIPI->SetVSFloat(var, val);
}

void gcIPInterface::SetVSInt(const char *var, int val)
{
	pIPI->SetVSInt(var, val);
}

void gcIPInterface::SetVSBool(const char *var, bool val)
{
	pIPI->SetVSBool(var, val);
}

void gcIPInterface::SetVSFloat(const char *var, const void *val, int bytes)
{
	pIPI->SetVSFloat(var, val, bytes);
}

void gcIPInterface::SetVSInt(const char *var, const int *val, int bytes)
{
	pIPI->SetVSInt(var, val, bytes);
}

void gcIPInterface::SetVSBool(const char *var, const bool *val, int bytes)
{
	pIPI->SetVSBool(var, val, bytes);
}

void gcIPInterface::SetVSStruct(const char *var, const void *val, int bytes)
{
	pIPI->SetVSStruct(var, val, bytes);
}

void gcIPInterface::SetTexture(const char *var, SURFHANDLE hTex, DWORD flags)
{
	pIPI->SetTexture(var, hTex, flags);
}

void gcIPInterface::SetOutput(int id, SURFHANDLE hSrf)
{
	pIPI->SetOutput(id, hSrf);
}
	
bool gcIPInterface::IsOK()
{
	return pIPI->IsOK();
}

void gcIPInterface::SetOutputRegion(float w, float h, float x, float y)
{
	pIPI->SetTemplate(w, h, x, y);
}

void gcIPInterface::SetMesh(MESHHANDLE hMesh, const char *tex, ipicull cull)
{
	pIPI->SetMesh(hMesh, tex, cull);
}

bool gcIPInterface::Execute(bool bInScene)
{
	return pIPI->Execute(bInScene);
}

bool gcIPInterface::Execute(DWORD blendop, bool bInScene, ipitemplate mde)
{
	return pIPI->Execute(blendop, bInScene, mde);
}

int gcIPInterface::FindDefine(const char *key)
{
	return pIPI->FindDefine(key);
}
