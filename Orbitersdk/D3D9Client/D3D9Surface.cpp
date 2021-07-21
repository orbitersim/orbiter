// ===========================================================================================
// D3D9Surface.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2011 - 2016 Jarmo Nikkanen
// ===========================================================================================

#define STRICT

#include "D3D9Surface.h"
#include "D3D9Client.h"
#include "D3D9Config.h"
#include "D3D9Catalog.h"
#include "D3D9Util.h"
#include "AABBUtil.h"
#include "Log.h"

using namespace oapi;

ID3DXEffect* D3D9ClientSurface::FX = 0;
D3DXHANDLE   D3D9ClientSurface::eTech = 0;
D3DXHANDLE   D3D9ClientSurface::eFlush = 0;
D3DXHANDLE   D3D9ClientSurface::eSketch = 0;
D3DXHANDLE   D3D9ClientSurface::eRotate = 0;
D3DXHANDLE   D3D9ClientSurface::eVP = 0;
D3DXHANDLE   D3D9ClientSurface::eColor = 0;
D3DXHANDLE   D3D9ClientSurface::eTex0 = 0;
D3DXHANDLE   D3D9ClientSurface::eSize = 0;
D3DXHANDLE   D3D9ClientSurface::eKey = 0;
WORD *		 D3D9ClientSurface::Index = 0;
GPUBLITVTX * D3D9ClientSurface::pGPUVtx = 0;
WORD 		 D3D9ClientSurface::GPUBltIdx = 0;
D3D9Client * D3D9ClientSurface::gc = 0;
D3D9ClientSurface * D3D9ClientSurface::pPrevSrc = 0;


void D3D9ClientSurface::PrintError(int err)
{
	if ((err == ERR_DC_NOT_AVAILABLE) && (ErrWrn&err) == 0) LogErr("SurfHandle=0x%X, Never use Sketchpad::GetDC() hDC not available", this);
	if ((err == ERR_USED_NOT_DEFINED) && (ErrWrn&err) == 0) LogErr("SurfHandle=0x%X, Surface is being read without being initialized first", this);
	ErrWrn |= err;
}



HRESULT D3D9ClientSurface::AddQueue(D3D9ClientSurface *src, LPRECT s, LPRECT t)
{

	if ((pPrevSrc!=NULL && pPrevSrc!=src) || GPUBltIdx>60) {
		HRESULT hr = FlushQueue(); if (hr!=S_OK) return hr;
	}
	pGPUVtx[GPUBltIdx].sx = short(s->left);	pGPUVtx[GPUBltIdx].sy = short(s->top);
	pGPUVtx[GPUBltIdx].tx = short(t->left);	pGPUVtx[GPUBltIdx].ty = short(t->top);
	GPUBltIdx++;
	pGPUVtx[GPUBltIdx].sx = short(s->left);	pGPUVtx[GPUBltIdx].sy = short(s->bottom);
	pGPUVtx[GPUBltIdx].tx = short(t->left);	pGPUVtx[GPUBltIdx].ty = short(t->bottom);
	GPUBltIdx++;
	pGPUVtx[GPUBltIdx].sx = short(s->right); pGPUVtx[GPUBltIdx].sy = short(s->bottom);
	pGPUVtx[GPUBltIdx].tx = short(t->right); pGPUVtx[GPUBltIdx].ty = short(t->bottom);
	GPUBltIdx++;
	pGPUVtx[GPUBltIdx].sx = short(s->right); pGPUVtx[GPUBltIdx].sy = short(s->top);
	pGPUVtx[GPUBltIdx].tx = short(t->right); pGPUVtx[GPUBltIdx].ty = short(t->top);
	GPUBltIdx++;

	pPrevSrc = src;

	return S_OK;
}



HRESULT D3D9ClientSurface::FlushQueue()
{

	// ATTENTION:  Must use texture address mode CLAMP

	if (GPUBltIdx==0) return S_OK;
	if (pPrevSrc==NULL) return S_OK;

	if (pPrevSrc->ColorKey) {
		FX->SetValue(eColor, &pPrevSrc->ClrKey, sizeof(D3DXCOLOR));
		FX->SetBool(eKey, true);
	}
	else FX->SetBool(eKey, false);

	FX->SetTexture(eTex0, pPrevSrc->GetTexture());

	float srw = 1.0f / float(pPrevSrc->desc.Width);
	float srh = 1.0f / float(pPrevSrc->desc.Height);

	FX->SetVector(eSize, &D3DXVECTOR4(srw, srh, 1, 1));

	FX->CommitChanges();

	pDevice->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, GPUBltIdx, GPUBltIdx>>1, Index, D3DFMT_INDEX16, pGPUVtx, sizeof(GPUBLITVTX));
	
	GPUBltIdx = 0;
	pPrevSrc = NULL;

	return S_OK;
}



HRESULT D3D9ClientSurface::BeginBlitGroup()
{
	if (bBltGroup) return S_OK;

	if (IsRenderTarget()==false) {
		LogWrn("BeginBlitGroup(): Invalid surface type");
		return -1;
	}

	if (BindGPU()) {
		pDevice->SetVertexDeclaration(pGPUBlitDecl);
		FX->SetMatrix(eVP, pVP);
		FX->SetTechnique(eFlush);
		UINT numPasses = 0;
		FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
		FX->BeginPass(0);
		bBltGroup = true;
		return S_OK;
	}
	return -1;
}


void D3D9ClientSurface::EndBlitGroup()
{
	if (!bBltGroup) return;

	if (iBindCount>0) {
		FlushQueue();
		FX->EndPass();
		FX->End();
		ReleaseGPU();
	}
	bBltGroup = false;
}

int D3D9ClientSurface::GetQueueSize()
{
	return GPUBltIdx>>2;
}
// -----------------------------------------------------------------------------------------------
//
HRESULT D3D9ClientSurface::GPUCopyRect(D3D9ClientSurface *src, LPRECT s, LPRECT t)
{
	// ATTENTION:  Must use texture address mode CLAMP

	pDevice->SetVertexDeclaration(pPosTexDecl);

	FX->SetTechnique(eTech);
	FX->SetMatrix(eVP, pVP);
	FX->SetValue(eColor, &src->ClrKey, sizeof(D3DXCOLOR));
	FX->SetTexture(eTex0, src->pTex);

	float srw = 1.0f / float(src->desc.Width);
	float srh = 1.0f / float(src->desc.Height);
	float lwq = float(s->left) * srw;
	float thq = float(s->top) * srh;
	float rwq = float(s->right) * srw;
	float bhq = float(s->bottom) * srh;

	SMVERTEX Vertex[4] = {
		{float(t->left),  float(t->top),    0, lwq, thq},
		{float(t->left),  float(t->bottom), 0, lwq, bhq},
		{float(t->right), float(t->bottom), 0, rwq, bhq},
		{float(t->right), float(t->top),    0, rwq, thq}
	};

	static WORD cIndex[6] = {0,2,1,0,3,2};

	UINT numPasses = 0;
	FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE);
	FX->BeginPass(0);

	HR(pDevice->DrawIndexedPrimitiveUP(D3DPT_TRIANGLELIST, 0, 4, 2, &cIndex, D3DFMT_INDEX16, &Vertex, sizeof(SMVERTEX)));
	
	FX->EndPass();
	FX->End();

	return S_OK;
}

// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::SetupViewPort()
{
	if (!pVP) pVP = new D3DXMATRIX;
	if (!pViewPort) pViewPort = new D3DVIEWPORT9;

	D3DXMatrixOrthoOffCenterLH(pVP, 0.0f, (float)desc.Width, (float)desc.Height, 0.0f, 0.0f, 1.0f);

	pViewPort->X = 0;
	pViewPort->Y = 0;
	pViewPort->Width  = desc.Width;
	pViewPort->Height = desc.Height;
	pViewPort->MinZ = 0.0f;
	pViewPort->MaxZ = 1.0f;
}


// -----------------------------------------------------------------------------------------------
//
D3D9ClientSurface::D3D9ClientSurface(LPDIRECT3DDEVICE9 pDev, const char* name_ /* = NULL */) : alloc_id('SURF')
{
	Clear();
	strcpy_s(this->name, ARRAYSIZE(this->name), (name_ ? name_ : "???"));
	pDevice = pDev;
	SurfaceCatalog->Add(this);
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::Clear()
{
	ErrWrn		= 0;
	Refs		= 1;
	ColorKey	= 0;
	ClrKey		= D3DXCOLOR(ColorKey);
	pSkp		= NULL;
	pVP			= NULL;
	pViewPort	= NULL;
	pTex		= NULL;
	pSurf		= NULL;
	pRTS		= NULL;
	pDCSub		= NULL;
	hDefFont	= NULL;
	pStencil	= NULL;
	iBindCount  = 0;
	Initial		= 0;
	Active		= 0;
	GDIBltCtr	= 0;
	SketchPad	= SKETCHPAD_NONE;
	bDCOpen		= false;
	bBltGroup   = false;
	bBackBuffer = false;
	bLockable	= false;
	bMainDC		= true;
	bDCSys		= false;
	bBltSys		= false;
	bAdvanced   = false;
	bHard		= false;
	pDevice		= NULL;
	memset2(&desc, 0, sizeof(D3DSURFACE_DESC));
	memset2(pMap, 0, sizeof(pMap));
}


// -----------------------------------------------------------------------------------------------
//
D3D9ClientSurface::~D3D9ClientSurface()
{
	// Notify client about a surface destruction
	gc->clbkSurfaceDeleted(this);


	if (SurfaceCatalog->Remove(this)==false) {
		LogErr("Surface 0x%X wasn't in the catalog",this);
	}

	if (pSkp) {
		if (pSkp->IsStillDrawing()) LogErr("D3D9ClientSurface::~D3D9ClientSurface(0x%X) SketchPad 0x%X not released properly", this, pSkp);
		SAFE_DELETE(pSkp);
	}

	if (bDCOpen) {
		LogErr("D3D9ClientSurface::~D3D9ClientSurface(0x%X) HDC not released properly", this);
	}

	if (bHard) {
		LogErr("D3D9ClientSurface::~D3D9ClientSurface(0x%X) bHard is True", this);
	}

	if (bBackBuffer) {
		SAFE_RELEASE(pSurf);
		SAFE_DELETE(pVP);
		SAFE_DELETE(pViewPort);
		if (pTex) LogErr("BackBuffer has a Texture Interface");
		return;
	}

	LogBlu("Deleting Surface oapi:0x%X dx:0x%X (%s) (%u,%u)...",this, pSurf, name,desc.Width,desc.Height);

	if (!pTex) { SAFE_RELEASE(pSurf); }
	else { SAFE_RELEASE(pSurf); }

	SAFE_RELEASE(pTex);
	SAFE_RELEASE(pStencil);
	SAFE_RELEASE(pDCSub);
	SAFE_RELEASE(pRTS);
	SAFE_DELETE(pVP);
	SAFE_DELETE(pViewPort);

	for (int i = 0; i < ARRAYSIZE(pMap); i++) SAFE_RELEASE(pMap[i]);
}

// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::BindGPU()
{
	if (iBindCount>0) {
		FlushQueue();
		iBindCount++;
		return true;
	}

	if (IsCompressed()) Decompress();

	if ((desc.Usage&D3DUSAGE_RENDERTARGET)==0) {
		LogErr("BindGPU() Failed for 0x%X not a render target",this);
		return false;
	}

	if (!pViewPort || !pVP) SetupViewPort();

	if (pRTS==NULL) {
		if (D3DXCreateRenderToSurface(pDevice, desc.Width, desc.Height, desc.Format, false, D3DFMT_UNKNOWN, &pRTS)!=S_OK) {
			pRTS=NULL;
			LogErr("D3D9ClientSurface::BindGPU() Failed");
			return false;
		}
	}

	if (pRTS) {
		if (pRTS->BeginScene(pSurf, pViewPort)==S_OK) {
			iBindCount++;
			return true;
		}
		LogErr("D3D9ClientSurface::BindGPU() Failed for surface 0x%X", this);
	}
	return false;
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::ReleaseGPU()
{
	iBindCount--;

	if (iBindCount==0) {
		if (pRTS) HR(pRTS->EndScene(D3DX_FILTER_NONE));
		return;
	}
	if (iBindCount<0) LogErr("D3D9ClientSurface::ReleaseGPU()  iBindCount=%d",iBindCount);
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::ConvertSurface(DWORD attrib)
{
	DWORD flags = 0;
	Initial = attrib;
	Active = attrib;

	// Create System Memory Surface ---------------------------------------------------------------------------------------
	//
	if (attrib&OAPISURFACE_SYSMEM) {
		ConvertToPlain();
		return;
	}

	// Create Texture ------------------------------------------------------------------------------------------------------
	//
	if (attrib&OAPISURFACE_TEXTURE) {

		if (attrib&OAPISURFACE_RENDER3D) MakeDepthStencil();

		flags = OAPISURFACE_RENDERTARGET|OAPISURFACE_GDI;

		if ((attrib&flags)==flags) {
			ConvertToRenderTargetTexture();
			CreateSubSurface();
			return;
		}

		if (attrib&OAPISURFACE_GDI) {
			ConvertToTexture(true);
			return;
		}

		if (attrib&OAPISURFACE_RENDERTARGET) {
			ConvertToRenderTargetTexture();
			return;
		}

		ConvertToTexture(true);
		return;
	}


	// Create Non Texture Surface --------------------------------------------------------------------------------------------
	//
	if (attrib&OAPISURFACE_RENDER3D) MakeDepthStencil();

	flags = OAPISURFACE_RENDERTARGET|OAPISURFACE_GDI;

	if ((attrib&flags)==flags) {
		ConvertToRenderTarget(true);
		return;
	}

	if (attrib&OAPISURFACE_RENDERTARGET) {
		ConvertToRenderTarget(false);
		return;
	}

	ConvertToRenderTarget();
}



// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::CreateSurface(int w, int h, DWORD attrib)
{
	DWORD flags = 0;
	DWORD usage = 0;

	// Process flag conflicts and issues ---------------------------------------------------------------------------------
	//
	flags = OAPISURFACE_RENDERTARGET|OAPISURFACE_SYSMEM;
	if ((attrib&flags)==flags) {
		LogErr("oapiCreateSurfaceEx() Cannot combine OAPISURFACE_RENDERTARGET | OAPISURFACE_SYSMEM");
		attrib-=OAPISURFACE_SYSMEM;
	}

	flags = OAPISURFACE_RENDER3D|OAPISURFACE_SYSMEM;
	if ((attrib&flags)==flags) {
		LogErr("oapiCreateSurfaceEx() Cannot combine OAPISURFACE_RENDER3D | OAPISURFACE_SYSMEM");
		attrib-=OAPISURFACE_SYSMEM;
	}

	flags = OAPISURFACE_TEXTURE|OAPISURFACE_SYSMEM;
	if ((attrib&flags)==flags) {
		LogErr("oapiCreateSurfaceEx() Cannot combine OAPISURFACE_TEXTURE | OAPISURFACE_SYSMEM");
		attrib-=OAPISURFACE_SYSMEM;
	}

	flags = OAPISURFACE_MIPMAPS|OAPISURFACE_GDI;
	if ((attrib&flags)==flags) {
		LogErr("oapiCreateSurfaceEx() Cannot combine OAPISURFACE_MIPMAPS | OAPISURFACE_GDI");
		attrib-=OAPISURFACE_SYSMEM;
	}

	/*
	if (attrib&OAPISURFACE_SKETCHPAD) {
		attrib |= OAPISURFACE_RENDERTARGET;
		attrib &= ~(OAPISURFACE_SYSMEM|OAPISURFACE_GDI);
	}*/

	// Process Surface Format ---------------------------------------------------------------------------------------------
	//
	D3DFORMAT fmt = D3DFMT_X8R8G8B8;
	if (attrib&OAPISURFACE_ALPHA) fmt = D3DFMT_A8R8G8B8;
	if (attrib&OAPISURFACE_GDI)   fmt = D3DFMT_X8R8G8B8;

	// Process Surface Pool ------------------------------------------------------------------------------------------------
	//
	D3DPOOL pool = D3DPOOL_DEFAULT;
	if (attrib&OAPISURFACE_SYSMEM) pool = D3DPOOL_SYSTEMMEM;
	if (attrib&OAPISURFACE_TEXTURE) if (attrib&OAPISURFACE_MIPMAPS) usage = D3DUSAGE_AUTOGENMIPMAP;

	MakeEmptySurfaceEx(w, h, usage, fmt, pool, attrib);

	ConvertSurface(attrib);
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::MakeEmptySurfaceEx(UINT Width, UINT Height, DWORD Usage, D3DFORMAT Format, D3DPOOL pool, DWORD Flags)
{
	if (Width==0 || Height==0) { LogErr("Trying to create a surface with zero size Handle=0x%X",this); return;	}
	if (Width>8192 || Height>8192) { LogErr("Large surface created Handle=0x%X (%u,%u)", this, Width, Height); return;	}

	Initial = Active = Flags;

	desc.Format = Format;
	desc.Height = Height;
	desc.Pool = pool;
	desc.Usage = Usage;
	desc.Width = Width;
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::MakeEmptyTextureEx(UINT Width, UINT Height, DWORD Usage, D3DFORMAT Format)
{
	if (Width==0 || Height==0) { LogErr("Trying to create a texture with zero size Handle=0x%X",this); return;	}
	if (Width>8192 || Height>8192) { LogErr("Large texture created Handle=0x%X (%u,%u)", this, Width, Height); return;	}

	Initial = Active = OAPISURFACE_TEXTURE;
	
	desc.Format = Format;
	desc.Height = Height;
	desc.Pool = D3DPOOL_DEFAULT;
	desc.Usage = Usage;
	desc.Width = Width;
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::MakeDepthStencil()
{
	if (pStencil) return;
	LogBlu("D3D9ClientSurface: Creating DepthStencil SubSurface for 0x%X", this);
	HR(pDevice->CreateDepthStencilSurface(desc.Width, desc.Height, D3DFMT_D24S8, D3DMULTISAMPLE_NONE, 0, true, &pStencil, NULL));
}


// -----------------------------------------------------------------------------------------------
// Use only in D3D9Frame.cpp
//
void D3D9ClientSurface::MakeBackBuffer(LPDIRECT3DSURFACE9 pBuf)
{
	_TRACE;
	LogBlu("Creating a BackBuffer SURFHANDLE=0x%X  D3DSURF=0x%X",this,pBuf);
	strcpy_s(name,32,"BackBuffer");
	pSurf = pBuf;
	pTex = NULL;
	Initial = Active = OAPISURFACE_RENDERTARGET;
	bBackBuffer = true;
	GetDesc(&desc);
	SetupViewPort();
}


// -----------------------------------------------------------------------------------------------
// This function will reload the surface with decompression
//
void D3D9ClientSurface::Decompress(DWORD Attr)
{
	SAFE_RELEASE(pSurf);
	SAFE_RELEASE(pTex);

	// Decompress all formats to rendertarget unless sysmem is requested
	if ((GetAttribs()&OAPISURFACE_SYSMEM)==0) Attr |= OAPISURFACE_RENDERTARGET;

	// Must reload the file because it's initially loaded in default pool
	if (LoadSurface(name, Attr|GetAttribs()|OAPISURFACE_UNCOMPRESS, true)) {
		LogBlu("Texture 0x%X (%s) Decompressed",this,name);
		if (IsRenderTarget()) SetupViewPort();
		return;
	}

	LogErr("Failed to decompress surface");
	LogSpecs("Surface");
	assert(false);
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::CreateSubSurface()
{
	if (pDCSub) return;
	HR(pDevice->CreateRenderTarget(desc.Width, desc.Height, desc.Format, D3DMULTISAMPLE_NONE, 0, true, &pDCSub, NULL));
	LogBlu("Creating SubSurface for 0x%X", this);
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::SyncSubSurface()
{
	HR(pDevice->StretchRect(pSurf, NULL, pDCSub, NULL, D3DTEXF_POINT));
	LogBlu("Surface 0x%X (%s) Synced (%u,%u)", this, name, desc.Width, desc.Height);
}


// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::ConvertToPlain()
{
	if (GetAttribs()&OAPISURFACE_RENDERTARGET) return false;
	if (GetAttribs()&OAPISURFACE_TEXTURE) return false;
	if (bBackBuffer) return false;

	LPDIRECT3DSURFACE9 pNew=NULL;
	SAFE_RELEASE(pRTS);
	SAFE_RELEASE(pDCSub);

	bLockable = false;

	HR(pDevice->CreateOffscreenPlainSurface(desc.Width, desc.Height, desc.Format, D3DPOOL_SYSTEMMEM, &pNew, NULL));	
	
	if (!pNew) {
		LogSpecs("Surface");
		assert(false);
		return false;
	}

	// -------------------------------------------------------
	if (pSurf==NULL) {
		pTex = NULL;
		pSurf = pNew;
		GetDesc(&desc);
		LogBlu("New PlainSurface 0x%X (%s) (%u,%u) (Sysmem)", this, name, desc.Width, desc.Height);
		return true;
	}

	if (desc.Pool==D3DPOOL_SYSTEMMEM) {	HR(pDevice->UpdateSurface(pSurf, NULL, pNew, NULL)); }
	if (desc.Usage&D3DUSAGE_RENDERTARGET) {	HR(pDevice->GetRenderTargetData(pSurf, pNew)); }

	SAFE_RELEASE(pSurf); SAFE_RELEASE(pTex);
	pSurf = pNew;
	GetDesc(&desc);
	LogBlu("Surface 0x%X (%s) Converted to PlainSurface (SysMem) (%u,%u)", this, name, desc.Width, desc.Height);
	return true;
}


// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::ConvertToRenderTargetTexture()
{
	LPDIRECT3DSURFACE9 pTgt;
	LPDIRECT3DTEXTURE9 pNew;

	bLockable = false;
	if (bBackBuffer) return false;
	if (GetAttribs()&OAPISURFACE_SYSMEM) return false;

	// Remove dynamic property, conflicts with rt
	if (desc.Usage&D3DUSAGE_DYNAMIC) desc.Usage-=D3DUSAGE_DYNAMIC;

	if (IsCompressed()) { Decompress();	return true; }

	DWORD Usage = desc.Usage|D3DUSAGE_RENDERTARGET;
	DWORD Mips = 1;

	if (GetAttribs()&OAPISURFACE_MIPMAPS) {
		Mips = 0;
		Usage |= D3DUSAGE_AUTOGENMIPMAP;
	}

	if (pDevice->CreateTexture(desc.Width, desc.Height, Mips, Usage, desc.Format, D3DPOOL_DEFAULT, &pNew, NULL)!=S_OK) {
		LogErr("CreateTexture Failed in ConvertToRenderTargetTexture(0x%X) W=%u, H=%u, usage=0x%X, Format=0x%X", this, desc.Width, desc.Height, desc.Usage, desc.Format);
		LogSpecs("Surface");
		assert(false);
		return false;
	}

	HR(pNew->GetSurfaceLevel(0, &pTgt));

	// -------------------------------------------------------
	if (pSurf==NULL) {
		pTex = pNew;
		pSurf = pTgt;
		GetDesc(&desc);
		SetupViewPort();
		LogBlu("New RenderTargetTexture oapi:0x%X dx:0x%X (%s) (%u,%u)", this, pSurf, name, desc.Width, desc.Height);
		return true;
	}

	// -------------------------------------------------------
	if (desc.Pool==D3DPOOL_DEFAULT) {
		if (pDevice->StretchRect(pSurf, NULL, pTgt, NULL, D3DTEXF_POINT)!=S_OK) {
			LogErr("StretchRect failed in ConvertToRenderTargetTexture()");
			LogSpecs("Surface");
			assert(false);
			return false;
		}
	}

	// -------------------------------------------------------
	if (desc.Pool==D3DPOOL_SYSTEMMEM) {
		if (pDevice->UpdateSurface(pSurf, NULL, pTgt, NULL)!=S_OK) {
			LogErr("UpdateSurface failed in ConvertToRenderTargetTexture()");
			LogSpecs("Surface");
			assert(false);
			return false;
		}
	}
	
	SAFE_RELEASE(pSurf);
	SAFE_RELEASE(pTex);

	pTex  = pNew;
	pSurf = pTgt;

	//Active = OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE;

	GetDesc(&desc);
	SetupViewPort();
	LogBlu("Surface 0x%X (%s) Converted to RenderTargetTexture (%u,%u)", this, name, desc.Width, desc.Height);
	return true;
}


// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::ConvertToRenderTarget(bool bLock)
{
	LPDIRECT3DSURFACE9 pTgt;

	if (bBackBuffer) return true;
	if (GetAttribs()&OAPISURFACE_SYSMEM) return false;
	if (GetAttribs()&OAPISURFACE_TEXTURE) return false;
	if ((bLockable==bLock) && (desc.Usage&D3DUSAGE_RENDERTARGET) && pTex==NULL) return true;

	if (pDevice->CreateRenderTarget(desc.Width, desc.Height, desc.Format, D3DMULTISAMPLE_NONE, 0, bLock, &pTgt, NULL)!=S_OK) {
		LogErr("CreateRenderTarget Failed in ConvertToRenderTarget(0x%X) W=%u, H=%u, usage=0x%X, Format=0x%X", this, desc.Width, desc.Height, desc.Usage, desc.Format);
		LogSpecs("Surface");
		assert(false);
		return false;
	}

	// -------------------------------------------------------
	if (pSurf==NULL) {
		pTex = NULL;
		pSurf = pTgt;
		bLockable = bLock;
		GetDesc(&desc);
		SetupViewPort();
		LogBlu("New RenderTarget oapi:0x%X dx:0x%X (%s) (%u,%u)", this, pSurf, name, desc.Width, desc.Height);
		return true;
	}

	// -------------------------------------------------------
	if (desc.Pool==D3DPOOL_DEFAULT) {
		if (pDevice->StretchRect(pSurf, NULL, pTgt, NULL, D3DTEXF_POINT)!=S_OK) {
			LogErr("StretchRect failed in ConvertToRenderTarget()");
			LogSpecs("Surface");
			assert(false);
			return false;
		}
	}

	// -------------------------------------------------------
	if (desc.Pool==D3DPOOL_SYSTEMMEM) {
		if (pDevice->UpdateSurface(pSurf, NULL, pTgt, NULL)!=S_OK) {
			LogErr("UpdateSurface failed in ConvertToRenderTarget()");
			LogSpecs("Surface");
			assert(false);
			return false;
		}
	}
	
	SAFE_RELEASE(pSurf);
	SAFE_RELEASE(pTex);

	bLockable = bLock;
	pTex = NULL;
	pSurf = pTgt;

	//Active = OAPISURFACE_RENDERTARGET;
	//if (bLock) Active |= OAPISURFACE_GDI;

	GetDesc(&desc);
	SetupViewPort();

	if (bLock) LogBlu("Surface 0x%X (%s) Converted to Lock-able RenderTarget (%u,%u)", this, name, desc.Width, desc.Height);
	else	   LogBlu("Surface 0x%X (%s) Converted to RenderTarget (%u,%u)", this, name, desc.Width, desc.Height);
	return true;
}


// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::ConvertToTexture(bool bDynamic)
{
	if (bBackBuffer) return false;
	if (GetAttribs()&OAPISURFACE_SYSMEM) return false;
	if (GetAttribs()&OAPISURFACE_RENDERTARGET) return false;

	LPDIRECT3DSURFACE9 pTgt=NULL;
	LPDIRECT3DTEXTURE9 pNew=NULL;
	SAFE_RELEASE(pRTS);
	bLockable = false;
	
	DWORD Mips = 1;
	if (desc.Usage&D3DUSAGE_AUTOGENMIPMAP) Mips = 0;
	if (GetAttribs()&OAPISURFACE_MIPMAPS) Mips = 0;
	
	if (bDynamic) {
		HR(pDevice->CreateTexture(desc.Width, desc.Height, Mips, D3DUSAGE_DYNAMIC, desc.Format, D3DPOOL_DEFAULT, &pNew, NULL));	
		if (pNew) {
			HR(pNew->GetSurfaceLevel(0, &pTgt));
		}
	}
	else {
		HR(pDevice->CreateTexture(desc.Width, desc.Height, Mips, 0, desc.Format, D3DPOOL_DEFAULT, &pNew, NULL));	
		if (pNew) {
			HR(pNew->GetSurfaceLevel(0, &pTgt));
		}
	}

	if (!pTgt || !pNew) return false;

	// -------------------------------------------------------
	if (pSurf==NULL) {
		pTex = pNew;
		pSurf = pTgt;
		GetDesc(&desc);
		if (bDynamic) LogBlu("New Texture 0x%X (%s) (%u,%u) Dynamic", this, name, desc.Width, desc.Height);
		else		  LogBlu("New Texture 0x%X (%s) (%u,%u)", this, name, desc.Width, desc.Height);
		return true;
	}

	// -------------------------------------------------------
	if (desc.Pool==D3DPOOL_SYSTEMMEM) {
		HR(pDevice->UpdateSurface(pSurf, NULL, pTgt, NULL));
		SAFE_RELEASE(pSurf);
		SAFE_RELEASE(pTex);
		pTex = pNew;
		pSurf = pTgt;
		GetDesc(&desc);
		if (bDynamic) LogBlu("Surface 0x%X (%s) Converted to Dynamic Texture (%u,%u)", this, name, desc.Width, desc.Height);
		else		  LogBlu("Surface 0x%X (%s) Converted to Texture (%u,%u)", this, name, desc.Width, desc.Height);
		return true;
	}

	LogErr("ConvertToTexture(0x%X) No Conversion Rule", this);
	LogSpecs("Surface");
	assert(false);
	return false;
}



// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::CopyRect(D3D9ClientSurface *src, LPRECT s, LPRECT t, UINT ck)
{
	_TRACE;
	bool bRestart = true;

	// Check failure and abort conditions -------------------------------------------------------
	//
	if (t->right > (long)desc.Width || t->bottom > (long)desc.Height) goto invalid_input;
	if (t->left < 0 || t->top < 0)  goto invalid_input;

	if (s->right > (long)src->desc.Width || s->bottom > (long)src->desc.Height)  goto invalid_input;
	if (s->left < 0 || s->top < 0)  goto invalid_input;

	if (s->left > s->right)  goto invalid_input;
	if (t->left > t->right)  goto invalid_input;
	if (s->top > s->bottom)  goto invalid_input;
	if (t->top > t->bottom)  goto invalid_input;

	DWORD Width = s->right - s->left;
	DWORD Height = s->bottom - s->top;
	DWORD TgtWidth = t->right - t->left;
	DWORD TgtHeight = t->bottom - t->top;

	if (Width==0 || Height==0 || TgtWidth==0 || TgtHeight==0)  goto invalid_input;

	// Is scaling operation required -------------------------------------------------------------
	//
	bool bStretch = true;
	D3DTEXTUREFILTERTYPE Filter = D3DTEXF_LINEAR;
	if (Width==TgtWidth && Height==TgtHeight) bStretch = false, Filter = D3DTEXF_POINT;


	// =====================================================================================================
	// If target not yet exists
	//
	if (!Exists()) {
		if (GetAttribs()&OAPISURFACE_TEXTURE) ConvertToRenderTargetTexture();
		else								  ConvertToRenderTarget(true);
	}


	// =====================================================================================================
	// If source not yet exists
	//
	if (!src->Exists()) {
		src->PrintError(ERR_USED_NOT_DEFINED);
		src->Clear(0xFF000000);
	}


	// If compressed surface is involved in blitting then decompress ------------------------------
	//
	if (IsCompressed()) Decompress();
	if (src->IsCompressed()) src->Decompress();


	// =====================================================================================================
	// ANOMALIUS CASE: If target is non-render target texture.. Convert.. 
	//
	if (pTex && (desc.Usage&D3DUSAGE_RENDERTARGET)==0) ConvertToRenderTargetTexture();


	// =====================================================================================================
	// StretchRect InSurface Work-a-round
	//
	if (src==this) {

		if (!IsRenderTarget()) ConvertToRenderTargetTexture();
		if (!HasSubSurface()) CreateSubSurface();
	
		// Do the Blitting
		if (pDevice->StretchRect(pSurf, s, pDCSub, s, D3DTEXF_POINT)==S_OK) {
			if (pDevice->StretchRect(pDCSub, s, pSurf, t, D3DTEXF_POINT)==S_OK) {
				LogOk("InSurface-StretchRect 0x%X (%s) -> 0x%X (%s) (%u,%u)", src, src->name, this, name, Width, Height);
				return;
			}
		}
		LogErr("StretchRect Blitting Failed 0x%X (%s) -> 0x%X (%s)", src, src->name, this, name);
		goto error_report;	
	}

	// ==========================================================================================================
	// SPECIAL CASE: ColorKeyed blitting using primary GPU
	//
	if (src->ColorKey) {

		if (src->IsTexture()==false) {
			src->ConvertToRenderTargetTexture();
			src->Active |= OAPISURFACE_TEXTURE;
		}

		if (bBackBuffer) {
			if (GPUCopyRect(src, s, t)==S_OK) {
				LogOk("GPU ColorKey Blitting 0x%X (%s) -> 0x%X (%s) (%u,%u)", src, src->name, this, name, Width, Height);
				return;
			}
		} else {
			if (!IsRenderTarget()) {
				ConvertToRenderTargetTexture();
				Active |= OAPISURFACE_RENDERTARGET;
			}
			if (BindGPU()) {
				GPUCopyRect(src, s, t);
				ReleaseGPU();
				LogOk("GPU ColorKey_2 Blitting 0x%X (%s) -> 0x%X (%s) (%u,%u)", src, src->name, this, name, Width, Height);
				return;
			}
		}
		LogErr("Colorkeyed blit failed 0x%X (%s) -> 0x%X (%s)", src, src->name, this, name);
		goto error_report;	
	}


	// ==========================================================================================================
	// SPECIAL CASE: Color Conversion from Source to Target is required
	//
	if (src->desc.Format != desc.Format) {
		if (src->IsTexture()==false) {
			src->ConvertToRenderTargetTexture();
			src->Active |= OAPISURFACE_TEXTURE;
		}
		if (bBackBuffer) {
			if (GPUCopyRect(src, s, t)==S_OK) {
				LogOk("GPU Blitting 0x%X (%s) -> 0x%X (%s) (%u,%u)", src, src->name, this, name, Width, Height);
				return;
			}
		} else {
			if (!IsRenderTarget()) {
				ConvertToRenderTargetTexture();
				Active |= OAPISURFACE_RENDERTARGET;
			}
			if (BindGPU()) {
				if (GPUCopyRect(src, s, t)==S_OK) {
					LogOk("GPU Blitting 0x%X (%s) -> 0x%X (%s) (%u,%u)", src, src->name, this, name, Width, Height);
					ReleaseGPU();
					return;
				} else LogErr("SketchRect CC Failed 0x%X (%s) -> 0x%X (%s)", src, src->name, this, name);
				ReleaseGPU();
				goto error_report;
			}
		}
	}


	// =====================================================================================================
	// SPECIAL CASE: For screen capture or similar thing
	// Get Render Target Data
	//
	if (src->desc.Pool==D3DPOOL_DEFAULT && desc.Pool==D3DPOOL_SYSTEMMEM) {
		if (src->desc.Usage&D3DUSAGE_RENDERTARGET) {
			if (!HasSubSurface()) CreateSubSurface();
			// Copy graphics in sub surface and then in a sysmem main surf
			if (pDevice->StretchRect(src->pSurf, s, pDCSub, t, D3DTEXF_POINT)==S_OK) {
				if (pDevice->GetRenderTargetData(pDCSub, pSurf)==S_OK) {
					LogOk("- ! - GetRenderTargetData 0x%X (%s) -> 0x%X (%s) (%u,%u) - ! -", src, src->name, this, name, Width, Height);
					return;
				}
			}
			LogErr("GetRenderTargetData Failed 0x%X (%s) -> 0x%X (%s)", src, src->name, this, name);
			goto error_report;	
		}
	}
	

	// =====================================================================================================
	// StretchRect Blitting Technique
	//
	if (desc.Pool==D3DPOOL_DEFAULT && src->desc.Pool==D3DPOOL_DEFAULT) {
		if (pDevice->StretchRect(src->pSurf, s, pSurf, t, Filter)==S_OK) {
			LogOk("StretchRect 0x%X (%s) -> 0x%X (%s) (%u,%u)", src, src->name, this, name, Width, Height);
			return;
		}
		LogErr("StretchRect Blitting Failed 0x%X (%s) -> 0x%X (%s)", src, src->name, this, name);
		goto error_report;
	}
	

	// =====================================================================================================
	// UpdateSurface Blitting Technique
	//
	if (desc.Pool==D3DPOOL_DEFAULT && src->desc.Pool==D3DPOOL_SYSTEMMEM) {
		POINT p; p.x = t->left; p.y=t->top;
		if (pDevice->UpdateSurface(src->pSurf, s, pSurf, &p)==S_OK) {
			LogOk("UpdateSurface 0x%X (%s) -> 0x%X (%s) (%u,%u)", src, src->name, this, name, Width, Height);
			return;
		}
		LogErr("UpdateSurface Blitting Failed 0x%X (%s) -> 0x%X (%s)", src, src->name, this, name);
		goto error_report;
	}
	


	// Unable to perform blitting ----------------------------------------------------------------------------------------
	//
error_report:

	_SETLOG(5);
	LogErr("D3D9ClientSurface::CopyRect() Failed");
	LogErr("Source Rect (%d,%d,%d,%d) (w=%u,h=%u) HANDLE=0x%X (%s)",s->left,s->top,s->right,s->bottom, abs(s->left-s->right), abs(s->top-s->bottom), src, src->name);
	LogErr("Target Rect (%d,%d,%d,%d) (w=%u,h=%u) HANDLE=0x%X (%s)",t->left,t->top,t->right,t->bottom, abs(t->left-t->right), abs(t->top-t->bottom), this, name);
	LogSpecs("Target");
	src->LogSpecs("Source");
	assert(false);
	_POPLOG;
	return;

invalid_input:
	//LogWrn("Source Rect (%d,%d,%d,%d) (w=%u,h=%u) HANDLE=0x%X (%s)",s->left,s->top,s->right,s->bottom, abs(s->left-s->right), abs(s->top-s->bottom), src, src->name);
	//LogWrn("Target Rect (%d,%d,%d,%d) (w=%u,h=%u) HANDLE=0x%X (%s)",t->left,t->top,t->right,t->bottom, abs(t->left-t->right), abs(t->top-t->bottom), this, name);
	return;
}


// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::Fill(LPRECT rect, DWORD c)
{
	if (!Exists()) {
		if (GetAttribs()&OAPISURFACE_TEXTURE) ConvertToRenderTargetTexture();
		else ConvertToRenderTarget();
	}
	else {
		if (IsCompressed()) Decompress();
		if (!IsRenderTarget() && !IsDynamic()) {
			if (IsTexture()) ConvertToRenderTargetTexture();
			else			 ConvertToRenderTarget(true);
		}
	}

	
	LPRECT r;
	RECT re;

	if (rect==NULL) { re.left=0, re.top=0, re.right=desc.Width, re.bottom=desc.Height; r=&re; }
	else r = rect;

	if (desc.Pool==D3DPOOL_DEFAULT) {
		if (desc.Usage&D3DUSAGE_RENDERTARGET || (pTex==NULL && desc.Usage==0)) {
			if (pDevice->ColorFill(pSurf, r, c)!=S_OK) {
				LogErr("GPU ColorFill Failed");
				LogSpecs("Surface");
				assert(false);
				return false;
			}
			LogOk("ColorFill 0x%X (%s) (%u,%u)", this, name, (r->right-r->left), (r->bottom-r->top));
			return true;
		}
	}

	if (desc.Usage&D3DUSAGE_DYNAMIC) {

		HDC hDC = GetDCHard();

		if (hDC) {
			DWORD color = RGB((c>>16)&0xFF, (c>>8)&0xFF, c&0xFF);
			HBRUSH hBrush = CreateSolidBrush((COLORREF)color);
			HGDIOBJ hOld = SelectObject(hDC, hBrush);
			Rectangle(hDC,r->left,r->top,r->right,r->bottom);
			SelectObject(hDC, hOld);
			DeleteObject(hBrush);
			ReleaseDCHard(hDC);
			return true;
		}
		else {
			LogErr("GDI Fill Failed");
			LogSpecs("Surface");
			assert(false);
			return false;
		}
	}

	LogErr("ColorFill Failed");
	LogSpecs("Surface");
	assert(false);
	return false;
}


// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::Clear(DWORD c)
{
	if (!Exists()) {
		if (GetAttribs()&OAPISURFACE_TEXTURE) ConvertToRenderTargetTexture();
		else ConvertToRenderTarget();
	}
	else {
		if (IsCompressed()) Decompress();
		if (!IsRenderTarget() && !IsDynamic()) {
			if (IsTexture()) ConvertToRenderTargetTexture();
			else			 ConvertToRenderTarget(true);
		}
	}

	
	if (desc.Pool==D3DPOOL_DEFAULT) {
		if (desc.Usage&D3DUSAGE_RENDERTARGET || (pTex==NULL && desc.Usage==0)) {
			if (pDevice->ColorFill(pSurf, NULL, c)!=S_OK) {
				LogErr("GPU ClearSurface Failed");
				LogSpecs("Surface");
				assert(false);
				return false;
			}
			else LogOk("Clear Surface 0x%X (%s)(%u,%u)", this, name, desc.Width, desc.Height);
			return true;
		}
	}

	if (desc.Usage&D3DUSAGE_DYNAMIC || desc.Pool==D3DPOOL_SYSTEMMEM) {

		HDC hDC = GetDCHard();

		if (hDC) {
			DWORD color = RGB((c>>16)&0xFF, (c>>8)&0xFF, c&0xFF);
			HBRUSH hBrush = CreateSolidBrush((COLORREF)color);
			RECT r; r.left = 0; r.top = 0; r.right = desc.Width; r.bottom = desc.Height;
			FillRect(hDC, &r, hBrush);
			DeleteObject(hBrush);
			ReleaseDCHard(hDC);
			LogOk("Clear Surface GDI 0x%X (%s)(%u,%u)", this, name, desc.Width, desc.Height);
			return true;
		}
		else {
			LogErr("GDI Clear Failed");
			LogSpecs("Surface");
			assert(false);
			return false;
		}
	}

	LogErr("ClearSurface Failed");
	LogSpecs("Surface");
	assert(false);
	return false;
}


// -----------------------------------------------------------------------------------------------
// Only for internal use in D3D9ClientSurface
//
HDC	D3D9ClientSurface::GetDCHard()
{
	GetDCTime = 0.0;
	HDC hDC;
	if (iBindCount!=0) {
		LogErr("Surface bind count is %d in D3D9ClientSurface::GetDCHard()",iBindCount);
		return NULL;
	}
	double time = D3D9GetTime();
	if (pSurf->GetDC(&hDC) == S_OK) {
		D3D9SetTime(D3D9Stats.Timer.LockWait, time);
		GetDCTime = D3D9GetTime();
		bHard = true;
		return hDC;
	}
	LogErr("D3D9ClientSurface: GetDCHard() Failed");
	LogSpecs("Surface");
	assert(false);
	return NULL;
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::ReleaseDCHard(HDC hDC)
{
	if (bHard) {
		bHard = false;
		HR(pSurf->ReleaseDC(hDC));
		if (GetDCTime != 0.0) D3D9SetTime(D3D9Stats.Timer.GetDC, GetDCTime);
		return;
	}
	LogErr("D3D9ClientSurface: ReleaseDCHard() Failed");
	LogSpecs("Surface");
	assert(false);
}


// -----------------------------------------------------------------------------------------------
//
HDC	D3D9ClientSurface::GetDC()
{
	bHard = false;

	GetDCTime = 0.0;

	if (!Exists()) ConvertToTexture(true);

	if (IsCompressed()) Decompress(OAPISURFACE_NOALPHA);

	if (bDCOpen) {
		LogErr("DC is already open");
		return NULL;
	}

	if (iBindCount!=0) {
		LogErr("Surface bind count is %d in D3D9ClientSurface::GetDC()",iBindCount);
		return NULL;
	}

	if (desc.Format == D3DFMT_A8R8G8B8) {
		LogErr("Alpha channel surface in GetDC()");
		return NULL;
	}

	if (desc.Usage&D3DUSAGE_RENDERTARGET) {
		if (pTex && !HasSubSurface()) CreateSubSurface();
		if (!pTex && !bLockable) ConvertToRenderTarget(true);
	}
	
	bDCOpen = false;
	bMainDC = true;
	HDC hDC = NULL;

	double time = D3D9GetTime();

	// Attempting to acquire a hDC for a BackBuffer
	//
	if (bBackBuffer) {
		LogOk("GetDC() Surface=0x%X, BackBuffer", this);
		if (pSurf->GetDC(&hDC)==S_OK) { 
			D3D9SetTime(D3D9Stats.Timer.LockWait, time);
			GetDCTime = D3D9GetTime();
			bDCOpen = true;
			return hDC; 
		}
		goto skip;
	}

	if (desc.Usage&D3DUSAGE_DYNAMIC) {
		LogOk("GetDC() Surface=0x%X, Dynamic", this);
		if (pSurf->GetDC(&hDC)==S_OK) {
			D3D9SetTime(D3D9Stats.Timer.LockWait, time);
			GetDCTime = D3D9GetTime();
			bDCOpen = true; 
			return hDC; 
		}
		goto skip;
	}

	if (desc.Pool==D3DPOOL_SYSTEMMEM) {
		LogOk("GetDC() Surface=0x%X, SysMem", this);
		if (pSurf->GetDC(&hDC)==S_OK) {
			D3D9SetTime(D3D9Stats.Timer.LockWait, time);
			GetDCTime = D3D9GetTime();
			bDCOpen = true; 
			return hDC; 
		}
		goto skip;
	}

	if ((desc.Usage&D3DUSAGE_RENDERTARGET) && bLockable) {
		LogOk("GetDC() Surface=0x%X, RT-Lock", this);
		if (pSurf->GetDC(&hDC)==S_OK) { 
			D3D9SetTime(D3D9Stats.Timer.LockWait, time);
			GetDCTime = D3D9GetTime();
			bDCOpen = true; 
			return hDC;
		}
		goto skip;
	}

	// Provide DC into a sub-surface ---------------------------------------
	//
	if (pDCSub) {
		bMainDC = false;
		LogOk("GetDC() Surface=0x%X, Sub-RT-Lock", this);
		HR(pDevice->StretchRect(pSurf, NULL, pDCSub, NULL, D3DTEXF_POINT));
		if (pDCSub->GetDC(&hDC)==S_OK) { 
			D3D9SetTime(D3D9Stats.Timer.LockWait, time);
			GetDCTime = D3D9GetTime();
			bDCOpen = true; 
			return hDC; 
		}
		goto skip;
	}

skip:
	LogErr("D3D9ClientSurface: GetDC() Failed");
	LogSpecs("Surface");
	assert(false);
	return NULL;
}





// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::ReleaseDC(HDC hDC)
{
	bDCOpen = false;
	if (bMainDC) { 
		HR(pSurf->ReleaseDC(hDC)); 
		if (GetDCTime != 0.0) D3D9SetTime(D3D9Stats.Timer.GetDC, GetDCTime);
		return; 
	}
	if (pDCSub) {
		if (pDCSub->ReleaseDC(hDC)==S_OK) {
			if (GetDCTime != 0.0) D3D9SetTime(D3D9Stats.Timer.GetDC, GetDCTime);
			HR(pDevice->StretchRect(pDCSub, NULL, pSurf, NULL, D3DTEXF_POINT));			
			return;
		}
	}
	LogErr("ReleaseDC() Failed");
	LogSpecs("Surface");
	assert(false);
}


// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::CreateName(char *out, int mlen, const char *fname, const char *id)
{
	char buffe[128];
	strcpy_s(buffe, 128, fname);
	char *p = strrchr(buffe, '.');
	if (p != NULL) {
		*p = '\0';
		sprintf_s(out, mlen, "%s_%s.%s", buffe, id, ++p);
	}
	return (p != NULL);
}

// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::Reload()
{
	bool bGoTex = true;

	//if (Initial&OAPISURFACE_SYSMEM) bGoTex = false;
	//if (Initial&OAPISURFACE_UNCOMPRESS) bGoTex = false;
	//if (Initial&OAPISURFACE_RENDERTARGET) bGoTex = false;
	//if (Initial&OAPISURFACE_NOMIPMAPS) bGoTex = false;
	//if (Initial&OAPISURFACE_MIPMAPS) bGoTex = false;
	//if (Initial&OAPISURFACE_GDI) bGoTex = false;
	//if (Initial&OAPISURFACE_ALPHA) bGoTex = false;
	//if (Initial&OAPISURFACE_NOALPHA) bGoTex = false;

	if (bGoTex) {
		SAFE_RELEASE(pSurf);
		SAFE_RELEASE(pTex);
		for (int i = 0; i < ARRAYSIZE(pMap); i++) SAFE_RELEASE(pMap[i]);
		LoadTexture(name);
	}
}

// LoadSurface -------------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::LoadSurface(const char *fname, DWORD flags, bool bDecompress)
{
	char path[MAX_PATH];

	if (gc==NULL) {
		LogErr("D3D9ClientSurface::LoadTexture() No Client Pointer");
		return false;
	}

	if (bDecompress) Active = flags; 
	else Initial = Active = flags;

	// If nothing special is requested then switch to LoadTexture()
	//
	bool bGoTex = true;
	if (flags&OAPISURFACE_SYSMEM) bGoTex = false;
	if (flags&OAPISURFACE_UNCOMPRESS) bGoTex = false;
	if (flags&OAPISURFACE_RENDERTARGET) bGoTex = false;
	if (flags&OAPISURFACE_NOMIPMAPS) bGoTex = false;
	if (flags&OAPISURFACE_MIPMAPS) bGoTex = false;
	if (flags&OAPISURFACE_GDI) bGoTex = false;
	if (flags&OAPISURFACE_ALPHA) bGoTex = false;
	if (flags&OAPISURFACE_NOALPHA) bGoTex = false;

	if (flags&OAPISURFACE_SKETCHPAD) {
		LogErr("OAPISURFACE_SKETCHPAD in D3D9ClientSurface::LoadSurface (should not happen)");
	}

	if (bGoTex) return LoadTexture(fname);

	if (gc->TexturePath(fname, path)) {

		// Get information about the file
		//
		D3DXIMAGE_INFO info;
		HR(D3DXGetImageInfoFromFile(path, &info));

		if (info.Height>8192 || info.Width>8192) LogErr("Loading a large surface Handle=0x%X (%u,%u)", this, info.Width, info.Height);



		// Uncompress these cases
		if (flags&OAPISURFACE_RENDERTARGET) flags |= OAPISURFACE_UNCOMPRESS;
		if (flags&OAPISURFACE_SYSMEM) flags |= (OAPISURFACE_UNCOMPRESS | OAPISURFACE_NOMIPMAPS);
		if (flags&OAPISURFACE_GDI) flags |= (OAPISURFACE_UNCOMPRESS | OAPISURFACE_NOMIPMAPS);

		// Check render target mipmap support
		if ((flags&OAPISURFACE_RENDERTARGET) && !(flags&OAPISURFACE_TEXTURE)) flags |= OAPISURFACE_NOMIPMAPS;



		// Check usage and pool ---------------------------------
		DWORD Usage = 0;
		D3DPOOL Pool = D3DPOOL_DEFAULT;

		if (flags&OAPISURFACE_SYSMEM) Pool = D3DPOOL_SYSTEMMEM, flags |= OAPISURFACE_TEXTURE;
		else {
			if (flags&OAPISURFACE_RENDERTARGET) Usage = D3DUSAGE_RENDERTARGET;
			else if ((flags&OAPISURFACE_GDI) && (flags&OAPISURFACE_TEXTURE)) Usage = D3DUSAGE_DYNAMIC;
		}


		// Mipmaps ----------------------------------------------
		DWORD Mips = info.MipLevels;
		if (Mips>1) Mips=0;
		if (flags&OAPISURFACE_NOMIPMAPS) Mips = 1;
		if (flags&OAPISURFACE_MIPMAPS) Mips = 0;


		// Decompress -------------------------------------------
		D3DFORMAT Format = info.Format;
		if (flags&OAPISURFACE_UNCOMPRESS) {
			if (Format==D3DFMT_DXT5) Format = D3DFMT_A8R8G8B8;
			if (Format==D3DFMT_DXT3) Format = D3DFMT_A8R8G8B8;
			if (Format==D3DFMT_DXT1) Format = D3DFMT_X8R8G8B8;
		}


		// Alpha ------------------------------------------------
		if (flags&OAPISURFACE_ALPHA) Format = D3DFMT_A8R8G8B8;
		if (flags&OAPISURFACE_NOALPHA) Format = D3DFMT_X8R8G8B8;


		// Convert Non-supported format -------------------------
		if (Format==D3DFMT_A4R4G4B4) Format = D3DFMT_A8R8G8B8;


		// Check Autogen status ---------------------------------
		if ((Mips != 1) && (flags&OAPISURFACE_UNCOMPRESS) && (flags&OAPISURFACE_TEXTURE)) Usage |= D3DUSAGE_AUTOGENMIPMAP;
		

		// Load Texture Section ====================================================================================================================
		//
		if (flags&OAPISURFACE_TEXTURE) {

			if (flags&OAPISURFACE_RENDERTARGET) {

				LPDIRECT3DTEXTURE9 pSys = NULL;
				LPDIRECT3DSURFACE9 pTemp = NULL;

				HR(pDevice->CreateTexture(info.Width, info.Height, Mips, Usage, Format, Pool, &pTex, NULL));

				if (pTex) {
					HR(D3DXCreateTextureFromFileExA(pDevice, path, info.Width, info.Height, 0, 0, Format, D3DPOOL_SYSTEMMEM, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pSys));
					if (pSys) {
						HR(pTex->GetSurfaceLevel(0, &pSurf));
						HR(pSys->GetSurfaceLevel(0, &pTemp));
						HR(pDevice->UpdateSurface(pTemp, NULL, pSurf, NULL));
						pTex->GenerateMipSubLevels();
						GetDesc(&desc);
						SetName(fname);
						SAFE_RELEASE(pTemp);
						SAFE_RELEASE(pSys);
						if (!bDecompress) LogBlu("Surface %s found. Handle=0x%X, (%ux%u), MipMaps=%u, Flags=0x%X, Format=0x%X", fname, this, desc.Width, desc.Height, pTex->GetLevelCount(), flags, DWORD(Format));
						
						// Load special texture maps
						LoadSpecials(fname);

						return true;
					}
				}
			}
			else if (D3DXCreateTextureFromFileExA(pDevice, path, info.Width, info.Height, Mips, Usage, Format, Pool, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pTex)==S_OK) {
				SetName(fname);
				HR(pTex->GetSurfaceLevel(0, &pSurf));
				GetDesc(&desc);
				if (!bDecompress) LogBlu("Surface %s found. Handle=0x%X, (%ux%u), MipMaps=%u, Flags=0x%X, Format=0x%X",fname, this, desc.Width, desc.Height, pTex->GetLevelCount(), flags, DWORD(Format));
				
				// Load special texture maps
				LoadSpecials(fname);

				return true;
			}

			LogErr("Surface %s failed to load. InitialFlags=0x%X",fname,Initial);
			LogSpecs("Surface");
			assert(false);
			return false;
		}

		// Load Surface Section ====================================================================================================================
		//
		else {	

			bLockable = false;
			if (flags&OAPISURFACE_GDI) bLockable = true;

			HR(pDevice->CreateRenderTarget(info.Width, info.Height, Format, D3DMULTISAMPLE_NONE, 0, bLockable, &pSurf, NULL));
			
			if (pSurf) {
				if (D3DXCreateTextureFromFileExA(pDevice, path, info.Width, info.Height, 1, 0, Format, D3DPOOL_SYSTEMMEM, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pTex)==S_OK) {
					SetName(fname);
					LPDIRECT3DSURFACE9 pTemp;
					if (pTex) {
						HR(pTex->GetSurfaceLevel(0, &pTemp));
						HR(pDevice->UpdateSurface(pTemp, NULL, pSurf, NULL));
						SAFE_RELEASE(pTemp); SAFE_RELEASE(pTex);
						GetDesc(&desc);
						if (!bDecompress) LogBlu("Surface %s loaded. Handle=0x%X, (%ux%u), Flags=0x%X, Format=0x%X",fname, this, desc.Width, desc.Height, flags, DWORD(Format));
						
						// Load special texture maps
						LoadSpecials(fname);

						return true;
					}
				}
			}
			LogSpecs("Surface");
			assert(false);
			return false;
		}
	}

	LogWrn("Surface %s not found. Handle=0x%X", fname, this);
	SetName(fname);
	return false;
}


// Load a special texture -------------------------------------------------------------------------------------------------
//
//
bool D3D9ClientSurface::LoadSpecialTexture(const char *fname, const char *ext, int id)
{
	if (pMap[id]) return true;

	char name[128];
	CreateName(name, ARRAYSIZE(name), fname, ext);

	char path[MAX_PATH];
	if ( gc->TexturePath(name, path) ) {
		D3DXIMAGE_INFO info;
		pMap[id] = NULL;
		if (D3DXGetImageInfoFromFileA(path, &info) == S_OK) {
			DWORD Mips = D3DFMT_FROM_FILE;
			if (Config->TextureMips == 2) Mips = 0;                         // Autogen all
			if (Config->TextureMips == 1 && info.MipLevels == 1) Mips = 0;  // Autogen missing

			if (S_OK == D3DXCreateTextureFromFileExA(pDevice, path, info.Width, info.Height, Mips, 0, D3DFMT_FROM_FILE, D3DPOOL_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pMap[id])) {
				LogBlu("SpecialTexture %s Loaded, (%ux%u), MipMaps=%u, Format=0x%X", name, info.Width, info.Height, info.MipLevels, info.Format);
				bAdvanced = true;
				return true;
			}
			// ...failed to create texture from file
			pMap[id] = NULL;
		}
		LogErr("Failed to load image (%s)", name);
	}
	return false;
}



// Load a texture -------------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::LoadTexture(const char *fname)
{
	char path[MAX_PATH];

	SetName(fname);

	// Construct normal map name
	//
	if (gc->TexturePath(fname, path)) {

		// Get information about the file
		//
		D3DXIMAGE_INFO info;
		HR(D3DXGetImageInfoFromFile(path, &info));

		D3DFORMAT Format = info.Format;
		
		// Convert Non-supported format -------------------------
		if (Format==D3DFMT_A4R4G4B4) Format = D3DFMT_A8R8G8B8;

		// Diffuse Texture Section ====================================================================================================================
		//
		DWORD Mips = D3DFMT_FROM_FILE;
		if (Config->TextureMips == 2) Mips = 0;							 // Autogen all
		if (Config->TextureMips == 1 && info.MipLevels == 1) Mips = 0;	 // Autogen missing

		if (D3DXCreateTextureFromFileExA(pDevice, path, info.Width, info.Height, Mips, 0, Format, D3DPOOL_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pTex) == S_OK) {
			HR(pTex->GetSurfaceLevel(0, &pSurf));
			GetDesc(&desc);
			LogBlu("Texture %s Loaded. Handle=0x%X, (%ux%u), MipMaps=%u, Format=0x%X", fname, this, desc.Width, desc.Height, pTex->GetLevelCount(), DWORD(Format));

			// Load special texture maps
			LoadSpecials(fname);
			return true;
		}
		else {
			LogErr("Texture %s failed to load", fname);
			return false;
		}
	}

	LogWrn("Texture %s not found. Handle=0x%X",fname,this);
	return false;
}





// Load special texture maps -------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::LoadSpecials(const char *fname)
{
	char path[MAX_PATH];

	if (!Config->UseNormalMap) return;

	LPDIRECT3DTEXTURE9 pBumpMap = NULL;

	// Bump Map Section =======================================================================================================================
	//
	char bname[128] = {};

	CreateName(bname, ARRAYSIZE(bname), fname, "bump");

	if (gc->TexturePath(bname, path)) {
		D3DXIMAGE_INFO info;
		if (D3DXGetImageInfoFromFileA(path, &info) == S_OK) {
			if (D3DXCreateTextureFromFileExA(pDevice, path, info.Width, info.Height, 0, 0, D3DFMT_FROM_FILE, D3DPOOL_SYSTEMMEM, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pBumpMap) == S_OK) {
				if (D3DXCreateTexture(pDevice, info.Width, info.Height, 0, 0, D3DFMT_R8G8B8, D3DPOOL_DEFAULT, &pMap[MAP_NORMAL]) == S_OK) {
					DWORD Channel = D3DX_CHANNEL_RED;
					if (info.Format == D3DFMT_A8) Channel = D3DX_CHANNEL_ALPHA;
					if (info.Format == D3DFMT_L8) Channel = D3DX_CHANNEL_LUMINANCE;
					if (D3DXComputeNormalMap(pMap[MAP_NORMAL], pBumpMap, NULL, 0, Channel, float(Config->BumpAmp)) != S_OK) LogErr("BumpMap conversion Failed (%s)", bname);
					else bAdvanced = true;
				}
				pBumpMap->Release();
			}
			else {
				pMap[MAP_NORMAL] = NULL;
				LogErr("Failed to load image (%s)", bname);
			}
		}
	}

	// Special Map Section =======================================================================================================================
	//
	LoadSpecialTexture(fname, "metal", MAP_METALNESS);
	LoadSpecialTexture(fname, "rghn", MAP_ROUGHNESS);
	LoadSpecialTexture(fname, "norm", MAP_NORMAL);
	LoadSpecialTexture(fname, "emis", MAP_EMISSION);
	LoadSpecialTexture(fname, "transl", MAP_TRANSLUCENCE);
	LoadSpecialTexture(fname, "transm", MAP_TRANSMITTANCE);
	LoadSpecialTexture(fname, "refl", MAP_REFLECTION);
	LoadSpecialTexture(fname, "spec", MAP_SPECULAR);
	LoadSpecialTexture(fname, "heat", MAP_HEAT);
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::SaveSurface(const char *fname)
{
	/*
	LPDIRECT3DTEXTURE9 pSystem = NULL;
	LPDIRECT3DTEXTURE9 pTemp = NULL;
	LPDIRECT3DSURFACE9 pTempS = NULL;
	LPDIRECT3DSURFACE9 pSystemS = NULL;

	if (desc.Pool!=D3DPOOL_SYSTEMMEM) {
		if (Type==D3D9S_TEXTURE || Type==D3D9S_DYNAMIC) {
			HR(D3DXCreateTexture(pDevice, desc.Width, desc.Height, 1, D3DUSAGE_RENDERTARGET, desc.Format, D3DPOOL_DEFAULT, &pTemp));
			HR(pTemp->GetSurfaceLevel(0, &pTempS));
			HR(pDevice->StretchRect(pSurf, NULL, pTempS, NULL, D3DTEXF_POINT));
			HR(D3DXCreateTexture(pDevice, desc.Width, desc.Height, 1, 0, desc.Format, D3DPOOL_SYSTEMMEM, &pSystem));
			HR(pSystem->GetSurfaceLevel(0, &pSystemS));
			HR(pDevice->GetRenderTargetData(pTempS, pSystemS));
			HR(D3DXSaveSurfaceToFileA(fname, D3DXIFF_DDS, pSystemS, NULL, NULL));

			pSystemS->Release();
			pSystem->Release();
			pTempS->Release();
			pTemp->Release();
			return;
		}
		if (Type==D3D9S_RENDER) {
			HR(D3DXCreateTexture(pDevice, desc.Width, desc.Height, 1, 0, desc.Format, D3DPOOL_SYSTEMMEM, &pSystem));
			HR(pSystem->GetSurfaceLevel(0, &pSystemS));
			HR(pDevice->GetRenderTargetData(pSurf, pSystemS));
			HR(D3DXSaveSurfaceToFileA(fname, D3DXIFF_DDS, pSystemS, NULL, NULL));
			pSystemS->Release();
			pSystem->Release();
			return;
		}
	}
	else {
		HR(D3DXSaveSurfaceToFileA(fname, D3DXIFF_DDS, pSurf, NULL, NULL));
	}*/
}

// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::LogSpecs(char *xname)
{
	char buf[512];

	if (pSurf) {
		D3DSURFACE_DESC desc;
		GetDesc(&desc);
		LogErr("%s name is %s Handle=0x%X (%u,%u)",xname,name,this,desc.Width,desc.Height);
	}
	else LogErr("%s name is %s Handle=0x%X",xname,name,this);

	if (pTex) LogErr("%s Has a Texture Interface",xname);
	if (pSurf) LogErr("%s Has a Surface Interface",xname);
	if (pDCSub) LogErr("%s Has a SubSurface Interface",xname);
	if (pRTS) LogErr("%s Has a Rendering Interface",xname);
	if (bLockable) LogErr("%s Is Lockable",xname);

	if (pSurf) {
		if (desc.Pool==D3DPOOL_DEFAULT)   LogErr("%s is in a DefaultPool",xname);
		if (desc.Pool==D3DPOOL_SYSTEMMEM) LogErr("%s is in a SystemMemPool",xname);
		if (desc.Pool==D3DPOOL_MANAGED)   LogErr("%s is in a ManagedPool",xname);
		if (desc.Usage&D3DUSAGE_DYNAMIC)  LogErr("%s has DYNAMIC usage",xname);
		if (desc.Usage&D3DUSAGE_RENDERTARGET) LogErr("%s has RENDERTARGET usage",xname);
		if (desc.Usage==0 && pTex==NULL) LogErr("%s is a PLAIN surface",xname);
		LogErr("%s Format is %u",xname, desc.Format);
	}

	if (ColorKey!=0x0) LogErr("%s has a ColorKey",xname);
	if (pTex) LogErr("%s Has %u MipMaps",xname, pTex->GetLevelCount());
	if (bDCOpen) LogErr("%s Has Open DC",xname);

	strcpy_s(buf,512,"ActiveFlags( ");

	DWORD AF = GetAttribs();

	if (AF&OAPISURFACE_TEXTURE)		 strcat_s(buf,512,"OAPISURFACE_TEXTURE ");
	if (AF&OAPISURFACE_RENDERTARGET) strcat_s(buf,512,"OAPISURFACE_RENDERTARGET ");
	if (AF&OAPISURFACE_GDI)			 strcat_s(buf,512,"OAPISURFACE_GDI ");
	if (AF&OAPISURFACE_SKETCHPAD)	 strcat_s(buf,512,"OAPISURFACE_SKETCHPAD ");
	if (AF&OAPISURFACE_MIPMAPS)		 strcat_s(buf,512,"OAPISURFACE_MIPMAPS ");
	if (AF&OAPISURFACE_NOMIPMAPS)	 strcat_s(buf,512,"OAPISURFACE_NOMIPMAPS ");
	if (AF&OAPISURFACE_ALPHA)		 strcat_s(buf,512,"OAPISURFACE_ALPHA ");
	if (AF&OAPISURFACE_NOALPHA)		 strcat_s(buf,512,"OAPISURFACE_NOALPHA ");
	if (AF&OAPISURFACE_UNCOMPRESS)	 strcat_s(buf,512,"OAPISURFACE_UNCOMPRESS ");
	if (AF&OAPISURFACE_SYSMEM)		 strcat_s(buf,512,"OAPISURFACE_SYSMEM ");

	LogErr("%s)",buf);
}


// -----------------------------------------------------------------------------------------------
//
DWORD D3D9ClientSurface::GetMipMaps()
{
	if (pTex) return pTex->GetLevelCount();
	return 1;
}


// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::GetDesc(D3DSURFACE_DESC *pD)
{
	bCompressed = IsCompressed();
	if (pSurf->GetDesc(pD)==S_OK) return true; else return false;
	return false;
}

// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::IncRef()
{
	Refs++;
}

// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::Release()
{
	Refs--;
	if (Refs<=0) return true;
	return false;
}

// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::SetColorKey(DWORD ck)
{
	ColorKey = ck;
	ClrKey = D3DXCOLOR(ColorKey);
}

// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::IsGDISurface()
{
	if (desc.Pool==D3DPOOL_SYSTEMMEM) return true;
	if (desc.Usage&D3DUSAGE_DYNAMIC) return true;
	if (desc.Usage==0 && pTex==NULL) return true; // Plain surface
	if (bLockable) return true;
	return false;
}

// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::IsRenderTarget()
{
	if (bBackBuffer) return true;
	if (desc.Pool==D3DPOOL_DEFAULT && desc.Usage&D3DUSAGE_RENDERTARGET) return true;
	return false;
}

// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::Is3DRenderTarget()
{
	if (bBackBuffer) return true;
	if (desc.Pool==D3DPOOL_DEFAULT && desc.Usage&D3DUSAGE_RENDERTARGET && pStencil!=NULL) return true;
	return false;
}

// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::IsBackBuffer()
{
	if (bBackBuffer) return true;
	return false;
}

// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::IsCompressed()
{
	if (desc.Format==D3DFMT_DXT1) return true;
	if (desc.Format==D3DFMT_DXT3) return true;
	if (desc.Format==D3DFMT_DXT5) return true;
	return false;
}

// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::IsPowerOfTwo() const
{
	DWORD w = desc.Width, h = desc.Height;
	for (int i=0;i<14;i++) if ((w&1)==0) w=w>>1; else { if (w!=1) return false; else break; }
	for (int i=0;i<14;i++) if ((h&1)==0) h=h>>1; else { if (h!=1) return false; else break; }
	return true;
}

// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::SetName(const char *n)
{
	strcpy_s(name, 128, n);
	int i = -1;
	while (name[++i]!=0) if (name[i]=='/') name[i]='\\';
}

// -----------------------------------------------------------------------------------------------
//
DWORD D3D9ClientSurface::GetWidth()
{
	return desc.Width;
}

// -----------------------------------------------------------------------------------------------
//
DWORD D3D9ClientSurface::GetHeight()
{
	return desc.Height;
}

// -----------------------------------------------------------------------------------------------
//
DWORD D3D9ClientSurface::GetAttribs(int What)
{
	if (What==0) return Initial;
	if (What==1) return Active;

	DWORD Cur = 0;
	if (pTex) Cur |= OAPISURFACE_TEXTURE;
	if (desc.Usage&D3DUSAGE_RENDERTARGET) Cur |= OAPISURFACE_RENDERTARGET;
	if (desc.Pool==D3DPOOL_SYSTEMMEM) Cur |= (OAPISURFACE_SYSMEM|OAPISURFACE_GDI);
	if (desc.Format==D3DFMT_X8R8G8B8) Cur |= OAPISURFACE_NOALPHA;
	if (desc.Format==D3DFMT_A8R8G8B8) Cur |= OAPISURFACE_ALPHA;
	if (pStencil && desc.Usage&D3DUSAGE_RENDERTARGET) Cur |= OAPISURFACE_RENDER3D;
	if (bLockable) Cur |= OAPISURFACE_GDI;
	if (pDCSub) Cur |= OAPISURFACE_GDI;
	if (pTex) {
		if (pTex->GetLevelCount()>1) Cur |= OAPISURFACE_MIPMAPS;
		else						 Cur |= OAPISURFACE_NOMIPMAPS;
	}
	return Cur;
}

// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::GenerateMipMaps()
{
	if (desc.Usage&D3DUSAGE_AUTOGENMIPMAP) {
		pTex->GenerateMipSubLevels();
		return true;
	}
	else {

		DWORD mips = GetMipMaps();

		if (desc.Usage&D3DUSAGE_RENDERTARGET && pTex && mips>1) {

			LPDIRECT3DSURFACE9 pSrf = pSurf;
			LPDIRECT3DSURFACE9 pMip = NULL;

			for (DWORD i=1;i<mips;i++) {
				pTex->GetSurfaceLevel(i, &pMip);
				if (pMip) {
					HR(pDevice->StretchRect(pSrf, NULL, pMip, NULL, D3DTEXF_LINEAR));
					if (pSrf!=pSurf) SAFE_RELEASE(pSrf);
					pSrf = pMip;	
				}
				else break;
			}

			if (pSrf!=pSurf) SAFE_RELEASE(pSrf);

			return true;
		}
	}
	return false;
}

// -----------------------------------------------------------------------------------------------
//
LPDIRECT3DTEXTURE9 D3D9ClientSurface::GetTexture()
{
	_TRACE;

	if (!Exists()) {
		ConvertToTexture(true);
		LogErr("Texture used without being initialized 0x%X", this);
	}

	if (pTex==NULL) ConvertToRenderTargetTexture();
	if (pTex==NULL) {
		LogErr("D3D9ClientSurface::GetTexture() Failed 0x%X", this);
		LogSpecs("Surface");
		assert(false);
		return NULL;
	}
	return pTex;
}

// -----------------------------------------------------------------------------------------------
//
LPDIRECT3DSURFACE9 D3D9ClientSurface::GetDepthStencil()
{
	return pStencil;
}

// -----------------------------------------------------------------------------------------------
//
LPDIRECT3DSURFACE9 D3D9ClientSurface::GetSurface()
{
	return pSurf;
}

// -----------------------------------------------------------------------------------------------
//
DWORD D3D9ClientSurface::GetTextureSizeInBytes(LPDIRECT3DTEXTURE9 pT)
{
	if (!pT) return 0;
	D3DSURFACE_DESC d; pT->GetLevelDesc(0,&d);
	DWORD size = GetSizeInBytes(d.Format, d.Height * d.Width);
	if (pTex->GetLevelCount() > 1) size += ((size>>2) + (size>>4) + (size>>8));
	return size;
}

// -----------------------------------------------------------------------------------------------
//
DWORD D3D9ClientSurface::GetSizeInBytes(D3DFORMAT Format, DWORD pixels)
{
	if (Format==D3DFMT_DXT1) return pixels>>1;
	if (Format==D3DFMT_DXT3) return pixels;
	if (Format==D3DFMT_DXT5) return pixels;
	if (Format==D3DFMT_A8R8G8B8) return pixels<<2;
	if (Format==D3DFMT_X8R8G8B8) return pixels<<2;
	if (Format==D3DFMT_R5G6B5) return pixels<<1;
	if (Format==D3DFMT_A4R4G4B4) return pixels<<1;
	if (Format==D3DFMT_R8G8B8) return pixels*3;
	return pixels;
}


// -----------------------------------------------------------------------------------------------
//
DWORD D3D9ClientSurface::GetSizeInBytes()
{
	if (pTex==NULL) return GetSizeInBytes(desc.Format, desc.Height * desc.Width);
	DWORD size = GetTextureSizeInBytes(pTex);
	for (int i = 0; i < ARRAYSIZE(pMap);i++)  size += GetTextureSizeInBytes(pMap[i]);
	return size;
}


// -----------------------------------------------------------------------------------------------
//
D3D9Pad *D3D9ClientSurface::GetD3D9Pad()
{
	if (!IsRenderTarget()) {
		LogErr("Can't optain a Sketchpad to a non-render target surface 0x%X", this);
		return NULL;
	}
	if (!pSkp) pSkp = new D3D9Pad(this, "D3D9SurfacePad");
	return pSkp;
}




// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::GlobalExit()
{
	delete []Index;
	delete []pGPUVtx;
	SAFE_RELEASE(FX);
}

// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::D3D9TechInit(D3D9Client *_gc, LPDIRECT3DDEVICE9 pDev, const char *folder)
{
	LogAlw("----- Initialize D3D9ClientSurface Tech -----");

	gc = _gc;

	char name[256];
	sprintf_s(name,256,"Modules/%s/CKBlit.fx",folder);

	// Create the Effect from a .fx file.
	ID3DXBuffer* errors = 0;

	HR(D3DXCreateEffectFromFileA(pDev, name, 0, 0, 0, 0, &FX, &errors));

	if (errors) {
		LogErr("Effect Error: %s",(char*)errors->GetBufferPointer());
		MessageBoxA(0, (char*)errors->GetBufferPointer(), "CKBlit.fx Error", 0);
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	if (FX==0) {
		LogErr("Failed to create an Effect (%s)",name);
		MissingRuntimeError();
		return;
	}

	eFlush = FX->GetTechniqueByName("FlushTech");
	eTech  = FX->GetTechniqueByName("BlitTech");
	eSketch= FX->GetTechniqueByName("SketchTech");
	eRotate= FX->GetTechniqueByName("RotateTech");
	eVP    = FX->GetParameterByName(0,"gVP");
	eTex0  = FX->GetParameterByName(0,"gTex0");
	eColor = FX->GetParameterByName(0,"gColor");
	eSize  = FX->GetParameterByName(0,"gSize");
	eKey   = FX->GetParameterByName(0,"gKey");

	HR(FX->SetTechnique(eTech));

	WORD xIndex[6] = {0,2,1,0,3,2};

	Index = new WORD[17*6];
	pGPUVtx = new GPUBLITVTX[17*4];

	int z=0, x=0;
	for (int i=0;i<16;i++) {
		for (int k=0;k<6;k++) Index[x+k] = xIndex[k]+z;
		z+=4;
		x+=6;
	}
}
