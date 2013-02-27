
// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2012 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

#define STRICT

#include "D3D9Surface.h"
#include "D3D9Client.h"
#include "D3D9Config.h"
#include "D3D9Catalog.h"
#include "D3D9Util.h"
#include "Log.h"

using namespace oapi;

ID3DXEffect* D3D9ClientSurface::FX = 0;			
D3DXHANDLE   D3D9ClientSurface::eTech = 0;	
D3DXHANDLE   D3D9ClientSurface::eFlush = 0;	
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

bool bX = false;


HRESULT D3D9ClientSurface::AddQueue(D3D9ClientSurface *src, LPRECT s, LPRECT t)
{

	if ((pPrevSrc!=NULL && pPrevSrc!=src) || GPUBltIdx>60) {
		HRESULT hr = FlushQueue(); if (hr!=S_OK) return hr;
	}

	pGPUVtx[GPUBltIdx].sx = short(s->left); 
	pGPUVtx[GPUBltIdx].sy = short(s->top);
	pGPUVtx[GPUBltIdx].tx = short(t->left);
	pGPUVtx[GPUBltIdx].ty = short(t->top);
	GPUBltIdx++;

	pGPUVtx[GPUBltIdx].sx = short(s->left); 
	pGPUVtx[GPUBltIdx].sy = short(s->bottom);
	pGPUVtx[GPUBltIdx].tx = short(t->left);
	pGPUVtx[GPUBltIdx].ty = short(t->bottom);
	GPUBltIdx++;

	pGPUVtx[GPUBltIdx].sx = short(s->right); 
	pGPUVtx[GPUBltIdx].sy = short(s->bottom);
	pGPUVtx[GPUBltIdx].tx = short(t->right);
	pGPUVtx[GPUBltIdx].ty = short(t->bottom);
	GPUBltIdx++;

	pGPUVtx[GPUBltIdx].sx = short(s->right); 
	pGPUVtx[GPUBltIdx].sy = short(s->top);
	pGPUVtx[GPUBltIdx].tx = short(t->right);
	pGPUVtx[GPUBltIdx].ty = short(t->top);
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
	gc->GetStats()->Draw++;
	gc->GetStats()->ColorKey += (GPUBltIdx>>2);

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
	gc->GetStats()->Draw++;
	gc->GetStats()->ColorKey++;
	
	FX->EndPass();
	FX->End();	

	return S_OK;
}


// -----------------------------------------------------------------------------------------------
//
HRESULT D3D9ClientSurface::GPUCopyTemp(LPRECT s, LPRECT t)
{
	// ATTENTION:  Must use texture address mode CLAMP

	pDevice->SetVertexDeclaration(pPosTexDecl);

	FX->SetTechnique(eTech);
	FX->SetMatrix(eVP, pVP);
	FX->SetValue(eColor, &ClrKey, sizeof(D3DXCOLOR));
	FX->SetTexture(eTex0, pTempTex);
	
	D3DSURFACE_DESC ds;
	pTemp->GetDesc(&ds);

	float srw = 1.0f / float(ds.Width);
	float srh = 1.0f / float(ds.Height);
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
	gc->GetStats()->Draw++;
	gc->GetStats()->ColorKey++;
	
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

	LogOk("ViewPort Setup 0x%X",this);
}


// -----------------------------------------------------------------------------------------------
//
D3D9ClientSurface::D3D9ClientSurface(LPDIRECT3DDEVICE9 pDev)
{
	Clear();	
	strcpy_s(name,64,"???");
	pDevice = pDev;
	SurfaceCatalog->Add(DWORD(this));
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::Clear()
{
	Refs		= 1;
	ColorKey	= 0;
	ClrKey		= D3DXCOLOR(ColorKey);
	pVP			= NULL;
	pViewPort	= NULL;
	pTex		= NULL;
	pSurf		= NULL;
	pTemp		= NULL;
	pTempTex    = NULL;
	pRTS		= NULL;
	pDCTemp		= NULL;
	pNormalMap	= NULL;
	pEmissionMap = NULL;
	pSpecularMap = NULL;
	pReflectionMap = NULL;
	Type		= 0;
	iBindCount  = 0;
	Initial		= 0;
	bDCOpen		= false;
	bNoGDI		= false;
	bClear		= false;
	bDC			= false;
	bSkpGetDCEr	= false;
	bSkpGetDC   = false;
	bBltGroup   = false;
	cClear		= 0;
	pDevice		= NULL;
	Creation	= 0;
	gNormalType = 0;
	memset(&desc, 0, sizeof(D3DSURFACE_DESC));
}


// -----------------------------------------------------------------------------------------------
//
D3D9ClientSurface::~D3D9ClientSurface()
{
	if (SurfaceCatalog->Remove(DWORD(this))==false) {
		LogErr("Surface 0x%X wasn't in the catalog",this);
	}

	if (Creation==D3D9C_BACKBUF) {
		SAFE_RELEASE(pSurf);
		SAFE_DELETE(pVP);
		SAFE_DELETE(pViewPort);
		return;
	}
	
	LogBlu("Deleting Surface 0x%X (%s) (%u,%u)...",this,name,desc.Width,desc.Height);

	if (pSurf) {
		//if ((n=pSurf->Release())!=0) LogWrn("Plain Surface(0x%X) failed to release %u instance(s) remains",this,n);
		//else pSurf = NULL;
		pSurf->Release();
		pSurf = NULL;
	}

	if (pTex) {
		//if ((n=pTex->Release())!=0) LogWrn("Texture Surface(0x%X) (%u,%u) failed to release %u instance(s) remains",this,desc.Width,desc.Height,n);
		//else pTex = NULL;
		pTex->Release();
		pTex = NULL;
	}

	SAFE_RELEASE(pTemp);
	SAFE_RELEASE(pTempTex);
	
	SAFE_RELEASE(pNormalMap);
	SAFE_RELEASE(pEmissionMap);
	SAFE_RELEASE(pSpecularMap);
	SAFE_RELEASE(pDCTemp);
	SAFE_RELEASE(pRTS);
	SAFE_DELETE(pVP);
	SAFE_DELETE(pViewPort);
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

	if ((desc.Usage&D3DUSAGE_RENDERTARGET)==0) return false;

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
		LogErr("D3D9ClientSurface::BindGPU() Failed");
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
void D3D9ClientSurface::MakeTextureEx(UINT Width, UINT Height, DWORD Usage, D3DFORMAT Format, D3DPOOL Pool)
{
	Type = D3D9S_TEXTURE;
	
	if (Width==0 || Height==0) { LogErr("Trying to create a texture with zero size Handle=0x%X",this); return;	}
	if (Width>8192 || Height>8192) { LogErr("Large surface created Handle=0x%X (%u,%u)", this, Width, Height);	return;	}

	HR(pDevice->CreateTexture(Width, Height, 1, Usage|D3DUSAGE_AUTOGENMIPMAP, Format, Pool, &pTex, NULL));
	if (pTex) pTex->GetSurfaceLevel(0, &pSurf);

	GetDesc(&desc);
	LogBlu("D3D9ClientSurface: New Texture(0x%X) w=%u, h=%u", this, Width, Height);

	if (desc.Width!=Width || desc.Height!=Height) LogErr("^^Requested surface size and allocation size doesn't match Allocated(%u,%u)^^",desc.Width,desc.Height);
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::MakeSurfaceEx(UINT Width, UINT Height, D3DFORMAT Format, D3DPOOL Pool)
{
	if (Width==0 || Height==0) { LogWrn("Trying to create a surface with zero size Handle=0x%X",this); return; }
	if (Width>8192 || Height>8192) { LogErr("Large surface created Handle=0x%X (%u,%u)", this, Width, Height); return; }

	pTex = NULL;
	pSurf = NULL;
	Type = D3D9S_PLAIN;

	HR(pDevice->CreateOffscreenPlainSurface(Width, Height, Format, Pool, &pSurf, NULL));
	
	LogBlu("D3D9ClientSurface: New Surface(0x%X) w=%u, h=%u",this,Width,Height);
	GetDesc(&desc);
	
	if (desc.Width!=Width || desc.Height!=Height) LogErr("^^Requested surface size and allocation size doesn't match Allocated(%u,%u)^^",desc.Width,desc.Height);
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::MakeRenderTargetEx(UINT Width, UINT Height, bool bTexture, bool bLock, D3DFORMAT Format)
{
	if (Width==0 || Height==0) { LogWrn("Trying to create a surface with zero size Handle=0x%X",this); return; }
	if (Width>8192 || Height>8192) { LogErr("Large surface created Handle=0x%X (%u,%u)", this, Width, Height); return; }

	pTex = NULL;
	pSurf = NULL;
	Type = D3D9S_RTGTTEX;

	//HR(pDevice->CreateRenderTarget(Width, Height, Format, D3DMULTISAMPLE_NONE, 0, bLock, &pSurf, NULL));
	HR(D3DXCreateTexture(pDevice, Width, Height, 1, D3DUSAGE_RENDERTARGET, Format, D3DPOOL_DEFAULT, &pTex));
	if (pTex) pTex->GetSurfaceLevel(0, &pSurf);
	LogBlu("D3D9ClientSurface: New RenderTarget(0x%X) w=%u, h=%u",this,Width,Height);
	GetDesc(&desc);
	SetupViewPort();

	if (desc.Width!=Width || desc.Height!=Height) LogErr("^^Requested surface size and allocation size doesn't match Allocated(%u,%u)^^",desc.Width,desc.Height);
}

void D3D9ClientSurface::Make3DRenderTarget(UINT Width, UINT Height, UINT flags)
{
	if (Width==0 || Height==0) { LogWrn("Trying to create a surface with zero size Handle=0x%X",this); return; }
	if (Width>8192 || Height>8192) { LogErr("Large surface created Handle=0x%X (%u,%u)", this, Width, Height); return; }

	pTex = NULL;
	pSurf = NULL;
	Type = D3D9S_RTGTTEX;

	/*
	HR(pDevice->CreateRenderTarget(Width, Height, Format, D3DMULTISAMPLE_NONE, 0, bLock, &pSurf, NULL));
	HR(D3DXCreateTexture(pDevice, Width, Height, 1, D3DUSAGE_RENDERTARGET, Format, D3DPOOL_DEFAULT, &pTex));
	if (pTex) pTex->GetSurfaceLevel(0, &pSurf);
	LogBlu("D3D9ClientSurface: New RenderTarget(0x%X) w=%u, h=%u",this,Width,Height);
	*/
	GetDesc(&desc);
	SetupViewPort();

	if (desc.Width!=Width || desc.Height!=Height) LogErr("^^Requested surface size and allocation size doesn't match Allocated(%u,%u)^^",desc.Width,desc.Height);
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
	Type = D3D9S_RTGTTEX;
	Creation = D3D9C_BACKBUF;
	GetDesc(&desc);
	SetupViewPort();
}

// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::MakeTexture(UINT Width, UINT Height, D3DFORMAT Format)
{
	MakeTextureEx(Width, Height, D3DUSAGE_DYNAMIC | D3DUSAGE_AUTOGENMIPMAP, Format, D3DPOOL_DEFAULT);
}

// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::MakeRenderingTexture(UINT Width, UINT Height, D3DFORMAT Format)
{
	MakeRenderTargetEx(Width, Height, true, false, Format);
}

// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::MakePlainSurface(UINT Width, UINT Height, D3DPOOL Pool)
{
	MakeSurfaceEx(Width, Height, D3DFMT_X8R8G8B8, Pool);
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::BringToSystemMem()
{
	if (Type==D3D9S_PLAIN && Creation==D3D9C_SURFACE && bNoGDI==false) {
		LPDIRECT3DSURFACE9 pNew;
		//HR(pDevice->CreateOffscreenPlainSurface(desc.Width, desc.Height, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT, &pNew, NULL));
		HR(pDevice->CreateOffscreenPlainSurface(desc.Width, desc.Height, D3DFMT_X8R8G8B8, D3DPOOL_SYSTEMMEM, &pNew, NULL));
		HR(D3DXLoadSurfaceFromSurface(pNew, NULL, NULL, pSurf, NULL, NULL, D3DX_DEFAULT, 0));
		SAFE_RELEASE(pSurf);
		SAFE_RELEASE(pTex);
		Type = D3D9S_PLAIN;
		pTex = NULL;
		pSurf = pNew;
		GetDesc(&desc);
		LogBlu("Surface 0x%X (%s) converted into a GDI surface (%u,%u)",this,name,desc.Width,desc.Height);
	}
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::ConvertToPlainSurface()
{
	
}

// -----------------------------------------------------------------------------------------------
// This function will reload the surface with decompression
//
void D3D9ClientSurface::Decompress()
{
	/*
	D3DFORMAT Format;
	LPDIRECT3DSURFACE9 pTgt = NULL;
	LPDIRECT3DTEXTURE9 pNew = NULL;

	if (desc.Format==D3DFMT_DXT5) Format = D3DFMT_A8R8G8B8;
	if (desc.Format==D3DFMT_DXT3) Format = D3DFMT_X8R8G8B8;
	if (desc.Format==D3DFMT_DXT1) Format = D3DFMT_X8R8G8B8;

	HR(D3DXCreateTexture(pDevice, desc.Width, desc.Height, 1, 0, Format, D3DPOOL_SYSTEMMEM, &pNew));
	pNew->GetSurfaceLevel(0, &pTgt);
	HR(D3DXLoadSurfaceFromSurface(pTgt, NULL, NULL, pSurf, NULL, NULL, D3DX_FILTER_POINT, NULL));

	SAFE_RELEASE(pSurf);
	SAFE_RELEASE(pTex);

	pSurf = pTgt;
	pTex = pNew;

	GetDesc(&desc);
	*/

	SAFE_RELEASE(pSurf);
	SAFE_RELEASE(pTex);
	
	// Must reload the file because it's initially loaded in default pool
	//
	if (LoadTexture(name, Initial|0x2)) {
		LogBlu("Texture 0x%X (%s) decompressed",this,name);
		return;
	}

	LogErr("Failed to decompress surface");
	LogSpecs("Surface");
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::ConvertToRenderTargetTexture()
{
	if (Type==D3D9S_RTGTTEX) return;

	LPDIRECT3DSURFACE9 pTgt;
	LPDIRECT3DTEXTURE9 pNew;

	HR(D3DXCreateTexture(pDevice, desc.Width, desc.Height, 1, D3DUSAGE_RENDERTARGET, desc.Format, D3DPOOL_DEFAULT, &pNew));
	HR(pNew->GetSurfaceLevel(0,&pTgt));

	if (desc.Pool==D3DPOOL_SYSTEMMEM && Type==D3D9S_TEXTURE) {
		HR(D3DXLoadSurfaceFromSurface(pTgt, NULL, NULL, pSurf, NULL, NULL, D3DX_DEFAULT, 0));	
	}
	else {

		if (desc.Pool==D3DPOOL_DEFAULT) {
			if (pDevice->StretchRect(pSurf, NULL, pTgt, NULL, D3DTEXF_LINEAR)!=S_OK) {
				LogErr("StretchRect failed in ConvertToRenderTargetTexture()");
				LogSpecs("Surface");
				return;
			}
		}

		if (desc.Pool==D3DPOOL_SYSTEMMEM) {
			if (pDevice->UpdateSurface(pSurf, NULL, pTgt, NULL)!=S_OK) {
				LogErr("UpdateSurface failed in ConvertToRenderTargetTexture()");
				LogSpecs("Surface");
				return;
			}
		}
	}

	SAFE_RELEASE(pSurf);
	SAFE_RELEASE(pTex);

	Type  = D3D9S_RTGTTEX;
	pTex  = pNew;
	pSurf = pTgt;

	GetDesc(&desc);
	SetupViewPort();
	LogBlu("Surface 0x%X Converted to RenderTargetTexture (%u,%u)", this, desc.Width, desc.Height);
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::ConvertToDynamicTexture()
{
	if (Type==D3D9S_DYNAMIC) return;
	if (Type==D3D9S_PLAIN || desc.Pool==D3DPOOL_SYSTEMMEM) {

		LPDIRECT3DSURFACE9 pTgt;
		LPDIRECT3DTEXTURE9 pNew;

		HR(D3DXCreateTexture(pDevice, desc.Width, desc.Height, 1, D3DUSAGE_DYNAMIC, desc.Format, D3DPOOL_DEFAULT, &pNew));
		HR(pNew->GetSurfaceLevel(0,&pTgt));
		HR(D3DXLoadSurfaceFromSurface(pTgt, NULL, NULL, pSurf, NULL, NULL, D3DX_DEFAULT, 0));

		SAFE_RELEASE(pSurf);
		SAFE_RELEASE(pTex);

		Type = D3D9S_DYNAMIC;
		pTex = pNew;
		pSurf = pTgt;
		GetDesc(&desc);
		LogBlu("Surface 0x%X Converted to Dynamic Texture (%u,%u)",this,desc.Width,desc.Height);
	}

	if (Type==D3D9S_RTGTTEX) {
		LPDIRECT3DSURFACE9 pTgt;
		LPDIRECT3DTEXTURE9 pSys, pNew;

		HR(pDevice->CreateTexture(desc.Width, desc.Height, 1, 0, desc.Format, D3DPOOL_SYSTEMMEM, &pSys, NULL));
		HR(pDevice->CreateTexture(desc.Width, desc.Height, 1, D3DUSAGE_DYNAMIC, desc.Format, D3DPOOL_DEFAULT, &pNew, NULL));
		HR(pSys->GetSurfaceLevel(0, &pTgt));
		HR(pDevice->GetRenderTargetData(pSurf, pTgt));
		SAFE_RELEASE(pTgt);

		HR(pDevice->UpdateTexture(pSys, pNew));

		SAFE_RELEASE(pSurf);
		SAFE_RELEASE(pTex);
		SAFE_RELEASE(pSys);

		Type = D3D9S_DYNAMIC;
		pTex = pNew;
		HR(pTex->GetSurfaceLevel(0, &pSurf));
		GetDesc(&desc);
		LogBlu("-!- -!- Surface 0x%X Converted to Dynamic Texture (%u,%u)",this,desc.Width,desc.Height);
	}
}




void D3D9ClientSurface::CheckTemp(DWORD Width, DWORD Height)
{
	if (pTemp==NULL) {
		D3DXCreateTexture(pDevice, Width, Height, 1, D3DUSAGE_RENDERTARGET, desc.Format, D3DPOOL_DEFAULT, &pTempTex);
		pTempTex->GetSurfaceLevel(0, &pTemp);	
	}
	if (pTemp==NULL) {
		LogErr("CheckTemp Failed 2");	
		return;
	}

	D3DSURFACE_DESC ds;
	pTemp->GetDesc(&ds);

	if (ds.Width<Width || ds.Height<Height || ds.Format!=desc.Format) {
		SAFE_RELEASE(pTemp);
		SAFE_RELEASE(pTempTex);
		D3DXCreateTexture(pDevice, max(ds.Width,Width), max(ds.Height,Height), 1, D3DUSAGE_RENDERTARGET, desc.Format, D3DPOOL_DEFAULT, &pTempTex);
		pTempTex->GetSurfaceLevel(0, &pTemp);	
	}

	if (pTemp==NULL) LogErr("CheckTemp Failed");
}




// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::CopyRect(D3D9ClientSurface *src, LPRECT s, LPRECT t, UINT ck)
{
	_TRACER;

	bClear = false;
	bool bRestart = true;

	gc->GetStats()->Blits++;

	// Check failure and abort conditions -------------------------------------------------------
	//
	if (t->right > (long)desc.Width || t->bottom > (long)desc.Height) return;
	if (t->left < 0 || t->top < 0) return;
	if (s->right > (long)src->desc.Width || s->bottom > (long)src->desc.Height) return;
	if (s->left < 0 || s->top < 0) return;

	DWORD Width = s->right - s->left;
	DWORD Height = s->bottom - s->top;		
	DWORD TgtWidth = t->right - t->left;
	DWORD TgtHeight = t->bottom - t->top;	

	if (Width==0 || Height==0 || TgtWidth==0 || TgtHeight==0) return;

	// Is scaling operation required -------------------------------------------------------------
	//
	bool bStretch = true;
	if (Width==TgtWidth && Height==TgtHeight) bStretch = false;

	// If compressed surface is involved in blitting then decompress ------------------------------
	//
	if (IsCompressed()) Decompress();
	if (src->IsCompressed()) src->Decompress();



	// Special in-surface blitting workaround =====================================================
	//
	//
	if (src == this) {
		
		if (Type==D3D9S_TEXTURE || Type==D3D9S_DYNAMIC) ConvertToRenderTargetTexture();

		if (bStretch) {
			LogErr("Stretching in in-surface blitting");
			goto error_report;
		}

		CheckTemp(Width, Height);

		if (pTemp) {

			RECT dr; 
			dr.left   = 0; 
			dr.top    = 0;
			dr.right  = Width;
			dr.bottom = Height;

			if (pDevice->StretchRect(pSurf, s, pTemp, &dr, D3DTEXF_POINT)==S_OK) {	

				//if (ColorKey==0x0) {
					if (pDevice->StretchRect(pTemp, &dr, pSurf, t, D3DTEXF_POINT)==S_OK) {	
						return;
					}	
				/*}
				else {
					if (BindGPU()) {
						GPUCopyTemp(&dr, t);
						ReleaseGPU();
						return;
					}
				}*/
			}	
		}
		LogErr("InSurface StretchRect Blitting Failed 0x%X",pSurf);
		goto error_report;
	}



	// ========================================================================================================================================
	// Blit a Surface using primary GPU
	//

	if (FX) {

		// Special =====================================================================================
		//
		if (Creation==D3D9C_BACKBUF) {
			if (src->desc.Format == D3DFMT_R32F) {
				GPUCopyRect(src, s, t);
				return;
			}

			if (src->desc.Format == D3DFMT_A32B32G32R32F) {
				GPUCopyRect(src, s, t);
				return;
			}
		}


		// Special Handler for colorkeyed blitting ==================================================
		//
		if (src->ColorKey!=0x0) {
			
			if (Creation==D3D9C_BACKBUF) {

				// Prepare source for blitting
				if (src->pTex==NULL) {
					if (src->bDC) src->ConvertToDynamicTexture();
					else          src->ConvertToRenderTargetTexture();
				}
				if (src->pTex) if (GPUCopyRect(src, s, t)==S_OK) {
					LogOk("GPU ColorKey Blitting 0x%X (%s) -> 0x%X (%s) (%u,%u)", src, src->name, this, name, Width, Height);
					return;
				}
			}
			else {

				// Prepare Target Surface
				//
				if (IsRenderTarget()==false) ConvertToRenderTargetTexture();
				
				// Prepare Source Surface
				//
				if (src->pTex==NULL) {	
					if (src->bDC) src->ConvertToDynamicTexture();
					else		  src->ConvertToRenderTargetTexture();
				}

				if (IsRenderTarget() && src->pTex) {
					if (BindGPU()) {
						GPUCopyRect(src, s, t);
						ReleaseGPU();
						LogOk("GPU ColorKey_2 Blitting 0x%X (%s) -> 0x%X (%s) (%u,%u)", src, src->name, this, name, Width, Height);
						return;
					}
				}
			}
		}
	}




	// ====================================================================================================
	// Blit Surface using UpdateSurfaces() function. This won't work with multisampled surfaces
	//

	if (src->desc.Pool==D3DPOOL_SYSTEMMEM) {

		if (desc.Pool==D3DPOOL_DEFAULT && src->desc.Format==desc.Format && !bStretch) {
		
			POINT p; p.x = t->left, p.y=t->top;
		
			if (pDevice->UpdateSurface(src->pSurf, s, pSurf, &p)==S_OK) {
				LogOk("UpdateSurface 0x%X (%s) -> 0x%X (%s) (%u,%u)", src, src->name, this, name, Width, Height);
				return;
			}
			else {

				// This code section is required by old style 2D-Panel MFDs when a 4x AA is enabled. (AA will cause UpdateSurface() to Fail)
				//
				if (Creation==D3D9C_BACKBUF) {
					src->ConvertToDynamicTexture();	// Bring a system memory surface into a video memory
				}
				else {
					LogErr("UpdateSurface Failed");
					goto error_report;
				}
			}	
		}
	}


restart_blit:


	// =====================================================================================================
	// StretchRect Blitting Technique 
	//

	if (desc.Pool==D3DPOOL_DEFAULT && src->desc.Pool==D3DPOOL_DEFAULT) {

		// Check the ability to use StretchRect()
		//
		bool bStretchBlit = false;
		if (desc.Usage==D3DUSAGE_RENDERTARGET) bStretchBlit = true;
		else if (Type==D3D9S_PLAIN && src->Type==D3D9S_PLAIN && bStretch==false) bStretchBlit = true;
		
		if (bStretchBlit) {
			if (pDevice->StretchRect(src->pSurf, s, pSurf, t, D3DTEXF_POINT)==S_OK) {	
				LogOk("StretchRect 0x%X (%s) -> 0x%X (%s) (%u,%u)", src, src->name, this, name, Width, Height);
				return;
			}		
			LogErr("StretchRect Blitting Failed 0x%X (%s) -> 0x%X (%s)", src, src->name, this, name);
			goto error_report;
		}
	}

	// =====================================================================================================
	// Fall back in GDI Blitting Techniques 
	//

	if (src->desc.Pool==D3DPOOL_SYSTEMMEM && desc.Pool==D3DPOOL_SYSTEMMEM) {
		HDC hSrc = src->GetDCHard();
		HDC hTgt = GetDCHard();
		if (hSrc && hTgt) StretchBlt(hTgt, t->left, t->top, TgtWidth, TgtHeight, hSrc, s->left, s->top, Width, Height, SRCCOPY);
		else { LogErr("GDI BLITTING FAILED"); goto error_report; }
		if (hTgt) this->ReleaseDC(hTgt);
		if (hSrc) src->ReleaseDC(hSrc);
		return;
	}

	if (Type==D3D9S_PLAIN && src->Type==D3D9S_PLAIN && bStretch==true) {
		HDC hSrc = src->GetDCHard();
		HDC hTgt = GetDCHard();
		if (hSrc && hTgt) StretchBlt(hTgt, t->left, t->top, TgtWidth, TgtHeight, hSrc, s->left, s->top, Width, Height, SRCCOPY);
		if (hTgt) this->ReleaseDC(hTgt);
		if (hSrc) src->ReleaseDC(hSrc);
		return;
	}

	
	// Unable to perform blitting ----------------------------------------------------------------------------------------
	//
	LogWrn("No Fitting Blitting Routine Exists... for 0x%X -> 0x%X blit. Converting...",src,this);

	if (Type==D3D9S_PLAIN && desc.Pool==D3DPOOL_SYSTEMMEM && src->desc.Pool==D3DPOOL_DEFAULT) {
		src->BringToSystemMem();   
		if (bRestart) { bRestart=false; goto restart_blit; } 
	}
	else {
		ConvertToRenderTargetTexture();
		if (bRestart) { bRestart=false; goto restart_blit; } 
	}

error_report:

	_SETLOG(5);
	LogErr("D3D9ClientSurface::CopyRect() Failed");
	LogMsg("Source Rect (%d,%d,%d,%d) (w=%u,h=%u) HANDLE=0x%X (%s)",s->left,s->top,s->right,s->bottom, abs(s->left-s->right), abs(s->top-s->bottom), src, src->name);
	LogMsg("Target Rect (%d,%d,%d,%d) (w=%u,h=%u) HANDLE=0x%X (%s)",t->left,t->top,t->right,t->bottom, abs(t->left-t->right), abs(t->top-t->bottom), this, name);
	LogSpecs("Target");
	src->LogSpecs("Source");
	_POPLOG;
}


// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::Fill(LPRECT rect, DWORD c)
{
	_TRACER;
	bClear = false;

	DWORD color = 0;
	LPRECT r;
	RECT re;
	
	if (rect==NULL) { re.left=0, re.top=0, re.right=desc.Width, re.bottom=desc.Height; r=&re; }
	else r = rect;

	if (desc.Pool==D3DPOOL_SYSTEMMEM || desc.Usage&D3DUSAGE_DYNAMIC) {
		
		HDC hDC = GetDCHard();

		if (hDC) {		
			color = RGB((c>>16)&0xFF, (c>>8)&0xFF, c&0xFF);
			HBRUSH hBrush = CreateSolidBrush((COLORREF)color);
			HGDIOBJ hOld = SelectObject(hDC, hBrush);
			Rectangle(hDC,r->left,r->top,r->right,r->bottom);
			SelectObject(hDC, hOld);
			DeleteObject(hBrush);
			ReleaseDC(hDC);
			return true;
		}
		else {
			LogErr("GDI Fill Failed");
			LogSpecs("Surface");
			return false;
		}
	}

	if (desc.Pool==D3DPOOL_DEFAULT && (Type==D3D9S_PLAIN || Type==D3D9S_RTGTTEX)) { 
		if (pDevice->ColorFill(pSurf, r, c)!=S_OK) {
			LogErr("GPU ColorFill Failed");
			LogSpecs("Surface");
			return false;
		}
		LogOk("ColorFill 0x%X (%s) (%u,%u)", this, name, (r->right-r->left), (r->bottom-r->top));	
		return true;
	}

	return false;
}


// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::Clear(DWORD c)
{
	DWORD color = 0;

	if (desc.Pool==D3DPOOL_SYSTEMMEM || desc.Usage&D3DUSAGE_DYNAMIC) {
		
		HDC hDC = GetDCHard();

		if (hDC) {	
			color = RGB((c>>16)&0xFF, (c>>8)&0xFF, c&0xFF);
			HBRUSH hBrush = CreateSolidBrush((COLORREF)color);
			RECT r; r.left = 0; r.top = 0; r.right = desc.Width; r.bottom = desc.Height;
			FillRect(hDC, &r, hBrush);
			DeleteObject(hBrush);
			ReleaseDC(hDC);
			LogOk("Clear Surface GDI 0x%X (%s)(%u,%u)", this, name, desc.Width, desc.Height);
			bClear = true;
			cClear = c;
			return true;
		}
		else {
			LogErr("GDI Fill Failed");
			LogSpecs("Surface");
			return false;
		}
	}
	else {
		if (Type==D3D9S_TEXTURE) return false; 
		if (pDevice->ColorFill(pSurf, NULL, c)!=S_OK) {
			LogErr("GPU ColorFill Failed");
			LogSpecs("Surface");
			return false;
		}
		else {
			LogOk("Clear Surface 0x%X (%s)(%u,%u)", this, name, desc.Width, desc.Height);
			bClear = true;
			cClear = c;
		}
	}
	return true;
}

// -----------------------------------------------------------------------------------------------
// Only for internal use in D3D9ClientSurface
//
HDC	D3D9ClientSurface::GetDCHard()
{
	bHard = true;
	HDC hDC;
	if (iBindCount!=0) {
		LogErr("Surface bind count is %d in D3D9ClientSurface::GetDCHard()",iBindCount);
		return NULL;
	}
	if (pSurf->GetDC(&hDC)==S_OK) return hDC;
	LogErr("D3D9ClientSurface: GetDCHard() Failed");
	LogSpecs("Surface");
	return NULL;
}


// -----------------------------------------------------------------------------------------------
//
HDC	D3D9ClientSurface::GetDC()
{
	bHard = false;
	bDC = true;

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

	if (IsCompressed()) {
		LogErr("Compressed surface in GetDC()");
		return NULL;
	}

	bDCOpen = false;
	HDC hDC = NULL;

	// Attempting to acquire a hDC for a BackBuffer
	//
	if (Creation==D3D9C_BACKBUF) {
		if (pSurf->GetDC(&hDC)==S_OK) { bDCOpen=true; return hDC; }
		return NULL;
	}

	// Oh Shit... Acquiring a hDC for rendering target surface
	//
	if (Type==D3D9S_RTGTTEX) {	
	
		if (Config->RejectRTDC) {
			LogErr("[GDI access to render target 0x%X (%u,%u) rejected]", this, desc.Width, desc.Height);
			return NULL;
		}

		// Create a temporary system memory copy
		if (pDCTemp==NULL) pDevice->CreateOffscreenPlainSurface(desc.Width, desc.Height, desc.Format, D3DPOOL_SYSTEMMEM, &pDCTemp, NULL);
			
		if (pDCTemp) {

			// Download the render target into a system memory copy
			// If the bClear flag is set then the download can be skipped because the surface is filled with a single color "oClear"
			if (!bClear) pDevice->GetRenderTargetData(pSurf, pDCTemp);
				
			if (pDCTemp->GetDC(&hDC)==S_OK) {
				if (bClear) {  // Fill the system memory copy with a color
					HBRUSH hBrush = CreateSolidBrush(RGB((cClear>>16)&0xFF, (cClear>>8)&0xFF, cClear&0xFF));
					RECT r; r.left = 0; r.top = 0; r.right = desc.Width; r.bottom = desc.Height;
					FillRect(hDC, &r, hBrush);
					DeleteObject(hBrush);
				}
				bDCOpen = true;
				return hDC;
			}
			LogErr("pDCTemp->GetDC(&hDC)!=S_OK");
			return NULL;
		}
	}
	
	if (Type==D3D9S_PLAIN && Creation==D3D9C_SURFACE && desc.Pool==D3DPOOL_DEFAULT && bNoGDI==false) BringToSystemMem();	// THIS IS VITAL
	
	if (pSurf->GetDC(&hDC)==S_OK) { bDCOpen = true; return hDC; }
	
	LogErr("D3D9ClientSurface: GetDC() Failed");
	LogSpecs("Surface");
	return NULL;
}





// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::ReleaseDC(HDC hDC)
{
	bDCOpen = false;

	if (Creation==D3D9C_BACKBUF) {
		HR(pSurf->ReleaseDC(hDC)); 
		if (!bHard) bClear = false;
		return;
	}

	if (Type==D3D9S_RTGTTEX && pDCTemp) {
		HR(pDCTemp->ReleaseDC(hDC));
		if (Config->GDIRTSWrn && !bClear) {	// Identify RT-GDI conflict by flashing the surface
			bFlash = !bFlash;
			if (bFlash) pDevice->ColorFill(pSurf, NULL, 0xFFFF0000);
			else        pDevice->ColorFill(pSurf, NULL, 0xFFFFFF00);
		}
		// Upload the system memory copy of the render target back to video memory
		else HR(pDevice->UpdateSurface(pDCTemp, NULL, pSurf, NULL));
		if (!bHard) bClear = false;
		return;
	}

	if (!bHard) bClear = false;
	if (pSurf->ReleaseDC(hDC)==S_OK) return;  

	LogErr("ReleaseDC() Failed");
	LogSpecs("Surface");
}


// -----------------------------------------------------------------------------------------------
//
LPDIRECT3DTEXTURE9 D3D9ClientSurface::GetTextureHard()
{
	if (pTex==NULL) return NULL;
	if (desc.Pool==D3DPOOL_DEFAULT) return pTex;
	
	// Must bring the texture from a system memory to video memory. Create mipmaps in the process.
	//
	DWORD levels = pTex->GetLevelCount();
	LogBlu("Moving a texture 0x%X from POOL_SYSTEMEME to POOL_DEFAULT MipMapCount=%u", this, levels);
	
	LPDIRECT3DTEXTURE9 pNew;
	LPDIRECT3DSURFACE9 pTgt;

	int Mips=0;
	if (Initial&0x4) Mips=1;

	HR(D3DXCreateTexture(pDevice, desc.Width, desc.Height, Mips, desc.Usage|D3DUSAGE_AUTOGENMIPMAP, desc.Format, D3DPOOL_DEFAULT, &pNew));
	HR(pNew->SetAutoGenFilterType(D3DTEXF_ANISOTROPIC));
	HR(pNew->GetSurfaceLevel(0,&pTgt));
	HR(pDevice->UpdateSurface(pSurf, NULL, pTgt, NULL));

	pNew->GenerateMipSubLevels();	

	SAFE_RELEASE(pSurf);
	SAFE_RELEASE(pTex);
	
	pSurf = pTgt;
	pTex = pNew;	

	GetDesc(&desc);

	return pTex;
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


// Load a texture ------------------------------------------------------------------------------------------------- 
//
//
bool D3D9ClientSurface::LoadTexture(const char *fname, int flags)
{
	char cpath[256];
	char xpath[256];
	char nname[128];
	char sname[128];
	char ename[128];
	char bname[128];
	char rname[128];
	
	bClear = false;

	if (gc==NULL) {
		LogErr("D3D9ClientSurface::LoadTexture() No Client Pointer");
		return false;
	}

	// Construct normal map name
	//
	if (gc->TexturePath(fname, cpath)) {

		if (Config->UseNormalMap) {
			CreateName(nname, 128, fname, "norm");
			CreateName(sname, 128, fname, "spec");
			CreateName(ename, 128, fname, "emis");
			CreateName(bname, 128, fname, "bump");
			CreateName(rname, 128, fname, "refl");
		}
			
		// Get information about the file
		//
		D3DXIMAGE_INFO info;
		HR(D3DXGetImageInfoFromFile(cpath, &info));

		if (info.Height>8192 || info.Width>8192) LogErr("Loading a large surface Handle=0x%X (%u,%u)", this, info.Width, info.Height);

		D3DFORMAT Format = info.Format;
		D3DPOOL Pool = D3DPOOL_DEFAULT;
		DWORD Usage = 0;
		DWORD Mips = 0;

		if (Config->LoadInSystemMem) Pool = D3DPOOL_SYSTEMMEM;

		LPDIRECT3DTEXTURE9 pBumpMap = NULL;
		Initial = flags;
		Type = D3D9S_TEXTURE;

		// System Memory requested ------------------------------
		if (flags&0x1) {
			Pool = D3DPOOL_SYSTEMMEM;
		}

		// Decompress -------------------------------------------
		if (flags&0x2) {  
			if (Format==D3DFMT_DXT5) Format = D3DFMT_A8R8G8B8;
			if (Format==D3DFMT_DXT3) Format = D3DFMT_A8R8G8B8;
			if (Format==D3DFMT_DXT1) Format = D3DFMT_X8R8G8B8;
			Pool = D3DPOOL_SYSTEMMEM;
		}

		// No Mipmaps --------------------------------------------
		if (flags&0x4) {
			Mips = 1;
			Usage = 0;
		}

		if (Config->UseNormalMap) {

			// Bump Map Section =======================================================================================================================
			//
			if (gc->TexturePath(bname, xpath)) {
				D3DXIMAGE_INFO info;
				if (D3DXGetImageInfoFromFileA(xpath, &info)==S_OK) {
					if (D3DXCreateTextureFromFileExA(pDevice, xpath, 0, 0, 0, Usage, D3DFMT_FROM_FILE, D3DPOOL_SYSTEMMEM, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pBumpMap)==S_OK) {
						if (D3DXCreateTexture(pDevice, info.Width, info.Height, 0, 0, D3DFMT_R8G8B8, D3DPOOL_DEFAULT, &pNormalMap)==S_OK) {
							DWORD Channel = D3DX_CHANNEL_RED;
							if (info.Format==D3DFMT_A8) Channel = D3DX_CHANNEL_ALPHA;
							if (info.Format==D3DFMT_L8) Channel = D3DX_CHANNEL_LUMINANCE;
							if (D3DXComputeNormalMap(pNormalMap, pBumpMap, NULL, 0, Channel, float(Config->BumpAmp))==S_OK) {
								LogAlw("Bump Map %s Loaded Successfully",bname);
								gNormalType = 1;
							}
							else LogErr("BumpMap conversion Failed (%s)",bname);
						}
						pBumpMap->Release();
					}
					else {
						pNormalMap = NULL;
						LogErr("Failed to load image (%s)",bname);
					}
				}
				else LogErr("Failed to acquire image information for (%s)",sname);
			}

			// Normal Map Section =======================================================================================================================
			//
			if (gc->TexturePath(nname, xpath) && pBumpMap==NULL) {
				D3DXIMAGE_INFO info;
				pNormalMap = NULL;
				DWORD Usage = 0;
				if (D3DXGetImageInfoFromFileA(xpath, &info)==S_OK) {
			
					switch (info.Format) {
						case D3DFMT_R8G8B8:
						case D3DFMT_X8R8G8B8:
						case D3DFMT_A8R8G8B8:
						case D3DFMT_DXT1:
						case D3DFMT_DXT3:
							gNormalType = 1;
							break;
						
						case D3DFMT_V8U8: 
							gNormalType = 0;
							break;

						default:
							LogErr("Format %u isn't supported for normal map. (%s)",info.Format,nname);
							break;
					}

					if (D3DXCreateTextureFromFileExA(pDevice, xpath, 0, 0, 0, Usage, D3DFMT_FROM_FILE, D3DPOOL_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pNormalMap)==S_OK) {
						LogAlw("Normal Map %s Loaded Successfully",nname);
					}
					else {
						pNormalMap = NULL;
						LogErr("Failed to load image (%s)",nname);
					}
				}
				else LogErr("Failed to acquire image information for (%s)",nname);
			}

			// Specular Map Section =======================================================================================================================
			//
			if (gc->TexturePath(sname, xpath)) {
				D3DXIMAGE_INFO info;
				pSpecularMap = NULL;
				if (D3DXGetImageInfoFromFileA(xpath, &info)==S_OK) {
					if (D3DXCreateTextureFromFileExA(pDevice, xpath, 0, 0, 0, Usage, D3DFMT_FROM_FILE, D3DPOOL_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pSpecularMap)==S_OK) {
						LogAlw("Specular Map %s Loaded Successfully",sname);
					}
					else {
						pSpecularMap = NULL;
						LogErr("Failed to load image (%s)",sname);
					}
				}
				else LogErr("Failed to acquire image information for (%s)",sname);
			}

			// Emission Map Section =======================================================================================================================
			//
			if (gc->TexturePath(ename, xpath)) {
				D3DXIMAGE_INFO info;
				pEmissionMap = NULL;
				if (D3DXGetImageInfoFromFileA(xpath, &info)==S_OK) {
					if (D3DXCreateTextureFromFileExA(pDevice, xpath, 0, 0, 0, Usage, D3DFMT_FROM_FILE, D3DPOOL_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pEmissionMap)==S_OK) {
						LogAlw("Emission Map %s Loaded Successfully",ename);
					}
					else {
						pEmissionMap = NULL;
						LogErr("Failed to load image (%s)",ename);
					}
				}
				else LogErr("Failed to acquire image information for (%s)",ename);
			}

			// Reflection Map Section =======================================================================================================================
			//
			if (gc->TexturePath(rname, xpath)) {
				D3DXIMAGE_INFO info;
				pEmissionMap = NULL;
				if (D3DXGetImageInfoFromFileA(xpath, &info)==S_OK) {
					if (D3DXCreateTextureFromFileExA(pDevice, xpath, 0, 0, 0, Usage, D3DFMT_FROM_FILE, D3DPOOL_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pReflectionMap)==S_OK) {
						LogAlw("Reflection Map %s Loaded Successfully",rname);
					}
					else {
						pReflectionMap = NULL;
						LogErr("Failed to load image (%s)",rname);
					}
				}
				else LogErr("Failed to acquire image information for (%s)",rname);
			}
		}

		// Diffuse Texture Section ====================================================================================================================
		//
		if (D3DXCreateTextureFromFileExA(pDevice, cpath, 0, 0, Mips, Usage, Format, Pool, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pTex)==S_OK) {	
			SetName(fname);
			HR(pTex->GetSurfaceLevel(0, &pSurf));
			GetDesc(&desc);
			LogBlu("Texture %s found. Handle=0x%X, (%ux%u), MipMaps=%u, Flags=0x%X, Format=0x%X",fname, this, desc.Width, desc.Height, pTex->GetLevelCount(), flags, DWORD(Format));
			return true;
		}
		else {
			LogErr("Texture %s failed to load",fname); 
			return false;
		}	
	}

	LogWrn("Texture %s not found. Handle=0x%X",fname,this);
	SetName(fname);
	return false;
}


// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::SaveSurface(const char *fname)
{
	LPDIRECT3DTEXTURE9 pSystem = NULL;
	LPDIRECT3DTEXTURE9 pTemp = NULL;
	LPDIRECT3DSURFACE9 pTempS = NULL;
	LPDIRECT3DSURFACE9 pSystemS = NULL;
	
	if (desc.Pool!=D3DPOOL_SYSTEMMEM) {
		if (Type==D3D9S_TEXTURE || Type==D3D9S_DYNAMIC || Type==D3D9S_PLAIN) {		
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
		if (Type==D3D9S_RTGTTEX) {
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
	}
}

// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::LogSpecs(char *xname)
{
	char buf[256]; 
	_SETLOG(4); 

	D3DSURFACE_DESC desc;
	GetDesc(&desc);
	LogMsg("%s name is %s Handle=0x%X (%u,%u)",xname,name,this,desc.Width,desc.Height);
	if (Type==D3D9S_TEXTURE) LogMsg("%s is a Texture",xname);
	if (Type==D3D9S_DYNAMIC) LogMsg("%s is a Dynamic Texture",xname);
	if (Type==D3D9S_PLAIN)   LogMsg("%s is a Plain Surface",xname);
	if (Type==D3D9S_RTGTTEX) LogMsg("%s is a Render Target Texture",xname);
	
	if (pTex) LogMsg("%s has texture inteface",xname);
	if (pSurf) LogMsg("%s has surface interface",xname);
	if (desc.Pool==D3DPOOL_DEFAULT)   LogMsg("%s is in a DefaultPool",xname);
	if (desc.Pool==D3DPOOL_SYSTEMMEM) LogMsg("%s is in a SystemMemPool",xname);
	if (desc.Pool==D3DPOOL_MANAGED)   LogMsg("%s is in a ManagedPool",xname);
	if (desc.Usage&D3DUSAGE_DYNAMIC)  LogMsg("%s has DYNAMIC usage",xname);
	if (desc.Usage&D3DUSAGE_RENDERTARGET) LogMsg("%s has RENDERTARGET usage",xname);
	LogMsg("%s Format is %u",xname, desc.Format);

	if (pTex) LogMsg("%s Has %u MipMaps",xname, pTex->GetLevelCount());
	if (pRTS) LogMsg("%s Has Rendering Interface",xname);
	if (bDCOpen) LogMsg("%s Has Open DC",xname);

	strcpy_s(buf,255,"InitialFlags( ");
	if (Initial&0x1) strcat_s(buf,255,"SYSTEMMEM ");
	if (Initial&0x2) strcat_s(buf,255,"DECOMPRESS ");
	if (Initial&0x4) strcat_s(buf,255,"NO_MIPMAP ");
	if (Initial&0x8) strcat_s(buf,255,"MANAGED ");
	LogMsg("%s)",buf);

	_POPLOG;
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
	if (Type==D3D9S_DYNAMIC) return true;
	if (Type==D3D9S_PLAIN) return true;
	return false;
}

// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::IsRenderTarget()
{
	if (Creation==D3D9C_BACKBUF) return true;
	if (desc.Pool==D3DPOOL_DEFAULT && desc.Usage&D3DUSAGE_RENDERTARGET) return true;
	return false;
}

// -----------------------------------------------------------------------------------------------
//
bool D3D9ClientSurface::IsBackBuffer()
{
	if (Creation==D3D9C_BACKBUF) return true;
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
bool D3D9ClientSurface::ScanNameSubId(const char *n)
{
	char lbl[64];
	char buf[256];
	sprintf_s(lbl,64,"\\%s\\",n);
	strcpy(buf, name);
	_strupr_s(lbl, 64);
	_strupr_s(buf, 256);
	if (strstr(buf,lbl)) return true;
	return false;
}

// -----------------------------------------------------------------------------------------------
//
void D3D9ClientSurface::SetCreation(int cr)
{
	Creation = cr;
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
LPDIRECT3DTEXTURE9 D3D9ClientSurface::GetTexture()
{
	LPDIRECT3DTEXTURE9 pTexture = GetTextureHard();
	if (pTexture==NULL) pTexture = gc->GetDefaultTexture()->GetTexture();
	return pTexture;
}

// -----------------------------------------------------------------------------------------------
//
LPDIRECT3DTEXTURE9 D3D9ClientSurface::GetNormalMap()
{
	return pNormalMap;
}

// -----------------------------------------------------------------------------------------------
//
LPDIRECT3DTEXTURE9 D3D9ClientSurface::GetEmissionMap()
{
	return pEmissionMap;
}

// -----------------------------------------------------------------------------------------------
//
LPDIRECT3DTEXTURE9 D3D9ClientSurface::GetSpecularMap()
{
	return pSpecularMap;
}

// -----------------------------------------------------------------------------------------------
//
LPDIRECT3DTEXTURE9 D3D9ClientSurface::GetReflectionMap()
{
	return pReflectionMap;
}

// -----------------------------------------------------------------------------------------------
//
DWORD D3D9ClientSurface::GetTextureSizeInBytes(LPDIRECT3DTEXTURE9 pT)
{
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
	if (pNormalMap) size += GetTextureSizeInBytes(pNormalMap);
	if (pSpecularMap) size += GetTextureSizeInBytes(pSpecularMap);
	if (pEmissionMap) size += GetTextureSizeInBytes(pEmissionMap);
	
	return size;
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

	LogMsg("...rendering technique initialized");
}