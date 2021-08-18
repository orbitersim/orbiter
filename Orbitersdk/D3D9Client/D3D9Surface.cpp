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

extern D3D9Client* g_client;



LPDIRECT3DTEXTURE9 NatLoadSpecialTexture(const char* fname, const char* ext)
{
	char path[MAX_PATH];
	char name[MAX_PATH];

	NatCreateName(name, ARRAYSIZE(name), fname, ext);

	LPDIRECT3DTEXTURE9 pTex = NULL;
	
	if (g_client->TexturePath(name, path)) {
		D3DXIMAGE_INFO info;	
		if (D3DXGetImageInfoFromFileA(path, &info) == S_OK) {

			DWORD Mips = D3DFMT_FROM_FILE;

			if (Config->TextureMips == 2) Mips = 0;                         // Autogen all
			if (Config->TextureMips == 1 && info.MipLevels == 1) Mips = 0;  // Autogen missing

			if (S_OK == D3DXCreateTextureFromFileExA(g_client->GetDevice(), path, info.Width, info.Height, Mips, 0, D3DFMT_FROM_FILE, D3DPOOL_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pTex))
			{
				return pTex;
			}
		}
	}
	return NULL;
}



// ======================================================================================
// Main loading routine
//
SURFHANDLE NatLoadSurface(const char* file, DWORD flags)
{
	LPDIRECT3DTEXTURE9 pTex = NULL;
	SurfNative* pNat = NULL;

	char path[MAX_PATH];

	if (!g_client->TexturePath(file, path)) {
		return NULL;
	}

	// Load regular texture with additional maps if exists
	//
	if (flags == OAPISURFACE_TEXTURE)
	{
		D3DXIMAGE_INFO info;

		if (D3DXGetImageInfoFromFileA(path, &info) == S_OK)
		{

			DWORD Mips = D3DFMT_FROM_FILE;
			if (Config->TextureMips == 2) Mips = 0;                         // Autogen all
			if (Config->TextureMips == 1 && info.MipLevels == 1) Mips = 0;  // Autogen missing

			if (S_OK == D3DXCreateTextureFromFileExA(g_client->GetDevice(), path, info.Width, info.Height, Mips, 0, info.Format, D3DPOOL_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pTex))
			{
				LogBlu("TextureLoaded[%s] PLAIN Format=%u (%u,%u)", file, info.Format, info.Width, info.Height);

				pNat = new SurfNative(pTex, flags);
				pNat->SetName(file);
				
				pNat->AddMap(MAP_HEAT, NatLoadSpecialTexture(file, "heat"));
				pNat->AddMap(MAP_NORMAL, NatLoadSpecialTexture(file, "norm"));
				pNat->AddMap(MAP_SPECULAR, NatLoadSpecialTexture(file, "spec"));
				pNat->AddMap(MAP_EMISSION, NatLoadSpecialTexture(file, "emis"));
				pNat->AddMap(MAP_ROUGHNESS, NatLoadSpecialTexture(file, "rghn"));
				pNat->AddMap(MAP_METALNESS, NatLoadSpecialTexture(file, "metal"));
				pNat->AddMap(MAP_REFLECTION, NatLoadSpecialTexture(file, "refl"));
				pNat->AddMap(MAP_TRANSLUCENCE, NatLoadSpecialTexture(file, "transl"));
				pNat->AddMap(MAP_TRANSMITTANCE, NatLoadSpecialTexture(file, "transm"));
			}
		}

		return SURFHANDLE(pNat);
	}


	// Load more complex surface
	//
	D3DXIMAGE_INFO info;
	HR(D3DXGetImageInfoFromFileA(path, &info));

	if (flags & OAPISURFACE_SKETCHPAD) flags |= OAPISURFACE_RENDERTARGET;
	if (flags & OAPISURFACE_RENDERTARGET) flags |= OAPISURFACE_UNCOMPRESS;

	DWORD Mips = D3DX_FROM_FILE;
	DWORD Usage = 0;
	D3DFORMAT Format = info.Format;
	D3DPOOL Pool = D3DPOOL_DEFAULT;
	D3DMULTISAMPLE_TYPE Multi = D3DMULTISAMPLE_NONE;
	bool bLock = false;

	// File Formats Not Supported
	if (info.Format == D3DFMT_A4R4G4B4) Format = D3DFMT_A8R8G8B8;
	if (info.Format == D3DFMT_X4R4G4B4) Format = D3DFMT_X8R8G8B8;

	// Predict the surface format
	//
	if (info.ImageFileFormat == D3DXIFF_JPG) Format = D3DFMT_X8R8G8B8;
	if (info.ImageFileFormat == D3DXIFF_PNG) Format = D3DFMT_X8R8G8B8;
	if (info.ImageFileFormat == D3DXIFF_BMP) Format = D3DFMT_X8R8G8B8;

	if (flags & OAPISURFACE_UNCOMPRESS)
	{
		D3DFORMAT Fmt = (flags & OAPISURFACE_ALPHA) ? D3DFMT_A8R8G8B8 : D3DFMT_X8R8G8B8;
		if (info.Format == D3DFMT_DXT1) Format = Fmt;
		if (info.Format == D3DFMT_DXT5) Format = Fmt;
		if (info.Format == D3DFMT_DXT3) Format = Fmt;
	}

	// User defined format
	//
	D3DFORMAT Fmt = D3DFORMAT(NatConvertFormat_OAPI_to_DX(flags));
	if (Fmt != 0) Format = Fmt;
	
	if (flags & OAPISURFACE_RENDERTARGET) Usage = D3DUSAGE_RENDERTARGET;
	if (flags & OAPISURFACE_GDI) Usage = D3DUSAGE_DYNAMIC;
	if (flags & OAPISURFACE_SYSMEM) Pool = D3DPOOL_SYSTEMMEM;
	if (flags & OAPISURFACE_NOMIPMAPS) Mips = 1;
	if (flags & OAPISURFACE_MIPMAPS) Mips = 0;
	
	if (flags & OAPISURFACE_TEXTURE)
	{
		if (S_OK == D3DXCreateTextureFromFileExA(g_client->GetDevice(), path, info.Width, info.Height, Mips,
			Usage, Format, Pool, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pTex))
		{
			LogBlu("TextureLoaded[%s] Usage=%u, Format=%u (%u,%u)", file, Usage, Format, info.Width, info.Height);
			SurfNative *pSrf = new SurfNative(pTex, flags);
			pSrf->SetName(file);
			return pSrf;
		}
	}

	if (flags & OAPISURFACE_RENDERTARGET)
	{
		LPDIRECT3DSURFACE9 pSurf = NULL;
		if (S_OK == g_client->GetDevice()->CreateRenderTarget(info.Width, info.Height, Format, Multi, 0, bLock, &pSurf, NULL))
		{
			if (S_OK == D3DXLoadSurfaceFromFile(pSurf, NULL, NULL, path, NULL, D3DX_DEFAULT, 0, NULL))
			{
				LogBlu("SurfaceLoaded[%s] RENDERTARGET Format=%u (%u,%u)", file, Format, info.Width, info.Height);
				SurfNative* pSrf = new SurfNative(pSurf, flags);
				pSrf->SetName(file);
				return pSrf;
			}
		}
	}

	assert(false);
	return NULL;
}



// ===============================================================================================
//
bool NatSaveSurface(const char* file, LPDIRECT3DRESOURCE9 pResource)
{
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	
	D3DXIMAGE_FILEFORMAT fmt = D3DXIMAGE_FILEFORMAT(0);

	if (contains(file, ".dds")) fmt = D3DXIFF_DDS;
	if (contains(file, ".bmp")) fmt = D3DXIFF_BMP;
	if (contains(file, ".jpg")) fmt = D3DXIFF_JPG;
	if (contains(file, ".png")) fmt = D3DXIFF_PNG;


	if (pResource->GetType() == D3DRTYPE_SURFACE)
	{
		LPDIRECT3DSURFACE9 pSurf = static_cast<LPDIRECT3DSURFACE9>(pResource);
		if (D3DXSaveSurfaceToFileA(file, fmt, pSurf, NULL, NULL) == S_OK) return true;
		oapiWriteLog("NatSaveSurface(SURF):");
		NatDumpResource(pResource);
		return false;
	}


	if (pResource->GetType() == D3DRTYPE_TEXTURE)
	{
		LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(pResource);
		D3DSURFACE_DESC desc;
		pTex->GetLevelDesc(0, &desc);

		if (desc.Pool == D3DPOOL_SYSTEMMEM || desc.Usage & D3DUSAGE_DYNAMIC) {
			if (D3DXSaveTextureToFileA(file, fmt, pTex, NULL) == S_OK) return true;
			oapiWriteLog("NatSaveSurface(TEX):");
			NatDumpResource(pResource);
			return false;
		}

		if (desc.Usage & D3DUSAGE_RENDERTARGET) {
			LPDIRECT3DTEXTURE9 pSys = NULL;
			DWORD Mips = pTex->GetLevelCount();
			HR(D3DXCreateTexture(pDev, desc.Width, desc.Height, Mips, 0, desc.Format, D3DPOOL_SYSTEMMEM, &pSys));
			for (DWORD i = 0; i < Mips; i++) {
				LPDIRECT3DSURFACE9 pSrc, pTgt;
				HR(pTex->GetSurfaceLevel(i, &pSrc));
				HR(pSys->GetSurfaceLevel(i, &pTgt));
				HR(pDev->GetRenderTargetData(pSrc, pTgt));
				pSrc->Release();
				pTgt->Release();
			}
			if (D3DXSaveTextureToFile(file, fmt, pSys, NULL) == S_OK) {
				pSys->Release();
				return true;
			}
			pSys->Release();
		}

		if (D3DXSaveTextureToFileA(file, fmt, pTex, NULL) == S_OK) return true;
	}

	oapiWriteLog("NatSaveSurface():");
	NatDumpResource(pResource);
	return false;
}



// ===============================================================================================
//
SURFHANDLE NatCreateSurface(int width, int height, DWORD flags)
{
	DWORD Mips = 1;
	DWORD Usage = 0;
	D3DPOOL Pool = D3DPOOL_DEFAULT;
	D3DMULTISAMPLE_TYPE Multi = D3DMULTISAMPLE_NONE;
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();

	// Append dependend flags
	if (flags & OAPISURFACE_RENDER3D) flags |= OAPISURFACE_RENDERTARGET;
	if (flags & OAPISURFACE_SKETCHPAD) flags |= OAPISURFACE_RENDERTARGET;

	if (flags & OAPISURFACE_RENDERTARGET) {
		assert((flags & OAPISURFACE_GDI) == 0);			// Potentially invalid combo
		assert((flags & OAPISURFACE_SYSMEM) == 0);		// Potentially invalid combo
	}

	if (flags & OAPISURFACE_MIPMAPS) {
		assert((flags & OAPISURFACE_TEXTURE) != 0);		// Potentially invalid combo
		assert((flags & OAPISURFACE_GDI) == 0);			// Potentially invalid combo
	}

	
	if (flags & OAPISURFACE_RENDERTARGET) Usage = D3DUSAGE_RENDERTARGET;
	if (flags & OAPISURFACE_GDI) Usage = D3DUSAGE_DYNAMIC;
	if (flags & OAPISURFACE_SYSMEM) Pool = D3DPOOL_SYSTEMMEM;
	if (flags & OAPISURFACE_NOMIPMAPS) Mips = 1;
	if (flags & OAPISURFACE_MIPMAPS) Mips = 0;	// Fullchain
	if (flags & OAPISURFACE_ANTIALIAS) Multi = D3DMULTISAMPLE_8_SAMPLES;

	D3DFORMAT Format = D3DFORMAT(NatConvertFormat_OAPI_to_DX(flags));

	if (Format == 0)
	{
		if (flags & OAPISURFACE_ALPHA) Format = D3DFMT_A8R8G8B8;
		else Format = D3DFMT_X8R8G8B8;
	}


	if ((flags & OAPISURFACE_TEXTURE) || (flags & OAPISURFACE_SYSMEM) || (flags & OAPISURFACE_GDI))
	{
		LPDIRECT3DTEXTURE9 pTex = NULL;
		LPDIRECT3DSURFACE9 pDepth = NULL;

		if (S_OK == D3DXCreateTexture(pDev, width, height, Mips, Usage, Format, Pool, &pTex))
		{
			SurfNative* pNat = new SurfNative(pTex, flags);

			if (flags & OAPISURFACE_RENDER3D)
			{
				HR(pDev->CreateDepthStencilSurface(width, height, D3DFMT_D24S8, D3DMULTISAMPLE_NONE, 0, true, &pDepth, NULL));	
				pNat->AddSubSurface(pDepth);
			}

			return SURFHANDLE(pNat);
		}
	}


	if (flags & OAPISURFACE_RENDERTARGET)
	{
		LPDIRECT3DSURFACE9 pSurf = NULL;
		LPDIRECT3DSURFACE9 pDepth = NULL;

		if (S_OK == pDev->CreateRenderTarget(width, height, Format, Multi, 0, false, &pSurf, NULL))
		{
			SurfNative* pNat = new SurfNative(pSurf, flags);

			if (flags & OAPISURFACE_RENDER3D)
			{
				HR(pDev->CreateDepthStencilSurface(width, height, D3DFMT_D24S8, Multi, 0, true, &pDepth, NULL));
				pNat->AddSubSurface(pDepth);
			}

			return SURFHANDLE(pNat);
		}
	}

	assert(false);
	return NULL;
}


// ===============================================================================================
//
SURFHANDLE	NatGetMipSublevel(SURFHANDLE hSrf, int level)
{
	LPDIRECT3DRESOURCE9 pResource = static_cast<LPDIRECT3DRESOURCE9>(hSrf);

	if (pResource->GetType() == D3DRTYPE_TEXTURE)
	{
		LPDIRECT3DSURFACE9 pSurf = NULL;
		LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(hSrf);
		if (pTex->GetSurfaceLevel(level, &pSurf) == S_OK) return SURFHANDLE(pSurf);
	}
	return NULL;
}


// ===============================================================================================
//
SURFHANDLE NatCompressSurface(SURFHANDLE hSurface, DWORD flags)
{
	D3DSURFACE_DESC desc;
	LPDIRECT3DSURFACE9 pDest = NULL;
	LPDIRECT3DTEXTURE9 pTex = NULL;
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	LPDIRECT3DRESOURCE9 pResource = SURFACE(hSurface)->GetResource();

	DWORD Mips = 1;
	D3DFORMAT Fmt = D3DFMT_DXT1;
	D3DPOOL Pool = D3DPOOL_DEFAULT;
	if (flags & OAPISURFACE_MIPMAPS) Mips = 0;
	if ((flags & OAPISURFACE_PF_MASK) == OAPISURFACE_PF_DXT3) Fmt = D3DFMT_DXT3;
	if ((flags & OAPISURFACE_PF_MASK) == OAPISURFACE_PF_DXT5) Fmt = D3DFMT_DXT5;
	if (flags & OAPISURFACE_SYSMEM) Pool = D3DPOOL_SYSTEMMEM;

	if (pResource->GetType() == D3DRTYPE_SURFACE) {
		LPDIRECT3DSURFACE9 pSurf = static_cast<LPDIRECT3DSURFACE9>(hSurface);
		HR(pSurf->GetDesc(&desc));
		HR(D3DXCreateTexture(pDev, desc.Width, desc.Height, Mips, 0, Fmt, Pool, &pTex));
		for (DWORD i = 0; i < pTex->GetLevelCount(); i++) {
			HR(pTex->GetSurfaceLevel(i, &pDest));
			HR(D3DXLoadSurfaceFromSurface(pDest, NULL, NULL, pSurf, NULL, NULL, D3DX_FILTER_BOX, 0));
			pDest->Release();
		}
		return SURFHANDLE(pTex);
	}
	else if (pResource->GetType() == D3DRTYPE_TEXTURE) {
		LPDIRECT3DSURFACE9 pSurf = NULL;
		LPDIRECT3DTEXTURE9 pInp = static_cast<LPDIRECT3DTEXTURE9>(hSurface);
		HR(pInp->GetLevelDesc(0, &desc));
		HR(D3DXCreateTexture(pDev, desc.Width, desc.Height, Mips, 0, Fmt, Pool, &pTex));
		HR(pInp->GetSurfaceLevel(0, &pSurf));
		for (DWORD i = 0; i < pTex->GetLevelCount(); i++) {
			HR(pTex->GetSurfaceLevel(i, &pDest));
			HR(D3DXLoadSurfaceFromSurface(pDest, NULL, NULL, pSurf, NULL, NULL, D3DX_FILTER_BOX, 0));
			pDest->Release();
		}
		pSurf->Release();
		return SURFHANDLE(pTex);
	}

	return NULL;
}







// -----------------------------------------------------------------------------------------------
//
SurfNative::SurfNative(LPDIRECT3DRESOURCE9 pRes, DWORD flags) :
	pResource(pRes),
	pDX7(NULL),
	pSkp(NULL),
	pSub(NULL),
	pTexSurf(NULL),
	pGDICache(NULL),
	pDevice(g_client->GetDevice()),
	ColorKey(SURF_NO_CK),
	Flags(flags),
	type(D3DRTYPE_FORCE_DWORD),
	Mipmaps(1),
	RefCount(1)
{
	assert(pRes != NULL);

	SurfaceCatalog.insert(this);

	memset(pMap, 0, sizeof(pMap));
	memset(&desc, 0, sizeof(desc));
	memset(&DC, 0, sizeof(DC));

	strcpy_s(name, sizeof(name), "null");

	type = pResource->GetType();
	
	if (type == D3DRTYPE_SURFACE) {
		LPDIRECT3DSURFACE9 pSrf = static_cast<LPDIRECT3DSURFACE9>(pResource);
		pSrf->GetDesc(&desc);
	}

	if (type == D3DRTYPE_TEXTURE) {
		LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(pResource);
		pTex->GetLevelDesc(0, &desc);
		Mipmaps = pTex->GetLevelCount();
	}
}


// -----------------------------------------------------------------------------------------------
//
SurfNative::~SurfNative()
{
	if (SurfaceCatalog.erase(this) != 1) assert(false);

	for (int i = 0; i < MAP_MAX_COUNT; i++) SAFE_RELEASE(pMap[i]);

	SAFE_RELEASE(pSub);
	SAFE_RELEASE(pDX7);
	SAFE_RELEASE(pTexSurf);
	SAFE_RELEASE(pResource);
	SAFE_RELEASE(pGDICache);
	SAFE_DELETE(pSkp);
}


// -----------------------------------------------------------------------------------------------
//
void SurfNative::AddMap(DWORD id, LPDIRECT3DTEXTURE9 _pMap)
{
	if (id >= MAP_MAX_COUNT) return;
	SAFE_RELEASE(pMap[id]);
	pMap[id] = _pMap;
	Flags |= OAPISURFACE_MAPS;
}


// -----------------------------------------------------------------------------------------------
//
void SurfNative::AddSubSurface(LPDIRECT3DSURFACE9 _pSub)
{
	SAFE_RELEASE(pSub);
	pSub = _pSub;
}


// -----------------------------------------------------------------------------------------------
//
LPDIRECT3DTEXTURE9 SurfNative::GetTexture() const
{
	if (type == D3DRTYPE_TEXTURE) return static_cast<LPDIRECT3DTEXTURE9>(pResource);
	return NULL;
}


// -----------------------------------------------------------------------------------------------
//
LPDIRECT3DSURFACE9 SurfNative::GetSubSurface()
{
	if (pSub) return pSub;
	HR(pDevice->CreateRenderTarget(desc.Width, desc.Height, desc.Format, D3DMULTISAMPLE_NONE, 0, false, &pSub, NULL));
	return pSub;
}


// -----------------------------------------------------------------------------------------------
//
LPDIRECT3DSURFACE9 SurfNative::GetSurface()
{
	if (type == D3DRTYPE_SURFACE) return static_cast<LPDIRECT3DSURFACE9>(pResource);

	if (type == D3DRTYPE_TEXTURE)
	{
		LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(pResource);

		if (!pTexSurf)
		{
			HR(pTex->GetSurfaceLevel(0, &pTexSurf))
		}
		return pTexSurf;
	}
	return NULL;
}


// -----------------------------------------------------------------------------------------------
//
void SurfNative::SetColorKey(DWORD ck)
{
	ColorKey = ck;
}


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::IsGDISurface() const
{
	if (desc.Pool == D3DPOOL_SYSTEMMEM) return true;
	if (desc.Usage & D3DUSAGE_DYNAMIC) return true;
	return false;
}

// -----------------------------------------------------------------------------------------------
//
bool SurfNative::IsRenderTarget() const
{
	if (Flags & OAPISURFACE_BACKBUFFER) return true;
	if (desc.Pool == D3DPOOL_DEFAULT && desc.Usage & D3DUSAGE_RENDERTARGET) return true;
	return false;
}


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::Is3DRenderTarget() const
{
	if (Flags & OAPISURFACE_BACKBUFFER) return true;
	if (Flags & OAPISURFACE_RENDER3D) return true;
	return false;
}


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::IsBackBuffer() const
{
	if (Flags & OAPISURFACE_BACKBUFFER) return true;
	return false;
}


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::IsCompressed() const
{
	if (desc.Format == D3DFMT_DXT1) return true;
	if (desc.Format == D3DFMT_DXT3) return true;
	if (desc.Format == D3DFMT_DXT5) return true;
	if (desc.Format == D3DFMT_DXT2) return true;
	if (desc.Format == D3DFMT_DXT4) return true;
	return false;
}


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::IsPowerOfTwo() const
{
	DWORD w = desc.Width, h = desc.Height;
	for (int i = 0; i < 14; i++) if ((w & 1) == 0) w = w >> 1; else { if (w != 1) return false; else break; }
	for (int i = 0; i < 14; i++) if ((h & 1) == 0) h = h >> 1; else { if (h != 1) return false; else break; }
	return true;
}


// -----------------------------------------------------------------------------------------------
//
void SurfNative::SetName(const char* n)
{
	strcpy_s(name, 128, n);
	int i = -1;
	while (name[++i] != 0) if (name[i] == '/') name[i] = '\\';
}


// -----------------------------------------------------------------------------------------------
//
LPDIRECT3DTEXTURE9 SurfNative::GetGDICache(DWORD Flags)
{
	if (pGDICache) return pGDICache;
	D3DPOOL Pool = (Flags & OAPISURFACE_SYSMEM) ? D3DPOOL_SYSTEMMEM : D3DPOOL_DEFAULT;
	DWORD Usage = (Flags & OAPISURFACE_SYSMEM) ? 0 : D3DUSAGE_DYNAMIC;
	HR(D3DXCreateTexture(pDevice, desc.Width, desc.Height, 1, Usage, D3DFMT_X8R8G8B8, Pool, (LPDIRECT3DTEXTURE9 *)&pGDICache));
	return pGDICache;
}


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::GenerateMipMaps()
{
	if (type == D3DRTYPE_TEXTURE)
	{
		LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(pResource);

		if (desc.Usage & D3DUSAGE_AUTOGENMIPMAP) {
			pTex->GenerateMipSubLevels();
			return true;
		}
		else {
			
			DWORD nMip = pTex->GetLevelCount();
			if (nMip <= 1) return false;

			LPDIRECT3DSURFACE9 pHigh = NULL;
			LPDIRECT3DSURFACE9 pLow = NULL;

			HR(pTex->GetSurfaceLevel(0, &pHigh));

			if (pHigh) {
				for (DWORD i = 1; i < nMip; i++) {
					if (pTex->GetSurfaceLevel(i, &pLow) == S_OK) {
						HR(pDevice->StretchRect(pHigh, NULL, pLow, NULL, D3DTEXF_LINEAR));
						pHigh->Release();
						pHigh = pLow;
					}
				}
				pHigh->Release();
				return true;
			}
			return false;

		}
	}
	return false;
}


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::CreateDX7()
{
	if (pDX7) return true;
	if (desc.Format == D3DFMT_X8R8G8B8)
	{
		if (S_OK == g_client->GetDevice()->CreateRenderTarget(desc.Width, desc.Height, D3DFMT_X8R8G8B8, D3DMULTISAMPLE_NONE, 0, true, &pDX7, NULL))
		{
			oapiWriteLogV("Surface %s (%u,%u) going in DX7 compatibility mode", _PTR(this), desc.Width, desc.Height);
			return true;
		}
	}
	else {
		return false;
	}
}


// -----------------------------------------------------------------------------------------------
//
void SurfNative::DX7Sync(bool bUp)
{
	if (bUp) {
		HR(pDevice->StretchRect(GetSurface(), NULL, pDX7, NULL, D3DTEXF_POINT));		
	}
	else {
		HR(pDevice->StretchRect(pDX7, NULL, GetSurface(), NULL, D3DTEXF_POINT));
	}
}


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::Fill(LPRECT rect, DWORD c)
{
	
	LPRECT r;
	RECT re;

	if (rect==NULL) { re.left=0, re.top=0, re.right=desc.Width, re.bottom=desc.Height; r=&re; }
	else r = rect;

	if (desc.Pool==D3DPOOL_DEFAULT)
	{
		if (desc.Usage&D3DUSAGE_RENDERTARGET)
		{
			LPDIRECT3DSURFACE9 pSrf = GetSurface();
			if (pDevice->ColorFill(pSrf, r, c) == S_OK) return true;
		}
	}

	LogErr("ColorFill Failed");
	LogSpecs();
	HALT();
	return false;
}


// -----------------------------------------------------------------------------------------------
//
HDC	SurfNative::GetDC()
{
	if (!DC.hDC)
	{
		if (IsGDISurface()) {
			LPDIRECT3DSURFACE9 pSrf = GetSurface();
			if (pSrf->GetDC(&DC.hDC) == S_OK)
			{
				DC.pSrf = pSrf;
				return DC.hDC;
			}
		}
		else {
			if (CreateDX7())
			{
				DX7Sync(true);
				if (pDX7->GetDC(&DC.hDC) == S_OK)
				{
					DC.pSrf = pDX7;
					return DC.hDC;
				}
			}
		}
	}
	else
	{
		LogErr("SurfNative: GetDC() Is Already Open");
	}

	LogErr("SurfNative: GetDC() Failed");
	LogSpecs();
	HALT();
	return NULL;
}



// -----------------------------------------------------------------------------------------------
//
void SurfNative::ReleaseDC(HDC _hDC)
{
	if (!_hDC) return;

	assert(_hDC == DC.hDC);

	HR(DC.pSrf->ReleaseDC(DC.hDC));

	if (DC.pSrf == pDX7)
	{
		DX7Sync(false);
	}

	DC.pSrf = NULL;
	DC.hDC = NULL;	
}



// -----------------------------------------------------------------------------------------------
//
bool SurfNative::Decompress()
{
	if (IsCompressed())
	{
		LPDIRECT3DTEXTURE9 pDecomp;
		LPDIRECT3DSURFACE9 pDeSrf;
		D3DFORMAT Format = D3DFMT_FROM_FILE;

		if (desc.Format == D3DFMT_DXT1) Format = D3DFMT_X8R8G8B8;
		if (desc.Format == D3DFMT_DXT5) Format = D3DFMT_A8R8G8B8;
		if (desc.Format == D3DFMT_DXT3) Format = D3DFMT_A8R8G8B8;

		char path[MAX_PATH];

		if (!g_client->TexturePath(name, path)) {
			oapiWriteLogV("SurfNative::Reload() File Not Found [%s]", path);
			return false;
		}

		if (S_OK == D3DXCreateTextureFromFileExA(pDevice, path, desc.Width, desc.Height, Mipmaps, D3DUSAGE_RENDERTARGET, Format,
			D3DPOOL_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pDecomp))
		{
			if (S_OK == pDecomp->GetSurfaceLevel(0, &pDeSrf))
			{
				SAFE_RELEASE(pTexSurf);
				SAFE_RELEASE(pResource);
				pResource = pDecomp;
				pTexSurf = pDeSrf;
				HR(pDecomp->GetLevelDesc(0, &desc));
				Mipmaps = pDecomp->GetLevelCount();
				type = pDecomp->GetType();
				Flags = OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE;
				return true;		
			}
		}

		LogSpecs();
		return false;
	}

	return true;
}


// -----------------------------------------------------------------------------------------------
//
void SurfNative::Reload()
{	
	SAFE_RELEASE(pTexSurf);
	SAFE_RELEASE(pResource);
	for (int i = 0; i < ARRAYSIZE(pMap); i++) SAFE_RELEASE(pMap[i]);

	char path[MAX_PATH];

	if (!g_client->TexturePath(name, path)) {
		oapiWriteLogV("SurfNative::Reload() File Not Found [%s]", path);
		return;
	}

	if (Flags == OAPISURFACE_TEXTURE)
	{
		D3DXIMAGE_INFO info;

		if (D3DXGetImageInfoFromFileA(path, &info) == S_OK)
		{
			DWORD Mips = D3DFMT_FROM_FILE;
			if (Config->TextureMips == 2) Mips = 0;                         // Autogen all
			if (Config->TextureMips == 1 && info.MipLevels == 1) Mips = 0;  // Autogen missing

			if (S_OK == D3DXCreateTextureFromFileExA(g_client->GetDevice(), path, info.Width, info.Height, Mips, 0,
				D3DFMT_FROM_FILE, D3DPOOL_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, (LPDIRECT3DTEXTURE9 *)&pResource))
			{
				AddMap(MAP_HEAT, NatLoadSpecialTexture(name, "heat"));
				AddMap(MAP_NORMAL, NatLoadSpecialTexture(name, "norm"));
				AddMap(MAP_SPECULAR, NatLoadSpecialTexture(name, "spec"));
				AddMap(MAP_EMISSION, NatLoadSpecialTexture(name, "emis"));
				AddMap(MAP_ROUGHNESS, NatLoadSpecialTexture(name, "rghn"));
				AddMap(MAP_METALNESS, NatLoadSpecialTexture(name, "metal"));
				AddMap(MAP_REFLECTION, NatLoadSpecialTexture(name, "refl"));
				AddMap(MAP_TRANSLUCENCE, NatLoadSpecialTexture(name, "transl"));
				AddMap(MAP_TRANSMITTANCE, NatLoadSpecialTexture(name, "transm"));
			}
		}
	}
}



// -----------------------------------------------------------------------------------------------
//
void SurfNative::LogSpecs() const
{
	LogErr("Surface name is [%s] OAPI_Handle=%s", name, _PTR(this));
	if (pTexSurf) LogErr("Has a Surface Interface");
	if (pSub) LogErr("Has a SubSurface Interface");
	if (pDX7) LogErr("Surface is in DX7 compatibility mode");
	if (DC.hDC) LogErr("Has a HDC [%s]", _PTR(DC.hDC));
	LogErr("OAPI_Attribs: %s", NatOAPIFlags(Flags));
	NatDumpResource(pResource);
}



// -----------------------------------------------------------------------------------------------
//
DWORD SurfNative::GetTextureSizeInBytes(LPDIRECT3DTEXTURE9 pT)
{
	if (!pT) return 0;
	D3DSURFACE_DESC d; pT->GetLevelDesc(0,&d);
	DWORD size = GetFormatSizeInBytes(d.Format, d.Height * d.Width);
	if (pT->GetLevelCount() > 1) size += ((size>>2) + (size>>4) + (size>>6));
	return size;
}

// -----------------------------------------------------------------------------------------------
//
DWORD SurfNative::GetFormatSizeInBytes(D3DFORMAT Format, DWORD pixels)
{
	if (Format == D3DFMT_DXT1) return pixels >> 1;
	if (Format == D3DFMT_DXT3) return pixels;
	if (Format == D3DFMT_DXT5) return pixels;
	if (Format == D3DFMT_A8R8G8B8) return pixels * 4;
	if (Format == D3DFMT_X8R8G8B8) return pixels * 4;
	if (Format == D3DFMT_R5G6B5) return pixels * 2;
	if (Format == D3DFMT_A4R4G4B4) return pixels *2;
	if (Format == D3DFMT_R8G8B8) return pixels * 3;
	if (Format == D3DFMT_L16) return pixels * 2;
	if (Format == D3DFMT_R16F) return pixels * 2;
	if (Format == D3DFMT_G16R16F) return pixels * 4;
	if (Format == D3DFMT_R32F) return pixels * 4;
	if (Format == D3DFMT_G32R32F) return pixels * 8;
	if (Format == D3DFMT_L8) return pixels;
	if (Format == D3DFMT_A8) return pixels;
	if (Format == D3DFMT_A32B32G32R32F) return pixels * 16;
	if (Format == D3DFMT_A16B16G16R16F) return pixels * 8;
	return pixels;
}


// -----------------------------------------------------------------------------------------------
//
DWORD SurfNative::GetSizeInBytes()
{
	if (type == D3DRTYPE_SURFACE) return GetFormatSizeInBytes(desc.Format, desc.Height * desc.Width);
	if (type == D3DRTYPE_TEXTURE)
	{
		DWORD size = GetTextureSizeInBytes(static_cast<LPDIRECT3DTEXTURE9>(pResource));
		for (int i = 0; i < MAP_MAX_COUNT; i++)  size += GetTextureSizeInBytes(pMap[i]);
		return size;
	}
	return 0;
}


// -----------------------------------------------------------------------------------------------
//
D3D9Pad * SurfNative::GetPooledSketchPad()
{
	if (!IsRenderTarget()) {
		LogErr("Can't optain a Sketchpad to a non-render target surface %s", _PTR(this));
		assert(false);
		return NULL;
	}
	if (!pSkp) pSkp = new D3D9Pad(this, "SurfNative.Pooled");
	return pSkp;
}



// -----------------------------------------------------------------------------------------------
//
bool NatCreateName(char* out, int mlen, const char* fname, const char* id)
{
	char buffe[MAX_PATH];
	strcpy_s(buffe, MAX_PATH, fname);
	char* p = strrchr(buffe, '.');
	if (p != NULL) {
		*p = '\0';
		sprintf_s(out, mlen, "%s_%s.%s", buffe, id, ++p);
	}
	return (p != NULL);
}


// -----------------------------------------------------------------------------------------------
//
DWORD NatConvertFormat_DX_to_OAPI(DWORD Format)
{
	DWORD Out = OAPISURFACE_NOALPHA;
	if (Format == D3DFMT_X8R8G8B8) return Out | OAPISURFACE_PF_XRGB;
	if (Format == D3DFMT_R5G6B5) return Out | OAPISURFACE_PF_RGB565;
	if (Format == D3DFMT_L16) return Out | OAPISURFACE_PF_S16R;
	if (Format == D3DFMT_R16F) return Out | OAPISURFACE_PF_F16R;
	if (Format == D3DFMT_G16R16F) return Out | OAPISURFACE_PF_F16RG;
	if (Format == D3DFMT_R32F) return Out | OAPISURFACE_PF_F32R;
	if (Format == D3DFMT_G32R32F) return Out | OAPISURFACE_PF_F32RG;
	if (Format == D3DFMT_DXT1) return Out | OAPISURFACE_PF_DXT1;
	if (Format == D3DFMT_L8) return Out | OAPISURFACE_PF_GRAY;

	Out = OAPISURFACE_ALPHA;
	if (Format == D3DFMT_A8) return Out | OAPISURFACE_PF_ALPHA;
	if (Format == D3DFMT_A32B32G32R32F) return Out | OAPISURFACE_PF_F32RGBA;
	if (Format == D3DFMT_A16B16G16R16F) return Out | OAPISURFACE_PF_F16RGBA;
	if (Format == D3DFMT_A8R8G8B8) return Out | OAPISURFACE_PF_ARGB;
	if (Format == D3DFMT_DXT3) return Out | OAPISURFACE_PF_DXT3;
	if (Format == D3DFMT_DXT5) return Out | OAPISURFACE_PF_DXT5;
	return 0;
}


// -----------------------------------------------------------------------------------------------
//
DWORD NatConvertFormat_OAPI_to_DX(DWORD Format)
{
	Format &= OAPISURFACE_PF_MASK;

	if (Format == OAPISURFACE_PF_XRGB) return D3DFMT_X8R8G8B8;
	if (Format == OAPISURFACE_PF_ARGB) return D3DFMT_A8R8G8B8;
	if (Format == OAPISURFACE_PF_RGB565) return D3DFMT_R5G6B5;
	if (Format == OAPISURFACE_PF_S16R) return D3DFMT_L16;
	if (Format == OAPISURFACE_PF_F16R) return D3DFMT_R16F;
	if (Format == OAPISURFACE_PF_F16RG) return D3DFMT_G16R16F;
	if (Format == OAPISURFACE_PF_F32R) return D3DFMT_R32F;
	if (Format == OAPISURFACE_PF_F32RG) return D3DFMT_G32R32F;
	if (Format == OAPISURFACE_PF_DXT1) return D3DFMT_DXT1;
	if (Format == OAPISURFACE_PF_F32RGBA) return D3DFMT_A32B32G32R32F;
	if (Format == OAPISURFACE_PF_F16RGBA) return D3DFMT_A16B16G16R16F;
	if (Format == OAPISURFACE_PF_ARGB) return D3DFMT_A8R8G8B8;
	if (Format == OAPISURFACE_PF_DXT3) return D3DFMT_DXT3;
	if (Format == OAPISURFACE_PF_DXT5) return D3DFMT_DXT5;
	if (Format == OAPISURFACE_PF_GRAY) return D3DFMT_L8;
	if (Format == OAPISURFACE_PF_ALPHA) return D3DFMT_A8;
	return 0;
}


// -----------------------------------------------------------------------------------------------
//
const char* NatUsage(DWORD Usage)
{
	static char buf[128];
	strcpy_s(buf, 128, "");
	if (Usage & D3DUSAGE_AUTOGENMIPMAP) strcat_s(buf, "AUTOGENMIPMAP ");
	if (Usage & D3DUSAGE_RENDERTARGET) strcat_s(buf, "RENDERTARGET ");
	if (Usage & D3DUSAGE_DYNAMIC) strcat_s(buf, "DYNAMIC ");
	if (Usage == 0) strcat_s(buf, "DEFAULT ");
	return buf;
}


// -----------------------------------------------------------------------------------------------
//
const char* NatPool(D3DPOOL Pool)
{
	static char buf[64];
	if (Pool == D3DPOOL_DEFAULT) strcpy_s(buf, 64, "D3DPOOL_DEFAULT");
	if (Pool == D3DPOOL_SYSTEMMEM) strcpy_s(buf, 64, "D3DPOOL_SYSTEMMEM");
	if (Pool == D3DPOOL_MANAGED) strcpy_s(buf, 64, "D3DPOOL_MANAGED");
	return buf;
}


// -----------------------------------------------------------------------------------------------
//
const char* NatOAPIFlags(DWORD AF)
{
	static char buf[512]; strcpy_s(buf, 512, "");

	if (AF & OAPISURFACE_TEXTURE)		strcat_s(buf, 512, "OAPISURFACE_TEXTURE, ");
	if (AF & OAPISURFACE_RENDERTARGET)	strcat_s(buf, 512, "OAPISURFACE_RENDERTARGET, ");
	if (AF & OAPISURFACE_GDI)			strcat_s(buf, 512, "OAPISURFACE_GDI, ");
	if (AF & OAPISURFACE_SKETCHPAD)		strcat_s(buf, 512, "OAPISURFACE_SKETCHPAD, ");
	if (AF & OAPISURFACE_MIPMAPS)		strcat_s(buf, 512, "OAPISURFACE_MIPMAPS, ");
	if (AF & OAPISURFACE_NOMIPMAPS)		strcat_s(buf, 512, "OAPISURFACE_NOMIPMAPS, ");
	if (AF & OAPISURFACE_ALPHA)			strcat_s(buf, 512, "OAPISURFACE_ALPHA, ");
	if (AF & OAPISURFACE_NOALPHA)		strcat_s(buf, 512, "OAPISURFACE_NOALPHA, ");
	if (AF & OAPISURFACE_UNCOMPRESS)	strcat_s(buf, 512, "OAPISURFACE_UNCOMPRESS, ");
	if (AF & OAPISURFACE_SYSMEM)		strcat_s(buf, 512, "OAPISURFACE_SYSMEM, ");
	if (AF & OAPISURFACE_ANTIALIAS)		strcat_s(buf, 512, "OAPISURFACE_ANTIALIAS, ");
	if (AF & OAPISURFACE_RENDER3D)		strcat_s(buf, 512, "OAPISURFACE_RENDER3D, ");
	return buf;
}


// -----------------------------------------------------------------------------------------------
//
const char* NatOAPIFormat(DWORD PF)
{
	static char buf[64];
	strcpy_s(buf, 64, "UNKNOWN");
	DWORD AF = PF & OAPISURFACE_PF_MASK;

	if (AF == OAPISURFACE_PF_XRGB)	strcpy_s(buf, 64, "OAPISURFACE_PF_XRGB ");
	if (AF == OAPISURFACE_PF_ARGB)	strcpy_s(buf, 64, "OAPISURFACE_PF_ARGB ");
	if (AF == OAPISURFACE_PF_RGB565)strcpy_s(buf, 64, "OAPISURFACE_PF_RGB565 ");
	if (AF == OAPISURFACE_PF_S16R)	strcpy_s(buf, 64, "OAPISURFACE_PF_S16R ");
	if (AF == OAPISURFACE_PF_F32R)	strcpy_s(buf, 64, "OAPISURFACE_PF_F32R ");
	if (AF == OAPISURFACE_PF_F32RG)	strcpy_s(buf, 64, "OAPISURFACE_PF_F32RG ");
	if (AF == OAPISURFACE_PF_F32RGBA)strcpy_s(buf, 64, "OAPISURFACE_PF_F32RGBA ");
	if (AF == OAPISURFACE_PF_F16R)	strcpy_s(buf, 64, "OAPISURFACE_PF_F16R ");
	if (AF == OAPISURFACE_PF_F16RG)	strcpy_s(buf, 64, "OAPISURFACE_PF_F16RG ");
	if (AF == OAPISURFACE_PF_F16RGBA)strcpy_s(buf, 64, "OAPISURFACE_PF_F16RGBA ");
	if (AF == OAPISURFACE_PF_DXT1)	strcpy_s(buf, 64, "OAPISURFACE_PF_DXT1 ");
	if (AF == OAPISURFACE_PF_DXT3)	strcpy_s(buf, 64, "OAPISURFACE_PF_DXT3 ");
	if (AF == OAPISURFACE_PF_DXT5)	strcpy_s(buf, 64, "OAPISURFACE_PF_DXT5 ");
	if (AF == OAPISURFACE_PF_ALPHA)	strcpy_s(buf, 64, "OAPISURFACE_PF_ALPHA ");
	if (AF == OAPISURFACE_PF_GRAY)	strcpy_s(buf, 64, "OAPISURFACE_PF_GRAY ");
	return buf;
}


// -----------------------------------------------------------------------------------------------
//
void NatDumpResource(LPDIRECT3DRESOURCE9 pResource)
{
	D3DSURFACE_DESC desc; memset(&desc, 0, sizeof(desc));

	static const char* sType[] = { "Unknown", "Surface", "Volume", "Texture", "3DTexture", "CubeTexture" };

	D3DRESOURCETYPE type = pResource->GetType();

	if (type == D3DRTYPE_SURFACE)
	{
		LPDIRECT3DSURFACE9 pSurf = static_cast<LPDIRECT3DSURFACE9>(pResource);
		pSurf->GetDesc(&desc);
	}
	else if (type == D3DRTYPE_TEXTURE)
	{
		LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(pResource);
		pTex->GetLevelDesc(0, &desc);
		oapiWriteLogV("DX9_DUMP: Mips = %d", pTex->GetLevelCount());
	}

	oapiWriteLogV("DX9_DUMP: Type = %s", sType[type]);

	if (type == D3DRTYPE_SURFACE || type == D3DRTYPE_TEXTURE)
	{
		DWORD f = NatConvertFormat_DX_to_OAPI(desc.Format);

		oapiWriteLogV("DX9_DUMP: Usage = %s", NatUsage(desc.Usage));
		oapiWriteLogV("DX9_DUMP: Pool = %s", NatPool(desc.Pool));
		oapiWriteLogV("DX9_DUMP: Format = %s (%u)", NatOAPIFormat(f), desc.Format);
		oapiWriteLogV("DX9_DUMP: Multisample = %u", desc.MultiSampleType);
		oapiWriteLogV("DX9_DUMP: Size = (%u, %u)", desc.Width, desc.Height);
	}
}
