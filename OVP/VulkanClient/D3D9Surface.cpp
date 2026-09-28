// ===========================================================================================
// D3D9Surface.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2011-2026 Jarmo Nikkanen
// ===========================================================================================

// STRICT left out: Win32 build switch

#include "D3D9Surface.h"
#include "D3D9Client.h"
#include "D3D9Config.h"
#include "D3D9Catalog.h"
#include "D3D9Util.h"
#include "D3D9Frame.h"
#include "AABBUtil.h"
#include "Log.h"
#include <QImage>
#include <QPainter>
#include <thread>

using namespace oapi;

extern D3D9Client* g_client;

// not upstream: usage every surface image gets (sampled, blit and readback), D3D9 allowed these per pool
static const VkImageUsageFlags NatUsageBase = VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT;

// not upstream: channel mapping of an OAPISURFACE_PF_* format (see NatConvertFormat_OAPI_to_DX)
static VkSwz NatSwizzle_OAPI(DWORD Format)
{
	Format &= OAPISURFACE_PF_MASK;
	if (Format == OAPISURFACE_PF_XRGB) return SWZ_NOALPHA;
	if (Format == OAPISURFACE_PF_S16R) return SWZ_LUM;
	if (Format == OAPISURFACE_PF_GRAY) return SWZ_LUM;
	if (Format == OAPISURFACE_PF_ALPHA) return SWZ_ALPHA;
	return SWZ_NONE;
}


void NatCheckFlags(DWORD &flags)
{
	// Append dependend flags
	if (flags & OAPISURFACE_RENDER3D) flags |= OAPISURFACE_RENDERTARGET;
	if (flags & OAPISURFACE_SKETCHPAD) flags |= OAPISURFACE_RENDERTARGET;

	if (flags & OAPISURFACE_RENDERTARGET) {
		if (flags & OAPISURFACE_GDI)
		{
			oapiWriteLog((char*)"OAPISURFACE_GDI is incomaptible with OAPISURFACE_RENDERTARGET and OAPISURFACE_SKETCHPAD");
			HALT();
		}
		if (flags & OAPISURFACE_SYSMEM)
		{
			oapiWriteLog((char*)"OAPISURFACE_SYSMEM is incomaptible with OAPISURFACE_RENDERTARGET and OAPISURFACE_SKETCHPAD");
			HALT();
		}
	}

	if (flags & OAPISURFACE_MIPMAPS) {
		if ((flags & OAPISURFACE_TEXTURE) == 0)
		{
			oapiWriteLog((char*)"OAPISURFACE_MIPMAPS can be only assigned to a OAPISURFACE_TEXTURE");
			HALT();
		}
		if (flags & OAPISURFACE_SYSMEM)
		{
			oapiWriteLog((char*)"OAPISURFACE_MIPMAPS is incomaptible with OAPISURFACE_SYSMEM");
			HALT();
		}
	}
}

VkTex *NatLoadSpecialTexture(const char* fname, const char* ext)
{
	char path[MAX_PATH];
	char name[MAX_PATH];

	NatCreateName(name, (int)std::size(name), fname, ext);

	VkTex *pTex = NULL;
	
	if (g_client->TexturePath(name, path)) {
		VkImageInfo info;
		if (VkGetImageInfoFromFile(path, &info)) {

			DWORD Mips = VKTEX_FROM_FILE;

			if (Config->TextureMips == 2) Mips = 0;                         // Autogen all
			if (Config->TextureMips == 1 && info.MipLevels == 1) Mips = 0;  // Autogen missing

			pTex = VkCreateTextureFromFile(g_client->GetDevice(), path, info.Width, info.Height, Mips, VK_FORMAT_UNDEFINED, SWZ_NONE, NatUsageBase);
			if (pTex) return pTex;
		}
	}
	return NULL;
}



// ======================================================================================
// Main loading routine
//
SURFHANDLE NatLoadSurface(const char* file, DWORD flags, bool bPath)
{
	VkTex *pTex = NULL;
	SurfNative* pNat = NULL;

	NatCheckFlags(flags);

	char path[MAX_PATH];

	if (bPath) strcpy(path, file);
	else {
		if (!g_client->TexturePath(file, path)) {
			return NULL;
		}
	}
	
	DWORD pass = OAPISURFACE_TEXTURE | OAPISURFACE_SHARED;

	// Load regular texture with additional maps if exists
	//
	if ((flags & ~pass) == 0)
	{
		VkImageInfo info;

		if (VkGetImageInfoFromFile(path, &info))
		{

			if (info.ImageFileFormat == VKIFF_JPG) { info.Format = VK_FORMAT_B8G8R8A8_UNORM; info.Swizzle = SWZ_NOALPHA; }
			if (info.ImageFileFormat == VKIFF_PNG) { info.Format = VK_FORMAT_B8G8R8A8_UNORM; info.Swizzle = SWZ_NONE; }
			if (info.ImageFileFormat == VKIFF_BMP) { info.Format = VK_FORMAT_B8G8R8A8_UNORM; info.Swizzle = SWZ_NOALPHA; }

			DWORD Mips = VKTEX_FROM_FILE;
			if (Config->TextureMips == 2) Mips = 0;                         // Autogen all
			if (Config->TextureMips == 1 && info.MipLevels == 1) Mips = 0;  // Autogen missing

			if ((pTex = VkCreateTextureFromFile(g_client->GetDevice(), path, info.Width, info.Height, Mips, info.Format, info.Swizzle, NatUsageBase)))
			{
				pNat = new SurfNative(pTex, flags);
				pNat->SetName(file);

				LogBlu("TextureLoaded [%s] PLAIN Mips=%u Format=%u (%u,%u) Flags=0x%X, %s", file, pTex->levels, (UINT)info.Format, info.Width, info.Height, flags, _PTR(pNat));

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
			else oapiWriteLogV("FAILED: NatLoadSurface(%s)", path);
		}
		else oapiWriteLogV("FAILED: NatLoadSurface(%s)", path);

		return SURFHANDLE(pNat);
	}


	// Load more complex surface
	//
	VkImageInfo info;

	if (VkGetImageInfoFromFile(path, &info))
	{
		if (flags & OAPISURFACE_SKETCHPAD) flags |= OAPISURFACE_RENDERTARGET;
		if (flags & OAPISURFACE_RENDERTARGET) flags |= OAPISURFACE_UNCOMPRESS;

		DWORD Mips = VKTEX_FROM_FILE;
		VkImageUsageFlags Usage = NatUsageBase;
		VkFormat Format = info.Format;
		VkSwz Swz = info.Swizzle;
		// Pool (SYSTEMMEM) and lockability: the surface flags say it, see SurfNative's desc

		// File Formats Not Supported
		// (A4R4G4B4 and X4R4G4B4 files are expanded to 8 bits per channel by the loader)

		// Predict the surface format
		//
		if (info.ImageFileFormat == VKIFF_JPG) { Format = VK_FORMAT_B8G8R8A8_UNORM; Swz = SWZ_NOALPHA; }
		if (info.ImageFileFormat == VKIFF_PNG) { Format = VK_FORMAT_B8G8R8A8_UNORM; Swz = SWZ_NOALPHA; }
		if (info.ImageFileFormat == VKIFF_BMP) { Format = VK_FORMAT_B8G8R8A8_UNORM; Swz = SWZ_NOALPHA; }

		if (flags & OAPISURFACE_UNCOMPRESS)
		{
			VkSwz Fs = (flags & OAPISURFACE_ALPHA) ? SWZ_NONE : SWZ_NOALPHA;
			if (info.Format == VK_FORMAT_BC1_RGBA_UNORM_BLOCK) { Format = VK_FORMAT_B8G8R8A8_UNORM; Swz = Fs; }
			if (info.Format == VK_FORMAT_BC3_UNORM_BLOCK) { Format = VK_FORMAT_B8G8R8A8_UNORM; Swz = Fs; }
			if (info.Format == VK_FORMAT_BC2_UNORM_BLOCK) { Format = VK_FORMAT_B8G8R8A8_UNORM; Swz = Fs; }
		}

		// User defined format
		//
		VkFormat Fmt = VkFormat(NatConvertFormat_OAPI_to_DX(flags));
		if (Fmt != 0) { Format = Fmt; Swz = NatSwizzle_OAPI(flags); }

		if (flags & OAPISURFACE_RENDERTARGET) Usage |= VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
		if (flags & OAPISURFACE_NOMIPMAPS) Mips = 1;
		if (flags & OAPISURFACE_MIPMAPS) Mips = 0;

		if (flags & OAPISURFACE_TEXTURE)
		{
			if ((pTex = VkCreateTextureFromFile(g_client->GetDevice(), path, info.Width, info.Height, Mips, Format, Swz, Usage)))
			{
				SurfNative* pSrf = new SurfNative(pTex, flags);
				pSrf->SetName(file);
				LogBlu("TextureLoaded [%s] Mips=%u, Usage=%u, Format=%u (%u,%u) Flags=0x%X, %s", file, pTex->levels, Usage, (UINT)Format, info.Width, info.Height, flags, _PTR(pSrf));
				return SURFHANDLE(pSrf);
			}
			return NULL;
		}

		if (flags & OAPISURFACE_RENDERTARGET)
		{
			VkSurf *pSurf = new VkSurf(g_client->GetDevice(), info.Width, info.Height, Format, Usage); // CreateRenderTarget
			VkPixels px;
			if (pSurf->tex->img && VkLoadPixels(path, px) && VkLoadTextureLevel(pSurf->tex, 0, 0, px)) // D3DXLoadSurfaceFromFile
			{
				if (Swz != SWZ_NONE) pSurf->tex->SetSwizzle(VkSwizzleMap(Swz));
				SurfNative* pSrf = new SurfNative(pSurf, flags);
				pSrf->SetName(file);
				LogBlu("SurfaceLoaded [%s] RENDERTARGET Format=%u (%u,%u) Flags=0x%X %s", file, (UINT)Format, info.Width, info.Height, flags, _PTR(pSrf));
				return SURFHANDLE(pSrf);
			}
			delete pSurf;
		}
	}

	return NULL;
}



// ===============================================================================================
//
bool NatSaveSurface(const char* file, VkTex *pResource)
{
	VkDev *pDev = g_client->GetDevice();
	
	VkImageFileFormat fmt = VkImageFileFormat(0);

	if (contains(file, ".dds")) fmt = VKIFF_DDS;
	if (contains(file, ".bmp")) fmt = VKIFF_BMP;
	if (contains(file, ".jpg")) fmt = VKIFF_JPG;
	if (contains(file, ".png")) fmt = VKIFF_PNG;

	// surfaces and textures (render target or not) alike: read the image back (GetRenderTargetData) and write it
	VkPixels px;
	if (VkReadPixels(pDev, pResource, px, fmt == VKIFF_DDS ? 0 : 1))
	{
		px.swz = VkSwizzleOf(pResource->swizzle);
		if (VkSavePixels(file, fmt, px)) return true;
	}

	oapiWriteLog((char*)"NatSaveSurface():");
	NatDumpResource(pResource);
	return false;
}



// ===============================================================================================
//
SURFHANDLE NatCreateSurface(int width, int height, DWORD flags)
{
	DWORD Mips = 1;
	VkImageUsageFlags Usage = NatUsageBase;
	VkSampleCountFlagBits Multi = VK_SAMPLE_COUNT_1_BIT;
	VkDev *pDev = g_client->GetDevice();

	NatCheckFlags(flags);
	
	if (flags & OAPISURFACE_RENDERTARGET) Usage |= VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
	// OAPISURFACE_GDI (D3DUSAGE_DYNAMIC) and OAPISURFACE_SYSMEM (D3DPOOL_SYSTEMMEM) are kept in the surface flags
	if (flags & OAPISURFACE_NOMIPMAPS) Mips = 1;

	if (flags & OAPISURFACE_MIPMAPS)
	{
		Mips = 0;
		// D3DUSAGE_AUTOGENMIPMAP: the surface flags say it, GenerateMipMaps blits the chain
	}

	if (flags & OAPISURFACE_ANTIALIAS) { // D3DMULTISAMPLE_8_SAMPLES, or the most the device has
		VkSampleCountFlags sc = pDev->props.limits.framebufferColorSampleCounts & pDev->props.limits.framebufferDepthSampleCounts;
		for (int s = 8; s > 1; s >>= 1) if (sc & s) { Multi = (VkSampleCountFlagBits)s; break; }
	}

	VkFormat Format = VkFormat(NatConvertFormat_OAPI_to_DX(flags));
	VkSwz Swz = NatSwizzle_OAPI(flags);

	if (Format == 0)
	{
		Format = VK_FORMAT_B8G8R8A8_UNORM;
		Swz = (flags & OAPISURFACE_ALPHA) ? SWZ_NONE : SWZ_NOALPHA; // A8R8G8B8 : X8R8G8B8
	}


	if ((flags & OAPISURFACE_TEXTURE) || (flags & OAPISURFACE_SYSMEM) || (flags & OAPISURFACE_GDI))
	{
		VkTex *pTex = new VkTex(pDev, width, height, Mips, Format, Usage);
		VkSurf *pDepth = NULL;

		if (pTex->img)
		{
			if (Swz != SWZ_NONE) pTex->SetSwizzle(VkSwizzleMap(Swz));
			if (flags & OAPISURFACE_RENDER3D)
			{
				pDepth = new VkSurf(pDev, width, height, g_client->GetFramework()->GetDepthFormat(), VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT);
			}
			SurfNative *pNat = new SurfNative(pTex, flags, pDepth);
			if (!pNat->IsCompressed()) pDev->ColorFill(pNat->GetSurface(), NULL, 0); // not upstream: new images start black, not undefined
			return SURFHANDLE(pNat);
		}
		delete pTex;
	}


	if (flags & OAPISURFACE_RENDERTARGET)
	{
		if (Multi != VK_SAMPLE_COUNT_1_BIT) Usage &= ~VK_IMAGE_USAGE_SAMPLED_BIT; // D3D9 render target surfaces aren't read by shaders
		VkSurf *pSurf = new VkSurf(pDev, width, height, Format, Usage, Multi); // CreateRenderTarget
		VkSurf *pDepth = NULL;

		if (pSurf->tex->img)
		{
			if (Swz != SWZ_NONE) pSurf->tex->SetSwizzle(VkSwizzleMap(Swz));
			if (flags & OAPISURFACE_RENDER3D)
			{
				pDepth = new VkSurf(pDev, width, height, g_client->GetFramework()->GetDepthFormat(), VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, Multi);
			}
			SurfNative *pNat = new SurfNative(pSurf, flags, pDepth);
			pDev->ColorFill(pNat->GetSurface(), NULL, 0); // not upstream: new images start black, not undefined
			return SURFHANDLE(pNat);
		}
		delete pSurf;
	}
	assert(false);
	return NULL;
}


// ===============================================================================================
//
SURFHANDLE NatGetMipSublevel(SURFHANDLE hSrf, int level)
{
	static const DWORD fl = OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE;

	if ((SURFACE(hSrf)->Flags & fl) == fl)
	{
		VkTex *pTex = SURFACE(hSrf)->GetTexture();
		if (pTex && (UINT)level < pTex->levels) {
			VkSurf *pSurf = new VkSurf(pTex, level, 0); // GetSurfaceLevel
			SurfNative* pNat = new SurfNative(pSurf, OAPISURFACE_RENDERTARGET, NULL);
			return SURFHANDLE(pNat);
		}
	}
	else {
		LogErr("NatGetMipSublevel() Surface is not a rendertarget-texture. Handle = %s", _PTR(hSrf));
	}
	return NULL;
}


// ===============================================================================================
//
SURFHANDLE NatCompressSurface(SURFHANDLE hSurface, DWORD flags)
{
	VkTex *pTex = NULL;
	VkDev *pDev = g_client->GetDevice();
	VkTex *pResource = SURFACE(hSurface)->GetResource();

	DWORD Mips = 1;
	VkFormat Fmt = VK_FORMAT_BC1_RGBA_UNORM_BLOCK;

	if (flags & OAPISURFACE_MIPMAPS) Mips = 0;
	if ((flags & OAPISURFACE_PF_MASK) == OAPISURFACE_PF_DXT1) Fmt = VK_FORMAT_BC1_RGBA_UNORM_BLOCK;
	if ((flags & OAPISURFACE_PF_MASK) == OAPISURFACE_PF_DXT3) Fmt = VK_FORMAT_BC2_UNORM_BLOCK;
	if ((flags & OAPISURFACE_PF_MASK) == OAPISURFACE_PF_DXT5) Fmt = VK_FORMAT_BC3_UNORM_BLOCK;
	// OAPISURFACE_SYSMEM (D3DPOOL_SYSTEMMEM) stays a surface flag

	// surface or texture: its top level, box filtered into the mip chain (D3DXLoadSurfaceFromSurface, D3DX_FILTER_BOX)
	VkPixels px, cv;
	if (pResource && VkReadPixels(pDev, pResource, px, 1))
	{
		px.swz = VkSwizzleOf(pResource->swizzle);
		if (VkConvertPixels(px, cv, Fmt, SWZ_NONE, px.w, px.h, Mips) && (pTex = VkCreateTexture(pDev, cv, NatUsageBase)))
		{
			return new SurfNative(pTex, flags);
		}
	}

	return NULL;
}


// ===============================================================================================
//
bool NatGenerateMipmaps(SURFHANDLE hSrf)
{
	VkTex *pTex = SURFACE(hSrf)->GetTexture();
	if (!pTex) return false;

	DWORD nMip = pTex->levels;
	if (nMip <= 1) return false;

	VkDev *pDev = g_client->GetDevice();

	for (DWORD i = 1; i < nMip; i++) {
		VkSurf pHigh(pTex, i - 1), pLow(pTex, i); // GetSurfaceLevel
		pDev->StretchRect(&pHigh, NULL, &pLow, NULL, VK_FILTER_LINEAR);
	}
	return true;
}








// -----------------------------------------------------------------------------------------------
//
SurfNative::SurfNative(VkTex *pRes, DWORD flags, VkSurf *_pDepth) :
	pResource(pRes),
	pSurface(NULL),
	pSkp(NULL),
	pTemp(NULL),
	pDepth(_pDepth),
	hOrigin(this),
	pTexSurf(NULL),
	pDevice(g_client->GetDevice()),
	ColorKey(SURF_NO_CK),
	Flags(flags),
	type(NATTYPE_TEXTURE),
	Mipmaps(1),
	RefCount(1),
	ClientFlags(0)
{

	assert(pRes != NULL);
	assert(std::this_thread::get_id() == g_client->GetMainThread());

	SurfaceCatalog.insert(this);

	memset(pMap, 0, sizeof(pMap));
	memset(&desc, 0, sizeof(desc));
	memset(&DC, 0, sizeof(DC));

	strcpy(name, "null");

	// GetLevelDesc(0) → the image's own description, pool and dynamic usage from the flags it was made with
	desc.Width = pRes->w;
	desc.Height = pRes->h;
	desc.Format = pRes->fmt;
	desc.Swizzle = VkSwizzleOf(pRes->swizzle);
	desc.Usage = pRes->usage;
	desc.RenderTarget = (pRes->usage & VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) != 0;
	desc.Dynamic = (flags & OAPISURFACE_GDI) != 0;
	desc.AutoGenMipMap = (flags & OAPISURFACE_MIPMAPS) != 0;
	desc.SysMem = (flags & OAPISURFACE_SYSMEM) != 0;
	desc.MultiSampleType = pRes->samples;
	Mipmaps = pRes->levels;
	pRes->autoGenMips = desc.AutoGenMipMap;
}


// -----------------------------------------------------------------------------------------------
// not upstream: the surface resource half of the constructor above (D3DRTYPE_SURFACE)
//
SurfNative::SurfNative(VkSurf *pSrf, DWORD flags, VkSurf *_pDepth) :
	pResource(NULL),
	pSurface(pSrf),
	pSkp(NULL),
	pTemp(NULL),
	pDepth(_pDepth),
	hOrigin(this),
	pTexSurf(NULL),
	pDevice(g_client->GetDevice()),
	ColorKey(SURF_NO_CK),
	Flags(flags),
	type(NATTYPE_SURFACE),
	Mipmaps(1),
	RefCount(1),
	ClientFlags(0)
{
	assert(pSrf != NULL);
	assert(std::this_thread::get_id() == g_client->GetMainThread());

	SurfaceCatalog.insert(this);

	memset(pMap, 0, sizeof(pMap));
	memset(&desc, 0, sizeof(desc));
	memset(&DC, 0, sizeof(DC));

	strcpy(name, "null");

	desc.Width = pSrf->w;
	desc.Height = pSrf->h;
	desc.Format = pSrf->tex->fmt;
	desc.Swizzle = VkSwizzleOf(pSrf->tex->swizzle);
	desc.Usage = pSrf->tex->usage;
	desc.RenderTarget = (pSrf->tex->usage & VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) != 0;
	desc.Dynamic = (flags & OAPISURFACE_GDI) != 0;
	desc.SysMem = (flags & OAPISURFACE_SYSMEM) != 0;
	desc.MultiSampleType = pSrf->tex->samples;
}


// -----------------------------------------------------------------------------------------------
//
SurfNative::SurfNative(SurfNative* pOrigin)
{
	pResource = pOrigin->pResource;
	pSurface = pOrigin->pSurface;
	pSkp = NULL;
	pTemp = NULL;
	pDepth = pOrigin->pDepth;
	hOrigin = pOrigin;
	pTexSurf = pOrigin->pTexSurf;
	pDevice = g_client->GetDevice();
	ColorKey = pOrigin->ColorKey;
	Flags = pOrigin->Flags;
	type = pOrigin->type;
	Mipmaps = pOrigin->Mipmaps;
	RefCount = 1;
	ClientFlags = 0;
	desc = pOrigin->desc;
	memset(&DC, 0, sizeof(DC));

	for (int i = 0; i < MAP_MAX_COUNT; i++) pMap[i] = pOrigin->pMap[i];

	strcpy(name, pOrigin->name);
}


// -----------------------------------------------------------------------------------------------
//
SurfNative::~SurfNative()
{
	if (SurfaceCatalog.erase(this) != 1) assert(false);

	if (hOrigin == this)
	{
		for (int i = 0; i < MAP_MAX_COUNT; i++) SAFE_DELETE(pMap[i]);

		if (!(Flags & OAPISURFACE_BACKBUFFER))
		{
			SAFE_DELETE(pResource);
			SAFE_DELETE(pSurface);
			SAFE_DELETE(pDepth);
		}
		SAFE_DELETE(pTexSurf);
	}

	SAFE_DELETE(pTemp);
	if (DC.hDC) { DC.hDC->end(); delete DC.hDC; }
	SAFE_DELETE(DC.pSrf);
	SAFE_DELETE(pSkp);
}


// -----------------------------------------------------------------------------------------------
//
void SurfNative::AddMap(DWORD id, VkTex *_pMap)
{
	if (id >= MAP_MAX_COUNT) return;
	SAFE_DELETE(pMap[id]);
	pMap[id] = _pMap;
	Flags |= OAPISURFACE_MAPS;
}


// -----------------------------------------------------------------------------------------------
//
VkTex *SurfNative::GetTexture() const
{
	if (type == NATTYPE_TEXTURE) return pResource;
	return NULL;
}


// -----------------------------------------------------------------------------------------------
//
VkSurf *SurfNative::GetTempSurface()
{
	if (pTemp) return pTemp;
	pTemp = new VkSurf(pDevice, desc.Width, desc.Height, desc.Format, NatUsageBase | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT); // CreateRenderTarget
	return pTemp;
}


// -----------------------------------------------------------------------------------------------
//
VkSurf *SurfNative::GetSurface()
{
	if (type == NATTYPE_SURFACE) return pSurface;

	if (type == NATTYPE_TEXTURE)
	{
		if (!pTexSurf)
		{
			pTexSurf = new VkSurf(pResource, 0, 0); // GetSurfaceLevel(0)
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
	if (desc.SysMem) return true;
	if (desc.Dynamic) return true;
	return false;
}

// -----------------------------------------------------------------------------------------------
//
bool SurfNative::IsRenderTarget() const
{
	if (Flags & OAPISURFACE_BACKBUFFER) return true;
	if (!desc.SysMem && desc.RenderTarget) return true;
	return false;
}


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::Is3DRenderTarget() const
{
	return (pDepth != NULL) && ((Flags & OAPISURFACE_RENDER3D) != 0);
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
	if (desc.Format == VK_FORMAT_BC1_RGBA_UNORM_BLOCK) return true;
	if (desc.Format == VK_FORMAT_BC1_RGB_UNORM_BLOCK) return true;
	if (desc.Format == VK_FORMAT_BC2_UNORM_BLOCK) return true; // DXT2, DXT3
	if (desc.Format == VK_FORMAT_BC3_UNORM_BLOCK) return true; // DXT4, DXT5
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
	snprintf(name, 128, "%s", n);
	int i = -1;
	while (name[++i] != 0) if (name[i] == '/') name[i] = '\\';
}


// -----------------------------------------------------------------------------------------------
// GetGDICache left out (see D3D9Surface.h)


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::GetSpecs(gcCore::SurfaceSpecs* sp, int size)
{
	if (size == sizeof(gcCore::SurfaceSpecs))
	{
		sp->Flags = Flags;
		sp->Width = desc.Width;
		sp->Height = desc.Height;
		sp->Mips = Mipmaps;
		return true;
	}
	return false;
}


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::GenerateMipMaps()
{
	if (type == NATTYPE_TEXTURE)
	{
		VkTex *pTex = pResource;

		if (desc.AutoGenMipMap && desc.RenderTarget) {
			if (!pDevice->IsRecording()) return false;
			pDevice->EndRendering();
			pTex->GenerateMips(pDevice->Cmd()); // GenerateMipSubLevels
			pTex->mipsDirty = false;
			return true;
		}
		else {
			
			DWORD nMip = pTex->levels;
			if (nMip <= 1) return false;

			for (DWORD i = 1; i < nMip; i++) {
				VkSurf pHigh(pTex, i - 1), pLow(pTex, i); // GetSurfaceLevel
				pDevice->StretchRect(&pHigh, NULL, &pLow, NULL, VK_FILTER_LINEAR);
			}
			return true;

		}
	}
	return false;
}


// -----------------------------------------------------------------------------------------------
// CreateDX7 and DX7Sync left out (see D3D9Surface.h)


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::Fill(LPRECT rect, DWORD c)
{
	
	LPRECT r;
	RECT re;

	if (rect==NULL) { re.left=0, re.top=0, re.right=desc.Width, re.bottom=desc.Height; r=&re; }
	else r = rect;

	if (!desc.SysMem)
	{
		if (desc.RenderTarget)
		{
			VkSurf *pSrf = GetSurface();
			pDevice->ColorFill(pSrf, r, c);
			return true;
		}
	}

	if (IsGDISurface())
	{
		QPainter *hDC = SurfNative::GetDC();
		if (hDC) {
			hDC->fillRect(r->left, r->top, r->right - r->left, r->bottom - r->top, QColor(c & 0xFF, (c >> 8) & 0xFF, (c >> 16) & 0xFF)); // CreateSolidBrush takes a COLORREF
			SurfNative::ReleaseDC(hDC);
			return true;
		}
	}

	LogErr("ColorFill Failed");
	LogSpecs();
	HALT();
	return false;
}


// -----------------------------------------------------------------------------------------------
//
QPainter *SurfNative::GetDC()
{
	if (!DC.hDC)
	{
		// OAPISURFACE_CAPTURE: D3D9 returned NULL while the captured copy was still in flight; the readback below waits for it
		// GDI, render target and plain surfaces alike: the painter draws on a CPU copy that ReleaseDC uploads
		VkSurf *pSrf = GetSurface();
		VkPixels px, cv;
		if (pSrf && VkReadPixels(pDevice, pSrf->tex, px, pSrf->level + 1))
		{
			VkPixels lv;
			lv.w = pSrf->w; lv.h = pSrf->h; lv.levels = 1; lv.layers = 1; lv.fmt = px.fmt; lv.swz = desc.Swizzle;
			lv.data.assign(1, px.Level(pSrf->level, pSrf->layer));
			if (VkConvertPixels(lv, cv, VK_FORMAT_B8G8R8A8_UNORM, SWZ_NONE, lv.w, lv.h, 1))
			{
				QImage::Format f = (desc.Swizzle == SWZ_NOALPHA || desc.Format == VK_FORMAT_R5G6B5_UNORM_PACK16) ? QImage::Format_RGB32 : QImage::Format_ARGB32;
				DC.pSrf = new QImage(lv.w, lv.h, f);
				for (UINT y = 0; y < lv.h; y++) memcpy(DC.pSrf->scanLine(y), &cv.Level(0)[(size_t)y * lv.w * 4], (size_t)lv.w * 4);
				if (f == QImage::Format_RGB32) for (UINT y = 0; y < lv.h; y++) { QRgb *p = (QRgb *)DC.pSrf->scanLine(y); for (UINT x = 0; x < lv.w; x++) p[x] |= 0xFF000000; }
				DC.hDC = new QPainter(DC.pSrf);
				return DC.hDC;
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
void SurfNative::ReleaseDC(QPainter *_hDC)
{
	if (!_hDC) return;

	assert(_hDC == DC.hDC);

	DC.hDC->end();
	delete DC.hDC;

	// upload the painted copy (the DX7 copy's StretchRect back upstream)
	VkSurf *pSrf = GetSurface();
	VkPixels px;
	px.w = DC.pSrf->width(); px.h = DC.pSrf->height(); px.levels = 1; px.layers = 1;
	px.fmt = VK_FORMAT_B8G8R8A8_UNORM;
	px.swz = SWZ_NONE;
	px.data.assign(1, std::vector<BYTE>((size_t)px.w * px.h * 4));
	for (UINT y = 0; y < px.h; y++) memcpy(&px.data[0][(size_t)y * px.w * 4], DC.pSrf->constScanLine(y), (size_t)px.w * 4);
	if (pSrf) VkLoadTextureLevel(pSrf->tex, pSrf->level, pSrf->layer, px);

	delete DC.pSrf;
	DC.pSrf = NULL;
	DC.hDC = NULL;	
}



// -----------------------------------------------------------------------------------------------
//
bool SurfNative::Decompress()
{
	if (IsCompressed())
	{
		VkTex *pDecomp = NULL;
		VkFormat Format = VK_FORMAT_UNDEFINED;
		VkSwz Swz = SWZ_NONE;

		if (desc.Format == VK_FORMAT_BC1_RGBA_UNORM_BLOCK) { Format = VK_FORMAT_B8G8R8A8_UNORM; Swz = SWZ_NOALPHA; }
		if (desc.Format == VK_FORMAT_BC3_UNORM_BLOCK) Format = VK_FORMAT_B8G8R8A8_UNORM;
		if (desc.Format == VK_FORMAT_BC2_UNORM_BLOCK) Format = VK_FORMAT_B8G8R8A8_UNORM;

		char path[MAX_PATH];

		if (!g_client->TexturePath(name, path)) {
			oapiWriteLogV("SurfNative::Reload() File Not Found [%s]", path);
			return false;
		}

		if ((pDecomp = VkCreateTextureFromFile(pDevice, path, desc.Width, desc.Height, Mipmaps, Format, Swz, NatUsageBase | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)))
		{
			SAFE_DELETE(pTexSurf);
			SAFE_DELETE(pResource);

			pResource = pDecomp;
			pTexSurf = new VkSurf(pDecomp, 0, 0);

			desc.Width = pDecomp->w;
			desc.Height = pDecomp->h;
			desc.Format = pDecomp->fmt;
			desc.Swizzle = VkSwizzleOf(pDecomp->swizzle);
			desc.Usage = pDecomp->usage;
			desc.RenderTarget = true;
			desc.Dynamic = desc.SysMem = false;
			Mipmaps = pDecomp->levels;
			type = NATTYPE_TEXTURE;
			Flags = OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE;
			return true;		
			
		}

		LogSpecs();
		return false;
	}

	return true;
}


// -----------------------------------------------------------------------------------------------
//
bool SurfNative::DeClone()
{
	char path[MAX_PATH];

	if (!IsClone()) return false;
	else
	{
		LogWrn("DeCloning Surface [%s] Handle=%s", name, _PTR(this));

		assert(pTemp == NULL);
		assert(DC.hDC == NULL);

		VkFormat Format = VK_FORMAT_UNDEFINED;
		VkSwz Swz = SWZ_NONE;
		VkTex *pTex = NULL;

		// Decompress
		if (desc.Format == VK_FORMAT_BC1_RGBA_UNORM_BLOCK) { Format = VK_FORMAT_B8G8R8A8_UNORM; Swz = SWZ_NOALPHA; }
		if (desc.Format == VK_FORMAT_BC3_UNORM_BLOCK) Format = VK_FORMAT_B8G8R8A8_UNORM;
		if (desc.Format == VK_FORMAT_BC2_UNORM_BLOCK) Format = VK_FORMAT_B8G8R8A8_UNORM;

		if (!g_client->TexturePath(name, path)) {
			oapiWriteLogV("SurfNative::DeClone() File Not Found [%s]", path);
			return false;
		}

		if ((pTex = VkCreateTextureFromFile(pDevice, path, desc.Width, desc.Height, Mipmaps, Format, Swz, NatUsageBase | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)))
		{
			pResource = pTex;
			pSurface = NULL;
			hOrigin = this;

			pTexSurf = new VkSurf(pTex, 0, 0);
			desc.Width = pTex->w;
			desc.Height = pTex->h;
			desc.Format = pTex->fmt;
			desc.Swizzle = VkSwizzleOf(pTex->swizzle);
			desc.Usage = pTex->usage;
			desc.RenderTarget = true;
			desc.Dynamic = desc.SysMem = false;
			Mipmaps = pTex->levels;
			type = NATTYPE_TEXTURE;
			Flags = OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE;
			return true;
		}
	}
	
	LogErr("DeClone Failed");
	LogSpecs();
	return false;
}

// -----------------------------------------------------------------------------------------------
//
void SurfNative::Reload()
{	
	SAFE_DELETE(pTexSurf);
	SAFE_DELETE(pResource);
	for (int i = 0; i < (int)std::size(pMap); i++) SAFE_DELETE(pMap[i]);

	char path[MAX_PATH];

	if (!g_client->TexturePath(name, path)) {
		oapiWriteLogV("SurfNative::Reload() File Not Found [%s]", path);
		return;
	}

	if (Flags == OAPISURFACE_TEXTURE)
	{
		VkImageInfo info;

		if (VkGetImageInfoFromFile(path, &info))
		{
			DWORD Mips = VKTEX_FROM_FILE;
			if (Config->TextureMips == 2) Mips = 0;                         // Autogen all
			if (Config->TextureMips == 1 && info.MipLevels == 1) Mips = 0;  // Autogen missing

			if ((pResource = VkCreateTextureFromFile(g_client->GetDevice(), path, info.Width, info.Height, Mips,
				VK_FORMAT_UNDEFINED, SWZ_NONE, NatUsageBase)))
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
	if (pTemp) LogErr("Has a In-surface temp layer");
	if (pDepth) LogErr("Has a DepthStencil surface");
	if (DC.hDC) LogErr("Has a HDC [%s]", _PTR(DC.hDC));
	LogErr("OAPI_Attribs: %s", NatOAPIFlags(Flags));
	NatDumpResource(GetResource());
}



// -----------------------------------------------------------------------------------------------
//
DWORD SurfNative::GetTextureSizeInBytes(VkTex *pT)
{
	if (!pT) return 0;
	DWORD size = GetFormatSizeInBytes(pT->fmt, pT->h * pT->w);
	if (pT->levels > 1) size += ((size>>2) + (size>>4) + (size>>6));
	return size;
}

// -----------------------------------------------------------------------------------------------
//
DWORD SurfNative::GetFormatSizeInBytes(VkFormat Format, DWORD pixels)
{
	if (Format == VK_FORMAT_BC1_RGBA_UNORM_BLOCK) return pixels >> 1;
	if (Format == VK_FORMAT_BC2_UNORM_BLOCK) return pixels;
	if (Format == VK_FORMAT_BC3_UNORM_BLOCK) return pixels;
	return pixels * VkFormatBlockSize(Format); // A8R8G8B8, R5G6B5, L16, R16F, G16R16F, R32F, G32R32F, L8, A8, ...
}


// -----------------------------------------------------------------------------------------------
//
DWORD SurfNative::GetSizeInBytes()
{
	if (type == NATTYPE_SURFACE) return GetFormatSizeInBytes(desc.Format, desc.Height * desc.Width);
	if (type == NATTYPE_TEXTURE)
	{
		DWORD size = GetTextureSizeInBytes(pResource);
		for (int i = 0; i < MAP_MAX_COUNT; i++)  size += GetTextureSizeInBytes(pMap[i]);
		return size;
	}
	return 0;
}


// -----------------------------------------------------------------------------------------------
//
DWORD* SurfNative::GetClientFlags()
{
	return &ClientFlags;
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
	snprintf(buffe, MAX_PATH, "%s", fname);
	char* p = strrchr(buffe, '.');
	if (p != NULL) {
		*p = '\0';
		snprintf(out, mlen, "%s_%s.%s", buffe, id, ++p);
	}
	return (p != NULL);
}


// -----------------------------------------------------------------------------------------------
//
DWORD NatConvertFormat_DX_to_OAPI(DWORD Format, VkSwz swz)
{
	DWORD Out = OAPISURFACE_NOALPHA;
	if (Format == VK_FORMAT_B8G8R8A8_UNORM && swz == SWZ_NOALPHA) return Out | OAPISURFACE_PF_XRGB;
	if (Format == VK_FORMAT_R5G6B5_UNORM_PACK16) return Out | OAPISURFACE_PF_RGB565;
	if (Format == VK_FORMAT_R16_UNORM) return Out | OAPISURFACE_PF_S16R;
	if (Format == VK_FORMAT_R16_SFLOAT) return Out | OAPISURFACE_PF_F16R;
	if (Format == VK_FORMAT_R16G16_SFLOAT) return Out | OAPISURFACE_PF_F16RG;
	if (Format == VK_FORMAT_R32_SFLOAT) return Out | OAPISURFACE_PF_F32R;
	if (Format == VK_FORMAT_R32G32_SFLOAT) return Out | OAPISURFACE_PF_F32RG;
	if (Format == VK_FORMAT_BC1_RGBA_UNORM_BLOCK) return Out | OAPISURFACE_PF_DXT1;
	if (Format == VK_FORMAT_R8_UNORM && swz != SWZ_ALPHA) return Out | OAPISURFACE_PF_GRAY;

	Out = OAPISURFACE_ALPHA;
	if (Format == VK_FORMAT_R8_UNORM) return Out | OAPISURFACE_PF_ALPHA;
	if (Format == VK_FORMAT_R32G32B32A32_SFLOAT) return Out | OAPISURFACE_PF_F32RGBA;
	if (Format == VK_FORMAT_R16G16B16A16_SFLOAT) return Out | OAPISURFACE_PF_F16RGBA;
	if (Format == VK_FORMAT_B8G8R8A8_UNORM) return Out | OAPISURFACE_PF_ARGB;
	if (Format == VK_FORMAT_BC2_UNORM_BLOCK) return Out | OAPISURFACE_PF_DXT3;
	if (Format == VK_FORMAT_BC3_UNORM_BLOCK) return Out | OAPISURFACE_PF_DXT5;
	return 0;
}


// -----------------------------------------------------------------------------------------------
// returns a VkFormat (D3DFORMAT upstream); the channel mapping comes from NatSwizzle_OAPI
//
DWORD NatConvertFormat_OAPI_to_DX(DWORD Format)
{
	Format &= OAPISURFACE_PF_MASK;

	if (Format == OAPISURFACE_PF_XRGB) return VK_FORMAT_B8G8R8A8_UNORM;
	if (Format == OAPISURFACE_PF_ARGB) return VK_FORMAT_B8G8R8A8_UNORM;
	if (Format == OAPISURFACE_PF_RGB565) return VK_FORMAT_R5G6B5_UNORM_PACK16;
	if (Format == OAPISURFACE_PF_S16R) return VK_FORMAT_R16_UNORM;
	if (Format == OAPISURFACE_PF_F16R) return VK_FORMAT_R16_SFLOAT;
	if (Format == OAPISURFACE_PF_F16RG) return VK_FORMAT_R16G16_SFLOAT;
	if (Format == OAPISURFACE_PF_F32R) return VK_FORMAT_R32_SFLOAT;
	if (Format == OAPISURFACE_PF_F32RG) return VK_FORMAT_R32G32_SFLOAT;
	if (Format == OAPISURFACE_PF_DXT1) return VK_FORMAT_BC1_RGBA_UNORM_BLOCK;
	if (Format == OAPISURFACE_PF_F32RGBA) return VK_FORMAT_R32G32B32A32_SFLOAT;
	if (Format == OAPISURFACE_PF_F16RGBA) return VK_FORMAT_R16G16B16A16_SFLOAT;
	if (Format == OAPISURFACE_PF_ARGB) return VK_FORMAT_B8G8R8A8_UNORM;
	if (Format == OAPISURFACE_PF_DXT3) return VK_FORMAT_BC2_UNORM_BLOCK;
	if (Format == OAPISURFACE_PF_DXT5) return VK_FORMAT_BC3_UNORM_BLOCK;
	if (Format == OAPISURFACE_PF_GRAY) return VK_FORMAT_R8_UNORM;
	if (Format == OAPISURFACE_PF_ALPHA) return VK_FORMAT_R8_UNORM;
	return 0;
}


// -----------------------------------------------------------------------------------------------
//
const char* NatUsage(DWORD Usage)
{
	static char buf[128];
	buf[0] = '\0';
	if (Usage & VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) strcat(buf, "RENDERTARGET ");
	if (Usage & VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) strcat(buf, "DEPTHSTENCIL ");
	if (Usage & VK_IMAGE_USAGE_SAMPLED_BIT) strcat(buf, "SAMPLED ");
	if (Usage & (VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT)) strcat(buf, "TRANSFER ");
	if (Usage == 0) strcat(buf, "DEFAULT ");
	return buf;
}


// -----------------------------------------------------------------------------------------------
//
const char* NatPool(bool SysMem)
{
	static char buf[64];
	strcpy(buf, SysMem ? "D3DPOOL_SYSTEMMEM" : "D3DPOOL_DEFAULT"); // D3DPOOL_MANAGED: no counterpart
	return buf;
}


// -----------------------------------------------------------------------------------------------
//
const char* NatOAPIFlags(DWORD AF)
{
	static char buf[512]; buf[0] = '\0';

	if (AF & OAPISURFACE_TEXTURE)		strcat(buf, "OAPISURFACE_TEXTURE, ");
	if (AF & OAPISURFACE_RENDERTARGET)	strcat(buf, "OAPISURFACE_RENDERTARGET, ");
	if (AF & OAPISURFACE_GDI)			strcat(buf, "OAPISURFACE_GDI, ");
	if (AF & OAPISURFACE_SKETCHPAD)		strcat(buf, "OAPISURFACE_SKETCHPAD, ");
	if (AF & OAPISURFACE_MIPMAPS)		strcat(buf, "OAPISURFACE_MIPMAPS, ");
	if (AF & OAPISURFACE_NOMIPMAPS)		strcat(buf, "OAPISURFACE_NOMIPMAPS, ");
	if (AF & OAPISURFACE_ALPHA)			strcat(buf, "OAPISURFACE_ALPHA, ");
	if (AF & OAPISURFACE_NOALPHA)		strcat(buf, "OAPISURFACE_NOALPHA, ");
	if (AF & OAPISURFACE_UNCOMPRESS)	strcat(buf, "OAPISURFACE_UNCOMPRESS, ");
	if (AF & OAPISURFACE_SYSMEM)		strcat(buf, "OAPISURFACE_SYSMEM, ");
	if (AF & OAPISURFACE_ANTIALIAS)		strcat(buf, "OAPISURFACE_ANTIALIAS, ");
	if (AF & OAPISURFACE_RENDER3D)		strcat(buf, "OAPISURFACE_RENDER3D, ");
	return buf;
}


// -----------------------------------------------------------------------------------------------
//
const char* NatOAPIFormat(DWORD PF)
{
	static char buf[64];
	strcpy(buf, "UNKNOWN");
	DWORD AF = PF & OAPISURFACE_PF_MASK;

	if (AF == OAPISURFACE_PF_XRGB)	strcpy(buf, "OAPISURFACE_PF_XRGB ");
	if (AF == OAPISURFACE_PF_ARGB)	strcpy(buf, "OAPISURFACE_PF_ARGB ");
	if (AF == OAPISURFACE_PF_RGB565)strcpy(buf, "OAPISURFACE_PF_RGB565 ");
	if (AF == OAPISURFACE_PF_S16R)	strcpy(buf, "OAPISURFACE_PF_S16R ");
	if (AF == OAPISURFACE_PF_F32R)	strcpy(buf, "OAPISURFACE_PF_F32R ");
	if (AF == OAPISURFACE_PF_F32RG)	strcpy(buf, "OAPISURFACE_PF_F32RG ");
	if (AF == OAPISURFACE_PF_F32RGBA)strcpy(buf, "OAPISURFACE_PF_F32RGBA ");
	if (AF == OAPISURFACE_PF_F16R)	strcpy(buf, "OAPISURFACE_PF_F16R ");
	if (AF == OAPISURFACE_PF_F16RG)	strcpy(buf, "OAPISURFACE_PF_F16RG ");
	if (AF == OAPISURFACE_PF_F16RGBA)strcpy(buf, "OAPISURFACE_PF_F16RGBA ");
	if (AF == OAPISURFACE_PF_DXT1)	strcpy(buf, "OAPISURFACE_PF_DXT1 ");
	if (AF == OAPISURFACE_PF_DXT3)	strcpy(buf, "OAPISURFACE_PF_DXT3 ");
	if (AF == OAPISURFACE_PF_DXT5)	strcpy(buf, "OAPISURFACE_PF_DXT5 ");
	if (AF == OAPISURFACE_PF_ALPHA)	strcpy(buf, "OAPISURFACE_PF_ALPHA ");
	if (AF == OAPISURFACE_PF_GRAY)	strcpy(buf, "OAPISURFACE_PF_GRAY ");
	return buf;
}


// -----------------------------------------------------------------------------------------------
//
void NatDumpResource(VkTex *pResource)
{
	if (!pResource) { oapiWriteLogV("DX9_DUMP: no resource"); return; }

	static const char* sType[] = { "Unknown", "Surface", "Volume", "Texture", "3DTexture", "CubeTexture" };

	DWORD type = pResource->depth > 1 ? 4 : (pResource->cube ? 5 : NATTYPE_TEXTURE);

	oapiWriteLogV("DX9_DUMP: Mips = %d", pResource->levels);
	oapiWriteLogV("DX9_DUMP: Type = %s", sType[type]);

	DWORD f = NatConvertFormat_DX_to_OAPI(pResource->fmt, VkSwizzleOf(pResource->swizzle));

	oapiWriteLogV("DX9_DUMP: Usage = %s", NatUsage(pResource->usage));
	oapiWriteLogV("DX9_DUMP: Format = %s (%u)", NatOAPIFormat(f), (UINT)pResource->fmt);
	oapiWriteLogV("DX9_DUMP: Multisample = %u", (UINT)pResource->samples);
	oapiWriteLogV("DX9_DUMP: Size = (%u, %u)", pResource->w, pResource->h);
}
