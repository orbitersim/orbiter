// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012-2026 Jarmo Nikkanen
// ==============================================================

#ifndef __D3DSURFACE_H
#define __D3DSURFACE_H

#include "OrbiterAPI.h"
#include "D3D9Client.h"
#include "D3D9Pad.h"
#include "GDIPad.h"
//#include "gcCore.h"
#include "VkTexFile.h" // d3d9.h/d3dx9.h: textures, surfaces and the texture file functions

#define	MAP_NORMAL			0
#define	MAP_SPECULAR		1
#define	MAP_EMISSION		2
#define	MAP_REFLECTION		3
#define	MAP_TRANSLUCENCE	4
#define	MAP_TRANSMITTANCE	5
#define	MAP_ROUGHNESS		6
#define	MAP_METALNESS		7
#define	MAP_HEAT			8
#define MAP_MAX_COUNT		9

#define OAPISURFACE_MAPS		0x80000000		// Additional Texture Maps
#define OAPISURFACE_BACKBUFFER	0x40000000		// It's a backbuffer
#define OAPISURFACE_ORIGIN		0x20000000		// The origin from where the clones are being made, can't change (immutable)
#define OAPISURFACE_CAPTURE		0x10000000		// The origin from where the clones are being made, can't change (immutable)

#define OAPISURF_SKP_GDI_WARN	0x00000001

// not upstream: D3DSURFACE_DESC counterpart (Pool and Usage as flags)
struct SurfDesc {
	UINT Width, Height;
	VkFormat Format;
	VkSwz Swizzle;          // X8R8G8B8, L8, A8 and A8L8 formats
	VkImageUsageFlags Usage;
	bool RenderTarget;      // D3DUSAGE_RENDERTARGET
	bool Dynamic;           // D3DUSAGE_DYNAMIC
	bool AutoGenMipMap;     // D3DUSAGE_AUTOGENMIPMAP
	bool SysMem;            // D3DPOOL_SYSTEMMEM: GDI/CPU access; the image itself is on the GPU like the rest
	VkSampleCountFlagBits MultiSampleType;
};

// resource types, numbered as D3DRESOURCETYPE
#define NATTYPE_SURFACE		1
#define NATTYPE_TEXTURE		3

VkTex *				NatLoadSpecialTexture(const char* fname, const char* ext);
SURFHANDLE			NatLoadSurface(const char* file, DWORD flags, bool bPath = false);
bool				NatSaveSurface(const char* file, VkTex *pResource);
SURFHANDLE			NatCreateSurface(int width, int height, DWORD flags);
SURFHANDLE			NatGetMipSublevel(SURFHANDLE hSrf, int level);
bool				NatGenerateMipmaps(SURFHANDLE hSrf);
SURFHANDLE			NatCompressSurface(SURFHANDLE hSurface, DWORD flags);
bool				NatCreateName(char* out, int mlen, const char* fname, const char* id);
DWORD				NatConvertFormat_DX_to_OAPI(DWORD Format, VkSwz swz = SWZ_NONE);
DWORD				NatConvertFormat_OAPI_to_DX(DWORD Format);
const char*			NatUsage(DWORD Usage);
const char*			NatPool(bool SysMem);
const char*			NatOAPIFlags(DWORD AF);
const char*			NatOAPIFormat(DWORD PF);
void				NatDumpResource(VkTex *pResource);


#define ERR_DC_NOT_AVAILABLE		0x1
#define ERR_USED_NOT_DEFINED		0x2


// Every SURFHANDLE in the client is a pointer into the SurfNative class

class SurfNative
{
	friend class D3D9Client;
	friend class D3D9Pad;
	friend class GDIPad;

	struct _HDC_LOCAL {
		QPainter *hDC;
		QImage *pSrf;         // CPU copy the painter draws into, uploaded by ReleaseDC
	};

public:

							SurfNative(VkTex *pTex, DWORD Flags, VkSurf *pDep = NULL);	// texture resource
							SurfNative(VkSurf *pSrf, DWORD Flags, VkSurf *pDep = NULL);	// surface resource (owned)
							SurfNative(SurfNative* hOrigin);
							~SurfNative();

	void					AddMap(DWORD id, VkTex *pMap);
	const SurfDesc*			GetDesc() const { return &desc; }
	bool					GenerateMipMaps();
	bool					Decompress();
	// GetGDICache left out: GetDC paints on a CPU copy of any surface
	void					IncRef() { RefCount++; }
	bool					DecRef() { RefCount--; return RefCount <= 0; }
	bool					DeClone();
	bool					GetSpecs(gcCore::SurfaceSpecs* sp, int size);

	void					Reload();

	DWORD					GetMipMaps() const { return Mipmaps; }
	DWORD					GetWidth() const { return desc.Width; }
	DWORD					GetHeight() const { return desc.Height; }
	DWORD					GetOAPIFlags() const { return Flags; }
	DWORD					GetType() const { return (DWORD)type; }
	DWORD					GetSizeInBytes();
	DWORD*					GetClientFlags();

	const char*				GetName() const { return name; }
	void					SetName(const char*);
	QPainter *				GetDC();
	void					ReleaseDC(QPainter *);

	bool					IsGDISurface() const;
	bool					IsCompressed() const;
	bool					IsBackBuffer() const;
	bool					IsTexture() const { return (type == NATTYPE_TEXTURE); }
	bool					IsRenderTarget() const;
	bool					Is3DRenderTarget() const;
	bool					IsPowerOfTwo() const;
	bool					IsSystemMem() const { return desc.SysMem; }
	bool					IsAdvanced() const { return (Flags & OAPISURFACE_MAPS); }
	bool					IsColorKeyEnabled() const { return (ColorKey != SURF_NO_CK); }
	bool					IsClone() const { return hOrigin != this; }

	VkSurf *				GetTempSurface();
	VkTex *					GetResource() const { return pResource ? pResource : (pSurface ? pSurface->tex : NULL); }
	VkSurf *				GetDepthStencil() const { return pDepth; }
	VkSurf *				GetSurface();
	VkTex *					GetTexture() const;
	VkTex *					GetMap(int type) const { return pMap[type]; }
	VkTex *					GetMap(int type, int type2) const { return (pMap[type] ? pMap[type] : pMap[type2]); }
	D3D9Pad*				GetPooledSketchPad();
	void					SetColorKey(DWORD ck);			// Enable and set color key
	DWORD					GetColorKey() const { return ColorKey; }

	bool					Fill(LPRECT r, DWORD color);

	DWORD					GetTextureSizeInBytes(VkTex *pT);
	DWORD					GetFormatSizeInBytes(VkFormat Format, DWORD pixels);

	void					LogSpecs() const;
	// CreateDX7/DX7Sync left out: the DX7 lockable copy only existed to give render targets a GDI DC


	// -------------------------------------------------------------------------------

	char					name[128];				// Surface name
	SURFHANDLE				hOrigin;
	SurfDesc				desc;					// Surface size and format description
	DWORD					type;					// Resource type
	VkSurf *				pTemp;					// Cache for in-surface blitting
	VkSurf *				pDepth;					// DepthStencil surface for 3D rendering
	VkSurf *				pTexSurf;				// Texture "surface" level cache
	VkTex *					pResource;				// Main resource (texture)
	VkSurf *				pSurface;				// Main resource (surface)
	VkTex *					pMap[MAP_MAX_COUNT];	// Additional texture maps _norm, _rghn, _spec, etc...
	VkDev *					pDevice;
	DWORD					ColorKey;
	DWORD					Flags;					// Surface Flags/Attribs
	DWORD					Mipmaps;				// Mipmap count. 1 = no mipmaps
	DWORD					ClientFlags;
	int						RefCount;
	D3D9Pad*				pSkp;					// Pooled sketchpad interface cache
	_HDC_LOCAL				DC;
};


#endif

