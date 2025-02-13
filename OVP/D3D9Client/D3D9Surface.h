// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

#ifndef __D3DSURFACE_H
#define __D3DSURFACE_H

#include "OrbiterAPI.h"
#include "D3D9Client.h"
#include "D3D9Pad.h"
#include "GDIPad.h"
//#include "gcCore.h"
#include <d3d9.h>
#include <d3dx9.h>

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

LPDIRECT3DTEXTURE9	NatLoadSpecialTexture(const char* fname, const char* ext);
SURFHANDLE			NatLoadSurface(const char* file, uint32_t flags, bool bPath = false);
bool				NatSaveSurface(const char* file, LPDIRECT3DRESOURCE9 pResource);
SURFHANDLE			NatCreateSurface(int width, int height, uint32_t flags);
SURFHANDLE			NatGetMipSublevel(SURFHANDLE hSrf, int level);
bool				NatGenerateMipmaps(SURFHANDLE hSrf);
SURFHANDLE			NatCompressSurface(SURFHANDLE hSurface, uint32_t flags);
bool				NatCreateName(char* out, int mlen, const char* fname, const char* id);
uint32_t				NatConvertFormat_DX_to_OAPI(uint32_t Format);
uint32_t				NatConvertFormat_OAPI_to_DX(uint32_t Format);
const char*			NatUsage(uint32_t Usage);
const char*			NatPool(D3DPOOL Pool);
const char*			NatOAPIFlags(uint32_t AF);
const char*			NatOAPIFormat(uint32_t PF);
void				NatDumpResource(LPDIRECT3DRESOURCE9 pResource);


#define ERR_DC_NOT_AVAILABLE		0x1
#define ERR_USED_NOT_DEFINED		0x2


// Every SURFHANDLE in the client is a pointer into the SurfNative class

class SurfNative
{
	friend class D3D9Client;
	friend class D3D9Pad;
	friend class GDIPad;

	struct _HDC_LOCAL {
		HDC hDC;
		LPDIRECT3DSURFACE9 pSrf;
	};

public:

							SurfNative(LPDIRECT3DRESOURCE9 pSrf, uint32_t Flags, LPDIRECT3DSURFACE9 pDep = NULL);
							SurfNative(SurfNative* hOrigin);
							~SurfNative();

	void					AddMap(uint32_t id, LPDIRECT3DTEXTURE9 pMap);
	const D3DSURFACE_DESC*	GetDesc() const { return &desc; }
	bool					GenerateMipMaps();
	bool					Decompress();
	LPDIRECT3DTEXTURE9		GetGDICache(uint32_t Flags);
	void					IncRef() { RefCount++; }
	bool					DecRef() { RefCount--; return RefCount <= 0; }
	bool					DeClone();
	bool					GetSpecs(gcCore::SurfaceSpecs* sp, int size);

	void					Reload();

	uint32_t					GetMipMaps() const { return Mipmaps; }
	uint32_t					GetWidth() const { return desc.Width; }
	uint32_t					GetHeight() const { return desc.Height; }
	uint32_t					GetOAPIFlags() const { return Flags; }
	uint32_t					GetType() const { return (uint32_t)type; }
	uint32_t					GetSizeInBytes();
	uint32_t*					GetClientFlags();

	const char*				GetName() const { return name; }
	void					SetName(const char*);
	HDC						GetDC();
	void					ReleaseDC(HDC);

	bool					IsGDISurface() const;
	bool					IsCompressed() const;
	bool					IsBackBuffer() const;
	bool					IsTexture() const { return (type == D3DRTYPE_TEXTURE); }
	bool					IsRenderTarget() const;
	bool					Is3DRenderTarget() const;
	bool					IsPowerOfTwo() const;
	bool					IsSystemMem() const { return (desc.Pool == D3DPOOL_SYSTEMMEM); }
	bool					IsAdvanced() const { return (Flags & OAPISURFACE_MAPS); }
	bool					IsColorKeyEnabled() const { return (ColorKey != SURF_NO_CK); }
	bool					IsClone() const { return hOrigin != this; }

	LPDIRECT3DSURFACE9		GetTempSurface();
	LPDIRECT3DRESOURCE9		GetResource() const { return pResource; }
	LPDIRECT3DSURFACE9		GetDepthStencil() const { return pDepth; }
	LPDIRECT3DSURFACE9		GetSurface();
	LPDIRECT3DTEXTURE9		GetTexture() const;
	LPDIRECT3DTEXTURE9		GetMap(int type) const { return pMap[type]; }
	LPDIRECT3DTEXTURE9		GetMap(int type, int type2) const { return (pMap[type] ? pMap[type] : pMap[type2]); }
	D3D9Pad*				GetPooledSketchPad();
	void					SetColorKey(uint32_t ck);			// Enable and set color key
	uint32_t					GetColorKey() const { return ColorKey; }

	bool					Fill(LPRECT r, uint32_t color);

	uint32_t					GetTextureSizeInBytes(LPDIRECT3DTEXTURE9 pT);
	uint32_t					GetFormatSizeInBytes(D3DFORMAT Format, uint32_t pixels);

	void					LogSpecs() const;
	bool					CreateDX7();
	void					DX7Sync(bool bUp);


	// -------------------------------------------------------------------------------

	char					name[128];				// Surface name
	SURFHANDLE				hOrigin;
	D3DSURFACE_DESC			desc;					// Surface size and format description
	D3DRESOURCETYPE			type;					// Resource type
	LPDIRECT3DTEXTURE9		pGDICache;				// Low level GDI cache for surface syncing
	LPDIRECT3DSURFACE9		pTemp;					// Cache for in-surface blitting
	LPDIRECT3DSURFACE9		pDepth;					// DepthStencil surface for 3D rendering
	LPDIRECT3DSURFACE9		pTexSurf;				// Texture "surface" level cache
	LPDIRECT3DRESOURCE9		pResource;				// Main resource
	LPDIRECT3DSURFACE9		pDX7;
	LPDIRECT3DTEXTURE9		pMap[MAP_MAX_COUNT];	// Additional texture maps _norm, _rghn, _spec, etc...
	LPDIRECT3DDEVICE9		pDevice;
	uint32_t					ColorKey;
	uint32_t					Flags;					// Surface Flags/Attribs
	uint32_t					Mipmaps;				// Mipmap count. 1 = no mipmaps
	uint32_t					ClientFlags;
	int						RefCount;
	D3D9Pad*				pSkp;					// Pooled sketchpad interface cache
	_HDC_LOCAL				DC;
};


#endif
