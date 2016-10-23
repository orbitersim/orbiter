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
#include <d3d9.h>
#include <d3dx9.h>

#define	MAP_NORMAL			0
#define	MAP_SPECULAR		1
#define	MAP_EMISSION		2
#define	MAP_REFLECTION		3
#define	MAP_TRANSLUCENCE	4
#define	MAP_TRANSMITTANCE	5
#define	MAP_ROUGHNESS		6
#define	MAP_FRESNEL			7
#define MAP_MAX_COUNT		8


#define ERR_DC_NOT_AVAILABLE		0x1
#define ERR_USED_NOT_DEFINED		0x2


// Every SURFHANDLE in the client is a pointer into the D3D9ClientSurface class

class D3D9ClientSurface {

	friend class D3D9Client;
	friend class D3D9Pad;
	friend class GDIPad;

public:
						// Initialize global (shared) resources
	static void			D3D9TechInit(class D3D9Client *gc, LPDIRECT3DDEVICE9 pDev, const char *folder);
	static void			GlobalExit();

						// Create empty surface. Must use Create methods to make a valid surface
						D3D9ClientSurface(LPDIRECT3DDEVICE9 pDevice, const char* name = "???");

						// Destroy the class and release the texture (pTex) if exists. Value of Reference counter doesn't matter.
						~D3D9ClientSurface();

	void				ConvertSurface(DWORD attrib);
	void				CreateSurface(int w, int h, DWORD attrib);
	void				MakeBackBuffer(LPDIRECT3DSURFACE9);
	void				MakeEmptySurfaceEx(UINT Width, UINT Height, DWORD Usage=0, D3DFORMAT Format=D3DFMT_X8R8G8B8, D3DPOOL pool=D3DPOOL_SYSTEMMEM, DWORD Flags=0);
	void				MakeEmptyTextureEx(UINT Width, UINT Height, DWORD Usage=0, D3DFORMAT Format=D3DFMT_X8R8G8B8);
	void				MakeDepthStencil();

	bool				GetDesc(D3DSURFACE_DESC *);
	bool				GenerateMipMaps();

	bool				LoadSurface(const char *fname, DWORD flags, bool bDecompress=false);
	bool				LoadTexture(const char *fname);
	void				SaveSurface(const char *fname);
	bool				LoadSpecialTexture(const char *fname, const char *ext, int id);

	DWORD				GetMipMaps();
	DWORD				GetWidth();
	DWORD				GetHeight();
	DWORD				GetSizeInBytes();
	
	void				IncRef();	// Increase surface reference counter
	bool				Release();	// Decrease the counter
	int					RefCount() { return Refs; }

	const char *		GetName() const { return name; }
	void				SetName(const char *);

	inline bool			Exists() { return (pSurf!=NULL); }
	inline bool			HasSubSurface() { return (pDCSub!=NULL); }
	bool				IsCompressed();
	bool				IsGDISurface();
	bool				IsBackBuffer();
	bool				IsTexture() const { return (pTex!=NULL); }
	bool				IsRenderTarget();
	bool				Is3DRenderTarget();
	bool				IsPowerOfTwo() const;
	inline bool			IsSystemMem() { return (desc.Pool==D3DPOOL_SYSTEMMEM); }
	inline bool			IsDynamic() { return (desc.Usage&D3DUSAGE_DYNAMIC)!=0; }
	inline bool			IsPlainSurface() { return (desc.Usage==0 && pTex==NULL); }
	inline bool			IsDualLayer() { return (pDCSub!=NULL); }
	inline bool			IsAdvanced() { return bAdvanced; }
	inline bool			IsColorKeyEnabled() { return (ColorKey != 0); }

	DWORD				GetAttribs(int What=1);
	LPDIRECT3DSURFACE9	GetDepthStencil();
	LPDIRECT3DSURFACE9	GetSurface();
	LPDIRECT3DTEXTURE9	GetMap(int type) const { return pMap[type]; }
	LPDIRECT3DTEXTURE9	GetMap(int type, int type2) const { return (pMap[type] ? pMap[type] : pMap[type2]); }

	LPDIRECT3DTEXTURE9	GetTexture();
	LPDIRECT3DDEVICE9	GetDevice() { return pDevice; }
	int					GetSketchPadMode() { return SketchPad; }

	void				SetColorKey(DWORD ck);			// Enable and set color key

	HDC					GetDC();
	void				ReleaseDC(HDC);
	
	HRESULT				AddQueue(D3D9ClientSurface *src, LPRECT s, LPRECT t);
	HRESULT				FlushQueue();
	void				CopyRect(D3D9ClientSurface *src, LPRECT srcrect, LPRECT tgtrect, UINT ck=0);
	HRESULT				GPUCopyRect(D3D9ClientSurface *src, LPRECT srcrect, LPRECT tgtrect);
	
	bool				Fill(LPRECT r, DWORD color);
	bool				Clear(DWORD color);
	
	bool				BindGPU();
	void				ReleaseGPU();
	HRESULT				BeginBlitGroup();
	void				EndBlitGroup();
	int					GetQueueSize();

	void				PrintError(int err);

private:

	bool				ConvertToRenderTargetTexture();
	bool				ConvertToRenderTarget(bool bLock=false);
	bool				ConvertToPlain();
	bool				ConvertToTexture(bool bDynamic=false);
	void				SyncSubSurface();
	void				CreateSubSurface();
	bool				CreateName(char *out, int len, const char *fname, const char *id);
	void				Decompress(DWORD Attribs=0);
	DWORD				GetTextureSizeInBytes(LPDIRECT3DTEXTURE9 pT);
	DWORD				GetSizeInBytes(D3DFORMAT Format, DWORD pixels);
	HDC					GetDCHard();
	void				SetupViewPort();
	void				LogSpecs(char *name);
	void				Clear();

	// -------------------------------------------------------------------------------
	char				name[128];
	bool				bCompressed;	// True if the surface is compressed
	bool				bDCOpen;		// DC is Open. This is TRUE between GetDC() and ReleaseDC() calls.
	bool				bHard;			// hDC is acquired using GetDCHard()
	bool				bBltGroup;		// BlitGroup operation is active
	bool				bBackBuffer;
	bool				bLockable;
	bool				bMainDC;
	bool				bDCSys;
	bool				bBltSys;
	bool				bAdvanced;		// Additional textures maps has been loaded
	int					Refs;
	int					Initial;		// Initial creation Attributes flags
	int					Active;			// Active Attribute flags
	int					SketchPad;		// Currently Active Sketchpad 0=None, 1=GDI, 2=GPU
	int					iBindCount;		// GPU Bind reference counter
	int					ErrWrn;
	D3DSURFACE_DESC		desc;
	LPDIRECT3DSURFACE9	pStencil;		
	LPDIRECT3DSURFACE9	pSurf;		// This is a pointer to a plain surface or a pointer to the first level in a texture
	LPDIRECT3DTEXTURE9	pTex;		// This is a NULL if Type==D3D9S_PLAIN or Creation==D3D9C_BACKBUF
	LPDIRECT3DSURFACE9	pDCSub;		// Containing a temporary system memory copy of a render target texture
	LPDIRECT3DTEXTURE9	pMap[MAP_MAX_COUNT];
	LPDIRECT3DDEVICE9	pDevice;
	D3DXCOLOR			ClrKey;
	DWORD				ColorKey;
	//DWORD				Flags;
	DWORD				GDIBltCtr;
	LPD3DXMATRIX		pVP;
	D3DVIEWPORT9 *		pViewPort;
	HFONT				hDefFont;
	double				GetDCTime;

	ID3DXRenderToSurface *pRTS;

	// Rendering pipeline configuration. Applies to every instance of this class
	//
	static D3D9Client * gc;
	static ID3DXEffect*	FX;
	static D3DXHANDLE	eTech;
	static D3DXHANDLE	eFlush;
	static D3DXHANDLE	eSketch;
	static D3DXHANDLE	eRotate;
	static D3DXHANDLE	eVP;		// Transformation matrix
	static D3DXHANDLE	eColor;		// Color key
	static D3DXHANDLE	eTex0;		// Source Texture
	static D3DXHANDLE	eSize;
	static D3DXHANDLE	eKey;
	static WORD *		Index;
	static GPUBLITVTX * pGPUVtx;
	static WORD			GPUBltIdx;
	static D3D9ClientSurface *pPrevSrc;
};


#endif