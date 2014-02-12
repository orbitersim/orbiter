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

#ifndef __D3DSURFACE_H
#define __D3DSURFACE_H

#include "OrbiterAPI.h"
#include "D3D9Client.h"
#include "D3D9Pad.h"
#include "GDIPad.h"
#include <d3d9.h> 
#include <d3dx9.h>


#define D3D9S_PLAIN		0x1
#define D3D9S_TEXTURE	0x2
#define D3D9S_DYNAMIC	0x3
#define D3D9S_RTGTTEX	0x4

#define D3D9C_LOAD		0x1		// Created by clbkLoadTexture
#define D3D9C_TEXTURE   0x2	    // Created by clbkCreateTexture
#define D3D9C_SURFACE   0x3		// Created by clbkCreateSurface
#define D3D9C_BACKBUF   0x4		// Created by MakeBackBuffer


// Every SURFHANDLE in the client is a pointer into the D3D9ClientSurface class

class D3D9ClientSurface {

				friend class D3D9Client;
				friend class D3D9Pad;
				friend class GDIPad;

public:
				// Initialize global (shared) resources  
				static void D3D9TechInit(class D3D9Client *gc, LPDIRECT3DDEVICE9 pDev, const char *folder);
				static void GlobalExit();

				// Create empty surface. Must use Create methods to make a valid surface
				D3D9ClientSurface(LPDIRECT3DDEVICE9 pDevice, const char* name = "???");
				
				// Destroy the class and release the texture (pTex) if exists. Value of Reference counter doesn't matter.
				~D3D9ClientSurface();

	void		MakeBackBuffer(LPDIRECT3DSURFACE9);
	void		MakeTextureEx(UINT Width, UINT Height, DWORD Usage, D3DFORMAT Format=D3DFMT_X8R8G8B8, D3DPOOL Pool=D3DPOOL_DEFAULT);
	void		MakeSurfaceEx(UINT Width, UINT Height, D3DFORMAT Format=D3DFMT_X8R8G8B8, D3DPOOL Pool=D3DPOOL_DEFAULT);
	void		MakeRenderTargetEx(UINT Width, UINT Height, bool bTexture, bool bLock, D3DFORMAT Format=D3DFMT_X8R8G8B8);
	void		Make3DRenderTarget(UINT Width, UINT Height, UINT flags);
	
	void		MakeRenderingTexture(UINT Width, UINT Height, D3DFORMAT Format=D3DFMT_X8R8G8B8);
	void		MakeTexture(UINT width, UINT height, D3DFORMAT Format=D3DFMT_X8R8G8B8);
	void		MakePlainSurface(UINT width, UINT height, D3DPOOL pool=D3DPOOL_DEFAULT);

	bool		GetDesc(D3DSURFACE_DESC *);

	bool		LoadTexture(const char *fname, int flags=0);
	void		SaveSurface(const char *fname);
	
	DWORD		GetMipMaps();
	DWORD		GetWidth();
	DWORD		GetHeight();
	DWORD		GetSizeInBytes();

	void		IncRef();	// Increase surface reference counter
	bool		Release();	// Decrease the counter
	int			RefCount() { return Refs; }

	const char* GetName() const { return name; }
	void		SetName(const char *);
	

	bool		IsCompressed();
	bool		IsGDISurface();
	bool		IsBackBuffer();
	bool		IsTexture() const { return (pTex!=NULL); }
	bool		IsRenderTarget(); 
	bool		IsPowerOfTwo() const;
	bool		IsSystemMem() { return (desc.Pool==D3DPOOL_SYSTEMMEM); }

	LPDIRECT3DTEXTURE9 GetTextureHard();
	LPDIRECT3DTEXTURE9 GetNormalMap();
	LPDIRECT3DTEXTURE9 GetEmissionMap();
	LPDIRECT3DTEXTURE9 GetSpecularMap();
	LPDIRECT3DTEXTURE9 GetReflectionMap();
	LPDIRECT3DTEXTURE9 GetTexture();
	LPDIRECT3DDEVICE9  GetDevice() { return pDevice; }
	
	
	void		SetColorKey(DWORD ck);			// Enable and set color key
	void		DisableColorKey();				// Disable color key.
	
	void		SetCreation(int);

	HDC			GetDC();	
	void		ReleaseDC(HDC);

	HRESULT		AddQueue(D3D9ClientSurface *src, LPRECT s, LPRECT t);
	HRESULT		FlushQueue();
	void		CopyRect(D3D9ClientSurface *src, LPRECT srcrect, LPRECT tgtrect, UINT ck=0);
	bool		Fill(LPRECT r, DWORD color);
	bool		Clear(DWORD color);
	
	bool		BindGPU();
	void		ReleaseGPU();
	HRESULT		BeginBlitGroup();
	void		EndBlitGroup();
	int			GetQueueSize();
	bool		ScanNameSubId(const char *n);
	bool		ComputeReflAlpha();

private:

	bool		CreateName(char *out, int len, const char *fname, const char *id);
	void		Decompress();
	void		BringToSystemMem();
	void		ConvertToPlainSurface();
	void		ConvertToDynamicTexture();
	void		ConvertToRenderTargetTexture();
	DWORD		GetTextureSizeInBytes(LPDIRECT3DTEXTURE9 pT);
	DWORD		GetSizeInBytes(D3DFORMAT Format, DWORD pixels);
	void		CheckTemp(DWORD Width, DWORD Height);

	HRESULT		GPUCopyRect(D3D9ClientSurface *src, LPRECT srcrect, LPRECT tgtrect);
	HRESULT		GPUCopyTemp(LPRECT s, LPRECT t);
	HDC			GetDCHard();
	void		SetupViewPort();
	void		LogSpecs(char *name);
	void		Clear();
	
	bool		bCompressed;	// True if the surface is compressed
	bool		bDCOpen;		// DC is Open. This is TRUE between GetDC() and ReleaseDC() calls.
	bool		bNoGDI;			// Prevent a conversion into a GDI (System Memory) surface
	bool		bClear;			// True if the entire surface is cleared using Clear() and it's still clean. Note: (Fill color = "oClear")
	bool		bFlash;			// Make RT-GDI conflict surface to flash
	bool		bHard;			// hDC is acquired using GetDCHard()
	bool		bDC;			// This surface is used for GDI drawing. Flag is set in GetDC() 
	bool		bSkpGetDCEr;	
	bool		bSkpGetDC;
	bool		bBltGroup;		// BlitGroup operation is active
	int			Refs;
	DWORD		Type;
	DWORD		cClear;
	int			Initial;		// Initial creation flags
	int			Creation;		// Method of surface creation
	int			SketchPad;		// 0=None, 1=GDI, 2=GPU
	int			iBindCount;		// GPU Bind reference counter

	D3DSURFACE_DESC		  desc;


	LPDIRECT3DSURFACE9	  pTemp;		// Temporary surface for in-surface blitting
	LPDIRECT3DTEXTURE9	  pTempTex;		// Temporary surface for in-surface blitting
	LPDIRECT3DSURFACE9	  pSurf;		// This is a pointer to a plain surface or a pointer to the first level in a texture
	LPDIRECT3DTEXTURE9	  pTex;			// This is a NULL if Type==D3D9S_PLAIN or Creation==D3D9C_BACKBUF
	LPDIRECT3DSURFACE9	  pDCTemp;		// Containing a temporary system memory copy of a render target texture
	LPDIRECT3DTEXTURE9	  pNormalMap;
	LPDIRECT3DTEXTURE9	  pSpecularMap;
	LPDIRECT3DTEXTURE9	  pEmissionMap;
	LPDIRECT3DTEXTURE9	  pReflectionMap;
	LPDIRECT3DDEVICE9	  pDevice;

	ID3DXRenderToSurface *pRTS;
	D3D9ClientSurface    *pSrc_prev;
	D3DXCOLOR			  ClrKey;
	DWORD				  ColorKey;
	
	LPD3DXMATRIX		  pVP;
	D3DVIEWPORT9		  *pViewPort;

	char				  name[128];

	// Rendering pipeline configuration. Applies to every instance of this class
	//
	static D3D9Client * gc;
	static ID3DXEffect*	FX;			
	static D3DXHANDLE	eTech;	
	static D3DXHANDLE	eFlush;
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