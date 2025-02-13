// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __TEXTURE_H
#define __TEXTURE_H

#define STRICT 1
#include <windows.h>
#include <d3d.h>
#include <stdio.h>

#define MAXFMT 6 // max number of different pixel formats

struct RAWDDS {
	DDSURFACEDESC2 ddsd;
	LPBYTE data;
};

// =======================================================================
// class TextureManager2
// =======================================================================

class TextureManager2 {
public:
	TextureManager2 (LPDIRECT3DDEVICE7 dev);
	~TextureManager2 ();

	HRESULT ReadTexture (FILE *file, LPDIRECTDRAWSURFACE7 *pptex, DWORD flags = 0);
	// Read a texture from an open stream
	// flags: bit 0 set: force creation in system memory
	//        bit 1 set: uncompress texture

	HRESULT ReadCompatibleSurface(FILE* file, LPDIRECTDRAWSURFACE7* ppdds, DWORD flags = 0);
	// Read a DDS surface from an open stream
	// Uncompress if required by the current device
	// flags: bit 1 set: force map to uncompressed surface, even if hardware understands compression format
	//        other flags are passed on to ReadDDSSurface

	int ReadTextures (FILE *file, LPDIRECTDRAWSURFACE7 *pptex, int n, DWORD flags = 0);
	// Read up to 'n' textures from an open stream into texture
	// array 'pptex'. Return value is actual number of textures read

	int OpenTexture (const char *name, const char *ext, long ofs, LPDIRECTDRAWSURFACE7 *pptex, DWORD flags = 0);
	// Read a single texture from a multi-texture file at offset ofs
	// returns number of loaded textures (1, or 0 on failure)
	// flags: passed on to ReadCompatibleSurface

	int OpenTextures (const char *name, const char *ext, LPDIRECTDRAWSURFACE7 *pptex, int n);
	// Read the first n textures from a multi-texture file
	// Returns number of actually loaded textures (<= n)

	static HRESULT ReadRawDDSSurface (const char *name, RAWDDS &dds, DWORD flags);
	// Read the raw contents of a DDS file. Doese not use the D3D instance and can therefore be used
	// on a separate thread.
	// On success, dds.data has been dynamically allocated to contain the surface contents. The
	// caller is responsible for deleting the array.
	// Should this be moved elsewhere?

	HRESULT SurfaceFromData (RAWDDS &dds, LPDIRECTDRAWSURFACE7 *pptex);
	// Create a surface from the raw data. This is the counterpart to ReadRawDDSSurface.
	// The dds.data array is released by this function on exit.

	LPDIRECT3DDEVICE7 Device() const { return pDev; }

	HRESULT ReadCompatibleSurfaceFromMemory (BYTE *buf, DWORD nbuf,
		LPDIRECTDRAWSURFACE7 *ppdds, DWORD flags);

private:
	HRESULT ReadDDSSurface (FILE *file,
		DDSURFACEDESC2 *pddsd, LPDIRECTDRAWSURFACE7 *ppddsDXT, DWORD flags = 0);
	// Read a compressed DDS surface from an open stream
	// pddsdComp    : DDS surface description
	// pppddsCompTop: DDS surface
	// flags: bit 0 set: force creation in system memory
	//        bit 2 set: do not load mipmaps, even if they are present

	HRESULT ReadDDSSurfaceFromMemory (BYTE *buf, DWORD nbuf,
		DDSURFACEDESC2 *pddsd, LPDIRECTDRAWSURFACE7* ppddsDXT, DWORD flags);

	HRESULT FindBestPixelFormatMatch (DDPIXELFORMAT ddsdDDSTexture,
		DDPIXELFORMAT *pddsdBestMatch);
	// given a texture pixel format 'ddsdDDSTexture', return
	// the best match compatible with the current device in
	// 'pddsdBestMatch'

	HRESULT LookupPixelFormat (DDPIXELFORMAT ddsdDDSTexture,
		DDPIXELFORMAT *pddsdBestMatch);
	// given a texture pixel format 'ddsdDDSTexture', return
	// the best match compatible with the current device in
	// 'pddsdBestMatch'. This is faster than the previous
	// routine because it checks for previously registered
	// matches.

	HRESULT BltToUncompressedSurface (DDSURFACEDESC2 ddsd,
		DDPIXELFORMAT ddpf, LPDIRECTDRAWSURFACE7 pddsDXT,
		LPDIRECTDRAWSURFACE7 *ppddsUncomp);
	// Creates an uncompressed surface and copies the DXT
	// surface into it

	LPDIRECT3DDEVICE7 pDev;
	LPDIRECTDRAW7     pDD;

	struct PixelFormatPair {
		DDPIXELFORMAT pixelfmt;
		DDPIXELFORMAT bestmatch;
	} pfp[MAXFMT]; // list of pixel formats and best matches
	int npfp;      // number of valid entries in pfp
};

// =======================================================================
// Utility routines
// =======================================================================

HRESULT DDCopyBitmap (LPDIRECTDRAWSURFACE7 pdds, HBITMAP hbm,
    int x, int y, int dx, int dy);

LPDIRECTDRAWSURFACE7 DDCopyBitmap (LPDIRECTDRAW7 pDD, HBITMAP hbm);

bool WriteDIB(const char *fname, HANDLE hDIB);

LPDIRECTDRAWSURFACE7 CreateTexture (int w, int h);

LPDIRECTDRAWSURFACE7 CreateSurfaceFromBmp (HBITMAP hBmp,
	bool release_bmp = true);

void FillSurface (LPDIRECTDRAWSURFACE7 surf, DWORD col);





// EVERYTHING BELOW IS OBSOLETE

struct TextureRec {
	char fname[256];            // texture name
	DWORD active;               // texture reference count
	LPDIRECTDRAWSURFACE7 tex;   // texture handle
	DDSURFACEDESC2 ddsd;        // surface properties
	DWORD size;                 // texture data size
	TextureRec *prev, *next;
};

struct PixelFormatPair {
	DDPIXELFORMAT pixelfmt;
	DDPIXELFORMAT bestmatch;
};

class TextureManager {
public:
	TextureManager (LPDIRECT3DDEVICE7 _dev, int _maxsize);
	// Create texture manager for device _dev, with maximum
	// texture buffer size _maxsize

	~TextureManager ();

	void UnsetDevice ();
	// Deallocate all textures, but keep the records. Must
	// be called before the current device is destroyed.
	// The texture manager is in an undefined state until
	// SetDevice is called with the new device

	void SetDevice (LPDIRECT3DDEVICE7 _dev);
	// set a new device and re-acquire all textures in the list

	void SetTexturePath (char *path);
	// path to texture directory

	LPDIRECTDRAWSURFACE7 AcquireTexture (const char *fname, bool uncompress = false);

	bool IncRefCount (LPDIRECTDRAWSURFACE7 tex);
	// Increment reference counter for texture tex
	// (for example if the surface pointer is copied)

	bool DecRefCount (LPDIRECTDRAWSURFACE7 tex);
	// Decrement reference counter for texture tex
	// (for example if a copied pointer is discarded)

	bool ReleaseTexture (LPDIRECTDRAWSURFACE7 tex);
	// mark the record containing texture tex as inactive
	// (i.e. can be deallocated on request)
	// return value is true if record was found

	void Clear ();
	// release all textures (including active)

	int MemAllocated() const { return alloc_size; }
	// memory allocated for textures

	int nTextures() const { return ntex; }
	// number of textures in list

	int nActive() const { return nactive; }
	// number of active textures

	void OutputInfo ();
	// output some texture diagnostic info to DBG_MSG

private:
	LPDIRECTDRAWSURFACE7 AddRec (TextureRec *_rec);
	LPDIRECTDRAWSURFACE7 FindRec (const char *fname);
	HRESULT LoadTexture (const char *fname, TextureRec &rec, bool uncompress = false);

	bool DeallocRec ();
	// Deallocate last inactive record in the buffer
	// return value is true if an inactive record was found

	void DeallocRec (TextureRec *_rec);
	// Deallocate record _rec

	bool PixelFormatMatch (DDPIXELFORMAT ddpf, DDPIXELFORMAT &match);
	// return the best match for pixel format ddpf in match

	LPDIRECT3DDEVICE7 dev;
	TextureRec *rec0, *recN;
	PixelFormatPair *pfp;
	int npfp, pfp_buflen;
	int ntex, nactive;  // number of textures in list/active textures
	int maxsize;     // maximum texture allocation size
	int alloc_size;  // current texture allocation size
	char texturepath[256];
};

#endif // !__TEXTURE_H