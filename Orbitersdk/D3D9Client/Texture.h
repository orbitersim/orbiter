// ==============================================================
// Texture.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
// ==============================================================

// ==============================================================
// Texture loading and management routines for the D3D9 client.
//
// Methods for loading single (.dds) and multi-texture files (.tex)
// stored in DXT? format into DIRECTDRAWSURFACE7 instances.
// ==============================================================

#ifndef __TEXTURE_H
#define __TEXTURE_H

#include "D3D9Client.h"
#include <stdio.h>

// ==============================================================
// Class TextureManager

class TextureManager {
public:

	explicit TextureManager(oapi::D3D9Client *gclient);
	~TextureManager();
	
	HRESULT LoadTexture(const char *fname, LPD3D9CLIENTSURFACE *ppdds, int flags);

	int LoadTextures(const char *fname, LPDIRECT3DTEXTURE9 *ppdds, DWORD flags, int count);
	// Read a texture from file 'fname' into the DX7 surface
	// pointed to by 'ppdds'.

	bool GetTexture(const char *fname, LPD3D9CLIENTSURFACE *ppdds,int flags);
	// Retrieve a texture. First scans the repository of loaded textures.
	// If not found, loads the texture from file and adds it to the repository

	bool IsInRepository (SURFHANDLE p);

protected:

	DWORD MakeTexId(const char *fname);
	// simple checksum of a string. Used for speeding up texture searches.

private:
	oapi::D3D9Client *gc;
	LPDIRECT3DDEVICE9 pDev;

	// simple repository of loaded textures: linked list
	struct TexRec {
		LPD3D9CLIENTSURFACE tex;
		char fname[64];
		DWORD id;
		struct TexRec *next;
	} *firstTex;

	//TexRec *pRepo;

	// Some repository management functions below.
	// This could be made more sophisticated (defining a maximum size of
	// the repository, deallocating unused textures as required, etc.)
	// Would also require a reference counter and a size parameter in the
	// TexRec structure.

	TexRec *ScanRepository (const char *fname);
	// Return a matching texture entry from the repository, if found.
	// Otherwise, return NULL.

	void AddToRepository (const char *fname, LPD3D9CLIENTSURFACE pdds);
	// Add a new entry to the repository

	void ClearRepository ();
	// De-allocates the repository and release the DX7 textures
};


// ==============================================================
// Non-member utility functions

#endif // !__TEXTURE_H