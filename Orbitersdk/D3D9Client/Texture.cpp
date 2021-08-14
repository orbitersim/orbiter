// ==============================================================
// Texture.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006 -2016 Martin Schweiger
// ==============================================================

// ==============================================================
// Texture loading and management routines for the D3D9 client.
//
// Methods for loading single (.dds) and multi-texture files (.tex)
// stored in DXT? format into DIRECTDRAWSURFACE7 instances.
// ==============================================================

#include "windows.h"
#include "Texture.h"
#include "D3D9Surface.h"
#include "D3D9Catalog.h"
#include <ddraw.h>

using namespace oapi;


#pragma pack(push, 1)
typedef struct _DDDESC2_x64
{
	DWORD               dwSize;                 // size of the DDSURFACEDESC structure
	DWORD               dwFlags;                // determines what fields are valid
	DWORD               dwHeight;               // height of surface to be created
	DWORD               dwWidth;                // width of input surface
	union
	{
		LONG            lPitch;                 // distance to start of next line (return value only)
		DWORD           dwLinearSize;           // Formless late-allocated optimized surface size
	} DUMMYUNIONNAMEN(1);
	union
	{
		DWORD           dwBackBufferCount;      // number of back buffers requested
		DWORD           dwDepth;                // the depth if this is a volume texture 
	} DUMMYUNIONNAMEN(5);
	union
	{
		DWORD           dwMipMapCount;          // number of mip-map levels requestde
												// dwZBufferBitDepth removed, use ddpfPixelFormat one instead
		DWORD           dwRefreshRate;          // refresh rate (used when display mode is described)
		DWORD           dwSrcVBHandle;          // The source used in VB::Optimize
	} DUMMYUNIONNAMEN(2);
	DWORD               dwAlphaBitDepth;        // depth of alpha buffer requested
	DWORD               dwReserved;             // reserved
	DWORD               lpSurface;              // pointer to the associated surface memory
	union
	{
		DDCOLORKEY      ddckCKDestOverlay;      // color key for destination overlay use
		DWORD           dwEmptyFaceColor;       // Physical color for empty cubemap faces
	} DUMMYUNIONNAMEN(3);
	DDCOLORKEY          ddckCKDestBlt;          // color key for destination blt use
	DDCOLORKEY          ddckCKSrcOverlay;       // color key for source overlay use
	DDCOLORKEY          ddckCKSrcBlt;           // color key for source blt use
	union
	{
		DDPIXELFORMAT   ddpfPixelFormat;        // pixel format description of the surface
		DWORD           dwFVF;                  // vertex format description of vertex buffers
	} DUMMYUNIONNAMEN(4);
	DDSCAPS2            ddsCaps;                // direct draw surface capabilities
	DWORD               dwTextureStage;         // stage in multitexture cascade
} DDSURFACEDESC2_x64;
#pragma pack(pop)



// ==============================================================
// ==============================================================
// Class TextureManager
// ==============================================================
// ==============================================================

TextureManager::TextureManager(D3D9Client *gclient) :
	gc(gclient),
	pDev(gclient->GetDevice()),
	firstTex()
{
}

// ==============================================================

TextureManager::~TextureManager ()
{
	ClearRepository();
}


HRESULT TextureManager::LoadTexture(const char *fname, LPD3D9CLIENTSURFACE *pSurf, int flags)
{
	(*pSurf) = new D3D9ClientSurface(pDev, fname);

	DWORD attrib = OAPISURFACE_TEXTURE;
	if (flags & 0x1) attrib |= OAPISURFACE_SYSMEM;
	if (flags & 0x2) attrib |= OAPISURFACE_UNCOMPRESS | OAPISURFACE_NOALPHA;
	if (flags & 0x4) attrib |= OAPISURFACE_NOMIPMAPS;
	//else			 attrib |= OAPISURFACE_MIPMAPS;

	if ((flags&0x2) && ((flags&0x1)==0)) attrib |= OAPISURFACE_RENDERTARGET; // Uncompress means that it's going to do something bad, so, let's prepare for the worst.

	if ((*pSurf)->LoadSurface(fname, attrib)==true) return S_OK;
	
	delete (*pSurf);
	*pSurf = NULL;
	return -1;
}


int TextureManager::LoadTextures(const char *fname, LPDIRECT3DTEXTURE9 *ppdds, DWORD flags, int amount)
{
	_TRACE;

	char path[MAX_PATH];

	if (gc->TexturePath (fname, path)) {

		FILE *f;

		if (fopen_s(&f, path, "rb")) return 0;

		int ntex = 0;
		char *buffer, *location;
		fseek(f, 0, SEEK_END);
		long size = ftell(f);
		long BytesLeft = size;
		buffer = new char[size+1];
		rewind(f);
		fread(buffer, 1, size, f);
		fclose(f);

		location = buffer;
		while (ntex < amount && BytesLeft > 0)
		{
			DWORD Magic = *(DWORD*)location;
			if (Magic != MAKEFOURCC('D','D','S',' ')) break;

			DDSURFACEDESC2 *header = (DDSURFACEDESC2*)(location + sizeof(Magic));

			if ((header->dwFlags&DDSD_LINEARSIZE)==0 && (header->dwFlags&DDSD_PITCH)==0) {
				header->dwFlags|=DDSD_LINEARSIZE;
				     if (header->ddpfPixelFormat.dwFourCC==MAKEFOURCC('D','X','T','5')) header->dwLinearSize = header->dwHeight * header->dwWidth;
				else if (header->ddpfPixelFormat.dwFourCC==MAKEFOURCC('D','X','T','3')) header->dwLinearSize = header->dwHeight * header->dwWidth;
				else if (header->ddpfPixelFormat.dwFourCC==MAKEFOURCC('D','X','T','1')) header->dwLinearSize = header->dwHeight * header->dwWidth / 2;
				else header->dwLinearSize = header->dwHeight * header->dwWidth * header->ddpfPixelFormat.dwRGBBitCount/8;
			}

			long bytes = (header->dwFlags & DDSD_LINEARSIZE) ? header->dwLinearSize : (header->dwHeight * header->dwWidth * header->ddpfPixelFormat.dwRGBBitCount/8);

			bytes += sizeof(Magic) + sizeof(DDSURFACEDESC2_x64);

			D3DXIMAGE_INFO Info;
			LPDIRECT3DTEXTURE9 pTex = NULL;

			if (D3DXCreateTextureFromFileInMemoryEx(pDev, location, bytes, 0, 0, 1, 0, D3DFMT_FROM_FILE,
				D3DPOOL_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, &Info, NULL, &pTex)==S_OK) {
				ppdds[ntex] = pTex;
				TileCatalog->Add(pTex);
				//LogAlw("Loaded a texture from %s, 0x%X (%u x %u)", fname, pTex, Info.Width, Info.Height);
			}
			else {
				delete[] buffer;
				LogErr("Failed to surface tile (%d tiles loaded for %s)",ntex,fname);
				return ntex;
			}

			location += bytes;
			BytesLeft -= bytes;
			ntex++;
		}
		delete[] buffer;
		LogOk("Loaded %d textures for %s",ntex,fname);
		return ntex;
	}
	LogWrn("File %s not found",fname);
	return 0;
}

// =======================================================================
// Retrieve a texture. First scans the repository of loaded textures.
// If not found, loads the texture from file and adds it to the repository
//
bool TextureManager::GetTexture(const char *fname, LPD3D9CLIENTSURFACE *pd3dt, int flags)
{
	TexRec *texrec = ScanRepository(fname);

	if (texrec) {
		// found in repository
		*pd3dt = texrec->tex;
		texrec->tex->IncRef();
		LogOk("Texture %s (%s) found from repository. ReferenceCount=%d", _PTR(*pd3dt), fname, (*pd3dt)->RefCount());
		return true;
	}
	else if (SUCCEEDED(LoadTexture(fname, pd3dt, flags))) {
		// loaded from file
		LogAlw("Texture %s (%s) added in repository", _PTR(*pd3dt), fname);
		AddToRepository (fname, *pd3dt);
		return true;
	}
	else {
		LogWrn("Texture %s not found",fname);
		// not found
		return false;
	}
}

// =======================================================================
// Return a matching texture entry from the repository, if found.
// Otherwise, return NULL.

TextureManager::TexRec *TextureManager::ScanRepository (const char *fname)
{
	TexRec *texrec;
	DWORD id = MakeTexId (fname);
	for (texrec = firstTex; texrec; texrec = texrec->next) {
		if (id == texrec->id) if (!strncmp (fname, texrec->fname, 64))
			return texrec;
	}
	return NULL;
}

// =======================================================================
// Return a true if the surface is in repository

bool TextureManager::IsInRepository (SURFHANDLE p)
{
	TexRec *texrec;
	for (texrec = firstTex; texrec; texrec = texrec->next) if (p == texrec->tex) return true;
	return false;
}

// =======================================================================
// Add a new entry to the repository

void TextureManager::AddToRepository (const char *fname, LPD3D9CLIENTSURFACE pdds)
{
	TexRec *texrec = new TexRec;
	texrec->tex = pdds;
	strncpy_s(texrec->fname, 63, fname, 64);
	texrec->id = MakeTexId (fname);
	texrec->next = firstTex; // add to beginning of list
	firstTex = texrec;
}

// =======================================================================
// De-allocates the repository and release the DX7 textures

void TextureManager::ClearRepository()
{
	while (firstTex) {
		TexRec *tmp = firstTex;
		firstTex = firstTex->next;
		SAFE_DELETE(tmp->tex);
		delete tmp;
	}
}

// =======================================================================

DWORD TextureManager::MakeTexId (const char *fname)
{
	DWORD id = 0;
	for (const char *c = fname; *c; c++) id += *c;
	return id;
}