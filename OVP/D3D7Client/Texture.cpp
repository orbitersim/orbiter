// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// Texture.cpp
// Texture loading and management routines for the D3D7 client.
//
// Methods for loading single (.dds) and multi-texture files (.tex)
// stored in DXT? format into DIRECTDRAWSURFACE7 instances.
// ==============================================================

#include "Texture.h"

using namespace oapi;

// ==============================================================
// Local prototypes

WORD GetNumberOfBits (DWORD dwMask);
HRESULT WINAPI EnumTextureFormats (DDPIXELFORMAT* pPixelFormat, VOID* pUserArg);

// ==============================================================
// ==============================================================
// Class TextureManager
// ==============================================================
// ==============================================================

TextureManager::TextureManager (D3D7Client *gclient)
{
	gc         = gclient;
	pDD        = gclient->GetDirectDraw();
	pDev       = gclient->GetDevice();
	devMemType = gclient->GetFramework()->GetDeviceMemType();
	bMipmap    = (gclient->GetFramework()->SupportsMipmaps() ? true:false);
	npfp       = 0;
	firstTex   = NULL;
}

// ==============================================================

TextureManager::~TextureManager ()
{
	ClearRepository();
	if (npfp) delete []pfp;
}

// ==============================================================

HRESULT TextureManager::ReadTexture (FILE *file, LPDIRECTDRAWSURFACE7 *ppdds, DWORD flags)
{
	HRESULT              hr;
	DDSURFACEDESC2       ddsd;
	DDPIXELFORMAT        ddpfBestMatch;
	LPDIRECTDRAWSURFACE7 pddsDXT    = NULL;
	LPDIRECTDRAWSURFACE7 pddsUncomp = NULL;

	if (devMemType != DDSCAPS_VIDEOMEMORY) flags |= 1; // load to system memory
	if (!bMipmap)                          flags |= 4; // don't load mipmaps

	if (FAILED (hr = ReadDDSSurface (file, &ddsd, &pddsDXT, flags))) {
		return hr;
	}
	if (FAILED (hr = LookupPixelFormat (ddsd.ddpfPixelFormat, &ddpfBestMatch))) {
		//LOGOUT_DDERR(hr);
		return hr;
	}

	// not sure whether here is the best place to do this
	if (ddpfBestMatch.dwFlags & DDPF_ALPHAPREMULT) {
		pDev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_ONE);
	} else {
        pDev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
  }

	if ((ddsd.ddpfPixelFormat.dwFourCC == ddpfBestMatch.dwFourCC) && !(flags&2)) {
		// device supports pixel format directly
		*ppdds = pddsDXT;
	} else {
		// convert into compatible pixel format

		// Retrieve the pixel format from the render surface - may need some thought
		memset (&ddpfBestMatch, 0, sizeof(DDPIXELFORMAT));
		ddpfBestMatch.dwSize = sizeof (DDPIXELFORMAT);
		gc->GetFramework()->GetBackBuffer()->GetPixelFormat (&ddpfBestMatch);

		ddsd.ddpfPixelFormat = ddpfBestMatch;
		if (FAILED (hr = BltToUncompressedSurface (ddsd, ddpfBestMatch,
			pddsDXT, &pddsUncomp))) {
			//LOGOUT_DDERR(hr);
			return hr;
		}
		pddsDXT->Release();
		*ppdds = pddsUncomp;
	}
	return hr;
}

// =======================================================================

HRESULT TextureManager::ReadTextureFromMemory (const BYTE *buf, DWORD nbuf, LPDIRECTDRAWSURFACE7 *ppdds, DWORD flags)
{
	HRESULT              hr;
	DDSURFACEDESC2       ddsd;
	DDPIXELFORMAT        ddpfBestMatch;
	LPDIRECTDRAWSURFACE7 pddsDXT    = NULL;
	LPDIRECTDRAWSURFACE7 pddsUncomp = NULL;

	if (devMemType != DDSCAPS_VIDEOMEMORY) flags |= 1; // load to system memory
	if (!bMipmap)                          flags |= 4; // don't load mipmaps

	if (FAILED (hr = ReadDDSSurfaceFromMemory (buf, nbuf, &ddsd, &pddsDXT, flags))) {
		return hr;
	}
	if (FAILED (hr = LookupPixelFormat (ddsd.ddpfPixelFormat, &ddpfBestMatch))) {
		//LOGOUT_DDERR(hr);
		return hr;
	}

	// not sure whether here is the best place to do this
	if (ddpfBestMatch.dwFlags & DDPF_ALPHAPREMULT) {
		pDev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_ONE);
	} else {
        pDev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
  }

	if ((ddsd.ddpfPixelFormat.dwFourCC == ddpfBestMatch.dwFourCC) && !(flags&2)) {
		// device supports pixel format directly
		*ppdds = pddsDXT;
	} else {
		// convert into compatible pixel format

		// Retrieve the pixel format from the render surface - may need some thought
		memset (&ddpfBestMatch, 0, sizeof(DDPIXELFORMAT));
		ddpfBestMatch.dwSize = sizeof (DDPIXELFORMAT);
		gc->GetFramework()->GetBackBuffer()->GetPixelFormat (&ddpfBestMatch);

		ddsd.ddpfPixelFormat = ddpfBestMatch;
		if (FAILED (hr = BltToUncompressedSurface (ddsd, ddpfBestMatch,
			pddsDXT, &pddsUncomp))) {
			//LOGOUT_DDERR(hr);
			return hr;
		}
		pddsDXT->Release();
		*ppdds = pddsUncomp;
	}
	return hr;
}

// =======================================================================

HRESULT TextureManager::LoadTexture (const char *fname, LPDIRECTDRAWSURFACE7 *ppdds, DWORD flags)
{
	HRESULT hr = S_FALSE;
	FILE* ftex = 0;
	char cpath[256];
	*ppdds = 0;
	if (gc->TexturePath(fname, cpath)) {
		ftex = fopen(cpath, "rb");
		if (ftex) {
			hr = ReadTexture(ftex, ppdds, flags);
			fclose(ftex);
		}
	}
	return hr;
}

// =======================================================================

HRESULT TextureManager::LoadTexture (const char *fname, long ofs, LPDIRECTDRAWSURFACE7 *ppdds, DWORD flags)
{
	HRESULT hr = S_OK;
	FILE* ftex = 0;
	char cpath[256];
	*ppdds = 0;
	if (gc->TexturePath(fname, cpath)) {
		ftex = fopen(cpath, "rb");
		if (ftex) {
			fseek(ftex, ofs, SEEK_SET);
			hr = ReadTexture(ftex, ppdds, flags);
			fclose(ftex);
		}
	}
	return hr;
}

// =======================================================================

int TextureManager::LoadTextures (const char *fname, LPDIRECTDRAWSURFACE7 *ppdds, DWORD flags, int n)
{
	char cpath[256];
	int ntex = 0;
	FILE* ftex = 0;
	if (gc->TexturePath(fname, cpath)) {
		ftex = fopen(cpath, "rb");
		if (ftex) {
			for (ntex = 0; ntex < n; ntex++) {
				if (FAILED(ReadTexture(ftex, ppdds + ntex, flags)))
					break;
			}
			fclose(ftex);
		}
	}
	return ntex;
}

// =======================================================================
// Retrieve a texture. First scans the repository of loaded textures.
// If not found, loads the texture from file and adds it to the repository

bool TextureManager::GetTexture (const char *fname, LPDIRECTDRAWSURFACE7 *ppdds, DWORD flags)
{
	TexRec *texrec = ScanRepository (fname);
	if (texrec) {
		// found in repository
		*ppdds = texrec->tex;
		return true;
	} else if (SUCCEEDED (LoadTexture (fname, ppdds, flags))) {
		// loaded from file
		AddToRepository (fname, *ppdds);
		return true;
	} else {
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
		if (id == texrec->id && !strncmp (fname, texrec->fname, 64))
			return texrec;
	}
	return NULL;
}

// =======================================================================
// Add a new entry to the repository

void TextureManager::AddToRepository (const char *fname, LPDIRECTDRAWSURFACE7 pdds)
{
	TexRec *texrec = new TexRec;
	texrec->tex = pdds;
	strncpy (texrec->fname, fname, 64);
	texrec->id = MakeTexId (fname);
	texrec->next = firstTex; // add to beginning of list
	firstTex = texrec;
}

// =======================================================================
// De-allocates the repository and release the DX7 textures

void TextureManager::ClearRepository ()
{
	while (firstTex) {
		TexRec *tmp = firstTex;
		firstTex = firstTex->next;
		if (tmp->tex) tmp->tex->Release();
		delete tmp;
	}
}

// =======================================================================

DWORD TextureManager::MakeTexId (const char *fname)
{
	DWORD id = 0;
	for (const char *c = fname; *c; c++)
		id += *c;
	return id;
}

// =======================================================================
// LookupPixelFormat()
// Search for a match for a given pixel format in the list and return it,
// or add the new format to the list, if no match is found
// =======================================================================

HRESULT TextureManager::LookupPixelFormat (DDPIXELFORMAT ddsdDDSTexture, DDPIXELFORMAT *pddsdBestMatch)
{
	HRESULT hr;

	// Check whether pixel format is registered already
	for (int i = 0; i < npfp; i++)
		if (!memcmp (&pfp[i].pixelfmt, &ddsdDDSTexture, sizeof (DDPIXELFORMAT))) {
			*pddsdBestMatch = pfp[i].bestmatch;
			return S_OK;
		}

	// Find new match and add to list
	if (FAILED (hr = FindBestPixelFormatMatch (ddsdDDSTexture, pddsdBestMatch))) {
		//LOGOUT_DDERR(hr);
		return hr;
	}

	// Enter new pixel format to list
	PixelFormatPair *tmp = new PixelFormatPair[npfp+1];
	if (npfp) {
		memcpy (tmp, pfp, npfp*sizeof(PixelFormatPair));
		delete []pfp;
	}
	pfp = tmp;
	pfp[npfp].pixelfmt = ddsdDDSTexture;
	pfp[npfp].bestmatch = *pddsdBestMatch;
	npfp++;

	return S_OK;
}

struct PixelFormatNode {
    DDPIXELFORMAT       ddpf;
    PixelFormatNode*    pNext;
    int                 nAlphaBits;
    int                 nRedBits;
    int                 nGreenBits;
    int                 nBlueBits;
    BOOL                bPremultipliedAlpha;
};

// =======================================================================
// FindBestPixelFormatMatch()
// Given a pixel format from a compressed surface, it finds the best
// pixel format match format that is supported by the current
// renderer.  pddsdBestMatch contains the best match found.
// =======================================================================

HRESULT TextureManager::FindBestPixelFormatMatch (DDPIXELFORMAT ddsdDDSTexture, DDPIXELFORMAT* pddsdBestMatch)
{
    HRESULT          hr;
    PixelFormatNode* pHead          = NULL;
    PixelFormatNode* pNode          = NULL;
    PixelFormatNode* pGoodMatchNode = NULL;
    int              nCompAlphaBits;
    int              nHighestFound = 0;
    BOOL             bCompressedTexture; 
    BOOL             bPremultipliedAlpha;

    bCompressedTexture = TRUE; // assume true

    // set how many alpha bits are in the compressed texture 
    switch (ddsdDDSTexture.dwFourCC) {
    case 0:
		// this dds texture isn't compressed so we need an
        // exact pixel format match to render this surface
        // (or do a manual pixel conversion)
        bCompressedTexture = FALSE;  
        break;
    case MAKEFOURCC ('D','X','T','1'):
        nCompAlphaBits = 1;
        bPremultipliedAlpha = FALSE;
        break;
    case MAKEFOURCC ('D','X','T','2'):
        nCompAlphaBits = 4;
        bPremultipliedAlpha = TRUE;
        break;
    case MAKEFOURCC ('D','X','T','3'):
        nCompAlphaBits = 4;
        bPremultipliedAlpha = FALSE;
        break;
    case MAKEFOURCC ('D','X','T','4'):
        nCompAlphaBits = 8;
        bPremultipliedAlpha = TRUE;
        break;
    case MAKEFOURCC ('D','X','T','5'):
        nCompAlphaBits = 8;
        bPremultipliedAlpha = FALSE;
        break;
    }

    // pixelFormatEnum is just a placeholder.
    // pixelFormatEnum.pNext will start the list
    if (FAILED (hr = pDev->EnumTextureFormats (EnumTextureFormats, 
        (VOID*)&pHead))) return hr;

    if (!bCompressedTexture) {
        // if the texture isn't compressed then look for an exact
        // pixel format match, otherwise fail since this sample
        // doesn't implement any manual pixel format conversion 
        // algorithms.
        int nTextureABits = GetNumberOfBits (ddsdDDSTexture.dwRGBAlphaBitMask);
        int nTextureRBits = GetNumberOfBits (ddsdDDSTexture.dwRBitMask);
        int nTextureGBits = GetNumberOfBits (ddsdDDSTexture.dwGBitMask);
        int nTextureBBits = GetNumberOfBits (ddsdDDSTexture.dwBBitMask);

        pGoodMatchNode = NULL;

        // run through list looking for an exact match
        pNode = pHead;

        while (NULL != pNode) {
            if (pNode->ddpf.dwFourCC == 0             &&
                pNode->nAlphaBits    == nTextureABits &&
                pNode->nRedBits      == nTextureRBits &&
                pNode->nGreenBits    == nTextureGBits &&
                pNode->nBlueBits     == nTextureBBits) {
                // this is an exact pixel format match, so it works
                pGoodMatchNode = pNode;
                break;
            }
            pNode = pNode->pNext; // advance along list
        }

        // pGoodMatchNode will be NULL if no exact match found, 
        // and since this is an uncompressed DDS texture format 
        // the blt can not convert between pixel formats.  
        // A manual conversion of the pixels could be done, but 
        // this is not implemeneted in this sample
    } else {
        // search for an exact pixel format match 
        // if renderer supports compressed textures 
        pNode = pHead;
        while (NULL != pNode) {
            if (pNode->ddpf.dwFourCC == ddsdDDSTexture.dwFourCC) {
                // look no further, since this is the best possible match
                pGoodMatchNode = pNode;
                break;
            }
            pNode = pNode->pNext; // advance along list
        }

        // if a good match was not found then keep looking
        if (NULL == pGoodMatchNode) {
            // search for exact or highest alpha bit rate match 
            // and also make sure the texture isn't blitted from
            // premultipled alpha to non-premultipled alpha or visa-versa
            pNode = pHead;
            nHighestFound = -1;
            pGoodMatchNode = NULL;

            while (NULL != pNode) {
                if (pNode->nAlphaBits          == nCompAlphaBits &&
                    pNode->bPremultipliedAlpha == bPremultipliedAlpha &&
                    pNode->nRedBits            >= 4 &&
                    pNode->nGreenBits          >= 4 &&
                    pNode->nBlueBits           >= 4) {
                    // look no further, since this is the next best possible match
                    pGoodMatchNode = pNode;
                    break;
                }
                if (pNode->nAlphaBits          >  nHighestFound &&
                    pNode->bPremultipliedAlpha == bPremultipliedAlpha &&
                    pNode->nRedBits            >= 4 &&
                    pNode->nGreenBits          >= 4 &&
                    pNode->nBlueBits           >= 4) {
                    nHighestFound = pNode->nAlphaBits;
                    pGoodMatchNode = pNode;
                    // keep looking for a better match
                }
                pNode = pNode->pNext; // advance along list
            }
        }
    }

    // if no match was found then either no texture pixel formats 
    // are supported by the renderer or this in an uncompressed
    // pixel format and an exact pixel format match was not found
    if (NULL == pGoodMatchNode) {
        hr = E_FAIL;
        goto LFail; // delete linked list
    }

    // choose the highest alpha rate possible as the best match
    *pddsdBestMatch = pGoodMatchNode->ddpf;  

LFail:
    // delete the nodes of the linked list
    while (NULL != pHead) { // while more in list, keep deleting
        pNode = pHead;
        pHead = pHead->pNext;
        delete pNode;
    }

    return hr;
}

// =======================================================================
// ReadDDSSurface()
// Read a compressed DDS surface from a stream
//    pddsdComp     contains the DDS surface description, and
//    pppddsCompTop contains the DDS surface
// =======================================================================

HRESULT TextureManager::ReadDDSSurface (FILE *file,
	DDSURFACEDESC2 *pddsd, LPDIRECTDRAWSURFACE7* ppddsDXT, DWORD flags)
{
	HRESULT              hr = E_FAIL;
	LPDIRECTDRAWSURFACE7 pdds         = NULL;
	LPDIRECTDRAWSURFACE7 pddsAttached = NULL;
	DDSURFACEDESC2       ddsd;
	DWORD                dwMagic;

	// Read the magic number
	if (!fread (&dwMagic, sizeof(DWORD), 1, file))
		goto LFail;
	if (dwMagic != MAKEFOURCC('D','D','S',' ')) {
		//LOGOUT_ERR("Invalid DDS signature");
		goto LFail;
	}

	// Read the surface description
	fread (pddsd, sizeof(DDSURFACEDESC2), 1, file);

	// Mask/set surface caps appropriately for the application
	if (flags&1) pddsd->ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;
	else         pddsd->ddsCaps.dwCaps2 |= DDSCAPS2_TEXTUREMANAGE;

	// this should only be set for textures which will never be
	// locked for dynamic modification
	pddsd->ddsCaps.dwCaps2 |= DDSCAPS2_OPAQUE;

    // Handle special case for hardware that doesn't support mipmaping
    if (flags&4) {
        pddsd->dwMipMapCount = 0;
        pddsd->dwFlags &= ~DDSD_MIPMAPCOUNT;
        pddsd->ddsCaps.dwCaps &= ~(DDSCAPS_MIPMAP | DDSCAPS_COMPLEX);
    }

	// Clear unwanted flags
	pddsd->dwFlags &= (~DDSD_PITCH);
	pddsd->dwFlags &= (~DDSD_LINEARSIZE);

    // create a new surface based on the surface description
    if (FAILED (hr = pDD->CreateSurface( pddsd, ppddsDXT, NULL))) {
		//LOGOUT_DDERR(hr);
        goto LFail;
	}
    pdds = *ppddsDXT;
    pdds->AddRef();

	while (TRUE) {
		ZeroMemory (&ddsd, sizeof (DDSURFACEDESC2));
		ddsd.dwSize = sizeof (DDSURFACEDESC2);

		if (FAILED (hr = pdds->Lock (NULL, &ddsd, DDLOCK_WAIT, NULL))) {
			//LOGOUT_DDERR(hr);
			goto LFail;
		}
		if (ddsd.dwFlags & DDSD_LINEARSIZE) {
			fread (ddsd.lpSurface, ddsd.dwLinearSize, 1, file);
		} else {
			DWORD yp;
			BYTE *pbDest = (BYTE*)ddsd.lpSurface;
			LONG dataBytesPerRow = ddsd.dwWidth * ddsd.ddpfPixelFormat.dwRGBBitCount / 8;
			for (yp = 0; yp < ddsd.dwHeight; yp++) {
				fread (pbDest, dataBytesPerRow, 1, file);
				pbDest += ddsd.lPitch;
			}
		}
		pdds->Unlock (NULL);

		if (flags&4) {
			// For mipless hardware, don't copy mipmaps
			pdds->Release();
			break;
		}
		ddsd.ddsCaps.dwCaps = DDSCAPS_TEXTURE | DDSCAPS_MIPMAP | DDSCAPS_COMPLEX;
		ddsd.ddsCaps.dwCaps2 = 0;
		ddsd.ddsCaps.dwCaps3 = 0;
		ddsd.ddsCaps.dwCaps4 = 0;

		if (FAILED (hr = pdds->GetAttachedSurface (&ddsd.ddsCaps, &pddsAttached))) {
			pdds->Release();
			break;
		}
		pdds->Release();
		pdds = pddsAttached;
	}
	hr = S_OK; // Everything worked

LFail:
	return hr;
}

// =======================================================================
// Read a compressed DDS surface from a memory buffer

HRESULT TextureManager::ReadDDSSurfaceFromMemory (const BYTE *buf, DWORD nbuf,
	DDSURFACEDESC2 *pddsd, LPDIRECTDRAWSURFACE7* ppddsDXT, DWORD flags)
{
	HRESULT              hr = E_FAIL;
	LPDIRECTDRAWSURFACE7 pdds         = NULL;
	LPDIRECTDRAWSURFACE7 pddsAttached = NULL;
	DDSURFACEDESC2       ddsd;
	DWORD                dwMagic;

	// Read the magic number
	if (nbuf >= sizeof(DWORD)) {
		memcpy(&dwMagic, buf, sizeof(DWORD));
		buf += sizeof(DWORD);
		nbuf -= sizeof(DWORD);
	} else goto LFail;
	if (dwMagic != MAKEFOURCC('D','D','S',' ')) {
		//LOGOUT_ERR("Invalid DDS signature");
		goto LFail;
	}

	// Read the surface description
	memcpy(pddsd, buf, sizeof(DDSURFACEDESC2));
	buf += sizeof(DDSURFACEDESC2);
	nbuf -= sizeof(DDSURFACEDESC2);

	// Mask/set surface caps appropriately for the application
	if (flags&1) pddsd->ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;
	else         pddsd->ddsCaps.dwCaps2 |= DDSCAPS2_TEXTUREMANAGE;

	// this should only be set for textures which will never be
	// locked for dynamic modification
	pddsd->ddsCaps.dwCaps2 |= DDSCAPS2_OPAQUE;

    // Handle special case for hardware that doesn't support mipmaping
    if (flags&4) {
        pddsd->dwMipMapCount = 0;
        pddsd->dwFlags &= ~DDSD_MIPMAPCOUNT;
        pddsd->ddsCaps.dwCaps &= ~(DDSCAPS_MIPMAP | DDSCAPS_COMPLEX);
    }

	// Clear unwanted flags
	pddsd->dwFlags &= (~DDSD_PITCH);
	pddsd->dwFlags &= (~DDSD_LINEARSIZE);

    // create a new surface based on the surface description
    if (FAILED (hr = pDD->CreateSurface(pddsd, ppddsDXT, NULL))) {
		//LOGOUT_DDERR(hr);
        goto LFail;
	}
    pdds = *ppddsDXT;
    pdds->AddRef();

	while (TRUE) {
		ZeroMemory (&ddsd, sizeof (DDSURFACEDESC2));
		ddsd.dwSize = sizeof (DDSURFACEDESC2);

		if (FAILED (hr = pdds->Lock (NULL, &ddsd, DDLOCK_WAIT, NULL))) {
			//LOGOUT_DDERR(hr);
			goto LFail;
		}
		if (ddsd.dwFlags & DDSD_LINEARSIZE) {
			memcpy(ddsd.lpSurface, buf, ddsd.dwLinearSize);
			buf += ddsd.dwLinearSize;
			nbuf -= ddsd.dwLinearSize;
		} else {
			DWORD yp;
			BYTE *pbDest = (BYTE*)ddsd.lpSurface;
			LONG dataBytesPerRow = ddsd.dwWidth * ddsd.ddpfPixelFormat.dwRGBBitCount / 8;
			for (yp = 0; yp < ddsd.dwHeight; yp++) {
				memcpy(pbDest, buf, dataBytesPerRow);
				buf += dataBytesPerRow;
				nbuf -= dataBytesPerRow;
				pbDest += ddsd.lPitch;
			}
		}
		pdds->Unlock (NULL);

		if (flags&4) {
			// For mipless hardware, don't copy mipmaps
			pdds->Release();
			break;
		}
		ddsd.ddsCaps.dwCaps = DDSCAPS_TEXTURE | DDSCAPS_MIPMAP | DDSCAPS_COMPLEX;
		ddsd.ddsCaps.dwCaps2 = 0;
		ddsd.ddsCaps.dwCaps3 = 0;
		ddsd.ddsCaps.dwCaps4 = 0;

		if (FAILED (hr = pdds->GetAttachedSurface (&ddsd.ddsCaps, &pddsAttached))) {
			pdds->Release();
			break;
		}
		pdds->Release();
		pdds = pddsAttached;
	}
	hr = S_OK; // Everything worked

LFail:
	return hr;
}

// =======================================================================
// BltToUncompressedSurface()
// Creates an uncompressed surface and blits the compressed surface to 
// it using the specified pixel format.
// =======================================================================

HRESULT TextureManager::BltToUncompressedSurface (DDSURFACEDESC2 ddsd, 
    DDPIXELFORMAT ddpf, LPDIRECTDRAWSURFACE7 pddsDXT, 
    LPDIRECTDRAWSURFACE7* ppddsNewSurface)
{
	HRESULT hr;
    LPDIRECTDRAWSURFACE7 pddsDXTAttached;
    LPDIRECTDRAWSURFACE7 pddsNew;
    LPDIRECTDRAWSURFACE7 pddsNewAttached;

    // Set surface caps for the new surface
    ddsd.ddpfPixelFormat = ddpf;

    // Create an un-compressed surface based on the enumerated texture formats
    if (FAILED (hr = pDD->CreateSurface (&ddsd, &pddsNew, NULL))) {
		//LOGOUT_DDERR(hr);
        return hr;
	}
    *ppddsNewSurface = pddsNew;

    // Copy compress image to un-compressed surface, including mips (if any)
    while (TRUE) {
        if (FAILED (hr = pddsNew->Blt (NULL, pddsDXT, NULL, DDBLT_WAIT, NULL))) {
			//LOGOUT_DDERR(hr);
            return hr;
		}

        // Get next surface in DXT's mipmap chain
        ddsd.ddsCaps.dwCaps = DDSCAPS_TEXTURE | DDSCAPS_MIPMAP | DDSCAPS_COMPLEX;
		ddsd.ddsCaps.dwCaps2 = 0;
		ddsd.ddsCaps.dwCaps3 = 0;
		ddsd.ddsCaps.dwCaps4 = 0;

        if (FAILED (pddsDXT->GetAttachedSurface (&ddsd.ddsCaps, &pddsDXTAttached)))
            return S_OK; // Failure here means were done with the mipmap chain
        pddsDXT = pddsDXTAttached;  

        // Get next surface in the new surface's mipmap chain
        if (FAILED (pddsNew->GetAttachedSurface (&ddsd.ddsCaps, &pddsNewAttached)))
            return E_FAIL;
        pddsNew = pddsNewAttached;
    }
}

// =======================================================================
// =======================================================================
// Non-member utility functions
// =======================================================================
// =======================================================================

// =======================================================================
// GetNumberOfBits()
// xReturns the number of bits set in a DWORD mask
// =======================================================================

WORD GetNumberOfBits (DWORD dwMask)
{
    WORD wBits = 0;
    while (dwMask) {
        dwMask = dwMask & (dwMask - 1);
        wBits++;
    }
    return wBits;
}

// =======================================================================
// EnumTextureFormats()
// Callback fn for enumerating the pixel formats the current renderer 
// supports.  Enumerated pixel formats are collected in a linked list
// The head for this linked list is in (PixelFormatNode*)pUserArg->pNext. 
// =======================================================================

HRESULT WINAPI EnumTextureFormats (DDPIXELFORMAT* pPixelFormat, VOID* pUserArg)
{
    PixelFormatNode** ppHead = (PixelFormatNode**)pUserArg; 
    PixelFormatNode* pNode = NULL;

    // create a new node 
    pNode = new PixelFormatNode;
    if (NULL == pNode) return DDENUMRET_CANCEL;
    
    // insert new node at beginning of list
    pNode->pNext = *ppHead;
    *ppHead = pNode;

    // fill up node info
    pNode->ddpf = *pPixelFormat;
    pNode->nAlphaBits = GetNumberOfBits (pPixelFormat->dwRGBAlphaBitMask);
    pNode->nRedBits   = GetNumberOfBits (pPixelFormat->dwRBitMask);
    pNode->nGreenBits = GetNumberOfBits (pPixelFormat->dwGBitMask);
    pNode->nBlueBits  = GetNumberOfBits (pPixelFormat->dwBBitMask);
    pNode->bPremultipliedAlpha = pPixelFormat->dwFlags & DDPF_ALPHAPREMULT;

    // continue enumerating all supported pixel formats
    return DDENUMRET_OK;
}

