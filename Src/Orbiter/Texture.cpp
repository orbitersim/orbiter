// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Orbiter.h"
#include "Texture.h"
#include "Log.h"
#include "OGraphics.h"

// =======================================================================
// Externals
// =======================================================================

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern char DBG_MSG[256];

// =======================================================================
// Prototypes for local helper functions
// =======================================================================

static HRESULT ReadDDSSurface (FILE *file, LPDIRECTDRAW7 pDD,
	DDSURFACEDESC2 *pddsd, LPDIRECTDRAWSURFACE7* ppddsDXT, DWORD flags = 0);

static HRESULT ReadDDSSurfaceFromMemory (BYTE *buf, DWORD nbuf, LPDIRECTDRAW7 pDD,
	DDSURFACEDESC2 *pddsd, LPDIRECTDRAWSURFACE7* ppddsDXT, DWORD flags);

static HRESULT FindBestPixelFormatMatch (LPDIRECT3DDEVICE7 pd3dDevice, 
    DDPIXELFORMAT ddsdDDSTexture, DDPIXELFORMAT* pddsdBestMatch);

static HRESULT BltToUncompressedSurface (LPDIRECTDRAW7 pDD, DDSURFACEDESC2 ddsd, 
    DDPIXELFORMAT ddpf, LPDIRECTDRAWSURFACE7 pddsDXT, 
    LPDIRECTDRAWSURFACE7* ppddsNewSurface);

// =======================================================================
// class TextureManager2
// =======================================================================

TextureManager2::TextureManager2 (LPDIRECT3DDEVICE7 dev)
{
	pDev = dev;
	pDD = g_pOrbiter->GetInlineGraphicsClient()->GetDirectDraw();
	npfp = 0;
}

TextureManager2::~TextureManager2 ()
{}

HRESULT TextureManager2::ReadTexture (FILE *file, LPDIRECTDRAWSURFACE7 *pptex, DWORD flags)
{
	// obsolete; use ReadCompatibleSurface directly
	return ReadCompatibleSurface (file, pptex, flags);
}

int TextureManager2::ReadTextures (FILE *file, LPDIRECTDRAWSURFACE7 *pptex, int n, DWORD flags)
{
	int i;
	for (i = 0; i < n; i++) {
		if (FAILED (ReadCompatibleSurface (file, pptex+i, flags)))
			break;
	}
	return i;
}

int TextureManager2::OpenTexture (const char *name, const char *ext, long ofs, LPDIRECTDRAWSURFACE7 *pptex, DWORD flags)
{
	FILE *file;
	int ntex = 0;
	if (file = g_pOrbiter->OpenTextureFile (name, ext)) {
		if (ofs) fseek (file, ofs, SEEK_SET);
		HRESULT h = ReadCompatibleSurface (file, pptex, flags);
		fclose (file);
		return (h == DD_OK ? 1:0);
	} else
		return 0;
}

int TextureManager2::OpenTextures (const char *name, const char *ext, LPDIRECTDRAWSURFACE7 *pptex, int n)
{
	FILE *file;
	int ntex = 0;
	if (file = g_pOrbiter->OpenTextureFile (name, ext)) {
		ntex = ReadTextures (file, pptex, n);
		fclose (file);
		g_pOrbiter->OutputLoadTick (1, true);
	} else
		g_pOrbiter->OutputLoadTick (1, false);

	return ntex;
}

#pragma optimize ("", off)

HRESULT TextureManager2::ReadCompatibleSurface (FILE *file,
	LPDIRECTDRAWSURFACE7 *ppdds, DWORD flags)
{
	HRESULT              hr;
	DDSURFACEDESC2       ddsd;
	DDPIXELFORMAT        ddpfBestMatch;
	LPDIRECTDRAWSURFACE7 pddsDXT    = NULL;
	LPDIRECTDRAWSURFACE7 pddsUncomp = NULL;

	if (FAILED (hr = ReadDDSSurface (file, &ddsd, &pddsDXT, flags))) {
		return hr;
	}
	if (FAILED (hr = LookupPixelFormat (ddsd.ddpfPixelFormat, &ddpfBestMatch))) {
		LOGOUT_DDERR(hr);
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
		ddsd.ddpfPixelFormat = ddpfBestMatch;
		if (FAILED (hr = BltToUncompressedSurface (ddsd, ddpfBestMatch,
			pddsDXT, &pddsUncomp))) {
			LOGOUT_DDERR(hr);
			return hr;
		}
		pddsDXT->Release();
		*ppdds = pddsUncomp;
	}
	return hr;
}

HRESULT TextureManager2::ReadCompatibleSurfaceFromMemory (BYTE *buf, DWORD nbuf,
	LPDIRECTDRAWSURFACE7 *ppdds, DWORD flags)
{
	HRESULT              hr;
	DDSURFACEDESC2       ddsd;
	DDPIXELFORMAT        ddpfBestMatch;
	LPDIRECTDRAWSURFACE7 pddsDXT    = NULL;
	LPDIRECTDRAWSURFACE7 pddsUncomp = NULL;

	if (FAILED (hr = ReadDDSSurfaceFromMemory (buf, nbuf, &ddsd, &pddsDXT, flags))) {
		return hr;
	}
	if (FAILED (hr = LookupPixelFormat (ddsd.ddpfPixelFormat, &ddpfBestMatch))) {
		LOGOUT_DDERR(hr);
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
		ddsd.ddpfPixelFormat = ddpfBestMatch;
		if (FAILED (hr = BltToUncompressedSurface (ddsd, ddpfBestMatch,
			pddsDXT, &pddsUncomp))) {
			LOGOUT_DDERR(hr);
			return hr;
		}
		pddsDXT->Release();
		*ppdds = pddsUncomp;
	}
	return hr;
}

#pragma optimize ("", off)

HRESULT TextureManager2::ReadDDSSurface (FILE *file,
	DDSURFACEDESC2 *pddsd, LPDIRECTDRAWSURFACE7* ppddsDXT, DWORD flags)
{
	return ::ReadDDSSurface (file, pDD, pddsd, ppddsDXT, flags);
}

HRESULT TextureManager2::ReadDDSSurfaceFromMemory (BYTE *buf, DWORD nbuf,
	DDSURFACEDESC2 *pddsd, LPDIRECTDRAWSURFACE7* ppddsDXT, DWORD flags)
{
	return ::ReadDDSSurfaceFromMemory (buf, nbuf, pDD, pddsd, ppddsDXT, flags);
}

HRESULT TextureManager2::FindBestPixelFormatMatch (DDPIXELFORMAT ddsdDDSTexture,
	DDPIXELFORMAT* pddsdBestMatch)
{
	return ::FindBestPixelFormatMatch (pDev, ddsdDDSTexture, pddsdBestMatch);
}

HRESULT TextureManager2::LookupPixelFormat (DDPIXELFORMAT ddsdDDSTexture,
	DDPIXELFORMAT *pddsdBestMatch)
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
		LOGOUT_DDERR(hr);
		return hr;
	}

	// Enter new pixel format to list
	if (npfp < MAXFMT) {
		pfp[npfp].pixelfmt = ddsdDDSTexture;
		pfp[npfp].bestmatch = *pddsdBestMatch;
		npfp++;
	}
	return S_OK;
}

HRESULT TextureManager2::BltToUncompressedSurface (DDSURFACEDESC2 ddsd,
	DDPIXELFORMAT ddpf, LPDIRECTDRAWSURFACE7 pddsDXT,
	LPDIRECTDRAWSURFACE7 *ppddsUncomp)
{
	return ::BltToUncompressedSurface (pDD, ddsd, ddpf, pddsDXT, ppddsUncomp);
}

// =======================================================================
// The following helper routines for loading compressed textures from
// DDS files have been adapted from the Compress example in the SDK
// =======================================================================

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
// ReadDDSSurface()
// Read a compressed DDS surface from a stream
//    pddsdComp     contains the DDS surface description, and
//    pppddsCompTop contains the DDS surface
// =======================================================================

HRESULT ReadDDSSurface (FILE *file, LPDIRECTDRAW7 pDD,
	DDSURFACEDESC2 *pddsd, LPDIRECTDRAWSURFACE7* ppddsDXT, DWORD flags)
{
	CD3DFramework7      *framework = g_pOrbiter->GetInlineGraphicsClient()->GetFramework();
	HRESULT              hr = E_FAIL;
	LPDIRECTDRAWSURFACE7 pdds         = NULL;
	LPDIRECTDRAWSURFACE7 pddsAttached = NULL;
	DDSURFACEDESC2       ddsd;
	DWORD                dwMagic;
	bool                 bLoadMip = ((flags&4) == 0 && framework->SupportsMipmaps());

	// Read the magic number
	if (!fread (&dwMagic, sizeof(DWORD), 1, file))
		goto LFail;
	if (dwMagic != MAKEFOURCC('D','D','S',' ')) {
		LOGOUT_ERR("Invalid DDS signature");
		goto LFail;
	}

	// Read the surface description
	fread (pddsd, sizeof(DDSURFACEDESC2), 1, file);

	// Mask/set surface caps appropriately for the application
	if ((framework->GetDeviceMemType() == DDSCAPS_VIDEOMEMORY) && !(flags&1))
		pddsd->ddsCaps.dwCaps2 |= DDSCAPS2_TEXTUREMANAGE;
	else
		pddsd->ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;

	// this should only be set for textures which will never be
	// locked for dynamic modification
	pddsd->ddsCaps.dwCaps2 |= DDSCAPS2_OPAQUE;

    if (!bLoadMip) { // remove mipmap parameters if not requested
        pddsd->dwMipMapCount = 0;
        pddsd->dwFlags &= ~DDSD_MIPMAPCOUNT;
        pddsd->ddsCaps.dwCaps &= ~(DDSCAPS_MIPMAP | DDSCAPS_COMPLEX);
    }

	// Clear unwanted flags
	pddsd->dwFlags &= (~DDSD_PITCH);
	pddsd->dwFlags &= (~DDSD_LINEARSIZE);

    // create a new surface based on the surface description
    if (FAILED (hr = pDD->CreateSurface(pddsd, ppddsDXT, NULL))) {
		LOGOUT_DDERR(hr);
        goto LFail;
	}
    pdds = *ppddsDXT;
    pdds->AddRef();

	while (TRUE) {
		ZeroMemory (&ddsd, sizeof (DDSURFACEDESC2));
		ddsd.dwSize = sizeof (DDSURFACEDESC2);

		if (FAILED (hr = pdds->Lock (NULL, &ddsd, DDLOCK_WAIT, NULL))) {
			LOGOUT_DDERR(hr);
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

		if (!bLoadMip) { // skip loading of mipmaps
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

HRESULT ReadDDSSurfaceFromMemory (BYTE *buf, DWORD nbuf, LPDIRECTDRAW7 pDD,
	DDSURFACEDESC2 *pddsd, LPDIRECTDRAWSURFACE7* ppddsDXT, DWORD flags)
{
	CD3DFramework7      *framework = g_pOrbiter->GetInlineGraphicsClient()->GetFramework();
	HRESULT              hr = E_FAIL;
	LPDIRECTDRAWSURFACE7 pdds         = NULL;
	LPDIRECTDRAWSURFACE7 pddsAttached = NULL;
	DDSURFACEDESC2       ddsd;
	DWORD                dwMagic;
	bool                 bLoadMip = ((flags&4) == 0 && framework->SupportsMipmaps());

	// Read the magic number
	if (nbuf >= sizeof(DWORD)) {
		memcpy(&dwMagic, buf, sizeof(DWORD));
		buf += sizeof(DWORD);
		nbuf -= sizeof(DWORD);
	} else goto LFail;
	if (dwMagic != MAKEFOURCC('D','D','S',' ')) {
		LOGOUT_ERR("Invalid DDS signature");
		goto LFail;
	}

	// Read the surface description
	memcpy(pddsd, buf, sizeof(DDSURFACEDESC2));
	buf += sizeof(DDSURFACEDESC2);
	nbuf -= sizeof(DDSURFACEDESC2);

	// Mask/set surface caps appropriately for the application
	if ((framework->GetDeviceMemType() == DDSCAPS_VIDEOMEMORY) && !(flags&1))
		pddsd->ddsCaps.dwCaps2 |= DDSCAPS2_TEXTUREMANAGE;
	else
		pddsd->ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;

	// this should only be set for textures which will never be
	// locked for dynamic modification
	pddsd->ddsCaps.dwCaps2 |= DDSCAPS2_OPAQUE;

    if (!bLoadMip) { // remove mipmap parameters if not requested
        pddsd->dwMipMapCount = 0;
        pddsd->dwFlags &= ~DDSD_MIPMAPCOUNT;
        pddsd->ddsCaps.dwCaps &= ~(DDSCAPS_MIPMAP | DDSCAPS_COMPLEX);
    }

	// Clear unwanted flags
	pddsd->dwFlags &= (~DDSD_PITCH);
	pddsd->dwFlags &= (~DDSD_LINEARSIZE);

    // create a new surface based on the surface description
    if (FAILED (hr = pDD->CreateSurface(pddsd, ppddsDXT, NULL))) {
		LOGOUT_DDERR(hr);
        goto LFail;
	}
    pdds = *ppddsDXT;
    pdds->AddRef();

	while (TRUE) {
		ZeroMemory (&ddsd, sizeof (DDSURFACEDESC2));
		ddsd.dwSize = sizeof (DDSURFACEDESC2);

		if (FAILED (hr = pdds->Lock (NULL, &ddsd, DDLOCK_WAIT, NULL))) {
			LOGOUT_DDERR(hr);
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

		if (!bLoadMip) { // skip loading of mipmaps
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
HRESULT TextureManager2::ReadRawDDSSurface (const char *name, RAWDDS &dds, DWORD flags)
{
	CD3DFramework7 *framework = g_pOrbiter->GetInlineGraphicsClient()->GetFramework();
	FILE           *file;
	HRESULT         hr = E_FAIL;
	DWORD           dwMagic;
	LONG			dwDataSize;
	bool            bLoadMip = ((flags&4) == 0 && framework->SupportsMipmaps());

	if (!(file = g_pOrbiter->OpenTextureFile (name, ".dds")))
		return hr;

	// Read the magic number
	if (!fread (&dwMagic, sizeof(DWORD), 1, file))
		goto LFail;
	if (dwMagic != MAKEFOURCC('D','D','S',' ')) {
		LOGOUT_ERR("Invalid DDS signature");
		goto LFail;
	}

	// Read the surface description
	fread (&dds.ddsd, sizeof(DDSURFACEDESC2), 1, file);

	// Mask/set surface caps appropriately for the application
	if ((framework->GetDeviceMemType() == DDSCAPS_VIDEOMEMORY) && !(flags&1))
		dds.ddsd.ddsCaps.dwCaps2 |= DDSCAPS2_TEXTUREMANAGE;
	else
		dds.ddsd.ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;

	// this should only be set for textures which will never be
	// locked for dynamic modification
	dds.ddsd.ddsCaps.dwCaps2 |= DDSCAPS2_OPAQUE;

    if (!bLoadMip) { // remove mipmap parameters if not requested
        dds.ddsd.dwMipMapCount = 0;
        dds.ddsd.dwFlags &= ~DDSD_MIPMAPCOUNT;
        dds.ddsd.ddsCaps.dwCaps &= ~(DDSCAPS_MIPMAP | DDSCAPS_COMPLEX);
    }

	if (dds.ddsd.dwFlags & DDSD_LINEARSIZE) {
		dwDataSize = dds.ddsd.dwLinearSize;
	} else {
		LONG dataBytesPerRow = dds.ddsd.dwWidth * dds.ddsd.ddpfPixelFormat.dwRGBBitCount / 8;
		dwDataSize = dds.ddsd.dwHeight * dataBytesPerRow;
	}
	dds.data = new BYTE[dwDataSize];
	fread (dds.data, dwDataSize, 1, file);
	hr = S_OK; // Everything worked

LFail:
	fclose (file);
	return hr;
}

// =======================================================================

HRESULT TextureManager2::SurfaceFromData (RAWDDS &dds, LPDIRECTDRAWSURFACE7 *pptex)
{
	HRESULT              hr = E_FAIL;
	LPDIRECTDRAWSURFACE7 pdds = NULL;
	DDSURFACEDESC2       ddsd;

	// Clear unwanted flags
	dds.ddsd.dwFlags &= (~DDSD_PITCH);
	dds.ddsd.dwFlags &= (~DDSD_LINEARSIZE);

    if (FAILED (hr = pDD->CreateSurface(&dds.ddsd, pptex, NULL))) {
		LOGOUT_DDERR(hr);
        goto LFail;
	}
    pdds = *pptex;
    pdds->AddRef();

	ZeroMemory (&ddsd, sizeof (DDSURFACEDESC2));
	ddsd.dwSize = sizeof (DDSURFACEDESC2);
	if (FAILED (hr = pdds->Lock (NULL, &ddsd, DDLOCK_WAIT, NULL))) {
		LOGOUT_DDERR(hr);
		goto LFail;
	}
	if (ddsd.dwFlags & DDSD_LINEARSIZE) {
		memcpy (ddsd.lpSurface, dds.data, ddsd.dwLinearSize);
	} else {
		DWORD yp;
		BYTE *pbDest = (BYTE*)ddsd.lpSurface;
		BYTE *pbSrc  = dds.data;
		LONG dataBytesPerRow = ddsd.dwWidth * ddsd.ddpfPixelFormat.dwRGBBitCount / 8;
		for (yp = 0; yp < ddsd.dwHeight; yp++) {
			memcpy (pbDest, pbSrc, dataBytesPerRow);
			pbSrc  += dataBytesPerRow;
			pbDest += ddsd.lPitch;
		}
	}
	pdds->Release();

	pdds->Unlock (NULL);
	hr = S_OK; // Everything worked

LFail:
	delete []dds.data;
	dds.data = NULL;
	return hr;
}

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
    pNode = new PixelFormatNode; TRACENEW
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

// =======================================================================
// FindBestPixelFormatMatch()
// Given a pixel format from a compressed surface, it finds the best
// pixel format match format that is supported by the current
// renderer.  pddsdBestMatch contains the best match found.
// =======================================================================

HRESULT FindBestPixelFormatMatch (LPDIRECT3DDEVICE7 pd3dDevice, 
    DDPIXELFORMAT ddsdDDSTexture, DDPIXELFORMAT* pddsdBestMatch)
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
    if (FAILED (hr = pd3dDevice->EnumTextureFormats (EnumTextureFormats, 
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
// BltToUncompressedSurface()
// Creates an uncompressed surface and blits the compressed surface to 
// it using the specified pixel format.
// =======================================================================

HRESULT BltToUncompressedSurface (LPDIRECTDRAW7 pDD, DDSURFACEDESC2 ddsd, 
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
		LOGOUT_DDERR(hr);
        return hr;
	}
    *ppddsNewSurface = pddsNew;

    // Copy compress image to un-compressed surface, including mips (if any)
    while (TRUE) {
        if (FAILED (hr = pddsNew->Blt (NULL, pddsDXT, NULL, DDBLT_WAIT, NULL))) {
			LOGOUT_DDERR(hr);
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
// DDCopyBitmap()
// Draw a bitmap into a DirectDrawSurface
// =======================================================================

HRESULT DDCopyBitmap (LPDIRECTDRAWSURFACE7 pdds, HBITMAP hbm,
    int x, int y, int dx, int dy)
{
    HDC                     hdcImage;
    HDC                     hdc;
    BITMAP                  bm;
    DDSURFACEDESC2          ddsd;
    HRESULT                 hr;

    if (hbm == NULL || pdds == NULL)
        return E_FAIL;
    //
    // Make sure this surface is restored.
    //
    pdds->Restore();
    //
    // Select bitmap into a memoryDC so we can use it.
    //
    hdcImage = CreateCompatibleDC(NULL);
    if (!hdcImage)
        OutputDebugString("createcompatible dc failed\n");
    SelectObject(hdcImage, hbm);
    //
    // Get size of the bitmap
    //
    GetObject(hbm, sizeof(bm), &bm);
    dx = dx == 0 ? bm.bmWidth : dx;     // Use the passed size, unless zero
    dy = dy == 0 ? bm.bmHeight : dy;
    //
    // Get size of surface.
    //
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;
    pdds->GetSurfaceDesc(&ddsd);

    if ((hr = pdds->GetDC(&hdc)) == DD_OK)
    {
        StretchBlt(hdc, 0, 0, ddsd.dwWidth, ddsd.dwHeight, hdcImage, x, y,
                   dx, dy, SRCCOPY);
        pdds->ReleaseDC(hdc);
    }
    DeleteDC(hdcImage);
    return hr;
}

LPDIRECTDRAWSURFACE7 DDCopyBitmap (LPDIRECTDRAW7 pDD, HBITMAP hbm)
{
    BITMAP               bm;
	DDSURFACEDESC2       ddsd; 
    LPDIRECTDRAWSURFACE7 pdds;
	//
	// Get the size of the bitmap.
	// 
    GetObject (hbm, sizeof(bm), &bm);
	// 
    // Now, return to DirectX function calls. 
    // Create a DirectDrawSurface for this bitmap.
	// 
    ZeroMemory (&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd); 
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH; 
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN; 
    ddsd.dwWidth = bm.bmWidth;
	ddsd.dwHeight = bm.bmHeight;  
    if (pDD->CreateSurface(&ddsd, &pdds, NULL) != DD_OK)
		return NULL;  
    DDCopyBitmap (pdds, hbm, 0, 0, 0, 0);
	DeleteObject (hbm);  
    return pdds; 
} 

LPDIRECTDRAWSURFACE7 CreateSurfaceFromBmp (HBITMAP hBmp, bool release_bmp)
{
	BITMAP bm;
    GetObject (hBmp, sizeof(bm), &bm);
	LPDIRECTDRAWSURFACE7 surf;
	LPDIRECTDRAW7 pDD = g_pOrbiter->GetInlineGraphicsClient()->GetDirectDraw();
	DDSURFACEDESC2 ddsd;
    ZeroMemory (&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd); 
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH; 
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
    ddsd.dwWidth  = bm.bmWidth;
	ddsd.dwHeight = bm.bmHeight;
	if (pDD->CreateSurface (&ddsd, &surf, NULL) != DD_OK)
		return NULL;
	if (DDCopyBitmap (surf, hBmp, 0, 0, 0, 0) != S_OK) {
		surf->Release();
		return NULL;
	}
	if (release_bmp) DeleteObject ((HGDIOBJ)hBmp);
	return surf;
}

bool WriteDIB(const char *fname, HANDLE hDIB)
{
	BITMAPFILEHEADER	hdr;
	LPBITMAPINFOHEADER	lpbi;

	if (!hDIB)
		return FALSE;

	FILE *file = fopen (fname, "wb");
	if (!file) return false;

	lpbi = (LPBITMAPINFOHEADER)hDIB;

	int nColors = 1 << lpbi->biBitCount;

	// Fill in the fields of the file header 
	hdr.bfType		= ((WORD) ('M' << 8) | 'B');	// is always "BM"
	hdr.bfSize		= GlobalSize (hDIB) + sizeof( hdr );
	hdr.bfReserved1 	= 0;
	hdr.bfReserved2 	= 0;
	hdr.bfOffBits		= (DWORD) (sizeof( hdr ) + lpbi->biSize +
						nColors * sizeof(RGBQUAD));

	// Write the file header
	fwrite (&hdr, sizeof(hdr), 1, file);

	// Write the DIB header and the bits
	fwrite (lpbi, GlobalSize(hDIB), 1, file);

	fclose (file);
	return true;
}

void FillSurface (LPDIRECTDRAWSURFACE7 surf, DWORD col)
{
	static DDBLTFX ddbf;
	ddbf.dwSize = sizeof(DDBLTFX);
	ddbf.dwFillColor = col;
	surf->Blt (NULL, NULL, NULL, DDBLT_COLORFILL, &ddbf);
}

LPDIRECTDRAWSURFACE7 CreateTexture (int w, int h)
{
	LPDIRECTDRAW7 pDD = g_pOrbiter->GetInlineGraphicsClient()->GetDirectDraw();
	LPDIRECTDRAWSURFACE7 surf;
	DDSURFACEDESC2 ddsd;
	ZeroMemory (&ddsd, sizeof(ddsd));
	ddsd.dwSize = sizeof(ddsd);
	ddsd.dwFlags = DDSD_WIDTH | DDSD_HEIGHT | DDSD_PIXELFORMAT | DDSD_CAPS;
	ddsd.dwWidth = w;
	ddsd.dwHeight = h;
	ddsd.ddsCaps.dwCaps = DDSCAPS_TEXTURE;
	ddsd.ddpfPixelFormat.dwSize = sizeof (DDPIXELFORMAT);
	ddsd.ddpfPixelFormat.dwFlags = 0;
	g_pOrbiter->GetInlineGraphicsClient()->GetFramework()->GetBackBuffer()->GetPixelFormat (&ddsd.ddpfPixelFormat);
	if (pDD->CreateSurface (&ddsd, &surf, NULL) != DD_OK)
		return NULL;
	return surf;
}

// EVERYTHING BELOW IS OBSOLETE!

// =======================================================================
// Local prototypes

BOOL g_bSupportsMipmaps = FALSE; // should be set from device caps
BOOL g_bMipTexture = FALSE;
CHAR g_strDiskPixelFormat[20];

// =======================================================================
// class TextureManager

TextureManager::TextureManager (LPDIRECT3DDEVICE7 _dev, int _maxsize)
{
	dev = _dev;
	rec0 = 0, recN = 0;
	pfp = new PixelFormatPair[pfp_buflen = 4]; TRACENEW
	npfp = 0;
	alloc_size = 0;
	maxsize = _maxsize;
	ntex = nactive = 0;
	texturepath[0] = '\0';
}

TextureManager::~TextureManager ()
{
	Clear();
	delete []pfp;
	pfp = NULL;
}

void TextureManager::UnsetDevice ()
{
	TextureRec *r;
	for (r = rec0; r; r = r->next) {
		if (r->tex) r->tex->Release();
		r->tex = 0;
        ZeroMemory (&r->ddsd, sizeof(DDSURFACEDESC2));
		r->size = 0;
	}
	alloc_size = 0;
	dev = 0;
}

void TextureManager::SetDevice (LPDIRECT3DDEVICE7 _dev)
{
	TextureRec new_rec, *r;
	dev = _dev;

	// find pixel format matches for existing formats
	for (int i = 0; i < npfp; i++)
		FindBestPixelFormatMatch (dev, pfp[i].pixelfmt, &pfp[i].bestmatch);

	// re-read textures for all existing records
	for (r = rec0; r; r = r->next) {
		if (SUCCEEDED (LoadTexture (r->fname, new_rec))) {
			r->tex = new_rec.tex;
			r->ddsd = new_rec.ddsd;
			r->size = new_rec.size;
			alloc_size += r->size;
		}
	}
}

void TextureManager::SetTexturePath (char *path)
{
	strcpy (texturepath, path);
	if (texturepath[0] && texturepath[strlen(texturepath)-1] != '\\')
		strcat (texturepath, "\\");
}

LPDIRECTDRAWSURFACE7 TextureManager::AcquireTexture (const char *fname, bool uncompress)
{
	LPDIRECTDRAWSURFACE7 tex;

	if (!(tex = FindRec (fname))) {
		TextureRec *new_rec = new TextureRec; TRACENEW
		if (SUCCEEDED (LoadTexture (fname, *new_rec, uncompress))) {
			tex = AddRec (new_rec);
		} else {
			LOGOUT_WARN("Texture not found: %s\nSkipping.", fname);
			delete new_rec;
		}
	}
	return tex;
}

bool TextureManager::IncRefCount (LPDIRECTDRAWSURFACE7 tex)
{
	TextureRec *r;
	for (r = rec0; r; r = r->next) {
		if (r->tex == tex) {
			r->active++;
			return true;
		}
	}
	return false;
}

bool TextureManager::DecRefCount (LPDIRECTDRAWSURFACE7 tex)
{
	TextureRec *r;
	for (r = rec0; r; r = r->next) {
		if (r->tex == tex) {
			if (r->active) {
				r->active--;
				return true;
			} else {
				return false;
			}
		}
	}
	return false;
}

bool TextureManager::ReleaseTexture (LPDIRECTDRAWSURFACE7 tex)
{
	TextureRec *r;
	for (r = rec0; r; r = r->next) {
		if (r->tex == tex) {
			if (r->active) {
				if (--(r->active) == 0) nactive--;
			}
			return true;
		}
	}
	return false;
}

bool TextureManager::DeallocRec ()
{
	TextureRec *r = recN;
	while (r && r->active) r = r->prev;
	if (!r) return false; // no inactive record found
	DeallocRec (r);
	return true;
}

void TextureManager::DeallocRec (TextureRec *_rec)
{
	// unlink record
	if (_rec->prev) _rec->prev->next = _rec->next;
	else            rec0 = _rec->next;
	if (_rec->next) _rec->next->prev = _rec->prev;
	else            recN = _rec->prev;

	// deallocate record
	if (_rec->active) nactive--;
	ntex--;
	alloc_size -= _rec->size;
	_rec->tex->Release();
	delete _rec;
}

void TextureManager::Clear ()
{
	while (ntex) DeallocRec (rec0);
}

LPDIRECTDRAWSURFACE7 TextureManager::AddRec (TextureRec *_rec)
{
	_rec->next = rec0;
	_rec->prev = 0;
	_rec->active = 1;
	if (rec0) rec0->prev = _rec;
	else      recN = _rec;
	rec0 = _rec;
	alloc_size += _rec->size;
	ntex++;
	nactive++;

	// If we exceed max allocation size, release textures
	// from end of list, but keep at least one
	while (alloc_size > maxsize && ntex > nactive)
		DeallocRec ();

	return _rec->tex;
}

LPDIRECTDRAWSURFACE7 TextureManager::FindRec (const char *fname)
{
	TextureRec *r;
	for (r = rec0; r; r = r->next) {
		if (!strcmp (r->fname, fname)) {
			// texture found, move to beginning of list
			// to mark as most recently accessed
			if (r->prev) {
				r->prev->next = r->next;
				if (r->next) r->next->prev = r->prev;
				else         recN = r->prev;
				r->next = rec0;
				r->prev = 0;
				rec0->prev = r;
				rec0 = r;
			}
			if (!r->active)
				nactive++;
			r->active++;
			return r->tex;
		}
	}
	return 0;
}

bool TextureManager::PixelFormatMatch (DDPIXELFORMAT ddpf, DDPIXELFORMAT &match)
{
	int i;

	for (i = 0; i < npfp; i++)
		if (!memcmp (&pfp[i].pixelfmt, &ddpf, sizeof (DDPIXELFORMAT))) {
			match = pfp[i].bestmatch;
			return true;
		}

	// Find new match and add to list
	if (FAILED (FindBestPixelFormatMatch (dev, ddpf, &match)))
		return false;
	if (npfp == pfp_buflen) {
		PixelFormatPair *tmp = new PixelFormatPair[pfp_buflen += 4]; TRACENEW
		memcpy (tmp, pfp, npfp*sizeof(PixelFormatPair));
		delete []pfp;
		pfp = tmp;
	}
	pfp[npfp].pixelfmt = ddpf;
	pfp[npfp].bestmatch = match;
	npfp++;
	return true;
}

HRESULT TextureManager::LoadTexture (const char *fname, TextureRec &rec, bool uncompress)
{
    HRESULT              hr;
    LPDIRECTDRAWSURFACE7 pDDSNewTop = NULL;
    LPDIRECTDRAWSURFACE7 pDDSDXTTop = NULL;
    DDPIXELFORMAT        ddpfBestMatch;
	FILE                 *file;

	LPDIRECTDRAW7        pDD              = g_pOrbiter->GetInlineGraphicsClient()->GetDirectDraw();
	LPDIRECTDRAWSURFACE7 pddsRenderTarget = g_pOrbiter->GetInlineGraphicsClient()->GetRenderTarget();

	//char namebuf[256], *fullname;
	//if (texturepath[0]) {
	//	strcpy (namebuf, texturepath);
	//	strcat (namebuf, fname);
	//	fullname = namebuf;
	//} else {
	//	fullname = fname;
	//}

#ifdef UNDEF
    // Get the render target surface
    if (FAILED (hr = dev->GetRenderTarget (&pddsRenderTarget))) {
		LOGOUT1P("ERROR: TextureManager::LoadTexture|GetRenderTarget (code %d)", hr);
        goto LFail;
	}
    // Get a DDraw ptr (from render target) for creating surfaces
    if (FAILED (hr = pddsRenderTarget->GetDDInterface ((VOID**)&pDD)))
        goto LFail;
#endif

    // Create a DDS texture surface based on the dds file
    // this surface may or may not be compressed
	if ((file = g_pOrbiter->OpenTextureFile (fname, "")) == NULL) return E_FAIL;
	//if ((file = fopen (fullname, "rb")) == NULL) return E_FAIL;
	hr = ReadDDSSurface (file, pDD, &rec.ddsd, &pDDSDXTTop);
	fclose (file);
    if (FAILED (hr)) {
		LOGOUT_ERR("ReadDDSSurface failed (code: %d)", hr);
		return hr;
	}
    // enumerate all pixel formats, then choose the best match
    // based on the read-in DDS texture format
	if (!PixelFormatMatch (rec.ddsd.ddpfPixelFormat, ddpfBestMatch)) {
		LOGOUT_ERR("PixelFormatMatch failed");
        return hr;
	}

#ifdef UNDEF
    if (ddpfBestMatch.dwFlags & DDPF_ALPHAPREMULT) {
        // Use D3DBLEND_ONE if DDPF_ALPHAPREMULT is on
        dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_ONE);
    } else {
        // Use D3DBLEND_SRCALPHA if DDPF_ALPHAPREMULT is off
        dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
    }
#endif

    // Does the renderer support the compress texture format or 
    // is the dds texture already uncompressed? 
    if (uncompress || (rec.ddsd.ddpfPixelFormat.dwFourCC != ddpfBestMatch.dwFourCC)) {

        // blt the compressed surface to an uncompressed 
        // surface using the best pixel format match

		// TEMPORARY
		memset (&ddpfBestMatch, 0, sizeof (DDPIXELFORMAT));
		ddpfBestMatch.dwSize = sizeof (DDPIXELFORMAT);
		ddpfBestMatch.dwFlags = 0;
		g_pOrbiter->GetInlineGraphicsClient()->GetFramework()->GetBackBuffer()->GetPixelFormat (&ddpfBestMatch);


        if (FAILED (hr = BltToUncompressedSurface (pDD, rec.ddsd, ddpfBestMatch,
			pDDSDXTTop, &pDDSNewTop))) {
			LOGOUT_ERR("BltToUncompressedSurface failed (code: %d)", hr);
            return hr;
		}
		// get properties of uncompressed surface
		if (FAILED (hr = pDDSNewTop->GetSurfaceDesc (&rec.ddsd))) {
			LOGOUT_ERR("GetSurfaceDesc failed (code: %d)", hr);
			return hr;
		}
        // get the texture interface from the  new uncompressed texture surface
		rec.tex = pDDSNewTop;
		pDDSDXTTop->Release();
    } else {
        // don't uncompress texture since renderer 
        // natively supports this pixel format
		rec.tex = pDDSDXTTop;
    }
	strcpy (rec.fname, fname);
	if (rec.ddsd.dwFlags & DDSD_LINEARSIZE) {
		rec.size = rec.ddsd.dwLinearSize;
	} else {
		rec.size = rec.ddsd.dwWidth * rec.ddsd.dwHeight;
		if (rec.ddsd.ddpfPixelFormat.dwFlags & DDPF_RGB) {
			rec.size *= rec.ddsd.ddpfPixelFormat.dwRGBBitCount;
			rec.size /= 8;
		}
	}

    return S_OK;
}


void TextureManager::OutputInfo ()
{
	sprintf (DBG_MSG, "ntex=%d, nactive=%d, alloc=%dbytes", ntex, nactive, alloc_size);
}
