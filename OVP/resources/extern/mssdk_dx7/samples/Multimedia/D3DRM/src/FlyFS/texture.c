/*
 *  Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File: texture.c
 *
 *  Loads and manages textures.  Part of D3DApp.
 *
 *  D3DApp is a collection of helper functions for Direct3D applications.
 *  D3DApp consists of the following files:
 *      d3dapp.h    Main D3DApp header to be included by application
 *      d3dappi.h   Internal header
 *      d3dapp.c    D3DApp functions seen by application.
 *      ddcalls.c   All calls to DirectDraw objects except textures
 *      d3dcalls.c  All calls to Direct3D objects except textures
 *      texture.c   Texture loading and managing texture list
 *      misc.c      Miscellaneous calls
 */

// Borland CBuilder3 doesn't support nameless unions...
#define NONAMELESSUNION

#include "d3dappi.h"

#define MAGICBYTES 2

/*
 * STATIC FUNCTION DECLARATIONS
 */
static void D3DAppIAddPathList(const char *path);
static void D3DAppIInitialisePathList();
static FILE * D3DAppIFindFile(const char *name, const char *mode);
static BOOL loadPPMHeader(FILE *fp, DWORD *width, DWORD *height, DWORD *maxgrey);


/***************************************************************************/
/*                        Managing the texture list                        */
/***************************************************************************/
/*
 * D3DAppILoadTextureSurf
 * Creates a texture map surface and texture object from the numbered PPM
 * file.  This is done in a two step process.  A source texture surface and
 * object are created in system memory.  A second, initially empty, texture
 * surface is created (in video memory if hardware is present).  The source
 * texture is loaded into the destination texture surface and then discarded.
 * This process allows a device to compress or reformat a texture map as it
 * enters video memory during the Load call.
 */
BOOL
D3DAppILoadTextureSurf(int n, BOOL* bInVideo)
{
    DDSURFACEDESC ddsd;
    LPDIRECTDRAWSURFACE lpSrcTextureSurf = NULL;
    LPDIRECT3DTEXTURE2 lpSrcTexture = NULL;
    LPDIRECTDRAWPALETTE lpDstPalette = NULL;
    PALETTEENTRY ppe[256];
    DWORD pcaps;
    /*
     * Release the surface if it is hanging around
     */
    RELEASE(d3dappi.lpTextureSurf[n]);
    /*
     * Create a surface in system memory and load the PPM file into it.
     * Query for the texture interface.
     */
    lpSrcTextureSurf = D3DAppILoadSurface(d3dappi.lpDD, d3dappi.ImageFile[n],
                                          &d3dappi.ThisTextureFormat.ddsd,
                                          DDSCAPS_SYSTEMMEMORY);
    if (!lpSrcTextureSurf)
        goto exit_with_error;
    LastError = lpSrcTextureSurf->lpVtbl->QueryInterface(lpSrcTextureSurf,
                                             &IID_IDirect3DTexture2,
                                             (LPVOID*)&lpSrcTexture);
    if (LastError != DD_OK) {
        D3DAppISetErrorString("Failed to obtain D3D texture interface for a source texture.\n%s", D3DAppErrorToString(LastError));
        goto exit_with_error;
    }
    /*
     * Create an empty texture surface to load the source texture into.
     * The DDSCAPS_ALLOCONLOAD flag allows the DD driver to wait until the
     * load call to allocate the texture in memory because at this point,
     * we may not know how much memory the texture will take up (e.g. it
     * could be compressed to an unknown size in video memory).
     * Make sure SW renderers get textures in system memory
     */
    LastError = D3DAppIGetSurfDesc(&ddsd, lpSrcTextureSurf);
    if (LastError != DD_OK) {
        D3DAppISetErrorString("Could not get the surface description of the source texture.\n%s",
                              D3DAppErrorToString(LastError));
        goto exit_with_error;
    }
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH | DDSD_PIXELFORMAT;
    ddsd.ddsCaps.dwCaps = DDSCAPS_TEXTURE | DDSCAPS_ALLOCONLOAD;
    if (!d3dappi.ThisDriver.bIsHardware)
        ddsd.ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;
    LastError = D3DAppICreateSurface(&ddsd, &d3dappi.lpTextureSurf[n]);
    if (LastError != DD_OK) {
        D3DAppISetErrorString("Could not create the destination texture surface.\n%s",
                              D3DAppErrorToString(LastError));
        goto exit_with_error;
    }
    if (ddsd.ddpfPixelFormat.dwFlags & DDPF_PALETTEINDEXED8) {
        pcaps = DDPCAPS_8BIT | DDPCAPS_ALLOW256;
    } else if (ddsd.ddpfPixelFormat.dwFlags & DDPF_PALETTEINDEXED4) {
        pcaps = DDPCAPS_4BIT;
    } else {
        pcaps = 0;
    }
    if (pcaps) {
        memset(ppe, 0, sizeof(PALETTEENTRY) * 256);
        LastError = d3dappi.lpDD->lpVtbl->CreatePalette(d3dappi.lpDD, pcaps,
                                                 ppe, &lpDstPalette, NULL);
        if (LastError != DD_OK) {
            D3DAppISetErrorString("Failed to create a palette for the destination texture.\n%s",
                                  D3DAppErrorToString(LastError));
            goto exit_with_error;
        }
        LastError = d3dappi.lpTextureSurf[n]->lpVtbl->SetPalette(d3dappi.lpTextureSurf[n],
                                lpDstPalette);
        if (LastError != DD_OK) {
            D3DAppISetErrorString("Failed to set the destination texture's palette.\n%s",
                                  D3DAppErrorToString(LastError));
            goto exit_with_error;
        }
        lpDstPalette->lpVtbl->Release(lpDstPalette);
    }
    /*
     * Query our destination surface for a texture interface
     */
    LastError = d3dappi.lpTextureSurf[n]->lpVtbl->QueryInterface(d3dappi.lpTextureSurf[n],
                                             &IID_IDirect3DTexture2,
                                             (LPVOID*)&d3dappi.lpTexture[n]);
    if (LastError != DD_OK) {
        D3DAppISetErrorString("Failed to obtain D3D texture interface for a destination texture.\n%s",
                              D3DAppErrorToString(LastError));
        goto exit_with_error;
    }
    /*
     * Load the source texture into the destination.  During this call, a
     * driver could compress or reformat the texture surface and put it in
     * video memory.
     */
    LastError = d3dappi.lpTexture[n]->lpVtbl->Load(d3dappi.lpTexture[n], lpSrcTexture);
    if (LastError != DD_OK) {
        D3DAppISetErrorString("Could not load a source texture into a destination texture.\n%s",
                              D3DAppErrorToString(LastError));
        goto exit_with_error;
    }

    /*
     * Now we are done with the source texture
     */
    RELEASE(lpSrcTexture);
    RELEASE(lpSrcTextureSurf);

    /*
     * Did the texture end up in video memory?
     */
    LastError = D3DAppIGetSurfDesc(&ddsd, d3dappi.lpTextureSurf[n]);
    if (LastError != DD_OK) {
        D3DAppISetErrorString("Could not get the surface description of the loaded texture surface.\n%s",
                              D3DAppErrorToString(LastError));
        goto exit_with_error;
    }
    if (ddsd.ddsCaps.dwCaps & DDSCAPS_VIDEOMEMORY)
        *bInVideo = TRUE;
    else
        *bInVideo = FALSE;


    return TRUE;

exit_with_error:
    RELEASE(lpSrcTexture);
    RELEASE(lpSrcTextureSurf);
    RELEASE(lpDstPalette);
    RELEASE(d3dappi.lpTexture[n]);
    RELEASE(d3dappi.lpTextureSurf[n]);
    return FALSE;
}

/*
 * D3DAppIReloadTextureSurf
 * Reloads a lost and restored texture surface
 */
BOOL
D3DAppIReloadTextureSurf(int n)
{
    LPDIRECTDRAWSURFACE lpSrcTextureSurf = NULL;
    LPDIRECT3DTEXTURE2 lpSrcTexture = NULL;

    /*
     * Create a surface in system memory and load the PPM file into it.
     * Query for the texture interface.
     */
    lpSrcTextureSurf = D3DAppILoadSurface(d3dappi.lpDD, d3dappi.ImageFile[n],
                                          &d3dappi.ThisTextureFormat.ddsd,
                                          DDSCAPS_SYSTEMMEMORY);
    if (!lpSrcTextureSurf)
        goto exit_with_error;
    LastError = lpSrcTextureSurf->lpVtbl->QueryInterface(lpSrcTextureSurf,
                                             &IID_IDirect3DTexture2,
                                             (LPVOID*)&lpSrcTexture);
    if (LastError != DD_OK) {
        D3DAppISetErrorString("Failed to obtain D3D texture interface for a source texture.\n%s", D3DAppErrorToString(LastError));
        goto exit_with_error;
    }
    /*
     * Load the source texture into the destination.  During this call, a
     * driver could compress or reformat the texture surface and put it in
     * video memory.
     */
    LastError = d3dappi.lpTexture[n]->lpVtbl->Load(d3dappi.lpTexture[n], lpSrcTexture);
    if (LastError != DD_OK) {
        D3DAppISetErrorString("Could not load a source texture into a destination texture.\n%s",
                              D3DAppErrorToString(LastError));
        goto exit_with_error;
    }
    /*
     * Now we are done with the source texture
     */
    RELEASE(lpSrcTexture);
    RELEASE(lpSrcTextureSurf);

    return TRUE;

exit_with_error:
    RELEASE(lpSrcTexture);
    RELEASE(lpSrcTextureSurf);
    return FALSE;
}


/*
 * D3DAppIGetTextureHandle
 * Get a texture handle from the current D3D device for this texture and save
 * it in the MasterTextureHandle list and public texture handle list.
 */
BOOL
D3DAppIGetTextureHandle(int n)
{
    LastError = d3dappi.lpTexture[n]->lpVtbl->GetHandle(d3dappi.lpTexture[n],
                               d3dappi.lpD3DDevice, &MasterTextureHandle[n]);
    if (LastError != DD_OK) {
        D3DAppISetErrorString("Could not get a handle to loaded texture %i.\n%s",
                              n, D3DAppErrorToString(LastError));
        goto exit_with_error;
    }
    /*
     * If textures are enabled, put the handle in the public texture list,
     * otherwise, keep it as zero.
     */
    if (!d3dappi.bTexturesDisabled) {
        d3dappi.TextureHandle[n] = MasterTextureHandle[n];
    } else {
        d3dappi.TextureHandle[n] = 0;
    }
    return TRUE;
exit_with_error:
    MasterTextureHandle[n] = 0;
    d3dappi.TextureHandle[n] = 0;
    return FALSE;
}

/*
 * D3DAppIReleaseTexture
 * Release this texture surface and texture interface.  Remember, a texture
 * handle is NOT and object and does not need to be released or destroyed.
 * The handle is no longer valid after the device is destroyed, so set it to
 * zero here.
 */
void
D3DAppIReleaseTexture(int n)
{
    RELEASE(d3dappi.lpTexture[n]);
    RELEASE(d3dappi.lpTextureSurf[n]);
    MasterTextureHandle[n] = 0;
    d3dappi.TextureHandle[n] = 0;
}

/*
 * D3DAppIReleaseAllTextures
 * Release all texture surfaces and texture interfaces
 */
void
D3DAppIReleaseAllTextures(void)
{
    int i;
    for (i = 0; i < d3dappi.NumTextures; i++) {
        D3DAppIReleaseTexture(i);
    }
}

/*
 * D3DAppILoadAllTextures
 * Load all texture surfaces, qeury them for texture interfaces and get
 * handles for them from the current D3D driver.
 */
BOOL
D3DAppILoadAllTextures(void)
{
    int i;
    if (d3dappi.ThisDriver.bDoesTextures) {
        d3dappi.NumUsableTextures = 0;
        for (i = 0; i < d3dappi.NumTextures; i++) {
            BOOL bInVideo;
            ATTEMPT(D3DAppILoadTextureSurf(i, &bInVideo));
            if (!bInVideo && d3dappi.ThisDriver.bIsHardware) {
                /*
                 * If a texture fails to load into video memory for a hardware
                 * device, stop the NumUsableTextures count here.
                 */
                D3DAppIReleaseTexture(i);
                break;
            } else {
                ++d3dappi.NumUsableTextures;
            }
        }
        for (i = 0; i < d3dappi.NumUsableTextures; i++) {
            ATTEMPT(D3DAppIGetTextureHandle(i));
        }
    } else {
        d3dappi.NumUsableTextures = 0;
    }
    return TRUE;

exit_with_error:
    for (i = 0; i < d3dappi.NumTextures; i++) {
        D3DAppIReleaseTexture(i);
    }
    return FALSE;
}

/***************************************************************************/
/*                    Loading a PPM file into a surface                    */
/***************************************************************************/
/*
 * LoadSurface
 * Loads a ppm file into a texture map DD surface of the given format.  The
 * memory flag specifies DDSCAPS_SYSTEMMEMORY or DDSCAPS_VIDEOMEMORY.
 */
LPDIRECTDRAWSURFACE
D3DAppILoadSurface(LPDIRECTDRAW lpDD, LPCSTR lpName,
                   LPDDSURFACEDESC lpFormat, DWORD memoryflag)
{
    LPDIRECTDRAWSURFACE lpDDS;
    DDSURFACEDESC ddsd, format;
    D3DCOLOR colors[256];
    D3DCOLOR c;
    DWORD dwWidth, dwHeight, dwMaxGrey;
    int i, j;
    FILE *fp;
    char *lpC;
    LPDIRECTDRAWPALETTE lpDDPal;
    PALETTEENTRY ppe[256];
    int psize;
    DWORD pcaps;
    int color_count;
    BOOL bQuant = FALSE;
    HRESULT ddrval;

    /*
     * Find the image file and open it
     */
    fp = D3DAppIFindFile(lpName, "rb");
    if (fp == NULL) {
        D3DAppISetErrorString("Cannot find %s.\n", lpName);
        return NULL;
    }
    /*
     * Parse the PPM header
     */
    if (!loadPPMHeader(fp, &dwWidth, &dwHeight, &dwMaxGrey)) {
        fclose(fp);
        D3DAppISetErrorString("Could not load or parse PPM header in %s.\n", lpName);
        return NULL;
    }
    /*
     * Create a surface of the given format using the dimensions of the PPM
     * file.
     */
    memcpy(&format, lpFormat, sizeof(DDSURFACEDESC));
    if (format.ddpfPixelFormat.dwFlags & DDPF_PALETTEINDEXED8) {
        bQuant = TRUE;
        psize = 256;
        pcaps = DDPCAPS_8BIT | DDPCAPS_ALLOW256;
    } else if (format.ddpfPixelFormat.dwFlags & DDPF_PALETTEINDEXED4) {
        bQuant = TRUE;
        psize = 16;
        pcaps = DDPCAPS_4BIT;
    }
    memcpy(&ddsd, &format, sizeof(DDSURFACEDESC));
    ddsd.dwSize = sizeof(DDSURFACEDESC);
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH | DDSD_PIXELFORMAT;
    ddsd.ddsCaps.dwCaps = DDSCAPS_TEXTURE | memoryflag;
    ddsd.dwHeight = dwHeight;
    ddsd.dwWidth = dwWidth;

    ddrval = lpDD->lpVtbl->CreateSurface(lpDD, &ddsd, &lpDDS, NULL);
    if (ddrval != DD_OK) {
        D3DAppISetErrorString("CreateSurface for texture failed (loadtex).\n%s",
                              D3DAppErrorToString(ddrval));
        return NULL;
    }
    /*
     * Lock the surface so it can be filled with the PPM file
     */
    memset(&ddsd, 0, sizeof(DDSURFACEDESC));
    ddsd.dwSize = sizeof(DDSURFACEDESC);
    ddrval = lpDDS->lpVtbl->Lock(lpDDS, NULL, &ddsd, 0, NULL);
    if (ddrval != DD_OK) {
        lpDDS->lpVtbl->Release(lpDDS);
        D3DAppISetErrorString("Lock failed while loading surface (loadtex).\n%s",
                              D3DAppErrorToString(ddrval));
        return NULL;
    }
    /*
     * The method of loading depends on the pixel format of the dest surface
     */
    if (!bQuant) {
        /*
         * The texture surface is not palettized
         */
        unsigned long* lpLP;
        unsigned short* lpSP;
        unsigned char* lpCP;
        unsigned long m;
        int s;
        int red_shift, red_scale;
        int green_shift, green_scale;
        int blue_shift, blue_scale;
        /*
         * Determine the red, green and blue masks' shift and scale.
         */
        for (s = 0, m = format.ddpfPixelFormat.u2.dwRBitMask; !(m & 1);
                                                               s++, m >>= 1);
        red_shift = s;
        red_scale = 255 / (format.ddpfPixelFormat.u2.dwRBitMask >> s);
        for (s = 0, m = format.ddpfPixelFormat.u3.dwGBitMask; !(m & 1);
                                                               s++, m >>= 1);
        green_shift = s;
        green_scale = 255 / (format.ddpfPixelFormat.u3.dwGBitMask >> s);
        for (s = 0, m = format.ddpfPixelFormat.u4.dwBBitMask; !(m & 1);
                                                               s++, m >>= 1);
        blue_shift = s;
        blue_scale = 255 / (format.ddpfPixelFormat.u4.dwBBitMask >> s);
        /*
         * Each RGB bit count requires different pointers
         */
        switch (format.ddpfPixelFormat.u1.dwRGBBitCount) {
            case 32 :
                for (j = 0; j < (int)dwHeight; j++) {
                    /*
                     * Point to next row in texture surface
                     */
                    lpLP = (unsigned long*)(((char*)ddsd.lpSurface) +
                                                            ddsd.u1.lPitch * j);
                    for (i = 0; i < (int)dwWidth; i++) {
                        int r, g, b;
                        /*
                         * Read each value, scale it and shift it into position
                         */
                        r = getc(fp) / red_scale;
                        g = getc(fp) / green_scale;
                        b = getc(fp) / blue_scale;
                        *lpLP = (r << red_shift) | (g << green_shift) |
                                (b << blue_shift);
                        lpLP++;
                    }
                }
                break;
            case 16 :
                for (j = 0; j < (int)dwHeight; j++) {
                    lpSP = (unsigned short*)(((char*)ddsd.lpSurface) +
                                                            ddsd.u1.lPitch * j);
                    for (i = 0; i < (int)dwWidth; i++) {
                        int r, g, b;
                        r = getc(fp) / red_scale;
                        g = getc(fp) / green_scale;
                        b = getc(fp) / blue_scale;
                        *lpSP = (r << red_shift) | (g << green_shift) |
                                (b << blue_shift);
                        lpSP++;
                    }
                }
                break;
            case 8:
                for (j = 0; j < (int)dwHeight; j++) {
                    lpCP = (unsigned char*)(((char*)ddsd.lpSurface) +
                                                            ddsd.u1.lPitch * j);
                    for (i = 0; i < (int)dwWidth; i++) {
                        int r, g, b;
                        r = getc(fp) / red_scale;
                        g = getc(fp) / green_scale;
                        b = getc(fp) / blue_scale;
                        *lpCP = (r << red_shift) | (g << green_shift) |
                                (b << blue_shift);
                        lpCP++;
                    }
                }
                break;
            default:
                /*
                 * This wasn't a format I recognize
                 */
                lpDDS->lpVtbl->Unlock(lpDDS, NULL);
                fclose(fp);
                lpDDS->lpVtbl->Release(lpDDS);
                D3DAppISetErrorString("Unknown pixel format (loadtex).");
                return NULL;
        }
        /*
         * Unlock the texture and return the surface pointer
         */
        lpDDS->lpVtbl->Unlock(lpDDS, NULL);
        fclose(fp);
        return (lpDDS);
    }

    /*
     * We assume the 8-bit palettized case
     */
    color_count = 0;    /* number of colors in the texture */
    for (j = 0; j < (int)dwHeight; j++) {
        /*
         * Point to next row in surface
         */
        lpC = ((char*)ddsd.lpSurface) + ddsd.u1.lPitch * j;
        for (i = 0; i < (int)dwWidth; i++) {
            int r, g, b, k;
            /*
             * Get the next red, green and blue values and turn them into a
             * D3DCOLOR
             */
            r = getc(fp);
            g = getc(fp);
            b = getc(fp);
            c = RGB_MAKE(r, g, b);
            /*
             * Search for this color in a table of colors in this texture
             */
            for (k = 0; k < color_count; k++)
                if (c == colors[k]) break;
            if (k == color_count) {
                /*
                 * This is a new color, so add it to the list
                 */
                color_count++;
                /*
                 * More than 256 and we fail (8-bit)
                 */
                if (color_count > psize) {
                    color_count--;
                    k = color_count - 1;
                    //goto burst_colors;
                }
                colors[k] = c;
            }
            /*
             * Set the "pixel" value on the surface to be the index into the
             * color table
             */
            if (psize == 16) {
                if ((i & 1) == 0)
                    *lpC = k & 0xf;
                else {
                    *lpC |= (k & 0xf) << 4;
                    lpC++;
                }
            } else {
                *lpC = (char)k;
                lpC++;
            }
        }
    }
    /*
     * Close the file and unlock the surface
     */
    fclose(fp);
    lpDDS->lpVtbl->Unlock(lpDDS, NULL);

//burst_colors:
    if (color_count > psize) {
        /*
         * If there are more than 256 colors, we overran our palette
         */
        lpDDS->lpVtbl->Unlock(lpDDS, NULL);
        lpDDS->lpVtbl->Release(lpDDS);
        D3DAppISetErrorString("Palette burst. (loadtex).\n");
        return (NULL);
    }

    /*
     * Create a palette with the colors in our color table
     */
    memset(ppe, 0, sizeof(PALETTEENTRY) * 256);
    for (i = 0; i < color_count; i++) {
        ppe[i].peRed = (unsigned char)RGB_GETRED(colors[i]);
        ppe[i].peGreen = (unsigned char)RGB_GETGREEN(colors[i]);
        ppe[i].peBlue = (unsigned char)RGB_GETBLUE(colors[i]);
    }
    /*
     * Set all remaining entry flags to D3DPAL_RESERVED, which are ignored by
     * the renderer.
     */
    for (; i < 256; i++)
        ppe[i].peFlags = D3DPAL_RESERVED;
    /*
     * Create the palette with the DDPCAPS_ALLOW256 flag because we want to
     * have access to all entries.
     */
    ddrval = lpDD->lpVtbl->CreatePalette(lpDD,
                                         DDPCAPS_INITIALIZE | pcaps,
                                         ppe, &lpDDPal, NULL);
    if (ddrval != DD_OK) {
        lpDDS->lpVtbl->Release(lpDDS);
        D3DAppISetErrorString("Create palette failed while loading surface (loadtex).\n%s",
                              D3DAppErrorToString(ddrval));
        return (NULL);
    }
    /*
     * Finally, bind the palette to the surface
     */
    ddrval = lpDDS->lpVtbl->SetPalette(lpDDS, lpDDPal);
    if (ddrval != DD_OK) {
        lpDDS->lpVtbl->Release(lpDDS);
        lpDDPal->lpVtbl->Release(lpDDPal);
        D3DAppISetErrorString("SetPalette failed while loading surface (loadtex).\n%s",
                              D3DAppErrorToString(ddrval));
        return (NULL);
    }

    lpDDPal->lpVtbl->Release(lpDDPal);

    return lpDDS;
}

static BOOL
ppm_getbyte(FILE *fp, char *newByte)
{
    char cchar;
    int cc;

    /* Get a byte, and dump comments */
    cchar = cc = getc(fp);
    if (cc == EOF) {
      return FALSE;
    }

    if (cchar == '#') {
        /* Read until next end of line */
        do {
            cchar = cc = getc(fp);
            if (cc == EOF)
                return FALSE;
        } while (cchar != '\n' && cchar != '\r');
    }

  *newByte = cchar;

  return TRUE;
}

static BOOL
ppm_getint(FILE *fp, DWORD *newInt)
{
  DWORD cint;
  char cchar;

  do {
    if (!ppm_getbyte(fp, &cchar)) return FALSE;
  } while (isspace(cchar));

  if (!isdigit(cchar)) {
    return FALSE;
  }

  cint = 0;

  do {
    cint = (cint * 10) + (cchar - '0');
    if (!ppm_getbyte(fp, &cchar)) return FALSE;
  } while(isdigit(cchar));

  *newInt = cint;

  return TRUE;
}

static BOOL
loadPPMHeader(FILE *fp, DWORD *width, DWORD *height, DWORD *maxgrey)
{
    char magic[MAGICBYTES], cchar;

    /* Slurp up ppm header until we get width, height and maxgrey values */

    /* Read and check the magic bytes */
    if (fread(magic, MAGICBYTES, 1, fp) != 1)
        return FALSE;
    if (magic[0] != 'P' || magic[1] != '6')
        return FALSE;

    /* Now we can actually read some numbers */
    if (!ppm_getint(fp, width))
        return FALSE;
    if (!ppm_getint(fp, height))
        return FALSE;
    if (!ppm_getint(fp, maxgrey))
        return FALSE;

    /* Slurp up rest of white space so we get to actual data */
    do {
        if (!ppm_getbyte(fp, &cchar))
            return FALSE;
    } while (cchar == ' ' || cchar == '\t' || cchar == '\n' || cchar == '\r');

    fseek(fp, -1, SEEK_CUR);

    return TRUE;
}


/***************************************************************************/
/*                         Finding Textures                                */
/***************************************************************************/

#define MAXPATH    256
#define PATHSEP    ';'
#define FILESEP    '\\'
#define MAXCONTENTS 25
#define RESPATH     "Software\\Microsoft\\Direct3D"

static int PathListInitialised = FALSE;

/*
 * PathList structure
 * A list of directories in which to search for the texture.
 */
static struct {
    int count;
    char *contents[MAXCONTENTS];
} PathList;

/*
 * D3DAppIAddPathList
 * Add this string to the search path list
 */
static void
D3DAppIAddPathList(const char *path)
{
    char *p;
    char *elt;
    int len;

    while (path) {
        p = strchr(path, PATHSEP);
        if (p)
            len = p - path;
        else
            len = lstrlen(path);
        elt = (char *) malloc(len + 1);
        if (elt == NULL)
            return;
        lstrcpyn(elt, path, len + 1);
        elt[len] = '\0';
        PathList.contents[PathList.count] = elt;
        PathList.count++;
        if (p)
            path = p + 1;
        else
            path = NULL;
        if (PathList.count == MAXCONTENTS)
            return;
    }
    return;
}

/*
 * D3DAppIInitialisePathList
 * Create a search path with the D3DPATH env. var and D3D Path registry entry
 */
static void
D3DAppIInitialisePathList()
{
    long result;
    HKEY key;
    DWORD type, size;
    static char buf[512];
    char* path;

    if (PathListInitialised)
        return;
    PathListInitialised = TRUE;

    PathList.count = 0;
    path = getenv("D3DPATH");
    D3DAppIAddPathList(".");
    if (path != NULL) {
        D3DAppIAddPathList(path);
        return;
    }
    result = RegOpenKeyEx(HKEY_LOCAL_MACHINE, RESPATH, 0, KEY_READ, &key);
    if (result == ERROR_SUCCESS) {
        size = sizeof(buf);
        result = RegQueryValueEx(key, "D3D Path", NULL, &type, (LPBYTE) buf,
                                 &size);
        RegCloseKey(key);
        if (result == ERROR_SUCCESS && type == REG_SZ)
            D3DAppIAddPathList(buf);
    }
}


/*
 * D3DAppIFindFile
 * Find and open a file using the current search path.
 */
static FILE*
D3DAppIFindFile(const char *name, const char *mode)
{
    FILE *fp;
    char buf[MAXPATH];
    static char filesep[] = {FILESEP, 0};
    int i;

    D3DAppIInitialisePathList();

    fp = fopen(name, mode);
    if (fp != NULL)
        return fp;

    for (i = 0; i < PathList.count; i++) {
        lstrcpy(buf, PathList.contents[i]);
        lstrcat(buf, filesep);
        lstrcat(buf, name);
        fp = fopen(buf, mode);
        if (fp)
            return fp;
    }
    return NULL;
}

/*
 * D3DAppIReleasePathList
 * Release the path list for program termination
 */
void
D3DAppIReleasePathList(void)
{
    int i;
    for (i = 0; i < PathList.count; i++) {
        free(PathList.contents[i]);
        PathList.contents[i] = NULL;
    }
    PathList.count = 0;
    PathListInitialised = FALSE;
}

