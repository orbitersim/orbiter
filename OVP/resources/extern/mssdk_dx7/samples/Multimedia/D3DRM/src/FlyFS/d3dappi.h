/*
 *  Copyright (C) 1998 Microsoft Corporation. All Rights Reserved.
 *
 *  File: d3dappi.h
 *
 *  Internal header.  Part of D3DApp.
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

#ifndef __D3DAPPI_H__
#define __D3DAPPI_H__

/*
 * INCLUDED HEADERS
 */
#include <d3drm.h>
#include <stdio.h>
#include "d3dapp.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * MACROS
 */
#undef ATTEMPT
#define ATTEMPT(x) if (!(x)) goto exit_with_error
#undef RELEASE
#define RELEASE(x) if (x) { x->lpVtbl->Release(x); x = NULL; }
#undef MAX
#define MAX(x, y) ((x) > (y)) ? (x) : (y)
#undef MIN
#define MIN(x, y) ((x) > (y)) ? (y) : (x)
#undef ZEROMEM
#define ZEROMEM(x) memset(&x, 0, sizeof(x))

/*
 * GLOBAL VARIABLES
 *    see d3dapp.c for descriptions
 */
extern D3DAppInfo d3dappi;
extern D3DAppRenderState d3dapprs;
extern BOOL bD3DAppInitialized;
extern HRESULT LastError;
extern LPDIRECTDRAWCLIPPER lpClipper;
extern LPDIRECTDRAWPALETTE lpPalette;
extern BOOL(*D3DDeviceDestroyCallback)(LPVOID);
extern LPVOID D3DDeviceDestroyCallbackContext;
extern BOOL(*D3DDeviceCreateCallback)(int, int, LPDIRECT3DVIEWPORT*, LPVOID);
extern LPVOID D3DDeviceCreateCallbackContext;
extern BOOL bPrimaryPalettized;
extern BOOL bPaletteActivate;
extern BOOL bIgnoreWM_SIZE;
extern PALETTEENTRY ppe[256];
extern PALETTEENTRY Originalppe[256];
extern char LastErrorString[256];
extern SIZE szLastClient;
extern SIZE szBuffers;
extern int NumDirtyClientRects, NumDirtyBackRects, NumDirtyZRects;
extern D3DRECT DirtyClient[D3DAPP_MAXCLEARRECTS];
extern D3DRECT DirtyBack[D3DAPP_MAXCLEARRECTS];
extern D3DRECT DirtyZ[D3DAPP_MAXCLEARRECTS];
extern D3DTEXTUREHANDLE MasterTextureHandle[D3DAPP_MAXTEXTURES];
extern int CallbackRefCount;

#ifdef __cplusplus
};
#endif

/*
 * INTERNAL FUNCTION PROTOTYPES
 */
BOOL D3DAppISetRenderState(void);
BOOL D3DAppIEnumDDDevices(int* NumDriver, D3DAppDDDriver* lpDriver);
BOOL D3DAppIEnumDevices(void);
BOOL D3DAppIPickDriver(int* driver, DWORD depths);
BOOL D3DAppICreateD3D(void);
BOOL D3DAppIEnumTextureFormats(void);
BOOL D3DAppICreateZBuffer(int w, int h, int driver);
BOOL D3DAppICreateDevice(int driver);
BOOL D3DAppILoadTextureSurf(int n, BOOL* bInVideo);
BOOL D3DAppIGetTextureHandle(int n);
BOOL D3DAppILoadAllTextures(void);
void D3DAppIReleaseTexture(int n);
void D3DAppIReleaseAllTextures(void);
BOOL D3DAppIReloadTextureSurf(int n);
BOOL D3DAppISetCoopLevel(HWND hwnd, BOOL bFullscreen);
BOOL D3DAppISetDisplayMode(int w, int h, int bpp);
BOOL D3DAppICheckForPalettized(void);
BOOL D3DAppIRestoreDispMode(void);
BOOL D3DAppIVerifyDriverAndMode(int* lpdriver, int* lpmode);
BOOL D3DAppIFilterDrivers(int mode);
DWORD D3DAppTotalVideoMemory(void);
BOOL D3DAppIEnumDisplayModes(void);
BOOL D3DAppIPickDisplayMode(int* mode, DWORD depths);
BOOL D3DAppISetDispMode(int w, int h, int bpp);
BOOL D3DAppICreateDD(DWORD flags, LPGUID lpGuid);
BOOL D3DAppIFilterDisplayModes(int driver);
HRESULT D3DAppICreateSurface(LPDDSURFACEDESC lpDDSurfDesc,
                LPDIRECTDRAWSURFACE FAR *lpDDSurface);
HRESULT D3DAppIGetSurfDesc(LPDDSURFACEDESC lpDDSurfDesc,
                           LPDIRECTDRAWSURFACE lpDDSurf);
BOOL D3DAppICreateBuffers(HWND hwnd, int w, int h, int bpp, BOOL bFullscreen, BOOL bIsHardware);
BOOL D3DAppIRememberWindowsMode(void);
BOOL D3DAppIClearBuffers(void);
DWORD D3DAppIBPPToDDBD(int bpp);
void D3DAppIReleasePathList(void);
LPDIRECTDRAWSURFACE D3DAppILoadSurface(LPDIRECTDRAW lpDD, LPCSTR lpName,
                                       LPDDSURFACEDESC lpFormat,
                                       DWORD memoryflag);
void D3DAppISetClientSize(HWND hwnd, int w,int h,BOOL bReturnFromFullscreen);
void D3DAppIGetClientWin(HWND hwnd);
void D3DAppISetDefaults(void);
BOOL D3DAppICallDeviceDestroyCallback(void);
BOOL D3DAppICallDeviceCreateCallback(int w, int h);
void D3DAppIMergeRectLists(int* dstnum, LPD3DRECT dst, int src1num,
                           LPD3DRECT src1, int src2num, LPD3DRECT src2);
void D3DAppICopyRectList(int* dstnum, LPD3DRECT dst, int srcnum,
                         LPD3DRECT src);
void D3DAppIValidateDirtyRects(void);
BOOL D3DAppIHandleWM_SIZE(LRESULT* lresult, HWND hwnd, UINT message,
                          WPARAM wParam, LPARAM lParam);
void D3DAppISetErrorString( LPSTR fmt, ... );

#endif // __D3DAPPI_H__
