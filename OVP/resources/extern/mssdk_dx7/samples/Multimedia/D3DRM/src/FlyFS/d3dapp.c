/*
 *  Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File: d3dapp.c
 *
 *  Top level D3DApp functions and internal global variables.  See
 *  d3dapp.h for more information.
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

#include "d3dappi.h"

/***************************************************************************/
/*                           GLOBAL VARIABLES                              */
/***************************************************************************/
/*
 * All DD and D3D objects which are also available to the application
 * See d3dapp.h for typedef
 */
D3DAppInfo d3dappi;
/*
 * Internal record of the render state.  See d3dapp.h for typedef
 */
D3DAppRenderState d3dapprs;
/*
 * Callback functions for D3D device creation and destruction
 */
BOOL(*D3DDeviceDestroyCallback)(LPVOID);
LPVOID D3DDeviceDestroyCallbackContext;
BOOL(*D3DDeviceCreateCallback)(int, int, LPDIRECT3DVIEWPORT*, LPVOID);
LPVOID D3DDeviceCreateCallbackContext;
/*
 * The last error code and string
 */
HRESULT LastError;
char LastErrorString[256];
/*
 * List of dirty rectangles on back buffer and client area
 */
int NumDirtyClientRects, NumDirtyBackRects, NumDirtyZRects;
D3DRECT DirtyClient[D3DAPP_MAXCLEARRECTS];
D3DRECT DirtyBack[D3DAPP_MAXCLEARRECTS];
D3DRECT DirtyZ[D3DAPP_MAXCLEARRECTS];
/*
 * List of texture handles which is copied to D3DAppInfo structure when
 * necessary
 */
D3DTEXTUREHANDLE MasterTextureHandle[D3DAPP_MAXTEXTURES];

LPDIRECTDRAWCLIPPER lpClipper; /* Clipper in windowed case */
LPDIRECTDRAWPALETTE lpPalette; /* Front buffer's palette */
PALETTEENTRY ppe[256];         /* Current palette entries */
PALETTEENTRY Originalppe[256]; /* Windows palette entries at startup */
BOOL bD3DAppInitialized;       /* Is D3DApp initialized? */
BOOL bPrimaryPalettized;       /* Is the front buffer palettized? */
BOOL bPaletteActivate;         /* Is the front buffer's palette valid? */
BOOL bIgnoreWM_SIZE;           /* Ignore this WM_SIZE messages */
SIZE szLastClient;             /* Dimensions of the last window */
SIZE szBuffers;                /* Current buffer dimensions, not necessarily
                                  the same as the client window */
int CallbackRefCount;          /* How many times DeviceCreateCallback has
                                  been called in a row */

/***************************************************************************/
/*                               FUNCTIONS                                 */
/***************************************************************************/

/*
 * D3DAppEnumerateDDDevices
 */
BOOL D3DAppEnumerateDDDevices(int* NumDevices, D3DAppDDDriver* lpDriver)
{
    if (!NumDevices || !lpDriver) {
        D3DAppISetErrorString("Invalid parameters passe to D3DAppEnumerateDDDevices.\n");
        return FALSE;
    }
    return D3DAppIEnumDDDevices(NumDevices, lpDriver);
}

/*
 * D3DAppCreateFromHWND
 */
BOOL D3DAppCreateFromHWND(DWORD flags, HWND hwnd, LPGUID lpDDGuid,
                          BOOL(*DeviceCreateCallback)(int, int,
                                                      LPDIRECT3DVIEWPORT*,
                                                      LPVOID),
                          LPVOID lpCreateContext,
                          BOOL(*DeviceDestroyCallback)(LPVOID),
                          LPVOID lpDestroyContext,
                          D3DAppInfo** D3DApp)
{
    int driver, mode, w, h;
    /*
     * Clean the global varaibles and check the flags
     */
    D3DAppISetDefaults();
    if (flags & D3DAPP_ONLYSYSTEMMEMORY) {
        d3dappi.bOnlySystemMemory = TRUE;
        d3dappi.bOnlyEmulation = TRUE;
    }
    if (flags & D3DAPP_ONLYD3DEMULATION)
        d3dappi.bOnlyEmulation = TRUE;
    /*
     * Create DirectDraw, remember the Windows display mode and enumerate the
     * display modes
     */
    ATTEMPT(D3DAppICreateDD(d3dappi.bOnlyEmulation ?
                            D3DAPP_ONLYDDEMULATION : 0L, lpDDGuid));
    ATTEMPT(D3DAppIRememberWindowsMode());
    ATTEMPT(D3DAppIEnumDisplayModes());
    /*
     * Create Direct3D and enumerate the D3D drivers
     */
    ATTEMPT(D3DAppICreateD3D());
    ATTEMPT(D3DAppIEnumDevices());

    /*
     * Set the device creation and destroy callback functions
     */
    D3DDeviceDestroyCallback = DeviceDestroyCallback;
    D3DDeviceDestroyCallbackContext = lpDestroyContext;
    D3DDeviceCreateCallback = DeviceCreateCallback;
    D3DDeviceCreateCallbackContext = lpCreateContext;
    *D3DApp = &d3dappi;
    d3dappi.hwnd = hwnd;
    /*
     * Choose a driver and display mode.  Using the current window is
     * prefered, but a fullscreen mode may be selected.  Set the cooperative
     * level and create the front and back buffers for this mode.
     */
    driver = D3DAPP_YOUDECIDE;
    mode = D3DAPP_YOUDECIDE;
    ATTEMPT(D3DAppIVerifyDriverAndMode(&driver, &mode));
    D3DAppIGetClientWin(hwnd);
    if (mode == D3DAPP_USEWINDOW) {
        w = d3dappi.szClient.cx;
        h = d3dappi.szClient.cy;
        ATTEMPT(D3DAppISetCoopLevel(hwnd, FALSE));
        ATTEMPT(D3DAppICreateBuffers(hwnd, w, h, D3DAPP_BOGUS, FALSE,
                                     d3dappi.Driver[driver].bIsHardware));
        /*
         * Change the currently selected mode if it's not compatible with
         * this driver.  Just to make sure that CurrMode is always a mode the
         * current driver can do.
         */
        if (!(d3dappi.Driver[driver].Desc.dwDeviceRenderBitDepth &
              D3DAppIBPPToDDBD(d3dappi.Mode[d3dappi.CurrMode].bpp))){
            ATTEMPT(D3DAppIPickDisplayMode(&d3dappi.CurrMode,
                        d3dappi.Driver[driver].Desc.dwDeviceRenderBitDepth));
        }
    } else {
        szLastClient = d3dappi.szClient;
        w = d3dappi.Mode[mode].w;
        h = d3dappi.Mode[mode].h;
        d3dappi.szClient.cx = w; d3dappi.szClient.cy = h;
        ATTEMPT(D3DAppISetCoopLevel(hwnd, TRUE));
        ATTEMPT(D3DAppISetDisplayMode(w, h, d3dappi.Mode[mode].bpp));
        d3dappi.CurrMode = mode;
        ATTEMPT(D3DAppICreateBuffers(hwnd, w, h, d3dappi.Mode[mode].bpp, TRUE,
                                     d3dappi.Driver[driver].bIsHardware));
    }
    /*
     * If the front buffer is palettized, initialize its palette
     */
    ATTEMPT(D3DAppICheckForPalettized());
    /*
     * Create the Z-buffer
     */
    ATTEMPT(D3DAppICreateZBuffer(w, h, driver));
    /*
     * Create the D3D device, load the textures, call the device create
     * callback and set a default render state
     */
    ATTEMPT(D3DAppICreateDevice(driver));
    ATTEMPT(D3DAppILoadAllTextures());
    ATTEMPT(D3DAppIFilterDisplayModes(driver));  /* bThisDriverCanDo flags */
    ATTEMPT(D3DAppICallDeviceCreateCallback(w, h));
    ATTEMPT(D3DAppISetRenderState());
    /*
     * Initialize dirty rectangle information
     */
    D3DAppIValidateDirtyRects();
    /*
     * Ready to render
     */
    bD3DAppInitialized = TRUE;
    d3dappi.bRenderingIsOK = TRUE;
    return TRUE;

exit_with_error:
    D3DAppICallDeviceDestroyCallback();
    RELEASE(d3dappi.lpD3DDevice);
    RELEASE(d3dappi.lpZBuffer);
    RELEASE(lpPalette);
    RELEASE(lpClipper);
    RELEASE(d3dappi.lpBackBuffer);
    RELEASE(d3dappi.lpFrontBuffer);
    if (d3dappi.bFullscreen) {
        D3DAppIRestoreDispMode();
        D3DAppISetCoopLevel(hwnd, FALSE);
    }
    RELEASE(d3dappi.lpD3D);
    RELEASE(d3dappi.lpDD);
    return FALSE;
}

/*
 * D3DAppFullscreen
 */
BOOL D3DAppFullscreen(int mode)
{
    int w, h, bpp;
    BOOL b; /* was already in a fullscreen mode? */

    d3dappi.bRenderingIsOK = FALSE;
    /*
     * Make sure this is a valid request, otherwise doctor mode so it will
     * work with this driver.
     */
    ATTEMPT(D3DAppIVerifyDriverAndMode(&d3dappi.CurrDriver, &mode));
    /*
     * Release everything
     */
    ATTEMPT(D3DAppICallDeviceDestroyCallback());
    if (d3dappi.bFullscreen) {
        ATTEMPT(D3DAppIClearBuffers());
    }
    D3DAppIReleaseAllTextures();
    RELEASE(d3dappi.lpD3DDevice);
    RELEASE(d3dappi.lpZBuffer);
    RELEASE(lpPalette);
    RELEASE(lpClipper);
    RELEASE(d3dappi.lpBackBuffer);
    RELEASE(d3dappi.lpFrontBuffer);
    /*
     * Record information about the current status
     */
    b = d3dappi.bFullscreen;
    w = d3dappi.Mode[mode].w;
    h = d3dappi.Mode[mode].h;
    bpp = d3dappi.Mode[mode].bpp;
    if (!b) {
        /*
         * If this is not a fullscreen mode, we'll need to record the window
         * size for when we return to it.
         */
        szLastClient = d3dappi.szClient;
    }
    /*
     * Set the cooperative level and create front and back buffers
     */
    d3dappi.szClient.cx = w; d3dappi.szClient.cy = h;
    ATTEMPT(D3DAppISetCoopLevel(d3dappi.hwnd, TRUE));
    ATTEMPT(D3DAppISetDisplayMode(w, h, bpp));
    d3dappi.CurrMode = mode;
    ATTEMPT(D3DAppICreateBuffers(d3dappi.hwnd, w, h, bpp, TRUE,
                                 d3dappi.Driver[d3dappi.CurrDriver].bIsHardware));
    /*
     * If the front buffer is palettized, initialize its palette
     */
    ATTEMPT(D3DAppICheckForPalettized());
    /*
     * Create the Z-buffer
     */
    ATTEMPT(D3DAppICreateZBuffer(w, h, d3dappi.CurrDriver));
    /*
     * Create the D3D device, load the textures, call the device create
     * callback and set a default render state
     */
    ATTEMPT(D3DAppICreateDevice(d3dappi.CurrDriver));
    ATTEMPT(D3DAppILoadAllTextures());
    ATTEMPT(D3DAppICallDeviceCreateCallback(w, h));
    ATTEMPT(D3DAppISetRenderState());
    /*
     * Set current mode and clear dirty rectangle information
     */
    d3dappi.CurrMode = mode;
    D3DAppIValidateDirtyRects();
    d3dappi.bRenderingIsOK = TRUE;
    return TRUE;

exit_with_error:
    D3DAppICallDeviceDestroyCallback();
    RELEASE(d3dappi.lpD3DDevice);
    RELEASE(d3dappi.lpZBuffer);
    RELEASE(lpPalette);
    RELEASE(lpClipper);
    RELEASE(d3dappi.lpBackBuffer);
    RELEASE(d3dappi.lpFrontBuffer);
    if (!b) {
        D3DAppIRestoreDispMode();
        D3DAppISetCoopLevel(d3dappi.hwnd, FALSE);
    }
    return FALSE;
}

/*
 * D3DAppWindow
 */
BOOL
D3DAppWindow(int w, int h)
{
    BOOL b; /* changing from a fullscreen mode? */

    if (!d3dappi.bIsPrimary) {
        D3DAppISetErrorString("It is not possible to create a D3D window with a hardware DirectDraw device.  Check the bIsPrimary flag before calling D3DAppWindow.");
        return FALSE;
    }
    b = d3dappi.bFullscreen;
    /*
     * If asked to set the window size, return it to the last value or use
     * a default value.
     */
    if (w == D3DAPP_YOUDECIDE) {
        w = b ? szLastClient.cx : D3DAPP_DEFAULTWINDOWDIM;
    }
    if (h == D3DAPP_YOUDECIDE) {
        h = b ? szLastClient.cy : D3DAPP_DEFAULTWINDOWDIM;
    }
    /*
     * Release everything
     */
    d3dappi.bRenderingIsOK = FALSE;
    ATTEMPT(D3DAppICallDeviceDestroyCallback());
    if (b) {
        ATTEMPT(D3DAppIClearBuffers());
    }
    D3DAppIReleaseAllTextures();
    RELEASE(d3dappi.lpD3DDevice);
    RELEASE(d3dappi.lpZBuffer);
    RELEASE(lpPalette);
    RELEASE(lpClipper);
    RELEASE(d3dappi.lpBackBuffer);
    RELEASE(d3dappi.lpFrontBuffer);
    /*
     * Restore the display mode if we were in a fullscreen mode
     */
    if (b) {
        D3DAppIRestoreDispMode();
    }
    /*
     * Set the cooperative level and create front and back buffers
     */
    D3DAppISetCoopLevel(d3dappi.hwnd, FALSE);
    D3DAppISetClientSize(d3dappi.hwnd, w, h, b);
    ATTEMPT(D3DAppICreateBuffers(d3dappi.hwnd, w, h, D3DAPP_BOGUS, FALSE,
                                 d3dappi.Driver[d3dappi.CurrDriver].bIsHardware));
    /*
     * If the front buffer is palettized, initialize its palette
     */
    ATTEMPT(D3DAppICheckForPalettized());
    /*
     * Create the Z-buffer
     */
    ATTEMPT(D3DAppICreateZBuffer(szBuffers.cx, szBuffers.cy,
                                 d3dappi.CurrDriver));
    /*
     * Create the D3D device, load the textures, call the device create
     * callback and set a default render state
     */
    ATTEMPT(D3DAppICreateDevice(d3dappi.CurrDriver));
    ATTEMPT(D3DAppILoadAllTextures());
    ATTEMPT(D3DAppICallDeviceCreateCallback(szBuffers.cx, szBuffers.cy));
    ATTEMPT(D3DAppISetRenderState());
    /*
     * Clear dirty rectangle information
     */
    D3DAppIValidateDirtyRects();
    d3dappi.bRenderingIsOK = TRUE;
    return TRUE;

exit_with_error:
    D3DAppICallDeviceDestroyCallback();
    RELEASE(d3dappi.lpD3DDevice);
    RELEASE(d3dappi.lpZBuffer);
    RELEASE(lpPalette);
    RELEASE(lpClipper);
    RELEASE(d3dappi.lpBackBuffer);
    RELEASE(d3dappi.lpFrontBuffer);
    return FALSE;
}


/*
 * D3DAppChangeDriver
 */
BOOL
D3DAppChangeDriver(int driver, DWORD flags)
{
    int mode;

    d3dappi.bRenderingIsOK = FALSE;
    /*
     * Verify the compatibility of this mode with the specified driver.
     * The mode may change.
     */
    if (d3dappi.bFullscreen)
        mode = d3dappi.CurrMode;
    else
        mode = D3DAPP_USEWINDOW;
    ATTEMPT(D3DAppIVerifyDriverAndMode(&driver, &mode));
    if (driver == D3DAPP_BOGUS || mode == D3DAPP_BOGUS)
        goto exit_with_error;
    /*
     * Update the current driver and set bThisDriverCanDo flags
     */
    d3dappi.CurrDriver = driver;
    ATTEMPT(D3DAppIFilterDisplayModes(driver));
    /*
     * Either call D3DAppWindow or D3DAppFullscreen depending on mode
     */
    if (mode == D3DAPP_USEWINDOW) {
        if (d3dappi.bFullscreen) {
            /*
             * We need to switch to a window.  D3DApp will either use the
             * size of the last window it saw or use a default size.
             */
            ATTEMPT(D3DAppWindow(D3DAPP_YOUDECIDE, D3DAPP_YOUDECIDE));
        } else {
            /*
             * We need to recreate the current window.  Don't let D3DApp
             * decide on the size.
             */
            ATTEMPT(D3DAppWindow(d3dappi.szClient.cx, d3dappi.szClient.cy));
        }
        /*
         * Change the currently selected mode if it's not compatible with
         * this driver.  Just to make sure that CurrMode is always a mode the
         * current driver can do.
         */
        if (!(d3dappi.Driver[driver].Desc.dwDeviceRenderBitDepth &
              D3DAppIBPPToDDBD(d3dappi.Mode[d3dappi.CurrMode].bpp))){
            ATTEMPT(D3DAppIPickDisplayMode(&d3dappi.CurrMode,
                        d3dappi.Driver[driver].Desc.dwDeviceRenderBitDepth));
        }
        d3dappi.bRenderingIsOK = TRUE;
        return TRUE;
    } else {
        /*
         * We need to switch to fullscreen or switch fullscreen modes or stay
         * in the same fullscreen mode.  In any of these cases, we call the
         * same function.
         */
        ATTEMPT(D3DAppFullscreen(mode));
        d3dappi.bRenderingIsOK = TRUE;
        return TRUE;
    }

exit_with_error:
    /*
     * The failed mode setting call would have released everything
     */
    return FALSE;
}


/*
 * D3DAppWindowProc
 */
BOOL
D3DAppWindowProc(BOOL* bStopProcessing, LRESULT* lresult, HWND hwnd,
                 UINT message, WPARAM wParam, LPARAM lParam)
{
    PAINTSTRUCT ps;
    *bStopProcessing = FALSE;
    if (!bD3DAppInitialized)
        return TRUE;
    /*
     * Look for messages which effect rendering.  In some cases, we will not
     * want the app to continue processing the message, so set the flag and
     * provide a return value in lresult.
     */
    switch(message) {
        case WM_SIZE:
            if (!bIgnoreWM_SIZE && d3dappi.bRenderingIsOK && d3dappi.lpD3DDevice) {
                d3dappi.bRenderingIsOK = FALSE;
                /*
                 * Too long to fit here, see ddcalls.c. Updates the buffers
                 * and re-creates the device.
                 */
                ATTEMPT(D3DAppIHandleWM_SIZE(lresult, d3dappi.hwnd, message,
                                             wParam, lParam));
                *bStopProcessing = TRUE;
                d3dappi.bRenderingIsOK = TRUE;
            }
            break;
        case WM_MOVE:
            /*
             * Update client window position information
             */
            d3dappi.pClientOnPrimary.x = d3dappi.pClientOnPrimary.y = 0;
            ClientToScreen(hwnd, &d3dappi.pClientOnPrimary);
            break;
        case WM_ACTIVATE:
            /*
             * Set the front buffer's palette
             */
            if (bPaletteActivate && bPrimaryPalettized &&
                d3dappi.lpFrontBuffer) {
                d3dappi.lpFrontBuffer->lpVtbl->SetPalette(d3dappi.lpFrontBuffer,
                                                          lpPalette);
            }
            break;
        case WM_ACTIVATEAPP:
            d3dappi.bAppActive = (BOOL)wParam;
            break;
        case WM_SETCURSOR:
            /*
             * Prevent the cursor from being shown in fullscreen
             */
            if (d3dappi.bFullscreen && !d3dappi.bPaused) {
                SetCursor(NULL);
                *lresult = 1;
                *bStopProcessing = TRUE;
                return TRUE;
            }
            break;
        case WM_MOVING:
            /*
             * Prevent the window from moving in fullscreen
             */
            if (d3dappi.bFullscreen) {
                GetWindowRect(hwnd, (LPRECT)lParam);
                *lresult = 1;
                *bStopProcessing = TRUE;
                return TRUE;
            }
            break;
        case WM_GETMINMAXINFO:
            /*
             * Ensure the window won't resize in fullscreen
             */
            if (d3dappi.bFullscreen) {
                ((LPMINMAXINFO)lParam)->ptMaxTrackSize.x= d3dappi.ThisMode.w;
                ((LPMINMAXINFO)lParam)->ptMaxTrackSize.y= d3dappi.ThisMode.h;
                ((LPMINMAXINFO)lParam)->ptMinTrackSize.x= d3dappi.ThisMode.w;
                ((LPMINMAXINFO)lParam)->ptMinTrackSize.y= d3dappi.ThisMode.h;
                *lresult = 0;
                *bStopProcessing = TRUE;
                return TRUE;
            } else {
                ((LPMINMAXINFO)lParam)->ptMaxTrackSize.x =
                                                    d3dappi.WindowsDisplay.w;
                ((LPMINMAXINFO)lParam)->ptMaxTrackSize.y =
                                                    d3dappi.WindowsDisplay.h;
                *lresult = 0;
                *bStopProcessing = TRUE;
                return TRUE;
            }
            break;
        case WM_PAINT:
            /*
             * Clear the rectangle and blt the backbuffer image
             */
            BeginPaint(hwnd, &ps);
            if (d3dappi.bRenderingIsOK && !d3dappi.bFullscreen) {
                D3DAppShowBackBuffer(D3DAPP_SHOWALL);
            }
            D3DAppIValidateDirtyRects();
            EndPaint(hwnd, &ps);
            *lresult = 1;
            *bStopProcessing = TRUE;
            return TRUE;
        case WM_NCPAINT:
            /*
             * When in fullscreen mode, don't draw the window frame.
             */
            if (d3dappi.bFullscreen && !d3dappi.bPaused) {
                *lresult = 0;
                *bStopProcessing = TRUE;
                return TRUE;
            }
            break;
    }
    return TRUE;

exit_with_error:
    return FALSE;
}

/*
 * D3DAppAddTexture
 */
BOOL
D3DAppAddTexture(const char* imagefile)
{
    if (d3dappi.NumTextures == D3DAPP_MAXTEXTURES - 1) {
        D3DAppISetErrorString("Can only load %i textures.", D3DAPP_MAXTEXTURES);
        return FALSE;
    }
    lstrcpy(d3dappi.ImageFile[d3dappi.NumTextures], imagefile);
    /*
     * If this driver does texture mapping, load the texture.
     * This test also tests that a device has been created.
     */
    if (d3dappi.ThisDriver.bDoesTextures && d3dappi.NumUsableTextures == d3dappi.NumTextures) {
        BOOL bInVideo;
        ATTEMPT(D3DAppILoadTextureSurf(d3dappi.NumTextures, &bInVideo));
        if (!bInVideo && d3dappi.ThisDriver.bIsHardware) {
            D3DAppIReleaseTexture(d3dappi.NumTextures);
        } else {
            ATTEMPT(D3DAppIGetTextureHandle(d3dappi.NumTextures));
            ++d3dappi.NumUsableTextures;
        }
    }
    d3dappi.NumTextures++;
    return TRUE;

exit_with_error:
    d3dappi.ImageFile[d3dappi.NumTextures][0] = 0;
    return FALSE;
}

/*
 * D3DAppChangeTextureFormat
 */
BOOL
D3DAppChangeTextureFormat(int format)
{
    /*
     * Release all the textures, change the format and load them again
     */
    d3dappi.bRenderingIsOK = FALSE;
    D3DAppIReleaseAllTextures();
    d3dappi.CurrTextureFormat = format;
    memcpy(&d3dappi.ThisTextureFormat, &d3dappi.TextureFormat[format],
           sizeof(D3DAppTextureFormat));
    ATTEMPT(D3DAppILoadAllTextures());
    d3dappi.bRenderingIsOK = TRUE;
    return TRUE;

exit_with_error:
    D3DAppIReleaseAllTextures();
    return FALSE;
}

/*
 * D3DAppDisableTextures
 */
BOOL
D3DAppDisableTextures(BOOL flag)
{
    int i;
    if (flag == d3dappi.bTexturesDisabled)
        return TRUE;
    if (flag) {
        /*
         * Set all the texture handles to 0
         */
        d3dappi.bTexturesDisabled = TRUE;
        for (i = 0; i < d3dappi.NumTextures; i++)
            d3dappi.TextureHandle[i] = 0;
    } else {
        /*
         * Restore the texture handles from the master array
         */
        d3dappi.bTexturesDisabled = FALSE;
        memcpy(d3dappi.TextureHandle, MasterTextureHandle,
               sizeof(D3DTEXTUREHANDLE) * D3DAPP_MAXTEXTURES);
    }
    return TRUE;
}

/*
 * D3DAppSwapTextures
 */
BOOL
D3DAppSwapTextures()
{
    int i;
    char tempfile[30];
    LPDIRECT3DTEXTURE2 lptempTexture;
    LPDIRECTDRAWSURFACE lptempSurface;
    if (d3dappi.bTexturesDisabled || d3dappi.NumTextures == 0) {
        D3DAppISetErrorString("Cannot swap textures which are disable or not loaded.\n");
        goto exit_with_error;
    }
    if (!d3dappi.ThisDriver.bDoesTextures)
        return TRUE;
    /*
     * Swap texture 1 with 2, then 2 with 3, then 3 with 4, etc.
     * Don't forget the image file names, texture objects and surfaces
     */
    for (i = 0; i < d3dappi.NumUsableTextures - 1; i++) {
        lstrcpy(tempfile, d3dappi.ImageFile[i]);
        lstrcpy(d3dappi.ImageFile[i], d3dappi.ImageFile[i+1]);
        lstrcpy(d3dappi.ImageFile[i+1], tempfile);
        d3dappi.lpD3DDevice->lpVtbl->SwapTextureHandles(d3dappi.lpD3DDevice,
                                                      d3dappi.lpTexture[i],
                                                      d3dappi.lpTexture[i+1]);
        lptempTexture = d3dappi.lpTexture[i];
        d3dappi.lpTexture[i] = d3dappi.lpTexture[i+1];
        d3dappi.lpTexture[i+1] = lptempTexture;
        lptempSurface = d3dappi.lpTextureSurf[i];
        d3dappi.lpTextureSurf[i] = d3dappi.lpTextureSurf[i+1];
        d3dappi.lpTextureSurf[i+1] = lptempSurface;
    }
    return TRUE;
exit_with_error:
    return FALSE;
}

/*
 * D3DAppSetRenderState
 */
BOOL
D3DAppSetRenderState(D3DAppRenderState* lpState)
{
    /*
     * If none was provided, reset the current render state.
     */
    if (!lpState)
        lpState = &d3dapprs;
    /*
     * Record this render state and set it.
     */
    if (lpState != &d3dapprs)
        memcpy(&d3dapprs, lpState, sizeof(D3DAppRenderState));
    if (d3dappi.bRenderingIsOK) {
        ATTEMPT(D3DAppISetRenderState());
    }
    return TRUE;

exit_with_error:
    return FALSE;
}

/*
 * D3DAppGetRenderState
 */
BOOL
D3DAppGetRenderState(D3DAppRenderState* lpState)
{
    memcpy(lpState, &d3dapprs, sizeof(D3DAppRenderState));
    return TRUE;
}

/*
 * D3DAppShowBackBuffer
 */
BOOL
D3DAppShowBackBuffer(DWORD flags)
{
    if (!d3dappi.bRenderingIsOK) {
        D3DAppISetErrorString("Cannot call D3DAppShowBackBuffer while bRenderingIsOK is FALSE.\n");
        return FALSE;
    }
    if (d3dappi.bPaused)
        return TRUE;
    if (d3dappi.bFullscreen) {
        int numtemp;
        D3DRECT temp[D3DAPP_MAXCLEARRECTS];
        /*
         * Flip the back and front buffers
         */
        LastError = d3dappi.lpFrontBuffer->lpVtbl->Flip(d3dappi.lpFrontBuffer,
                                                        d3dappi.lpBackBuffer,
                                                        1);
        if (LastError == DDERR_SURFACELOST) {
            d3dappi.lpFrontBuffer->lpVtbl->Restore(d3dappi.lpFrontBuffer);
            d3dappi.lpBackBuffer->lpVtbl->Restore(d3dappi.lpBackBuffer);
            D3DAppIClearBuffers();
        } else if (LastError != DD_OK) {
            D3DAppISetErrorString("Flipping complex display surface failed.\n%s", D3DAppErrorToString(LastError));
            return FALSE;
        }
        if (d3dappi.bBackBufferInVideo) {
            /*
             * This is a real flip, so the client and back buffer dirty
             * rectangles also flip
             */
            D3DAppICopyRectList(&numtemp, temp, NumDirtyClientRects,
                                DirtyClient);
            D3DAppICopyRectList(&NumDirtyClientRects, DirtyClient,
                                NumDirtyBackRects, DirtyBack);
            D3DAppICopyRectList(&NumDirtyBackRects, DirtyBack, numtemp, temp);
        } else {
            /*
             * The flip is being emulated as a blt from a system memory back
             * buffer, so the back buffer's dirty rectangles are now also the
             * client's.
             */
            D3DAppICopyRectList(&NumDirtyClientRects, DirtyClient,
                                NumDirtyBackRects, DirtyBack);
        }
    } else {
        int NumFrontRects, NumBufferRects, i;
        RECT front[D3DAPP_MAXCLEARRECTS];
        RECT buffer[D3DAPP_MAXCLEARRECTS];
        /*
         * Set the rectangle to blt from the back to front bufer
         */
        if (flags & D3DAPP_SHOWALL) {
            /*
             * Set to entire client window
             */
            NumBufferRects = 1;
            SetRect(&buffer[0], 0, 0, d3dappi.szClient.cx,
                    d3dappi.szClient.cy);
            SetRect(&front[0],
                    d3dappi.pClientOnPrimary.x, d3dappi.pClientOnPrimary.y,
                    d3dappi.szClient.cx + d3dappi.pClientOnPrimary.x,
                    d3dappi.szClient.cy + d3dappi.pClientOnPrimary.y);
        } else {
            /*
             * Merge the back and front buffer dirty rectangle lists to get
             * a list of rectangles to blt.  This will simultaneously clear
             * the smallest front buffer region while blt'ing all the back
             * buffer which changed.
             */
            D3DAppIMergeRectLists(&NumBufferRects, (LPD3DRECT)buffer,
                                  NumDirtyClientRects, DirtyClient,
                                  NumDirtyBackRects, DirtyBack);
            D3DAppICopyRectList(&NumFrontRects, (LPD3DRECT)front,
                                NumBufferRects, (LPD3DRECT)buffer);
            for (i = 0; i < NumFrontRects; i++) {
                front[i].top += d3dappi.pClientOnPrimary.y;
                front[i].left += d3dappi.pClientOnPrimary.x;
                front[i].bottom += d3dappi.pClientOnPrimary.y;
                front[i].right += d3dappi.pClientOnPrimary.x;
            }
        }
        /*
         * Blt the list of rectangles from the back to front buffer
         */
        for (i = 0; i < NumBufferRects; i++) {
            LastError =
                    d3dappi.lpFrontBuffer->lpVtbl->Blt(d3dappi.lpFrontBuffer,
                                             &front[i], d3dappi.lpBackBuffer,
                                             &buffer[i], DDBLT_WAIT, NULL);
            if (LastError == DDERR_SURFACELOST) {
                d3dappi.lpFrontBuffer->lpVtbl->Restore(d3dappi.lpFrontBuffer);
                d3dappi.lpBackBuffer->lpVtbl->Restore(d3dappi.lpBackBuffer);
                D3DAppIClearBuffers();
            } else if (LastError != DD_OK) {
                D3DAppISetErrorString("Blt of back buffer to front buffer failed.\n%s", D3DAppErrorToString(LastError));
                return FALSE;
            }
        }
        /*
         * The back buffer's dirty rectangles are now also the client's
         */
        D3DAppICopyRectList(&NumDirtyClientRects, DirtyClient,
                            NumDirtyBackRects, DirtyBack);
    }
    return TRUE;
}

/*
 * D3DAppRenderExtents
 */
BOOL
D3DAppRenderExtents(DWORD dwCount, LPD3DRECT extent, DWORD flags)
{
    if (dwCount > D3DAPP_MAXCLEARRECTS) {
        D3DAppISetErrorString("The number of clear rectangles exceeded D3DAPP_MAXCLEARRECTS.");
        return FALSE;
    }
    if (flags & D3DAPP_CLEARALL) {
        /*
         * Set the back buffer dirty rectangle to the entire client area
         */
        D3DRECT dummy;
        dummy.x1 = dummy.y1 = 0;
        dummy.x2 = d3dappi.szClient.cx;
        dummy.y2 = d3dappi.szClient.cy;
        D3DAppICopyRectList(&NumDirtyBackRects, DirtyBack, 1, &dummy);
        D3DAppICopyRectList(&NumDirtyClientRects, DirtyClient, 1, &dummy);
        D3DAppICopyRectList(&NumDirtyZRects, DirtyZ, 1, &dummy);
    } else {
        /*
         * Set the back and Z buffer dirty rectangle list as specified
         */
        D3DAppICopyRectList(&NumDirtyBackRects, DirtyBack, dwCount, extent);
        D3DAppICopyRectList(&NumDirtyZRects, DirtyZ, dwCount, extent);
    }
    return TRUE;
}

/*
 * D3DAppClearBackBuffer
 */
BOOL
D3DAppClearBackBuffer(DWORD flags)
{
    if (!d3dappi.bRenderingIsOK) {
        D3DAppISetErrorString("Cannot call D3DAppClearBackBuffer while bRenderingIsOK is FALSE.\n");
        return FALSE;
    }
    if (flags & D3DAPP_CLEARALL) {
        /*
         * Clear the entire back buffer
         */
        int clearflags;
        D3DRECT dummy;
        /*
         * Decided wether to clear just back buffer or also z-buffer
         */
        clearflags = D3DCLEAR_TARGET;
        if (d3dapprs.bZBufferOn)
            clearflags |= D3DCLEAR_ZBUFFER;
        dummy.x1 = dummy.y1 = 0;
        dummy.x2 = d3dappi.szClient.cx;
        dummy.y2 = d3dappi.szClient.cy;
        LastError =
                  d3dappi.lpD3DViewport->lpVtbl->Clear(d3dappi.lpD3DViewport,
                                                       1, &dummy,
                                                       clearflags);
        if (LastError != D3D_OK) {
            D3DAppISetErrorString("Viewport clear failed.\n%s",
                                  D3DAppErrorToString(LastError));
            return FALSE;
        }
    } else {
        /*
         * Clear the dirty rectangles on the back buffer
         */
        LastError =
                  d3dappi.lpD3DViewport->lpVtbl->Clear(d3dappi.lpD3DViewport,
                                                       NumDirtyBackRects,
                                                      DirtyBack, D3DCLEAR_TARGET);
        if (LastError != D3D_OK) {
            D3DAppISetErrorString("Viewport clear of back buffer failed.\n%s",
                                  D3DAppErrorToString(LastError));
            return FALSE;
        }
        /*
         * Clear the dirty rectangles on the Z buffer
         */
        LastError =
                  d3dappi.lpD3DViewport->lpVtbl->Clear(d3dappi.lpD3DViewport,
                                                       NumDirtyZRects,
                                                      DirtyZ, D3DCLEAR_ZBUFFER);
        if (LastError != D3D_OK) {
            D3DAppISetErrorString("Viewport clear of Z buffer failed.\n%s",
                                  D3DAppErrorToString(LastError));
            return FALSE;
        }

    }
    return TRUE;
}

/*
 * D3DAppCheckForLostSurfaces
 */
#define CHECKSURF(x) if (x) {                                               \
                        if (x->lpVtbl->IsLost(x) == DDERR_SURFACELOST) {    \
                            LastError = x->lpVtbl->Restore(x);              \
                            if (LastError != DD_OK) goto exit_with_error;   \
                            b = TRUE;                                       \
                        }                                                   \
                     }
BOOL
D3DAppCheckForLostSurfaces(void)
{
    int i;
    BOOL b = FALSE;
    /*
     * Check all the surfaces D3DApp owns and restore them if lost.
     */
    CHECKSURF(d3dappi.lpFrontBuffer);
    CHECKSURF(d3dappi.lpBackBuffer);
    CHECKSURF(d3dappi.lpZBuffer);
    if (b) {
        /*
         * If any of the surfaces were lost and restored, clear all the buffers.
         * If this fails, that's fine, just move on.
         */
        D3DAppIClearBuffers();
    }
    for (i = 0; i < d3dappi.NumUsableTextures; i++) {
        b = FALSE;
        CHECKSURF(d3dappi.lpTextureSurf[i]);
        if (b) {
            ATTEMPT(D3DAppIReloadTextureSurf(i));
        }
    }
    return TRUE;

exit_with_error:
    D3DAppISetErrorString("Restoring of a lost surface failed.\n%s",
                          D3DAppErrorToString(LastError));
    return FALSE;
}

/*
 * D3DAppPause
 */
BOOL
D3DAppPause(BOOL flag)
{
    static int pausecount;

    /*
     * Keep a count of the number of times D3DAppPause has been called to
     * prevent pausing more than once in a row.
     */
    if (pausecount != 0) {
        if (flag) {
            ++pausecount;
            return TRUE;
        } else {
            --pausecount;
            if (pausecount != 0)
                return TRUE;
        }
    }

    d3dappi.bPaused = flag;
    if (!flag) {
        /*
         * Returning from a pause
         */
        if (d3dappi.bFullscreen && bPrimaryPalettized && lpPalette) {
            /*
             * Set front buffer's palette back to what it was before pause
             */
            LastError = lpPalette->lpVtbl->SetEntries(lpPalette, 0, 0, 256,
                                                      &ppe[0]);
            if (LastError != DD_OK) {
                D3DAppISetErrorString("Setting palette entries during unpause failed.\n%s", D3DAppErrorToString(LastError));
                goto exit_with_error;
            }
        }
        /*
         * Dirty rectangle info is no longer valid
         */
        D3DAppIValidateDirtyRects();
    }
    if (flag && d3dappi.bFullscreen) {
        /*
         * Pausing in a fullscreen mode
         */
        if (bPrimaryPalettized && lpPalette) {
            /*
             * Save the front buffer's current palette and restore the
             * original Windows palette.
             */
            int i;
            LastError = lpPalette->lpVtbl->GetEntries(lpPalette, 0, 0, 256,
                                                      &ppe[0]);
            if (LastError != DD_OK) {
                D3DAppISetErrorString("Getting palette entries before a pause failed.\n%s", D3DAppErrorToString(LastError));
                goto exit_with_error;
            }
            for (i = 10; i < 246; i++)
                Originalppe[i] = ppe[i];
            LastError = lpPalette->lpVtbl->SetEntries(lpPalette, 0, 0, 256,
                                                      &Originalppe[0]);
            if (LastError != DD_OK) {
                D3DAppISetErrorString("Returning palette entries to defaults failed.\n%s", D3DAppErrorToString(LastError));
                goto exit_with_error;
            }
        }
        /*
         * Flip to GDI surface (either front or back buffer)
         */
        if (d3dappi.lpDD) {
            LastError = d3dappi.lpDD->lpVtbl->FlipToGDISurface(d3dappi.lpDD);
            if (LastError != DD_OK) {
                D3DAppISetErrorString("Flipping to GDI surface failed.\n%s", D3DAppErrorToString(LastError));
                goto exit_with_error;
            }
        }
        /*
         * Draw the menu and frame
         */
        DrawMenuBar(d3dappi.hwnd);
        RedrawWindow(d3dappi.hwnd, NULL, NULL, RDW_FRAME);
    }
    return TRUE;
exit_with_error:
    return FALSE;
}

/*
 * D3DAppCreateSurface
 */
BOOL
D3DAppCreateSurface(DDSURFACEDESC *ddsd, LPDIRECTDRAWSURFACE *lplpSurf)
{
    return D3DAppICreateSurface(ddsd, lplpSurf);
}

/*
 * D3DAppLastError
 */
HRESULT
D3DAppLastError(void)
{
    return LastError;
}

/*
 * D3DAppLastD3DAppISetErrorString
 */
char*
D3DAppLastErrorString(void)
{
    return LastErrorString;
}


/*
 * D3DAppDestroy
 */
BOOL
D3DAppDestroy(void)
{
    /*
     * Destroys all objects including Direct Draw.
     */
    bD3DAppInitialized = FALSE;
    d3dappi.bRenderingIsOK = FALSE;
    d3dappi.hwnd = NULL;
    ATTEMPT(D3DAppICallDeviceDestroyCallback());
    D3DAppIReleaseAllTextures();
    RELEASE(d3dappi.lpD3DDevice);
    RELEASE(d3dappi.lpZBuffer);
    RELEASE(lpPalette);
    RELEASE(lpClipper);
    RELEASE(d3dappi.lpBackBuffer);
    RELEASE(d3dappi.lpFrontBuffer);
    if (d3dappi.bFullscreen) {
        D3DAppIRestoreDispMode();
        D3DAppISetCoopLevel(d3dappi.hwnd, FALSE);
    }
    D3DAppIReleasePathList();
    RELEASE(d3dappi.lpD3D);
    RELEASE(d3dappi.lpDD);
    memset(&d3dappi, 0, sizeof(d3dappi));
    return TRUE;
exit_with_error:
    return FALSE;
}


/*
 * D3DAppErrorToString
 */
char*
D3DAppErrorToString(HRESULT error)
{
    switch(error) {
        case DD_OK:
            return "No error.\0";
        case DDERR_ALREADYINITIALIZED:
            return "This object is already initialized.\0";
        case DDERR_BLTFASTCANTCLIP:
            return "Return if a clipper object is attached to the source surface passed into a BltFast call.\0";
        case DDERR_CANNOTATTACHSURFACE:
            return "This surface can not be attached to the requested surface.\0";
        case DDERR_CANNOTDETACHSURFACE:
            return "This surface can not be detached from the requested surface.\0";
        case DDERR_CANTCREATEDC:
            return "Windows can not create any more DCs.\0";
        case DDERR_CANTDUPLICATE:
            return "Can't duplicate primary & 3D surfaces, or surfaces that are implicitly created.\0";
        case DDERR_CLIPPERISUSINGHWND:
            return "An attempt was made to set a cliplist for a clipper object that is already monitoring an hwnd.\0";
        case DDERR_COLORKEYNOTSET:
            return "No src color key specified for this operation.\0";
        case DDERR_CURRENTLYNOTAVAIL:
            return "Support is currently not available.\0";
        case DDERR_DIRECTDRAWALREADYCREATED:
            return "A DirectDraw object representing this driver has already been created for this process.\0";
        case DDERR_EXCEPTION:
            return "An exception was encountered while performing the requested operation.\0";
        case DDERR_EXCLUSIVEMODEALREADYSET:
            return "An attempt was made to set the cooperative level when it was already set to exclusive.\0";
        case DDERR_GENERIC:
            return "Generic failure.\0";
        case DDERR_HEIGHTALIGN:
            return "Height of rectangle provided is not a multiple of reqd alignment.\0";
        case DDERR_HWNDALREADYSET:
            return "The CooperativeLevel HWND has already been set. It can not be reset while the process has surfaces or palettes created.\0";
        case DDERR_HWNDSUBCLASSED:
            return "HWND used by DirectDraw CooperativeLevel has been subclassed, this prevents DirectDraw from restoring state.\0";
        case DDERR_IMPLICITLYCREATED:
            return "This surface can not be restored because it is an implicitly created surface.\0";
        case DDERR_INCOMPATIBLEPRIMARY:
            return "Unable to match primary surface creation request with existing primary surface.\0";
        case DDERR_INVALIDCAPS:
            return "One or more of the caps bits passed to the callback are incorrect.\0";
        case DDERR_INVALIDCLIPLIST:
            return "DirectDraw does not support the provided cliplist.\0";
        case DDERR_INVALIDDIRECTDRAWGUID:
            return "The GUID passed to DirectDrawCreate is not a valid DirectDraw driver identifier.\0";
        case DDERR_INVALIDMODE:
            return "DirectDraw does not support the requested mode.\0";
        case DDERR_INVALIDOBJECT:
            return "DirectDraw received a pointer that was an invalid DIRECTDRAW object.\0";
        case DDERR_INVALIDPARAMS:
            return "One or more of the parameters passed to the function are incorrect.\0";
        case DDERR_INVALIDPIXELFORMAT:
            return "The pixel format was invalid as specified.\0";
        case DDERR_INVALIDPOSITION:
            return "Returned when the position of the overlay on the destination is no longer legal for that destination.\0";
        case DDERR_INVALIDRECT:
            return "Rectangle provided was invalid.\0";
        case DDERR_LOCKEDSURFACES:
            return "Operation could not be carried out because one or more surfaces are locked.\0";
        case DDERR_NO3D:
            return "There is no 3D present.\0";
        case DDERR_NOALPHAHW:
            return "Operation could not be carried out because there is no alpha accleration hardware present or available.\0";
        case DDERR_NOBLTHW:
            return "No blitter hardware present.\0";
        case DDERR_NOCLIPLIST:
            return "No cliplist available.\0";
        case DDERR_NOCLIPPERATTACHED:
            return "No clipper object attached to surface object.\0";
        case DDERR_NOCOLORCONVHW:
            return "Operation could not be carried out because there is no color conversion hardware present or available.\0";
        case DDERR_NOCOLORKEY:
            return "Surface doesn't currently have a color key\0";
        case DDERR_NOCOLORKEYHW:
            return "Operation could not be carried out because there is no hardware support of the destination color key.\0";
        case DDERR_NOCOOPERATIVELEVELSET:
            return "Create function called without DirectDraw object method SetCooperativeLevel being called.\0";
        case DDERR_NODC:
            return "No DC was ever created for this surface.\0";
        case DDERR_NODDROPSHW:
            return "No DirectDraw ROP hardware.\0";
        case DDERR_NODIRECTDRAWHW:
            return "A hardware-only DirectDraw object creation was attempted but the driver did not support any hardware.\0";
        case DDERR_NOEMULATION:
            return "Software emulation not available.\0";
        case DDERR_NOEXCLUSIVEMODE:
            return "Operation requires the application to have exclusive mode but the application does not have exclusive mode.\0";
        case DDERR_NOFLIPHW:
            return "Flipping visible surfaces is not supported.\0";
        case DDERR_NOGDI:
            return "There is no GDI present.\0";
        case DDERR_NOHWND:
            return "Clipper notification requires an HWND or no HWND has previously been set as the CooperativeLevel HWND.\0";
        case DDERR_NOMIRRORHW:
            return "Operation could not be carried out because there is no hardware present or available.\0";
        case DDERR_NOOVERLAYDEST:
            return "Returned when GetOverlayPosition is called on an overlay that UpdateOverlay has never been called on to establish a destination.\0";
        case DDERR_NOOVERLAYHW:
            return "Operation could not be carried out because there is no overlay hardware present or available.\0";
        case DDERR_NOPALETTEATTACHED:
            return "No palette object attached to this surface.\0";
        case DDERR_NOPALETTEHW:
            return "No hardware support for 16 or 256 color palettes.\0";
        case DDERR_NORASTEROPHW:
            return "Operation could not be carried out because there is no appropriate raster op hardware present or available.\0";
        case DDERR_NOROTATIONHW:
            return "Operation could not be carried out because there is no rotation hardware present or available.\0";
        case DDERR_NOSTRETCHHW:
            return "Operation could not be carried out because there is no hardware support for stretching.\0";
        case DDERR_NOT4BITCOLOR:
            return "DirectDrawSurface is not in 4 bit color palette and the requested operation requires 4 bit color palette.\0";
        case DDERR_NOT4BITCOLORINDEX:
            return "DirectDrawSurface is not in 4 bit color index palette and the requested operation requires 4 bit color index palette.\0";
        case DDERR_NOT8BITCOLOR:
            return "DirectDrawSurface is not in 8 bit color mode and the requested operation requires 8 bit color.\0";
        case DDERR_NOTAOVERLAYSURFACE:
            return "Returned when an overlay member is called for a non-overlay surface.\0";
        case DDERR_NOTEXTUREHW:
            return "Operation could not be carried out because there is no texture mapping hardware present or available.\0";
        case DDERR_NOTFLIPPABLE:
            return "An attempt has been made to flip a surface that is not flippable.\0";
        case DDERR_NOTFOUND:
            return "Requested item was not found.\0";
        case DDERR_NOTLOCKED:
            return "Surface was not locked.  An attempt to unlock a surface that was not locked at all, or by this process, has been attempted.\0";
        case DDERR_NOTPALETTIZED:
            return "The surface being used is not a palette-based surface.\0";
        case DDERR_NOVSYNCHW:
            return "Operation could not be carried out because there is no hardware support for vertical blank synchronized operations.\0";
        case DDERR_NOZBUFFERHW:
            return "Operation could not be carried out because there is no hardware support for zbuffer blitting.\0";
        case DDERR_NOZOVERLAYHW:
            return "Overlay surfaces could not be z layered based on their BltOrder because the hardware does not support z layering of overlays.\0";
        case DDERR_OUTOFCAPS:
            return "The hardware needed for the requested operation has already been allocated.\0";
        case DDERR_OUTOFMEMORY:
            return "DirectDraw does not have enough memory to perform the operation.\0";
        case DDERR_OUTOFVIDEOMEMORY:
            return "DirectDraw does not have enough memory to perform the operation.\0";
        case DDERR_OVERLAYCANTCLIP:
            return "The hardware does not support clipped overlays.\0";
        case DDERR_OVERLAYCOLORKEYONLYONEACTIVE:
            return "Can only have ony color key active at one time for overlays.\0";
        case DDERR_OVERLAYNOTVISIBLE:
            return "Returned when GetOverlayPosition is called on a hidden overlay.\0";
        case DDERR_PALETTEBUSY:
            return "Access to this palette is being refused because the palette is already locked by another thread.\0";
        case DDERR_PRIMARYSURFACEALREADYEXISTS:
            return "This process already has created a primary surface.\0";
        case DDERR_REGIONTOOSMALL:
            return "Region passed to Clipper::GetClipList is too small.\0";
        case DDERR_SURFACEALREADYATTACHED:
            return "This surface is already attached to the surface it is being attached to.\0";
        case DDERR_SURFACEALREADYDEPENDENT:
            return "This surface is already a dependency of the surface it is being made a dependency of.\0";
        case DDERR_SURFACEBUSY:
            return "Access to this surface is being refused because the surface is already locked by another thread.\0";
        case DDERR_SURFACEISOBSCURED:
            return "Access to surface refused because the surface is obscured.\0";
        case DDERR_SURFACELOST:
            return "Access to this surface is being refused because the surface memory is gone. The DirectDrawSurface object representing this surface should have Restore called on it.\0";
        case DDERR_SURFACENOTATTACHED:
            return "The requested surface is not attached.\0";
        case DDERR_TOOBIGHEIGHT:
            return "Height requested by DirectDraw is too large.\0";
        case DDERR_TOOBIGSIZE:
            return "Size requested by DirectDraw is too large, but the individual height and width are OK.\0";
        case DDERR_TOOBIGWIDTH:
            return "Width requested by DirectDraw is too large.\0";
        case DDERR_UNSUPPORTED:
            return "Action not supported.\0";
        case DDERR_UNSUPPORTEDFORMAT:
            return "FOURCC format requested is unsupported by DirectDraw.\0";
        case DDERR_UNSUPPORTEDMASK:
            return "Bitmask in the pixel format requested is unsupported by DirectDraw.\0";
        case DDERR_VERTICALBLANKINPROGRESS:
            return "Vertical blank is in progress.\0";
        case DDERR_WASSTILLDRAWING:
            return "Informs DirectDraw that the previous Blt which is transfering information to or from this Surface is incomplete.\0";
        case DDERR_WRONGMODE:
            return "This surface can not be restored because it was created in a different mode.\0";
        case DDERR_XALIGN:
            return "Rectangle provided was not horizontally aligned on required boundary.\0";
        case D3DERR_BADMAJORVERSION:
            return "D3DERR_BADMAJORVERSION\0";
        case D3DERR_BADMINORVERSION:
            return "D3DERR_BADMINORVERSION\0";
        case D3DERR_EXECUTE_LOCKED:
            return "D3DERR_EXECUTE_LOCKED\0";
        case D3DERR_EXECUTE_NOT_LOCKED:
            return "D3DERR_EXECUTE_NOT_LOCKED\0";
        case D3DERR_EXECUTE_CREATE_FAILED:
            return "D3DERR_EXECUTE_CREATE_FAILED\0";
        case D3DERR_EXECUTE_DESTROY_FAILED:
            return "D3DERR_EXECUTE_DESTROY_FAILED\0";
        case D3DERR_EXECUTE_LOCK_FAILED:
            return "D3DERR_EXECUTE_LOCK_FAILED\0";
        case D3DERR_EXECUTE_UNLOCK_FAILED:
            return "D3DERR_EXECUTE_UNLOCK_FAILED\0";
        case D3DERR_EXECUTE_FAILED:
            return "D3DERR_EXECUTE_FAILED\0";
        case D3DERR_EXECUTE_CLIPPED_FAILED:
            return "D3DERR_EXECUTE_CLIPPED_FAILED\0";
        case D3DERR_TEXTURE_NO_SUPPORT:
            return "D3DERR_TEXTURE_NO_SUPPORT\0";
        case D3DERR_TEXTURE_NOT_LOCKED:
            return "D3DERR_TEXTURE_NOT_LOCKED\0";
        case D3DERR_TEXTURE_LOCKED:
            return "D3DERR_TEXTURELOCKED\0";
        case D3DERR_TEXTURE_CREATE_FAILED:
            return "D3DERR_TEXTURE_CREATE_FAILED\0";
        case D3DERR_TEXTURE_DESTROY_FAILED:
            return "D3DERR_TEXTURE_DESTROY_FAILED\0";
        case D3DERR_TEXTURE_LOCK_FAILED:
            return "D3DERR_TEXTURE_LOCK_FAILED\0";
        case D3DERR_TEXTURE_UNLOCK_FAILED:
            return "D3DERR_TEXTURE_UNLOCK_FAILED\0";
        case D3DERR_TEXTURE_LOAD_FAILED:
            return "D3DERR_TEXTURE_LOAD_FAILED\0";
        case D3DERR_MATRIX_CREATE_FAILED:
            return "D3DERR_MATRIX_CREATE_FAILED\0";
        case D3DERR_MATRIX_DESTROY_FAILED:
            return "D3DERR_MATRIX_DESTROY_FAILED\0";
        case D3DERR_MATRIX_SETDATA_FAILED:
            return "D3DERR_MATRIX_SETDATA_FAILED\0";
        case D3DERR_SETVIEWPORTDATA_FAILED:
            return "D3DERR_SETVIEWPORTDATA_FAILED\0";
        case D3DERR_MATERIAL_CREATE_FAILED:
            return "D3DERR_MATERIAL_CREATE_FAILED\0";
        case D3DERR_MATERIAL_DESTROY_FAILED:
            return "D3DERR_MATERIAL_DESTROY_FAILED\0";
        case D3DERR_MATERIAL_SETDATA_FAILED:
            return "D3DERR_MATERIAL_SETDATA_FAILED\0";
        case D3DERR_LIGHT_SET_FAILED:
            return "D3DERR_LIGHT_SET_FAILED\0";
        case D3DRMERR_BADOBJECT:
            return "D3DRMERR_BADOBJECT\0";
        case D3DRMERR_BADTYPE:
            return "D3DRMERR_BADTYPE\0";
        case D3DRMERR_BADALLOC:
            return "D3DRMERR_BADALLOC\0";
        case D3DRMERR_FACEUSED:
            return "D3DRMERR_FACEUSED\0";
        case D3DRMERR_NOTFOUND:
            return "D3DRMERR_NOTFOUND\0";
        case D3DRMERR_NOTDONEYET:
            return "D3DRMERR_NOTDONEYET\0";
        case D3DRMERR_FILENOTFOUND:
            return "The file was not found.\0";
        case D3DRMERR_BADFILE:
            return "D3DRMERR_BADFILE\0";
        case D3DRMERR_BADDEVICE:
            return "D3DRMERR_BADDEVICE\0";
        case D3DRMERR_BADVALUE:
            return "D3DRMERR_BADVALUE\0";
        case D3DRMERR_BADMAJORVERSION:
            return "D3DRMERR_BADMAJORVERSION\0";
        case D3DRMERR_BADMINORVERSION:
            return "D3DRMERR_BADMINORVERSION\0";
        case D3DRMERR_UNABLETOEXECUTE:
            return "D3DRMERR_UNABLETOEXECUTE\0";
        default:
            return "Unrecognized error value.\0";
    }
}
