/*
 *  Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File: misc.c
 *
 *  Miscellaneous functions not involving DD and D3D.  Part of D3DApp.
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
/*                          Setting Defaults                               */
/***************************************************************************/
/*
 * D3DAppISetDefaults
 * Set all the global variables to their default values.  Do not reset the
 * image files.
 */
void
D3DAppISetDefaults(void)
{
    int n;
    char backup[D3DAPP_MAXTEXTURES][50];

    n = d3dappi.NumTextures;
    memcpy(&backup[0][0], &d3dappi.ImageFile[0][0], 50 * D3DAPP_MAXTEXTURES);
    ZEROMEM(d3dappi);
    memcpy(&d3dappi.ImageFile[0][0], &backup[0][0], 50 * D3DAPP_MAXTEXTURES);
    d3dappi.NumTextures = n;
    d3dappi.bAppActive = TRUE;
    ZEROMEM(d3dapprs);
    d3dapprs.bZBufferOn = TRUE;
    d3dapprs.bPerspCorrect = FALSE;
    d3dapprs.ShadeMode = D3DSHADE_GOURAUD;
    d3dapprs.TextureFilter = D3DFILTER_NEAREST;
    d3dapprs.TextureBlend = D3DTBLEND_MODULATE;
    d3dapprs.FillMode = D3DFILL_SOLID;
    d3dapprs.bDithering = FALSE;
    d3dapprs.bSpecular = TRUE;
    d3dapprs.bAntialiasing = FALSE;
    d3dapprs.bFogEnabled = FALSE;
    d3dapprs.FogColor = RGB_MAKE(0, 0, 0);
    d3dapprs.FogMode = D3DFOG_LINEAR;
    d3dapprs.FogStart = D3DVAL(6.0);
    d3dapprs.FogEnd = D3DVAL(11.0);

    lpClipper = NULL;
    lpPalette = NULL;
    bPrimaryPalettized = FALSE;
    bPaletteActivate = FALSE;
    bIgnoreWM_SIZE = FALSE;
    ZEROMEM(ppe);
    ZEROMEM(Originalppe);
    LastError = DD_OK;
    ZEROMEM(LastErrorString);
    D3DDeviceDestroyCallback = NULL;
    D3DDeviceDestroyCallbackContext = NULL;
    D3DDeviceCreateCallback = NULL;
    D3DDeviceCreateCallbackContext = NULL;
}

/***************************************************************************/
/*                Calling Device Create And Destroy Callbacks              */
/***************************************************************************/
BOOL
D3DAppICallDeviceDestroyCallback(void)
{
    if (D3DDeviceDestroyCallback) {
        if (CallbackRefCount) {
            --CallbackRefCount;
            return (D3DDeviceDestroyCallback)(D3DDeviceDestroyCallbackContext);
        }
    }
    return TRUE;
}

BOOL
D3DAppICallDeviceCreateCallback(int w, int h)
{
    if (D3DDeviceCreateCallback) {
        ++CallbackRefCount;
        return (D3DDeviceCreateCallback)(w, h, &d3dappi.lpD3DViewport,
                                         D3DDeviceCreateCallbackContext);
    }
    return TRUE;
}

/***************************************************************************/
/*            Choosing and verifying the driver and display mode           */
/***************************************************************************/
/*
 * D3DAppIPickDriver
 * Choose a driver from the list of available drivers which can render to one
 * of the given depths.  Hardware is prefered.  Mono-lighting drivers are
 * prefered over RGB.
 */
BOOL
D3DAppIPickDriver(int* driver, DWORD depths)
{
    int i, j;
    j = 0;
    for (i = 0; i < d3dappi.NumDrivers; i++)
        if (d3dappi.Driver[i].Desc.dwDeviceRenderBitDepth & depths)
            break;
    if (i >= d3dappi.NumDrivers) {
        *driver = D3DAPP_BOGUS;
        return TRUE;
    }
    j = i;
    for (i = 0; i < d3dappi.NumDrivers; i++) {
        if (d3dappi.Driver[i].Desc.dwDeviceRenderBitDepth & depths) {
            if (d3dappi.Driver[i].bIsHardware &&
                                              !d3dappi.Driver[j].bIsHardware)
                                                  j = i;
            else if (d3dappi.Driver[i].bIsHardware ==
                                             d3dappi.Driver[j].bIsHardware) {
                if (d3dappi.Driver[i].Desc.dcmColorModel & D3DCOLOR_MONO &&
                    !(d3dappi.Driver[j].Desc.dcmColorModel & D3DCOLOR_MONO))
                        j = i;
            }
        }
    }
    if (j >= d3dappi.NumDrivers)
        *driver = D3DAPP_BOGUS;
    else
        *driver = j;
    return TRUE;
}

/*
 * D3DAppIFilterDisplayModes
 * Set the bThisDriverCanDo flag for each display mode if the given driver
 * can render in that depth.  Also checks to make sure there is enough
 * total video memory for front/back/z-buffer in video memory if it's a
 * hardware device.
 */
BOOL
D3DAppIFilterDisplayModes(int driver)
{
    int i;
    DWORD depths = d3dappi.Driver[driver].Desc.dwDeviceRenderBitDepth;

    for (i = 0; i < d3dappi.NumModes; i++) {
        d3dappi.Mode[i].bThisDriverCanDo = FALSE;
        if (!(D3DAppIBPPToDDBD(d3dappi.Mode[i].bpp) & depths))
            continue;
        d3dappi.Mode[i].bThisDriverCanDo = TRUE;

    }
    d3dappi.ThisMode.bThisDriverCanDo =
                             d3dappi.Mode[d3dappi.CurrMode].bThisDriverCanDo;
    return TRUE;
}

/*
 * D3DAppIPickDisplayMode
 * Pick a display mode of one of the given depths.  640x480x16 is prefered.
 */
BOOL
D3DAppIPickDisplayMode(int *mode, DWORD depths)
{
    int i, j;
    for (i = 0; i < d3dappi.NumModes; i++)
        if (D3DAppIBPPToDDBD(d3dappi.Mode[i].bpp) & depths)
            break;
    j = i;
    for (; i < d3dappi.NumModes; i++) {
        if (!(D3DAppIBPPToDDBD(d3dappi.Mode[i].bpp) & depths))
            continue;
        if (d3dappi.Mode[i].w == 640 && d3dappi.Mode[i].h == 480 &&
            d3dappi.Mode[i].bpp == 16) {
            j = i;
            break;
        }
    }
    if (j >= d3dappi.NumModes)
        *mode = D3DAPP_BOGUS;
    else
        *mode = j;
    return TRUE;
}

/*
 * D3DAppIVerifyDriverAndMode
 * Verifies the selected driver and mode combination.  If the driver is
 * specified, the mode will be changed to accomodate the driver if it's not
 * compatible.  If the driver is not specified, one will be selected which is
 * compatible with the specified mode.  If neither are specified, a suitable
 * pair will be returned.
 */
BOOL
D3DAppIVerifyDriverAndMode(int* lpdriver, int* lpmode)
{
    DWORD depths;
    int driver, mode, i;
    driver = *lpdriver; mode = *lpmode;

    if (mode == D3DAPP_USEWINDOW && !d3dappi.bIsPrimary) {
        D3DAppISetErrorString("Cannot render to a window when the DirectDraw device is not the primary.\n");
        goto exit_with_error;
    }

    /*
     * If I've been ask to choose a driver, choose one which is compatible
     * with the specified mode.
     */
    if (driver == D3DAPP_YOUDECIDE) {
        if (mode == D3DAPP_USEWINDOW) {
            /*
             * I must find a driver for this display depth
             */
            depths = D3DAppIBPPToDDBD(d3dappi.WindowsDisplay.bpp);
            ATTEMPT(D3DAppIPickDriver(&driver, depths));
            if (driver == D3DAPP_BOGUS) {
                D3DAppISetErrorString("Cannot find a D3D device driver which is compatible with the current display depth.\n");
                goto exit_with_error;
            }
            /*
             * I don't need to go through the mode selection since I've
             * verified it here
             */
            goto ret_ok;
        } else if (mode == D3DAPP_YOUDECIDE) {
            /*
             * I'm free to choose any driver which can use even one
             * supported depth
             */
            if (d3dappi.bIsPrimary)
                depths = D3DAppIBPPToDDBD(d3dappi.WindowsDisplay.bpp);
            else
                depths = 0;
            for (i = 0; i < d3dappi.NumModes; i++)
                depths |= D3DAppIBPPToDDBD(d3dappi.Mode[i].bpp);
            ATTEMPT(D3DAppIPickDriver(&driver, depths));
            if (driver == D3DAPP_BOGUS) {
                D3DAppISetErrorString("Cannot find a D3D device driver which is compatible with the current display depth or any supported fullscreen mode.\n");
                goto exit_with_error;
            }
            /*
             * The mode will be chosen in the next section
             */
        } else {
            /*
             * Must pick a driver which uses the given mode depth
             */
            ATTEMPT(D3DAppIPickDriver(&driver,
                                  D3DAppIBPPToDDBD(d3dappi.Mode[mode].bpp)));
            if (driver == D3DAPP_BOGUS) {
                D3DAppISetErrorString("Cannot find a D3D device driver which is compatible with the specified fullscreen mode.\n");
                goto exit_with_error;
            }
            /*
             * I don't need to go through the mode selection since I've
             * verified it here
             */
            goto ret_ok;
        }
    }

    /*
     * At this stage, I have a driver I want to match the mode to.
     */
    if (mode == D3DAPP_YOUDECIDE) {
        /*
         * If it's my choice, I prefer windowed over fullscreen
         */
        if (d3dappi.bIsPrimary) {
            if (D3DAppIBPPToDDBD(d3dappi.WindowsDisplay.bpp) &
                    d3dappi.Driver[driver].Desc.dwDeviceRenderBitDepth) {
                mode = D3DAPP_USEWINDOW;
                goto ret_ok;
            }
        }
        /*
         * Either this is not a primary DD device or the driver cannot use
         * the Windows display depth
         */
        ATTEMPT(D3DAppIPickDisplayMode(&mode,
                        d3dappi.Driver[driver].Desc.dwDeviceRenderBitDepth));
        if (mode == D3DAPP_BOGUS) {
            D3DAppISetErrorString("The selected D3D device driver is not compatible with the current display depth or any supported fullscreen modes.\n");
            goto exit_with_error;
        }
        goto ret_ok;
    } else if (mode == D3DAPP_USEWINDOW) {
        /*
         * Check to see if this driver can use the Windows display depth
         */
        if (D3DAppIBPPToDDBD(d3dappi.WindowsDisplay.bpp) &
                d3dappi.Driver[driver].Desc.dwDeviceRenderBitDepth) {
            goto ret_ok;
        } else {
            /*
             * Since it cannot, call this function again to choose any
             * display mode which is compatible
             */
            mode = D3DAPP_YOUDECIDE;
            ATTEMPT(D3DAppIVerifyDriverAndMode(&driver, &mode));
            if (driver == D3DAPP_BOGUS)
                goto exit_with_error;
            goto ret_ok;
        }
    } else {
        /*
         * Check to see if this driver can use the specified fullscreen mode
         */
        if (D3DAppIBPPToDDBD(d3dappi.Mode[mode].bpp) &
                d3dappi.Driver[driver].Desc.dwDeviceRenderBitDepth) {
            goto ret_ok;
        } else {
            /*
             * Since it cannot, call this function again to choose any
             * display mode which is compatible
             */
            mode = D3DAPP_YOUDECIDE;
            ATTEMPT(D3DAppIVerifyDriverAndMode(&driver, &mode));
            if (driver == D3DAPP_BOGUS)
                goto exit_with_error;
            goto ret_ok;
        }
    }

ret_ok:
    *lpdriver = driver; *lpmode = mode;
    return TRUE;
exit_with_error:
    return FALSE;
}

/***************************************************************************/
/*                        Dirty Rectangle Functions                        */
/***************************************************************************/
/*
 * D3DAppIValidateDirtyRects
 * Set the dirty rectangles for the front and back buffers to the entire
 * client size.
 */
void
D3DAppIValidateDirtyRects(void)
{
    NumDirtyClientRects = 1; NumDirtyBackRects = 1; NumDirtyZRects = 1;
    SetRect((LPRECT)&DirtyClient[0], 0, 0, d3dappi.szClient.cx,
            d3dappi.szClient.cy);
    SetRect((LPRECT)&DirtyBack[0], 0, 0, d3dappi.szClient.cx,
            d3dappi.szClient.cy);
    SetRect((LPRECT)&DirtyZ[0], 0, 0, d3dappi.szClient.cx,
            d3dappi.szClient.cy);
}

/*
 * D3DAppICopyRectList
 * Copy a list of rectangles to another
 */
void
D3DAppICopyRectList(int* dstnum, LPD3DRECT dst, int srcnum, LPD3DRECT src)
{
    int i;
    for (i = 0; i < srcnum; i++)
        dst[i] = src[i];
    *dstnum = srcnum;
}

/*
 * MERGE macro
 * Set first rectangle to be the smallest rectangle containing both rects
 */
#undef MERGE
#define MERGE(rc1, rc2)                         \
    do {                                        \
        if (rc2.x1 < rc1.x1) rc1.x1 = rc2.x1;   \
        if (rc2.y1 < rc1.y1) rc1.y1 = rc2.y1;   \
        if (rc2.x2 > rc1.x2) rc1.x2 = rc2.x2;   \
        if (rc2.y2 > rc1.y2) rc1.y2 = rc2.y2;   \
    } while(0)

/*
 * D3DAppIMergeRectLists
 * Merge two lists of rectangles to create another list of rectangles. The
 * merged list of rectangles covers all the area of the original two with NO
 * OVERLAPPING amongst it's rectangles.
 */
void
D3DAppIMergeRectLists(int* dstnum, LPD3DRECT dst, int src1num, LPD3DRECT src1,
                     int src2num, LPD3DRECT src2)
{
    LPD3DRECT rc;
    int* isvalid;
    int num, i, j, intersect;
    rc = (LPD3DRECT)malloc(sizeof(D3DRECT) * D3DAPP_MAXCLEARRECTS * 2);
    memset(rc, 0, sizeof(D3DRECT) * D3DAPP_MAXCLEARRECTS * 2);
    isvalid = (int*)malloc(sizeof(int) * D3DAPP_MAXCLEARRECTS * 2);
    memset(isvalid, 0, sizeof(int) * D3DAPP_MAXCLEARRECTS * 2);
    for (i = 0; i < src1num; i++) {
        memcpy(&rc[i], &src1[i], sizeof(D3DRECT));
        if ((rc[i].x1 == 0 && rc[i].x2 == 0) ||
            (rc[i].y1 == 0 && rc[i].y2 == 0))
            isvalid[i] = 0;
        else if (rc[i].x1 <= rc[i].x2 && rc[i].y1 <= rc[i].y2)
            isvalid[i] = 1;
        else
            isvalid[i] = 0;
    }
    for (i = 0; i < src2num; i++) {
        memcpy(&rc[i + src1num], &src2[i], sizeof(D3DRECT));
        if (rc[i + src1num].x1 <= rc[i + src1num].x2 &&
            rc[i + src1num].y1 <= rc[i + src1num].y2)
            isvalid[i + src1num] = 1;
        else
            isvalid[i + src1num] = 0;

    }
    num = src1num + src2num;
    for (i = 0; i < num - 1; i++) {
        if (!isvalid[i]) continue;
        j = i + 1;
        do {
            intersect = 0;
            for (; j < num; j++) {
                if (j != i && isvalid[j]) {
                    if (rc[i].x1 < rc[j].x1) {
                        if (rc[i].x2 < rc[j].x1)
                            continue;
                    } else {
                        if (rc[j].x2 < rc[i].x1)
                            continue;
                    }
                    if (rc[i].y1 < rc[j].y1) {
                        if (rc[i].y2 < rc[j].y1)
                            continue;
                    } else {
                        if (rc[j].y2 < rc[i].y1)
                            continue;
                    }
                    MERGE(rc[i], rc[j]);
                    isvalid[j] = 0;
                    j = 0; intersect = 1;
                    break;
                }
            }
        } while(intersect);
    }

    for (i = 0, j = 0; i < num; i++)
        if (isvalid[i]) ++j;
    if (j > D3DAPP_MAXCLEARRECTS) {
        for (i = 0; i < num; i++)
            if (isvalid[i]) break;
        if (i < num) {
            *dstnum = 1;
            dst[0] = rc[i];
            for (; i < num; i++) {
                if (isvalid[i]) {
                    MERGE(dst[0], rc[i]);
                }
            }
        } else {
            *dstnum = 0;
        }
    } else {
        for (i = 0, j = 0; i < num; i++) {
            if (isvalid[i]) {
                memcpy(&dst[j], &rc[i], sizeof(D3DRECT));
                ++j;
            }
        }
        *dstnum = j;
    }
    free(rc);
    free(isvalid);
}

/***************************************************************************/
/*                     Getting and Setting Window Attribs                  */
/***************************************************************************/
/*
 * D3DAppISetClientSize
 * Set the client size of the given window.  A WM_SIZE message is generated,
 * but ignored.
 */
void
D3DAppISetClientSize(HWND hwnd, int w, int h, BOOL bReturnFromFullscreen)
{
    RECT rc;

    bIgnoreWM_SIZE = TRUE;
    if (bReturnFromFullscreen) {
        SetRect(&rc, 0, 0, w, h);
        AdjustWindowRectEx(&rc, GetWindowLong(hwnd, GWL_STYLE),
                           GetMenu(hwnd) != NULL,
                           GetWindowLong(hwnd, GWL_EXSTYLE));
        SetWindowPos(hwnd, NULL, 0, 0, rc.right-rc.left,
                     rc.bottom-rc.top,
                     SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE);
        SetWindowPos(hwnd, HWND_NOTOPMOST, 0, 0, 0, 0,
                     SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);

    } else {
        /*
         * This is the only way to set the client size correctly if the menu
         * is stacked, so do it unless we are returning from a fullscreen
         * mode.
         */
        SendMessage(hwnd, WM_SIZE, SIZE_RESTORED, w + (h << 16) );
        GetWindowRect(hwnd, &rc);
        SetWindowPos(hwnd, NULL, 0, 0, rc.right-rc.left,
                     rc.bottom-rc.top,
                     SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE);
        SetWindowPos(hwnd, HWND_NOTOPMOST, 0, 0, 0, 0,
                     SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);
    }
    bIgnoreWM_SIZE = FALSE;
    d3dappi.pClientOnPrimary.x = d3dappi.pClientOnPrimary.y = 0;
    ClientToScreen(hwnd, &d3dappi.pClientOnPrimary);
    d3dappi.szClient.cx = w; d3dappi.szClient.cy = h;
}

/*
 * D3DAppIGetClientWin
 * Gets the client window size and sets it in the D3DAppInfo structure
 */
void
D3DAppIGetClientWin(HWND hwnd)
{
    RECT rc;
    if (!d3dappi.bFullscreen) {
        d3dappi.pClientOnPrimary.x = d3dappi.pClientOnPrimary.y = 0;
        ClientToScreen(hwnd, &d3dappi.pClientOnPrimary);
        GetClientRect(hwnd, &rc);
        d3dappi.szClient.cx = rc.right;
        d3dappi.szClient.cy = rc.bottom;
    } else {
        /*
         * In the fullscreen case, we must be careful because if the window
         * frame has been drawn, the client size has shrunk and this can
         * cause problems, so it's best to report the entire screen.
         */
        d3dappi.pClientOnPrimary.x = d3dappi.pClientOnPrimary.y = 0;
        d3dappi.szClient.cx = d3dappi.ThisMode.w;
        d3dappi.szClient.cy = d3dappi.ThisMode.h;
    }
}


/***************************************************************************/
/*                              Error reporting                            */
/***************************************************************************/

/*
 * D3DAppISetErrorString
 * Set the global variable which records the last error string.
 */
void
D3DAppISetErrorString( LPSTR fmt, ... )
{
    char buff[256];
    va_list args;

    buff[0] = 0;
    va_start(args, fmt);
    wvsprintf(&buff[0], fmt, args);
    va_end(args);

    lstrcat(buff, "\r\n");
    lstrcpy(LastErrorString, buff);
}

/* dpf
 * Debug printf.  Very useful for fullscreen exclusive mode or when surfaces
 * are in video memory.
 */
void __cdecl
dpf( LPSTR fmt, ... )
{
    char buff[256];
    va_list args;

    lstrcpy(buff, "D3DApp: ");
    va_start(args, fmt);
    wvsprintf(&buff[lstrlen(buff)], fmt, args);
    va_end(args);

    lstrcat(buff, "\r\n");
    OutputDebugString(buff);
}
