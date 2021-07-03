//-----------------------------------------------------------------------------
// File: Stretch.CPP
//
// Desc: This program demonstrated clipped blting and stretched clipped blting.
//       It is a non-exclusive mode application that displays a rotating donut
//       in a window.  Clipped blting can be demonstrated by moving another
//       window partially or completely in front of the stretch window.  The
//       rotating donut does not overwrite the clipping window.
//
//       The size of the rotating donut can be changed with menu selections.
//       Any other size than 1x1 demonstrates stretch blting.  The window can
//       also be resized by grabbing any one of the corners with the mouse.
//
//       Another menu option can be used to change the rate of rotation of the
//       donut.
//
//       This is not an exclusive mode application and so it is incapable of
//       setting the display mode.  Therefore, it must be executed on an 8 bit
//       per pixel display.  It will not work correctly with other pixel
//       depths.
//
// Copyright (c) 1995-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
//-----------------------------------------------------------------------------
// Include files
//-----------------------------------------------------------------------------
#include <windows.h>
#include <stdio.h>
#include <stdarg.h>
#include <ddraw.h>
#include "resource.h"
#include "ddutil.h"

//-----------------------------------------------------------------------------
// Local definitions
//-----------------------------------------------------------------------------
#define NAME                "Stretch"
#define TITLE               "Direct Draw Stretch Example"

//-----------------------------------------------------------------------------
// Default settings
//-----------------------------------------------------------------------------
#define SIZEX               64
#define SIZEY               64

//-----------------------------------------------------------------------------
// Global data
//-----------------------------------------------------------------------------
LPDIRECTDRAW7               g_pDD = NULL;        // DirectDraw object
LPDIRECTDRAWSURFACE7        g_pDDSPrimary = NULL;// DirectDraw primary surface
LPDIRECTDRAWSURFACE7        g_pDDSOne = NULL;    // Offscreen surface 1
LPDIRECTDRAWCLIPPER         g_pClipper = NULL;   // Clipper for primary
LPDIRECTDRAWPALETTE         g_pDDPal = NULL;     // The primary surface palette
BOOL                        g_bActive = FALSE;   // Is application active?

//-----------------------------------------------------------------------------
// Local data
//-----------------------------------------------------------------------------
// Name of our bitmap resource.
static char                *szBitmap = "DONUT";
static DWORD                dwUpdateDelay = 13;




//-----------------------------------------------------------------------------
// Name: ReleaseAllObjects()
// Desc: Finished with all objects we use; release them
//-----------------------------------------------------------------------------
static void
ReleaseAllObjects(void)
{
    if (g_pDD != NULL)
    {
        if (g_pDDSPrimary != NULL)
        {
            g_pDDSPrimary->Release();
            g_pDDSPrimary = NULL;
        }
        if (g_pDDSOne != NULL)
        {
            g_pDDSOne->Release();
            g_pDDSOne = NULL;
        }
        if (g_pDDPal != NULL)
        {
            g_pDDPal->Release();
            g_pDDPal = NULL;
        }
        g_pDD->Release();
        g_pDD = NULL;
    }
}




//-----------------------------------------------------------------------------
// Name: InitFail()
// Desc: This function is called if an initialization function fails
//-----------------------------------------------------------------------------
HRESULT
InitFail(HWND hWnd, HRESULT hRet, LPCTSTR szError,...)
{
    char                        szBuff[128];
    va_list                     vl;

    va_start(vl, szError);
    vsprintf(szBuff, szError, vl);
    ReleaseAllObjects();
    MessageBox(hWnd, szBuff, TITLE, MB_OK);
    DestroyWindow(hWnd);
    va_end(vl);
    return hRet;
}




//-----------------------------------------------------------------------------
// Name: RestoreAll()
// Desc: Restore all lost objects
//-----------------------------------------------------------------------------
BOOL
RestoreAll(void)
{
    return g_pDDSPrimary->Restore() == DD_OK &&
        g_pDDSOne->Restore() == DD_OK &&
        DDReLoadBitmap(g_pDDSOne, szBitmap) == DD_OK;

}




//-----------------------------------------------------------------------------
// Name: UpdateFrame()
// Desc: Decide what needs to be blitted next, wait for flip to complete,
//       then flip the buffers.
//-----------------------------------------------------------------------------
void
UpdateFrame(HWND hWnd)
{
    static DWORD                lastTickCount = 0;
    static int                  currentFrame = 0;
    static BOOL                 haveBackground = FALSE;
    DWORD                       thisTickCount;
    RECT                        rcRect;
    RECT                        destRect;
    HRESULT                     hRet;
    POINT                       pt;

    thisTickCount = GetTickCount();
    if ((thisTickCount - lastTickCount) <= dwUpdateDelay)
        return;

    // Move to next frame;
    lastTickCount = thisTickCount;
    currentFrame++;
    if (currentFrame > 59)
        currentFrame = 0;

    // Blit the stuff for the next frame
    rcRect.left = currentFrame % 10 * 64;
    rcRect.top = currentFrame / 10 * 64;
    rcRect.right = currentFrame % 10 * 64 + 64;
    rcRect.bottom = currentFrame / 10 * 64 + 64;

    GetClientRect(hWnd, &destRect);
    if (destRect.right < 128)
        destRect.right = 64;
    if (destRect.bottom < 64)
        destRect.bottom = 64;

    pt.x = pt.y = 0;
    ClientToScreen(hWnd, &pt);
    OffsetRect(&destRect, pt.x, pt.y);

    while (TRUE)
    {
        hRet = g_pDDSPrimary->Blt(&destRect, g_pDDSOne, &rcRect, 0, NULL);

        if (hRet == DD_OK)
            break;
        if (hRet == DDERR_SURFACELOST)
        {
            if (!RestoreAll())
                return;
        }
        if (hRet != DDERR_WASSTILLDRAWING)
            return;
    }
    if (hRet != DD_OK)
        return;
}




//-----------------------------------------------------------------------------
// Name: WindowProc()
// Desc: The Main Window Procedure
//-----------------------------------------------------------------------------
long FAR PASCAL 
WindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    RECT                        rc;
    int                         xRight = -1;
    int                         yBottom = -1;
    MINMAXINFO                 *pMinMax;

    switch (message)
    {
        case WM_ACTIVATE:
            // Pause if minimized
            g_bActive = !((BOOL)HIWORD(wParam));
            return 0L;

        case WM_COMMAND:
            // Handle all menu and accelerator commands 
            switch (LOWORD(wParam))
            {
                case ID_ROTATION_STOP:
                    dwUpdateDelay = 0x7fffffff;
                    break;
                case ID_ROTATION_SLOW:
                    dwUpdateDelay = 200;
                    break;
                case ID_ROTATION_FAST:
                    dwUpdateDelay = 13;
                    break;
                case ID_FILE_EXIT:
                    PostMessage(hWnd, WM_CLOSE, 0, 0L);
                    return 0L;

                case ID_SIZE_1X1:
                    xRight = SIZEX * 1;
                    yBottom = SIZEY * 1;
                    break;
                case ID_SIZE_2X1:
                    xRight = SIZEX * 2;
                    yBottom = SIZEY * 1;
                    break;
                case ID_SIZE_3X1:
                    xRight = SIZEX * 3;
                    yBottom = SIZEY * 1;
                    break;
                case ID_SIZE_1X2:
                    xRight = SIZEX * 1;
                    yBottom = SIZEY * 2;
                    break;
                case ID_SIZE_2X2:
                    xRight = SIZEX * 2;
                    yBottom = SIZEY * 2;
                    break;
                case ID_SIZE_3X2:
                    xRight = SIZEX * 3;
                    yBottom = SIZEY * 2;
                    break;
                case ID_SIZE_1X3:
                    xRight = SIZEX * 1;
                    yBottom = SIZEY * 3;
                    break;
                case ID_SIZE_2X3:
                    xRight = SIZEX * 2;
                    yBottom = SIZEY * 3;
                    break;
                case ID_SIZE_3X3:
                    xRight = SIZEX * 3;
                    yBottom = SIZEY * 3;
                    break;
            }
            if (xRight != -1)
            {
                // Change the window size if set
                SetRect(&rc, 0, 0, xRight, yBottom);
                AdjustWindowRectEx(&rc,
                                   GetWindowLong(hWnd, GWL_STYLE),
                                   GetMenu(hWnd) != NULL,
                                   GetWindowLong(hWnd, GWL_EXSTYLE));
                SetWindowPos(hWnd, NULL, 0, 0, rc.right - rc.left,
                             rc.bottom - rc.top,
                             SWP_NOZORDER | SWP_NOMOVE | SWP_NOACTIVATE);
                return 0L;
            }
            break;

        case WM_DESTROY:
            // Clean up and close the app
            ReleaseAllObjects();
            PostQuitMessage(0);
            return 0L;

        case WM_GETMINMAXINFO:
            // Fix the minimum size of the window to SIZEX x SIZEY
            pMinMax = (MINMAXINFO *)lParam;
            pMinMax->ptMinTrackSize.x = SIZEX+GetSystemMetrics(SM_CXSIZEFRAME)*2;
            pMinMax->ptMinTrackSize.y = SIZEY +
                                        GetSystemMetrics(SM_CYSIZEFRAME) * 2 +
                                        GetSystemMetrics(SM_CYCAPTION) +
                                        GetSystemMetrics(SM_CYMENU);
            return 0L;

        case WM_KEYDOWN:
            // Handle any non-accelerated key commands
            switch (wParam)
            {
                case VK_ESCAPE:
                case VK_F12:
                    PostMessage(hWnd, WM_CLOSE, 0, 0);
                    return 0L;
            }
            break;

        case WM_PALETTECHANGED:
            if ((HWND) wParam == hWnd)
                break;
            // Fall through to WM_QUERYNEWPALETTE
        case WM_QUERYNEWPALETTE:
            // Install our palette here
            if (g_pDDPal)
                g_pDDSPrimary->SetPalette(g_pDDPal);
            // Reload the bitmap into the surface because the palette
            // has changed..
            DDReLoadBitmap(g_pDDSOne, szBitmap);
            return 0L;
    }
    return DefWindowProc(hWnd, message, wParam, lParam);
}




//-----------------------------------------------------------------------------
// Name: InitApp()
// Desc: Do work required for every instance of the application:
//          Create the window, initialize data
//-----------------------------------------------------------------------------
static HRESULT
InitApp(HINSTANCE hInstance, int nCmdShow, HWND *phWnd)
{
    WNDCLASS                    wc;
    DDSURFACEDESC2              ddsd;
    HRESULT                     hRet;

    // Set up and register window class
    wc.style = CS_HREDRAW | CS_VREDRAW;
    wc.lpfnWndProc = WindowProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hInstance;
    wc.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_MAIN_ICON));
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH) (COLOR_WINDOW + 1);
    wc.lpszMenuName = MAKEINTRESOURCE(IDR_MENU);
    wc.lpszClassName = NAME;
    RegisterClass(&wc);

    // Create a window
    *phWnd = CreateWindowEx(0,
                            NAME,
                            TITLE,
                            WS_OVERLAPPEDWINDOW,
                            CW_USEDEFAULT,
                            CW_USEDEFAULT,
                            128,
                            128,
                            NULL,
                            NULL,
                            hInstance,
                            NULL);
    if (!*phWnd)
        return DDERR_GENERIC;
    PostMessage(*phWnd, WM_COMMAND, ID_SIZE_1X1, 0);
    ShowWindow(*phWnd, nCmdShow);
    UpdateWindow(*phWnd);

    ///////////////////////////////////////////////////////////////////////////
    // Create the main DirectDraw object
    ///////////////////////////////////////////////////////////////////////////
    hRet = DirectDrawCreateEx(NULL, (VOID**)&g_pDD, IID_IDirectDraw7, NULL);
    if (hRet != DD_OK)
        return InitFail(*phWnd, hRet, "DirectDrawCreateEx FAILED");

    // Get normal mode
    hRet = g_pDD->SetCooperativeLevel(*phWnd, DDSCL_NORMAL);
    if (hRet != DD_OK)
        return InitFail(*phWnd, hRet, "SetCooperativeLevel FAILED");

    // Create the primary surface
    ZeroMemory(&ddsd, sizeof(ddsd));
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_CAPS;
    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;
    hRet = g_pDD->CreateSurface(&ddsd, &g_pDDSPrimary, NULL);
    if (hRet != DD_OK)
        return InitFail(*phWnd, hRet, "CreateSurface FAILED");

    // Create a clipper for the primary surface
    hRet = g_pDD->CreateClipper(0, &g_pClipper, NULL);
    if (hRet != DD_OK)
        return InitFail(*phWnd, hRet, "CreateClipper FAILED");
    hRet = g_pClipper->SetHWnd(0, *phWnd);
    if (hRet != DD_OK)
        return InitFail(*phWnd, hRet, "SetHWnd FAILED");
    hRet = g_pDDSPrimary->SetClipper(g_pClipper);
    if (hRet != DD_OK)
        return InitFail(*phWnd, hRet, "SetClipper FAILED");

    // Create and set the palette
    g_pDDPal = DDLoadPalette(g_pDD, szBitmap);
    if (g_pDDPal)
        g_pDDSPrimary->SetPalette(g_pDDPal);

    // Load our bitmap
    g_pDDSOne = DDLoadBitmap(g_pDD, szBitmap, 0, 0);
    if (g_pDDSOne == NULL)
        return InitFail(*phWnd, hRet, "DDLoadBitmap FAILED");

    return DD_OK;
}




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Initialization, message loop
//-----------------------------------------------------------------------------
int PASCAL
WinMain(HINSTANCE hInstance,
        HINSTANCE hPrevInstance,
        LPSTR lpCmdLine,
        int nCmdShow)
{
    MSG                         msg;
    HWND                        hWnd;

    if (InitApp(hInstance, nCmdShow, &hWnd) != DD_OK)
        return FALSE;

    while (TRUE)
    {
        if (PeekMessage(&msg, NULL, 0, 0, PM_NOREMOVE))
        {
            if (!GetMessage(&msg, NULL, 0, 0))
                return msg.wParam;
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        else if (g_bActive)
        {
            UpdateFrame(hWnd);
        }
        else
        {
            // Make sure we go to sleep if we have nothing else to do
            WaitMessage();
        }
    }
}


