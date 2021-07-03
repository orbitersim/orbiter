//-----------------------------------------------------------------------------
// File: DDOvrly.CPP
//
// Desc: Sample application to demonstrate the implimentation
//       of overlays using DirectDraw.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Include files
//-----------------------------------------------------------------------------
#include <windows.h>
#include <ddraw.h>
#include "ddutil.h"
#include "resource.h"

//-----------------------------------------------------------------------------
// Global data
//-----------------------------------------------------------------------------
// Global Application Window Data
HWND                        g_hAppWnd;           // The program's window handle
// Global Application DirectDraw Data
LPDIRECTDRAW7               g_pDD = NULL;        // DirectDraw object
LPDIRECTDRAWSURFACE7        g_pDDSPrimary = NULL;// DirectDraw primary surface
LPDIRECTDRAWSURFACE7        g_pDDSBack = NULL;   // DirectDraw back surface
LPDIRECTDRAWSURFACE7        g_pDDSOverlay = NULL;// DirectDraw overlay surface
LPDIRECTDRAWCLIPPER         g_pClipper = NULL;   // DirectDraw clipping struct
DDOVERLAYFX                 g_OverlayFX;         // DirectDraw overlay effects struct
DDCAPS                      g_DDCaps;            // DirectDraw hardware capabilities struct
DWORD                       g_OverlayFlags = 0;  // DirectDraw overlay flags variable
// Global Application data
DWORD                       g_dwXRatio,
                            g_dwYRatio;          // The ratios between the src and dst rects
BOOL                        g_bInError = FALSE;  // Flag to see if we're inside an error
// Source and destination rectangles for overlay
RECT                        g_rcSrc = {0, 0, BMAPX, BMAPY},
                            g_rcDst = {0, 0, BMAPX, BMAPY};




//-----------------------------------------------------------------------------
// Name: DisplayError()
// Desc: Create a dialog box and tell the user what went wrong
//-----------------------------------------------------------------------------
int
DisplayError(LPSTR lpstrErr)
{
    int                         retval = 0;

    // Since it is possible (and common) to recieve additional paint messages
    // while displaying an error (due to other window's overlapping, not to
    // mention the popup of the MessageBox) we will use this global flag
    // "g_bInError" to check if we are currently displaying an error message.
    // If we are, just ignore the error (ie. don't show the user, and demand a
    // response) and continue
    if (!g_bInError)
    {
        g_bInError = TRUE;
        retval = MessageBox(g_hAppWnd, lpstrErr, "Error!", MB_OK);
        g_bInError = FALSE;
        return retval;
    }
    return IDOK;
}




//-----------------------------------------------------------------------------
// Name: DestroyOverlay()
// Desc: Releases the overlay surface
//-----------------------------------------------------------------------------
void
DestroyOverlay()
{
    if (g_pDDSOverlay)
    {
        g_pDDSOverlay->Release();
        g_pDDSOverlay = NULL;
    }
}




//-----------------------------------------------------------------------------
// Name: DestroyPrimary()
// Desc: Releases the primary surface
//-----------------------------------------------------------------------------
void
DestroyPrimary()
{
    if (g_pDDSPrimary)
    {
        g_pDDSPrimary->Release();
        g_pDDSPrimary = NULL;
    }
}




//-----------------------------------------------------------------------------
// Name: DestroyDDraw()
// Desc: Releases core DirectDraw objects
//-----------------------------------------------------------------------------
void
DestroyDDraw()
{
    DestroyPrimary();
    // Release the DDraw object
    if (g_pDD)
    {
        g_pDD->Release();
        g_pDD = NULL;
    }
}




//-----------------------------------------------------------------------------
// Name: CheckBoundries()
// Desc: Checks and corrects all boundries for alignment and stretching
//-----------------------------------------------------------------------------
void
CheckBoundries(void)
{
    // This is the default window title, we reset it every time through, just
    // in case we set it with an error last time.  We display errors in the
    // window title just to clean up the usability of the app a little bit.
    // This is mostly a factor for "full drag" apps, because having to pop up a
    // dialog box EVERY update, if the window is too small or too large, gets
    // to be pretty rediculous.
    SetWindowText(g_hAppWnd, "DDOverlay Sample App");

    // Make sure the coordinates fulfill the stretching requirements.  Often
    // the hardware will require a certain ammount of stretching to do
    // overlays. This stretch factor is held in dwMinOverlayStretch as the
    // stretch factor multiplied by 1000 (to keep an accuracy of 3 decimal
    // places).
    if ((g_DDCaps.dwCaps & DDCAPS_OVERLAYSTRETCH) && (g_DDCaps.dwMinOverlayStretch)
        && (g_dwXRatio < g_DDCaps.dwMinOverlayStretch))
    {
        g_rcDst.right = 2 * GetSystemMetrics(SM_CXSIZEFRAME) + g_rcDst.left + (BMAPX
                                 * (g_DDCaps.dwMinOverlayStretch + 1)) / 1000;
        SetWindowText(g_hAppWnd, "Window is too small!");
    }
    if ((g_DDCaps.dwCaps & DDCAPS_OVERLAYSTRETCH) && (g_DDCaps.dwMaxOverlayStretch)
        && (g_dwXRatio > g_DDCaps.dwMaxOverlayStretch))
    {
        g_rcDst.right = 2 * GetSystemMetrics(SM_CXSIZEFRAME) + g_rcDst.left + (BMAPY
                               * (g_DDCaps.dwMaxOverlayStretch + 999)) / 1000;
        SetWindowText(g_hAppWnd, "Window is too large!");
    }

    // Recalculate the ratio's for the upcoming calculations
    g_dwXRatio = (g_rcDst.right - g_rcDst.left) * 1000 / (g_rcSrc.right - g_rcSrc.left);
    g_dwYRatio = (g_rcDst.bottom - g_rcDst.top) * 1000 / (g_rcSrc.bottom - g_rcSrc.top);

    // Check to make sure we're within the screen's boundries, if not then fix
    // the problem by adjusting the source rectangle which we draw from.
    if (g_rcDst.left < 0)
    {
        g_rcSrc.left = -g_rcDst.left * 1000 / g_dwXRatio;
        g_rcDst.left = 0;
    }
    if (g_rcDst.right > GetSystemMetrics(SM_CXSCREEN))
    {
        g_rcSrc.right = BMAPX - ((g_rcDst.right - GetSystemMetrics(SM_CXSCREEN)) *
                                1000 / g_dwXRatio);
        g_rcDst.right = GetSystemMetrics(SM_CXSCREEN);
    }
    if (g_rcDst.bottom > GetSystemMetrics(SM_CYSCREEN))
    {
        g_rcSrc.bottom = BMAPY - ((g_rcDst.bottom - GetSystemMetrics(SM_CYSCREEN))
                                 * 1000 / g_dwYRatio);
        g_rcDst.bottom = GetSystemMetrics(SM_CYSCREEN);
    }
    // I don't know how useful this is... but just in case someone can do it - here's the check.
    if (g_rcDst.top < 0)
    {
        g_rcSrc.top = -g_rcDst.top * 1000 / g_dwYRatio;
        g_rcDst.top = 0;
    }

    // Make sure the coordinates fulfill the alignment requirements
    // these expressions (x & -y) just do alignment by dropping low order bits...
    // so to round up, we add first, then truncate.
    if ((g_DDCaps.dwCaps & DDCAPS_ALIGNBOUNDARYSRC) && g_DDCaps.dwAlignBoundarySrc)
        g_rcSrc.left = (g_rcSrc.left + g_DDCaps.dwAlignBoundarySrc / 2) & -(signed)
            (g_DDCaps.dwAlignBoundarySrc);
    if ((g_DDCaps.dwCaps & DDCAPS_ALIGNSIZESRC) && g_DDCaps.dwAlignSizeSrc)
        g_rcSrc.right = g_rcSrc.left + (g_rcSrc.right - g_rcSrc.left + g_DDCaps.dwAlignSizeSrc
                                   / 2) & -(signed) (g_DDCaps.dwAlignSizeSrc);
    if ((g_DDCaps.dwCaps & DDCAPS_ALIGNBOUNDARYDEST) && g_DDCaps.dwAlignBoundaryDest)
        g_rcDst.left = (g_rcDst.left + g_DDCaps.dwAlignBoundaryDest / 2) & -(signed)
            (g_DDCaps.dwAlignBoundaryDest);
    if ((g_DDCaps.dwCaps & DDCAPS_ALIGNSIZEDEST) && g_DDCaps.dwAlignSizeDest)
        g_rcDst.right = g_rcDst.left + (g_rcDst.right - g_rcDst.left) & -(signed) (g_DDCaps.dwAlignSizeDest);
}




//-----------------------------------------------------------------------------
// Name: DrawOverlay()
// Desc: Load the bitmap and copy it to the overlay surface
//-----------------------------------------------------------------------------
int
DrawOverlay()
{
    HBITMAP                     hbm;        // The handle to the bitmap
    HRESULT                     hRet;       // This is where we put return values from DirectDraw.
    HDC                         hdcImage;   // The handle to the Image's DC
    HDC                         hdcSurf;    // The handle to the Overlay's DC

    // Load the bitmap from the resource file.  Note that LoadImage is used
    // instead of LoadBitmap.  This is because LoadBitmap will force the picture
    // into the same pixel format as the screen, but we can have the overlay
    // surface at a different pixel format than the screen, so we don't want it
    // to convert the picture for us.
    hbm = (HBITMAP) LoadImage(GetModuleHandle(NULL), MAKEINTRESOURCE(IDB_BITMAP1),
                              IMAGE_BITMAP, BMAPX, BMAPY, LR_CREATEDIBSECTION);
    if (hbm == NULL)
        return FALSE;

    // Create a DC that is compatible with (the same attributes as) the whole
    // screen (primary surface), and associate a bitmap with it
    hdcImage = CreateCompatibleDC(NULL);
    SelectObject(hdcImage, hbm);

    // Get the DC for the Overlay surface
    hRet = g_pDDSOverlay->GetDC(&hdcSurf);
    if (hRet != DD_OK)
    {
        if (hdcImage)
            DeleteDC(hdcImage);
        if (hbm)
            DeleteObject(hbm);
        return FALSE;
    }

    // Copy the bitmap to the overlay surface
    if (BitBlt(hdcSurf, 0, 0, BMAPX, BMAPY, hdcImage, 0, 0, SRCCOPY) == FALSE)
    {
        if (hdcSurf)
            g_pDDSOverlay->ReleaseDC(hdcSurf);
        if (hdcImage)
            DeleteDC(hdcImage);
        if (hbm)
            DeleteObject(hbm);
        return FALSE;
    }

    // Release all DC's and memory we've used
    g_pDDSOverlay->ReleaseDC(hdcSurf);
    DeleteDC(hdcImage);
    DeleteObject(hbm);

    // Setup effects structure
    memset(&g_OverlayFX, 0, sizeof(g_OverlayFX));
    g_OverlayFX.dwSize = sizeof(g_OverlayFX);

    // Setup overlay flags.
    g_OverlayFlags = DDOVER_SHOW;

    // Check for destination color keying capability
    if (g_DDCaps.dwCKeyCaps & DDCKEYCAPS_DESTOVERLAY)
    {
        // If so, we'll use it to clip the bitmap when other windows go on top
        // of us. Just for the record - this color range for color keying (the
        // high/low values) are not heavily supported right now, so for almost
        // all cards, just use the same color for both.
        g_OverlayFX.dckDestColorkey.dwColorSpaceLowValue = DDColorMatch(g_pDDSPrimary,
                                                            RGB(255, 0, 255));
        g_OverlayFX.dckDestColorkey.dwColorSpaceHighValue = DDColorMatch(g_pDDSPrimary,
                                                            RGB(255, 0, 255));
        g_OverlayFlags |= DDOVER_DDFX | DDOVER_KEYDESTOVERRIDE;
    }
    else
    {
        // If not, we'll setup a clipper for the window.  This will fix the
        // problem on a few video cards - but the ones that don't shouldn't
        // care.
        hRet = g_pDD->CreateClipper(0, &g_pClipper, NULL);
        if (hRet != DD_OK)
            return FALSE;
        hRet = g_pClipper->SetHWnd(0, g_hAppWnd);
        if (hRet != DD_OK)
            return FALSE;
        hRet = g_pDDSPrimary->SetClipper(g_pClipper);
        if (hRet != DD_OK)
            return FALSE;
    }
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: DDPrimaryInit()
// Desc: Init the primary surface
//-----------------------------------------------------------------------------
int
DDPrimaryInit()
{
    HRESULT                     hRet;
    DDSURFACEDESC2              ddsd;  // A surface description structure

    // Create the primary surface.  The primary surface is the full screen -
    // since we're a windowed app - we'll just write to the portion of the
    // screen within our window.
    memset(&ddsd, 0, sizeof(ddsd)); // Set all fields of struct to 0 and set .dwSize to
    ddsd.dwSize = sizeof(ddsd);     // Sizeof the variable - these two steps required for most DDraw structs
    ddsd.dwFlags = DDSD_CAPS;       // Set flags for variables we're using...
    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;  // Set the variables we said we would in dwFlags
    hRet = g_pDD->CreateSurface(&ddsd, &g_pDDSPrimary, NULL);
    if (hRet != DD_OK)
        return FALSE;
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: DDInit()
// Desc: Init DirectDraw Stuff
//-----------------------------------------------------------------------------
int
DDInit()
{
    HRESULT                     hRet;

    hRet = DirectDrawCreateEx(NULL, (VOID**)&g_pDD, IID_IDirectDraw7, NULL);
    if (hRet != DD_OK)
        return FALSE;

    // Set cooperation level with other windows to be normal (ie. not full screen)
    // You MUST set the cooperation level to be SOMETHING, for windowed apps use
    // DDSCL_NORMAL, for full screen use: DDSCL_EXCLUSIVE | DDSCL_FULLSCREEN.
    hRet = g_pDD->SetCooperativeLevel(g_hAppWnd, DDSCL_NORMAL);
    if (hRet != DD_OK)
        return FALSE;

    if (!DDPrimaryInit())
        return FALSE;
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: DDOverlayInit()
// Desc: Setup the overlay object
//-----------------------------------------------------------------------------
int
DDOverlayInit()
{
    DDSURFACEDESC2              ddsd;  // DirectDraw surface descriptor
    HRESULT                     hRet;  // I'm not even going to try...
    DDPIXELFORMAT               ddpfOverlayFormat =
    // The pixel format that we want the surface to be in
                                {
                                    sizeof(DDPIXELFORMAT),
                                    DDPF_RGB,
                                    0, 16,
                                    0xF800, 0x07e0, 0x001F,   // 16 bit RGB 5:6:5
                                    0
                                };


    // Setup the overlay surface's attributes in the surface descriptor
    memset(&ddsd, 0, sizeof(ddsd));
    ddsd.dwSize = sizeof(ddsd);
    ddsd.ddsCaps.dwCaps = DDSCAPS_OVERLAY | DDSCAPS_VIDEOMEMORY;
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH | DDSD_PIXELFORMAT;
    ddsd.dwBackBufferCount = 0;
    ddsd.dwWidth = BMAPX;
    ddsd.dwHeight = BMAPY;
    ddsd.ddpfPixelFormat = ddpfOverlayFormat;  // Use 16 bit RGB 5:6:5 pixel format

    // Attempt to create the surface with theses settings
    hRet = g_pDD->CreateSurface(&ddsd, &g_pDDSOverlay, NULL);
    if (hRet != DD_OK)
        return FALSE;
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: CheckOverlaySupport()
// Desc: Make sure the hardware can do overlays
//-----------------------------------------------------------------------------
int
CheckOverlaySupport()
{
    // Get hardware's CAPabilitieS
    memset(&g_DDCaps, 0, sizeof(g_DDCaps));
    g_DDCaps.dwSize = sizeof(g_DDCaps);
    if (g_pDD->GetCaps(&g_DDCaps, 0))
        return FALSE;

    // Make sure it supports overlays
    if (!(g_DDCaps.dwCaps & DDCAPS_OVERLAY))
        return FALSE;

    // Make sure it supports stretching (scaling)
    if (!(g_DDCaps.dwCaps & DDCAPS_OVERLAYSTRETCH))
        return FALSE;
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: WndProc()
// Desc: Windows message handler
//-----------------------------------------------------------------------------
LRESULT CALLBACK
WndProc(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
    PAINTSTRUCT                 ps;         // Structure for the paint message
    POINT                       p = {0, 0}; // Translation point for the window's client region
    HRESULT                     hRet;

    switch (iMsg)
    {
        case WM_MOVE:
            // Make sure we're not moving to be minimized - because otherwise
            // our ratio varialbes (g_dwXRatio and g_dwYRatio) will end up
            // being 0, and once we hit CheckBoundries it divides by 0.
            if (!IsIconic(hwnd))
            {

                g_rcSrc.left = 0;
                g_rcSrc.right = BMAPX;
                g_rcSrc.top = 0;
                g_rcSrc.bottom = BMAPY;
                GetClientRect(hwnd, &g_rcDst);
                g_dwXRatio = (g_rcDst.right - g_rcDst.left) * 1000 /
                             (g_rcSrc.right - g_rcSrc.left);
                g_dwYRatio = (g_rcDst.bottom - g_rcDst.top) * 1000 /
                             (g_rcSrc.bottom - g_rcSrc.top);
                ClientToScreen(hwnd, &p);
                g_rcDst.left = p.x;
                g_rcDst.top = p.y;
                g_rcDst.bottom += p.y;
                g_rcDst.right += p.x;
                CheckBoundries();
            }
            else
                // Else, hide the overlay... just in case we can't do
                // destination color keying, this will pull the overlay
                // off of the screen for the user.
                if (g_pDDSOverlay && g_pDDSPrimary)
                    hRet = g_pDDSOverlay->UpdateOverlay(NULL, g_pDDSPrimary, NULL, DDOVER_HIDE,
                                                        NULL);
            // Check to make sure our window exists before we tell it to
            // repaint. This will fail the first time (while the window is
            // being created).
            if (hwnd)
            {
                InvalidateRect(hwnd, NULL, FALSE);
                UpdateWindow(hwnd);
            }
            return 0L;

        case WM_SIZE:
            // Another check for the minimization action.  This check is
            // quicker though...
            if (wParam != SIZE_MINIMIZED)
            {
                GetClientRect(hwnd, &g_rcDst);
                ClientToScreen(hwnd, &p);
                g_rcDst.left = p.x;
                g_rcDst.top = p.y;
                g_rcDst.bottom += p.y;
                g_rcDst.right += p.x;
                g_rcSrc.left = 0;
                g_rcSrc.right = BMAPX;
                g_rcSrc.top = 0;
                g_rcSrc.bottom = BMAPY;
                // Here we multiply by 1000 to preserve 3 decimal places in the
                // division opperation (we picked 1000 to be on the same order
                // of magnitude as the stretch factor for easier comparisons)
                g_dwXRatio = (g_rcDst.right - g_rcDst.left) * 1000 /
                             (g_rcSrc.right - g_rcSrc.left);
                g_dwYRatio = (g_rcDst.bottom - g_rcDst.top) * 1000 /
                             (g_rcSrc.bottom - g_rcSrc.top);
                CheckBoundries();
            }
            return 0L;

        case WM_PAINT:
            BeginPaint(hwnd, &ps);
            // Check the primary surface to see if it's lost - if so you can
            // pretty much bet that the other surfaces are also lost - thus
            // restore EVERYTHING!  If we got our surfaces stolen by a full
            // screen app - then we'll destroy our primary - and won't be able
            // to initialize it again. When we get our next paint message (the
            // full screen app closed for example) we'll want to try to reinit
            // the surfaces again - that's why there is a check for
            // g_pDDSPrimary == NULL.  The other option, is that our program
            // went through this process, could init the primary again, but it
            // couldn't init the overlay, that's why there's a third check for
            // g_pDDSOverlay == NULL.  Make sure that the check for
            // !g_pDDSPrimary is BEFORE the IsLost call - that way if the
            // pointer is NULL (ie. !g_pDDSPrimary is TRUE) - the compiler
            // won't try to evaluate the IsLost function (which, since the
            // g_pDDSPrimary surface is NULL, would be bad...).
            if (!g_pDDSPrimary || (g_pDDSPrimary->IsLost() != DD_OK) ||
                (g_pDDSOverlay == NULL))
            {
                DestroyOverlay();
                DestroyPrimary();
                if (DDPrimaryInit())
                    if (DDOverlayInit())
                        if (!DrawOverlay())
                            DestroyOverlay();
            }
            // UpdateOverlay is how we put the overlay on the screen.
            if (g_pDDSOverlay && g_pDDSPrimary)
                hRet = g_pDDSOverlay->UpdateOverlay(&g_rcSrc, g_pDDSPrimary,
                                                    &g_rcDst, g_OverlayFlags,
                                                    &g_OverlayFX);
            EndPaint(hwnd, &ps);
            return 0L;

        case WM_DISPLAYCHANGE:
            // This not only checks for overlay support in the new video mode -
            // but gets the new caps for the new display settings.  That way we
            // have more accurate info about min/max stretch factors, color
            // keying, and it's credit history (just seeing if you're paying
            // attention!).
            if (CheckOverlaySupport() == FALSE)
                DisplayError("You have changed your adapter settings such that you no longer support this overlay.");
            return 0L;

        case WM_DESTROY:
            // It's important to destroy DD object BEFORE you destroy the
            // window - because it relies on the window handle.
            if (g_pClipper)
                g_pClipper->Release();
            DestroyOverlay();
            DestroyDDraw();
            // Now, shut down the window...
            PostQuitMessage(0);
            return 0L;
    }
    return DefWindowProc(hwnd, iMsg, wParam, lParam);
}




//-----------------------------------------------------------------------------
// Name: WinInit()
// Desc: Initialize Windows stuff
//-----------------------------------------------------------------------------
int
WinInit(HINSTANCE hInstance, int nCmdShow)
{

    static char                 szAppName[] = "DDOverlay";  // The name of our app
    WNDCLASSEX                  wndclass;  // Our app's windows class

    // Setup the windows class structure for our window
    wndclass.cbSize = sizeof(wndclass);
    wndclass.style = CS_HREDRAW | CS_VREDRAW;
    wndclass.lpfnWndProc = WndProc;
    wndclass.cbClsExtra = 0;
    wndclass.cbWndExtra = 0;
    wndclass.hInstance = hInstance;
    wndclass.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_DDOVERICO));
    wndclass.hCursor = LoadCursor(NULL, IDC_ARROW);
    // This is our color key color
    wndclass.hbrBackground = CreateSolidBrush(RGB(255, 0, 255));
    wndclass.lpszMenuName = NULL;
    wndclass.lpszClassName = szAppName;
    wndclass.hIconSm = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_DDOVERICO));

    // If Windows won't let us register a new window -
    //  then take our bat and go home!
    if (RegisterClassEx(&wndclass) == FALSE)
        return FALSE;

    // Setup the new window's physical parameters -
    //  and tell Windows to create it
    g_hAppWnd = CreateWindow(szAppName,  // Window class name
                             "DDOverlay Sample App",  // Window caption
                             WS_OVERLAPPEDWINDOW,  // Window style
                             0,         // Initial x pos
                             0,         // Initial y pos
                             BMAPX,     // Initial x size
                             BMAPY,     // Initial y size
                             NULL,      // parent window handle
                             NULL,      // window menu handle
                             hInstance, // program instance handle
                             NULL);     // Creation parameters
    // If Windows won't let us create the new window -
    //  well... we didn't want to anyway :) ...
    if (g_hAppWnd == NULL)
        return FALSE;

    // We're done - the window is good to go!
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: The entry point to our windows program
//-----------------------------------------------------------------------------
int WINAPI
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
        PSTR szCmdLine, int iCmdShow)
{
    MSG                         msg;  // The message recieved that we'll pass to WinProc

    if (WinInit(hInstance, iCmdShow) == FALSE)
    {
        DisplayError("Unable to initialize the program's window.");
        return FALSE;
    }

    if (DDInit() == FALSE)
    {
        DestroyDDraw();
        DisplayError("Unable to initialize the DirectDraw object");
        return FALSE;
    }

    // Check for overlay support from the hardware. Note: this must happen
    // after we have created an object with DirectDrawCreate.  This is the
    // primary reason why the overlay surface is created and destroyed appart
    // from the primary surface and DD object.  In order to make proper
    // evaluation of the hardware's stand on overlays (which by the way, is
    // normally quite optimistic) - we have to instantiate the DD object first.
    if (CheckOverlaySupport() == FALSE)
    {
        DestroyDDraw();
        DisplayError("Your display adapter and/or driver do not support overlays.");
        return FALSE;
    }

    // Setup the window to be of a size which corresponds to all limitations
    // of the hardware
    //   Note: The top & left coordinates are arbitrary, only the right hand
    //         coordinate is really important
    g_rcDst.top = 40;
    g_rcDst.left = 40;
    if (g_DDCaps.dwMinOverlayStretch)
    {
        if (g_DDCaps.dwAlignBoundaryDest)
            g_rcDst.right = (2 * GetSystemMetrics(SM_CXSIZEFRAME) + 40 + (BMAPX *
                             (g_DDCaps.dwMinOverlayStretch + 1)) / 1000) &
                             -(signed) g_DDCaps.dwAlignBoundaryDest;
        else
            g_rcDst.right = 2 * GetSystemMetrics(SM_CXSIZEFRAME) + 40 + (BMAPX *
                             (g_DDCaps.dwMinOverlayStretch + 1)) / 1000;
    }
    else
        g_rcDst.right = 2 * GetSystemMetrics(SM_CXSIZEFRAME) + 40 + BMAPX;

    // The bottom doesn't really matter - so just set it to something at least
    // the size of the bitmap just to attempt compatibility with non-shrinking
    // display adapters.
    g_rcDst.bottom = (int) (BMAPY * 1.5);

    SetWindowPos(g_hAppWnd, NULL, g_rcDst.left, g_rcDst.top, g_rcDst.right,
                 g_rcDst.bottom, WS_VISIBLE);
    if (DDOverlayInit() == FALSE)
    {
        DestroyOverlay();
        DestroyDDraw();
        DisplayError("Could not create the overlay surface.  The display card may not support the requested format, or may not have enough memory available.");
        return FALSE;
    }

    if (DrawOverlay() == FALSE)
    {
        DestroyOverlay();
        DestroyDDraw();
        DisplayError("Could not draw the bitmap on the overlay surface...");
        return FALSE;
    }

    ShowWindow(g_hAppWnd, SW_SHOW);

    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return msg.wParam;
}

