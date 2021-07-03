//-----------------------------------------------------------------------------
// File: FSWindow
//
// Desc: This code will allow you to update a window in DirectDraw full-screen
//       exclusive mode on a device that doesn't support GDI.  They will also
//       handle devices that do support GDI.
//
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
//-----------------------------------------------------------------------------
// Include files
//-----------------------------------------------------------------------------
#include <windows.h>
#include <ddraw.h>
#include "FSWindow.h"

//-----------------------------------------------------------------------------
// Local definitions
//-----------------------------------------------------------------------------
static IDirectDraw7        *ddObject = NULL;
static IDirectDrawSurface7 *ddFrontBuffer = NULL;
static IDirectDrawSurface7 *ddBackBuffer = NULL;
static IDirectDrawClipper  *ddClipper = NULL;
static SIZE                 structSurfaceSize = {0, 0};
static BOOL                 bNonGDI = FALSE;
static HWND                 hwndFSWindow = NULL;
static HBITMAP              hwndFSWindowBMP = NULL;
static HWND                 hwndAppWindow = NULL;




//-----------------------------------------------------------------------------
// Name: CreateDibBMP()
// Desc: Created an empty bitmap, used exclusively in CreateBMPFromWindow().
//       Note that this is an internal (not exported) function.
//-----------------------------------------------------------------------------
static HBITMAP 
CreateDibBMP(HDC hdc, int w, int h, unsigned short bpp)
{
    LPVOID                      lpBits;
    struct
    {
        BITMAPINFOHEADER        bi;
        DWORD                   ct[256];
    } dib;

    dib.bi.biSize = sizeof(BITMAPINFOHEADER);
    dib.bi.biWidth = w;
    dib.bi.biHeight = h;
    dib.bi.biBitCount = bpp;
    dib.bi.biPlanes = 1;
    dib.bi.biCompression = 0;
    dib.bi.biSizeImage = 0;
    dib.bi.biClrUsed = 0;

    if (bpp == 15)
    {
        dib.bi.biBitCount = 16;
    }
    else
        if (bpp == 16)
        {
            dib.bi.biCompression = BI_BITFIELDS;
            dib.ct[0] = 0xF800;
            dib.ct[1] = 0x07E0;
            dib.ct[2] = 0x001F;
        }

    return CreateDIBSection(hdc, (LPBITMAPINFO) & dib, DIB_RGB_COLORS, &lpBits,
                            NULL, 0);
}




//-----------------------------------------------------------------------------
// Name: CreateBMPFromWindow()
// Desc: Takes the hwnd of the content window, and returns a bitmap handle.
//       Note that this is an internal (not exported) function.
//-----------------------------------------------------------------------------
static HBITMAP 
CreateBMPFromWindow(HWND hwnd)
{
    RECT                        rc;
    int                         x;
    int                         y;
    int                         cx;
    int                         cy;
    HDC                         hdcScreen;
    HDC                         hdcMemory;
    HBITMAP                     hbmBitmap;

    // Create a bitmap of the window passed in
    GetWindowRect(hwnd, &rc);
    x = rc.left;
    y = rc.top;
    cx = rc.right - rc.left;
    cy = rc.bottom - rc.top;
    hdcScreen = GetDC(NULL);
    hdcMemory = CreateCompatibleDC(NULL);
    hbmBitmap = CreateDibBMP(hdcScreen, cx, cy, 16);

    // BLT the image from screen to bitmap
    SelectObject(hdcMemory, hbmBitmap);
    BitBlt(hdcMemory, 0, 0, cx, cy, hdcScreen, x, y, SRCCOPY);
    DeleteDC(hdcMemory);
    ReleaseDC(NULL, hdcScreen);

    return hbmBitmap;
}





//-----------------------------------------------------------------------------
// Name: FSWindow_Init()
// Desc: Does preliminary setup of global values for FSWindow. It should get
//       called each time DirectDraw surfaces are altered (i.e. changes to the
//       device that the client application is running under).
//-----------------------------------------------------------------------------
void 
FSWindow_Init(HWND hwndApp, IDirectDraw7 *dd, IDirectDrawSurface7 *FrontBuffer,
              IDirectDrawSurface7 *BackBuffer)
{
    DDSURFACEDESC2              ddsd;
    DDCAPS                      ddcaps;

    // Save handle to application window
    hwndAppWindow = hwndApp;

    ZeroMemory(&ddcaps, sizeof(ddcaps));
    ddcaps.dwSize = sizeof(ddcaps);
    dd->GetCaps(&ddcaps, NULL);
    if (ddcaps.dwCaps2 & DDCAPS2_CANRENDERWINDOWED)
        bNonGDI = FALSE;
    else
        bNonGDI = TRUE;

    // Save DirectDraw object passed in
    ddObject = dd;

    // Save buffers passed in
    ddFrontBuffer = FrontBuffer;
    ddBackBuffer = BackBuffer;

    // Get DirectDraw surface dimensions
    ZeroMemory(&ddsd, sizeof(ddsd));
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;
    ddBackBuffer->GetSurfaceDesc(&ddsd);
    structSurfaceSize.cx = ddsd.dwWidth;
    structSurfaceSize.cy = ddsd.dwHeight;
}




//-----------------------------------------------------------------------------
// Name: FSWindow_Begin()
// Desc: Prepairs the DirectDraw surface depending on 3D hardware. It should
//       get called whenever a window (represented by the hwnd parameter) needs
//       to be displayed under DirectDraw. FSWindow_Begin() should also get
//       called if the window changes its content (if its static content
//       becomes dynamic, and vice-versa).
//-----------------------------------------------------------------------------
HWND 
FSWindow_Begin(HWND hwnd, BOOL bStaticContent)
{
    RECT                        rc;

    // If no handle passed in, assume existing content window
    if (hwnd == NULL)
        hwnd = hwndFSWindow;

    if (hwnd == NULL)
        return NULL;

    if (bNonGDI)
    {
        // Constrain cursor to DirectDraw surface
        rc.left =0;
        rc.top = 0;
        rc.right = structSurfaceSize.cx;
        rc.bottom = structSurfaceSize.cy;
        ClipCursor(&rc);

        // Clear out lingering content
        if (hwndFSWindowBMP)
            DeleteObject(hwndFSWindowBMP);

        hwndFSWindowBMP = NULL;

        // Need to create an image of content window just once
        if (bStaticContent)
        {
            if (!FSWindow_IsActive())
                UpdateWindow(hwnd);

            // Assign content window image to global
            hwndFSWindowBMP = CreateBMPFromWindow(hwnd);
        }
    }
    else
    {
        // Create a clipper (used in IDirectDrawSurface::Blt call)
        if (ddObject->CreateClipper(0, &ddClipper, NULL) == DD_OK)
            ddClipper->SetHWnd(0, hwndAppWindow);

        // Normal GDI device, so just flip to GDI so content window can be seen
        ddObject->FlipToGDISurface();
    }
    hwndFSWindow = hwnd;
    return hwndFSWindow;
}




//-----------------------------------------------------------------------------
// Name: FSWindow_End()
// Desc: Deletes objects associated with the content window. Note that these
//       are objects created within this module, not objects created by the
//       calling client (e.g. content window). Call this function whenever the
//       content window is destroyed (e.g. WM_CLOSE).
//-----------------------------------------------------------------------------
void 
FSWindow_End()
{
    if (hwndFSWindow)
        hwndFSWindow = NULL;

    if (hwndFSWindowBMP)
        DeleteObject(hwndFSWindowBMP);

    hwndFSWindowBMP = NULL;

    if (bNonGDI)
        ClipCursor(NULL);

    // Get rid of clipper object
    if (ddClipper)
    {
        ddClipper->Release();
        ddClipper = NULL;
    }
}




//-----------------------------------------------------------------------------
// Name: FSWindow_Update()
// Desc: Is responsible for the actual rendering of the content window
//       (held in global hwndFSWindow). This function must be called each
//       time a DirectDraw frame gets rendered and FSWindow_IsActive() returns
//       TRUE, so it should be placed in the main application's DirectDraw
//       rendering routine. An example of this might look like the following:
//
//  void RenderFrame()
//  {
//      if (FSWindow_IsActive())
//          FSWindow_Update();
//      else
//          FrontBuffer->Blt(...);
//  }
//-----------------------------------------------------------------------------
void 
FSWindow_Update()
{
    POINT                           pt;
    RECT                            rc;
    int                             x;
    int                             y;
    int                             cx;
    int                             cy;
    HDC                             hdcScreen;
    HDC                             hdcBackBuffer;
    HRGN                            hrgn;
    HDC                             hdcMemory;
    static HCURSOR                  MouseCursor;
    static ICONINFO                 IconInfo;
    HCURSOR                         MouseCursorCur;

    if (bNonGDI)
    {
        GetWindowRect(hwndFSWindow, &rc);
        x = rc.left;
        y = rc.top;
        cx = rc.right - rc.left;
        cy = rc.bottom - rc.top;
        // Get a DC to the screen (where our window is) and
        // Get a DC to the backbuffer on the non-GDI device (where we need to copy it)
        hdcScreen = GetDC(NULL);
        ddBackBuffer->GetDC(&hdcBackBuffer);

        // If window has a complex region associated with it, be sure to include it in the draw
        hrgn = CreateRectRgn(0, 0, 0, 0);
        if (GetWindowRgn(hwndFSWindow, hrgn) == COMPLEXREGION)
        {
            OffsetRgn(hrgn, rc.left, rc.top);
            SelectClipRgn(hdcBackBuffer, hrgn);
        }

        // If content window is static (no animations, roll-overs, etc.) then
        // create a dc for the bitmap and blt to the back buffer
        if (FSWindow_IsStatic())
        {
            hdcMemory = CreateCompatibleDC(NULL);
            SelectObject(hdcMemory, hwndFSWindowBMP);
            BitBlt(hdcBackBuffer, x, y, cx, cy, hdcMemory, 0, 0, SRCCOPY);
            DeleteDC(hdcMemory);
        }
        else
        {
// Special case for potentially quirky non-GDI drivers
#if 0
            // If content is dynamic (updated each frame), always grab the screen copy
            // by calling CreateBMPFromWindow to update image held in Bitmap
            HDC                         hdcMemory = CreateCompatibleDC(NULL);
            HBITMAP                     Bitmap = CreateBMPFromWindow(hwndFSWindow);
            SelectObject(hdcMemory, Bitmap);
            BitBlt(hdcBackBuffer, x, y, cx, cy, hdcMemory, 0, 0, SRCCOPY);
            DeleteDC(hdcMemory);
            DeleteObject(Bitmap);
#else
            // Do a blt directly from the windows screen to the backbuffer
            BitBlt(hdcBackBuffer, x, y, cx, cy, hdcScreen, x, y, SRCCOPY);
#endif
        }

        // Remove clipping region and clean up
        SelectClipRgn(hdcBackBuffer, NULL);
        DeleteObject(hrgn);

        // Now draw the mouse on the backbuffer
        MouseCursorCur = GetCursor();
        if (MouseCursorCur != MouseCursor)
        {
            MouseCursor = MouseCursorCur;
            GetIconInfo(MouseCursor, &IconInfo);

            if (IconInfo.hbmMask)
                DeleteObject(IconInfo.hbmMask);

            if (IconInfo.hbmColor)
                DeleteObject(IconInfo.hbmColor);
        }

        GetCursorPos(&pt);
        pt.x -= IconInfo.xHotspot;
        pt.y -= IconInfo.yHotspot;
        DrawIcon(hdcBackBuffer, pt.x, pt.y, MouseCursor);

        ddBackBuffer->ReleaseDC(hdcBackBuffer);
        ReleaseDC(NULL, hdcScreen);

        ddFrontBuffer->Flip(NULL, DDFLIP_WAIT);
    }
    else
    {
        // GDI hardware

        // Update the surface with a blt
        ddFrontBuffer->SetClipper(ddClipper);
        ddFrontBuffer->Blt(NULL, ddBackBuffer, NULL, DDBLT_WAIT, NULL);
    }
}




//-----------------------------------------------------------------------------
// Name: FSWindow_IsActive()
// Desc: Simply checks to see if there's a content window displayed. This check
//       should be made prior to calling FSWindow_Update().
//-----------------------------------------------------------------------------
BOOL 
FSWindow_IsActive(void)
{
    return (hwndFSWindow != NULL);
}




//-----------------------------------------------------------------------------
// Name: FSWindow_IsStatic()
// Desc: Checks to see whether or not the content window needs to be regularly 
//       updated (its content is dynamic, such as an animation or text entry
//       field). A static window is created (an image of the window created
//       with a call to CreateBMPFromWindow()) once and used over and over.
//-----------------------------------------------------------------------------
BOOL 
FSWindow_IsStatic(void)
{
    return (hwndFSWindowBMP != NULL);
}

