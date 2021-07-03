//-----------------------------------------------------------------------------
// File: switcher.cpp
//
// Desc: This sample demonstrates how to switch between Windowed and
//       Full-Screen exclusive DDraw cooperative levels.
//
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#define NAME    "Switcher"
#define TITLE   "DDraw Window to Full-Screen Switching Example"

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <stdio.h>
#include <stdarg.h>
#include <windows.h>
#include <ddraw.h>
#include "resource.h"

//-----------------------------------------------------------------------------
// Global Variables
//-----------------------------------------------------------------------------
HINSTANCE				g_hInstance;
LPDIRECTDRAW7           g_pDD         = NULL;	// DirectDraw object
LPDIRECTDRAWSURFACE7    g_pDDSPrimary = NULL;   // DirectDraw primary surface
LPDIRECTDRAWSURFACE7    g_pDDSBack    = NULL;   // DirectDraw back surface
RECT                    g_rcWindow;             // Saves the window size & pos.
RECT                    g_rcViewport;           // Pos. & size to blt from
RECT                    g_rcScreen;             // Screen pos. for blt
BOOL                    g_bActive     = FALSE;  // App is running/active
BOOL                    g_bReady      = FALSE;  // App is ready for updates
BOOL                    g_bWindowed   = TRUE;   // App is in windowed mode




//-----------------------------------------------------------------------------
// Name: ReleaseAllObjects()
// Desc: Release all DDraw objects we use
//-----------------------------------------------------------------------------
HRESULT
ReleaseAllObjects(HWND hWnd)
{
    if (g_pDD != NULL)
    {
        g_pDD->SetCooperativeLevel(hWnd, DDSCL_NORMAL);
        if (g_pDDSBack != NULL)
        {
            g_pDDSBack->Release();
            g_pDDSBack = NULL;
        }
        if (g_pDDSPrimary != NULL)
        {
            g_pDDSPrimary->Release();
            g_pDDSPrimary = NULL;
        }
    }
    return DD_OK;
}




//-----------------------------------------------------------------------------
// Name: InitFail()
// Desc: This function is called if an initialization function fails
//-----------------------------------------------------------------------------
HRESULT
InitFail(HWND hWnd, HRESULT hRet, LPCTSTR szError, ...)
{
    char            szBuff[128];
    va_list         vl;

    va_start(vl, szError);
    vsprintf(szBuff, szError, vl);
    ReleaseAllObjects(hWnd);
    MessageBox(hWnd, szBuff, TITLE, MB_OK);
    DestroyWindow(hWnd);
    va_end(vl);
    return hRet;
}




//-----------------------------------------------------------------------------
// Name: UpdateFrame()
// Desc: Blts and moves a bouncing ball, as well as displays helpful text
//-----------------------------------------------------------------------------
BOOL
UpdateFrame(HWND hWnd)
{
    DDBLTFX     ddbltfx;
    HDC         hDC;
    static int  x1 = 0,
                y1 = 0,
                x2 = 40,
                y2 = 40;
    HBRUSH      hOldBrush;
    HPEN        hOldPen;
    static int  xDir = +4,
                yDir = +4;

    // Use the blter to do a color fill to clear the back buffer
    ZeroMemory(&ddbltfx, sizeof(ddbltfx));
    ddbltfx.dwSize = sizeof(ddbltfx);
    ddbltfx.dwFillColor = 0;
    g_pDDSBack->Blt(NULL, NULL, NULL, DDBLT_COLORFILL | DDBLT_WAIT, &ddbltfx);

    if (g_pDDSBack->GetDC(&hDC) == DD_OK)
    {
        // Paint the bouncing ball
        SetBkColor(hDC, RGB(0, 0, 255));
        SetTextColor(hDC, RGB(255, 255, 0));
        hOldBrush = (HBRUSH )SelectObject(hDC, GetStockObject(LTGRAY_BRUSH));
        hOldPen = (HPEN )SelectObject(hDC, GetStockObject(WHITE_PEN));
        Ellipse(hDC, x1, y1, x2, y2);
        SelectObject(hDC, hOldPen);
        SelectObject(hDC, hOldBrush);

        // Move the bouncing ball and make it bounce
        x1 += xDir;
        x2 += xDir;
        if (x1 < 0)
        {
            x1 = 0;
            x2 = 40;
            xDir = -xDir;
        }
        if (x2 >= 640)
        {
            x1 = 640 - 1 - 40;
            x2 = 640 - 1;
            xDir = -xDir;
        }
        y1 += yDir;
        y2 += yDir;
        if (y1 < 0)
        {
            y1 = 0;
            y2 = 40;
            yDir = -yDir;
        }
        if (y2 >= 480)
        {
            y1 = 480 - 1 - 40;
            y2 = 480 - 1;
            yDir = -yDir;
        }

        // Display the proper text
        TextOut(hDC, 0, 0, "Press Escape to quit", 20);
        if (g_bWindowed)
            TextOut(hDC, 0, 20, "Press Alt-Enter to switch to Full-Screen mode", 45);
        else
            TextOut(hDC, 0, 20, "Press Alt-Enter to switch to Windowed mode", 42);

        g_pDDSBack->ReleaseDC(hDC);
    }
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: InitSurfaces()
// Desc: Create all the needed DDraw surfaces and set the coop level
//-----------------------------------------------------------------------------
static HRESULT
InitSurfaces(HWND hWnd)
{
    HRESULT		        hRet;
    DDSURFACEDESC2      ddsd;
    DDSCAPS2            ddscaps;
    LPDIRECTDRAWCLIPPER pClipper;

    if (g_bWindowed)
    {
        // Get normal windowed mode
        hRet = g_pDD->SetCooperativeLevel(hWnd, DDSCL_NORMAL);
        if (hRet != DD_OK)
            return InitFail(hWnd, hRet, "SetCooperativeLevel FAILED");

    	// Get the dimensions of the viewport and screen bounds
    	GetClientRect(hWnd, &g_rcViewport);
    	GetClientRect(hWnd, &g_rcScreen);
    	ClientToScreen(hWnd, (POINT*)&g_rcScreen.left);
    	ClientToScreen(hWnd, (POINT*)&g_rcScreen.right);

        // Create the primary surface
        ZeroMemory(&ddsd,sizeof(ddsd));
        ddsd.dwSize = sizeof(ddsd);
        ddsd.dwFlags = DDSD_CAPS;
        ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;
        hRet = g_pDD->CreateSurface(&ddsd, &g_pDDSPrimary, NULL);
        if (hRet != DD_OK)
            return InitFail(hWnd, hRet, "CreateSurface FAILED");

        // Create a clipper object since this is for a Windowed render
        hRet = g_pDD->CreateClipper(0, &pClipper, NULL);
        if (hRet != DD_OK)
            return InitFail(hWnd, hRet, "CreateClipper FAILED");

        // Associate the clipper with the window
        pClipper->SetHWnd(0, hWnd);
        g_pDDSPrimary->SetClipper(pClipper);
        pClipper->Release();
        pClipper = NULL;

        // Get the backbuffer. For fullscreen mode, the backbuffer was created
        // along with the primary, but windowed mode still needs to create one.
        ddsd.dwFlags        = DDSD_WIDTH | DDSD_HEIGHT | DDSD_CAPS;
        ddsd.dwWidth        = 640;
        ddsd.dwHeight       = 480;
        ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
        hRet = g_pDD->CreateSurface(&ddsd, &g_pDDSBack, NULL);
        if (hRet != DD_OK)
            return InitFail(hWnd, hRet, "CreateSurface2 FAILED");
    }
    else
    {
        // Get exclusive mode
        hRet = g_pDD->SetCooperativeLevel(hWnd, DDSCL_EXCLUSIVE |
                                                DDSCL_FULLSCREEN);
        if (hRet != DD_OK)
            return InitFail(hWnd, hRet, "SetCooperativeLevel FAILED");

        // Set the video mode to 640x480x8
        hRet = g_pDD->SetDisplayMode( 640, 480, 8, 0, 0);
        if (hRet != DD_OK)
            return InitFail(hWnd, hRet, "SetDisplayMode FAILED");

    	// Get the dimensions of the viewport and screen bounds
    	// Store the rectangle which contains the renderer
    	SetRect(&g_rcViewport, 0, 0, 640, 480 );
    	memcpy(&g_rcScreen, &g_rcViewport, sizeof(RECT) );

        // Create the primary surface with 1 back buffer
        ZeroMemory(&ddsd,sizeof(ddsd));
        ddsd.dwSize = sizeof(ddsd);
        ddsd.dwFlags = DDSD_CAPS |
                       DDSD_BACKBUFFERCOUNT;
        ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE |
                			  DDSCAPS_FLIP |
                			  DDSCAPS_COMPLEX;
        ddsd.dwBackBufferCount = 1;
        hRet = g_pDD->CreateSurface( &ddsd, &g_pDDSPrimary, NULL);
        if (hRet != DD_OK)
            return InitFail(hWnd, hRet, "CreateSurface FAILED");

        ZeroMemory(&ddscaps, sizeof(ddscaps));
        ddscaps.dwCaps = DDSCAPS_BACKBUFFER;
        hRet = g_pDDSPrimary->GetAttachedSurface(&ddscaps, &g_pDDSBack);
        if (hRet != DD_OK)
            return InitFail(hWnd, hRet, "GetAttachedSurface FAILED");
    }
    return DD_OK;
}




//-----------------------------------------------------------------------------
// Name: ChangeCoopLevel()
// Desc: Called when the user wants to toggle between Full-Screen & Windowed
//-----------------------------------------------------------------------------
HRESULT
ChangeCoopLevel(HWND hWnd )
{
    HRESULT hRet;

    // Release all objects that need to be re-created for the new device
    if (FAILED(hRet = ReleaseAllObjects(hWnd)))
        return InitFail(hWnd, hRet, "ReleaseAllObjects FAILED");

    // In case we're coming from a fullscreen mode, restore the window size
    if (g_bWindowed)
    {
        SetWindowPos(hWnd, HWND_NOTOPMOST, g_rcWindow.left, g_rcWindow.top,
                     (g_rcWindow.right - g_rcWindow.left), 
                     (g_rcWindow.bottom - g_rcWindow.top), SWP_SHOWWINDOW );
    }

    // Re-create the surfaces
    hRet = InitSurfaces(hWnd);
    return hRet;
}




//-----------------------------------------------------------------------------
// Name: AboutDlgProc()
// Desc: The About Dialog Box Procedure
//-----------------------------------------------------------------------------
LRESULT CALLBACK
AboutDlgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
        case WM_INITDIALOG:
            return TRUE;
        case WM_COMMAND:
            switch (wParam)
            {
                case IDOK:
                    EndDialog(hDlg, TRUE);
                    return TRUE;
            }
            break;
    }

    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: MainWndProc()
// Desc: The Main Window Procedure
//-----------------------------------------------------------------------------
LRESULT CALLBACK
MainWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    HRESULT         hRet;
    MINMAXINFO      *pMinMax;

    switch (msg)
    {
        case WM_ACTIVATE:
            // Pause if minimized
            g_bActive = !((BOOL)HIWORD(wParam));
            return 0L;

        case WM_COMMAND:
            // Handle all menu and accelerator commands 
            switch (LOWORD(wParam))
            {
                case IDM_ABOUT:
                    DialogBox(g_hInstance, "AboutDlg", hWnd, (DLGPROC)AboutDlgProc);
                    return 0L;

                case IDM_TOGGLEFULLSCREEN:
                    // Toggle the fullscreen/window mode
                    if (g_bActive && g_bReady)
                    {
                        g_bReady = FALSE;
                        if (g_bWindowed)
                            GetWindowRect(hWnd, &g_rcWindow);
                        g_bWindowed = !g_bWindowed;
                        ChangeCoopLevel(hWnd);
                        g_bReady = TRUE;
                    }
                    return 0L;

                case IDM_EXIT:
                    // Received key/menu command to exit app
            	    PostMessage(hWnd, WM_CLOSE, 0, 0);
                    return 0L;
            }
            break;

        case WM_DESTROY:
            // Clean up and close the app
            ReleaseAllObjects(hWnd);
            PostQuitMessage(0);
            return 0L;

        case WM_GETMINMAXINFO:
            // Fix the size of the window to 640x480 (client size)
            pMinMax = (MINMAXINFO *)lParam;
            pMinMax->ptMinTrackSize.x = 640+GetSystemMetrics(SM_CXSIZEFRAME)*2;
            pMinMax->ptMinTrackSize.y = 480+GetSystemMetrics(SM_CYSIZEFRAME)*2
                                           +GetSystemMetrics(SM_CYMENU);
            pMinMax->ptMaxTrackSize.x = pMinMax->ptMinTrackSize.x;
            pMinMax->ptMaxTrackSize.y = pMinMax->ptMinTrackSize.y;
            break;

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

        case WM_MOVE:
            // Retrieve the window position after a move
            if (g_bActive && g_bReady && g_bWindowed)
            {
                GetWindowRect(hWnd, &g_rcWindow);
            	GetClientRect(hWnd, &g_rcViewport);
            	GetClientRect(hWnd, &g_rcScreen);
            	ClientToScreen(hWnd, (POINT*)&g_rcScreen.left);
            	ClientToScreen(hWnd, (POINT*)&g_rcScreen.right);
            }
            break;

        case WM_PAINT:
            // Update the screen if we need to refresh
            if (g_bWindowed && g_bReady)
            {
                while (TRUE)
                {
                    // If we are in windowed mode, perform a blt.
                    hRet = g_pDDSPrimary->Blt(&g_rcScreen, g_pDDSBack,
                                              &g_rcViewport, DDBLT_WAIT,
                                              NULL);
                    if (hRet == DD_OK)
                        break;
                    if (hRet == DDERR_SURFACELOST)
                    {
                        hRet = g_pDDSPrimary->Restore();
                        if (hRet != DD_OK )
                        	break;
                    }
                    if (hRet != DDERR_WASSTILLDRAWING)
                        break;
                }
            }
            break;

        case WM_SETCURSOR:
            // Display the cursor in the window if windowed
            if (g_bActive && g_bReady && !g_bWindowed)
            {
                SetCursor(NULL);
                return TRUE;
            }
            break;

        case WM_SIZE:
            // Check to see if we are losing our window...
            if (SIZE_MAXHIDE==wParam || SIZE_MINIMIZED==wParam)
                g_bActive = FALSE;
            else
                g_bActive = TRUE;
            break;
    }
    return DefWindowProc(hWnd, msg, wParam, lParam);
}




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point to the program. Initializes everything and calls
//       UpdateFrame() when idle from the message pump.
//-----------------------------------------------------------------------------
int PASCAL
WinMain(HINSTANCE   hInstance,
        HINSTANCE   hPrevInstance,
        LPSTR       lpszCmdLine,
        int         nCmdShow)
{
    WNDCLASS	    wc;
    MSG			    msg;
    HWND            hWnd;
    HACCEL          hAccel;
    HRESULT         hRet;
    BOOL            page = FALSE;   // Which screen to render (Front or Back text)
    int             cx,cy;

    if (!hPrevInstance)
    {
        // Register the Window Class
        wc.lpszClassName = NAME;
        wc.lpfnWndProc = MainWndProc;
        wc.style = CS_VREDRAW | CS_HREDRAW;
        wc.hInstance = hInstance;
        wc.hIcon = LoadIcon( hInstance, MAKEINTRESOURCE(IDI_MAIN_ICON));
        wc.hCursor = LoadCursor(NULL, IDC_ARROW);
        wc.hbrBackground = (HBRUSH) (COLOR_WINDOW + 1);
        wc.lpszMenuName = MAKEINTRESOURCE(IDR_MENU);
        wc.cbClsExtra = 0;
        wc.cbWndExtra = 0;
        RegisterClass(&wc);
    }

    g_hInstance = hInstance;

    // Calculate the proper size for the window given a client of 640x480
    cx = 640+GetSystemMetrics(SM_CXSIZEFRAME)*2;
    cy = 480+GetSystemMetrics(SM_CYSIZEFRAME)*2+GetSystemMetrics(SM_CYMENU);
    // Create and Show the Main Window
    hWnd = CreateWindowEx(0,
                          NAME,
                          TITLE,
                          WS_OVERLAPPEDWINDOW,
                          CW_USEDEFAULT,
                          CW_USEDEFAULT,
  	                      cx,
                          cy,
                          NULL,
                          NULL,
                          hInstance,
                          NULL);
    if (hWnd == NULL)
    	return FALSE;
    ShowWindow(hWnd, nCmdShow);
    UpdateWindow(hWnd);

    // Save the window size/pos for switching modes
    GetWindowRect(hWnd, &g_rcWindow);

    // Load keyboard accelerators
    hAccel = LoadAccelerators(g_hInstance, MAKEINTRESOURCE(IDR_MAIN_ACCEL));

    // Create the main DirectDraw object
    hRet = DirectDrawCreateEx(NULL, (VOID**)&g_pDD, IID_IDirectDraw7, NULL);
    if (FAILED(hRet))
        return InitFail(hWnd, hRet, "DirectDrawCreateEx FAILED");

    // Initialize all the surfaces we need
    hRet = InitSurfaces(hWnd);
    if (FAILED(hRet))
    	return FALSE;

    g_bReady = TRUE;
    //-------------------------------------------------------------------------
    //                          The Message Pump
    //-------------------------------------------------------------------------
    while (TRUE)
    {
        if (PeekMessage(&msg, NULL, 0, 0, PM_NOREMOVE))
        {
            if (!GetMessage(&msg, NULL, 0, 0 ))
                break;
            // Translate and dispatch the message
            if (0 == TranslateAccelerator(hWnd, hAccel, &msg))
            {
                TranslateMessage(&msg); 
                DispatchMessage(&msg);
            }
        }
        else
            if (g_bActive && g_bReady)
            {
                //-------------------------------------------------------------
                //                    Idle processing
                //-------------------------------------------------------------
                // Update the background and flip every time the timer ticks
                UpdateFrame(hWnd);
                page = !page;
                while (TRUE)
                {
                    // If we are in windowed mode, perform a blt.
                    if (g_bWindowed)
                    {
                        hRet = g_pDDSPrimary->Blt(&g_rcScreen, g_pDDSBack,
                                                  &g_rcViewport, DDBLT_WAIT,
                                                  NULL);
                    }
                    else
                    {
                        // Else we are in fullscreen mode, so perform a flip.
                        hRet = g_pDDSPrimary->Flip( NULL, 0L );
                    }
                    if (hRet == DD_OK )
                        break;
                    if (hRet == DDERR_SURFACELOST )
                    {
                        hRet = g_pDDSPrimary->Restore();
                        if (hRet != DD_OK )
                        	break;
                    }
                    if (hRet != DDERR_WASSTILLDRAWING )
                        break;
                }
            }
            else
            {
                // Make sure we go to sleep if we have nothing else to do
                WaitMessage();
            }
    }
    // Release the main DDraw interface
    if (g_pDD)
        g_pDD->Release();

    return msg.wParam;
}

