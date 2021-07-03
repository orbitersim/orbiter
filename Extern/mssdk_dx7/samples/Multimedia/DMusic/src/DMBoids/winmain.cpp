/*
**-----------------------------------------------------------------------------
**  File:       WinMain.cpp
**  Purpose:    D3D sample showing DrawPrimitive functionality 
**  Notes:
**
**	There are 3 main C++ classes that make this sample work.
**
**		DDDrvMgr:	Enumerates all drivers, modes, devices, etc.
**		D3DWindow:	D3D Frameworks
**		D3DScene:	Sample Scene Renderer
**
**  Copyright (c) 1995-1999 by Microsoft, all rights reserved
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Include files
**-----------------------------------------------------------------------------
*/

// Note:  Define INITGUID in one and only one file!!!

#include "Common.h"
#include "Debug.h"
#include "WinMain.h"
#include "WinProc.h"
#include "DrvMgr.h"
#include "D3DWin.h"
#include "D3DScene.h"
#include "input.h"
#include "boids.h"
#include "music.h"



/*
**-----------------------------------------------------------------------------
**  Global Variables
**-----------------------------------------------------------------------------
*/

// Application variables
HINSTANCE	g_hMainInstance		= NULL;
HWND        g_hMainWindow       = NULL;
HACCEL      g_hMainAccel        = NULL;
HCURSOR		g_hMainCursor		= NULL;

LPCTSTR     g_szMainName        = TEXT ("Boids3");
LPCTSTR     g_szMainClass		= TEXT ("BoidsClass");
LPCTSTR     g_szMainTitle		= TEXT ("Boids3 Sample");
LPCTSTR     g_szPaused          = TEXT ("Paused");

INT         g_nExitCode         = 0L;

BoidMusic	g_Music;

//
// D3D Variables
//

// Driver Management (DD Drivers, modes, D3D devices)
DDDrvMgr	g_DrvMgr;

//	D3D framework (DD/D3D interfaces, surfaces, etc.)
LPD3DWindow		g_lpd3dWin;

//  The Sample Framework (Materials, lights, textures, rendering, etc.)
LPD3DScene		g_lpd3dScene;



/*
**-----------------------------------------------------------------------------
**  Function Prototypes
**-----------------------------------------------------------------------------
*/

HRESULT CreateD3DWindow (
	DWORD		dwStyleEx,			/* In:  Standard CreateWindowEx parameters */
	LPCTSTR		lpszClass, 
    LPCTSTR		lpszName, 
	DWORD		dwStyle,
	int			x, 
	int			y, 
	int			nWidth, 
	int			nHeight,
	HWND		hParent, 
	HMENU		hMenu,
	HINSTANCE	hInstance, 
	LPVOID		lpParam, 

	BOOL        fUseZBuffer,		/* In:  Use Z-buffer */
	HWND *		lphWnd,				/* Out:	Window handle */
	LPD3DWindow * lpd3dWnd);		/* Out: Created D3DWindow pointer */

HRESULT DestroyD3DWindow (void);



/*
**-----------------------------------------------------------------------------
**  Function Definitions
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Name:       InitMain
**  Purpose:	Initialize Application
**-----------------------------------------------------------------------------
*/

BOOL InitMain (void)
{
    DPF (DEBUG_DETAILS, TEXT("InitApp"));

    // Get hInstance handle
    g_hMainInstance = (HINSTANCE) GetModuleHandle (NULL);
    if (NULL == g_hMainInstance)
    {
		// Error - GetModule Handle 

		DPF (DEBUG_CRITICAL, TEXT("InitApp - GetModuleHandle() failed."));
        return FALSE;
    }

    // Check for Previous instance
    if (! CheckPreviousApp ())
    {
        return FALSE;
    }

    // Init main window
    if (! InitMainWindow ())
    {
        return FALSE;
    }

	if (!g_Music.LoadMusic(g_hMainWindow))
    {
        OutputDebugString("Failed to initialize DirectMusic.\n");
        return FALSE;
    }

	// init dinput
	if (!DI_Init()) 
	{
		OutputDebugString("InitApp - DirectInput init failed.\n");
        return FALSE;
    }    

    DPF (DEBUG_DETAILS, TEXT("InitApp - Success"));

    // Success
    return TRUE;
} // End InitMain



/*
**-----------------------------------------------------------------------------
**  Name:       FiniMain
**  Purpose:	Cleanup Application
**-----------------------------------------------------------------------------
*/

void FiniMain (void)
{
	DI_Term();
	g_Music.EndMusic();
	DestroyD3DWindow ();

    UnregisterClass (g_szMainClass, g_hMainInstance);
} // End FiniMain
  

  
/*
**-----------------------------------------------------------------------------
**  Name:       CheckPreviousApp
**  Purpose:	Check for previous instance of this application
**-----------------------------------------------------------------------------
*/

BOOL CheckPreviousApp (void)
{
    HWND hwndFind;
    HWND hwndLast;
    HWND hwndForeGround;
    DWORD dwFindID;
    DWORD dwForeGroundID;

    // Check if application is already running
    hwndFind = FindWindow (g_szMainClass, g_szMainTitle);
    if (hwndFind)
    {
        // Bring previously running application's main
        // window to the user's attention
        hwndForeGround = GetForegroundWindow ();
        dwForeGroundID = GetWindowThreadProcessId (hwndForeGround, NULL);
        dwFindID = GetWindowThreadProcessId (hwndFind, NULL);

        // Don't do anything if window is already in foreground
        // Unless it is iconized.
        if ((dwFindID != dwForeGroundID) || (IsIconic (hwndFind)))
        {
            hwndLast = GetLastActivePopup (hwndFind);
            if (IsIconic (hwndLast))
            {
                ShowWindow (hwndLast, SW_RESTORE);
            }

            BringWindowToTop (hwndLast);
            SetForegroundWindow (hwndLast);
        }

        // Prevent additional instance's of this application
        // from running

        DPF (DEBUG_WARN, TEXT("CheckPrevious App - Another instance of this app is already running."));
        return FALSE;
    }

    return TRUE;
} // End CheckPreviousApp
  
  

/*
**-----------------------------------------------------------------------------
**  Name:       RunMain
**  Purpose:    Main Message Pump
**-----------------------------------------------------------------------------
*/

void RunMain (void)
{
    MSG msg;

    DPF (DEBUG_DETAILS, TEXT("RunMain - Enter"));

    if ((! g_hMainInstance) || (! g_hMainWindow))
    {
        return;
    }
    g_hMainAccel = LoadAccelerators (g_hMainInstance, MAKEINTRESOURCE (IDR_MAIN_ACCEL));
	g_Music.StartMusic();
  
    //
    //  The Main Message loop
    //
    //  Note:  In order to catch Idle behavior we use PeekMessage instead of GetMessage
    //
    while (TRUE)
    {
        while (TRUE)
        {

            if (! PeekMessage (&msg, NULL, 0, 0, PM_REMOVE))
            {
                // App is Idle
                break;
            }

            // Exit App ?!?
            if (msg.message == WM_QUIT)
            {
                DPF (DEBUG_DETAILS, TEXT("RunMain - Exit"));
                g_nExitCode = msg.wParam;
                return;
            }

            if (g_hMainAccel)
            {
                if (! TranslateAccelerator (g_hMainWindow, g_hMainAccel, &msg))
                {
                    TranslateMessage (&msg);
                    DispatchMessage (&msg);
                }
            }
            else
            {
                TranslateMessage (&msg);
                DispatchMessage (&msg);
            }
        }

        // Do some Idle processing
		ProcessKBInput();			// grab any keyboard input
        OnIdle (g_hMainWindow);
		g_Music.HandleNotifies();
    } // End While

} // End RunMain


  
/*
**-----------------------------------------------------------------------------
**  Name:       WinMain
**  Purpose:    Application EntryPoint
**-----------------------------------------------------------------------------
*/

INT WINAPI WinMain(
    HINSTANCE   hInstance,	    // handle to current instance
    HINSTANCE   hPrevInstance,	// handle to previous instance
    LPSTR       lpCmdLine,	    // pointer to command line
    INT         nShowCmd) 	    // show state of window
{
    g_nExitCode = 0;
    
    DPF (DEBUG_DETAILS, TEXT("WinMain - Enter"));

    //  Initialize App
    if (! InitMain ())
        return g_nExitCode;

    //  Main Message Loop
    RunMain ();

    //  Cleanup App
    FiniMain ();

    //  Success
    DPF (DEBUG_DETAILS, TEXT("WinMain - Exit, status = %08lX"), (DWORD)g_nExitCode);
    return g_nExitCode;
} // End WinMain



 /*
**-----------------------------------------------------------------------------
**  Name:       InitMainWindow
**  Purpose:    
**-----------------------------------------------------------------------------
*/

BOOL InitMainWindow (void)
{
	HRESULT		hResult;
    WNDCLASS    wc;
    HWND        hWnd;
	DWORD		dwStyle, dwStyleEx;

    DPF (DEBUG_DETAILS, TEXT("InitMainWindow - Enter"));

	g_hMainCursor = LoadCursor ((HINSTANCE)NULL, IDC_ARROW);

    // Register Main Window Class
    wc.style            = CS_DBLCLKS; 
    wc.lpfnWndProc      = (WNDPROC)D3DWindowProc; 
    wc.cbClsExtra       = 0; 
    wc.cbWndExtra       = 4;  // Reserve space for lpd3dWindow pointer
    wc.hInstance        = g_hMainInstance; 
    wc.hIcon            = LoadIcon (g_hMainInstance, MAKEINTRESOURCE (IDI_MAIN_ICON));
    wc.hCursor          = g_hMainCursor;
    wc.hbrBackground    = (HBRUSH)GetStockObject (WHITE_BRUSH);
    wc.lpszMenuName     = MAKEINTRESOURCE (IDR_MAIN_MENU); 
    wc.lpszClassName    = g_szMainClass; 

    if (! RegisterClass (&wc))
    {
        // Error - Register class failed
        DPF (DEBUG_CRITICAL, TEXT("InitMainWindow - RegisterClass failed."));
        return FALSE;
    }


	// We want a popup window
	dwStyleEx = WS_EX_APPWINDOW;
	dwStyle   = WS_POPUP | WS_SYSMENU | WS_CAPTION | WS_MINIMIZEBOX;

	//
    // Create Main Window
	//
	hResult = CreateD3DWindow (dwStyleEx, 
							   g_szMainClass,
							   g_szMainTitle,
							   dwStyle,
							   0, 0, 640, 480,
							   NULL,
							   NULL,
							   g_hMainInstance,
							   NULL,
							   TRUE,	// Use Z-Buffer
							   &hWnd,
							   &g_lpd3dWin);
	if (FAILED (hResult))
    {
        // Error - CreateWindow failed
        DPF (DEBUG_CRITICAL, TEXT("InitMainWindow - CreateWindow failed."));
        return FALSE;
    }

	//
	// Create our D3D scene
	//

	g_lpd3dScene = new D3DScene;
	if (! g_lpd3dScene)
	{
        // Error - Construct D3D Scene failed
        DPF (DEBUG_CRITICAL, TEXT("InitMainWindow - Not enough memory for D3D scene."));
        return FALSE;
	}

	// Attach our D3DScene to the Window
	hResult = g_lpd3dWin->AttachScene (g_lpd3dScene);
	if (FAILED (hResult))
	{
        // Error - Attaching our D3D Scene to our D3D Window failed
        DPF (DEBUG_CRITICAL, TEXT("InitMainWindow - AttachScene failed."));
        return FALSE;
	}

    g_hMainWindow = hWnd;

    // Success
    DPF (DEBUG_DETAILS, TEXT("InitMainWindow - Success"));
    return TRUE;
} // End InitMainWindow

  
/*
**-----------------------------------------------------------------------------
** Name:    CreateD3DWindow
** Purpose: Creates a simple D3D window 
** Notes:   
**-----------------------------------------------------------------------------
*/

HRESULT CreateD3DWindow (
	DWORD		dwStyleEx,			/* In:  CreateWindowEx parameters */
	LPCTSTR		lpszClass, 
    LPCTSTR		lpszName, 
	DWORD		dwStyle,
	int			x, 
	int			y, 
	int			nWidth, 
	int			nHeight,
	HWND		hParent, 
	HMENU		hMenu,
	HINSTANCE	hInstance, 
	LPVOID		lpParam, 

	BOOL        fUseZBuffer,		/* In:  Use Z-buffer */
	HWND *		lphWnd,				/* Out:	Window handle */
	LPD3DWindow * lpd3dWnd)			/* Out: Created D3DWindow pointer */
{
    HRESULT             hResult;
    HWND                hWnd;
    LPD3DWindow         lpWnd;

	// Check Parameters
	if (! lpszClass)
	{
		REPORTERR (DDERR_INVALIDPARAMS);
		return DDERR_INVALIDPARAMS;
	}

	// Fill in defaults, if missing
	if (! hInstance)
		hInstance = GetModuleHandle (NULL);

	if (! hParent)
		hParent = GetDesktopWindow ();

    // Create D3D Window 
    lpWnd = new D3DWindow ();
    if (! lpWnd)
    {
		// Error, not enough memory
		REPORTERR (DDERR_OUTOFMEMORY);
        return DDERR_OUTOFMEMORY;
    }

    // Create Extended Window
    hWnd = CreateWindowEx (
                dwStyleEx,				// extended window style
                lpszClass,				// window class
                lpszName,			    // window name (title)
                dwStyle,				// window style
                x,						// horizontal position of window
                y,						// vertical position of window
                nWidth,					// window width
                nHeight,	            // window height
                hParent,				// handle to parent window
                hMenu,                  // handle to menu
                hInstance,              // handle to app instance
                lpParam);               // Extra Data
    if (! hWnd)
    {
        // Error, unable to create window
        delete lpWnd;
		REPORTERR (DDERR_GENERIC);
        return DDERR_GENERIC;
    }

    //
    //  Gotcha:  You must Show the window before you change to Fullscreen
    //           Otherwise the following can happen.
    //		   1. SetCooperativeLevel (DDSCL_EXCLUSIVE) works
    //             2. SetDisplayMode (w,h,bpp) loses the exclusive mode state
    //                because the window is not fully initialized.
    //                (DDraw gets confused by the responses it gets from the window)
    //             3. CreateSurface (PRIMARY | FLIP) fails with DDERR_NOEXCLUSIVEMODE.
    //                As the exlusive mode state has been lost
    //

    // Show the Window and Paint it's contents
    ShowWindow (hWnd, SW_SHOWDEFAULT);
    UpdateWindow (hWnd);

    // Create D3DWindow from hWnd
    hResult = lpWnd->Create (hWnd);
    if (FAILED (hResult))
    {
        // Error, unable to create window
        delete lpWnd;
        DestroyWindow (hWnd);
        return hResult;
    }

    // Return results
    if (lphWnd)
        *lphWnd = hWnd;
    if (lpd3dWnd)
        *lpd3dWnd = lpWnd;

    // Success
    return DD_OK;
} // End CreateD3DWindow



/*
**-----------------------------------------------------------------------------
** Name:    DestroyD3DWindow
** Purpose: Destroys a D3DWindow (window and D3DWindow object)
**-----------------------------------------------------------------------------
*/

HRESULT DestroyD3DWindow (void)
{
	// Check Parameters
    if ((g_hMainWindow) && (IsWindow (g_hMainWindow)))
	{
		DestroyWindow (g_hMainWindow);
		g_hMainWindow = NULL;
	}

    // Free memory associated with D3DWindow Object
    if (g_lpd3dWin)
	{
        delete g_lpd3dWin;
		g_lpd3dWin = NULL;
	}

	// Sucess
    return DD_OK;
} // End DestroyD3DWindow

  
/*
**-----------------------------------------------------------------------------
**  End of File
**-----------------------------------------------------------------------------
*/


