/*
 *  Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File: rmfull.cpp
 *
 *  Each of the Direct3D retained mode (D3DRM) samples may be linked with
 *  this file.  It contains the code which allows them to run in the Windows
 *  environment as a window or fullscreen.  It is a modified version of
 *  d3dmain.cpp.  Comparing these two files is instructive.
 *
 *  A window is created using rmfull.res which allows the user to select the
 *  Direct3D driver to use and change the render options.  The D3DApp
 *  collection of functions is used to initialize DirectDraw, Direct3D and
 *  keep surfaces and D3D devices available for rendering.
 *
 *  Frame rate and a screen mode information buffer is Blt'ed to the screen
 *  by functions in rmstats.cpp.
 *
 *  Individual samples are executed through two functions, BuildScene and
 *  OverrideDefaults. Samples can also read mouse input via ReadMouse.
 */

#define INITGUID
#include "rmfull.h"

/*
 * Functions to build the customize the app for each sample
 */
BOOL BuildScene(LPDIRECT3DRM3 pD3DRM,
                LPDIRECT3DRMDEVICE3 dev, LPDIRECT3DRMVIEWPORT2 view,
                LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 camera);

void OverrideDefaults(BOOL* bNoTextures, BOOL* pbResizingDisabled,
                      BOOL* pbConstRenderQuality, CHAR** pstrName);

/*
 * GLOBAL VARIABLES
 */
D3DAppInfo* d3dapp;         /* Pointer to read only collection of DD and D3D
                               objects maintained by D3DApp */
rmfullglobals myglobs;      /* collection of global variables */
LPDIRECT3DRM3 lpD3DRM;      /* Direct3DRM object */

/*
 *  INTERNAL FUNCTION PROTOTYPES
 */
static BOOL AppInit(HINSTANCE hInstance, LPSTR lpCmdLine);
static BOOL CreateD3DApp(LPSTR lpCmdLine);
static BOOL BeforeDeviceDestroyed(LPVOID lpContext);
static BOOL AfterDeviceCreated(int w, int h, LPDIRECT3DVIEWPORT* lpViewport,
                               LPVOID lpContext);
void CleanUpAndPostQuit(void);
static void InitGlobals(void);
static BOOL AppPause(BOOL f);
void ReportD3DAppError(void);
static BOOL RenderLoop(void);
static BOOL RestoreSurfaces();
long FAR PASCAL WindowProc(HWND hWnd, UINT message, WPARAM wParam,
                           LPARAM lParam );
void ReadMouse(int*, int*, int*);
char* D3DRMErrorToString(HRESULT error);
BOOL CreateD3DRM(HWND win);
BOOL SetRenderState(void);

/****************************************************************************/
/*                            WinMain                                       */
/****************************************************************************/
/*
 * Initializes the application then enters a message loop which calls sample's
 * RenderScene until a quit message is received.
 */
int PASCAL
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine,
        int nCmdShow)
{
    int failcount = 0; /* number of times RenderLoop has failed */
    MSG msg;
    HACCEL hAccelApp;

    hPrevInstance;
    /*
     * Create the window and initialize all objects needed to begin rendering
     */
    if(!AppInit(hInstance, lpCmdLine))
        return FALSE;
    hAccelApp = LoadAccelerators(hInstance, "AppAccel");

    while (!myglobs.bQuit) {
        /*
         * Monitor the message queue until there are no pressing
         * messages
         */
        if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
            if (msg.message == WM_QUIT) {
                CleanUpAndPostQuit();
                break;
            }
            if (!myglobs.hWndMain || !TranslateAccelerator(myglobs.hWndMain,
                                                           hAccelApp, &msg)) {
                TranslateMessage(&msg);
                DispatchMessage(&msg);
            }
        /*
         * If the app is not minimized, not about to quit, not paused, either the
         * active fullscreen app or in a window and D3D has been initialized,
         * we can render
         */
        } else if (d3dapp->bRenderingIsOK && !d3dapp->bMinimized
                   && !d3dapp->bPaused && !myglobs.bQuit
                   && (d3dapp->bAppActive || !d3dapp->bFullscreen)) {
            /*
             * If were are not in single step mode or if we are and the
             * bDrawAFrame flag is set, render one frame
             */
            if (!(myglobs.bSingleStepMode && !myglobs.bDrawAFrame)) {
                /*
                 * Attempt to render a frame, if it fails, take a note.  If
                 * rendering fails more than twice, abort execution.
                 */
                if (!RenderLoop()) {
                    ++failcount;
                    if (failcount == 3) {
                        Msg("Rendering has failed too many times.  Aborting execution.\n");
                        CleanUpAndPostQuit();
                        break;
                    }
                }
            }
            /*
             * Reset the bDrawAFrame flag if we are in single step mode
             */
            if (myglobs.bSingleStepMode)
                myglobs.bDrawAFrame = FALSE;
        } else {
            WaitMessage();
        }
    }
    DestroyWindow(myglobs.hWndMain);
    return msg.wParam;
}


//-----------------------------------------------------------------------------
// Name: AddMediaPath()
// Desc: Looks in the system registry to determine the media path for the
//       sample. Then, it adds that path to the string passed in, checks if the
//       file exists, and returns a path to the file.
//-----------------------------------------------------------------------------
VOID AddMediaPath( LPDIRECT3DRM3 pD3DRM )
{
	HKEY   key;
	LONG   result;
	TCHAR  strPath[512];
	DWORD  type, size = 512;

	// Open the registry
	result = RegOpenKeyEx( HKEY_LOCAL_MACHINE, "Software\\Microsoft\\DirectX",
						   0, KEY_READ, &key );
	if( ERROR_SUCCESS != result )
		return;

	// Search for the desired registry value, and close the registry
        result = RegQueryValueEx( key, "DXSDK Samples Path", NULL, &type, 
		                      (BYTE*)strPath, &size );
	RegCloseKey( key );

	if( ERROR_SUCCESS != result )
		return;

	strcat( strPath, "\\D3DRM\\Media" );

	pD3DRM->AddSearchPath( strPath );

	return;
}

/****************************************************************************/
/*             D3DApp Initialization and callback functions                 */
/****************************************************************************/
/*
 * AppInit
 * Creates the window and initializes all objects necessary to begin rendering
 */
static BOOL
AppInit(HINSTANCE hInstance, LPSTR lpCmdLine)
{
    WNDCLASS wc;
    HMENU hmenu;
    DWORD flags;
    int i;

    /*
     * Register the window class
     */
    wc.style = CS_HREDRAW | CS_VREDRAW;
    wc.lpfnWndProc = WindowProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hInstance;
    wc.hIcon = LoadIcon( hInstance, "AppIcon");
    wc.hCursor = LoadCursor( NULL, IDC_ARROW );
    wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
    wc.lpszMenuName = "AppMenu";
    wc.lpszClassName = "Example";
    if (!RegisterClass(&wc))
        return FALSE;
    /*
     * Initialize the global variables and allow the sample code to override
     * some of these default settings.
     */
    InitGlobals();
    myglobs.hInstApp = hInstance;
    BOOL bResizingDisabled = FALSE;
    CHAR *szName = "D3DRM Example";
    OverrideDefaults(&myglobs.bNoTextures,
                     &bResizingDisabled,
                     &myglobs.bConstRenderQuality,
                     &szName);
    myglobs.lpCmdLine = lpCmdLine;

    /*
     * Enumerate the DD drivers
     */
    if (!D3DAppEnumerateDDDevices(&myglobs.NumDDDrivers, &myglobs.DDDriver[0])) {
        ReportD3DAppError();
        return FALSE;
    }
    /*
     * Choose the last device enumerated which will probably be secondary 3d hardware.
     */
    myglobs.CurrDDDriver = myglobs.NumDDDrivers - 1;

    /*
     * Create the window
     */
    if (bResizingDisabled)
        flags =  WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU |
                 WS_MINIMIZEBOX | WS_MAXIMIZEBOX;
    else
        flags = WS_OVERLAPPEDWINDOW;
    /*
     * Create a window with some default settings that may change
     */
    myglobs.hWndMain = CreateWindowEx(
          WS_EX_APPWINDOW,
         "Example",
         szName,
         flags,
         CW_USEDEFAULT, CW_USEDEFAULT,
         START_WIN_SIZE, START_WIN_SIZE,
         NULL,                              /* parent window */
         NULL,                              /* menu handle */
         hInstance,                         /* program handle */
         NULL);                             /* create parms */
    if (!myglobs.hWndMain){
        Msg("CreateWindowEx failed");
        return FALSE;
    }
    /*
     * Display the window
     */
    ShowWindow(myglobs.hWndMain, SW_SHOWNORMAL);
    UpdateWindow(myglobs.hWndMain);
    /*
     * Add the list of DD drivers D3DApp found to the file menu
     */
    hmenu = GetSubMenu(GetMenu(myglobs.hWndMain), 0);
    for (i = 0; i < myglobs.NumDDDrivers; i++) {
        InsertMenu(hmenu,  6 + i, MF_BYPOSITION | MF_STRING,
                   MENU_FIRST_DDDRIVER + i, myglobs.DDDriver[i].Name);
    }
    /*
     * Create the D3DRM object which are initialized only when the program
     * starts
     */
    if (!CreateD3DRM(myglobs.hWndMain))
        return FALSE;
    /*
     * Call D3DApp to initialize all DD and D3D objects necessary to render.
     * D3DApp will call the device creation callback which will initialize the
     * viewport and the sample's render states.
     */
    if (!CreateD3DApp(lpCmdLine))
        return FALSE;
    /*
     * Create the scene to be rendered by calling this sample's BuildScene
     */
	AddMediaPath(lpD3DRM);
    if (!BuildScene(lpD3DRM, myglobs.dev, myglobs.view, myglobs.scene, myglobs.camera))
        return FALSE;

    return TRUE;
}

/*
 * CreateD3DRM
 * Create main D3DRM objects which are only initialized once.
 */
BOOL
CreateD3DRM(HWND win)
{
    HRESULT rval;

    /*
     * Create the D3DRM object
     */

    LPDIRECT3DRM pD3DRM;

    rval = Direct3DRMCreate(&pD3DRM);
    if (rval != D3DRM_OK) {
        Msg("Failed to create Direct3DRM.\n%s", D3DAppErrorToString(rval));
        return FALSE;
    }

    rval = pD3DRM->QueryInterface(IID_IDirect3DRM3, (LPVOID *)&lpD3DRM);

    pD3DRM->Release();

    if (rval != D3DRM_OK) {
        Msg("Failed to QI for IID_IDirect3DRM3.\n%s", D3DAppErrorToString(rval));
        return FALSE;
    }

    /*
     * Create the master scene frame and camera frame
     */
    rval = lpD3DRM->CreateFrame(NULL, &myglobs.scene);
    if (rval != D3DRM_OK) {
        Msg("Failed to create the master scene frame.\n%s", D3DAppErrorToString(rval));
        return FALSE;
    }
    rval = lpD3DRM->CreateFrame(myglobs.scene, &myglobs.camera);
    if (rval != D3DRM_OK) {
        Msg("Failed to create the camera's frame.\n%s", D3DAppErrorToString(rval));
        return FALSE;
    }
    rval = myglobs.camera->SetPosition(myglobs.scene, D3DVAL(0.0), D3DVAL(0.0), D3DVAL(0.0));
    if (rval != D3DRM_OK) {
        Msg("Failed to position the camera in the frame.\n%s", D3DAppErrorToString(rval));
        return FALSE;
    }
    return TRUE;
}

/*
 * CreateD3DApp
 * Create all DirectDraw and Direct3D objects necessary to begin rendering.
 * Add the list of D3D drivers to the file menu.
 */
static BOOL
CreateD3DApp(LPSTR lpCmdLine)
{
    HMENU hmenu;
    int i;
    LPSTR option;
    BOOL bOnlySystemMemory, bOnlyEmulation;
    DWORD flags;

    /*
     * Parse the command line in seach of one of the following options:
     *     systemmemory  All surfaces should be created in system memory.
     *                   Hardware DD and D3D devices are disabled, but
     *                   debugging during the Win16 lock becomes possible.
     *     emulation     Do not use hardware DD or D3D devices.
     */
    bOnlySystemMemory = FALSE;
    bOnlyEmulation = FALSE;
    option = strtok(lpCmdLine, " -");
    while(option != NULL )   {
        if (!lstrcmp(option, "systemmemory")) {
            bOnlySystemMemory = TRUE;
        } else if (!lstrcmp(option, "emulation")) {
            bOnlyEmulation = TRUE;
        } else {
            Msg("Invalid command line options given.\nLegal options: -systemmemory, -emulation\n");
            return FALSE;
        }
        option = strtok(NULL, " -");
    }
    /*
     * Set the flags to pass to the D3DApp creation based on command line
     */
    flags = ((bOnlySystemMemory) ? D3DAPP_ONLYSYSTEMMEMORY : 0) |
            ((bOnlyEmulation) ? (D3DAPP_ONLYD3DEMULATION |
                                 D3DAPP_ONLYDDEMULATION) : 0);
    /*
     * Create all the DirectDraw and D3D objects neccesary to render.  The
     * AfterDeviceCreated callback function is called by D3DApp to create the
     * viewport and the example's execute buffers.
     */
    if (!D3DAppCreateFromHWND(flags, myglobs.hWndMain,
                              myglobs.DDDriver[myglobs.CurrDDDriver].bIsPrimary ? NULL : &myglobs.DDDriver[myglobs.CurrDDDriver].Guid,
                              AfterDeviceCreated, NULL, BeforeDeviceDestroyed, NULL, &d3dapp)) {
        ReportD3DAppError();
        return FALSE;
    }
    /*
     * Add the the list of display modes D3DApp found to the mode menu
     */
    hmenu = GetSubMenu(GetMenu(myglobs.hWndMain), 2);
    for (i = 0; i < d3dapp->NumModes; i++) {
        char ach[80];
        wsprintf(ach,"%dx%dx%d", d3dapp->Mode[i].w, d3dapp->Mode[i].h,
                 d3dapp->Mode[i].bpp);
        AppendMenu(hmenu, MF_STRING, MENU_FIRST_MODE+i, ach);
    }
    /*
     * Add the list of D3D drivers D3DApp foudn to the file menu
     */
    hmenu = GetSubMenu(GetMenu(myglobs.hWndMain), 0);
    for (i = 0; i < d3dapp->NumDrivers; i++) {
        InsertMenu(hmenu, 7 + myglobs.NumDDDrivers + i, MF_BYPOSITION | MF_STRING,
                   MENU_FIRST_DRIVER + i, d3dapp->Driver[i].Name);
    }

    return TRUE;
}

/*
 * DestroyD3DApp
 *
 * Destroy D3DApp and changes to menu
 */
static void
DestroyD3DApp(void)
{
    HMENU hmenu;
    int i;

    /*
     * Remove the list of display modes
     */
    hmenu = GetSubMenu(GetMenu(myglobs.hWndMain), 4);
    for (i = 0; i < d3dapp->NumModes; i++) {
        DeleteMenu(hmenu, MENU_FIRST_MODE + i, MF_BYCOMMAND);
    }
    /*
     * Remove the list of D3D drivers
     */
    hmenu = GetSubMenu(GetMenu(myglobs.hWndMain), 0);
    for (i = 0; i < d3dapp->NumDrivers; i++) {
        DeleteMenu(hmenu, MENU_FIRST_DRIVER + i, MF_BYCOMMAND);
    }
    RELEASE(myglobs.lpInfoBuffer);
    RELEASE(myglobs.lpFrameRateBuffer);
    D3DAppDestroy();
}

/*
 * AfterDeviceCreated
 * D3DApp will call this function immediately after the D3D device has been
 * created (or re-created).  D3DApp expects the D3D viewport to be created and
 * returned.  In this case, we will return NULL because we only have a D3DRM
 * viewport.  This is fine as long as we don't use any of the D3D viewport
 * functionality of D3DApp.
 */
static BOOL
AfterDeviceCreated(int w, int h, LPDIRECT3DVIEWPORT* lplpViewport, LPVOID lpContext)
{
    HRESULT rval;

    rval = lpD3DRM->CreateDeviceFromD3D(d3dapp->lpD3D, d3dapp->lpD3DDevice,
                                        &myglobs.dev);
    if (rval != D3DRM_OK) {
        Msg("Creation of D3DRM device failed.\n%s", D3DAppErrorToString(rval));
        return FALSE;
    }
    /*
     * Create the D3DRM viewport using the camera frame.  Set the background
     * depth to a large number.  The width and height may be slightly
     * adjusted, so get them from the device to be sure.
     */
    w = myglobs.dev->GetWidth();
    h = myglobs.dev->GetHeight();
    rval = lpD3DRM->CreateViewport(myglobs.dev, myglobs.camera, 0, 0, w,
                                   h, &myglobs.view);
    if (rval != D3DRM_OK) {
        Msg("Failed to create the D3DRM viewport.\n%s",
            D3DAppErrorToString(rval));
        RELEASE(myglobs.dev);
        return FALSE;
    }
    rval = myglobs.view->SetBack(D3DVAL(5000.0));
    if (rval != D3DRM_OK) {
        Msg("Failed to set the background depth of the D3DRM viewport.\n%s",
            D3DAppErrorToString(rval));
        RELEASE(myglobs.dev);
        RELEASE(myglobs.view);
        return FALSE;
    }
    /*
     * Set the render quality, fill mode, lighting state and color shade info
     */
    if (!SetRenderState())
        return FALSE;

    /*
     * Return NULL for the viewport
     */
    *lplpViewport = NULL;
    /*
     * Create and initialize the surfaces containing the frame rate and
     * window information
     */
    InitFontAndTextBuffers();

    return TRUE;
}

/*
 * BeforeDeviceDestroyed
 * D3DApp will call this function before the current D3D device is destroyed
 * to give the app the opportunity to destroy objects it has created with the
 * DD or D3D objects.
 */
static BOOL
BeforeDeviceDestroyed(LPVOID lpContext)
{
    RELEASE(myglobs.view);
    RELEASE(myglobs.dev);
    return TRUE;
}

/****************************************************************************/
/*                            Rendering loop                                */
/****************************************************************************/
/*
 * RenderLoop
 * Render the next frame and update the window
 */
static BOOL
RenderLoop()
{
    D3DRECT extents[D3DAPP_MAXCLEARRECTS];
    int count;
    HRESULT rval;
    static BOOL b = FALSE; // Clear the second buffer on also

    /*
     * If all the DD and D3D objects have been initialized we can render
     */
    if (d3dapp->bRenderingIsOK) {
        /*
         * Restore any lost surfaces
         */
        if (!RestoreSurfaces()) {
            /*
             * Restoring surfaces sometimes fails because the surfaces cannot
             * yet be restored.  If this is the case, the error will show up
             * somewhere else and we should return success here to prevent
             * unnecessary error's being reported.
             */
            return TRUE;
        }
        /*
         * Force an update of the entire client window if the resized flag is set
         */
        if (myglobs.bResized || b)
            myglobs.view->ForceUpdate(0, 0, d3dapp->szClient.cx, d3dapp->szClient.cy);
        /*
         * Use b to makesure the second buffer is cleared also
         */
        if (b)
            b = FALSE;
        if (myglobs.bResized)
            b = TRUE;

        /*
         * Calculate the frame rate
         */
        if (!CalculateFrameRate())
            return FALSE;

        /*
         * Tick the scene
         */
        rval = myglobs.scene->Move(D3DVAL(1.0));
        if (rval != D3DRM_OK) {
            Msg("Moving scene failed.\n%s", D3DAppErrorToString(rval));
            return FALSE;
        }
        /*
         * Clear the viewport
         */
        rval = myglobs.view->Clear(D3DRMCLEAR_ALL);
        if (rval != D3DRM_OK) {
            Msg("Clearing viewport failed.\n%s", D3DAppErrorToString(rval));
            return FALSE;
        }
        /*
         * Render the scene to the viewport
         */
        rval = myglobs.view->Render(myglobs.scene);
        if (rval != D3DRM_OK) {
            Msg("Rendering scene failed.\n%s", D3DAppErrorToString(rval));
            return FALSE;
        }
        /*
         * Blt the frame rate and window stat text to the back buffer
         */
        count = 0;
        if (!DisplayFrameRate(&count, &extents[0]))
            return FALSE;
        for (;count;--count)
            myglobs.view->ForceUpdate(extents[count-1].x1, extents[count-1].y1,
                                      extents[count-1].x2, extents[count-1].y2);
        /*
         * Update the window
         */
        rval = myglobs.dev->Update();
        if (rval != D3DRM_OK) {
            Msg("Updating device failed.\n%s", D3DAppErrorToString(rval));
            return FALSE;
        }
        /*
         * Blt or flip the back buffer to the front buffer.  If this fails,
         * don't report an error.
         */
        D3DAppShowBackBuffer(myglobs.bResized ? D3DAPP_SHOWALL : NULL);

        /*
         * Reset the resize flag
         */
        myglobs.bResized = FALSE;
    }
    return TRUE;
}

/*
 * AppPause
 * Pause and unpause the application
 */
static BOOL
AppPause(BOOL f)
{
    /*
     * Flip to the GDI surface and halt rendering
     */
    if (!D3DAppPause(f))
        return FALSE;
    /*
     * When returning from a pause, reset the frame rate count
     */
    if (!f) {
        ResetFrameRate();
        myglobs.bResized = TRUE;
    }
    return TRUE;
}

/*
 * RestoreSurfaces
 * Restores any lost surfaces.  Returns TRUE if all surfaces are not lost and
 * FALSE if one or more surfaces is lost and can not be restored at the
 * moment.
 */
static BOOL
RestoreSurfaces()
{
    HRESULT d3drval;

    /*
     * Have D3DApp check all the surfaces it's in charge of
     */
    if (!D3DAppCheckForLostSurfaces()) {
            return FALSE;
    }
    /*
     * Check frame rate and info surfaces and re-write them if they
     * were lost.
     */
    if (myglobs.lpFrameRateBuffer->IsLost() == DDERR_SURFACELOST) {
        d3drval = myglobs.lpFrameRateBuffer->Restore();
        if (d3drval != DD_OK) {
            return FALSE;
        }
        if (!WriteFrameRateBuffer(0.0f, 0))
            return FALSE;
    }
    if (myglobs.lpInfoBuffer->IsLost() == DDERR_SURFACELOST) {
        d3drval = myglobs.lpInfoBuffer->Restore();
        if (d3drval != DD_OK) {
            return FALSE;
        }
        if (!WriteInfoBuffer())
            return FALSE;
    }
    return TRUE;
}


/*************************************************************************
  Windows message handlers
 *************************************************************************/
/*
 * AppAbout
 * About box message handler
 */
BOOL
FAR PASCAL AppAbout(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  switch (msg)
  {
    case WM_COMMAND:
      if (LOWORD(wParam) == IDOK)
        EndDialog(hwnd, TRUE);
      break;

    case WM_INITDIALOG:
      return TRUE;
  }
  return FALSE;
}

/*
 * WindowProc
 * Main window message handler.
 */
long
FAR PASCAL WindowProc(HWND hWnd, UINT message, WPARAM wParam,
                           LPARAM lParam )
{
    int i;
    BOOL bStop;
    LRESULT lresult;

    /*
     * Give D3DApp an opportunity to process any messages it MUST see in order
     * to perform it's function.
     */
    if (!D3DAppWindowProc(&bStop, &lresult, hWnd, message, wParam, lParam)) {
        ReportD3DAppError();
        CleanUpAndPostQuit();
        return 0;
    }
    /*
     * If bStop is set by D3DApp, the app should not process the message but
     * return lresult.
     */
    if (bStop)
        return lresult;

    if (!d3dapp || !d3dapp->bRenderingIsOK) {
        return DefWindowProc(hWnd, message, wParam, lParam);
    }

    switch( message ) {
        case WM_LBUTTONDOWN:
        case WM_LBUTTONUP:
        case WM_RBUTTONDOWN:
        case WM_RBUTTONUP:
        case WM_MOUSEMOVE:
            /*
             * Record the mouse state for ReadMouse
             */
            myglobs.mouse_buttons = wParam;
            myglobs.mouse_x = LOWORD(lParam);
            myglobs.mouse_y = HIWORD(lParam);
            break;
        case WM_ENTERMENULOOP:
            AppPause(TRUE);
            break;
        case WM_EXITMENULOOP:
            AppPause(FALSE);
            break;
        case WM_DESTROY:
            myglobs.hWndMain = NULL;
            CleanUpAndPostQuit();
            break;
        case WM_INITMENUPOPUP:
            /*
             * Check and enable the appropriate menu items
             */
            CheckMenuItem((HMENU)wParam, MENU_STEP, (myglobs.bSingleStepMode) ? MF_CHECKED : MF_UNCHECKED);
            EnableMenuItem((HMENU)wParam, MENU_GO, (myglobs.bSingleStepMode) ? MF_ENABLED : MF_GRAYED);
            EnableMenuItem((HMENU)wParam, MENU_PHONG, MF_GRAYED);
            if (!myglobs.bConstRenderQuality) {
                CheckMenuItem((HMENU)wParam, MENU_LIGHTING, (myglobs.RenderQuality & D3DRMLIGHT_MASK) == D3DRMLIGHT_ON ? MF_CHECKED : MF_GRAYED);
                CheckMenuItem((HMENU)wParam, MENU_FLAT, (myglobs.RenderQuality & D3DRMSHADE_MASK) == D3DRMSHADE_FLAT ? MF_CHECKED : MF_UNCHECKED);
                CheckMenuItem((HMENU)wParam, MENU_GOURAUD, (myglobs.RenderQuality & D3DRMSHADE_MASK) == D3DRMSHADE_GOURAUD ? MF_CHECKED : MF_UNCHECKED);
                CheckMenuItem((HMENU)wParam, MENU_PHONG, (myglobs.RenderQuality & D3DRMSHADE_MASK) == D3DRMSHADE_PHONG ? MF_CHECKED : MF_UNCHECKED);
                EnableMenuItem((HMENU)wParam, MENU_PHONG, MF_GRAYED);
                CheckMenuItem((HMENU)wParam, MENU_POINT, (myglobs.RenderQuality & D3DRMFILL_MASK) == D3DRMFILL_POINTS ? MF_CHECKED : MF_UNCHECKED);
                CheckMenuItem((HMENU)wParam, MENU_WIREFRAME, (myglobs.RenderQuality & D3DRMFILL_MASK) == D3DRMFILL_WIREFRAME ? MF_CHECKED : MF_UNCHECKED);
                CheckMenuItem((HMENU)wParam, MENU_SOLID, (myglobs.RenderQuality & D3DRMFILL_MASK) == D3DRMFILL_SOLID ? MF_CHECKED : MF_UNCHECKED);
            } else {
                EnableMenuItem((HMENU)wParam, MENU_LIGHTING, MF_GRAYED);
                EnableMenuItem((HMENU)wParam, MENU_FLAT, MF_GRAYED);
                EnableMenuItem((HMENU)wParam, MENU_GOURAUD, MF_GRAYED);
                EnableMenuItem((HMENU)wParam, MENU_PHONG, MF_GRAYED);
                EnableMenuItem((HMENU)wParam, MENU_POINT, MF_GRAYED);
                EnableMenuItem((HMENU)wParam, MENU_WIREFRAME, MF_GRAYED);
                EnableMenuItem((HMENU)wParam, MENU_SOLID, MF_GRAYED);
            }
            if (!myglobs.bNoTextures && d3dapp->ThisDriver.bDoesTextures) {
                CheckMenuItem((HMENU)wParam, MENU_POINT_FILTER, (myglobs.TextureQuality == D3DRMTEXTURE_NEAREST) ? MF_CHECKED : MF_UNCHECKED);
                CheckMenuItem((HMENU)wParam, MENU_LINEAR_FILTER, (myglobs.TextureQuality == D3DRMTEXTURE_LINEAR) ? MF_CHECKED : MF_UNCHECKED);
            } else {
                EnableMenuItem((HMENU)wParam, MENU_POINT_FILTER, MF_GRAYED);
                EnableMenuItem((HMENU)wParam, MENU_LINEAR_FILTER, MF_GRAYED);
            }
            CheckMenuItem((HMENU)wParam, MENU_DITHERING, (myglobs.bDithering) ? MF_CHECKED : MF_UNCHECKED);
            CheckMenuItem((HMENU)wParam, MENU_ANTIALIAS, (myglobs.bAntialiasing) ? MF_CHECKED : MF_UNCHECKED);
            EnableMenuItem((HMENU)wParam, MENU_ANTIALIAS, MF_GRAYED);
            if (d3dapp->bIsPrimary) {
                CheckMenuItem((HMENU)wParam, MENU_FULLSCREEN, d3dapp->bFullscreen ? MF_CHECKED : MF_UNCHECKED);
                EnableMenuItem((HMENU)wParam, MENU_FULLSCREEN, d3dapp->bFullscreen && !d3dapp->ThisDriver.bCanDoWindow ? MF_GRAYED : MF_ENABLED);
                EnableMenuItem((HMENU)wParam, MENU_NEXT_MODE, (!d3dapp->bFullscreen) ? MF_GRAYED : MF_ENABLED);
                EnableMenuItem((HMENU)wParam, MENU_PREVIOUS_MODE, (!d3dapp->bFullscreen) ? MF_GRAYED : MF_ENABLED);
            } else {
                EnableMenuItem((HMENU)wParam, MENU_FULLSCREEN, MF_GRAYED);
                EnableMenuItem((HMENU)wParam, MENU_NEXT_MODE, MF_GRAYED);
                EnableMenuItem((HMENU)wParam, MENU_PREVIOUS_MODE, MF_GRAYED);
            }
            for (i = 0; i < d3dapp->NumModes; i++) {
                CheckMenuItem((HMENU)wParam, MENU_FIRST_MODE + i, (i == d3dapp->CurrMode) ? MF_CHECKED : MF_UNCHECKED);
                EnableMenuItem((HMENU)wParam, MENU_FIRST_MODE + i, (d3dapp->Mode[i].bThisDriverCanDo) ? MF_ENABLED : MF_GRAYED);
            }
            for (i = 0; i < d3dapp->NumDrivers; i++) {
                CheckMenuItem((HMENU)wParam, MENU_FIRST_DRIVER + i, (i == d3dapp->CurrDriver) ? MF_CHECKED : MF_UNCHECKED);
            }
            break;
        case WM_COMMAND:
            switch(LOWORD(wParam)) {
                case MENU_ABOUT:
                    AppPause(TRUE);
                    DialogBox(myglobs.hInstApp, "AppAbout", myglobs.hWndMain, (DLGPROC)AppAbout);
                    AppPause(FALSE);
                    break;
                case MENU_EXIT:
                    CleanUpAndPostQuit();
                    break;
                case MENU_STEP:
                    /*
                     * Begin single step more or draw a frame if in single
                     * step mode
                     */
                    if (!myglobs.bSingleStepMode) {
                        myglobs.bSingleStepMode = TRUE;
                        myglobs.bDrawAFrame = TRUE;
                    } else if (!myglobs.bDrawAFrame) {
                        myglobs.bDrawAFrame = TRUE;
                    }
                    break;
                case MENU_GO:
                    /*
                     * Exit single step mode
                     */
                    if (myglobs.bSingleStepMode) {
                        myglobs.bSingleStepMode = FALSE;
                        ResetFrameRate();
                    }
                    break;
                case MENU_STATS:
                    /*
                     * Toggle output of frame rate and window info
                     */
                    if ((myglobs.bShowFrameRate) && (myglobs.bShowInfo)) {
                        myglobs.bShowFrameRate = FALSE;
                        myglobs.bShowInfo = FALSE;
                        break;
                    }
                    if ((!myglobs.bShowFrameRate) && (!myglobs.bShowInfo)) {
                        myglobs.bShowFrameRate = TRUE;
                        break;
                    }
                    myglobs.bShowInfo = TRUE;
                    break;
                case MENU_FULLSCREEN:
                    if (d3dapp->bFullscreen) {
                        /*
                         * Return to a windowed mode.  Let D3DApp decide which
                         * D3D driver to use in case this one cannot render to
                         * the Windows display depth
                         */
                        if (!D3DAppWindow(D3DAPP_YOUDECIDE, D3DAPP_YOUDECIDE)) {
                            ReportD3DAppError();
                            CleanUpAndPostQuit();
                            break;
                        }
                    } else {
                        /*
                         * Enter the current fullscreen mode.  D3DApp may
                         * resort to another mode if this driver cannot do
                         * the currently selected mode.
                         */
                        if (!D3DAppFullscreen(d3dapp->CurrMode)) {
                            ReportD3DAppError();
                            CleanUpAndPostQuit();
                            break;
                        }
                    }
                    break;
                /*
                 * Lighting toggle
                 */
                case MENU_LIGHTING:
                    myglobs.RenderQuality ^= D3DRMLIGHT_ON;
                    SetRenderState();
                    break;
                /*
                 * Fill mode selection
                 */
                case MENU_POINT:
                    myglobs.RenderQuality = (myglobs.RenderQuality & ~D3DRMFILL_MASK) | D3DRMFILL_POINTS;
                    SetRenderState();
                    break;
                case MENU_WIREFRAME:
                    myglobs.RenderQuality = (myglobs.RenderQuality & ~D3DRMFILL_MASK) | D3DRMFILL_WIREFRAME;
                    SetRenderState();
                    break;
                case MENU_SOLID:
                    myglobs.RenderQuality = (myglobs.RenderQuality & ~D3DRMFILL_MASK) | D3DRMFILL_SOLID;
                    SetRenderState();
                    break;
                /*
                 * Shade mode selection
                 */
                case MENU_FLAT:
                    myglobs.RenderQuality = (myglobs.RenderQuality & ~D3DRMSHADE_MASK) | D3DRMSHADE_FLAT;
                    SetRenderState();
                    break;
                case MENU_GOURAUD:
                    myglobs.RenderQuality = (myglobs.RenderQuality & ~D3DRMSHADE_MASK) | D3DRMSHADE_GOURAUD;
                    SetRenderState();
                    break;
                case MENU_PHONG:
                    myglobs.RenderQuality = (myglobs.RenderQuality & ~D3DRMSHADE_MASK) | D3DRMSHADE_PHONG;
                    SetRenderState();
                    break;

                case MENU_DITHERING:
                    myglobs.bDithering = !myglobs.bDithering;
                    SetRenderState();
                    break;
                case MENU_ANTIALIAS:
                    myglobs.bAntialiasing = !myglobs.bAntialiasing;
                    SetRenderState();
                    break;
                /*
                 * Texture filter selection
                 */
                case MENU_POINT_FILTER:
                    if (myglobs.TextureQuality == D3DRMTEXTURE_NEAREST)
                        break;
                    myglobs.TextureQuality = D3DRMTEXTURE_NEAREST;
                    SetRenderState();
                    break;
                case MENU_LINEAR_FILTER:
                    if (myglobs.TextureQuality == D3DRMTEXTURE_LINEAR)
                        break;
                    myglobs.TextureQuality = D3DRMTEXTURE_LINEAR;
                    SetRenderState();
                    break;
                case MENU_NEXT_MODE:
                    /*
                     * Enter the next usable fullscreen mode
                     */
                    i = d3dapp->CurrMode;
                    do {
                        ++i;
                        if (i >= d3dapp->NumModes)
                            i = 0;
                        if (!d3dapp->Mode[i].bThisDriverCanDo)
                            continue;
                        else {
                            if (!D3DAppFullscreen(i)) {
                                ReportD3DAppError();
                                CleanUpAndPostQuit();
                            }
                            break;
                        }
                    } while(i != d3dapp->CurrMode);
                    break;
                case MENU_PREVIOUS_MODE:
                    /*
                     * Enter the previous usable fullscreen mode
                     */
                    i = d3dapp->CurrMode;
                    do {
                        --i;
                        if (i < 0)
                            i = d3dapp->NumModes - 1;
                        if (!d3dapp->Mode[i].bThisDriverCanDo)
                            continue;
                        else {
                            if (!D3DAppFullscreen(i)) {
                                ReportD3DAppError();
                                CleanUpAndPostQuit();
                            }
                            break;
                        }
                    } while(i != d3dapp->CurrMode);
                    break;
            }
            if (   LOWORD(wParam) >= MENU_FIRST_DRIVER
                && LOWORD(wParam) < MENU_FIRST_DRIVER + D3DAPP_MAXD3DDRIVERS
                && d3dapp->CurrDriver != LOWORD(wParam) - MENU_FIRST_DRIVER) {
                /*
                 * Change the D3D driver
                 */
                if (!D3DAppChangeDriver(LOWORD(wParam) - MENU_FIRST_DRIVER,
                                        NULL)) {
                    ReportD3DAppError();
                    CleanUpAndPostQuit();
                }
            }
            if (   LOWORD(wParam) >= MENU_FIRST_MODE
                && LOWORD(wParam) < MENU_FIRST_MODE+100) {
                /*
                 * Switch to the selected fullscreen mode
                 */
                if (!D3DAppFullscreen(LOWORD(wParam) - MENU_FIRST_MODE)) {
                    ReportD3DAppError();
                    CleanUpAndPostQuit();
                }
            }
            if (   LOWORD(wParam) >= MENU_FIRST_DDDRIVER
                && LOWORD(wParam) < MENU_FIRST_DDDRIVER + D3DAPP_MAXDDDRIVERS
                && myglobs.CurrDDDriver != LOWORD(wParam) - MENU_FIRST_DDDRIVER) {
                /*
                 * Change the DD driver
                 */
                DestroyD3DApp();
                myglobs.CurrDDDriver = LOWORD(wParam) - MENU_FIRST_DDDRIVER;
                SetWindowPos(myglobs.hWndMain, HWND_NOTOPMOST, 0, 0, START_WIN_SIZE, START_WIN_SIZE, SWP_NOMOVE | SWP_SHOWWINDOW);
                if (!CreateD3DApp(myglobs.lpCmdLine))
                    return FALSE;
            }
            /*
             * Whenever we receive a command in single step mode, draw a frame
             */
            if (myglobs.bSingleStepMode)
                myglobs.bDrawAFrame = TRUE;
            return 0L;
    }
    return DefWindowProc(hWnd, message, wParam, lParam);
}

/****************************************************************************/
/*                          Additional Functions                            */
/****************************************************************************/
/*
 * ReadMouse
 * Returns the mouse status for interaction with sample code
 */
void
ReadMouse(int* b, int* x, int* y)
{
    *b = myglobs.mouse_buttons;
    *x = myglobs.mouse_x;
    *y = myglobs.mouse_y;
}

/*
 * SetRenderState
 * Set the render quality, dither toggle and shade info if any of them has
 * changed
 */
BOOL
SetRenderState(void)
{
    HRESULT rval;
    /*
     * Set the number of buffers so D3DRM can keep track of extents properly
     */
    rval = myglobs.dev->SetBufferCount(d3dapp->bFullscreen &&
                                       d3dapp->bBackBufferInVideo ? 2 : 1);
    if (rval != D3DRM_OK) {
        Msg("Setting the buffer count failed.\n%s", D3DAppErrorToString(rval));
        return FALSE;
    }
    /*
     * Set the render quality (light toggle, fill mode, shade mode)
     */
    if (myglobs.dev->GetQuality() != myglobs.RenderQuality) {
        rval = myglobs.dev->SetQuality(myglobs.RenderQuality);
        if (rval != D3DRM_OK) {
            Msg("Setting the render quality failed.\n%s",
                D3DAppErrorToString(rval));
            return FALSE;
        }
    }
    /*
     * Set dithering toggle
     */
    if (myglobs.dev->GetDither() != myglobs.bDithering) {
        rval = myglobs.dev->SetDither(myglobs.bDithering);
        if (rval != D3DRM_OK) {
            Msg("Setting dither mode failed.\n%s", D3DAppErrorToString(rval));
            return FALSE;
        }
    }
    /*
     * Set the texture quality (point or linear filtering)
     */
    if (myglobs.dev->GetTextureQuality() != myglobs.TextureQuality) {
        rval = myglobs.dev->SetTextureQuality(myglobs.TextureQuality);
        if (rval != D3DRM_OK) {
            Msg("Setting texture quality failed.\n%s",
                D3DAppErrorToString(rval));
            return FALSE;
        }
    }
    /*
     * Set shade info based on current bits per pixel
     */
    switch (d3dapp->ThisMode.bpp) {
        case 1:
            if (FAILED(myglobs.dev->SetShades(4)))
                goto shades_error;
            if (FAILED(lpD3DRM->SetDefaultTextureShades(4)))
                goto shades_error;
            break;
        case 16:
            if (FAILED(myglobs.dev->SetShades(32)))
                goto shades_error;
            if (FAILED(lpD3DRM->SetDefaultTextureColors(64)))
                goto shades_error;
            if (FAILED(lpD3DRM->SetDefaultTextureShades(32)))
                goto shades_error;
            break;
        case 24:
        case 32:
            if (FAILED(myglobs.dev->SetShades(256)))
                goto shades_error;
            if (FAILED(lpD3DRM->SetDefaultTextureColors(64)))
                goto shades_error;
            if (FAILED(lpD3DRM->SetDefaultTextureShades(256)))
                goto shades_error;
            break;
    }
    return TRUE;
shades_error:
    Msg("A failure occurred while setting color shade information.\n");
    return FALSE;
}

/****************************************************************************/
/*          Initialization, error reporting and release functions.          */
/****************************************************************************/
/*
 * InitGlobals
 * Called once at program initialization to initialize global variables.
 */
static void
InitGlobals(void)
{
    d3dapp = NULL;
    memset(&myglobs, 0, sizeof(myglobs));
    myglobs.bShowFrameRate = TRUE;
    myglobs.bShowInfo = TRUE;
    myglobs.RenderQuality = D3DRMLIGHT_ON | D3DRMFILL_SOLID |
                            D3DRMSHADE_GOURAUD;
    myglobs.TextureQuality = D3DRMTEXTURE_NEAREST;
}

/*
 * CleanUpAndPostQuit
 * Release all D3D objects, post a quit message and set the bQuit flag
 */
void
CleanUpAndPostQuit(void)
{
    if (myglobs.bQuit)
        return;
    if (!D3DAppDestroy())
        ReportD3DAppError();
    RELEASE(myglobs.scene);
    RELEASE(myglobs.camera);
    RELEASE(lpD3DRM);
    myglobs.bQuit = TRUE;
    PostQuitMessage( 0 );
}

/*
 * ReportD3DAppError
 * Reports an error during a d3d app call.
 */
void
ReportD3DAppError(void)
{
    Msg("%s", D3DAppLastErrorString());
}

/* Msg
 * Message output for error notification.
 */
void __cdecl
Msg( LPSTR fmt, ... )
{
    char buff[256];
    va_list args;

    va_start(args, fmt);
    wvsprintf(buff, fmt, args);
    va_end(args);

    lstrcat(buff, "\r\n");
    AppPause(TRUE);
    if (d3dapp && d3dapp->bFullscreen)
        SetWindowPos(myglobs.hWndMain, HWND_NOTOPMOST, 0, 0, 0, 0,
                     SWP_NOSIZE | SWP_NOMOVE);
    MessageBox( NULL, buff, "D3D Example Message", MB_OK );
    if (d3dapp && d3dapp->bFullscreen)
        SetWindowPos(myglobs.hWndMain, HWND_TOPMOST, 0, 0, 0, 0,
                     SWP_NOSIZE | SWP_NOMOVE);
    AppPause(FALSE);
}

/*
 * D3DRMErrorToString
 * Allows the samples to return error strings.
 */
char*
D3DRMErrorToString(HRESULT error)
{
    return D3DAppErrorToString(error);
}
