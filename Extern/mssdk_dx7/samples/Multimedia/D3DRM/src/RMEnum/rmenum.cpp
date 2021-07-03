//-----------------------------------------------------------------------------
// File: egg.cpp
//
// Desc:
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#define INITGUID

#include <windows.h>   /* Standard Windows header file */
#include <direct.h>    /* DirectDraw definitions */
#include <d3drmwin.h>  /* D3DRM definitions */
#include "RMEnum.h"    /* Defines constants used in RMEnum.rc */
#include "rmerror.h"   /* Prototypes for error reporting functions in
                             rmerror.c */

#define MAX_DRIVERS 5  /* Arbitrary limit for the maximum number of  
                            Direct3D drivers expected to be found */

/* Macro to release an object. */
#define SAFE_RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}

/* 
 * GLOBAL VARIABLES
 */
LPDIRECT3DRM lpD3DRM = NULL;            /* Direct3DRM object */
LPDIRECTDRAWCLIPPER lpDDClipper = NULL; /* DirectDrawClipper object */

struct _myglobs {
    /* Direct3DRM device */
    LPDIRECT3DRMDEVICE dev;           

    /* GUIDs of the available Direct3D (D3D) drivers */
    GUID DriverGUID[MAX_DRIVERS];     

    /* names of the available D3D drivers */
    char DriverName[MAX_DRIVERS][50]; 

    /* counter indicating the number of available D3D drivers */
    int  NumDrivers;                  

    /* number of the D3D driver currently being used; set to
     * the preferred driver by enumDeviceFunc */
    int  CurrDriver;                  

    /* program is about to terminate */
    BOOL bQuit;                 

    /* all D3DRM objects have been initialized */
    BOOL bInitialized;          
    
    /* bit depth of the current display mode */
    DWORD BPP;                  
} myglobs;

/*
 * PROTOTYPES
 */
BOOL CreateObjects(HWND win);
static DWORD GetCurrentBitDepth(HWND);
static BOOL EnumDevices(HWND);
static HRESULT WINAPI enumDeviceFunc(LPGUID, 
                                     LPSTR, 
                                     LPSTR, 
                                     LPD3DDEVICEDESC, 
                                     LPD3DDEVICEDESC, 
                                     LPVOID);
void ChangeDriver(HWND, WPARAM);
static void CleanUpAndQuit(void);

/* Standard Windows functions */
static HWND InitApp(HINSTANCE, int);
int APIENTRY WinMain (HINSTANCE, HINSTANCE, LPSTR, int);
LRESULT CALLBACK AppAbout(HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK WindowProc(HWND, UINT, WPARAM, LPARAM);

/************************************************************************/
/*                             CreateObjects                            */                              
/************************************************************************/
/*
 * Initialize globals, enumerate devices and create objects. 
 */
BOOL CreateObjects(HWND win)
{
    HRESULT rval; /* Return value */
    RECT rc;      /* Bounding rectangle for main window */

    /*
     * Initialize the global variables. 
     */
    memset(&myglobs, 0, sizeof(myglobs));

    /*
     * Record the current display bit depth.
     */
     myglobs.BPP = GetCurrentBitDepth(win);

    /*
     * Enumerate the D3D drivers and select one.
     */
    if (!EnumDevices(win))
        return FALSE;

    /*
     * Create the Direct3DRM object and the window object.
     */
    rval = Direct3DRMCreate(&lpD3DRM);
    if (rval != D3DRM_OK) {
        Msg("Failed to create Direct3DRM.\n%s", D3DRMErrorToString(rval));
        return FALSE;
    }

    /*
     * Create a DirectDrawClipper object and associate the window with it.
     */
    rval = DirectDrawCreateClipper(0, &lpDDClipper, NULL);
    if (rval != DD_OK) {
        Msg("Failed to create DirectDrawClipper object");
        return FALSE;
    }
    rval = lpDDClipper->SetHWnd(0, win);
    if (rval != DD_OK) {
        Msg("Failed to set the window handle for the DirectDrawClipper");
        return FALSE;
    }
    /*
     * Create the D3DRM device with the selected D3D driver. The GUID can 
     * be NULL to create a default device without enumeration.
     */
    GetClientRect(win, &rc);

    rval = lpD3DRM->CreateDeviceFromClipper(lpDDClipper, 
                           &myglobs.DriverGUID[myglobs.CurrDriver],
                           rc.right, rc.bottom, &myglobs.dev);

    if (rval) {
        Msg("Failed to create the D3DRM device.\n%s", 
            D3DRMErrorToString(rval));
        return FALSE;
    }

    /*
     * Globals are initialized
     */
    myglobs.bInitialized = TRUE;

    return TRUE;
}

/************************************************************************/
/*                          GetCurrentBitDepth                          */
/************************************************************************/
/*
 * Retrieve a DirectDraw bit depth flag reflecting the current 
 * display bit depth in bits per pixel (BPP).
 */
static DWORD GetCurrentBitDepth(HWND win) 
{
    HDC hdc;
    int BPP;

    /*
     * Record the current display's BPP.
     */
    hdc = GetDC(win);
    BPP = GetDeviceCaps(hdc, BITSPIXEL);
    ReleaseDC(win, hdc);

    /*
     * Map BPP value to DirectDraw bit depth.
     */
    switch(BPP) {
        case 1:
            return DDBD_1;
        case 2:
            return DDBD_2;
        case 4:
            return DDBD_4;
        case 8:
            return DDBD_8;
        case 16:
            return DDBD_16;
        case 24:
            return DDBD_24;
        case 32:
            return DDBD_32;
        default:
            return 0;
    }
}


/************************************************************************/
/*                         D3D Device Enumeration                       */
/************************************************************************/
/*
 * EnumDevices
 * Enumerate the available Direct3D drivers, add them to the file menu, 
 * and choose one to use.
 */
static BOOL EnumDevices(HWND win)
{
    LPDIRECTDRAW lpDD;
    LPDIRECT3D lpD3D;
    HRESULT rval;
    HMENU hmenu;
    int i;

    /*
     * Create a DirectDraw object and query for the Direct3D interface 
     * to use to enumerate the drivers.
     */
    rval = DirectDrawCreate(NULL, &lpDD, NULL);
    if (rval != DD_OK) {
        Msg("Creation of DirectDraw HEL failed.\n%s", 
            D3DRMErrorToString(rval));
        return FALSE;
    }
    rval = lpDD->QueryInterface(IID_IDirect3D, (void**) &lpD3D);
    if (rval != DD_OK) {
        Msg("Creation of Direct3D interface failed.\n%s", 
            D3DRMErrorToString(rval));
        lpDD->Release();
        return FALSE;
    }
    /*
     * Enumerate the drivers. Pass the arbitrary value of -1 as
     * application-defined data. The enumDeviceFunc callback function
     * performs special-case initialization code in response to a -1.
     */
    myglobs.CurrDriver = -1;
    rval = lpD3D->EnumDevices(enumDeviceFunc, &myglobs.CurrDriver);
    if (rval != DD_OK) {
        Msg("Enumeration of drivers failed.\n%s", 
            D3DRMErrorToString(rval));
        return FALSE;
    }
    /*
     * Make sure we found at least one valid driver.
     */
    if (myglobs.NumDrivers == 0) {
        Msg("Could not find a D3D driver that is compatible with this \
            display depth");
        return FALSE;
    }
    lpD3D->Release();
    lpDD->Release();
    /*
     * Add the driver names to the File menu.
     */
    hmenu = GetSubMenu(GetMenu(win), 0);
    for (i = 0; i < myglobs.NumDrivers; i++) {
        InsertMenu(hmenu, 2 + i, MF_BYPOSITION | MF_STRING, 
                   MENU_FIRST_DRIVER + i,
                   myglobs.DriverName[i]);
    }
    return TRUE;
}


/************************************************************************/
/*
 * enumDeviceFunc
 * Callback function which records each usable D3D driver's name and 
 * GUID. It selects the preferred driver on the system by choosing
 * hardware drivers over software drivers, and color lights over 
 * monochrome lights. It sets myglobs.CurrDriver to indicate the 
 * preferred driver.
 */
static HRESULT WINAPI enumDeviceFunc(LPGUID lpGuid, 
                                     LPSTR lpDeviceDescription, 
                                     LPSTR lpDeviceName,
                                     LPD3DDEVICEDESC lpHWDesc, 
                                     LPD3DDEVICEDESC lpHELDesc, 
                                     LPVOID lpContext)
{
    static BOOL hardware = FALSE; /* current start driver is software */
    static BOOL mono = FALSE;     /* current start driver is color */
    LPD3DDEVICEDESC lpDesc;       /* description of current driver */
    
    /* 
     * Data defined by this application that, when it equals -1, 
     * indicates that the current driver is the first to be enumerated 
     */
    int *lpStartDriver = (int *)lpContext; 

    /*
     * Decide which device description to consult. The driver is either
     * hardware (HW) or software. Software drivers provide support
     * through the hardware emulation layer (HEL).
     */
    lpDesc = lpHWDesc->dcmColorModel ? lpHWDesc : lpHELDesc;
    /*
     * If this driver cannot render in the current display bit depth,
     * return D3DENUMRET_OK to skip this driver. Enumeration continues
     * automatically as D3D calls enumDeviceFunc again for the next
     * driver.
     */
    if (!(lpDesc->dwDeviceRenderBitDepth & myglobs.BPP)) 
        return D3DENUMRET_OK;
    /*
     * Record this driver's GUID and name.
     */
    memcpy(&myglobs.DriverGUID[myglobs.NumDrivers], lpGuid, sizeof(GUID));
    lstrcpy(&myglobs.DriverName[myglobs.NumDrivers][0], lpDeviceName);
    /*
     * Choose hardware over software, and color lights over monochrome
     * lights.
     */
    if (*lpStartDriver == (int)-1) {
        /*
         * This is the first valid driver, so record whether this driver
         * is a hardware driver or not, and whether it is limited to 
         * monochrome lights or not.
         */
        myglobs.CurrDriver = myglobs.NumDrivers;
        hardware = lpDesc == lpHWDesc ? TRUE : FALSE;
        mono = lpDesc->dcmColorModel & D3DCOLOR_MONO ? TRUE : FALSE;
    } else if (lpDesc == lpHWDesc && !hardware) {
        /*
         * If this driver is a hardware driver and the start driver is not,
         * then make this driver the new preferred driver and record
         * its hardware and mono capabilities. The next time D3D calls 
         * enumDeviceFunc, that driver will be compared against this 
         * new start driver.
         */
        myglobs.CurrDriver = myglobs.NumDrivers;
        hardware = lpDesc == lpHWDesc ? TRUE : FALSE;
        mono = lpDesc->dcmColorModel & D3DCOLOR_MONO ? TRUE : FALSE;
    } else if ((lpDesc == lpHWDesc && hardware ) || (lpDesc == lpHELDesc
                                                     && !hardware)) {
        if (lpDesc->dcmColorModel == D3DCOLOR_RGB && mono) {
            /*
             * If this driver and the start driver are the same type 
             * (both hardware or both software) and this driver is 
             * color while the start driver is not, then make this driver
             * the new preferred driver and record its capabilities.
             * The next time D3D calls enumDeviceFunc, that driver will 
             * be compared against this new start driver.
             */
            myglobs.CurrDriver = myglobs.NumDrivers;
            hardware = lpDesc == lpHWDesc ? TRUE : FALSE;
            mono = lpDesc->dcmColorModel & D3DCOLOR_MONO ? TRUE : FALSE;
        }
    }
    /* 
     * Increment the current driver number in preparation for the next
     * driver. 
     */
    myglobs.NumDrivers++; 

    /* Maximum number of drivers reached, stop enumeration. */
    if (myglobs.NumDrivers == MAX_DRIVERS)
        return (D3DENUMRET_CANCEL);
    /* 
     * Continue enumerating drivers. D3D will call enumDeviceFunc
     * again with information for the next driver. 
     */
    return (D3DENUMRET_OK);
}


/************************************************************************/
/*                    Change Driver                                     */
/************************************************************************/
/*
 * Release the current device and create the new one.
 */
void ChangeDriver(HWND win, WPARAM wparam)
{
    HRESULT rval;
    RECT rc;

    /* 
     * Save the previous driver selection as LastDriver.
     */
    int LastDriver = myglobs.CurrDriver;

    /*
     * Globals are no longer correct since the selection is changing.
     */
    myglobs.bInitialized = FALSE;

    /* 
     * Release the current device.
     */ 
    SAFE_RELEASE(myglobs.dev);   

    /* 
     * Set the current driver to the driver selected through the menu.
     */
    myglobs.CurrDriver = LOWORD(wparam)-MENU_FIRST_DRIVER;

    /* 
     * Obtain the window rectangle and create the new current driver
     * for that window using the global DirectDrawClipper object.
     */
    GetClientRect(win, &rc);

    rval = lpD3DRM->CreateDeviceFromClipper(lpDDClipper, 
                             &myglobs.DriverGUID[myglobs.CurrDriver],
                             rc.right, rc.bottom, &myglobs.dev);

    /* 
     * If an error occurred, try to recreate the previous driver.
     */
    if (rval) {
        myglobs.CurrDriver = LastDriver;

        rval = lpD3DRM->CreateDeviceFromClipper(lpDDClipper, 
                                 &myglobs.DriverGUID[myglobs.CurrDriver],
                                 rc.right, rc.bottom, &myglobs.dev);
        if (rval) {
            Msg("Failed to create the D3DRM device.\n%s", 
                D3DRMErrorToString(rval));
            CleanUpAndQuit();
        }
        else {
            Msg("There was not enough video memory available to use the \
            	 3D accelerated hardware device.\nRestoring old software \
            	 device.");
            myglobs.bInitialized = TRUE;
        }
    }
    else {
        /* 
         * Globals are properly initialized again.
         */
        myglobs.bInitialized = TRUE;
    }
}


/************************************************************************/
/*                          CleanUpAndQuit                              */
/************************************************************************/
/*
 * CleanUpAndQuit
 * Release all D3DRM objects and set the bQuit flag.
 */
void CleanUpAndQuit(void)
{
    myglobs.bInitialized = FALSE;
    SAFE_RELEASE(myglobs.dev);
    SAFE_RELEASE(lpD3DRM);
    SAFE_RELEASE(lpDDClipper);
    myglobs.bQuit = TRUE;
}


/************************************************************************/
/*                        Standard Windows functions                    */
/************************************************************************/
/*                   InitApp, WinMain, AppAbout, WindowProc             */
/************************************************************************/
/*
 * InitApp
 * Creates window and initializes objects. 
 */
static HWND
InitApp(HINSTANCE this_inst, int cmdshow)
{
    HWND win;     /* Main window handle */
    WNDCLASS wc;

    /*
     * Set up and register the window class.
     */
    wc.style = CS_HREDRAW | CS_VREDRAW;
    wc.lpfnWndProc = WindowProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = sizeof(DWORD);
    wc.hInstance = this_inst;
    wc.hIcon = LoadIcon(this_inst, "AppIcon");
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
    wc.lpszMenuName = "AppMenu";
    wc.lpszClassName = "D3DRM Example";
    if (!RegisterClass(&wc))
        return FALSE;

    /*
     * Create the window.
     */
    win =
        CreateWindow
        (   "D3DRM Example",              /* class */
            "RMEnum Direct3DRM Example",  /* caption */
            WS_OVERLAPPEDWINDOW,          /* style */
            CW_USEDEFAULT,                /* init. x pos */
            CW_USEDEFAULT,                /* init. y pos */
            300,                          /* init. x size */
            300,                          /* init. y size */
            NULL,                         /* parent window */
            NULL,                         /* menu handle */
            this_inst,                    /* program handle */
            NULL                          /* create parms */
        );
    if (!win)
        return FALSE;

    /*
     * Initialize global variables, enumerate devices, and create the 
     * D3DRM objects.
     */
    if (!CreateObjects(win))
        return FALSE;

    /*
     * Display the window.
     */
    ShowWindow(win, cmdshow);
    UpdateWindow(win);

    return win;
}

/************************************************************************/
/*                               WinMain                                */
/************************************************************************/
/*
 * Initialize the application.
 */
int APIENTRY
WinMain (HINSTANCE this_inst, 
         HINSTANCE prev_inst, 
         LPSTR cmdline, 
         int cmdshow)
{
    HWND    hwnd;
    MSG     msg;
    HACCEL  accel;
    prev_inst;
    cmdline;

    /*
     * Create the window and initialize objects. 
     */
    if (!(hwnd = InitApp(this_inst, cmdshow)))
        return 1;

    accel = LoadAccelerators(this_inst, "AppAccel");

    while (!myglobs.bQuit) {
        /* 
         * Monitor the message queue until there are no pressing
         * messages.
         */
        while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
            if (msg.message == WM_QUIT) {
                CleanUpAndQuit();
                break;
            }
            if (!TranslateAccelerator(msg.hwnd, accel, &msg)) {
                TranslateMessage(&msg);
                DispatchMessage(&msg);
            }
        }
    if (myglobs.bQuit)
        break;
    WaitMessage();
    }
    DestroyWindow(hwnd);
    return msg.wParam;
}


/************************************************************************/
/*                    Windows Message Handlers                          */
/************************************************************************/
/*
 * AppAbout
 * About box message handler
 */
LRESULT CALLBACK AppAbout(HWND hwnd, 
                          UINT msg, 
                          WPARAM wparam, 
                          LPARAM lParam)
{
  switch (msg)
  {
    case WM_COMMAND:
      if (LOWORD(wparam) == IDOK)
        EndDialog(hwnd, TRUE);
      break;

    case WM_INITDIALOG:
      return TRUE;
  }
  return FALSE;
}

/*
 * WindowProc
 * Main window message handler
 */
LRESULT CALLBACK WindowProc(HWND win, 
                            UINT msg, 
                            WPARAM wparam, 
                            LPARAM lparam)
{
    int i;

    if (!myglobs.bInitialized) {
        return DefWindowProc(win, msg, wparam, lparam);
    }

    switch (msg)    {
        case WM_INITMENUPOPUP:
            /*
             * Check and enable the appropriate driver menu items.
             */
            for (i = 0; i < myglobs.NumDrivers; i++) {
                CheckMenuItem((HMENU)wparam, MENU_FIRST_DRIVER + i,
                              (i == myglobs.CurrDriver) ? 
                                  MF_CHECKED : MF_UNCHECKED);
            }
            break;
        case WM_COMMAND:
            switch(LOWORD(wparam)) {
                case MENU_ABOUT:
                    DialogBox((HINSTANCE)GetWindowLong(win, GWL_HINSTANCE),
                              "AppAbout", win, (DLGPROC)AppAbout);
                    break;
                case MENU_EXIT:
                    CleanUpAndQuit();
                    break;
                }

                /*
                 * Changing the D3D Driver in response to a driver menu 
                 * choice.
                 */
                if (LOWORD(wparam) >= MENU_FIRST_DRIVER &&
                    LOWORD(wparam) < MENU_FIRST_DRIVER + MAX_DRIVERS &&
                    myglobs.CurrDriver != LOWORD(wparam) - 
                                              MENU_FIRST_DRIVER) {
                    /*
                     * Release the current viewport and device and create
                     * the new one.
                     */
                    ChangeDriver(win, wparam);
                }
            break;

    case WM_DESTROY:
        CleanUpAndQuit();
        break;

    default:
        return DefWindowProc(win, msg, wparam, lparam);
    }
    return 0L;
}

