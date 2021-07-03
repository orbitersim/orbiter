//-----------------------------------------------------------------------------
// File: ModeTest.cpp
//
// Desc: This example demonstrates basic usage of the IDirectDraw7::StartModeTest
//   and IDirectDraw7::EvaluateMode methods. Together, these methods allow an
//   application to explore what display modes and refresh rates the monitor
//   connected to this display device is able to display, through a manual
//   user-controlled process. The application will present the UI that asks
//   the user if the current mode under test is being displayed correctly by
//   the monitor.
//
//   Applications should use these methods when they are interested in using
//   higher refresh rates.
//
//   The basic idea is that DirectDraw will setup a list of modes to be tested
//   (based on the list the app passed in), and then sequentially test them
//   under application control. The app starts the test process, and then
//   calls IDirectDraw7::EvaluateMode continuously. DirectDraw will take care
//   of setting the modes. All the app has to do is SetCooperativeLevel
//   beforehand, and then handle surface loss and drawing the UI that asks the
//   user if they can see the current mode under test. DirectDraw returns
//   enough information from IDirectDraw7::EvaluateMode to allow the app to
//   know when to do these things, and when to stop testing. The app can pass
//   a flag to IDirectDraw7::EvaluateMode if the user happened to say they
//   could see the mode corretly, which will cause DirectDraw to mark the mode
//   as good and move on. DirectDraw may also decide that time as run out and
//   give up on a certain mode.
//
//   DirectDraw uses information at its disposal from any automated means to
//   make the testing process as short as possible, and applications only need
//   to test modes they are interested in.
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <windows.h>
#include <stdio.h>
#include <commdlg.h>
#include <initguid.h>
#include <ddraw.h>
#include "resource.h"




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
BOOL CALLBACK MainDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam );
HRESULT GetDirectDrawDevices();
VOID    OnInitDialog();

HRESULT ResetDeviceModes( DWORD dwDeviceIndex );
HRESULT UpdateModesListBox( DWORD dwDeviceIndex );

BOOL    WINAPI DDEnumCallbackEx( GUID*, LPSTR, LPSTR, LPVOID, HMONITOR );
HRESULT WINAPI EnumModesCallback( LPDDSURFACEDESC pddsd,  LPVOID pContext );
HRESULT WINAPI EnumAllModesCallback( LPDDSURFACEDESC2 pddsd, LPVOID pContext );

HRESULT OnModeTest();
HRESULT PerformDirectDrawModeTest( LPDIRECTDRAW7 pDD, SIZE* aTestModes, 
                                   DWORD dwTestModes );




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }

struct DDRAW_DEVICE_STRUCT
{
    GUID  guid;
    CHAR  strDescription[256];
    CHAR  strDriverName[64];
    DWORD dwModeCount;
    SIZE  aModeSize[256];
};

HWND                g_hDlg  = NULL;
LPDIRECTDRAW        g_pDD   = NULL;

DDRAW_DEVICE_STRUCT g_aDevices[16];
DWORD               g_dwDeviceCount;




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point for the application.  Since we use a simple dialog for 
//       user interaction we don't need to pump messages.
//-----------------------------------------------------------------------------
INT APIENTRY WinMain( HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR pCmdLine, 
                      INT nCmdShow )
{
    if( FAILED( GetDirectDrawDevices() ) )
        return FALSE;

    // Display the main dialog box.
    DialogBox( hInst, MAKEINTRESOURCE(IDD_MAIN), NULL, MainDlgProc );

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: MainDlgProc()
// Desc: Handles dialog messages
//-----------------------------------------------------------------------------
BOOL CALLBACK MainDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
    switch( msg ) 
    {
        case WM_INITDIALOG:
            // Store HWND in global
            g_hDlg = hDlg;

            OnInitDialog();
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDCANCEL:
                    EndDialog( hDlg, IDCANCEL );
                    break;

                case IDC_TEST:
                    if( FAILED( OnModeTest() ) )
                    {
                        MessageBox( g_hDlg, "Error doing starting mode test. "
                                    "Sample will now exit.", "DirectDraw Sample", 
                                    MB_OK | MB_ICONERROR );
                        EndDialog( g_hDlg, IDABORT );
                    }
                    break;

                case IDC_RESET:
                    HWND hWndDeviceList;
                    DWORD dwDeviceIndex;

                    // Get the currently selected DirectDraw device
                    hWndDeviceList = GetDlgItem( hDlg, IDC_DDRAW_DEVICE_LIST );
                    dwDeviceIndex = SendMessage( hWndDeviceList, LB_GETCURSEL, 0, 0 );

                    // Reset the modes for it
                    if( FAILED( ResetDeviceModes( dwDeviceIndex ) ) )
                    {
                        MessageBox( g_hDlg, "Error reset DirectDraw device. "
                                    "Sample will now exit.", "DirectDraw Sample", 
                                    MB_OK | MB_ICONERROR );
                        EndDialog( g_hDlg, IDABORT );
                    }
                    break;

                case IDC_DDRAW_DEVICE_LIST:
                    switch( HIWORD(wParam) )
                    {
                        case LBN_SELCHANGE:
                            // Get the currently selected DirectDraw device
                            DWORD dwDeviceIndex;
                            dwDeviceIndex = SendMessage( (HWND) lParam, 
                                                         LB_GETCURSEL, 0, 0 );

                            // Update the list boxes using it 
                            if( FAILED( UpdateModesListBox( dwDeviceIndex ) ) )
                            {
                                MessageBox( g_hDlg, "Error enumerating DirectDraw modes."
                                            "Sample will now exit.", "DirectDraw Sample", 
                                            MB_OK | MB_ICONERROR );
                                EndDialog( g_hDlg, IDABORT );
                            }

                            break;

                        default: 
                            return FALSE;
                    }
                    break;

                default:
                    return FALSE; // Didn't handle message
            }
            break;

        default:
            return FALSE; // Didn't handle message
    }

    return TRUE; // Handled message
}




//-----------------------------------------------------------------------------
// Name: GetDirectDrawDevices()
// Desc: Retrieves all available DirectDraw devices and stores the information
//       in g_aDevices[]
//-----------------------------------------------------------------------------
HRESULT GetDirectDrawDevices()
{
    return DirectDrawEnumerateEx( DDEnumCallbackEx, 
                                  NULL,
                                  DDENUM_ATTACHEDSECONDARYDEVICES |
                                  DDENUM_DETACHEDSECONDARYDEVICES |
                                  DDENUM_NONDISPLAYDEVICES );
}




//-----------------------------------------------------------------------------
// Name: DDEnumCallbackEx()
// Desc: Enumerates all available DirectDraw devices
//-----------------------------------------------------------------------------
BOOL WINAPI DDEnumCallbackEx( GUID* pGUID,    
                              LPSTR strDriverDescription, 
                              LPSTR strDriverName,        
                              LPVOID pContext,           
                              HMONITOR hm )       
{
    HRESULT hr;
    LPDIRECTDRAW pDD = NULL;
   
    // Create a DirectDraw device using the enumerated guid 
    hr = DirectDrawCreateEx( pGUID, (VOID**)&pDD, IID_IDirectDraw7, NULL );

    if( SUCCEEDED(hr) )
    {
        if( pGUID )
        {
            // Add it to the global storage structure
            g_aDevices[ g_dwDeviceCount ].guid = *pGUID;
        }
        else
        {
            // Clear the guid from the global storage structure
            ZeroMemory( &g_aDevices[ g_dwDeviceCount ].guid, 
                        sizeof(GUID) );
        }

        // Copy the description of the driver into the structure
        lstrcpyn( g_aDevices[ g_dwDeviceCount ].strDescription, 
                  strDriverDescription, 256 );
        lstrcpyn( g_aDevices[ g_dwDeviceCount ].strDriverName, 
                  strDriverName, 64 );

        // Retrive the modes this device can support
        g_aDevices[ g_dwDeviceCount ].dwModeCount = 0;
        hr = pDD->EnumDisplayModes( 0, NULL, NULL, EnumModesCallback );
    
        // Increase the counter for the number of devices found
        g_dwDeviceCount++;

        // Release this device 
        SAFE_RELEASE( pDD );
    }

    // Continue looking for more devices
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: EnumModesCallback()
// Desc: Enumerates the available modes for the device from which 
//       EnumDisplayModes() was called.  It records the unique mode sizes in 
//       the g_aDevices[g_dwDeviceCount].aModeSize array
//-----------------------------------------------------------------------------
HRESULT WINAPI EnumModesCallback( LPDDSURFACEDESC pddsd,  
                                  LPVOID pContext )
{
    DWORD i;
    DWORD dwModeSizeX;
    DWORD dwModeSizeY;
    DWORD dwModeCount;

    // For each mode, look through all previously found modes
    // to see if this mode has already been added to the list
    dwModeCount = g_aDevices[ g_dwDeviceCount ].dwModeCount;

    for( i = 0; i < dwModeCount; i ++ )
    {
        dwModeSizeX = g_aDevices[ g_dwDeviceCount ].aModeSize[i].cx;
        dwModeSizeY = g_aDevices[ g_dwDeviceCount ].aModeSize[i].cy;

        if ( ( dwModeSizeX == pddsd->dwWidth ) &&
             ( dwModeSizeY == pddsd->dwHeight ) )
        {
            // If this mode has been added, then stop looking
            break;
        }
    }

    // If this mode was not in g_aDevices[g_dwDeviceCount].aModeSize[]
    // then added it. 
    if( i == g_aDevices[ g_dwDeviceCount ].dwModeCount )
    {
        g_aDevices[ g_dwDeviceCount ].aModeSize[i].cx = pddsd->dwWidth;
        g_aDevices[ g_dwDeviceCount ].aModeSize[i].cy = pddsd->dwHeight;

        // Increase the number of modes found for this device
        g_aDevices[ g_dwDeviceCount ].dwModeCount++;
    }

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: OnInitDialog()
// Desc: Initializes the dialogs (sets up UI controls, etc.)
//-----------------------------------------------------------------------------
VOID OnInitDialog()
{
    // Load the icon
    HINSTANCE hInst = (HINSTANCE) GetWindowLong( g_hDlg, GWL_HINSTANCE );
    HICON hIcon = LoadIcon( hInst, MAKEINTRESOURCE( IDI_ICON1 ) );

    // Set the icon for this dialog.
    PostMessage( g_hDlg, WM_SETICON, ICON_BIG,   (LPARAM) hIcon );  // Set big icon
    PostMessage( g_hDlg, WM_SETICON, ICON_SMALL, (LPARAM) hIcon );  // Set small icon

    // Show all available DirectDraw devices in the listbox
    HWND hWndDeviceList = GetDlgItem( g_hDlg, IDC_DDRAW_DEVICE_LIST );
    for( UINT i = 0; i < g_dwDeviceCount; i++ )
    {
        SendMessage( hWndDeviceList, LB_ADDSTRING, 0,
            (LPARAM) g_aDevices[i].strDescription );
        SendMessage( hWndDeviceList, LB_SETITEMDATA, i, (LPARAM) i);
    }

    // Select the first device by default
    DWORD dwCurrentSelect = 0;

    SendMessage( hWndDeviceList, LB_SETCURSEL, dwCurrentSelect, 0);
    if( FAILED( UpdateModesListBox( dwCurrentSelect ) ) )
    {
        MessageBox( g_hDlg, "Error enumerating DirectDraw modes."
                    "Sample will now exit.", "DirectDraw Sample", 
                    MB_OK | MB_ICONERROR );
        EndDialog( g_hDlg, IDABORT );
    }

    SetFocus( hWndDeviceList );
}




//-----------------------------------------------------------------------------
// Name: ResetDeviceModes()
// Desc: If the user makes a mistake, and accidently says YES when a mode 
//       cannot be seen, or NO when a mode really can be seen (thus ending up 
//       with a lower refresh rate than possible) this allows the user to reset 
//       the test results and try again.
//-----------------------------------------------------------------------------
HRESULT ResetDeviceModes( DWORD dwDeviceIndex )
{
    LPDIRECTDRAW7 pDD = NULL;
    HRESULT       hr;    

    // Create a DirectDraw device based using the selected device guid
    hr = DirectDrawCreateEx( &g_aDevices[dwDeviceIndex].guid, 
                             (VOID**) &pDD, IID_IDirectDraw7, NULL);

    if( SUCCEEDED(hr) )
    {
        // Set the cooperative level to normal
        if( SUCCEEDED( hr = pDD->SetCooperativeLevel( g_hDlg, DDSCL_NORMAL ) ) )
        {
            // Clear the previous mode tests
            //
            // We ignore the return code, since we would do nothing different on error returns.
            // Note that most applications would never need to call StartModeTest this way.
            // The reset functionality is intended to be used when a user accidentally accepted
            // a mode that didn't actually display correctly.
            pDD->StartModeTest( NULL, 0, 0 );
        }

        // Release this device
        SAFE_RELEASE( pDD ); 
    }

    hr = UpdateModesListBox( dwDeviceIndex );

    return hr;
}




//-----------------------------------------------------------------------------
// Name: UpdateModesListBox()
// Desc: Updates the "modes to test" and "all modes" list boxes
//-----------------------------------------------------------------------------
HRESULT UpdateModesListBox( DWORD dwDeviceIndex )
{
    LPDIRECTDRAW7 pDD = NULL;
    HRESULT       hr;    

    HWND hWndModesToTest = GetDlgItem( g_hDlg, IDC_TEST_MODES_LIST );
    SendMessage( hWndModesToTest, LB_RESETCONTENT, 0, 0 );

    // Update the "modes to test" list box based on the display device selected
    for( DWORD i = 0; i < g_aDevices[dwDeviceIndex].dwModeCount; i++ )
    {
        CHAR strMode[64];

        // Make a string based on the this mode's size
        sprintf( strMode, TEXT("%u x %u"),
                 g_aDevices[dwDeviceIndex].aModeSize[i].cx, 
                 g_aDevices[dwDeviceIndex].aModeSize[i].cy );

        // Add it to the list box
        SendMessage( hWndModesToTest, LB_ADDSTRING, 0, (LPARAM) strMode );
        SendMessage( hWndModesToTest, LB_SETITEMDATA, i, (LPARAM) i );
    }

    // Create a DirectDraw device based using the selected device guid
    if( SUCCEEDED( hr = DirectDrawCreateEx( &g_aDevices[dwDeviceIndex].guid, 
                                         (VOID**) &pDD, IID_IDirectDraw7, NULL) ) )
    {
        HWND hWndAllModes = GetDlgItem( g_hDlg, IDC_ALL_MODES_LIST );
        SendMessage( hWndAllModes, LB_RESETCONTENT, 0, 0 );

        // Enumerate and display all supported modes along
        // with supported bit depth, and refresh rates 
        // in the "All Modes" listbox
        hr = pDD->EnumDisplayModes( DDEDM_REFRESHRATES, NULL,
                                                (VOID*) hWndAllModes, 
                                                EnumAllModesCallback );

        // Release this device
        SAFE_RELEASE( pDD ); 
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: EnumAllModesCallback()
// Desc: For each mode enumerated, it adds it to the "All Modes" listbox.
//-----------------------------------------------------------------------------
HRESULT WINAPI EnumAllModesCallback( LPDDSURFACEDESC2 pddsd,  
                                     LPVOID pContext )
{
    CHAR strMode[64];
    HWND hWnd = (HWND) pContext;

    sprintf( strMode, TEXT("%ux%ux%u - %u Hz"),
             pddsd->dwWidth, 
             pddsd->dwHeight,
             pddsd->ddpfPixelFormat.dwRGBBitCount, 
             pddsd->dwRefreshRate );

    SendMessage( hWnd, LB_ADDSTRING, 0, (LPARAM) strMode );

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: OnModeTest()
// Desc: User hit the "Test" button
//-----------------------------------------------------------------------------
HRESULT OnModeTest() 
{
    HWND hWndModesToTest = GetDlgItem( g_hDlg, IDC_TEST_MODES_LIST );
    DWORD dwSelectCount = SendMessage( hWndModesToTest, LB_GETSELCOUNT, 0, 0 );

    if( dwSelectCount > 0 )
    {
        LPDIRECTDRAW7 pDD = NULL;
        HRESULT       hr;    
        HWND          hWndDeviceList;
        DWORD         dwDeviceIndex;

        // Get the currently selected DirectDraw device
        hWndDeviceList = GetDlgItem( g_hDlg, IDC_DDRAW_DEVICE_LIST );
        dwDeviceIndex = SendMessage( hWndDeviceList, LB_GETCURSEL, 0, 0 );

        // Create a DirectDraw device based using the selected device guid
        if( FAILED( hr = DirectDrawCreateEx( &g_aDevices[dwDeviceIndex].guid, 
                                            (VOID**) &pDD, IID_IDirectDraw7, NULL) ) )
            return hr;

        // This is a good usage of DDSCL_CREATEDEVICEWINDOW: DirectDraw will create a window that covers
        // the monitor, and won't mess around with our dialog box. Any mouse clicks on the cover window
        // will therefore not be received and misinterpreted by the dialog box, since such clicks will
        // be sent to DirectDraw's internal message procedure and therein ignored.

        if( FAILED( hr = pDD->SetCooperativeLevel( g_hDlg,
                                               DDSCL_EXCLUSIVE          | 
                                               DDSCL_FULLSCREEN         |
                                               DDSCL_CREATEDEVICEWINDOW | 
                                               DDSCL_SETFOCUSWINDOW ) ) )
        {
            SAFE_RELEASE( pDD ); 
            return hr;
        }



        SIZE aTestModes[256];
        DWORD dwTestModes = 0;

        // Find out which modes are selected, then just test those
        for( DWORD i = 0; i < g_aDevices[dwDeviceIndex].dwModeCount; i++ )
        {
            if( SendMessage( hWndModesToTest, LB_GETSEL, i, 0 ) )
            {
                // Record the selected modes in aTestModes[]
                aTestModes[dwTestModes] = g_aDevices[dwDeviceIndex].aModeSize[i];
                dwTestModes++;
            }
        }

        // Perform test on each of the selected modes on the selected device
        hr = PerformDirectDrawModeTest( pDD, aTestModes, dwTestModes );

        // Release this device
        SAFE_RELEASE( pDD ); 

        switch (hr)
        {
        case DDERR_NOMONITORINFORMATION:
            // No EDID data is present for the current monitor.
            MessageBox(g_hDlg,
                "The current monitor cannot be identified electronically.\n"
                "High refresh rates are not allowed on such monitors, so the test will not be performed.",
                "Testing Will Not Proceed", MB_OK | MB_ICONINFORMATION);
            break;
        case DDERR_NODRIVERSUPPORT:
            // The driver cannot support refresh rate testing.
            MessageBox(g_hDlg,
                "The driver does not support specific refresh rates. Test cannot be performed.",
                "Testing Cannot Proceed", MB_OK | MB_ICONINFORMATION);
            break;
        default:
            if( SUCCEEDED(hr) )
            {
                MessageBox( g_hDlg, TEXT("Mode test completed"), TEXT("Result"), MB_OK );
                break;
            }
            else
            {
                // A StartModeTest error occurred.
                MessageBox(g_hDlg,
                "StartModeTest returned an unexpected value when called with the DDSMT_ISTESTREQUIRED flag.",
                "StartModeTest Error", MB_OK | MB_ICONEXCLAMATION);
                return hr;
            }
        }

        // Update the mode list boxes based on the device selected
        if( FAILED( hr = UpdateModesListBox( dwDeviceIndex ) ) )
            return hr;
    }
    else
    {
        // There weren't any modes selected to test
        MessageBox( g_hDlg, 
                    TEXT("Select one or more modes to test from the list box"), 
                    TEXT("No modes selected"), MB_OK );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetupPrimarySurface()
// Desc: Setups a primary DirectDraw surface
//-----------------------------------------------------------------------------
HRESULT SetupPrimarySurface( LPDIRECTDRAW7 pDD, LPDIRECTDRAWSURFACE7* ppDDS )
{
    DDSURFACEDESC2 ddsd;
 
    ZeroMemory( &ddsd, sizeof(ddsd) );

    ddsd.dwSize         = sizeof(ddsd);
    ddsd.dwFlags        = DDSD_CAPS;
    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;

    return pDD->CreateSurface(&ddsd, ppDDS, NULL);
}




//-----------------------------------------------------------------------------
// Name: UpdatePrimarySurface()
// Desc: Fills the primary surface with white, and diplays the timeout value
//       on screen
//-----------------------------------------------------------------------------
HRESULT UpdatePrimarySurface( LPDIRECTDRAWSURFACE7 pDDS, DWORD dwTimeout )
{
    DDBLTFX ddbltfx;
    HDC     hDC;
    char    strTimeout[128];
    RECT    rect;
    HRESULT hr;
  
    // Clear the screen:  
    ZeroMemory( &ddbltfx, sizeof(ddbltfx) );
    ddbltfx.dwSize      = sizeof(ddbltfx);
    ddbltfx.dwFillColor = 0xFFFFFFFF;

    hr = pDDS->Blt( NULL, NULL, NULL, DDBLT_COLORFILL | DDBLT_WAIT, &ddbltfx );
    if( FAILED( hr ) )
        return hr;

    // Display the timeout value 
    if( FAILED( hr = pDDS->GetDC( &hDC ) ) )
        return hr;

    GetWindowRect( g_hDlg, &rect );
    wsprintf( strTimeout, TEXT("Press space to accept or escape to reject. ")
              TEXT("%2d seconds until timeout"), dwTimeout );
    DrawText( hDC, strTimeout, strlen(strTimeout), &rect, DT_CENTER | DT_VCENTER | DT_SINGLELINE );

    // Cleanup
    pDDS->ReleaseDC( hDC );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: PerformDirectDrawModeTest()
// Desc: Perform the IDirectDraw7::StartModeTest and IDirectDraw7::EvaluateMode
//       tests
// Returns: S_OK if no modes needed testing, or all modes tested successfully,
//          informative error code otherwise.
//-----------------------------------------------------------------------------
HRESULT PerformDirectDrawModeTest( LPDIRECTDRAW7 pDD, SIZE* aTestModes, 
                                   DWORD dwTestModes )
{
    LPDIRECTDRAWSURFACE7 pDDSPrimary = NULL;
    HRESULT hr;
    MSG     msg;
    DWORD   dwFlags = 0;
    DWORD   dwTimeout;
    BOOL    bMsgReady;

    // First call StartModeTest with the DDSMT_ISTESTREQUIRED flag to determine
    // whether the tests can be performed and need to be performed.
    hr = pDD->StartModeTest( aTestModes, dwTestModes, DDSMT_ISTESTREQUIRED);

    switch (hr)
    {
    case DDERR_NEWMODE:
        // DDERR_NEWMODE means that there are modes that need testing.
        break;
    case DDERR_TESTFINISHED:
        // DDERR_TESTFINISHED means that all the modes that we wish to test have already been tested correctly
        return S_OK;
    default:
        //Possible return codes here include DDERR_NOMONITORINFORMATION or DDERR_NODRIVERSUPPORT or
        //other fatal error codes (DDERR_INVALIDPARAMS, DDERR_NOEXCLUSIVEMODE, etc.)
        return hr;
    }

    hr = pDD->StartModeTest( aTestModes, dwTestModes, 0 );
    if( hr == DDERR_TESTFINISHED )
    {
        // The tests completed early, so return
        return S_OK;
    }

    // Create the primary DirectDraw surface
    if( FAILED( SetupPrimarySurface( pDD, &pDDSPrimary ) ) )
        return hr;

    // Loop until the mode tests are complete
    while( TRUE )
    {
        bMsgReady = PeekMessage(&msg, NULL, 0, 0, PM_REMOVE);

        if( bMsgReady )
        {
            if (msg.message == WM_KEYDOWN)
            {
                switch (msg.wParam)
                {
                case VK_SPACE:
                    dwFlags = DDEM_MODEPASSED;
                    break;

                case VK_ESCAPE:
                    dwFlags = DDEM_MODEFAILED;
                    break;
                }
            }
            else
            {
                TranslateMessage( &msg );
                DispatchMessage( &msg );
            }
        }
        else
        {
            // This method will only succeed with monitors that contain EDID data. 
            // If the monitor is not EDID compliant, then the method will return 
            // DDERR_TESTFINISHED without testing any modes. If the EDID table does not 
            // contain values higher than 60hz, no modes will tested. Refresh rates 
            // higher than 100 hz will only be tested if the EDID table contains values 
            // higher than 85hz.
            hr = pDD->EvaluateMode(dwFlags, &dwTimeout);

            if( hr == DD_OK )
            {
                if( pDDSPrimary )
                {
                    // Clear the screen, and display the timeout value
                    UpdatePrimarySurface( pDDSPrimary, dwTimeout );
                }
            }
            else if( hr == DDERR_NEWMODE )
            {
                // Cleanup the last DirectDraw surface, and create
                // a new one for the new mode
                SAFE_RELEASE( pDDSPrimary );

                if( FAILED( SetupPrimarySurface( pDD, &pDDSPrimary ) ) )
                    return hr;

                dwFlags = 0;
            }
            else if( hr == DDERR_TESTFINISHED )
            {
                // Test complete, so stop looping
                break;
            }

            Sleep( 100 );
        }
    }

    // Cleanup
    SAFE_RELEASE( pDDSPrimary );

    return S_OK;
}


