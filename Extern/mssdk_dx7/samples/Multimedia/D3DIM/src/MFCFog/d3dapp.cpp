//-----------------------------------------------------------------------------
// File: D3DApp.cpp
//
// Desc: Main file for the D3DIM fog app that uses MFC.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include "stdafx.h"
#include <stdio.h>
#include <mmsystem.h>
#include "D3DEnum.h"
#include "D3DFrame.h"
#include "D3DUtil.h"
#include "D3DApp.h"


//-----------------------------------------------------------------------------
// External function-prototypes
//-----------------------------------------------------------------------------
HRESULT App_ConfirmDevice( DDCAPS*, D3DDEVICEDESC7* );
HRESULT App_OneTimeSceneInit();
HRESULT App_InitDeviceObjects( HWND, LPDIRECT3DDEVICE7 );
VOID    App_DeleteDeviceObjects( HWND, LPDIRECT3DDEVICE7 );
HRESULT App_FrameMove( LPDIRECT3DDEVICE7, FLOAT );
HRESULT App_Render( LPDIRECT3DDEVICE7 );
HRESULT App_RestoreSurfaces();
HRESULT App_FinalCleanup();
VOID    App_SetFogParameters( BOOL, DWORD, DWORD, BOOL, BOOL, FLOAT, FLOAT,
                              FLOAT );


enum    APPMSGTYPE { MSG_NONE, MSGERR_APPMUSTEXIT,
                     MSGWARN_SWITCHTOSOFTWARE, MSGWARN_CANTDOFULLSCREEN };
VOID    DisplayFrameworkError( HRESULT hr, APPMSGTYPE errType );




//-----------------------------------------------------------------------------
// The MFC macros are all listed here
//-----------------------------------------------------------------------------
IMPLEMENT_DYNCREATE( CAppDoc,      CDocument )
IMPLEMENT_DYNCREATE( CAppFrameWnd, CFrameWnd )
IMPLEMENT_DYNCREATE( CAppForm,     CFormView )


BEGIN_MESSAGE_MAP( CAppFrameWnd, CFrameWnd )
    ON_WM_MOVE()
END_MESSAGE_MAP()


BEGIN_MESSAGE_MAP( CD3DApp, CWinApp )
    //{{AFX_MSG_MAP(CD3DApp)
    //}}AFX_MSG_MAP
END_MESSAGE_MAP()


BEGIN_MESSAGE_MAP( CAppForm, CFormView )
    //{{AFX_MSG_MAP(CAppForm)
    ON_COMMAND(    IDC_VIEWFULLSCREEN, OnToggleFullScreen )
    ON_COMMAND(    IDM_CHANGEDEVICE,   OnChangeDevice )
    ON_WM_HSCROLL()
    ON_BN_CLICKED( IDC_FOGCOLOR,       OnFogColor )
    ON_BN_CLICKED( IDC_RANGEBASEDFOG,  OnRangeBasedFog )
    ON_BN_CLICKED( IDC_VERTEXFOG,      OnVertexFog )
    ON_BN_CLICKED( IDC_TABLEFOG,       OnTableFog )
    ON_BN_CLICKED( IDC_LINEARFOGMODE,  OnFogMode )
    ON_BN_CLICKED( IDC_LORESTERRAIN,   OnTerrainResolution)
    ON_BN_CLICKED( IDM_CHANGEDEVICE,   OnChangeDevice )
    ON_BN_CLICKED( IDC_EXPFOGMODE,     OnFogMode )
    ON_BN_CLICKED( IDC_EXP2FOGMODE,    OnFogMode )
    ON_BN_CLICKED( IDC_HIRESTERRAIN,   OnTerrainResolution)
    //}}AFX_MSG_MAP
END_MESSAGE_MAP()




//-----------------------------------------------------------------------------
// Global data and objects
//-----------------------------------------------------------------------------
CD3DApp       g_D3DApp;
CAppForm*     g_AppFormView = NULL;
extern TCHAR* g_strAppTitle;




//-----------------------------------------------------------------------------
// Name: FullScreenWndProc()
// Desc: The WndProc funtion used when the app is in fullscreen mode. This is
//       needed simply to trap the ESC key.
//-----------------------------------------------------------------------------
LRESULT CALLBACK FullScreenWndProc( HWND hWnd, UINT msg, WPARAM wParam,
                                    LPARAM lParam )
{
    if( msg == WM_CLOSE )
    {
        // User wants to exit, so go back to windowed mode and exit for real
        g_AppFormView->GoWindowed();
        g_D3DApp.GetMainWnd()->PostMessage( WM_CLOSE, 0, 0 );
    }

    if( msg == WM_SETCURSOR )
    {
        SetCursor( NULL );
    }

    if( msg == WM_KEYUP && wParam == VK_ESCAPE )
    {
        // User wants to leave fullscreen mode
        g_AppFormView->GoWindowed();
    }

    return DefWindowProc( hWnd, msg, wParam, lParam );
}




//-----------------------------------------------------------------------------
// Name: CD3DApp()
// Desc: Constructor for the app, if needed
//-----------------------------------------------------------------------------
CD3DApp::CD3DApp()
{
}




//-----------------------------------------------------------------------------
// Name: InitInstance()
// Desc: This is the main entry point for the application. The MFC window stuff
//       is initialized here. See also the main initialization routine for the
//       CAppForm class, which is called indirectly from here.
//-----------------------------------------------------------------------------
BOOL CD3DApp::InitInstance()
{
    // Asscociate the MFC app with the frame window and doc/view classes
    AddDocTemplate( new CSingleDocTemplate( IDR_MAINFRAME, 
                                            RUNTIME_CLASS(CAppDoc),
                                            RUNTIME_CLASS(CAppFrameWnd),
                                            RUNTIME_CLASS(CAppForm) ) );

    // Dispatch commands specified on the command line (req'd by MFC). This
    // also initializes the the CAppDoc, CAppFrameWnd, and CAppForm classes.
    CCommandLineInfo cmdInfo;
    ParseCommandLine( cmdInfo );
    if( !ProcessShellCommand( cmdInfo ) )
        return FALSE;

    // The formview window has been initialized, so show and update it.
    m_pMainWnd->MoveWindow( 0, 0, 590, 370, TRUE );
    m_pMainWnd->SetWindowText( g_strAppTitle );
    m_pMainWnd->UpdateWindow();

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: OnIdle()
// Desc: Uses idle time to render the 3D scene.
//-----------------------------------------------------------------------------
BOOL CD3DApp::OnIdle( LONG )
{
    // Do not render if the app is minimized
    if( m_pMainWnd->IsIconic() )
        return FALSE;

    // Update and render a frame
    g_AppFormView->Render3DEnvironment();

    // Keep requesting more idle time
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: PreCreateWindow()
// Desc: Change the window style (so it cannot maximize or be sized) before
//       the main frame window is created.
//-----------------------------------------------------------------------------
BOOL CAppFrameWnd::PreCreateWindow( CREATESTRUCT& cs )
{
    cs.style = WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX;

    return CFrameWnd::PreCreateWindow( cs );
}




//-----------------------------------------------------------------------------
// Name: OnMove()
// Desc: Trap move messages so we can tell the appform where to render the 3D
//       scene.
//-----------------------------------------------------------------------------
VOID CAppFrameWnd::OnMove( int x, int y )
{
    CFrameWnd::OnMove( x, y );

    g_AppFormView->Move( x, y );
}




//-----------------------------------------------------------------------------
// Name: CAppForm()
// Desc: Constructor for the dialog resource form
//-----------------------------------------------------------------------------
CAppForm::CAppForm()
         :CFormView( IDD_FORMVIEW )
{
    m_pFramework            = NULL;
    m_bReady                = FALSE;
    m_bWindowed             = TRUE;
    m_pDeviceInfo           = NULL;
    m_pFullScreenDeviceInfo = NULL;

    m_bHiResTerrain         = FALSE;
    m_fFogStart             = 0.0f;
    m_fFogEnd               = 1.0f;
    m_fFogDensity           = 0.0f;
    m_bRangeBasedFog        = FALSE;
    m_bUsingTableFog        = FALSE;
    m_dwFogMode             = D3DFOG_LINEAR;
    m_dwFogColor            = 0x00b5b5ff;
    g_AppFormView           = this;
}




//-----------------------------------------------------------------------------
// Name: ~CAppForm()
// Desc: Destructor for the dialog resource form. Shuts down the app
//-----------------------------------------------------------------------------
CAppForm::~CAppForm()
{
    Cleanup3DEnvironment();
}




//-----------------------------------------------------------------------------
// Name: OnInitialUpdate()
// Desc: When the AppForm object is created, this function is called to
//       initialize it. Here we getting access ptrs to some of the controls,
//       and setting the initial state of some of them as well.
//-----------------------------------------------------------------------------
VOID CAppForm::OnInitialUpdate()
{
    HRESULT hr;

    m_bReady = FALSE;

    CFormView::OnInitialUpdate();
    ((CSliderCtrl*)GetDlgItem( IDC_FOGSTART_SLIDER ))->SetRange(0,100,TRUE);
    ((CSliderCtrl*)GetDlgItem( IDC_FOGSTART_SLIDER ))->SetPos(0);
    ((CSliderCtrl*)GetDlgItem( IDC_FOGEND_SLIDER ))->SetRange(0,100,TRUE);
    ((CSliderCtrl*)GetDlgItem( IDC_FOGEND_SLIDER ))->SetPos(100);
    ((CSliderCtrl*)GetDlgItem( IDC_FOGDENSITY_SLIDER ))->SetRange(0,100,TRUE);
    ((CSliderCtrl*)GetDlgItem( IDC_FOGDENSITY_SLIDER ))->SetPos(0);
    ((CButton*)GetDlgItem( IDC_LORESTERRAIN ))->SetCheck(TRUE);
    ((CButton*)GetDlgItem(IDC_VERTEXFOG))->SetCheck(TRUE);
    OnVertexFog();

    // Enumerate available D3D devices. Set a callback, so the app can
    // confirm/reject each enumerated device depending on what capabilities
    // the app requires.
    if( FAILED( hr = D3DEnum_EnumerateDevices( App_ConfirmDevice ) ) )
    {
        DisplayFrameworkError( hr, MSGERR_APPMUSTEXIT );
        return;
    }

    // Select a default device to render with
    D3DEnum_SelectDefaultDevice( &m_pDeviceInfo );
    UpdateDeviceInfo();

    // Check if we could not get a device that renders into a window, which
    // means the display must be 16- or 256-color mode. If so, let's bail.
    if( FALSE == m_pDeviceInfo->bWindowed )
    {
        Cleanup3DEnvironment();
        DisplayFrameworkError( D3DFWERR_INVALIDMODE, MSGERR_APPMUSTEXIT );
        return;
    }

    // Save static reference to the render window
    m_hwndRenderWindow = GetDlgItem(IDC_RENDERVIEW)->GetSafeHwnd();

    // Initialize the 3D environment for the app
    if( FAILED( hr = Initialize3DEnvironment() ) )
    {
        Cleanup3DEnvironment();
        DisplayFrameworkError( hr, MSGERR_APPMUSTEXIT );
        return;
    }

    // Register a class for a fullscreen window
    WNDCLASS wndClass = { CS_HREDRAW | CS_VREDRAW, FullScreenWndProc, 0, 0, NULL,
                          NULL, NULL, (HBRUSH)GetStockObject(WHITE_BRUSH), NULL,
                          TEXT("Fullscreen Window") };
    RegisterClass( &wndClass );

    m_bReady = TRUE;
}




//-----------------------------------------------------------------------------
// Name: UpdateDeviceInfo()
// Desc: Saves information about the currently selected device. 
//-----------------------------------------------------------------------------
VOID CAppForm::UpdateDeviceInfo()
{
    // Save the selected device information. However, if the device can
    // only do fullscreen operation, then use software emulation for
    // windowed mode.
    m_pFullScreenDeviceInfo = m_pDeviceInfo;
    
    if( m_pDeviceInfo->ddDriverCaps.dwCaps2 & DDCAPS2_CANRENDERWINDOWED )
        m_bUsingHELForWindowedMode = FALSE;
    else
    {
        D3DEnum_SelectDefaultDevice( &m_pDeviceInfo, D3DENUM_SOFTWAREONLY );
        m_bUsingHELForWindowedMode = TRUE;
    }
}




//-----------------------------------------------------------------------------
// Name: AppInitialize()
// Desc: Initializes the sample framework, then calls the app-specific function
//       to initialize device specific objects. This code is structured to
//       handled any errors that may occur duing initialization
//-----------------------------------------------------------------------------
HRESULT CAppForm::AppInitialize( HWND hWnd )
{
    GUID*           pDriverGUID;
    GUID*           pDeviceGUID;
    DDSURFACEDESC2* pMode;
    DWORD           dwFrameworkFlags;
    HRESULT         hr;

    if( m_bWindowed )
    {
        pDriverGUID      = m_pDeviceInfo->pDriverGUID;
        pDeviceGUID      = m_pDeviceInfo->pDeviceGUID;
        pMode            = NULL;
        dwFrameworkFlags = D3DFW_ZBUFFER;
    }
    else
    {
        pDriverGUID      = m_pFullScreenDeviceInfo->pDriverGUID;
        pDeviceGUID      = m_pFullScreenDeviceInfo->pDeviceGUID;
        pMode            = &m_pFullScreenDeviceInfo->ddsdFullscreenMode;
        dwFrameworkFlags = D3DFW_ZBUFFER|D3DFW_FULLSCREEN;
    }

    // Initialize the D3D framework
    if( SUCCEEDED( hr = m_pFramework->Initialize( hWnd, pDriverGUID,
                       pDeviceGUID, pMode, dwFrameworkFlags ) ) )
    {
        m_pd3dDevice = m_pFramework->GetD3DDevice();

        // Let the app run its startup code which creates the 3d scene.
        if( SUCCEEDED( hr = App_InitDeviceObjects( hWnd, m_pd3dDevice ) ) )
        {
            // Update UI, and device's fog parameters
            UpdateUIForDeviceCapabilites();
            SetFogParameters();
            return S_OK;
        }
        else
        {
            App_DeleteDeviceObjects( hWnd, m_pd3dDevice );
            m_pFramework->DestroyObjects();
        }
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: Initialize3DEnvironment()
// Desc: One-time initialization for the 3D portion of the app. Any changes to
//       the device will be handled with the Change3DEnvironment() function.
//-----------------------------------------------------------------------------
HRESULT CAppForm::Initialize3DEnvironment()
{
    HRESULT hr;

    // Initialize the app
    if( FAILED( hr = App_OneTimeSceneInit() ) )
        return E_FAIL;

    // Create a new CD3DFramework class. This class does all of our D3D
    // initialization and manages the common D3D objects.
    if( NULL == ( m_pFramework = new CD3DFramework7() ) )
        return E_OUTOFMEMORY;

    // Finally, initialize the framework and scene.
    if( SUCCEEDED( hr = AppInitialize( m_hwndRenderWindow ) ) )
        return S_OK;

    // If we get here, the first initialization passed failed. If that was with a
    // hardware device, try again using a software rasterizer instead.
    if( m_pDeviceInfo->bHardware )
    {
        // Try again with a software rasterizer
        DisplayFrameworkError( hr, MSGWARN_SWITCHTOSOFTWARE );
        D3DEnum_SelectDefaultDevice( &m_pDeviceInfo, D3DENUM_SOFTWAREONLY );
        UpdateDeviceInfo();
        hr = AppInitialize( m_hwndRenderWindow );
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: Change3DEnvironment()
// Desc: Handles device and/or mode changes for the app.
//-----------------------------------------------------------------------------
HRESULT CAppForm::Change3DEnvironment()
{
    if( NULL == m_pFramework )
        return E_FAIL;

    // In case we're coming from a fullscreen mode, restore the window size
    HWND hWnd = m_hwndRenderWindow;
    if( m_bWindowed == FALSE )
        hWnd = m_hwndRenderFullScreen;

    // Release all objects that need to be re-created for the new device
    App_DeleteDeviceObjects( hWnd, m_pd3dDevice );
    m_pFramework->DestroyObjects();

    // Inform the framework class of the device change. It will internally
    // re-create valid surfaces, a d3ddevice, etc.
    return AppInitialize( hWnd );
}




//-----------------------------------------------------------------------------
// Name: Render3DEnvironment()
// Desc: Draws the scene
//-----------------------------------------------------------------------------
HRESULT CAppForm::Render3DEnvironment()
{
    if( FALSE == m_bReady )
        return S_OK;

    // If fullscreen, check the cooperative level before rendering
    if( FALSE == m_bWindowed )
    {
        if( FAILED( m_pFramework->GetDirectDraw()->TestCooperativeLevel() ) )
        {
            // If we lost fullscreen mode, let's bail out of it for real
            GoWindowed();
            return S_OK;
        }
    }
    
    // FrameMove (animate) the scene
    App_FrameMove( m_pd3dDevice, timeGetTime()*0.001f );

    //Render the scene
    App_Render( m_pd3dDevice );

    // Keep track of the frame rate, and output it every 500 milliseconds to
    // the appropiate UI control.
    static DWORD dwLastTime = 0;
    static DWORD dwNumFrames = 0;
    dwNumFrames++;
    
    if( timeGetTime()-dwLastTime > 500 )
    {
        FLOAT fFPS = dwNumFrames*1000.0f/( timeGetTime() - dwLastTime );
        dwLastTime = timeGetTime();
        dwNumFrames=0;;
        CHAR buffer[20];
        sprintf(buffer,"%4.1f fps", fFPS );
        GetDlgItem(IDC_FPS_TEXT)->SetWindowText(buffer);
    }

    // Output any special messages
    if( m_bWindowed == FALSE )
    {
        OutputText( 0, 0, "Hit <ESC> to return" );
    }
    else 
    {
        if( m_bUsingHELForWindowedMode )
        {
            OutputText( 0, 0, "Note: Must go fullscreen" );
            OutputText( 0, 15, "for the selected device" );
        }
    }

    // Show the frame on the primary surface.
    if( DDERR_SURFACELOST == m_pFramework->ShowFrame() )
    {
        m_pFramework->RestoreSurfaces();
        App_RestoreSurfaces();
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: OutputText()
// Desc: Draws text on the window.
//-----------------------------------------------------------------------------
HRESULT CAppForm::OutputText( DWORD x, DWORD y, CHAR* str )
{
    // Get a DC for the render surface. Then, write out the buffer
    HDC hDC;
    if( SUCCEEDED( m_pFramework->GetRenderSurface()->GetDC(&hDC) ) )
    {
        SetTextColor( hDC, RGB(255,255,0) );
        SetBkMode( hDC, TRANSPARENT );
        ExtTextOut( hDC, x, y, 0, NULL, str, strlen(str), NULL );
    
        m_pFramework->GetRenderSurface()->ReleaseDC(hDC);
    }
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: Move()
// Desc: Called when the render window is moved
//-----------------------------------------------------------------------------
VOID CAppForm::Move( int x, int y )
{
    // Inform the framework of the new window position
    if( m_pFramework && m_bReady )
    {
        RECT rc;
        ::GetClientRect( m_hwndRenderWindow, &rc );
        ::ClientToScreen( m_hwndRenderWindow, (POINT*)&rc.left );
        ::ClientToScreen( m_hwndRenderWindow, (POINT*)&rc.right );

        m_pFramework->Move( (SHORT)rc.left, (SHORT)rc.top );
    }
}




//-----------------------------------------------------------------------------
// Name: Cleanup3DEnvironment()
// Desc: Cleanup scene objects
//-----------------------------------------------------------------------------
VOID CAppForm::Cleanup3DEnvironment()
{
    if( m_pFramework )
    {
        HWND hWnd = m_hwndRenderWindow;
        if( m_bWindowed == FALSE )
            hWnd = m_hwndRenderFullScreen;
        
        App_DeleteDeviceObjects( hWnd, m_pd3dDevice );
        App_FinalCleanup();

        SAFE_DELETE( m_pFramework );
    }
}




//-----------------------------------------------------------------------------
// Name: GoFullScreen()
// Desc: Makes the 3D rendering go to fullscreen mode
//-----------------------------------------------------------------------------
VOID CAppForm::GoFullScreen()
{
    HRESULT hr;

    if( FALSE == m_bWindowed )
        return;

    ((CButton*)GetDlgItem(IDC_VIEWFULLSCREEN))->SetCheck(m_bWindowed);
    m_bWindowed = FALSE;

    // Create a new fullscreen window
    RECT rc = { 0, 0, 100, 100 };
    m_hwndRenderFullScreen = CreateWindow( TEXT("Fullscreen Window"), NULL,
                                           WS_POPUP|WS_VISIBLE, CW_USEDEFAULT,
                                           CW_USEDEFAULT, 100, 100,
                                           m_hwndRenderWindow, 0L, NULL, 0L );
    
    if( FAILED( hr = Change3DEnvironment() ) )
    {
        GoWindowed();
        DisplayFrameworkError( hr, MSGWARN_CANTDOFULLSCREEN );
    }
}




//-----------------------------------------------------------------------------
// Name: GoWindowed()
// Desc: Makes the 3D rendering go back to windowed mode
//-----------------------------------------------------------------------------
VOID CAppForm::GoWindowed()
{
    HRESULT hr;

    if( TRUE == m_bWindowed )
        return;

    ((CButton*)GetDlgItem(IDC_VIEWFULLSCREEN))->SetCheck(m_bWindowed);
    m_bWindowed = TRUE;

    if( FAILED( hr = Change3DEnvironment() ) )
    {
        // Display an error message and exit
        DisplayFrameworkError( hr, MSGERR_APPMUSTEXIT );
        g_D3DApp.GetMainWnd()->PostMessage( WM_CLOSE, 0, 0 );
        return;
    }

    if( m_hwndRenderFullScreen )
        ::DestroyWindow( m_hwndRenderFullScreen );
    m_hwndRenderFullScreen = NULL;
}




//-----------------------------------------------------------------------------
// Name: DisplayFrameworkError()
// Desc: Displays error messages in a message box
//-----------------------------------------------------------------------------
VOID DisplayFrameworkError( HRESULT hr, APPMSGTYPE errType )
{
    CHAR strMsg[512];

    switch( hr )
    {
        case D3DENUMERR_NOCOMPATIBLEDEVICES:
            strcpy( strMsg, TEXT("Could not find any compatible Direct3D\n"
                    "devices.") );
            break;
        case D3DENUMERR_SUGGESTREFRAST:
            strcpy( strMsg, TEXT("Could not find any compatible devices.\n\n"
                    "Try enabling the reference rasterizer using\n"
                    "EnableRefRast.reg.") );
            break;
        case D3DENUMERR_ENUMERATIONFAILED:
            strcpy( strMsg, TEXT("Enumeration failed. Your system may be in an\n"
                    "unstable state and need to be rebooted") );
            break;
        case D3DFWERR_INITIALIZATIONFAILED:
            strcpy( strMsg, TEXT("Generic initialization error.\n\nEnable "
                    "debug output for detailed information.") );
            break;
        case D3DFWERR_NODIRECTDRAW:
            strcpy( strMsg, TEXT("No DirectDraw") );
            break;
        case D3DFWERR_NODIRECT3D:
            strcpy( strMsg, TEXT("No Direct3D") );
            break;
        case D3DFWERR_INVALIDMODE:
            strcpy( strMsg, TEXT("This sample requires a 16-bit (or higher) "
                    "display mode\nto run in a window.\n\nPlease switch "
                    "your desktop settings accordingly.") );
            break;
        case D3DFWERR_COULDNTSETCOOPLEVEL:
            strcpy( strMsg, TEXT("Could not set Cooperative Level") );
            break;
        case D3DFWERR_NO3DDEVICE:
            strcpy( strMsg, TEXT("No 3D Device") );
            break;
        case D3DFWERR_NOZBUFFER:
            strcpy( strMsg, TEXT("No ZBuffer") );
            break;
        case D3DFWERR_NOPRIMARY:
            strcpy( strMsg, TEXT("No primary") );
            break;
        case D3DFWERR_NOCLIPPER:
            strcpy( strMsg, TEXT("No Clipper") );
            break;
        case D3DFWERR_BADDISPLAYMODE:
            strcpy( strMsg, TEXT("Bad display mode") );
            break;
        case D3DFWERR_NOBACKBUFFER:
            strcpy( strMsg, TEXT("No backbuffer") );
            break;
        case D3DFWERR_NONZEROREFCOUNT:
            strcpy( strMsg, TEXT("Non-zero ref count") );
            break;
        case D3DFWERR_NORENDERTARGET:
            strcpy( strMsg, TEXT("No render target") );
            break;
        case E_OUTOFMEMORY:
            strcpy( strMsg, TEXT("Not enough memory!") );
            break;
        case DDERR_OUTOFVIDEOMEMORY:
            strcpy( strMsg, TEXT("There was insufficient video memory "
                    "to use the\nhardware device.") );
            break;
        default:
            strcpy( strMsg, TEXT("Generic application error.\n\nEnable "
                    "debug output for detailed information.") );
    }

    if( MSGERR_APPMUSTEXIT == errType )
    {
        strcat( strMsg, TEXT("\n\nThis sample will now exit.") );
        MessageBox( NULL, strMsg, g_strAppTitle, MB_ICONERROR|MB_OK );
    }
    else
    {
        if( MSGWARN_SWITCHTOSOFTWARE == errType )
            strcat( strMsg, TEXT("\n\nSwitching to software rasterizer.") );
        if( MSGWARN_CANTDOFULLSCREEN == errType )
            strcat( strMsg, TEXT("\n\nCan't do fullscreen mode.") );
        MessageBox( NULL, strMsg, g_strAppTitle, MB_ICONWARNING|MB_OK );
    }
}




//-----------------------------------------------------------------------------
// The remaining code handles the UI for the MFC-based app.
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
// Name: OnChangeDevice()
// Desc: Use hit the "Change Device.." button. Display the dialog for the user
//       to select a new device/mode, and call Change3DEnvironment to
//       use the new device/mode.
//-----------------------------------------------------------------------------
VOID CAppForm::OnChangeDevice()
{
    HRESULT hr;

    if( m_bReady )
    {
        m_bReady = FALSE;
        D3DEnum_UserChangeDevice( &m_pDeviceInfo );
        UpdateDeviceInfo();
        if( FAILED( hr = Change3DEnvironment() ) )
        {
            // Handle the error case 
            DisplayFrameworkError( hr, MSGERR_APPMUSTEXIT );
            g_D3DApp.GetMainWnd()->PostMessage( WM_CLOSE, 0, 0 );
            return;
        }
        m_bReady = TRUE;
    }
}




//-----------------------------------------------------------------------------
// Name: OnToggleFullScreen()
// Desc: Called when user toggles the fullscreen mode
//-----------------------------------------------------------------------------
void CAppForm::OnToggleFullScreen()
{
    if( m_bWindowed )
        GoFullScreen();
    else
        GoWindowed();
}




//-----------------------------------------------------------------------------
// Name: OnTerrainResolution()
// Desc: Called when the user selects the terrain resolution
//-----------------------------------------------------------------------------
VOID CAppForm::OnTerrainResolution() 
{
    m_bHiResTerrain = ((CButton*)GetDlgItem(IDC_HIRESTERRAIN))->GetCheck();

    SetFogParameters();
}




//-----------------------------------------------------------------------------
// Name: OnHScroll()
// Desc: Called when the user moves any scroll bar. Check which scrollbar was
//       moved, and extract the appropiate value to a global variable.
//-----------------------------------------------------------------------------
void CAppForm::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
    // Get the new fog parameters
    m_fFogStart   = ((CSliderCtrl*)GetDlgItem(IDC_FOGSTART_SLIDER))->GetPos()/100.0f;
    m_fFogEnd     = ((CSliderCtrl*)GetDlgItem(IDC_FOGEND_SLIDER))->GetPos()/100.0f;
    m_fFogDensity = ((CSliderCtrl*)GetDlgItem(IDC_FOGDENSITY_SLIDER))->GetPos()/100.0f;

    SetFogParameters();

    CFormView::OnHScroll(nSBCode, nPos, pScrollBar);
}




//-----------------------------------------------------------------------------
// Name: OnFogColor()
// Desc: Called when the user hits the "fog color..." button. Display a color
//       selection dialog box, and set the global fog color variable.
//-----------------------------------------------------------------------------
void CAppForm::OnFogColor() 
{
    CColorDialog dlg;

    if( IDOK == dlg.DoModal() )
    {
        m_dwFogColor = ((((DWORD)dlg.GetColor())&0x000000ff)<<16) +
                       ((((DWORD)dlg.GetColor())&0x0000ff00)) +
                       ((((DWORD)dlg.GetColor())&0x00ff0000)>>16);
        SetFogParameters();
    }
}




//-----------------------------------------------------------------------------
// Name: OnRangeBasedFog()
// Desc: Toggle the boolean variable for whether RangeBasedFog is enabled.
//-----------------------------------------------------------------------------
void CAppForm::OnRangeBasedFog() 
{
    m_bRangeBasedFog = ((CButton*)GetDlgItem(IDC_RANGEBASEDFOG))->GetCheck();

    SetFogParameters();
}




//-----------------------------------------------------------------------------
// Name: OnVertexFog()
// Desc: User selected vertex fog. Upadte the global variables as appropiate.
//-----------------------------------------------------------------------------
void CAppForm::OnVertexFog() 
{
    // Note: We always assume range fog is available if doing vertex fog
    GetDlgItem(IDC_RANGEBASEDFOG)->EnableWindow(TRUE);

    GetDlgItem(IDC_LINEARFOGMODE)->EnableWindow(TRUE);
    GetDlgItem(IDC_EXPFOGMODE)->EnableWindow(FALSE);
    GetDlgItem(IDC_EXP2FOGMODE)->EnableWindow(FALSE);
    ((CButton*)GetDlgItem(IDC_LINEARFOGMODE))->SetCheck(TRUE);
    ((CButton*)GetDlgItem(IDC_EXPFOGMODE))->SetCheck(FALSE);
    ((CButton*)GetDlgItem(IDC_EXP2FOGMODE))->SetCheck(FALSE);

    GetDlgItem(IDC_FOGSTARTMIN_TEXT)->SetWindowText( "near (1.0)" );
    GetDlgItem(IDC_FOGSTARTMAX_TEXT)->SetWindowText( "far (150.0)" );
    GetDlgItem(IDC_FOGENDMIN_TEXT)->SetWindowText( "near (1.0)" );
    GetDlgItem(IDC_FOGENDMAX_TEXT)->SetWindowText( "far (150.0)" );

    m_bUsingTableFog = FALSE;
    OnFogMode();
}




//-----------------------------------------------------------------------------
// Name: OnTableFog()
// Desc: User selected table fog. Upadte the global variables as appropiate.
//-----------------------------------------------------------------------------
void CAppForm::OnTableFog() 
{
    // Note: We only assume range fog is available if doing vertex fog
    GetDlgItem(IDC_RANGEBASEDFOG)->EnableWindow(FALSE);
    ((CButton*)GetDlgItem(IDC_RANGEBASEDFOG))->SetCheck(FALSE);

    GetDlgItem(IDC_LINEARFOGMODE)->EnableWindow(TRUE);
    GetDlgItem(IDC_EXPFOGMODE)->EnableWindow(TRUE);
    GetDlgItem(IDC_EXP2FOGMODE)->EnableWindow(TRUE);

    if( m_bCanDoWFog )
    {
        GetDlgItem(IDC_FOGSTARTMIN_TEXT)->SetWindowText( "near (1.0)" );
        GetDlgItem(IDC_FOGSTARTMAX_TEXT)->SetWindowText( "far (150.0)" );
        GetDlgItem(IDC_FOGENDMIN_TEXT)->SetWindowText( "near (1.0)" );
        GetDlgItem(IDC_FOGENDMAX_TEXT)->SetWindowText( "far (150.0)" );
    }
    else
    {
        GetDlgItem(IDC_FOGSTARTMIN_TEXT)->SetWindowText( "near (0.0)" );
        GetDlgItem(IDC_FOGSTARTMAX_TEXT)->SetWindowText( "far (1.0)" );
        GetDlgItem(IDC_FOGENDMIN_TEXT)->SetWindowText( "near (0.0)" );
        GetDlgItem(IDC_FOGENDMAX_TEXT)->SetWindowText( "far (1.0)" );
    }

    m_bUsingTableFog = TRUE;
    OnFogMode();
}




//-----------------------------------------------------------------------------
// Name: OnFogMode()
// Desc: User changed the fog mode. Update the UI and global variables, as many
//       controls are mutually exclusive.
//-----------------------------------------------------------------------------
void CAppForm::OnFogMode() 
{
    if( ((CButton*)GetDlgItem(IDC_LINEARFOGMODE))->GetCheck() )
    {
        m_dwFogMode = D3DFOG_LINEAR;

        GetDlgItem(IDC_FOGSTART_TEXT)->EnableWindow(TRUE);
        GetDlgItem(IDC_FOGSTARTMIN_TEXT)->EnableWindow(TRUE);
        GetDlgItem(IDC_FOGSTARTMAX_TEXT)->EnableWindow(TRUE);
        GetDlgItem(IDC_FOGSTART_SLIDER)->EnableWindow(TRUE);
        GetDlgItem(IDC_FOGEND_TEXT)->EnableWindow(TRUE);
        GetDlgItem(IDC_FOGENDMIN_TEXT)->EnableWindow(TRUE);
        GetDlgItem(IDC_FOGENDMAX_TEXT)->EnableWindow(TRUE);
        GetDlgItem(IDC_FOGEND_SLIDER)->EnableWindow(TRUE);
        GetDlgItem(IDC_FOGDENSITY_TEXT)->EnableWindow(FALSE);
        GetDlgItem(IDC_FOGDENSITYMIN_TEXT)->EnableWindow(FALSE);
        GetDlgItem(IDC_FOGDENSITYMAX_TEXT)->EnableWindow(FALSE);
        GetDlgItem(IDC_FOGDENSITY_SLIDER)->EnableWindow(FALSE);
    }
    else
    {
        GetDlgItem(IDC_FOGSTART_TEXT)->EnableWindow(FALSE);
        GetDlgItem(IDC_FOGSTARTMIN_TEXT)->EnableWindow(FALSE);
        GetDlgItem(IDC_FOGSTARTMAX_TEXT)->EnableWindow(FALSE);
        GetDlgItem(IDC_FOGSTART_SLIDER)->EnableWindow(FALSE);
        GetDlgItem(IDC_FOGEND_TEXT)->EnableWindow(FALSE);
        GetDlgItem(IDC_FOGENDMIN_TEXT)->EnableWindow(FALSE);
        GetDlgItem(IDC_FOGENDMAX_TEXT)->EnableWindow(FALSE);
        GetDlgItem(IDC_FOGEND_SLIDER)->EnableWindow(FALSE);
        GetDlgItem(IDC_FOGDENSITY_TEXT)->EnableWindow(TRUE);
        GetDlgItem(IDC_FOGDENSITYMIN_TEXT)->EnableWindow(TRUE);
        GetDlgItem(IDC_FOGDENSITYMAX_TEXT)->EnableWindow(TRUE);
        GetDlgItem(IDC_FOGDENSITY_SLIDER)->EnableWindow(TRUE);

        if( ((CButton*)GetDlgItem(IDC_EXPFOGMODE))->GetCheck() )
            m_dwFogMode = D3DFOG_EXP;
        if( ((CButton*)GetDlgItem(IDC_EXP2FOGMODE))->GetCheck() )
            m_dwFogMode = D3DFOG_EXP2;
    }

    SetFogParameters();
}




//-----------------------------------------------------------------------------
// Name: UpdateUIForDeviceCapabilites()
// Desc: Whenever we get a new device, call this function to enable/disable the
//       appropiate UI controls to match the device's capabilities.
//-----------------------------------------------------------------------------
VOID CAppForm::UpdateUIForDeviceCapabilites()
{
    // Check the capabilities of the device
    DWORD dwCaps = m_pFullScreenDeviceInfo->ddDeviceDesc.dpcTriCaps.dwRasterCaps;
    m_bCanDoTableFog  = (dwCaps&D3DPRASTERCAPS_FOGTABLE)  ? TRUE : FALSE;
    m_bCanDoVertexFog = (dwCaps&D3DPRASTERCAPS_FOGVERTEX) ? TRUE : FALSE;
    m_bCanDoWFog      = (dwCaps&D3DPRASTERCAPS_WFOG)      ? TRUE : FALSE;

    // Update the UI checkbox states
    ((CButton*)GetDlgItem(IDC_TABLEFOG))->EnableWindow(m_bCanDoTableFog);
    ((CButton*)GetDlgItem(IDC_VERTEXFOG))->EnableWindow(m_bCanDoVertexFog);

    if( m_bCanDoWFog )
        GetDlgItem(IDC_USINGWFOG)->SetWindowText( "Device using W-fog" );
    else
        GetDlgItem(IDC_USINGWFOG)->SetWindowText( "Device using Z-fog" );

    if( m_bUsingTableFog && m_bCanDoTableFog )
    {
        ((CButton*)GetDlgItem(IDC_VERTEXFOG))->SetCheck(FALSE);
        ((CButton*)GetDlgItem(IDC_TABLEFOG))->SetCheck(TRUE);
    }
    else if( m_bCanDoVertexFog )
    {
        ((CButton*)GetDlgItem(IDC_VERTEXFOG))->SetCheck(TRUE);
        ((CButton*)GetDlgItem(IDC_TABLEFOG))->SetCheck(FALSE);
    }
    else
    {
        ((CButton*)GetDlgItem(IDC_VERTEXFOG))->SetCheck(FALSE);
        ((CButton*)GetDlgItem(IDC_TABLEFOG))->SetCheck(m_bCanDoTableFog);
    }

    // Set up table or vertex mode, as appropiate
    if( ((CButton*)GetDlgItem(IDC_TABLEFOG))->GetCheck() )
        OnTableFog();
    if( ((CButton*)GetDlgItem(IDC_VERTEXFOG))->GetCheck() )
        OnVertexFog();
}




//-----------------------------------------------------------------------------
// Name: SetFogParameters()
// Desc: Sets the apps parameters for rendering the scene
//-----------------------------------------------------------------------------
VOID CAppForm::SetFogParameters()
{
    App_SetFogParameters( m_bHiResTerrain, m_dwFogMode, m_dwFogColor,
                          m_bRangeBasedFog, m_bUsingTableFog, 
                          m_fFogStart, m_fFogEnd, m_fFogDensity );
}





