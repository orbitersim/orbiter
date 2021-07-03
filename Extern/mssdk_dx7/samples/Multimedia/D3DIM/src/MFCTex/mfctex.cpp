//-----------------------------------------------------------------------------
// File: D3DApp.cpp
//
// Desc: Main file for the D3DIM mtexture app that uses MFC.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include "stdafx.h"
#include <stdio.h>
#include <mmsystem.h>
#include "D3DEnum.h"
#include "D3DFrame.h"
#include "D3DUtil.h"
#include "MFCTex.h"




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




//-----------------------------------------------------------------------------
// Global data and objects
//-----------------------------------------------------------------------------
CD3DApp       g_D3DApp;
CAppForm*     g_AppFormView = NULL;
extern TCHAR* g_strAppTitle;
extern BOOL   g_bAppUseZBuffer;




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
    ON_COMMAND( IDC_VIEWFULLSCREEN, OnToggleFullScreen )
    ON_COMMAND( IDC_VIEWCODE,       OnViewCode )
    ON_COMMAND( IDM_CHANGEDEVICE,   OnChangeDevice )
    ON_BN_CLICKED( IDM_CHANGEDEVICE,  OnChangeDevice )
    ON_EN_CHANGE(  IDC_TEX0_NAME,     OnChangeTex )
    ON_EN_CHANGE(  IDC_TEX1_NAME,     OnChangeTex )
    ON_EN_CHANGE(  IDC_TEX2_NAME,     OnChangeTex )
    ON_BN_CLICKED( IDC_SELECTTEX0,    OnSelectTexture0Name )
    ON_BN_CLICKED( IDC_SELECTTEX1,    OnSelectTexture1Name )
    ON_BN_CLICKED( IDC_SELECTTEX2,    OnSelectTexture2Name )
    ON_EN_CHANGE(  IDC_BLEND_FACTOR,  OnChangeBlendFactor )
    ON_EN_CHANGE(  IDC_DIFFUSE_COLOR, OnChangeDiffuseColor )
    ON_CBN_SELCHANGE( IDC_PRESET_EFFECTS, OnChangePresetEffects )
    ON_CBN_SELCHANGE( IDC_TEX0_COLORARG1, OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX0_COLOROP,   OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX0_COLORARG2, OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX0_ALPHAARG1, OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX0_ALPHAOP,   OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX0_ALPHAARG2, OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX1_COLORARG1, OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX1_COLOROP,   OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX1_COLORARG2, OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX1_ALPHAARG1, OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX1_ALPHAOP,   OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX1_ALPHAARG2, OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX2_COLORARG1, OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX2_COLOROP,   OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX2_COLORARG2, OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX2_ALPHAARG1, OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX2_ALPHAOP,   OnChangeStageArgs )
    ON_CBN_SELCHANGE( IDC_TEX2_ALPHAARG2, OnChangeStageArgs )
    //}}AFX_MSG_MAP
END_MESSAGE_MAP()




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
    m_pMainWnd->MoveWindow( 0, 0, 640, 350, TRUE );
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
    
    InitializeUIControls();

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
        dwFrameworkFlags = 0L;
    }
    else
    {
        pDriverGUID      = m_pFullScreenDeviceInfo->pDriverGUID;
        pDeviceGUID      = m_pFullScreenDeviceInfo->pDeviceGUID;
        pMode            = &m_pFullScreenDeviceInfo->ddsdFullscreenMode;
        dwFrameworkFlags = D3DFW_FULLSCREEN;
    }

    // Create a zbuffer, if the app requests one
    if( g_bAppUseZBuffer )
        dwFrameworkFlags |= D3DFW_ZBUFFER;

    // Initialize the D3D framework
    if( SUCCEEDED( hr = m_pFramework->Initialize( hWnd, pDriverGUID,
                       pDeviceGUID, pMode, dwFrameworkFlags ) ) )
    {
        m_pd3dDevice = m_pFramework->GetD3DDevice();

        // Let the app run its startup code which creates the 3d scene.
        if( SUCCEEDED( hr = App_InitDeviceObjects( hWnd, m_pd3dDevice ) ) )
        {
            // Update UI, and device's parameters
            UpdateUIForDeviceCapabilites();
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

    // Output device validation
    DWORD dwNumPasses;
    switch( m_pd3dDevice->ValidateDevice( &dwNumPasses ) )
    {
        case D3DERR_UNSUPPORTEDCOLOROPERATION:
            OutputText( 0, 0, _T("Unsupported color op") );
            break;
        case D3DERR_UNSUPPORTEDCOLORARG:
            OutputText( 0, 0, _T("Unsupported color arg") );
            break;
        case D3DERR_UNSUPPORTEDALPHAOPERATION:
            OutputText( 0, 0, _T("Unsupported alpha op") );
            break;
        case D3DERR_UNSUPPORTEDALPHAARG:
            OutputText( 0, 0, _T("Unsupported alpha arg") );
            break;
        case D3DERR_TOOMANYOPERATIONS:
            OutputText( 0, 0, _T("Too many texture ops") );
            break;
        case D3DERR_WRONGTEXTUREFORMAT:
            OutputText( 0, 0, _T("Incompatible texture formats") );
            break;
        case D3D_OK:
            OutputText( 0, 0, _T("Device validated OK") );
            break;
        default:
            OutputText( 0, 0, _T("Using DX5 driver") );
            break;
    }

    // Output any special messages
    if( m_bWindowed == FALSE )
    {
        OutputText( 0, 15, "Hit <ESC> to return" );
    }
    else 
    {
        if( m_bUsingHELForWindowedMode )
        {
            OutputText( 0, 15, "Note: Must go fullscreen" );
            OutputText( 0, 30, "for the selected device" );
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




