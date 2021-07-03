//-----------------------------------------------------------------------------
// File: Filter.cpp
//
// Desc: Example code showing how to enable various filtering modes in
//       Direct3D.
//
//       Note: This code uses the D3D Framework helper library.
//
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <math.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "resource.h"




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    D3DVERTEX           m_pvVertices[4];

    // Filter modes
    D3DTEXTUREMAGFILTER m_dwLeftTexMagState,     m_dwRightTexMagState;
    D3DTEXTUREMINFILTER m_dwLeftTexMinState,     m_dwRightTexMinState;
    DWORD               m_dwLeftAnisotropyLevel, m_dwRightAnisotropyLevel;

    // Device capabilities
    BOOL                m_bDeviceDoesFlatCubic;
    BOOL                m_bDeviceDoesGaussianCubic;
    DWORD               m_dwDeviceMaxAnisotropy;

    HRESULT UpdateDeviceCapabilities();
    VOID    SetMenuStates();

protected:
    HRESULT OneTimeSceneInit();
    HRESULT InitDeviceObjects();
    HRESULT DeleteDeviceObjects();
    HRESULT Render();
    HRESULT FrameMove( FLOAT fTimeKey );
    HRESULT FinalCleanup();

public:
    LRESULT MsgProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam );
    CMyD3DApplication();
};




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point to the program. Initializes everything, and goes into a
//       message-processing loop. Idle time is used to render the scene.
//-----------------------------------------------------------------------------
INT WINAPI WinMain( HINSTANCE hInst, HINSTANCE, LPSTR strCmdLine, INT )
{
    CMyD3DApplication d3dApp;

    if( FAILED( d3dApp.Create( hInst, strCmdLine ) ) )
        return 0;

    return d3dApp.Run();
}




//-----------------------------------------------------------------------------
// Name: CMyD3DApplication()
// Desc: Application constructor. Sets attributes for the app.
//-----------------------------------------------------------------------------
CMyD3DApplication::CMyD3DApplication()
                  :CD3DApplication()
{
    m_strWindowTitle  = TEXT( "Filter: D3D Filtering Sample" );
    m_bAppUseZBuffer  = FALSE;
    m_bAppUseStereo   = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    D3DVECTOR vNorm( 0.0f, 0.0f, 1.0f );
    m_pvVertices[0] = D3DVERTEX( D3DVECTOR( 1, 1, 0), vNorm, 1.0f, 0.0f );
    m_pvVertices[1] = D3DVERTEX( D3DVECTOR( 1,-1, 0), vNorm, 1.0f, 1.0f );
    m_pvVertices[2] = D3DVERTEX( D3DVECTOR(-1, 1, 0), vNorm, 0.0f, 0.0f );
    m_pvVertices[3] = D3DVERTEX( D3DVECTOR(-1,-1, 0), vNorm, 0.0f, 1.0f );

    // Create some textures
    D3DTextr_CreateTextureFromFile( "Filter.bmp" );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    D3DMATRIX matWorldSpin;
    D3DUtil_SetRotateXMatrix( matWorldSpin, (FLOAT)sin( fTimeKey ) );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorldSpin );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: Render()
// Desc: Called once per frame, the call is the entry point for 3d
//       rendering. This function sets up render states, clears the
//       viewport, and renders the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::Render()
{
    // Clear the viewport
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET, 0x003399ff, 0.0f, 0L );

    // Begin the scene
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        D3DMATRIX matWorld;
        m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );

        // Set the filter states for the left image
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER,     m_dwLeftTexMagState );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER,     m_dwLeftTexMinState );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAXANISOTROPY, m_dwLeftAnisotropyLevel );

        // Draw the left image
        matWorld._41 = -1.1f;
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                     m_pvVertices, 4, NULL );

        // Set the filter states for the left image
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER,     m_dwRightTexMagState );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER,     m_dwRightTexMinState );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAXANISOTROPY, m_dwRightAnisotropyLevel );

        // Draw the right image
        matWorld._41 = 1.1f;
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                     m_pvVertices, 4, NULL );
        
        // End the scene.
        m_pd3dDevice->EndScene();
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );

    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("Filter.bmp") );

    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,  0xffffffff );

    // Set the transform matrices
    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f, 0.0f,-2.5f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 0.0f, 0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );
    D3DMATRIX matProj;

    SetViewParams( &vEyePt, &vLookatPt, &vUpVec, 0.1f );
    D3DUtil_SetProjectionMatrix( matProj, 1.57f, 1.0f, 1.0f, 100.0f );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    UpdateDeviceCapabilities();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DeleteDeviceObjects()
// Desc: Called when the app is exitting, or the device is being changed,
//       this function deletes any device dependant objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DeleteDeviceObjects()
{
    D3DTextr_InvalidateAllTextures();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FinalCleanup()
// Desc: Called before the app exits, this function gives the app the chance
//       to cleanup after itself.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FinalCleanup()
{
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: UpdateDeviceCapabilities()
// Desc: Called for a new device, this functions checks the capabilities of
//       the device and stores them in global variables and updates the menu
//       accordingly.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::UpdateDeviceCapabilities()
{
    // Initialize the default filtering states
    m_bDeviceDoesFlatCubic         = FALSE;
    m_bDeviceDoesGaussianCubic     = FALSE;
    m_dwDeviceMaxAnisotropy        = 1;
    
    m_dwLeftTexMagState      = D3DTFG_POINT;
    m_dwLeftTexMinState      = D3DTFN_POINT;
    m_dwLeftAnisotropyLevel  = 1;
    m_dwRightTexMagState     = D3DTFG_POINT;
    m_dwRightTexMinState     = D3DTFN_POINT;
    m_dwRightAnisotropyLevel = 1;

    // Get the device caps
    DWORD dwRasterCaps = m_pDeviceInfo->ddDeviceDesc.dpcTriCaps.dwRasterCaps;
    DWORD dwFilterCaps = m_pDeviceInfo->ddDeviceDesc.dpcTriCaps.dwTextureFilterCaps;

    // Check the device for supported filtering methods
    if( dwRasterCaps & D3DPRASTERCAPS_ANISOTROPY )
        m_dwDeviceMaxAnisotropy = m_pDeviceInfo->ddDeviceDesc.dwMaxAnisotropy;
    if( dwFilterCaps & D3DPTFILTERCAPS_MAGFAFLATCUBIC )
        m_bDeviceDoesFlatCubic = TRUE;
    if( dwFilterCaps & D3DPTFILTERCAPS_MAGFGAUSSIANCUBIC )
        m_bDeviceDoesGaussianCubic = TRUE;

    SetMenuStates();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetMenuStates()
// Desc: Overrrides the main WndProc, so the sample can do custom message 
//       handling (e.g. processing mouse, keyboard, or menu commands).
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::SetMenuStates()
{
    HMENU hMenu = GetMenu( m_hWnd );

    // Enable the appropiate menu items
    DWORD dwAnisotropy2 = ( m_dwDeviceMaxAnisotropy >= 2L ) ? MF_ENABLED : MF_GRAYED;
    DWORD dwAnisotropy4 = ( m_dwDeviceMaxAnisotropy >= 4L ) ? MF_ENABLED : MF_GRAYED;
    DWORD dwAnisotropy8 = ( m_dwDeviceMaxAnisotropy >= 8L ) ? MF_ENABLED : MF_GRAYED;
    EnableMenuItem( hMenu, IDM_LEFT_MAGANISOTROPIC,  dwAnisotropy2 );
    EnableMenuItem( hMenu, IDM_LEFT_MINANISOTROPIC,  dwAnisotropy2 );
    EnableMenuItem( hMenu, IDM_LEFT_ANISOTROPY2,     dwAnisotropy2 );
    EnableMenuItem( hMenu, IDM_LEFT_ANISOTROPY4,     dwAnisotropy4 );
    EnableMenuItem( hMenu, IDM_LEFT_ANISOTROPY8,     dwAnisotropy8 );
    EnableMenuItem( hMenu, IDM_RIGHT_MAGANISOTROPIC, dwAnisotropy2 );
    EnableMenuItem( hMenu, IDM_RIGHT_MINANISOTROPIC, dwAnisotropy2 );
    EnableMenuItem( hMenu, IDM_RIGHT_ANISOTROPY2,    dwAnisotropy2 );
    EnableMenuItem( hMenu, IDM_RIGHT_ANISOTROPY4,    dwAnisotropy4 );
    EnableMenuItem( hMenu, IDM_RIGHT_ANISOTROPY8,    dwAnisotropy8 );

    DWORD dwFlatCubic = ( TRUE == m_bDeviceDoesFlatCubic ) ? MF_ENABLED : MF_GRAYED;
    DWORD dwGaussian  = ( TRUE == m_bDeviceDoesGaussianCubic ) ? MF_ENABLED : MF_GRAYED;
    EnableMenuItem( hMenu, IDM_LEFT_MAGFLATCUBIC,      dwFlatCubic );
    EnableMenuItem( hMenu, IDM_LEFT_MAGGAUSSIANCUBIC,  dwGaussian );
    EnableMenuItem( hMenu, IDM_RIGHT_MAGFLATCUBIC,     dwFlatCubic );
    EnableMenuItem( hMenu, IDM_RIGHT_MAGGAUSSIANCUBIC, dwGaussian );
    
    // Put checks by the appropiate menu items
    CheckMenuItem( hMenu, IDM_LEFT_MAGPOINT,
                   ( m_dwLeftTexMagState == D3DTFG_POINT ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_LEFT_MAGLINEAR,
                   ( m_dwLeftTexMagState == D3DTFG_LINEAR ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_LEFT_MAGFLATCUBIC,
                   ( m_dwLeftTexMagState == D3DTFG_FLATCUBIC ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_LEFT_MAGGAUSSIANCUBIC,
                   ( m_dwLeftTexMagState == D3DTFG_GAUSSIANCUBIC ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_LEFT_MAGANISOTROPIC,
                   ( m_dwLeftTexMagState == D3DTFG_ANISOTROPIC ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_LEFT_MINPOINT,
                   ( m_dwLeftTexMinState == D3DTFN_POINT ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_LEFT_MINLINEAR,
                   ( m_dwLeftTexMinState == D3DTFN_LINEAR ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_LEFT_MINANISOTROPIC,
                   ( m_dwLeftTexMinState == D3DTFN_ANISOTROPIC ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_LEFT_ANISOTROPY1,
                   ( m_dwLeftAnisotropyLevel == 1 ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_LEFT_ANISOTROPY2,
                   ( m_dwLeftAnisotropyLevel == 2 ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_LEFT_ANISOTROPY4,
                   ( m_dwLeftAnisotropyLevel == 4 ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_LEFT_ANISOTROPY8,
                   ( m_dwLeftAnisotropyLevel == 8 ) ? MF_CHECKED : MF_UNCHECKED );

    CheckMenuItem( hMenu, IDM_RIGHT_MAGPOINT,
                   ( m_dwRightTexMagState == D3DTFG_POINT ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_RIGHT_MAGLINEAR,
                   ( m_dwRightTexMagState == D3DTFG_LINEAR ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_RIGHT_MAGFLATCUBIC,
                   ( m_dwRightTexMagState == D3DTFG_FLATCUBIC ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_RIGHT_MAGGAUSSIANCUBIC,
                   ( m_dwRightTexMagState == D3DTFG_GAUSSIANCUBIC ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_RIGHT_MAGANISOTROPIC,
                   ( m_dwRightTexMagState == D3DTFG_ANISOTROPIC ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_RIGHT_MINPOINT,
                   ( m_dwRightTexMinState == D3DTFN_POINT ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_RIGHT_MINLINEAR,
                   ( m_dwRightTexMinState == D3DTFN_LINEAR ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_RIGHT_MINANISOTROPIC,
                   ( m_dwRightTexMinState == D3DTFN_ANISOTROPIC ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_RIGHT_ANISOTROPY1,
                   ( m_dwRightAnisotropyLevel == 1 ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_RIGHT_ANISOTROPY2,
                   ( m_dwRightAnisotropyLevel == 2 ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_RIGHT_ANISOTROPY4,
                   ( m_dwRightAnisotropyLevel == 4 ) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_RIGHT_ANISOTROPY8,
                   ( m_dwRightAnisotropyLevel == 8 ) ? MF_CHECKED : MF_UNCHECKED );
}




//-----------------------------------------------------------------------------
// Name: MsgProc()
// Desc: Overrrides the main WndProc, so the sample can do custom message 
//       handling (e.g. processing mouse, keyboard, or menu commands).
//-----------------------------------------------------------------------------
LRESULT CMyD3DApplication::MsgProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                                    LPARAM lParam )
{
    if( WM_COMMAND == uMsg )
    {
        switch( LOWORD(wParam) )
        {
            case IDM_LEFT_MAGPOINT:
                m_dwLeftTexMagState = D3DTFG_POINT;
                break;
            case IDM_LEFT_MAGLINEAR:
                m_dwLeftTexMagState = D3DTFG_LINEAR;
                break;
            case IDM_LEFT_MAGFLATCUBIC:
                m_dwLeftTexMagState = D3DTFG_FLATCUBIC;
                break;
            case IDM_LEFT_MAGGAUSSIANCUBIC:
                m_dwLeftTexMagState = D3DTFG_GAUSSIANCUBIC;
                break;
            case IDM_LEFT_MAGANISOTROPIC:
                m_dwLeftTexMagState = D3DTFG_ANISOTROPIC;
                break;
            case IDM_LEFT_MINPOINT:
                m_dwLeftTexMinState = D3DTFN_POINT;
                break;
            case IDM_LEFT_MINLINEAR:
                m_dwLeftTexMinState = D3DTFN_LINEAR;
                break;
            case IDM_LEFT_MINANISOTROPIC:
                m_dwLeftTexMinState = D3DTFN_ANISOTROPIC;
                break;
            case IDM_LEFT_ANISOTROPY1:
                m_dwLeftAnisotropyLevel = 1;
                break;
            case IDM_LEFT_ANISOTROPY2:
                m_dwLeftAnisotropyLevel = 2;
                break;
            case IDM_LEFT_ANISOTROPY4:
                m_dwLeftAnisotropyLevel = 4;
                break;
            case IDM_LEFT_ANISOTROPY8:
                m_dwLeftAnisotropyLevel = 8;
                break;

            case IDM_RIGHT_MAGPOINT:
                m_dwRightTexMagState = D3DTFG_POINT;
                break;
            case IDM_RIGHT_MAGLINEAR:
                m_dwRightTexMagState = D3DTFG_LINEAR;
                break;
            case IDM_RIGHT_MAGFLATCUBIC:
                m_dwRightTexMagState = D3DTFG_FLATCUBIC;
                break;
            case IDM_RIGHT_MAGGAUSSIANCUBIC:
                m_dwRightTexMagState = D3DTFG_GAUSSIANCUBIC;
                break;
            case IDM_RIGHT_MAGANISOTROPIC:
                m_dwRightTexMagState = D3DTFG_ANISOTROPIC;
                break;
            case IDM_RIGHT_MINPOINT:
                m_dwRightTexMinState = D3DTFN_POINT;
                break;
            case IDM_RIGHT_MINLINEAR:
                m_dwRightTexMinState = D3DTFN_LINEAR;
                break;
            case IDM_RIGHT_MINANISOTROPIC:
                m_dwRightTexMinState = D3DTFN_ANISOTROPIC;
                break;
            case IDM_RIGHT_ANISOTROPY1:
                m_dwRightAnisotropyLevel = 1;
                break;
            case IDM_RIGHT_ANISOTROPY2:
                m_dwRightAnisotropyLevel = 2;
                break;
            case IDM_RIGHT_ANISOTROPY4:
                m_dwRightAnisotropyLevel = 4;
                break;
            case IDM_RIGHT_ANISOTROPY8:
                m_dwRightAnisotropyLevel = 8;
                break;
        }

        // Update the menu to reflect any new changes
        SetMenuStates();
    }
    return CD3DApplication::MsgProc( hWnd, uMsg, wParam, lParam );
}



