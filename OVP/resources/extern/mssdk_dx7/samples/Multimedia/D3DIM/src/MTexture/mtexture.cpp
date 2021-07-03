//-----------------------------------------------------------------------------
// File: MTexture.cpp
//
// Desc: Example code showing how to enable multiple-texturing. This
//       sample shows the interior of a room "lit" with a light map
//       using multiple textures.
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <math.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"
#include "resource.h"




//-----------------------------------------------------------------------------
// Define a custom vertex that uses XYZ, a normal, and two sets of tex coords
//-----------------------------------------------------------------------------
struct MTVERTEX
{
    D3DVALUE x, y, z;
    D3DVALUE tuBase, tvBase;
    D3DVALUE tuLightMap, tvLightMap;
};


#define FILL_MTVERTEX( v, ax, ay, az, atu1, atv1, atu2, atv2 )  \
{   v.x = ax; v.y = ay; v.z = az; \
    v.tuBase     = atu1; v.tvBase     = atv1; \
    v.tuLightMap = atu2; v.tvLightMap = atv2; \
}


enum TEXTUREMODE
{
    TEXTURE_NONE,
    TEXTURE_SINGLEPASS,
    TEXTURE_MULTIPASSCOLOR,
    TEXTURE_MULTIPASSALPHA
};




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    D3DVERTEX            m_avFloorVertices[4];  // Vertex data
    D3DVERTEX            m_avCeilVertices[4];
    MTVERTEX             m_avWallVertices[10];

    LPDIRECTDRAWSURFACE7 m_pddsWallTexture;     // Textures for the app
    LPDIRECTDRAWSURFACE7 m_pddsFloorTexture; 
    LPDIRECTDRAWSURFACE7 m_pddsLightMap;
    LPDIRECTDRAWSURFACE7 m_pddsAlphaLightMap;

    HMENU m_hMenu;
    BOOL  m_bCanDoMultiTexture;      // Whether device does mtexture
    BOOL  m_bCanDoColorBlend;        // Device can do multi pass w/color blend
    BOOL  m_bCanDoAlphaBlend;        // Device can do multi pass w/alpha blend
    BOOL  m_bCanDoTextureAlpha;      // Device can do use textures with alpha

    TEXTUREMODE m_wTextureMode;      // Which texturing mode we are using.

    HRESULT CreateInvAlphaTexFromTex( LPDIRECTDRAWSURFACE7*,
                                      LPDIRECTDRAWSURFACE7, DDPIXELFORMAT* );
    static HRESULT ConfirmDevice( DDCAPS* pddDriverCaps, 
                                  D3DDEVICEDESC7* pd3dDeviceDesc );

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
    m_strWindowTitle  = TEXT( "MTexture: D3D Multi-Texturing Demo" );
    m_bAppUseZBuffer  = FALSE;
    m_bAppUseStereo   = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = NULL;

    m_pddsWallTexture   = NULL; // Textures for the app
    m_pddsFloorTexture  = NULL; 
    m_pddsLightMap      = NULL;
    m_pddsAlphaLightMap = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Declare the vertices which define the room's walls, floor, and ceiling
    D3DVECTOR vNorm(0,1,0);

    FILL_MTVERTEX( m_avWallVertices[ 0], -5,-5, 5, 0.0f, 1.0f, 0.0, 1.0f );
    FILL_MTVERTEX( m_avWallVertices[ 1], -5, 5, 5, 0.0f, 0.0f, 0.0, 0.0f );
    FILL_MTVERTEX( m_avWallVertices[ 2],  5,-5, 5, 1.0f, 1.0f, 1.0, 1.0f );
    FILL_MTVERTEX( m_avWallVertices[ 3],  5, 5, 5, 1.0f, 0.0f, 1.0, 0.0f );
    FILL_MTVERTEX( m_avWallVertices[ 4],  5,-5,-5, 2.0f, 1.0f, 2.0, 1.0f );
    FILL_MTVERTEX( m_avWallVertices[ 5],  5, 5,-5, 2.0f, 0.0f, 2.0, 0.0f );
    FILL_MTVERTEX( m_avWallVertices[ 6], -5,-5,-5, 3.0f, 1.0f, 3.0, 1.0f );
    FILL_MTVERTEX( m_avWallVertices[ 7], -5, 5,-5, 3.0f, 0.0f, 3.0, 0.0f );
    FILL_MTVERTEX( m_avWallVertices[ 8], -5,-5, 5, 4.0f, 1.0f, 4.0, 1.0f );
    FILL_MTVERTEX( m_avWallVertices[ 9], -5, 5, 5, 4.0f, 0.0f, 4.0, 0.0f );

    m_avFloorVertices[0] = D3DVERTEX( D3DVECTOR(-5,-5, 5), vNorm, 0.0f, 0.0f );
    m_avFloorVertices[1] = D3DVERTEX( D3DVECTOR( 5,-5, 5), vNorm, 0.0f, 1.0f );
    m_avFloorVertices[2] = D3DVERTEX( D3DVECTOR(-5,-5,-5), vNorm, 1.0f, 0.0f );
    m_avFloorVertices[3] = D3DVERTEX( D3DVECTOR( 5,-5,-5), vNorm, 1.0f, 1.0f );

    m_avCeilVertices[0]  = D3DVERTEX( D3DVECTOR(-5, 5, 5),-vNorm, 0.0f, 0.0f );
    m_avCeilVertices[1]  = D3DVERTEX( D3DVECTOR(-5, 5,-5),-vNorm, 1.0f, 0.0f );
    m_avCeilVertices[2]  = D3DVERTEX( D3DVECTOR( 5, 5, 5),-vNorm, 0.0f, 1.0f );
    m_avCeilVertices[3]  = D3DVERTEX( D3DVECTOR( 5, 5,-5),-vNorm, 1.0f, 1.0f );

    // Create some textures
    D3DTextr_CreateTextureFromFile( "wall.bmp", 0 );
    D3DTextr_CreateTextureFromFile( "floor.bmp", 0 );
    D3DTextr_CreateTextureFromFile( "spotlite.bmp", 1 );
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Setup the world spin matrix
    D3DMATRIX matWorldSpin;
    D3DUtil_SetRotateYMatrix( matWorldSpin, -fTimeKey/9 );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorldSpin );

    // Rotate the light map around the walls each frame
    FLOAT tuNew = (FLOAT)fmod( fTimeKey/5, 1.0f );

    for( int i=0; i<5; i++ )
    {
        m_avWallVertices[2*i+0].tuLightMap = tuNew + i;
        m_avWallVertices[2*i+1].tuLightMap = tuNew + i;
    }

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
    // Begin the scene
    if( FAILED( m_pd3dDevice->BeginScene() ) )
        // Don't return an error, unless we want the app to exit
        return S_OK;

    // Set the texture state's for single-texturing mode
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_TEXCOORDINDEX, 0 );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_SELECTARG1 );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAOP,   D3DTOP_SELECTARG1 );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP,   D3DTOP_DISABLE );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_ALPHAOP,   D3DTOP_DISABLE );

    // Render the floor and cieling in single-texture mode
    m_pd3dDevice->SetTexture( 0, m_pddsFloorTexture );
    m_pd3dDevice->SetTexture( 1, NULL );
    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, 
                                 m_avFloorVertices, 4, NULL );
    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, 
                                 m_avCeilVertices, 4, NULL );
    
    // Now, render the walls using either single-pass multitexturing, or one
    // of the multipass techniques.
    if( m_wTextureMode == TEXTURE_SINGLEPASS )
    {
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_TEXCOORDINDEX, 1 );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLORARG1, D3DTA_TEXTURE );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLORARG2, D3DTA_CURRENT ); 
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP,   D3DTOP_MODULATE );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_ALPHAOP,   D3DTOP_SELECTARG2 );

        // Draw the walls in multi-texture mode
        m_pd3dDevice->SetTexture( 0, m_pddsWallTexture );
        m_pd3dDevice->SetTexture( 1, m_pddsLightMap );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_XYZ|D3DFVF_TEX2,
                                     m_avWallVertices, 10, NULL );
    }
    
    // Multi-pass using color blending
    else if( m_wTextureMode == TEXTURE_MULTIPASSCOLOR ) 
    {
        // Draw the wall, using the single-texturing for the 1st pass
        m_pd3dDevice->SetTexture( 0, m_pddsWallTexture );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_XYZ|D3DFVF_TEX2,
                                     m_avWallVertices, 10, NULL );

        // Turn on color blending
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_SRCCOLOR );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );

        // Use the lightmap texture for the 2nd pass
        m_pd3dDevice->SetTexture( 0, m_pddsLightMap );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_TEXCOORDINDEX, 1 );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_XYZ|D3DFVF_TEX2,
                                     m_avWallVertices, 10, NULL );
        // Restore state
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );
    }
    
    // Multi-pass using alpha blending
    else if( m_wTextureMode == TEXTURE_MULTIPASSALPHA ) 
    {
        // Draw the wall, using the single-texturing for the 1st pass
        m_pd3dDevice->SetTexture( 0, m_pddsWallTexture );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_XYZ|D3DFVF_TEX2,
                                     m_avWallVertices, 10, NULL );

        // Turn on alpha blending
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );

        // Use the inverse-alpha lightmap texture for the 2nd pass
        m_pd3dDevice->SetTexture( 0, m_pddsAlphaLightMap );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_TEXCOORDINDEX, 1 );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_XYZ|D3DFVF_TEX2,
                                     m_avWallVertices, 10, NULL );
        // Restore state
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );
    }

    // End the scene.
    m_pd3dDevice->EndScene();

    // Output which rendering technique we're using
    if( m_wTextureMode == TEXTURE_SINGLEPASS )
        OutputText( 5, 20, "Single-pass multi-texture" );

    if( m_wTextureMode == TEXTURE_MULTIPASSCOLOR )
        OutputText( 5, 20, "Multi-pass with color blend" );

    if( m_wTextureMode == TEXTURE_MULTIPASSALPHA )
        OutputText( 5, 20, "Multi-pass with alpha blend" );

    if( m_wTextureMode == TEXTURE_NONE )
        OutputText( 5, 20, "Device cannot do m-texturing" );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: TextureSearchCallback()
// Desc: Callback function used to enumerate texture formats.
//-----------------------------------------------------------------------------
HRESULT CALLBACK TextureSearchCallback( DDPIXELFORMAT* pddpf, VOID* param )
{
    // Skip unwanted formats. We are looking for a 4444 ARGB format.
    if( pddpf->dwRGBAlphaBitMask != 0x0000f000 )
        return DDENUMRET_OK;

    memcpy( (DDPIXELFORMAT*)param, pddpf, sizeof(DDPIXELFORMAT) );
    return DDENUMRET_CANCEL;
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    m_hMenu = GetMenu( m_hWnd );

    // Load the textures to the device
    D3DTextr_RestoreAllTextures( m_pd3dDevice );

    if( NULL == ( m_pddsWallTexture = D3DTextr_GetSurface( "wall.bmp" ) ) )
        return E_FAIL;
    if( NULL == ( m_pddsFloorTexture = D3DTextr_GetSurface( "floor.bmp" ) ) )
        return E_FAIL;
    if( NULL == ( m_pddsLightMap = D3DTextr_GetSurface( "spotlite.bmp" ) ) )
        return E_FAIL;

    // The lightmap is a greyscale 565 RGB texture which is used to build a 
    // a 4444 ARGB texture with an inverted alpha (1-alpha), which is used for
    // alternate multi-texture rendering methods.
    DDPIXELFORMAT ddpfAlphaFormat;
    ddpfAlphaFormat.dwRGBBitCount = 0L;
    m_pd3dDevice->EnumTextureFormats( TextureSearchCallback, &ddpfAlphaFormat );
    if( ddpfAlphaFormat.dwRGBBitCount > 0 )
    {
        if( FAILED( CreateInvAlphaTexFromTex( &m_pddsAlphaLightMap,
                                              m_pddsLightMap,
                                              &ddpfAlphaFormat ) ) )
            return E_FAIL;
    }
    else
        m_pddsAlphaLightMap = NULL;

    // Set the transform matrices
    D3DMATRIX matWorld, matProj;
    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f, 0.0f,-2.5f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 0.0f, 0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );

    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetProjectionMatrix( matProj, g_PI/2, 1.0f, 1.0f, 100.0f );
    SetViewParams( &vEyePt, &vLookatPt, &vUpVec, 0.1f);

    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set any appropiate state
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE, FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT, 0xffffffff );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_TEXTUREPERSPECTIVE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_MAGFILTER, D3DTFG_LINEAR );

    // Set all device capabilites initially to FALSE
    m_bCanDoMultiTexture = FALSE;
    m_bCanDoColorBlend   = FALSE;
    m_bCanDoAlphaBlend   = FALSE;
    
    // Check if the device supports single pass multiple texture.
    if( m_pDeviceInfo->ddDeviceDesc.wMaxTextureBlendStages > 1 )
        if( m_pDeviceInfo->ddDeviceDesc.wMaxSimultaneousTextures > 1 )
            if( m_pDeviceInfo->ddDeviceDesc.dwTextureOpCaps & D3DTEXOPCAPS_MODULATE )
                m_bCanDoMultiTexture = TRUE;

    // Check whether device can do mulit-pass color blending
    if( m_pDeviceInfo->ddDeviceDesc.dpcTriCaps.dwDestBlendCaps & D3DPBLENDCAPS_SRCCOLOR )
        if( m_pDeviceInfo->ddDeviceDesc.dpcTriCaps.dwSrcBlendCaps & D3DPBLENDCAPS_ZERO )
            m_bCanDoColorBlend = TRUE;

    // Check whether device can do multi-pass blending w/alpha textures
    if( m_pddsAlphaLightMap )
        if( m_pDeviceInfo->ddDeviceDesc.dpcTriCaps.dwDestBlendCaps & D3DPBLENDCAPS_INVSRCALPHA )
            if( m_pDeviceInfo->ddDeviceDesc.dpcTriCaps.dwSrcBlendCaps & D3DPBLENDCAPS_SRCALPHA )
                m_bCanDoAlphaBlend = TRUE;

    // Set the menu states for multitexture devices and devices that emulate
    // multitexture using multipass rendering.
    m_wTextureMode = TEXTURE_NONE;
    EnableMenuItem( m_hMenu, IDM_MULTITEXTURE,   MF_GRAYED );
    EnableMenuItem( m_hMenu, IDM_MULTIPASSCOLOR, MF_GRAYED );
    EnableMenuItem( m_hMenu, IDM_MULTIPASSALPHA, MF_GRAYED );

    if( m_bCanDoAlphaBlend )
    {
        m_wTextureMode = TEXTURE_MULTIPASSALPHA;
        EnableMenuItem( m_hMenu, IDM_MULTIPASSALPHA, MF_ENABLED );
        CheckMenuItem(  m_hMenu, IDM_MULTITEXTURE,   MF_UNCHECKED );
        CheckMenuItem(  m_hMenu, IDM_MULTIPASSCOLOR, MF_UNCHECKED );
        CheckMenuItem(  m_hMenu, IDM_MULTIPASSALPHA, MF_CHECKED );
    }
    if( m_bCanDoColorBlend )
    {
        m_wTextureMode = TEXTURE_MULTIPASSCOLOR;
        EnableMenuItem( m_hMenu, IDM_MULTIPASSCOLOR, MF_ENABLED );
        CheckMenuItem(  m_hMenu, IDM_MULTITEXTURE,   MF_UNCHECKED );
        CheckMenuItem(  m_hMenu, IDM_MULTIPASSCOLOR, MF_CHECKED );
        CheckMenuItem(  m_hMenu, IDM_MULTIPASSALPHA, MF_UNCHECKED );
    }
    if( m_bCanDoMultiTexture )
    {
        m_wTextureMode = TEXTURE_SINGLEPASS;
        EnableMenuItem( m_hMenu, IDM_MULTITEXTURE,   MF_ENABLED );
        CheckMenuItem(  m_hMenu, IDM_MULTITEXTURE,   MF_CHECKED );
        CheckMenuItem(  m_hMenu, IDM_MULTIPASSCOLOR, MF_UNCHECKED );
        CheckMenuItem(  m_hMenu, IDM_MULTIPASSALPHA, MF_UNCHECKED );
    }

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
// Name: DeleteDeviceObjects()
// Desc: Called when the app is exitting, or the device is being changed,
//       this function deletes any device dependant objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DeleteDeviceObjects()
{
    D3DTextr_InvalidateAllTextures();

    // Release the alpha lightmap which was explicity created by this app
    SAFE_RELEASE( m_pddsAlphaLightMap );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: ConfirmDevice()
// Desc: Called during device intialization, this code checks the device
//       for some minimum set of capabilities
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::ConfirmDevice( DDCAPS* pddDriverCaps, 
                                          D3DDEVICEDESC7* pd3dDeviceDesc )
{
    // Accept devices that really support multiple textures. 
    if( pd3dDeviceDesc->wMaxTextureBlendStages > 1 )
        if( pd3dDeviceDesc->wMaxSimultaneousTextures > 1 )
            if( pd3dDeviceDesc->dwTextureOpCaps & D3DTEXOPCAPS_MODULATE )
                return S_OK;

    // Accept devices that can do multipass, color blending
    if( pd3dDeviceDesc->dpcTriCaps.dwDestBlendCaps & D3DPBLENDCAPS_SRCCOLOR )
        if( pd3dDeviceDesc->dpcTriCaps.dwSrcBlendCaps & D3DPBLENDCAPS_ZERO )
            return S_OK;

    // Accept devices that can do multipass, alpha blending
    if( pd3dDeviceDesc->dpcTriCaps.dwDestBlendCaps & D3DPBLENDCAPS_INVSRCALPHA )
        if( pd3dDeviceDesc->dpcTriCaps.dwSrcBlendCaps & D3DPBLENDCAPS_SRCALPHA )
            return S_OK;

    return E_FAIL;
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
            case IDM_MULTITEXTURE:
                m_wTextureMode = TEXTURE_SINGLEPASS;
                CheckMenuItem( m_hMenu, IDM_MULTITEXTURE,      MF_CHECKED   );
                CheckMenuItem( m_hMenu, IDM_MULTIPASSCOLOR,    MF_UNCHECKED );
                CheckMenuItem( m_hMenu, IDM_MULTIPASSALPHA,    MF_UNCHECKED );
                break;
            case IDM_MULTIPASSCOLOR:
                m_wTextureMode = TEXTURE_MULTIPASSCOLOR;
                CheckMenuItem( m_hMenu, IDM_MULTITEXTURE,      MF_UNCHECKED   );
                CheckMenuItem( m_hMenu, IDM_MULTIPASSCOLOR,    MF_CHECKED );
                CheckMenuItem( m_hMenu, IDM_MULTIPASSALPHA,    MF_UNCHECKED );
                break;
            case IDM_MULTIPASSALPHA:
                m_wTextureMode = TEXTURE_MULTIPASSALPHA;
                CheckMenuItem( m_hMenu, IDM_MULTITEXTURE,      MF_UNCHECKED   );
                CheckMenuItem( m_hMenu, IDM_MULTIPASSCOLOR,    MF_UNCHECKED );
                CheckMenuItem( m_hMenu, IDM_MULTIPASSALPHA,    MF_CHECKED );
                break;
        }
    }
    return CD3DApplication::MsgProc( hWnd, uMsg, wParam, lParam );
}




//-----------------------------------------------------------------------------
// Name: CreateInvAlphaTexFromTex()
// Desc: Copy (and convert) a 565 texture into a new 4444 ARGB texture.
//       The complement of the upper 4 of 5 source R bits are copied into the
//       four dest A bits. The complement is equivalent to 1-alpha when the
//       alpha scale is from 0 to 1. The dest RGB values are set to 0.
//
//       Note that the source texture is ASSUMED to be 565!
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::CreateInvAlphaTexFromTex(
                                      LPDIRECTDRAWSURFACE7* ppddsDst,
                                      LPDIRECTDRAWSURFACE7 pddsSrc,
                                      DDPIXELFORMAT* pddpf )
{
    HRESULT hr;

    // Initially set the destination texture ptr to NULL...in case of failure
    (*ppddsDst) = NULL;

    // Prepare a DDSURFACEDESC to create the destination texture surface
    DDSURFACEDESC2 ddsd;
    ddsd.dwSize = sizeof(DDSURFACEDESC2);
    if( FAILED( hr = pddsSrc->GetSurfaceDesc( &ddsd ) ) )
        return hr;

    // Modify the surface description obtained from the source texture to 
    // be 4444 ARGB, for creating the new texture.
    ddsd.dwFlags         = DDSD_CAPS|DDSD_WIDTH|DDSD_HEIGHT|DDSD_PIXELFORMAT;
    ddsd.ddsCaps.dwCaps  = DDSCAPS_TEXTURE;
    ddsd.ddsCaps.dwCaps2 = 0;
    ddsd.ddsCaps.dwCaps3 = 0;
    ddsd.ddsCaps.dwCaps4 = 0;
    memcpy( &ddsd.ddpfPixelFormat, pddpf, sizeof(DDPIXELFORMAT) );

    // Turn on texture management for hardware devices
    if( m_pDeviceInfo->bHardware )
        ddsd.ddsCaps.dwCaps2 |= DDSCAPS2_TEXTUREMANAGE;
    else
        ddsd.ddsCaps.dwCaps  |= DDSCAPS_SYSTEMMEMORY;

    // Create the destination texture's surface
    if( FAILED( hr = m_pDD->CreateSurface( &ddsd, ppddsDst, NULL ) ) )
        return hr;

    // Lock the surfaces to get access to their bits
    pddsSrc->Lock( NULL, &ddsd, DDLOCK_WAIT, 0 );
    DWORD lSrcPitch = ddsd.lPitch;
    WORD* pSrc      = (WORD*)ddsd.lpSurface;

    (*ppddsDst)->Lock( NULL, &ddsd, DDLOCK_WAIT, 0 );
    DWORD lDstPitch = ddsd.lPitch;
    WORD* pDst      = (WORD*)ddsd.lpSurface;

    // Copy the source texture to the new texture, converting RGB to ARGB in
    // the process, with alpha inverted and all new RGB bits set to 0.
    // Bitwise-notting the alpha bits is equivalent to doing 1-alpha.
    for( DWORD y = 0; y < ddsd.dwHeight; y++ )
    {
        WORD* pCurrentSrcWord = pSrc;
        WORD* pCurrentDstWord = pDst;

        for( DWORD x = 0; x < ddsd.dwWidth; x++ )
        {
            // Mask the most significant 4 bits of the red component from the
            // source 565 RGB texture, and use it as the alpha component of the
            // destination 4444 ARGB texture.
            *pCurrentDstWord = 0xf000 & (~(*pCurrentSrcWord));
            
            pCurrentDstWord++;
            pCurrentSrcWord++;
        }
        pSrc += lSrcPitch/sizeof(WORD); // Move to the next line
        pDst += lDstPitch/sizeof(WORD);
    }

    (*ppddsDst)->Unlock(0);
    pddsSrc->Unlock(0);
    
    return hr;
}




