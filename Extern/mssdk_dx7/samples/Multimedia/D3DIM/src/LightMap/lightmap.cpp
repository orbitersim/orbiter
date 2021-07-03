//-----------------------------------------------------------------------------
// File: Lightmap.cpp
//
// Desc: Example code showing how to enable multiple-texturing. This
//       samples shows the interior of a room "lit" with a light map
//       using mulitple textures.
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
// Define a custom vertex that uses XYZ, a color, and two sets of tex coords
//-----------------------------------------------------------------------------
struct MTVERTEX
{
    FLOAT x, y, z;
    DWORD dwColor;
    FLOAT tuBase, tvBase;
    FLOAT tuLightMap, tvLightMap;
};

#define FILL_MTVERTEX( v, ax, ay, az, atu1, atv1, atu2, atv2 )  \
{   v.x = ax; v.y = ay; v.z = az; \
    v.dwColor = 0xffffffff; \
    v.tuBase     = atu1; v.tvBase     = atv1; \
    v.tuLightMap = atu2; v.tvLightMap = atv2; \
}




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    HMENU     m_hMenu;
    BOOL      m_bCanDoMultiTexture;     // Whether device does mtexture
    BOOL      m_bUseMultiTexture;

    D3DMATRIX m_matLightBillboardingMatrix;
    D3DVERTEX m_avLightVertices[4];
    D3DVERTEX m_avStringVertices[2];
    MTVERTEX  m_avWallVertices[36];
    D3DDRAWPRIMITIVESTRIDEDDATA m_WallData;      // Vertex data
    D3DDRAWPRIMITIVESTRIDEDDATA m_FloorCielData;

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
    m_strWindowTitle  = TEXT( "Lightmap: D3D Lightmap Demo" );
    m_bAppUseZBuffer  = FALSE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = ConfirmDevice;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Declare the vertices which define the room's walls, floor, and ceiling
    FILL_MTVERTEX( m_avWallVertices[ 0], -5,-5, 5, 0, 1, 0, 1 );
    FILL_MTVERTEX( m_avWallVertices[ 1], -5, 5, 5, 0, 0, 0, 0 );
    FILL_MTVERTEX( m_avWallVertices[ 2],  5,-5, 5, 1, 1, 1, 1 );
    FILL_MTVERTEX( m_avWallVertices[ 3],  5, 5, 5, 1, 0, 1, 0 );
    FILL_MTVERTEX( m_avWallVertices[ 4],  5,-5, 5, 1, 1, 1, 1 );
    FILL_MTVERTEX( m_avWallVertices[ 5], -5, 5, 5, 0, 0, 0, 0 );

    FILL_MTVERTEX( m_avWallVertices[ 6],  5,-5, 5, 0, 1, 0, 1 );
    FILL_MTVERTEX( m_avWallVertices[ 7],  5, 5, 5, 0, 0, 0, 0 );
    FILL_MTVERTEX( m_avWallVertices[ 8],  5,-5,-5, 1, 1, 1, 1 );
    FILL_MTVERTEX( m_avWallVertices[ 9],  5, 5,-5, 1, 0, 1, 0 );
    FILL_MTVERTEX( m_avWallVertices[10],  5,-5,-5, 1, 1, 1, 1 );
    FILL_MTVERTEX( m_avWallVertices[11],  5, 5, 5, 0, 0, 0, 0 );

    FILL_MTVERTEX( m_avWallVertices[12],  5,-5,-5, 0, 1, 0, 1 );
    FILL_MTVERTEX( m_avWallVertices[13],  5, 5,-5, 0, 0, 0, 0 );
    FILL_MTVERTEX( m_avWallVertices[14], -5,-5,-5, 1, 1, 1, 1 );
    FILL_MTVERTEX( m_avWallVertices[15], -5, 5,-5, 1, 0, 1, 0 );
    FILL_MTVERTEX( m_avWallVertices[16], -5,-5,-5, 1, 1, 1, 1 );
    FILL_MTVERTEX( m_avWallVertices[17],  5, 5,-5, 0, 0, 0, 0 );

    FILL_MTVERTEX( m_avWallVertices[18], -5,-5,-5, 0, 1, 0, 1 );
    FILL_MTVERTEX( m_avWallVertices[19], -5, 5,-5, 0, 0, 0, 0 );
    FILL_MTVERTEX( m_avWallVertices[20], -5,-5, 5, 1, 1, 1, 1 );
    FILL_MTVERTEX( m_avWallVertices[21], -5, 5, 5, 1, 0, 1, 0 );
    FILL_MTVERTEX( m_avWallVertices[22], -5,-5, 5, 1, 1, 1, 1 );
    FILL_MTVERTEX( m_avWallVertices[23], -5, 5,-5, 0, 0, 0, 0 );

    FILL_MTVERTEX( m_avWallVertices[24],  5,-5, 5, 0, 1, 0, 1 );
    FILL_MTVERTEX( m_avWallVertices[25],  5,-5,-5, 1, 1, 1, 1 );
    FILL_MTVERTEX( m_avWallVertices[26], -5,-5, 5, 0, 0, 0, 0 );
    FILL_MTVERTEX( m_avWallVertices[27],  5,-5,-5, 1, 1, 1, 1 );
    FILL_MTVERTEX( m_avWallVertices[28], -5,-5,-5, 1, 0, 1, 0 );
    FILL_MTVERTEX( m_avWallVertices[29], -5,-5, 5, 0, 0, 0, 0 );

    FILL_MTVERTEX( m_avWallVertices[30],  5, 5, 5, 0, 1, 0, 1 );
    FILL_MTVERTEX( m_avWallVertices[31], -5, 5, 5, 0, 0, 0, 0 );
    FILL_MTVERTEX( m_avWallVertices[32],  5, 5,-5, 1, 1, 1, 1 );
    FILL_MTVERTEX( m_avWallVertices[33], -5, 5,-5, 1, 0, 1, 0 );
    FILL_MTVERTEX( m_avWallVertices[34],  5, 5,-5, 1, 1, 1, 1 );
    FILL_MTVERTEX( m_avWallVertices[35], -5, 5, 5, 0, 0, 0, 0 );

    // Setup the vertices for the hanging light and its string
    D3DVECTOR vNorm(0,1,0);
    m_avLightVertices[0]  = D3DVERTEX( D3DVECTOR(-0.5, 0.5, 0), vNorm, 0, 0 );
    m_avLightVertices[1]  = D3DVERTEX( D3DVECTOR(-0.5,-0.5, 0), vNorm, 1, 0 );
    m_avLightVertices[2]  = D3DVERTEX( D3DVECTOR( 0.5, 0.5, 0), vNorm, 0, 1 );
    m_avLightVertices[3]  = D3DVERTEX( D3DVECTOR( 0.5,-0.5, 0), vNorm, 1, 1 );
    m_avStringVertices[0] = D3DVERTEX( D3DVECTOR(0,5,0), vNorm, 0.0f, 0.0f );
    m_avStringVertices[1] = D3DVERTEX( D3DVECTOR(0,5,0), vNorm, 0.5f, 0.5f );

    // Set up data structures for using strided vertices.
    m_WallData.position.lpvData          = &m_avWallVertices[0].x;
    m_WallData.diffuse.lpvData           = &m_avWallVertices[0].dwColor;
    m_WallData.textureCoords[0].lpvData  = &m_avWallVertices[0].tuBase;
    m_WallData.textureCoords[1].lpvData  = &m_avWallVertices[0].tuLightMap;
    m_WallData.position.dwStride         = sizeof(MTVERTEX);
    m_WallData.diffuse.dwStride          = sizeof(MTVERTEX);
    m_WallData.textureCoords[0].dwStride = sizeof(MTVERTEX);
    m_WallData.textureCoords[1].dwStride = sizeof(MTVERTEX);

    m_FloorCielData.position.lpvData          = &m_avWallVertices[24].x;
    m_FloorCielData.diffuse.lpvData           = &m_avWallVertices[24].dwColor;
    m_FloorCielData.textureCoords[0].lpvData  = &m_avWallVertices[24].tuBase;
    m_FloorCielData.textureCoords[1].lpvData  = &m_avWallVertices[24].tuLightMap;
    m_FloorCielData.position.dwStride         = sizeof(MTVERTEX);
    m_FloorCielData.diffuse.dwStride          = sizeof(MTVERTEX);
    m_FloorCielData.textureCoords[0].dwStride = sizeof(MTVERTEX);
    m_FloorCielData.textureCoords[1].dwStride = sizeof(MTVERTEX);

    // Create some textures. The number passed in is a hint (for some hardware)
    // as to which texture stage state the texture will be used.
    D3DTextr_CreateTextureFromFile( "wall.bmp",   0 );
    D3DTextr_CreateTextureFromFile( "floor.bmp",  0 );
    D3DTextr_CreateTextureFromFile( "lightmap.bmp", 1 );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Rotate the camera around the room
    FLOAT fViewAngle = fTimeKey/5;
    D3DMATRIX matView;
    D3DVECTOR up(0.0f, 1.0f, 0.0f);
    D3DVECTOR from( (FLOAT)sin(fViewAngle), 0, (FLOAT)cos(fViewAngle) );
    D3DVECTOR at( 0.0f, 0.0f, 0.0f );
    D3DUtil_SetViewMatrix( matView, 5.0f * from, at, up );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );

    // Move light, as a swinging pendulum
    FLOAT     fPendulumTheta = 1.0f*(FLOAT)sin( fTimeKey );
    D3DVECTOR vLightPos;
    vLightPos.x = 0.0f + 3.0f*(FLOAT)sin( fPendulumTheta );
    vLightPos.y = 3.0f - 3.0f*(FLOAT)cos( fPendulumTheta );
    vLightPos.z = 0.0f;

    // Position the end of the string
    m_avStringVertices[1].x = vLightPos.x;
    m_avStringVertices[1].y = vLightPos.y;

    // Build the billboarding matrix for the light (so the light always 
    // renders face-on to the camera)
    D3DUtil_SetRotateYMatrix( m_matLightBillboardingMatrix, fViewAngle );
    m_matLightBillboardingMatrix._41 += vLightPos.x;
    m_matLightBillboardingMatrix._42 += vLightPos.y;
    m_matLightBillboardingMatrix._43 += vLightPos.z;

    // Apply the lightmap to the six sides of the room
    for( int w=0; w<6; w++ )
    {
        FLOAT fDistance, tuLightPos, tvLightPos;

        // Calc the lightmaps tex coords based on the light pos
        switch( w )
        {
            case 0: fDistance = 5.0f;
                    tuLightPos = -vLightPos.x/5;
                    tvLightPos = vLightPos.y/5;
                    break;
            case 1: fDistance = 5.0f - vLightPos.x;
                    tuLightPos = 0.0f;
                    tvLightPos = vLightPos.y/5;
                    break;
            case 2: fDistance = 5.0f;
                    tuLightPos = vLightPos.x/5;
                    tvLightPos = vLightPos.y/5;
                    break;
            case 3: fDistance = 5.0f + vLightPos.x;
                    tuLightPos = 0.0f;
                    tvLightPos = vLightPos.y/5;
                    break;
            case 4: fDistance = 5.0f + vLightPos.y;
                    tuLightPos = 0.0f;
                    tvLightPos = -vLightPos.x/5;
                    break;
            case 5: fDistance = 5.0f - vLightPos.y;
                    tuLightPos = 0.0f;
                    tvLightPos = -vLightPos.x/5;
                    break;
        }

        // Calculate the size and color for the lightmaps' vertices
        FLOAT fBrightnessScale = (7.5f-fDistance)*(7.5f-fDistance)/30.0f;
        DWORD dwDiffuse        = 0x01010101 * (DWORD)(255*fBrightnessScale);
        FLOAT fSizeScale       = 1.0f + fDistance/5.0f;

        // Apply the lightmap to the vertices
        for( int v=0; v<6; v++ )
        {
            // Translate and scale the lightmap
            FLOAT tu = m_avWallVertices[6*w+v].tuBase - 0.5f + fSizeScale/2;
            FLOAT tv = m_avWallVertices[6*w+v].tvBase - 0.5f + fSizeScale/2;
            m_avWallVertices[6*w+v].tuLightMap = ( tu + tuLightPos ) / fSizeScale;
            m_avWallVertices[6*w+v].tvLightMap = ( tv + tvLightPos ) / fSizeScale;
            
            // Modify the brightness of the lightmap
            m_avWallVertices[6*w+v].dwColor = dwDiffuse;
        }
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
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        if( m_bUseMultiTexture ) // Use mtexture features of device
        {
            // Set up the texture stages (Note: we don't really need to do this
            // every frame)
            m_pd3dDevice->SetTextureStageState( 0, D3DTSS_TEXCOORDINDEX, 0 );
            m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
            m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
            m_pd3dDevice->SetTextureStageState( 1, D3DTSS_TEXCOORDINDEX, 1 );
            m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLORARG1, D3DTA_TEXTURE );
            m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLORARG2, D3DTA_CURRENT ); 
            m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP,   D3DTOP_MODULATE );

            m_WallData.textureCoords[0].lpvData  = &m_avWallVertices[0].tuBase;
            m_WallData.textureCoords[1].lpvData  = &m_avWallVertices[0].tuLightMap;

            // Draw the walls in multi-texture mode
            m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("wall.bmp") );
            m_pd3dDevice->SetTexture( 1, D3DTextr_GetSurface("lightmap.bmp") );
            m_pd3dDevice->DrawPrimitiveStrided( D3DPT_TRIANGLELIST,
                                        D3DFVF_XYZ|D3DFVF_DIFFUSE|D3DFVF_TEX2,
                                        &m_WallData, 24, NULL );

            // Draw the floor in single-texture mode
            m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("floor.bmp") );
            m_pd3dDevice->SetTexture( 1, NULL );
            m_pd3dDevice->DrawPrimitiveStrided( D3DPT_TRIANGLELIST,
                                        D3DFVF_XYZ|D3DFVF_DIFFUSE|D3DFVF_TEX2,
                                        &m_FloorCielData, 12, NULL );

            // Restore state
            m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP, D3DTOP_DISABLE );
        }
        else // Else, resort to multipass rendering
        {
            // Draw the first textures normally. Use the 1st set of tex coords.
            m_pd3dDevice->SetTextureStageState( 0, D3DTSS_TEXCOORDINDEX, 0 );
            m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
            m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );

            m_WallData.textureCoords[0].lpvData = &m_avWallVertices[0].tuBase;
            m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("wall.bmp") );
            m_pd3dDevice->DrawPrimitiveStrided( D3DPT_TRIANGLELIST,
                                        D3DFVF_XYZ|D3DFVF_DIFFUSE|D3DFVF_TEX2,
                                        &m_WallData, 24, NULL );
            m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("floor.bmp") );
            m_pd3dDevice->DrawPrimitiveStrided( D3DPT_TRIANGLELIST,
                                        D3DFVF_XYZ|D3DFVF_DIFFUSE|D3DFVF_TEX2,
                                        &m_FloorCielData, 12, NULL );

            // Draw the lightmap using blending, with the 2nd set of tex coords
            m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );
            m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO );
            m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_SRCCOLOR );

            m_WallData.textureCoords[0].lpvData = &m_avWallVertices[0].tuLightMap;
            m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("lightmap.bmp") );
            m_pd3dDevice->DrawPrimitiveStrided( D3DPT_TRIANGLELIST,
                                        D3DFVF_XYZ|D3DFVF_DIFFUSE|D3DFVF_TEX2,
                                        &m_WallData, 36, NULL );
            // Restore state
            m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );
        }

        // Draw the string
        m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("lightmap.bmp") );
        m_pd3dDevice->DrawPrimitive( D3DPT_LINELIST, D3DFVF_VERTEX, 
                                     m_avStringVertices, 2, NULL );
        // Draw the light
        D3DMATRIX matSave;
        m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD, &matSave );
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,
                                    &m_matLightBillboardingMatrix );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ONE );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, 
                                     m_avLightVertices, 4, NULL );
        // Restore state
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matSave );

        // End the scene.
        m_pd3dDevice->EndScene();

        // Output which rendering technique we're using
        if( m_bUseMultiTexture )
            OutputText( 0, 20, "Using Multi-texture" );
        else
            OutputText( 0, 20, "Using Multi-pass" );
    }

    return S_OK;
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

    // Set the menu states for multitexture devices and devices that emulate
    // multitexture using multipass rendering.
    if( m_pDeviceInfo->ddDeviceDesc.wMaxSimultaneousTextures >=2 )
    {
        EnableMenuItem( m_hMenu, IDM_MULTITEXTURE, MF_ENABLED );
        CheckMenuItem(  m_hMenu, IDM_MULTIPASS,    MF_UNCHECKED );
        CheckMenuItem(  m_hMenu, IDM_MULTITEXTURE, MF_CHECKED );
        m_bUseMultiTexture = TRUE;
        m_bCanDoMultiTexture = TRUE;
    }
    else
    {
        EnableMenuItem( m_hMenu, IDM_MULTITEXTURE, MF_GRAYED );
        CheckMenuItem(  m_hMenu, IDM_MULTIPASS,    MF_CHECKED );
        CheckMenuItem(  m_hMenu, IDM_MULTITEXTURE, MF_UNCHECKED );
        m_bUseMultiTexture = FALSE;
        m_bCanDoMultiTexture = FALSE;
    }

    // Set the transform matrices
    D3DMATRIX matWorld, matView, matProj;
    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetProjectionMatrix( matProj, g_PI/2, 1.0f, 1.0f, 100.0f );

    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set any appropiate state
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_TEXTUREPERSPECTIVE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,       TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE,     FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING,           FALSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP );

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
    // Accept devices that really support multiple textures. If not, accept
    // device that support alpha blending to emulate multi-texturing with
    // mulit-pass rendering.
    if( pd3dDeviceDesc->wMaxSimultaneousTextures > 1 )
        return S_OK;
    if( pd3dDeviceDesc->dpcTriCaps.dwDestBlendCaps & D3DPBLENDCAPS_SRCCOLOR )
        if( pd3dDeviceDesc->dpcTriCaps.dwSrcBlendCaps & D3DPBLENDCAPS_ZERO )
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
            case IDM_MULTIPASS:
                m_bUseMultiTexture = FALSE;
                CheckMenuItem( m_hMenu, IDM_MULTIPASS,    MF_CHECKED );
                CheckMenuItem( m_hMenu, IDM_MULTITEXTURE, MF_UNCHECKED );
                break;
            case IDM_MULTITEXTURE:
                m_bUseMultiTexture = TRUE;
                CheckMenuItem( m_hMenu, IDM_MULTIPASS,    MF_UNCHECKED );
                CheckMenuItem( m_hMenu, IDM_MULTITEXTURE, MF_CHECKED );
                break;
        }
    }
    return CD3DApplication::MsgProc( hWnd, uMsg, wParam, lParam );
}



