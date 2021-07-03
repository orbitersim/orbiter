//-----------------------------------------------------------------------------
// File: EnvCube.cpp
//
// Desc: Example code showing how to do environment cube-mapping.
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <windows.h>
#include <math.h>
#include <stdio.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"
#include "D3DFile.h"




// Stuff to make a sphere
#define PI  ( 3.14159265358979f)
#define D2R ( 0.0174532925199433f)
#define R2D (57.2957795130823f)
#define SPH_NUM    14       // Number of lattitude lines
#define SPH_TRIS   (2*(SPH_NUM*2) + (SPH_NUM-2)*(SPH_NUM*4))
#define SPH_VTXS   (SPH_TRIS*3)
#define SPH_RADIUS 0.35f

struct SPHVERTEX
{
    D3DVERTEX v;
    FLOAT     nz;
};


// Structure to hold information for a environment texture
struct EnvMapContainer
{
    LPDIRECTDRAWSURFACE7 pddsSurface[6];    // Surfaces of the map
    DWORD     dwWidth;
    DWORD     dwHeight;
};




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    CD3DFile*        m_pFileObject1;
    CD3DFile*        m_pFileObject2;
    CD3DFile*        m_pEnvCubeObject;
    D3DVIEWPORT7     m_vpViewport;
    D3DMATERIAL7     m_mtrl;
    D3DVECTOR        m_vEyePt;
    SPHVERTEX        m_SphVtx[SPH_VTXS];
    EnvMapContainer* m_pEnvMap;

    VOID    CreateSphere();
    VOID    ReflectNormals( SPHVERTEX *pVIn, LONG cV );
    HRESULT CreateEnvMap( EnvMapContainer** );
    VOID    DeleteEnvMap( EnvMapContainer* );
    HRESULT RestoreEnvMap( EnvMapContainer* );
    VOID    InvalidateEnvMap( EnvMapContainer* );
    HRESULT RenderEnvMap( EnvMapContainer* pEnvMap );
    HRESULT ChangeRenderTarget( LPDIRECTDRAWSURFACE7 );

    static HRESULT ConfirmDevice( DDCAPS* pddDriverCaps,
                                  D3DDEVICEDESC7* pd3dDeviceDesc );

protected:
    HRESULT OneTimeSceneInit();
    HRESULT InitDeviceObjects();
    HRESULT DeleteDeviceObjects();
    HRESULT Render();
    HRESULT FrameMove( FLOAT fTimeKey );
    HRESULT RestoreSurfaces();
    HRESULT FinalCleanup();

public:
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
    m_strWindowTitle  = TEXT( "EnvCube: Environment cube-mapping" );
    m_bAppUseZBuffer  = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = ConfirmDevice;

    m_pFileObject1   = NULL;
    m_pFileObject2   = NULL;
    m_pEnvCubeObject = NULL;
    m_vEyePt         = D3DVECTOR( 0.0f, 0.0f, 3.0f );
    m_pEnvMap        = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    CreateSphere();

    m_pFileObject1 = new CD3DFile();
    if( FAILED( m_pFileObject1->Load( "car.x" ) ) )
        MessageBox( NULL, TEXT("Error loading car.x file"),
                    TEXT("EnvMap"), MB_OK|MB_ICONERROR );

    m_pFileObject2 = new CD3DFile();
    if( FAILED( m_pFileObject2->Load( "tiger.x" ) ) )
        MessageBox( NULL, TEXT("Error loading tiger.x file"),
                    TEXT("EnvMap"), MB_OK|MB_ICONERROR );

    m_pEnvCubeObject = new CD3DFile();
    if( FAILED( m_pEnvCubeObject->Load( "envcube.x" ) ) )
        MessageBox( NULL, TEXT("Error loading envcube.x file"),
                    TEXT("EnvMap"), MB_OK|MB_ICONERROR );

    if( FAILED( CreateEnvMap( &m_pEnvMap ) ) )
    {
        MessageBox( NULL, TEXT( "Could not create EnvMap" ),
                    TEXT( "EnvMap" ), MB_ICONWARNING | MB_OK );
        DEBUG_MSG( "Could not create EnvMap" );
        return E_FAIL;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Animate 1st file object
    if( m_pFileObject1 )
    {
        CD3DFileObject* pObject = m_pFileObject1->FindObject( "CAR20_Root" );
        if( pObject )
        {
            D3DMATRIX* pmat = pObject->GetMatrix();
            D3DMATRIX  mat1;

            D3DUtil_SetScaleMatrix( *pmat, 0.4f, 0.4f, 0.4f );
            D3DUtil_SetTranslateMatrix( mat1, 3.0f, 0.0f, 0.0f );
            D3DMath_MatrixMultiply( *pmat, *pmat, mat1 );

            D3DUtil_SetRotateYMatrix( mat1, 0.1f*fTimeKey );
            D3DMath_MatrixMultiply( *pmat, *pmat, mat1 );
        }
    }

    // Animate 2nd file object
    if( m_pFileObject2 )
    {
        CD3DFileObject* pObject = m_pFileObject2->FindObject( "D3DFile_Root" );
        if( pObject )
        {
            D3DMATRIX* pmat = pObject->GetMatrix();
            D3DMATRIX  mat1;

            D3DUtil_SetScaleMatrix( *pmat, 0.5f, 0.5f, 0.5f );
            D3DUtil_SetTranslateMatrix( mat1, 0.0f, 2.4f, 0.0f );
            D3DMath_MatrixMultiply( *pmat, *pmat, mat1 );

            D3DUtil_SetRotateXMatrix( mat1, -0.5f*fTimeKey );
            D3DMath_MatrixMultiply( *pmat, *pmat, mat1 );

            D3DUtil_SetRotateYMatrix( mat1, 0.025f*fTimeKey );
            D3DMath_MatrixMultiply( *pmat, *pmat, mat1 );
        }
    }

    // Animate envcube
    if( m_pEnvCubeObject )
    {
        CD3DFileObject* pObject = m_pEnvCubeObject->FindObject( "CUBE_Root" );
        if( pObject )
        {
            D3DMATRIX* pmat = pObject->GetMatrix();
            D3DMATRIX  mat1;

            D3DUtil_SetScaleMatrix( *pmat, 32.0f, 32.0f, 32.0f );
            D3DUtil_SetRotateXMatrix( mat1, 0.2f*fTimeKey );
            D3DMath_MatrixMultiply( *pmat, *pmat, mat1 );

            D3DUtil_SetRotateYMatrix( mat1, -0.1f*fTimeKey );
            D3DMath_MatrixMultiply( *pmat, *pmat, mat1 );
        }
    }

    RenderEnvMap( m_pEnvMap );
    ReflectNormals( &m_SphVtx[0], SPH_VTXS );

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
    // Restore the viewport. We do this because of potential bad-timing between
    // SetRenderTarget() and the user alt-tabbing away from our app can cause
    // an inconsistency in viewport dimensions.
    m_pd3dDevice->SetViewport( &m_vpViewport );

    // Clear the viewport
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_ZBUFFER, 0x000000ff, 1.0f, 0L );

    // Begin the scene
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        if( m_pFileObject1 )
            m_pFileObject1->Render( m_pd3dDevice );
        if( m_pFileObject2 )
            m_pFileObject2->Render( m_pd3dDevice );
        if( m_pEnvCubeObject )
            m_pEnvCubeObject->Render( m_pd3dDevice );

        m_pd3dDevice->SetTexture( 0, m_pEnvMap->pddsSurface[0] );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_TEXCOORDINDEX, 0 );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP, D3DTOP_MODULATE );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAOP, D3DTOP_MODULATE );

        D3DMATRIX matSave;
        D3DMATRIX matWorld;

        m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD,      &matSave );

        D3DUtil_SetScaleMatrix( matWorld, 5.0f, 5.0f, 5.0f );

        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );

        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLELIST,
                                     D3DFVF_XYZ|D3DFVF_NORMAL|D3DFVF_TEX1|D3DFVF_TEXCOORDSIZE3(0),
                                     &m_SphVtx[0], SPH_VTXS, 0x0 );

        // Put everything back
        m_pd3dDevice->SetTexture( 0, NULL );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_TEXCOORDINDEX, 0 );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP, D3DTOP_SELECTARG1 );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1 );
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matSave );

        // End the scene.
        m_pd3dDevice->EndScene();
    }
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CMyD3DApplication::InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    if( FAILED( RestoreEnvMap( m_pEnvMap ) ) )
        return E_FAIL;

    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MIPFILTER, D3DTFP_NONE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ADDRESSU, D3DTADDRESS_MIRROR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ADDRESSV, D3DTADDRESS_MIRROR );

    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_TEXTUREPERSPECTIVE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,        TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_CULLMODE,       D3DCULL_NONE );

    // Set the transform matrices
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 0.0f,  0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f,  0.0f );
    D3DMATRIX matWorld, matView, matProj;

    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetViewMatrix( matView, m_vEyePt, vLookatPt, vUpVec );
    D3DUtil_SetProjectionMatrix( matProj, g_PI/1.8f, 1.0f, 0.5f, 500.0f );

    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Save the viewport settings for use in the Render() loop.
    m_pd3dDevice->GetViewport(&m_vpViewport);

    // Setup a material
    D3DUtil_InitMaterial( m_mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &m_mtrl );

    // Set up the light
    if( m_pDeviceInfo->ddDeviceDesc.dwVertexProcessingCaps &
                                                D3DVTXPCAPS_DIRECTIONALLIGHTS )
    {
        D3DLIGHT7 light;
        D3DUtil_InitLight( light, D3DLIGHT_DIRECTIONAL, 0.0f, 0.0f, -1.0f );
        light.dcvAmbient.r = 0.3f;
        light.dcvAmbient.g = 0.3f;
        light.dcvAmbient.b = 0.3f;
        m_pd3dDevice->SetLight( 0, &light );
        m_pd3dDevice->LightEnable( 0, TRUE );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, TRUE );
    }
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT, 0x00a0a0a0 );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RestoreSurfaces()
// Desc: Restores DDraw surfaces that are lost from application switching.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RestoreSurfaces()
{
    if( m_pEnvMap )
    {
        for( DWORD i=0; i<6; i++ )
        {
            if( m_pEnvMap->pddsSurface[i] )
                m_pEnvMap->pddsSurface[i]->Restore();
        }
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
    DeleteEnvMap( m_pEnvMap );

    SAFE_DELETE( m_pFileObject1 );
    SAFE_DELETE( m_pFileObject2 );
    SAFE_DELETE( m_pEnvCubeObject );

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
    InvalidateEnvMap( m_pEnvMap );

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
    // Check for cubemapping devices
    if( pd3dDeviceDesc->dpcTriCaps.dwTextureCaps & D3DPTEXTURECAPS_CUBEMAP )
        return S_OK;

    return E_FAIL;
}




//----------------------------------------------------------------------------
// Name: CreateSphere()
// Desc: Stuff to make a sphere
//----------------------------------------------------------------------------
VOID CMyD3DApplication::CreateSphere()
{
    int x, y, iSph = 0;
    FLOAT fDAngY  = ((180.0f/(FLOAT)SPH_NUM)*D2R);
    FLOAT fDAngX  = ((360.0f/(FLOAT)(SPH_NUM*2))*D2R);
    FLOAT fDAngY0 = fDAngY;
    FLOAT fDAngY1 = fDAngY0 + fDAngY;

    // Make middle
    for( y = 0; y < (SPH_NUM-2); y++ )
    {
        FLOAT y0 = cosf(fDAngY0);
        FLOAT y1 = cosf(fDAngY1);
        FLOAT r0 = sinf(fDAngY0);
        FLOAT r1 = sinf(fDAngY1);

        for( x = 0; x < (SPH_NUM*2); x++ )
        {
            FLOAT fDAngX0 = (x+0) * fDAngX;
            FLOAT fDAngX1 = (x+1) * fDAngX;

            if( x == (SPH_NUM*2-1) )
                fDAngX1 = 0.0f;

            FLOAT x00 = r0 * sinf(fDAngX0);
            FLOAT x01 = r0 * sinf(fDAngX1);
            FLOAT x10 = r1 * sinf(fDAngX0);
            FLOAT x11 = r1 * sinf(fDAngX1);

            FLOAT z00 = r0 * cosf(fDAngX0);
            FLOAT z01 = r0 * cosf(fDAngX1);
            FLOAT z10 = r1 * cosf(fDAngX0);
            FLOAT z11 = r1 * cosf(fDAngX1);

            m_SphVtx[iSph++].v = D3DVERTEX( SPH_RADIUS*D3DVECTOR(x00,y0,z00),
                                            D3DVECTOR(x00,y0,z00), 0.0f, 0.0f );
            m_SphVtx[iSph++].v = D3DVERTEX( SPH_RADIUS*D3DVECTOR(x10,y1,z10),
                                            D3DVECTOR(x10,y1,z10), 0.0f, 0.0f );
            m_SphVtx[iSph++].v = D3DVERTEX( SPH_RADIUS*D3DVECTOR(x11,y1,z11),
                                            D3DVECTOR(x11,y1,z11), 0.0f, 0.0f );

            m_SphVtx[iSph++].v = D3DVERTEX( SPH_RADIUS*D3DVECTOR(x00,y0,z00),
                                            D3DVECTOR(x00,y0,z00), 0.0f, 0.0f );
            m_SphVtx[iSph++].v = D3DVERTEX( SPH_RADIUS*D3DVECTOR(x11,y1,z11),
                                            D3DVECTOR(x11,y1,z11), 0.0f, 0.0f );
            m_SphVtx[iSph++].v = D3DVERTEX( SPH_RADIUS*D3DVECTOR(x01,y0,z01),
                                            D3DVECTOR(x01,y0,z01), 0.0f, 0.0f );
        }
        fDAngY0 = fDAngY1;
        fDAngY1 += fDAngY;
    }

    // Make top
    fDAngY1  = fDAngY;
    FLOAT y1 = cosf(fDAngY1);
    FLOAT r1 = sinf(fDAngY1);
    for( x = 0; x < (SPH_NUM*2); x++ )
    {
        FLOAT fDAngX0 = (x+0) * fDAngX;
        FLOAT fDAngX1 = (x+1) * fDAngX;

        if( x == (SPH_NUM*2-1) )
            fDAngX1 = 0.0f;

        FLOAT x10 = r1 * sinf(fDAngX0);
        FLOAT x11 = r1 * sinf(fDAngX1);
        FLOAT z10 = r1 * cosf(fDAngX0);
        FLOAT z11 = r1 * cosf(fDAngX1);

        m_SphVtx[iSph++].v = D3DVERTEX( SPH_RADIUS*D3DVECTOR(0.0f,1.0f,0.0f),
                                        D3DVECTOR(0.0f,1.0f,0.0f), 0.0f, 0.0f );
        m_SphVtx[iSph++].v = D3DVERTEX( SPH_RADIUS*D3DVECTOR(x10,y1,z10),
                                        D3DVECTOR(x10,y1,z10), 0.0f, 0.0f );
        m_SphVtx[iSph++].v = D3DVERTEX( SPH_RADIUS*D3DVECTOR(x11,y1,z11),
                                        D3DVECTOR(x11,y1,z11), 0.0f, 0.0f );
    }

    // Make bottom
    fDAngY1 = fDAngY0;
    y1 = cosf(fDAngY1);
    r1 = sinf(fDAngY1);
    for( x = 0; x < (SPH_NUM*2); x++ )
    {
        FLOAT fDAngX0 = (x+0) * fDAngX;
        FLOAT fDAngX1 = (x+1) * fDAngX;

        if( x == (SPH_NUM*2-1) )
            fDAngX1 = 0.0f;

        // To keep the same orientation
        FLOAT x11 = r1 * sinf(fDAngX0);
        FLOAT x10 = r1 * sinf(fDAngX1);
        FLOAT z11 = r1 * cosf(fDAngX0);
        FLOAT z10 = r1 * cosf(fDAngX1);

        m_SphVtx[iSph++].v = D3DVERTEX( SPH_RADIUS*D3DVECTOR(0.0f,-1.0f,0.0f),
                                        D3DVECTOR(0.0f,-1.0f,0.0f), 0.0f, 0.0f );
        m_SphVtx[iSph++].v = D3DVERTEX( SPH_RADIUS*D3DVECTOR(x10,y1,z10),
                                        D3DVECTOR(x10,y1,z10), 0.0f, 0.0f );
        m_SphVtx[iSph++].v = D3DVERTEX( SPH_RADIUS*D3DVECTOR(x11,y1,z11),
                                        D3DVECTOR(x11,y1,z11), 0.0f, 0.0f );
    }
}




//-----------------------------------------------------------------------------
// Name: ReflectNormals()
// Desc:
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::ReflectNormals( SPHVERTEX* pVIn, LONG cV )
{
    for( LONG i = 0; i < cV; i++ )
    {
        // eye vector (doesn't need to be normalized)
        FLOAT fENX = m_vEyePt.x - pVIn->v.x;
        FLOAT fENY = m_vEyePt.y - pVIn->v.y;
        FLOAT fENZ = m_vEyePt.z - pVIn->v.z;

        FLOAT fNDotE = pVIn->v.nx*fENX + pVIn->v.ny*fENY + pVIn->v.nz*fENZ;
        FLOAT fNDotN = pVIn->v.nx*pVIn->v.nx + pVIn->v.ny*pVIn->v.ny + pVIn->v.nz*pVIn->v.nz;
        fNDotE *= 2.0F;
        // reflected vector
        pVIn->v.tu = pVIn->v.nx*fNDotE - fENX*fNDotN;
        pVIn->v.tv = pVIn->v.ny*fNDotE - fENY*fNDotN;
        pVIn->nz = pVIn->v.nz*fNDotE - fENZ*fNDotN;

        pVIn++;
    }
}




//-----------------------------------------------------------------------------
// Name: CreateEnvMap()
// Desc: Creates the mipmap structure and loads image data from bitmaps.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::CreateEnvMap( EnvMapContainer** ppEnvMap )
{
    (*ppEnvMap) = new EnvMapContainer;
    if( NULL == (*ppEnvMap) )
        return E_FAIL;

    for( int i=0; i<6; i++ )
        (*ppEnvMap)->pddsSurface[i] = NULL;

    (*ppEnvMap)->dwWidth  = 64;
    (*ppEnvMap)->dwHeight = 64;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: TextureSearchCallback()
// Desc: Callback function used to enumerate texture formats.
//-----------------------------------------------------------------------------
HRESULT CALLBACK TextureSearchCallback( DDPIXELFORMAT* pddpf, VOID* param )
{
    DDSURFACEDESC2* pddsd = (DDSURFACEDESC2*)param;

    // Skip unwanted formats
    if( pddpf->dwRGBBitCount != pddsd->dwFlags )
        return DDENUMRET_OK;
    if( pddpf->dwFlags & (DDPF_LUMINANCE|DDPF_ALPHAPIXELS) )
        return DDENUMRET_OK;
    if( pddpf->dwFlags & (DDPF_BUMPLUMINANCE|DDPF_BUMPDUDV) )
        return DDENUMRET_OK;
    if( 0 != pddpf->dwFourCC )
        return DDENUMRET_OK;

    memcpy( &pddsd->ddpfPixelFormat, pddpf, sizeof(DDPIXELFORMAT) );
    return DDENUMRET_CANCEL;
}




//-----------------------------------------------------------------------------
// Name: RestoreEnvmap()
// Desc: Creates the device-dependant surface and for the environment map
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RestoreEnvMap( EnvMapContainer* pEnvMap )
{
    // Check params
    if( NULL == pEnvMap )
        return E_INVALIDARG;

    // Release any previously created objects
    InvalidateEnvMap( pEnvMap );

    // Set up and create the mipmap surface
    DDSURFACEDESC2 ddsd;
    ZeroMemory( &ddsd, sizeof(ddsd) );
    ddsd.dwSize         = sizeof(DDSURFACEDESC2);
    ddsd.dwFlags        = DDSD_CAPS|DDSD_WIDTH|DDSD_HEIGHT|DDSD_PIXELFORMAT;
    ddsd.ddsCaps.dwCaps = DDSCAPS_TEXTURE|DDSCAPS_MIPMAP|DDSCAPS_3DDEVICE;
    ddsd.dwWidth        = pEnvMap->dwWidth;
    ddsd.dwHeight       = pEnvMap->dwHeight;

    // Enumerate a good texture format. Search for a 16-bit format first
    DDSURFACEDESC2 ddsdSearch;
    ddsdSearch.dwFlags = m_ddsdRenderTarget.ddpfPixelFormat.dwRGBBitCount;
    m_pd3dDevice->EnumTextureFormats( TextureSearchCallback, &ddsdSearch );

    // If that wasn't found, check for a 32-bit format
    if( m_ddsdRenderTarget.ddpfPixelFormat.dwRGBBitCount != ddsdSearch.ddpfPixelFormat.dwRGBBitCount )
    {
        return E_FAIL;
    }

    // If we got a good texture format, use it to create the surface
    memcpy( &ddsd.ddpfPixelFormat, &ddsdSearch.ddpfPixelFormat,
            sizeof(DDPIXELFORMAT) );

    ddsd.ddsCaps.dwCaps  = DDSCAPS_COMPLEX|DDSCAPS_3DDEVICE|DDSCAPS_TEXTURE;
    ddsd.ddsCaps.dwCaps2 = DDSCAPS2_CUBEMAP|DDSCAPS2_CUBEMAP_ALLFACES;
    ddsd.ddsCaps.dwCaps3 = 0;
    ddsd.ddsCaps.dwCaps4 = 0;

    // Force system memory for software devices
    if( m_pDeviceInfo->bHardware == FALSE )
        ddsd.ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;

    // Create the envmap surface and texture
    if( FAILED( m_pDD->CreateSurface( &ddsd, &pEnvMap->pddsSurface[0], NULL ) ) )
        return E_FAIL;

    // Loop through each surface in the mipmap, copying the bitmap to the temp
    // surface, and then blitting the temp surface to the real one.
    LPDIRECTDRAWSURFACE7 pddsPrev = pEnvMap->pddsSurface[0];

    for( WORD wNum=1; wNum < 6; wNum++ )
    {
        if( 1 == wNum )
            ddsd.ddsCaps.dwCaps2 = DDSCAPS2_CUBEMAP_NEGATIVEX|DDSCAPS2_CUBEMAP;
        if( 2 == wNum )
            ddsd.ddsCaps.dwCaps2 = DDSCAPS2_CUBEMAP_POSITIVEY|DDSCAPS2_CUBEMAP;
        if( 3 == wNum )
            ddsd.ddsCaps.dwCaps2 = DDSCAPS2_CUBEMAP_NEGATIVEY|DDSCAPS2_CUBEMAP;
        if( 4 == wNum )
            ddsd.ddsCaps.dwCaps2 = DDSCAPS2_CUBEMAP_POSITIVEZ|DDSCAPS2_CUBEMAP;
        if( 5 == wNum )
            ddsd.ddsCaps.dwCaps2 = DDSCAPS2_CUBEMAP_NEGATIVEZ|DDSCAPS2_CUBEMAP;

        HRESULT hr = pddsPrev->GetAttachedSurface( &ddsd.ddsCaps,
                                                   &pEnvMap->pddsSurface[wNum] );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RenderEnvMap()
// Desc: Renders to the device-dependent environment map
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RenderEnvMap( EnvMapContainer* pEnvMap )
{
    // Check params
    if( NULL==pEnvMap )
        return E_INVALIDARG;

    D3DVIEWPORT7 ViewDataSave;
    D3DMATRIX matWorldSave, matViewSave, matProjSave;
    DWORD dwRSAntiAlias;

    // Save render states of the device
    m_pd3dDevice->GetViewport( &ViewDataSave );
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorldSave );
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_VIEW,       &matViewSave );
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProjSave );
    m_pd3dDevice->GetRenderState( D3DRENDERSTATE_ANTIALIAS, &dwRSAntiAlias );

    // Set up a viewport for rendering into the envmap
    D3DVIEWPORT7 ViewData;
    m_pd3dDevice->GetViewport( &ViewData );
    ViewData.dwWidth  = pEnvMap->dwWidth;
    ViewData.dwHeight = pEnvMap->dwHeight;

    // Since the environment maps are small, anti-alias when we render to them
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ANTIALIAS, D3DANTIALIAS_SORTINDEPENDENT );

    // Render to the six faces of the cube map
    for( DWORD i=0; i<6; i++ )
    {
        ChangeRenderTarget( pEnvMap->pddsSurface[i] );
        m_pd3dDevice->SetViewport( &ViewData );

        // Standard view that will be overridden below
        D3DVECTOR vEnvEyePt = D3DVECTOR( 0.0f, 0.0f, 0.0f );
        D3DVECTOR vLookatPt, vUpVec;

        switch( i )
        {
            case 0: // pos X
                vLookatPt = D3DVECTOR( 1.0f, 0.0f, 0.0f );
                vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );
                break;
            case 1: // neg X
                vLookatPt = D3DVECTOR(-1.0f, 0.0f, 0.0f );
                vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );
                break;
            case 2: // pos Y
                vLookatPt = D3DVECTOR( 0.0f, 1.0f, 0.0f );
                vUpVec    = D3DVECTOR( 0.0f, 0.0f,-1.0f );
                break;
            case 3: // neg Y
                vLookatPt = D3DVECTOR( 0.0f,-1.0f, 0.0f );
                vUpVec    = D3DVECTOR( 0.0f, 0.0f, 1.0f );
                break;
            case 4: // pos Z
                vLookatPt = D3DVECTOR( 0.0f, 0.0f, 1.0f );
                vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );
                break;
            case 5: // neg Z
                vLookatPt = D3DVECTOR( 0.0f, 0.0f,-1.0f );
                vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );
                break;
        }

        D3DMATRIX matWorld, matView, matProj;
        D3DUtil_SetIdentityMatrix( matWorld );
        D3DUtil_SetViewMatrix( matView, vEnvEyePt, vLookatPt, vUpVec );
        D3DUtil_SetProjectionMatrix( matProj, g_PI/2, 1.0f, 0.5f, 1000.0f );

        // Set the transforms for this view
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

        // Clear the zbuffer
        m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_ZBUFFER, 0x000000ff, 1.0f, 0L );

        // Begin the scene
        if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
        {
            if( m_pFileObject1 )
                m_pFileObject1->Render( m_pd3dDevice );
            if( m_pFileObject2 )
                m_pFileObject2->Render( m_pd3dDevice );
            if( m_pEnvCubeObject )
                m_pEnvCubeObject->Render( m_pd3dDevice );

            // End the scene.
            m_pd3dDevice->EndScene();
        }
    }

    ChangeRenderTarget( m_pddsRenderTarget );
    m_pd3dDevice->SetViewport(&ViewDataSave);
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorldSave );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matViewSave );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProjSave );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ANTIALIAS, dwRSAntiAlias );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InvalidateEnvMap()
// Desc: Frees device dependant objects for the mipmap
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::InvalidateEnvMap( EnvMapContainer* pEnvMap )
{
    if( pEnvMap )
    {
        // Note: DDraw doesn't play nice with releasing chained surfaces, so
        // make sure you release surfaces in the opposite order that they were
        // obtained.
        for( int i = 5; i>=0; i-- )
            SAFE_RELEASE( pEnvMap->pddsSurface[i] );
    }
}




//-----------------------------------------------------------------------------
// Name: DeleteEnvMap()
// Desc: Frees device dependant objects for the mipmap
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::DeleteEnvMap( EnvMapContainer* pEnvMap )
{
    if( pEnvMap )
    {
        InvalidateEnvMap( pEnvMap );
        delete pEnvMap;
    }
}




//-----------------------------------------------------------------------------
// Name: ChangeRenderTarget()
// Desc: Wrapper for SetRenderTarget() function, which also handles the
//       trnasfer of the zbuffer to the new render target.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::ChangeRenderTarget(
                                    LPDIRECTDRAWSURFACE7 pddsNewRenderTarget )
{
    LPDIRECTDRAWSURFACE7 pddsOldRenderTarget = NULL;
    m_pd3dDevice->GetRenderTarget( &pddsOldRenderTarget );

    if( pddsOldRenderTarget )
    {
        LPDIRECTDRAWSURFACE7 pddsZBuffer = NULL;
        DDSCAPS2 ddscaps = { DDSCAPS_ZBUFFER, 0, 0, 0 };
        pddsOldRenderTarget->GetAttachedSurface( &ddscaps, &pddsZBuffer );

        if( pddsZBuffer )
        {
            pddsOldRenderTarget->DeleteAttachedSurface( 0, pddsZBuffer );
            pddsNewRenderTarget->AddAttachedSurface( pddsZBuffer );
            pddsZBuffer->Release();
        }
        pddsOldRenderTarget->Release();
    }

    m_pd3dDevice->SetRenderTarget( pddsNewRenderTarget, 0 );

    return S_OK;
}




