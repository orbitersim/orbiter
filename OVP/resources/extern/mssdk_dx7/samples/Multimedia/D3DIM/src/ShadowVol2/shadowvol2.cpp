//-----------------------------------------------------------------------------
// File: Shadowvol2.cpp
//
// Desc: Example code showing how to use stencil buffers to implement shadows
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include "shadow.h"
#include "D3DApp.h"




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define BAR_TEXTURENAME      TEXT("banana.bmp")
#define SPHERE_TEXTURENAME   TEXT("banana.bmp")
#define OBJ_TEXTURENAME      TEXT("stripe2.bmp")
#define RECEIVER_TEXTURENAME TEXT("stripe.bmp")

// If g_dwMaxStencilValue < NUM_SHADOWS, default to 1-bit stencil buffer mode, which
// only allows non-overlapping shadows and must be sorted front-to-back
#define NUM_SHADOWS 11

D3DVERTEX    g_pSquareVertices[4];  // Vertices of square
SHADOW       g_Shad[NUM_SHADOWS];
SHADOWCASTER g_Caster[NUM_SHADOWS];

DWORD g_dwMaxVerticesCount = 0L;
DWORD g_dwNumCasters; // number of active shadow caster objects
DWORD g_dwNumObjects;    // number of casters+receivers

// "tmp" vertex buffers used to compute shadow volumes and sort shadows in Z
LPDIRECT3DVERTEXBUFFER7 g_pVB_xformed         = NULL;
LPDIRECT3DVERTEXBUFFER7 g_pVB_castertestverts = NULL;

// Defines several user-selectable levels of tesselation
WORD  g_TessLevs[NUMTESSLEVELS] = {4,8,17,28};
DWORD g_CurTessLevel = 0;

DWORD g_dwMaxStencilValue;   // maximum value the stencil buffer will hold

BOOL g_bUseOneBitStencil       = FALSE; // Use one-bit stencil buffer algorithm
BOOL g_bCanOnlyDoOneBitStencil = FALSE; // Stencil buffer only holds 1 bit
BOOL g_bDrawShadowVolumes      = FALSE; // Instead of shadows, draw transparent shadow volumes
BOOL g_bDrawShadowVolCaps      = TRUE;  // Draw tops of shadowvols (needed for correct rendering if top would be visible)
BOOL g_bSortZInOneBitStencil   = TRUE;  // Render shadvols in sorted z order, front-to-back (needed for correct rendering in 1-bit mode)
BOOL g_bDoShadows              = TRUE;  // Draw shadows/shadow volumes

BOOL g_bSwitchBitModes         = FALSE; // Changed to/from 1-bit algorithm this frame
BOOL g_bReInitObjs             = FALSE; // Need to reinit geometry

D3DSTENCILOP g_StencDecOp, g_StencIncOp;




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    LPDIRECTDRAWSURFACE7 m_pddsDepthBuffer;

    HRESULT CreateStencilBuffer();
    VOID    SetMenuStates();
    VOID    DeleteShadowObjects();
    HRESULT Init3DGeometry();
    HRESULT MakeShadowVolume( SHADOW* pShadow, D3DVERTEX* pInVertices,
                              DWORD dwNumVertices, D3DVECTOR& vLight );

    HRESULT RenderShadow( SHADOW* pShadow, LPDIRECT3DVERTEXBUFFER7 pVB );
    HRESULT DrawShadow();
    static HRESULT ConfirmDevice( DDCAPS* pddDriverCaps,
                                  D3DDEVICEDESC7* pd3dDeviceDesc );
protected:
    HRESULT OneTimeSceneInit();
    HRESULT InitDeviceObjects();
    HRESULT DeleteDeviceObjects();
    HRESULT RestoreSurfaces();
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
    m_strWindowTitle  = TEXT( "ShadowVol2: Stencil Shadows Sample" );
    m_bAppUseZBuffer  = FALSE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = ConfirmDevice;

    m_pddsDepthBuffer = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Create some textures
    D3DTextr_CreateTextureFromFile( BAR_TEXTURENAME );
    D3DTextr_CreateTextureFromFile( OBJ_TEXTURENAME );
    D3DTextr_CreateTextureFromFile( SPHERE_TEXTURENAME );
    D3DTextr_CreateTextureFromFile( RECEIVER_TEXTURENAME );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    HRESULT hr;

    FLOAT fRotateViewAngle  = fTimeKey*0.2f;
    FLOAT fRotateLightAngle = fTimeKey*1.0f;
    FLOAT fRotateAngleX     = fTimeKey*0.4f;
    FLOAT fRotateAngleY     = fTimeKey*4.0f;

    D3DVECTOR vLightPos;
    vLightPos.x =  0.6f * cosf( fRotateLightAngle );
    vLightPos.y = -1.0f;
    vLightPos.z =  0.6f * sinf( fRotateLightAngle );

    if( g_bSwitchBitModes )
    {
        g_bSwitchBitModes   = FALSE;
        g_bUseOneBitStencil = !g_bUseOneBitStencil;
        g_bReInitObjs       = TRUE;
    }

    if( g_bReInitObjs )
    {
MessageBeep( MB_OK );
        // Free old geom, make new stuff
        g_bReInitObjs = FALSE;

        DeleteShadowObjects();

        if( FAILED( hr = Init3DGeometry() ) )
            return hr;
    }

    // Setup the world spin matrix
    D3DMATRIX matWorldSpin;
    D3DUtil_SetRotateYMatrix( matWorldSpin, fRotateViewAngle );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorldSpin );

    D3DVERTEX* pTempVerts = new D3DVERTEX[g_dwMaxVerticesCount];

    for( DWORD i=0; i < g_dwNumObjects; i++ )
    {
        if( i >= g_dwNumCasters )
        {
            // Dont move receivers
            memcpy( g_Caster[i].pRVertices, g_Caster[i].pVertices,
                    g_Caster[i].dwNumVertices*sizeof(D3DVERTEX) );
            continue;
        }
        if( g_bUseOneBitStencil )
        {
            if( g_Caster[i].vCenter.z != 0.0f )
                TransRotateVertexInX(-g_Caster[i].vCenter, -fRotateAngleY,
                                      g_Caster[i].dwNumVertices, g_Caster[i].pVertices,
                                      g_Caster[i].pRVertices );
            else
                TransRotateVertexInZ(-g_Caster[i].vCenter, fRotateAngleY,
                                      g_Caster[i].dwNumVertices, g_Caster[i].pVertices,
                                      g_Caster[i].pRVertices );

            RotateVertexInY( fRotateAngleY, g_Caster[i].dwNumVertices,
                             g_Caster[i].pRVertices, pTempVerts );
            RotateVertexInX( fRotateAngleX, g_Caster[i].dwNumVertices,
                             pTempVerts, g_Caster[i].pRVertices );
        }
        else
        {
            if( i==g_dwNumCasters-1 )
            {
                TransRotateVertexInY(-g_Caster[i].vCenter, fRotateAngleY,
                                      g_Caster[i].dwNumVertices, g_Caster[i].pVertices,
                                      pTempVerts );
                TransRotateVertexInZ(-g_Caster[i].vCenter, fRotateAngleX*2.0f,
                                      g_Caster[i].dwNumVertices, pTempVerts,
                                      g_Caster[i].pRVertices );
            }
            else
            {
                RotateVertexInY( fRotateAngleY, g_Caster[i].dwNumVertices,
                                 g_Caster[i].pVertices, pTempVerts );
                RotateVertexInX( fRotateAngleX, g_Caster[i].dwNumVertices,
                                 pTempVerts, g_Caster[i].pRVertices );
            }
        }
    }

    delete pTempVerts;

    if( g_bDoShadows )
    {
        // Make new shadow volumes from xformed objects
        for( DWORD i=0; i<g_dwNumCasters; i++ )
        {
            if( FAILED( hr = MakeShadowVolume( &g_Shad[i], g_Caster[i].pRVertices,
                                               g_Caster[i].dwNumVertices, vLightPos ) ) )
               return hr;
        }
    }

    D3DLIGHT7 light;
    D3DUtil_InitLight( light, D3DLIGHT_DIRECTIONAL,
                       vLightPos.x, vLightPos.y, vLightPos.z );
    m_pd3dDevice->SetLight( 0, &light );

    return D3D_OK;
}




//-----------------------------------------------------------------------------
// Name: RenderShadow()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RenderShadow( SHADOW* pShadow,
                                         LPDIRECT3DVERTEXBUFFER7 pVB )
{
    // Turn depth buffer off, and stencil buffer on
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZWRITEENABLE,  FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILENABLE, TRUE );

    // Dont bother with interpolating color
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SHADEMODE,     D3DSHADE_FLAT );

    // Set up stencil compare fuction, reference value, and masks
    // Stencil test passes if ((ref & mask) cmpfn (stencil & mask)) is true
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFUNC,     D3DCMP_ALWAYS );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILZFAIL, D3DSTENCILOP_KEEP );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFAIL,  D3DSTENCILOP_KEEP );

    if( g_bUseOneBitStencil )
    {
        // If ztest passes, write !(g_bInvertStencilBufferSense) into stencil buffer
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILREF,      0x1 );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILMASK,     0x1 );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILWRITEMASK,0x1 );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILPASS,  D3DSTENCILOP_REPLACE );
    }
    else
    {
        // If ztest passes, inc/decrement stencil buffer value
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILREF,       0x1 );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILMASK,      0xffffffff );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILWRITEMASK, 0xffffffff );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILPASS,      g_StencIncOp );
    }

    // Since destcolor=SRCBLEND * SRC_COLOR + DESTBLEND * DEST_COLOR,
    // this should result in the tri color being completely dropped
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE );

    // draw front-side of shadow volume in stencil/z only

    m_pd3dDevice->DrawIndexedPrimitiveVB( D3DPT_TRIANGLESTRIP, /*pShadow->VB*/pVB,
                                        0, pShadow->dwNumVertices,
                                        pShadow->pwShadVolSideIndices,
                                        pShadow->dwNumSideIndices, 0x0);
    if(g_bDrawShadowVolCaps)
    {
        m_pd3dDevice->DrawIndexedPrimitiveVB( D3DPT_TRIANGLEFAN,/*pShadow->VB*/pVB,
                                            0, pShadow->dwNumVertices,
                                            pShadow->pwShadVolCapIndices,
                                            pShadow->dwNumCapIndices, 0x0);
    }

    // Now reverse cull order so back sides of shadow volume are written.

    if(g_bUseOneBitStencil)
    {
        // write 0's/1's into stencil buffer to erase pixels beyond back of shadow
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILREF, 0x0 );
    }
    else
    {
        // increment stencil buffer value
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILPASS, g_StencDecOp );
    }

    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_CULLMODE,   D3DCULL_CW );

    // Draw back-side of shadow volume in stencil/z only
    m_pd3dDevice->DrawIndexedPrimitiveVB( D3DPT_TRIANGLESTRIP, pVB,
                                            0, pShadow->dwNumVertices,
                                            pShadow->pwShadVolSideIndices,
                                            pShadow->dwNumSideIndices, 0x0);

    if(g_bDrawShadowVolCaps)
    {
        m_pd3dDevice->DrawIndexedPrimitiveVB( D3DPT_TRIANGLEFAN, pVB,
                                            0, pShadow->dwNumVertices,
                                            pShadow->pwShadVolCapIndices,
                                            pShadow->dwNumCapIndices, 0x0);
    }

    // Restore render states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_CULLMODE, D3DCULL_CCW );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZWRITEENABLE,     TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILENABLE,    FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SHADEMODE, D3DSHADE_GOURAUD );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DrawShadow()
// Desc: Draws a big grey polygon over scene, and blend it with pixels with
//       stencil 1, which are in shadow.  Could optimize this by keeping track
//       of rendered 2D extent rect of all shadow vols.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DrawShadow()
{
    // Use alpha blending to draw the transparent shadow
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA );

    // Set stencil states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,       FALSE );

    // Only write where stencil val >= 1 (count indicates # of shadows that
    // overlap that pixel)
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILREF,  0x1 );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFUNC, D3DCMP_LESSEQUAL );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILPASS, D3DSTENCILOP_KEEP );

    // Set the world matrix to identity to draw the big grey square
    D3DMATRIX matWorld, matIdentity;
    D3DUtil_SetIdentityMatrix( matIdentity );
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matIdentity );

    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport( &vp );
    COLORVERTEX sqverts[4];
    FLOAT w = (FLOAT)vp.dwWidth;
    FLOAT h = (FLOAT)vp.dwHeight;
    sqverts[0].p = D3DVECTOR(-w/2,-h/2, 0.0f );
    sqverts[1].p = D3DVECTOR( w/2, h/2, 0.0f );
    sqverts[2].p = D3DVECTOR( w/2,-h/2, 0.0f );
    sqverts[3].p = D3DVECTOR(-w/2, h/2, 0.0f );
    sqverts[0].c = sqverts[1].c = sqverts[2].c = sqverts[3].c = 0x7f000000;

    // Draw the big gray shadow polygon
    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_XYZ|D3DFVF_DIFFUSE,
                               sqverts, 4, NULL );

    // Restore render states
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,          TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILENABLE,    FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );

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
    DWORD i;

    // Clear the viewport, zbuffer, and stencil buffer
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL,
                         0x000000ff, 1.0f, 0x0 );

    // Begin the scene
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        // Draw ground square
        m_pd3dDevice->SetTexture( 0, NULL);

        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                     g_pSquareVertices, 4, NULL );

        if(g_bUseOneBitStencil)
        {
            m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface(SPHERE_TEXTURENAME) );

            for(i=0;i<g_dwNumObjects;i++)
            {
                if(i==g_dwNumCasters)
                    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface(RECEIVER_TEXTURENAME) );

                m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                                                  g_Caster[i].pRVertices, g_Caster[i].dwNumVertices,
                                                  g_Caster[i].pIndices, g_Caster[i].dwNumIndices, NULL );
            }
        }
        else
        {
            m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface(BAR_TEXTURENAME) );

            for(i=0;i<g_dwNumObjects;i++)
            {
                if(i==2)
                    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface(SPHERE_TEXTURENAME) );
                else if(i==g_dwNumCasters)
                    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface(RECEIVER_TEXTURENAME) );
                else if(i==(g_dwNumCasters-1))
                    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface(OBJ_TEXTURENAME) );

                m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                                                  g_Caster[i].pRVertices, g_Caster[i].dwNumVertices,
                                                  g_Caster[i].pIndices, g_Caster[i].dwNumIndices, NULL );
            }
        }

        m_pd3dDevice->SetTexture( 0, NULL );

        if( g_bDoShadows )
        {
            SHADOW *shadorder[NUM_SHADOWS];
            DWORD shadnum;

            for(shadnum=0;shadnum<g_dwNumCasters;shadnum++)
            {
                shadorder[shadnum]=&g_Shad[shadnum];
            }

            if( g_bUseOneBitStencil && g_bSortZInOneBitStencil )
            {
                // need to render in front-to-back order so that backside of front shadow vols
                // do not erase stencil pixels set by background shadow vols.
                // shadowvols are restricted to be non-overlapping in Z, might be able to allow
                // overlapping shadows as long as I could guarantee I could render the shadvols sorted
                // in order of their backfaces, so that the frontmost remaining
                // backface always is rendered first (and thus no already-rendered shadow pixels behind it
                // would be incorrectly turned off)

                COLORVERTEX *pColorVertices;
                FLOAT Z_array[NUM_SHADOWS];

                if( SUCCEEDED( g_pVB_castertestverts->Lock( DDLOCK_NOSYSLOCK | DDLOCK_WRITEONLY | DDLOCK_WAIT | DDLOCK_SURFACEMEMORYPTR,
                                                            (VOID**)&pColorVertices, NULL ) ) )
                {
                    for( shadnum=0;shadnum<g_dwNumCasters;shadnum++,pColorVertices++ )
                    {
                        // taking first vtx as representative point
                        memcpy( &pColorVertices->p, (D3DVECTOR*)(&g_Caster[shadnum].pRVertices[0].x), sizeof(D3DVECTOR) );
                    }
                    g_pVB_castertestverts->Unlock();
                }

                g_pVB_xformed->ProcessVertices( D3DVOP_TRANSFORM, 0, g_dwNumCasters,
                                                g_pVB_castertestverts, 0, m_pd3dDevice, 0 );

                if( SUCCEEDED( g_pVB_xformed->Lock( DDLOCK_NOSYSLOCK | DDLOCK_READONLY | DDLOCK_WAIT | DDLOCK_SURFACEMEMORYPTR,
                                                    (VOID**)&pColorVertices, NULL ) ) )
                {
                    for( shadnum=0;shadnum<g_dwNumCasters;shadnum++ )
                    {
                        Z_array[shadnum]=pColorVertices->p.z;

                        // output VB is XYZ+RHW+COLOR
                        pColorVertices = (COLORVERTEX*)(((char*)pColorVertices)+(sizeof(D3DVECTOR)+sizeof(float)+sizeof(DWORD)));
                    }

                    g_pVB_xformed->Unlock();
                }

                // insertion sort Z_array and shadorder array to get front-to-back shadorder

                int i,j;
                SHADOW *tmp_sptr;
                float tmp_z;

                for(i=1;i<((int)g_dwNumCasters);i++)
                {
                    tmp_z=Z_array[i];
                    tmp_sptr=shadorder[i];

                    j=i;
                    while((--j >=0) && (tmp_z < Z_array[j]))
                    {
                        Z_array[j+1]=Z_array[j];
                        shadorder[j+1]=shadorder[j];
                    }

                    Z_array[j+1]=tmp_z;
                    shadorder[j+1]=tmp_sptr;
                }
            }

            if(g_bDrawShadowVolumes)
            {
                m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );

                // Since destcolor=SRCBLEND * SRC_COLOR + DESTBLEND * DEST_COLOR,
                // this results in destcolor= (AlphaSrc) * SRC_COLOR + (1-AlphaSrc)*DestColor
                m_pd3dDevice->SetRenderState(D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
                m_pd3dDevice->SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);

                m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SHADEMODE, D3DSHADE_GOURAUD );
            }

            for(shadnum=0;shadnum<g_dwNumCasters;shadnum++)
            {
                g_pVB_xformed->ProcessVertices( (D3DVOP_TRANSFORM | D3DVOP_CLIP),
                                                0, shadorder[shadnum]->dwNumVertices,
                                                shadorder[shadnum]->VB, 0, m_pd3dDevice, 0 );
                if(g_bDrawShadowVolumes)
                {
                    m_pd3dDevice->DrawIndexedPrimitiveVB( D3DPT_TRIANGLESTRIP,
                                                        g_pVB_xformed,
                                                        0, shadorder[shadnum]->dwNumVertices,
                                                        shadorder[shadnum]->pwShadVolSideIndices,
                                                        shadorder[shadnum]->dwNumSideIndices,
                                                        0x0);
                    if(g_bDrawShadowVolCaps)
                    {
                        m_pd3dDevice->DrawIndexedPrimitiveVB( D3DPT_TRIANGLEFAN,
                                                g_pVB_xformed,
                                                0, shadorder[shadnum]->dwNumVertices,
                                                shadorder[shadnum]->pwShadVolCapIndices,
                                                shadorder[shadnum]->dwNumCapIndices, 0x0);
                    }
                }
                else
                {
                    // Render the shadow volume into the stencil buffer
                    RenderShadow( shadorder[shadnum],g_pVB_xformed );
                }
            }

            if(g_bDrawShadowVolumes)
            {
                m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );
            }
            else
            {
                DrawShadow();
            }
        }

        // End the scene.
        m_pd3dDevice->EndScene();
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: EnumZBufferFormatsCallback()
// Desc: Enumeration function to report valid pixel formats for z-buffers.
//-----------------------------------------------------------------------------
static HRESULT WINAPI EnumZBufferFormatsCallback( DDPIXELFORMAT* pddpf,
                                                  VOID* pddpfDesired )
{
    if( NULL==pddpf || NULL==pddpfDesired )
        return D3DENUMRET_CANCEL;

    // If the current pixel format's match the desired ones (DDPF_ZBUFFER and
    // possibly DDPF_STENCILBUFFER), lets copy it and return. This function is
    // not choosy...it accepts the first valid format that comes along.
    if( pddpf->dwFlags == ((DDPIXELFORMAT*)pddpfDesired)->dwFlags )
    {
        memcpy( pddpfDesired, pddpf, sizeof(DDPIXELFORMAT) );
        return D3DENUMRET_CANCEL;
    }

    return D3DENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateStencilBuffer()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::CreateStencilBuffer()
{
    g_bCanOnlyDoOneBitStencil=FALSE;

    DWORD dwStencilCaps = m_pDeviceInfo->ddDeviceDesc.dwStencilCaps;

    if( (!(dwStencilCaps & D3DSTENCILCAPS_INCR) && !(dwStencilCaps & D3DSTENCILCAPS_INCRSAT))
       ||(!(dwStencilCaps & D3DSTENCILCAPS_DECR) && !(dwStencilCaps & D3DSTENCILCAPS_DECRSAT)))
    {
        // Must do 1-bit stencil buffer
        g_bCanOnlyDoOneBitStencil=TRUE;
    }
    else
    {
        // Prefer sat ops that cap at 0/max, but can use other ones as long as enough stencil bits
        g_StencIncOp=(dwStencilCaps & D3DSTENCILCAPS_INCRSAT)? D3DSTENCILOP_INCRSAT:D3DSTENCILOP_INCR;
        g_StencDecOp=(dwStencilCaps & D3DSTENCILCAPS_DECRSAT)? D3DSTENCILOP_DECRSAT:D3DSTENCILOP_DECR;
    }

    m_pddsRenderTarget->DeleteAttachedSurface( 0,NULL );

    // Get z-buffer dimensions from the render target
    // Setup the surface desc for the z-buffer.
    DDSURFACEDESC2 ddsd;
    D3DUtil_InitSurfaceDesc( ddsd );
    m_pddsRenderTarget->GetSurfaceDesc( &ddsd );
    ddsd.dwFlags         = DDSD_WIDTH | DDSD_HEIGHT | DDSD_CAPS | DDSD_PIXELFORMAT;
    ddsd.ddsCaps.dwCaps  = DDSCAPS_ZBUFFER;
    ddsd.ddsCaps.dwCaps2 = 0;
    ddsd.ddsCaps.dwCaps3 = 0;
    ddsd.ddsCaps.dwCaps4 = 0;
    ddsd.ddpfPixelFormat.dwFlags = DDPF_ZBUFFER | DDPF_STENCILBUFFER;

    if( m_pDeviceInfo->bHardware )
        ddsd.ddsCaps.dwCaps  |= DDSCAPS_VIDEOMEMORY;
    else
        ddsd.ddsCaps.dwCaps  |= DDSCAPS_SYSTEMMEMORY;

    // Get an appropiate pixel format from enumeration of the formats.
    m_pD3D->EnumZBufferFormats( (*m_pDeviceInfo->pDeviceGUID),
                              EnumZBufferFormatsCallback,
                              (VOID*)&ddsd.ddpfPixelFormat );

    assert(ddsd.ddpfPixelFormat.dwStencilBitDepth!=0);

    g_bCanOnlyDoOneBitStencil=g_bCanOnlyDoOneBitStencil || ((1<<ddsd.ddpfPixelFormat.dwStencilBitDepth)<NUM_SHADOWS);

    g_dwMaxStencilValue=(1<<ddsd.ddpfPixelFormat.dwStencilBitDepth)-1;

    // Leave g_bUseOneBitStencil set for window-resize case
    if( !g_bUseOneBitStencil )
        g_bUseOneBitStencil=g_bCanOnlyDoOneBitStencil;

    SetMenuStates();

    // Create and attach a z-buffer
    if( FAILED( m_pDD->CreateSurface( &ddsd, &m_pddsDepthBuffer, NULL ) ) )
        return E_FAIL;

    if( FAILED( m_pddsRenderTarget->AddAttachedSurface( m_pddsDepthBuffer ) ) )
        return E_FAIL;

    // The SetRenderTarget() call is needed to rebuild internal structures for
    // the newly attached zbuffer.
    return m_pd3dDevice->SetRenderTarget( m_pddsRenderTarget, 0L );
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    // Create the stencil buffer
    if( FAILED( CreateStencilBuffer() ) )
        return E_FAIL;

    // Create and set up the shine materials w/ textures
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    mtrl.power = 40.0f;
    m_pd3dDevice->SetMaterial( &mtrl );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE, 1 );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT, 0x40404040 );

    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,  D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );

    // Set the transform matrices
    D3DMATRIX matWorld, matView, matProj;
    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f,  5.0f, -15.0f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, -1.0f,  0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f,  1.0f,  0.0f );

    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    D3DUtil_SetProjectionMatrix( matProj, g_PI/4, 1.0f, 0.2f, 50.0f );

    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set up the light
    D3DLIGHT7 light;
    D3DUtil_InitLight( light, D3DLIGHT_POINT, 0.0f, 0.0f, -12.0f );
    light.dvAttenuation0 = 1.0f;
    m_pd3dDevice->SetLight( 0, &light );
    m_pd3dDevice->LightEnable( 0, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING,    TRUE );

    return Init3DGeometry();
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
// Name: DeleteShadowObjects()
// Desc:
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::DeleteShadowObjects()
{
    for( DWORD shadnum=0; shadnum<NUM_SHADOWS; shadnum++ )
    {
        SAFE_RELEASE( g_Shad[shadnum].VB );
        SAFE_DELETE( g_Shad[shadnum].pwShadVolIndices );
    }

    for( DWORD i=0; i<NUM_SHADOWS; i++ )
    {
        SAFE_DELETE( g_Caster[i].pVertices );
        SAFE_DELETE( g_Caster[i].pRVertices );
        SAFE_DELETE( g_Caster[i].pIndices );
    }

    ZeroMemory( g_Caster, NUM_SHADOWS*sizeof(SHADOWCASTER) );
    ZeroMemory( g_Shad,   NUM_SHADOWS*sizeof(SHADOW) );

    // Must release all VertexBufs because they are associated with D3D
    // which is being released
    SAFE_RELEASE( g_pVB_xformed );
    SAFE_RELEASE( g_pVB_castertestverts );
}




//-----------------------------------------------------------------------------
// Name: DeleteDeviceObjects()
// Desc: Called when the app is exitting, or the device is being changed,
//       this function deletes any device dependant objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DeleteDeviceObjects()
{
    D3DTextr_InvalidateAllTextures();

    DeleteShadowObjects();

    SAFE_RELEASE( m_pddsDepthBuffer );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RestoreSurfaces()
// Desc: Surfaces (defined as any video-memory dependant objects, including
//       vertex buffers) are usually lost after a mode switch from/to
//       fullscreen display modes. This function is called to restore any
//       video-memory dependant objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RestoreSurfaces()
{
    // Instruct the app to recreate the vertex buffer objects before rendering
    g_bReInitObjs = TRUE;

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
    DWORD dwStencilCaps = pd3dDeviceDesc->dwStencilCaps;

    if( !(dwStencilCaps & D3DSTENCILCAPS_KEEP) )
        return E_FAIL;
    if( !(dwStencilCaps & D3DSTENCILCAPS_REPLACE) )
        return E_FAIL;

    // Make sure device supports point lights
    if( 0 == ( pd3dDeviceDesc->dwVertexProcessingCaps &
                                            D3DVTXPCAPS_POSITIONALLIGHTS ) )
        return E_FAIL;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: MakeShadowVolume()
// Desc: Build the polygons of the shadow volume for a directional light
//       pInVertices are copied into a vertex buffer created within *pShadow
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::MakeShadowVolume( SHADOW* pShadow,
                                             D3DVERTEX* pInVertices,
                                             DWORD dwNumVertices,
                                             D3DVECTOR& vLight )
{
    D3DMATRIX matWorld, matView, matProj;
    D3DMATRIX matNewView, matIdentity;
    DWORD i;
    HRESULT hr;

    LPDIRECT3DVERTEXBUFFER7 VB_Proj;
    D3DVERTEXBUFFERDESC     vbDesc;
    vbDesc.dwSize = sizeof(D3DVERTEXBUFFERDESC);
    // All buffers created with this desc are used with ProcessVertices, which always
    // requires system memory
    vbDesc.dwCaps = D3DVBCAPS_SYSTEMMEMORY;
    vbDesc.dwFVF  = D3DFVF_XYZ | D3DFVF_DIFFUSE;

    // Create vertex buffer to hold shadow volumes verts
    if( NULL == pShadow->VB  )
    {
        // Else re-use old VB
        assert( NULL == pShadow->pwShadVolIndices );

        // Now form array of indices that will make the triangles
        ZeroMemory( pShadow, sizeof(SHADOW) );
        vbDesc.dwNumVertices  = dwNumVertices*2;  // Times 2 to hold top of shadowvolume for infinite light source

        if( FAILED( hr = m_pD3D->CreateVertexBuffer( &vbDesc, &pShadow->VB, 0 ) ) )
            return hr;

        // Allocate enough indices to hold the largest-case shadowvolume (max #
        // of vertices in c-hull is dwNumVertices).
        // (dwNumVertices+1)*2 for triangle mesh to hold shadowvolume sides +
        // dwNumVertices to hold triangle-fan
        pShadow->pwShadVolIndices = new WORD[(dwNumVertices+1)*3];
    }

    // Create VB_Proj vertex buffer as a target for the vertex-projection
    // operation used to compute the silhouette.
    vbDesc.dwNumVertices = dwNumVertices;
    vbDesc.dwFVF         = D3DFVF_XYZRHW;
    // Note: even though RHW not used, we must specify it or ProcessVerticess()
    // will not consider this as a valid target to xform verts into

    if( FAILED( hr = m_pD3D->CreateVertexBuffer( &vbDesc, &VB_Proj, 0 ) ) )
        return hr;

    // Must lock VB, then copy vertices into its space.
    COLORVERTEX* pVBVertices;
    if( SUCCEEDED( pShadow->VB->Lock( DDLOCK_NOSYSLOCK | DDLOCK_WRITEONLY | DDLOCK_WAIT | DDLOCK_SURFACEMEMORYPTR,
                                      (VOID**)&pVBVertices, NULL ) ) )
    {
        // Have to copy verts into VB memory. We reformat into COLORVERTEX to do
        // this. We could prevent reformat and do a straight memcpy if
        // Find2DConvexHull() used D3DVERTEX, though.
        for( i=0; i<dwNumVertices; i++ )
        {
            pVBVertices[i].p.x = pInVertices[i].x;
            pVBVertices[i].p.y = pInVertices[i].y;
            pVBVertices[i].p.z = pInVertices[i].z;
            pVBVertices[i].c   = 0x5f000000; // Vertex is semi-transparent black
        }

        pShadow->VB->Unlock();
    }

    // Save our matrices so we can use xform pipeline to project vertices
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // For view matrix, all we want is anything that projects the verts onto a
    // plane perpendicular to light direction. So any eyepoint is OK (try to
    // make object near origin though, so look at one of the verticess). Don't
    // care what direction is view up vector (y).
    D3DVECTOR at   = pVBVertices[0].p;
    D3DVECTOR from = at - 7.0f*vLight;  // Make sure eye is far enough away
    D3DVECTOR up;

    // anything perp to light vector is OK
    if( (vLight.y==0.0f) && (vLight.x==0.0f) )
        up = D3DVECTOR( 0.0f, 1.0f, 0.0f );
    else
        up = D3DVECTOR( vLight.y, -vLight.x, 0.0f );

    D3DUtil_SetViewMatrix( matNewView, from, at, up);
    D3DUtil_SetIdentityMatrix( matIdentity );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matIdentity );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matNewView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matIdentity );

    // Do the planar projection
    VB_Proj->ProcessVertices( D3DVOP_TRANSFORM, 0, dwNumVertices, pShadow->VB,
                              0, m_pd3dDevice, 0 );

    // Restore matrices
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    COLORVERTEX* pProjVertices;
    WORD*        pHullIndices;
    DWORD        dwNumHullIndices;

    if( SUCCEEDED( VB_Proj->Lock( DDLOCK_NOSYSLOCK | DDLOCK_WAIT | DDLOCK_SURFACEMEMORYPTR,
                                  (VOID**)&pProjVertices, NULL ) ) )
    {
        Find2DConvexHull( dwNumVertices, pProjVertices, &dwNumHullIndices, &pHullIndices );

        VB_Proj->Unlock();
    }
    VB_Proj->Release();   // Just needed the indices of hull

    if( SUCCEEDED( pShadow->VB->Lock( DDLOCK_NOSYSLOCK | DDLOCK_WRITEONLY | DDLOCK_WAIT | DDLOCK_SURFACEMEMORYPTR,
                                      (VOID**)&pVBVertices, NULL ) ) )
    {
        // Make shadow volume by taking hull verts and project them along the light
        // direction far enough to be offscreen

        // Add verts to end of VB
        for( i=0; i<dwNumHullIndices; i++ )
        {
            pVBVertices[dwNumVertices+i].p = pVBVertices[pHullIndices[i]].p + 20.0f*vLight;
            pVBVertices[dwNumVertices+i].c = 0x7f000000;
        }

        pShadow->dwNumVertices=dwNumVertices+dwNumHullIndices;

        // Now form array of indices that will make the triangles. The ShadowVolume
        // will have dwNumHullIndices square sides.

        pShadow->dwNumSideIndices = (dwNumHullIndices+1)*2;

        // If shadvol is not capped, shadow may be drawn in place where a
        // backfacing cap is missing even though no geometry is there
        if( g_bDrawShadowVolCaps )
           pShadow->dwNumCapIndices = dwNumHullIndices;
        else
            pShadow->dwNumCapIndices = 0;

        WORD* pIndexPtr = pShadow->pwShadVolSideIndices=pShadow->pwShadVolIndices;

        // Triangles for all facets but final one
        for( i=0; i<dwNumHullIndices; i++ )
        {
            // pHullIndices[i] is the index of the ith vertex of the
            // dwNumHullIndices'th convex hull vertices. dwNumVertices+i is the
            // index of the projected vertex corresponding to the pHullIndices[i]
            // vertex.
            *pIndexPtr++ = pHullIndices[i];
            *pIndexPtr++ = (WORD)( dwNumVertices+i );
        }

        // Add tris for final facet (i==dwNumHullIndices)
        *pIndexPtr++ = pHullIndices[0];
        *pIndexPtr++ = (WORD)( dwNumVertices+0 );

        pShadow->pwShadVolCapIndices = pIndexPtr;

        if( g_bDrawShadowVolCaps )
        {
            for( i=(dwNumVertices+dwNumHullIndices-1); i>=dwNumVertices; i-- )
            {
                // Draw a fan over the shadvolume cap. Note: only the back-facing
                // cap is done here (which is why we're count backwards).
                *pIndexPtr++ = (WORD)i;
            }
        }

        delete pHullIndices;   // Free memory allocated by Find2DConvexHull
        pShadow->VB->Unlock();
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetMenuStates()
// Desc: init menu state based on app globals
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::SetMenuStates()
{
    HMENU hMenu = GetMenu( m_hWnd );

    // Set the state of all the menus
    CheckMenuItem(  hMenu, IDM_DO_SHADOWS, (g_bDoShadows)?MF_CHECKED:MF_UNCHECKED );

    BOOL bIn1BitMode;

    if( g_bSwitchBitModes )
        bIn1BitMode =! g_bUseOneBitStencil;
    else
        bIn1BitMode = g_bUseOneBitStencil;

    if( !g_bDoShadows )
    {
        EnableMenuItem( hMenu, IDM_SHOWSHADVOL, MF_GRAYED );
        EnableMenuItem( hMenu, IDM_1BITMODE, MF_GRAYED );
        EnableMenuItem( hMenu, IDM_SORTEDZ, MF_GRAYED );
        EnableMenuItem( hMenu, IDM_DRAWSHADOWVOLUMECAPS, MF_GRAYED );
    }
    else
    {
        EnableMenuItem( hMenu, IDM_SHOWSHADVOL, MF_ENABLED);
        EnableMenuItem( hMenu, IDM_1BITMODE, MF_ENABLED);
        EnableMenuItem( hMenu, IDM_DRAWSHADOWVOLUMECAPS, MF_ENABLED);
        if( bIn1BitMode )
            EnableMenuItem( hMenu, IDM_SORTEDZ, MF_ENABLED );
        else
            EnableMenuItem( hMenu, IDM_SORTEDZ, MF_GRAYED);
    }

    CheckMenuItem(  hMenu, IDM_SHOWSHADVOL, (g_bDrawShadowVolumes)?MF_CHECKED:MF_UNCHECKED );
    CheckMenuItem(  hMenu, IDM_1BITMODE,   (bIn1BitMode)?MF_CHECKED:MF_UNCHECKED );
    CheckMenuItem(  hMenu, IDM_SORTEDZ,    (g_bSortZInOneBitStencil)?MF_CHECKED:MF_UNCHECKED );
    CheckMenuItem(  hMenu, IDM_DRAWSHADOWVOLUMECAPS,  (g_bDrawShadowVolCaps)?MF_CHECKED:MF_UNCHECKED );

    for( DWORD i=0; i<NUMTESSLEVELS; i++ )
    {
        CheckMenuItem(  hMenu, IDM_TESS1+i, (i==g_CurTessLevel)?MF_CHECKED:MF_UNCHECKED );
    }
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
        if( LOWORD(wParam)>=IDM_TESS1 && LOWORD(wParam)<=IDM_TESS4 )
        {
            g_CurTessLevel = LOWORD(wParam) - IDM_TESS1;
            g_bReInitObjs  = TRUE;
        }
        else switch( LOWORD(wParam) )
        {
            case IDM_DO_SHADOWS:
                g_bDoShadows=!g_bDoShadows;
                break;

            case IDM_SHOWSHADVOL:
                g_bDrawShadowVolumes=!g_bDrawShadowVolumes;
                break;

            case IDM_DRAWSHADOWVOLUMECAPS:
                g_bDrawShadowVolCaps=!g_bDrawShadowVolCaps;
                break;

            case IDM_1BITMODE:
                if(g_bUseOneBitStencil && g_bCanOnlyDoOneBitStencil)
                    break;
                g_bSwitchBitModes=TRUE;
                break;

            case IDM_SORTEDZ:
                g_bSortZInOneBitStencil=!g_bSortZInOneBitStencil;
                break;
        }

        // Update the menu to reflect any new changes
        SetMenuStates();
    }
    return CD3DApplication::MsgProc( hWnd, uMsg, wParam, lParam );
}




HRESULT CMyD3DApplication::Init3DGeometry()
{
#define PWIDTH 5.5f
#define DEPTHBELOW -5.5f
#define BARLENGTH 3.0f

    HRESULT hr;

    // Vertices for the lowermost square
    g_pSquareVertices[0] = D3DVERTEX( D3DVECTOR(-PWIDTH, DEPTHBELOW,-PWIDTH),
                                      D3DVECTOR(0,1,0), 0, 1 );
    g_pSquareVertices[1] = D3DVERTEX( D3DVECTOR(-PWIDTH, DEPTHBELOW, PWIDTH),
                                      D3DVECTOR(0,1,0), 0, 0 );
    g_pSquareVertices[2] = D3DVERTEX( D3DVECTOR( PWIDTH, DEPTHBELOW,-PWIDTH),
                                      D3DVECTOR(0,1,0), 1, 1 );
    g_pSquareVertices[3] = D3DVERTEX( D3DVECTOR( PWIDTH, DEPTHBELOW, PWIDTH),
                                      D3DVECTOR(0,1,0), 1, 0 );

    ZeroMemory( g_Shad,   NUM_SHADOWS*sizeof(SHADOW) );
    ZeroMemory( g_Caster, NUM_SHADOWS*sizeof(SHADOWCASTER) );

    D3DVERTEXBUFFERDESC vbDesc;
    vbDesc.dwSize = sizeof(D3DVERTEXBUFFERDESC);
    // All buffers created with this desc are used with ProcessVertices, which always
    // requires system memory
    vbDesc.dwCaps = D3DVBCAPS_SYSTEMMEMORY;

    // Generate the object data
    WORD      SphNumRings    = g_TessLevs[g_CurTessLevel];
    WORD      SphNumSections = g_TessLevs[g_CurTessLevel];
    DWORD     casternum=0;

    // One-bit stencil buffer case will use simpler geometry
    if( g_bUseOneBitStencil )
    {
        vbDesc.dwNumVertices = NUM_SHADOWS;  // this VB is just for z-ordering the casters
        vbDesc.dwFVF         = D3DFVF_XYZ | D3DFVF_DIFFUSE;  // this is a src vbuf

        if( FAILED( hr = m_pD3D->CreateVertexBuffer( &vbDesc, &g_pVB_castertestverts, 0 ) ) )
            return hr;

        GenerateSphere( &g_Caster[casternum++], D3DVECTOR( BARLENGTH, 0.0f, 0.0f ),
                        1.0f, SphNumRings, SphNumSections, 1.0f, 0.4f, 1.0f );
        GenerateSphere( &g_Caster[casternum++], D3DVECTOR(-BARLENGTH, 0.0f, 0.0f ),
                        1.0f, SphNumRings, SphNumSections, 1.0f, 0.4f, 1.0f );
        GenerateSphere( &g_Caster[casternum++], D3DVECTOR( 0.0f, 0.0f, BARLENGTH ),
                        1.0f, SphNumRings, SphNumSections, 1.0f, 0.4f, 1.0f );
        GenerateSphere( &g_Caster[casternum++], D3DVECTOR( 0.0f, 0.0f,-BARLENGTH ),
                        1.0f, SphNumRings, SphNumSections, 1.0f, 0.4f, 1.0f );

        g_dwNumCasters = casternum;

        // Spheres on plane to receive shadows. These wont really cast shadows
        // since they would overlap and we've only got 1bit stenc

        GenerateSphere( &g_Caster[casternum++], D3DVECTOR(-BARLENGTH,DEPTHBELOW,0.0f),
                        2.0f, SphNumRings, SphNumSections, 1.0f, 0.4f, 1.0f );
        GenerateSphere( &g_Caster[casternum++], D3DVECTOR(BARLENGTH,DEPTHBELOW,0.0f),
                        2.0f, SphNumRings, SphNumSections, 1.0f, 0.4f, 1.0f );
        GenerateSphere( &g_Caster[casternum++], D3DVECTOR(0.0f,DEPTHBELOW,-BARLENGTH),
                        2.0f, SphNumRings, SphNumSections, 1.0f, 0.4f, 1.0f );
        GenerateSphere( &g_Caster[casternum++], D3DVECTOR(0.0f,DEPTHBELOW,BARLENGTH),
                        2.0f, SphNumRings, SphNumSections, 1.0f, 0.4f, 1.0f );

        g_dwNumObjects = casternum;
    }
    else
    {
         // Since numrings/numsections is same for all spheres, indices will be the same,
         // so we can use same counts and index array for all of them

         GenerateSphere( &g_Caster[casternum++], D3DVECTOR(0.0f,0.0f,0.0f),
                         1.0f, SphNumRings, SphNumSections, BARLENGTH, 0.5f, 0.5f);
         GenerateSphere( &g_Caster[casternum++], D3DVECTOR(0.0f,0.0f,0.0f),
                         1.0f, SphNumRings, SphNumSections, 0.5f, 0.5f, BARLENGTH );

         GenerateSphere( &g_Caster[casternum++], D3DVECTOR(BARLENGTH,0.0f,0.0f),
                         1.0f, SphNumRings, SphNumSections, 1.0f, 1.0f, 1.0f );
         GenerateSphere( &g_Caster[casternum++], D3DVECTOR(-BARLENGTH,0.0f,0.0f),
                         1.0f, SphNumRings, SphNumSections, 1.0f, 1.0f, 1.0f );
         GenerateSphere( &g_Caster[casternum++], D3DVECTOR(0.0f,0.0f,BARLENGTH),
                         1.0f, SphNumRings, SphNumSections, 1.0f, 1.0f, 1.0f );
         GenerateSphere( &g_Caster[casternum++], D3DVECTOR(0.0f,0.0f,-BARLENGTH),
                         1.0f, SphNumRings, SphNumSections, 1.0f, 1.0f, 1.0f );

         // Above plane sph to receive shadows
         GenerateSphere( &g_Caster[casternum++], D3DVECTOR(0.0f,DEPTHBELOW/1.3f,0.0f),
                         1.7f, SphNumRings, SphNumSections, 1.0f, 0.35f, 1.0f );

         g_dwNumCasters = casternum;

         // Spheres on plane -- to receive main shadows
         GenerateSphere( &g_Caster[casternum++],D3DVECTOR(-BARLENGTH,DEPTHBELOW,0.0f),
                         2.0f, SphNumRings, SphNumSections, 1.0f, 0.4f, 1.0f );
         GenerateSphere( &g_Caster[casternum++],D3DVECTOR(BARLENGTH,DEPTHBELOW,0.0f),
                         2.0f, SphNumRings, SphNumSections, 1.0f, 0.4f, 1.0f );
         GenerateSphere( &g_Caster[casternum++],D3DVECTOR(0.0f,DEPTHBELOW,-BARLENGTH),
                         2.0f, SphNumRings, SphNumSections, 1.0f, 0.4f, 1.0f );
         GenerateSphere( &g_Caster[casternum++],D3DVECTOR(0.0f,DEPTHBELOW,BARLENGTH),
                         2.0f, SphNumRings, SphNumSections, 1.0f, 0.4f, 1.0f );

         g_dwNumObjects = casternum;
    }

    // set up vertex buffer to use as target for z-sorting shadow volumes in 1bit stencil mode and for
    // rendering shadow volumes

    g_dwMaxVerticesCount=0;

    for( DWORD i=0; i<g_dwNumObjects; i++ )
    {
        g_Caster[i].pRVertices = new D3DVERTEX[g_Caster[i].dwNumVertices];
        if( g_dwMaxVerticesCount < g_Caster[i].dwNumVertices )
            g_dwMaxVerticesCount = g_Caster[i].dwNumVertices;
    }

    assert(g_dwMaxVerticesCount>1);

    // Set caps for the transformed vertex buffer.
    vbDesc.dwFVF         = D3DFVF_XYZRHW | D3DFVF_DIFFUSE;
    vbDesc.dwNumVertices = g_dwMaxVerticesCount*2;  // *2 to hold top of shadvol for infin light source
                                             // this is an big overestimate, the shadvol will not include all verts of obj (unless it's a simple polygon)
    hr = m_pD3D->CreateVertexBuffer(&vbDesc, &g_pVB_xformed, 0 );

    return hr;
}




