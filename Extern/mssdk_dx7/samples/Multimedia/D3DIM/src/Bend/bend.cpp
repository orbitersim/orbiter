//-----------------------------------------------------------------------------
// File: Bend.cpp
//
// Desc: Example code showing how to do a skinning effect, using the vertex
//       blending feature of Direct3D. Normally, Direct3D transforms each
//       vertex through the world matrix. The vertex blending feature,
//       however, uses mulitple world matrices and a per-vertex blend factor
//       to transform each vertex.
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




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define PI                  3.14159265358979323846f
#define ELLIPSE_NUMRINGS    20
#define ELLIPSE_NUMSECTIONS 20
#define ELLIPSE_X_LENGTH    1.5f
#define ELLIPSE_Y_LENGTH    1.5f
#define ELLIPSE_Z_LENGTH    4.5f




//-----------------------------------------------------------------------------
// Name: struct D3DBLENDVERTEX
// Desc: Custom vertex which includes a blending factor
//-----------------------------------------------------------------------------
struct D3DBLENDVERTEX
{
    D3DVECTOR v;
    FLOAT     blend;
    D3DVECTOR n;
    FLOAT     tu, tv;
};




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{

    LPDIRECT3DVERTEXBUFFER7 m_pvbRenderVerticesVB;

    D3DBLENDVERTEX*         m_pRenderVertices;
    DWORD                   m_dwNumVertices;
    
    WORD*                   m_pRenderIndices;
    DWORD                   m_dwNumIndices;

    VOID    SetBlendFactor( DWORD dwCount, D3DBLENDVERTEX* pIn );
    VOID    RotateVertexInX( FLOAT, DWORD, D3DBLENDVERTEX*, D3DBLENDVERTEX* );
    BOOL    GenerateEllipse( DWORD, DWORD, FLOAT, FLOAT, FLOAT,
                             D3DBLENDVERTEX**, DWORD*, WORD**, DWORD* );
    HRESULT CreateSceneVertexBuffers();
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
    CMyD3DApplication();
};




//-----------------------------------------------------------------------------
// Name: InitBlendVertex()
// Desc: Initializes the custom blending vertex
//-----------------------------------------------------------------------------
VOID InitBlendVertex( D3DBLENDVERTEX& vtx, D3DVECTOR& v, FLOAT blend,
                      D3DVECTOR& n, FLOAT tu, FLOAT tv )
{
    vtx.v     = v;
    vtx.blend = blend;
    vtx.n     = n;
    vtx.tu    = tu;
    vtx.tv    = tv;
}




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
    m_strWindowTitle  = TEXT( "Bend: Surface Skinning Example" );
    m_bAppUseZBuffer  = TRUE;
    m_bAppUseStereo   = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = ConfirmDevice;

    m_pRenderVertices = NULL;
    m_pRenderIndices  = NULL;
    m_dwNumVertices   = 0;
    m_dwNumIndices    = 0;

    m_pvbRenderVerticesVB = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Generate the object data
    GenerateEllipse( ELLIPSE_NUMRINGS, ELLIPSE_NUMSECTIONS,
                     ELLIPSE_X_LENGTH, ELLIPSE_Y_LENGTH, ELLIPSE_Z_LENGTH,
                     &m_pRenderVertices, &m_dwNumVertices,
                     &m_pRenderIndices, &m_dwNumIndices );

    SetBlendFactor( m_dwNumVertices, m_pRenderVertices );

    // Create textures
    D3DTextr_CreateTextureFromFile( "Banana.bmp" );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Set the vertex blending bend angle for this frame
    FLOAT fBendAngle = ( sinf(fTimeKey) + 1.0f ) * 0.6f;

    D3DMATRIX matUpperArm, matLowerArm;
    D3DUtil_SetIdentityMatrix( matUpperArm );
    D3DUtil_SetRotateXMatrix( matLowerArm, -fBendAngle );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,  &matUpperArm );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD1, &matLowerArm );

    // Rotate the camera about the y-axis
    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f, 0.0f, 0.0f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 0.0f, 0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );
    vEyePt.x =  sinf(fTimeKey/3.0f) * 6.5f;
    vEyePt.z = -cosf(fTimeKey/3.0f) * 6.5f;

    // Set the app view matrix for normal viewing
    D3DMATRIX matView;
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    SetAppViewMatrix( matView );

    // Set the view matrices for stereoscopic-enabled viewing
    D3DVECTOR vView = vLookatPt - vEyePt;
    vView = Normalize( CrossProduct( vView, vUpVec ) ) * 0.1f;
    D3DUtil_SetViewMatrix( matView, vEyePt + vView, vLookatPt, vUpVec );
    SetAppLeftViewMatrix( matView );
    D3DUtil_SetViewMatrix( matView, vEyePt - vView, vLookatPt, vUpVec );
    SetAppRightViewMatrix( matView );

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
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER,
                         0x000000ff, 1.0f, 0L );

    // Begin the scene
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        // Display the object
        m_pd3dDevice->DrawIndexedPrimitiveVB( D3DPT_TRIANGLELIST,
                                    m_pvbRenderVerticesVB, 0, m_dwNumVertices,
                                    m_pRenderIndices, m_dwNumIndices, 0x0 );

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
    // Set up the textures
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("Banana.bmp") );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );

    // Set miscellaneous render states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,        TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,        0x40404040 );

    // Set render states for blending vertices
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_NORMALIZENORMALS,  TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_VERTEXBLEND, D3DVBLEND_1WEIGHT );

    // Set wrap flags for both coordinates in 1st set of texcoords:
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_WRAP0, D3DWRAPCOORD_0|D3DWRAPCOORD_1 );

    // Set the projection matrix
    D3DMATRIX matProj;
    D3DUtil_SetProjectionMatrix( matProj, 1.57f, 1.0f, 1.0f, 100.0f );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Setup a material
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );

    // Set up lighting states
    if( m_pDeviceInfo->ddDeviceDesc.dwVertexProcessingCaps &
                                                D3DVTXPCAPS_DIRECTIONALLIGHTS )
    {
        D3DLIGHT7 light;
        D3DUtil_InitLight( light, D3DLIGHT_DIRECTIONAL, 0.0f, -1.0f, 0.0f );
        m_pd3dDevice->SetLight( 0, &light );
        m_pd3dDevice->LightEnable( 0, TRUE );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, TRUE );
    }

    CreateSceneVertexBuffers();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateSceneVertexBuffers()
// Desc: Create vertex buffers for the scene
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::CreateSceneVertexBuffers()
{
    // Create and fill the vertex buffer.
    D3DVERTEXBUFFERDESC vbdesc;
    ZeroMemory( &vbdesc, sizeof(D3DVERTEXBUFFERDESC) );
    vbdesc.dwSize        = sizeof(D3DVERTEXBUFFERDESC);
    vbdesc.dwCaps        = D3DVBCAPS_WRITEONLY;
    vbdesc.dwFVF         = D3DFVF_XYZB1|D3DFVF_NORMAL|D3DFVF_TEX1;
    vbdesc.dwNumVertices = m_dwNumVertices;

    // If the device does not support transform and lighting in hardware, make
    // sure the vertex buffers end up in system memory.
    if( IID_IDirect3DTnLHalDevice != (*m_pDeviceInfo->pDeviceGUID) )
        vbdesc.dwCaps |= D3DVBCAPS_SYSTEMMEMORY;

    if( FAILED( m_pD3D->CreateVertexBuffer( &vbdesc, &m_pvbRenderVerticesVB, 0L ) ) )
        return E_FAIL;

    // Lock the vertex buffer and fill it
    VOID* pVBVertices;
    if( SUCCEEDED( m_pvbRenderVerticesVB->Lock( DDLOCK_WAIT, &pVBVertices, NULL ) ) )
    {
        memcpy( pVBVertices, m_pRenderVertices, (4+3+2)*sizeof(FLOAT)*m_dwNumVertices );
        m_pvbRenderVerticesVB->Unlock();

        // Optimize the vertex buffer
        m_pvbRenderVerticesVB->Optimize( m_pd3dDevice, 0L );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RestoreSurfaces()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RestoreSurfaces()
{
    // We need to restore the contents of our vertex buffer, which may have
    // been stored in video memory. Since D3D does not let you update the
    // contents of optimized vertex buffers, we need to destroy and recreate
    // the optimized vertxe buffers used by this app.
    SAFE_RELEASE( m_pvbRenderVerticesVB );
    CreateSceneVertexBuffers();

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

    SAFE_RELEASE( m_pvbRenderVerticesVB );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FinalCleanup()
// Desc: Called before the app exits, this function gives the app the chance
//       to cleanup after itself.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FinalCleanup()
{
    SAFE_DELETE( m_pRenderVertices );
    SAFE_DELETE( m_pRenderIndices );

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
    if( pd3dDeviceDesc->wMaxVertexBlendMatrices >= 2 )
        return S_OK;

    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: SetBlendFactor()
// Desc: Sets the blend factor for the vertices
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::SetBlendFactor( DWORD dwCount, D3DBLENDVERTEX* pIn )
{
    FLOAT fMinZ = -ELLIPSE_Z_LENGTH;
    FLOAT fMaxZ = +ELLIPSE_Z_LENGTH;

    for( DWORD i=0; i<dwCount; i++ )
    {
        FLOAT a = ( pIn[i].v.z - fMinZ ) / ( fMaxZ - fMinZ );

        if( a >= 0.75f )
            pIn[i].blend = 0.0f;
        else if( a >= 0.5f )
            pIn[i].blend = 8.0f*((0.75f-a)*(0.75f-a));
        else if( a >= 0.25f )
            pIn[i].blend = 1.0f-8.0f*((a-0.25f)*(a-0.25f));
        else
            pIn[i].blend = 1.0f;
    }
}




//-----------------------------------------------------------------------------
// Name: GenerateEllipse()
// Desc: Makes vertex and index data for a sphere.
//-----------------------------------------------------------------------------
BOOL CMyD3DApplication::GenerateEllipse( DWORD dwNumRings, DWORD dwNumSections,
                            FLOAT sx, FLOAT sy, FLOAT sz,
                            D3DBLENDVERTEX** ppVertices, DWORD* pdwNumVertices,
                            WORD** ppIndices, DWORD* pdwNumIndices )
{
    DWORD i, j, n, m; // Counters

    // Generate space for the required triangles and vertices.
    DWORD      dwNumTriangles = (dwNumRings+1) * dwNumSections * 2;
    DWORD      dwNumVertices  = (dwNumRings+1) * dwNumSections + 2;
    DWORD      dwNumIndices   = dwNumTriangles*3;
    D3DBLENDVERTEX* pVertices = new D3DBLENDVERTEX[dwNumVertices];
    WORD*           pIndices  = new WORD[dwNumIndices];

    // Generate vertices at the end points.
    D3DVECTOR vPoint  = D3DVECTOR( 0.0f, 0.0f, sz );
    D3DVECTOR vNormal = D3DVECTOR( 0.0f, 0.0f, 1.0f );
    InitBlendVertex( pVertices[0], vPoint, 1.0f, vNormal, 0.0f, 0.0f );
    InitBlendVertex( pVertices[dwNumVertices-1], -vPoint, 1.0f, -vNormal, 0.0f, 0.0f );

    // Generate vertex points for rings
    FLOAT dtheta = (FLOAT)(PI / (dwNumRings + 2));     //Angle between each ring
    FLOAT dphi   = (FLOAT)(2*PI / dwNumSections); //Angle between each section
    FLOAT theta  = dtheta;
    n = 1; //vertex being generated, begins at 1 to skip top point

    for( i = 0; i < (dwNumRings+1); i++ )
    {
        FLOAT z   = cosf(theta); // z is the same for each ring
        FLOAT tv  = theta / PI;                  // v is the same for each ring
        FLOAT phi = 0.0f;
        FLOAT rsintheta = sinf(theta);

        for( j = 0; j < dwNumSections; j++ )
        {
            FLOAT x  =  rsintheta * sinf(phi);
            FLOAT y  = -rsintheta * cosf(phi);
            FLOAT tu = (FLOAT)(1.0 - phi / (2*PI) );

            vPoint  = D3DVECTOR( sx*x, sy*y, sz*z );
            vNormal = D3DVECTOR( x, y, z );
            InitBlendVertex( pVertices[n], vPoint, 1.0f, vNormal, tu, tv );

            phi += dphi;
            ++n;
        }
        theta += dtheta;
    }

    // Generate triangles for top and bottom caps.
    for( i = 0; i < dwNumSections; i++ )
    {
        DWORD t1 = 3*i;
        DWORD t2 = 3*(dwNumTriangles - dwNumSections + i);

        pIndices[t1+0] = (WORD)(0);
        pIndices[t1+1] = (WORD)(i + 1);
        pIndices[t1+2] = (WORD)(1 + ((i + 1) % dwNumSections));

        pIndices[t2+0] = (WORD)( dwNumVertices - 1 );
        pIndices[t2+1] = (WORD)( dwNumVertices - 2 - i );
        pIndices[t2+2] = (WORD)( dwNumVertices - 2 - ((1 + i) % dwNumSections) );
    }

    // Generate triangles for the rings
    m = 1;            // 1st vertex begins at 1 to skip top point
    n = dwNumSections; // triangle being generated, skip the top cap

    for( i = 0; i < dwNumRings; i++ )
    {
        for( j = 0; j < dwNumSections; j++ )
        {
            pIndices[3*n+0] = (WORD)(m + j);
            pIndices[3*n+1] = (WORD)(m + dwNumSections + j);
            pIndices[3*n+2] = (WORD)(m + dwNumSections + ((j + 1) % dwNumSections));
            n++;

            pIndices[3*n+0] = (WORD)(m + j);
            pIndices[3*n+1] = (WORD)(m + dwNumSections + ((j + 1) % dwNumSections));
            pIndices[3*n+2] = (WORD)(m + ((j + 1) % dwNumSections));
            n++;
        }
        m += dwNumSections;
    }

    (*pdwNumIndices)  = dwNumIndices;
    (*ppIndices)      = pIndices;
    (*pdwNumVertices) = dwNumVertices;
    (*ppVertices)     = pVertices;

    return TRUE;
}




