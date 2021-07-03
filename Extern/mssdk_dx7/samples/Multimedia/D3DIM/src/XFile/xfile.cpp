//-----------------------------------------------------------------------------
// File: XFile.cpp
//
// Desc: Example code showing how to load .X files in D3DIM.
//
//       Note: This code uses the D3D Framework helper library.
//
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <windows.h>
#include <commdlg.h>
#include <math.h>
#include <stdio.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"
#include "D3DFile.h"
#include "resource.h"




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    CD3DFile* m_pFileObject;
    FLOAT     m_fObjectRadius;
    FLOAT     m_fEyeDistance;

    D3DMATRIX m_matSave;
    D3DMATRIX m_matRotate;
    BOOL      m_fLMouseCapture;
    POINT     m_ptCursorCenter;
    
    BOOL      m_bCull;
    BOOL      m_bFlat;
    BOOL      m_bWire;

    HRESULT LoadFile();

protected:
    HRESULT OneTimeSceneInit();
    HRESULT InitDeviceObjects();
    HRESULT DeleteDeviceObjects();
    HRESULT Render();
    HRESULT FrameMove( FLOAT fTimeKey );
    HRESULT FinalCleanup();

public:
    CMyD3DApplication();

    LRESULT MsgProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam );
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
    m_strWindowTitle  = TEXT( "XFile: Loading .X files in D3DIM" );
    m_bAppUseZBuffer  = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = NULL;

    m_pFileObject     = NULL;
    m_fObjectRadius   = 0.0f;
    m_fEyeDistance    = 0.0f;
    m_bCull           = TRUE;
    m_bFlat           = FALSE;
    m_bWire           = FALSE;
    m_fLMouseCapture  = FALSE;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    D3DUtil_SetIdentityMatrix( m_matSave );
    D3DUtil_SetIdentityMatrix( m_matRotate );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Use mouse input to setup the world matrix
    if( m_fLMouseCapture )
    {
        POINT ptCursor;
        GetCursorPos( &ptCursor );

        int dx = ptCursor.x - m_ptCursorCenter.x;
        int dy = ptCursor.y - m_ptCursorCenter.y;

        D3DMATRIX mX,mY;
        D3DUtil_SetRotateYMatrix( mX, -dx * 2.0f * g_PI / 512.0f );
        D3DUtil_SetRotateXMatrix( mY,  dy * 2.0f * g_PI / 512.0f );
        D3DMath_MatrixMultiply( m_matRotate, mY, mX );
        D3DMath_MatrixMultiply( m_matRotate, m_matRotate, m_matSave );
    }
    else
        m_matSave = m_matRotate;

    // Set the object's world matrix
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &m_matRotate );

    // Adjust the camera position
    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f, 0.0f, 0.0f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 0.0f, 0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );
    vEyePt.z += m_fObjectRadius*3.0f + m_fObjectRadius*m_fEyeDistance;
        
    // Set the view matrix
    D3DMATRIX matView;
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );

    // Just for kicks, demonstrate the feature to find a frame and animate it.
    // Here, if the user loads a file with a frame named "prop" (such as
    // "triplane.x"), this code will find the matrix for that frame and
    // animate it.
    if( m_pFileObject )
    {
        CD3DFileObject* pObject = m_pFileObject->FindObject( "prop" );
        if( pObject )
        {
            D3DMATRIX* pmat = pObject->GetMatrix();
            D3DUtil_SetRotateZMatrix( *pmat, 5.0f*fTimeKey );
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
    // Clear the viewport
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER,
                         0x000000ff, 1.0f, 0L );

    DWORD dwCullMode  = m_bCull ? D3DCULL_CCW : D3DCULL_NONE;
    DWORD dwShadeMode = m_bFlat ? D3DSHADE_FLAT : D3DSHADE_GOURAUD;
    DWORD dwFillMode  = m_bWire ? D3DFILL_WIREFRAME : D3DFILL_SOLID;
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_CULLMODE,  dwCullMode );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SHADEMODE, dwShadeMode );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_FILLMODE,  dwFillMode );

    // Begin the scene 
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        if( m_pFileObject )
            m_pFileObject->Render( m_pd3dDevice );

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
    // Load a DirectX .X file the first time around
    static BOOL bFirstInstance = TRUE;
    if( bFirstInstance ) 
    {
        Pause( TRUE );
        LoadFile();
        Pause( FALSE );
        bFirstInstance = FALSE;
    }

    // Setup a material
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );

    // Setup the textures
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,        TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_NORMALIZENORMALS, TRUE );

    // Set the projection matrix
    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport(&vp);
    FLOAT fAspect = ((FLOAT)vp.dwHeight) / vp.dwWidth;

    D3DMATRIX matProj;
    D3DUtil_SetProjectionMatrix( matProj, g_PI/4, fAspect, 1.0f, m_fObjectRadius*10 );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set up the light
    if( m_pDeviceInfo->ddDeviceDesc.dwVertexProcessingCaps &
                                                D3DVTXPCAPS_DIRECTIONALLIGHTS )
    {
        D3DLIGHT7 light;
        D3DUtil_InitLight( light, D3DLIGHT_DIRECTIONAL, 0.0f, -1.0f, -1.0f );
        m_pd3dDevice->SetLight( 0, &light );
        m_pd3dDevice->LightEnable( 0, TRUE );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, TRUE );
        m_pd3dDevice->SetRenderState(  D3DRENDERSTATE_AMBIENT, 0x00000000 );
    }
    else
        m_pd3dDevice->SetRenderState(  D3DRENDERSTATE_AMBIENT, 0xffffffff );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CMyD3DApplication::DeleteDeviceObjects()
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
    SAFE_DELETE( m_pFileObject );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CalcFileObjectSizeCB()
// Desc: Callback used to calculate the radius of a sphere that encloses all
//       the meshes in the file.
//-----------------------------------------------------------------------------
BOOL CalcFileObjectSizeCB( CD3DFileObject* pObject, D3DMATRIX* pmat,
                           VOID* pContext )
{
    FLOAT*     pfRadius = (FLOAT*)pContext;
    D3DVERTEX* pVertices;
    DWORD      dwNumVertices;

    if( SUCCEEDED( pObject->GetMeshGeometry( &pVertices, &dwNumVertices, 
                                             NULL, NULL ) ) )
    {
        for( DWORD i=0; i<dwNumVertices; i++ )
        {
            FLOAT x = pVertices[i].x;
            FLOAT y = pVertices[i].y;
            FLOAT z = pVertices[i].z;

            FLOAT mx = x*pmat->_11 + y*pmat->_21 + z*pmat->_31 + pmat->_41;
            FLOAT my = x*pmat->_12 + y*pmat->_22 + z*pmat->_32 + pmat->_42;
            FLOAT mz = x*pmat->_13 + y*pmat->_23 + z*pmat->_33 + pmat->_43;

            // Store the largest r (radius) for any point in the mesh
            FLOAT r = sqrtf( mx*mx + my*my + mz*mz );
            if( r > (*pfRadius) )
                (*pfRadius) = r;
        }
    }

    // Keep enumerating file objects
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: LoadFile()
// Desc: Uses Windows' OpenFileName dialog to get the name of an X file to
//       load, then proceeds to load that file.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::LoadFile()
{
    static TCHAR strInitialDir[512] = "";
    static TCHAR strFileName[512];
    TCHAR strCurrentName[512] = "*.x";
    
    if( '\0' == strInitialDir[0] )
        strcpy( strInitialDir, D3DUtil_GetDXSDKMediaPath() );

    OPENFILENAME ofn = { sizeof(OPENFILENAME), m_hWnd, NULL,
                         "X Files (*.x)\0*.x\0\0",
                         NULL, 0, 1, strCurrentName, 512, strFileName, 512,
                         strInitialDir, "Open X File", OFN_FILEMUSTEXIST, 0, 1,
                         ".X", 0, NULL, NULL };

    // Run the OpenFileName dialog.
    if( FALSE == GetOpenFileName( &ofn ) )
        return S_OK;

    // Store the initial directory for next time
    strcpy( strInitialDir, strCurrentName );
    strstr( strInitialDir, strFileName )[0] = '\0';

    CD3DFile* pFileObject = new CD3DFile();
    
    if( FAILED( pFileObject->Load( strFileName ) ) )
    {
        MessageBox( NULL, TEXT("Error loading specified X file"),
                    TEXT("XFile"), MB_OK|MB_ICONERROR );
        return E_FAIL;
    }

    // If the file was successfully loaded, delete the old one and use this one
    // instead.
    SAFE_DELETE( m_pFileObject );
    m_pFileObject = pFileObject;

    // Get the new object position and size
    D3DUtil_SetIdentityMatrix( m_matRotate );
    D3DUtil_SetIdentityMatrix( m_matSave );
    m_fObjectRadius = 0.0f;
    m_pFileObject->EnumObjects( CalcFileObjectSizeCB, NULL, (VOID*)&m_fObjectRadius );

    // Set the projection matrix
    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport(&vp);
    FLOAT fAspect = ((FLOAT)vp.dwHeight) / vp.dwWidth;

    D3DMATRIX matProj;
    D3DUtil_SetProjectionMatrix( matProj, g_PI/4, fAspect, 1.0f, m_fObjectRadius*10 );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Restore the textures (if there's a device).
    D3DTextr_RestoreAllTextures( m_pd3dDevice );

    // Return successful
    return S_OK;
}




//----------------------------------------------------------------------------
// Name: MsgProc()
// Desc: App custom WndProc function for handling mouse and keyboard input.
//----------------------------------------------------------------------------
LRESULT CMyD3DApplication::MsgProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                                    LPARAM lParam )
{
    switch( uMsg )
    {
        case WM_COMMAND:
            // Handle menu and key commands
            switch( LOWORD(wParam) )
            {
                case ID_OPTIONS_ZOOMIN:    m_fEyeDistance -= 0.2f; break;
                case ID_OPTIONS_ZOOMOUT:   m_fEyeDistance += 0.2f; break;
                case ID_OPTIONS_CULLING:   m_bCull = !m_bCull; break;
                case ID_OPTIONS_SHADING:   m_bFlat = !m_bFlat; break;
                case ID_OPTIONS_WIREFRAME: m_bWire = !m_bWire; break;

                case IDM_LOADFILE:
                    Pause( TRUE );
                    LoadFile();
                    Pause( FALSE );
                    break;
            }
            break;

        case WM_LBUTTONDOWN:
            // With the left mouse button down, this sample tracks the mouse
            // position, using it to rotate the object
            SetCapture( hWnd );
            GetCursorPos( &m_ptCursorCenter );
            m_fLMouseCapture = TRUE;
            break;

        case WM_LBUTTONUP:
            // The user released the left mouse button
            ReleaseCapture();
            m_fLMouseCapture = FALSE;
            break;
    }

    // Fall through to the app's main windows proc function
    return CD3DApplication::MsgProc( hWnd, uMsg, wParam, lParam );
}




