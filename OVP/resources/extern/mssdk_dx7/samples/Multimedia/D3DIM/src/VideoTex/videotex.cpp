//-----------------------------------------------------------------------------
// File: VideoTex.cpp
//
// Desc: Example code showing how to do use an AVI file as a texture map. This
//       sample draws a cube with an AVI texture mapped to each of its faces.
//       Ideally, the texture would be created with the DDSCAPS2_HINTDYNAMIC
//       flag set.
//
//       Note: This code uses the D3D Framework helper library.
//
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <time.h>
#include <stdio.h>
#include <windows.h>
#include <vfw.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define CUBE_SCALE        5.0f
#define NUM_CUBE_VERTICES (4*6)
#define NUM_CUBE_INDICES  (6*6)




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
	D3DVERTEX     m_pCubeVertices[NUM_CUBE_VERTICES]; // Vertices for the cube
	WORD          m_pCubeIndices[NUM_CUBE_INDICES];   // Indices for the cube

	PAVISTREAM    m_paviStream;    // The AVI stream
	PGETFRAME     m_pgfFrame;      // Where in the stream to get the next frame
	AVISTREAMINFO m_psiStreamInfo; // Info about the AVI stream

	HRESULT UpdateTexture();
	HRESULT CreateCube( D3DVERTEX* pVertices, WORD* pIndices );

protected:
	HRESULT OneTimeSceneInit();
	HRESULT InitDeviceObjects();
	HRESULT DeleteDeviceObjects();
	HRESULT Render();
	HRESULT FrameMove( FLOAT fTimeKey );
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
	m_strWindowTitle  = TEXT("VideoTex: Textures with an AVI file");
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
    HRESULT hr;

    // Generate the cube data
    CreateCube( m_pCubeVertices, m_pCubeIndices );

    // Create a texture to write the AVI file into
    D3DTextr_CreateEmptyTexture( "VideoTexture", 128, 128, 0,
		                         D3DTEXTR_16BITSPERPIXEL );

    // Initialize the AVI library
    AVIFileInit();

    // Open our AVI file
    if( FAILED( hr = AVIStreamOpenFromFile( &m_paviStream, "skiing.avi",
                                     streamtypeVIDEO, 0, OF_READ, NULL ) ) )
    {
		// If the file wasn't found, look in the DirectX SDK media path
		CHAR strFullPath[512];
		strcpy( strFullPath, D3DUtil_GetDXSDKMediaPath() );
		strcat( strFullPath, "skiing.avi" );

        hr = AVIStreamOpenFromFile( &m_paviStream, strFullPath,
                                    streamtypeVIDEO, 0, OF_READ, NULL );

        // If the AVI was still not found, return failure.
        if( FAILED( hr ) )
        {
            MessageBox( NULL, "Can't find SKIING.AVI file.", m_strWindowTitle,
                        MB_ICONERROR|MB_OK );
            return E_FAIL;
        }
    }

    // Load the video stream
    if( NULL == ( m_pgfFrame = AVIStreamGetFrameOpen( m_paviStream, NULL ) ) )
        return E_FAIL;

    // Get the stream info
    if( FAILED( hr = AVIStreamInfo( m_paviStream, &m_psiStreamInfo,
                                    sizeof(AVISTREAMINFO) ) ) )
        return E_FAIL;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Rotate the cube's world matrix
    D3DMATRIX matScale, matRotate, matWorld;
    D3DUtil_SetScaleMatrix( matScale, CUBE_SCALE,CUBE_SCALE,CUBE_SCALE );
    D3DUtil_SetRotateYMatrix( matRotate, fTimeKey );
    D3DMath_MatrixMultiply( matWorld, matScale, matRotate );
    m_pd3dDevice->SetTransform(D3DTRANSFORMSTATE_WORLD, &matWorld );

    // Update the AVI frame in the texture
    UpdateTexture();

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
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET,
                         0x000000ff, 1.0f, 0L );

    // Begin the scene
    if( FAILED( m_pd3dDevice->BeginScene() ) )
        return S_OK; // Don't return a "fatal" error

    // Draw the cube
    m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                                        m_pCubeVertices, NUM_CUBE_VERTICES,
                                        m_pCubeIndices,  NUM_CUBE_INDICES, 0 );

    // End the scene.
    m_pd3dDevice->EndScene();

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
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT, 0xffffffff );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_TEXTUREPERSPECTIVE , TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    
    // Set the projection matrix
    D3DMATRIX matProj;
    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport(&vp);
    FLOAT fAspect = ((FLOAT)vp.dwHeight) / vp.dwWidth;
    D3DUtil_SetProjectionMatrix( matProj, g_PI/4, fAspect, 1.0f, 100.0f );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set the view matrix
    D3DVECTOR vEyePt( 0.0f, 10.0f, -15.0f );
    D3DVECTOR vLookatPt( 0.0f, 0.0f, 0.0f );
    D3DVECTOR vUpVec( 0.0f, 1.0f, 0.0f );
    SetViewParams( &vEyePt, &vLookatPt, &vUpVec, 0.3f);

    // Set up the texture
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("VideoTexture") );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FinalCleanup()
// Desc: Called before the app exits, this function gives the app the chance
//       to cleanup after itself.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FinalCleanup()
{
    // Release the AVI file stream, and close the AVI library
    AVIStreamRelease( m_paviStream );
    AVIFileExit();

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
// Name: UpdateTexture()
// Desc: Called once per frame, updates the texture with the next video frame.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::UpdateTexture()
{
    // Get the texture surface so we can write to it.
    LPDIRECTDRAWSURFACE7 pddsTexture = D3DTextr_GetSurface("VideoTexture");
    if( NULL == pddsTexture )
        return E_FAIL;

    // Lock the surface so we can access it's bits
    DDSURFACEDESC2 ddsd;
    HRESULT hr;
    ddsd.dwSize = sizeof(DDSURFACEDESC2);
    hr = pddsTexture->Lock( NULL, &ddsd, DDLOCK_WAIT | DDLOCK_WRITEONLY, NULL );
    if( FAILED(hr) )
        return hr;

    // Check the surface's pixel format. Depending on the device and hardware
    // we're using, the system could create either 565 or 555 format 16 bit
    // surfaces. This app fuddles with texture bits, so we need to know what
    // format we're writing the AVI images to.
    BOOL bTextureIs565 = (ddsd.ddpfPixelFormat.dwGBitMask==0x7E0)?TRUE:FALSE;

    // Save the start time of the AVI file
    static FLOAT fAVIStartTime = ((FLOAT)clock())/CLOCKS_PER_SEC;

    // Use the clock to find which frame we should be drawing
    FLOAT fCurrTime     = ((FLOAT)clock())/CLOCKS_PER_SEC;
    FLOAT fElapsedTime  = fCurrTime-fAVIStartTime;
    FLOAT fAVITimeScale = ((FLOAT)m_psiStreamInfo.dwRate) / m_psiStreamInfo.dwScale;
    DWORD dwCurrFrame   = (DWORD)( fElapsedTime * fAVITimeScale );

    // If we exceed the AVI length, wrap to the start of the AVI
    if( dwCurrFrame >= m_psiStreamInfo.dwLength )
    {
        fAVIStartTime = ((FLOAT)clock())/CLOCKS_PER_SEC;
        dwCurrFrame   = m_psiStreamInfo.dwStart + 1;
    }

    // Get the current frame of the video
    BITMAPINFO* pbmi;
    if( FAILED( pbmi = (BITMAPINFO*)AVIStreamGetFrame( m_pgfFrame, dwCurrFrame ) ) )
        return E_FAIL;

    // Copy the current frame image to the surface. We recieve the video data
    // as a void pointer to a packed DIB. We have to skip past the header that
    // preceeds the raw video data.
    WORD* pSrc  = (WORD*)( sizeof(BITMAPINFO) + (BYTE*)pbmi );
    WORD* pDest = (WORD*)ddsd.lpSurface;

    // Copy the bits, pixel by pixel. Note that we need to handle the 565 and
    // 555 formats diffrently.
    if( bTextureIs565 )
    {
        for( DWORD i=0; i < 128*128; i++ )
        {
            WORD color = *pSrc++;
            *pDest++ = ((color & 0x1F)|((color & 0xFFE0)<<1));
        }
    }
    else
        memcpy( pDest, pSrc, sizeof(WORD)*128*128 );

    // We're done. Unlock the texture surface and return.
    pddsTexture->Unlock(NULL);

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateCube()
// Desc: Sets up the vertices for a cube.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::CreateCube( D3DVERTEX* pVertices, WORD* pIndices )
{
    // Define the normals for the cube
    D3DVECTOR n0( 0.0f, 0.0f,-1.0f ); // Front face
    D3DVECTOR n1( 0.0f, 0.0f, 1.0f ); // Back face
    D3DVECTOR n2( 0.0f,-1.0f, 0.0f ); // Top face
    D3DVECTOR n3( 0.0f, 1.0f, 0.0f ); // Bottom face
    D3DVECTOR n4(-1.0f, 0.0f, 0.0f ); // Right face
    D3DVECTOR n5( 1.0f, 0.0f, 0.0f ); // Left face

    // Set up the vertices for the cube. Note: to prevent tiling problems,
    // the u/v coords are knocked slightly inwards.

    // Front face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f,-1.0f), n0, 0.01f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f,-1.0f), n0, 0.99f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f,-1.0f), n0, 0.99f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f,-1.0f), n0, 0.01f, 0.01f );

    // Back face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f, 1.0f), n1, 0.99f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f, 1.0f), n1, 0.99f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f, 1.0f), n1, 0.01f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f, 1.0f), n1, 0.01f, 0.99f );

    // Top face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f, 1.0f), n2, 0.01f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f, 1.0f), n2, 0.99f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f,-1.0f), n2, 0.99f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f,-1.0f), n2, 0.01f, 0.01f );

    // Bottom face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f, 1.0f), n3, 0.01f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f,-1.0f), n3, 0.01f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f,-1.0f), n3, 0.99f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f, 1.0f), n3, 0.99f, 0.99f );

    // Right face
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f,-1.0f), n4, 0.01f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f, 1.0f), n4, 0.99f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f, 1.0f), n4, 0.99f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f,-1.0f), n4, 0.01f, 0.01f );

    // Left face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f,-1.0f), n5, 0.99f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f,-1.0f), n5, 0.99f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f, 1.0f), n5, 0.01f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f, 1.0f), n5, 0.01f, 0.99f );

    // Set up the indices for the cube
    *pIndices++ =  0+0;   *pIndices++ =  0+1;   *pIndices++ =  0+2;
    *pIndices++ =  0+2;   *pIndices++ =  0+3;   *pIndices++ =  0+0;
    *pIndices++ =  4+0;   *pIndices++ =  4+1;   *pIndices++ =  4+2;
    *pIndices++ =  4+2;   *pIndices++ =  4+3;   *pIndices++ =  4+0;
    *pIndices++ =  8+0;   *pIndices++ =  8+1;   *pIndices++ =  8+2;
    *pIndices++ =  8+2;   *pIndices++ =  8+3;   *pIndices++ =  8+0;
    *pIndices++ = 12+0;   *pIndices++ = 12+1;   *pIndices++ = 12+2;
    *pIndices++ = 12+2;   *pIndices++ = 12+3;   *pIndices++ = 12+0;
    *pIndices++ = 16+0;   *pIndices++ = 16+1;   *pIndices++ = 16+2;
    *pIndices++ = 16+2;   *pIndices++ = 16+3;   *pIndices++ = 16+0;
    *pIndices++ = 20+0;   *pIndices++ = 20+1;   *pIndices++ = 20+2;
    *pIndices++ = 20+2;   *pIndices++ = 20+3;   *pIndices++ = 20+0;

    return S_OK;
}




