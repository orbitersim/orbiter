//-----------------------------------------------------------------------------
// File: Compress.cpp
//
// Desc: Example code shows how to display a compressed DDS texture.
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <math.h>
#include <stdio.h>
#include <windows.h>
#include <mmsystem.h>
#include "D3DApp.h"
#include "D3DUtil.h"
#include "D3DMath.h"
#include "resource.h"




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define NUM_CUBE_VERTICES (6*4)
#define NUM_CUBE_INDICES  (6*2*3)




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    static DDPIXELFORMAT m_PixelFormats[100]; // Stores pixel formats
    static DWORD         m_dwNumPixelFormats;

    LPDIRECTDRAWSURFACE7 m_pddsTexture; // The compressed DDS texture 

    D3DVERTEX m_pCubeVertices[NUM_CUBE_VERTICES];
    WORD      m_pCubeIndices[NUM_CUBE_INDICES];

    DWORD     m_dwDXT;
    BOOL      m_bMipTexture;
    BOOL      m_bSupportsMipmaps;

    CHAR      m_strDiskPixelFormat[20];
    CHAR      m_strRendererPixelFormat[20];

    HRESULT CreateCube( D3DVERTEX* pVertices, WORD* pIndices );
    HRESULT ChangeTexture();
    HRESULT LoadTexture( CHAR* strName, LPDIRECTDRAWSURFACE7* ppddsTexture );
    HRESULT ReadDDSTexture( CHAR* strName, DDSURFACEDESC2* pddsdComp, 
                            LPDIRECTDRAWSURFACE7* ppddsCompTop );
    HRESULT BltToTextureSurface( DDSURFACEDESC2 ddsd, DDPIXELFORMAT ddpf, 
                                 LPDIRECTDRAWSURFACE7 pddsDXT, 
                                 LPDIRECTDRAWSURFACE7* ppddsNewSurface );
    HRESULT FindBestPixelFormatMatch( DDPIXELFORMAT ddsdDDSTexture, 
                                      DDPIXELFORMAT* pddsdBestMatch );
    static HRESULT WINAPI EnumTextureFormats( DDPIXELFORMAT* pddpf, VOID* );

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
// Static members
//-----------------------------------------------------------------------------
DDPIXELFORMAT CMyD3DApplication::m_PixelFormats[100];
DWORD         CMyD3DApplication::m_dwNumPixelFormats;




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
    m_strWindowTitle  = TEXT( "Compress: Using Compressed Textures" );
    m_bAppUseZBuffer  = FALSE;
    m_bAppUseStereo   = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = NULL;

    m_pddsTexture     = NULL;
    m_bMipTexture     = TRUE;
    m_dwDXT           = 0L;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Generate the cube data
    CreateCube( m_pCubeVertices, m_pCubeIndices );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    FLOAT z;
    
    // If texture has mip maps, make the cube move toward and away
    // from viewer so all mip levels can be seen.  Otherwise, keep
    // the cube at a fixed distance.
    if ( m_bMipTexture )
        z = 30.0f * ( sinf(fTimeKey) + 1.0f );
    else
        z = 0.0f;
    
    // Setup the world matrix to spin and translate the object
    D3DMATRIX matSpinY, matSpinX, matTrans, matWorld;
    D3DUtil_SetRotateYMatrix( matSpinY, fTimeKey / 0.5f );
    D3DUtil_SetRotateXMatrix( matSpinX, fTimeKey / 0.3f );
    D3DUtil_SetTranslateMatrix( matWorld, 0.0f, 0.0f, z );
    D3DMath_MatrixMultiply( matWorld, matSpinY, matWorld );
    D3DMath_MatrixMultiply( matWorld, matSpinX, matWorld );
    
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );

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
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET, 0x000000ff, 1.0f, 0L );

    // Begin the scene 
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        // Display the object
        m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                                            m_pCubeVertices, NUM_CUBE_VERTICES, 
                                            m_pCubeIndices,  NUM_CUBE_INDICES, 0 );

        // End the scene.
        m_pd3dDevice->EndScene();
    }

    // Output the texture's pixel formats to the frame buffer
    CHAR buffer[80];
    sprintf( buffer, "TEXTURE%d.DDS ( %s as %s )", m_dwDXT,
                     m_strDiskPixelFormat, m_strRendererPixelFormat );
    OutputText( 0, 20, buffer );

    // If an error occurred, output that as well
    if( NULL == m_pddsTexture )
        OutputText( 0, 40, "Couldn't load/blit compressed texture file!" );

    return S_OK;
}





//-----------------------------------------------------------------------------
// Name: ChangeTexture()
// Desc: Frees the old texture and loads a new one.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::ChangeTexture()
{
    // Release the old texture
    SAFE_RELEASE( m_pddsTexture );

    // Load the new texture
    CHAR  strTextureName[256];
    sprintf( strTextureName, "TEXTURE%d.DDS", m_dwDXT );

    if( FAILED( LoadTexture( strTextureName, &m_pddsTexture ) ) )
    {
        m_pddsTexture = NULL;
        return E_FAIL;
    }

    m_pd3dDevice->SetTexture( 0, m_pddsTexture );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    m_bSupportsMipmaps  = FALSE;
    m_dwNumPixelFormats = 0;

    // Set texture states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );

    // Set the transform matrices
    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f, 0.0f, -6.5f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 0.0f,  0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f,  0.0f );
    D3DMATRIX matWorld, matProj;

    SetViewParams( &vEyePt, &vLookatPt, &vUpVec , 0.1f);

    D3DUtil_SetProjectionMatrix( matProj, g_PI/4, 1.0f, 1.0f, 100.0f );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Create and set up the object material
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );

    // Set up the light
    if( m_pDeviceInfo->ddDeviceDesc.dwVertexProcessingCaps &
                                                D3DVTXPCAPS_DIRECTIONALLIGHTS )
    {
        D3DLIGHT7 light;
        D3DUtil_InitLight( light, D3DLIGHT_DIRECTIONAL, 0.0f, -0.4f, 1.0f );
        m_pd3dDevice->SetLight( 0, &light );
        m_pd3dDevice->LightEnable( 0, TRUE );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, TRUE );
    }

    // Check if device supports mipmapping 
    DWORD dwFilterCaps = m_pDeviceInfo->ddDeviceDesc.dpcTriCaps.dwTextureFilterCaps;
    if( dwFilterCaps & (D3DPTFILTERCAPS_MIPNEAREST|D3DPTFILTERCAPS_MIPLINEAR ) )
        m_bSupportsMipmaps = TRUE;

    // Enumerate the available texture formats into a static list
    if( FAILED( m_pd3dDevice->EnumTextureFormats( EnumTextureFormats, NULL ) ) )
        return E_FAIL;

    // Set up the texture
    ChangeTexture();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DeleteDeviceObjects()
// Desc: Called when the app is exitting, or the device is being changed,
//       this function deletes any device dependant objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DeleteDeviceObjects()
{
    SAFE_RELEASE( m_pddsTexture );

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
// Name: GetNumberOfBits()
// Desc: Returns the number of bits set in a DWORD mask
//-----------------------------------------------------------------------------
WORD GetNumberOfBits( DWORD dwMask )
{
    for( WORD wBits = 0; dwMask; wBits++ )
        dwMask = dwMask & ( dwMask - 1 );  

    return wBits;
}




//-----------------------------------------------------------------------------
// Name: PixelFormatToString()
// Desc: Creates a string describing a pixel format.
//-----------------------------------------------------------------------------
VOID PixelFormatToString( CHAR* strPixelFormat, DDPIXELFORMAT* pddpf )
{
    switch( pddpf->dwFourCC )
    {
        case 0:
            // This dds texture isn't compressed so write out ARGB format
            sprintf( strPixelFormat, "ARGB-%d%d%d%d%s", 
                     GetNumberOfBits( pddpf->dwRGBAlphaBitMask ), 
                     GetNumberOfBits( pddpf->dwRBitMask ),
                     GetNumberOfBits( pddpf->dwGBitMask ),
                     GetNumberOfBits( pddpf->dwBBitMask ),
                     pddpf->dwBBitMask & DDPF_ALPHAPREMULT ? "-premul" : "" );
            break;

        case MAKEFOURCC('D','X','T','1'):
            strcpy( strPixelFormat, "DXT1" );
            break;

        case MAKEFOURCC('D','X','T','2'):
            strcpy( strPixelFormat, "DXT2" );
            break;

        case MAKEFOURCC('D','X','T','3'):
            strcpy( strPixelFormat, "DXT3" );
            break;

        case MAKEFOURCC('D','X','T','4'):
            strcpy( strPixelFormat, "DXT4" );
            break;

        case MAKEFOURCC('D','X','T','5'):
            strcpy( strPixelFormat, "DXT5" );
            break;
    }
}




//-----------------------------------------------------------------------------
// Name: ReadDDSTexture()
// Desc: Reads a DDS texture format from disk given a filename.
//       ppCompTop contains the DDS surface, and 
//       pddsdComp contains the DDS surface description
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::ReadDDSTexture( CHAR* strTextureName,
                                           DDSURFACEDESC2* pddsdNew, 
                                           LPDIRECTDRAWSURFACE7* ppddsNew )
{
    LPDIRECTDRAWSURFACE7 pdds = NULL;
    DDSURFACEDESC2       ddsd;
    DWORD   dwMagic;
    HRESULT hr;

    // Open the compressed texture file
    FILE* file = fopen( strTextureName, "rb" );
    if( file == NULL )
    {
        CHAR strPathName[80] = "";
        strcat( strPathName, D3DUtil_GetDXSDKMediaPath() );
        strcat( strPathName, strTextureName );

        file = fopen( strPathName, "rb" );
        if( file == NULL )
            return E_FAIL;
    }

    // Read magic number
    fread( &dwMagic, sizeof(DWORD), 1, file );
    if( dwMagic != MAKEFOURCC('D','D','S',' ') )
    {
        fclose( file );
        return E_FAIL;
    }

    // Read the surface description
    fread( &ddsd, sizeof(DDSURFACEDESC2), 1, file );

    // Load DDS into system memory
    ddsd.ddsCaps.dwCaps  |= DDSCAPS_SYSTEMMEMORY;

    // Handle special case for hardware that doesn't support mipmapping
    if( !m_bSupportsMipmaps )
    {
        ddsd.dwMipMapCount   = 0;
        ddsd.dwFlags        &= ~DDSD_MIPMAPCOUNT;
        ddsd.ddsCaps.dwCaps &= ~( DDSCAPS_MIPMAP | DDSCAPS_COMPLEX );
    }

    // Does texture have mipmaps?
    m_bMipTexture = ( ddsd.dwMipMapCount > 0 ) ? TRUE : FALSE;

    // Clear unwanted flags
    ddsd.dwFlags &= (~DDSD_PITCH);
    ddsd.dwFlags &= (~DDSD_LINEARSIZE);

    // Store the return copy of this surfacedesc
    (*pddsdNew) = ddsd;

    // Create a new surface based on the surface description
    if( FAILED( hr = m_pDD->CreateSurface( &ddsd, &pdds, NULL ) ) )
    {
        fclose( file );
        return hr;
    }

    // Store the return copy of the compressed surface
    (*ppddsNew) = pdds;
    (*ppddsNew)->AddRef();

    while( TRUE )
    {
        if( FAILED( hr = pdds->Lock( NULL, &ddsd, DDLOCK_WAIT, NULL )))
        {
            fclose( file );
            return hr;
        }

        if( ddsd.dwFlags & DDSD_LINEARSIZE )
        {
            fread( ddsd.lpSurface, ddsd.dwLinearSize, 1, file );
        }
        else
        {
            BYTE* pDest = (BYTE*)ddsd.lpSurface;
            DWORD dwBytesPerRow = ddsd.dwWidth * ddsd.ddpfPixelFormat.dwRGBBitCount / 8;
            
            for( DWORD yp = 0; yp < ddsd.dwHeight; yp++ )
            {
                fread( pDest, dwBytesPerRow, 1, file );
                pDest += ddsd.lPitch;
            }
        }

        pdds->Unlock( NULL );

        if( !m_bSupportsMipmaps )
        {
            // For mipless hardware, don't copy mipmaps
            pdds->Release();
            fclose( file );
            return S_OK;
        }

        // Get the next surface.
        LPDIRECTDRAWSURFACE7 pddsNext;
        DDSCAPS2 ddsCaps = { DDSCAPS_TEXTURE|DDSCAPS_MIPMAP|DDSCAPS_COMPLEX, 0, 0, 0 };
        
        if( FAILED( hr = pdds->GetAttachedSurface( &ddsCaps, &pddsNext ) ) )
        {
            // Failure means were done with the mipmaps
            pdds->Release();
            fclose( file );
            return S_OK;
        }

        pdds->Release();
        pdds = pddsNext;
    }
}




//-----------------------------------------------------------------------------
// Name: EnumTextureFormats()
// Desc: Stores enumerates texture formats in a global array.
//-----------------------------------------------------------------------------
HRESULT WINAPI CMyD3DApplication::EnumTextureFormats( DDPIXELFORMAT* pddpf,
                                                      VOID* )
{
    m_PixelFormats[m_dwNumPixelFormats++] = (*pddpf);

    return DDENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: FindBestPixelFormatMatch()
// Desc: Finds the best pixel format match for the cuurent device.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FindBestPixelFormatMatch( DDPIXELFORMAT ddpfDDSTexture, 
                                                     DDPIXELFORMAT* pddpfBestMatch )
{
    int  nCompAlphaBits;
    int  nHighestFound = 0;
    BOOL bCompressedTexture;
    BOOL bPremultipliedAlpha;

    // Set how many alpha bits are in the compressed texture 
    switch( ddpfDDSTexture.dwFourCC )
    {
        case 0:
            // This dds texture isn't compressed so we need an exact pixel
            // format match to render this surface (or do a manual pixel
            // conversion)
            bCompressedTexture = FALSE;  
            break;

        case MAKEFOURCC('D','X','T','1'):
            nCompAlphaBits      = 1;
            bPremultipliedAlpha = FALSE;
            bCompressedTexture  = TRUE;
            break;

        case MAKEFOURCC('D','X','T','2'):
            nCompAlphaBits      = 4;
            bPremultipliedAlpha = TRUE;
            bCompressedTexture  = TRUE;
            break;

        case MAKEFOURCC('D','X','T','3'):
            nCompAlphaBits      = 4;
            bPremultipliedAlpha = FALSE;
            bCompressedTexture  = TRUE;
            break;

        case MAKEFOURCC('D','X','T','4'):
            nCompAlphaBits      = 8;
            bPremultipliedAlpha = TRUE;
            bCompressedTexture  = TRUE;
            break;

        case MAKEFOURCC('D','X','T','5'):
            nCompAlphaBits      = 8;
            bPremultipliedAlpha = FALSE;
            bCompressedTexture  = TRUE;
            break;
    }

    if( FALSE == bCompressedTexture )
    {
        // If the texture isn't compressed then look for an exact pixel format
        // match, otherwise fail since this sample doesn't implement any manual
        // pixel format conversion algorithms.
        int nTextureABits = GetNumberOfBits( ddpfDDSTexture.dwRGBAlphaBitMask );
        int nTextureRBits = GetNumberOfBits( ddpfDDSTexture.dwRBitMask );
        int nTextureGBits = GetNumberOfBits( ddpfDDSTexture.dwGBitMask );
        int nTextureBBits = GetNumberOfBits( ddpfDDSTexture.dwBBitMask );

        for( DWORD i=0; i<m_dwNumPixelFormats; i++ )
        {
            DDPIXELFORMAT* pddpf = &m_PixelFormats[i];

            int nFormatABits = GetNumberOfBits( pddpf->dwRGBAlphaBitMask );
            int nFormatRBits = GetNumberOfBits( pddpf->dwRBitMask );
            int nFormatGBits = GetNumberOfBits( pddpf->dwGBitMask );
            int nFormatBBits = GetNumberOfBits( pddpf->dwBBitMask );

            if( pddpf->dwFourCC == 0 &&
                nFormatABits == nTextureABits &&
                nFormatRBits == nTextureRBits &&
                nFormatGBits == nTextureGBits &&
                nFormatBBits == nTextureBBits ) 
            {
                // This is an exact pixel format match, so it works
                (*pddpfBestMatch) = (*pddpf);
                return S_OK;
            }
        }

        // pBestPixelFormat will be NULL if no exact match found, and since
        // this is an uncompressed DDS texture format  the blt can not convert
        // between pixel formats. A manual conversion of the pixels could be
        // done, but this is not implemeneted in this sample
    }
    else
    {
        // Search for an exact pixel format match  if renderer supports
        // compressed textures 
        for( DWORD i=0; i<m_dwNumPixelFormats; i++ )
        {
            DDPIXELFORMAT* pddpf = &m_PixelFormats[i];

            if( pddpf->dwFourCC == ddpfDDSTexture.dwFourCC )
            {
                // Look no further, since this is the best possible match
                (*pddpfBestMatch) = (*pddpf);
                return S_OK;
            }
        }

        // A good match was not found then keep looking. Search for exact or
        // highest alpha bit rate match and also make sure the texture isn't
        // blitted to/from premultipled alpha and non-premultipled alpha.
        nHighestFound = -1;

        for( i=0; i<m_dwNumPixelFormats; i++ )
        {
            DDPIXELFORMAT* pddpf = &m_PixelFormats[i];
            int  nFormatABits   = GetNumberOfBits( pddpf->dwRGBAlphaBitMask );
            int  nFormatRBits   = GetNumberOfBits( pddpf->dwRBitMask );
            int  nFormatGBits   = GetNumberOfBits( pddpf->dwGBitMask );
            int  nFormatBBits   = GetNumberOfBits( pddpf->dwBBitMask );
            BOOL bFormatPMAlpha = pddpf->dwFlags & DDPF_ALPHAPREMULT;
            
            if( ( bFormatPMAlpha == bPremultipliedAlpha ) &&
                ( nFormatABits == nCompAlphaBits ) && ( nFormatRBits >= 4 ) &&
                ( nFormatGBits >= 4 ) && ( nFormatBBits >= 4 ) ) 
            {
                // Look no further, this is the next best possible match
                (*pddpfBestMatch) = (*pddpf);
                return S_OK;
            }

            if( ( bFormatPMAlpha == bPremultipliedAlpha ) &&
                ( nFormatABits > nHighestFound ) && ( nFormatRBits >= 4 ) &&
                ( nFormatGBits >= 4 && nFormatBBits >= 4 ) ) 
            {
                // Record, but keep looking for a better match
                nHighestFound     = nFormatABits;
                (*pddpfBestMatch) = (*pddpf);
            }
        
            // If no match was found then either no texture pixel formats are
            // supported by the renderer or this in an uncompressed pixel
            // format and an exact pixel format match was not found
            if( i >= m_dwNumPixelFormats )
                return E_FAIL;
        }
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: BltToTextureSurface()
// Desc: Creates an surface in a format that the renderer supports, and copies
//       from the loaded DDS to the texture surface.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::BltToTextureSurface( DDSURFACEDESC2 ddsd, 
                              DDPIXELFORMAT ddpf, LPDIRECTDRAWSURFACE7 pddsDXT, 
                              LPDIRECTDRAWSURFACE7* ppddsNewSurface )
{
    LPDIRECTDRAWSURFACE7 pddsNew;
    HRESULT hr;

    // Set surface caps for the new surface
    ddsd.ddpfPixelFormat = ddpf;

    // If this is a hardware device, clear the DDSCAPS_SYSTEMMEMORY
    // flag and turn on automatic texture management
    if( m_pDeviceInfo->bHardware )
    {
        ddsd.ddsCaps.dwCaps2 |= DDSCAPS2_TEXTUREMANAGE;
        ddsd.ddsCaps.dwCaps &= ~DDSCAPS_SYSTEMMEMORY;
    }

    // Create a surface based on the enumerated texture formats
    if( FAILED( hr = m_pDD->CreateSurface( &ddsd, &pddsNew, NULL ) ) )
        return hr;
    *ppddsNewSurface = pddsNew;

    // Transfer image to texture surface, including mips (if any)
    while( TRUE )
    {
        if( FAILED( hr = pddsNew->Blt( NULL, pddsDXT, NULL, DDBLT_WAIT, NULL ) ) )
            return hr;

        // Get next surface in DXT's mipmap chain
        LPDIRECTDRAWSURFACE7 pddsNext;
        DDSCAPS2 ddsCaps = { DDSCAPS_TEXTURE|DDSCAPS_MIPMAP|DDSCAPS_COMPLEX, 0, 0, 0 };

        if( FAILED( pddsDXT->GetAttachedSurface( &ddsCaps, &pddsNext ) ) )
        {
            // Failure here means were done with the mipmap chain
            return S_OK;
        }
        pddsDXT = pddsNext;  

        // Get next surface in the new surface's mipmap chain
        if( FAILED( hr = pddsNew->GetAttachedSurface( &ddsCaps, &pddsNext ) ) )
            return hr;
        pddsNew = pddsNext;
    }
}




//-----------------------------------------------------------------------------
// Name: LoadTexture()
// Desc: Creates the device-dependant surface and loads a DDS texture 
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::LoadTexture( CHAR* strTextureName, 
                                        LPDIRECTDRAWSURFACE7* ppddsTexture )
{
    HRESULT              hr;
    LPDIRECTDRAWSURFACE7 pDDSLoad = NULL;
    DDSURFACEDESC2       ddsd;
    DDPIXELFORMAT        ddpfBestMatch;

    // Create a surface based on the DDS file
    // this surface may or may not be compressed
    if( FAILED( hr = ReadDDSTexture( strTextureName,  &ddsd, &pDDSLoad ) ) )
    {
        SAFE_RELEASE( pDDSLoad );
        return hr;
    }

    // Enumerate all pixel formats, then choose the best match
    // based on the read-in DDS pixel format
    if( FAILED( hr = FindBestPixelFormatMatch( ddsd.ddpfPixelFormat, 
                                               &ddpfBestMatch ) ) )
    {
        SAFE_RELEASE( pDDSLoad );
        return hr;
    }

    // Create string descriptor of the best match found, to be displayed in the
    // render window.
    PixelFormatToString( m_strDiskPixelFormat, &ddsd.ddpfPixelFormat );
    PixelFormatToString( m_strRendererPixelFormat, &ddpfBestMatch );

    // Use D3DBLEND_ONE if DDPF_ALPHAPREMULT is on, and D3DBLEND_SRCALPHA if
    // DDPF_ALPHAPREMULT is off
    if( ddpfBestMatch.dwFlags & DDPF_ALPHAPREMULT )
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND, D3DBLEND_ONE );
    else
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA );

    LPDIRECTDRAWSURFACE7 pDDSNew = NULL;

    // Blt the loaded surface to an texture surface using the best
    // pixel format match
    if( FAILED( hr = BltToTextureSurface( ddsd, ddpfBestMatch,
                                          pDDSLoad, &pDDSNew ) ) )
    {
        SAFE_RELEASE( pDDSNew );
        SAFE_RELEASE( pDDSLoad );
        return hr;
    }

    // Get the texture interface from the new texture surface
    (*ppddsTexture) = pDDSNew;

    // Done with loaded surface, so release it
    SAFE_RELEASE( pDDSLoad );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: MsgProc()
// Desc: App-specific message proc
//-----------------------------------------------------------------------------
LRESULT CMyD3DApplication::MsgProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                                    LPARAM lParam )
{
    if( WM_COMMAND == uMsg )
    {
        switch( LOWORD(wParam) )
        {
            case IDM_CHANGEDXT:
                // Recieved command to cycle the texture
                m_dwDXT = (m_dwDXT+1) % 4; // Increment and wrap counter 
                ChangeTexture();
                return 0;
        }
    }

    return CD3DApplication::MsgProc( hWnd, uMsg, wParam, lParam );
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
    D3DVECTOR n2( 0.0f, 1.0f, 0.0f ); // Bottom face
    D3DVECTOR n3( 0.0f,-1.0f, 0.0f ); // Top face
    D3DVECTOR n4( 1.0f, 0.0f, 0.0f ); // Left face
    D3DVECTOR n5(-1.0f, 0.0f, 0.0f ); // Right face

    // Front face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f,-1.0f), n0, 0.01f, 0.01f ); 
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f,-1.0f), n0, 0.99f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f,-1.0f), n0, 0.99f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f,-1.0f), n0, 0.01f, 0.99f ); 

    // Back face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f, 1.0f), n1, 0.99f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f, 1.0f), n1, 0.99f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f, 1.0f), n1, 0.01f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f, 1.0f), n1, 0.01f, 0.01f );

    // Top face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f, 1.0f), n2, 0.01f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f, 1.0f), n2, 0.99f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f,-1.0f), n2, 0.99f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f,-1.0f), n2, 0.01f, 0.99f );

    // Bottom face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f, 1.0f), n3, 0.01f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f,-1.0f), n3, 0.01f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f,-1.0f), n3, 0.99f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f, 1.0f), n3, 0.99f, 0.99f );

    // Right face
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f,-1.0f), n4, 0.01f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f, 1.0f), n4, 0.99f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f, 1.0f), n4, 0.99f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f,-1.0f), n4, 0.01f, 0.99f );

    // Left face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f,-1.0f), n5, 0.99f, 0.01f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f,-1.0f), n5, 0.99f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f, 1.0f), n5, 0.01f, 0.99f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f, 1.0f), n5, 0.01f, 0.01f );

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





