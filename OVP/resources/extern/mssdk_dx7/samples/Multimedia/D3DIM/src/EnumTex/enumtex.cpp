//-----------------------------------------------------------------------------
// Name: EnumTex.cpp
//
// Desc: Simple command-line app to show how to enumerate texture formats.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define  INITGUID
#include <d3d.h>
#include <string.h>
#include <stdio.h>




//-----------------------------------------------------------------------------
// Global variables for storing enumerated texture formats
//-----------------------------------------------------------------------------
#define ENUMTEXERR_BADALPHABITCOUNT   0x80000001
#define ENUMTEXERR_BADALPHAPIXELSFLAG 0x80000002
#define ENUMTEXERR_BADBITCOUNT        0x80000003
#define ENUMTEXERR_BADALPHAFLAGS      0x80000004
#define ENUMTEXERR_BADFOURCCBITCOUNT  0x80000005
#define ENUMTEXERR_BADFOURCCFLAGS     0x80000006
#define ENUMTEXERR_UNKNOWNFORMAT      0x80000007

#define MAX_NUM_FORMATS 64
DDPIXELFORMAT g_ddpfTextureFormatList[MAX_NUM_FORMATS];
DWORD         g_dwNumTextureFormats = 0;




//-----------------------------------------------------------------------------
// Name: TextureEnumerationCallback()
// Desc: Callback used by the texture enumeration process. Builds a global
//       list of texture formats.
//-----------------------------------------------------------------------------
HRESULT CALLBACK TextureEnumerationCallback( DDPIXELFORMAT* pddpf, VOID* )
{
    g_ddpfTextureFormatList[g_dwNumTextureFormats++] = (*pddpf);

    return DDENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: BuildTextureFormatList()
// Desc: Creates a D3DDevice, and uses it to enumerate all available texture
//       formats for that device.
//-----------------------------------------------------------------------------
HRESULT BuildTextureFormatList( GUID deviceGUID )
{
    // Local variables for DirectX objects
    HRESULT              hr;
    LPDIRECTDRAW7        pDD         = NULL;
    LPDIRECT3D7          pD3D        = NULL;
    LPDIRECTDRAWSURFACE7 pddsSurface = NULL;
    LPDIRECT3DDEVICE7    pd3dDevice  = NULL;
    DDSURFACEDESC2       ddsd;

    // Structure for creating a surface
    ZeroMemory( &ddsd, sizeof(ddsd) );
    ddsd.dwSize         = sizeof( ddsd );
    ddsd.dwFlags        = DDSD_CAPS;
    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE | DDSCAPS_3DDEVICE;

    // Create the DirectDraw object
    hr = DirectDrawCreateEx( NULL, (VOID**)&pDD, IID_IDirectDraw7, NULL );
    if( FAILED( hr ) )
    {
        MessageBox( NULL, "Could not create DirectDraw!\n",
                    "EnumTex", MB_ICONERROR );
        return hr;
    }

    // Set the cooperative level
    hr = pDD->SetCooperativeLevel( NULL, DDSCL_NORMAL );
    if( FAILED( hr ) )
    {
        MessageBox( NULL, "Could not set the DirectDraw cooperative level!\n",
                    "EnumTex", MB_ICONERROR );
        return hr;
    }

    // Create the Direct3D object
    hr = pDD->QueryInterface( IID_IDirect3D7, (VOID**)&pD3D );
    if( FAILED( hr ) )
    {
        pDD->Release();
        MessageBox( NULL, "Could not create a Direct3D interface!\n",
                    "EnumTex", MB_ICONERROR );
        return hr;
    }

    // Create a DirectDraw surface
    hr = pDD->CreateSurface( &ddsd, &pddsSurface, NULL );
    if( FAILED( hr ) )
    {
        pD3D->Release();
        pDD->Release();
        MessageBox( NULL, "Could not create a DirectDraw surface!\n",
                    "EnumTex", MB_ICONERROR );
        return hr;
    }

    // Create a Direct3D device
    hr = pD3D->CreateDevice( deviceGUID, pddsSurface, &pd3dDevice );
    if( FAILED( hr ) )
    {
        pddsSurface->Release();
        pD3D->Release();
        pDD->Release();
        MessageBox( NULL, "Could not create a Direct3DDevice!\n"
                    "Try changing your color depth to 16-bit.\n",
                    "EnumTex", MB_ICONERROR );
        return hr;
    }

    // Enumerate the device's texture formats
    hr = pd3dDevice->EnumTextureFormats( TextureEnumerationCallback, NULL );
    if( FAILED( hr ) )
    {
        pd3dDevice->Release();
        pddsSurface->Release();
        pD3D->Release();
        pDD->Release();
        MessageBox( NULL, "Could not enumerate texture formats!\n",
                    "EnumTex", MB_ICONERROR );
         return hr;
    }

    // Release objects and return successfully
    pd3dDevice->Release();
    pddsSurface->Release();
    pD3D->Release();
    pDD->Release();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: WndProc()
// Desc: This is the basic Windows-programming function that processes
//       Windows messages. We need to handle window movement, painting,
//       and destruction.
//-----------------------------------------------------------------------------
LRESULT CALLBACK WndProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam )
{
    return DefWindowProc( hWnd, uMsg, wParam, lParam );
}
            



//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entrance point for console-based aps
//-----------------------------------------------------------------------------
INT WINAPI WinMain( HINSTANCE hInst, HINSTANCE, LPSTR strCmdLine, INT )
{
    CHAR strOutput[4096] = ""; // Output text buffer
    CHAR strBuffer[80];        // Temp text buffer
    GUID deviceGUID;           // Device GUID used to enumerate textures

    // Select a device from commandline params
    if( !strcmp( strCmdLine, "-HAL" ) || !strcmp( strCmdLine, "-hal" ) )
        deviceGUID = IID_IDirect3DHALDevice;
    else if( !strcmp( strCmdLine, "-RGB" ) || !strcmp( strCmdLine, "-rgb" ) )
        deviceGUID = IID_IDirect3DRGBDevice;
    else if( !strcmp( strCmdLine, "-REF" ) || !strcmp( strCmdLine, "-ref" ) )
        deviceGUID = IID_IDirect3DRefDevice;
    else
    {
        deviceGUID = IID_IDirect3DRGBDevice;
        strCmdLine = "-RGB";
    }

    // Use DirectDraw/Direct3D to build a list of a device's supported texture
    // formats.
    if( FAILED( BuildTextureFormatList( deviceGUID ) ) )
        return 0;

    // Loop through all supported formats, outputting info about each one.
    for( DWORD i=0; i<g_dwNumTextureFormats; i++ )
    {
        DWORD dwError         = 0;
        DWORD dwFlags         = g_ddpfTextureFormatList[i].dwFlags;
        DWORD dwFourCC        = g_ddpfTextureFormatList[i].dwFourCC;
        DWORD dwTotalBitCount = g_ddpfTextureFormatList[i].dwRGBBitCount;
        DWORD dwAlphaBitCount = 0;
        DWORD dwRedBitCount   = 0;
        DWORD dwGreenBitCount = 0;
        DWORD dwBlueBitCount  = 0;
        DWORD mask;

        // Count number of bits for each color component
        for( mask = g_ddpfTextureFormatList[i].dwRGBAlphaBitMask; mask; mask>>=1 )
            dwAlphaBitCount += ( mask & 0x1 );
        for( mask = g_ddpfTextureFormatList[i].dwRBitMask; mask; mask>>=1 )
            dwRedBitCount += ( mask & 0x1 );
        for( mask = g_ddpfTextureFormatList[i].dwGBitMask; mask; mask>>=1 )
            dwGreenBitCount += ( mask & 0x1 );
        for( mask = g_ddpfTextureFormatList[i].dwBBitMask; mask; mask>>=1 )
            dwBlueBitCount += ( mask & 0x1 );

        // Check for invalid formats
        if( ( dwFlags & DDPF_ALPHAPIXELS ) && ( dwAlphaBitCount == 0 ) )
            dwError = ENUMTEXERR_BADALPHABITCOUNT;
        if( !( dwFlags & DDPF_ALPHAPIXELS ) && ( dwAlphaBitCount != 0 ) )
            dwError = ENUMTEXERR_BADALPHAPIXELSFLAG;
        if( !(dwFlags & DDPF_FOURCC ) && dwTotalBitCount == 0 )
            dwError = ENUMTEXERR_BADBITCOUNT;

        // Output info about the current texture format.
        sprintf( strBuffer, "Format %2d: ", i+1 );
        strcat( strOutput, strBuffer );

        if( dwFlags & DDPF_ALPHA )
        {
            // Alpha-only formats
            if( ( dwFlags & DDPF_ALPHA ) && ( dwFlags != DDPF_ALPHA ) )
                dwError = ENUMTEXERR_BADALPHAFLAGS;

            sprintf( strBuffer, "%d-bit, alpha-only format.\n", dwTotalBitCount );
            strcat( strOutput, strBuffer );

            if( !dwError )
            {
                // Note: If you want this format, accept it here
            }
        }
        else if( dwFlags & DDPF_BUMPLUMINANCE )
        {
            // Bumpmap formats
            sprintf( strBuffer, "%d-bit, bump luminance format.\n", dwTotalBitCount );
            strcat( strOutput, strBuffer );

            if( !dwError )
            {
                // Note: If you want this format, accept it here
            }
        }
        else if( dwFlags & DDPF_BUMPDUDV )
        {
            // More bumpmap formats
            sprintf( strBuffer, "%d-bit, bump format.\n", dwTotalBitCount );
            strcat( strOutput, strBuffer );

            if( !dwError )
            {
                // Note: If you want this format, accept it here
            }
        }
        else if( dwFlags & DDPF_LUMINANCE )
        {
            // Luminance formats. Check whether format also has alpha.
            if( dwAlphaBitCount )
                sprintf( strBuffer, "%d-bit luminance, %d-bit alpha format.\n",
                        dwTotalBitCount-dwAlphaBitCount, dwAlphaBitCount );
            else
                sprintf( strBuffer, "%d-bit luminance format.\n", dwTotalBitCount );
            strcat( strOutput, strBuffer );

            if( !dwError )
            {
                // Note: If you want this format, accept it here
            }
        }
        else if( dwFlags & DDPF_FOURCC )
        {
            // FourCC formats. Check for a valid format.
            if( dwTotalBitCount!=0 || dwRedBitCount!=0 || dwGreenBitCount!=0 ||
                dwBlueBitCount!=0 || dwAlphaBitCount!= 0 )
                dwError = ENUMTEXERR_BADFOURCCBITCOUNT;
            if( ( dwFlags & DDPF_FOURCC ) && ( dwFlags != DDPF_FOURCC ) )
                dwError = ENUMTEXERR_BADFOURCCFLAGS;
                            
            // Also check whether it is a DXT format.
            if( ( dwFourCC == FOURCC_DXT1 ) || ( dwFourCC == FOURCC_DXT2 ) ||
                ( dwFourCC == FOURCC_DXT3 ) || ( dwFourCC == FOURCC_DXT4 ) ||
                ( dwFourCC == FOURCC_DXT5 ) )
                printf( "DXTn " );
            
            sprintf( strBuffer, "FourCC (%c%c%c%c), format.\n", dwFourCC&0xff,
                    ((dwFourCC>>8)&0xff), ((dwFourCC>>16)&0xff),
                    ((dwFourCC>>24)&0xff) );
            strcat( strOutput, strBuffer );

            if( !dwError )
            {
                // Note: If you want this format, accept it here
            }
        }
        else if( dwFlags & (DDPF_PALETTEINDEXED1|DDPF_PALETTEINDEXED2|
                            DDPF_PALETTEINDEXED4|DDPF_PALETTEINDEXED8) )
        {
            // Palettized formats
            if( dwAlphaBitCount )
                sprintf( strBuffer, "%d-bit palettized format with %d-bit alpha.\n",
                        dwTotalBitCount - dwAlphaBitCount, dwAlphaBitCount );
            else
                sprintf( strBuffer, "%d-bit palettized format.\n", dwTotalBitCount );
            strcat( strOutput, strBuffer );

            if( !dwError )
            {
                // Note: If you want this format, accept it here
            }
        }
        else if( dwFlags & DDPF_RGB )
        {
            // RGB formats
            if( dwAlphaBitCount )
                sprintf( strBuffer, "%d%d%d%d ARGB (%d-bit) format.\n", dwAlphaBitCount,
                        dwRedBitCount, dwGreenBitCount, dwBlueBitCount,
                        dwTotalBitCount );
            else
                sprintf( strBuffer, "%d%d%d RGB (%d-bit) format.\n", dwRedBitCount,
                        dwGreenBitCount, dwBlueBitCount, dwTotalBitCount );
            strcat( strOutput, strBuffer );

            if( !dwError )
            {
                // Note: If you want this format, accept it here
            }
        }
        else
        {
            // Unknown formats
            dwError = ENUMTEXERR_UNKNOWNFORMAT;

            sprintf( strBuffer, "%d-bit unknown format.\n", dwTotalBitCount );
            strcat( strOutput, strBuffer );

            // Note: Never accept an unknown format
        }

        // Print out error message
        if( dwError == ENUMTEXERR_BADALPHABITCOUNT )
            strcat( strOutput, "> Error: DDPF_ALPHAPIXELS flag set with zero alpha mask!\n" );
        if( dwError == ENUMTEXERR_BADALPHAPIXELSFLAG )
            strcat( strOutput, "> Error: Alpha bit mask set without DDPF_ALPHAPIXELS flag set!\n" );
        if( dwError == ENUMTEXERR_BADBITCOUNT )
            strcat( strOutput, "> Error: Format has zero bit count!\n" );
        if( dwError == ENUMTEXERR_BADALPHAFLAGS )
            strcat( strOutput, "> Error: Other flags illegally combined with DDPF_ALPHA!\n" );
        if( dwError == ENUMTEXERR_BADFOURCCBITCOUNT )
            strcat( strOutput, "> Error: FourCC code has non-zero bitcount.\n" );
        if( dwError == ENUMTEXERR_BADFOURCCFLAGS )
            strcat( strOutput, "> Error: Other flags illegally combined with DDPF_FOURCC!\n" );
        if( dwError == ENUMTEXERR_UNKNOWNFORMAT )
            strcat( strOutput, "> Warning: Unkown format!\n" );
    }

    // Display the output
    sprintf( strBuffer, "EnumTex %s", strCmdLine );
    MessageBox( NULL, strOutput, strBuffer, MB_OK );

    // Exit program
    return 0;
}



