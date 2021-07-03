//-----------------------------------------------------------------------------
// File: dxver.cpp
//
// Desc: Windows code for GetDXVersion() sample
//
//       This code calls GetDXVersion and displays the results
//
//
// Copyright (c) 1998 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include <windows.h>




//-----------------------------------------------------------------------------
// External function-prototypes
//-----------------------------------------------------------------------------
extern void GetDXVersion( DWORD* pdwDXVersion, DWORD* pdwDXPlatform );




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point to the program. Initializes everything, and pops
//       up a message box with the results of the GetDXVersion call
//-----------------------------------------------------------------------------
int PASCAL WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    LPSTR strCmdLine, int nCmdShow )
{
    CHAR  strBuff[256];
    DWORD dwDXVersion;
    DWORD dwDXPlatform;

    GetDXVersion( &dwDXVersion, &dwDXPlatform );

    switch( dwDXPlatform )
    {
        case VER_PLATFORM_WIN32_WINDOWS:
            strcpy( strBuff, "OS:\tWindows 9x\n" );
            break;
        case VER_PLATFORM_WIN32_NT:
            strcpy( strBuff, "OS:\tWindows NT\n" );
            break;
        default:
            strcpy( strBuff, "Error!\n" );
            break;
    }

    switch( dwDXVersion )
    {
        case 0x000:
            strcat( strBuff, "Dx:\tNo DirectX installed" );
            break;
        case 0x100:
            strcat( strBuff, "Dx:\tDirectX 1" );
            break;
        case 0x200:
            strcat( strBuff, "Dx:\tDirectX 2" );
            break;
        case 0x300:
            strcat( strBuff, "Dx:\tDirectX 3" );
            break;
        case 0x500:
            strcat( strBuff, "Dx:\tDirectX 5" );
            break;
        case 0x600:
            strcat( strBuff, "Dx:\tDirectX 6" );
            break;
        case 0x601:
            strcat( strBuff, "Dx:\tDirectX 6.1" );
            break;
        case 0x700:
            strcat( strBuff, "Dx:\tDirectX 7 or better" );
            break;
        default:
            strcat( strBuff,"Unknown version of DirectX installed." );
            break;
    }

    MessageBox( NULL, strBuff, "DirectX Version:",
                MB_OK | MB_ICONINFORMATION );
    
    return 0;
}



