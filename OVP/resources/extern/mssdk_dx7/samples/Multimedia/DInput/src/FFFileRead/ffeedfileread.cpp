//-----------------------------------------------------------------------------
// File: FFeedFileRead.cpp
//
// Desc: DirectInput support to enumerate and play all effects in stored in a 
//       DirectInput effects file.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define INITGUID
#include <windows.h>
#include <commdlg.h>
#include <dinput.h>
#include "resource.h"




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }

struct EFFECTS_NODE
{
    LPDIRECTINPUTEFFECT pDIEffect;
    DWORD               dwPlayRepeatCount;
    EFFECTS_NODE*       pNext;
};




//-----------------------------------------------------------------------------
// Global variables for the DirectMusic sample 
//-----------------------------------------------------------------------------
LPDIRECTINPUT7        g_pDI       = NULL;         
LPDIRECTINPUTDEVICE7  g_pFFDevice = NULL;
EFFECTS_NODE          g_EffectsList;
static TCHAR s_strDirectInputMedia[] = "\\DInput\\Media";




//-----------------------------------------------------------------------------
// Local function-prototypes
//-----------------------------------------------------------------------------
HRESULT       InitDirectInput( HWND hDlg );
BOOL          GetDirectInputMediaPath( TCHAR strDirectInputMediaPath[MAX_PATH] );
BOOL CALLBACK EnumFFDevicesCallback( LPCDIDEVICEINSTANCE pDDI, LPVOID pvRef );
BOOL CALLBACK EnumAndCreateEffectsCallback( LPCDIFILEEFFECT pDIFileEffect, LPVOID pvRef );
VOID          EmptyEffectList();




//-----------------------------------------------------------------------------
// Name: InitDirectInput()
// Desc: Initialize the DirectInput variables.
//-----------------------------------------------------------------------------
HRESULT InitDirectInput( HWND hDlg )
{
    HRESULT hr;

    HINSTANCE hInst = (HINSTANCE) GetWindowLong( hDlg, GWL_HINSTANCE );

    // Create a DInput object
    if( FAILED( hr = DirectInputCreateEx( hInst, DIRECTINPUT_VERSION, 
                                          IID_IDirectInput7,
                                          (VOID**)&g_pDI, NULL ) ) )
        return hr;

    // Get the first enumerated force feedback device
    if (FAILED( hr = g_pDI->EnumDevices( 0, EnumFFDevicesCallback, 0, 
                                         DIEDFL_ATTACHEDONLY | 
                                         DIEDFL_FORCEFEEDBACK ) ) )
        return hr;
    
    if( g_pFFDevice == NULL )
    {
        MessageBox( hDlg, "No force feedback device found.  "
                    "The sample will now exit.", "DirectInput Sample", 
                    MB_ICONERROR | MB_OK );
        EndDialog( hDlg, 0 );
        return S_OK;
    }

    // Set the data format
    if( FAILED( hr = g_pFFDevice->SetDataFormat( &c_dfDIJoystick ) ) )
        return hr;

    // Set the coop level
    if( FAILED( hr = g_pFFDevice->SetCooperativeLevel( hDlg, DISCL_EXCLUSIVE | 
                                                             DISCL_BACKGROUND ) ) )
        return hr;

    // Disable auto-centering spring
    DIPROPDWORD dipdw;
    dipdw.diph.dwSize       = sizeof(DIPROPDWORD);
    dipdw.diph.dwHeaderSize = sizeof(DIPROPHEADER);
    dipdw.diph.dwObj        = 0;
    dipdw.diph.dwHow        = DIPH_DEVICE;
    dipdw.dwData            = FALSE;

    if( FAILED( hr = g_pFFDevice->SetProperty( DIPROP_AUTOCENTER, &dipdw.diph ) ) )
        return hr;

    // Acquire the device
    if( FAILED( hr = g_pFFDevice->Acquire() ) )
        return hr;

    // Setup the g_EffectsList circular linked list
    ZeroMemory( &g_EffectsList, sizeof( EFFECTS_NODE ) );
    g_EffectsList.pNext = &g_EffectsList;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: EnumFFDevicesCallback()
// Desc: Get the first enumerated force feedback device
//-----------------------------------------------------------------------------
BOOL CALLBACK EnumFFDevicesCallback( LPCDIDEVICEINSTANCE pDDI, LPVOID pvRef )
{
    if( FAILED( g_pDI->CreateDeviceEx( pDDI->guidInstance, IID_IDirectInputDevice7, 
                                      (LPVOID*) &g_pFFDevice, NULL ) ) )

        return DIENUM_CONTINUE; // If failed, try again

    // Stop when a device was successfully found
    return DIENUM_STOP;
}




//-----------------------------------------------------------------------------
// Name: FreeDirectInput()
// Desc: Initialize the DirectInput variables.
//-----------------------------------------------------------------------------
HRESULT FreeDirectInput()
{
    // Release any DirectInputEffect objects.
    if( g_pFFDevice ) 
    {
        EmptyEffectList();
        g_pFFDevice->Unacquire();
        SAFE_RELEASE( g_pFFDevice );
    }

    // Release any DirectInput objects.
    SAFE_RELEASE( g_pDI );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: EmptyEffectList()
// Desc: Goes through the circular linked list and releases the effects, 
//       and deletes the nodes
//-----------------------------------------------------------------------------
VOID EmptyEffectList()
{
    EFFECTS_NODE* pEffectNode = g_EffectsList.pNext;
    EFFECTS_NODE* pEffectDelete;

    while ( pEffectNode != &g_EffectsList )
    {
        pEffectDelete = pEffectNode;       
        pEffectNode = pEffectNode->pNext;

        SAFE_RELEASE( pEffectDelete->pDIEffect );
        SAFE_DELETE( pEffectDelete );
    }

    g_EffectsList.pNext = &g_EffectsList;
}




//-----------------------------------------------------------------------------
// Name: OnReadFile()
// Desc: Reads a file contain a collection of DirectInput force feedback 
//       effects.  It creates each of effect read in and stores it 
//       in the linked list, g_EffectsList.
//-----------------------------------------------------------------------------
HRESULT OnReadFile( HWND hDlg )
{
    HRESULT hr;

    static TCHAR strFileName[MAX_PATH] = TEXT("");
    static TCHAR strPath[MAX_PATH] = TEXT("");

    // Setup the OPENFILENAME structure
    OPENFILENAME ofn = { sizeof(OPENFILENAME), hDlg, NULL,
                         TEXT("FEdit Files\0*.ffe\0All Files\0*.*\0\0"), NULL,
                         0, 1, strFileName, MAX_PATH, NULL, 0, strPath,
                         TEXT("Open FEdit File"),
                         OFN_FILEMUSTEXIST|OFN_HIDEREADONLY, 0, 0,
                         TEXT(".ffe"), 0, NULL, NULL };

    // Get the default media path (something like C:\MSSDK\SAMPLES\MULTIMEDIA\DINPUT\MEDIA)
    if( '\0' == strPath[0] )
    {
        GetDirectInputMediaPath( strPath );
    }

    // Display the OpenFileName dialog. Then, try to load the specified file
    if( FALSE == GetOpenFileName( &ofn ) )
        return S_OK;

    EmptyEffectList();

    // Enumerate the effects in the file selected, and create them in the callback
    if( FAILED( hr = g_pFFDevice->EnumEffectsInFile( strFileName, 
                                                     EnumAndCreateEffectsCallback, 
                                                     NULL, 0 ) ) )
        return hr;

    // Remember the path for next time
    strcpy( strPath, strFileName );
    char* strLastSlash = strrchr( strPath, '\\' );
    strLastSlash[0] = '\0';

    // If list of effects is empty, then we haven't been able to create any effects
    if( g_EffectsList.pNext == &g_EffectsList )
    {
        // Pop up a box informing the user
        MessageBox( hDlg, "Unable to create any effects.",
                    "DirectInput Sample", MB_ICONEXCLAMATION | MB_OK );
        EnableWindow( GetDlgItem( hDlg, IDC_PLAY_EFFECTS ), FALSE );
    }
    else
    {
        // We have effects so enable the 'play effects' button
        EnableWindow( GetDlgItem( hDlg, IDC_PLAY_EFFECTS ), TRUE );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: EnumAndCreateEffectsCallback()
// Desc: Create the effects as they are enumerated and add them to the 
//       linked list, g_EffectsList
//-----------------------------------------------------------------------------
BOOL CALLBACK EnumAndCreateEffectsCallback( LPCDIFILEEFFECT pDIFileEffect, LPVOID pvRef )
{   
    HRESULT hr;
    LPDIRECTINPUTEFFECT pDIEffect = NULL;

    // Create the file effect
    if( FAILED( hr = g_pFFDevice->CreateEffect( pDIFileEffect->GuidEffect, 
                                                pDIFileEffect->lpDiEffect, 
                                                &pDIEffect, NULL ) ) )
    {
        OutputDebugString( "Could not create force feedback effect on this device.\n" );
        return DIENUM_CONTINUE;
    }

    // Create a new effect node
    EFFECTS_NODE* pEffectNode = new EFFECTS_NODE;
    if( NULL == pEffectNode )
    {
        return DIENUM_STOP;
    }

    // Fill the pEffectNode up
    ZeroMemory( pEffectNode, sizeof( EFFECTS_NODE ) );
    pEffectNode->pDIEffect         = pDIEffect;
    pEffectNode->dwPlayRepeatCount = 1;

    // Add pEffectNode to the circular linked list, g_EffectsList
    pEffectNode->pNext  = g_EffectsList.pNext;
    g_EffectsList.pNext = pEffectNode;

    return DIENUM_CONTINUE;
}




//-----------------------------------------------------------------------------
// Name: OnPlayEffects()
// Desc: Plays all of the effects enumerated in the file 
//-----------------------------------------------------------------------------
HRESULT OnPlayEffects( HWND hDlg )
{
    EFFECTS_NODE*       pEffectNode = g_EffectsList.pNext;
    LPDIRECTINPUTEFFECT pDIEffect   = NULL;
    HRESULT             hr;

    // Stop all previous forces
    if( FAILED( hr = g_pFFDevice->SendForceFeedbackCommand( DISFFC_STOPALL ) ) )
        return hr;

    while ( pEffectNode != &g_EffectsList )
    {
        // Play all of the effects enumerated in the file 
        pDIEffect = pEffectNode->pDIEffect;

        if ( NULL != pDIEffect )
        {
            if( FAILED( hr = pDIEffect->Start( pEffectNode->dwPlayRepeatCount, 0 ) ) )
                return hr;
        }

        pEffectNode = pEffectNode->pNext;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: GetDirectInputMediaPath()
// Desc: Finds and returns the DirectInput SDK media path from the registry
//-----------------------------------------------------------------------------
BOOL GetDirectInputMediaPath( TCHAR strDirectInputMediaPath[MAX_PATH] )
{
    HKEY  hKeyDirectX = NULL;
    TCHAR strPath[MAX_PATH];
    DWORD cbPath;

    // Get DirectX SDK search path from the registry
    if( RegOpenKeyEx( HKEY_LOCAL_MACHINE, "Software\\Microsoft\\DirectX",
                      0, KEY_READ, &hKeyDirectX ) )
    {
        return FALSE;
    }

    cbPath = sizeof(strPath);
    if( RegQueryValueEx( hKeyDirectX, "DXSDK Samples Path", NULL, NULL,
                         (BYTE*)strPath, &cbPath ) == ERROR_SUCCESS )
    {
        if( cbPath + sizeof(s_strDirectInputMedia) > MAX_PATH )
        {
            RegCloseKey( hKeyDirectX );
            return FALSE;
        }

        strcat( strPath, s_strDirectInputMedia );
        strcpy( strDirectInputMediaPath, strPath );
    }

    RegCloseKey( hKeyDirectX );

    return TRUE;
}



