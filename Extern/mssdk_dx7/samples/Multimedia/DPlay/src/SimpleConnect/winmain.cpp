//----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: The main game file for the SimpleConnect sample.  It connects 
//       players together with two dialog boxes to prompt users on the 
//       connection settings to join or create a session. After the user 
//       connects to a sesssion, the sample displays a multiplayer stage. 
// 
//       After a new game has started the sample begins a very simplistic 
//       game called "The Greeting Game".  When two or more players are connected
//       to the game, the players have the option of sending a single simple 
//       DirectPlay message to all of the other players.
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define IDIRECTPLAY2_OR_GREATER
#include <windows.h>
#include <dplobby.h>
#include <dplay.h>
#include <stdio.h>
#include "resource.h"




//-----------------------------------------------------------------------------
// Defines, and constants
//-----------------------------------------------------------------------------
#define SAFE_DELETE(p)       { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_DELETE_ARRAY(p) { if(p) { delete[] (p);   (p)=NULL; } }
#define SAFE_RELEASE(p)      { if(p) { (p)->Release(); (p)=NULL; } }

#define DPLAY_SAMPLE_KEY     TEXT("Software\\Microsoft\\DirectX DirectPlay Samples")

#define MAX_PLAYER_NAME     14
#define MAX_SESSION_NAME    256

#define EXITCODE_FORWARD     1  // Dialog success, so go forward
#define EXITCODE_BACKUP      2  // Dialog canceled, show previous dialog
#define EXITCODE_QUIT        3  // Dialog quit, close app
#define EXITCODE_ERROR       4  // Dialog error, terminate app

// This GUID allows DirectPlay to find other instances of the same game on
// the network.  So it must be unique for every game, and the same for 
// every instance of that game.  // {C6334FC0-3B80-4fed-89F1-A4DEFEB6DB20}
GUID g_AppGUID = { 0xc6334fc0, 0x3b80, 0x4fed, { 0x89, 0xf1, 0xa4, 0xde, 0xfe, 0xb6, 0xdb, 0x20 } };




//-----------------------------------------------------------------------------
// Global variables
//-----------------------------------------------------------------------------
LPDIRECTPLAY4A     g_pDP                                     = NULL;
LPDPLCONNECTION    g_pDPLConnection                          = NULL;  
LPDIRECTPLAYLOBBY3 g_pDPLobby                                = NULL;  
HANDLE             g_hDPMessageEvent                         = NULL;
BOOL               g_bUseProtocol;
HKEY               g_hDPlaySampleRegKey                      = NULL;
TCHAR              g_strLocalPlayerName[MAX_PLAYER_NAME];
TCHAR              g_strSessionName[MAX_SESSION_NAME];
TCHAR              g_strPreferredProvider[MAX_SESSION_NAME];
VOID*              g_pvDPMsgBuffer                           = NULL;
DWORD              g_dwDPMsgBufferSize                       = 0;
BOOL               g_bHostPlayer;
DPID               g_LocalPlayerDPID; 
DWORD              g_dwNumberOfActivePlayers;
TCHAR              g_strAppName[256] = "SimpleConnect Greeting Game";




//-----------------------------------------------------------------------------
// App specific DirectPlay messages and structures 
//-----------------------------------------------------------------------------
#define GAMEMSG_WAVE        1

struct GAMEMSG_GENERIC
{
    DWORD dwType;
};




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
extern int     DPConnect_StartDirectPlayConnect( HINSTANCE hInst, BOOL bBackTrack = FALSE );
extern HRESULT DPConnect_CheckForLobbyLaunch( BOOL* pbLaunchedByLobby );

HRESULT        DoConnectAndGame( HINSTANCE hInst );
int            DoGreetingGame( HINSTANCE hInst );
BOOL CALLBACK  GreetingDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam );
HRESULT        OnInitDialog( HWND hDlg );
VOID           DisplayNumberPlayersInGame( HWND hDlg );
HRESULT        ProcessDirectPlayMessages( HWND hDlg );
HRESULT        HandleAppMessages( HWND, GAMEMSG_GENERIC*, DWORD, DPID, DPID );
HRESULT        HandleSystemMessages( HWND, DPMSG_GENERIC*, DWORD, DPID, DPID );
HRESULT        WaveToAllPlayers();
HRESULT        DisplayPlayerWave( HWND hDlg, DPID idFrom );

HRESULT        ReadRegKey( HKEY hKey, TCHAR* strName, TCHAR* strValue, DWORD dwLength, TCHAR* strDefault );
HRESULT        WriteRegKey( HKEY hKey, TCHAR* strName, TCHAR* strValue );




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point for the application.  Since we use a simple dialog for 
//       user interaction we don't need to pump messages.
//-----------------------------------------------------------------------------
INT APIENTRY WinMain( HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR pCmdLine, 
                      INT nCmdShow )
{
    HRESULT hr;

    // Read information from registry
    RegCreateKeyEx( HKEY_CURRENT_USER, DPLAY_SAMPLE_KEY, 0, NULL,
                    REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL, 
                    &g_hDPlaySampleRegKey, NULL );

    ReadRegKey( g_hDPlaySampleRegKey, "Player Name", 
                g_strLocalPlayerName, MAX_PLAYER_NAME, "" );
    ReadRegKey( g_hDPlaySampleRegKey, "Session Name", 
                g_strSessionName, MAX_SESSION_NAME, "" );
    ReadRegKey( g_hDPlaySampleRegKey, "Preferred Provider", 
                g_strPreferredProvider, MAX_SESSION_NAME, "" );

    g_hDPMessageEvent = CreateEvent( NULL, FALSE, FALSE, NULL );

    if( FAILED( hr = CoInitialize( NULL ) ) )
        return FALSE;

    hr = DoConnectAndGame( hInst );
    if( SUCCEEDED( hr ) )
    {
        // Write information to the registry
        WriteRegKey( g_hDPlaySampleRegKey, "Player Name", g_strLocalPlayerName );
        WriteRegKey( g_hDPlaySampleRegKey, "Session Name", g_strSessionName );
        WriteRegKey( g_hDPlaySampleRegKey, "Preferred Provider", g_strPreferredProvider );
    }

    // Cleanup DirectPlay
    if( g_pDP )
    {
        g_pDP->DestroyPlayer( g_LocalPlayerDPID );
        g_pDP->Close();

        SAFE_DELETE_ARRAY( g_pDPLConnection );
        SAFE_RELEASE( g_pDPLobby );
        SAFE_RELEASE( g_pDP );
    }

    CoUninitialize();

    CloseHandle( g_hDPlaySampleRegKey );
    CloseHandle( g_hDPMessageEvent );

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: DoConnectAndGame()
// Desc: Connect to other players using DirectPlay and begin the the game.
//-----------------------------------------------------------------------------
HRESULT DoConnectAndGame( HINSTANCE hInst )
{
    int     nExitCode;
    HRESULT hr;
    BOOL    bLaunchedByLobby;

    // See if we were launched from a lobby server
    hr = DPConnect_CheckForLobbyLaunch( &bLaunchedByLobby );
    if( FAILED(hr) )
    {
        if( hr == DPERR_USERCANCEL )
            return S_OK;

        return hr;
    }

    if( !bLaunchedByLobby )
    {
        // If not, the first step is to prompt the user about the network 
        // connection and which session they would like to join or 
        // if they want to create a new one.
        nExitCode = DPConnect_StartDirectPlayConnect( hInst, FALSE );

        // See the above EXITCODE #defines for what nExitCode could be 
        if( nExitCode == EXITCODE_QUIT )
        {
            // The user canceled the mutliplayer connect.
            // The sample will now quit.
            return E_ABORT;
        }

        if( nExitCode == EXITCODE_ERROR || g_pDP == NULL )
        {
            MessageBox( NULL, TEXT("Mutliplayer connect failed. "
                        "The sample will now quit."),
                        TEXT("DirectPlay Sample"), MB_OK | MB_ICONERROR );
            return E_FAIL;
        }
    }

    // The next step is to start the game
    nExitCode = DoGreetingGame( hInst );

    if( nExitCode == EXITCODE_ERROR )
    {
        MessageBox( NULL, "An error occured during the game. "
                    "The sample will now quit.",
                    TEXT("DirectPlay Sample"), MB_OK | MB_ICONERROR );
        return E_FAIL;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DoGreetingGame()
// Desc: Creates the main game window, and process Windows and DirectPlay 
//       messages
//-----------------------------------------------------------------------------
int DoGreetingGame( HINSTANCE hInst )
{
    HWND    hDlg = NULL;
    BOOL    bDone = FALSE;
    int     nExitCode;
    HRESULT hr; 
    DWORD   dwResult;
    MSG     msg;

    if( g_pDP == NULL )
        return E_FAIL;  // Sanity check

    // Display the greeting game dialog box.
    hDlg = CreateDialog( hInst, MAKEINTRESOURCE(IDD_MAIN_GAME), 
                         NULL, GreetingDlgProc );

    while( !bDone ) 
    { 
        dwResult = MsgWaitForMultipleObjects( 1, &g_hDPMessageEvent, 
                                              FALSE, INFINITE, QS_ALLEVENTS );
        switch( dwResult )
        {
            case WAIT_OBJECT_0 + 0:
                // g_hDPMessageEvent is signaled, so there are
                // DirectPlay messages available
                if( FAILED( hr = ProcessDirectPlayMessages( hDlg ) ) ) 
                    return EXITCODE_ERROR;
                break;

            case WAIT_OBJECT_0 + 1:
                // Windows messages are available
                while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) ) 
                { 
                    if( !IsDialogMessage( hDlg, &msg ) )  
                    {
                        TranslateMessage( &msg ); 
                        DispatchMessage( &msg ); 
                    }

                    if( msg.message == WM_QUIT )
                    {
                        // See the above EXITCODE #defines for 
                        // what nExitCode could be 
                        nExitCode = msg.wParam;

                        EndDialog( hDlg, nExitCode );
                        bDone = TRUE;
                    }
                }
                break;
        }
    }

    return nExitCode;
}




//-----------------------------------------------------------------------------
// Name: GreetingDlgProc()
// Desc: Handles dialog messages
//-----------------------------------------------------------------------------
BOOL CALLBACK GreetingDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
    HRESULT hr;

    switch( msg ) 
    {
        case WM_INITDIALOG:
            {
                if( FAILED( hr = OnInitDialog( hDlg ) ) )
                    PostQuitMessage( EXITCODE_ERROR );
            }
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDC_WAVE:
                    if( FAILED( hr = WaveToAllPlayers() ) )
                        PostQuitMessage( EXITCODE_ERROR );
                    return TRUE;

                case IDCANCEL:
                    PostQuitMessage( EXITCODE_QUIT );
                    return TRUE;
            }
            break;
    }

    return FALSE; // Didn't handle message
}




//-----------------------------------------------------------------------------
// Name: OnInitDialog()
// Desc: Inits the dialog for the greeting game.
//-----------------------------------------------------------------------------
HRESULT OnInitDialog( HWND hDlg )
{
    DWORD           dwBufferSize;
    BYTE*           pData = NULL;
    DPSESSIONDESC2* pdpsd;
    HRESULT         hr;

    // Load and set the icon
    HINSTANCE hInst = (HINSTANCE) GetWindowLong( hDlg, GWL_HINSTANCE );
    HICON hIcon = LoadIcon( hInst, MAKEINTRESOURCE( IDI_MAIN ) );
    SendMessage( hDlg, WM_SETICON, ICON_BIG,   (LPARAM) hIcon );  // Set big icon
    SendMessage( hDlg, WM_SETICON, ICON_SMALL, (LPARAM) hIcon );  // Set small icon

    // Display local player's name
    SetDlgItemText( hDlg, IDC_PLAYER_NAME, g_strLocalPlayerName );

    // Get the size of the dpsd
    g_pDP->GetSessionDesc( NULL, &dwBufferSize );

    // Allocate space for it now that we know the size 
    pData = new BYTE[ dwBufferSize ];
    if( pData == NULL )
        return E_OUTOFMEMORY;

    // Now, get the dpsd in the buffer
    if( FAILED( hr = g_pDP->GetSessionDesc( pData, &dwBufferSize ) ) )
        return hr;

    // Typecast the data to a DPSESSIONDESC2* 
    pdpsd = (DPSESSIONDESC2*) pData;
    g_dwNumberOfActivePlayers = pdpsd->dwCurrentPlayers;

    // Update the dialog box
    DisplayNumberPlayersInGame( hDlg );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DisplayNumberPlayersInGame()
// Desc: Displays the number of active players 
//-----------------------------------------------------------------------------
VOID DisplayNumberPlayersInGame( HWND hDlg )
{
    TCHAR strNumberPlayers[32];

    sprintf( strNumberPlayers, "%d", g_dwNumberOfActivePlayers );
    SetDlgItemText( hDlg, IDC_NUM_PLAYERS, strNumberPlayers );
}




//-----------------------------------------------------------------------------
// Name: ProcessDirectPlayMessages()
// Desc: Processes for DirectPlay messages
//-----------------------------------------------------------------------------
HRESULT ProcessDirectPlayMessages( HWND hDlg )
{
    DPID    idFrom;
    DPID    idTo;
    LPVOID  pvMsgBuffer;
    DWORD   dwMsgBufferSize;
    HRESULT hr;

    // Read all messages in queue
    dwMsgBufferSize = g_dwDPMsgBufferSize;
    pvMsgBuffer     = g_pvDPMsgBuffer;
    
    while( TRUE )
    {
        // See what's out there
        idFrom = 0;
        idTo   = 0;

        hr = g_pDP->Receive( &idFrom, &idTo, DPRECEIVE_ALL, 
                             pvMsgBuffer, &dwMsgBufferSize );

        if( hr == DPERR_BUFFERTOOSMALL )
        {
            // The current buffer was too small, 
            // so reallocate it and try again
            SAFE_DELETE_ARRAY( pvMsgBuffer );

            pvMsgBuffer = new BYTE[ dwMsgBufferSize ];
            if( pvMsgBuffer == NULL )
                return E_OUTOFMEMORY;

            // Save new buffer in globals
            g_pvDPMsgBuffer     = pvMsgBuffer;
            g_dwDPMsgBufferSize = dwMsgBufferSize;

            continue; // Now that the buffer is bigger, try again
        }

        if( DPERR_NOMESSAGES == hr )
            return S_OK;

        if( FAILED(hr) )
            return hr;

        // Handle the messages. If its from DPID_SYSMSG, its a system message, 
        // otherwise its an application message. 
        if( idFrom == DPID_SYSMSG )
        {
            hr = HandleSystemMessages( hDlg, (DPMSG_GENERIC*)pvMsgBuffer, 
                                       dwMsgBufferSize, idFrom, idTo );
            if( FAILED(hr) )
                return hr;
        }
        else
        {
            hr = HandleAppMessages( hDlg, (GAMEMSG_GENERIC*)pvMsgBuffer, 
                                    dwMsgBufferSize, idFrom, idTo );
            if( FAILED(hr) )
                return hr;
        }
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: HandleAppMessages()
// Desc: Evaluates application messages and performs appropriate actions
//-----------------------------------------------------------------------------
HRESULT HandleAppMessages( HWND hDlg, GAMEMSG_GENERIC* pMsg, DWORD dwMsgSize, 
                           DPID idFrom, DPID idTo )
{
    HRESULT hr;

    switch( pMsg->dwType )
    {
        case GAMEMSG_WAVE:
            {
                // This message is sent when a player has waved to us.
                if( FAILED( hr = DisplayPlayerWave( hDlg, idFrom ) ) ) 
                    return hr;
            }
            break;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: HandleSystemMessages()
// Desc: Evaluates system messages and performs appropriate actions
//-----------------------------------------------------------------------------
HRESULT HandleSystemMessages( HWND hDlg, DPMSG_GENERIC* pMsg, DWORD dwMsgSize, 
                              DPID idFrom, DPID idTo )
{  
    switch( pMsg->dwType )
    {
        case DPSYS_SESSIONLOST:
            // Non-host message.  This message is sent to all players 
            // when the host exits the game.
            if( g_bHostPlayer )
                return E_FAIL; // Sanity check

            PostQuitMessage( DPERR_SESSIONLOST );
            break;

        case DPSYS_CREATEPLAYERORGROUP:
            DPMSG_CREATEPLAYERORGROUP* pCreateMsg;
            pCreateMsg = (DPMSG_CREATEPLAYERORGROUP*) pMsg;

            // Update the number of active players
            g_dwNumberOfActivePlayers++;

            DisplayNumberPlayersInGame( hDlg );
            break;

        case DPSYS_DESTROYPLAYERORGROUP:
            DPMSG_DESTROYPLAYERORGROUP* pDeleteMsg;
            pDeleteMsg = (DPMSG_DESTROYPLAYERORGROUP*) pMsg;

            // Update the number of active players
            g_dwNumberOfActivePlayers--;

            DisplayNumberPlayersInGame( hDlg );
            break;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: WaveToAllPlayers()
// Desc: Send a app-defined "wave" DirectPlay message to all connected players
//-----------------------------------------------------------------------------
HRESULT WaveToAllPlayers()
{
    HRESULT hr;

    if( g_dwNumberOfActivePlayers == 1 )
    {
        MessageBox( NULL, "No one is around to wave at! :(", 
                    "The Greeting Game", MB_OK );
    }
    else
    {
        GAMEMSG_GENERIC msgWave;
        msgWave.dwType = GAMEMSG_WAVE;

        // Send it to all of the players
        if( FAILED( hr = g_pDP->Send( g_LocalPlayerDPID, DPID_ALLPLAYERS, 
                                      0, &msgWave, sizeof(GAMEMSG_WAVE) ) ) ) 
            return hr;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DisplayPlayerWave()
// Desc: A player has waved to us, so find out the player's name and 
//       tell the local player about it.
//-----------------------------------------------------------------------------
HRESULT DisplayPlayerWave( HWND hDlg, DPID idFrom )
{
    HRESULT hr;
    BYTE*   pData = NULL;
    DWORD   dwBufferSize;
    TCHAR   szWaveMessage[MAX_PLAYER_NAME + 50];

    // Get the size of the buffer needed
    hr = g_pDP->GetPlayerName( idFrom, NULL, &dwBufferSize );
    if( hr != DPERR_BUFFERTOOSMALL && FAILED(hr) )
        return hr;

    // Allocate the buffer now that we know the size
    pData = new BYTE[ dwBufferSize ];
    if( NULL == pData )
        return E_OUTOFMEMORY;

    // Get the data from DirectPlay
    if( FAILED( hr = g_pDP->GetPlayerName( idFrom, pData, &dwBufferSize ) ) )
        return hr;

    // Typecast the buffer into a DPNAME structure
    DPNAME* pdpname = (DPNAME*) pData;

    // Make wave message and display it.
    sprintf( szWaveMessage, "%s just waved at you, %s!", pdpname->lpszShortNameA,
                                                    g_strLocalPlayerName );
    MessageBox( hDlg, szWaveMessage, "The Greeting Game", MB_OK );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: ReadRegKey()
// Desc: Read a registry key 
//-----------------------------------------------------------------------------
HRESULT ReadRegKey( HKEY hKey, TCHAR* strName, TCHAR* strValue, 
                    DWORD dwLength, TCHAR* strDefault )
{
	DWORD dwType;
	LONG bResult;

	bResult = RegQueryValueEx( hKey, strName, 0, &dwType, 
							 (LPBYTE) strValue, &dwLength );
	if ( bResult != ERROR_SUCCESS )
		strcpy( strValue, strDefault );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: WriteRegKey()
// Desc: Writes a registry key 
//-----------------------------------------------------------------------------
HRESULT WriteRegKey( HKEY hKey, TCHAR* strName, TCHAR* strValue )
{
	LONG bResult;

	bResult = RegSetValueEx( hKey, strName, 0, REG_SZ, 
							 (LPBYTE) strValue, strlen(strValue) + 1 );
	if ( bResult != ERROR_SUCCESS )
		return E_FAIL;

    return S_OK;
}

