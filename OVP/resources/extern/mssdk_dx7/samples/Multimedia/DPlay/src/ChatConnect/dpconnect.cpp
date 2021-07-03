//-----------------------------------------------------------------------------
// File: DPConnect.cpp
//
// Desc: Support file to prompt the user for the DirectPlay connection 
//       and the DirectPlay session they want to join or create. 
//
//       Call DPConnect_StartDirectPlayConnect() to start the series of 
//       dialogs boxes.  At the end the player will be connected to an active
//       DirectPlay session that the user either created or joined.  Or the 
//       function may also return an EXITCODE as defined below in the event
//       of error or user cancel.  
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define IDIRECTPLAY2_OR_GREATER
#define STRICT
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

#define MAX_PLAYER_NAME      14
#define MAX_SESSION_NAME     256

struct DPSessionInfo
{
	GUID guidSession;
	TCHAR szSession[MAX_SESSION_NAME];
	DPSessionInfo* pDPSINext;
};




//-----------------------------------------------------------------------------
// Global variables
//-----------------------------------------------------------------------------
#define EXITCODE_FORWARD      1  // Dialog success, so go forward
#define EXITCODE_BACKUP       2  // Dialog canceled, show previous dialog
#define EXITCODE_QUIT         3  // Dialog quit, close app
#define EXITCODE_ERROR        4  // Dialog error, terminate app
#define EXITCODE_LOBBYCONNECT 5  // Dialog connected from lobby, connect success

extern LPDIRECTPLAY4A       g_pDP;
extern LPDPLCONNECTION      g_pDPLConnection;
extern LPDIRECTPLAYLOBBY3   g_pDPLobby;
extern GUID                 g_AppGUID; 
extern HANDLE               g_hDPMessageEvent;
extern TCHAR                g_strPreferredProvider[MAX_SESSION_NAME];
extern TCHAR                g_strSessionName[MAX_SESSION_NAME];
extern TCHAR                g_strLocalPlayerName[MAX_PLAYER_NAME];
extern DPID                 g_LocalPlayerDPID; 
extern BOOL                 g_bHostPlayer;
extern TCHAR                g_strAppName[256];
extern BOOL                 g_bUseProtocol;

DPSessionInfo               g_DPSIHead;
BOOL                        g_bSearchingForSessions;
BOOL                        g_bWaitForConnectionSettings;




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
int             DPConnect_StartDirectPlayConnect( HINSTANCE hInst, BOOL bBackTrack );

BOOL CALLBACK   DPConnect_ConnectionsDlgProc( HWND, UINT, WPARAM, LPARAM );
BOOL FAR PASCAL DPConnect_EnumConnectionsCallback( LPCGUID, VOID*, DWORD, LPCDPNAME, DWORD, VOID* );
HRESULT         DPConnect_ConnectionsDlgFillListBox( HWND hDlg );
HRESULT         DPConnect_ConnectionsDlgOnOK( HWND hDlg );
VOID            DPConnect_ConnectionsDlgCleanup( HWND hDlg );

BOOL CALLBACK   DPConnect_SessionsDlgProc( HWND, UINT, WPARAM, LPARAM );
BOOL CALLBACK   DPConnect_CreateSessionDlgProc( HWND, UINT, WPARAM, LPARAM );
BOOL FAR PASCAL DPConnect_EnumSessionsCallback( LPCDPSESSIONDESC2, DWORD*, DWORD, VOID* );
VOID            DPConnect_SessionsDlgInitListbox( HWND hDlg );
HRESULT         DPConnect_SessionsDlgShowGames( HWND hDlg );
HRESULT         DPConnect_SessionsDlgJoinGame( HWND hDlg );
HRESULT         DPConnect_SessionsDlgCreateGame( HWND hDlg );
VOID            DPConnect_SessionsDlgCleanup();

BOOL CALLBACK   DPConnect_LobbyWaitDlgProc( HWND, UINT, WPARAM, LPARAM );
HRESULT         DPConnect_CheckForLobbyLaunch( BOOL* pbLaunchedByLobby );
HRESULT         DPConnect_DoLobbyLaunch();
HRESULT         DPConnect_WaitForLobbyLaunch( HWND hParentDlg );
HRESULT         DPConnect_GetLobbyConnectionSettingsMessage();




//-----------------------------------------------------------------------------
// Name: DPConnect_StartDirectPlayConnect()
// Desc: Prompts the user for the DirectPlay connection and the DirectPlay 
//       session they want to join or create.  This function returns one
//       of the EXITCODEs as #defined above
//-----------------------------------------------------------------------------
int DPConnect_StartDirectPlayConnect( HINSTANCE hInst, BOOL bBackTrack = FALSE )
{
    int nExitCode;
    int nStep;

    // Setup the g_DPSIHead circular linked list
    ZeroMemory( &g_DPSIHead, sizeof( DPSessionInfo ) );
    g_DPSIHead.pDPSINext = &g_DPSIHead;
    
    // If the back track flag is true, then the user has already been through
    // the connect process once, and has back tracked out of the main 'game' 
    // so we need start at the last dialog box
    if( bBackTrack )
        nStep = 1;
    else
        nStep = 0;

    // Show the dialog boxes to connect
    while( TRUE )
    {
        switch( nStep )
        {
            case 0:
                // Display the multiplayer connect dialog box.
                nExitCode = DialogBox( hInst, MAKEINTRESOURCE(IDD_MUTLIPLAYER_CONNECT), 
                                       NULL, DPConnect_ConnectionsDlgProc );
                break;

            case 1:
                // Display the multiplayer games dialog box.
                nExitCode = DialogBox( hInst, MAKEINTRESOURCE(IDD_MUTLIPLAYER_GAMES), 
                                       NULL, DPConnect_SessionsDlgProc );
                break;
        }

        if( nExitCode == EXITCODE_ERROR || 
            nExitCode == EXITCODE_QUIT )
            return nExitCode;

        if( nExitCode == EXITCODE_BACKUP )
            nStep--;
        else
            nStep++;

        if( nExitCode == EXITCODE_LOBBYCONNECT )
            return EXITCODE_LOBBYCONNECT;

        if( nStep == 2 )
            break;
    }

    return EXITCODE_FORWARD;
}




//-----------------------------------------------------------------------------
// Name: DPConnect_ConnectionsDlgProc()
// Desc: Handles messages for the multiplayer connect dialog
//-----------------------------------------------------------------------------
BOOL CALLBACK DPConnect_ConnectionsDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
    HRESULT hr;

    switch( msg )
    {
        case WM_INITDIALOG:
            {
                SetDlgItemText( hDlg, IDC_PLAYER_NAME_EDIT, g_strLocalPlayerName );

                // Load and set the icon
                HINSTANCE hInst = (HINSTANCE) GetWindowLong( hDlg, GWL_HINSTANCE );
                HICON hIcon = LoadIcon( hInst, MAKEINTRESOURCE( IDI_MAIN ) );
                SendMessage( hDlg, WM_SETICON, ICON_BIG,   (LPARAM) hIcon );  // Set big icon
                SendMessage( hDlg, WM_SETICON, ICON_SMALL, (LPARAM) hIcon );  // Set small icon

                // Set the window title
                TCHAR strWindowTitle[256];
                sprintf( strWindowTitle, "%s - Multiplayer Connect", g_strAppName );
                SetWindowText( hDlg, strWindowTitle );

                if( FAILED( hr = DPConnect_ConnectionsDlgFillListBox( hDlg ) ) )
                    EndDialog( hDlg, EXITCODE_ERROR );
            }
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDC_CONNECTION_LIST:
                    if( HIWORD(wParam) != LBN_DBLCLK )
                        break;
                    // Fall through

                case IDOK:
                    if( FAILED( hr = DPConnect_ConnectionsDlgOnOK( hDlg ) ) )
                        EndDialog( hDlg, EXITCODE_ERROR );

                    break;

                case IDCANCEL:
                    EndDialog( hDlg, EXITCODE_QUIT );
                    break;

                default:
                    return FALSE; // Message not handled
            }
            break;
        
        case WM_DESTROY:
            DPConnect_ConnectionsDlgCleanup( hDlg );
            break;

        default:
            return FALSE; // Message not handled
    }

    // Message was handled
    return TRUE; 
}




//-----------------------------------------------------------------------------
// Name: DPConnect_ConnectionsDlgFillListBox()
// Desc: Fills the DirectPlay connection listbox, and also adds
//       a "Wait for Lobby" connection option.
//-----------------------------------------------------------------------------
HRESULT DPConnect_ConnectionsDlgFillListBox( HWND hDlg )
{
    HRESULT       hr;
    HWND          hWndListBox = GetDlgItem( hDlg, IDC_CONNECTION_LIST );
    LPDIRECTPLAY4 pDP = NULL;
    int           nIndex;

    // Create a IDirectPlay object
    if( FAILED( hr = CoCreateInstance( CLSID_DirectPlay, NULL, CLSCTX_ALL, 
                                       IID_IDirectPlay4A, (VOID**)&pDP ) ) )
    {
        if( hr == E_NOINTERFACE )
        {
            MessageBox( NULL, TEXT("This application requires DirectPlay 6 or later. "
                        "The sample will now quit."),
                        TEXT("DirectPlay Sample"), MB_OK | MB_ICONERROR );
        }

        return hr;
    }

    // Enumerate all DirectPlay connections, and store them in the listbox
    if( FAILED( hr = pDP->EnumConnections( &g_AppGUID, DPConnect_EnumConnectionsCallback, 
                                           hWndListBox, 0 ) ) )
    {
        SAFE_RELEASE( pDP );
        return hr;
    }

    SAFE_RELEASE( pDP );
    
    // Add "Wait for Lobby Connection" selection in list box
    SendMessage( hWndListBox, LB_ADDSTRING, 0, 
                 (LPARAM)"Wait for Lobby Connection" );

    SetFocus( hWndListBox );
    nIndex = SendMessage( hWndListBox, LB_FINDSTRINGEXACT, (WPARAM)-1, 
                          (LPARAM)g_strPreferredProvider );
    if( nIndex != LB_ERR ) 
        SendMessage( hWndListBox, LB_SETCURSEL, nIndex, 0 );
    else
        SendMessage( hWndListBox, LB_SETCURSEL, 0, 0 );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPConnect_EnumConnectionsCallback()
// Desc: Enumerates through all DirectPlay connections types, 
//       and stores them in the listbox
//-----------------------------------------------------------------------------
BOOL FAR PASCAL DPConnect_EnumConnectionsCallback( LPCGUID   pguidSP, 
                                                   VOID*     pConnection, 
                                                   DWORD     dwConnectionSize, 
                                                   LPCDPNAME pName, 
                                                   DWORD     dwFlags, 
                                                   VOID*     pvContext )
{
    HRESULT       hr;
    LPDIRECTPLAY4 pDP = NULL;
    VOID*         pConnectionBuffer = NULL;
    HWND          hWndListBox = (HWND)pvContext;
    LRESULT       iIndex;

    // Create a IDirectPlay object
    if( FAILED( hr = CoCreateInstance( CLSID_DirectPlay, NULL, CLSCTX_ALL, 
                                       IID_IDirectPlay4A, (VOID**)&pDP ) ) )
        return FALSE; // Error, stop enumerating
    
    // Test the if the connection is available by attempting to initialize 
    // the connection
    if( FAILED( hr = pDP->InitializeConnection( pConnection, 0 ) ) )
    {
        SAFE_RELEASE( pDP );
        return TRUE; // Unavailable connection, keep enumerating
    }

    // Don't need the IDirectPlay interface anymore, so release it
    SAFE_RELEASE( pDP ); 
    
    // Found a good connection, so put it in the listbox
    iIndex = SendMessage( hWndListBox, LB_ADDSTRING, 0, 
                          (LPARAM)pName->lpszShortNameA );
    if( iIndex == CB_ERR )
        return FALSE; // Error, stop enumerating

    pConnectionBuffer = new BYTE[ dwConnectionSize ];
    if( pConnectionBuffer == NULL )
        return FALSE; // Error, stop enumerating

    // Store pointer to GUID in listbox
    memcpy( pConnectionBuffer, pConnection, dwConnectionSize );
    SendMessage( hWndListBox, LB_SETITEMDATA, iIndex, 
                 (LPARAM)pConnectionBuffer );

    return TRUE; // Keep enumerating
}




//-----------------------------------------------------------------------------
// Name: DPConnect_ConnectionsDlgOnOK()
// Desc: Stores the player name g_strPlayerName, and in creates a IDirectPlay
//       object based on the connection type the user selected. 
//-----------------------------------------------------------------------------
HRESULT DPConnect_ConnectionsDlgOnOK( HWND hDlg )
{
    VOID*   pConnection = NULL;
    LRESULT iIndex;
    HRESULT hr;

    GetDlgItemText( hDlg, IDC_PLAYER_NAME_EDIT, g_strLocalPlayerName, 
                    MAX_PLAYER_NAME );

    if( strlen( g_strLocalPlayerName ) == 0 )
    {
        MessageBox( hDlg, TEXT("You must enter a valid player name."),
                    TEXT("DirectPlay Sample"), MB_OK );
        return S_OK;
    }

    HWND hWndListBox = GetDlgItem( hDlg, IDC_CONNECTION_LIST );

    iIndex = SendMessage( hWndListBox, LB_GETCURSEL, 0, 0 );
    SendMessage( hWndListBox, LB_GETTEXT, iIndex, (LPARAM)g_strPreferredProvider );

    pConnection = (VOID*) SendMessage( hWndListBox, LB_GETITEMDATA, iIndex, 0 );
    if( NULL == pConnection )
    {
        if( FAILED( hr = DPConnect_WaitForLobbyLaunch( hDlg ) ) )
        {
            if( hr == DPERR_USERCANCEL )
                return S_OK;

            return hr;
        }

        EndDialog( hDlg, EXITCODE_LOBBYCONNECT );
        return S_OK;
    }

    // Release previously selected DirectPlay object, if any.
    SAFE_RELEASE( g_pDP );

    // Create the DirectPlay object
    if( FAILED( hr = CoCreateInstance( CLSID_DirectPlay, NULL, CLSCTX_ALL, 
                                       IID_IDirectPlay4A, (VOID**)&g_pDP ) ) )
        return hr;
    
    // Initialize the connection based on the selected connection type
    if( FAILED( hr = g_pDP->InitializeConnection( pConnection, 0 ) ) )
        return hr;

    EndDialog( hDlg, EXITCODE_FORWARD );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPConnect_ConnectionsDlgCleanup()
// Desc: Deletes the connection buffers from the listbox
//-----------------------------------------------------------------------------
VOID DPConnect_ConnectionsDlgCleanup( HWND hDlg )
{
    VOID*   pConnectionBuffer = NULL;
    DWORD   iIndex;
    DWORD   dwCount;

    HWND hWndListBox = GetDlgItem( hDlg, IDC_CONNECTION_LIST );

    dwCount = SendMessage( hWndListBox, LB_GETCOUNT, 0, 0 );
    for( iIndex = 0; iIndex < dwCount; iIndex++ )
    {
        pConnectionBuffer = (VOID*) SendMessage( hWndListBox, LB_GETITEMDATA, 
                                                 iIndex, 0 );
        SAFE_DELETE_ARRAY( pConnectionBuffer );
    }
}




//-----------------------------------------------------------------------------
// Name: DPConnect_SessionsDlgProc()
// Desc: Handles messages fro the multiplayer games dialog
//-----------------------------------------------------------------------------
BOOL CALLBACK DPConnect_SessionsDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
    HRESULT hr;

    switch( msg )
    {
        case WM_INITDIALOG:
            {
                // Load and set the icon
                HINSTANCE hInst = (HINSTANCE) GetWindowLong( hDlg, GWL_HINSTANCE );
                HICON hIcon = LoadIcon( hInst, MAKEINTRESOURCE( IDI_MAIN ) );
                SendMessage( hDlg, WM_SETICON, ICON_BIG,   (LPARAM) hIcon );  // Set big icon
                SendMessage( hDlg, WM_SETICON, ICON_SMALL, (LPARAM) hIcon );  // Set small icon

                // Set the window title
                TCHAR strWindowTitle[256];
                sprintf( strWindowTitle, "%s - Multiplayer Games", g_strAppName );
                SetWindowText( hDlg, strWindowTitle );

                g_bSearchingForSessions = FALSE;
                SetDlgItemText( hDlg, IDC_SEARCH_CHECK, "Start Search" );
                DPConnect_SessionsDlgInitListbox( hDlg );
            }
            break;

        case WM_TIMER:
            if( FAILED( hr = DPConnect_SessionsDlgShowGames( hDlg ) ) )
            {
                KillTimer( hDlg, 1 );
                MessageBox( hDlg, TEXT("Error enumerating DirectPlay games."),
                            TEXT("DirectPlay Sample"), 
                            MB_OK | MB_ICONERROR );
            }
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDC_SEARCH_CHECK:
                    g_bSearchingForSessions = !g_bSearchingForSessions;

                    if( g_bSearchingForSessions )
                    {
                        // Start the timer, and start the async enumeratation
                        SetDlgItemText( hDlg, IDC_SEARCH_CHECK, "Searching..." );
                        SetTimer( hDlg, 1, 250, NULL );
                        if( FAILED( hr = DPConnect_SessionsDlgShowGames( hDlg ) ) )
                        {
                            KillTimer( hDlg, 1 );
                            MessageBox( hDlg, TEXT("Error enumerating DirectPlay games."),
                                        TEXT("DirectPlay Sample"), 
                                        MB_OK | MB_ICONERROR );
                        }
                    }
                    else
                    {
                        // Stop the timer, and stop the async enumeration
                        KillTimer( hDlg, 1 );

                        DPSESSIONDESC2 dpsd;
                        ZeroMemory( &dpsd, sizeof(dpsd) );
                        dpsd.dwSize          = sizeof(dpsd);
                        dpsd.guidApplication = g_AppGUID;
                        hr = g_pDP->EnumSessions( &dpsd, 0, DPConnect_EnumSessionsCallback, 
                                                  NULL,  DPENUMSESSIONS_STOPASYNC );
                        if( hr != DPERR_GENERIC && FAILED(hr) ) 
                            EndDialog( hDlg, EXITCODE_ERROR );

                        SetDlgItemText( hDlg, IDC_SEARCH_CHECK, "Start Search" );

                        DPConnect_SessionsDlgInitListbox( hDlg );
                    }
                    break;

                case IDC_GAMES_LIST:
                    if( HIWORD(wParam) != LBN_DBLCLK )
                        break;
                    // Fall through

                case IDC_JOIN:
                    if( FAILED( hr = DPConnect_SessionsDlgJoinGame( hDlg ) ) )
                    {
                        MessageBox( hDlg, TEXT("Unable to join game."),
                                    TEXT("DirectPlay Sample"), 
                                    MB_OK | MB_ICONERROR );
                    }                    
                    break;

                case IDC_CREATE:
                    if( FAILED( hr = DPConnect_SessionsDlgCreateGame( hDlg ) ) )
                    {
                        MessageBox( hDlg, TEXT("Unable to create game."),
                                    TEXT("DirectPlay Sample"), 
                                    MB_OK | MB_ICONERROR );
                    }
                    break;

                case IDCANCEL: // The close button was press
                    EndDialog( hDlg, EXITCODE_QUIT );
                    break;

                case IDC_BACK: // "Cancel" button was pressed
                    EndDialog( hDlg, EXITCODE_BACKUP );
                    break;

                default:
                    return FALSE; // Message not handled
            }
            break;
        
        case WM_DESTROY:
            KillTimer( hDlg, 1 );                    
            DPConnect_SessionsDlgCleanup();
            break;

        default:
            return FALSE; // Message not handled
    }

    // Message was handled
    return TRUE; 
}




//-----------------------------------------------------------------------------
// Name: DPConnect_SessionsDlgInitListbox()
// Desc: Initializes the listbox
//-----------------------------------------------------------------------------
VOID DPConnect_SessionsDlgInitListbox( HWND hDlg )
{
    HWND          hWndListBox = GetDlgItem( hDlg, IDC_GAMES_LIST );
    LPDIRECTPLAY4 pDP = NULL;
    
    // Clear the contents from the list box, and 
    // display "Looking for games" text in listbox
    SendMessage( hWndListBox, LB_RESETCONTENT, 0, 0 );
    if( g_bSearchingForSessions )
    {
        SendMessage( hWndListBox, LB_ADDSTRING, 0, 
                     (LPARAM)"Looking for games..." );
    }
    else
    {
        SendMessage( hWndListBox, LB_ADDSTRING, 0, 
                     (LPARAM)"Click Start Search to see a list of games.  "
                             "Click Create to start a new game." );
    }

    SendMessage( hWndListBox, LB_SETITEMDATA,  0, NULL );
    SendMessage( hWndListBox, LB_SETCURSEL,    0, 0 );

    // Disable the join button until sessions are found
    EnableWindow( GetDlgItem( hDlg, IDC_JOIN ), FALSE );
}




//-----------------------------------------------------------------------------
// Name: DPConnect_SessionsDlgShowGames()
// Desc: Enumerates the DirectPlay sessions, and displays them in the listbox
//-----------------------------------------------------------------------------
HRESULT DPConnect_SessionsDlgShowGames( HWND hDlg )
{
    HRESULT        hr;
    HWND           hWndListBox = GetDlgItem( hDlg, IDC_GAMES_LIST );
    DPSESSIONDESC2 dpsd;
    DPSessionInfo* pDPSISelected = NULL;
    int            nItemSelected;
    GUID           guidSelectedSession;
    BOOL           bFindSelectedGUID;
    BOOL           bFoundSelectedGUID;

    // Try to keep the same session selected unless it goes away or 
    // there is no real session currently selected
    bFindSelectedGUID  = FALSE;
    bFoundSelectedGUID = TRUE;

    nItemSelected = SendMessage( hWndListBox, LB_GETCURSEL, 0, 0 );
    if( nItemSelected != LB_ERR )
    {
        pDPSISelected = (DPSessionInfo*) SendMessage( hWndListBox, LB_GETITEMDATA, 
                                                      nItemSelected, 0 );
        if( pDPSISelected != NULL )
        {
            guidSelectedSession = pDPSISelected->guidSession;
            bFindSelectedGUID = TRUE;
        }
    }
    
    // Remove the previous games from the linked list
    DPConnect_SessionsDlgCleanup();

    // Enum sessions and let DirectPlay decide the timeout
    ZeroMemory( &dpsd, sizeof(dpsd) );
    dpsd.dwSize          = sizeof(dpsd);
    dpsd.guidApplication = g_AppGUID;

    // Enumerate all the active DirectPlay games on the selected connection
    hr = g_pDP->EnumSessions( &dpsd, 0, DPConnect_EnumSessionsCallback, NULL, 
                              DPENUMSESSIONS_ALL | DPENUMSESSIONS_ASYNC );    
    if( FAILED(hr) )
    {
        if( hr == DPERR_USERCANCEL )
        {
            // The user canceled the DirectPlay connection dialog, 
            // so stop the search
            if( g_bSearchingForSessions )
            {
                CheckDlgButton( hDlg, IDC_SEARCH_CHECK, BST_UNCHECKED );
                SendMessage( hDlg, WM_COMMAND, IDC_SEARCH_CHECK, 0 );
            }

            return S_OK;
        }
        else 
        {
            DPConnect_SessionsDlgInitListbox( hDlg );
            if ( hr == DPERR_CONNECTING )
                return S_OK;

            return hr;
        }
    }

    // Tell listbox not to redraw itself since the contents are going to change
    SendMessage( hWndListBox, WM_SETREDRAW, FALSE, 0 );

    // Add the enumerated sessions to the listbox
    if( g_DPSIHead.pDPSINext != &g_DPSIHead )
    {
        // Clear the contents from the list box and enable the join button
        SendMessage( hWndListBox, LB_RESETCONTENT, 0, 0 );
        EnableWindow( GetDlgItem( hDlg, IDC_JOIN ), TRUE );
        
        DPSessionInfo* pDPSI = g_DPSIHead.pDPSINext;
        while ( pDPSI != &g_DPSIHead )
        {
            // Add session to the list box
            int nIndex = SendMessage( hWndListBox, LB_ADDSTRING, 0, 
                                      (LPARAM)pDPSI->szSession );
            SendMessage( hWndListBox, LB_SETITEMDATA, nIndex, (LPARAM)pDPSI );

            if( bFindSelectedGUID )
            {
                // Look for the session the was selected before
                if( pDPSI->guidSession == guidSelectedSession )
                {
                    SendMessage( hWndListBox, LB_SETCURSEL, nIndex, 0 );
                    bFoundSelectedGUID = TRUE;
                }
            }

            pDPSI = pDPSI->pDPSINext;
        }

        if( !bFindSelectedGUID || !bFoundSelectedGUID )
            SendMessage( hWndListBox, LB_SETCURSEL, 0, 0 );
    }
    else
    {
        // There are no active session, so just reset the listbox
        DPConnect_SessionsDlgInitListbox( hDlg );
    }

    // Tell listbox to redraw itself now since the contents have changed
    SendMessage( hWndListBox, WM_SETREDRAW, TRUE, 0 );
    InvalidateRect( hWndListBox, NULL, FALSE );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPConnect_EnumSessionsCallback()
// Desc: Enumerates the sessions, and stores them in the linked list, g_DPSIHead
//-----------------------------------------------------------------------------
BOOL FAR PASCAL DPConnect_EnumSessionsCallback( LPCDPSESSIONDESC2 pdpsd, 
                                                DWORD* pdwTimeout, 
                                                DWORD dwFlags, 
                                                VOID* pvContext )
{
    DPSessionInfo* pDPSINew = NULL;
    
    if( dwFlags & DPESC_TIMEDOUT )
        return FALSE; // The enumeration has timed out, so stop the enumeration.
    
    // Found a good session, save it
    pDPSINew = new DPSessionInfo; 
    if( NULL == pDPSINew )
        return FALSE;

    ZeroMemory( pDPSINew, sizeof(DPSessionInfo) );

    // Copy the information into pDPSINew
    pDPSINew->guidSession = pdpsd->guidInstance;
    sprintf( pDPSINew->szSession, "%s (%d/%d)", pdpsd->lpszSessionNameA, 
             pdpsd->dwCurrentPlayers, pdpsd->dwMaxPlayers );

    // Add pDPSINew to the circular linked list, g_pDPSIFirst
    pDPSINew->pDPSINext = g_DPSIHead.pDPSINext;
    g_DPSIHead.pDPSINext = pDPSINew;
    
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: DPConnect_SessionsDlgJoinGame()
// Desc: Joins the selected DirectPlay session
//-----------------------------------------------------------------------------
HRESULT DPConnect_SessionsDlgJoinGame( HWND hDlg )
{
    DPSESSIONDESC2 dpsd;
    DPNAME         dpname;
    HRESULT        hr;
    HWND           hWndListBox = GetDlgItem( hDlg, IDC_GAMES_LIST );
    DPSessionInfo* pDPSISelected = NULL;
    int            nItemSelected;

    nItemSelected = SendMessage( hWndListBox, LB_GETCURSEL, 0, 0 );
    
    // Add status text in list box
    pDPSISelected = (DPSessionInfo*) SendMessage( hWndListBox, LB_GETITEMDATA, 
                                                  nItemSelected, 0 );

    if( NULL == pDPSISelected )
    {
        MessageBox( hDlg, TEXT("There are no games to join."),
                    TEXT("DirectPlay Sample"), MB_OK );
        return S_OK;
    }

    // Setup the DPSESSIONDESC2, and get the session guid from 
    // the selected listbox item
    ZeroMemory( &dpsd, sizeof(dpsd) );
    dpsd.dwSize          = sizeof(dpsd);
    dpsd.guidInstance    = pDPSISelected->guidSession;
    dpsd.guidApplication = g_AppGUID;

    // Join the session
    g_bHostPlayer = FALSE;
    if( FAILED( hr = g_pDP->Open( &dpsd, DPOPEN_JOIN ) ) )
        return hr;

    // Create player based on g_strLocalPlayerName.  
    // Store the player's DPID in g_LocalPlayerDPID.
    // Also all DirectPlay messages for this player will signal g_hDPMessageEvent
    ZeroMemory( &dpname, sizeof(DPNAME) );
    dpname.dwSize         = sizeof(DPNAME);
    dpname.lpszShortNameA = g_strLocalPlayerName;
    
    if( FAILED( hr = g_pDP->CreatePlayer( &g_LocalPlayerDPID, &dpname, 
                                          g_hDPMessageEvent, NULL, 0, 0 ) ) )
        return hr;

    // DirectPlay connect successful, so end dialog
    EndDialog( hDlg, EXITCODE_FORWARD );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPConnect_SessionsDlgCreateGame()
// Desc: Asks the user the session name, and creates a new DirectPlay session
//-----------------------------------------------------------------------------
HRESULT DPConnect_SessionsDlgCreateGame( HWND hDlg )
{
    DPSESSIONDESC2 dpsd;
    DPNAME         dpname;
    HRESULT        hr;
    int            nResult; 
    HINSTANCE      hInst;

    hInst = (HINSTANCE) GetWindowLong( hDlg, GWL_HINSTANCE );

    // Display a modal multiplayer connect dialog box.
    EnableWindow( hDlg, FALSE ); 
    nResult = DialogBox( hInst, MAKEINTRESOURCE(IDD_MULTIPLAYER_CREATE), 
                         hDlg, DPConnect_CreateSessionDlgProc );
    EnableWindow( hDlg, TRUE ); 

    if( nResult == IDCANCEL )
        return S_OK;

    // Setup the DPSESSIONDESC2 based on g_AppGUID, and g_strSessionName.
    // The DPSESSION_KEEPALIVE flag keeps the session alive if players abnormally exit
    ZeroMemory( &dpsd, sizeof(dpsd) );
    dpsd.dwSize           = sizeof(dpsd);
    dpsd.guidApplication  = g_AppGUID;
    dpsd.lpszSessionNameA = g_strSessionName;
    dpsd.dwMaxPlayers     = 10;
    dpsd.dwFlags          = DPSESSION_KEEPALIVE | DPSESSION_MIGRATEHOST;
    if( g_bUseProtocol )
        dpsd.dwFlags |= DPSESSION_DIRECTPLAYPROTOCOL;

    // Create a new session
    g_bHostPlayer = TRUE;
    if( FAILED( hr = g_pDP->Open( &dpsd, DPOPEN_CREATE ) ) )
        return hr;

    // Create player based on g_strLocalPlayerName.  
    // Store the player's DPID in g_LocalPlayerDPID.
    // Also all DirectPlay messages for this player will signal g_hDPMessageEvent
    ZeroMemory( &dpname, sizeof(DPNAME) );
    dpname.dwSize         = sizeof(DPNAME);
    dpname.lpszShortNameA = g_strLocalPlayerName;

    if( FAILED( hr = g_pDP->CreatePlayer( &g_LocalPlayerDPID, &dpname, 
                                          g_hDPMessageEvent, NULL, 0, 
                                          DPPLAYER_SERVERPLAYER ) ) )
        return hr;

    // DirectPlay connect successful, so end dialog
    EndDialog( hDlg, EXITCODE_FORWARD );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPConnect_CreateSessionDlgProc()
// Desc: Handles messages fro the multiplayer create game dialog
//-----------------------------------------------------------------------------
BOOL CALLBACK DPConnect_CreateSessionDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
    DWORD dwNameLength;

    switch( msg ) 
    {
        case WM_INITDIALOG:
            SetDlgItemText( hDlg, IDC_EDIT_SESSION_NAME, g_strSessionName );
            CheckDlgButton( hDlg, IDC_CHECK_DPLAY_PROTOCOL, BST_CHECKED );
            return TRUE;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDOK:
                    dwNameLength = GetDlgItemText( hDlg, IDC_EDIT_SESSION_NAME, 
                                                   g_strSessionName, 
                                                   MAX_SESSION_NAME );
                    if( dwNameLength == 0 )
                        return TRUE; // Don't accept blank session names

                    g_bUseProtocol = ( IsDlgButtonChecked( hDlg, 
                                       IDC_CHECK_DPLAY_PROTOCOL ) == BST_CHECKED );

                    EndDialog( hDlg, IDOK );
                    return TRUE;

                case IDCANCEL:
                    EndDialog( hDlg, IDCANCEL );
                    return TRUE;
            }
            break;
    }

    return FALSE; // Didn't handle message
}




//-----------------------------------------------------------------------------
// Name: DPConnect_SessionsDlgCleanup()
// Desc: Deletes the linked list, g_DPSIHead
//-----------------------------------------------------------------------------
VOID DPConnect_SessionsDlgCleanup()
{
    DPSessionInfo* pDPSI = g_DPSIHead.pDPSINext;
    DPSessionInfo* pDPSIDelete;

    while ( pDPSI != &g_DPSIHead )
    {
        pDPSIDelete = pDPSI;       
        pDPSI = pDPSI->pDPSINext;

        SAFE_DELETE( pDPSIDelete );
    }

    // Re-link the g_DPSIHead circular linked list
    g_DPSIHead.pDPSINext = &g_DPSIHead;
}




//-----------------------------------------------------------------------------
// Name: DPConnect_WaitForLobbyLaunch()
// Desc: Wait for the a lobby connection and connect when found
//-----------------------------------------------------------------------------
HRESULT DPConnect_WaitForLobbyLaunch( HWND hParentDlg )
{
    HRESULT hr;
    BOOL    bLaunchedByLobby;

    // Create a new lobby object
    SAFE_RELEASE( g_pDPLobby );

    if( FAILED( hr = CoCreateInstance( CLSID_DirectPlayLobby, NULL,
                                       CLSCTX_INPROC_SERVER, IID_IDirectPlayLobby3A,
                                       (VOID**)&g_pDPLobby ) ) )
        return hr;

    // Put the application into wait mode
    if( FAILED( hr = g_pDPLobby->WaitForConnectionSettings( 0 ) ) )
        return hr;

    // Use the IDD_LOBBY_WAIT_STATUS dialog to check for a connection message
    g_bWaitForConnectionSettings = TRUE;

    EnableWindow( hParentDlg, FALSE );

    // Display a dialog box and wait for the connection settings
    DialogBoxParam( NULL, MAKEINTRESOURCE( IDD_LOBBY_WAIT_STATUS ), hParentDlg,
                    DPConnect_LobbyWaitDlgProc, (LPARAM)&hr );
   
    EnableWindow( hParentDlg, TRUE );

    if( FAILED(hr) )
    {
        g_pDPLobby->WaitForConnectionSettings( DPLWAIT_CANCEL );
        SAFE_RELEASE( g_pDPLobby );

        return hr;
    }

    // The lobby server has launched the game, so check for a lobby
    // launch to continue on to connect to the session and create the player
    if( FAILED( hr = DPConnect_CheckForLobbyLaunch( &bLaunchedByLobby ) ) )
        return hr; // This may be DPERR_USERCANCEL if the user canceled the dialog

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPConnect_CheckForLobbyLaunch()
// Desc: Determines if we were launched by a lobby. If so, gets the connection
//       settings and creates our player using the information from the lobby
//-----------------------------------------------------------------------------
HRESULT DPConnect_CheckForLobbyLaunch( BOOL* pbLaunchedByLobby )
{
    HRESULT hr;
    DWORD   dwSize;

    if( NULL == pbLaunchedByLobby ) 
        return E_FAIL;

    // Create a new lobby object
    SAFE_RELEASE( g_pDPLobby );

    if( FAILED( hr = CoCreateInstance( CLSID_DirectPlayLobby, NULL,
                                       CLSCTX_INPROC_SERVER, IID_IDirectPlayLobby3A,
                                       (VOID**)&g_pDPLobby ) ) )
    {
        if( hr == E_NOINTERFACE )
        {
            MessageBox( NULL, TEXT("This application requires DirectPlay 6 or later. "
                        "The sample will now quit."),
                        TEXT("DirectPlay Sample"), MB_OK | MB_ICONERROR );
        }

        return hr;
    }

    // Get connection settings from the lobby (into g_pdplConnection)
    hr = g_pDPLobby->GetConnectionSettings( 0, NULL, &dwSize );
    if( FAILED(hr) && (DPERR_BUFFERTOOSMALL != hr) )
    {
        if( DPERR_NOTLOBBIED == hr )
        {
            *pbLaunchedByLobby = FALSE;

            // Cleanup since we were not launched from the lobby
            SAFE_DELETE_ARRAY( g_pDPLConnection );
            SAFE_RELEASE( g_pDPLobby );

            return S_OK;
        }

        return hr; 
    }

    // Allocate memory for the connection
    SAFE_DELETE_ARRAY( g_pDPLConnection );
    g_pDPLConnection = (DPLCONNECTION*)new BYTE[ dwSize ];

    // Get the connection settings
    if( FAILED( hr = g_pDPLobby->GetConnectionSettings( 0, g_pDPLConnection, 
                                                        &dwSize ) ) )
        return hr;

    *pbLaunchedByLobby = TRUE;

    if( FAILED( hr = DPConnect_DoLobbyLaunch() ) )
    {
        // Cleanup 
        SAFE_DELETE_ARRAY( g_pDPLConnection );
        SAFE_RELEASE( g_pDPLobby );

        return hr; // This may be DPERR_USERCANCEL if the user canceled the dialog
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPConnect_DoLobbyLaunch()
// Desc: The app was launched by the lobby server, so connect to the session
//       and create the player
//-----------------------------------------------------------------------------
HRESULT DPConnect_DoLobbyLaunch()
{
    HRESULT hr;

    // Detrimine if this is the host player 
    g_bHostPlayer = ( (g_pDPLConnection->dwFlags & DPLCONNECTION_CREATESESSION) != 0 );

    // Set our session flags
    g_pDPLConnection->lpSessionDesc->dwFlags = DPSESSION_KEEPALIVE | 
                                               DPSESSION_MIGRATEHOST;

    // Let lobby know our connection flags
    if( FAILED( hr = g_pDPLobby->SetConnectionSettings( 0, 0, g_pDPLConnection ) ) )
        return hr;

    // If we're hosting, just call ConnectEx syncronously.  Otherwise, display
    // the connecting dialog and do the connect asynchronously.
    if( g_bHostPlayer )
    {
        // Connect to the lobby syncronously
        if( FAILED( hr = g_pDPLobby->ConnectEx( 0, IID_IDirectPlay4A, 
                                                (VOID**)&g_pDP, NULL ) ) )
            return hr;
    }
    else
    {
        // Use the IDD_LOBBY_WAIT_STATUS dialog to attempt to connect to 
        // the new session
        g_bWaitForConnectionSettings = FALSE;

        // Connect to the lobby asyncronously
        DialogBoxParam( NULL, MAKEINTRESOURCE( IDD_LOBBY_WAIT_STATUS ), NULL,
                        DPConnect_LobbyWaitDlgProc, (LPARAM)&hr );
        if( FAILED(hr) )
            return hr; // This may be DPERR_USERCANCEL if the user canceled the dialog
    }

    strcpy( g_strLocalPlayerName, g_pDPLConnection->lpPlayerName->lpszShortNameA );

    // Create our player
    DPNAME dpname;
    ZeroMemory( &dpname, sizeof(DPNAME) );
    dpname.dwSize         = sizeof(DPNAME);
    dpname.lpszShortNameA = g_strLocalPlayerName;

    if( FAILED( hr = g_pDP->CreatePlayer( &g_LocalPlayerDPID, &dpname, 
                                          g_hDPMessageEvent, NULL, 0, 0 ) ) )
        return hr;

    // Successfully lobby launch, note that we don't release the lobby object
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPConnect_LobbyWaitDlgProc()
// Desc: Handles dialog messages.  This dialog starts a timer that attempts to
//       either receive a lobby connection settings message or 
//       connect to new session via the lobby connection depending on the 
//       g_bWaitForConnectionSettings flag.
//-----------------------------------------------------------------------------
BOOL CALLBACK DPConnect_LobbyWaitDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
    static HRESULT* phr = NULL;

    switch( msg )
    {
        case WM_INITDIALOG:
            // Save the HRESULT pointer
            phr = (HRESULT*)lParam;           

            if( g_bWaitForConnectionSettings )
                SetDlgItemText( hDlg, IDC_WAIT_TEXT, TEXT("Waiting for connection...") );
            else
                SetDlgItemText( hDlg, IDC_WAIT_TEXT, TEXT("Finding Game...") );

            // Create a timer.  The dialog will attempt to connect at every WM_TIMER
            SetTimer( hDlg, 0, 250, NULL );
            break;

        case WM_DESTROY:
            KillTimer( hDlg, 0 );
            break;

        case WM_TIMER:
            // Attempt to connect to the game
            if( g_bWaitForConnectionSettings )
            {
                *phr = DPConnect_GetLobbyConnectionSettingsMessage();
                if( *phr == S_OK )
                    EndDialog( hDlg, 0 );
            }
            else
            {
                *phr = g_pDPLobby->ConnectEx( DPCONNECT_RETURNSTATUS, IID_IDirectPlay4A, 
                                              (VOID**)&g_pDP, NULL );
                if( *phr == DPERR_CONNECTING )
                    break; // Try again on the next WM_TIMER message

                EndDialog( hDlg, 0 );
            }

            break;
        
        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDC_LOBBYCONNECTCANCEL:
                case IDCANCEL:
                    // the async Connect will be stopped when the
                    // IDirectPlayLobby3 interface is released.
                    
                    // Set the HRESULT to user cancel and close the dialog
                    *phr = DPERR_USERCANCEL;
                    EndDialog( hDlg, 0 );
                    break;

                default:
                    return FALSE; // Message not handled
            }
            break;

        default:
            return FALSE; // Message not handled
    }

    return TRUE; // Message handled
}




//-----------------------------------------------------------------------------
// Name: DPConnect_GetLobbyConnectionSettingsMessage()
// Desc: Attepts to get a lobby connection settings message.  If it does 
//       it returns S_OK otherwise it returns S_FALSE 
//-----------------------------------------------------------------------------
HRESULT DPConnect_GetLobbyConnectionSettingsMessage()
{
    HRESULT hr = NOERROR;
    LPVOID  pvMsgBuffer = NULL;
    DWORD   dwMsgBufferSize = 0;
    DWORD   dwMsgFlags;

    while( TRUE )
    {
        hr = g_pDPLobby->ReceiveLobbyMessage( 0, 0, &dwMsgFlags,
                                              pvMsgBuffer, &dwMsgBufferSize );
        if( hr == DPERR_BUFFERTOOSMALL ) 
        {
            // The current buffer was too small, 
            // so reallocate it and try again
            SAFE_DELETE_ARRAY( pvMsgBuffer );

            pvMsgBuffer = new BYTE[ dwMsgBufferSize ];
            if( pvMsgBuffer == NULL )
                return E_OUTOFMEMORY;

            continue; // Now that the buffer is bigger, try again
        }

        if( DPERR_NOMESSAGES == hr )
            break;

        if( FAILED(hr) )
            break;

        // Found a messages, check to see if its a CONNECTIONSETTINGS message
        if( dwMsgFlags & DPLMSG_SYSTEM )
        {
            // If it is found then stop looking for it
            if( ( (DPLMSG_GENERIC*)pvMsgBuffer)->dwType == DPLSYS_NEWCONNECTIONSETTINGS )
                break;
        }
    }

    // Cleanup buffer
    SAFE_DELETE_ARRAY( pvMsgBuffer );

    // Return S_FALSE if there we no connection settings messages
    if( DPERR_NOMESSAGES == hr )
        return S_FALSE;

    // If the message was recieved the hr should be S_OK
    return hr; 
}


