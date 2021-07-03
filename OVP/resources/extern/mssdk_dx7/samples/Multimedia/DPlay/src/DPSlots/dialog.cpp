//-----------------------------------------------------------------------------
// File: dialog.cpp
//
// Desc: Creates a dialog to query the user for connection settings
//       and establish a connection.
//
// Copyright (C) 1996-1999 Microsoft Corporation.  All Rights Reserved.
//-----------------------------------------------------------------------------
#define INITGUID
#include <windows.h>
#include <windowsx.h>
#include <wtypes.h>
#include <cguid.h>

#include <dplobby.h>    // to init guids
#include "dpslots.h"
#include "resource.h"

// constants
const UINT      TIMERID         = 1;            // timer ID to use
const UINT      TIMERINTERVAL   = 1000;         // timer interval

// structures

// server configuration information
struct SERVERCONFIG
{
    CHAR strServerName[MAXSTRLEN];
    CHAR strDatabaseFile[MAXSTRLEN];
    CHAR strSecurityProvider[MAXSTRLEN];
    BOOL bRequireSecureLogin;
};

// client login information
struct CLIENTLOGIN
{
    CHAR strPlayerName[MAXSTRLEN];
    CHAR strUserName[MAXSTRLEN];
    CHAR strPassword[MAXSTRLEN];
    CHAR strDomain[MAXSTRLEN];
};

// prototypes
BOOL FAR PASCAL DirectPlayEnumConnectionsCallback( const GUID* pSPGUID,
                        VOID* pConnection, DWORD dwConnectionSize,
                        const DPNAME* pName, DWORD dwFlags, VOID* pContext );
BOOL CALLBACK   ConnectWndProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                                LPARAM lParam );
HRESULT CreateDirectPlayInterface( LPDIRECTPLAY4A* ppDPlay );
HRESULT DestroyDirectPlayInterface( HWND hWnd, LPDIRECTPLAY4A pDPlay );
HRESULT HostSession( LPDIRECTPLAY4A pDPlay, SERVERCONFIG* pServerConfig,
                     DPLAYINFO* pDPInfo );
HRESULT JoinSession( LPDIRECTPLAY4A pDPlay, GUID* pSessionInstanceGUID,
                     CLIENTLOGIN* pClientLogin, DPLAYINFO* pDPInfo );
HRESULT EnumSessions( HWND hWnd, LPDIRECTPLAY4A pDPlay );
HRESULT GetConnection( HWND hWnd, VOID** ppConnection );
VOID    DeleteConnectionList( HWND hWnd );
HRESULT GetSessionInstanceGuid( HWND hWnd, GUID* pguidSessionInstanceGUID );
VOID    SelectSessionInstance( HWND hWnd, GUID* pguidSessionInstanceGUID );
VOID    DeleteSessionInstanceList( HWND hWnd);
BOOL    GetServerConfig( HWND hWnd, SERVERCONFIG* pServerConfig );
BOOL    GetClientLogin( HWND hWnd, CLIENTLOGIN* pClientLogin );
HRESULT GetSessionDesc( LPDIRECTPLAY4A pDPlay, DPSESSIONDESC2** ppSessionDesc );




//-----------------------------------------------------------------------------
// Name: ConnectUsingDialog()
// Desc:
//-----------------------------------------------------------------------------
HRESULT ConnectUsingDialog( HINSTANCE hInstance, DPLAYINFO* pDPInfo )
{
    // Ask user for connection settings
    if( DialogBoxParam( hInstance, MAKEINTRESOURCE(IDD_CONNECTDIALOG),
                        NULL, (DLGPROC)ConnectWndProc, (LPARAM)pDPInfo ) )
        return DP_OK;
    else
        return DPERR_USERCANCEL;
}




//-----------------------------------------------------------------------------
// Name: ConnectWndProc()
// Desc:
//-----------------------------------------------------------------------------
BOOL CALLBACK ConnectWndProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                              LPARAM lParam )
{
    static DPLAYINFO*     pDPInfo;
    static LPDIRECTPLAY4A pDPlay;
    static UINT           idTimer = 0;
    GUID         guidSessionInstance;
    SERVERCONFIG serverConfig;
    CLIENTLOGIN  clientLogin;
    VOID*        pConnection = NULL;
    DWORD        dwNameSize;
    HRESULT      hr;

    switch( uMsg )
    {
        case WM_INITDIALOG:
            // Save the connection info pointer
            pDPInfo = (DPLAYINFO*)lParam;
            pDPlay  = NULL;

            // Create an IDirectPlay3 interface
            hr = CreateDirectPlayInterface( &pDPlay );

            if( FAILED(hr) )
                goto SETUP_FAILURE;

            // Set first item in the connections combo box
            SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_ADDSTRING, 0,
                                (LPARAM)"<Select a service provider>");
            SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_SETITEMDATA, 0, 0 );
            SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_SETCURSEL, 0, 0 );

            // Put all the available connections in a combo box
            pDPlay->EnumConnections( &DPSLOTS_GUID,
                                     DirectPlayEnumConnectionsCallback,
                                     hWnd, 0 );

            // Setup initial button state
            EnableDlgButton( hWnd, IDC_HOSTBUTTON, FALSE );
            EnableDlgButton( hWnd, IDC_JOINBUTTON, FALSE );
            break;
    
    SETUP_FAILURE:
            ErrorBox("Could not create DirectPlay object because of error %s", hr);
            EndDialog( hWnd, FALSE );
            break;

        case WM_DESTROY:
            // delete information stored along with the lists
            DeleteConnectionList( hWnd );
            DeleteSessionInstanceList( hWnd );
            break;

        case WM_TIMER:
            // Refresh the session list
            // Guard against leftover timer messages after timer has been killed
            if( idTimer )
            {
                hr = EnumSessions( hWnd, pDPlay );
                if( FAILED(hr) && hr != DPERR_CONNECTING )
                {
                    KillTimer( hWnd, idTimer );
                    idTimer = 0;
                    if( hr != DPERR_USERCANCEL )
                        ErrorBox("Enumerating sessions has failed; error %s", hr);
                }
            }
            break;

        case WM_COMMAND:
            switch(LOWORD(wParam))
            {
                case IDC_SPCOMBO:
                    switch (HIWORD(wParam))
                    {
                        case CBN_SELCHANGE:
                            // Service provider changed, so rebuild display and
                            // delete any existing DirectPlay interface
                            if( idTimer )
                            {
                                KillTimer( hWnd, idTimer );
                                idTimer = 0;
                            }
                            hr = DestroyDirectPlayInterface( hWnd, pDPlay );
                            pDPlay = NULL;

                            // Get pointer to the selected connection
                            hr = GetConnection( hWnd, &pConnection );
                            if( FAILED(hr) )
                                goto SP_FAILURE;

                            if( pConnection )
                            {
                                hr = CreateDirectPlayInterface( &pDPlay );

                                if( FAILED(hr) || NULL == pDPlay )
                                    goto SP_FAILURE;

                                // Initialize the connection
                                hr = pDPlay->InitializeConnection( pConnection, 0 );
                                if( FAILED(hr) )
                                    goto SP_FAILURE;

                                // OK to host now
                                EnableDlgButton( hWnd, IDC_HOSTBUTTON, TRUE );

                                // Start enumerating the sessions
                                hr = EnumSessions( hWnd, pDPlay );
                                if( FAILED(hr) )
                                    goto SP_FAILURE;

                                // Set a timer to refresh the session list
                                idTimer = SetTimer( hWnd, TIMERID, TIMERINTERVAL, NULL );
                            }
                            else
                            {
                                // They've selected the generic option "<Select
                                // a service provider>"
                                EnableDlgButton( hWnd, IDC_HOSTBUTTON, FALSE );
                                EnableDlgButton( hWnd, IDC_JOINBUTTON, FALSE );
                            }
                            break;
                        }
                        break;

                    SP_FAILURE:
                        if( hr != DPERR_USERCANCEL )
                            ErrorBox("Could not select service provider because of error %s", hr);
                        break;

                    case IDC_HOSTBUTTON:
                        // Should have an interface by now
                        if( pDPlay == NULL )
                            break;

                        if( idTimer )
                        {
                            KillTimer( hWnd, idTimer );
                            idTimer = 0;
                        }
                        // Get server configuration from user
                        ZeroMemory( &serverConfig, sizeof(SERVERCONFIG) );
                        if( !GetServerConfig( hWnd, &serverConfig ) )
                            break;

                        // Host a new session on this service provider
                        hr = HostSession( pDPlay, &serverConfig, pDPInfo );
                        if( FAILED(hr) )
                            goto HOST_FAILURE;

                        // Dismiss dialog if we succeeded in hosting
                        EndDialog( hWnd, TRUE );
                        break;

                    HOST_FAILURE:
                        ErrorBox("Could not host session because of error %s", hr);
                        break;

                    case IDC_JOINBUTTON:
                        // Should have an interface by now
                        if( pDPlay == NULL )
                            break;

                        if( idTimer )
                        {
                            KillTimer( hWnd, idTimer );
                            idTimer = 0;
                        }
                        // Get guid of selected session instance
                        hr = GetSessionInstanceGuid( hWnd, &guidSessionInstance );
                        if( FAILED(hr) )
                            goto JOIN_FAILURE;

                        // Get server configuration from user
                        ZeroMemory( &clientLogin, sizeof(CLIENTLOGIN) );

                        // Use user name for player name
                        dwNameSize = MAXSTRLEN;
                        GetUserName( clientLogin.strPlayerName, &dwNameSize );
                                    
                        // Join this session
                        hr = JoinSession( pDPlay, &guidSessionInstance,
                                          &clientLogin, pDPInfo );

                        // Need to ask user for credentials
                        if( hr == DPERR_LOGONDENIED )
                        {
                            if( !GetClientLogin( hWnd, &clientLogin ) )
                                break;

                            // Try again, this time with credentials
                            hr = JoinSession( pDPlay, &guidSessionInstance,
                                              &clientLogin, pDPInfo );
                        }

                        if( FAILED(hr) )
                            goto JOIN_FAILURE;

                        // Dismiss dialog if we succeeded in joining
                        EndDialog( hWnd, TRUE );
                        break;

                    JOIN_FAILURE:
                        ErrorBox("Could not join session because of error %s", hr);
                        break;

                case IDCANCEL:
                        if( idTimer )
                        {
                            KillTimer( hWnd, idTimer );
                            idTimer = 0;
                        }
                        // Delete any interface created if cancelling
                        hr = DestroyDirectPlayInterface( hWnd, pDPlay );
                        pDPlay = NULL;

                        EndDialog( hWnd, FALSE );
                        break;
                        
                default:
                    // Message was not handled 
                    return FALSE;
            }
            break;
            
        default:
            // Message was not handled 
            return FALSE;
    }

    // Message was handled 
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: DirectPlayEnumConnectionsCallback()
// Desc:
//-----------------------------------------------------------------------------
BOOL FAR PASCAL DirectPlayEnumConnectionsCallback( const GUID* pguidSP,
                           VOID* pConnection, DWORD dwConnectionSize,
                           const DPNAME* pName, DWORD dwFlags, VOID* pContext )
{
    HWND    hWnd = (HWND)pContext;
    LRESULT iIndex;
    VOID*   pConnectionBuffer;

    // Store service provider name in combo box
    iIndex = SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_ADDSTRING, 0, 
                                 (LPARAM)pName->lpszShortNameA );
    if( iIndex == CB_ERR )
        return TRUE;

    // Make space for Connection Shortcut
    pConnectionBuffer = GlobalAllocPtr( GHND, dwConnectionSize );
    if( pConnectionBuffer == NULL )
        return TRUE;

    // Store pointer to GUID in combo box
    memcpy( pConnectionBuffer, pConnection, dwConnectionSize );
    SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_SETITEMDATA, (WPARAM)iIndex, 
                        (LPARAM)pConnectionBuffer );

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: CreateDirectPlayInterface()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CreateDirectPlayInterface( LPDIRECTPLAY4A* ppDPlay )
{
    // Create an IDirectPlay interface
    return CoCreateInstance( CLSID_DirectPlay, NULL, CLSCTX_INPROC_SERVER,
                             IID_IDirectPlay4A, (VOID**)ppDPlay );
}




//-----------------------------------------------------------------------------
// Name: DestroyDirectPlayInterface()
// Desc:
//-----------------------------------------------------------------------------
HRESULT DestroyDirectPlayInterface( HWND hWnd, LPDIRECTPLAY4A pDPlay )
{
    if( NULL == pDPlay )
        return DP_OK;

    DeleteSessionInstanceList( hWnd );
    EnableDlgButton( hWnd, IDC_JOINBUTTON, FALSE );

    return pDPlay->Release();
}




//-----------------------------------------------------------------------------
// Name: HostSession()
// Desc:
//-----------------------------------------------------------------------------
HRESULT HostSession( LPDIRECTPLAY4A pDPlay, SERVERCONFIG* pServerConfig,
                     DPLAYINFO* pDPInfo )
{
    DPID            dpidPlayer;
    DPSESSIONDESC2  sessionDesc;
    DPSECURITYDESC  securityDesc;
    DPSECURITYDESC* pSecurityDesc;
    HRESULT         hr;

    // Check for valid interface
    if( pDPlay == NULL)
        return DPERR_INVALIDOBJECT;

    // Host a new session
    ZeroMemory(&sessionDesc, sizeof(DPSESSIONDESC2));
    sessionDesc.dwSize  = sizeof(DPSESSIONDESC2);
    sessionDesc.dwFlags = SESSIONFLAGS( pServerConfig->bRequireSecureLogin );
    sessionDesc.guidApplication  = DPSLOTS_GUID;
    sessionDesc.dwMaxPlayers     = 0;
    sessionDesc.lpszSessionNameA = pServerConfig->strServerName;

    // Assume no security descriptor will be needed
    pSecurityDesc = NULL;

    if( pServerConfig->bRequireSecureLogin )
    {
        // A service provider string was entered, so use it
        if( lstrlen( pServerConfig->strSecurityProvider ) )
        {
            ZeroMemory(&securityDesc, sizeof(DPSECURITYDESC));
            securityDesc.dwSize = sizeof(DPSECURITYDESC);
            securityDesc.dwFlags = 0;
            securityDesc.lpszSSPIProviderA = pServerConfig->strSecurityProvider;
            pSecurityDesc = &securityDesc;
        }
    }

    // Host the session
    hr = pDPlay->SecureOpen( &sessionDesc, DPOPEN_CREATE, pSecurityDesc, NULL );
    if( FAILED(hr) )
        goto FAILURE;

    // Create a server player with no name
    hr = pDPlay->CreatePlayer( &dpidPlayer, NULL, pDPInfo->hPlayerEvent, NULL,
                               0, SERVERPLAYERFLAGS );
    if( FAILED(hr) )
        goto FAILURE;

    // Return connection info
    pDPInfo->pDPlay     = pDPlay;
    pDPInfo->dpidPlayer = dpidPlayer;
    pDPInfo->bIsHost    = TRUE;
    pDPInfo->bIsSecure  = pServerConfig->bRequireSecureLogin;

    // Save database name in global
    lstrcpy( g_strDatabaseName, pServerConfig->strDatabaseFile );

    return DP_OK;

FAILURE:
    pDPlay->Close();
    return hr;
}




//-----------------------------------------------------------------------------
// Name: JoinSession()
// Desc:
//-----------------------------------------------------------------------------
HRESULT JoinSession( LPDIRECTPLAY4A pDPlay, GUID* pSessionInstanceGUID,
                     CLIENTLOGIN* pClientLogin, DPLAYINFO* pDPInfo )
{
    DPID            dpidPlayer;
    DPNAME          dpName;
    DPSESSIONDESC2  sessionDesc;
    DPSESSIONDESC2* pSessionDesc = NULL;
    DPCREDENTIALS   credentials;
    DPCREDENTIALS*  pCredentials;
    HRESULT         hr;

    // Check for valid interface
    if( pDPlay == NULL)
        return DPERR_INVALIDOBJECT;

    // Join existing session
    ZeroMemory( &sessionDesc, sizeof(DPSESSIONDESC2) );
    sessionDesc.dwSize = sizeof(DPSESSIONDESC2);
    sessionDesc.guidInstance = *pSessionInstanceGUID;

    // Assume no credentials are going to be used
    pCredentials = NULL;

    // Setup credentials
    ZeroMemory( &credentials, sizeof(DPCREDENTIALS) );
    credentials.dwSize = sizeof(DPCREDENTIALS);
    credentials.dwFlags = 0;

    if( lstrlen( pClientLogin->strUserName ) )
    {
        credentials.lpszUsernameA = pClientLogin->strUserName;
        pCredentials = &credentials;
    }

    if( lstrlen( pClientLogin->strPassword ) )
    {
        credentials.lpszPasswordA = pClientLogin->strPassword; 
        pCredentials = &credentials;
    }

    if( lstrlen( pClientLogin->strDomain ) )
    {
        credentials.lpszDomainA = pClientLogin->strDomain; 
        pCredentials = &credentials;
    }

    // Join the session 
    hr = pDPlay->SecureOpen( &sessionDesc, DPOPEN_JOIN, NULL, pCredentials );
    
    if( FAILED(hr) )
        goto FAILURE;

    // Fill out name structure
    ZeroMemory( &dpName, sizeof(DPNAME) );
    dpName.dwSize = sizeof(DPNAME);
    dpName.lpszShortNameA = pClientLogin->strPlayerName;
    dpName.lpszLongNameA = NULL;

    // Create a player with this name
    hr = pDPlay->CreatePlayer( &dpidPlayer, &dpName, pDPInfo->hPlayerEvent,
                               NULL, 0, CLIENTPLAYERFLAGS );
    if( FAILED(hr) )
        goto FAILURE;

    // Get the session desc
    hr = GetSessionDesc( pDPlay, &pSessionDesc );
    if( FAILED(hr) )
        goto FAILURE;

    // Return connection info
    pDPInfo->pDPlay     = pDPlay;
    pDPInfo->dpidPlayer = dpidPlayer;
    pDPInfo->bIsHost    = FALSE;

    if( pSessionDesc->dwFlags & DPSESSION_SECURESERVER )
        pDPInfo->bIsSecure = TRUE;
    else
        pDPInfo->bIsSecure = FALSE;

    GlobalFreePtr( pSessionDesc );

    return DP_OK;

FAILURE:
    if( pSessionDesc )
        GlobalFreePtr( pSessionDesc );

    pDPlay->Close();
    return hr;
}




//-----------------------------------------------------------------------------
// Name: EnumSessionsCallback()
// Desc:
//-----------------------------------------------------------------------------
BOOL FAR PASCAL EnumSessionsCallback( const DPSESSIONDESC2* pSessionDesc,
                                      DWORD* pdwTimeOut, DWORD dwFlags,
                                      VOID* pContext )
{
    HWND  hWnd = (HWND)pContext;
    GUID* pGuid;
    LONG  iIndex;

    // See if last session has been enumerated
    if( dwFlags & DPESC_TIMEDOUT )
        return FALSE;

    // Store session name in list
    iIndex = SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_ADDSTRING, 
                                 0, (LPARAM)pSessionDesc->lpszSessionNameA );
    if( iIndex == LB_ERR )
        return TRUE;

    // Make space for session instance guid
    pGuid = (GUID*)GlobalAllocPtr( GHND, sizeof(GUID) );
    if( pGuid == NULL )
        return TRUE;

    // Store pointer to guid in list
    *pGuid = pSessionDesc->guidInstance;
    SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_SETITEMDATA,
                        (WPARAM)iIndex, (LPARAM)pGuid );

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: EnumSessions()
// Desc:
//-----------------------------------------------------------------------------
HRESULT EnumSessions( HWND hWnd, LPDIRECTPLAY4A pDPlay )
{
    DPSESSIONDESC2 sessionDesc;
    GUID           guidSessionInstance;
    LONG           iIndex;
    HRESULT        hr;

    // Check for valid interface
    if( pDPlay == NULL)
        return DPERR_INVALIDOBJECT;

    // Get guid of currently selected session
    guidSessionInstance = GUID_NULL;
    hr = GetSessionInstanceGuid( hWnd, &guidSessionInstance );

    // Delete existing session list
    DeleteSessionInstanceList( hWnd );

    // Add sessions to session list
    ZeroMemory( &sessionDesc, sizeof(DPSESSIONDESC2) );
    sessionDesc.dwSize = sizeof(DPSESSIONDESC2);
    sessionDesc.guidApplication = DPSLOTS_GUID;

    hr = pDPlay->EnumSessions( &sessionDesc, 0, EnumSessionsCallback,
                               hWnd,
                               DPENUMSESSIONS_AVAILABLE|DPENUMSESSIONS_ASYNC );

    // Select the session that was previously selected
    SelectSessionInstance( hWnd, &guidSessionInstance );

    // Hilite "Join" button only if there are sessions to join
    iIndex = SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_GETCOUNT, 0, 0 );

    EnableDlgButton( hWnd, IDC_JOINBUTTON, (iIndex > 0) ? TRUE : FALSE );

    return hr;
}




//-----------------------------------------------------------------------------
// Name: GetConnection()
// Desc:
//-----------------------------------------------------------------------------
HRESULT GetConnection( HWND hWnd, VOID** ppConnection )
{
    LONG iIndex;

    // get index of the item currently selected in the combobox
    iIndex = SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_GETCURSEL, 0, 0 );
    if( iIndex == CB_ERR )
        return DPERR_GENERIC;

    // Get the pointer to the connection shortcut associated with
    // the item
    iIndex = SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_GETITEMDATA,
                                 (WPARAM)iIndex, 0 );
    if( iIndex == CB_ERR )
        return DPERR_GENERIC;

    *ppConnection = (VOID*)iIndex;

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: DeleteConnectionList()
// Desc:
//-----------------------------------------------------------------------------
VOID DeleteConnectionList( HWND hWnd )
{
    WPARAM i;
    LONG   pData;
    
    // Destroy the GUID's stored with each service provider name
    i = 0;
    while( TRUE )
    {
        // Get data pointer stored with item
        pData = SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_GETITEMDATA,
                                    (WPARAM) i, 0 );
        if( pData == CB_ERR )
            break;

        if( pData != 0)
            GlobalFreePtr( (VOID*)pData );

        i += 1;
    }

    // Delete all items in combo box
    SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_RESETCONTENT, 0, 0 );
}




//-----------------------------------------------------------------------------
// Name: GetSessionInstanceGuid()
// Desc:
//-----------------------------------------------------------------------------
HRESULT GetSessionInstanceGuid( HWND hWnd, GUID* pguidSessionInstance )
{
    LONG iIndex;

    // get guid for session
    iIndex = SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_GETCURSEL, 0, 0 );
    if( iIndex == LB_ERR )
        return DPERR_GENERIC;

    iIndex = SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_GETITEMDATA,
                                 (WPARAM)iIndex, 0 );
    if( (iIndex == LB_ERR) || (iIndex == 0) )
        return DPERR_GENERIC;

    *pguidSessionInstance = *((GUID*)iIndex);

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: DeleteSessionInstanceList()
// Desc:
//-----------------------------------------------------------------------------
VOID DeleteSessionInstanceList( HWND hWnd )
{
    WPARAM i;
    LONG   pData;
    
    // Destroy the GUID's stored with each session name
    i = 0;
    while( TRUE )
    {
        // get data pointer stored with item
        pData = SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_GETITEMDATA,
                                    (WPARAM) i, (LPARAM)0 );
        if( pData == CB_ERR )
            break;
        if( pData == 0 )
            continue;

        GlobalFreePtr( (VOID*)pData );
        i += 1;
    }

    // Delete all items in list
    SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_RESETCONTENT, 0, 0 );
}




//-----------------------------------------------------------------------------
// Name: SelectSessionInstance()
// Desc:
//-----------------------------------------------------------------------------
VOID SelectSessionInstance( HWND hWnd, GUID* pguidSessionInstance )
{
    WPARAM i, iIndex;
    LONG   pData;
    
    // Loop over the GUID's stored with each session name
    // to find the one that matches what was passed in
    i = 0;
    iIndex = 0;
    while( TRUE )
    {
        // Get data pointer stored with item
        pData = SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_GETITEMDATA,
                                    (WPARAM)i, 0 );
        if( pData == CB_ERR)
            break;

        if( pData == 0)
            continue;

        // Guid matches
        if( IsEqualGUID( *pguidSessionInstance, *((GUID*)pData ) ) )
        {
            iIndex = i;
            break;
        }

        i += 1;
    }

    // Select this item
    SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_SETCURSEL, (WPARAM)iIndex, 0 );
}




//-----------------------------------------------------------------------------
// Name: GetSessionDesc()
// Desc:
//-----------------------------------------------------------------------------
HRESULT GetSessionDesc( LPDIRECTPLAY4A pDPlay, DPSESSIONDESC2** ppSessionDesc )
{
    DPSESSIONDESC2* pSessionDesc = NULL;
    DWORD           dwSessionDescSize;
    HRESULT         hr;

    // Get size of session desc
    hr = pDPlay->GetSessionDesc( NULL, &dwSessionDescSize );
    if( hr != DPERR_BUFFERTOOSMALL )
        goto FAILURE;

    // Make room for it
    pSessionDesc = (DPSESSIONDESC2*)GlobalAllocPtr( GHND, dwSessionDescSize );
    if( pSessionDesc == NULL )
    {
        hr = DPERR_OUTOFMEMORY;
        goto FAILURE;
    }

    // Get the session desc
    hr = pDPlay->GetSessionDesc( pSessionDesc, &dwSessionDescSize );
    if( FAILED(hr) )
        goto FAILURE;

    // Return account description
    *ppSessionDesc = pSessionDesc;

    return S_OK;

FAILURE:
    if( pSessionDesc )
        GlobalFreePtr( pSessionDesc );

    return hr;
}




//-----------------------------------------------------------------------------
// Name: InitializeServerConfigWindow()
// Desc:
//-----------------------------------------------------------------------------
VOID InitializeServerConfigWindow( HWND hWnd )
{
    CHAR  strSessionName[MAXSTRLEN];
    DWORD dwNameSize = MAXSTRLEN;

    // Use username for default
    if( GetComputerName( strSessionName, &dwNameSize ) )
        SetDlgItemText( hWnd, IDC_SERVERNAMEEDIT, strSessionName );
    
    // Use default name
    SetDlgItemText( hWnd, IDC_DATABASEFILEEDIT, DEFAULTDATABASE );

    // Security off by default
    CheckDlgItem( hWnd, IDC_SECURECHECK, FALSE );
}




//-----------------------------------------------------------------------------
// Name: SaveServerConfig() 
// Desc:
//-----------------------------------------------------------------------------
VOID SaveServerConfig( HWND hWnd, SERVERCONFIG* pServerConfig )
{
    // Get strings from dialog
    GetDlgItemText( hWnd, IDC_SERVERNAMEEDIT, pServerConfig->strServerName, MAXSTRLEN );
    GetDlgItemText( hWnd, IDC_DATABASEFILEEDIT, pServerConfig->strDatabaseFile, MAXSTRLEN );
    GetDlgItemText( hWnd, IDC_SECURITYPROVIDEREDIT, pServerConfig->strSecurityProvider, MAXSTRLEN );

    if( DlgItemIsChecked(hWnd, IDC_SECURECHECK) )
        pServerConfig->bRequireSecureLogin = TRUE;
    else
        pServerConfig->bRequireSecureLogin = FALSE;
}




//-----------------------------------------------------------------------------
// Name: ServerConfigWndProc()
// Desc:
//-----------------------------------------------------------------------------
BOOL CALLBACK ServerConfigWndProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                                   LPARAM lParam )
{
    static SERVERCONFIG* pServerConfig;

    switch( uMsg )
    {
        case WM_INITDIALOG:
            // Globals are passed in lParam
            pServerConfig = (SERVERCONFIG*)lParam;

            // Initialize dialog with appropriate information
            InitializeServerConfigWindow( hWnd );
            break;

        case WM_DESTROY:
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDOK:
                    // save changes they made
                    SaveServerConfig( hWnd, pServerConfig );
            
                    // Return success
                    EndDialog( hWnd, TRUE );
                    break;

                case IDCANCEL:
                    // Return failure
                    EndDialog( hWnd, FALSE );
                    break;
            }
            break;
    }

    // Allow for default processing
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: GetServerConfig()
// Desc:
//-----------------------------------------------------------------------------
BOOL GetServerConfig( HWND hWnd, SERVERCONFIG* pServerConfig )
{
    // Collect server config from dialog
    return DialogBoxParam( ghInstance, MAKEINTRESOURCE(IDD_SERVERCONFIGDIALOG),
                           hWnd, (DLGPROC)ServerConfigWndProc,
                           (LPARAM)pServerConfig );
}




//-----------------------------------------------------------------------------
// Name: InitializeClientLoginWindow()
// Desc:
//-----------------------------------------------------------------------------
VOID InitializeClientLoginWindow( HWND hWnd )
{
    CHAR  strPlayerName[MAXSTRLEN];
    DWORD dwNameSize;

    // Use user name for player name
    dwNameSize = MAXSTRLEN;
    if( GetUserName( strPlayerName, &dwNameSize ) )
        SetDlgItemText( hWnd, IDC_USERNAMEEDIT, strPlayerName );
}




//-----------------------------------------------------------------------------
// Name: SaveClientLogin()
// Desc:
//-----------------------------------------------------------------------------
VOID SaveClientLogin( HWND hWnd, CLIENTLOGIN* pClientLogin )
{
    // Get strings from dialog
    GetDlgItemText( hWnd, IDC_USERNAMEEDIT, pClientLogin->strUserName, MAXSTRLEN );
    GetDlgItemText( hWnd, IDC_PASSWORDEDIT, pClientLogin->strPassword, MAXSTRLEN );
    GetDlgItemText( hWnd, IDC_DOMAINEDIT,   pClientLogin->strDomain,   MAXSTRLEN );
}




//-----------------------------------------------------------------------------
// Name: ClientLoginWndProc()
// Desc:
//-----------------------------------------------------------------------------
BOOL CALLBACK ClientLoginWndProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                                  LPARAM lParam)
{
    static CLIENTLOGIN* pClientLogin;

    switch( uMsg )
    {
        case WM_INITDIALOG:
            // Globals are passed in lParam
            pClientLogin = (CLIENTLOGIN*)lParam;

            // Initialize dialog with appropriate information
            InitializeClientLoginWindow( hWnd );
            break;

        case WM_DESTROY:
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDOK:
                    // save changes they made
                    SaveClientLogin( hWnd, pClientLogin );
                    // Return success
                    EndDialog( hWnd, TRUE );
                    break;

                case IDCANCEL:
                    // Return failure
                    EndDialog( hWnd, FALSE );
                    break;
            }
            break;
    }

    // Allow for default processing
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: GetClientLogin()
// Desc:
//-----------------------------------------------------------------------------
BOOL GetClientLogin( HWND hWnd, CLIENTLOGIN* pClientLogin )
{
    // Collect server config from dialog
    return DialogBoxParam( ghInstance, MAKEINTRESOURCE(IDD_CLIENTLOGINDIALOG),
                           hWnd, (DLGPROC)ClientLoginWndProc,
                           (LPARAM)pClientLogin );
}



