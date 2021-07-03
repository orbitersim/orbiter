//-----------------------------------------------------------------------------
// File: Override.cpp
//
// Desc: Implementation of a DirectPlay launching utility
//
// Copyright (C) 1996-1999 Microsoft Corporation.  All Rights Reserved.
//-----------------------------------------------------------------------------
#define INITGUID
#include <windows.h>
#include <windowsx.h>
#include <objbase.h>
#include <wtypes.h>
#include <cguid.h>
#include "dplay.h"
#include "dplobby.h"
#include "resource.h"

#if defined(UNICODE) || defined(_UNICODE)
#error This app does not support UNICODE
#endif




//-----------------------------------------------------------------------------
// Constants
//-----------------------------------------------------------------------------
#define NAMEMAX         200     // string size
#define TIMERID         1       // timer ID to use
#define TIMERINTERVAL   1000    // timer interval
#define MAXSTRLEN       200     // maximum size of temp strings

struct STATUSCONTEXT
{
    LPDIRECTPLAY4A pDP;
    GUID           guidInstance;
};

// guid for this application
DEFINE_GUID(OVERRIDE_GUID, 
0x126e6180, 0xd307, 0x11d0, 0x9c, 0x4f, 0x0, 0xa0, 0xc9, 0x5, 0x42, 0x5e );




//-----------------------------------------------------------------------------
// Function prototypes
//-----------------------------------------------------------------------------
BOOL CALLBACK OverrideWndProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                               LPARAM lParam );
BOOL CALLBACK SessionsWndProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                               LPARAM lParam );
BOOL FAR PASCAL EnumConnectionsCallback( const GUID* pSPGUID, VOID* pConnection,
                                         DWORD dwConnectionSize,
                                         const DPNAME* pName, DWORD dwFlags,
                                         VOID* pContext );
HRESULT InitializeOverrideWindow( HWND hWnd, LPDIRECTPLAYLOBBY3A* ppDPLobby );
VOID    DestroyOverrideWindow( HWND hWnd, LPDIRECTPLAY4A pDP,
                               LPDIRECTPLAYLOBBY3A pDPLobby );
HRESULT UpdateAddressInfo( HWND hWnd, LPDIRECTPLAYLOBBY3A pDPLobby );
HRESULT DoHostOrJoin( HINSTANCE hInstance, HWND hWnd,
                      LPDIRECTPLAYLOBBY3A pDPLobby, BOOL bHost,
                      LPDIRECTPLAY4A* ppDP );
HRESULT GetServiceProviderGuid( HWND hWnd, GUID* pServiceProviderGUID );
VOID    DeleteServiceProviderCombo( HWND hWnd );
HRESULT FillModemComboBox( HWND hWnd, LPDIRECTPLAYLOBBY3A pDPLobby );
BOOL    DlgItemIsChecked( HWND hDlg, int nIDDlgItem );
VOID    EnableDlgButton( HWND hDlg, int nIDDlgItem, BOOL bEnable );
VOID    ErrorBox( LPSTR strErrorStr, HRESULT hr );




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Main windows entry point.
//-----------------------------------------------------------------------------
int WINAPI WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    LPSTR lpCmdLine, int nCmdShow )
{
    // Initialize COM library
    HRESULT hr = CoInitialize( NULL );
    if( FAILED(hr) )
    {
        ErrorBox( "CoInitialize failed. Error 0x%0X", hr );
        return 0;
    }

    int iResult = DialogBoxParam( hInstance,
                                  MAKEINTRESOURCE(IDD_OVERRIDEDIALOG), NULL,
                                  (DLGPROC)OverrideWndProc, (LPARAM)hInstance );

    // Uninitialize the COM library
    CoUninitialize();

    return iResult;
}




//-----------------------------------------------------------------------------
// Name: OverrideWndProc()
// Desc: Message callback function for Override dialog.
//-----------------------------------------------------------------------------
BOOL CALLBACK OverrideWndProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                               LPARAM lParam )
{
    static HINSTANCE           hInstance;
    static LPDIRECTPLAY4A      pDP;
    static LPDIRECTPLAYLOBBY3A pDPLobby;
    HRESULT hr;

    switch( uMsg )
    {
        case WM_INITDIALOG:
            // Save the instance handle
            hInstance = (HINSTANCE)lParam;
            
            // Initialize dialog with launcher information
            pDP = NULL;
            pDPLobby = NULL;
            hr = InitializeOverrideWindow( hWnd, &pDPLobby );
            if( FAILED(hr) )
                SetDlgItemText( hWnd, IDC_STATUSEDIT, "Could not initialize DirectPlay" );
            return TRUE;

        case WM_DESTROY:
            // Destroy launcher information in dialog
            DestroyOverrideWindow( hWnd, pDP, pDPLobby );
            break;  // continue with default handling

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDC_SPCOMBO:
                    switch (HIWORD(wParam))
                    {
                        case CBN_SELCHANGE:
                            UpdateAddressInfo( hWnd, pDPLobby );
                            return TRUE;
                    }
                    break;

                case IDC_HOSTBUTTON:
                    SetDlgItemText( hWnd, IDC_STATUSEDIT, "Hosting..." );
                    hr = DoHostOrJoin( hInstance, hWnd, pDPLobby, TRUE, &pDP );
                    if( FAILED(hr) )
                        SetDlgItemText( hWnd, IDC_STATUSEDIT, "Failed to host" );
                    else
                        SetDlgItemText( hWnd, IDC_STATUSEDIT, "Host successfull" );
                    return TRUE;

            case IDC_JOINBUTTON:
                SetDlgItemText( hWnd, IDC_STATUSEDIT, "Joining..." );
                hr = DoHostOrJoin( hInstance, hWnd, pDPLobby, FALSE, &pDP );
                if FAILED(hr)
                    SetDlgItemText( hWnd, IDC_STATUSEDIT, "Failed to join" );
                else
                    SetDlgItemText( hWnd, IDC_STATUSEDIT, "Join successfull" );
                return TRUE;

            case IDCANCEL:
                // Return failure
                EndDialog( hWnd, TRUE );
                return TRUE;
        }
        break;
    }

    // Allow for default processing
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: InitializeOverrideWindow()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT InitializeOverrideWindow( HWND hWnd, LPDIRECTPLAYLOBBY3A* ppDPLobby )
{
    LPDIRECTPLAY4A      pDP      = NULL;
    LPDIRECTPLAYLOBBY3A pDPLobby = NULL;
    HRESULT hr;
        
    // Create a temporary ANSI DirectPlay interface
    hr = CoCreateInstance( CLSID_DirectPlay, NULL, CLSCTX_INPROC_SERVER, 
                           IID_IDirectPlay4A, (VOID**)&pDP );
    if( FAILED(hr) )
        return hr;

    // Get ANSI DirectPlayLobby interface
    hr = CoCreateInstance( CLSID_DirectPlayLobby, NULL, CLSCTX_INPROC_SERVER,
                           IID_IDirectPlayLobby3A, (VOID**)&pDPLobby );
    if( FAILED(hr) )
    {
        pDP->Release();
        return hr;
    }

    // Put all the service providers in combo box
    pDP->EnumConnections( NULL, EnumConnectionsCallback, hWnd, 0 );
    SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_SETCURSEL, 0, 0 );

    // Fill modem combo box with available modems
    FillModemComboBox( hWnd, pDPLobby );

    // Limit Port number edit field to 5 digits
    SendDlgItemMessage( hWnd, IDC_PORTEDIT, EM_LIMITTEXT, 5, 0 );

    // Update display first service provider
    UpdateAddressInfo( hWnd, pDPLobby );

    // Return the ANSI lobby interface
    *ppDPLobby = pDPLobby;

    pDP->Release();
    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: DestroyOverrideWindow()
// Desc: 
//-----------------------------------------------------------------------------
VOID DestroyOverrideWindow( HWND hWnd, LPDIRECTPLAY4A pDP, 
                            LPDIRECTPLAYLOBBY3A pDPLobby )
{
    // Delete combo box data items
    DeleteServiceProviderCombo( hWnd );

    // Release the dplay interface
    if( pDP )
        pDP->Release();

    // Release the lobby interface
    if( pDPLobby )
        pDPLobby->Release();
}




//-----------------------------------------------------------------------------
// Name: EnumConnectionsCallback()
// Desc: Enumerates service providers registered with DirectPlay.
//-----------------------------------------------------------------------------
BOOL FAR PASCAL EnumConnectionsCallback( const GUID* pSPGUID, VOID* pConnection,
                                         DWORD dwConnectionSize,
                                         const DPNAME* pName, DWORD dwFlags,
                                         VOID* pContext )
{
    HWND    hWnd = (HWND)pContext;
    LRESULT iIndex;
    GUID*   pGuid;

    // Store service provider name in combo box
    iIndex = SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_ADDSTRING, 0,
                                 (LPARAM)pName->lpszShortNameA );
    if( iIndex == CB_ERR )
        return TRUE;

    // Make space for application GUID
    pGuid = (GUID*)GlobalAllocPtr( GHND, sizeof(GUID) );
    if( pGuid == NULL )
        return TRUE;

    // Store pointer to GUID in combo box
    *pGuid = *pSPGUID;
    SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_SETITEMDATA, (WPARAM)iIndex,
                        (LPARAM)pGuid );

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: GetServiceProviderGuid()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT GetServiceProviderGuid( HWND hWnd, GUID* pServiceProviderGUID )
{
    LONG iIndex;

    // Get guid for service provider
    iIndex = SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_GETCURSEL, 0, 0 );
    if( iIndex == CB_ERR )
        return DPERR_GENERIC;

    iIndex = SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_GETITEMDATA,
                                 (WPARAM)iIndex, 0 );
    if( (iIndex == CB_ERR) || (iIndex == 0) )
        return DPERR_GENERIC;

    *pServiceProviderGUID = *((GUID*)iIndex);

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: DeleteServiceProviderCombo()
// Desc: 
//-----------------------------------------------------------------------------
VOID DeleteServiceProviderCombo( HWND hWnd )
{
    WPARAM i;
    LONG   pData;
    
    // Destroy the data stored with each combo box item
    i = 0;
    while( TRUE )
    {
        // Get data pointer stored with item
        pData = SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_GETITEMDATA, i, 0 );
        if( pData == CB_ERR )           // error getting data
            break;

        if( pData )                     // data to delete
            GlobalFreePtr( (VOID*)pData );
        
        i++;
    }

    // Delete all items in list
    SendDlgItemMessage( hWnd, IDC_SPCOMBO, CB_RESETCONTENT, 0, 0 );
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
        // Get data pointer stored with item
        pData = SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_GETITEMDATA,
                                    i, 0 );
        if( pData == CB_ERR )           // error getting data
            break;
        
        if( pData )                     // data to delete
            GlobalFreePtr( (VOID*)pData );

        i++;
    }

    // Delete all items in list
    SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_RESETCONTENT, 0, 0 );
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
VOID SelectSessionInstance( HWND hWnd, GUID* pSessionInstanceGUID )
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
                                    i, 0 );
        if( pData == CB_ERR )           // error getting data
            break;

        if( pData == 0 )                // no data to compare to
            continue;

        // Guid matches
        if( IsEqualGUID( *pSessionInstanceGUID, *((GUID*)pData) ) )
        {
            iIndex = i;                 // store index of this string
            break;
        }

        i++;
    }

    // Select this item
    SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_SETCURSEL, iIndex, 0);
}




//-----------------------------------------------------------------------------
// Name: GetSessionInstanceGuid()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT GetSessionInstanceGuid( HWND hWnd, GUID* pSessionInstanceGUID )
{
    LONG iIndex;

    // Get guid for session
    iIndex = SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_GETCURSEL, 0, 0 );
    if( iIndex == LB_ERR )
        return DPERR_GENERIC;

    iIndex = SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_GETITEMDATA,
                                 (WPARAM) iIndex, 0 );
    if( (iIndex == LB_ERR) || (iIndex == 0) )
        return DPERR_GENERIC;

    *pSessionInstanceGUID = *((GUID*)iIndex );

    return DP_OK;
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
    SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_SETITEMDATA, (WPARAM)iIndex,
                        (LPARAM)pGuid );

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: SessionsWndProc()
// Desc: 
//-----------------------------------------------------------------------------
BOOL CALLBACK SessionsWndProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                               LPARAM lParam )
{
    static STATUSCONTEXT* pContext;
    static LPDIRECTPLAY4A pDP;
    static UINT           idTimer;
    static BOOL           bInsideEnumSessions;
    DPSESSIONDESC2 sessionDesc;
    GUID           guidSessionInstance;
    LONG           iIndex;
    HRESULT        hr;

    switch( uMsg )
    {
        case WM_INITDIALOG:
            pContext = (STATUSCONTEXT*)lParam;
            pDP = pContext->pDP;
            bInsideEnumSessions = FALSE;

            // Can't join until there is a session
            EnableDlgButton( hWnd, IDC_JOINSESSIONBUTTON, FALSE );

            // Set a timer to refresh the session list
            idTimer = SetTimer( hWnd, TIMERID, TIMERINTERVAL, NULL );
            break;

        case WM_DESTROY:
            if( idTimer )
            {
                KillTimer( hWnd, idTimer ); 
                idTimer = 0;
            }
            DeleteSessionInstanceList( hWnd );
            break;

        case WM_TIMER:
            // Make sure we don't re-enter EnumSessions
            if( bInsideEnumSessions )
                break;

            // Get guid of currently selected session
            guidSessionInstance = GUID_NULL;
            hr = GetSessionInstanceGuid( hWnd, &guidSessionInstance );

            // Delete existing session list
            DeleteSessionInstanceList( hWnd );

            // Enum sessions
            ZeroMemory( &sessionDesc, sizeof(DPSESSIONDESC2) );
            sessionDesc.dwSize = sizeof(DPSESSIONDESC2);
            sessionDesc.guidApplication = OVERRIDE_GUID;

            bInsideEnumSessions = TRUE;
            hr = pDP->EnumSessions( &sessionDesc, 0, EnumSessionsCallback,
                                    hWnd, DPENUMSESSIONS_AVAILABLE |
                                          DPENUMSESSIONS_ASYNC |
                                          DPENUMSESSIONS_RETURNSTATUS );
            bInsideEnumSessions = FALSE;

            // Select the session that was previously selected
            SelectSessionInstance( hWnd, &guidSessionInstance );

            // Hilight "Join" button only if there are sessions to join
            iIndex = SendDlgItemMessage( hWnd, IDC_SESSIONLIST, LB_GETCOUNT,
                                         0, 0 );

            EnableDlgButton( hWnd, IDC_JOINSESSIONBUTTON,
                             (iIndex > 0) ? TRUE : FALSE );

            switch( hr )
            {
                case DP_OK:
                    SetDlgItemText( hWnd, IDC_SESSIONSTATUSEDIT, "Searching for sessions..." );
                    break;

                case DPERR_CONNECTING:
                    SetDlgItemText( hWnd, IDC_SESSIONSTATUSEDIT, "Making connection..." );
                    break;

                case DPERR_NOCONNECTION:
                    SetDlgItemText( hWnd, IDC_SESSIONSTATUSEDIT, "Connection failed" );
                    KillTimer( hWnd, idTimer ); 
                    idTimer = 0;
                    break;

                default:
                    SetDlgItemText( hWnd, IDC_SESSIONSTATUSEDIT, "Error making connection" );
                    KillTimer( hWnd, idTimer ); 
                    idTimer = 0;
                    break;
            }
            break;
        
        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDC_JOINSESSIONBUTTON:
                    // Return guid of session to join
                    hr = GetSessionInstanceGuid( hWnd, &pContext->guidInstance );
                    if( SUCCEEDED(hr) )
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
// Name: UpdateAddressInfo()
// Desc: Updates address information elements in dialog. Calls
//       EnumAddressTypes() to determine what address information should be
//       displayed and arranges dialog to display and collect the needed
//       information.
//-----------------------------------------------------------------------------
HRESULT UpdateAddressInfo( HWND hWnd, LPDIRECTPLAYLOBBY3A pDPLobby )
{
    GUID    guidServiceProvider;
    HRESULT hr;

    // Clear and hide address dialog items
    ShowWindow( GetDlgItem(hWnd, IDC_PHONEEDIT), SW_HIDE );
    ShowWindow( GetDlgItem(hWnd, IDC_PHONEEDITLABEL), SW_HIDE );
    ShowWindow( GetDlgItem(hWnd, IDC_MODEMCOMBO), SW_HIDE );
    ShowWindow( GetDlgItem(hWnd, IDC_MODEMCOMBOLABEL), SW_HIDE );
    ShowWindow( GetDlgItem(hWnd, IDC_TCPEDIT), SW_HIDE );
    ShowWindow( GetDlgItem(hWnd, IDC_TCPEDITLABEL), SW_HIDE );
    ShowWindow( GetDlgItem(hWnd, IDC_PORTEDIT), SW_HIDE );
    ShowWindow( GetDlgItem(hWnd, IDC_PORTEDITLABEL), SW_HIDE );
    ShowWindow( GetDlgItem(hWnd, IDC_IPXLABEL), SW_HIDE );
    ShowWindow( GetDlgItem(hWnd, IDC_SERVICEPROVIDERLABEL), SW_HIDE );

    // Get currently selected service provider
    hr = GetServiceProviderGuid( hWnd, &guidServiceProvider );
    if( FAILED(hr) )
        return hr;

    if( IsEqualGUID( guidServiceProvider, DPSPGUID_MODEM ) )
    {
        // Modem service provider
        // Show edit control to collect phone number and modem
        ShowWindow( GetDlgItem(hWnd, IDC_PHONEEDIT), SW_SHOW );
        ShowWindow( GetDlgItem(hWnd, IDC_PHONEEDITLABEL), SW_SHOW );
        ShowWindow( GetDlgItem(hWnd, IDC_MODEMCOMBO), SW_SHOW );
        ShowWindow( GetDlgItem(hWnd, IDC_MODEMCOMBOLABEL), SW_SHOW );
    }
    else if( IsEqualGUID( guidServiceProvider, DPSPGUID_TCPIP ) )
    {
        // Internet TCP/IP service provider
        // Show edit control to collect IP address and port number
        ShowWindow( GetDlgItem(hWnd, IDC_TCPEDIT), SW_SHOW );
        ShowWindow( GetDlgItem(hWnd, IDC_TCPEDITLABEL), SW_SHOW );
        ShowWindow( GetDlgItem(hWnd, IDC_PORTEDIT), SW_SHOW );
        ShowWindow( GetDlgItem(hWnd, IDC_PORTEDITLABEL), SW_SHOW );
    }
    else if( IsEqualGUID( guidServiceProvider, DPSPGUID_IPX ) )
    {
        // IPX service provider
        // No address info is needed, so just display a string
        ShowWindow( GetDlgItem(hWnd, IDC_IPXLABEL), SW_SHOW );
    }
    else
    {
        // Anything else, let service provider collect settings, if any
        ShowWindow( GetDlgItem(hWnd, IDC_SERVICEPROVIDERLABEL), SW_SHOW );
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: CreateServiceProviderAddress()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT CreateServiceProviderAddress( HWND hWnd, LPDIRECTPLAYLOBBY3A pDPLobby,
                                      VOID** ppAddress, DWORD* pdwAddressSize )
{
    DPCOMPOUNDADDRESSELEMENT addressElements[3];
    CHAR    strIPAddressString[NAMEMAX];
    CHAR    strPhoneNumberString[NAMEMAX];
    CHAR    strModemString[NAMEMAX];
    VOID*   pAddress = NULL;
    DWORD   dwAddressSize = 0;
    DWORD   dwElementCount;
    GUID    guidServiceProvider;
    WORD    wPort;
    HRESULT hr;

    // Get currently selected service provider
    hr = GetServiceProviderGuid( hWnd, &guidServiceProvider );
    if( FAILED(hr) )
        return hr;

    dwElementCount = 0;

    if( IsEqualGUID( guidServiceProvider, DPSPGUID_MODEM ) )
    {
        // Modem needs a service provider, a phone number string and a modem string

        // Dervice provider
        addressElements[dwElementCount].guidDataType = DPAID_ServiceProvider;
        addressElements[dwElementCount].dwDataSize   = sizeof(GUID);
        addressElements[dwElementCount].lpData       = (VOID*)&DPSPGUID_MODEM;
        dwElementCount++;

        // Sdd a modem string if available
        lstrcpy( strModemString, "" );
        if( GetDlgItemText( hWnd, IDC_MODEMCOMBO, strModemString, NAMEMAX ) )
        {
            addressElements[dwElementCount].guidDataType = DPAID_Modem;
            addressElements[dwElementCount].dwDataSize   = lstrlen(strModemString) + 1;
            addressElements[dwElementCount].lpData       = strModemString;
            dwElementCount++;
        }

        // Add phone number string
        lstrcpy( strPhoneNumberString, "" );
        GetDlgItemText(hWnd, IDC_PHONEEDIT, strPhoneNumberString, NAMEMAX);
        addressElements[dwElementCount].guidDataType = DPAID_Phone;
        addressElements[dwElementCount].dwDataSize   = lstrlen(strPhoneNumberString) + 1;
        addressElements[dwElementCount].lpData       = strPhoneNumberString;
        dwElementCount++;
    }
    else if( IsEqualGUID( guidServiceProvider, DPSPGUID_TCPIP ) )
    {
        // TCP/IP needs a service provider, an IP address, and optional port #

        // Service provider
        addressElements[dwElementCount].guidDataType = DPAID_ServiceProvider;
        addressElements[dwElementCount].dwDataSize   = sizeof(GUID);
        addressElements[dwElementCount].lpData       = (VOID*)&DPSPGUID_TCPIP;
        dwElementCount++;

        // IP address string
        lstrcpy( strIPAddressString, "" );
        GetDlgItemText(hWnd, IDC_TCPEDIT, strIPAddressString, NAMEMAX);
        addressElements[dwElementCount].guidDataType = DPAID_INet;
        addressElements[dwElementCount].dwDataSize   = lstrlen(strIPAddressString) + 1;
        addressElements[dwElementCount].lpData       = strIPAddressString;
        dwElementCount++;

        // Optional Port number
        wPort = (WORD)GetDlgItemInt( hWnd, IDC_PORTEDIT, NULL, FALSE );
        if( wPort > 0 )
        {
            addressElements[dwElementCount].guidDataType = DPAID_INetPort;
            addressElements[dwElementCount].dwDataSize   = sizeof(WORD);
            addressElements[dwElementCount].lpData       = &wPort;
            dwElementCount++;
        }
    }
    else if( IsEqualGUID( guidServiceProvider, DPSPGUID_IPX ) )
    {
        // IPX just needs a service provider

        // Service provider
        addressElements[dwElementCount].guidDataType = DPAID_ServiceProvider;
        addressElements[dwElementCount].dwDataSize   = sizeof(GUID);
        addressElements[dwElementCount].lpData       = (VOID*)&DPSPGUID_IPX;
        dwElementCount++;
    }
    else
    {
        // Anything else, let service provider collect settings, if any
        
        // Service provider
        addressElements[dwElementCount].guidDataType = DPAID_ServiceProvider;
        addressElements[dwElementCount].dwDataSize   = sizeof(GUID);
        addressElements[dwElementCount].lpData       = (VOID*)&guidServiceProvider;
        dwElementCount++;
    }

    // See how much room is needed to store this address
    hr = pDPLobby->CreateCompoundAddress( addressElements, dwElementCount,
                                          NULL, &dwAddressSize );
    if( hr != DPERR_BUFFERTOOSMALL )
        return hr;

    // Allocate space
    pAddress = GlobalAllocPtr( GHND, dwAddressSize );
    if( pAddress == NULL )
        return DPERR_NOMEMORY;

    // Create the address
    hr = pDPLobby->CreateCompoundAddress( addressElements, dwElementCount,
                                          pAddress, &dwAddressSize );
    if( FAILED(hr) )
    {
        GlobalFreePtr( pAddress );
        return hr;
    }

    // Return the address info
    *ppAddress = pAddress;
    *pdwAddressSize = dwAddressSize;

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: DoHostOrJoin()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT DoHostOrJoin( HINSTANCE hInstance, HWND hWnd,
                      LPDIRECTPLAYLOBBY3A pDPLobby, BOOL bHost,
                      LPDIRECTPLAY4A* ppDP )
{
    LPDIRECTPLAY4A pDP           = NULL;
    VOID*          pAddress      = NULL;
    DWORD          dwAddressSize = 0;
    DPSESSIONDESC2 sessionDesc;
    STATUSCONTEXT  statusContext;
    HRESULT        hr;

    // Bail if we don't have a lobby interface
    if( pDPLobby == NULL )
        return DPERR_INVALIDOBJECT;

    // Get service provider address from information in dialog
    hr = CreateServiceProviderAddress( hWnd, pDPLobby, &pAddress,
                                       &dwAddressSize );
    if( FAILED(hr) )
        return hr;

    // interface already exists, so release it
    if( *ppDP )
    {
        (*ppDP)->Close();
        (*ppDP)->Release();
        *ppDP = NULL;
    }

    // Create an ANSI DirectPlay4 interface
    hr = CoCreateInstance( CLSID_DirectPlay, NULL, CLSCTX_INPROC_SERVER, 
                           IID_IDirectPlay4A, (VOID**)&pDP );
    if( FAILED(hr) )
        return hr;

    // Initialize the connection using the address
    hr = pDP->InitializeConnection( pAddress, 0 );
    if( FAILED(hr) )
    {
        pDP->Release();
        return hr;
    }

    if( bHost )
    {
        // Host a new session
        ZeroMemory( &sessionDesc, sizeof(DPSESSIONDESC2) );
        sessionDesc.dwSize  = sizeof(DPSESSIONDESC2);
        sessionDesc.dwFlags = DPSESSION_MIGRATEHOST | DPSESSION_KEEPALIVE;
        sessionDesc.guidApplication  = OVERRIDE_GUID;
        sessionDesc.dwMaxPlayers     = 0;
        sessionDesc.lpszSessionNameA = "Override";

        // Open it
        hr = pDP->Open( &sessionDesc, DPOPEN_CREATE );
    }
    else
    {
        // Enumerate the sessions
        // Display status dialog and enumerate until we find a session
        statusContext.pDP = pDP;
        statusContext.guidInstance = GUID_NULL;

        if( !DialogBoxParam( hInstance, MAKEINTRESOURCE(IDD_SESSIONSDIALOG),
                             hWnd, (DLGPROC)SessionsWndProc, (LPARAM)&statusContext ) )
        {
            pDP->Release();
            return DPERR_USERCANCEL;
        }

        // Open the session selected by the use
        ZeroMemory( &sessionDesc, sizeof(DPSESSIONDESC2) );
        sessionDesc.dwSize          = sizeof(DPSESSIONDESC2);
        sessionDesc.guidApplication = OVERRIDE_GUID;
        sessionDesc.guidInstance    = statusContext.guidInstance;

        // Open it
        hr = pDP->Open( &sessionDesc, DPOPEN_JOIN );
    }

    if( FAILED(hr) )
    {
        pDP->Close();
        pDP->Release();
        return hr;
    }

    // Return the connected interface
    *ppDP = pDP;

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: EnumModemAddress()
// Desc: Enumerates the DirectPlay address chunks. If the chunk contains modem
//       strings, add them to the control.
//-----------------------------------------------------------------------------
BOOL FAR PASCAL EnumModemAddress( REFGUID guidDataType, DWORD dwDataSize,
                                  const VOID* pData, VOID* pContext )
{
    HWND  hWnd   = (HWND)pContext;
    LPSTR strStr = (LPSTR)pData;

    // Modem
    if( IsEqualGUID( guidDataType, DPAID_Modem ) )
    {
        // Loop over all strings in list
        while( lstrlen( strStr ) )
        {
            // Store modem name in combo box
            SendDlgItemMessage( hWnd, IDC_MODEMCOMBO, CB_ADDSTRING, 0,
                                (LPARAM)strStr );

            // Skip to next string
            strStr += lstrlen(strStr) + 1;
        }
    }

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: FillModemComboBox()
// Desc: Fills combo box with modem names
//-----------------------------------------------------------------------------
HRESULT FillModemComboBox( HWND hWnd, LPDIRECTPLAYLOBBY3A pDPLobby )
{
    LPDIRECTPLAY   pDP1 = NULL;
    LPDIRECTPLAY4A pDP = NULL;
    VOID*          pAddress = NULL;
    DWORD          dwAddressSize = 0;
    GUID           guidServiceProvider = DPSPGUID_MODEM;
    HRESULT        hr;

    // Use the obsolete DirectPlayCreate() as quick way to load a specific
    // service provider.  Trade off using DirectPlayCreate() and
    // QueryInterface() vs using CoCreateInitialize() and building a
    // DirectPlay Address for InitializeConnection().
    hr = DirectPlayCreate( &guidServiceProvider, &pDP1, NULL );
    if( FAILED(hr) )
        return hr;

    // Query for an ANSI DirectPlay4 interface
    hr = pDP1->QueryInterface( IID_IDirectPlay4A, (VOID**)&pDP );
    if( FAILED(hr) )
    {
        pDP1->Release();
        return hr;
    }

    // Get size of player address for player zero
    hr = pDP->GetPlayerAddress( DPID_ALLPLAYERS, NULL, &dwAddressSize );
    if( hr != DPERR_BUFFERTOOSMALL )
    {
        pDP1->Release();
        pDP->Release();
        return hr;
    }

    // Make room for it
    pAddress = GlobalAllocPtr( GHND, dwAddressSize );
    if( pAddress == NULL )
    {
        pDP1->Release();
        pDP->Release();
        return DPERR_NOMEMORY;
    }

    // Get the address
    hr = pDP->GetPlayerAddress( DPID_ALLPLAYERS, pAddress, &dwAddressSize );
    if( FAILED(hr) )
    {
        pDP1->Release();
        pDP->Release();
        if( pAddress )
            GlobalFreePtr( pAddress );
        return hr;
    }
    
    // Get modem strings from address and put them in the combo box
    hr = pDPLobby->EnumAddress( EnumModemAddress, pAddress, dwAddressSize, hWnd );
    if( FAILED(hr) )
    {
        pDP1->Release();
        pDP->Release();
        if( pAddress )
            GlobalFreePtr( pAddress );
        return hr;
    }

    // Select first item in list
    SendDlgItemMessage( hWnd, IDC_MODEMCOMBO, CB_SETCURSEL, 0, 0 );

    // Cleanup and return
    pDP1->Release();
    pDP->Release();
    if( pAddress )
        GlobalFreePtr( pAddress );
    
    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: DlgItemIsChecked()
// Desc: 
//-----------------------------------------------------------------------------
BOOL DlgItemIsChecked( HWND hDlg, int nIDDlgItem )
{
    return( ( SendDlgItemMessage( hDlg, nIDDlgItem, BM_GETCHECK, 0, 0)
              == BST_CHECKED) ? TRUE : FALSE );
}




//-----------------------------------------------------------------------------
// Name: EnableDlgButton()
// Desc: 
//-----------------------------------------------------------------------------
VOID EnableDlgButton( HWND hDlg, int nIDDlgItem, BOOL bEnable )
{
    EnableWindow( GetDlgItem( hDlg, nIDDlgItem ), bEnable );
}




//-----------------------------------------------------------------------------
// Name: ErrorBox()
// Desc: 
//-----------------------------------------------------------------------------
VOID ErrorBox( LPSTR strError, HRESULT hr )
{
    CHAR str[MAXSTRLEN];

    wsprintf( str, strError, hr );
    MessageBox(NULL, str, "DPLaunch Error", MB_OK );
}



