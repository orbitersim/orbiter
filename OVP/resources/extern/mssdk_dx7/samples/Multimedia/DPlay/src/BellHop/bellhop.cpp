//-----------------------------------------------------------------------------
// File: Bellhop.cpp
//
// Desc: Simple lobby program using DirectPlay.
//
// Copyright (C) 1996-1999 Microsoft Corporation.  All Rights Reserved.
//-----------------------------------------------------------------------------
#define INITGUID
#include "bellhop.h"
#include "resource.h"

#if defined(UNICODE) || defined(_UNICODE)
#error This app does not support UNICODE
#endif

enum
{
    WM_USER_ADDSTRING = WM_USER+257,  // Msg to add string to chat str list
    WM_USER_UPDATE                    // Msg to update lobby display
};

// globals
HANDLE    g_hReceiveThread    = NULL; // Handle of receive thread
DWORD     g_idReceiveThread   = 0;    // ID of receive thread
HANDLE    g_hKillReceiveEvent = NULL; // Event used to kill receive thread
HWND      g_hMainWnd          = NULL; // Main window
HINSTANCE g_hInstance;                // Application instance




//-----------------------------------------------------------------------------
// Function prototypes
//-----------------------------------------------------------------------------
BOOL FAR PASCAL DirectPlayEnumConnectionsCallback( const GUID* pguidSP,
                        VOID* pConnection, DWORD dwSize, const DPNAME* pName,
                        DWORD dwFlags, VOID* pContext );
HRESULT GetConnection( HWND hWnd,  int idCombo, VOID** ppConnection );
HRESULT GetConnectionSPGuid( HWND hWnd, int idCombo, GUID* pGuidSP );
CHAR*   GetDirectPlayErrStr( HRESULT hr );




//-----------------------------------------------------------------------------
// Name: WinMain
// Desc:
//-----------------------------------------------------------------------------
int WINAPI WinMain( HINSTANCE hInstance, HINSTANCE, LPSTR, int )
{
    DPLAYINFO   DPInfo;
    int         iResult = 0;
    HRESULT     hr;

    g_hInstance = hInstance;

    // Initialize COM library
    hr = CoInitialize(NULL);
    if FAILED(hr)
        return 0;

    // Detup the connection
    hr = SetupConnection(hInstance, &DPInfo);
    if FAILED(hr)
    {
        hr = ShutdownConnection( &DPInfo );
        CoUninitialize();
        return 0;
    }

    // Dhow the chat window
    iResult = DialogBoxParam( hInstance, MAKEINTRESOURCE(IDD_LOBBYDIALOG),
                              NULL, (DLGPROC)LobbyWndProc, (LPARAM)&DPInfo );

    // Shut down the connection
    hr = ShutdownConnection(&DPInfo);

    // Uninitialize the COM library
    CoUninitialize();

    return iResult;
}




//-----------------------------------------------------------------------------
// Name: LobbyWndProc()
// Desc:
//-----------------------------------------------------------------------------
BOOL CALLBACK LobbyWndProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam )
{
    static DPLAYINFO* pDPInfo;
    DWORD dwTextLen;

    switch( uMsg )
    {
        case WM_INITDIALOG:
        {
            // Save the connection info pointer
            pDPInfo = (DPLAYINFO*)lParam;

            // Get the dialog spacing info
            HDC hDC = GetDC(hWnd);
            int xPixelsPerInch = GetDeviceCaps(hDC, LOGPIXELSX);
            int yPixelsPerInch = GetDeviceCaps(hDC, LOGPIXELSY);
            ReleaseDC(hWnd, hDC);
            pDPInfo->xSpacing = xPixelsPerInch / 8;
            pDPInfo->ySpacing = yPixelsPerInch / 8;
            pDPInfo->xHalfSplitWidth = xPixelsPerInch / 12;

            RECT rect;
            GetClientRect( hWnd, &rect );
            pDPInfo->xPaneSplit = ( (rect.right - rect.left) -
                                    (2 * pDPInfo->xSpacing ) ) / 3 
                                  - pDPInfo->xHalfSplitWidth;   

            // Create the tree control
            pDPInfo->pGroupTree = new CGroupTree;

            if( pDPInfo->pGroupTree )
            {
                // Create thread to receive player messages
                g_hReceiveThread = CreateThread( NULL, 0, ReceiveThread,
                                                pDPInfo, 0, &g_idReceiveThread );
                if( g_hReceiveThread == NULL )
                {
                    OutputDebugString( "Couldn't create Receive thread\n");
                    DebugBreak();
                }

                pDPInfo->pGroupTree->Init(  GetDlgItem(hWnd, IDT_MESSAGEVIEW), 
                                            pDPInfo->pDP,
                                            pDPInfo->dpidPlayer );
                pDPInfo->pGroupTree->Refresh();
                pDPInfo->pGroupTree->CreatePlayer( pDPInfo->dpidPlayer, NULL,
                                                    pDPInfo->dwPlayerFlags );
                // Store global window
                g_hMainWnd = hWnd;

                // Initiallize lobby
                InitializeLobby( hWnd, pDPInfo );

                OnSize( hWnd, pDPInfo );
            }
            else
            {
                OutputDebugString( "Couldn't create CGroupTree\n");
                DebugBreak();
            }
            break;
        }

        case WM_DESTROY:
            g_hMainWnd = NULL;
            break;

        case WM_SIZE:
            OnSize( hWnd, pDPInfo );
            break;

        case WM_LBUTTONDOWN:
        {
            int x = (int)(short)LOWORD(lParam);
            //Check to see if they've clicked on the splitter.
            if( (x > pDPInfo->xPaneSplit) && 
                (x < pDPInfo->xPaneSplit+pDPInfo->xHalfSplitWidth * 2) )
            {
                // If so, begin window adjustments.
                pDPInfo->bSplitMove = TRUE;
                SetCapture( hWnd );
            }
            break;
        }

        case WM_LBUTTONUP:
            if( pDPInfo->bSplitMove )
            {
                pDPInfo->bSplitMove = FALSE;
                ReleaseCapture();
            }
            else
            {
                pDPInfo->pGroupTree->OnLButtonUp();
            }
            break;

        case WM_MOUSEMOVE:
            if( pDPInfo->bSplitMove )
            {
                // Track mouse movement while adjusting the divider
                RECT rect;
                // Change the value from unsigned to signed
                int  x = (int)(short)LOWORD(lParam);

                GetClientRect( hWnd, &rect );
                if( rect.left > x )
                {
                    x = rect.left;
                }
                else if( rect.right < x )
                {
                    x = rect.right;
                }
                pDPInfo->xPaneSplit = ( x - (pDPInfo->xSpacing) );
                OnSize( hWnd, pDPInfo );
            }
            else
            {
                pDPInfo->pGroupTree->OnMouseMove( LOWORD(lParam), HIWORD(lParam) );
            }
            break;

        case WM_USER_ADDSTRING:
            // This is a user-defined message used to add strings
            // Get length of text in log window
            dwTextLen = SendDlgItemMessage( hWnd, IDC_LOGEDIT, WM_GETTEXTLENGTH,
                                            0, 0 );

            // Put selection at end
            dwTextLen = SendDlgItemMessage( hWnd, IDC_LOGEDIT, EM_SETSEL,
                                            (WPARAM)dwTextLen, (LPARAM)dwTextLen );

            // Add string in lParam to log window
            SendDlgItemMessage( hWnd, IDC_LOGEDIT, EM_REPLACESEL,
                                (WPARAM)FALSE, (LPARAM)lParam );
            GlobalFreePtr( (VOID*)lParam );
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case ID_ROOT_CREATEGROUP:
                    DoCreateRoom( hWnd, pDPInfo );
                    break;          

                case ID_GROUP_CONNECTIONSETTINGS:
                    DoGroupConnectionSettings( hWnd, pDPInfo );
                    break;          

                case ID_DESTROYGROUP:
                    DoDeleteRoom( hWnd, pDPInfo );
                    break;          

                case ID_PLAYERINGROUP_DELETEPLAYERFROMGROUP:
                    DoDeletePlayerFromGroup( hWnd, pDPInfo );
                    break;          

                case ID_CREATEGROUPINGROUP:
                    DoCreateTable( hWnd, pDPInfo );
                    break;          

                case ID_SHORTCUT_DELETEGROUPFROMGROUP:
                    DoDeleteTable( hWnd, pDPInfo );
                    break;          

                case ID_GROUP_STARTSESSION:
                    DoLaunch( hWnd, pDPInfo );
                    break;          

                case ID_PLAYER_SETPLAYERNAME:
                case ID_PLAYERINGROUP_SETPLAYERNAME:
                case ID_GROUP_SETGROUPNAME:
                    pDPInfo->pGroupTree->EditLabel();
                    break;
                    
                case ID_PLAYERINGROUP_SETGROUPOWNER:
                    DoSetGroupOwner( hWnd, pDPInfo );
                    break;

                case IDC_SENDBUTTON:
                    SendChatMessage( hWnd, pDPInfo );
                    break;          

                case IDCANCEL:
                    EndDialog( hWnd, FALSE );
                    break;

                case ID_ROOT_ENUMRECURSIVE:
                    pDPInfo->pGroupTree->Refresh();
                    pDPInfo->pGroupTree->CreatePlayer( pDPInfo->dpidPlayer, NULL, 
                                                       pDPInfo->dwPlayerFlags );
                   break;

                case ID_ROOT_ENUMGROUPS:
                    pDPInfo->pGroupTree->Refresh( FALSE );
                    pDPInfo->pGroupTree->CreatePlayer( pDPInfo->dpidPlayer, NULL, 
                                                       pDPInfo->dwPlayerFlags );
                   break;
            }
            break;

        case WM_NOTIFY :
            return pDPInfo->pGroupTree->OnWM_NOTIFY( wParam, lParam );
            break;

        case WM_MENUSELECT:
            return FALSE;
            break;
    }

    // Allow for default processing
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: SetupConnection()
// Desc:
//-----------------------------------------------------------------------------
HRESULT SetupConnection( HINSTANCE hInstance, DPLAYINFO* pDPInfo )
{
    HRESULT hr;

    ZeroMemory( pDPInfo, sizeof(DPLAYINFO) );

    // Create event used by DirectPlay to signal a message has arrived
    pDPInfo->hPlayerEvent = CreateEvent( NULL, FALSE, FALSE, NULL );
    if( pDPInfo->hPlayerEvent == NULL )
    {
        ShutdownConnection( pDPInfo );
        return DPERR_NOMEMORY;
    }

    // Create event used to signal that the receive thread should exit
    g_hKillReceiveEvent = CreateEvent( NULL, FALSE, FALSE, NULL );
    if( g_hKillReceiveEvent == NULL )
    {
        ShutdownConnection( pDPInfo );
        return DPERR_NOMEMORY;
    }

    // If there is no lobby connection, ask the user for settings
    hr = ConnectUsingDialog( hInstance, pDPInfo );
    if( FAILED(hr) )
    {
        ShutdownConnection( pDPInfo );
        return hr;
    }

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: ShutdownConnection()
// Desc:
//-----------------------------------------------------------------------------
HRESULT ShutdownConnection( DPLAYINFO* pDPInfo )
{
    if( g_hReceiveThread )
    {
        // Wake up receive thread and wait for it to quit
        SetEvent( g_hKillReceiveEvent );
        WaitForSingleObject( g_hReceiveThread, INFINITE );

        CloseHandle( g_hReceiveThread );
        g_hReceiveThread = NULL;
    }

    if( g_hKillReceiveEvent )
    {
        CloseHandle( g_hKillReceiveEvent );
        g_hKillReceiveEvent = NULL;
    }

    if( pDPInfo->pDP )
    {
        if( pDPInfo->dpidPlayer )
        {
            pDPInfo->pDP->DestroyPlayer( pDPInfo->dpidPlayer );
            pDPInfo->dpidPlayer = 0;
        }
        pDPInfo->pDP->Close();
        pDPInfo->pDP->Release();
        pDPInfo->pDP = NULL;

        pDPInfo->pDPLobby->Release();
        pDPInfo->pDPLobby = NULL;

    }

    if( pDPInfo->hPlayerEvent )
    {
        CloseHandle( pDPInfo->hPlayerEvent );
        pDPInfo->hPlayerEvent = NULL;
    }

    if( pDPInfo->pGroupTree )
    {
        delete pDPInfo->pGroupTree;
        pDPInfo->pGroupTree = NULL;
    }

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: ReceiveThread()
// Desc:
//-----------------------------------------------------------------------------
DWORD WINAPI ReceiveThread( VOID* pThreadParameter )
{
    DPLAYINFO* pDPInfo = (DPLAYINFO*)pThreadParameter;
    HANDLE     eventHandles[2];

    eventHandles[0] = pDPInfo->hPlayerEvent;
    eventHandles[1] = g_hKillReceiveEvent;

    // Loop waiting for player events. If the kill event is signaled
    // the thread will exit
    while( WaitForMultipleObjects( 2, eventHandles, FALSE, INFINITE )
           == WAIT_OBJECT_0 )
    {
        // Receive any messages in the queue
        ReceiveMessage( pDPInfo );
    }

    ExitThread( 0 );

    return 0;
}




//-----------------------------------------------------------------------------
// Name: ReceiveMessage()
// Desc:
//-----------------------------------------------------------------------------
HRESULT ReceiveMessage( DPLAYINFO* pDPInfo )
{
    DPID    idFrom, idTo;
    VOID*   pMsgBuffer      = NULL;
    DWORD   dwMsgBufferSize = 0L;
    HRESULT hr;

    // Loop to read all messages in queue
    do
    {
        // Loop until a single message is successfully read
        do
        {
            // Read messages from any player, including system player
            idFrom = 0;
            idTo   = 0;

            hr = pDPInfo->pDP->Receive( &idFrom, &idTo, DPRECEIVE_ALL,
                                        pMsgBuffer, &dwMsgBufferSize );

            // Not enough room, so resize buffer
            if( hr == DPERR_BUFFERTOOSMALL )
            {
                if( pMsgBuffer )
                    GlobalFreePtr( pMsgBuffer );

                pMsgBuffer = GlobalAllocPtr( GHND, dwMsgBufferSize );
                if( pMsgBuffer == NULL )
                    hr = DPERR_OUTOFMEMORY;
            }
        } while( hr == DPERR_BUFFERTOOSMALL );

        if( (SUCCEEDED(hr)) &&                           // successfully read a message
            (dwMsgBufferSize >= sizeof(DPMSG_GENERIC)) ) // and it is big enough
        {
            // Update the tree view UI.
            pDPInfo->pGroupTree->Update( pMsgBuffer );

            // Check for system message
            if( idFrom == DPID_SYSMSG )
            {
                HandleSystemMessage( pDPInfo, (DPMSG_GENERIC*)pMsgBuffer,
                                     dwMsgBufferSize, idFrom, idTo );
            }
            else
            {
                HandleApplicationMessage( pDPInfo, (DPMSG_GENERIC*)pMsgBuffer,
                                          dwMsgBufferSize, idFrom, idTo );
            }
        }
    } while( SUCCEEDED(hr) );

    // Free any memory we created
    if( pMsgBuffer )
        GlobalFreePtr( pMsgBuffer );

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: HandleApplicationMessage()
// Desc:
//-----------------------------------------------------------------------------
VOID HandleApplicationMessage( DPLAYINFO* pDPInfo, DPMSG_GENERIC* pMsg,
                               DWORD dwMsgSize, DPID idFrom, DPID idTo )
{
    // Any messages received from the lobby server would be handled here.
    // In this case, we don't have any
}




//-----------------------------------------------------------------------------
// Name: HandleSystemMessage()
// Desc:
//-----------------------------------------------------------------------------
VOID HandleSystemMessage( DPLAYINFO* pDPInfo, DPMSG_GENERIC* pMsg,
                          DWORD dwMsgSize, DPID idFrom, DPID idTo )
{
    LPSTR strStr = NULL;

    // The body of each case is there so you can set a breakpoint and examine
    // the contents of the message received.
    switch( pMsg->dwType )
    {
        case DPSYS_CREATEPLAYERORGROUP:
        {
            DPMSG_CREATEPLAYERORGROUP* p = (DPMSG_CREATEPLAYERORGROUP*)pMsg;
            LPSTR strName;
            LPSTR strDisplayFormat;
            
            if( p->dwPlayerType == DPPLAYERTYPE_PLAYER)
                strDisplayFormat = "Player \"%s\" created\r\n";
            else if( p->dwPlayerType == DPPLAYERTYPE_GROUP)
                strDisplayFormat = "Group \"%s\" created\r\n";
            else
                strDisplayFormat = "Unknown object \"%s\" created\r\n";

            // Get pointer to player name
            if( p->dpnName.lpszShortNameA)
                strName = p->dpnName.lpszShortNameA;
            else
                strName = "unknown";

            // Allocate space for string
            strStr = (LPSTR)GlobalAllocPtr( GHND, lstrlen(strDisplayFormat) +
                                                   lstrlen(strName) + 1);
            if( strStr == NULL )
                break;

            // Build string
            wsprintf( strStr, strDisplayFormat, strName );
            break;
        }

        case DPSYS_DESTROYPLAYERORGROUP:
        {
            DPMSG_DESTROYPLAYERORGROUP* p = (DPMSG_DESTROYPLAYERORGROUP*)pMsg;
            LPSTR strName;
            LPSTR strDisplayFormat;
            
            if( p->dwPlayerType == DPPLAYERTYPE_PLAYER )
                strDisplayFormat = "Player \"%s\" destroyed\r\n";
            else if( p->dwPlayerType == DPPLAYERTYPE_GROUP )
                strDisplayFormat = "Group \"%s\" destroyed\r\n";
            else
                strDisplayFormat = "Unknown object \"%s\" destroyed\r\n";

            // Get pointer to player name
            if( p->dpnName.lpszShortNameA )
                strName = p->dpnName.lpszShortNameA;
            else
                strName = "unknown";

            // Allocate space for string
            strStr = (LPSTR)GlobalAllocPtr( GHND, lstrlen(strDisplayFormat) +
                                                  lstrlen(strName) + 1);
            if( strStr == NULL )
                break;

            // Build string
            wsprintf( strStr, strDisplayFormat, strName );
            break;
        }

        case DPSYS_ADDPLAYERTOGROUP:
        case DPSYS_DELETEPLAYERFROMGROUP:
        {
            DPMSG_ADDPLAYERTOGROUP*      pAddMsg = (DPMSG_ADDPLAYERTOGROUP*)pMsg;
            DPMSG_DELETEPLAYERFROMGROUP* pDeleteMsg = (DPMSG_DELETEPLAYERFROMGROUP*)pMsg;
            DPID    dpidPlayer, dpidGroup;
            LPSTR   strPlayerName, strGroupName;
            DPNAME* pPlayerName;
            DPNAME* pGroupName;
            LPSTR   strDisplayFormat;
            HRESULT hr;

            if( pMsg->dwType == DPSYS_ADDPLAYERTOGROUP )
            {
                dpidPlayer = pAddMsg->dpIdPlayer;
                dpidGroup  = pAddMsg->dpIdGroup;
                strDisplayFormat = "Player \"%s\" added to group \"%s\"\r\n";
            }
            else
            {
                dpidPlayer = pDeleteMsg->dpIdPlayer;
                dpidGroup  = pDeleteMsg->dpIdGroup;
                strDisplayFormat = "Player \"%s\" removed from group \"%s\"\r\n";
            }

            // Get pointer to player name
            hr = GetPlayerName( pDPInfo->pDP, dpidPlayer, &pPlayerName );
            if( FAILED(hr) )
            {
                // A failure may mean that the player has been deleted
                // since we began processing this message
                pPlayerName = NULL;
            }

            if( (pPlayerName) && (pPlayerName->lpszShortNameA) )
                strPlayerName = pPlayerName->lpszShortNameA;
            else
                strPlayerName = "unknown";

            // Get pointer to group name
            hr = GetGroupName( pDPInfo->pDP, dpidGroup, &pGroupName );
            if( FAILED(hr) )
            {
                // A failure may mean that the group has been deleted
                // since we began processing this message
                pGroupName = NULL;
            }

            if( (pGroupName) && (pGroupName->lpszShortNameA) )
                strGroupName = pGroupName->lpszShortNameA;
            else
                strGroupName = "unknown";


            // Allocate space for string
            strStr = (LPSTR)GlobalAllocPtr(GHND, lstrlen(strDisplayFormat) +
                                                 lstrlen(strPlayerName) +
                                                 lstrlen(strGroupName) + 1);
            // Build string
            if( strStr )
                wsprintf( strStr, strDisplayFormat, strPlayerName, strGroupName );

            // Free data we allocated
            if( pPlayerName )
                GlobalFreePtr( pPlayerName );
            if( pGroupName )
                GlobalFreePtr( pGroupName );
            break;
        }

        case DPSYS_ADDGROUPTOGROUP:
        case DPSYS_DELETEGROUPFROMGROUP:
        {
            DPMSG_ADDGROUPTOGROUP*      pAddMsg    = (DPMSG_ADDGROUPTOGROUP*)pMsg;
            DPMSG_DELETEGROUPFROMGROUP* pDeleteMsg = (DPMSG_DELETEGROUPFROMGROUP*)pMsg;
            DPID    dpidParentGroup, dpidGroup;
            LPSTR   strParentGroupName, strGroupName;
            DPNAME* pParentGroupName;
            DPNAME* pGroupName;
            LPSTR   strDisplayFormat;
            HRESULT hr;

            if( pMsg->dwType == DPSYS_ADDGROUPTOGROUP )
            {
                dpidGroup = pAddMsg->dpIdGroup;
                dpidParentGroup = pAddMsg->dpIdParentGroup;
                strDisplayFormat = "Group \"%s\" added to group \"%s\"\r\n";
            }
            else
            {
                dpidParentGroup = pDeleteMsg->dpIdParentGroup;
                dpidGroup = pDeleteMsg->dpIdGroup;
                strDisplayFormat = "Group \"%s\" removed from group \"%s\"\r\n";
            }

            // Get pointer to player name
            hr = GetGroupName( pDPInfo->pDP, dpidParentGroup, &pParentGroupName );
            if( FAILED(hr) )
                pParentGroupName = NULL;

            if( (pParentGroupName) && (pParentGroupName->lpszShortNameA) )
                strParentGroupName = pParentGroupName->lpszShortNameA;
            else
                strParentGroupName = "unknown";

            // Get pointer to group name
            hr = GetGroupName( pDPInfo->pDP, dpidGroup, &pGroupName );
            if( FAILED(hr) )
                pGroupName = NULL;

            if( (pGroupName) && (pGroupName->lpszShortNameA) )
                strGroupName = pGroupName->lpszShortNameA;
            else
                strGroupName = "unknown";

            // Allocate space for string
            strStr = (LPSTR)GlobalAllocPtr( GHND, lstrlen(strDisplayFormat) +
                                                  lstrlen(strParentGroupName) +
                                                  lstrlen(strGroupName) + 1 );
            // Build string
            if( strStr )
                wsprintf( strStr, strDisplayFormat, strGroupName, strParentGroupName );

            // Free data we allocated
            if( pParentGroupName )
                GlobalFreePtr( pParentGroupName );
            if( pGroupName )
                GlobalFreePtr( pGroupName );
            break;
        }

        case DPSYS_SESSIONLOST:
        {
            DPMSG_SESSIONLOST* p = (DPMSG_SESSIONLOST*)pMsg;
            LPSTR strDisplayFormat = "Session lost.\r\n";

            // Allocate space for string
            strStr = (LPSTR)GlobalAllocPtr( GHND, lstrlen(strDisplayFormat) + 1 );
            if( strStr == NULL )
                break;

            // Build string
            lstrcpy( strStr, strDisplayFormat );
            break;
        }

        case DPSYS_HOST:
        {
            DPMSG_HOST* p = (DPMSG_HOST*)pMsg;
            LPSTR       strDisplayFormat = "You have become the host\r\n";

            // Allocate space for string
            strStr = (LPSTR)GlobalAllocPtr( GHND, lstrlen(strDisplayFormat) + 1 );
            if( strStr == NULL )
                break;

            // Build string
            lstrcpy( strStr, strDisplayFormat );

            // We are now the host
            pDPInfo->bIsHost = TRUE;
            break;
        }

        case DPSYS_SETPLAYERORGROUPDATA:
        {
            DPMSG_SETPLAYERORGROUPDATA* p = (DPMSG_SETPLAYERORGROUPDATA*)pMsg;
            break;
        }

        case DPSYS_SETPLAYERORGROUPNAME:
        {
            DPMSG_SETPLAYERORGROUPNAME* p = (DPMSG_SETPLAYERORGROUPNAME*)pMsg;
            break;
        }

        case DPSYS_STARTSESSION:
        {
            DPMSG_STARTSESSION* p = (DPMSG_STARTSESSION*)pMsg;

            HandleStartSession( p->lpConn, pDPInfo );
            break;
        }

        case DPSYS_SETGROUPOWNER:
        {
            DPMSG_SETGROUPOWNER* p = (DPMSG_SETGROUPOWNER*)pMsg;
            LPSTR   strGroupName = NULL;
            LPSTR   strNewOwnerName = NULL;
            LPSTR   strOldOwnerName = NULL;
            DPNAME* pGroupName = NULL;
            DPNAME* pNewOwnerName = NULL;
            DPNAME* pOldOwnerName = NULL;
            LPSTR   strDisplayFormat = "Owner changed for group \"%s\" to \"%s\" from \"%s\"\r\n";
            HRESULT hr;

            // Get pointer to Group name
            hr = GetGroupName( pDPInfo->pDP, p->idGroup, &pGroupName );
            if( FAILED(hr) )
                pGroupName = NULL;

            if( (pGroupName) && (pGroupName->lpszShortNameA) )
                strGroupName = pGroupName->lpszShortNameA;
            else
                strGroupName = "unknown";

            // Get pointer to new owner name
            hr = GetPlayerName( pDPInfo->pDP, p->idNewOwner, &pNewOwnerName );
            if( FAILED(hr) )
                pNewOwnerName = NULL;

            if( (pNewOwnerName) && (pNewOwnerName->lpszShortNameA) )
                strNewOwnerName = pNewOwnerName->lpszShortNameA;
            else
                strNewOwnerName = "unknown";

            // Get pointer to old owner name
            hr = GetPlayerName( pDPInfo->pDP, p->idOldOwner, &pOldOwnerName );
            if( FAILED(hr) )
                pOldOwnerName = NULL;

            if( (pOldOwnerName) && (pOldOwnerName->lpszShortNameA) )
                strOldOwnerName = pOldOwnerName->lpszShortNameA;
            else
                strOldOwnerName = "unknown";

            // Allocate space for string
            strStr = (LPSTR)GlobalAllocPtr( GHND, lstrlen(strDisplayFormat) +
                                                  lstrlen(strGroupName) +
                                                  lstrlen(strNewOwnerName) +
                                                  lstrlen(strOldOwnerName) + 1 );
            // Build string
            if( strStr )
                wsprintf( strStr, strDisplayFormat, strGroupName,
                          strNewOwnerName, strOldOwnerName );

            // Free data we allocated
            if( pGroupName )
                GlobalFreePtr( pGroupName );
            if( pNewOwnerName )
                GlobalFreePtr( pNewOwnerName );
            if( pOldOwnerName )
                GlobalFreePtr( pOldOwnerName );
            break;
        }

        case DPSYS_CHAT:
        {
            DPMSG_CHAT* p = (DPMSG_CHAT*)pMsg;
            DWORD dwSize = lstrlen( p->lpChat->lpszMessageA ) + 12;
                            //Allow extra room for "{whisper} ", in case this message
                            //was sent just to me.

            // Allocate space for string
            strStr = (LPSTR)GlobalAllocPtr( GHND, dwSize );

            if( strStr == NULL )
                break;

            if( NULL == p->idToGroup )
            {
                //This message was sent just to me, not to a whole group
                lstrcpy( strStr, "{whisper} " );
                lstrcat( strStr, p->lpChat->lpszMessageA );
            }
            else
            {
                // build string
                lstrcpy( strStr, p->lpChat->lpszMessageA );
            }
        }
        break;

    }

    // Post string to chat window
    if( strStr )
    {
        // Make sure window is still valid
        if( g_hMainWnd )
            PostMessage( g_hMainWnd, WM_USER_ADDSTRING, 0, (LPARAM)strStr );
        else
            GlobalFreePtr( strStr );
    }
}




//-----------------------------------------------------------------------------
// Name: GetPlayerName()
// Desc:
//-----------------------------------------------------------------------------
HRESULT GetPlayerName( LPDIRECTPLAY4A pDP, DPID dpidPlayer, DPNAME** ppName )
{
    DPNAME* pName;
    DWORD   dwNameSize;
    HRESULT hr;

    // Get size of player name data
    hr = pDP->GetPlayerName( dpidPlayer, NULL, &dwNameSize );
    if( hr != DPERR_BUFFERTOOSMALL )
        return hr;

    // make room for it
    pName = (DPNAME*)GlobalAllocPtr( GHND, dwNameSize );
    if( pName == NULL )
        return DPERR_OUTOFMEMORY;

    // Get player name data
    hr = pDP->GetPlayerName( dpidPlayer, pName, &dwNameSize );
    if( FAILED(hr) )
    {
        GlobalFreePtr( pName );
        return hr;
    }

    // Return pointer to name structure
    *ppName = pName;

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: GetGroupName()
// Desc:
//-----------------------------------------------------------------------------
HRESULT GetGroupName( LPDIRECTPLAY4A pDP, DPID dpidGroup, DPNAME** ppName )
{
    DPNAME* pName = NULL;
    DWORD   dwNameSize;
    HRESULT hr;

    // Get size of player name data
    hr = pDP->GetGroupName( dpidGroup, NULL, &dwNameSize );
    if( hr != DPERR_BUFFERTOOSMALL )
        return hr;

    // Make room for it
    pName = (DPNAME*)GlobalAllocPtr( GHND, dwNameSize );
    if( pName == NULL )
        return DPERR_OUTOFMEMORY;

    // Get player name data
    hr = pDP->GetGroupName( dpidGroup, pName, &dwNameSize );
    if( FAILED(hr) )
    {
        GlobalFreePtr(pName);
        return hr;
    }

    // Return pointer to name structure
    *ppName = pName;

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: InitializeLobbyGroupWindow()
// Desc:
//-----------------------------------------------------------------------------
VOID InitializeLobbyGroupWindow( HWND hWnd, LOBBYGROUPCONTEXT* pContext )
{
    HRESULT         hr = DPERR_GENERIC;
    ENUMCONNSTRUCT  enStruct;

    if( pContext->dpidRoom == 0 )
        SetWindowText( hWnd, "Create Room" );
    else
        SetWindowText( hWnd, "Create Table" );

    // Initialize max players
    SetDlgItemInt( hWnd, IDC_MAXPLAYERSEDIT, 0, FALSE );

    // Put all the DirectPlay applications in a combo box
    pContext->pDPInfo->pDPLobby->EnumLocalApplications( EnumApp, hWnd, 0 );
    SendDlgItemMessage( hWnd, IDC_APPCOMBO, CB_SETCURSEL, 0, 0 );

    // Put all the available connections in a combo box
    enStruct.hWnd    = hWnd;
    enStruct.idCombo = IDC_GROUPCONNECTIONSPCOMBO;

    hr = pContext->pDPInfo->pDP->EnumConnections( &BELLHOP_GUID, 
                                        DirectPlayEnumConnectionsCallback,
                                        &enStruct, DPCONNECTION_DIRECTPLAY );

    SendDlgItemMessage( hWnd, IDC_GROUPCONNECTIONSPCOMBO, CB_SETCURSEL, 
                        0, 0 );
}




//-----------------------------------------------------------------------------
// Name: CreateLobbyGroup()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CreateLobbyGroup( HWND hWnd, LOBBYGROUPCONTEXT* pContext )
{
    CHAR           strShortName[MAXSTRLEN];
    CHAR           strLongName[MAXSTRLEN];
    CHAR           strPassword[MAXSTRLEN];
    DPNAME         dpName;
    DWORD          dwMaxPlayers;
    DPID           dpID;
    DPLCONNECTION  dplconn;
    DPSESSIONDESC2 dpsess;
    HRESULT        hr;
    DWORD          dwFlags = 0;

    // Get strings from dialog
    GetDlgItemText( hWnd, IDC_SHORTNAMEEDIT, strShortName, MAXSTRLEN );
    GetDlgItemText( hWnd, IDC_LONGNAMEEDIT, strLongName, MAXSTRLEN );

    if( BST_CHECKED == SendDlgItemMessage( hWnd, IDC_STAGINGAREA, BM_GETCHECK, 0, 0 ) )
    {
        GetDlgItemText( hWnd, IDC_PASSWORDEDIT, strPassword, MAXSTRLEN );
        dwMaxPlayers = GetDlgItemInt( hWnd, IDC_MAXPLAYERSEDIT, NULL, FALSE );
        dwFlags |= DPGROUP_STAGINGAREA;
    }

    if( BST_CHECKED == SendDlgItemMessage( hWnd, IDC_HIDDEN, BM_GETCHECK, 0, 0 ) )
    {
        dwFlags |= DPGROUP_HIDDEN;
    }

    // Build a dpname structure
    ZeroMemory( &dpName, sizeof(DPNAME) );
    dpName.dwSize         = sizeof(DPNAME);
    dpName.lpszShortNameA = strShortName;
    dpName.lpszLongNameA  = strLongName;

    if( pContext->dpidRoom == 0 )
    {
        // Create a root group
        hr = pContext->pDPInfo->pDP->CreateGroup( &dpID, &dpName, NULL, 0,
                                                  dwFlags );
    }
    else
    {
        // Create the table group
        hr = pContext->pDPInfo->pDP->CreateGroupInGroup( pContext->dpidRoom, 
                                                         &dpID, &dpName, 
                                                         NULL, 0, dwFlags );
    }

    if( FAILED(hr) )
        return hr;

    if( DPGROUP_STAGINGAREA & dwFlags )
    {
        // Fill out the DPSESSIONDESC2 structure
        ZeroMemory( &dpsess, sizeof(DPSESSIONDESC2) );
        dpsess.dwSize  = sizeof( DPSESSIONDESC2 );
        dpsess.dwFlags = DPSESSION_KEEPALIVE | DPSESSION_MIGRATEHOST;

        CoCreateGuid( &(dpsess.guidInstance) );

        GetComboBoxGuid( hWnd, IDC_APPCOMBO, &dpsess.guidApplication );

        dpsess.dwMaxPlayers = dwMaxPlayers;
        dpsess.dwCurrentPlayers = 0;
        dpsess.lpszSessionNameA = dpName.lpszShortNameA;
        if( lstrlen(strPassword) )
            dpsess.lpszPasswordA = strPassword;

        // Fill out the DPLCONNECTION structure
        ZeroMemory( &dplconn, sizeof(DPLCONNECTION) );
        dplconn.dwSize        = sizeof( DPLCONNECTION );
        dplconn.lpSessionDesc = &dpsess;

        GetConnectionSPGuid( hWnd, IDC_GROUPCONNECTIONSPCOMBO,
                             &dplconn.guidSP );

        // The rest of the DPLCONNECTION structure gets 
        // filled in by the lobby
        hr = pContext->pDPInfo->pDP->SetGroupConnectionSettings( 0, dpID, &dplconn );

        if( FAILED(hr) )
            return hr;
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: LobbyGroupWndProc()
// Desc:
//-----------------------------------------------------------------------------
BOOL CALLBACK LobbyGroupWndProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                                 LPARAM lParam )
{
    LOBBYGROUPCONTEXT* pContext = (LOBBYGROUPCONTEXT*)GetWindowLong( hWnd, DWL_USER );

    switch( uMsg )
    {
        case WM_INITDIALOG:
            // Context passed in lParam
            pContext = (LOBBYGROUPCONTEXT*)lParam;

            // Save the globals with the window
            SetWindowLong( hWnd, DWL_USER, (LONG)pContext );
            
            // Initialize dialog with appropriate information
            InitializeLobbyGroupWindow( hWnd, pContext );
            break;

        case WM_DESTROY:
        {
            WPARAM  index;
            LRESULT pData;

            // Destroy the GUID's stored with each app name
            index = 0;
            while( TRUE )
            {
                pData = SendDlgItemMessage( hWnd, IDC_APPCOMBO, CB_GETITEMDATA,
                                            index, 0 );
                if( (pData == CB_ERR) || (pData == 0) )
                    break;

                GlobalFreePtr( (VOID*)pData );
                index++;
            }

            // Destroy the connection info in the combo box.
            index = 0;
            while( TRUE )
            {
                pData = SendDlgItemMessage( hWnd, IDC_GROUPCONNECTIONSPCOMBO,
                                            CB_GETITEMDATA, index, 0 );
                if( (pData == CB_ERR) || (pData == 0) )
                    break;

                GlobalFreePtr( (VOID*)pData );
                index++;
            }
            break;
        }

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDOK:
                    // save changes they made
                    CreateLobbyGroup( hWnd, pContext );
                    // Return success
                    EndDialog( hWnd, TRUE );
                    break;

                case IDCANCEL:
                    // Return failure
                    EndDialog( hWnd, FALSE );
                    break;

                case IDC_STAGINGAREA:
                {
                    int i = SendDlgItemMessage( hWnd, IDC_STAGINGAREA, BM_GETCHECK, 0, 0 );
                    EnableWindow( GetDlgItem( hWnd, IDC_PASSWORDEDIT ), (BST_CHECKED==i) );
                    EnableWindow( GetDlgItem( hWnd, IDC_APPCOMBO ), (BST_CHECKED==i) );
                    EnableWindow( GetDlgItem( hWnd, IDC_MAXPLAYERSEDIT ), (BST_CHECKED==i) );
                    EnableWindow( GetDlgItem( hWnd, IDC_GROUPCONNECTIONSPCOMBO ), (BST_CHECKED==i) );
                    break;
                }
            }
            break;
    }

    // Allow for default processing
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: DoGroupConnectionSettings()
// Desc:
//-----------------------------------------------------------------------------
HRESULT DoGroupConnectionSettings( HWND hWnd, DPLAYINFO* pDPInfo )
{
    LOBBYGROUPCONTEXT context;
    context.pDPInfo  = pDPInfo;
    context.dpidRoom = pDPInfo->pGroupTree->GetDPIDOfCurrentSelection();

    if( DialogBoxParam( g_hInstance,
                        MAKEINTRESOURCE(IDD_CONNECTIONSETTINGSDIALOG),
                        hWnd, (DLGPROC)ConnectionSettingsDialogProc,
                        (LPARAM)&context ) )
    {
        // something changed
        // We could update a status bar here.
    }

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: DoCreateRoom()
// Desc:
//-----------------------------------------------------------------------------
HRESULT DoCreateRoom( HWND hWnd, DPLAYINFO* pDPInfo )
{
    LOBBYGROUPCONTEXT context;
    context.pDPInfo  = pDPInfo;
    context.dpidRoom = (DPID)0;

    if( DialogBoxParam( g_hInstance,
                        MAKEINTRESOURCE(IDD_LOBBYGROUPDIALOG), hWnd,
                        (DLGPROC)LobbyGroupWndProc, (LPARAM)&context ) )
    {
        // something changed
        // We could update a status bar here.
    }

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: DoDeleteRoom()
// Desc:
//-----------------------------------------------------------------------------
HRESULT DoDeleteRoom( HWND hWnd, DPLAYINFO* pDPInfo )
{
    DPID dpidRoom = pDPInfo->pGroupTree->GetDPIDOfCurrentSelection();

    return pDPInfo->pDP->DestroyGroup( dpidRoom );
}




//-----------------------------------------------------------------------------
// Name: DoDeletePlayerFromGroup()
// Desc:
//-----------------------------------------------------------------------------
HRESULT DoDeletePlayerFromGroup( HWND hWnd, DPLAYINFO* pDPInfo )
{
    DPID dpidRoom   = pDPInfo->pGroupTree->GetDPIDOfCurrentSelectionParent();
    DPID dpidPlayer = pDPInfo->pGroupTree->GetDPIDOfCurrentSelection();

    // Delete the player from the group
    return pDPInfo->pDP->DeletePlayerFromGroup( dpidRoom, dpidPlayer );
}




//-----------------------------------------------------------------------------
// Name: DoCreateTable
// Desc:
//-----------------------------------------------------------------------------
HRESULT DoCreateTable( HWND hWnd, DPLAYINFO* pDPInfo )
{
    LOBBYGROUPCONTEXT   context;
    context.pDPInfo  = pDPInfo;
    context.dpidRoom = pDPInfo->pGroupTree->GetDPIDOfCurrentSelection();

    DialogBoxParam( g_hInstance, MAKEINTRESOURCE(IDD_LOBBYGROUPDIALOG), hWnd,
                    (DLGPROC)LobbyGroupWndProc, (LPARAM)&context );
    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: DoSetGroupOwner()
// Desc:
//-----------------------------------------------------------------------------
HRESULT DoSetGroupOwner( HWND hWnd, DPLAYINFO* pDPInfo )
{
    DPID dpidOwner = pDPInfo->pGroupTree->GetDPIDOfCurrentSelection();
    DPID dpidGroup = pDPInfo->pGroupTree->GetDPIDOfCurrentSelectionParent();

    return pDPInfo->pDP->SetGroupOwner( dpidGroup, dpidOwner );
}




//-----------------------------------------------------------------------------
// Name: DoDeleteTable()
// Desc:
//-----------------------------------------------------------------------------
HRESULT DoDeleteTable( HWND hWnd, DPLAYINFO* pDPInfo )
{
    DPID dpidRoom     = pDPInfo->pGroupTree->GetDPIDOfCurrentSelectionParent();
    DPID dpidShortcut = pDPInfo->pGroupTree->GetDPIDOfCurrentSelection();

    return pDPInfo->pDP->DeleteGroupFromGroup( dpidRoom, dpidShortcut );
}




//-----------------------------------------------------------------------------
// Name: DoLaunch()
// Desc:
//-----------------------------------------------------------------------------
HRESULT DoLaunch(HWND hWnd, DPLAYINFO* pDPInfo)
{
    DPID dpidRoom = pDPInfo->pGroupTree->GetDPIDOfCurrentSelection();

    return pDPInfo->pDP->StartSession( 0, dpidRoom );
}




//-----------------------------------------------------------------------------
// Name: InitializeLobby()
// Desc:
//-----------------------------------------------------------------------------
HRESULT InitializeLobby( HWND hWnd, DPLAYINFO* pDPInfo )
{
    DPNAME  name;
    DPID    dpID;
    HRESULT hr;

    if( pDPInfo->bIsHost )
    {   
        // Add some groups to start with
        ZeroMemory( &name, sizeof(DPNAME) );
        name.dwSize = sizeof(DPNAME);

        name.lpszShortNameA = "Golf Shack";
        hr = pDPInfo->pDP->CreateGroup( &dpID, &name, NULL, 0, 0 );
        if( FAILED(hr) )
            return hr;

        name.lpszShortNameA = "Monster Truck Rally";
        hr = pDPInfo->pDP->CreateGroup( &dpID, &name, NULL, 0, 0 );
        if( FAILED(hr) )
            return hr;

        name.lpszShortNameA = "Club Hellbender";
        hr = pDPInfo->pDP->CreateGroup( &dpID, &name, NULL, 0, 0 );
        if( FAILED(hr) )
            return hr;
    }

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: LogString()
// Desc:
//-----------------------------------------------------------------------------
VOID LogString( LPSTR strDisplayFormat, LPSTR strData )
{
    // Allocate space for string
    LPSTR strStr = (LPSTR) GlobalAllocPtr( GHND, lstrlen(strDisplayFormat) +
                                           lstrlen(strData) + 1 );
    if( strStr == NULL )
        return;

    // Build string
    wsprintf( strStr, strDisplayFormat, strData );

    // Post string to chat window
    // Make sure window is still valid
    if( g_hMainWnd )
        PostMessage( g_hMainWnd, WM_USER_ADDSTRING, 0, (LPARAM)strStr );
    else
        GlobalFreePtr( strStr );
}




//-----------------------------------------------------------------------------
// Name: SendChatMessage()
// Desc:
//-----------------------------------------------------------------------------
HRESULT SendChatMessage( HWND hWnd, DPLAYINFO* pDPInfo )
{
    LPSTR   strChatStr = NULL;
    LPSTR   strStr = NULL;
    LONG    lStrLen;
    HRESULT hr;
    DPCHAT  dpc;
    DPID    dpidTarget;

    // Get length of item text
    lStrLen = SendDlgItemMessage( hWnd, IDC_SENDEDIT, WM_GETTEXTLENGTH, 0, 0 );

    // Make room for it
    strChatStr = (LPSTR)GlobalAllocPtr(GHND, lStrLen + 1);
    if( strChatStr == NULL )
    {
        hr = DPERR_OUTOFMEMORY;
        goto FAILURE;
    }

    // Get item text
    lStrLen = GetDlgItemText( hWnd, IDC_SENDEDIT, strChatStr, lStrLen + 1 );

    // Create string to display this text
    hr = NewChatString( pDPInfo->pDP, pDPInfo->dpidPlayer, strChatStr, &strStr );
    if( FAILED(hr) )
        goto FAILURE;

    // Setup the DPCHAT struct
    memset( &dpc, 0, sizeof(DPCHAT) );
    dpc.dwSize = sizeof(DPCHAT);
    dpc.lpszMessageA = strStr;
    BRANCHSTRUCT bs;
    
    dpidTarget = pDPInfo->pGroupTree->GetDPIDOfCurrentSelection( &bs );

    hr = pDPInfo->pDP->SendChatMessage( pDPInfo->dpidPlayer, dpidTarget,
                                        DPSEND_GUARANTEED, &dpc );
    if( FAILED(hr) )
        goto FAILURE;

    // Display this string
    PostMessage( hWnd, WM_USER_ADDSTRING, 0, (LPARAM)strStr );
    strStr = NULL;                      // set to NULL so we don't delete it below

FAILURE:
    if( strChatStr )
        GlobalFreePtr( strChatStr );
    if( strStr )
        GlobalFreePtr( strStr );

    SetDlgItemText( hWnd, IDC_SENDEDIT, "" );

    return hr;
}




//-----------------------------------------------------------------------------
// Name: NewChatString()
// Desc:
//-----------------------------------------------------------------------------
HRESULT NewChatString( LPDIRECTPLAY4A pDP, DPID dpidPlayer, LPSTR strMsg,
                       LPSTR* pstrStr )
{
    DPNAME* pName  = NULL;
    LPSTR   strStr = NULL;
    LPSTR   strPlayerName;
    LPSTR   strDisplayFormat = "%s>\t%s\r\n";
    HRESULT hr;
    
    // Get name of player
    hr = GetPlayerName( pDP, dpidPlayer, &pName );
    if( FAILED(hr) )
        goto FAILURE;

    if( pName->lpszShortNameA )
        strPlayerName = pName->lpszShortNameA;
    else
        strPlayerName = "unknown";

    // Allocate space for display string
    strStr = (LPSTR)GlobalAllocPtr( GHND, lstrlen(strDisplayFormat) +
                                           lstrlen(strPlayerName) +
                                           lstrlen(strMsg) + 1);
    if( strStr == NULL )
    {
        hr = DPERR_OUTOFMEMORY;
        goto FAILURE;
    }

    // Build string
    wsprintf( strStr, strDisplayFormat, strPlayerName, strMsg );

    *pstrStr = strStr;
    strStr = NULL;

FAILURE:
    if( strStr )
        GlobalFreePtr( strStr );
    if( pName )
        GlobalFreePtr( pName );

    return hr;
}




//-----------------------------------------------------------------------------
// Name: WaitForRunMsg()
// Desc:
//-----------------------------------------------------------------------------
HRESULT WaitForRunMsg( DWORD dwAppId, HANDLE hReceiveEvent, DWORD* pdwStatus,
                       DPLAYINFO* pDPInfo )
{
    VOID*   pMsg          = NULL;
    DWORD   dwMsgFlags;
    DWORD   dwMsgSize     = 0;
    BOOL    bContinue     = TRUE;
    BOOL    bNotFinished  = TRUE;
    HRESULT hr            = E_FAIL;

    // For this function, we could have spun a seperate thread or integrated
    // the app/lobby client messaging into the main receive thread. 
    // For simplicity sake, the app/lobby client messaging loop is contained below. 

    // There are seven possible states that get us out of this function: 
    //  1. receive DPLSYS_DPLAYCONNECTSUCCEEDED
    //  2. receive DPLSYS_DPLAYCONNECTFAILED
    //  3. (Option) timeout waiting for hReceiveEvent
    //  4. DPERR_OUTOFMEMORY
    //  5. We receive an unexpected player-to-player msg.
    //  6. The app terminated.
    //  7. We get a system message we don't know about.
    //  8. We get an error calling receive

    *pdwStatus = 0;

    while( bNotFinished )
    {
        if( hReceiveEvent )
        {
            if( WAIT_TIMEOUT == WaitForSingleObject( hReceiveEvent, 20000 ) )
            {
                hr = DPERR_TIMEOUT;
                bNotFinished = FALSE;
                break;
            }
        }

        do
        {
            hr = pDPInfo->pDPLobby->ReceiveLobbyMessage( 0, dwAppId, &dwMsgFlags, 
                                                         pMsg, &dwMsgSize );

            switch( hr )
            {
                case DPERR_BUFFERTOOSMALL:
                {
                    if( pMsg )
                    {
                        GlobalFreePtr( pMsg );
                        pMsg = NULL;
                    }

                    pMsg = GlobalAllocPtr( GHND, dwMsgSize );
                    if( !pMsg )
                    {   hr = DPERR_OUTOFMEMORY;
                        goto end;
                    }
                    break;
                }

                case DP_OK:
                {
                    // This better be a system message
                    if( !(dwMsgFlags & DPLAD_SYSTEM) )
                    {
                        DPMSG_GENERIC* p = (DPMSG_GENERIC*)pMsg;
                        hr = DPERR_GENERIC;
                        goto end;
                    }

                    if( pMsg )
                    {
                        // Switch on the system message type
                        *pdwStatus = *(DWORD*)pMsg;

                        switch( *pdwStatus )
                        {
                            case DPLSYS_APPTERMINATED:
                                // App shut down
                                bNotFinished = FALSE;
                                bContinue = FALSE;
                                break;

                            case DPLSYS_CONNECTIONSETTINGSREAD:
                                break;

                            case DPLSYS_DPLAYCONNECTSUCCEEDED:
                            case DPLSYS_DPLAYCONNECTFAILED:
                                bNotFinished = FALSE;
                                break;

                            default:
                                // RUNDPMSGLOG: Unexpected system message type
                                hr = DPERR_GENERIC;
                                bContinue = FALSE;
                                bNotFinished = FALSE;
                                break;
                        }
                    }
                    break;
                }

                case DPERR_NOMESSAGES:
                    // There are no messages left. 
                    // We need to stop and wait for another
                    bContinue = FALSE;
                    break;

                default:
                    goto end;
                    break;
            }

        } while( bContinue );
    }

end:
    // Free resources
    GlobalFreePtr( pMsg );

    // Return 
    return( DPERR_NOMESSAGES == hr ? DP_OK : hr );
}




//-----------------------------------------------------------------------------
// Name: HandleStartSession()
// Desc:
//-----------------------------------------------------------------------------
VOID HandleStartSession( DPLCONNECTION* pConn, DPLAYINFO* pDPInfo )
{
    DWORD   dwAppID;
    CHAR    strStr[MAXSTRLEN];
    HRESULT hr;
    HANDLE  hEvent = CreateEvent( NULL, FALSE, FALSE, NULL );
    DWORD   dwStatus = 0;
    
    if( hEvent )
    {
        // Display name of application to launch
        hr = GetLocalAppName( pDPInfo->pDPLobby, 
                              &pConn->lpSessionDesc->guidApplication, strStr );
        if( FAILED(hr) )
            lstrcpy( strStr, "unknown" );
        
        LogString( "Launching \"%s\"...\r\n", strStr );
        
        // Call RunApplication
        hr = pDPInfo->pDPLobby->RunApplication( 0, &dwAppID, pConn, hEvent );

        if( FAILED(hr) )
        {
            ErrorBox( "Could not launch application because of error %s", hr );
        }
        else
        {
            // Wait for the app to launch
            hr = WaitForRunMsg( dwAppID, hEvent, &dwStatus, pDPInfo );
        }

        CloseHandle( hEvent );
    }
}




//-----------------------------------------------------------------------------
// Name: EnumApp()
// Desc:
//-----------------------------------------------------------------------------
BOOL FAR PASCAL EnumApp( const DPLAPPINFO* pAppInfo, VOID* pContext,
                         DWORD dwFlags )
{
    HWND    hWnd = (HWND)pContext;
    LRESULT iIndex;
    GUID*   pGuid;

    // Store application name in combo box
    iIndex = SendDlgItemMessage( hWnd, IDC_APPCOMBO, CB_ADDSTRING, 0,
                                 (LPARAM)pAppInfo->lpszAppNameA );
    if( iIndex == LB_ERR )
        return TRUE;

    // Make space for application GUID
    pGuid = (GUID*)GlobalAllocPtr( GHND, sizeof(GUID) );
    if( pGuid == NULL )
        return TRUE;

    // Store pointer to GUID in combo box
    *pGuid = pAppInfo->guidApplication;
    SendDlgItemMessage( hWnd, IDC_APPCOMBO, CB_SETITEMDATA, (WPARAM)iIndex,
                        (LPARAM)pGuid );

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: GetComboBoxGuid()
// Desc:
//-----------------------------------------------------------------------------
HRESULT GetComboBoxGuid( HWND hWnd, LONG iDialogItem, GUID* pguidReturn )
{
    // Get index of selected item
    LONG iIndex = SendDlgItemMessage( hWnd, iDialogItem, CB_GETCURSEL, 0, 0 );
    if( iIndex == CB_ERR )
        return DPERR_GENERIC;

    // Get data associated with this item
    iIndex = SendDlgItemMessage( hWnd, iDialogItem, CB_GETITEMDATA,
                                 (WPARAM)iIndex, 0 );
    if( (iIndex == CB_ERR) || (iIndex == 0) )
        return DPERR_GENERIC;

    // Data is a pointer to a guid
    *pguidReturn = *((GUID*)iIndex);

    return DP_OK;
}




//-----------------------------------------------------------------------------
// Name: GetLocalAppNameCallback()
// Desc:
//-----------------------------------------------------------------------------
BOOL FAR PASCAL GetLocalAppNameCallback( const DPLAPPINFO* pAppInfo,
                                         VOID* pContext, DWORD dwFlags )
{
    APPNAMECONTEXT* pAppNameContext = (APPNAMECONTEXT*)pContext;

    if( IsEqualGUID( pAppInfo->guidApplication, pAppNameContext->guidApplication ) )
    {
        lstrcpy( pAppNameContext->strAppName, pAppInfo->lpszAppNameA );
        return FALSE;
    }

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: GetLocalAppName()
// Desc:
//-----------------------------------------------------------------------------
HRESULT GetLocalAppName( LPDIRECTPLAYLOBBY pDPLobby,
                         GUID* pguidApplication, LPSTR strAppName )
{
    HRESULT         hr;
    APPNAMECONTEXT  AppNameContext;

    ZeroMemory( &AppNameContext, sizeof(APPNAMECONTEXT));
    AppNameContext.guidApplication = *pguidApplication;

    // Search local apps for matching guid
    hr = pDPLobby->EnumLocalApplications( GetLocalAppNameCallback,
                                          &AppNameContext, 0 );
    if( FAILED(hr) )
        return hr;

    // Return failure if no local app found matching this guid
    if( lstrlen( AppNameContext.strAppName ) == 0 )
        return DPERR_GENERIC;

    // Return name
    lstrcpy( strAppName, AppNameContext.strAppName );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: OnSize()
// Desc:
//-----------------------------------------------------------------------------
VOID OnSize( HWND hWnd, DPLAYINFO* pDPInfo )
{
    HDWP hDWP;
    RECT ClientRect;
    int  Height;
    int  xSpacing = pDPInfo->xSpacing;
    int  ySpacing = pDPInfo->ySpacing;
    HWND hKeyTreeWnd;
    HWND hValueListWnd;
    HWND hMsgEditWnd;
    HWND hSendButtonWnd;
    int  x;
    int  dx;
    RECT SendButtonRect;
    int  sendbuttonwidth;
    int  sendbuttonheight;

    if( IsIconic(hWnd) )
        return;

    if( (hDWP = BeginDeferWindowPos(4)) != NULL )
    {
        //  Data structure used when calling GetEffectiveClientRect (which takes into
        //  account space taken up by the toolbars/status bars).  First half of the
        //  pair is zero when at the end of the list, second half is the control id.
        int s_EffectiveClientRectData[] =
        {
            1, 0,                               //  For the menu bar, but is unused
            0, 0                                //  First zero marks end of data
        };

        GetEffectiveClientRect( hWnd, &ClientRect, s_EffectiveClientRectData );
        Height = ClientRect.bottom - ClientRect.top - (ySpacing * 5);

        // Resize the tree control
        hKeyTreeWnd = GetDlgItem( hWnd, IDT_MESSAGEVIEW );

        DeferWindowPos( hDWP, hKeyTreeWnd, NULL, 
                        xSpacing, ClientRect.top + ySpacing, 
                        pDPInfo->xPaneSplit, Height, 
                        SWP_NOZORDER | SWP_NOACTIVATE);

        x  = pDPInfo->xPaneSplit + pDPInfo->xHalfSplitWidth * 2;
        dx = ClientRect.right - ClientRect.left - x - xSpacing;

        // Resize the logging window
        hValueListWnd = GetDlgItem( hWnd, IDC_LOGEDIT );

        DeferWindowPos( hDWP, hValueListWnd, NULL, 
                        x, ClientRect.top+ySpacing, 
                        dx, Height,
                        SWP_NOZORDER | SWP_NOACTIVATE );

        // Move the Send button, its size is constant
        hSendButtonWnd = GetDlgItem( hWnd, IDC_SENDBUTTON );

        GetWindowRect( hSendButtonWnd, &SendButtonRect );
        sendbuttonwidth = SendButtonRect.right - SendButtonRect.left; 
        sendbuttonheight = SendButtonRect.bottom - SendButtonRect.top; 

        DeferWindowPos( hDWP, hSendButtonWnd, NULL, 
                        ClientRect.right - ( sendbuttonwidth + xSpacing ), 
                        ClientRect.bottom - ( sendbuttonheight + ySpacing ), 
                        sendbuttonwidth, sendbuttonheight,
                        SWP_NOZORDER | SWP_NOACTIVATE );

        // Resize and move the message edit control
        hMsgEditWnd = GetDlgItem( hWnd, IDC_SENDEDIT );

        DeferWindowPos( hDWP, hMsgEditWnd, NULL, 
                        ClientRect.left + xSpacing, 
                        ClientRect.bottom - ( sendbuttonheight + ySpacing ), 
                        ClientRect.right - ClientRect.left - (sendbuttonwidth + xSpacing * 3), 
                        sendbuttonheight, SWP_NOZORDER | SWP_NOACTIVATE );

        EndDeferWindowPos( hDWP );
    }
}




//-----------------------------------------------------------------------------
// Name: ErrorBox()
// Desc:
//-----------------------------------------------------------------------------
VOID ErrorBox( LPSTR strError, HRESULT hr )
{
    CHAR strStr[MAXSTRLEN];
    wsprintf( strStr, strError, GetDirectPlayErrStr(hr) );

    MessageBox( NULL, strStr, "Error", MB_OK );
}




//-----------------------------------------------------------------------------
// Name: GetDirectPlayErrStr()
// Desc:
//-----------------------------------------------------------------------------
CHAR* GetDirectPlayErrStr( HRESULT hr )
{
    switch( hr )
    {
        case DP_OK: return ("DP_OK");
        case DPERR_ALREADYINITIALIZED: return ("DPERR_ALREADYINITIALIZED");
        case DPERR_ACCESSDENIED: return ("DPERR_ACCESSDENIED");
        case DPERR_ACTIVEPLAYERS: return ("DPERR_ACTIVEPLAYERS");
        case DPERR_BUFFERTOOSMALL: return ("DPERR_BUFFERTOOSMALL");
        case DPERR_CANTADDPLAYER: return ("DPERR_CANTADDPLAYER");
        case DPERR_CANTCREATEGROUP: return ("DPERR_CANTCREATEGROUP");
        case DPERR_CANTCREATEPLAYER: return ("DPERR_CANTCREATEPLAYER");
        case DPERR_CANTCREATESESSION: return ("DPERR_CANTCREATESESSION");
        case DPERR_CAPSNOTAVAILABLEYET: return ("DPERR_CAPSNOTAVAILABLEYET");
        case DPERR_EXCEPTION: return ("DPERR_EXCEPTION");
        case DPERR_GENERIC: return ("DPERR_GENERIC");
        case DPERR_INVALIDFLAGS: return ("DPERR_INVALIDFLAGS");
        case DPERR_INVALIDOBJECT: return ("DPERR_INVALIDOBJECT");
    //  case DPERR_INVALIDPARAM: return ("DPERR_INVALIDPARAM");  dup value
        case DPERR_INVALIDPARAMS: return ("DPERR_INVALIDPARAMS");
        case DPERR_INVALIDPLAYER: return ("DPERR_INVALIDPLAYER");
        case DPERR_INVALIDGROUP: return ("DPERR_INVALIDGROUP");
        case DPERR_NOCAPS: return ("DPERR_NOCAPS");
        case DPERR_NOCONNECTION: return ("DPERR_NOCONNECTION");
    //  case DPERR_NOMEMORY: return ("DPERR_NOMEMORY");     dup value
        case DPERR_OUTOFMEMORY: return ("DPERR_OUTOFMEMORY");
        case DPERR_NOMESSAGES: return ("DPERR_NOMESSAGES");
        case DPERR_NONAMESERVERFOUND: return ("DPERR_NONAMESERVERFOUND");
        case DPERR_NOPLAYERS: return ("DPERR_NOPLAYERS");
        case DPERR_NOSESSIONS: return ("DPERR_NOSESSIONS");
        case DPERR_PENDING: return ("DPERR_PENDING");
        case DPERR_SENDTOOBIG: return ("DPERR_SENDTOOBIG");
        case DPERR_TIMEOUT: return ("DPERR_TIMEOUT");
        case DPERR_UNAVAILABLE: return ("DPERR_UNAVAILABLE");
        case DPERR_UNSUPPORTED: return ("DPERR_UNSUPPORTED");
        case DPERR_BUSY: return ("DPERR_BUSY");
        case DPERR_USERCANCEL: return ("DPERR_USERCANCEL");
        case DPERR_NOINTERFACE: return ("DPERR_NOINTERFACE");
        case DPERR_CANNOTCREATESERVER: return ("DPERR_CANNOTCREATESERVER");
        case DPERR_PLAYERLOST: return ("DPERR_PLAYERLOST");
        case DPERR_SESSIONLOST: return ("DPERR_SESSIONLOST");
        case DPERR_UNINITIALIZED: return ("DPERR_UNINITIALIZED");
        case DPERR_NONEWPLAYERS: return ("DPERR_NONEWPLAYERS");
        case DPERR_INVALIDPASSWORD: return ("DPERR_INVALIDPASSWORD");
        case DPERR_CONNECTING: return ("DPERR_CONNECTING");
        case DPERR_CONNECTIONLOST: return ("DPERR_CONNECTIONLOST");
        case DPERR_UNKNOWNMESSAGE: return ("DPERR_UNKNOWNMESSAGE");
        case DPERR_CANCELFAILED: return ("DPERR_CANCELFAILED");
        case DPERR_INVALIDPRIORITY: return ("DPERR_INVALIDPRIORITY");
        case DPERR_NOTHANDLED: return ("DPERR_NOTHANDLED");
        case DPERR_CANCELLED: return ("DPERR_CANCELLED");
        case DPERR_ABORTED: return ("DPERR_ABORTED");
        case DPERR_BUFFERTOOLARGE: return ("DPERR_BUFFERTOOLARGE");
        case DPERR_CANTCREATEPROCESS: return ("DPERR_CANTCREATEPROCESS");
        case DPERR_APPNOTSTARTED: return ("DPERR_APPNOTSTARTED");
        case DPERR_INVALIDINTERFACE: return ("DPERR_INVALIDINTERFACE");
        case DPERR_NOSERVICEPROVIDER: return ("DPERR_NOSERVICEPROVIDER");
        case DPERR_UNKNOWNAPPLICATION: return ("DPERR_UNKNOWNAPPLICATION");
        case DPERR_NOTLOBBIED: return ("DPERR_NOTLOBBIED");
        case DPERR_SERVICEPROVIDERLOADED: return ("DPERR_SERVICEPROVIDERLOADED");
        case DPERR_ALREADYREGISTERED: return ("DPERR_ALREADYREGISTERED");
        case DPERR_NOTREGISTERED: return ("DPERR_NOTREGISTERED");
        case DPERR_AUTHENTICATIONFAILED: return ("DPERR_AUTHENTICATIONFAILED");
        case DPERR_CANTLOADSSPI: return ("DPERR_CANTLOADSSPI");
        case DPERR_ENCRYPTIONFAILED: return ("DPERR_ENCRYPTIONFAILED");
        case DPERR_SIGNFAILED: return ("DPERR_SIGNFAILED");
        case DPERR_CANTLOADSECURITYPACKAGE: return ("DPERR_CANTLOADSECURITYPACKAGE");
        case DPERR_ENCRYPTIONNOTSUPPORTED: return ("DPERR_ENCRYPTIONNOTSUPPORTED");
        case DPERR_CANTLOADCAPI: return ("DPERR_CANTLOADCAPI");
        case DPERR_NOTLOGGEDIN: return ("DPERR_NOTLOGGEDIN");
        case DPERR_LOGONDENIED: return ("DPERR_LOGONDENIED");
    }

    // For errors not in the list, return HRESULT string
    static CHAR strTemp[12];
    wsprintf( strTemp, "0x%08X", hr );
    return strTemp;
}



