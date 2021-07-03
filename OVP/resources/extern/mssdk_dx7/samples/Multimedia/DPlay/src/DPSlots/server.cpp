//-----------------------------------------------------------------------------
// File: Server.cpp
//
// Desc: Slot machine server using DirectPlay.
//
// Copyright (C) 1996-1999 Microsoft Corporation.  All Rights Reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include <windowsx.h>
#include <stdio.h>
#include "dpslots.h"
#include "resource.h"




//-----------------------------------------------------------------------------
// Globals
//-----------------------------------------------------------------------------
const UINT  MAX_RECORD_LENGTH   = 256;          // Max database record size
const DWORD INDEX_JACKPOT       = 3;            // Index of jackpot image
const DWORD INDEX_SPOILER       = 5;            // Index of spoiler image

// Window messages
const UINT  WM_USER_ADDSTRING = WM_USER + 257;  // Add string to log window

// Structures
struct ACCOUNTINFO
{
    DWORD dwBalance;                            // Acct record from database
};

// globals
HWND  g_hwndServer = NULL; // Main window
FILE* g_pFile      = NULL; // Database file




//-----------------------------------------------------------------------------
// Function prototypes
//-----------------------------------------------------------------------------
HRESULT HandleBalanceRequest( DPLAYINFO* pDPInfo, MSG_BALANCEREQUEST* pBalance,
                              DPID idTo );
HRESULT HandleSpinRequest( DPLAYINFO* pDPInfo, MSG_SPINREQUEST* pBalance,
                           DPID idTo );
void    LogRequest( LPSTR strFormat, DPACCOUNTDESC* pAccountDesc,
                    DWORD dwValue );
LONG    GetAmountWonOrLost( DWORD dwAmountBet, DWORD dwIndex[] );
FILE*   OpenAccountDB( LPSTR strDBName );
void    CloseAccountDB( FILE* pFile );
BOOL    QueryAccount( FILE* pFile, LPSTR strAccountID,
                      ACCOUNTINFO* pAccountInfo );
BOOL    UpdateAccount( FILE* pFile, LPSTR strAccountID,
                       ACCOUNTINFO* pAccountInfo );
HRESULT GetAccountDesc( DPLAYINFO* pDPInfo, DPID idPlayer,
                        DPACCOUNTDESC** ppAccountDesc );




//-----------------------------------------------------------------------------
// Name: _strupr()
// Desc: Borland CBuilder3 is missing a _strupr(), so here's one
//-----------------------------------------------------------------------------
VOID __strupr( CHAR* str )
{
    while( *str )
    {
        if( islower( *str ) )
            *str = toupper( *str );
        str++;
    }
}




//-----------------------------------------------------------------------------
// Name: ServerWndProc()
// Desc:
//-----------------------------------------------------------------------------
BOOL CALLBACK ServerWndProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                             LPARAM lParam )
{
    static DPLAYINFO* pDPInfo = NULL;
    static UINT       idTimer = 0;
    static FILE*      pFile = NULL;
    DWORD             dwTextLen;

    switch( uMsg )
    {
        case WM_INITDIALOG:
            // Save the connection info pointer
            pDPInfo = (DPLAYINFO*)lParam;

            // Store global window
            g_hwndServer = hWnd;

            // Open account database
            g_pFile = OpenAccountDB( g_strDatabaseName );
            
            if( NULL == g_pFile )
            {
                // Create a buffer for the output string
                LPSTR strStr = (LPSTR)GlobalAllocPtr( GHND, 80 );
                if( strStr )
                {
                    strcpy( strStr, "Could not open database file! "
                                    "Make sure the file exists and restart.\r\n" );
                    PostMessage( g_hwndServer, WM_USER_ADDSTRING, 0, (LPARAM)strStr );
                }
            }

            break;

        case WM_DESTROY:
            // Stop the timer
            if( idTimer )
                KillTimer( hWnd, idTimer ); 
            idTimer = 0;

            // Close account database
            if( g_pFile )
                CloseAccountDB( g_pFile );

            g_hwndServer = NULL;
            break;

        case WM_USER_ADDSTRING:
            // This is a user-defined message used to add strings
            // Get length of text in log window
            dwTextLen = SendDlgItemMessage( hWnd, IDC_LOGEDIT,
                                            WM_GETTEXTLENGTH, 0, 0 );

            // Put selection at end
            dwTextLen = SendDlgItemMessage( hWnd, IDC_LOGEDIT, EM_SETSEL,
                                            (WPARAM)dwTextLen,
                                            (LPARAM)dwTextLen );

            // Add string in lParam to log window
            SendDlgItemMessage( hWnd, IDC_LOGEDIT, EM_REPLACESEL,
                                (WPARAM) FALSE,(LPARAM)lParam );
            GlobalFreePtr( (VOID*)lParam );
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDCANCEL:
                    EndDialog( hWnd, FALSE );
                    break;
            }
            break;
    }

    // Allow for default processing
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: ServerApplicationMessage()
// Desc:
//-----------------------------------------------------------------------------
VOID ServerApplicationMessage( DPLAYINFO* pDPInfo, DPMSG_GENERIC* pMsg,
                               DWORD dwMsgSize, DPID idFrom, DPID idTo )
{
    switch( pMsg->dwType )
    {
        case BALANCEREQUEST:
            HandleBalanceRequest( pDPInfo, (MSG_BALANCEREQUEST*)pMsg, idFrom );
            break;

        case SPINREQUEST:
            HandleSpinRequest( pDPInfo, (MSG_SPINREQUEST*)pMsg, idFrom );
            break;
    }
}




//-----------------------------------------------------------------------------
// Name: ServerSystemMessage()
// Desc:
//-----------------------------------------------------------------------------
VOID ServerSystemMessage( DPLAYINFO* pDPInfo, DPMSG_GENERIC* pMsg,
                          DWORD dwMsgSize, DPID idFrom, DPID idTo )
{
    // The body of each case is there so you can set a breakpoint and examine
    // the contents of the message received.
    switch( pMsg->dwType )
    {
        case DPSYS_CREATEPLAYERORGROUP:
        {
            DPMSG_CREATEPLAYERORGROUP* p = (DPMSG_CREATEPLAYERORGROUP*)pMsg;
            break;
        }

        case DPSYS_DESTROYPLAYERORGROUP:
        {
            DPMSG_DESTROYPLAYERORGROUP* p = (DPMSG_DESTROYPLAYERORGROUP*)pMsg;
            break;
        }

        case DPSYS_ADDPLAYERTOGROUP:
        {
            DPMSG_ADDPLAYERTOGROUP* p = (DPMSG_ADDPLAYERTOGROUP*)pMsg;
            break;
        }

        case DPSYS_DELETEPLAYERFROMGROUP:
        {
            DPMSG_DELETEPLAYERFROMGROUP* p = (DPMSG_DELETEPLAYERFROMGROUP*)pMsg;
            break;
        }

        case DPSYS_SESSIONLOST:
        {
            DPMSG_SESSIONLOST* p = (DPMSG_SESSIONLOST*)pMsg;
            break;
        }

        case DPSYS_HOST:
        {
            DPMSG_HOST* p = (DPMSG_HOST*)pMsg;
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

        case DPSYS_SECUREMESSAGE:
        {
            DPMSG_SECUREMESSAGE* p = (DPMSG_SECUREMESSAGE*)pMsg;

            ServerApplicationMessage( pDPInfo, (DPMSG_GENERIC*)p->lpData, 
                                      p->dwDataSize, p->dpIdFrom, idTo );

            break;
        }
    }
}




//-----------------------------------------------------------------------------
// Name: HandleBalanceRequest()
// Desc:
//-----------------------------------------------------------------------------
HRESULT HandleBalanceRequest( DPLAYINFO* pDPInfo,
                              MSG_BALANCEREQUEST* pBalance, DPID idTo )
{
    MSG_BALANCERESPONSE Msg;
    DPACCOUNTDESC*      pAccountDesc = NULL;
    ACCOUNTINFO         AccountInfo;
    HRESULT             hr;

    // Create balance response message
    ZeroMemory( &Msg, sizeof(MSG_BALANCERESPONSE) );
    Msg.dwType = BALANCERESPONSE;

    // Get account description for this player
    hr = GetAccountDesc( pDPInfo, idTo, &pAccountDesc );
    if( FAILED(hr) )
        goto FAILURE;

    // Get account information from database using account ID
    if( ( g_pFile == NULL ) ||
        ( !QueryAccount( g_pFile, pAccountDesc->lpszAccountIDA, &AccountInfo) ) )
    {
        hr = DPERR_ACCESSDENIED;
        goto FAILURE;
    }

    // Return balance from database
    Msg.dwBalance = AccountInfo.dwBalance;

FAILURE:
    Msg.hr = hr;

    if( FAILED(hr) )
    {
        LogRequest( "Balance request for \"%s\" failed: 0x%08X\r\n", pAccountDesc, hr );
        LogRequest( "You need to manually create an account "
                    "in the database file.\r\n", pAccountDesc, 0 );
    }
    else
    {
        LogRequest( "Balance request for \"%s\" returned $%d\r\n", pAccountDesc, Msg.dwBalance );
    }

    if( pAccountDesc )
        GlobalFreePtr( pAccountDesc );

    // Send the message
    return pDPInfo->pDPlay->Send( pDPInfo->dpidPlayer, idTo,
                                  SENDFLAGS(pDPInfo->bIsSecure),
                                  &Msg, sizeof(MSG_BALANCERESPONSE) );
}




//-----------------------------------------------------------------------------
// Name: HandleSpinRequest()
// Desc:
//-----------------------------------------------------------------------------
HRESULT HandleSpinRequest( DPLAYINFO* pDPInfo, MSG_SPINREQUEST* pSpin,
                           DPID idTo )
{
    MSG_SPINRESPONSE Msg;
    DPACCOUNTDESC*   pAccountDesc = NULL;
    ACCOUNTINFO      AccountInfo;
    HRESULT hr;
    DWORD   i;

    // Create spin response message
    ZeroMemory( &Msg, sizeof(MSG_SPINRESPONSE) );
    Msg.dwType = SPINRESPONSE;

    // Get account description for this player
    hr = GetAccountDesc(pDPInfo, idTo, &pAccountDesc);
    if FAILED(hr)
        goto FAILURE;

    // Get account information from database using account ID
    if( (g_pFile == NULL ) ||
        ( !QueryAccount( g_pFile, pAccountDesc->lpszAccountIDA, &AccountInfo ) ) )
    {
        hr = DPERR_ACCESSDENIED;
        goto FAILURE;
    }

    // Bet exceeds balance in database
    if( pSpin->dwAmountBet > AccountInfo.dwBalance )
    {
        hr = DPERR_UNAVAILABLE;
        goto FAILURE;
    }

    // Generate new slot settings
    for( i = 0; i < NUMWHEELS; i++ )
        Msg.dwIndex[i] = ((DWORD)rand()) % SLOTSPERWHEEL;

    // Determine amount won or lost
    Msg.dwAmountWonOrLost = GetAmountWonOrLost( pSpin->dwAmountBet, Msg.dwIndex );

    // Update account info in database for this player
    AccountInfo.dwBalance += Msg.dwAmountWonOrLost;

    if( !UpdateAccount( g_pFile, pAccountDesc->lpszAccountIDA, &AccountInfo ) )
    {
        hr = DPERR_ACCESSDENIED;
        goto FAILURE;
    }

    // Send new balance back
    Msg.dwBalance = AccountInfo.dwBalance;

FAILURE:
    Msg.hr = hr;

    if( FAILED(hr) )
        LogRequest( "Spin request for \"%s\" failed: 0x%08X\r\n", pAccountDesc, hr );
    else
        LogRequest( "Spin request for \"%s\" returned $%d\r\n", pAccountDesc, Msg.dwAmountWonOrLost);

    if( pAccountDesc )
        GlobalFreePtr( pAccountDesc );

    // Send the message
    return pDPInfo->pDPlay->Send( pDPInfo->dpidPlayer, idTo,
                                  SENDFLAGS(pDPInfo->bIsSecure),
                                  &Msg, sizeof(MSG_SPINRESPONSE) );
}




//-----------------------------------------------------------------------------
// Name: LogRequest()
// Desc:
//-----------------------------------------------------------------------------
VOID LogRequest( LPSTR strFormat, DPACCOUNTDESC* pAccountDesc, DWORD dwValue )
{
    LPSTR strStr;
    LPSTR strAccountID;

    // Make sure we have an account ID
    if( pAccountDesc == NULL )
        strAccountID = "unknown";
    else
        strAccountID = pAccountDesc->lpszAccountIDA;

    // Create a buffer for the output string, account string and a numeric value
    strStr = (LPSTR)GlobalAllocPtr( GHND, strlen(strFormat) + strlen(strAccountID) + 10 );
    if( strStr == NULL )
        return;

    // Format the string to log
    wsprintf( strStr, strFormat, strAccountID, dwValue );

    // Log it - main wnd proc will dispose of the string
    PostMessage( g_hwndServer, WM_USER_ADDSTRING, 0, (LPARAM)strStr );
}




//-----------------------------------------------------------------------------
// Name: GetAmountWonOrLost()
// Desc:
//-----------------------------------------------------------------------------
LONG GetAmountWonOrLost( DWORD dwAmountBet, DWORD dwIndex[] )
{
    LONG    nMultiplier;

    // Check wheels for winning combos
    if( (dwIndex[0] == INDEX_JACKPOT) &&
        (dwIndex[1] == INDEX_JACKPOT) &&
        (dwIndex[2] == INDEX_JACKPOT) )
    {
        // Check for jackpot
        nMultiplier = 100;
    }
    else if( (dwIndex[0] == dwIndex[1]) &&
             (dwIndex[1] == dwIndex[2]) )
    {
        // Three in a row
        nMultiplier = 25;
    }
    else if( dwIndex[0] == dwIndex[1] )
    {
        // First two match
        nMultiplier = 5;
    }
    else
    {
        // player loses
        nMultiplier = -1;
    }

    // Any spoiler and player loses
    if( (dwIndex[0] == INDEX_SPOILER) ||
        (dwIndex[1] == INDEX_SPOILER) ||
        (dwIndex[2] == INDEX_SPOILER) )
    {
        nMultiplier = -1;
    }

    // Return amount won or lost
    return( dwAmountBet * nMultiplier );
}




//-----------------------------------------------------------------------------
// Name: OpenAccountDB()
// Desc:
//-----------------------------------------------------------------------------
FILE* OpenAccountDB( LPSTR strDBName )
{
    return fopen( strDBName, "r+b" );
}




//-----------------------------------------------------------------------------
// Name: CloseAccountDB()
// Desc:
//-----------------------------------------------------------------------------
VOID CloseAccountDB( FILE* pFile )
{
    fclose( pFile );
}




//-----------------------------------------------------------------------------
// Name: GetRecord()
// Desc:
//-----------------------------------------------------------------------------
BOOL GetRecord( FILE* pFile, LPSTR strKey, LPSTR strRecord )
{
    rewind( pFile );
    
    while( fgets( strRecord, MAX_RECORD_LENGTH, pFile ) )
    {
        __strupr( strRecord );
        if( !strncmp( strRecord, strKey, strlen( strKey ) ) )
            return TRUE;
    }

    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: PutRecord()
// Desc:
//-----------------------------------------------------------------------------
BOOL PutRecord( FILE* pFile, LPSTR strKey, LPSTR strRecord )
{
    CHAR    strLine[MAX_RECORD_LENGTH];
    DWORD   dwRecordIndex;

    rewind( pFile );
    dwRecordIndex = 0;
    
    while( fgets( strLine, MAX_RECORD_LENGTH, pFile ) )
    {
        __strupr( strLine );
        if (!strncmp( strLine, strKey, strlen(strKey) ) )
        {
            fseek( pFile, dwRecordIndex, SEEK_SET );
            fputs( strRecord, pFile );
            return TRUE;
        }
        dwRecordIndex += strlen( strLine );
    }

    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: QueryAccount()
// Desc:
//-----------------------------------------------------------------------------
BOOL QueryAccount(FILE* pFile, LPSTR strAccountID, ACCOUNTINFO* pAccountInfo )
{
    CHAR  strBuffer[MAX_RECORD_LENGTH];
    CHAR* strToken;

    if( !GetRecord( pFile, strAccountID, strBuffer ) )
        return FALSE;

    strToken = strtok( strBuffer, "," );
    if( strToken )
    {
        strToken = strtok( NULL, "\n" );
        if( strToken )
        {
            pAccountInfo->dwBalance = atoi( strToken );
        }
    }

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: UpdateAccount()
// Desc:
//-----------------------------------------------------------------------------
BOOL UpdateAccount(FILE* pFile, LPSTR strAccountID, ACCOUNTINFO* pAccountInfo )
{
    CHAR strBuffer[MAX_RECORD_LENGTH];

    if( !GetRecord( pFile, strAccountID, strBuffer ) )
        return FALSE;

    sprintf( strBuffer, "%s,%8d", strAccountID, pAccountInfo->dwBalance );
    return PutRecord( pFile, strAccountID, strBuffer );
}




//-----------------------------------------------------------------------------
// Name: GetSecureAccountDesc()
// Desc:
//-----------------------------------------------------------------------------
HRESULT GetSecureAccountDesc( DPLAYINFO* pDPInfo, DPID idPlayer,
                              DPACCOUNTDESC** ppAccountDesc )
{
    DPACCOUNTDESC* pAccountDesc = NULL;
    DWORD          dwAccountDescSize;
    HRESULT        hr;

    // Get size of account description
    hr = pDPInfo->pDPlay->GetPlayerAccount( idPlayer, 0, NULL,
                                            &dwAccountDescSize );
    if( hr != DPERR_BUFFERTOOSMALL )
        goto FAILURE;

    // Make room for it
    pAccountDesc = (DPACCOUNTDESC*)GlobalAllocPtr( GHND, dwAccountDescSize );
    if( pAccountDesc == NULL )
    {
        hr = DPERR_OUTOFMEMORY;
        goto FAILURE;
    }

    // Get the account description for this player
    hr = pDPInfo->pDPlay->GetPlayerAccount( idPlayer, 0, pAccountDesc,
                                            &dwAccountDescSize );
    if( FAILED(hr) )
        goto FAILURE;

    // Return account description
    __strupr( pAccountDesc->lpszAccountIDA );
    *ppAccountDesc = pAccountDesc;
    pAccountDesc = NULL;

FAILURE:
    if( pAccountDesc )
        GlobalFreePtr( pAccountDesc );

    return hr;
}




//-----------------------------------------------------------------------------
// Name: GetUnsecureAccountDesc()
// Desc:
//-----------------------------------------------------------------------------
HRESULT GetUnsecureAccountDesc( DPLAYINFO* pDPInfo, DPID idPlayer,
                                DPACCOUNTDESC** ppAccountDesc )
{
    DPACCOUNTDESC* pAccountDesc = NULL;
    DWORD          dwAccountDescSize;
    DPNAME*        pName;
    HRESULT        hr;

    // Get size of player name
    hr = pDPInfo->pDPlay->GetPlayerName( idPlayer, NULL,
                                         &dwAccountDescSize );
    if( hr != DPERR_BUFFERTOOSMALL )
        goto FAILURE;

    // Make room for it
    pAccountDesc = (DPACCOUNTDESC*)GlobalAllocPtr( GHND, sizeof(DPACCOUNTDESC) + dwAccountDescSize );
    if( pAccountDesc == NULL )
    {
        hr = DPERR_OUTOFMEMORY;
        goto FAILURE;
    }

    // Get the player name
    pName = (DPNAME*)(((LPSTR)pAccountDesc) + sizeof(DPACCOUNTDESC));
    hr = pDPInfo->pDPlay->GetPlayerName( idPlayer, pName,
                                         &dwAccountDescSize );
    if( FAILED(hr) )
        goto FAILURE;

    // Return account description
    pAccountDesc->lpszAccountIDA = pName->lpszShortNameA;
    __strupr( pAccountDesc->lpszAccountIDA );
    *ppAccountDesc = pAccountDesc;
    pAccountDesc = NULL;

FAILURE:
    if( pAccountDesc )
        GlobalFreePtr( pAccountDesc );

    return hr;
}




//-----------------------------------------------------------------------------
// Name: GetAccountDesc()
// Desc:
//-----------------------------------------------------------------------------
HRESULT GetAccountDesc( DPLAYINFO* pDPInfo, DPID idPlayer,
                        DPACCOUNTDESC** ppAccountDesc )
{
    if( pDPInfo->bIsSecure )
    {
        return GetSecureAccountDesc( pDPInfo, idPlayer, ppAccountDesc );
    }
    else
    {
        return GetUnsecureAccountDesc( pDPInfo, idPlayer, ppAccountDesc );
    }
}



