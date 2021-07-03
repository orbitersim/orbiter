//-----------------------------------------------------------------------------
// File: DPStage.cpp
//
// Desc: Support file for a DirectPlay stage.  The stage allows all 
//       players connected to the same session to chat, and start a new game
//       at the same time when everyone is ready and the host player decides 
//       to begin.  The host player may also reject players or close player 
//       slots.  This allows host player to control who is allowed to join 
//       the game.  
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#define IDIRECTPLAY2_OR_GREATER
#define STRICT
#include <windows.h>
#include <DPlay.h>
#include <stdio.h>
#include "resource.h"




//-----------------------------------------------------------------------------
// Defines, and structures
//-----------------------------------------------------------------------------
#define SAFE_DELETE(p)       { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_DELETE_ARRAY(p) { if(p) { delete[] (p);   (p)=NULL; } }
#define SAFE_RELEASE(p)      { if(p) { (p)->Release(); (p)=NULL; } }

#define MAX_PLAYER_NAME      14
#define MAX_PLAYER_SLOTS     10
#define MAX_CHAT_STRINGS     50

#define SLOT_BUTTON_MASK     0x0F
#define SLOT_BUTTON_OPEN     0x00
#define SLOT_BUTTON_CLOSED   0x01
#define SLOT_BUTTON_FULL     0x02

#define SLOT_READY_MASK      0xF0
#define SLOT_READY_CHECKED   0x10

#define EXITCODE_FORWARD     1  // The dialog completed successfully, so go forward
#define EXITCODE_BACKUP      2  // The dialog was canceled and the previous 
                                // connection dialog should be shown
#define EXITCODE_QUIT        3  // The dialog was closed and the app should end
#define EXITCODE_ERROR       4  // An error occurred, so the app should exit
#define EXITCODE_REJECTED    5  // The player was rejected from the game, so 
                                // the previous connection dialog should be shown
#define EXITCODE_SESSIONLOST 6  // The DirectPlay session was lost, so 
                                // the previous connection dialog should be shown




//-----------------------------------------------------------------------------
// App specific DirectPlay messages and structures 
//-----------------------------------------------------------------------------
#define STAGEMSG_SETSLOTID     1
#define STAGEMSG_SETSTAGEDATA  2
#define STAGEMSG_REJECTPLAYER  3
#define STAGEMSG_SLOTCHECK     4
#define STAGEMSG_SLOTUNCHECK   5
#define STAGEMSG_STARTGAME     6
#define STAGEMSG_CANCELGAME    7

struct STAGEMSG_GENERIC
{
    DWORD dwType;
};

struct STAGEMSG_SLOT : public STAGEMSG_GENERIC
{
    DWORD dwSlotNumber;
};

struct STAGEMSG_STAGEDATA : public STAGEMSG_GENERIC
{
    DWORD dwSlotStatus[MAX_PLAYER_SLOTS];
    TCHAR strSlotName[MAX_PLAYER_SLOTS][MAX_PLAYER_NAME];
};




//-----------------------------------------------------------------------------
// Global variables valid for all players
//-----------------------------------------------------------------------------
extern HANDLE          g_hDPMessageEvent;
extern LPDIRECTPLAY4A  g_pDP;
extern TCHAR           g_strLocalPlayerName[MAX_PLAYER_NAME];
extern DPID            g_LocalPlayerDPID; 
extern BOOL            g_bHostPlayer;
extern VOID*           g_pvDPMsgBuffer;
extern DWORD           g_dwDPMsgBufferSize;
extern TCHAR           g_strAppName[256];
TCHAR                  g_strDlgTitle[256];
DWORD                  g_dwLocalSlotNumber;
BOOL                   g_bLocalPlayerReady;
BOOL                   g_bAllowPlayerJoin;
DWORD                  g_dwNumberOfFullSlots;




//-----------------------------------------------------------------------------
// Global variables valid for only host player 
//-----------------------------------------------------------------------------
BOOL  g_dwSlotStatus[MAX_PLAYER_SLOTS]; 
TCHAR g_strSlotName[MAX_PLAYER_SLOTS][MAX_PLAYER_NAME]; 
DWORD g_dwSlotDPID[MAX_PLAYER_SLOTS]; 
DWORD g_dwNumberSlotsOpen;




//-----------------------------------------------------------------------------
// Function-prototypes
//-----------------------------------------------------------------------------
int           DPStage_StartDirectPlayStage( HINSTANCE hInst );
BOOL CALLBACK DPStage_StageDlgProc( HWND, UINT, WPARAM, LPARAM );
HRESULT       DPStage_StageDlgInit( HWND hDlg );
HRESULT       DPStage_ProcessDirectPlayMessages( HWND hDlg );
HRESULT       DPStage_HandleSystemMessages( HWND hDlg, DPMSG_GENERIC* pMsg, DWORD dwMsgSize, DPID idFrom, DPID idTo );
HRESULT       DPStage_HandleAppMessages( HWND hDlg, STAGEMSG_GENERIC* pMsg, DWORD dwMsgSize, DPID idFrom, DPID idTo );
HRESULT       DPStage_SendChatMessage( HWND hDlg );
HRESULT       DPStage_GetFreePlayerSlot( DWORD* pdwPlayerSlotID );
HRESULT       DPStage_SendSlotDataToPlayers( HWND hDlg );
VOID          DPStage_DisplaySlotData( HWND hDlg, STAGEMSG_STAGEDATA* pStageDataMsg );
HRESULT       DPStage_AddPlayerToStage( HWND hDlg, DPMSG_CREATEPLAYERORGROUP* pCreateMsg );
HRESULT       DPStage_RemovePlayerToStage( HWND hDlg, DPMSG_DESTROYPLAYERORGROUP* pDeleteMsg );
VOID          DPStage_AddChatStringToListBox( HWND hDlg, LPSTR strMsgText );
HRESULT       DPStage_ChangePlayerReadyStatus( HWND hDlg, BOOL bPlayerReady );
HRESULT       DPStage_ChangePlayerSlotStatus( HWND hDlg, DWORD dwSlot );
HRESULT       DPStage_UpdateSessionDesc();
HRESULT       DPStage_StartGame( HWND hDlg );
HRESULT       DPStage_CancelGame();




//-----------------------------------------------------------------------------
// Name: DPStage_StartDirectPlayStage()
// Desc: Begins the stage.  This function returns one the following EXITCODEs, 
//       which are defined above: EXITCODE_STARTGAME, EXITCODE_BACKUP, 
//       EXITCODE_QUIT, EXITCODE_ERROR    
//-----------------------------------------------------------------------------
int DPStage_StartDirectPlayStage( HINSTANCE hInst )
{
    HWND    hDlg  = NULL;
    BOOL    bDone = FALSE;
    int     nExitCode;
    HRESULT hr;
    DWORD   dwResult;
    MSG     msg;

    strcpy( g_strDlgTitle, "DirectPlay Multiplayer Stage" );

    // Setup data for the player
    if( g_bHostPlayer )
    {
        g_dwNumberSlotsOpen = MAX_PLAYER_SLOTS;
        g_bAllowPlayerJoin  = TRUE;

        // Setup the 'is player slot open' array
        for( int iPlayer = 0; iPlayer < MAX_PLAYER_SLOTS; iPlayer++ )
        {
            g_dwSlotStatus[ iPlayer ] = SLOT_BUTTON_OPEN;
            g_dwSlotDPID[ iPlayer ]   = 0xFFFFFFFF;  // Not valid yet
        }

        // Host player is always the first slot
        g_dwLocalSlotNumber = 0;
        g_dwSlotStatus[ g_dwLocalSlotNumber ] = SLOT_BUTTON_FULL; 
        g_dwSlotDPID[ g_dwLocalSlotNumber ]   = g_LocalPlayerDPID;
        strcpy( g_strSlotName[ g_dwLocalSlotNumber ], g_strLocalPlayerName );
    }
    else
    {
        // The local slot is valid yet, a the host will send us a 
        // STAGEMSG_SETSLOTID message to tell us what slot we fit in 
        g_dwLocalSlotNumber = 0xFFFFFFFF; 
    }

    // Display the multiplayer stage dialog box.
    hDlg = CreateDialog( hInst, MAKEINTRESOURCE(IDD_MULTIPLAYER_STAGE), 
                         NULL, DPStage_StageDlgProc );

    // Show window immediately only if it is the host
    if( g_bHostPlayer )
        ShowWindow( hDlg, SW_SHOW );
    else
        ShowWindow( hDlg, SW_HIDE );

    while( !bDone ) 
    { 
        dwResult = MsgWaitForMultipleObjects( 1, &g_hDPMessageEvent, 
                                              FALSE, INFINITE, QS_ALLEVENTS );
        switch( dwResult )
        {
            case WAIT_OBJECT_0 + 0:
                // g_hDPMessageEvent is signaled, so there are
                // DirectPlay messages available
                if( FAILED( hr = DPStage_ProcessDirectPlayMessages( hDlg ) ) ) 
                    return EXITCODE_ERROR;
                break;

            case WAIT_OBJECT_0 + 1:
                // Messages are available
                while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) ) 
                { 
                    if( !IsDialogMessage( hDlg, &msg ) )  
                    {
                        TranslateMessage( &msg ); 
                        DispatchMessage( &msg ); 
                    }

                    if( msg.message == WM_QUIT )
                    {
                        // See the above EXITCODE #defines for what 
                        // the nExitCode could be 
                        nExitCode = msg.wParam;

                        EndDialog( hDlg, nExitCode );
                        bDone = TRUE;
                    }
                }
                break;
        }
    }

    // If the game was not started (nExitCode==EXITCODE_FORWARD), then we must 
    // destroy the player and close this DirectPlay session
    if( nExitCode != EXITCODE_FORWARD )
    {
        SAFE_DELETE_ARRAY( g_pvDPMsgBuffer );

        if( g_pDP )
        {
            if( FAILED( hr = g_pDP->DestroyPlayer( g_LocalPlayerDPID ) ) )
                return EXITCODE_ERROR;

            if( FAILED( hr = g_pDP->Close() ) )
                return EXITCODE_ERROR;
        }

        if( nExitCode == EXITCODE_REJECTED )
        {
            MessageBox( NULL, "You were dropped from the game.", 
                        g_strDlgTitle, MB_OK );
            return EXITCODE_BACKUP;
        }

        if( nExitCode == EXITCODE_SESSIONLOST )
        {
            MessageBox( hDlg, "The game was canceled.", 
                        g_strDlgTitle, MB_OK );
            return EXITCODE_BACKUP;
        }
    }

    return nExitCode;
}




//-----------------------------------------------------------------------------
// Name: DPStage_StageDlgProc()
// Desc: Handles messages for the multiplayer stage dialog
//-----------------------------------------------------------------------------
BOOL CALLBACK DPStage_StageDlgProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
    HRESULT hr;

    switch( msg ) 
    {
        case WM_INITDIALOG:
            // Set what gets notified when return is pressed 
            if( FAILED( hr = DPStage_StageDlgInit( hDlg ) ) )
                PostQuitMessage( EXITCODE_ERROR );
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDC_START_GAME: // 'Start game' was pressed
                    // This button should only be enabled if we are the host, and
                    // everything is ready, so we are all ready to start the game
                    if( FAILED( hr = DPStage_StartGame( hDlg ) ) )
                        PostQuitMessage( EXITCODE_ERROR );
                    break;
                    
                case IDCANCEL: // The close dlg button was pressed
                    if( g_bHostPlayer )
                    {
                        if( FAILED( hr = DPStage_CancelGame() ) )
                            PostQuitMessage( EXITCODE_ERROR );
                    }

                    PostQuitMessage( EXITCODE_QUIT );
                    break;

                case IDC_BACK: // 'Cancel' was pressed
                    if( g_bHostPlayer )
                    {
                        if( FAILED( hr = DPStage_CancelGame() ) )
                            PostQuitMessage( EXITCODE_ERROR );
                    }

                    PostQuitMessage( EXITCODE_BACKUP );
                    break;

                case IDC_RETURN:
                    // The enter key was pressed, so send out the chat message
                    if( FAILED( hr = DPStage_SendChatMessage( hDlg ) ) )
                        PostQuitMessage( EXITCODE_ERROR );
                    break;

                case IDC_READY_P1:
                case IDC_READY_P2:
                case IDC_READY_P3:
                case IDC_READY_P4:
                case IDC_READY_P5:
                case IDC_READY_P6:
                case IDC_READY_P7:
                case IDC_READY_P8:
                case IDC_READY_P9:
                case IDC_READY_P10:
                    // Only one of these checkboxs should be enabled (the one next to 
                    // the players slot), so change the players status and tell everyone
                    if( FAILED( hr = DPStage_ChangePlayerReadyStatus( hDlg, !g_bLocalPlayerReady ) ) )
                        PostQuitMessage( EXITCODE_ERROR );
                    break;

                case IDC_SLOT_BTN_P2:
                case IDC_SLOT_BTN_P3:
                case IDC_SLOT_BTN_P4:
                case IDC_SLOT_BTN_P5:
                case IDC_SLOT_BTN_P6:
                case IDC_SLOT_BTN_P7:
                case IDC_SLOT_BTN_P8:
                case IDC_SLOT_BTN_P9:
                case IDC_SLOT_BTN_P10:
                    // Only the host has access to use these buttons, and can not reject
                    // himself.  So figure out which slot button was pressed and 
                    // change its status
                    if( g_bHostPlayer )
                    {
                        DWORD dwSlot = LOWORD(wParam) - IDC_SLOT_BTN_P1;

                        if( FAILED( hr = DPStage_ChangePlayerSlotStatus( hDlg, dwSlot ) ) )
                            PostQuitMessage( EXITCODE_ERROR );

                    }
                    break;

                default:
                    return FALSE; // Message not handled
            }

            break;

        default:
            return FALSE; // Message not handled
    }

    return TRUE; // Handled message
}




//-----------------------------------------------------------------------------
// Name: DPStage_StageDlgInit()
// Desc: Setup the dialog for both host and players
//-----------------------------------------------------------------------------
HRESULT DPStage_StageDlgInit( HWND hDlg )
{
    HRESULT hr;
    int nIDDlgItem; 
    TCHAR strWindowTitle[256];

    // Load and set the icon
    HINSTANCE hInst = (HINSTANCE) GetWindowLong( hDlg, GWL_HINSTANCE );
    HICON hIcon = LoadIcon( hInst, MAKEINTRESOURCE( IDI_MAIN ) );
    SendMessage( hDlg, WM_SETICON, ICON_BIG,   (LPARAM) hIcon );  // Set big icon
    SendMessage( hDlg, WM_SETICON, ICON_SMALL, (LPARAM) hIcon );  // Set small icon

    // Set the window title
    sprintf( strWindowTitle, "%s - Multiplayer Stage", g_strAppName );
    SetWindowText( hDlg, strWindowTitle );

    g_bLocalPlayerReady = FALSE;

    // Set the default button id to be IDC_RETURN.  We handle in the dlg proc,
    // and make it send chat messages to all of the players
    SendMessage( hDlg, DM_SETDEFID, IDC_RETURN, 0L );

    // Disable all of the ready checkboxes.  
    for( nIDDlgItem = IDC_READY_P1; nIDDlgItem <= IDC_READY_P10; nIDDlgItem++ )
        EnableWindow( GetDlgItem( hDlg, nIDDlgItem ), FALSE );

    if( g_bHostPlayer )
    {
        // Since we are host, we are in slot 0, so enable our checkbox
        EnableWindow( GetDlgItem( hDlg, IDC_READY_P1 ),   TRUE );
        EnableWindow( GetDlgItem( hDlg, IDC_START_GAME ), FALSE );
        SetDlgItemText( hDlg, IDC_SLOT_TXT_P1, g_strLocalPlayerName );

        // Show all of the stage buttons controls, and hide the stage static text controls.  
        for( nIDDlgItem = IDC_SLOT_BTN_P2; nIDDlgItem <= IDC_SLOT_BTN_P10; nIDDlgItem++ )
            ShowWindow( GetDlgItem( hDlg, nIDDlgItem ), SW_SHOW );
        for( nIDDlgItem = IDC_SLOT_TXT_P2; nIDDlgItem <= IDC_SLOT_TXT_P10; nIDDlgItem++ )
            ShowWindow( GetDlgItem( hDlg, nIDDlgItem ), SW_HIDE );

        // Display our current state to the local player
        if( FAILED( hr = DPStage_SendSlotDataToPlayers( hDlg ) ) )
            return hr;
    }
    else
    {
        // Non-host players can not start a game
        ShowWindow( GetDlgItem( hDlg, IDC_START_GAME ), SW_HIDE );

        // Hide all of the stage buttons controls, and show the stage static text controls.  
        for( nIDDlgItem = IDC_SLOT_BTN_P2; nIDDlgItem <= IDC_SLOT_BTN_P10; nIDDlgItem++ )
            ShowWindow( GetDlgItem( hDlg, nIDDlgItem ), SW_HIDE );
        for( nIDDlgItem = IDC_SLOT_TXT_P1; nIDDlgItem <= IDC_SLOT_TXT_P10; nIDDlgItem++ )
            ShowWindow( GetDlgItem( hDlg, nIDDlgItem ), SW_SHOW );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPStage_ProcessDirectPlayMessages()
// Desc: Processes for DirectPlay messages
//-----------------------------------------------------------------------------
HRESULT DPStage_ProcessDirectPlayMessages( HWND hDlg )
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

            if( pvMsgBuffer == NULL )
            {
                pvMsgBuffer = new BYTE[ dwMsgBufferSize ];
                if( pvMsgBuffer == NULL )
                    return E_OUTOFMEMORY;
            }
            else if( dwMsgBufferSize > g_dwDPMsgBufferSize )
            {
                SAFE_DELETE_ARRAY( pvMsgBuffer );
                pvMsgBuffer = new BYTE[ dwMsgBufferSize ];
                if( pvMsgBuffer == NULL )
                    return E_OUTOFMEMORY;
            }

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
            hr = DPStage_HandleSystemMessages( hDlg, (DPMSG_GENERIC*)pvMsgBuffer, 
                                               dwMsgBufferSize, idFrom, idTo );
            if( FAILED(hr) )
                return hr;
        }
        else
        {
            hr = DPStage_HandleAppMessages( hDlg, (STAGEMSG_GENERIC*)pvMsgBuffer, 
                                            dwMsgBufferSize, idFrom, idTo );
            if( FAILED(hr) )
                return hr;
        }
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPStage_HandleAppMessages()
// Desc: Evaluates application messages and performs appropriate actions
//-----------------------------------------------------------------------------
HRESULT DPStage_HandleAppMessages( HWND hDlg, STAGEMSG_GENERIC* pMsg, DWORD dwMsgSize, 
                                   DPID idFrom, DPID idTo )
{
    HRESULT hr;

    switch( pMsg->dwType )
    {
        case STAGEMSG_SLOTCHECK:
            {
                // Host only message.  This message is sent by players when they 
                // have marked check box as ready
                if( !g_bHostPlayer )
                    return E_FAIL; // Sanity check

                // Update global slot status, and send it to everyone
                STAGEMSG_SLOT* pSlotIDMsg = (STAGEMSG_SLOT*) pMsg;
                g_dwSlotStatus[ pSlotIDMsg->dwSlotNumber ] |= SLOT_READY_CHECKED;
                if( FAILED( hr = DPStage_SendSlotDataToPlayers( hDlg ) ) )
                    return hr;
            }
            break;

        case STAGEMSG_SLOTUNCHECK:
            {
                // Host only message.  This message is sent by players when they 
                // have marked their check box as unready
                if( !g_bHostPlayer )
                    return E_FAIL; // Sanity check

                // Update global slot status, and send it to everyone
                STAGEMSG_SLOT* pSlotIDMsg = (STAGEMSG_SLOT*) pMsg;
                g_dwSlotStatus[ pSlotIDMsg->dwSlotNumber ] &= ~SLOT_READY_CHECKED;
                if( FAILED( hr = DPStage_SendSlotDataToPlayers( hDlg ) ) )
                    return hr;
            }
            break;

        case STAGEMSG_SETSLOTID:
            {
                // Non-host only message. This message is sent by the host player
                // when a player joins the stage.  The host is telling the player 
                // what slot number this player has been assigned.
                if( g_bHostPlayer )
                    return E_FAIL; // Sanity check

                // Set the local slot ID given to us by the host, also
                // enable our 'ready' checkbox
                STAGEMSG_SLOT* pSlotIDMsg = (STAGEMSG_SLOT*) pMsg;
                g_dwLocalSlotNumber = pSlotIDMsg->dwSlotNumber;
                EnableWindow( GetDlgItem( hDlg, IDC_READY_P1 + g_dwLocalSlotNumber ), 
                              TRUE );
            }
            break;

        case STAGEMSG_SETSTAGEDATA:
            {
                // Non-host only message.  This message is sent by the host when
                // the stage data changes. The players take this data and display
                // it in the dialog.
                if( g_bHostPlayer )
                    return E_FAIL; // Sanity check

                // Display the slot data sent to us by the host
                STAGEMSG_STAGEDATA* pStageDataMsg = (STAGEMSG_STAGEDATA*) pMsg;
                DPStage_DisplaySlotData( hDlg, pStageDataMsg );

                // Now it is safe to show the dialog, since slots text is valid
                ShowWindow( hDlg, SW_SHOW );
            }
            break;

        case STAGEMSG_REJECTPLAYER:
            {
                // Non-host only message.  This message is sent by the host when a 
                // player is rejected from the game either because there are no
                // free slots left or the host manually rejected the player.
                if( g_bHostPlayer )
                    return E_FAIL; // Sanity check

                PostQuitMessage( EXITCODE_REJECTED );
            }
            break;

        case STAGEMSG_STARTGAME:
            {
                // Non-host only message.  This message is sent by the host when  
                // the everyone is ready, and the host has started the game.
                if( g_bHostPlayer )
                    return E_FAIL; // Sanity check

                PostQuitMessage( EXITCODE_FORWARD );
            }
            break;

        case STAGEMSG_CANCELGAME:
            {
                // Non-host only message.  This message is sent by the host when  
                // the host has canceled the game.
                if( g_bHostPlayer )
                    return E_FAIL; // Sanity check

                PostQuitMessage( EXITCODE_SESSIONLOST );
            }
            break;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPStage_HandleSystemMessages()
// Desc: Evaluates system messages and performs appropriate actions
//-----------------------------------------------------------------------------
HRESULT DPStage_HandleSystemMessages( HWND hDlg, DPMSG_GENERIC* pMsg, DWORD dwMsgSize, 
                                      DPID idFrom, DPID idTo )
{  
    HRESULT hr;

    switch( pMsg->dwType )
    {
        case DPSYS_CHAT:
            {
                DPMSG_CHAT* pChatMsg = (DPMSG_CHAT*) pMsg;
                DPCHAT* pChatStruct = pChatMsg->lpChat;

                // A chat string came in, so add it to the listbox
                DPStage_AddChatStringToListBox( hDlg, pChatStruct->lpszMessageA );
            }
            break;

        case DPSYS_CREATEPLAYERORGROUP:
            {
                DPMSG_CREATEPLAYERORGROUP* pCreateMsg = (DPMSG_CREATEPLAYERORGROUP*) pMsg;

                // If we are the host player, then add this player to the stage 
                // and tell everyone about it 
                if( g_bHostPlayer )
                {
                    if( FAILED( hr = DPStage_AddPlayerToStage( hDlg, pCreateMsg ) ) )
                        return hr;
                }  
            }
            break;

        case DPSYS_DESTROYPLAYERORGROUP:
            {
                DPMSG_DESTROYPLAYERORGROUP* pDeleteMsg;
                pDeleteMsg = (DPMSG_DESTROYPLAYERORGROUP*) pMsg;

                // If we are the host player, then remove this player from the stage 
                // and tell everyone about it 
                if( g_bHostPlayer )
                {
                    if( FAILED( hr = DPStage_RemovePlayerToStage( hDlg, pDeleteMsg ) ) )
                        return hr;
                }
            }
            break;

        case DPSYS_SESSIONLOST:
                // Non-host message.  This message is sent to all players 
                // when the host cancels the game.
                if( g_bHostPlayer )
                    return E_FAIL; // Sanity check

                PostQuitMessage( EXITCODE_SESSIONLOST );
            break;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPStage_SendChatMessage()
// Desc: Create chat string based on the editbox and send it to everyone 
//-----------------------------------------------------------------------------
HRESULT DPStage_SendChatMessage( HWND hDlg )
{
    HRESULT hr;
    DPCHAT  dpc;
    TCHAR*  strEditboxBuffer = NULL;
    TCHAR*  strChatBuffer    = NULL;
    DWORD   dwEditboxBufferSize;
    DWORD   dwPlayerNameSize;
    DWORD   dwChatBufferSize;

    // Get length of item text
    dwEditboxBufferSize = SendDlgItemMessage( hDlg, IDC_CHAT_EDIT, 
                                              WM_GETTEXTLENGTH, 0, 0 );
    if( dwEditboxBufferSize == 0 )
        return S_OK;  // Don't do anything for blank messages 

    // Figure out how much room we need
    dwPlayerNameSize = strlen( g_strLocalPlayerName ) + 3;
    dwChatBufferSize = dwPlayerNameSize + dwEditboxBufferSize;

    // Make room for it
    strChatBuffer = (LPTSTR) new TCHAR[ dwChatBufferSize + 1 ];
    if( NULL == strChatBuffer )
        return E_OUTOFMEMORY;
    strEditboxBuffer = (LPTSTR) new TCHAR[ dwEditboxBufferSize + 1 ];
    if( NULL == strEditboxBuffer )
        return E_OUTOFMEMORY;

    // Make the chat string from the player's name and the edit box string
    GetDlgItemText( hDlg, IDC_CHAT_EDIT, strEditboxBuffer, dwEditboxBufferSize + 1 );
    sprintf( strChatBuffer, "<%s> %s", g_strLocalPlayerName, strEditboxBuffer );

    // Send the chat message to all of the other players
    ZeroMemory( &dpc, sizeof(DPCHAT) );
    dpc.dwSize       = sizeof(DPCHAT);
    dpc.lpszMessageA = strChatBuffer;

    if( FAILED( hr = g_pDP->SendChatMessage( g_LocalPlayerDPID, DPID_ALLPLAYERS, 
                                             0, &dpc ) ) )
        return hr;

    // Add the chat message to the local listbox
    DPStage_AddChatStringToListBox( hDlg, strChatBuffer );

    // Cleanup
    SAFE_DELETE_ARRAY( strChatBuffer );
    SAFE_DELETE_ARRAY( strEditboxBuffer );
    SetDlgItemText( hDlg, IDC_CHAT_EDIT, "" );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPStage_AddChatStringToListBox()
// Desc: Adds a string to the list box and ensures it is visible 
//-----------------------------------------------------------------------------
VOID DPStage_AddChatStringToListBox( HWND hDlg, LPSTR strMsgText )
{
    // Add the message to the local listbox
    HWND hWndChatBox = GetDlgItem( hDlg, IDC_CHAT_LISTBOX );
    int nCount = SendMessage( hWndChatBox, LB_GETCOUNT, 0, 0 );
    if( nCount > MAX_CHAT_STRINGS )
        SendMessage( hWndChatBox, LB_DELETESTRING, 0, 0 );

    // Add it, and make sure it is visible
    int nIndex = SendMessage( hWndChatBox, LB_ADDSTRING, 0, (LPARAM)strMsgText );
    SendMessage( hWndChatBox, LB_SETTOPINDEX, nIndex, 0 );
}




//-----------------------------------------------------------------------------
// Name: DPStage_GetFreePlayerSlot()
// Desc: Host only.  Returns the first open slot or S_FALSE if non-found
//-----------------------------------------------------------------------------
HRESULT DPStage_GetFreePlayerSlot( DWORD* pdwPlayerSlotID )
{
    DWORD dwPlayer;

    if( !g_bHostPlayer )
        return E_FAIL; // Sanity check

    for( dwPlayer = 0; dwPlayer < MAX_PLAYER_SLOTS; dwPlayer++ )
    {
        if( SLOT_BUTTON_OPEN == (g_dwSlotStatus[ dwPlayer ] & SLOT_BUTTON_MASK) ) 
        {
            *pdwPlayerSlotID = dwPlayer;
            return S_OK;
        }
    }

    // No open slot found
    return S_FALSE;
}




//-----------------------------------------------------------------------------
// Name: DPStage_SendSlotDataToPlayers()
// Desc: Host Only. Sends the slot data to all the players, and displays the 
//       same data on the local machine.  Other players will display this 
//       data when they receieve it.  
//-----------------------------------------------------------------------------
HRESULT DPStage_SendSlotDataToPlayers( HWND hDlg )
{
    HRESULT hr;

    if( !g_bHostPlayer )
        return E_FAIL; // Sanity check

    STAGEMSG_STAGEDATA msgSlotData;
    msgSlotData.dwType = STAGEMSG_SETSTAGEDATA;

    // Fill the STAGEMSG_STAGEDATA message
    for( int iSlot = 0; iSlot < MAX_PLAYER_SLOTS; iSlot++ )
    {
        msgSlotData.dwSlotStatus[iSlot] = g_dwSlotStatus[iSlot];
        strcpy( msgSlotData.strSlotName[iSlot], g_strSlotName[iSlot] );
    }

    // Send it to all of the players
    if( FAILED( hr = g_pDP->Send( g_LocalPlayerDPID, DPID_ALLPLAYERS, 
                                  0, &msgSlotData, sizeof(STAGEMSG_STAGEDATA) ) ) ) 
        return hr;

    // Display it on the host machine also
    DPStage_DisplaySlotData( hDlg, &msgSlotData );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPStage_DisplaySlotData()
// Desc: Display the new slot data for each of the slots, updating the 
//       button's text and also the ready checkbox.  Also update the stage 
//       status message 
//-----------------------------------------------------------------------------
VOID DPStage_DisplaySlotData( HWND hDlg, STAGEMSG_STAGEDATA* pStageDataMsg )
{
    BOOL bEveryOneReady = TRUE;
    TCHAR strStatus[256];

    g_dwNumberOfFullSlots = 0;

    for( int iSlot = 0; iSlot < MAX_PLAYER_SLOTS; iSlot++ )
    {
        // Update the button and checkbox status based on the 
        // STAGEMSG_STAGEDATA message.
        switch( pStageDataMsg->dwSlotStatus[iSlot] & SLOT_BUTTON_MASK ) 
        {
            case SLOT_BUTTON_OPEN:
                SetDlgItemText( hDlg, IDC_SLOT_BTN_P1 + iSlot, "Open" );
                SetDlgItemText( hDlg, IDC_SLOT_TXT_P1 + iSlot, "Open" );
                CheckDlgButton( hDlg, IDC_READY_P1 + iSlot, BST_UNCHECKED );
                break;

            case SLOT_BUTTON_CLOSED:
                SetDlgItemText( hDlg, IDC_SLOT_BTN_P1 + iSlot, "Closed" );
                SetDlgItemText( hDlg, IDC_SLOT_TXT_P1 + iSlot, "Closed" );
                CheckDlgButton( hDlg, IDC_READY_P1 + iSlot, BST_UNCHECKED );
                break;

            case SLOT_BUTTON_FULL:
                SetDlgItemText( hDlg, IDC_SLOT_BTN_P1 + iSlot, 
                                pStageDataMsg->strSlotName[iSlot] );
                SetDlgItemText( hDlg, IDC_SLOT_TXT_P1 + iSlot, 
                                pStageDataMsg->strSlotName[iSlot] );

                // Check the STAGEMSG_STAGEDATA msg to see if that player is ready
                if( ( pStageDataMsg->dwSlotStatus[iSlot] & SLOT_READY_MASK ) 
                                                    == SLOT_READY_CHECKED )
                {
                    CheckDlgButton( hDlg, IDC_READY_P1 + iSlot, BST_CHECKED );
                }
                else
                {
                    CheckDlgButton( hDlg, IDC_READY_P1 + iSlot, BST_UNCHECKED );
                    bEveryOneReady = FALSE;
                }

                g_dwNumberOfFullSlots++;
                break;
        }
    }

    // Keep the start game button disabled until everything is ready
    EnableWindow( GetDlgItem( hDlg, IDC_START_GAME ), FALSE );

    // Figure out what the status message should be
    if( bEveryOneReady )
    {
        if( g_bHostPlayer )
        {
            if( g_dwNumberOfFullSlots == 1 )
            {
                strcpy( strStatus, "Waiting for more players to join." );
            }
            else
            {
                strcpy( strStatus, "All are ready! Waiting for you to start the game." );

                // We are all set to start the game, so enable the button
                EnableWindow( GetDlgItem( hDlg, IDC_START_GAME ), TRUE );
            }
        }
        else
        {
            strcpy( strStatus, "All are ready! Waiting for host to start the game." );
        }
    }
    else
    {
        if( g_bLocalPlayerReady )
            strcpy( strStatus, "Waiting until all players check the Ready flag." );
        else
            strcpy( strStatus, "Ready?  Flag the checkbox next to your name." );
    }   

    // Set the status text on the dialog
    SetDlgItemText( hDlg, IDC_STATUS_TEXT, strStatus );
}




//-----------------------------------------------------------------------------
// Name: DPStage_AddPlayerToStage()
// Desc: Host Only. Adds a new player to an empty slot, if one exists and 
//       tells everyone that the new player has joined the stage
//-----------------------------------------------------------------------------
HRESULT DPStage_AddPlayerToStage( HWND hDlg, DPMSG_CREATEPLAYERORGROUP* pCreateMsg )
{
    STAGEMSG_SLOT msgSlotID;
    DWORD         dwPlayerSlot;
    HRESULT       hr;

    if( !g_bHostPlayer )
        return E_FAIL; // Sanity check

    // Get the next free slot id if there is one
    if( FAILED( hr = DPStage_GetFreePlayerSlot( &dwPlayerSlot ) ) )
        return hr;

    if( hr == S_FALSE )
    {
        // There are no more slots left, so send a message to the player the 
        // they have been rejected 
        STAGEMSG_GENERIC msgReject;
        msgReject.dwType = STAGEMSG_REJECTPLAYER;

        if( FAILED( hr = g_pDP->Send( g_LocalPlayerDPID, pCreateMsg->dpId, 
                                      0, &msgReject, sizeof(STAGEMSG_GENERIC) ) ) ) 
            return hr;

        return S_OK;
    }

    // Update slot global data
    g_dwSlotStatus[ dwPlayerSlot ] = SLOT_BUTTON_FULL;
    strcpy( g_strSlotName[ dwPlayerSlot ], pCreateMsg->dpnName.lpszShortNameA );
    g_dwSlotDPID[ dwPlayerSlot ] = pCreateMsg->dpId;

    // Send the new player the slot id assigned to it by the host session
    msgSlotID.dwType   = STAGEMSG_SETSLOTID;
    msgSlotID.dwSlotNumber = dwPlayerSlot;
    if( FAILED( hr = g_pDP->Send( g_LocalPlayerDPID, pCreateMsg->dpId, 
                                  0, &msgSlotID, sizeof(STAGEMSG_SLOT) ) ) ) 
        return hr;

    // Set the local player data to be the slot number of the player
    if( FAILED( hr = g_pDP->SetPlayerData( pCreateMsg->dpId, &msgSlotID.dwSlotNumber, 
                                           sizeof(DWORD), DPSET_LOCAL ) ) )
        return hr;

    // Send the new slot data to everyone
    if( FAILED( hr = DPStage_SendSlotDataToPlayers( hDlg ) ) )
        return hr;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPStage_RemovePlayerToStage()
// Desc: Host Only. Removes the player from the slot and 
//       tells everyone that the player has been removed from the stage
//-----------------------------------------------------------------------------
HRESULT DPStage_RemovePlayerToStage( HWND hDlg, DPMSG_DESTROYPLAYERORGROUP* pDeleteMsg )
{
    DWORD* pdwPlayerSlotNum;
    HRESULT hr;

    if( !g_bHostPlayer )
        return E_FAIL; // Sanity check

    // Get the local data of the player.  It should be the 
    // player's slot number.  If it is there, then update the globals to 
    // make this player's slot open.
    pdwPlayerSlotNum = (DWORD*) pDeleteMsg->lpLocalData;
    if( NULL != pdwPlayerSlotNum )
    {
        if( SLOT_BUTTON_FULL == (g_dwSlotStatus[ *pdwPlayerSlotNum ] & SLOT_BUTTON_MASK) )
        {
            g_dwSlotStatus[ *pdwPlayerSlotNum ] = SLOT_BUTTON_OPEN;
            g_dwSlotDPID[ *pdwPlayerSlotNum ] = 0xFFFFFFFF;
            strcpy( g_strSlotName[ *pdwPlayerSlotNum ], "" );

            // Tell everyone about the change
            if( FAILED( hr = DPStage_SendSlotDataToPlayers( hDlg ) ) )
                return hr;
        }
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPStage_UpdateSessionDesc()
// Desc: Host Only. Tells DirectPlay about changes the max players allowed 
//       in this session 
//-----------------------------------------------------------------------------
HRESULT DPStage_UpdateSessionDesc()
{
    DWORD           dwBufferSize;
    BYTE*           pData = NULL;
    DPSESSIONDESC2* pdpsd;
    HRESULT         hr;

    if( !g_bHostPlayer )
        return E_FAIL; // Sanity check

    // Get the size of the dpsd, and allocate space for it then get it for real
    g_pDP->GetSessionDesc( NULL, &dwBufferSize );

    pData = new BYTE[ dwBufferSize ];
    if( pData == NULL )
        return E_OUTOFMEMORY;

    if( FAILED( hr = g_pDP->GetSessionDesc( pData, &dwBufferSize ) ) )
        return hr;

    // Typecast the data to a DPSESSIONDESC2* 
    pdpsd = (DPSESSIONDESC2*) pData;

    // Change the 'max players' to what it should be
    pdpsd->dwMaxPlayers = g_dwNumberSlotsOpen;

    if( g_bAllowPlayerJoin )
        pdpsd->dwFlags &= ~DPSESSION_JOINDISABLED;
    else
        pdpsd->dwFlags |= DPSESSION_JOINDISABLED;


    // Tell DirectPlay about the change
    if( FAILED( hr = g_pDP->SetSessionDesc( pdpsd, 0 ) ) )
        return hr;

    // Cleanup the data
    SAFE_DELETE_ARRAY( pData );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPStage_ChangePlayerReadyStatus()
// Desc: The players has clicked on the player's ready checkbox, so tell
//       everyone that his has happened.
//-----------------------------------------------------------------------------
HRESULT DPStage_ChangePlayerReadyStatus( HWND hDlg, BOOL bPlayerReady )
{
    HRESULT hr;

    g_bLocalPlayerReady = bPlayerReady;

    if( g_bHostPlayer )
    {
        // Update the global slot data, and tell everyone.
        if( g_bLocalPlayerReady )
            g_dwSlotStatus[ g_dwLocalSlotNumber ] |= SLOT_READY_CHECKED;
        else
            g_dwSlotStatus[ g_dwLocalSlotNumber ] &= ~SLOT_READY_CHECKED;
    
        if( FAILED( hr = DPStage_SendSlotDataToPlayers( hDlg ) ) )
            return hr;
    }
    else
    {
        STAGEMSG_SLOT msgSlotCheck;

        // Send a message to the host player that this player has
        // changed their ready status.  The host player will in turn
        // tell everyone about the change
        msgSlotCheck.dwSlotNumber = g_dwLocalSlotNumber;
    
        // Send a different message based on the local ready status
        if( g_bLocalPlayerReady )
            msgSlotCheck.dwType = STAGEMSG_SLOTCHECK;
        else
            msgSlotCheck.dwType = STAGEMSG_SLOTUNCHECK;
    
        if( FAILED( hr = g_pDP->Send( g_LocalPlayerDPID, DPID_SERVERPLAYER, 0, 
                                      &msgSlotCheck, sizeof(STAGEMSG_SLOT) ) ) ) 
            return hr;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPStage_ChangePlayerSlotStatus()
// Desc: Host Only. The host has clicked on a player's button, so either reject 
//       that player or if the slot is empty, then flip it's status between 
//       open and closed
//-----------------------------------------------------------------------------
HRESULT DPStage_ChangePlayerSlotStatus( HWND hDlg, DWORD dwSlot )
{
    HRESULT hr;
    int nResult;

    if( !g_bHostPlayer )
        return E_FAIL; // Sanity check

    switch( g_dwSlotStatus[ dwSlot ] & SLOT_BUTTON_MASK )
    {
        case SLOT_BUTTON_FULL:
            TCHAR strQuestion[MAX_PLAYER_NAME + 256];
            sprintf( strQuestion, "Do you want to reject %s?", g_strSlotName[dwSlot] );
            nResult = MessageBox( hDlg, strQuestion, g_strDlgTitle, MB_YESNO );
            if( nResult == IDNO )
                return S_OK;

            STAGEMSG_GENERIC msgReject;
            msgReject.dwType = STAGEMSG_REJECTPLAYER;

            // Send a message to this player that they have been rejected 
            if( FAILED( hr = g_pDP->Send( g_LocalPlayerDPID, g_dwSlotDPID[dwSlot], 
                                          0, &msgReject, sizeof(STAGEMSG_GENERIC) ) ) ) 
                return hr;

            // Change the slot status to open
            g_dwSlotStatus[ dwSlot ] = SLOT_BUTTON_OPEN;
            break;


        case SLOT_BUTTON_OPEN:
            // Flip the slot status, and tell DirectPlay the max number of 
            // players has changed because of this
            g_dwSlotStatus[ dwSlot ] = SLOT_BUTTON_CLOSED;
            g_dwNumberSlotsOpen--;

            if( FAILED( hr = DPStage_UpdateSessionDesc() ) )
                return hr;
            break;

        case SLOT_BUTTON_CLOSED:
            // Flip the slot status, and tell DirectPlay the max number of 
            // players has changed because of this
            g_dwSlotStatus[ dwSlot ] = SLOT_BUTTON_OPEN;
            g_dwNumberSlotsOpen++;

            if( FAILED( hr = DPStage_UpdateSessionDesc() ) )
                return hr;
            break;
    }

    // Tell everyone about the new slot data 
    if( FAILED( hr = DPStage_SendSlotDataToPlayers( hDlg ) ) )
        return hr;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPStage_StartGame()
// Desc: Host Only. The host has pressed the start game buttton.  The button
//       should only be enabled when everyone is ready, and more than 1 player
//       in the stage.  
//-----------------------------------------------------------------------------
HRESULT DPStage_StartGame( HWND hDlg )
{
    HRESULT hr;

    if( !g_bHostPlayer )
        return E_FAIL; // Sanity check

    STAGEMSG_GENERIC msgStartGame;
    msgStartGame.dwType = STAGEMSG_STARTGAME;

    // Stop new players from joining the game after stage has closed,
    // and everyone has decided to begin a new game. Also set the max
    // number of players to be the number of slots that are currently full
    // since no new players can join anymore.
    g_bAllowPlayerJoin = FALSE;
    g_dwNumberSlotsOpen = g_dwNumberOfFullSlots;
    if( FAILED( hr = DPStage_UpdateSessionDesc() ) )
        return hr;

    if( FAILED( hr = g_pDP->Send( g_LocalPlayerDPID, DPID_ALLPLAYERS, 
                                  0, &msgStartGame, sizeof(STAGEMSG_GENERIC) ) ) ) 
        return hr;

    PostQuitMessage( EXITCODE_FORWARD ); 

    return S_OK;
}





//-----------------------------------------------------------------------------
// Name: DPStage_CancelGame()
// Desc: Host Only. The host has pressed the cancel game buttton.  So tell 
//       everyone that the game was canceled
//-----------------------------------------------------------------------------
HRESULT DPStage_CancelGame()
{
    HRESULT hr;

    if( !g_bHostPlayer )
        return E_FAIL; // Sanity check

    STAGEMSG_GENERIC msgCancelGame;
    msgCancelGame.dwType = STAGEMSG_CANCELGAME;

    if( FAILED( hr = g_pDP->Send( g_LocalPlayerDPID, DPID_ALLPLAYERS, 
                                  0, &msgCancelGame, sizeof(STAGEMSG_GENERIC) ) ) ) 
        return hr;

    return S_OK;
}


