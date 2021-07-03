//-----------------------------------------------------------------------------
// File: Bellhop.h
//
// Desc: Header file for bellhop
//
// Copyright (C) 1996-1999 Microsoft Corporation.  All Rights Reserved.
//-----------------------------------------------------------------------------
#define IDIRECTPLAY2_OR_GREATER
#include <windows.h>
#include <windowsx.h>
#include <dplay.h>
#include <dplobby.h>
#include "CGRpTree.h"

// constants
const DWORD MAXPLAYERS = 10;            // max no. players in the session
const DWORD MAXSTRLEN = 200;            // max. size of a string

// structure used to store DirectPlay information
struct DPLAYINFO
{
    LPDIRECTPLAY4A      pDP;        // DPlay4A interface pointer
    LPDIRECTPLAYLOBBY3A pDPLobby;   // DPlayLobby3A interface pointer
    HANDLE      hPlayerEvent;       // player event to use
    DPID        dpidPlayer;         // ID of player created
    BOOL        bIsHost;            // TRUE if we are hosting the session
    BOOL        bSecureSession;     // TRUE if the session is secure.
    DWORD       dwPlayerFlags;
    CGroupTree* pGroupTree;
    int         xPaneSplit;
    int         xHalfSplitWidth;
    int         ySpacing;
    int         xSpacing;
    BOOL        bSplitMove;
    CHAR        strSecureName[256];
    CHAR        strSecurePassword[256];
    CHAR        strSecureDomain[256];
};

struct ENUMCONNSTRUCT
{
    HWND    hWnd;
    int     idCombo;
};

struct CONNECTIONINFO
{
    GUID    guidSP;
    BYTE    Connection[1];
};

struct SESSIONINFO
{
    GUID    guidInstance;
    DWORD   dwFlags;
};

struct LOBBYGROUPCONTEXT
{
    DPLAYINFO*      pDPInfo;
    DPID            dpidRoom;
};

struct APPNAMECONTEXT
{
    GUID    guidApplication;
    CHAR    strAppName[MAXSTRLEN];
};

// guid for this application
// {4BF5D540-BDA5-11d0-9C4F-00A0C905425E}
DEFINE_GUID(BELLHOP_GUID, 
0x4bf5d540, 0xbda5, 0x11d0, 0x9c, 0x4f, 0x0, 0xa0, 0xc9, 0x5, 0x42, 0x5e);

// prototypes
HRESULT CreateDirectPlayInterface( LPDIRECTPLAY4A* ppDP );
HRESULT CreateDirectPlayLobbyInterface( LPDIRECTPLAYLOBBY3A* ppDPLobby );
HRESULT ConnectUsingDialog( HINSTANCE hInstance, DPLAYINFO* pDPInfo );
VOID    ErrorBox( LPSTR strError, HRESULT hr );

BOOL CALLBACK LobbyWndProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam );
BOOL CALLBACK ConnectionSettingsDialogProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                                            LPARAM lParam );

HRESULT SetupConnection( HINSTANCE hInstance, DPLAYINFO* pDPInfo );
HRESULT ShutdownConnection( DPLAYINFO* pDPInfo );
DWORD WINAPI ReceiveThread( VOID* pThreadParameter );
HRESULT ReceiveMessage( DPLAYINFO* pDPInfo );
VOID    HandleApplicationMessage( DPLAYINFO* pDPInfo, DPMSG_GENERIC* pMsg,
                                  DWORD dwMsgSize, DPID idFrom, DPID idTo );
VOID    HandleSystemMessage( DPLAYINFO* pDPInfo, DPMSG_GENERIC* pMsg,
                             DWORD dwMsgSize, DPID idFrom, DPID idTo );
HRESULT GetPlayerName( LPDIRECTPLAY4A pDP, DPID dpidPlayer,
                       DPNAME** ppName );
HRESULT GetGroupName( LPDIRECTPLAY4A pDP, DPID dpidGroup, DPNAME** ppName );
HRESULT NewChatString( LPDIRECTPLAY4A pDP, DPID dpidPlayer, LPSTR strMsg,
                       LPSTR* pstrStr );
HRESULT DoCreateRoom( HWND hWnd, DPLAYINFO* pDPInfo );
HRESULT DoDeleteRoom( HWND hWnd, DPLAYINFO* pDPInfo );
HRESULT DoCreateTable( HWND hWnd, DPLAYINFO* pDPInfo );
HRESULT DoDeleteTable( HWND hWnd, DPLAYINFO* pDPInfo );
HRESULT DoDeletePlayerFromGroup( HWND hWnd, DPLAYINFO* pDPInfo );
HRESULT DoLaunch( HWND hWnd, DPLAYINFO* pDPInfo );
HRESULT DoGroupConnectionSettings( HWND hWnd, DPLAYINFO* pDPInfo );
HRESULT DoSetGroupOwner( HWND hWnd, DPLAYINFO* pDPInfo );
HRESULT InitializeLobby( HWND hWnd, DPLAYINFO* pDPInfo );
HRESULT UpdateLobbyDisplay( HWND hWnd, DPLAYINFO* pDPInfo );
VOID    UpdateButtons( HWND hWnd, DPLAYINFO* pDPInfo );
HRESULT SendChatMessage( HWND hWnd, DPLAYINFO* pDPInfo );

VOID    EnableDlgButton( HWND hWnd, int nIDDlgItem, BOOL bEnable );
VOID    HandleStartSession( DPLCONNECTION* pConn, DPLAYINFO* pDPInfo );
BOOL FAR PASCAL EnumApp( const DPLAPPINFO* pAppInfo, VOID* pContext, DWORD dwFlags );
HRESULT GetComboBoxGuid( HWND hWnd, LONG iDialogItem, GUID* pguidReturn );
HRESULT GetLocalAppName( LPDIRECTPLAYLOBBY pDPLobby,
                         GUID* pguidApplication, LPSTR strAppName );
VOID    LogString( LPSTR strDisplayFormat, LPSTR strData );
VOID    OnSize( HWND hWnd, DPLAYINFO* pDPInfo );

// Globals
extern HINSTANCE g_hInstance;



