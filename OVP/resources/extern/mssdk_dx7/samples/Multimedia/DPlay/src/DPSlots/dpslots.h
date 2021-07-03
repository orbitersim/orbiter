//-----------------------------------------------------------------------------
// File: DPSlots.h
//
// Desc: Common header
//
// Copyright (C) 1996-1999 Microsoft Corporation.  All Rights Reserved.
//-----------------------------------------------------------------------------
#define IDIRECTPLAY2_OR_GREATER
#include <dplay.h>
#include <dplobby.h>

// Constants
#define NUMWHEELS               3       // no. wheels
#define SLOTSPERWHEEL           6       // no. slots per wheel
#define MAXSTRLEN               200     // max size of a string
#define DEFAULTDATABASE         "slotsdb.txt" // default name of database file

// Flags to use when creating players
#define CLIENTPLAYERFLAGS           0
#define SERVERPLAYERFLAGS           (DPPLAYER_SERVERPLAYER)

// Flags to use when sending messages
#define NONSECURESENDFLAGS          (DPSEND_GUARANTEED)
#define SECURESENDFLAGS             (DPSEND_GUARANTEED |    \
                                     DPSEND_SIGNED |        \
                                     DPSEND_ENCRYPTED)
#define SENDFLAGS(bIsSecure)        ((bIsSecure) ? SECURESENDFLAGS : NONSECURESENDFLAGS)

// Flags to use when creating sessions
#define NONSECURESESSIONFLAGS       (DPSESSION_KEEPALIVE |      \
                                     DPSESSION_CLIENTSERVER)
#define SECURESESSIONFLAGS          (DPSESSION_KEEPALIVE |      \
                                     DPSESSION_CLIENTSERVER |   \
                                     DPSESSION_SECURESERVER)
#define SESSIONFLAGS(bIsSecure)     ((bIsSecure) ? SECURESESSIONFLAGS : NONSECURESESSIONFLAGS)

// Client messages
#define BALANCEREQUEST          300     // request for account balance
#define SPINREQUEST             302     // request for spin

// Server messages
#define BALANCERESPONSE         400     // account balance reply
#define SPINRESPONSE            401     // spin reply

// Structure used to store DirectPlay information
struct DPLAYINFO
{
    LPDIRECTPLAY4A  pDPlay;       // IDirectPlay4A interface pointer
    HANDLE          hPlayerEvent; // Player event to use
    DPID            dpidPlayer;   // ID of player created
    BOOL            bIsHost;      // TRUE if we are hosting the session
    BOOL            bIsSecure;    // TRUE if session is secure
};

// Message structures

// Wsed to request the results of a wager
struct MSG_SPINREQUEST
{
    DWORD   dwType;                     // message type
    DWORD   dwAmountBet;                // amount to wager
};

// response to wager request
struct MSG_SPINRESPONSE
{
    DWORD   dwType;                     // message type
    HRESULT hr;                         // result of request
    LONG    dwAmountWonOrLost;          // amount won or lost
    DWORD   dwBalance;                  // current balance after wager
    DWORD   dwIndex[NUMWHEELS];         // slot settings
};

// used to request current balance
struct MSG_BALANCEREQUEST
{
    DWORD   dwType;                     // message type
};

// response to balance request
struct MSG_BALANCERESPONSE
{
    DWORD   dwType;                     // message type
    HRESULT hr;                         // result of request
    DWORD   dwBalance;                  // current balance
};

// guid for this application
// {EC4F7AA0-E1E0-11d0-9C50-00A0C905425E}
DEFINE_GUID(DPSLOTS_GUID, 
0xec4f7aa0, 0xe1e0, 0x11d0, 0x9c, 0x50, 0x0, 0xa0, 0xc9, 0x5, 0x42, 0x5e);

// globals
extern HINSTANCE ghInstance;        // instance of application
extern CHAR      g_strDatabaseName[MAXSTRLEN];

//-----------------------------------------------------------------------------
// Common functions
//-----------------------------------------------------------------------------
HRESULT ConnectUsingLobby( DPLAYINFO* pDPInfo );
HRESULT ConnectUsingDialog( HINSTANCE hInstance, DPLAYINFO* pDPInfo );
VOID    ErrorBox( LPSTR strErrorStr, HRESULT hr );
VOID    CheckDlgItem( HWND hDlg, int nIDDlgItem, BOOL bCheck );
BOOL    DlgItemIsChecked( HWND hDlg, int nIDDlgItem );
VOID    EnableDlgButton( HWND hDlg, int nIDDlgItem, BOOL bEnable );


//-----------------------------------------------------------------------------
// Client functions
//-----------------------------------------------------------------------------
BOOL CALLBACK ClientWndProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                             LPARAM lParam );
VOID          ClientSystemMessage( DPLAYINFO* pDPInfo, DPMSG_GENERIC* pMsg,
                                   DWORD dwMsgSize, DPID idFrom, DPID idTo );
VOID          ClientApplicationMessage( DPLAYINFO* pDPInfo,
                                        DPMSG_GENERIC* pMsg, DWORD dwMsgSize,
                                        DPID idFrom, DPID idTo );


//-----------------------------------------------------------------------------
// Server functions
//-----------------------------------------------------------------------------
BOOL CALLBACK ServerWndProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                             LPARAM lParam);
VOID          ServerSystemMessage( DPLAYINFO* pDPInfo, DPMSG_GENERIC* pMsg,
                                   DWORD dwMsgSize, DPID idFrom, DPID idTo );
VOID          ServerApplicationMessage( DPLAYINFO* pDPInfo, 
                                        DPMSG_GENERIC* pMsg, DWORD dwMsgSize,
                                        DPID idFrom, DPID idTo );



