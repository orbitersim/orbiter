//-----------------------------------------------------------------------------
// File: GameProc.h
//
// Desc: Game processing routines
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#define IDIRECTPLAY2_OR_GREATER
#include <ddraw.h>
#include <dplay.h>
#include <dsound.h>

// align on single byte boundaries
// this is a stop-gap measure until the structures can be re-arranged.
#pragma pack(1)

#define MAX_SHIP_X     (MAX_SCREEN_X - 32)
#define MAX_SHIP_Y     (MAX_SCREEN_Y - 32)
#define MAX_SHIP_FRAME 40
#define MAX_BULLET_X    (MAX_SCREEN_X - 3)
#define MAX_BULLET_Y    (MAX_SCREEN_Y - 3)
#define MAX_BULLET_FRAME 400

#define NUM_SHIP_TYPES 4

#define DEF_SHOW_DELAY     (2000)
#define MAX_BUFFER_SIZE    256

#define UPDATE_INTERVAL     40      // interval between position updates in milliseconds (25 FPS)
#define SYNC_INTERVAL       1000    // synchronize once every second
#define HIDE_TIMEOUT        5000    // time for which a ship is disabled after a hit

// Keyboard commands
#define KEY_STOP        0x00000001l
#define KEY_DOWN        0x00000002l
#define KEY_LEFT        0x00000004l
#define KEY_RIGHT       0x00000008l
#define KEY_UP          0x00000010l
#define KEY_FIRE        0x00000020l
#define KEY_ENGINEOFF   0x00000040l

// Offsets for the bullet bitmap
#define     BULLET_X    304
#define     BULLET_Y    0

struct FRAG
{
    double      dPosX;
    double      dPosY;
    LPDIRECTDRAWSURFACE pdds;
    RECT        src;
    double      dVelX;
    double      dVelY;
    BOOL        valid;
};

struct SHIP
{
    double              dPosX, dPosY;               // ship x and y position
    double              dBulletPosX, dBulletPosY;   // bullet x and y position
    DWORD               dwBulletFrame;              // bullet frame
    char                cFrame;                     // current ship frame
    BYTE                byType;                     // ship type 
    BOOL                bEnable;                    // is this ship active?
    BOOL                bBulletEnable;              // Is there an active bullet?

    double              dVelX, dVelY;               // ship x and y velocity (pixels/millisecond)
    double              dBulletVelX, dBulletVelY;   // bullet x and y velocity (pixels/millisecond)
    DWORD               dwScore;                    // current score
    DWORD               dwLastTick;                 // most recent time stamp
    BOOL                bIgnore;                    // flag used to synchronize ship explosions
    int                 iCountDown;                 // enable time-out            
    DWORD               dwFrameCount;               // number of frames since beginning of time

    // DSound members 
    DWORD                 dwKeys;                  // the keyboard state
    BOOL                  bEngineRunning;          // These BOOLs keep track of the ship's
    BOOL                  bMoving;                 //   last condition so we can play sounds
    BOOL                  bBounced;                //   when they change
    BOOL                  bBlockHit;
    BOOL                  bDeath;
    BOOL                  bFiring;
};

struct BLOCKS
{
    BYTE bits[30][5];
};




//-----------------------------------------------------------------------------
// communication packet structures
//-----------------------------------------------------------------------------
#define MSG_HOST        0x11    // message containing field layout, sent by host
#define MSG_BLOCKHIT    0x22    // block hit message
#define MSG_SHIPHIT     0x33    // ship hit message
#define MSG_ADDBLOCK    0x44    // add block message
#define MSG_CONTROL     0x55    // game keys message
#define MSG_SYNC        0x66    // synchronization message containing the rendezvous location

struct GENERICMSG
{
    BYTE byType;
};

struct OLDHOSTMSG
{
    BYTE byType;
    BLOCKS Blocks;
};

struct HOSTMSG
{
    BYTE        byType;
    BLOCKS      Blocks;
    int         usedShipTypes[NUM_SHIP_TYPES];
};

struct BLOCKHITMSG
{
    BYTE        byType;
    BYTE        byRow;
    BYTE        byCol;
    BYTE        byMask;
};

struct SHIPHITMSG
{
    BYTE        byType;
    DPID        Id;
};

struct ADDBLOCKMSG
{
    BYTE        byType;
    BYTE        byX;
    BYTE        byY;
};

struct CONTROLMSG
{
    BYTE        byType;
    BYTE        byState;
};

struct SYNCMSG
{
    BYTE        byType;
    BYTE        byShipType;     // this is needed only when sends are unreliable
    char        cFrame;
    double      dPosX;
    double      dPosY;
};




//-----------------------------------------------------------------------------
// Prototypes
//-----------------------------------------------------------------------------
VOID    LaunchGame();
VOID    ExitGame();
HRESULT InitOurShip();

HRESULT InitLocalSoundData();
BOOL WINAPI SetPlayerLocalSoundDataCB( DPID dpId, DWORD dwPlayerType,
                                       LPCDPNAME pName, DWORD dwFlags,
                                       VOID* pContext );

VOID    ReleaseLocalData();
VOID    ReleasePlayerLocalSoundData( SHIP* pShip );
BOOL WINAPI ReleasePlayerLocalDataCB( DPID dpId, DWORD dwPlayerType,
                                      LPCDPNAME pName, DWORD dwFlags,
                                      VOID* pContext );

VOID    UpdateState( SHIP* pShip, DWORD dwControls );
VOID    SendSync( SHIP* pShip );
VOID    UpdateDisplayStatus( SHIP* pShip );
VOID    UpdatePosition( DPID dpId, SHIP* ship );
BOOL    IsHit( int x, int y );
VOID    InitField();
BOOL    setBlock( int x, int y );
VOID    AddFrag( SHIP* pShip, int offX, int offY );
VOID    UpdateFragment( int i );
VOID    DestroyShip( SHIP* pShip );
VOID    DestroyGame();
BOOL    UpdateFrame();

VOID    ProcessSoundFlags( SHIP* pShip );
BOOL WINAPI RenderPlayerCB( DPID dpId, DWORD dwPlayerType, LPCDPNAME pName, 
                            DWORD dwFlags, VOID* pContext );
BOOL    DrawScreen();
BOOL    DrawScore();
VOID    DrawShip( SHIP* pShip );
VOID    DrawBlock( int x, int y );
VOID    DrawBullet( SHIP* pShip );
VOID    DrawFragments();
VOID    DisplayFrameRate();

VOID    GetConnection();
HRESULT ReceiveMessages();
VOID    DoSystemMessage( DPMSG_GENERIC* pMsg, DWORD dwMsgSize, DPID idFrom,
                         DPID idTo );
VOID    DoApplicationMessage( GENERICMSG* pMsg, DWORD dwMsgSize, DPID idFrom,
                              DPID idTo );
VOID    SendGameMessage( GENERICMSG* pMsg, DPID idTo );
VOID    CleanupComm();


HRESULT InitializeGameSounds();
VOID    CleanupGameSounds();



// restore default alignment
#pragma pack()



