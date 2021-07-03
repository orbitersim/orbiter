//-----------------------------------------------------------------------------
// File: GameProc.cpp
//
// Desc: Game processing routines
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <math.h>
#include "duel.h"
#include "gameproc.h"
#include "gfx.h"
#include "DPUtil.h"
#include "diutil.h"
#include "dsutil.h"
#include "lobby.h"
#include "dsound.h"
#include "stdio.h"


//-----------------------------------------------------------------------------
// Globals
//-----------------------------------------------------------------------------
extern DWORD               g_dwKeys;
extern LPDIRECTDRAWSURFACE g_pddsShip[NUM_SHIP_TYPES];
extern BOOL                g_bShowFrameCount;
extern LPDIRECTDRAWSURFACE g_pddsNumbers;
extern BOOL                g_bHostPlayer;
extern BOOL                g_bIsActive;
extern DPSESSIONDESC2*     g_pdpsd;
extern HWND                g_hwndMain;
extern HINSTANCE           g_hInst;
extern BOOL                g_bReliable;
extern BOOL                g_bAsync;
extern BOOL                g_bUseProtocol;

FRAG        g_Frags[64];                    // ship framents
BLOCKS      g_Blocks;                   // field layout
VOID*       g_pvReceiveBuffer = NULL;   // buffer to store received messages
DWORD       g_dwReceiveBufferSize = 0;  // size of buffer
DPID        g_LocalPlayerDPID;                    // our player id
BOOL        g_bHaveHostInit;                // do we need to do host initializaton
int         g_nProgramState;            // current state of the game
DWORD       g_dwFrameCount;             // used for fps calc
DWORD       g_dwFramesLast;             // ..
DWORD       g_dwFrameTime;              // ..
BOOL        g_bUpdate;                   // TRUE if player data needs to be updated
BOOL        g_bSessionLost;              // did we get disconnected ?
HOSTMSG     g_HostMsg;                   // message buffer
BLOCKHITMSG g_BlockHitMsg;               // ..
SHIPHITMSG  g_ShipHitMsg;                // ..
ADDBLOCKMSG g_AddBlockMsg;               // ..
CONTROLMSG  g_ControlMsg;                // .. 
SYNCMSG     g_SyncMsg;                   // ..

SoundObject* g_pBulletFiringSound = NULL;
SoundObject* g_pShipExplodeSound  = NULL;
SoundObject* g_pShipEngineSound   = NULL;
SoundObject* g_pShipStartSound  = NULL;
SoundObject* g_pShipStopSound   = NULL;
SoundObject* g_pShipBounceSound   = NULL;
SoundObject* g_pBlockExplodeSound = NULL;


// Used to determine new joiner's ship type
static int g_ShipTypesUsed[NUM_SHIP_TYPES];




//-----------------------------------------------------------------------------
// Name: InitMessageBuffers()
// Desc: Sets up buffes used for sending different types of messages
//-----------------------------------------------------------------------------
VOID InitMessageBuffers()
{
    g_HostMsg.byType     = MSG_HOST;
    g_BlockHitMsg.byType = MSG_BLOCKHIT;
    g_ShipHitMsg.byType  = MSG_SHIPHIT;    
    g_AddBlockMsg.byType = MSG_ADDBLOCK;
    g_ControlMsg.byType  = MSG_CONTROL; 
    g_SyncMsg.byType     = MSG_SYNC;
}




//-----------------------------------------------------------------------------
// Name: LaunchGame()
// Desc: Sets up the game layout and launches
//-----------------------------------------------------------------------------
VOID LaunchGame()
{
    HRESULT hr;

    // initialize message buffers and other module globals
    InitMessageBuffers();
    g_bSessionLost = FALSE;

    // get current session description (g_pdpsd is initialized in here)
    hr = DPUtil_GetSessionDesc();
    if (FAILED(hr) || (!g_pdpsd))
    {
        ShowError(IDS_DPLAY_ERROR_GSD);
        ExitGame();
        return;
    }

    // Is this session using the DirectPlay Protocol?
    g_bUseProtocol = (g_pdpsd->dwFlags & DPSESSION_DIRECTPLAYPROTOCOL) != 0;

    // update window title
    UpdateTitle();

    // initialize random number seed
    srand((int)GetTickCount());

    // clear front buffer before changing palette
    EraseScreen();
    FlipScreen();

    // Begin collecting input
    DIUtil_ReacquireInputDevices();

    // init the ship type tracking
    ZeroMemory( g_ShipTypesUsed, sizeof(g_ShipTypesUsed) );

    // initialize us
    hr = InitOurShip();
    if( FAILED(hr) )
    {
        ExitGame();
        return;
    }

    // get the field layout
    if( g_bHostPlayer )
    {
        // initialize field (done only by host)
       InitField();

        // we have host initialization
        g_bHaveHostInit = TRUE;

        // start updating screen
        g_bIsActive = TRUE;
    }
    else
    {
        // get it from host, if we are joining
        g_bHaveHostInit = FALSE;
    }

    return;
}




//-----------------------------------------------------------------------------
// Name: ExitGame()
// Desc: Game termination code
//-----------------------------------------------------------------------------
VOID ExitGame()
{
    // shut down app
    PostMessage( g_hwndMain, WM_CLOSE, 0, 0 );
}




//-----------------------------------------------------------------------------
// Name: InitOurShip()
// Desc: Initializes our ship's initial attributes
//-----------------------------------------------------------------------------
HRESULT InitOurShip()
{
    SHIP    ship;
    HRESULT hr;

    // Wipe out everything
    ZeroMemory( &ship, sizeof(ship) );

    // Calculate ship type based on the number of players in the game
    // we cycle through four ships (Y,R,G,B) for now.
    // This old algorithm's computation will be overridden by HOST_MSG 
    ship.byType = (BYTE) ((g_pdpsd->dwCurrentPlayers-1) % NUM_SHIP_TYPES);
    g_ShipTypesUsed[ship.byType] = 1;   // our color

    ship.dPosX   = randInt( 0, MAX_SHIP_X );
    ship.dPosY   = randInt( 0, MAX_SHIP_Y );
    ship.cFrame  = randInt( 0, MAX_SHIP_FRAME );
    ship.bEnable = TRUE;

    // Set our local data
    hr = DPUtil_SetPlayerLocalData( g_LocalPlayerDPID, &ship, sizeof(ship) );
    if( FAILED(hr) )
    {
        ShowError(IDS_DPLAY_ERROR_SPLD);
        return hr;
    }

    // No ship fragments
    for( int i=0; i<64; i++ )
        g_Frags[i].valid = FALSE;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetPlayerLocalSoundDataCB()
// Desc: Initializes and registers a player's local SOUND data ONLY.
//-----------------------------------------------------------------------------
BOOL WINAPI SetPlayerLocalSoundDataCB( DPID dpId, DWORD dwPlayerType,
                                       LPCDPNAME lpName, DWORD dwFlags,
                                       VOID* lpContext )
{
    SHIP ship;
    DWORD dwSize = sizeof(ship);
    HRESULT hr;

    DPUtil_GetPlayerLocalData(dpId, &ship, &dwSize); 

    // no player data yet
    if (0 == dwSize)
        return TRUE;

    hr = DPUtil_GetPlayerLocalData( dpId, &ship, &dwSize );
    return (DP_OK == hr);
};




//-----------------------------------------------------------------------------
// Name: InitLocalSoundData()
// Desc: Initializes and registers all players' sound data ONLY
//-----------------------------------------------------------------------------
HRESULT InitLocalSoundData()
{
    return DPUtil_EnumPlayers(&(g_pdpsd->guidInstance), SetPlayerLocalSoundDataCB, NULL, 0);
}




//-----------------------------------------------------------------------------
// Name: ReleaseLocalSoundData()
// Desc: Releases a single ship's local sound buffers.
//-----------------------------------------------------------------------------
VOID ReleasePlayerLocalSoundData( SHIP* pShip )
{
/*
    if( FALSE == g_bSoundInitialized )
        return;

    for( int i=0; i<MAX_SOUNDS; i++)
    {
        if( pShip->pDSBuffer[i] )
        {
            pShip->pDSBuffer[i]->Stop();
            pShip->pDSBuffer[i]->Release();
            pShip->pDSBuffer[i] = NULL;
        }

        if( pShip->pDS3DBuffer[i] )
        {
            pShip->pDS3DBuffer[i]->Release();   
            pShip->pDS3DBuffer[i] = NULL;
        }
    }
*/
}




//-----------------------------------------------------------------------------
// Name: ReleasePlayerLocalData()
// Desc: Retrieves and releases a player's local data from dplay.
//-----------------------------------------------------------------------------
BOOL WINAPI ReleasePlayerLocalDataCB( DPID dpId, DWORD dwPlayerType,
                                      LPCDPNAME lpName, DWORD dwFlags,
                                      VOID* lpContext)
{
    SHIP ship;
    DWORD dwSize = sizeof(SHIP);    
    HRESULT hr;
    
    hr = DPUtil_GetPlayerLocalData( dpId, &ship, &dwSize );
    if( FAILED(hr) )
        goto FAIL;

    // no player data yet
    if (0 == dwSize)
        return TRUE;

    ReleasePlayerLocalSoundData(&ship);

    hr = DPUtil_SetPlayerLocalData(dpId, &ship,  dwSize );
    if (FAILED(hr))
        goto FAIL;

    // success
    return TRUE;
FAIL:
    // we failed
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: ReleaseLocalData()
// Desc: Releases local data of ALL players.
//-----------------------------------------------------------------------------
VOID ReleaseLocalData()
{
     if( g_nProgramState == PS_ACTIVE )
     {
         if( FAILED( DPUtil_EnumPlayers( &g_pdpsd->guidInstance,
                                         ReleasePlayerLocalDataCB,
                                         NULL, 0 ) ) )
         {
             ShowError(IDS_DPLAY_ERROR_EP);
         }
     }
};




//-----------------------------------------------------------------------------
// Name: ProcessUserInput()
// Desc: Processes any input from the user and updates our ship state
//-----------------------------------------------------------------------------
VOID ProcessUserInput( SHIP* pShip )
{
    static dwOldKeys = 0;

    g_bUpdate = FALSE;

    // DSOUND: check if the engine was turned off
    if( !(g_dwKeys & (KEY_DOWN | KEY_UP)) && 
        (dwOldKeys & (KEY_DOWN | KEY_UP)) )
    {
        g_dwKeys |= KEY_ENGINEOFF;

        g_ControlMsg.byState = (BYTE)g_dwKeys;
        // let everyone know that we just turned our engine off
        SendGameMessage( (GENERICMSG*)&g_ControlMsg, DPID_ALLPLAYERS );
    }

    // update our ship state
    UpdateState( pShip, g_dwKeys );

    // remember current keys for next frame
    dwOldKeys = g_dwKeys;
}




//-----------------------------------------------------------------------------
// Name: UpdateState()
// Desc: Updates the current state of our ship, given user input
//-----------------------------------------------------------------------------
VOID UpdateState( SHIP* pShip, DWORD dwControls )
{
    static DWORD dwTimePrevUpdate = 0;
    DWORD dwTimeNow = timeGetTime();
    
    pShip->dwKeys = dwControls;

    // Ship orientation
    FLOAT fAngle = (FLOAT)( ( pShip->cFrame / 40.0f ) * 2 * 3.1415926283 );
        
    // Don't accept these keyboard inputs faster than 50
    // times per second (every 20 ms)
    if( dwTimeNow - dwTimePrevUpdate > 20 )
    {
        if( dwControls & KEY_LEFT )
        {
            g_bUpdate = TRUE;
            pShip->cFrame--;
            if( pShip->cFrame < 0 )
                pShip->cFrame += MAX_SHIP_FRAME;
        }
        if( dwControls & KEY_RIGHT )
        {
            g_bUpdate = TRUE;
            pShip->cFrame++;
            if( pShip->cFrame >= MAX_SHIP_FRAME )
                pShip->cFrame -= MAX_SHIP_FRAME;
        }

        if( dwControls & KEY_UP )
        {
            g_bUpdate = TRUE;
            pShip->dVelX +=  sin(fAngle) * 10.0 / 1000.0;
            pShip->dVelY += -cos(fAngle) * 10.0 / 1000.0;
        }
        if( dwControls & KEY_DOWN )
        {
            g_bUpdate = TRUE;
            pShip->dVelX -=  sin(fAngle) * 10.0 / 1000.0;
            pShip->dVelY -= -cos(fAngle) * 10.0 / 1000.0;
        }
        dwTimePrevUpdate = dwTimeNow;
    }

    if( dwControls & KEY_STOP )
    {
        g_bUpdate = TRUE;
        pShip->dVelX = 0.0;
        pShip->dVelY = 0.0;
    }
    if( !pShip->bBulletEnable && pShip->bEnable )
    {
        if( dwControls & KEY_FIRE )
        {
            g_bUpdate = TRUE;
            // launch a new bullet
            pShip->dBulletPosX = (WORD)( sin(fAngle)*6.0 + 16.0 + pShip->dPosX );
            pShip->dBulletPosY = (WORD)(-cos(fAngle)*6.0 + 16.0 + pShip->dPosY );
            pShip->dBulletVelX =  sin(fAngle)*500.0/1000.0;
            pShip->dBulletVelY = -cos(fAngle)*500.0/1000.0;
            pShip->bBulletEnable = TRUE;
            pShip->dwBulletFrame = 0;
        }
    }
}




//-----------------------------------------------------------------------------
// Name: SendSync()
// Desc: Sends a sync message with the rendevous position. We are using a
//       synchronization technique in which every player informs everyone else
//       where the player is going to be at the end of the next sync interval.
//       Based on this rendezvous position, everyone tries to move their
//       corresponding ghosts to the rendezvous position by the end of the
//       interval.
//-----------------------------------------------------------------------------
VOID SendSync( SHIP* pShip )
{
    g_SyncMsg.byShipType = pShip->byType;
    g_SyncMsg.cFrame     = pShip->cFrame;
    g_SyncMsg.dPosX      = pShip->dPosX + pShip->dVelX*1000;
    g_SyncMsg.dPosY      = pShip->dPosY + pShip->dVelY*1000;

    SendGameMessage( (GENERICMSG*)&g_SyncMsg, DPID_ALLPLAYERS );
}




//-----------------------------------------------------------------------------
// Name: UpdateDisplayStatus()
// Desc: Updates the disable timeout. Enables the ship if disable timeout has
//       elapsed.
//-----------------------------------------------------------------------------
VOID UpdateDisplayStatus( SHIP* pShip )
{
    DWORD dwTickDiff;
    DWORD dwTickCount;

    // current time
    dwTickCount = timeGetTime();

    // time elapsed since last update
    dwTickDiff = dwTickCount - pShip->dwLastTick;

    // timestamp
    pShip->dwLastTick = dwTickCount;

    // update time-out
    pShip->iCountDown -= dwTickDiff;

    // time-out ?
    if( pShip->iCountDown < 0 )
    {
        // get new position and enable our lpShip
        pShip->dPosX   = randInt( 0, MAX_SHIP_X );
        pShip->dPosY   = randInt( 0, MAX_SHIP_Y );
        pShip->cFrame  = randInt( 0, MAX_SHIP_FRAME );
        pShip->bEnable = TRUE;
    }
}




//-----------------------------------------------------------------------------
// Name: UpdatePosition()
// Desc: Updates our ship's position
//-----------------------------------------------------------------------------
VOID UpdatePosition( DPID dpId, SHIP* lpShip )
{
    int     x,y;
    BYTE    oldxCell, oldyCell, newxCell, newyCell, mask, col, row;
    double  thisTick, totalTick, xtick, ytick;
    DWORD   dwTickCount;
    DWORD   dwTickDiff;

    if( !lpShip->bEnable )
        return;

    // how long has it been since we last updated
    dwTickCount = timeGetTime();
    dwTickDiff = dwTickCount - lpShip->dwLastTick;

    // current timestamp
    lpShip->dwLastTick = dwTickCount;

    oldxCell = (int)(lpShip->dPosX+16.0) >> 4;
    oldyCell = (int)(lpShip->dPosY+16.0) >> 4;

    // compute new position
    lpShip->dPosX += lpShip->dVelX * dwTickDiff;
    lpShip->dPosY += lpShip->dVelY * dwTickDiff;

    newxCell = (int)(lpShip->dPosX+16.0) >> 4;
    newyCell = (int)(lpShip->dPosY+16.0) >> 4;
    if(oldxCell != newxCell)
    {
        // we allow ghosts to pass through the blocks
        if( (dpId == g_LocalPlayerDPID) && IsHit( newxCell, newyCell ) )
        {
            if( lpShip->dVelX > 0.0 )
                lpShip->dPosX = (oldxCell << 4) + 15 - 16;
            else
                lpShip->dPosX = (oldxCell << 4) - 16;
            lpShip->dVelX = -lpShip->dVelX*0.9;
            newxCell = oldxCell;
            lpShip->bBounced = TRUE;
        }
    }
    if(oldyCell != newyCell)
    {
        // we allow ghosts to pass through the blocks
        if( (dpId == g_LocalPlayerDPID) && IsHit( newxCell, newyCell ) )
        {
            if( lpShip->dVelY > 0.0 )
                lpShip->dPosY = (oldyCell << 4) + 15 - 16;
            else
                lpShip->dPosY = (oldyCell << 4) - 16;

            lpShip->dVelY = -lpShip->dVelY*0.9;
            lpShip->bBounced = TRUE;
        }
    }

    if( lpShip->dPosX > MAX_SHIP_X )
    {
        lpShip->dPosX = MAX_SHIP_X;
        lpShip->dVelX = -lpShip->dVelX*0.9;
        lpShip->bBounced = TRUE;
    }
    else if ( lpShip->dPosX < 0 )
    {
        lpShip->dPosX =0;
        lpShip->dVelX = -lpShip->dVelX*0.9;
        lpShip->bBounced = TRUE;
    }
    if( lpShip->dPosY > MAX_SHIP_Y )
    {
        lpShip->dPosY = MAX_SHIP_Y;
        lpShip->dVelY = -lpShip->dVelY*0.9;
        lpShip->bBounced = TRUE;
    }
    else if ( lpShip->dPosY < 0 )
    {
        lpShip->dPosY =0;
        lpShip->dVelY = -lpShip->dVelY*0.9;
        lpShip->bBounced = TRUE;
    }    

    if ((dpId == g_LocalPlayerDPID) && lpShip->bBounced)
    {
        SendSync(lpShip);
    }

    if( !lpShip->bBulletEnable )
        return;

    // update the active bullet
    lpShip->dwBulletFrame += dwTickDiff;
    if( lpShip->dwBulletFrame >= MAX_BULLET_FRAME )
    {
        lpShip->bFiring = FALSE;
        lpShip->bBulletEnable = FALSE;
        return;
    }


    if( lpShip->dBulletVelX != 0.0 )
        xtick = 8.0/lpShip->dBulletVelX;
    else
        xtick = 999999.0;

    if( lpShip->dBulletVelY != 0.0 )
        ytick = 8.0/lpShip->dBulletVelY;
    else
        ytick = 999999.0;

    if( xtick < 0.0 )
        xtick = -xtick;
    if( ytick < 0.0 )
        ytick = -ytick;

    if( xtick < ytick )
        thisTick = xtick;
    else
        thisTick = ytick;
    
    if( thisTick > dwTickDiff )
        thisTick = dwTickDiff;
            
    for( totalTick = 0.0; totalTick < dwTickDiff; )
    {
        totalTick += thisTick;

        lpShip->dBulletPosX += lpShip->dBulletVelX * thisTick;
        lpShip->dBulletPosY += lpShip->dBulletVelY * thisTick;

        if( lpShip->dBulletPosX > MAX_BULLET_X )
        {
            lpShip->dBulletPosX = MAX_BULLET_X;
            lpShip->dBulletVelX = -lpShip->dBulletVelX*0.9;
        }
        else if ( lpShip->dBulletPosX < 0 )
        {
            lpShip->dBulletPosX =0;
            lpShip->dBulletVelX = -lpShip->dBulletVelX*0.9;
        }
        if( lpShip->dBulletPosY > MAX_BULLET_Y )
        {
            lpShip->dBulletPosY = MAX_BULLET_Y;
            lpShip->dBulletVelY = -lpShip->dBulletVelY*0.9;
        }
        else if ( lpShip->dBulletPosY < 0 )
        {
            lpShip->dBulletPosY =0;
            lpShip->dBulletVelY = -lpShip->dBulletVelY*0.9;
        }
    
        // check to see if it hit anything
        x = (int)(lpShip->dBulletPosX + 0.5) + 1;
        y = (int)(lpShip->dBulletPosY + 0.5) + 1;
    
        row = y >> 4;
        col = x >> 4;
        mask = 1 << (col & 0x7);
        col = col >> 3;
        if( g_Blocks.bits[row][col] & mask )
        {
            // dwScored a block hit
            g_BlockHitMsg.byRow = row;
            g_BlockHitMsg.byCol = col;
            g_BlockHitMsg.byMask = mask;
            SendGameMessage( (GENERICMSG*)&g_BlockHitMsg, DPID_ALLPLAYERS);

            g_Blocks.bits[row][col] &= ~mask;
            lpShip->dwScore += 10;
            lpShip->bBulletEnable = FALSE;
            lpShip->bBlockHit = TRUE;
            lpShip->bFiring   = FALSE;
        }
    }
}




//-----------------------------------------------------------------------------
// Name: IsHit()
// Desc: Tells if there is a block at (x,y) location
//-----------------------------------------------------------------------------
BOOL IsHit( int x, int y )
{
    int col, mask;
    
    // outside screen boundaries?
    if( (x < 0) || (y < 0) || (x >= 40) || (y >= 30) )
        return TRUE;
    
    // look at the block bits
    mask = 1 << (x & 0x7);
    col = x >> 3;
    if( g_Blocks.bits[y][col] & mask )
        return TRUE;
    else
        return FALSE;
}




//-----------------------------------------------------------------------------
// Name: InitField
// Desc: Initializes block positions on the field
//-----------------------------------------------------------------------------
VOID InitField()
{
    int i, x, y;
    
    // clear all g_Blocks
    for(x=0; x<5; x++)
        for(y=0; y<30; y++)
            g_Blocks.bits[y][x] = 0;

    // set random gBlocks
    for(i=0; i<400; i++)
    {
        x = randInt(0, 40);
        y = randInt(0, 30);
        if( !setBlock(x, y) ) i--;
    }
}




//-----------------------------------------------------------------------------
// Name: AddBlock()
// Desc: Adds a block to the field
//-----------------------------------------------------------------------------
VOID AddBlock()
{
    // Maybe add a block?
    if( g_bHostPlayer && g_bIsActive && ( randInt( 0, 100 ) > 98 ) )
    {
        int x = randInt( 0, 40);
        int y = randInt( 0, 30);
        if( setBlock( x, y) )
        {
            g_AddBlockMsg.byX = (BYTE)x;
            g_AddBlockMsg.byY = (BYTE)y;
            SendGameMessage( (GENERICMSG*)&g_AddBlockMsg, DPID_ALLPLAYERS );
        }
    }
}




//-----------------------------------------------------------------------------
// Name: setBlock()
// Desc: Turns on a block
//-----------------------------------------------------------------------------
BOOL setBlock( int x, int y )
{
    BYTE  mask, col;

    mask = 1 << (x & 0x7);
    col = x >> 3;
    
    // is Block already set?
    if( g_Blocks.bits[y][col] & mask )
        return FALSE;
    
    // set the block and return success
    g_Blocks.bits[y][col] |= mask;
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: AddFrag()
// Desc: Turns on a fragment
//-----------------------------------------------------------------------------
VOID AddFrag( SHIP* pShip, int offX, int offY )
{
    for( int i=0; i<64; i++ ) // Find available fragment
    {
        if( !g_Frags[i].valid )
            break;
    }
    if( i == 64 )
        return;
        
    g_Frags[i].dPosX      = offX + pShip->dPosX;
    g_Frags[i].dPosY      = offY + pShip->dPosY;
    g_Frags[i].pdds       = g_pddsShip[pShip->byType];
    g_Frags[i].src.top    = 32 * ( (int)pShip->cFrame / 10 ) + offX;
    g_Frags[i].src.left   = 32 * ( (int)pShip->cFrame % 10 ) + offY;
    g_Frags[i].src.right  = g_Frags[i].src.left + 8;
    g_Frags[i].src.bottom = g_Frags[i].src.top + 8;
    g_Frags[i].dVelX      = ((double)offX - 12.0)/24.0;
    g_Frags[i].dVelY      = ((double)offY - 12.0)/24.0;
    g_Frags[i].valid      = TRUE;
}




//-----------------------------------------------------------------------------
// Name: UpdateFragment()
// Desc: Updates the position of a fragment
//-----------------------------------------------------------------------------
VOID UpdateFragment( int i )
{
    DWORD dwTickCount;
    static DWORD dwTickDiff;
    static DWORD dwLastTick;

    if( i == 0 )
    {
        dwTickCount = timeGetTime();
        dwTickDiff = dwTickCount - dwLastTick;
        dwLastTick = dwTickCount;

        // If the FPS is high, keep the fragments moving
        if( dwTickDiff < 10 )
            dwTickDiff = 10;
    }
    
    if( !g_Frags[i].valid )
        return;
    
    g_Frags[i].dPosX += (int) (g_Frags[i].dVelX * dwTickDiff);
    g_Frags[i].dPosY += (int) (g_Frags[i].dVelY * dwTickDiff);
    if( (g_Frags[i].dPosX < 0.0) || (g_Frags[i].dPosX >= 632.0) ||
        (g_Frags[i].dPosY < 0.0) || (g_Frags[i].dPosY >= 472.0) )
    {
        g_Frags[i].valid = FALSE;
    }
}




//-----------------------------------------------------------------------------
// Name: DestroyShip()
// Desc: Adds a bunch of fragments to show that the ship is destroyed
//-----------------------------------------------------------------------------
VOID DestroyShip( SHIP* pShip )
{
    // Set flag for explosion sound
    pShip->bDeath  = TRUE;

    // Add ship fragments
    AddFrag( pShip,  0,  0 );
    AddFrag( pShip,  8,  0 );
    AddFrag( pShip, 16,  0 );
    AddFrag( pShip, 24,  0 );
    AddFrag( pShip,  0,  8 );
    AddFrag( pShip,  8,  8 );
    AddFrag( pShip, 16,  8 );
    AddFrag( pShip, 24,  8 );
    AddFrag( pShip,  0, 16 );
    AddFrag( pShip,  8, 16 );
    AddFrag( pShip, 16, 16 );
    AddFrag( pShip, 24, 16 );
    AddFrag( pShip,  0, 24 );
    AddFrag( pShip,  8, 24 );
    AddFrag( pShip, 16, 24 );
    AddFrag( pShip, 24, 24 );

    // Play explosion sound
    ProcessSoundFlags( pShip );
}




//-----------------------------------------------------------------------------
// Name: UpdateFrame()
// Desc: Refreshes the screen
//-----------------------------------------------------------------------------
BOOL UpdateFrame()
{
    static DWORD dwSyncLastTick     = 0L;
    static DWORD dwUpdateLastTick   = 0L;
    static DWORD dwLobbyMsgLastTick = 0L;
    DWORD   dwTickCount;
    SHIP    ship;
    DWORD   dwSize;
    HRESULT hr;
    int     i;
    
    switch( g_nProgramState )
    {
        case PS_ACTIVE:
            // Use DirectInput to read game-play keys
            DIUtil_ReadKeys( &g_dwKeys );

            // get our local data
            dwSize = sizeof(ship);
            hr = DPUtil_GetPlayerLocalData( g_LocalPlayerDPID, &ship, &dwSize );
            if( FAILED(hr) )
                return FALSE;

            if( !ship.bEnable )
            {
                // Ppdate disable timeout and display status
                UpdateDisplayStatus( &ship );
            }
            else
            {
                // Process any change in game controls 
                ProcessUserInput( &ship );

                dwTickCount = timeGetTime();

                // Synchronize if it's time
                if( g_bIsActive && ((dwTickCount - dwSyncLastTick) > SYNC_INTERVAL) )
                {
                    SendSync( &ship );
                    dwSyncLastTick = dwTickCount;
                }

                // If our player changed any keys, let everyone know
                if( g_bUpdate )
                {
                    // Control the number of packets we send
                    if( (dwTickCount - dwUpdateLastTick) > UPDATE_INTERVAL )
                    {
                        // Let others know
                        g_ControlMsg.byState = (BYTE)g_dwKeys;
                        SendGameMessage( (GENERICMSG*)&g_ControlMsg, DPID_ALLPLAYERS );
                        dwUpdateLastTick = dwTickCount;
                    }
                }

                // If connected to a lobby, occasionally report score as a property
                if( DoingLobbyMessages() && 
                    (dwTickCount - dwLobbyMsgLastTick) > 10000 )    // every 10 sec
                {
                    DPLDATA_PLAYERSCORE playerScore;
                    ZeroMemory(&playerScore, sizeof(DPLDATA_PLAYERSCORE));
                    playerScore.dwScoreCount = 1;
                    playerScore.Score[0] = (LONG)ship.dwScore;
                    LobbyMessageSetProperty( &DPLPROPERTY_PlayerScore, &playerScore,
                                             sizeof(DPLDATA_PLAYERSCORE));
                    dwLobbyMsgLastTick = dwTickCount;
                }
            }

            // Save ship data as RenderPlayerCB reads stored data
            hr = DPUtil_SetPlayerLocalData( g_LocalPlayerDPID, &ship, sizeof(ship) );
            if( FAILED(hr) )
            {
                ShowError(IDS_DPLAY_ERROR_SPLD);
                return FALSE;
            }   

            // Update fragments
            for( i=0; i<64; i++ )
                UpdateFragment(i);

            // Add a block
            AddBlock();

            // Render everything        
            if( !DrawScreen() )
            {
                return FALSE;
            }
            break;

        case PS_REST:
            if( g_bHaveHostInit )
            {
                SetGamePalette();
                g_nProgramState = PS_ACTIVE;
            }
            break;
    }

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: ProcessSoundFlags()
// Desc: 
//-----------------------------------------------------------------------------
VOID ProcessSoundFlags( SHIP* pShip )
{
    // Get object positions for panning sounds
    RECT rc;
    GetClientRect( g_hwndMain, &rc );
    FLOAT fShipXPos   = (FLOAT)( ( 2 * pShip->dPosX ) / rc.right )  - 1;
    FLOAT fShipYpos   = (FLOAT)( (-2 * pShip->dPosY ) / rc.bottom ) - 1;
    FLOAT fBulletXPos = (FLOAT)( ( 2 * pShip->dPosX ) / rc.right )  - 1;
    FLOAT fBulletYpos = (FLOAT)( (-2 * pShip->dPosY ) / rc.bottom ) - 1;

    // Set sound state based on user input and ship state
    if( pShip->bDeath )          // nothing but the boom
    {
        DSUtil_StopSound( g_pBulletFiringSound );
        DSUtil_StopSound( g_pShipBounceSound );
        DSUtil_StopSound( g_pShipStopSound );
        DSUtil_StopSound( g_pShipStartSound );
        DSUtil_StopSound( g_pShipEngineSound );
        DSUtil_PlaySound( g_pShipExplodeSound, 0 );

        pShip->bEngineRunning = FALSE;
        pShip->bMoving        = FALSE;
        pShip->dwKeys         = 0;      // No input for a dead ship
        pShip->bDeath         = FALSE;  // Turn off sound flag.
    }

    if( pShip->dwKeys & KEY_STOP )
    {
        if( pShip->bMoving )
        {
            DSUtil_StopSound( g_pShipEngineSound );
            DSUtil_PlaySound( g_pShipStopSound, 0 );
            pShip->bEngineRunning = FALSE;
            pShip->bMoving        = FALSE;
        }
    }

    if( pShip->dwKeys & KEY_ENGINEOFF )
    {
        DSUtil_StopSound( g_pShipEngineSound );
        pShip->bEngineRunning = FALSE;
    }

    if( pShip->dwKeys & (KEY_UP | KEY_DOWN) )
    {
        if( !pShip->bEngineRunning ) // Turn on engine
        {
            DSUtil_PlaySound( g_pShipEngineSound, DSBPLAY_LOOPING );
            if( !pShip->bMoving )   //"fire-up-engine" sound
            {
                DSUtil_PlayPannedSound( g_pShipStartSound, fShipXPos );
                pShip->bMoving = TRUE;
            }
            pShip->bEngineRunning = TRUE;
        }
    }

    if( pShip->dwKeys & KEY_FIRE )
    {
        if( !pShip->bFiring )
        {
            DSUtil_PlayPannedSound( g_pBulletFiringSound, fShipXPos );

            pShip->bFiring = TRUE;
        }
    }

    if( pShip->bBlockHit )
    {
        DSUtil_PlayPannedSound( g_pBlockExplodeSound, fBulletXPos );
        pShip->bBlockHit = FALSE;
    }

    if( pShip->bBounced )
    {
        DSUtil_PlayPannedSound( g_pShipBounceSound, fShipXPos );
        pShip->bBounced = FALSE;
    }

    pShip->dwKeys = 0;
}




//-----------------------------------------------------------------------------
// Name: RenderPlayerCB()
// Desc: Renders a ship in its current state. Also checks if we hit the ship
//       and informs the ship that it has been destroyed.
//-----------------------------------------------------------------------------
BOOL WINAPI RenderPlayerCB( DPID dpId, DWORD dwPlayerType, LPCDPNAME lpName, 
                            DWORD dwFlags, VOID* lpContext )
{
    SHIP    ship;
    SHIP    ourShip;
    DWORD   dwSize = sizeof(ship);
    BOOL    bHit = FALSE;
    HRESULT hr;
    DWORD   dwTickCount;
    
    // Get ship data
    hr = DPUtil_GetPlayerLocalData( dpId, &ship, &dwSize );
    if( FAILED(hr) )
        return FALSE;

    // No player data yet
    if( 0 == dwSize )
        return TRUE; 

    // Ignore current ship ?
    if( ship.bIgnore )
    {
        // If this ship was being ignored, update ignore time-out
        // A time-out is used here to ensure that this ship doesn't get ignored
        // forever on our screen in case the destroy message was dropped.
        dwTickCount = timeGetTime();
        ship.iCountDown -= dwTickCount - ship.dwLastTick;
        ship.dwLastTick = dwTickCount;
        if( ship.iCountDown < 0 ) 
        {
            ship.bIgnore = FALSE;
        }

        // Save ship data
        hr = DPUtil_SetPlayerLocalData( dpId, &ship, sizeof(ship) );
        if( FAILED(hr) )
            return FALSE;
        
        // We are ignoring this ship, so just bail
        return TRUE;
    }

    // Bail if ship is disabled
    if( !ship.bEnable)
        return TRUE;

    // Update ship's position
    UpdatePosition( dpId, &ship );

    // Get our player data to compare with others
    dwSize = sizeof(ship);
    hr = DPUtil_GetPlayerLocalData( g_LocalPlayerDPID, &ourShip, &dwSize );
    if( FAILED(hr) )
        return FALSE;

    // Check if our bullet hit the current ship
    if( (dpId != g_LocalPlayerDPID) && ourShip.bBulletEnable && ship.bEnable )
    {
        if( (ourShip.dBulletPosX > ship.dPosX) &&
            (ourShip.dBulletPosX < (ship.dPosX + 32.0) ) &&
            (ourShip.dBulletPosY > ship.dPosY) &&
            (ourShip.dBulletPosY < (ship.dPosY + 32.0) ) )
        {
            // Hasta-la-vista baby
            DestroyShip( &ship );
            // We nailed it
            bHit = TRUE;
            // Turn off ship locally
            ship.bEnable = FALSE;
            // Temporarily ignore ship until we get a response
            ship.bIgnore = TRUE;
            // Set its ignore time-out
            ship.iCountDown = HIDE_TIMEOUT;
            // Time-stamp
            ship.dwLastTick = timeGetTime();
            // Turn our bullet off
            ship.bBulletEnable = FALSE;
            // Update our score
            ourShip.dwScore += 1000;
            // Save our score
            hr = DPUtil_SetPlayerLocalData( g_LocalPlayerDPID, &ourShip, sizeof(ourShip) );
            if( FAILED(hr) )
            {
                ShowError(IDS_DPLAY_ERROR_SPLD);
                return FALSE;
            }
        }
    }

    // Render the ship
    if( ship.bBulletEnable ) DrawBullet(&ship);
    if( ship.bEnable )       DrawShip(&ship);

    ProcessSoundFlags( &ship );

    // Save ship data
    hr = DPUtil_SetPlayerLocalData( dpId, &ship, sizeof(ship) );
    if( FAILED(hr) )
        return FALSE;

    // Inform the player
    if( bHit )
    {
        g_ShipHitMsg.Id = dpId;
        SendGameMessage( (GENERICMSG*)&g_ShipHitMsg, dpId );
    }

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: DrawScreen()
// Desc: Renders the current frame
//-----------------------------------------------------------------------------
BOOL DrawScreen()
{
    HRESULT hr;

    // Clear screen
    EraseScreen();
    
    // Render players
    hr = DPUtil_EnumPlayers( NULL, RenderPlayerCB, NULL, 0 );
    if( FAILED(hr) )
    {
        ShowError(IDS_DPLAY_ERROR_EP);
        return FALSE;
    }

    // Render field
    for( int y=0; y<30; y++ )
    {
        for( int x=0; x<40; x++ )
        {
            BYTE mask = 1 << (x & 0x7);
            BYTE col  = x >> 3;
            if( g_Blocks.bits[y][col] & mask )
                DrawBlock( x, y );
        }
    }
    
    // Render score
    if (!DrawScore())
        return FALSE;

    // Render fragments
    DrawFragments();    

    // Render frame rate
    if( g_bShowFrameCount )
        DisplayFrameRate();

    // Show
    FlipScreen();

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: DisplayFrameRate()
// Desc: Renders current frame rate
//-----------------------------------------------------------------------------
VOID DisplayFrameRate()
{
    static DWORD dwFrames = 0;
    
    g_dwFrameCount++;

    DWORD dwTime = timeGetTime() - g_dwFrameTime;
    if( dwTime > 1000 )
    {
        dwFrames      = (g_dwFrameCount*1000)/dwTime;
        g_dwFrameTime  = timeGetTime();
        g_dwFrameCount = 0;
    }

    if( dwFrames == 0 )
        return;

    if (dwFrames != g_dwFramesLast)
        g_dwFramesLast = dwFrames;

    char strFPS[256];
    sprintf( strFPS, "%d", dwFrames );
    BltNumber( strFPS, 295, 10);
}




//-----------------------------------------------------------------------------
// Name: DrawScore()
// Desc: Renders our current score
//-----------------------------------------------------------------------------
BOOL DrawScore()
{
    SHIP    ship;
    DWORD   dwSize;
    CHAR    strScore[11];
    int     rem;
    HRESULT hr;

    dwSize = sizeof(ship);
    hr = DPUtil_GetPlayerLocalData( g_LocalPlayerDPID, &ship, &dwSize );
    if( FAILED(hr) )
        return FALSE;

    // Calculate dwScore string
    strScore[0] = (BYTE)ship.dwScore/100000 + '0';
    rem = ship.dwScore % 100000;
    strScore[1] = rem/10000 + '0';
    rem = ship.dwScore % 10000;
    strScore[2] = rem/1000 + '0';
    rem = ship.dwScore % 1000;
    strScore[3] = rem/100 + '0';
    rem = ship.dwScore % 100;
    strScore[4] = rem/10 + '0';
    rem = ship.dwScore % 10;
    strScore[5] = rem + '0';
    strScore[6] = '\0';

    // Blt score
    BltNumber( strScore, 8, 8 );

    // Save ship data
    hr = DPUtil_SetPlayerLocalData( g_LocalPlayerDPID, &ship, sizeof(ship) );
    if( FAILED(hr) )
    {
        ShowError(IDS_DPLAY_ERROR_SPLD);
        return FALSE;
    }

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: DrawBlock()
// Desc: Renders a block
//-----------------------------------------------------------------------------
VOID DrawBlock( int x, int y )
{
    RECT src;
    src.top    = 0;
    src.left   = 224;
    src.right  = src.left + 16;
    src.bottom = src.top + 16;
    BltObject( x << 4, y << 4, g_pddsNumbers, &src, DDBLTFAST_SRCCOLORKEY );
}




//-----------------------------------------------------------------------------
// Name: DrawShip()
// Desc: Renders a ship
//-----------------------------------------------------------------------------
VOID DrawShip( SHIP* pShip )
{
    RECT src;
    src.top    = 32 * (pShip->cFrame / 10 );
    src.left   = 32 * (pShip->cFrame % 10 );
    src.right  = src.left + 32;
    src.bottom = src.top + 32;
    BltObject( (int)pShip->dPosX, (int)pShip->dPosY, g_pddsShip[pShip->byType],
               &src, DDBLTFAST_SRCCOLORKEY );
}




//-----------------------------------------------------------------------------
// Name: DrawBullet()
// Desc: Renders a bullet 
//-----------------------------------------------------------------------------
VOID DrawBullet( SHIP* pShip )
{
    RECT src;
    src.top    = BULLET_Y;
    src.left   = BULLET_X + (pShip->byType)*4;
    src.right  = src.left + 3;
    src.bottom = src.top + 3;
    BltObject( (int)pShip->dBulletPosX, (int)pShip->dBulletPosY, g_pddsNumbers,
               &src, DDBLTFAST_SRCCOLORKEY );
}




//-----------------------------------------------------------------------------
// Name: DrawFragments()
// Desc: Renders the fragments
//-----------------------------------------------------------------------------
VOID DrawFragments()
{
    for( int i=0; i<64; i++)
    {
        if( g_Frags[i].valid )
        {
            BltObject( (int)g_Frags[i].dPosX, (int)g_Frags[i].dPosY,
                       g_Frags[i].pdds, &g_Frags[i].src,
                       DDBLTFAST_SRCCOLORKEY );
        }
    }
}




//-----------------------------------------------------------------------------
// Name: ReceiveGameMessages()
// Desc: Checks if there are any messages for us and receives them
//-----------------------------------------------------------------------------
HRESULT ReceiveMessages()
{
    DPID    idFrom, idTo;
    LPVOID  pvMsgBuffer;
    DWORD   dwMsgBufferSize;
    HRESULT hr;

    // read all messages in queue
    dwMsgBufferSize = g_dwReceiveBufferSize;
    pvMsgBuffer     = g_pvReceiveBuffer;
    
    while( TRUE )
    {
        // See what's out there
        idFrom = 0;
        idTo   = 0;

        hr = DPUtil_Receive( &idFrom, &idTo, DPRECEIVE_ALL, pvMsgBuffer,
                             &dwMsgBufferSize );
        if( hr == DPERR_BUFFERTOOSMALL )
        {
            if( pvMsgBuffer == NULL )
            {
                pvMsgBuffer = GlobalAllocPtr( GHND, dwMsgBufferSize );
                if( pvMsgBuffer == NULL )
                    return DPERR_NOMEMORY;
                g_pvReceiveBuffer     = pvMsgBuffer;
                g_dwReceiveBufferSize = dwMsgBufferSize;
            }
            else if( dwMsgBufferSize > g_dwReceiveBufferSize )
            {
                pvMsgBuffer = GlobalReAllocPtr( pvMsgBuffer, dwMsgBufferSize, 0 );
                if( pvMsgBuffer == NULL )
                    return DPERR_NOMEMORY;
                g_pvReceiveBuffer     = pvMsgBuffer;
                g_dwReceiveBufferSize = dwMsgBufferSize;
            }
        }
        else if( SUCCEEDED(hr) &&
                 ( (dwMsgBufferSize >= sizeof(GENERICMSG)) || 
                   (dwMsgBufferSize >= sizeof(DPMSG_GENERIC) ) ) )
        {
            if( idFrom == DPID_SYSMSG )
            {
                DoSystemMessage( (DPMSG_GENERIC*)pvMsgBuffer, dwMsgBufferSize,
                                 idFrom, idTo );
            }
            else
            {
                DoApplicationMessage( (GENERICMSG*)pvMsgBuffer, dwMsgBufferSize,
                                      idFrom, idTo );
            }
        }
        else
            break;
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: DoSystemMessage()
// Desc: Evaluates system messages and performs appropriate actions
//-----------------------------------------------------------------------------
VOID DoSystemMessage( DPMSG_GENERIC* pMsg, DWORD dwMsgSize, DPID idFrom,
                      DPID idTo )
{
    switch( pMsg->dwType )
    {
        case DPSYS_CREATEPLAYERORGROUP:
        {
            DPMSG_CREATEPLAYERORGROUP* pAddMsg = (DPMSG_CREATEPLAYERORGROUP*)pMsg;
            int iLeastAmt  = -1;
            int iLeastType = -1;
            int i;

            if( g_bHostPlayer)
            {
                g_HostMsg.Blocks = g_Blocks;
                
                //Copy the used ship types array into the message to be sent
                for( i = 0; i < NUM_SHIP_TYPES; i++ )
                    g_HostMsg.usedShipTypes[i] = g_ShipTypesUsed[i];

                SendGameMessage( (GENERICMSG*)&g_HostMsg, pAddMsg->dpId );
            }

            // Loop through the used ship types array and find the one that's been used the least
            for( i = 0; i < NUM_SHIP_TYPES; i++ )
            {
                if( (iLeastAmt < 0) || (g_ShipTypesUsed[i] < iLeastAmt) )
                {
                    iLeastAmt = g_ShipTypesUsed[i];
                    iLeastType = i;
                }
            }
            g_ShipTypesUsed[iLeastType]++; // update our copy of the used ship types
            break;
        }

        case DPSYS_DESTROYPLAYERORGROUP:
        {
            SHIP* pShip;
            DPMSG_DESTROYPLAYERORGROUP* pDestroyMsg = (DPMSG_DESTROYPLAYERORGROUP*)pMsg;

            if( (sizeof(SHIP) != pDestroyMsg->dwLocalDataSize) || 
                (NULL == pDestroyMsg->lpLocalData))
                break;

            pShip = (SHIP*)pDestroyMsg->lpLocalData;
            g_ShipTypesUsed[pShip->byType]--; // decrement the leaving players' ship type usage counter
            ReleasePlayerLocalSoundData(pShip);
            break;
        }

        case DPSYS_HOST:
        {
            g_bHostPlayer = TRUE;
            UpdateTitle();
            break;
        }

        case DPSYS_SESSIONLOST:
            // inform user that session was lost
            TRACE(_T("Session lost\n"));
            if( !g_bSessionLost )
            {
                g_bSessionLost = TRUE;
                ShowError(IDS_DPLAY_ERROR_SL);
            }
            break;

        case DPSYS_SENDCOMPLETE:
        // Async send status
        {
            DPMSG_SENDCOMPLETE* pComplete = (DPMSG_SENDCOMPLETE*)pMsg;

            if( FAILED(pComplete->hr) )
            {
                TCHAR* strErr;
                switch( pComplete->hr )
                {
                    case DPERR_CANCELLED:   strErr = _T("Cancelled"); break;
                    case DPERR_ABORTED:     strErr = _T("Aborted");   break;
                    case DPERR_TIMEOUT:     strErr = _T("Timed out"); break;
                    case DPERR_GENERIC:     strErr = _T("Generic");   break;
                    default:                strErr = _T("Unknown");
                }
                TRACE(_T("Async msg %u to %u transmission failed. err = %s %#08X\n"),
                         pComplete->dwMsgID, pComplete->idTo, strErr,
                         pComplete->hr );
            }
            break;
        }
    }
}




//-----------------------------------------------------------------------------
// Name: DoApplicationMessage()
// Desc: Evaluates an application message and performs appropriate actions
//-----------------------------------------------------------------------------
VOID DoApplicationMessage( GENERICMSG* pMsg, DWORD dwMsgSize, DPID idFrom,
                           DPID idTo )
{        
    HRESULT hr;

    switch( pMsg->byType )
    {
        case MSG_HOST:
        {
            HOSTMSG* pHostMsg = (HOSTMSG*)pMsg;
            SHIP     ship;
            DWORD    dwSize = sizeof (ship);
            int      leastUsed = -1;

            if( !g_bHostPlayer )
            {
                // receive the field layout
                g_Blocks = pHostMsg->Blocks;

                // The Host keeps the official record of ship colors in use.
                // Sync up our data and recompute our color
                hr = DPUtil_GetPlayerLocalData( g_LocalPlayerDPID, &ship, &dwSize );

                // Insure that host is not a Beta version of Duel that does not
                // send the additional ShipTypesUsed.
                if( SUCCEEDED(hr) && dwMsgSize >= sizeof(HOSTMSG) )
                {
                    // Loop through the used ship types array we were given,
                    // copying it and finding the least used type for ourself.
                    for( int i = 0; i < NUM_SHIP_TYPES; i++ )
                    {
                        g_ShipTypesUsed[i] = pHostMsg->usedShipTypes[i];
                        if( (leastUsed < 0) || (g_ShipTypesUsed[i] < leastUsed) )
                        {
                            leastUsed   = g_ShipTypesUsed[i];
                            ship.byType = i;
                        }
                    }
                    g_ShipTypesUsed[ship.byType]++; // increment it because we used one

                    hr = DPUtil_SetPlayerLocalData( g_LocalPlayerDPID, &ship, dwSize );
                }

                // Have host initializtion at this point
                g_bHaveHostInit = TRUE;
                
                // Start updating screen
                g_bIsActive = TRUE;
            }
            break;
        }

        case MSG_BLOCKHIT:
        {
            BLOCKHITMSG* pBlockHitMsg = (BLOCKHITMSG*)pMsg;
            g_Blocks.bits[pBlockHitMsg->byRow][pBlockHitMsg->byCol] &= ~pBlockHitMsg->byMask;
            break;
        }

        case MSG_ADDBLOCK:
        {
            ADDBLOCKMSG* pAddBlockMsg = (ADDBLOCKMSG*)pMsg;
            setBlock( pAddBlockMsg->byX, pAddBlockMsg->byY );
            break;
        }

        case MSG_SHIPHIT:
        {
            SHIPHITMSG* pShipHitMsg = (SHIPHITMSG*)pMsg;
            SHIP        ship;
            DWORD       dwSize;

            dwSize = sizeof(SHIP);
            // Get player local data
            hr = DPUtil_GetPlayerLocalData( pShipHitMsg->Id, &ship, &dwSize );
            if( FAILED(hr) )
                return;

            // No player data yet
            if( 0 == dwSize )
                return;

            if( !ship.bIgnore ) 
            {
                // Explode the ship on our screen
                DestroyShip( &ship );

                // Turn it off
                ship.bEnable       = FALSE;
                ship.bBulletEnable = FALSE;

                // If it is us
                if( pShipHitMsg->Id == g_LocalPlayerDPID )
                {
                    // Set our hide time-out
                    ship.iCountDown = HIDE_TIMEOUT;
                    ship.dwLastTick = timeGetTime();
                    // Let the world know that we are dead
                    g_ShipHitMsg.Id = g_LocalPlayerDPID;
                    SendGameMessage( (GENERICMSG*)&g_ShipHitMsg, DPID_ALLPLAYERS );
                }
            }
            // Ppdate player local data
            DPUtil_SetPlayerLocalData( pShipHitMsg->Id, &ship, sizeof(ship) );
            break;
        }

        case MSG_CONTROL:
        {
            CONTROLMSG* pControlMsg = (CONTROLMSG*)pMsg;
            SHIP ship;
            DWORD dwSize;

            dwSize = sizeof(SHIP);
            // Get player local data
            hr = DPUtil_GetPlayerLocalData( idFrom, &ship, &dwSize );
            if( FAILED(hr) )
                return;

            // No player data yet
            if( 0 == dwSize )
                return;

            // Update its State
            UpdateState( &ship, (DWORD)pControlMsg->byState );

            // Save it back
            DPUtil_SetPlayerLocalData( idFrom, &ship, dwSize );
            break;
        }

        case MSG_SYNC:
        {
            SYNCMSG* pSyncMsg = (SYNCMSG*)pMsg;
            SHIP     ship;
            DWORD    dwSize;

            dwSize = sizeof(SHIP);
            // Get player local data
            hr = DPUtil_GetPlayerLocalData( idFrom, &ship, &dwSize );
            if( FAILED(hr) )
                return;

            // We are seeing this player for the first time
            // so do the initialization
            if( 0 == dwSize )
            {
                ZeroMemory( &ship, sizeof(ship) );

                ship.byType     = pSyncMsg->byShipType;
                ship.dPosX      = pSyncMsg->dPosX;
                ship.dPosY      = pSyncMsg->dPosY;
                ship.cFrame     = pSyncMsg->cFrame;
                ship.dwLastTick = timeGetTime();
                ship.bEnable    = TRUE;
            }
            
            if( ship.bEnable )
            {
                // Head towards rendezvous location (accelerate/decelerate as necessary)
                ship.dVelX  = (pSyncMsg->dPosX - ship.dPosX)/1000;
                ship.dVelY  = (pSyncMsg->dPosY - ship.dPosY)/1000;
                ship.cFrame = pSyncMsg->cFrame;
            }
            else if( !ship.bIgnore )
            {
                // Ship is alive, but we just don't know it.
                // So, display it at the rendezvous location.
                ship.dPosX      = pSyncMsg->dPosX;
                ship.dPosY      = pSyncMsg->dPosY;
                ship.cFrame     = pSyncMsg->cFrame;
                ship.dwLastTick = timeGetTime();
                ship.bEnable    = TRUE;
            }

            // Save it back
            DPUtil_SetPlayerLocalData( idFrom, &ship, sizeof(ship) );
            break;
        }

        default:
        {
            TRACE( TEXT("Unknown message type %d\n"), pMsg->byType );
            break;
        }
    }
}




//-----------------------------------------------------------------------------
// Name: SendGameMessage()
// Desc: Sends a message to specified player(s)
//-----------------------------------------------------------------------------
VOID SendGameMessage( GENERICMSG* pMsg, DPID idTo )
{
    int   nBytes;
    DWORD dwFlags = 0;

    // No sends when we are not in the session
    if( g_bSessionLost )
        return;

    switch( pMsg->byType )
    {
        case MSG_HOST:
            nBytes = sizeof( HOSTMSG );
            dwFlags = DPSEND_GUARANTEED;
            break;

        case MSG_BLOCKHIT:
            nBytes = sizeof( BLOCKHITMSG );
            break;

        case MSG_SHIPHIT:
            nBytes = sizeof( SHIPHITMSG );
            break;

        case MSG_ADDBLOCK:
            nBytes = sizeof( ADDBLOCKMSG );
            break;

        case MSG_CONTROL:
            nBytes = sizeof( CONTROLMSG );
            break;

        case MSG_SYNC:
            nBytes = sizeof( SYNCMSG );
            break;

        default:
            return;
    }

    if( g_bAsync )
        dwFlags |= DPSEND_ASYNC;
    if( g_bReliable )
        dwFlags |= DPSEND_GUARANTEED;

    // Send the message to the relevant player(s)
    DPUtil_Send( g_LocalPlayerDPID, idTo, dwFlags, (VOID*)pMsg, nBytes );
}




//-----------------------------------------------------------------------------
// Name: CleanupComm()
// Desc: Cleans up communication stuff
//-----------------------------------------------------------------------------
VOID CleanupComm()
{
    HRESULT hr;

    // Free up all the local sound buffers
    ReleaseLocalData();

    // Free the receive buffer
    if( g_pvReceiveBuffer )
    {
        GlobalFreePtr(g_pvReceiveBuffer);
        g_pvReceiveBuffer = NULL;
    }

    // Delete our player
    if( g_LocalPlayerDPID ) 
    {
        hr = DPUtil_DestroyPlayer( g_LocalPlayerDPID );
        if( FAILED(hr) )
        {
            ShowError(IDS_DPLAY_ERROR_DP);
        }
        g_LocalPlayerDPID = 0;
    }

    // Cleanup DPlay objects
    hr = DPUtil_FreeDirectPlay();
    hr = DPUtil_Release();
}




//-----------------------------------------------------------------------------
// Name: InitializeGameSounds()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT InitializeGameSounds()
{
    if( FAILED( DSUtil_InitDirectSound( g_hwndMain ) ) )
        return E_FAIL;

    g_pShipEngineSound   = DSUtil_CreateSound( TEXT("SENGINE"), 1 );
    g_pBulletFiringSound = DSUtil_CreateSound( TEXT("BFIRE"),   NUM_SHIP_TYPES );
    g_pShipExplodeSound  = DSUtil_CreateSound( TEXT("SBOOM"),   NUM_SHIP_TYPES );
    g_pShipStartSound    = DSUtil_CreateSound( TEXT("SSTART"),  NUM_SHIP_TYPES );
    g_pShipStopSound     = DSUtil_CreateSound( TEXT("SSTOP"),   NUM_SHIP_TYPES );
    g_pShipBounceSound   = DSUtil_CreateSound( TEXT("SBOUNCE"), NUM_SHIP_TYPES );
    g_pBlockExplodeSound = DSUtil_CreateSound( TEXT("LBOOM"),   NUM_SHIP_TYPES );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CleanupGameSounds()
// Desc: 
//-----------------------------------------------------------------------------
VOID CleanupGameSounds()
{
    DSUtil_DestroySound( g_pBulletFiringSound );
    DSUtil_DestroySound( g_pShipExplodeSound );
    DSUtil_DestroySound( g_pShipEngineSound );
    DSUtil_DestroySound( g_pShipStartSound );
    DSUtil_DestroySound( g_pShipStopSound );
    DSUtil_DestroySound( g_pShipBounceSound );
    DSUtil_DestroySound( g_pBlockExplodeSound );
    g_pBulletFiringSound = NULL;
    g_pShipExplodeSound  = NULL;
    g_pShipEngineSound   = NULL;
    g_pShipStartSound    = NULL;
    g_pShipStopSound     = NULL;
    g_pShipBounceSound   = NULL;
    g_pBlockExplodeSound = NULL;

    DSUtil_FreeDirectSound();
}



