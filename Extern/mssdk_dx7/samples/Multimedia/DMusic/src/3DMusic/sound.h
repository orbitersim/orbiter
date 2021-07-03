/*==========================================================================
 *
 *  Copyright (C) 1997-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File:       sound.h
 *  Content:    DirectSound routines include file
 *
 ***************************************************************************/


#include <mmsystem.h>
#include <dsound.h>

#define WINWD 300
#define WINHT 300

typedef struct
{
    LPDIRECTSOUNDBUFFER     lpdsb;
    LPDIRECTSOUND3DBUFFER   lpds3db;
} _3DSOUND;


void Drip( void );
void MoveSound( int, int );
void TurnListener( D3DVALUE lx, D3DVALUE ly, D3DVALUE lz );
BOOL Init3DMusic( HWND hwnd, HINSTANCE hinst );
void Cleanup3DMusic( void );


