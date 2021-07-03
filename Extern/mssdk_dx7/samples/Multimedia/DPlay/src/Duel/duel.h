//-----------------------------------------------------------------------------
// File: Duel.h
//
// Desc: Duel include file
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#ifndef DUEL_INCLUDED
#define DUEL_INCLUDED

// bcc32 does not support nameless unions in C mode
#if defined(__BORLANDC__) && !defined(__cplusplus)
#define NONAMELESSUNION
#endif

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#include <windowsx.h>
#include <mmsystem.h>
#include "resource.h"
#include <wtypes.h>
#include <tchar.h>




//-----------------------------------------------------------------------------
// Application constants
//-----------------------------------------------------------------------------

// Dialog exit codes
#define EXITCODE_FORWARD      1  // Dialog success, so go forward
#define EXITCODE_BACKUP       2  // Dialog canceled, show previous dialog
#define EXITCODE_QUIT         3  // Dialog quit, close app
#define EXITCODE_ERROR        4  // Dialog error, terminate app
#define EXITCODE_LOBBYCONNECT 5  // Dialog connected from lobby, connect success

// User messages
#define UM_LAUNCH       WM_USER
#define UM_ABORT        WM_USER+1
#define UM_RESTARTTIMER WM_USER+2

// program states
enum{ PS_SPLASH, PS_ACTIVE, PS_REST };

#define MAX_SCREEN_X      639
#define MAX_SCREEN_Y      479
#define MAX_PLAYER_NAME    14
#define MAX_SESSION_NAME  256
#define MAX_SPNAME         50
#define MAX_CLASSNAME      50
#define MAX_WINDOWTITLE    50
#define MAX_ERRORMSG      256
#define MAX_FONTNAME       50
#define MAX_HELPMSG       512

#define RECEIVE_TIMER_ID    1
#define RECEIVE_TIMEOUT  1000    // in milliseconds

#define ENUM_TIMER_ID       2
#define ENUM_TIMEOUT     2000    // in milliseconds

// Default window size
#define MAX_DEFWIN_X      640
#define MAX_DEFWIN_Y      480


// Tree view image info
#define CX_BITMAP        25
#define CY_BITMAP        25
#define NUM_BITMAPS      2

// registry info
#define DUEL_KEY (TEXT("Software\\Microsoft\\Duel"))




//-----------------------------------------------------------------------------
// Prototypes
//-----------------------------------------------------------------------------
VOID ShowError( int err );
VOID UpdateTitle();

// Functions defined in util.c
int     randInt( int low, int high );
double  randDouble( double low, double high );
#define TRACE   dtrace 
void dtrace( TCHAR* strFormat, ...);

#endif




