//-----------------------------------------------------------------------------
// File: JoyFFeed.h
//
// Desc: Header file for for DirectInput sample
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#ifndef JOY_FFEED_H
#define JOY_FFEED_H

#define STRICT
#include <windows.h>
#include <mmsystem.h>
#include <dinput.h>
#include "resource.h"




//-----------------------------------------------------------------------------
// External function-prototypes
//-----------------------------------------------------------------------------
extern HRESULT InitDirectInput( HWND hDlg );
extern HRESULT SetAcquire( HWND hDlg );
extern HRESULT FreeDirectInput();
extern HRESULT SetJoyForcesXY();




//-----------------------------------------------------------------------------
// External global variables
//-----------------------------------------------------------------------------
extern BOOL g_bActive;		
extern HINSTANCE g_hInst;
extern int g_nXForce;
extern int g_nYForce;




#endif



