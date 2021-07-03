//-----------------------------------------------------------------------------
// File: KeybdBuf.h
//
// Desc: Header file for for DirectInput sample
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef KEYBD_BUF_H
#define KEYBD_BUF_H

#define STRICT
#include <windows.h>
#include <dinput.h>
#include "resource.h"

//-----------------------------------------------------------------------------
// External function-prototypes
//-----------------------------------------------------------------------------
extern HRESULT InitDirectInput( HWND hDlg );
extern HRESULT SetAcquire( HWND hDlg );
extern HRESULT FreeDirectInput();
extern HRESULT UpdateInputState( HWND hDlg );

extern BOOL g_bActive;		
extern HINSTANCE g_hInst;		


#endif // !defined(KEYBD_BUF_H)
