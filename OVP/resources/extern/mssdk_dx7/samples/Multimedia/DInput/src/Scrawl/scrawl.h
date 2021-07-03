//-----------------------------------------------------------------------------
// File: Scrawl.h
//
// Desc: Header file for for DirectInput sample
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef SCRAWL_H
#define SCRAWL_H

#include <windows.h>
#include <dinput.h>
#include "resource.h"

#define IDC_CLEAR               64
#define IDC_ABOUT               65


typedef struct LEFTBUTTONINFO 
{
    HDC hdcWindow;
    BOOL bMoved;
    DWORD dwSeqLastSeen;

} LEFTBUTTONINFO, *PLEFTBUTTONINFO;


//-----------------------------------------------------------------------------
// External function-prototypes
//-----------------------------------------------------------------------------
extern HRESULT InitDirectInput( HWND hWnd );
extern HRESULT SetAcquire();
extern HRESULT FreeDirectInput();
extern void OnMouseInput( HWND hWnd );
extern void InvalidateCursorRect(HWND hWnd);
extern void UpdateCursorPosition( int dx, int dy );

extern void StartPenDraw( HWND hWnd, LEFTBUTTONINFO* plbInfo );
extern void FinishPenDraw( HANDLE hWnd );

extern void OnLeftButtonDown_FlushMotion( LEFTBUTTONINFO* plbInfo );
extern void OnRightButtonUp( HWND hWnd );


//-----------------------------------------------------------------------------
// External varibles
//-----------------------------------------------------------------------------
extern HINSTANCE g_hInst;	
extern BOOL g_bActive;
extern HANDLE g_hMouseEvent;


#endif // !defined(SCRAWL_H)
