//-----------------------------------------------------------------------------
// File: DIUtil.h
//
// Desc: Input routines
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#ifndef DIUTIL_H
#define DIUTIL_H

#include <dinput.h>


HRESULT DIUtil_InitInput( HWND hWnd );
VOID    DIUtil_ReadKeys( DWORD* pdwKey );
VOID    DIUtil_CleanupInput();
HRESULT DIUtil_ReacquireInputDevices();


#endif



