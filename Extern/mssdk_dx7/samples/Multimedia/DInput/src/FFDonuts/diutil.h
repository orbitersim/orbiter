//-----------------------------------------------------------------------------
// File: diutil.h
//
// Desc: DirectInput support
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#ifndef DIUTIL_H
#define DIUTIL_H

#include <dinput.h>




// Prototypes
HRESULT DIUtil_Initialize( HWND hWnd );
HRESULT DIUtil_GetDevices( DIDEVICEINSTANCE** ppDevice, DWORD* pdwCount );
LPDIRECTINPUTDEVICE2 DIUtil_CreateDevice( HWND, DIDEVICEINSTANCE* pdidi );

BOOL    DIUtil_IsForceFeedback( LPDIRECTINPUTDEVICE2 );
HRESULT DIUtil_CreateEffects();

VOID    DIUtil_CleanupDirectInput();



struct FFEffectObject
{
	LPDIRECTINPUTEFFECT pDIEffect;

	DWORD               dwEffectType;
	DIEFFECT            diEffect;
	DIENVELOPE          diEnvelope;
	union 
	{
	    DICUSTOMFORCE   dicustomf;
		DICONSTANTFORCE dicf;
		DIPERIODIC      dipf;
	};
};



FFEffectObject* DIUtil_CreateEffect();

HRESULT DIUtil_SetupCustomEffect( FFEffectObject*, LPDIRECTINPUTDEVICE2 );
HRESULT DIUtil_SetupPeriodicEffect( FFEffectObject*, LPDIRECTINPUTDEVICE2 );
HRESULT DIUtil_SetupConstantForceEffect( FFEffectObject*,
										 LPDIRECTINPUTDEVICE2, BOOL );
HRESULT DIUtil_PlayDirectionalEffect( FFEffectObject*, LONG );
HRESULT DIUtil_PlayEffect( FFEffectObject* );

VOID    DIUtil_DeleteEffect( FFEffectObject* );




// Constants used for scaling the input device
#define DEADZONE         2500         // 25% of the axis range
#define RANGE_MAX        1000         // Maximum positive axis value
#define RANGE_MIN       -1000         // Minimum negative axis value




#endif //DIUTIL_H


