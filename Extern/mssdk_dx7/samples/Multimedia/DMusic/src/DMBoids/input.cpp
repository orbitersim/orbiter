/*
**----------------------------------------------------------------------------
**
**  File:       input.cpp
**  Purpose:    Direct Input Symbols and Stuff
**  Notes:
**
**	Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
**----------------------------------------------------------------------------
*/

#include "Common.h"
#include "Debug.h"
#include "WinMain.h"
#include "WinProc.h"
#include "DrvMgr.h"
#include "D3DWin.h"
#include "D3DScene.h"
#include "DInput.h"
#include "input.h"
#include "music.h"

extern BoidMusic	g_Music;

LPDIRECTINPUT		lpDI; 
LPDIRECTINPUTDEVICE	lpDIDeviceKeyboard; 
LPDIRECTINPUTDEVICE	lpDIDeviceMouse; 
LPDIRECTINPUTDEVICE	lpDIDeviceJoystick; 
HANDLE				hevtMouse;

Keys	keys;

#define DINPUT_BUFFERSIZE  16

// buffer for the mouse data
DIPROPDWORD dipdw =
	{
		// the header
		{
			sizeof(DIPROPDWORD),        // diph.dwSize
			sizeof(DIPROPHEADER),       // diph.dwHeaderSize
			0,                          // diph.dwObj
			DIPH_DEVICE,                // diph.dwHow
		},
		// the data
		DINPUT_BUFFERSIZE,              // dwData
	};



BOOL WINAPI DI_Init(void) 
{ 
    HRESULT hr; 
 
    // Create the DirectInput object. 
    hr = DirectInputCreate(g_hMainInstance, DIRECTINPUT_VERSION, &lpDI, NULL); 
    if FAILED(hr) 
		return FALSE; 
 
	// set up the keyboard

    // Retrieve a pointer to the IDirectInputDevice interface 
    hr = lpDI->CreateDevice(GUID_SysKeyboard, &lpDIDeviceKeyboard, NULL); 
    if FAILED(hr) { 
        DI_Term(); 
        return FALSE; 
    } 

    // Set the data format using the predefined keyboard data 
    // format provided by the DirectInput object for keyboards. 
    hr = lpDIDeviceKeyboard->SetDataFormat(&c_dfDIKeyboard); 
    if FAILED(hr) { 
        DI_Term(); 
        return FALSE; 
    } 
 
    // Set the cooperative level 
    hr = lpDIDeviceKeyboard->SetCooperativeLevel(g_hMainWindow, 
                       DISCL_FOREGROUND | DISCL_NONEXCLUSIVE); 
    if FAILED(hr) { 
        DI_Term(); 
        return FALSE; 
    } 
 
    // Get access to the input device. 
    hr = lpDIDeviceKeyboard->Acquire(); 
    if FAILED(hr) { 
        DI_Term(); 
        return FALSE; 
    } 

	// now, set up the mouse

    // Retrieve a pointer to the IDirectInputDevice interface 
    hr = lpDI->CreateDevice(GUID_SysMouse, &lpDIDeviceMouse, NULL); 
    if FAILED(hr) { 
        DI_Term(); 
        return FALSE; 
    } 

    // Set the data format using the predefined mouse data 
    // format provided by the DirectInput object for mice. 
    hr = lpDIDeviceMouse->SetDataFormat(&c_dfDIMouse); 
    if FAILED(hr) { 
        DI_Term(); 
        return FALSE; 
    } 
 
    // Set the cooperative level 
    hr = lpDIDeviceMouse->SetCooperativeLevel(g_hMainWindow, 
                       DISCL_FOREGROUND | DISCL_EXCLUSIVE); 
    if FAILED(hr) { 
		DI_Term(); 
        return FALSE; 
    } 

	hevtMouse = CreateEvent(0, 0, 0, 0);

	if (hevtMouse == NULL) {
		DI_Term(); 
		return FALSE;
	}

	hr = lpDIDeviceMouse->SetEventNotification(hevtMouse);

	if (FAILED(hr)) {
		DI_Term(); 
		return FALSE;
	}

	// set buffer size
	hr = lpDIDeviceMouse->SetProperty(DIPROP_BUFFERSIZE, &dipdw.diph);
	if (FAILED(hr)) {
		DI_Term();
		return FALSE;
	}
 
    // Get access to the input device. 
    hr = lpDIDeviceMouse->Acquire(); 
    if FAILED(hr) { 
        DI_Term(); 
        return FALSE; 
    } 

    return TRUE; 
}	// end of DI_Init()
 
void WINAPI DI_Term(void) 
{ 
    if (lpDI) { 
        if (lpDIDeviceKeyboard) { 
            /* 
             *  Always unacquire the device before calling Release(). 
             */ 
             lpDIDeviceKeyboard->Unacquire(); 
             lpDIDeviceKeyboard->Release();
             lpDIDeviceKeyboard = NULL; 
        } 
        if (lpDIDeviceMouse) { 
            /* 
             *  Always unacquire the device before calling Release(). 
             */ 
             lpDIDeviceMouse->Unacquire(); 
             lpDIDeviceMouse->Release();
             lpDIDeviceMouse = NULL; 
        } 
        lpDI->Release();
        lpDI = NULL; 
    } 
}	// end of DI_Term()

void WINAPI ProcessKBInput(void) 
{ 
	#define KEYDOWN(name,key) (name[key] & 0x80) 

	char     buffer[256]; 
	HRESULT  hr; 

	if (lpDIDeviceKeyboard)
	{
		hr = lpDIDeviceKeyboard->GetDeviceState(sizeof(buffer),(LPVOID)&buffer); 
		if FAILED(hr) { 
			// If it failed, the device has probably been lost. 
			// We should check for (hr == DI_INPUTLOST) 
			// and attempt to reacquire it here. 
			if (hr == DIERR_INPUTLOST || hr == DIERR_NOTACQUIRED) {
				hr = lpDIDeviceKeyboard->Acquire(); 
				if FAILED(hr) { 
					return; 
				} 
			}
		} 
	}

	
	if (keys.separation != (KEYDOWN(buffer, DIK_S) || KEYDOWN(buffer, DIK_SPACE)))
	{
		keys.separation = (KEYDOWN(buffer, DIK_S) || KEYDOWN(buffer, DIK_SPACE));
		if (keys.separation)
		{
			g_Music.Collapse();
		}
		else
		{
			g_Music.Expand();
		}
	}
	keys.alignment = KEYDOWN(buffer, DIK_A);
	keys.cohesion = KEYDOWN(buffer, DIK_C);
	if (keys.migratory != KEYDOWN(buffer, DIK_M))
	{
		keys.migratory = KEYDOWN(buffer, DIK_M);
		if (keys.migratory)
		{
			g_Music.Migrate();
		}
	}
	keys.obstacle = KEYDOWN(buffer, DIK_O);
} 
