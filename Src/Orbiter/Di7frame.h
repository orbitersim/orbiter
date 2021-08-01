// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ====================================================================================
// File: Di7frame.h
// Desc: Class to manage the DirectInput environment objects
// ====================================================================================

#ifndef DI7FRAME_H
#define DI7FRAME_H
#define STRICT 1
#include <windows.h>
#include <dinput.h>
#include <d3d.h>

//-----------------------------------------------------------------------------
// Name: CDIFramework7
// Desc: The DirectInput framework class for DX7. Maintains the DI devices
//-----------------------------------------------------------------------------
class CDIFramework7
{
	LPDIRECTINPUT8       m_pDI;             // DInput object
	LPDIRECTINPUTDEVICE8 m_pdidKbdDevice;   // keyboard device
	LPDIRECTINPUTDEVICE8 m_pdidMouseDevice; // mouse device
	LPDIRECTINPUTDEVICE8 m_pdidJoyDevice;   // joystick device
	GUID                 m_guidJoystick;    // GUID for the joystick
	BOOL                 m_bUseKbd;
	BOOL                 m_bUseJoy;

	struct JLIST {
		DIDEVICEINSTANCE*    descJoy;     // list of enumerated joystick devices
		DWORD                nJoy;        // number of enumerated joysticks
	} jList;

	static BOOL CALLBACK EnumJoysticksCallback (LPCDIDEVICEINSTANCE pInst,
		VOID* pvContext);

public:
	CDIFramework7();
	~CDIFramework7();

	HRESULT Create (HINSTANCE hInst);
	// Initialize the DirectInput objects

	VOID Destroy ();
	// Destroys devices and DI object

	VOID GetJoysticks (DIDEVICEINSTANCE **dev, DWORD *pdwCount);
	// Returns the list of enumerated joysticks

	DWORD NumJoysticks () const { return jList.nJoy; }
	// number of enumerated joysticks

	HRESULT CreateDevice (HWND hWnd, LPDIRECTINPUT8 pDI,
		LPDIRECTINPUTDEVICE8 pDIDevice, GUID guidDevice, const DIDATAFORMAT *pdidDataFormat,
		DWORD dwFlags);

	HRESULT CreateKbdDevice (HWND hWnd);
	HRESULT CreateMouseDevice (HWND hWnd);
	HRESULT CreateJoyDevice (HWND hWnd, DWORD idx = 0);

	VOID DestroyDevices();

	// accessor functions
	inline LPDIRECTINPUTDEVICE8 GetKbdDevice() { return m_pdidKbdDevice; }
	inline LPDIRECTINPUTDEVICE8 GetMouseDevice() { return m_pdidMouseDevice; }
	inline LPDIRECTINPUTDEVICE8 GetJoyDevice() { return m_pdidJoyDevice; }
};

#endif // !DI7FRAME_H