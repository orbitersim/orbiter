// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ====================================================================================
// File: Di7frame.h
// Desc: Class to manage the DirectInput environment objects
// ====================================================================================

#ifndef DI7FRAME_H
#define DI7FRAME_H
#define STRICT 1
#ifndef __linux__
#include <windows.h>
#include <dinput.h>
#include <d3d.h>
#else // __linux__
// DirectInput 8 counterpart: keyboard state fed from the render window's key events, joysticks read from evdev
#include "OrbiterPlatform.h"
#include <vector>

// DIDEVICEINSTANCE counterpart: one enumerated evdev joystick
struct JoyDeviceInstance {
	char tszInstanceName[260]; // evdev device name
	char tszProductName[260];  // evdev device name
	char path[64];             // /dev/input/eventN
};

// DIJOYSTATE2 counterpart (the fields Orbiter reads); axes are scaled to the range set with SetAxisRange
struct JoyState {
	LONG  lX, lY, lZ;          // x, y, z axis
	LONG  lRx, lRy, lRz;       // x, y, z rotation
	LONG  rglSlider[2];        // sliders: ABS_THROTTLE, ABS_RUDDER
	DWORD rgdwPOV[4];          // hats in 1/100 deg clockwise from north, 0xFFFFFFFF when centred
	BYTE  rgbButtons[128];     // 0x80 = pressed
};

// DIDEVICEOBJECTDATA counterpart: one buffered key transition
struct KeyData {
	DWORD dwOfs;               // DIK scan code (OAPI_KEY_*)
	DWORD dwData;              // 0x80 = pressed
	DWORD dwTimeStamp;         // ms
	DWORD dwSequence;
};

// return codes of the device methods (DI_OK / DIERR_NOTACQUIRED counterparts)
#define DI_OK               0
#define DIERR_NOTACQUIRED  -1
#define DIERR_INPUTLOST    -2

// DirectInput keyboard device counterpart; the render window feeds it with KeyEvent
class KeyboardDevice {
public:
	KeyboardDevice (DWORD bufsize);
	void KeyEvent (int evcode, bool down);       // evdev key code (Qt nativeScanCode()-8 on X11 and Wayland)
	int  Acquire ();
	void Unacquire ();                           // focus lost: all keys released, like DISCL_FOREGROUND
	int  GetDeviceState (DWORD size, void *state);
	int  GetDeviceData (DWORD size, KeyData *dod, DWORD *n, DWORD flags);
	static DWORD DIKCode (int evcode);           // evdev key code -> DIK scan code (0 if none)
private:
	bool acquired;
	char kstate[256];
	std::vector<KeyData> buf;
	DWORD bufsize, seq;
};

// DirectInput joystick device counterpart on an evdev node
class JoystickDevice {
public:
	JoystickDevice (const char *path);
	~JoystickDevice ();
	bool Valid () const { return fd >= 0; }
	int  Acquire ();
	void Unacquire ();
	int  Poll ();
	int  GetDeviceState (DWORD size, JoyState *js);
	enum Axis { AX_X, AX_Y, AX_Z, AX_RX, AX_RY, AX_RZ, AX_SLIDER0, AX_SLIDER1, NAXIS };
	bool SetAxisRange (Axis a, LONG lMin, LONG lMax);  // DIPROP_RANGE
	bool SetAxisDeadzone (Axis a, DWORD dz);           // DIPROP_DEADZONE (0-10000)
	bool SetAxisSaturation (Axis a, DWORD sat);        // DIPROP_SATURATION (0-10000)
private:
	int  ReadAll ();                             // returns errno of a failed read (0 if the queue just ran dry)
	LONG Scale (int a) const;
	int fd;
	bool acquired;
	struct AxisPrm { bool present; int code, amin, amax, raw; LONG lMin, lMax; DWORD dz, sat; } axis[NAXIS];
	int hat[4][2];
	std::vector<int> btncode;                    // evdev button codes in HID order
	BYTE btn[128];
};
#endif // __linux__

//-----------------------------------------------------------------------------
// Name: CDIFramework7
// Desc: The DirectInput framework class for DX7. Maintains the DI devices
//-----------------------------------------------------------------------------
class CDIFramework7
{
#ifndef __linux__
	LPDIRECTINPUT8       m_pDI;             // DInput object
	LPDIRECTINPUTDEVICE8 m_pdidKbdDevice;   // keyboard device
	LPDIRECTINPUTDEVICE8 m_pdidMouseDevice; // mouse device
	LPDIRECTINPUTDEVICE8 m_pdidJoyDevice;   // joystick device
	GUID                 m_guidJoystick;    // GUID for the joystick
#else // __linux__
	// LPDIRECTINPUT8 m_pDI left out: evdev needs no input system object
	KeyboardDevice*      m_pdidKbdDevice;   // keyboard device
	// m_pdidMouseDevice left out: mouse input arrives as window events
	JoystickDevice*      m_pdidJoyDevice;   // joystick device
	// m_guidJoystick left out: devices are picked by index into jList
#endif // __linux__
	BOOL                 m_bUseKbd;
	BOOL                 m_bUseJoy;

	struct JLIST {
#ifndef __linux__
		DIDEVICEINSTANCE*    descJoy;     // list of enumerated joystick devices
#else // __linux__
		JoyDeviceInstance*   descJoy;     // list of enumerated joystick devices
#endif // __linux__
		DWORD                nJoy;        // number of enumerated joysticks
	} jList;

#ifndef __linux__
	static BOOL CALLBACK EnumJoysticksCallback (LPCDIDEVICEINSTANCE pInst,
		VOID* pvContext);
#else // __linux__
	static bool EnumJoysticksCallback (const JoyDeviceInstance *pInst,
		void* pvContext);
#endif // __linux__

public:
	CDIFramework7();
	~CDIFramework7();

#ifndef __linux__
	HRESULT Create (HINSTANCE hInst);
#else // __linux__
	int Create (void *hInst);
#endif // __linux__
	// Initialize the DirectInput objects

#ifndef __linux__
	VOID Destroy ();
#else // __linux__
	void Destroy ();
#endif // __linux__
	// Destroys devices and DI object

#ifndef __linux__
	VOID GetJoysticks (DIDEVICEINSTANCE **dev, DWORD *pdwCount);
#else // __linux__
	void GetJoysticks (JoyDeviceInstance **dev, DWORD *pdwCount);
#endif // __linux__
	// Returns the list of enumerated joysticks

	DWORD NumJoysticks () const { return jList.nJoy; }
	// number of enumerated joysticks

#ifndef __linux__
	HRESULT CreateDevice (HWND hWnd, LPDIRECTINPUT8 pDI,
		LPDIRECTINPUTDEVICE8 pDIDevice, GUID guidDevice, const DIDATAFORMAT *pdidDataFormat,
		DWORD dwFlags);

	HRESULT CreateKbdDevice (HWND hWnd);
	HRESULT CreateMouseDevice (HWND hWnd);
	HRESULT CreateJoyDevice (HWND hWnd, DWORD idx = 0);
#else // __linux__
	// generic CreateDevice left out: each device type is created by its own function

	int CreateKbdDevice (QWindow *hWnd);
	// CreateMouseDevice left out: unused upstream; the mouse arrives as window events
	int CreateJoyDevice (QWindow *hWnd, DWORD idx = 0);
#endif // __linux__

	void DestroyJoyDevice();
	void DestroyDevices();

	// accessor functions
#ifndef __linux__
	inline LPDIRECTINPUTDEVICE8 GetKbdDevice() { return m_pdidKbdDevice; }
	inline LPDIRECTINPUTDEVICE8 GetMouseDevice() { return m_pdidMouseDevice; }
	inline LPDIRECTINPUTDEVICE8 GetJoyDevice() { return m_pdidJoyDevice; }
#else // __linux__
	inline KeyboardDevice *GetKbdDevice() { return m_pdidKbdDevice; }
	inline JoystickDevice *GetJoyDevice() { return m_pdidJoyDevice; }
#endif // __linux__
};

#endif // !DI7FRAME_H
