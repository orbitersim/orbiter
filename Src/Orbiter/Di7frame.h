// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ====================================================================================
// File: Di7frame.h
// Desc: Class to manage the DirectInput environment objects
// ====================================================================================

#ifndef DI7FRAME_H
#define DI7FRAME_H
#define STRICT 1

#include <cstdint>
// #include <windows.h>
// #include <dinput.h>
// #include <d3d.h>

struct LPDIRECTINPUT8 {};
struct LPDIRECTINPUTDEVICE8 {};
struct GUID {};
typedef bool BOOL;
struct DIDEVICEINSTANCE{};
struct LPCDIDEVICEINSTANCE{};
struct DIDATAFORMAT{};

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
	bool                 m_bUseKbd;
	bool                 m_bUseJoy;

	struct JLIST {
		DIDEVICEINSTANCE*    descJoy;     // list of enumerated joystick devices
		uint32_t                nJoy;        // number of enumerated joysticks
	} jList;

	static BOOL EnumJoysticksCallback (LPCDIDEVICEINSTANCE pInst,
		void* pvContext);

public:
	CDIFramework7();
	~CDIFramework7();

	uint32_t Create ();
	// Initialize the DirectInput objects

	void Destroy ();
	// Destroys devices and DI object

	void GetJoysticks (DIDEVICEINSTANCE **dev, uint32_t *pdwCount);
	// Returns the list of enumerated joysticks

	uint32_t NumJoysticks () const { return jList.nJoy; }
	// number of enumerated joysticks

	uint32_t CreateDevice (LPDIRECTINPUT8 pDI,
		LPDIRECTINPUTDEVICE8 pDIDevice, GUID guidDevice, const DIDATAFORMAT *pdidDataFormat,
		uint32_t dwFlags);

	uint32_t CreateKbdDevice ();
	uint32_t CreateMouseDevice ();
	uint32_t CreateJoyDevice (uint32_t idx = 0);

	void DestroyJoyDevice();
	void DestroyDevices();

	// accessor functions
	inline LPDIRECTINPUTDEVICE8 GetKbdDevice() { return m_pdidKbdDevice; }
	inline LPDIRECTINPUTDEVICE8 GetMouseDevice() { return m_pdidMouseDevice; }
	inline LPDIRECTINPUTDEVICE8 GetJoyDevice() { return m_pdidJoyDevice; }
};

#endif // !DI7FRAME_H