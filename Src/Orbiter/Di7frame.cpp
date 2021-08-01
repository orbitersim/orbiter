// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ====================================================================================
// File: Di7frame.cpp
// Desc: Class to manage the DirectInput environment objects
// ====================================================================================

#include "Di7frame.h"
#include "D3d7util.h"
#include "Log.h"

//-----------------------------------------------------------------------------
// Name: CDIFramework7()
// Desc: Constructor
//-----------------------------------------------------------------------------
CDIFramework7::CDIFramework7 ()
{
	m_pDI             = NULL;
	m_pdidKbdDevice   = NULL;
	m_pdidMouseDevice = NULL;
	m_pdidJoyDevice   = NULL;
	jList.nJoy        = 0;
}

//-----------------------------------------------------------------------------
// Name: ~CDIFramework7()
// Desc: Destructor
//-----------------------------------------------------------------------------
CDIFramework7::~CDIFramework7 ()
{
	Destroy ();
}

//-----------------------------------------------------------------------------
// Name: Create()
// Desc: Initialises the DirectInput objects
//-----------------------------------------------------------------------------
HRESULT CDIFramework7::Create (HINSTANCE hInst)
{
	HRESULT hr;

	// Create the main DirectInput object
	if (FAILED (hr = DirectInput8Create (hInst, DIRECTINPUT_VERSION,
		IID_IDirectInput8, (LPVOID*)&m_pDI, NULL))) {
		LOGOUT("ERROR: DI: DirectInputCreate failed");
		LOGOUT_DIERR(hr);
		return hr;
	}

	// Check to see whether a joystick is present. If one is, the enumeration
	// callback will save the joystick's GUID, so we can create it later
	ZeroMemory (&m_guidJoystick, sizeof (GUID));
	m_pDI->EnumDevices (DI8DEVTYPE_JOYSTICK, EnumJoysticksCallback,
		(LPVOID)&jList, DIEDFL_ATTACHEDONLY);

	// pick first joystick by default
	if (jList.nJoy)
		memcpy (&m_guidJoystick, &jList.descJoy[0].guidInstance, sizeof (GUID));

	LOGOUT("Found %d joystick(s)", jList.nJoy);
	return S_OK;
}

//-----------------------------------------------------------------------------
// Name: Destroy()
// Desc: Deletes devices and DI object
//-----------------------------------------------------------------------------
VOID CDIFramework7::Destroy ()
{
	DestroyDevices();
	SAFE_RELEASE (m_pDI);
}

//-----------------------------------------------------------------------------
// Name: EnumJoysticksCallback()
// Desc: Called once for each enumerated joystick. If we find one, create a device
//       interface on it so that we can play with it
//-----------------------------------------------------------------------------
BOOL CALLBACK CDIFramework7::EnumJoysticksCallback (LPCDIDEVICEINSTANCE pInst, VOID* pvContext)
{
	// Check here whether the enumerated device is appropriate
	struct JLIST *jlist = (struct JLIST*)pvContext;
	DIDEVICEINSTANCE *tmp = new DIDEVICEINSTANCE[jlist->nJoy+1]; TRACENEW
	if (jlist->nJoy) {
		memcpy (tmp, jlist->descJoy, jlist->nJoy*sizeof(DIDEVICEINSTANCE));
		delete []jlist->descJoy;
	}
	jlist->descJoy = tmp;
	memcpy (jlist->descJoy + jlist->nJoy++, pInst, sizeof(DIDEVICEINSTANCE));
	return DIENUM_CONTINUE;
}

//-----------------------------------------------------------------------------
// Name: GetJoysticks()
// Desc: Returns the list of enumerated joysticks
//-----------------------------------------------------------------------------
VOID CDIFramework7::GetJoysticks (DIDEVICEINSTANCE **dev, DWORD *pdwCount)
{
	*dev = jList.descJoy;
	*pdwCount = jList.nJoy;
}

//-----------------------------------------------------------------------------
// Name: CreateDevice()
// Desc: Creates a DirectInput device
//-----------------------------------------------------------------------------
HRESULT CDIFramework7::CreateDevice (HWND hWnd, LPDIRECTINPUT8 pDI,
	LPDIRECTINPUTDEVICE8 pDIDevice, GUID guidDevice, const DIDATAFORMAT *pdidDataFormat,
	DWORD dwFlags)
{
	// Obtain an interface to the input device
	if (FAILED (pDI->CreateDevice (guidDevice, &pDIDevice, NULL))) {
		LOGOUT("ERROR: DI: CreateDeviceEx failed");
		return E_FAIL;
	}

	// Set the device data format. A data format specifies which controls on a
	// device you are interested in and indicates how they should be reported.
	if (FAILED (pDIDevice->SetDataFormat (pdidDataFormat))) {
		LOGOUT("ERROR: DI: SetDataFormat failed");
		return E_FAIL;
	}

	// Set the cooperative level to let DirectInput know how this device should
	// interact with the system and with other DirectInput applications
	if (FAILED (pDIDevice->SetCooperativeLevel (hWnd, dwFlags))) {
		LOGOUT("ERROR: DI: SetCooperativeLevel failed");
		return E_FAIL;
	}

	if (guidDevice == GUID_SysKeyboard)
		m_pdidKbdDevice = pDIDevice;
	else if (guidDevice == GUID_SysMouse)
		m_pdidMouseDevice = pDIDevice;
	else
		m_pdidJoyDevice = pDIDevice;

	return S_OK;
}

//-----------------------------------------------------------------------------
// Name: CreateKbdDevice()
// Desc: Creates a DirectInput device for the keyboard
//-----------------------------------------------------------------------------
HRESULT CDIFramework7::CreateKbdDevice (HWND hWnd)
{
	HRESULT hr;
	if (FAILED (hr = CreateDevice (hWnd, m_pDI, m_pdidKbdDevice, GUID_SysKeyboard,
		&c_dfDIKeyboard, DISCL_NONEXCLUSIVE | DISCL_FOREGROUND))) return hr;

	// set buffer size for storage of buffered key events
	DIPROPDWORD dipdw;
	dipdw.diph.dwSize = sizeof(DIPROPDWORD);
	dipdw.diph.dwHeaderSize = sizeof(DIPROPHEADER);
	dipdw.diph.dwObj = 0;
	dipdw.diph.dwHow = DIPH_DEVICE;
	dipdw.dwData = 10;
	return m_pdidKbdDevice->SetProperty (DIPROP_BUFFERSIZE, &dipdw.diph);
}

//-----------------------------------------------------------------------------
// Name: CreateMouseDevice()
// Desc: Creates a DirectInput device for the mouse
//-----------------------------------------------------------------------------
HRESULT CDIFramework7::CreateMouseDevice (HWND hWnd)
{
	HRESULT hr;
	if (FAILED (hr = CreateDevice (hWnd, m_pDI, m_pdidMouseDevice, GUID_SysMouse,
		&c_dfDIMouse, DISCL_NONEXCLUSIVE | DISCL_FOREGROUND))) return hr;

	// switch mouse to absolute mode
	DIPROPDWORD diprw;
	diprw.diph.dwSize       = sizeof (DIPROPDWORD);
	diprw.diph.dwHeaderSize = sizeof (DIPROPHEADER);
	diprw.diph.dwObj        = 0;
	diprw.diph.dwHow        = DIPH_DEVICE;
	diprw.dwData            = DIPROPAXISMODE_ABS;
	return m_pdidMouseDevice->SetProperty (DIPROP_AXISMODE, &diprw.diph);
}

//-----------------------------------------------------------------------------
// Name: CreateJoyDevice()
// Desc: Creates a DirectInput device for a joystick
//-----------------------------------------------------------------------------
HRESULT CDIFramework7::CreateJoyDevice (HWND hWnd, DWORD idx)
{
	if (idx >= jList.nJoy) return E_FAIL;
	return CreateDevice (hWnd, m_pDI, m_pdidJoyDevice, jList.descJoy[idx].guidInstance,
		&c_dfDIJoystick2, DISCL_EXCLUSIVE | DISCL_FOREGROUND);
}

//-----------------------------------------------------------------------------
// Name: DestroyDevices()
// Desc: Releases the DirectInput devices
//-----------------------------------------------------------------------------
VOID CDIFramework7::DestroyDevices()
{
	if (m_pdidKbdDevice) {
		m_pdidKbdDevice->Unacquire ();
		m_pdidKbdDevice->Release ();
		m_pdidKbdDevice = NULL;
	}
	if (m_pdidMouseDevice) {
		m_pdidMouseDevice->Unacquire ();
		m_pdidMouseDevice->Release ();
		m_pdidMouseDevice = NULL;
	}
	if (m_pdidJoyDevice) {
		m_pdidJoyDevice->Unacquire ();
		m_pdidJoyDevice->Release ();
		m_pdidJoyDevice = NULL;
	}
}

