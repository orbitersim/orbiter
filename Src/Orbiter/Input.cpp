// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// DirectInput user interface class
// =======================================================================

#include "Input.h"
#include "Log.h"
#include "Orbiter.h"

DInput::DInput (Orbiter *pOrbiter)
{
	orbiter = pOrbiter;
	diframe = NULL;
	m_hWnd = NULL;
}

DInput::~DInput ()
{
	Destroy();
}

#ifndef __linux__
HRESULT DInput::Create (HINSTANCE hInst)
#else // __linux__
int DInput::Create (void *hInst)
#endif // __linux__
{
	if (NULL == (diframe = new CDIFramework7())) {
		LOGOUT_ERR ("DirectInput: Could not create DI environment");
#ifndef __linux__
		return E_OUTOFMEMORY;
#else // __linux__
		return DIERR_INPUTLOST;
#endif // __linux__
	}
	return diframe->Create (hInst);
}

void DInput::Destroy ()
{
	if (diframe) {
		delete diframe;
		diframe = NULL;
	}
}

#ifndef __linux__
void DInput::SetRenderWindow(HWND hWnd)
#else // __linux__
void DInput::SetRenderWindow(QWindow *hWnd)
#endif // __linux__
{
	if (diframe)
		diframe->DestroyDevices();
	m_hWnd = hWnd;
}

bool DInput::CreateKbdDevice()
{
	if (!m_hWnd) return false; // no render window defined

#ifndef __linux__
	if (FAILED (diframe->CreateKbdDevice (m_hWnd))) {
#else // __linux__
	if (diframe->CreateKbdDevice (m_hWnd) != DI_OK) {
#endif // __linux__
		LOGOUT("ERROR: Could not create keyboard device");
		return false; // we need the keyboard, so give up
	}
	GetKbdDevice()->Acquire();
	return true;
}

bool DInput::CreateJoyDevice ()
{
	if (!m_hWnd) return false; // no render window defined

	Config *pcfg = orbiter->Cfg();
	if (!pcfg->CfgJoystickPrm.Joy_idx) return false; // no joystick requested

#ifndef __linux__
	if (FAILED (diframe->CreateJoyDevice (m_hWnd, pcfg->CfgJoystickPrm.Joy_idx-1))) {
#else // __linux__
	if (diframe->CreateJoyDevice (m_hWnd, pcfg->CfgJoystickPrm.Joy_idx-1) != DI_OK) {
#endif // __linux__
		LOGOUT_ERR("Could not create joystick device");
		return false;
	}
	
#ifndef __linux__
	HRESULT hr = GetJoyDevice()->Acquire();
	if (hr == DIERR_OTHERAPPHASPRIO) {
		Sleep(1000);
		hr = GetJoyDevice()->Acquire();
	}
	switch (hr) {
	case DIERR_OTHERAPPHASPRIO:
		hr = DI_OK;
		break;
	}
#else // __linux__
	GetJoyDevice()->Acquire();
	// DIERR_OTHERAPPHASPRIO retry left out: evdev devices are shared, nothing holds priority
#endif // __linux__

	if (SetJoystickProperties () != DI_OK) {
		LOGOUT_ERR("Could not set joystick properties");
		return false;
	}


	return true;
}

void DInput::DestroyDevices ()
{
	diframe->DestroyDevices();
}

void DInput::OptionChanged(DWORD cat, DWORD item)
{
	if (cat == OPTCAT_JOYSTICK) {
		switch (item) {
		case OPTITEM_JOYSTICK_DEVICE:
			diframe->DestroyJoyDevice();
			CreateJoyDevice();
			break;
		case OPTITEM_JOYSTICK_PARAM:
			SetJoystickProperties();
			break;
		}
	}
}

#ifndef __linux__
bool DInput::PollJoystick (DIJOYSTATE2 *js)
#else // __linux__
bool DInput::PollJoystick (JoyState *js)
#endif // __linux__
{
	// todo: return joystick data in device-independent format
	//       allow collecting data from more than one joystick

#ifndef __linux__
	LPDIRECTINPUTDEVICE8 dev = GetJoyDevice();
#else // __linux__
	JoystickDevice *dev = GetJoyDevice();
#endif // __linux__
	if (!dev) return false;
#ifndef __linux__
	HRESULT hr = dev->Poll();
#else // __linux__
	int hr = dev->Poll();
#endif // __linux__
	//if (hr == DI_OK || hr == DI_NOEFFECT)     // ignore error flag from poll. appears to occasionally return DIERR_UNPLUGGED
#ifndef __linux__
		hr = dev->GetDeviceState (sizeof(DIJOYSTATE2), js);
#else // __linux__
		hr = dev->GetDeviceState (sizeof(JoyState), js);
#endif // __linux__
		if (hr == DIERR_INPUTLOST || hr == DIERR_NOTACQUIRED) {
#ifndef __linux__
			if (SUCCEEDED(dev->Acquire())) {
#else // __linux__
			if (dev->Acquire() == DI_OK) {
#endif // __linux__
				dev->Poll();
#ifndef __linux__
				hr = dev->GetDeviceState(sizeof(DIJOYSTATE2), js);
#else // __linux__
				hr = dev->GetDeviceState(sizeof(JoyState), js);
#endif // __linux__
			}
		}
#ifndef __linux__
	return (hr == S_OK);
#else // __linux__
	return (hr == DI_OK);
#endif // __linux__
}

#ifndef __linux__
HRESULT DInput::SetJoystickProperties ()
#else // __linux__
int DInput::SetJoystickProperties ()
#endif // __linux__
{
#ifndef __linux__
	LPDIRECTINPUTDEVICE8 dev = GetJoyDevice();
#else // __linux__
	JoystickDevice *dev = GetJoyDevice();
#endif // __linux__
	if (!dev) return DI_OK;

#ifndef __linux__
	HRESULT hr;
	DIPROPRANGE diprg;
	DIPROPDWORD diprw;
#endif // !__linux__
	joyprop.bRudder = false;
	joyprop.bThrottle = false;
	Config *pcfg = orbiter->Cfg();

	// x-axis range
#ifndef __linux__
	diprg.diph.dwSize       = sizeof (diprg);
	diprg.diph.dwHeaderSize = sizeof (diprg.diph);
	diprg.diph.dwObj        = DIJOFS_X;
	diprg.diph.dwHow        = DIPH_BYOFFSET;
	diprg.lMin              = -1000;
	diprg.lMax              = +1000;
	if ((hr = dev->SetProperty (DIPROP_RANGE, &diprg.diph)) != DI_OK)
		return hr;
#else // __linux__
	if (!dev->SetAxisRange (JoystickDevice::AX_X, -1000, +1000))
		return DIERR_INPUTLOST;
#endif // __linux__

	// x-axis deadzone
#ifndef __linux__
	diprw.diph.dwSize       = sizeof (diprw);
	diprw.diph.dwHeaderSize = sizeof (diprw.diph);
	diprw.diph.dwObj        = DIJOFS_X;
	diprw.diph.dwHow        = DIPH_BYOFFSET;
	diprw.dwData            = pcfg->CfgJoystickPrm.Deadzone;
	if ((hr = dev->SetProperty (DIPROP_DEADZONE, &diprw.diph)) != DI_OK)
		return hr;
#else // __linux__
	if (!dev->SetAxisDeadzone (JoystickDevice::AX_X, pcfg->CfgJoystickPrm.Deadzone))
		return DIERR_INPUTLOST;
#endif // __linux__

	// y-axis range
#ifndef __linux__
	diprg.diph.dwSize       = sizeof (diprg);
	diprg.diph.dwHeaderSize = sizeof (diprg.diph);
	diprg.diph.dwObj        = DIJOFS_Y;
	diprg.diph.dwHow        = DIPH_BYOFFSET;
	diprg.lMin              = -1000;
	diprg.lMax              = +1000;
	if ((hr = dev->SetProperty (DIPROP_RANGE, &diprg.diph)) != DI_OK)
		return hr;
#else // __linux__
	if (!dev->SetAxisRange (JoystickDevice::AX_Y, -1000, +1000))
		return DIERR_INPUTLOST;
#endif // __linux__

	// y-axis deadzone
#ifndef __linux__
	diprw.diph.dwSize       = sizeof (diprw);
	diprw.diph.dwHeaderSize = sizeof (diprw.diph);
	diprw.diph.dwObj        = DIJOFS_Y;
	diprw.diph.dwHow        = DIPH_BYOFFSET;
	diprw.dwData            = pcfg->CfgJoystickPrm.Deadzone;
	if ((hr = dev->SetProperty (DIPROP_DEADZONE, &diprw.diph)) != DI_OK)
		return hr;
#else // __linux__
	if (!dev->SetAxisDeadzone (JoystickDevice::AX_Y, pcfg->CfgJoystickPrm.Deadzone))
		return DIERR_INPUTLOST;
#endif // __linux__

	joyprop.bRudder = true;
	joyprop.bThrottle = true;

#ifndef __linux__
	diprg.diph.dwSize       = sizeof (diprg);
	diprg.diph.dwHeaderSize = sizeof (diprg.diph);
	diprg.diph.dwObj        = DIJOFS_RZ;
	diprg.diph.dwHow        = DIPH_BYOFFSET;
	diprg.lMin              = -1000;
	diprg.lMax              = +1000;
	if (dev->SetProperty (DIPROP_RANGE, &diprg.diph) != DI_OK)
#else // __linux__
	if (!dev->SetAxisRange (JoystickDevice::AX_RZ, -1000, +1000))
#endif // __linux__
		joyprop.bRudder = false;

#ifndef __linux__
	diprw.diph.dwSize       = sizeof (diprw);
	diprw.diph.dwHeaderSize = sizeof (diprw.diph);
	diprw.diph.dwObj        = DIJOFS_RZ;
	diprw.diph.dwHow        = DIPH_BYOFFSET;
	diprw.dwData            = pcfg->CfgJoystickPrm.Deadzone;
	if (dev->SetProperty (DIPROP_DEADZONE, &diprw.diph) != DI_OK)
#else // __linux__
	if (!dev->SetAxisDeadzone (JoystickDevice::AX_RZ, pcfg->CfgJoystickPrm.Deadzone))
#endif // __linux__
		joyprop.bRudder = false;

	// z-axis range (throttle)
#ifndef __linux__
	DWORD thaxis;
	DIJOYSTATE2 js2;
#else // __linux__
	JoystickDevice::Axis thaxis;
	JoyState js2;
#endif // __linux__
	switch (pcfg->CfgJoystickPrm.ThrottleAxis) {
	case 1:
		LOGOUT ("Joystick throttle: Z-AXIS");
#ifndef __linux__
		thaxis = DIJOFS_Z;
#else // __linux__
		thaxis = JoystickDevice::AX_Z;
#endif // __linux__
		joyprop.ThrottleOfs = (BYTE*)&js2.lZ - (BYTE*)&js2;
		break;
	case 2:
		LOGOUT ("Joystick throttle: SLIDER 0");
#ifndef __linux__
		thaxis = DIJOFS_SLIDER(0);
#else // __linux__
		thaxis = JoystickDevice::AX_SLIDER0;
#endif // __linux__
		joyprop.ThrottleOfs = (BYTE*)&js2.rglSlider[0] - (BYTE*)&js2;
		break;
	case 3:
		LOGOUT ("Joystick throttle: SLIDER 1");
#ifndef __linux__
		thaxis = DIJOFS_SLIDER(1);
#else // __linux__
		thaxis = JoystickDevice::AX_SLIDER1;
#endif // __linux__
		joyprop.ThrottleOfs = (BYTE*)&js2.rglSlider[1] - (BYTE*)&js2;
		break;
	default:
		joyprop.bThrottle = false;
		LOGOUT ("Joystick throttle disabled by user");
		return DI_OK;
	}

#ifndef __linux__
	diprg.diph.dwSize       = sizeof (diprg);
	diprg.diph.dwHeaderSize = sizeof (diprg.diph);
	diprg.diph.dwObj        = thaxis;
	diprg.diph.dwHow        = DIPH_BYOFFSET;
	diprg.lMin              = -1000;
	diprg.lMax              = 0;
	if ((hr = dev->SetProperty (DIPROP_RANGE, &diprg.diph)) != DI_OK) {
#else // __linux__
	if (!dev->SetAxisRange (thaxis, -1000, 0)) {
#endif // __linux__
		joyprop.bThrottle = false;
		LOGOUT("No joystick throttle control detected");
#ifndef __linux__
		LOGOUT_DIERR(hr);
#endif // !__linux__
		return DI_OK;
	}
	LOGOUT("Joystick throttle control detected");

	// throttle saturation at extreme ends
#ifndef __linux__
	diprw.diph.dwSize       = sizeof (diprw);
	diprw.diph.dwHeaderSize = sizeof (diprw.diph);
	diprw.diph.dwObj        = thaxis;
	diprw.diph.dwHow        = DIPH_BYOFFSET;
	diprw.dwData            = pcfg->CfgJoystickPrm.ThrottleSaturation;
	if (dev->SetProperty (DIPROP_SATURATION, &diprw.diph) != DI_OK) {
#else // __linux__
	if (!dev->SetAxisSaturation (thaxis, pcfg->CfgJoystickPrm.ThrottleSaturation)) {
#endif // __linux__
		LOGOUT_ERR("Setting joystick throttle saturation failed");
	}
	return DI_OK;
}
