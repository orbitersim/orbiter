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

HRESULT DInput::Create (HINSTANCE hInst)
{
	if (NULL == (diframe = new CDIFramework7())) {
		LOGOUT_ERR ("DirectInput: Could not create DI environment");
		return E_OUTOFMEMORY;
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

void DInput::SetRenderWindow(HWND hWnd)
{
	if (diframe)
		diframe->DestroyDevices();
	m_hWnd = hWnd;
}

bool DInput::CreateKbdDevice()
{
	if (!m_hWnd) return false; // no render window defined

	if (FAILED (diframe->CreateKbdDevice (m_hWnd))) {
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

	if (FAILED (diframe->CreateJoyDevice (m_hWnd, pcfg->CfgJoystickPrm.Joy_idx-1))) {
		LOGOUT_ERR("Could not create joystick device");
		return false;
	}
	
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

bool DInput::PollJoystick (DIJOYSTATE2 *js)
{
	// todo: return joystick data in device-independent format
	//       allow collecting data from more than one joystick

	LPDIRECTINPUTDEVICE8 dev = GetJoyDevice();
	if (!dev) return false;
	HRESULT hr = dev->Poll();
	//if (hr == DI_OK || hr == DI_NOEFFECT)     // ignore error flag from poll. appears to occasionally return DIERR_UNPLUGGED
		hr = dev->GetDeviceState (sizeof(DIJOYSTATE2), js);
		if (hr == DIERR_INPUTLOST || hr == DIERR_NOTACQUIRED) {
			if (SUCCEEDED(dev->Acquire())) {
				dev->Poll();
				hr = dev->GetDeviceState(sizeof(DIJOYSTATE2), js);
			}
		}
	return (hr == S_OK);
}

HRESULT DInput::SetJoystickProperties ()
{
	LPDIRECTINPUTDEVICE8 dev = GetJoyDevice();
	if (!dev) return DI_OK;

	HRESULT hr;
	DIPROPRANGE diprg;
	DIPROPDWORD diprw;
	joyprop.bRudder = false;
	joyprop.bThrottle = false;
	Config *pcfg = orbiter->Cfg();

	// x-axis range
	diprg.diph.dwSize       = sizeof (diprg);
	diprg.diph.dwHeaderSize = sizeof (diprg.diph);
	diprg.diph.dwObj        = DIJOFS_X;
	diprg.diph.dwHow        = DIPH_BYOFFSET;
	diprg.lMin              = -1000;
	diprg.lMax              = +1000;
	if ((hr = dev->SetProperty (DIPROP_RANGE, &diprg.diph)) != DI_OK)
		return hr;

	// x-axis deadzone
	diprw.diph.dwSize       = sizeof (diprw);
	diprw.diph.dwHeaderSize = sizeof (diprw.diph);
	diprw.diph.dwObj        = DIJOFS_X;
	diprw.diph.dwHow        = DIPH_BYOFFSET;
	diprw.dwData            = pcfg->CfgJoystickPrm.Deadzone;
	if ((hr = dev->SetProperty (DIPROP_DEADZONE, &diprw.diph)) != DI_OK)
		return hr;

	// y-axis range
	diprg.diph.dwSize       = sizeof (diprg);
	diprg.diph.dwHeaderSize = sizeof (diprg.diph);
	diprg.diph.dwObj        = DIJOFS_Y;
	diprg.diph.dwHow        = DIPH_BYOFFSET;
	diprg.lMin              = -1000;
	diprg.lMax              = +1000;
	if ((hr = dev->SetProperty (DIPROP_RANGE, &diprg.diph)) != DI_OK)
		return hr;

	// y-axis deadzone
	diprw.diph.dwSize       = sizeof (diprw);
	diprw.diph.dwHeaderSize = sizeof (diprw.diph);
	diprw.diph.dwObj        = DIJOFS_Y;
	diprw.diph.dwHow        = DIPH_BYOFFSET;
	diprw.dwData            = pcfg->CfgJoystickPrm.Deadzone;
	if ((hr = dev->SetProperty (DIPROP_DEADZONE, &diprw.diph)) != DI_OK)
		return hr;

	joyprop.bRudder = true;
	joyprop.bThrottle = true;

	diprg.diph.dwSize       = sizeof (diprg);
	diprg.diph.dwHeaderSize = sizeof (diprg.diph);
	diprg.diph.dwObj        = DIJOFS_RZ;
	diprg.diph.dwHow        = DIPH_BYOFFSET;
	diprg.lMin              = -1000;
	diprg.lMax              = +1000;
	if (dev->SetProperty (DIPROP_RANGE, &diprg.diph) != DI_OK)
		joyprop.bRudder = false;

	diprw.diph.dwSize       = sizeof (diprw);
	diprw.diph.dwHeaderSize = sizeof (diprw.diph);
	diprw.diph.dwObj        = DIJOFS_RZ;
	diprw.diph.dwHow        = DIPH_BYOFFSET;
	diprw.dwData            = pcfg->CfgJoystickPrm.Deadzone;
	if (dev->SetProperty (DIPROP_DEADZONE, &diprw.diph) != DI_OK)
		joyprop.bRudder = false;

	// z-axis range (throttle)
	DWORD thaxis;
	DIJOYSTATE2 js2;
	switch (pcfg->CfgJoystickPrm.ThrottleAxis) {
	case 1:
		LOGOUT ("Joystick throttle: Z-AXIS");
		thaxis = DIJOFS_Z;
		joyprop.ThrottleOfs = (BYTE*)&js2.lZ - (BYTE*)&js2;
		break;
	case 2:
		LOGOUT ("Joystick throttle: SLIDER 0");
		thaxis = DIJOFS_SLIDER(0);
		joyprop.ThrottleOfs = (BYTE*)&js2.rglSlider[0] - (BYTE*)&js2;
		break;
	case 3:
		LOGOUT ("Joystick throttle: SLIDER 1");
		thaxis = DIJOFS_SLIDER(1);
		joyprop.ThrottleOfs = (BYTE*)&js2.rglSlider[1] - (BYTE*)&js2;
		break;
	default:
		joyprop.bThrottle = false;
		LOGOUT ("Joystick throttle disabled by user");
		return DI_OK;
	}

	diprg.diph.dwSize       = sizeof (diprg);
	diprg.diph.dwHeaderSize = sizeof (diprg.diph);
	diprg.diph.dwObj        = thaxis;
	diprg.diph.dwHow        = DIPH_BYOFFSET;
	diprg.lMin              = -1000;
	diprg.lMax              = 0;
	if ((hr = dev->SetProperty (DIPROP_RANGE, &diprg.diph)) != DI_OK) {
		joyprop.bThrottle = false;
		LOGOUT("No joystick throttle control detected");
		LOGOUT_DIERR(hr);
		return DI_OK;
	}
	LOGOUT("Joystick throttle control detected");

	// throttle saturation at extreme ends
	diprw.diph.dwSize       = sizeof (diprw);
	diprw.diph.dwHeaderSize = sizeof (diprw.diph);
	diprw.diph.dwObj        = thaxis;
	diprw.diph.dwHow        = DIPH_BYOFFSET;
	diprw.dwData            = pcfg->CfgJoystickPrm.ThrottleSaturation;
	if (dev->SetProperty (DIPROP_SATURATION, &diprw.diph) != DI_OK) {
		LOGOUT_ERR("Setting joystick throttle saturation failed");
	}
	return DI_OK;
}
