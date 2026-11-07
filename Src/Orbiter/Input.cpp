// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// User input: keyboard state and events, and joystick
// =======================================================================

#include "Input.h"
#include "Log.h"
#include "Orbiter.h"
#include "OrbiterPlatform.h"
#include "OrbiterJoystick.h"
#include <cmath>

InputManager::InputManager (Orbiter *pOrbiter)
{
	orbiter = pOrbiter;
	m_hWnd = NULL;
	m_bKbd = false;
	m_bJoy = false;
	joyprop = { false, false, 0 };
}

InputManager::~InputManager ()
{
	Destroy();
}

HRESULT InputManager::Create ()
{
	oplat::EnumerateJoysticks();
	return S_OK;
}

void InputManager::Destroy ()
{
	DestroyDevices();
}

void InputManager::SetRenderWindow (HWND hWnd)
{
	DestroyDevices();
	m_hWnd = hWnd;
}

bool InputManager::CreateKbdDevice ()
{
	if (!m_hWnd) return false;
	oplat::ClearKeyState();
	std::vector<oplat::KeyEvent> discard;
	oplat::TakeKeyEvents(discard);
	m_bKbd = true;
	return true;
}

bool InputManager::CreateJoyDevice ()
{
	if (!m_hWnd) return false;

	Config *pcfg = orbiter->Cfg();
	if (!pcfg->CfgJoystickPrm.Joy_idx) return false;

	if (!oplat::OpenJoystick((int)pcfg->CfgJoystickPrm.Joy_idx - 1)) {
		LOGOUT_ERR("Could not open joystick device");
		return false;
	}
	m_bJoy = true;
	SetJoystickProperties();
	return true;
}

void InputManager::DestroyDevices ()
{
	if (m_bJoy) oplat::CloseJoystick();
	m_bJoy = false;
	m_bKbd = false;
}

void InputManager::GetKeyState (char kstate[256]) const
{
	if (!m_bKbd) {
		memset(kstate, 0, 256);
		return;
	}
	memcpy(kstate, oplat::KeyState(), 256);
}

void InputManager::TakeKeyEvents (std::vector<KeyEvent> &events)
{
	events.clear();
	std::vector<oplat::KeyEvent> raw;
	oplat::TakeKeyEvents(raw);
	if (!m_bKbd) return;
	for (auto &e : raw)
		events.push_back({ e.key, e.down });
}

std::vector<std::string> InputManager::Joysticks () const
{
	return oplat::EnumerateJoysticks();
}

void InputManager::OptionChanged (DWORD cat, DWORD item)
{
	if (cat == OPTCAT_JOYSTICK) {
		switch (item) {
		case OPTITEM_JOYSTICK_DEVICE:
			if (m_bJoy) oplat::CloseJoystick();
			m_bJoy = false;
			CreateJoyDevice();
			break;
		case OPTITEM_JOYSTICK_PARAM:
			SetJoystickProperties();
			break;
		}
	}
}

static LONG ScaleAxis (float v, int deadzone)
{
	double dz = deadzone / 10000.0;
	double a = fabs((double)v);
	if (a <= dz) return 0;
	double s = (a - dz) / (1.0 - dz);
	if (s > 1.0) s = 1.0;
	return (LONG)lround((v < 0 ? -s : s) * 1000.0);
}

static LONG ScaleThrottle (float v, int saturation)
{
	double t = 0.5 * ((double)v + 1.0);
	double sat = saturation / 10000.0;
	if (sat > 0.0 && sat < 1.0) {
		double c = (t - 0.5) / sat + 0.5;
		t = c < 0.0 ? 0.0 : c > 1.0 ? 1.0 : c;
	}
	return (LONG)lround(-1000.0 + t * 1000.0);
}

bool InputManager::PollJoystick (JoyState *js)
{
	if (!m_bJoy) return false;
	oplat::JoystickRaw raw;
	if (!oplat::PollJoystick(raw)) return false;

	Config *pcfg = orbiter->Cfg();
	int dz = pcfg->CfgJoystickPrm.Deadzone;
	memset(js, 0, sizeof(*js));
	js->lX = raw.hasAxis[oplat::JOY_X] ? ScaleAxis(raw.axis[oplat::JOY_X], dz) : 0;
	js->lY = raw.hasAxis[oplat::JOY_Y] ? ScaleAxis(raw.axis[oplat::JOY_Y], dz) : 0;
	js->lRz = (joyprop.bRudder && raw.hasAxis[oplat::JOY_RZ]) ? ScaleAxis(raw.axis[oplat::JOY_RZ], dz) : 0;
	js->lRx = raw.hasAxis[oplat::JOY_RX] ? ScaleAxis(raw.axis[oplat::JOY_RX], dz) : 0;
	js->lRy = raw.hasAxis[oplat::JOY_RY] ? ScaleAxis(raw.axis[oplat::JOY_RY], dz) : 0;

	int sat = pcfg->CfgJoystickPrm.ThrottleSaturation;
	auto axisValue = [&](int ax, int throttleAxis) -> LONG {
		if (!raw.hasAxis[ax]) return 0;
		if (joyprop.bThrottle && joyprop.ThrottleAxis == throttleAxis)
			return ScaleThrottle(raw.axis[ax], sat);
		return ScaleAxis(raw.axis[ax], dz);
	};
	js->lZ = axisValue(oplat::JOY_Z, 1);
	js->rglSlider[0] = axisValue(oplat::JOY_SLIDER0, 2);
	js->rglSlider[1] = axisValue(oplat::JOY_SLIDER1, 3);

	for (int i = 0; i < 4; i++)
		js->rgdwPOV[i] = raw.pov[i] < 0 ? 0xFFFFFFFF : (DWORD)raw.pov[i];
	memcpy(js->rgbButtons, raw.buttons, sizeof(js->rgbButtons));
	return true;
}

LONG InputManager::ThrottleValue (const JoyState &js) const
{
	switch (joyprop.ThrottleAxis) {
	case 1: return js.lZ;
	case 2: return js.rglSlider[0];
	case 3: return js.rglSlider[1];
	default: return 0;
	}
}

HRESULT InputManager::SetJoystickProperties ()
{
	joyprop.bRudder = false;
	joyprop.bThrottle = false;
	joyprop.ThrottleAxis = 0;
	if (!m_bJoy) return S_OK;

	oplat::JoystickRaw raw;
	oplat::PollJoystick(raw);
	joyprop.bRudder = raw.hasAxis[oplat::JOY_RZ];

	Config *pcfg = orbiter->Cfg();
	int axis;
	switch (pcfg->CfgJoystickPrm.ThrottleAxis) {
	case 1:
		LOGOUT("Joystick throttle: Z-AXIS");
		axis = oplat::JOY_Z;
		break;
	case 2:
		LOGOUT("Joystick throttle: SLIDER 0");
		axis = oplat::JOY_SLIDER0;
		break;
	case 3:
		LOGOUT("Joystick throttle: SLIDER 1");
		axis = oplat::JOY_SLIDER1;
		break;
	default:
		LOGOUT("Joystick throttle disabled by user");
		return S_OK;
	}
	if (!raw.hasAxis[axis]) {
		LOGOUT("No joystick throttle control detected");
		return S_OK;
	}
	joyprop.bThrottle = true;
	joyprop.ThrottleAxis = (int)pcfg->CfgJoystickPrm.ThrottleAxis;
	LOGOUT("Joystick throttle control detected");
	return S_OK;
}
