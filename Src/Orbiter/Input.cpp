// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// DirectInput user interface class
// =======================================================================

#include "Input.h"
#include "Log.h"
#include "Orbiter.h"

DInput::DInput (Orbiter *pOrbiter) : orbiter(pOrbiter), joystick(nullptr), joyprop(), m_hWnd(nullptr)
{
}

DInput::~DInput ()
{
	DestroyDevices();
}

void DInput::SetRenderWindow(HWND hWnd)
{
	m_hWnd = hWnd;
}

bool DInput::CreateJoyDevice ()
{
	if (!m_hWnd) return false; // no render window defined

	Config *pcfg = orbiter->Cfg();
	if (!pcfg->CfgJoystickPrm.Joy_idx) return false; // no joystick requested

	joystick = SDL_OpenJoystick(pcfg->CfgJoystickPrm.Joy_idx);
	if (!joystick) {
		LOGOUT_ERR("Could not acquire joystick: %s", SDL_GetError());
		return false;
	}

	SetJoystickProperties();

	return true;
}

void DInput::DestroyDevices ()
{
	if (joystick) {
		SDL_CloseJoystick(joystick);
		joystick = nullptr;
	}
}

void DInput::OptionChanged(DWORD cat, DWORD item)
{
	if (cat == OPTCAT_JOYSTICK) {
		switch (item) {
		case OPTITEM_JOYSTICK_DEVICE:
			DestroyDevices();
			CreateJoyDevice();
			break;
		case OPTITEM_JOYSTICK_PARAM:
			SetJoystickProperties();
			break;
		default: break;
		}
	}
}

bool DInput::PollJoystick (JoyState *js) const {
	const auto& joyprm = orbiter->Cfg()->CfgJoystickPrm;
	const auto deadzone = static_cast<Sint16>(joyprm.Deadzone);
	js->xAx = PollAxisWithDeadzone(joyprop.xAxId, deadzone);
	js->yAx = PollAxisWithDeadzone(joyprop.yAxId, deadzone);
	if (joyprop.bRudder) {
		js->zRot = PollAxisWithDeadzone(joyprop.zRotId, deadzone);
	} else js->zRot = 0;
	if (joyprop.bThrottle) {
		js->throttle = PollAxisWithDeadzone(joyprop.ThrottleOfs, deadzone);
	} else js->throttle = 0;
	js->hat = SDL_GetJoystickHat(joystick, 0);
	js->btn3 = SDL_GetJoystickButton(joystick, 2);
	return true;
}

Sint16 DInput::PollAxisWithDeadzone(const int axis, const Sint16 deadzone) const {
	if (!joystick) return 0;

	const Sint16 axval = SDL_GetJoystickAxis(joystick, axis);

	if (abs(axval) < deadzone) return 0;
	return axval;
}

void DInput::SetJoystickProperties() {
	if (!joystick) return;
	auto naxes = SDL_GetNumJoystickAxes(joystick);
	joyprop.xAxId = 0;
	joyprop.yAxId = 1;
	if (naxes > 2) {
		// Probably have a rudder axis.
		// TODO: better input config
		joyprop.bRudder = true;
		joyprop.zRotId = 2;
	}
	joyprop.bThrottle = true;
	switch (orbiter->Cfg()->CfgJoystickPrm.ThrottleAxis) {
	case 1:
		LOGOUT ("Joystick throttle: Z-AXIS");
		joyprop.ThrottleOfs = 2;
		break;
	case 2:
		LOGOUT ("Joystick throttle: SLIDER 0");
		joyprop.ThrottleOfs = 3;
		break;
	case 3:
		LOGOUT ("Joystick throttle: SLIDER 1");
		joyprop.ThrottleOfs = 4;
		break;
	default:
		joyprop.bThrottle = false;
		LOGOUT ("Joystick throttle disabled by user");
		break;
	}
}

bool DInput::ConsumeEvent(const SDL_Event &event) {
	if (event.type == SDL_EVENT_KEY_UP || event.type == SDL_EVENT_KEY_DOWN) {
		m_bufferedEvents.push_back(event.key);
		return true;
	}
	return false;
}
