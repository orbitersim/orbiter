#pragma once

#include <windows.h>
#include <string>
#include <vector>

namespace oplat {

enum JoyAxis { JOY_X = 0, JOY_Y, JOY_Z, JOY_RX, JOY_RY, JOY_RZ, JOY_SLIDER0, JOY_SLIDER1, JOY_NAXIS };

struct JoystickRaw {
	float axis[JOY_NAXIS] = {};
	bool hasAxis[JOY_NAXIS] = {};
	int pov[4] = { -1, -1, -1, -1 };
	BYTE buttons[128] = {};
	int nButtons = 0;
	int nPOV = 0;
};

__attribute__((visibility("default"))) std::vector<std::string> EnumerateJoysticks();
__attribute__((visibility("default"))) bool OpenJoystick(int index);
__attribute__((visibility("default"))) void CloseJoystick();
__attribute__((visibility("default"))) bool PollJoystick(JoystickRaw &state);

}
