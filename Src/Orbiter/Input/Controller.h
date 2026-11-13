#pragma once

#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>

#include <string>
#include <vector>
#include <list>
#include <algorithm>
#include <stdio.h>
#include <windows.h> // for DWORD

class ControllerGraph;
class InputController {
	static inline ControllerGraph *graph;
	static inline struct {
		float x,y,Rz,throttle;
		int alt, hat;
	} state;
public:
    static void GlobalInit();
	static void GetJoystickCtrl(float ctrl[15]);
	static void GetInputs(float *x, float *y, float *Rz, int *hat, float *throttle, int *alt);
	static std::vector<std::pair<int, std::string>> GetControllerList();
	static void OptionChanged(DWORD cat, DWORD item);
	static void SelectJoystick(int id);
	static void SetJoystickProperties();
	static ControllerGraph *GetGraph() { return graph; }
};
