#pragma once

#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>

#include <string>
#include <unordered_map>
#include <memory>
#include "Joystick.h"

class ControllerGraph;
class NodeLegacyControls;
class InputController {
	static inline ControllerGraph *graph;
	static inline struct {
		float x,y,Rz,throttle;
		int alt, hat;
	} state;

	static inline std::unordered_map<SDL_JoystickID, std::unique_ptr<Joystick>> joysticks;
	static inline NodeLegacyControls *legacyCtrl;
public:
    static void GlobalInit();
	static void GetJoystickCtrl(float ctrl[15]);
	static ControllerGraph *GetGraph() { return graph; }
	static void ScanControllers();
};
