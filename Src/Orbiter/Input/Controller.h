#pragma once

#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>

#include <string>
#include <unordered_map>
#include <string>

class NodeJoystick;
class ControllerGraph;
class NodeLegacyControls;
class InputController {
	static inline ControllerGraph *graph;
	static inline struct {
		float x,y,Rz,throttle;
		int alt, hat;
	} state;
	// Map GUID to joystick instance
	static inline std::unordered_map<std::string, NodeJoystick *> joyMap;
	static inline NodeLegacyControls *legacyCtrl;
public:
    static void GlobalInit();
	static void GetJoystickCtrl(float ctrl[15]);
	static void GetInputs(float *x, float *y, float *Rz, int *hat, float *throttle, int *alt);
	static ControllerGraph *GetGraph() { return graph; }
	static void ScanControllers();
};
