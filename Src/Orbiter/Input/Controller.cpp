#include "Controller.h"
#include "OrbiterSDK.h"
#include "Orbiter.h"
#include "Config.h"
#include "Graph.h"

extern Orbiter* g_pOrbiter;

void InputController::GlobalInit()
{
	graph = new ControllerGraph();
}

void InputController::GetJoystickCtrl(float ctrl[15])
{
	SDL_UpdateJoysticks();
	graph->Update(false);
}

void InputController::GetInputs(float *x, float *y, float *Rz, int *hat, float *throttle, int *alt)
{
	auto normalize = [](int v) {
		return  v < 0 ? v / 32768.0f : v / 32767.0f;
	};

	*x = normalize(state.x);
	*y = normalize(state.y);
	*Rz = normalize(state.Rz);
	*hat = state.hat;
	*throttle = normalize(state.throttle);
	*alt = state.alt;
}

std::vector<std::pair<int, std::string>> InputController::GetControllerList()
{
	std::vector<std::pair<int, std::string>> ret;
	return ret;
}

void InputController::OptionChanged(DWORD cat, DWORD item)
{
}

void InputController::SelectJoystick(int id)
{
}

void InputController::SetJoystickProperties()
{
}
