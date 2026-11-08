#include "Controller.h"
#include "OrbiterSDK.h"
#include "Orbiter.h"
#include "Config.h"

extern Orbiter* g_pOrbiter;

void InputController::GlobalInit()
{
	int count = 0;
	SDL_JoystickID* ids = SDL_GetJoysticks(&count);
	
	for (int i = 0; i < count; ++i) {
		joysticks.emplace_back(ids[i]);
	}

	SDL_free(ids);
}

void InputController::PollDevices()
{
	SDL_UpdateJoysticks();
	Joystick *j = NULL;
	for(auto &joy: joysticks) {
		joy.UpdateState();
		if(joy.id == 1)//selectedJoystick)
			j = &joy;
	}

	if(j) {
		CFG_JOYSTICKPRM &cfg = g_pOrbiter->Cfg()->CfgJoystickPrm;
		if(cfg.XAxis != 0)
			state.x = j->state.axis[cfg.XAxis - 1];
		if(cfg.YAxis != 0)
			state.y = j->state.axis[cfg.YAxis - 1];
		if(cfg.ZAxis != 0)
			state.Rz = j->state.axis[cfg.ZAxis - 1];
		if(cfg.ThrottleAxis != 0)
			state.throttle = -j->state.axis[cfg.ThrottleAxis - 1]; // usually inverted
		if(cfg.AltButton != 0)
			state.alt = j->state.btns[cfg.AltButton - 1];

		state.hat = 0xffff;
		if(j->state.hats.size() > 0) {
			switch (j->state.hats[0]) {
			case SDL_HAT_UP:        state.hat = 0; break;
			case SDL_HAT_RIGHTUP:   state.hat = 4500; break;
			case SDL_HAT_RIGHT:     state.hat = 9000; break;
			case SDL_HAT_RIGHTDOWN: state.hat = 13500; break;
			case SDL_HAT_DOWN:      state.hat = 18000; break;
			case SDL_HAT_LEFTDOWN:  state.hat = 22500; break;
			case SDL_HAT_LEFT:      state.hat = 27000; break;
			case SDL_HAT_LEFTUP:    state.hat = 31500; break;
			default:                state.hat = 0xFFFFFFFF; break;
			}
		}
	}
}

void InputController::GetInputs(int *x, int *y, int *Rz, int *hat, int *throttle, int *alt)
{
	*x = state.x;
	*y = state.y;
	*Rz = state.Rz;
	*hat = state.hat;
	*throttle = state.throttle;
	*alt = state.alt;
}

std::vector<std::pair<int, std::string>> InputController::GetControllerList()
{
	std::vector<std::pair<int, std::string>> ret;
	for(auto &joy: joysticks) {
		ret.emplace_back(joy.id, joy.name);
	}
	return ret;
}

void InputController::OptionChanged(DWORD cat, DWORD item)
{
	if (cat == OPTCAT_JOYSTICK) {
		Config *pcfg = g_pOrbiter->Cfg();
		switch (item) {
		case OPTITEM_JOYSTICK_DEVICE:
			SelectJoystick(pcfg->CfgJoystickPrm.Joy_idx - 1);
			break;
		case OPTITEM_JOYSTICK_PARAM:
			SetJoystickProperties();
			break;
		}
	}
}

void InputController::SelectJoystick(int id)
{
	id--; // 0 == none
	selectedJoystick = id;
	for(auto &joy: joysticks) {
		if(joy.id == id)
			joy.enabled = true;
		else
			joy.enabled = false;
	}
}

void InputController::SetJoystickProperties()
{
}
