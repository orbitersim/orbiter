#pragma once

#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>

#include <string>
#include <vector>
#include <list>
#include <algorithm>
#include <stdio.h>
#include <windows.h> // for DWORD

class Joystick {
public:
	SDL_Joystick *joy;
	SDL_JoystickID id;
	bool enabled;
	int nbuttons;
	int naxis;
	int nhats;
	int nballs;
	std::string name;
	std::vector<std::string> btnnames;
	std::vector<std::string> axesnames;

	struct BallState {
		int dx = 0;
		int dy = 0;
	};

	struct JoyState {
		std::vector<int> btns; // should be bools but we don't want to use a specialized vector<bool>
		std::vector<int> axis;
		std::vector<int> hats;
		std::vector<BallState> balls;
	} state;
public:
	Joystick(SDL_JoystickID idjoy) {
		id = idjoy;
		joy = SDL_OpenJoystick(id);
		if (!joy) {
			enabled = false;
			return;
		}
		name     = SDL_GetJoystickName(joy);
		nbuttons = SDL_GetNumJoystickButtons(joy); 
		naxis    = SDL_GetNumJoystickAxes(joy); 
		nhats    = SDL_GetNumJoystickHats(joy); 
		nballs   = SDL_GetNumJoystickBalls(joy); 

		printf("Detected controller '%s' id=%d\n", name.c_str(), id);
		printf("Buttons: %d\nAxes: %d\nHats: %d\nBalls: %d\n", nbuttons, naxis, nhats, nballs);
		const char *serial = SDL_GetJoystickSerial(joy);
		const char *path = SDL_GetJoystickPath(joy);
		printf("Serial=%s\nPath=%s\n", serial, path);

		btnnames.resize(nbuttons);
		axesnames.resize(naxis);
		state.btns.resize(nbuttons);
		state.axis.resize(naxis);
		state.hats.resize(nhats);
		state.balls.resize(nballs);

		if(SDL_IsGamepad(id)) {
			printf("Is Gamepad\n");
			SDL_Gamepad *gamepad = SDL_OpenGamepad(id);
			SDL_GamepadType type =  SDL_GetGamepadType(gamepad);
			printf("%s\n", SDL_GetGamepadStringForType(type));
			for(int i = 0; i < nbuttons; i++) {
				btnnames[i] = SDL_GetGamepadStringForButton((SDL_GamepadButton)i);
			}
			for(int i = 0; i < naxis; i++) {
				axesnames[i] = SDL_GetGamepadStringForAxis((SDL_GamepadAxis)i);
			}
			SDL_CloseGamepad(gamepad);
		} else {
			for(int i = 0; i < nbuttons; i++) {
				btnnames[i] = std::string("Btn ") + std::to_string(i);
			}
			for(int i = 0; i < naxis; i++) {
				axesnames[i] = std::string("Axis ") + std::to_string(i);
			}
		}

		enabled = true;
	}
	~Joystick() {
		if(joy) SDL_CloseJoystick(joy);
	}
	const JoyState *GetState() {
		return &state;
	}

	void UpdateState() {
		if(!enabled || !SDL_JoystickConnected(joy)) {
			std::fill(state.btns.begin(), state.btns.end(), 0);
			std::fill(state.axis.begin(), state.axis.end(), 0);
			std::fill(state.hats.begin(), state.hats.end(), 0);
			std::fill(state.balls.begin(), state.balls.end(), BallState{0,0});
			return;
		}
		//printf("State:\n");
		for(int i=0; i<state.btns.size(); i++) {
			state.btns[i] = SDL_GetJoystickButton(joy, i) ? 1 : 0;
			//printf("btn%d - %d\n", i, state.btns[i]);
		}
		for(int i=0; i<state.axis.size(); i++) {
			state.axis[i] = SDL_GetJoystickAxis(joy, i);
			//printf("axis%d - %d\n", i, state.axis[i]);
		}
		for(int i=0; i<state.hats.size(); i++) {
			state.hats[i] = SDL_GetJoystickHat(joy, i);
			//printf("hat%d - %d\n", i, state.hats[i]);
		}
		for(int i=0; i<state.balls.size(); i++) {
			if(!SDL_GetJoystickBall(joy, i, &state.balls[i].dx, &state.balls[i].dy)) {
				state.balls[i].dx = 0;
				state.balls[i].dy = 0;
			}
		}
	}
};

class InputController {
	static inline std::list<Joystick> joysticks;
	static inline int selectedJoystick;
	static inline struct {
		int x,y,Rz,alt,throttle,hat;
	} state;
public:
    static void GlobalInit();
	static void PollDevices();
	static void GetInputs(int *x, int *y, int *Rz, int *hat, int *throttle, int *alt);
	static std::vector<std::pair<int, std::string>> GetControllerList();
	static void OptionChanged(DWORD cat, DWORD item);
	static void SelectJoystick(int id);
	static void SetJoystickProperties();
};
