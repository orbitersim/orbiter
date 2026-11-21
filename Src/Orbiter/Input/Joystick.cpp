#include "Joystick.h"
#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>

Joystick::Joystick(uint32_t idjoy) {
	joy = SDL_OpenJoystick(idjoy);
	// We used a void * in the header to limit SDL inclusion there
	// but here we need the correct type.
	SDL_Joystick *js = (SDL_Joystick *)joy;
	if (!joy) {
		return;
	}
	id = idjoy;
	name     = SDL_GetJoystickName(js);
	nbuttons = SDL_GetNumJoystickButtons(js); 
	naxis    = SDL_GetNumJoystickAxes(js); 
	nhats    = SDL_GetNumJoystickHats(js); 
//		nballs   = SDL_GetNumJoystickBalls(js); 

	printf("Detected controller '%s' id=%d\n", name.c_str(), id);
//		printf("Buttons: %d\nAxes: %d\nHats: %d\nBalls: %d\n", nbuttons, naxis, nhats, nballs);
	printf("Buttons: %d\nAxes: %d\nHats: %d\n", nbuttons, naxis, nhats);
	const char *serial = SDL_GetJoystickSerial(js);
	const char *path = SDL_GetJoystickPath(js);
	printf("Serial=%s\nPath=%s\n", serial, path);

	btnnames.resize(nbuttons);
	axesnames.resize(naxis);
	hatnames.resize(nhats);
	state.btns.resize(nbuttons);
	state.axis.resize(naxis);
	state.hats.resize(nhats);
//		state.balls.resize(nballs);

	for(int i = 0; i < nbuttons; i++) {
		btnnames[i].name = std::string("b") + std::to_string(i);
		btnnames[i].displayname = std::string("Btn ") + std::to_string(i);
	}
	for(int i = 0; i < naxis; i++) {
		axesnames[i].name = std::string("a") + std::to_string(i);
		axesnames[i].displayname = std::string("Axis ") + std::to_string(i);
	}
	for(int i = 0; i < nhats; i++) {
		hatnames[i].name = std::string("h") + std::to_string(i);
		hatnames[i].displayname = std::string("Hat ") + std::to_string(i);
	}

	SDL_GUID sguid = SDL_GetJoystickGUID(js);
	guid = std::string(reinterpret_cast<const char*>(sguid.data), sizeof(sguid.data));
}
Joystick::~Joystick() {
	if(joy) SDL_CloseJoystick((SDL_Joystick *)joy);
}

bool Joystick::IsConnected() const { return joy && SDL_JoystickConnected((SDL_Joystick *)joy); }

void Joystick::UpdateState() {
	SDL_Joystick *js = (SDL_Joystick *)joy;
	if(!joy || !SDL_JoystickConnected(js)) {
		std::fill(state.btns.begin(), state.btns.end(), 0);
		std::fill(state.axis.begin(), state.axis.end(), 0);
		std::fill(state.hats.begin(), state.hats.end(), 0);
//			std::fill(state.balls.begin(), state.balls.end(), BallState{0,0});
		return;
	}
	//printf("State:\n");
	for(int i=0; i<state.btns.size(); i++) {
		state.btns[i] = SDL_GetJoystickButton(js, i) ? 1 : 0;
		//printf("btn%d - %d\n", i, state.btns[i]);
	}
	for(int i=0; i<state.axis.size(); i++) {
		state.axis[i] = SDL_GetJoystickAxis(js, i);
		//printf("axis%d - %d\n", i, state.axis[i]);
	}
	for(int i=0; i<state.hats.size(); i++) {
		state.hats[i] = SDL_GetJoystickHat(js, i);
		//printf("hat%d - %d\n", i, state.hats[i]);
	}
//		for(int i=0; i<state.balls.size(); i++) {
//			if(!SDL_GetJoystickBall(js, i, &state.balls[i].dx, &state.balls[i].dy)) {
//				state.balls[i].dx = 0;
//				state.balls[i].dy = 0;
//			}
//		}
}
