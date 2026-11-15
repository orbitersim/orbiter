#include "NodeJoystick.h"
#include "Icon.h"
#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>

class Joystick {
public:
	SDL_Joystick *joy = NULL;
	SDL_JoystickID id = 0;
	int nbuttons = 0;
	int naxis = 0;
	int nhats = 0;
//	int nballs = 0;
	std::string name;
	std::vector<std::string> btnnames;
	std::vector<std::string> axesnames;

//	struct BallState {
//		int dx = 0;
//		int dy = 0;
//	};

	struct JoyState {
		std::vector<int> btns; // should be bools but we don't want to use a specialized vector<bool>
		std::vector<int> axis;
		std::vector<int> hats;
//		std::vector<BallState> balls;
	} state;
public:
	Joystick(SDL_JoystickID idjoy) {
		joy = SDL_OpenJoystick(idjoy);
		if (!joy) {
			return;
		}
		id = idjoy;
		name     = SDL_GetJoystickName(joy);
		nbuttons = SDL_GetNumJoystickButtons(joy); 
		naxis    = SDL_GetNumJoystickAxes(joy); 
		nhats    = SDL_GetNumJoystickHats(joy); 
//		nballs   = SDL_GetNumJoystickBalls(joy); 

		printf("Detected controller '%s' id=%d\n", name.c_str(), id);
//		printf("Buttons: %d\nAxes: %d\nHats: %d\nBalls: %d\n", nbuttons, naxis, nhats, nballs);
		printf("Buttons: %d\nAxes: %d\nHats: %d\n", nbuttons, naxis, nhats);
		const char *serial = SDL_GetJoystickSerial(joy);
		const char *path = SDL_GetJoystickPath(joy);
		printf("Serial=%s\nPath=%s\n", serial, path);

		btnnames.resize(nbuttons);
		axesnames.resize(naxis);
		state.btns.resize(nbuttons);
		state.axis.resize(naxis);
		state.hats.resize(nhats);
//		state.balls.resize(nballs);
		/*
		// Disable for now since indices between SDL_GetJoystickButton and SDL_GetGamepadButton are not the same...
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
		} else
		*/
		{
			for(int i = 0; i < nbuttons; i++) {
				btnnames[i] = std::string("Btn ") + std::to_string(i);
			}
			for(int i = 0; i < naxis; i++) {
				axesnames[i] = std::string("Axis ") + std::to_string(i);
			}
		}
	}
	~Joystick() {
		if(joy) SDL_CloseJoystick(joy);
	}
	Joystick(const Joystick&) = delete;
	Joystick& operator=(const Joystick&) = delete;

	bool IsConnected() const { return joy && SDL_JoystickConnected(joy); }
	const JoyState *GetState() const { return &state; }

	void UpdateState() {
		if(!joy || !SDL_JoystickConnected(joy)) {
			std::fill(state.btns.begin(), state.btns.end(), 0);
			std::fill(state.axis.begin(), state.axis.end(), 0);
			std::fill(state.hats.begin(), state.hats.end(), 0);
//			std::fill(state.balls.begin(), state.balls.end(), BallState{0,0});
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
//		for(int i=0; i<state.balls.size(); i++) {
//			if(!SDL_GetJoystickBall(joy, i, &state.balls[i].dx, &state.balls[i].dy)) {
//				state.balls[i].dx = 0;
//				state.balls[i].dy = 0;
//			}
//		}
	}
};

NodeJoystick::NodeJoystick(SDL_JoystickID jsid, int id) : Node(SDL_GetJoystickNameForID(jsid), id)
{
	js = new Joystick(jsid);

	for(auto &btn: js->btnnames) {
		AddOutput(btn.c_str(), Pin::type::Button);
	}
	for(auto &axis: js->axesnames) {
		AddOutput(axis.c_str(), Pin::type::Axis);
	}
	for(size_t hat = 0; hat < js->state.hats.size(); hat++) {
		char hatname[32];
		sprintf(hatname, "Hat%d", hat);
		AddOutput(hatname, Pin::type::Hat);
	}
}

NodeJoystick::~NodeJoystick()
{
	delete js;
}

void NodeJoystick::Update(bool dryrun)
{
	if(!js) return; // No joystick attached to this node

	js->UpdateState();
	auto normalize = [](int v) {
		return  v < 0 ? v / 32768.0f : v / 32767.0f;
	};
	const auto *state = js->GetState();
	size_t idx = 0;
	for(int i=0; i<state->btns.size(); i++) {
		outputs[idx++].bVal = state->btns[i];
	}
	for(int i=0; i<state->axis.size(); i++) {
		outputs[idx++].fVal = normalize(state->axis[i]);
	}
	for(int i=0; i<state->hats.size(); i++) {
		outputs[idx++].hVal = state->hats[i];
	}
}

bool NodeJoystick::IsConnected()
{
	return js && js->IsConnected();
}

bool NodeJoystick::IsAttached()
{
	return js != NULL;
}

void NodeJoystick::Detach()
{
	if(!js) return;

	// Clear the outputs to prevent stale values
	const auto *state = js->GetState();
	size_t idx = 0;
	for(int i=0; i<state->btns.size(); i++) {
		outputs[idx++].bVal = false;
	}
	for(int i=0; i<state->axis.size(); i++) {
		outputs[idx++].fVal = 0.0f;
	}
	for(int i=0; i<state->hats.size(); i++) {
		outputs[idx++].hVal = SDL_HAT_CENTERED;
	}

	delete js;
	js = NULL;
}

bool NodeJoystick::Attach(SDL_JoystickID id)
{
	assert(!js);
	js = new Joystick(id);
	if(!js->IsConnected()) {
		Detach();
		return false;
	}
	// Check that the attached joystick has the same mapping
	// It should only ever happen if two different controllers have the same GUID but different buttons
	const auto *state = js->GetState();
	size_t idx = 0;
//	if(outputs.size() != state->btns.size() + state->axis.size() + state->hats.size() + state->balls.size()) {
	if(outputs.size() != state->btns.size() + state->axis.size() + state->hats.size()) {
		Detach();
		return false;
	}

	for(int i=0; i<state->btns.size(); i++) {
		if(outputs[idx++].ptype != Pin::type::Button) {
			Detach();
			return false;
		}
	}
	for(int i=0; i<state->axis.size(); i++) {
		if(outputs[idx++].ptype != Pin::type::Axis) {
			Detach();
			return false;
		}
	}
	for(int i=0; i<state->hats.size(); i++) {
		if(outputs[idx++].ptype != Pin::type::Hat) {
			Detach();
			return false;
		}
	}
	return true;
}

