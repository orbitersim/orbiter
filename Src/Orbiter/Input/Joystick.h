#pragma once
#include <vector>
#include <string>

class Joystick {
public:
	void *joy = NULL; // SDL_Joystick *
	uint32_t id = 0; // SDL_JoystickID
	int nbuttons = 0;
	int naxis = 0;
	int nhats = 0;
//	int nballs = 0;
	std::string name;

	struct EntryName {
		std::string name;
		std::string displayname;
	};

	std::vector<EntryName> btnnames;
	std::vector<EntryName> axesnames;
	std::vector<EntryName> hatnames;
	std::string guid;

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
	Joystick(uint32_t idjoy); // SDL_JoystickID
	~Joystick();
	Joystick(const Joystick&) = delete;
	Joystick& operator=(const Joystick&) = delete;

	bool IsConnected() const;
	const JoyState *GetState() const { return &state; }

	void UpdateState();
};
