#pragma once

#include "Node.h"
#include <SDL3/SDL.h>

class Joystick;
class NodeJoystick : public Node {
public:
	Joystick *js = NULL;
	NodeJoystick(SDL_JoystickID jsid, int id);
	~NodeJoystick();
	void Update(bool dryrun) override;
	bool IsConnected();
	bool IsAttached();
	bool Attach(SDL_JoystickID id);
	void Detach();
};
