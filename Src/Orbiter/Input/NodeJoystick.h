#pragma once

#include "Node.h"
#include <SDL3/SDL.h>

class Joystick;
class NodeJoystick : public Node {
public:
	Joystick *js;
	NodeJoystick(SDL_JoystickID jsid, int id);
	void Update(bool dryrun) override;
};
