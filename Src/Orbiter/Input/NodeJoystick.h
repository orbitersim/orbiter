#pragma once

#include "Node.h"
#include <SDL3/SDL.h>
#include <memory>

class Joystick;
class NodeJoystick : public Node {
public:
	std::unique_ptr<Joystick> js;
	NodeJoystick(SDL_JoystickID jsid, int id);
	~NodeJoystick();
	void Update(bool dryrun) override;
	bool IsConnected();
	bool IsAttached();
	bool Attach(SDL_JoystickID id);
	void Detach();
};
