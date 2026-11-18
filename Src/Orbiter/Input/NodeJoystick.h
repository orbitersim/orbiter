#pragma once

#include "Node.h"
#include <SDL3/SDL.h>
#include <memory>
#include <string>

class Joystick;
class NodeJoystick : public Node {
public:
	Joystick * js;
	std::string guid;
	NodeJoystick(Joystick *js, int id);
	~NodeJoystick();
	void Update(bool dryrun) override;
	bool IsConnected();
	bool IsAttached();
	void Attach(Joystick *id);
	void Detach();
	Joystick *GetJoystick() const { return js; }
	bool CanAttach(Joystick *js);
	void ToJSON(crude_json::value &out) override;

	// Serialization
	static constexpr const char* TypeName = "NodeJoystick";
	const char *GetTypeName() const override { return TypeName; }
};
