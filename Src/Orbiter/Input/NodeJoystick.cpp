#include "NodeJoystick.h"
#include "Icon.h"
#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>
#include "Joystick.h"

NodeJoystick::NodeJoystick(Joystick *joy, int id) : Node(joy->name.c_str(), id)
{
	js = joy;

    const auto cbAxis = [this](Pin *pin) {
        ImGui::PushItemWidth(80.f);
		ImGui::TextUnformatted("Output configuration");
        ImGui::DragFloat("Deadzone", &pin->context.deadzone, 0.01f, 0.0f, 0.9f, "%0.02f");
		ImGui::Checkbox("Rescale [-1;1] -> [0;1]", &pin->context.rescale);
		ImGui::Checkbox("Flip axis", &pin->context.flip);
		ImGui::Text("Output: %f", pin->fVal);
        ImGui::PopItemWidth();
    };

	for(auto &btn: js->btnnames) {
		AddOutput(btn.c_str(), Pin::type::Button);
	}
	for(auto &axis: js->axesnames) {
		Pin *pin = AddOutput(axis.c_str(), Pin::type::Axis);
		pin->cbContextMenu = cbAxis;
	}
	for(size_t hat = 0; hat < js->state.hats.size(); hat++) {
		char hatname[32];
		sprintf(hatname, "Hat%zu", hat);
		AddOutput(hatname, Pin::type::Hat);
	}

	guid = js->guid;
}

NodeJoystick::~NodeJoystick()
{
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
		Pin &out = outputs[idx++];

		float f = normalize(state->axis[i]);
        float deadzone = out.context.deadzone;
        if(fabs(f) < deadzone) {
            f = 0.0f;
        } else if(f < 0.0) {
            f = (f + deadzone) / (1.0f - deadzone);
        } else {
            f = (f - deadzone) / (1.0f - deadzone);
        }

		if(out.context.rescale) {
			f = (f + 1.0f) / 2.0f;
			if(f < 0.001f) f = 0.0f;
		}
		if(out.context.flip) f = -f;

		out.fVal = f;
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
	js = NULL;

	// Clear the outputs to prevent stale values
    for (Pin& pin : outputs) {
        switch (pin.ptype) {
        case Pin::type::Button:
            pin.bVal = false;
            break;
        case Pin::type::Axis:
        case Pin::type::HalfAxis:
            pin.fVal = 0.0f;
            break;
        case Pin::type::Hat:
            pin.hVal = SDL_HAT_CENTERED;
            break;
        }
    }
}

void NodeJoystick::Attach(Joystick *joy)
{
    assert(!js);
    js = joy;
}

bool NodeJoystick::CanAttach(Joystick *js) {
    if(js->guid != guid) return false;

    // Check that the attached joystick has the same mapping
    // in case two different controllers have the same GUID but different buttons
    const auto *state = js->GetState();
    size_t idx = 0;
//	if(outputs.size() != state->btns.size() + state->axis.size() + state->hats.size() + state->balls.size()) {
    if(outputs.size() != state->btns.size() + state->axis.size() + state->hats.size()) {
        return false;
    }

    for(int i=0; i<state->btns.size(); i++) {
        if(outputs[idx++].ptype != Pin::type::Button) {
            return false;
        }
    }
    for(int i=0; i<state->axis.size(); i++) {
        if(outputs[idx++].ptype != Pin::type::Axis) {
            return false;
        }
    }
    for(int i=0; i<state->hats.size(); i++) {
        if(outputs[idx++].ptype != Pin::type::Hat) {
            return false;
        }
    }
    return true;
}
