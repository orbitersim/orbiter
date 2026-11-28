#pragma once

#include "Node.h"
#include <memory>
#include <string>

class Joystick;
DECLARE_NODE(NodeJoystick)

class NodeJoystick : public Node {
public:
    Joystick * js = NULL;
    std::string guid;
    std::function<void(Pin *)> cbPin;
    NodeJoystick();
    ~NodeJoystick();
    void RegisterPins() override;
    void UpdateOutputs(bool dryrun) override;
    bool IsConnected();
    bool IsAttached();
    void Attach(Joystick *id);
    void Detach();
    Joystick *GetJoystick() const { return js; }
    bool CanAttach(Joystick *js);
    void ToJSON(crude_json::value &out) override;
    void InitFromJoystick(Joystick *, int id);
    Pin *PinFromAxis(int axis);
    Pin *PinFromHat(int hat);

    DECLARE_NODE_TRAITS(NodeJoystick)

};

