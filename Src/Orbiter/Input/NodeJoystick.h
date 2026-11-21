#pragma once

#include "Node.h"
#include <memory>
#include <string>

class Joystick;
class NodeJoystick : public Node {
public:
    Joystick * js = NULL;
    std::string guid;
    std::function<void(Pin *)> cbPin;
    NodeJoystick();
    ~NodeJoystick();
    void RegisterPins() override;
    void Update(bool dryrun) override;
    bool IsConnected();
    bool IsAttached();
    void Attach(Joystick *id);
    void Detach();
    Joystick *GetJoystick() const { return js; }
    bool CanAttach(Joystick *js);
    void ToJSON(crude_json::value &out) override;
    void InitFromJoystick(Joystick *, int id);

    // Serialization
    static constexpr const char* TypeName = "NodeJoystick";
    const char *GetTypeName() const override { return TypeName; }
};
