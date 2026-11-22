#pragma once

#include "Node.h"

DECLARE_NODE_SINGLETON(NodeCamera)

class NodeCamera : public Node
{
public:
    InputPin<Pin::type::Button> enable;
    InputPin<Pin::type::Hat> hat;
    InputPin<Pin::type::Axis> rotX;
    InputPin<Pin::type::Axis> rotY;
    InputPin<Pin::type::ButtonPressed> reset;

    NodeCamera():Node("Camera") {}
    void RegisterPins() override;
    void UpdateOutputs(bool dryrun) override;

    DECLARE_NODE_TRAITS(NodeCamera)
};
