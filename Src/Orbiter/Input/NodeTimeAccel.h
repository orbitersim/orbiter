#pragma once

#include "Node.h"

DECLARE_NODE_SINGLETON(NodeTimeAccel)

class NodeTimeAccel : public Node
{
public:
    InputPin<Pin::type::Button> select;
    InputPin<Pin::type::ButtonPressed> pause;
    InputPin<Pin::type::ButtonPressed> inc;
    InputPin<Pin::type::ButtonPressed> dec;
    InputPin<Pin::type::ButtonPressed> x01;
    InputPin<Pin::type::ButtonPressed> x1;
    InputPin<Pin::type::ButtonPressed> x10;
    InputPin<Pin::type::ButtonPressed> x100;
    InputPin<Pin::type::ButtonPressed> x1000;
    InputPin<Pin::type::ButtonPressed> x10000;

    NodeTimeAccel():Node("Time Acceleration") {}
    void RegisterPins() override;
    void UpdateOutputs(bool dryrun) override;

    DECLARE_NODE_TRAITS(NodeTimeAccel)
};
