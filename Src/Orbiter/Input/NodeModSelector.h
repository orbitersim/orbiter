#pragma once

#include "Node.h"

DECLARE_NODE(NodeModSelector)

class NodeModSelector : public Node
{
public:
    InputPin<Pin::type::Button> A;
    InputPin<Pin::type::Button> B;
    OutputPin<Pin::type::Button> O1;
    OutputPin<Pin::type::Button> O2;
    OutputPin<Pin::type::Button> O3;
    OutputPin<Pin::type::Button> O4;

    NodeModSelector():Node("Mod Selector") {}
    void RegisterPins() override;
    void UpdateOutputs(bool dryrun) override;

    DECLARE_NODE_TRAITS(NodeModSelector)
};
