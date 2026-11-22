#pragma once

#include "Node.h"

DECLARE_NODE_SINGLETON(NodeRCSPrograms)

class NodeRCSPrograms : public Node
{
public:
    InputPin<Pin::type::Button> enable;
    InputPin<Pin::type::ButtonPressed> prograde;
    InputPin<Pin::type::ButtonPressed> retrograde;
    InputPin<Pin::type::ButtonPressed> normal;
    InputPin<Pin::type::ButtonPressed> antinormal;
    InputPin<Pin::type::ButtonPressed> killrot;
    InputPin<Pin::type::ButtonPressed> hlevel;
    InputPin<Pin::type::ButtonPressed> holdalt;
    InputPin<Pin::type::ButtonPressed> off;

    NodeRCSPrograms():Node("RCS Programs") {}
    void RegisterPins() override;
    void UpdateOutputs(bool dryrun) override;

    DECLARE_NODE_TRAITS(NodeRCSPrograms)
};
