#pragma once

#include "Node.h"

DECLARE_NODE_SINGLETON(NodeRCSMode)

class NodeRCSMode : public Node
{
public:
    InputPin<Pin::type::Button> select;
    InputPin<Pin::type::ButtonPressed> translation;
    InputPin<Pin::type::ButtonPressed> rotation;
    InputPin<Pin::type::ButtonPressed> off;
    InputPin<Pin::type::ButtonPressed> toggle;

    NodeRCSMode():Node("RCS Mode") {}
    void RegisterPins() override;
    void UpdateOutputs(bool dryrun) override;

    DECLARE_NODE_TRAITS(NodeRCSMode)
};
