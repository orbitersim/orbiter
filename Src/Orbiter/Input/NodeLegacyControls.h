#pragma once
#include "Node.h"

DECLARE_NODE_SINGLETON(NodeLegacyControls)

class NodeLegacyControls : public Node, public INodeLegacyController {
    InputPin<Pin::type::Button> ModKey;
    InputPin<Pin::type::Axis> MainEngine;
    InputPin<Pin::type::Axis> BankYaw;
    InputPin<Pin::type::Axis> PitchVert;
    InputPin<Pin::type::Axis> YawLat;
    InputPin<Pin::type::Hat>  CamPanel;

public:
    NodeLegacyControls();
    void RegisterPins() override;
    void UpdateOutputs(bool dryrun) override;
    void AddJoystickCtrl(float ctrl[15]) override;

    DECLARE_NODE_TRAITS(NodeLegacyControls)
};
