#pragma once
#include "Node.h"

class NodeLegacyControls : public Node {
	InputPin<Pin::type::Button> ModKey;
	InputPin<Pin::type::Axis> MainEngine;
	InputPin<Pin::type::Axis> BankYaw;
	InputPin<Pin::type::Axis> PitchVert;
	InputPin<Pin::type::Axis> YawLat;
	InputPin<Pin::type::Hat>  CamPanel;

public:
	NodeLegacyControls();
	void Update(bool dryrun) override;
	void GetJoystickCtrl(float ctrl[15]);
	void RegisterPins() override;

	// Serialization
	static constexpr const char* TypeName = "NodeLegacyControls";
	const char *GetTypeName() const override { return TypeName; }
};
