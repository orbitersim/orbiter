#pragma once
#include "Node.h"

class NodeLegacyControls : public Node {
public:
	NodeLegacyControls();
	void Update(bool dryrun) override;
	void GetJoystickCtrl(float ctrl[15]);
	void InitPins() override;

	// Serialization
	static constexpr const char* TypeName = "NodeLegacyControls";
	const char *GetTypeName() const override { return TypeName; }
};
