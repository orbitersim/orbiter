#pragma once
#include "Node.h"

class NodeLegacyControls : public Node {
public:
	NodeLegacyControls(int id);
	void Update(bool dryrun) override;
};
