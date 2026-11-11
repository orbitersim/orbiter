#pragma once
#include "OrbiterSDK.h"
#include <windows.h>
#include <vector>

class Node;
class ControllerGraph;

class InputEditor: public ImGuiDialog {
public:
	InputEditor();
	~InputEditor();
	void OnDraw() override;
	DWORD m_cmd;
	ControllerGraph *graph;
};
