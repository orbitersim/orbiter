#pragma once
#include "OrbiterSDK.h"
#include <windows.h>

class InputEditor: public ImGuiDialog {
public:
	InputEditor();
	~InputEditor();
	void OnDraw() override;
	DWORD m_cmd;
};
