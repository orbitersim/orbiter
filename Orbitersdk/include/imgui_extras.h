#pragma once

#include "imgui.h"

// ImGui extras

enum class ImGuiFont {
	DEFAULT,
	MONO
};


namespace ImGui {

	OAPIFUNC bool SliderFloatReset(const char* label, float* v, float v_min, float v_max, float v_default, const char* display_format = "%.3f");
	OAPIFUNC void HelpMarker(const char* desc, bool sameline = true);
	OAPIFUNC void BeginGroupPanel(const char* name, const ImVec2& size = ImVec2(-1.0f, -1.0f));
	OAPIFUNC void EndGroupPanel();
	OAPIFUNC bool MenuButton(const char *label, const char *tooltip = NULL, float xoffset = 0.0f);
	OAPIFUNC void PushFont(ImGuiFont);
};
