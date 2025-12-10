#pragma once

#include "imgui.h"

// ImGui extras

enum class ImGuiFont {
	DEFAULT,
	MONO,
	CONSOLE,
	MANUSCRIPT
};


namespace ImGui {
	OAPIFUNC bool SliderFloatReset(const char* label, float* v, float v_min, float v_max, float v_default, const char* display_format = "%.3f");
	OAPIFUNC bool SliderDouble(const char* label, double* v, double v_min, double v_max, const char* display_format = "%.3f");
	OAPIFUNC bool InputIntEx(const char* label, int* v, int v_min, int v_max, int step = 1, int step_fast = 100, ImGuiInputTextFlags flags = 0);
	OAPIFUNC bool InputDoubleEx(const char* label, double* v, double v_min, double v_max, double step = 1.0f, double step_fast = 100.0f, const char *fmt = "%.3f", ImGuiInputTextFlags flags = 0);
	OAPIFUNC void HelpMarker(const char* desc, bool sameline = true);
	OAPIFUNC bool MenuButton(const char *label, const char *tooltip = NULL, float xoffset = 0.0f);
	OAPIFUNC void PushFont(ImGuiFont, float scale = 1.0);
	OAPIFUNC ImTextureID GetImTextureID (SURFHANDLE surf);
	OAPIFUNC void Image(SURFHANDLE surf, const ImVec2& image_size, const ImVec2& uv0 = ImVec2(0, 0), const ImVec2& uv1 = ImVec2(1, 1), const ImVec4& tint_col = ImVec4(1, 1, 1, 1), const ImVec4& border_col = ImVec4(0, 0, 0, 0));
	OAPIFUNC bool ImageButton(const char* str_id, SURFHANDLE surf, const ImVec2& image_size, const ImVec2& uv0 = ImVec2(0, 0), const ImVec2& uv1 = ImVec2(1, 1), const ImVec4& bg_col = ImVec4(0, 0, 0, 0), const ImVec4& tint_col = ImVec4(1, 1, 1, 1));
	OAPIFUNC bool SliderEnum(const char *label, int *v, const char *values[], int nvalues);
	OAPIFUNC bool BeginAnimatedPopup(const char* name, float duration = 0.25f);
	OAPIFUNC void EndAnimatedPopup();
	OAPIFUNC bool BeginAnimatedCombo(const char* label, const char* preview_value, ImGuiComboFlags flags = 0, float duration = 0.25f);
	OAPIFUNC void EndAnimatedCombo();
	OAPIFUNC bool BeginAnimatedCollapsingHeader(const char* label, ImGuiTreeNodeFlags flags = 0, float speed = 1000.0f);
	OAPIFUNC void EndAnimatedCollapsingHeader();
};

