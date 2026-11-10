#pragma once
#define IMGUI_DEFINE_MATH_OPERATORS
#include <imgui.h>

enum class IconType: ImU32 { Flow, Circle, Square, Grid, RoundSquare, Diamond, Trigger, Plus, ButtonUp, ButtonDown, };

void DrawIcon(ImDrawList* drawList, const ImVec2& a, const ImVec2& b, IconType type, bool filled, ImU32 color, ImU32 innerColor);
void Icon(const ImVec2& size, IconType type, bool filled, const ImVec4& color = ImVec4(1, 1, 1, 1), const ImVec4& innerColor = ImVec4(0, 0, 0, 0));

void DrawAxisGauge(ImDrawList* drawList, const ImVec2& a, const ImVec2& b, float value, ImU32 color);
void Axis(const ImVec2& size, float value, const ImVec4& color);
