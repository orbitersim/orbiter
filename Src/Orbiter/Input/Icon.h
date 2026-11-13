#pragma once
#define IMGUI_DEFINE_MATH_OPERATORS
#include <imgui.h>

enum class IconType: ImU32 { Flow, Circle, Square, Grid, RoundSquare, Diamond, Trigger, Plus, ButtonUp, ButtonDown, };

void Icon(const ImVec2& size, IconType type, bool filled, const ImVec4& color = ImVec4(1, 1, 1, 1), const ImVec4& innerColor = ImVec4(0, 0, 0, 0));
void Axis(const ImVec2& size, float value, const ImVec4& color);
void Hat(const ImVec2& size, unsigned char mask, const ImVec4& color);
