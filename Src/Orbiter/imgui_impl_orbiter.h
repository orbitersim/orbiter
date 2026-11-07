#pragma once

#include <windows.h>

bool ImGui_ImplOrbiter_Init(HWND hWnd);
void ImGui_ImplOrbiter_Shutdown();
void ImGui_ImplOrbiter_NewFrame();
LRESULT ImGui_ImplOrbiter_WndProcHandler(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
