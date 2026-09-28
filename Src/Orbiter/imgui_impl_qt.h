// not upstream: Dear ImGui platform backend for a Qt window (stands in for imgui_impl_win32)
// Feeds display size, time, mouse, keyboard, focus, cursor shape and clipboard; no multi-viewport support.

#pragma once
#include "imgui.h"

class QWindow;
class QEvent;

bool ImGui_ImplQt_Init (QWindow *window);
void ImGui_ImplQt_Shutdown ();
void ImGui_ImplQt_NewFrame ();

// ImGui_ImplWin32_WndProcHandler counterpart: call with every event of the render window;
// returns true if the backend consumed the event (the caller checks io.WantCapture* itself)
bool ImGui_ImplQt_EventHandler (QWindow *window, QEvent *event);
