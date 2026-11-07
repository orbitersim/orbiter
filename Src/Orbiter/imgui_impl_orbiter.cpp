#include "imgui_impl_orbiter.h"
#include "imgui.h"
#include "OrbiterPlatform.h"
#include <chrono>

namespace {

struct BackendData {
	HWND hWnd = nullptr;
	std::chrono::steady_clock::time_point time;
	ImGuiMouseCursor lastCursor = ImGuiMouseCursor_COUNT;
	int mouseButtonsDown = 0;
};

BackendData *Data()
{
	return ImGui::GetCurrentContext() ? (BackendData *)ImGui::GetIO().BackendPlatformUserData : nullptr;
}

ImGuiKey VkToKey(WPARAM vk)
{
	switch (vk) {
	case VK_TAB: return ImGuiKey_Tab;
	case VK_LEFT: return ImGuiKey_LeftArrow;
	case VK_RIGHT: return ImGuiKey_RightArrow;
	case VK_UP: return ImGuiKey_UpArrow;
	case VK_DOWN: return ImGuiKey_DownArrow;
	case VK_PRIOR: return ImGuiKey_PageUp;
	case VK_NEXT: return ImGuiKey_PageDown;
	case VK_HOME: return ImGuiKey_Home;
	case VK_END: return ImGuiKey_End;
	case VK_INSERT: return ImGuiKey_Insert;
	case VK_DELETE: return ImGuiKey_Delete;
	case VK_BACK: return ImGuiKey_Backspace;
	case VK_SPACE: return ImGuiKey_Space;
	case VK_RETURN: return ImGuiKey_Enter;
	case VK_ESCAPE: return ImGuiKey_Escape;
	case VK_OEM_7: return ImGuiKey_Apostrophe;
	case VK_OEM_COMMA: return ImGuiKey_Comma;
	case VK_OEM_MINUS: return ImGuiKey_Minus;
	case VK_OEM_PERIOD: return ImGuiKey_Period;
	case VK_OEM_2: return ImGuiKey_Slash;
	case VK_OEM_1: return ImGuiKey_Semicolon;
	case VK_OEM_PLUS: return ImGuiKey_Equal;
	case VK_OEM_4: return ImGuiKey_LeftBracket;
	case VK_OEM_5: return ImGuiKey_Backslash;
	case VK_OEM_6: return ImGuiKey_RightBracket;
	case VK_OEM_3: return ImGuiKey_GraveAccent;
	case VK_CAPITAL: return ImGuiKey_CapsLock;
	case VK_SCROLL: return ImGuiKey_ScrollLock;
	case VK_NUMLOCK: return ImGuiKey_NumLock;
	case VK_SNAPSHOT: return ImGuiKey_PrintScreen;
	case VK_PAUSE: return ImGuiKey_Pause;
	case VK_NUMPAD0: return ImGuiKey_Keypad0;
	case VK_NUMPAD1: return ImGuiKey_Keypad1;
	case VK_NUMPAD2: return ImGuiKey_Keypad2;
	case VK_NUMPAD3: return ImGuiKey_Keypad3;
	case VK_NUMPAD4: return ImGuiKey_Keypad4;
	case VK_NUMPAD5: return ImGuiKey_Keypad5;
	case VK_NUMPAD6: return ImGuiKey_Keypad6;
	case VK_NUMPAD7: return ImGuiKey_Keypad7;
	case VK_NUMPAD8: return ImGuiKey_Keypad8;
	case VK_NUMPAD9: return ImGuiKey_Keypad9;
	case VK_DECIMAL: return ImGuiKey_KeypadDecimal;
	case VK_DIVIDE: return ImGuiKey_KeypadDivide;
	case VK_MULTIPLY: return ImGuiKey_KeypadMultiply;
	case VK_SUBTRACT: return ImGuiKey_KeypadSubtract;
	case VK_ADD: return ImGuiKey_KeypadAdd;
	case VK_LSHIFT: return ImGuiKey_LeftShift;
	case VK_LCONTROL: return ImGuiKey_LeftCtrl;
	case VK_LMENU: return ImGuiKey_LeftAlt;
	case VK_LWIN: return ImGuiKey_LeftSuper;
	case VK_RSHIFT: return ImGuiKey_RightShift;
	case VK_RCONTROL: return ImGuiKey_RightCtrl;
	case VK_RMENU: return ImGuiKey_RightAlt;
	case VK_RWIN: return ImGuiKey_RightSuper;
	case VK_APPS: return ImGuiKey_Menu;
	case VK_F1: return ImGuiKey_F1;
	case VK_F2: return ImGuiKey_F2;
	case VK_F3: return ImGuiKey_F3;
	case VK_F4: return ImGuiKey_F4;
	case VK_F5: return ImGuiKey_F5;
	case VK_F6: return ImGuiKey_F6;
	case VK_F7: return ImGuiKey_F7;
	case VK_F8: return ImGuiKey_F8;
	case VK_F9: return ImGuiKey_F9;
	case VK_F10: return ImGuiKey_F10;
	case VK_F11: return ImGuiKey_F11;
	case VK_F12: return ImGuiKey_F12;
	}
	if (vk >= '0' && vk <= '9') return (ImGuiKey)(ImGuiKey_0 + (vk - '0'));
	if (vk >= 'A' && vk <= 'Z') return (ImGuiKey)(ImGuiKey_A + (vk - 'A'));
	return ImGuiKey_None;
}

void UpdateModifiers(ImGuiIO &io)
{
	io.AddKeyEvent(ImGuiMod_Ctrl, (GetKeyState(VK_CONTROL) & 0x8000) != 0);
	io.AddKeyEvent(ImGuiMod_Shift, (GetKeyState(VK_SHIFT) & 0x8000) != 0);
	io.AddKeyEvent(ImGuiMod_Alt, (GetKeyState(VK_MENU) & 0x8000) != 0);
	io.AddKeyEvent(ImGuiMod_Super, ((GetKeyState(VK_LWIN) | GetKeyState(VK_RWIN)) & 0x8000) != 0);
}

void UpdateCursor(BackendData *bd)
{
	ImGuiIO &io = ImGui::GetIO();
	if (io.ConfigFlags & ImGuiConfigFlags_NoMouseCursorChange) return;
	ImGuiMouseCursor c = io.MouseDrawCursor ? ImGuiMouseCursor_None : ImGui::GetMouseCursor();
	if (c == bd->lastCursor) return;
	bd->lastCursor = c;
	if (!io.WantCaptureMouse) return;
	switch (c) {
	case ImGuiMouseCursor_TextInput: oplat::SetCursorShape(oplat::CURSOR_IBEAM); break;
	case ImGuiMouseCursor_ResizeEW: oplat::SetCursorShape(oplat::CURSOR_RESIZE_EW); break;
	case ImGuiMouseCursor_ResizeNS: oplat::SetCursorShape(oplat::CURSOR_RESIZE_NS); break;
	case ImGuiMouseCursor_Hand: oplat::SetCursorShape(oplat::CURSOR_HAND); break;
	default: oplat::SetCursorShape(oplat::CURSOR_ARROW); break;
	}
}

const char *GetClipboard(ImGuiContext *)
{
	static char buf[65536];
	return OpenClipboardText(buf, sizeof(buf)) ? buf : "";
}

void SetClipboard(ImGuiContext *, const char *text)
{
	SetClipboardText(text);
}

}

bool ImGui_ImplOrbiter_Init(HWND hWnd)
{
	ImGuiIO &io = ImGui::GetIO();
	if (io.BackendPlatformUserData) return false;
	BackendData *bd = IM_NEW(BackendData)();
	bd->hWnd = hWnd;
	bd->time = std::chrono::steady_clock::now();
	io.BackendPlatformUserData = bd;
	io.BackendPlatformName = "imgui_impl_orbiter";
	io.BackendFlags |= ImGuiBackendFlags_HasMouseCursors;
	ImGuiPlatformIO &pio = ImGui::GetPlatformIO();
	pio.Platform_GetClipboardTextFn = GetClipboard;
	pio.Platform_SetClipboardTextFn = SetClipboard;
	return true;
}

void ImGui_ImplOrbiter_Shutdown()
{
	BackendData *bd = Data();
	if (!bd) return;
	ImGuiIO &io = ImGui::GetIO();
	io.BackendPlatformName = nullptr;
	io.BackendPlatformUserData = nullptr;
	io.BackendFlags &= ~ImGuiBackendFlags_HasMouseCursors;
	IM_DELETE(bd);
}

void ImGui_ImplOrbiter_NewFrame()
{
	BackendData *bd = Data();
	if (!bd) return;
	ImGuiIO &io = ImGui::GetIO();
	RECT r = {0, 0, 0, 0};
	GetClientRect(bd->hWnd, &r);
	io.DisplaySize = ImVec2((float)(r.right - r.left), (float)(r.bottom - r.top));
	io.DisplayFramebufferScale = ImVec2(1.0f, 1.0f);
	auto now = std::chrono::steady_clock::now();
	float dt = std::chrono::duration<float>(now - bd->time).count();
	io.DeltaTime = dt > 0.0f ? dt : 1.0f / 60.0f;
	bd->time = now;
	UpdateCursor(bd);
}

LRESULT ImGui_ImplOrbiter_WndProcHandler(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	BackendData *bd = Data();
	if (!bd || bd->hWnd != hWnd) return 0;
	ImGuiIO &io = ImGui::GetIO();

	switch (msg) {
	case WM_MOUSEMOVE:
		io.AddMouseSourceEvent(ImGuiMouseSource_Mouse);
		io.AddMousePosEvent((float)(short)LOWORD(lParam), (float)(short)HIWORD(lParam));
		return 0;
	case WM_MOUSELEAVE:
		io.AddMousePosEvent(-FLT_MAX, -FLT_MAX);
		return 0;
	case WM_LBUTTONDOWN: case WM_LBUTTONDBLCLK:
	case WM_RBUTTONDOWN: case WM_RBUTTONDBLCLK:
	case WM_MBUTTONDOWN: case WM_MBUTTONDBLCLK: {
		int b = (msg == WM_LBUTTONDOWN || msg == WM_LBUTTONDBLCLK) ? 0 : (msg == WM_RBUTTONDOWN || msg == WM_RBUTTONDBLCLK) ? 1 : 2;
		bd->mouseButtonsDown |= 1 << b;
		io.AddMousePosEvent((float)(short)LOWORD(lParam), (float)(short)HIWORD(lParam));
		io.AddMouseButtonEvent(b, true);
		return 0;
	}
	case WM_LBUTTONUP: case WM_RBUTTONUP: case WM_MBUTTONUP: {
		int b = msg == WM_LBUTTONUP ? 0 : msg == WM_RBUTTONUP ? 1 : 2;
		bd->mouseButtonsDown &= ~(1 << b);
		io.AddMouseButtonEvent(b, false);
		return 0;
	}
	case WM_MOUSEWHEEL:
		io.AddMouseWheelEvent(0.0f, (float)GET_WHEEL_DELTA_WPARAM(wParam) / (float)WHEEL_DELTA);
		return 0;
	case WM_MOUSEHWHEEL:
		io.AddMouseWheelEvent(-(float)GET_WHEEL_DELTA_WPARAM(wParam) / (float)WHEEL_DELTA, 0.0f);
		return 0;
	case WM_KEYDOWN: case WM_KEYUP: case WM_SYSKEYDOWN: case WM_SYSKEYUP: {
		bool down = (msg == WM_KEYDOWN || msg == WM_SYSKEYDOWN);
		UpdateModifiers(io);
		WPARAM vk = wParam;
		if (vk == VK_SHIFT) {
			io.AddKeyEvent(ImGuiKey_LeftShift, (GetKeyState(VK_LSHIFT) & 0x8000) != 0);
			io.AddKeyEvent(ImGuiKey_RightShift, (GetKeyState(VK_RSHIFT) & 0x8000) != 0);
		} else if (vk == VK_CONTROL) {
			io.AddKeyEvent(ImGuiKey_LeftCtrl, (GetKeyState(VK_LCONTROL) & 0x8000) != 0);
			io.AddKeyEvent(ImGuiKey_RightCtrl, (GetKeyState(VK_RCONTROL) & 0x8000) != 0);
		} else if (vk == VK_MENU) {
			io.AddKeyEvent(ImGuiKey_LeftAlt, (GetKeyState(VK_LMENU) & 0x8000) != 0);
			io.AddKeyEvent(ImGuiKey_RightAlt, (GetKeyState(VK_RMENU) & 0x8000) != 0);
		} else {
			ImGuiKey key = VkToKey(vk);
			if (key == ImGuiKey_Enter && (HIWORD(lParam) & 0x100)) key = ImGuiKey_KeypadEnter;
			if (key != ImGuiKey_None) io.AddKeyEvent(key, down);
		}
		return 0;
	}
	case WM_SETFOCUS: case WM_KILLFOCUS:
		io.AddFocusEvent(msg == WM_SETFOCUS);
		return 0;
	case WM_CHAR:
		if (wParam > 0 && wParam < 0x10000) io.AddInputCharacterUTF16((unsigned short)wParam);
		return 0;
	}
	return 0;
}
