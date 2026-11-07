#pragma once

#include <windows.h>
#include <string>
#include <vector>
#include <functional>

namespace oplat {

struct WindowDesc {
	std::string title;
	int width = 1280;
	int height = 800;
	int x = -1;
	int y = -1;
	bool fullscreen = false;
	bool resizable = true;
	bool visible = true;
	bool borderless = false;
	bool metal = true;
	WNDPROC proc = nullptr;
	void *userData = nullptr;
};

struct KeyEvent {
	DWORD key;
	bool down;
};

struct DisplayMode {
	int width;
	int height;
	double refresh;
};

__attribute__((visibility("default"))) void Init();
__attribute__((visibility("default"))) HWND OpenWindow(const WindowDesc &desc);
__attribute__((visibility("default"))) void SetWindowProc(HWND hWnd, WNDPROC proc);
__attribute__((visibility("default"))) WNDPROC GetWindowProc(HWND hWnd);
__attribute__((visibility("default"))) void SetWindowUserData(HWND hWnd, void *data);
__attribute__((visibility("default"))) void *GetWindowUserData(HWND hWnd);
__attribute__((visibility("default"))) void *GetMetalLayer(HWND hWnd);
__attribute__((visibility("default"))) float GetBackingScale(HWND hWnd);
__attribute__((visibility("default"))) void GetDrawableSize(HWND hWnd, int *w, int *h);
__attribute__((visibility("default"))) void SetWindowClientSize(HWND hWnd, int w, int h);
__attribute__((visibility("default"))) void SetWindowPosition(HWND hWnd, int x, int y);
__attribute__((visibility("default"))) bool IsFullscreen(HWND hWnd);
__attribute__((visibility("default"))) void SetFullscreen(HWND hWnd, bool fullscreen);
__attribute__((visibility("default"))) void SetMinClientSize(HWND hWnd, int w, int h);

__attribute__((visibility("default"))) bool PumpEvents(bool wait);
__attribute__((visibility("default"))) bool QuitRequested();
__attribute__((visibility("default"))) int QuitCode();
__attribute__((visibility("default"))) void WakeEventLoop();

__attribute__((visibility("default"))) const BYTE *KeyState();
__attribute__((visibility("default"))) void TakeKeyEvents(std::vector<KeyEvent> &events);
__attribute__((visibility("default"))) void ClearKeyState();
__attribute__((visibility("default"))) void SetKeyCaptureWindow(HWND hWnd);

__attribute__((visibility("default"))) void SetCursorHidden(bool hidden);
__attribute__((visibility("default"))) void SetRelativeMouse(bool enabled);
__attribute__((visibility("default"))) void SetCursorShape(int shape);

__attribute__((visibility("default"))) std::vector<DisplayMode> DisplayModes();
__attribute__((visibility("default"))) DisplayMode CurrentDisplayMode();

__attribute__((visibility("default"))) std::string AppSupportDir();
__attribute__((visibility("default"))) std::string ResourceDir();
__attribute__((visibility("default"))) bool OpenURL(const std::string &url);
__attribute__((visibility("default"))) bool RevealInFinder(const std::string &path);
__attribute__((visibility("default"))) std::string OpenFileDialog(const std::string &title, const std::string &dir, const std::vector<std::string> &extensions);
__attribute__((visibility("default"))) std::string SaveFileDialog(const std::string &title, const std::string &dir, const std::string &name, const std::vector<std::string> &extensions);

enum CursorShape { CURSOR_ARROW = 0, CURSOR_WAIT, CURSOR_CROSS, CURSOR_HAND, CURSOR_IBEAM, CURSOR_RESIZE_EW, CURSOR_RESIZE_NS };

}
