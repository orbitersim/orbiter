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
__attribute__((visibility("default"))) HWND OpenBackdropWindow();
__attribute__((visibility("default"))) void SetWindowProc(HWND hWnd, WNDPROC proc);
__attribute__((visibility("default"))) WNDPROC GetWindowProc(HWND hWnd);
__attribute__((visibility("default"))) void SetWindowUserData(HWND hWnd, void *data);
__attribute__((visibility("default"))) void *GetWindowUserData(HWND hWnd);
__attribute__((visibility("default"))) void *GetMetalLayer(HWND hWnd);
__attribute__((visibility("default"))) float GetBackingScale(HWND hWnd);
__attribute__((visibility("default"))) void GetDrawableSize(HWND hWnd, int *w, int *h);
__attribute__((visibility("default"))) void SetWindowClientSize(HWND hWnd, int w, int h);
__attribute__((visibility("default"))) void SetWindowPosition(HWND hWnd, int x, int y);
struct Monitor {
	int x, y, w, h;
	int workX, workY, workW, workH;
	float scale;
};
__attribute__((visibility("default"))) void *NativeWindow(HWND hWnd);
__attribute__((visibility("default"))) void GetClientScreenRect(HWND hWnd, int *x, int *y, int *w, int *h);
__attribute__((visibility("default"))) void SetClientScreenRect(HWND hWnd, int x, int y, int w, int h);
__attribute__((visibility("default"))) void ShowWindowNoActivate(HWND hWnd);
__attribute__((visibility("default"))) void FocusWindow(HWND hWnd);
__attribute__((visibility("default"))) bool IsWindowFocused(HWND hWnd);
__attribute__((visibility("default"))) bool IsWindowMinimized(HWND hWnd);
__attribute__((visibility("default"))) void SetWindowAlpha(HWND hWnd, float alpha);
__attribute__((visibility("default"))) void AttachChildWindow(HWND parent, HWND child);
__attribute__((visibility("default"))) std::vector<Monitor> Monitors();
__attribute__((visibility("default"))) void CursorScreenPos(int *x, int *y);
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

namespace oplat {

struct Image {
	int width = 0;
	int height = 0;
	std::vector<uint8_t> bgra;
};

enum ImageFileType { IMGFILE_BMP = 0, IMGFILE_PNG, IMGFILE_JPG, IMGFILE_TIF };

__attribute__((visibility("default"))) bool LoadImageFile(const std::string &path, Image &img, int width = 0, int height = 0);
__attribute__((visibility("default"))) bool LoadImageMemory(const void *data, size_t size, Image &img, int width = 0, int height = 0);
__attribute__((visibility("default"))) bool SaveImageFile(const std::string &path, const uint8_t *pixels, int width, int height, int stride, int bpp, ImageFileType type, float quality);
__attribute__((visibility("default"))) bool CopyImageToClipboard(const uint8_t *pixels, int width, int height, int stride, int bpp);

}

namespace oplat {

// Read the category and description an Orbiter module declares with
// ORBITER_MODULE_INFO, without loading the module.
__attribute__((visibility("default"))) bool ReadModuleInfo(const std::string &path, std::string &category, std::string &description);

}

namespace oplat {

// Show a help page in the help window. 'dir' is the folder holding the pages,
// 'page' the page (with optional #anchor) relative to it, 'toc' an optional
// HTML Help contents file (.hhc) used for the sidebar.
__attribute__((visibility("default"))) void ShowHelp(const std::string &dir, const std::string &page, const std::string &toc, const std::string &title);

}

namespace oplat {

enum ConsoleMode { CONSOLE_OUTPUT_ONLY, CONSOLE_TERMINAL, CONSOLE_WINDOW };

// Text console used when Orbiter runs without a graphics engine.
__attribute__((visibility("default"))) void ConsoleOpen(const std::string &title, ConsoleMode mode);
__attribute__((visibility("default"))) void ConsoleClose();
__attribute__((visibility("default"))) void ConsoleWrite(const std::string &text);
__attribute__((visibility("default"))) bool ConsolePoll(std::string &line);
__attribute__((visibility("default"))) void ConsoleSetStatus(const std::string &status);
__attribute__((visibility("default"))) bool StdinIsTerminal();

}
