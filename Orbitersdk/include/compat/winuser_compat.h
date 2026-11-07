#pragma once

#define SM_CXSCREEN 0
#define SM_CYSCREEN 1
#define SM_CXVSCROLL 2
#define SM_CYHSCROLL 3
#define SM_CYCAPTION 4
#define SM_CXBORDER 5
#define SM_CYBORDER 6
#define SM_CXDLGFRAME 7
#define SM_CYDLGFRAME 8
#define SM_CXFRAME 32
#define SM_CYFRAME 33
#define SM_CXFIXEDFRAME SM_CXDLGFRAME
#define SM_CYFIXEDFRAME SM_CYDLGFRAME
#define SM_CXSIZEFRAME SM_CXFRAME
#define SM_CYSIZEFRAME SM_CYFRAME
#define SM_CYMENU 15
#define SM_CXFULLSCREEN 16
#define SM_CYFULLSCREEN 17
#define SM_CMONITORS 80
#define SM_CXVIRTUALSCREEN 78
#define SM_CYVIRTUALSCREEN 79
#define SM_XVIRTUALSCREEN 76
#define SM_YVIRTUALSCREEN 77
#define SM_CXMAXIMIZED 61
#define SM_CYMAXIMIZED 62

OCOMPAT_API int GetSystemMetrics(int index);
OCOMPAT_API WINBOOL GetCursorPos(POINT *pt);
OCOMPAT_API WINBOOL SetCursorPos(int x, int y);
OCOMPAT_API int ShowCursor(WINBOOL show);
OCOMPAT_API SHORT GetAsyncKeyState(int vkey);
OCOMPAT_API SHORT GetKeyState(int vkey);
OCOMPAT_API WINBOOL ScreenToClient(HWND hWnd, POINT *pt);
OCOMPAT_API WINBOOL ClientToScreen(HWND hWnd, POINT *pt);
OCOMPAT_API WINBOOL GetClientRect(HWND hWnd, RECT *rect);
OCOMPAT_API WINBOOL GetWindowRect(HWND hWnd, RECT *rect);
OCOMPAT_API WINBOOL IsWindow(HWND hWnd);
OCOMPAT_API WINBOOL IsWindowVisible(HWND hWnd);
OCOMPAT_API WINBOOL PostMessageA(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
#define PostMessage PostMessageA
OCOMPAT_API LRESULT SendMessageA(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
#define SendMessage SendMessageA
OCOMPAT_API void PostQuitMessage(int code);
OCOMPAT_API WINBOOL SetWindowTextA(HWND hWnd, const char *text);
#define SetWindowText SetWindowTextA
OCOMPAT_API HWND GetFocus(void);
OCOMPAT_API HWND SetFocus(HWND hWnd);
OCOMPAT_API HWND GetForegroundWindow(void);
OCOMPAT_API WINBOOL SetForegroundWindow(HWND hWnd);
OCOMPAT_API WINBOOL ShowWindow(HWND hWnd, int cmd);
OCOMPAT_API WINBOOL DestroyWindow(HWND hWnd);

OCOMPAT_API WINBOOL OpenClipboardText(char *buf, size_t size);
OCOMPAT_API WINBOOL SetClipboardText(const char *text);
OCOMPAT_API LRESULT DefWindowProcA(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
#define DefWindowProc DefWindowProcA
OCOMPAT_API LRESULT CallWindowProcA(WNDPROC proc, HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
#define CallWindowProc CallWindowProcA
OCOMPAT_API HWND SetCapture(HWND hWnd);
OCOMPAT_API WINBOOL ReleaseCapture(void);
OCOMPAT_API HWND GetCapture(void);
