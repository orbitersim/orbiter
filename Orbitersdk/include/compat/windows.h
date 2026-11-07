#pragma once

#ifndef _WIN32

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <wchar.h>
#include <ctype.h>
#include <unistd.h>
#include <math.h>
#include <time.h>
#include <assert.h>
#include <errno.h>

#if defined(__cplusplus)
#include <cstring>
#include <cstdio>
#endif

#define ORBITER_PLATFORM_MAC 1

#ifdef __cplusplus
#define OCOMPAT_EXTERN extern "C"
#else
#define OCOMPAT_EXTERN extern
#endif
#define OCOMPAT_API OCOMPAT_EXTERN __attribute__((visibility("default")))

#define WINAPI
#define CALLBACK
#define APIENTRY
#define WINAPIV
#define STDMETHODCALLTYPE
#define __stdcall
#define __cdecl
#define __fastcall
#define _cdecl
#define FAR
#define NEAR
#define IN
#define OUT
#define OPTIONAL
#define CONST const
#define VOID void
#define UNREFERENCED_PARAMETER(P) (void)(P)
#define __forceinline inline __attribute__((always_inline))
#define __assume(x) ((void)0)
#define _In_
#define _In_opt_
#define _Out_
#define _Out_opt_
#define _Inout_
#define _In_z_
#define _Printf_format_string_

typedef uint8_t BYTE;
typedef uint8_t UCHAR;
typedef uint8_t BOOLEAN;
typedef uint16_t WORD;
typedef uint16_t USHORT;
typedef uint32_t DWORD;
typedef uint32_t UINT;
typedef uint32_t ULONG;
typedef int32_t LONG;
typedef int32_t INT;
typedef int32_t WINBOOL;
#ifndef __OBJC__
typedef WINBOOL BOOL;
#endif
typedef int16_t SHORT;
typedef char CHAR;
typedef char TCHAR;
typedef wchar_t WCHAR;
typedef float FLOAT;
typedef double DOUBLE;
typedef int8_t INT8;
typedef int16_t INT16;
typedef int32_t INT32;
typedef int64_t INT64;
typedef uint8_t UINT8;
typedef uint16_t UINT16;
typedef uint32_t UINT32;
typedef uint64_t UINT64;
typedef uint64_t DWORD64;
typedef uint64_t QWORD;
typedef int64_t LONGLONG;
typedef uint64_t ULONGLONG;
typedef uint64_t DWORDLONG;
typedef int64_t LONG64;
typedef uint64_t ULONG64;
typedef int64_t __int64;
typedef int32_t __int32;
typedef int16_t __int16;
typedef int8_t __int8;
typedef intptr_t INT_PTR;
typedef uintptr_t UINT_PTR;
typedef intptr_t LONG_PTR;
typedef uintptr_t ULONG_PTR;
typedef uintptr_t DWORD_PTR;
typedef size_t SIZE_T;
typedef intptr_t SSIZE_T;
typedef int32_t HRESULT;
typedef uintptr_t WPARAM;
typedef intptr_t LPARAM;
typedef intptr_t LRESULT;
typedef DWORD COLORREF;
typedef WORD ATOM;
typedef DWORD LCID;

typedef void *PVOID;
typedef void *LPVOID;
typedef const void *LPCVOID;
typedef char *LPSTR;
typedef char *PSTR;
typedef const char *LPCSTR;
typedef const char *PCSTR;
typedef char *LPTSTR;
typedef const char *LPCTSTR;
typedef wchar_t *LPWSTR;
typedef const wchar_t *LPCWSTR;
typedef BYTE *LPBYTE;
typedef BYTE *PBYTE;
typedef WORD *LPWORD;
typedef DWORD *LPDWORD;
typedef DWORD *PDWORD;
typedef LONG *LPLONG;
typedef WINBOOL *LPBOOL;
typedef int *LPINT;
typedef UINT *LPUINT;
typedef float *LPFLOAT;
typedef COLORREF *LPCOLORREF;

#define DECLARE_OHANDLE(name) struct name##__; typedef struct name##__ *name
typedef void *HANDLE;
typedef HANDLE *PHANDLE;
typedef HANDLE *LPHANDLE;
DECLARE_OHANDLE(HWND);
DECLARE_OHANDLE(HINSTANCE);
DECLARE_OHANDLE(HDC);
DECLARE_OHANDLE(HBITMAP);
DECLARE_OHANDLE(HFONT);
DECLARE_OHANDLE(HPEN);
DECLARE_OHANDLE(HBRUSH);
DECLARE_OHANDLE(HICON);
DECLARE_OHANDLE(HMENU);
DECLARE_OHANDLE(HKEY);
DECLARE_OHANDLE(HRGN);
DECLARE_OHANDLE(HPALETTE);
DECLARE_OHANDLE(HMONITOR);
DECLARE_OHANDLE(HGLRC);
DECLARE_OHANDLE(HACCEL);
DECLARE_OHANDLE(HTREEITEM);
DECLARE_OHANDLE(HIMAGELIST);
DECLARE_OHANDLE(HDROP);
typedef HICON HCURSOR;
typedef HINSTANCE HMODULE;
typedef void *HGDIOBJ;
typedef void *HGLOBAL;
typedef void *HLOCAL;
typedef void *HRSRC;
typedef HKEY *PHKEY;

#define TRUE 1
#define FALSE 0
#ifndef NULL
#define NULL 0
#endif
#define MAX_PATH 260
#define _MAX_PATH 260
#define _MAX_DRIVE 3
#define _MAX_DIR 256
#define _MAX_FNAME 256
#define _MAX_EXT 256
#define INFINITE 0xFFFFFFFF
#define INVALID_HANDLE_VALUE ((HANDLE)(intptr_t)-1)
#define MAXDWORD 0xffffffff
#define MAXLONG 0x7fffffff
#define MAXINT 0x7fffffff
#define MAXINT32 0x7fffffff
#define MININT ((int)0x80000000)
#define MAXUINT32 0xffffffffu

typedef struct tagRECT { LONG left, top, right, bottom; } RECT, *PRECT, *LPRECT;
typedef const RECT *LPCRECT;
typedef struct tagPOINT { LONG x, y; } POINT, *PPOINT, *LPPOINT;
typedef struct tagSIZE { LONG cx, cy; } SIZE, *PSIZE, *LPSIZE;
typedef struct tagPOINTS { SHORT x, y; } POINTS;
typedef struct tagMSG { HWND hwnd; UINT message; WPARAM wParam; LPARAM lParam; DWORD time; POINT pt; } MSG, *LPMSG;
typedef struct _FILETIME { DWORD dwLowDateTime, dwHighDateTime; } FILETIME;
typedef struct _SYSTEMTIME { WORD wYear, wMonth, wDayOfWeek, wDay, wHour, wMinute, wSecond, wMilliseconds; } SYSTEMTIME;
typedef struct _GUID { uint32_t Data1; uint16_t Data2; uint16_t Data3; uint8_t Data4[8]; } GUID;
typedef GUID IID;
typedef GUID CLSID;
#ifdef __cplusplus
#define REFGUID const GUID &
#define REFIID const IID &
#define REFCLSID const CLSID &
#else
#define REFGUID const GUID *
#define REFIID const IID *
#define REFCLSID const CLSID *
#endif
typedef union _LARGE_INTEGER { struct { DWORD LowPart; LONG HighPart; }; LONGLONG QuadPart; } LARGE_INTEGER;
typedef union _ULARGE_INTEGER { struct { DWORD LowPart; DWORD HighPart; }; ULONGLONG QuadPart; } ULARGE_INTEGER;
typedef struct tagRGBQUAD { BYTE rgbBlue, rgbGreen, rgbRed, rgbReserved; } RGBQUAD;
typedef struct tagPALETTEENTRY { BYTE peRed, peGreen, peBlue, peFlags; } PALETTEENTRY;

#pragma pack(push, 2)
typedef struct tagBITMAPFILEHEADER { WORD bfType; DWORD bfSize; WORD bfReserved1, bfReserved2; DWORD bfOffBits; } BITMAPFILEHEADER;
#pragma pack(pop)
typedef struct tagBITMAPINFOHEADER { DWORD biSize; LONG biWidth, biHeight; WORD biPlanes, biBitCount; DWORD biCompression, biSizeImage; LONG biXPelsPerMeter, biYPelsPerMeter; DWORD biClrUsed, biClrImportant; } BITMAPINFOHEADER;
typedef struct tagBITMAPINFO { BITMAPINFOHEADER bmiHeader; RGBQUAD bmiColors[1]; } BITMAPINFO;
typedef struct tagBITMAP { LONG bmType, bmWidth, bmHeight, bmWidthBytes; WORD bmPlanes, bmBitsPixel; LPVOID bmBits; } BITMAP;
#define BI_RGB 0
#define BI_BITFIELDS 3

typedef INT_PTR (*FARPROC)();
typedef INT_PTR (*DLGPROC)(HWND, UINT, WPARAM, LPARAM);
typedef LRESULT (*WNDPROC)(HWND, UINT, WPARAM, LPARAM);
typedef void (*TIMERPROC)(HWND, UINT, UINT_PTR, DWORD);
typedef DWORD (*LPTHREAD_START_ROUTINE)(LPVOID);

#define LOBYTE(w) ((BYTE)(((DWORD_PTR)(w)) & 0xff))
#define HIBYTE(w) ((BYTE)((((DWORD_PTR)(w)) >> 8) & 0xff))
#define LOWORD(l) ((WORD)(((DWORD_PTR)(l)) & 0xffff))
#define HIWORD(l) ((WORD)((((DWORD_PTR)(l)) >> 16) & 0xffff))
#define MAKEWORD(a, b) ((WORD)(((BYTE)(a)) | ((WORD)((BYTE)(b))) << 8))
#define MAKELONG(a, b) ((LONG)(((WORD)(a)) | ((DWORD)((WORD)(b))) << 16))
#define MAKEWPARAM(l, h) ((WPARAM)(DWORD)MAKELONG(l, h))
#define MAKELPARAM(l, h) ((LPARAM)(DWORD)MAKELONG(l, h))
#define MAKELRESULT(l, h) ((LRESULT)(DWORD)MAKELONG(l, h))
#define RGB(r, g, b) ((COLORREF)(((BYTE)(r) | ((WORD)((BYTE)(g)) << 8)) | (((DWORD)(BYTE)(b)) << 16)))
#define GetRValue(rgb) (LOBYTE(rgb))
#define GetGValue(rgb) (LOBYTE(((WORD)(rgb)) >> 8))
#define GetBValue(rgb) (LOBYTE((rgb) >> 16))
#define MAKEINTRESOURCE(i) ((LPSTR)((ULONG_PTR)((WORD)(i))))
#define MAKEINTRESOURCEA MAKEINTRESOURCE
#define IS_INTRESOURCE(r) ((((ULONG_PTR)(r)) >> 16) == 0)
#define TEXT(s) s
#define _T(s) s

#ifndef S_OK
#define S_OK ((HRESULT)0)
#endif
#ifndef S_FALSE
#define S_FALSE ((HRESULT)1)
#endif
#ifndef E_FAIL
#define E_FAIL ((HRESULT)0x80004005)
#endif
#ifndef E_NOTIMPL
#define E_NOTIMPL ((HRESULT)0x80004001)
#endif
#ifndef E_INVALIDARG
#define E_INVALIDARG ((HRESULT)0x80070057)
#endif
#ifndef E_OUTOFMEMORY
#define E_OUTOFMEMORY ((HRESULT)0x8007000E)
#endif
#ifndef E_POINTER
#define E_POINTER ((HRESULT)0x80004003)
#endif
#ifndef SUCCEEDED
#define SUCCEEDED(hr) (((HRESULT)(hr)) >= 0)
#endif
#ifndef FAILED
#define FAILED(hr) (((HRESULT)(hr)) < 0)
#endif
#ifndef MAKE_HRESULT
#define MAKE_HRESULT(sev, fac, code) ((HRESULT)(((unsigned long)(sev) << 31) | ((unsigned long)(fac) << 16) | ((unsigned long)(code))))
#endif
#define ERROR_SUCCESS 0L
#define NO_ERROR 0L

#define ZeroMemory(d, l) memset((d), 0, (l))
#define SecureZeroMemory(d, l) memset((d), 0, (l))
#define CopyMemory(d, s, l) memcpy((d), (s), (l))
#define MoveMemory(d, s, l) memmove((d), (s), (l))
#define FillMemory(d, l, f) memset((d), (f), (l))
#define _countof(a) (sizeof(a) / sizeof((a)[0]))
#define ARRAYSIZE(a) _countof(a)

#define WM_NULL 0x0000
#define WM_CREATE 0x0001
#define WM_DESTROY 0x0002
#define WM_MOVE 0x0003
#define WM_SIZE 0x0005
#define WM_ACTIVATE 0x0006
#define WM_SETFOCUS 0x0007
#define WM_KILLFOCUS 0x0008
#define WM_ENABLE 0x000A
#define WM_SETREDRAW 0x000B
#define WM_SETTEXT 0x000C
#define WM_GETTEXT 0x000D
#define WM_GETTEXTLENGTH 0x000E
#define WM_PAINT 0x000F
#define WM_CLOSE 0x0010
#define WM_QUIT 0x0012
#define WM_ERASEBKGND 0x0014
#define WM_SHOWWINDOW 0x0018
#define WM_ACTIVATEAPP 0x001C
#define WM_SETCURSOR 0x0020
#define WM_MOUSEACTIVATE 0x0021
#define WM_GETMINMAXINFO 0x0024
#define WM_SETFONT 0x0030
#define WM_GETFONT 0x0031
#define WM_WINDOWPOSCHANGING 0x0046
#define WM_WINDOWPOSCHANGED 0x0047
#define WM_NOTIFY 0x004E
#define WM_HELP 0x0053
#define WM_NCCREATE 0x0081
#define WM_NCDESTROY 0x0082
#define WM_NCHITTEST 0x0084
#define WM_NCPAINT 0x0085
#define WM_NCACTIVATE 0x0086
#define WM_NCMOUSEMOVE 0x00A0
#define WM_NCLBUTTONDOWN 0x00A1
#define WM_NCLBUTTONUP 0x00A2
#define WM_NCLBUTTONDBLCLK 0x00A3
#define WM_NCRBUTTONDOWN 0x00A4
#define WM_KEYFIRST 0x0100
#define WM_KEYDOWN 0x0100
#define WM_KEYUP 0x0101
#define WM_CHAR 0x0102
#define WM_DEADCHAR 0x0103
#define WM_SYSKEYDOWN 0x0104
#define WM_SYSKEYUP 0x0105
#define WM_SYSCHAR 0x0106
#define WM_UNICHAR 0x0109
#define WM_KEYLAST 0x0109
#define WM_INITDIALOG 0x0110
#define WM_COMMAND 0x0111
#define WM_SYSCOMMAND 0x0112
#define WM_TIMER 0x0113
#define WM_HSCROLL 0x0114
#define WM_VSCROLL 0x0115
#define WM_INITMENU 0x0116
#define WM_MENUSELECT 0x011F
#define WM_ENTERIDLE 0x0121
#define WM_CTLCOLORMSGBOX 0x0132
#define WM_CTLCOLOREDIT 0x0133
#define WM_CTLCOLORLISTBOX 0x0134
#define WM_CTLCOLORBTN 0x0135
#define WM_CTLCOLORDLG 0x0136
#define WM_CTLCOLORSCROLLBAR 0x0137
#define WM_CTLCOLORSTATIC 0x0138
#define WM_MOUSEFIRST 0x0200
#define WM_MOUSEMOVE 0x0200
#define WM_LBUTTONDOWN 0x0201
#define WM_LBUTTONUP 0x0202
#define WM_LBUTTONDBLCLK 0x0203
#define WM_RBUTTONDOWN 0x0204
#define WM_RBUTTONUP 0x0205
#define WM_RBUTTONDBLCLK 0x0206
#define WM_MBUTTONDOWN 0x0207
#define WM_MBUTTONUP 0x0208
#define WM_MBUTTONDBLCLK 0x0209
#define WM_MOUSEWHEEL 0x020A
#define WM_XBUTTONDOWN 0x020B
#define WM_XBUTTONUP 0x020C
#define WM_MOUSEHWHEEL 0x020E
#define WM_MOUSELAST 0x020E
#define WM_SIZING 0x0214
#define WM_CAPTURECHANGED 0x0215
#define WM_MOVING 0x0216
#define WM_ENTERSIZEMOVE 0x0231
#define WM_EXITSIZEMOVE 0x0232
#define WM_DROPFILES 0x0233
#define WM_MOUSEHOVER 0x02A1
#define WM_MOUSELEAVE 0x02A3
#define WM_COPY 0x0301
#define WM_PASTE 0x0302
#define WM_USER 0x0400
#define WM_APP 0x8000

#define MK_LBUTTON 0x0001
#define MK_RBUTTON 0x0002
#define MK_SHIFT 0x0004
#define MK_CONTROL 0x0008
#define MK_MBUTTON 0x0010
#define WHEEL_DELTA 120
#define GET_WHEEL_DELTA_WPARAM(wParam) ((short)HIWORD(wParam))
#define GET_KEYSTATE_WPARAM(wParam) (LOWORD(wParam))

#define SIZE_RESTORED 0
#define SIZE_MINIMIZED 1
#define SIZE_MAXIMIZED 2
#define WA_INACTIVE 0
#define WA_ACTIVE 1
#define WA_CLICKACTIVE 2

#define VK_LBUTTON 0x01
#define VK_RBUTTON 0x02
#define VK_CANCEL 0x03
#define VK_MBUTTON 0x04
#define VK_BACK 0x08
#define VK_TAB 0x09
#define VK_CLEAR 0x0C
#define VK_RETURN 0x0D
#define VK_SHIFT 0x10
#define VK_CONTROL 0x11
#define VK_MENU 0x12
#define VK_PAUSE 0x13
#define VK_CAPITAL 0x14
#define VK_ESCAPE 0x1B
#define VK_SPACE 0x20
#define VK_PRIOR 0x21
#define VK_NEXT 0x22
#define VK_END 0x23
#define VK_HOME 0x24
#define VK_LEFT 0x25
#define VK_UP 0x26
#define VK_RIGHT 0x27
#define VK_DOWN 0x28
#define VK_SELECT 0x29
#define VK_PRINT 0x2A
#define VK_SNAPSHOT 0x2C
#define VK_INSERT 0x2D
#define VK_DELETE 0x2E
#define VK_HELP 0x2F
#define VK_LWIN 0x5B
#define VK_RWIN 0x5C
#define VK_APPS 0x5D
#define VK_NUMPAD0 0x60
#define VK_NUMPAD1 0x61
#define VK_NUMPAD2 0x62
#define VK_NUMPAD3 0x63
#define VK_NUMPAD4 0x64
#define VK_NUMPAD5 0x65
#define VK_NUMPAD6 0x66
#define VK_NUMPAD7 0x67
#define VK_NUMPAD8 0x68
#define VK_NUMPAD9 0x69
#define VK_MULTIPLY 0x6A
#define VK_ADD 0x6B
#define VK_SEPARATOR 0x6C
#define VK_SUBTRACT 0x6D
#define VK_DECIMAL 0x6E
#define VK_DIVIDE 0x6F
#define VK_F1 0x70
#define VK_F2 0x71
#define VK_F3 0x72
#define VK_F4 0x73
#define VK_F5 0x74
#define VK_F6 0x75
#define VK_F7 0x76
#define VK_F8 0x77
#define VK_F9 0x78
#define VK_F10 0x79
#define VK_F11 0x7A
#define VK_F12 0x7B
#define VK_NUMLOCK 0x90
#define VK_SCROLL 0x91
#define VK_LSHIFT 0xA0
#define VK_RSHIFT 0xA1
#define VK_LCONTROL 0xA2
#define VK_RCONTROL 0xA3
#define VK_LMENU 0xA4
#define VK_RMENU 0xA5
#define VK_OEM_1 0xBA
#define VK_OEM_PLUS 0xBB
#define VK_OEM_COMMA 0xBC
#define VK_OEM_MINUS 0xBD
#define VK_OEM_PERIOD 0xBE
#define VK_OEM_2 0xBF
#define VK_OEM_3 0xC0
#define VK_OEM_4 0xDB
#define VK_OEM_5 0xDC
#define VK_OEM_6 0xDD
#define VK_OEM_7 0xDE

#define MB_OK 0x00000000L
#define MB_OKCANCEL 0x00000001L
#define MB_ABORTRETRYIGNORE 0x00000002L
#define MB_YESNOCANCEL 0x00000003L
#define MB_YESNO 0x00000004L
#define MB_RETRYCANCEL 0x00000005L
#define MB_ICONHAND 0x00000010L
#define MB_ICONQUESTION 0x00000020L
#define MB_ICONEXCLAMATION 0x00000030L
#define MB_ICONASTERISK 0x00000040L
#define MB_ICONERROR MB_ICONHAND
#define MB_ICONSTOP MB_ICONHAND
#define MB_ICONWARNING MB_ICONEXCLAMATION
#define MB_ICONINFORMATION MB_ICONASTERISK
#define MB_DEFBUTTON1 0x00000000L
#define MB_DEFBUTTON2 0x00000100L
#define MB_APPLMODAL 0x00000000L
#define MB_SYSTEMMODAL 0x00001000L
#define MB_TASKMODAL 0x00002000L
#define MB_SETFOREGROUND 0x00010000L
#define MB_TOPMOST 0x00040000L
#define IDOK 1
#define IDCANCEL 2
#define IDABORT 3
#define IDRETRY 4
#define IDIGNORE 5
#define IDYES 6
#define IDNO 7
#define IDCLOSE 8
#define IDHELP 9

#define SW_HIDE 0
#define SW_SHOWNORMAL 1
#define SW_NORMAL 1
#define SW_SHOWMINIMIZED 2
#define SW_SHOWMAXIMIZED 3
#define SW_MAXIMIZE 3
#define SW_SHOWNOACTIVATE 4
#define SW_SHOW 5
#define SW_MINIMIZE 6
#define SW_SHOWNA 8
#define SW_RESTORE 9
#define SW_SHOWDEFAULT 10

OCOMPAT_API int MessageBoxA(HWND hWnd, const char *text, const char *caption, UINT type);
#define MessageBox MessageBoxA
inline void FatalAppExitA(UINT, const char *msg) { MessageBoxA(NULL, msg, "Orbiter", 0x10); exit(1); }
#define FatalAppExit FatalAppExitA
OCOMPAT_API WINBOOL MessageBeep(UINT type);
OCOMPAT_API void OutputDebugStringA(const char *str);
#define OutputDebugString OutputDebugStringA
#define DebugBreak() __builtin_debugtrap()
#ifndef _ASSERTE
#define _ASSERTE(e) assert(e)
#endif
#ifndef _ASSERT
#define _ASSERT(e) assert(e)
#endif
#define __debugbreak() __builtin_debugtrap()

OCOMPAT_API HMODULE LoadLibraryA(const char *path);
#define LoadLibrary LoadLibraryA
OCOMPAT_API HMODULE LoadLibraryExA(const char *path, HANDLE file, DWORD flags);
#define LoadLibraryEx LoadLibraryExA
#define LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR 0x00000100
#define LOAD_LIBRARY_SEARCH_DEFAULT_DIRS 0x00001000
#define LOAD_WITH_ALTERED_SEARCH_PATH 0x00000008
OCOMPAT_API WINBOOL FreeLibrary(HMODULE h);
OCOMPAT_API const char *GetCommandLineA(void);
#define GetCommandLine GetCommandLineA
OCOMPAT_API void *GetProcAddressImpl(HMODULE h, const char *name);
#ifdef __cplusplus
inline FARPROC GetProcAddress(HMODULE h, const char *name) { return reinterpret_cast<FARPROC>(GetProcAddressImpl(h, name)); }
#else
#define GetProcAddress(h, n) ((FARPROC)GetProcAddressImpl(h, n))
#endif
OCOMPAT_API DWORD GetModuleFileNameA(HMODULE h, char *buf, DWORD size);
#define GetModuleFileName GetModuleFileNameA
OCOMPAT_API HMODULE GetModuleHandleA(const char *name);
#define GetModuleHandle GetModuleHandleA
OCOMPAT_API DWORD GetLastError(void);
OCOMPAT_API void SetLastError(DWORD err);
OCOMPAT_API const char *OCompatModuleSuffix(void);
OCOMPAT_API const char *OCompatLastLoadError(void);

OCOMPAT_API DWORD GetCurrentDirectoryA(DWORD size, char *buf);
#define GetCurrentDirectory GetCurrentDirectoryA
OCOMPAT_API WINBOOL SetCurrentDirectoryA(const char *path);
#define SetCurrentDirectory SetCurrentDirectoryA
OCOMPAT_API WINBOOL CreateDirectoryA(const char *path, void *sa);
#define CreateDirectory CreateDirectoryA
OCOMPAT_API WINBOOL DeleteFileA(const char *path);
#define DeleteFile DeleteFileA
OCOMPAT_API WINBOOL CopyFileA(const char *src, const char *dst, WINBOOL failIfExists);
#define CopyFile CopyFileA
OCOMPAT_API WINBOOL MoveFileA(const char *src, const char *dst);
#define MoveFile MoveFileA
OCOMPAT_API DWORD GetFileAttributesA(const char *path);
#define GetFileAttributes GetFileAttributesA
#define INVALID_FILE_ATTRIBUTES ((DWORD)-1)
#define FILE_ATTRIBUTE_READONLY 0x00000001
#define FILE_ATTRIBUTE_HIDDEN 0x00000002
#define FILE_ATTRIBUTE_SYSTEM 0x00000004
#define FILE_ATTRIBUTE_DIRECTORY 0x00000010
#define FILE_ATTRIBUTE_ARCHIVE 0x00000020
#define FILE_ATTRIBUTE_NORMAL 0x00000080

typedef struct _WIN32_FIND_DATAA {
	DWORD dwFileAttributes;
	FILETIME ftCreationTime, ftLastAccessTime, ftLastWriteTime;
	DWORD nFileSizeHigh, nFileSizeLow;
	DWORD dwReserved0, dwReserved1;
	char cFileName[MAX_PATH];
	char cAlternateFileName[14];
} WIN32_FIND_DATAA, WIN32_FIND_DATA, *LPWIN32_FIND_DATA;
OCOMPAT_API HANDLE FindFirstFileA(const char *pattern, WIN32_FIND_DATAA *data);
#define FindFirstFile FindFirstFileA
OCOMPAT_API WINBOOL FindNextFileA(HANDLE h, WIN32_FIND_DATAA *data);
#define FindNextFile FindNextFileA
OCOMPAT_API WINBOOL FindClose(HANDLE h);

OCOMPAT_API DWORD GetTickCount(void);
OCOMPAT_API ULONGLONG GetTickCount64(void);
OCOMPAT_API DWORD timeGetTime(void);
OCOMPAT_API UINT timeBeginPeriod(UINT p);
OCOMPAT_API UINT timeEndPeriod(UINT p);
OCOMPAT_API WINBOOL QueryPerformanceCounter(LARGE_INTEGER *c);
OCOMPAT_API WINBOOL QueryPerformanceFrequency(LARGE_INTEGER *f);
OCOMPAT_API void Sleep(DWORD ms);
OCOMPAT_API void GetLocalTime(SYSTEMTIME *st);
OCOMPAT_API void GetSystemTime(SYSTEMTIME *st);


#define _stricmp strcasecmp
#define stricmp strcasecmp
#define _strcmpi strcasecmp
#define strcmpi strcasecmp
#define _strnicmp strncasecmp
#define strnicmp strncasecmp
#define _wcsicmp wcscasecmp
#define _strdup strdup
#define _access access
#define _getcwd getcwd
#define _chdir chdir
#define _rmdir rmdir
#define _unlink unlink
#define _fileno fileno
#define _isnan isnan
#define _finite isfinite
#define _hypot hypot
#define _copysign copysign
#define _fseeki64 fseeko
#define _ftelli64 ftello
#define _snprintf snprintf
#define _vsnprintf vsnprintf
#define fprintf_s fprintf
#define vfprintf_s vfprintf
OCOMPAT_API int sscanf_s(const char *buf, const char *fmt, ...);
OCOMPAT_API int fscanf_s(FILE *f, const char *fmt, ...);
#define _byteswap_ulong __builtin_bswap32
#define _byteswap_ushort __builtin_bswap16
#define _byteswap_uint64 __builtin_bswap64
#define _alloca alloca
#define __min(a, b) (((a) < (b)) ? (a) : (b))
#define __max(a, b) (((a) > (b)) ? (a) : (b))
#define _putenv putenv
#define _strtoui64 strtoull
#define _strtoi64 strtoll
#define _atoi64 atoll
#define strtok_s strtok_r
#define _execl execl
#define _TRUNCATE ((size_t)-1)
typedef int errno_t;

OCOMPAT_API int _mkdir(const char *path);
OCOMPAT_API char *_strlwr(char *s);
OCOMPAT_API char *_strupr(char *s);
#define strlwr _strlwr
#define strupr _strupr
OCOMPAT_API char *_itoa(int value, char *buf, int radix);
#define itoa _itoa
OCOMPAT_API char *_ltoa(long value, char *buf, int radix);
OCOMPAT_API char *_ultoa(unsigned long value, char *buf, int radix);
OCOMPAT_API char *_gcvt(double value, int digits, char *buf);
OCOMPAT_API void _splitpath(const char *path, char *drive, char *dir, char *fname, char *ext);
OCOMPAT_API void _makepath(char *path, const char *drive, const char *dir, const char *fname, const char *ext);
OCOMPAT_API char *_fullpath(char *abs, const char *rel, size_t size);
OCOMPAT_API void *_aligned_malloc(size_t size, size_t align);
OCOMPAT_API void _aligned_free(void *p);

struct _finddata_t {
	unsigned attrib;
	int64_t time_create, time_access, time_write;
	uint32_t size;
	char name[MAX_PATH];
};
#define _A_NORMAL 0x00
#define _A_RDONLY 0x01
#define _A_HIDDEN 0x02
#define _A_SYSTEM 0x04
#define _A_SUBDIR 0x10
#define _A_ARCH 0x20
OCOMPAT_API intptr_t _findfirst(const char *pattern, struct _finddata_t *data);
OCOMPAT_API int _findnext(intptr_t h, struct _finddata_t *data);
OCOMPAT_API int _findclose(intptr_t h);

#ifdef __cplusplus

inline errno_t memcpy_s(void *dst, size_t n, const void *src, size_t count) {
	if (!dst || (count && !src) || count > n) return 22;
	memcpy(dst, src, count);
	return 0;
}
inline int lstrlen(const char *s) { return s ? (int)strlen(s) : 0; }
inline char *lstrcpy(char *d, const char *s) { return strcpy(d, s); }
inline int lstrcmp(const char *a, const char *b) { return strcmp(a, b); }
inline int lstrcmpi(const char *a, const char *b) { return strcasecmp(a, b); }
#ifndef MAKEFOURCC
#define MAKEFOURCC(c0, c1, c2, c3) ((DWORD)(BYTE)(c0) | ((DWORD)(BYTE)(c1) << 8) | ((DWORD)(BYTE)(c2) << 16) | ((DWORD)(BYTE)(c3) << 24))
#endif
inline errno_t strcpy_s(char *dst, size_t n, const char *src) {
	if (!dst || !n) return 22;
	size_t len = src ? strlen(src) : 0;
	if (len >= n) len = n - 1;
	memcpy(dst, src, len); dst[len] = '\0';
	return 0;
}
template<size_t N> inline errno_t strcpy_s(char (&dst)[N], const char *src) { return strcpy_s(dst, N, src); }

inline errno_t strncpy_s(char *dst, size_t n, const char *src, size_t count) {
	if (!dst || !n) return 22;
	size_t len = src ? strnlen(src, count == _TRUNCATE ? n : count) : 0;
	if (len >= n) len = n - 1;
	memcpy(dst, src, len); dst[len] = '\0';
	return 0;
}
template<size_t N> inline errno_t strncpy_s(char (&dst)[N], const char *src, size_t count) { return strncpy_s(dst, N, src, count); }

inline errno_t strcat_s(char *dst, size_t n, const char *src) {
	size_t cur = strnlen(dst, n);
	if (cur >= n) return 22;
	return strcpy_s(dst + cur, n - cur, src);
}
template<size_t N> inline errno_t strcat_s(char (&dst)[N], const char *src) { return strcat_s(dst, N, src); }

inline errno_t strncat_s(char *dst, size_t n, const char *src, size_t count) {
	size_t cur = strnlen(dst, n);
	if (cur >= n) return 22;
	return strncpy_s(dst + cur, n - cur, src, count);
}
template<size_t N> inline errno_t strncat_s(char (&dst)[N], const char *src, size_t count) { return strncat_s(dst, N, src, count); }

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wformat-nonliteral"
#pragma clang diagnostic ignored "-Wformat-security"
template<typename... A> inline int sprintf_s(char *dst, size_t n, const char *fmt, A... args) { return snprintf(dst, n, fmt, args...); }
template<size_t N, typename... A> inline int sprintf_s(char (&dst)[N], const char *fmt, A... args) { return snprintf(dst, N, fmt, args...); }
template<typename... A> inline int _snprintf_s(char *dst, size_t n, size_t count, const char *fmt, A... args) { return snprintf(dst, count < n ? count + 1 : n, fmt, args...); }
template<size_t N, typename... A> inline int _snprintf_s(char (&dst)[N], size_t count, const char *fmt, A... args) { return snprintf(dst, count < N ? count + 1 : N, fmt, args...); }
template<size_t N> inline int vsprintf_s(char (&dst)[N], const char *fmt, va_list ap) { return vsnprintf(dst, N, fmt, ap); }
inline int vsprintf_s(char *dst, size_t n, const char *fmt, va_list ap) { return vsnprintf(dst, n, fmt, ap); }
inline int _vsnprintf_s(char *dst, size_t n, size_t count, const char *fmt, va_list ap) { return vsnprintf(dst, count < n ? count + 1 : n, fmt, ap); }
#pragma clang diagnostic pop

inline errno_t localtime_s(struct tm *out, const time_t *t) { return localtime_r(t, out) ? 0 : 22; }
inline errno_t gmtime_s(struct tm *out, const time_t *t) { return gmtime_r(t, out) ? 0 : 22; }

inline errno_t fopen_s(FILE **f, const char *name, const char *mode) {
	*f = fopen(name, mode);
	return *f ? 0 : 2;
}

inline errno_t _itoa_s(int value, char *buf, size_t, int radix) { _itoa(value, buf, radix); return 0; }
template<size_t N> inline errno_t _itoa_s(int value, char (&buf)[N], int radix) { _itoa(value, buf, radix); return 0; }
inline errno_t _strlwr_s(char *s, size_t) { _strlwr(s); return 0; }
inline errno_t _strupr_s(char *s, size_t) { _strupr(s); return 0; }
template<size_t N> inline errno_t _strlwr_s(char (&s)[N]) { _strlwr(s); return 0; }
template<size_t N> inline errno_t _strupr_s(char (&s)[N]) { _strupr(s); return 0; }

#endif

#define CP_ACP 0
#define CP_UTF8 65001
#define MB_ERR_INVALID_CHARS 0x08
OCOMPAT_API int MultiByteToWideChar(UINT codepage, DWORD flags, const char *src, int srclen, wchar_t *dst, int dstlen);
OCOMPAT_API int WideCharToMultiByte(UINT codepage, DWORD flags, const wchar_t *src, int srclen, char *dst, int dstlen, const char *defchar, WINBOOL *useddef);

#include "winuser_compat.h"
#include "winthread_compat.h"

#endif
