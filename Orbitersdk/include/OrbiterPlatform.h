// not upstream: Linux counterparts of the windows.h types the SDK headers use

#ifndef __ORBITERPLATFORM_H
#define __ORBITERPLATFORM_H

#include <cstdint>
#include <cstddef>

// integer types keep their Win32 names and sizes
typedef uint8_t   BYTE;
typedef uint8_t   UINT8;
typedef uint16_t  WORD;
typedef uint32_t  DWORD;
typedef int16_t   INT16;
typedef int32_t   LONG;
typedef unsigned int UINT;
typedef int       BOOL;
typedef uint32_t  COLORREF;
typedef intptr_t  INT_PTR;
typedef uintptr_t UINT_PTR;
typedef intptr_t  LONG_PTR;
typedef uintptr_t DWORD_PTR;
typedef uint64_t  DWORDLONG;
typedef int64_t   LONGLONG;
typedef int       INT;
typedef float     FLOAT;
typedef uintptr_t WPARAM; // MFD message parameters (MFDMODESPEC::msgproc)
typedef intptr_t  LPARAM;
#define VOID void

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

// windef.h plain data structs, same layout
typedef struct tagRECT { LONG left, top, right, bottom; } RECT;
typedef struct tagPOINT { LONG x, y; } POINT;
typedef struct tagSIZE { LONG cx, cy; } SIZE;
typedef RECT *LPRECT;
typedef SIZE *LPSIZE;
typedef wchar_t *LPWSTR; // wchar_t is UTF-32 on Linux, UTF-16 on Windows
typedef char *PSTR;
typedef const char *PCSTR;

// COLORREF is 0x00bbggrr, as on Windows
#define RGB(r,g,b) ((COLORREF)(((BYTE)(r)|((WORD)((BYTE)(g))<<8))|(((DWORD)(BYTE)(b))<<16)))
#define GetRValue(rgb) ((BYTE)(rgb))
#define GetGValue(rgb) ((BYTE)(((WORD)(rgb)) >> 8))
#define GetBValue(rgb) ((BYTE)((rgb)>>16))

// packed 16-bit pairs (Sketchpad char size, mouse wheel state)
#define LOWORD(l) ((WORD)(((DWORD_PTR)(l)) & 0xffff))
#define HIWORD(l) ((WORD)((((DWORD_PTR)(l)) >> 16) & 0xffff))
#define MAKELONG(a,b) ((LONG)(((WORD)(((DWORD_PTR)(a)) & 0xffff)) | ((DWORD)((WORD)(((DWORD_PTR)(b)) & 0xffff))) << 16))
#define MAKEWPARAM(l,h) ((WPARAM)(DWORD)MAKELONG(l,h))

// mouse event codes and key state flags passed to clbkProcessMouse keep their winuser.h values
#define WM_MOUSEMOVE     0x0200
#define WM_LBUTTONDOWN   0x0201
#define WM_LBUTTONUP     0x0202
#define WM_LBUTTONDBLCLK 0x0203
#define WM_RBUTTONDOWN   0x0204
#define WM_RBUTTONUP     0x0205
#define WM_RBUTTONDBLCLK 0x0206
#define WM_MBUTTONDOWN   0x0207
#define WM_MBUTTONUP     0x0208
#define WM_MBUTTONDBLCLK 0x0209
#define WM_MOUSEWHEEL    0x020A
#define WM_MOUSEHWHEEL   0x020E
#define MK_LBUTTON  0x0001
#define MK_RBUTTON  0x0002
#define MK_SHIFT    0x0004
#define MK_CONTROL  0x0008
#define MK_MBUTTON  0x0010
#define WHEEL_DELTA 120

// Linux PATH_MAX; Windows' 260 is too short for Linux paths
#define MAX_PATH 4096

// dialog command ids of the standard buttons (winuser.h values, as the .rc files use them)
#define IDOK     1
#define IDCANCEL 2
#define IDABORT  3
#define IDRETRY  4
#define IDIGNORE 5
#define IDYES    6
#define IDNO     7
#define IDCLOSE  8
#define IDHELP   9

// handle types are native: HWND -> QWidget (dialogs) / QWindow (render window), HINSTANCE -> dlopen handle (void*),
// HDC -> QPainter, HFONT -> QFont, HPEN -> QPen, HBRUSH -> QBrush, HBITMAP -> QImage, window messages -> QEvent
class QWidget;
class QWindow;
class QEvent;
class QPainter;
class QFont;
class QPen;
class QBrush;
class QImage;

// replaces DLGPROC: called once after a dialog is built from its resource; the module connects its controls' Qt signals
typedef void (*DLGINIT)(QWidget *hDlg, void *context);

#endif // !__ORBITERPLATFORM_H
