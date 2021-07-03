#ifndef WINPROC_H
#define WINPROC_H
/*
**-----------------------------------------------------------------------------
**  File:       WinProc.h
**  Purpose:    Sample Window Procedure code
**  Notes:
**
**  Copyright (c) 1995-1999 by Microsoft, all rights reserved
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Include files
**-----------------------------------------------------------------------------
*/

#include "Common.h"
#include "D3DWin.h"


/*
**-----------------------------------------------------------------------------
**  Defines
**-----------------------------------------------------------------------------
*/

#define MENU_DRIVER			0
#define MENU_DEVICE			1
#define MENU_MODE			2
#define MENU_TEXTURE		3
#define MENU_HELP			4

#define MENU_FIRST_DRIVER		1500
#define MENU_FIRST_DEVICE		1600
#define MENU_FIRST_MODE			1700
#define MENU_FIRST_TEXTURE		1800
#define MENU_FIRST_TEXFORMAT	1900
#define MENU_LAST_DYNAMIC		2000




/*
**-----------------------------------------------------------------------------
**  Function Prototypes
**-----------------------------------------------------------------------------
*/

// Windows routines
LRESULT CALLBACK D3DWindowProc (HWND hWindow, UINT uiMessage,
                                WPARAM wParam, LPARAM lParam);


// Window Message routines
LRESULT OnAbout (HWND hWindow);
LRESULT OnActivate (HWND hWindow, WPARAM wParam, LPARAM lParam);
LRESULT OnActivateApp (HWND hWindow, WPARAM wParam, LPARAM lParam);
LRESULT OnClose (HWND hWindow);
LRESULT OnCommand (HWND hWindow, WPARAM wParam, LPARAM lParam);
LRESULT OnCreate (HWND hWindow);
LRESULT OnDestroy (HWND hWindow);
LRESULT OnDisplayChange (HWND hWindow);
LRESULT OnEraseBackground (HWND hWindow, WPARAM wParam, LPARAM lParam);
LRESULT OnEnterMenuLoop (HWND hWindow, WPARAM wParam, LPARAM lParam);
LRESULT OnExitMenuLoop (HWND hWindow, WPARAM wParam, LPARAM lParam);
LRESULT OnGetMinMaxInfo (HWND hWindow, WPARAM wParam, LPARAM lParam);
LRESULT OnMove (HWND hWindow, WPARAM wParam, LPARAM lParam);
LRESULT OnNCPaint (HWND hWindow, WPARAM wParam, LPARAM lParam);
LRESULT OnPaint (HWND hWindow, HDC hdc, LPPAINTSTRUCT lpps);
LRESULT OnSetCursor (HWND hWindow, WPARAM wParam, LPARAM lParam);
LRESULT OnSize (HWND hWindow, WPARAM wParam, LPARAM lParam);
LRESULT OnSysCommand (HWND hWindow, WPARAM wParam, LPARAM lParam);
LRESULT OnWindowPosChanging (HWND hWindow, WPARAM wParam, LPARAM lParam);

// Special Non Message routines
void    OnIdle (HWND hWindow);
void    OnPause (HWND hWindow, BOOL fPause);

// D3D Notification routines
LRESULT	OnD3DInit (HWND hWindow, LPD3DWindow lpd3dWindow);
LRESULT	OnD3DFini (HWND hWindow, LPD3DWindow lpd3dWindow);
LRESULT	OnD3DChangeDriver (HWND hWindow);
LRESULT	OnD3DChangeMode (HWND hWindow);
LRESULT	OnD3DChangeDevice (HWND hWindow);
LRESULT	OnD3DChangeFullscreen (HWND hWindow);

// Misc Routines
void    PaintPaused (HWND hWindow, HDC hdc);



/*
**-----------------------------------------------------------------------------
**  End of File
**-----------------------------------------------------------------------------
*/
#endif // End WINPROC_H


