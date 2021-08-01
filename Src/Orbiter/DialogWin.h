// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Base class for dialog windows
// ======================================================================

#ifndef __DIALOGWIN_H
#define __DIALOGWIN_H

#include <windows.h>
#include "GraphicsAPI.h"

#define WM_USERMESSAGE (WM_USER+10)

const BOOL MSG_DEFAULT = -1;

class DialogWin {
public:
	// Creates a new instance of a dialog window class, but does not
	// actually create the window itself yet
	DialogWin (HINSTANCE hInstance, HWND hParent, int resourceId,
		DLGPROC pDlg, DWORD flags, void *pContext);

	// Creates a dialog window instance for an already existing window
	DialogWin (HINSTANCE hInstance, HWND hWindow, HWND hParent, DWORD flags);

	virtual ~DialogWin();

	// Opens the window and returns its handle
	virtual HWND OpenWindow ();

	virtual void Message (DWORD msg, void *data);
	virtual void ToggleShrink ();

	HWND GetHwnd () const { return hWnd; }
	HINSTANCE GetHinst () const { return hInst; }
	int GetResId () const { return resId; }
	void *GetContext () const { return context; }
	static DialogWin *GetDialogWin (HWND hDlg);

	virtual void Update ();

	virtual BOOL OnInitDialog (HWND hDlg, WPARAM wParam, LPARAM lParam) { return MSG_DEFAULT; }
	virtual BOOL OnCommand (HWND hDlg, WORD id, WORD code, HWND hControl);
	virtual BOOL OnUser1 (HWND hDlg, WPARAM wParam, LPARAM lParam) { return MSG_DEFAULT; }
	virtual BOOL OnUserMessage (HWND hDlg, WPARAM wParam, LPARAM lParam) { return MSG_DEFAULT; }
	virtual BOOL OnHScroll (HWND hDlg, WORD request, WORD curpos, HWND hControl)  { return MSG_DEFAULT; }
	virtual BOOL OnNotify (HWND hDlg, int idCtrl, LPNMHDR pnmh) { return MSG_DEFAULT; }
	virtual BOOL OnApp (HWND hDlg, WPARAM wParam, LPARAM lParam) { return MSG_DEFAULT; }
	virtual BOOL OnSize (HWND hDlg, WPARAM wParam, int w, int h);
	virtual BOOL OnMove (HWND hDlg, int x, int y);
	virtual BOOL OnMouseWheel (HWND hDlg, int vk, int dist, int x, int y) { return MSG_DEFAULT; }
	virtual BOOL OnLButtonDblClk (HWND hDlg, int vk, int x, int y) { return MSG_DEFAULT; }

	bool AddTitleButton (DWORD msg, HBITMAP hBmp, DWORD flag);
	DWORD GetTitleButtonState (DWORD msg);
	bool SetTitleButtonState (DWORD msg, DWORD state);
	void PaintTitleButtons ();
	bool CheckTitleButtons (const POINTS &pt);

	static bool Create_AddTitleButton (DWORD msg, HBITMAP hBmp, DWORD flag);
	static bool Create_SetTitleButtonState (DWORD msg, DWORD state);

	// Default message handler
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

protected:
	static DialogWin *dlg_create;

	oapi::GraphicsClient *gc; // graphics client instance
	HINSTANCE hInst;          // instance handle
	HWND hWnd;                // dialog window handle
	HWND hPrnt;               // parent window handle
	int  resId;               // dialog resource identifier
	void *context;            // dialog context pointer
	RECT *pos;                // window position; needs to be assigned in constructor of derived classes
	DLGPROC dlgproc;          // window message function
	DWORD flag;               // flags
	int psize;                // window height

	struct TitleBtn {         // custom buttons in title bar
		DWORD DlgMsg;
		DWORD flag;
		HBITMAP hBmp;
	} tbtn[5];
};

#endif // !__DIALOGWIN_H