// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Base class for dialog windows
// ======================================================================

#ifndef __DIALOGWIN_H
#define __DIALOGWIN_H

#ifndef __linux__
#include <windows.h>
#else // __linux__
#include "OrbiterPlatform.h"
#endif // __linux__
#include "GraphicsAPI.h"

#ifndef __linux__
#define WM_USERMESSAGE (WM_USER+10)
#else // __linux__
class QObject;
#endif // __linux__

const BOOL MSG_DEFAULT = -1;

class DialogWin {
public:
	// Creates a new instance of a dialog window class, but does not
	// actually create the window itself yet
#ifndef __linux__
	DialogWin (HINSTANCE hInstance, HWND hParent, int resourceId,
		DLGPROC pDlg, DWORD flags, void *pContext);
#else // __linux__
	DialogWin (void *hInstance, QWindow *hParent, int resourceId,
		DLGINIT pDlg, DWORD flags, void *pContext);
#endif // __linux__

	// Creates a dialog window instance for an already existing window
#ifndef __linux__
	DialogWin (HINSTANCE hInstance, HWND hWindow, HWND hParent, DWORD flags);
#else // __linux__
	DialogWin (void *hInstance, QWidget *hWindow, QWindow *hParent, DWORD flags);
#endif // __linux__

	virtual ~DialogWin();

	// Opens the window and returns its handle
#ifndef __linux__
	virtual HWND OpenWindow ();
#else // __linux__
	virtual QWidget *OpenWindow ();
#endif // __linux__

	virtual void Message (DWORD msg, void *data);
	virtual void ToggleShrink ();

	/**
	 * \brief Request an update at every time step
	 * \return If true, the Update method is called at each time step for all
	 *    open dialogs. If false, Update has to be called explicitly whenever
	 *    the dialog should update itself.
	 * \default true
	 */
	virtual bool UpdateContinuously() const { return true; }

#ifndef __linux__
	HWND GetHwnd () const { return hWnd; }
	HINSTANCE GetHinst () const { return hInst; }
#else // __linux__
	QWidget *GetHwnd () const { return hWnd; }
	void *GetHinst () const { return hInst; }
#endif // __linux__
	int GetResId () const { return resId; }
	void *GetContext () const { return context; }
#ifndef __linux__
	static DialogWin *GetDialogWin (HWND hDlg);
#else // __linux__
	static DialogWin *GetDialogWin (QWidget *hDlg);
#endif // __linux__

	virtual void Update ();

#ifndef __linux__
	virtual BOOL OnInitDialog (HWND hDlg, WPARAM wParam, LPARAM lParam) { return MSG_DEFAULT; }
	virtual BOOL OnCommand (HWND hDlg, WORD id, WORD code, HWND hControl);
	virtual BOOL OnUser1 (HWND hDlg, WPARAM wParam, LPARAM lParam) { return MSG_DEFAULT; }
	virtual BOOL OnUserMessage (HWND hDlg, WPARAM wParam, LPARAM lParam) { return MSG_DEFAULT; }
	virtual BOOL OnHScroll (HWND hDlg, WORD request, WORD curpos, HWND hControl)  { return MSG_DEFAULT; }
	virtual BOOL OnVScroll(HWND hDlg, WORD request, WORD curpos, HWND hControl) { return MSG_DEFAULT; }
	virtual BOOL OnNotify (HWND hDlg, int idCtrl, LPNMHDR pnmh) { return MSG_DEFAULT; }
	virtual BOOL OnApp (HWND hDlg, WPARAM wParam, LPARAM lParam) { return MSG_DEFAULT; }
	virtual BOOL OnSize (HWND hDlg, WPARAM wParam, int w, int h);
	virtual BOOL OnMove (HWND hDlg, int x, int y);
	virtual BOOL OnMouseWheel (HWND hDlg, int vk, int dist, int x, int y) { return MSG_DEFAULT; }
	virtual BOOL OnLButtonDblClk (HWND hDlg, int vk, int x, int y) { return MSG_DEFAULT; }
#else // __linux__
	// handlers of the dialog window's events; the controls' own events are connected by the DLGINIT function
	virtual BOOL OnInitDialog (QWidget *hDlg, void *context) { return MSG_DEFAULT; }
	virtual BOOL OnCommand (QWidget *hDlg, WORD id, WORD code, QWidget *hControl);
	virtual BOOL OnUserMessage (QWidget *hDlg, DWORD msg, void *data) { return MSG_DEFAULT; }
	virtual BOOL OnSize (QWidget *hDlg, int w, int h);
	virtual BOOL OnMove (QWidget *hDlg, int x, int y);
#endif // __linux__

#ifndef __linux__
	bool AddTitleButton (DWORD msg, HBITMAP hBmp, DWORD flag);
#else // __linux__
	bool AddTitleButton (DWORD msg, QImage *hBmp, DWORD flag);
#endif // __linux__
	DWORD GetTitleButtonState (DWORD msg);
	bool SetTitleButtonState (DWORD msg, DWORD state);
	void PaintTitleButtons ();
#ifndef __linux__
	bool CheckTitleButtons (const POINTS &pt);
#else // __linux__
	bool CheckTitleButtons (const POINT &pt);
#endif // __linux__

#ifndef __linux__
	static bool Create_AddTitleButton (DWORD msg, HBITMAP hBmp, DWORD flag);
#else // __linux__
	static bool Create_AddTitleButton (DWORD msg, QImage *hBmp, DWORD flag);
#endif // __linux__
	static bool Create_SetTitleButtonState (DWORD msg, DWORD state);

#ifndef __linux__
	// Default message handler
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	// Default event handler of the dialog window (DlgProc counterpart); true if handled
	static bool DlgProc (QWidget *hDlg, QEvent *event);
#endif // __linux__

protected:
	static DialogWin *dlg_create;

	oapi::GraphicsClient *gc; // graphics client instance
#ifndef __linux__
	HINSTANCE hInst;          // instance handle
	HWND hWnd;                // dialog window handle
	HWND hPrnt;               // parent window handle
#else // __linux__
	void *hInst;              // instance handle
	QWidget *hWnd;            // dialog window handle
	QWindow *hPrnt;           // parent window handle
#endif // __linux__
	int  resId;               // dialog resource identifier
	void *context;            // dialog context pointer
	RECT *pos;                // window position; needs to be assigned in constructor of derived classes
#ifndef __linux__
	DLGPROC dlgproc;          // window message function
#else // __linux__
	DLGINIT dlgproc;          // control set-up function of the module
#endif // __linux__
	DWORD flag;               // flags
	int psize;                // window height
#ifdef __linux__
	QObject *events;          // routes the dialog window's events to DlgProc
#endif // __linux__

	struct TitleBtn {         // custom buttons in title bar
		DWORD DlgMsg;
		DWORD flag;
#ifndef __linux__
		HBITMAP hBmp;
#else // __linux__
		QImage *hBmp;
#endif // __linux__
	} tbtn[5];
};

#endif // !__DIALOGWIN_H
