// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "DialogWin.h"
#include "DlgMgr.h"
#include "OrbiterAPI.h"
#ifdef __linux__
#include "OrbiterResource.h"
#endif // __linux__
#include "Orbiter.h"
#ifndef __linux__
#include "Resource.h"
#else // __linux__
#include "resource.h"
#endif // __linux__
#include "Log.h"
#ifdef __linux__
#include <QKeyEvent>
#include <QMoveEvent>
#include <QPointer>
#include <QResizeEvent>
#include <QWidget>
#include <QWindow>
#endif // __linux__

#define DLG_CAPTIONBUTTON (DLG_CAPTIONCLOSE|DLG_CAPTIONHELP)

extern Orbiter *g_pOrbiter;

#ifndef __linux__
static int x_sizeframe = GetSystemMetrics (SM_CXSIZEFRAME);
static int y_sizeframe = GetSystemMetrics (SM_CYSIZEFRAME);
static int x_fixedframe = GetSystemMetrics (SM_CXFIXEDFRAME);
static int y_fixedframe = GetSystemMetrics (SM_CYFIXEDFRAME);

#endif // !__linux__
DialogWin *DialogWin::dlg_create = 0;

#ifdef __linux__
// routes the events of a dialog window to DialogWin::DlgProc (window procedure hook)
class DialogEvents: public QObject {
public:
	DialogEvents (QWidget *hDlg): QObject (hDlg) { hDlg->installEventFilter (this); }
	bool eventFilter (QObject *obj, QEvent *event) override
	{
		QWidget *w = qobject_cast<QWidget*> (obj);
		return (w ? DialogWin::DlgProc (w, event) : false);
	}
};

#endif // __linux__
// ======================================================================

#ifndef __linux__
DialogWin::DialogWin (HINSTANCE hInstance, HWND hParent, int resourceId,
					  DLGPROC pDlg, DWORD flags, void *pContext)
#else // __linux__
DialogWin::DialogWin (void *hInstance, QWindow *hParent, int resourceId,
					  DLGINIT pDlg, DWORD flags, void *pContext)
#endif // __linux__
{
	gc      = g_pOrbiter->GetGraphicsClient();
	hInst   = hInstance;
	resId   = resourceId;
	flag    = flags;
	context = pContext;
	hPrnt   = hParent;
	hWnd    = NULL;
	pos     = NULL;
#ifndef __linux__
	dlgproc = (pDlg ? pDlg : DlgProc);
#else // __linux__
	dlgproc = pDlg; // without a module function, OnInitDialog sets up the controls
	events  = NULL;
#endif // __linux__

	memset (tbtn, 0, 5*sizeof(TitleBtn));
	//int i = 0;
	//if (flag & DLG_CAPTIONCLOSE) tbtn[i++].DlgMsg = IDCANCEL;
	//if (flag & DLG_CAPTIONHELP)  tbtn[i++].DlgMsg = IDHELP;
}

// ======================================================================

#ifndef __linux__
DialogWin::DialogWin (HINSTANCE hInstance, HWND hWindow, HWND hParent, DWORD flags)
#else // __linux__
DialogWin::DialogWin (void *hInstance, QWidget *hWindow, QWindow *hParent, DWORD flags)
#endif // __linux__
{
	gc      = g_pOrbiter->GetGraphicsClient();
	hInst   = hInstance;
	resId   = 0;
	flag    = flags;
	context = 0;
	hWnd    = hWindow;
	hPrnt   = hParent;
#ifdef __linux__
	pos     = NULL;
#endif // __linux__
	dlgproc = NULL;
#ifdef __linux__
	events  = NULL;
#endif // __linux__

	memset (tbtn, 0, 5*sizeof(TitleBtn));
	//int i = 0;
	//if (flag & DLG_CAPTIONCLOSE) tbtn[i++].DlgMsg = IDCANCEL;
	//if (flag & DLG_CAPTIONHELP)  tbtn[i++].DlgMsg = IDHELP;
}

// ======================================================================

DialogWin::~DialogWin ()
{
	if (hWnd) {
#ifndef __linux__
		if (!DestroyWindow(hWnd))
			LOGOUT_LASTERR();
#else // __linux__
		// DestroyWindow; deferred, since the request may come from a signal of one of the dialog's own controls
		if (events) hWnd->removeEventFilter (events);
		hWnd->setProperty ("DialogWin", QVariant());
		hWnd->hide();
		hWnd->deleteLater();
#endif // __linux__
	}
}

// ======================================================================

#ifndef __linux__
HWND DialogWin::OpenWindow ()
#else // __linux__
QWidget *DialogWin::OpenWindow ()
#endif // __linux__
{
	bool newwin = false;
	dlg_create = this; // is this still necessary ?

	if (gc) gc->clbkPreOpenPopup();

	if (!hWnd) { // otherwise window exists already
#ifndef __linux__
		hWnd = CreateDialogParam (hInst, MAKEINTRESOURCE(resId), hPrnt, dlgproc,
			(LPARAM)context);
#else // __linux__
		hWnd = oapiCreateResDialog (hInst, resId, NULL, hPrnt);
		if (!hWnd) {
			LOGOUT_ERR ("Dialog resource %d not found", resId);
			dlg_create = 0;
			return NULL;
		}
#endif // __linux__
		newwin = true;
	}
#ifndef __linux__
	SetWindowLongPtr (hWnd, DWLP_USER, (LONG_PTR)this);
#else // __linux__
	hWnd->setProperty ("DialogWin", QVariant::fromValue ((void*)this)); // DWLP_USER
	if (!events) events = new DialogEvents (hWnd);
	if (newwin) {
		// WM_INITDIALOG
		if (dlgproc) dlgproc (hWnd, context);
		else OnInitDialog (hWnd, context);
	}
#endif // __linux__
	if (newwin && pos && pos->right-pos->left) {
#ifndef __linux__
		if (GetWindowLongPtr (hWnd, GWL_STYLE) & WS_SIZEBOX)
			SetWindowPos (hWnd, NULL, pos->left, pos->top, pos->right-pos->left, pos->bottom-pos->top, SWP_NOZORDER);
#else // __linux__
		if (hWnd->minimumSize() != hWnd->maximumSize()) // WS_SIZEBOX
			hWnd->setGeometry (pos->left, pos->top, pos->right-pos->left, pos->bottom-pos->top);
#endif // __linux__
		else
#ifndef __linux__
			SetWindowPos (hWnd, NULL, pos->left, pos->top, 0, 0, SWP_NOZORDER | SWP_NOSIZE);
#else // __linux__
			hWnd->move (pos->left, pos->top);
#endif // __linux__
	}

#ifndef __linux__
	RECT r;
	ShowWindow (hWnd, SW_SHOWNOACTIVATE);
	GetWindowRect (hWnd, &r);
	psize = r.bottom - r.top;
#else // __linux__
	hWnd->setAttribute (Qt::WA_ShowWithoutActivating); // SW_SHOWNOACTIVATE
	hWnd->show();
	psize = hWnd->frameGeometry().height();
#endif // __linux__

	dlg_create = 0;

	return hWnd;
}

// ======================================================================

void DialogWin::Update ()
{
}

// ======================================================================

#ifndef __linux__
INT_PTR CALLBACK DialogWin::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
bool DialogWin::DlgProc (QWidget *hDlg, QEvent *event)
#endif // __linux__
{
#ifndef __linux__
	BOOL res = MSG_DEFAULT;
	switch (uMsg) {
	case WM_INITDIALOG:
		res = GetDialogWin (hDlg)->OnInitDialog (hDlg, wParam, lParam);
		break;
	case WM_MOVE:
		res = GetDialogWin (hDlg)->OnMove (hDlg, LOWORD(lParam), HIWORD(lParam));
		break;
	case WM_SIZE:
		res = GetDialogWin (hDlg)->OnSize (hDlg, wParam, LOWORD(lParam), HIWORD(lParam));
		break;
	case WM_COMMAND:
		res = GetDialogWin (hDlg)->OnCommand (hDlg, LOWORD(wParam), HIWORD(wParam), (HWND)lParam);
		break;
	case WM_HSCROLL:
		res = GetDialogWin (hDlg)->OnHScroll (hDlg, LOWORD(wParam), HIWORD(wParam), (HWND)lParam);
		break;
	case WM_VSCROLL:
		res = GetDialogWin (hDlg)->OnVScroll(hDlg, LOWORD(wParam), HIWORD(wParam), (HWND)lParam);
		break;
	case WM_NOTIFY:
		res = GetDialogWin (hDlg)->OnNotify (hDlg, (int)wParam, (LPNMHDR)lParam);
		break;
	case WM_MOUSEWHEEL:
		res = GetDialogWin (hDlg)->OnMouseWheel (hDlg, LOWORD(wParam), HIWORD(wParam), LOWORD(lParam), HIWORD(lParam));
		break;
	case WM_LBUTTONDBLCLK:
		res = GetDialogWin (hDlg)->OnLButtonDblClk (hDlg, wParam, LOWORD(lParam), HIWORD(lParam));
		break;
	case WM_APP:
		res = GetDialogWin (hDlg)->OnApp (hDlg, wParam, lParam);
		break;
	case WM_USER+1:
		res = GetDialogWin (hDlg)->OnUser1 (hDlg, wParam, lParam);
#else // __linux__
	DialogWin *dlg = GetDialogWin (hDlg);
	if (!dlg) return false;
	switch (event->type()) {
	case QEvent::Move: {
		QPoint p = static_cast<QMoveEvent*> (event)->pos();
		dlg->OnMove (hDlg, p.x(), p.y());
		} break;
	case QEvent::Resize: {
		QSize s = static_cast<QResizeEvent*> (event)->size();
		dlg->OnSize (hDlg, s.width(), s.height());
		} break;
	case QEvent::Close: // title bar close button: WM_COMMAND IDCANCEL
		event->ignore();
		dlg->OnCommand (hDlg, IDCANCEL, 0, NULL);
		return true;
	case QEvent::KeyPress:
		if (static_cast<QKeyEvent*> (event)->key() == Qt::Key_Escape) { // IDCANCEL from the keyboard
			dlg->OnCommand (hDlg, IDCANCEL, 0, NULL);
			return true;
		}
#endif // __linux__
		break;
#ifndef __linux__
	case WM_USERMESSAGE:
		res = GetDialogWin (hDlg)->OnUserMessage (hDlg, wParam, lParam);
#else // __linux__
	default:
#endif // __linux__
		break;
	}
#ifndef __linux__
	return (res != MSG_DEFAULT ? res : OrbiterDefDialogProc (hDlg, uMsg, wParam, lParam));
#else // __linux__
	return OrbiterDefDialogProc (hDlg, event);
#endif // __linux__
}

// ======================================================================

#ifndef __linux__
BOOL DialogWin::OnCommand (HWND hDlg, WORD id, WORD code, HWND hControl)
#else // __linux__
BOOL DialogWin::OnCommand (QWidget *hDlg, WORD id, WORD code, QWidget *hControl)
#endif // __linux__
{
	switch (id) {
	case IDCANCEL:
		g_pOrbiter->CloseDialog (hDlg);
		return TRUE;
	}
	return MSG_DEFAULT;
}

// ======================================================================

#ifndef __linux__
int DialogWin::OnSize (HWND hWnd, WPARAM wParam, int w, int h)
#else // __linux__
static void WindowRect (QWidget *hWnd, RECT *r)
{
	QRect g = hWnd->frameGeometry();
	r->left = g.left(), r->top = g.top(), r->right = g.left()+g.width(), r->bottom = g.top()+g.height();
}

int DialogWin::OnSize (QWidget *hWnd, int w, int h)
#endif // __linux__
{
#ifndef __linux__
	if (pos) GetWindowRect (hWnd, pos);
#else // __linux__
	if (pos) WindowRect (hWnd, pos);
#endif // __linux__
	return 0;
}

// ======================================================================

#ifndef __linux__
int DialogWin::OnMove (HWND hWnd, int x, int y)
#else // __linux__
int DialogWin::OnMove (QWidget *hWnd, int x, int y)
#endif // __linux__
{
#ifndef __linux__
	if (pos) GetWindowRect (hWnd, pos);
#else // __linux__
	if (pos) WindowRect (hWnd, pos);
#endif // __linux__
	return 0;
}

// ======================================================================

void DialogWin::Message (DWORD msg, void *data)
{
#ifndef __linux__
	PostMessage (hWnd, WM_USERMESSAGE, msg, (LPARAM)data);
#else // __linux__
	// PostMessage: delivered from the event loop, if the dialog is still open then
	QPointer<QWidget> w (hWnd);
	QMetaObject::invokeMethod (hWnd, [w, msg, data]() {
		DialogWin *dlg = (w ? (DialogWin*)w->property ("DialogWin").value<void*>() : NULL);
		if (dlg) dlg->OnUserMessage (w, msg, data);
	}, Qt::QueuedConnection);
#endif // __linux__
}

// ======================================================================

void DialogWin::ToggleShrink ()
{
#ifndef __linux__
	RECT r;
	GetWindowRect (hWnd, &r);
	int hw = r.bottom - r.top;
	int h0 = GetSystemMetrics (SM_CYMIN);
#else // __linux__
	QRect r = hWnd->geometry();
	int hw = r.height();
	int h0 = 1; // SM_CYMIN: no client area left below the title bar
	bool fixed = (hWnd->minimumHeight() == hWnd->maximumHeight());
#endif // __linux__
	if (hw == h0) { // restore window
		hw = psize;
#ifndef __linux__
		SetWindowPos (hWnd, 0, r.left, r.top, r.right-r.left, hw, SWP_SHOWWINDOW);
#endif // !__linux__
	} else {
		psize = hw;
#ifndef __linux__
		SetWindowPos (hWnd, 0, r.left, r.top, r.right-r.left, h0, SWP_SHOWWINDOW);
#else // __linux__
		hw = h0;
#endif // __linux__
	}
#ifdef __linux__
	if (fixed) hWnd->setFixedHeight (hw);
	else hWnd->resize (r.width(), hw);
	hWnd->show();
#endif // __linux__
}

// ======================================================================

#ifndef __linux__
DialogWin *DialogWin::GetDialogWin (HWND hDlg)
#else // __linux__
DialogWin *DialogWin::GetDialogWin (QWidget *hDlg)
#endif // __linux__
{
#ifndef __linux__
	DialogWin *dlg = (DialogWin*)GetWindowLongPtr (hDlg, DWLP_USER);
#else // __linux__
	DialogWin *dlg = (hDlg ? (DialogWin*)hDlg->property ("DialogWin").value<void*>() : NULL);
#endif // __linux__
	if (!dlg)
		dlg = dlg_create;
	return dlg;
}

// ======================================================================

#ifndef __linux__
bool DialogWin::AddTitleButton (DWORD msg, HBITMAP hBmp, DWORD flag)
#else // __linux__
bool DialogWin::AddTitleButton (DWORD msg, QImage *hBmp, DWORD flag)
#endif // __linux__
{
	for (int i = 0; i < 5; i++) {
		if (tbtn[i].DlgMsg == 0) {
			tbtn[i].DlgMsg = msg;
			tbtn[i].hBmp = hBmp;
			tbtn[i].flag = flag;
			return true;
		}
	}
	return false;
}

// ======================================================================

DWORD DialogWin::GetTitleButtonState (DWORD msg)
{
	for (int i = 0; i < 5; i++)
		if (tbtn[i].DlgMsg == msg)
			return (tbtn[i].flag & 0x80000000 ? 1:0);
	return 0;
}

// ======================================================================

bool DialogWin::SetTitleButtonState (DWORD msg, DWORD state)
{
	for (int i = 0; i < 5; i++)
		if (tbtn[i].DlgMsg == msg) {
			if (tbtn[i].flag & DLG_CB_TWOSTATE) {
				DWORD oldstate = (tbtn[i].flag & 0x80000000 ? 1:0);
				if (oldstate != state) {
					tbtn[i].flag ^= 0x80000000;
					PaintTitleButtons ();
#ifndef __linux__
					PostMessage (hWnd, WM_COMMAND, MAKELONG (tbtn[i].DlgMsg, state), 0);
#else // __linux__
					// PostMessage WM_COMMAND
					QPointer<QWidget> w (hWnd);
					WORD id = (WORD)tbtn[i].DlgMsg;
					QMetaObject::invokeMethod (hWnd, [w, id, state]() {
						DialogWin *dlg = (w ? (DialogWin*)w->property ("DialogWin").value<void*>() : NULL);
						if (dlg) dlg->OnCommand (w, id, (WORD)state, NULL);
					}, Qt::QueuedConnection);
#endif // __linux__
					return true;
				}
			}
			return false;
		}
	return false;
}

// ======================================================================

void DialogWin::PaintTitleButtons ()
{
#ifdef __linux__
	// The title bar belongs to the window manager, so the buttons are not drawn into it
	// (upstream's WM_NCPAINT hook that painted them is disabled as well)
#endif // __linux__
	if (!(flag & DLG_CAPTIONBUTTON)) return;
#ifndef __linux__
	RECT r;
	int x0, y0;
	GetWindowRect (hWnd, &r);
	if (GetWindowLongPtr (hWnd, GWL_STYLE) & WS_THICKFRAME) {
		x0 = -y_sizeframe,  y0 = x_sizeframe;
	} else {
		x0 = -y_fixedframe, y0 = x_fixedframe;
	}
	x0 += r.right-r.left-15;
	HDC hDC = GetWindowDC (hWnd);
	HDC hDCsrc = CreateCompatibleDC (hDC);
	HBITMAP hBmp = (HBITMAP)LoadImage (g_pOrbiter->GetInstance(), MAKEINTRESOURCE(IDB_DEFBUTTON), IMAGE_BITMAP, 15, 30, 0);
	SelectObject (hDCsrc, hBmp);
	int i = 0;
	if (flag & DLG_CAPTIONCLOSE) {
		BOOL res = BitBlt (hDC, x0, y0, 15, 15, hDCsrc, 0, 0, SRCCOPY);
		x0 -= 16;
		i++;
	}
	if (flag & DLG_CAPTIONHELP) {
		BitBlt (hDC, x0, y0, 15, 15, hDCsrc, 0, 15, SRCCOPY);
		x0 -= 16;
		i++;
	}
	for (; i < 5; i++) {
		if (tbtn[i].DlgMsg && tbtn[i].hBmp) {
			SelectObject (hDCsrc, tbtn[i].hBmp);
			BitBlt (hDC, x0, y0, 15, 15, hDCsrc, 0, tbtn[i].flag & 0x80000000 ? 15:0, SRCCOPY);
			x0 -= 16;
		}
	}
	DeleteDC (hDCsrc);
	DeleteObject (hBmp);
	ReleaseDC (hWnd, hDC);
#endif // !__linux__
}

// ======================================================================

#ifndef __linux__
bool DialogWin::CheckTitleButtons (const POINTS &pt)
#else // __linux__
bool DialogWin::CheckTitleButtons (const POINT &pt)
#endif // __linux__
{
#ifdef __linux__
	// no title bar buttons are drawn (see PaintTitleButtons), so none can be hit
#endif // __linux__
	if (!(flag & DLG_CAPTIONBUTTON)) return false;
#ifndef __linux__

	RECT r;
	GetWindowRect (hWnd, &r);
	int xm = pt.x-r.left;
	int ym = pt.y-r.top;
	int x0, y0;
	if (GetWindowLongPtr (hWnd, GWL_STYLE) & WS_THICKFRAME) {
		x0 = y_sizeframe,  y0 = x_sizeframe;
	} else {
		x0 = y_fixedframe, y0 = x_fixedframe;
	}
	if (ym < y0 || ym >= y0+15) return false;
	int nbt = (r.right-pt.x-x0)/16;
	if (nbt >= 0 && nbt < 5 && tbtn[nbt].DlgMsg) {
		WORD state = 0;
		if (tbtn[nbt].flag & DLG_CB_TWOSTATE) {
			tbtn[nbt].flag ^= 0x80000000;
			state = (tbtn[nbt].flag & 0x80000000 ? 1:0);
			PaintTitleButtons ();
		}
		PostMessage (hWnd, WM_COMMAND, MAKELONG (tbtn[nbt].DlgMsg, state), 0);
		return true;
	} else return false;
#else // __linux__
	return false;
#endif // __linux__
}

// ======================================================================

#ifndef __linux__
bool DialogWin::Create_AddTitleButton (DWORD msg, HBITMAP hBmp, DWORD flag)
#else // __linux__
bool DialogWin::Create_AddTitleButton (DWORD msg, QImage *hBmp, DWORD flag)
#endif // __linux__
{
	if (dlg_create) return dlg_create->AddTitleButton (msg, hBmp, flag);
	else            return false;
}

// ======================================================================

bool DialogWin::Create_SetTitleButtonState (DWORD msg, DWORD state)
{
	if (dlg_create) return dlg_create->SetTitleButtonState (msg, state);
	else            return false;
}
