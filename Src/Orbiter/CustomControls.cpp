// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __linux__
#include <windows.h>
#endif // !__linux__
#include "CustomControls.h"
#ifdef __linux__
#include "OrbiterResource.h"
#endif // __linux__
#include "Util.h"
#ifdef __linux__
#include <QMouseEvent>
#include <QResizeEvent>
#include <algorithm>
#endif // __linux__

using std::min;
using std::max;

// ===========================================================================

CustomCtrl::CustomCtrl ()
{
	hWnd = NULL;
	hParent = NULL;
}

// ---------------------------------------------------------------------------

#ifndef __linux__
CustomCtrl::CustomCtrl (HWND hCtrl)
#else // __linux__
CustomCtrl::CustomCtrl (QWidget *hCtrl)
#endif // __linux__
{
	SetHwnd (hCtrl);
}

// ---------------------------------------------------------------------------

#ifndef __linux__
void CustomCtrl::SetHwnd (HWND hCtrl)
#else // __linux__
void CustomCtrl::SetHwnd (QWidget *hCtrl)
#endif // __linux__
{
#ifdef __linux__
	if (hWnd) hWnd->removeEventFilter (this);
#endif // __linux__
	hWnd = hCtrl;
#ifndef __linux__
	SetWindowLongPtr (hWnd, 0, (LONG_PTR)this);
#else // __linux__
	hWnd->installEventFilter (this);
#endif // __linux__

#ifndef __linux__
	hParent = (HWND)GetWindowLongPtr (hCtrl, GWLP_HWNDPARENT);
#else // __linux__
	hParent = hCtrl->parentWidget();
#endif // __linux__
}

// ---------------------------------------------------------------------------

#ifndef __linux__
LRESULT CustomCtrl::WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
bool CustomCtrl::WndProc (QWidget *hWnd, QEvent *event)
#endif // __linux__
{
#ifndef __linux__
	return DefWindowProc (hWnd, uMsg, wParam, lParam);
#else // __linux__
	return false;
#endif // __linux__
}

// ---------------------------------------------------------------------------

#ifndef __linux__
void CustomCtrl::RegisterClass(HINSTANCE hInstance)
#else // __linux__
static QWidget *CreateDlgCtrl (const RESCONTROL*, QWidget *parent)
#endif // __linux__
{
#ifndef __linux__
	WNDCLASSEX wc;
	ZeroMemory(&wc, sizeof(WNDCLASSEX));
	wc.cbSize = sizeof(WNDCLASSEX);
	wc.cbWndExtra = 8;
	wc.hCursor = LoadCursor(hInstance, IDC_ARROW);
	wc.lpfnWndProc = CustomCtrl::s_WndProc;
	wc.lpszClassName = "OrbiterDlgCtrl";
	RegisterClassEx(&wc);
#else // __linux__
	return new QWidget (parent);
}

void CustomCtrl::RegisterClass(void *hInstance)
{
	oapiRegisterResControl (hInstance, "OrbiterDlgCtrl", CreateDlgCtrl);
#endif // __linux__
}

// ---------------------------------------------------------------------------

#ifndef __linux__
LRESULT CALLBACK CustomCtrl::s_WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
bool CustomCtrl::eventFilter (QObject *obj, QEvent *event)
#endif // __linux__
{
#ifndef __linux__
	CustomCtrl* pCtrl = (CustomCtrl*)GetWindowLongPtr(hWnd, 0);
	if (pCtrl) return pCtrl->WndProc(hWnd, uMsg, wParam, lParam);
	else       return DefWindowProc(hWnd, uMsg, wParam, lParam);
#else // __linux__
	if (obj == hWnd) return WndProc (hWnd, event);
	return false;
#endif // __linux__
}

// ===========================================================================

GenericCtrl::GenericCtrl()
	: CustomCtrl()
{
}

#ifndef __linux__
GenericCtrl::GenericCtrl(HWND hCtrl)
#else // __linux__
GenericCtrl::GenericCtrl(QWidget *hCtrl)
#endif // __linux__
	: CustomCtrl(hCtrl)
{
}

// ===========================================================================

SplitterCtrl::SplitterCtrl (): CustomCtrl ()
{
	staticPane = PANE_NONE;
	splitterW = 6;
	widthRatio = 0.5;
	isPushing = false;
#ifndef __linux__
	m_hCursor = LoadCursor(NULL, IDC_SIZEWE);
#endif // !__linux__
}

// ---------------------------------------------------------------------------

#ifndef __linux__
SplitterCtrl::SplitterCtrl (HWND hCtrl): CustomCtrl (hCtrl)
#else // __linux__
SplitterCtrl::SplitterCtrl (QWidget *hCtrl): CustomCtrl (hCtrl)
#endif // __linux__
{
	staticPane = PANE_NONE;
	splitterW = 6;
	widthRatio = 0.5;
	isPushing = false;
}

// ---------------------------------------------------------------------------

#ifndef __linux__
void SplitterCtrl::SetHwnd (HWND hCtrl, HWND hPane1, HWND hPane2)
#else // __linux__
void SplitterCtrl::SetHwnd (QWidget *hCtrl, QWidget *hPane1, QWidget *hPane2)
#endif // __linux__
{
	CustomCtrl::SetHwnd (hCtrl);
#ifdef __linux__
	hCtrl->setCursor (Qt::SizeHorCursor); // WM_SETCURSOR
#endif // __linux__
	hPane[0] = hPane1;
	hPane[1] = hPane2;
#ifndef __linux__
	RECT r;
	GetClientRect (hCtrl, &r);
	totalW = r.right;
	GetClientRect (hPane1, &r);
	paneW[0] = min ((int)r.right, totalW-splitterW-4);
#else // __linux__
	totalW = hCtrl->width();
	paneW[0] = min (hPane1->width(), totalW-splitterW-4);
#endif // __linux__
	paneW[1] = totalW-splitterW-paneW[0];
	widthRatio = (double)paneW[0]/(double)(totalW-splitterW);
	Refresh();
}

// ---------------------------------------------------------------------------

void SplitterCtrl::SetStaticPane (PaneId which, int width)
{
	staticPane = which;
	if (which != PANE_NONE) {
		if (!width) { // use current width
#ifndef __linux__
			RECT rect;
			GetClientRect (hPane[which-1], &rect);
			width = rect.right-rect.left;
#else // __linux__
			width = hPane[which-1]->width();
#endif // __linux__
		}
		paneW[which-1] = width;
		paneW[2-which] = totalW-splitterW-width;
		Refresh();
	}
}

// ---------------------------------------------------------------------------

int SplitterCtrl::GetPaneWidth (PaneId which)
{
	switch (which) {
	case PANE_NONE: return totalW;
	default:        return paneW[which-1];
	}
}

// ---------------------------------------------------------------------------

void SplitterCtrl::Refresh ()
{
	RECT r1, r2;
	r1 = r2 = GetClientPos (hParent, hWnd);
	r1.right = r1.left+paneW[0];
	r2.left = r2.right-paneW[1];
	SetClientPos (hParent, hPane[0], r1);
	SetClientPos (hParent, hPane[1], r2);
}

// ---------------------------------------------------------------------------

#ifndef __linux__
BOOL SplitterCtrl::OnSize (HWND hWnd, WPARAM wParam, int w, int h)
#else // __linux__
BOOL SplitterCtrl::OnSize (QWidget *hWnd, int w, int h)
#endif // __linux__
{
	int w1, w2;
	switch (staticPane) {
	case PANE_NONE: // retain relative widths
		w1 = (int)((w-splitterW)*widthRatio);
		w2 = w-splitterW-w1;
		break;
	case PANE1:
		w1 = min (paneW[0], w-splitterW-4);
		w2 = w-splitterW-w1;
		break;
	case PANE2:
		w2 = min (paneW[1], w-splitterW-4);
		w1 = w-splitterW-w2;
		break;
	}
	paneW[0] = w1;
	paneW[1] = w2;
	totalW = w;
	Refresh ();
	return 0;
}

// ---------------------------------------------------------------------------

#ifndef __linux__
BOOL SplitterCtrl::OnLButtonDown (HWND hWnd, LONG modifier, short x, short y)
#else // __linux__
BOOL SplitterCtrl::OnLButtonDown (QWidget *hWnd, Qt::KeyboardModifiers modifier, short x, short y)
#endif // __linux__
{
	isPushing = true;
	mouseX = x;
	mouseY = y;
#ifndef __linux__
	SetCapture (hWnd);
	return 0;
#else // __linux__
	return 0; // Qt grabs the mouse for the pressed widget (SetCapture)
#endif // __linux__
}

// ---------------------------------------------------------------------------

#ifndef __linux__
BOOL SplitterCtrl::OnLButtonUp (HWND hWnd, LONG modifier, short x, short y)
#else // __linux__
BOOL SplitterCtrl::OnLButtonUp (QWidget *hWnd, Qt::KeyboardModifiers modifier, short x, short y)
#endif // __linux__
{
	isPushing = false;
#ifndef __linux__
	ReleaseCapture ();
#endif // !__linux__
	return 0;
}

// ---------------------------------------------------------------------------

#ifndef __linux__
BOOL SplitterCtrl::OnMouseMove (HWND hWnd, short x, short y)
#else // __linux__
BOOL SplitterCtrl::OnMouseMove (QWidget *hWnd, short x, short y)
#endif // __linux__
{
	if (isPushing) {
		short dx = x - mouseX;
		if (dx) {
			if (dx < 0) {
				paneW[0] = max (4, paneW[0]+dx);
				paneW[1] = totalW-splitterW-paneW[0];
			} else {
				paneW[1] = max (4, paneW[1]-dx);
				paneW[0] = totalW-splitterW-paneW[1];
			}
			Refresh ();
			mouseX = x;
		}
	}
	return 0;
}

// ---------------------------------------------------------------------------

#ifndef __linux__
LRESULT SplitterCtrl::WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
bool SplitterCtrl::WndProc (QWidget *hWnd, QEvent *event)
#endif // __linux__
{
#ifndef __linux__
	switch (uMsg) {
	case WM_SIZE:
		return OnSize (hWnd, wParam, LOWORD(lParam), HIWORD(lParam));
	case WM_LBUTTONDOWN:
		return OnLButtonDown (hWnd, wParam, LOWORD(lParam), HIWORD(lParam));
	case WM_LBUTTONUP:
		return OnLButtonUp (hWnd, wParam, LOWORD(lParam), HIWORD(lParam));
	case WM_MOUSEMOVE:
		return OnMouseMove (hWnd, LOWORD(lParam), HIWORD(lParam));
	case WM_SETCURSOR:
		SetCursor(m_hCursor);
		return TRUE;
#else // __linux__
	QMouseEvent *me = static_cast<QMouseEvent*> (event);
	switch (event->type()) {
	case QEvent::Resize: {
		QSize s = static_cast<QResizeEvent*> (event)->size();
		OnSize (hWnd, s.width(), s.height());
		} return false;
	case QEvent::MouseButtonPress:
		if (me->button() != Qt::LeftButton) break;
		OnLButtonDown (hWnd, me->modifiers(), (short)me->position().x(), (short)me->position().y());
		return true;
	case QEvent::MouseButtonRelease:
		if (me->button() != Qt::LeftButton) break;
		OnLButtonUp (hWnd, me->modifiers(), (short)me->position().x(), (short)me->position().y());
		return true;
	case QEvent::MouseMove:
		OnMouseMove (hWnd, (short)me->position().x(), (short)me->position().y());
		return true;
	default:
		break;
#endif // __linux__
	}
#ifndef __linux__
	return CustomCtrl::WndProc (hWnd, uMsg, wParam, lParam);
#else // __linux__
	return CustomCtrl::WndProc (hWnd, event);
#endif // __linux__
}
