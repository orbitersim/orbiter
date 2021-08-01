// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <windows.h>
#include "CustomControls.h"
#include "Util.h"

// ===========================================================================

CustomCtrl::CustomCtrl ()
{
	hWnd = NULL;
	hParent = NULL;
}

// ---------------------------------------------------------------------------

CustomCtrl::CustomCtrl (HWND hCtrl)
{
	SetHwnd (hCtrl);
}

// ---------------------------------------------------------------------------

void CustomCtrl::SetHwnd (HWND hCtrl)
{
	hWnd = hCtrl;
	SetWindowLongPtr (hWnd, 0, (LONG_PTR)this);

	hParent = (HWND)GetWindowLongPtr (hCtrl, GWLP_HWNDPARENT);
}

// ---------------------------------------------------------------------------

LRESULT CustomCtrl::WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return DefWindowProc (hWnd, uMsg, wParam, lParam);
}

// ===========================================================================

SplitterCtrl::SplitterCtrl (): CustomCtrl ()
{
	staticPane = PANE_NONE;
	splitterW = 6;
	widthRatio = 0.5;
	isPushing = false;
}

// ---------------------------------------------------------------------------

SplitterCtrl::SplitterCtrl (HWND hCtrl): CustomCtrl (hCtrl)
{
	staticPane = PANE_NONE;
	splitterW = 6;
	widthRatio = 0.5;
	isPushing = false;
}

// ---------------------------------------------------------------------------

void SplitterCtrl::SetHwnd (HWND hCtrl, HWND hPane1, HWND hPane2)
{
	CustomCtrl::SetHwnd (hCtrl);
	hPane[0] = hPane1;
	hPane[1] = hPane2;
	RECT r;
	GetClientRect (hCtrl, &r);
	totalW = r.right;
	GetClientRect (hPane1, &r);
	paneW[0] = min (r.right, totalW-splitterW-4);
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
			RECT rect;
			GetClientRect (hPane[which-1], &rect);
			width = rect.right-rect.left;
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

void SplitterCtrl::RegisterClass (HINSTANCE hInstance)
{
	WNDCLASSEX wc;
	ZeroMemory (&wc, sizeof(WNDCLASSEX));
	wc.cbSize = sizeof(WNDCLASSEX);
	wc.cbWndExtra = 8;
	wc.hCursor = LoadCursor (NULL, IDC_SIZEWE);
	wc.lpfnWndProc = SplitterCtrl::WndProcHook;
	wc.lpszClassName = "SplitterCtrl";
	RegisterClassEx (&wc);
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

BOOL SplitterCtrl::OnSize (HWND hWnd, WPARAM wParam, int w, int h)
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

BOOL SplitterCtrl::OnLButtonDown (HWND hWnd, LONG modifier, short x, short y)
{
	isPushing = true;
	mouseX = x;
	mouseY = y;
	SetCapture (hWnd);
	return 0;
}

// ---------------------------------------------------------------------------

BOOL SplitterCtrl::OnLButtonUp (HWND hWnd, LONG modifier, short x, short y)
{
	isPushing = false;
	ReleaseCapture ();
	return 0;
}

// ---------------------------------------------------------------------------

BOOL SplitterCtrl::OnMouseMove (HWND hWnd, short x, short y)
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

LRESULT SplitterCtrl::WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_SIZE:
		return OnSize (hWnd, wParam, LOWORD(lParam), HIWORD(lParam));
	case WM_LBUTTONDOWN:
		return OnLButtonDown (hWnd, wParam, LOWORD(lParam), HIWORD(lParam));
	case WM_LBUTTONUP:
		return OnLButtonUp (hWnd, wParam, LOWORD(lParam), HIWORD(lParam));
	case WM_MOUSEMOVE:
		return OnMouseMove (hWnd, LOWORD(lParam), HIWORD(lParam));
	}
	return CustomCtrl::WndProc (hWnd, uMsg, wParam, lParam);
}

// ---------------------------------------------------------------------------

LRESULT CALLBACK SplitterCtrl::WndProcHook (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	SplitterCtrl *pCtrl = (SplitterCtrl*)GetWindowLongPtr (hWnd, 0);
	if (pCtrl) return pCtrl->WndProc (hWnd, uMsg, wParam, lParam);
	else       return DefWindowProc (hWnd, uMsg, wParam, lParam);
}
