// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "DlgCtrl.h"
#include "DlgCtrlLocal.h"

extern GDIRES g_GDI;
static void OnPaint (HWND hWnd);
static void OnLButtonDown (HWND hWnd, int x, int y);

LRESULT FAR PASCAL MsgProc_Switch (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_CREATE:
		SetWindowLongPtr (hWnd, 0, 0);
		SetWindowLongPtr (hWnd, 4, 0);
		return 0;
	case WM_PAINT:
		OnPaint (hWnd);
		return 0;
	case WM_LBUTTONDOWN:
		OnLButtonDown (hWnd, LOWORD (lParam), HIWORD (lParam));
		return 0;
	}
	return DefWindowProc (hWnd, uMsg, wParam, lParam);
}

void OnPaint (HWND hWnd)
{
	PAINTSTRUCT ps;
	RECT r;
	POINT pt[6];
	int w, h, xc, yc;
	HDC hDC = BeginPaint (hWnd, &ps);
	GetClientRect (hWnd, &r);
	w = r.right; h = r.bottom;
	xc = w/2; yc = h/2;
	int pos = (int)GetWindowLongPtr (hWnd, 0);
	DWORD flag = (DWORD)GetWindowLongPtr (hWnd, 4);
	bool vert = (!(flag & 0x4));
	if (vert) {
		int rad = (xc*8)/10;
		int irad = xc/2;
		int d = (int)(0.57735*(xc-1));
		int e = (int)(1.1547*(xc-1));
		int f = (xc*2)/5;
		int f2 = (xc*5)/10;
		int f3 = (xc*6)/10;
		int g1 = yc-1;
		int g2 = g1-xc/3;
		pt[0].x = pt[5].x = 1;    pt[0].y = pt[2].y = yc-d;
		pt[1].x = pt[4].x = xc;   pt[5].y = pt[3].y = yc+d;
		pt[2].x = pt[3].x = 2*xc-1;  pt[1].y = yc-e; pt[4].y = yc+e;
		SelectObject (hDC, GetStockObject (LTGRAY_BRUSH));
		SelectObject (hDC, GetStockObject (WHITE_PEN));
		Polygon (hDC, pt, 6);
		SelectObject (hDC, g_GDI.hPen2);
		Polyline (hDC, pt+2, 4);
		SelectObject (hDC, GetStockObject (WHITE_PEN));
		SelectObject (hDC, GetStockObject (LTGRAY_BRUSH));
		Ellipse (hDC, xc-rad, yc-rad, xc+rad+1, yc+rad+1);
		SelectObject (hDC, GetStockObject (GRAY_BRUSH));
		Ellipse (hDC, xc-irad, yc-irad, xc+irad+1, yc+irad+1);
		SelectObject (hDC, g_GDI.hPen2);
		Arc (hDC, xc-rad, yc-rad, xc+rad+1, yc+rad+1, xc-10, yc+10, xc+10, yc-10);
		Arc (hDC, xc-irad, yc-irad, xc+irad+1, yc+irad+1, xc+10, yc-10, xc-10, yc+10);
		if (pos == 0) {
			SelectObject (hDC, g_GDI.hPen2);
			SelectObject (hDC, GetStockObject (LTGRAY_BRUSH));
			pt[0].x = xc-f; pt[1].x = xc-f2; pt[2].x = xc+f2; pt[3].x = xc+f;
			pt[0].y = pt[3].y = yc+1; pt[1].y = pt[2].y = yc-g2;
			Polygon (hDC, pt, 4);
			SelectObject (hDC, GetStockObject (WHITE_BRUSH));
			Rectangle (hDC, xc-f2, yc-g2, xc+f2+1, yc-g1);
			SelectObject (hDC, GetStockObject (WHITE_PEN));
			MoveToEx (hDC, xc-f, yc+1, NULL);
			LineTo (hDC, xc-f2, yc-g2);
			LineTo (hDC, xc-f2, yc-g1);
			LineTo (hDC, xc+f2, yc-g1);
		} else if (pos == 1) {
			SelectObject (hDC, g_GDI.hPen2);
			SelectObject (hDC, GetStockObject (WHITE_BRUSH));
			pt[0].x = xc-f; pt[1].x = xc-f2; pt[2].x = xc+f2; pt[3].x = xc+f;
			pt[0].y = pt[3].y = yc-1; pt[1].y = pt[2].y = yc+g2;
			Polygon (hDC, pt, 4);
			SelectObject (hDC, GetStockObject (LTGRAY_BRUSH));
			Rectangle (hDC, xc-f2, yc+g2, xc+f2+1, yc+g1);
			SelectObject (hDC, GetStockObject (WHITE_PEN));
			MoveToEx (hDC, xc-f, yc-1, NULL);
			LineTo (hDC, xc-f2, yc+g2);
			LineTo (hDC, xc-f2, yc+g1);
			LineTo (hDC, xc+f2, yc+g1);
		} else {
			SelectObject (hDC, g_GDI.hPen2);
			pt[0].x = xc-f; pt[1].x = xc-f3; pt[2].x = xc+f3; pt[3].x = xc+f;
			pt[0].y = pt[3].y = yc-(xc/2); pt[1].y = pt[2].y = yc-xc/4;
			SelectObject (hDC, GetStockObject (LTGRAY_BRUSH));
			Polygon (hDC, pt, 4);
			pt[0].y = pt[3].y = yc+xc/2; pt[1].y = pt[2].y = yc+xc/4;
			SelectObject (hDC, GetStockObject (GRAY_BRUSH));
			Polygon (hDC, pt, 4);
			SelectObject (hDC, GetStockObject (WHITE_BRUSH));
			Rectangle (hDC, xc-f3, yc-xc/4, xc+f3+1, yc+xc/4+1);
			SelectObject (hDC, GetStockObject (WHITE_PEN));
		}
	}
	EndPaint (hWnd, &ps);
}

void OnLButtonDown (HWND hWnd, int x, int y)
{
	RECT r;
	GetClientRect (hWnd, &r);
	int pos = (int)GetWindowLongPtr (hWnd, 0);
	int npos = pos;
	DWORD flag = (DWORD)GetWindowLongPtr (hWnd, 4);
	bool is3 = ((flag & 0x1) != 0);
	if (y >= r.bottom/2) {
		if (pos == 0) npos = (is3 ? 2 : 1);
		else if (pos == 2) npos = 1;
	} else {
		if (pos == 1) npos = (is3 ? 2 : 0);
		else if (pos == 2) npos = 0;
	}
	if (pos != npos) {
		SetWindowLongPtr (hWnd, 0, npos);
		InvalidateRect (hWnd, NULL, TRUE);
		PostMessage (GetParent (hWnd), WM_COMMAND, MAKEWPARAM(GetDlgCtrlID (hWnd), BN_CLICKED), npos);
	}
}

void oapiSetSwitchParams (HWND hCtrl, SWITCHPARAM *sp, bool redraw)
{
	DWORD flag = 0;
	if (sp->mode  == SWITCHPARAM::THREESTATE) flag |= 0x1;
	if (sp->align == SWITCHPARAM::HORIZONTAL) flag |= 0x4;
	SetWindowLongPtr (hCtrl, 0, 0);
	SetWindowLongPtr (hCtrl, 4, flag);
}

int oapiSetSwitchState (HWND hCtrl, int state, bool redraw)
{
	if (state < 0 || state > 2) return -1;
	if (state == 2) {
		DWORD flag = (DWORD)GetWindowLongPtr (hCtrl, 4);
		if (!(flag & 0x1)) return -1;
	}
	SetWindowLongPtr (hCtrl, 0, state);
	if (redraw) InvalidateRect (hCtrl, NULL, TRUE);
	return state;
}

int oapiGetSwitchState (HWND hCtrl)
{
	return GetWindowLongPtr (hCtrl, 0);
}
