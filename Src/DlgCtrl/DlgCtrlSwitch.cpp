// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "DlgCtrl.h"
#include "DlgCtrlLocal.h"
#ifdef __linux__
#include <QMouseEvent>
#include <QPainter>
#endif // __linux__

extern GDIRES g_GDI;
#ifndef __linux__
static void OnPaint (HWND hWnd);
static void OnLButtonDown (HWND hWnd, int x, int y);
#else // __linux__
void GdiRectangle (QPainter &p, int l, int t, int r, int b);
#endif // __linux__

#ifndef __linux__
LRESULT FAR PASCAL MsgProc_Switch (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
SwitchCtrl::SwitchCtrl (QWidget *parent): QWidget (parent)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	QPalette pal = palette();
	pal.setColor (QPalette::Window, g_GDI.hBrush2); // class background
	setPalette (pal);
	setAutoFillBackground (true);
}

// GDI Ellipse (l,t,r,b) with its [l,r-1] x [t,b-1] bounding box
static void GdiEllipse (QPainter &p, int l, int t, int r, int b)
{
	p.drawEllipse (l, t, r-l-1, b-t-1);
#endif // __linux__
}

#ifndef __linux__
void OnPaint (HWND hWnd)
#else // __linux__
void SwitchCtrl::paintEvent (QPaintEvent*)
#endif // __linux__
{
#ifndef __linux__
	PAINTSTRUCT ps;
	RECT r;
	POINT pt[6];
#else // __linux__
	QPoint pt[6];
#endif // __linux__
	int w, h, xc, yc;
#ifndef __linux__
	HDC hDC = BeginPaint (hWnd, &ps);
	GetClientRect (hWnd, &r);
	w = r.right; h = r.bottom;
#else // __linux__
	QPainter p (this);
	w = width(); h = height();
#endif // __linux__
	xc = w/2; yc = h/2;
#ifndef __linux__
	int pos = (int)GetWindowLongPtr (hWnd, 0);
	DWORD flag = (DWORD)GetWindowLongPtr (hWnd, 4);
#else // __linux__
	const QBrush ltgray (QColor (0xc0, 0xc0, 0xc0)), gray (QColor (0x80, 0x80, 0x80)), white (Qt::white);
#endif // __linux__
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
#ifndef __linux__
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
#else // __linux__
		pt[0].setX (1);         pt[5].setX (1);         pt[0].setY (yc-d); pt[2].setY (yc-d);
		pt[1].setX (xc);        pt[4].setX (xc);        pt[5].setY (yc+d); pt[3].setY (yc+d);
		pt[2].setX (2*xc-1);    pt[3].setX (2*xc-1);    pt[1].setY (yc-e); pt[4].setY (yc+e);
		p.setBrush (ltgray);
		p.setPen (Qt::white);
		p.drawPolygon (pt, 6);
		p.setPen (g_GDI.hPen2);
		p.drawPolyline (pt+2, 4);
		p.setPen (Qt::white);
		p.setBrush (ltgray);
		GdiEllipse (p, xc-rad, yc-rad, xc+rad+1, yc+rad+1);
		p.setBrush (gray);
		GdiEllipse (p, xc-irad, yc-irad, xc+irad+1, yc+irad+1);
		p.setPen (g_GDI.hPen2);
		p.drawArc (xc-rad, yc-rad, 2*rad, 2*rad, 225*16, 180*16);   // lower right half
		p.drawArc (xc-irad, yc-irad, 2*irad, 2*irad, 45*16, 180*16); // upper left half
#endif // __linux__
		if (pos == 0) {
#ifndef __linux__
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
#else // __linux__
			p.setPen (g_GDI.hPen2);
			p.setBrush (ltgray);
			pt[0].setX (xc-f); pt[1].setX (xc-f2); pt[2].setX (xc+f2); pt[3].setX (xc+f);
			pt[0].setY (yc+1); pt[3].setY (yc+1); pt[1].setY (yc-g2); pt[2].setY (yc-g2);
			p.drawPolygon (pt, 4);
			p.setBrush (white);
			GdiRectangle (p, xc-f2, yc-g1, xc+f2+1, yc-g2);
			p.setPen (Qt::white);
			p.drawPolyline (QPolygon ({QPoint (xc-f, yc+1), QPoint (xc-f2, yc-g2), QPoint (xc-f2, yc-g1), QPoint (xc+f2, yc-g1)}));
#endif // __linux__
		} else if (pos == 1) {
#ifndef __linux__
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
#else // __linux__
			p.setPen (g_GDI.hPen2);
			p.setBrush (white);
			pt[0].setX (xc-f); pt[1].setX (xc-f2); pt[2].setX (xc+f2); pt[3].setX (xc+f);
			pt[0].setY (yc-1); pt[3].setY (yc-1); pt[1].setY (yc+g2); pt[2].setY (yc+g2);
			p.drawPolygon (pt, 4);
			p.setBrush (ltgray);
			GdiRectangle (p, xc-f2, yc+g2, xc+f2+1, yc+g1);
			p.setPen (Qt::white);
			p.drawPolyline (QPolygon ({QPoint (xc-f, yc-1), QPoint (xc-f2, yc+g2), QPoint (xc-f2, yc+g1), QPoint (xc+f2, yc+g1)}));
#endif // __linux__
		} else {
#ifndef __linux__
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
#else // __linux__
			p.setPen (g_GDI.hPen2);
			pt[0].setX (xc-f); pt[1].setX (xc-f3); pt[2].setX (xc+f3); pt[3].setX (xc+f);
			pt[0].setY (yc-(xc/2)); pt[3].setY (yc-(xc/2)); pt[1].setY (yc-xc/4); pt[2].setY (yc-xc/4);
			p.setBrush (ltgray);
			p.drawPolygon (pt, 4);
			pt[0].setY (yc+xc/2); pt[3].setY (yc+xc/2); pt[1].setY (yc+xc/4); pt[2].setY (yc+xc/4);
			p.setBrush (gray);
			p.drawPolygon (pt, 4);
			p.setBrush (white);
			GdiRectangle (p, xc-f3, yc-xc/4, xc+f3+1, yc+xc/4+1);
			p.setPen (Qt::white);
#endif // __linux__
		}
	}
#ifndef __linux__
	EndPaint (hWnd, &ps);
#endif // !__linux__
}

#ifndef __linux__
void OnLButtonDown (HWND hWnd, int x, int y)
#else // __linux__
void SwitchCtrl::mousePressEvent (QMouseEvent *event)
#endif // __linux__
{
#ifndef __linux__
	RECT r;
	GetClientRect (hWnd, &r);
	int pos = (int)GetWindowLongPtr (hWnd, 0);
#else // __linux__
	if (event->button() != Qt::LeftButton) return;
	int y = (int)event->position().y();
#endif // __linux__
	int npos = pos;
#ifndef __linux__
	DWORD flag = (DWORD)GetWindowLongPtr (hWnd, 4);
#endif // !__linux__
	bool is3 = ((flag & 0x1) != 0);
#ifndef __linux__
	if (y >= r.bottom/2) {
#else // __linux__
	if (y >= height()/2) {
#endif // __linux__
		if (pos == 0) npos = (is3 ? 2 : 1);
		else if (pos == 2) npos = 1;
	} else {
		if (pos == 1) npos = (is3 ? 2 : 0);
		else if (pos == 2) npos = 0;
	}
	if (pos != npos) {
#ifndef __linux__
		SetWindowLongPtr (hWnd, 0, npos);
		InvalidateRect (hWnd, NULL, TRUE);
		PostMessage (GetParent (hWnd), WM_COMMAND, MAKEWPARAM(GetDlgCtrlID (hWnd), BN_CLICKED), npos);
#else // __linux__
		pos = npos;
		update();
		emit clicked (npos);
#endif // __linux__
	}
}

#ifndef __linux__
void oapiSetSwitchParams (HWND hCtrl, SWITCHPARAM *sp, bool redraw)
#else // __linux__
void oapiSetSwitchParams (QWidget *hCtrl, SWITCHPARAM *sp, bool redraw)
#endif // __linux__
{
#ifdef __linux__
	SwitchCtrl *s = qobject_cast<SwitchCtrl*> (hCtrl);
	if (!s) return;
#endif // __linux__
	DWORD flag = 0;
	if (sp->mode  == SWITCHPARAM::THREESTATE) flag |= 0x1;
	if (sp->align == SWITCHPARAM::HORIZONTAL) flag |= 0x4;
#ifndef __linux__
	SetWindowLongPtr (hCtrl, 0, 0);
	SetWindowLongPtr (hCtrl, 4, flag);
#else // __linux__
	s->pos = 0;
	s->flag = flag;
#endif // __linux__
}

#ifndef __linux__
int oapiSetSwitchState (HWND hCtrl, int state, bool redraw)
#else // __linux__
int oapiSetSwitchState (QWidget *hCtrl, int state, bool redraw)
#endif // __linux__
{
#ifdef __linux__
	SwitchCtrl *s = qobject_cast<SwitchCtrl*> (hCtrl);
	if (!s) return -1;
#endif // __linux__
	if (state < 0 || state > 2) return -1;
	if (state == 2) {
#ifndef __linux__
		DWORD flag = (DWORD)GetWindowLongPtr (hCtrl, 4);
		if (!(flag & 0x1)) return -1;
#else // __linux__
		if (!(s->flag & 0x1)) return -1;
#endif // __linux__
	}
#ifndef __linux__
	SetWindowLongPtr (hCtrl, 0, state);
	if (redraw) InvalidateRect (hCtrl, NULL, TRUE);
#else // __linux__
	s->pos = state;
	if (redraw) s->update();
#endif // __linux__
	return state;
}

#ifndef __linux__
int oapiGetSwitchState (HWND hCtrl)
#else // __linux__
int oapiGetSwitchState (QWidget *hCtrl)
#endif // __linux__
{
#ifndef __linux__
	return GetWindowLongPtr (hCtrl, 0);
#else // __linux__
	SwitchCtrl *s = qobject_cast<SwitchCtrl*> (hCtrl);
	return (s ? s->pos : 0);
#endif // __linux__
}
