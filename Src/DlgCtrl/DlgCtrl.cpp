// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "DlgCtrl.h"
#include "DlgCtrlLocal.h"
#include <stdio.h>


GDIRES g_GDI;

static UINT g_timer = 0;
static HINSTANCE hInstModule = NULL;

static const DWORD LongPtrSize = sizeof(LONG_PTR);
static const DWORD WINOFS_POS = 0;
static const DWORD WINOFS_RMIN = LongPtrSize;
static const DWORD WINOFS_RMAX = LongPtrSize * 2;
static const DWORD WINOFS_FLAG = LongPtrSize * 3;
static const DWORD WINBUF_SIZE = LongPtrSize * 4;

void DragSlider(HWND hWnd, int x, int y);

void oapiRegisterCustomControls (HINSTANCE hInst)
{
	WNDCLASS wndClass;
	g_GDI.hPen1 = CreatePen (PS_SOLID, 1, 0x404040);
	g_GDI.hPen2 = CreatePen (PS_SOLID, 1, GetSysColor (COLOR_3DSHADOW));
	g_GDI.hBrush1 = CreateSolidBrush (0x0000ff);
	g_GDI.hBrush2 = CreateSolidBrush (GetSysColor (COLOR_3DFACE));

	// Register window class for level indicator
	wndClass.style = CS_HREDRAW | CS_VREDRAW;
	wndClass.lpfnWndProc   = MsgProc_Gauge;
	wndClass.cbClsExtra    = 0;
	wndClass.cbWndExtra    = WINBUF_SIZE;
	wndClass.hInstance     = hInst;
	wndClass.hIcon         = NULL;
	wndClass.hCursor       = NULL;
	wndClass.hbrBackground = (HBRUSH)GetStockObject (LTGRAY_BRUSH);
	wndClass.lpszMenuName  = NULL;
	wndClass.lpszClassName = "OrbiterCtrl_Gauge";
	RegisterClass (&wndClass);

	// Register window class for switch
	wndClass.lpfnWndProc   = MsgProc_Switch;
	wndClass.cbWndExtra    = 8;
	wndClass.hbrBackground = g_GDI.hBrush2; //(HBRUSH)GetStockObject (NULL_BRUSH);
	wndClass.lpszClassName = "OrbiterCtrl_Switch";
	RegisterClass (&wndClass);

	// Register window class for property list
	RegisterPropertyList (hInst);
}

void oapiUnregisterCustomControls (HINSTANCE hInst)
{
	UnregisterClass ("OrbiterCtrl_Gauge", hInst);
	UnregisterClass ("OrbiterCtrl_Switch", hInst);
	UnregisterPropertyList (hInst);

	DeleteObject (g_GDI.hPen1);
	DeleteObject (g_GDI.hPen2);
	DeleteObject (g_GDI.hBrush1);
	DeleteObject (g_GDI.hBrush2);
}

LRESULT FAR PASCAL MsgProc_Gauge (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	static HWND hPrevCapt = NULL;
	switch (uMsg) {
	case WM_PAINT: {
		PAINTSTRUCT ps;
		RECT r;
		DWORD bw, gw, x0, y0, dd;
		HDC hDC    = BeginPaint (hWnd, &ps);
		int pos    = GetWindowLongPtr (hWnd, WINOFS_POS);
		int rmin   = GetWindowLongPtr (hWnd, WINOFS_RMIN);
		int rmax   = GetWindowLongPtr (hWnd, WINOFS_RMAX);
		DWORD flag = GetWindowLongPtr (hWnd, WINOFS_FLAG);
		bool horz  = ((flag & 2) == 0);
		bool enabled = (GetWindowLongPtr(hWnd, GWL_STYLE) & WS_DISABLED) == 0;

		GetClientRect (hWnd, &r);
		SelectObject (hDC, g_GDI.hPen1);
		SelectObject(hDC, GetStockObject(enabled ? WHITE_BRUSH : NULL_BRUSH));
		if (horz) {
			bw = r.bottom; dd = bw / 4; gw = r.right - 2 * bw;
			Rectangle(hDC, 0, 0, bw, bw);
			Rectangle(hDC, r.right - bw, 0, r.right, bw);
			MoveToEx(hDC, x0 = dd, y0 = bw / 2, NULL);
			LineTo(hDC, x0 += dd, y0 -= dd);
			LineTo(hDC, x0, y0 += 2 * dd);
			LineTo(hDC, x0 -= dd, y0 -= dd);
			MoveToEx(hDC, x0 = r.right - dd - 1, y0 = bw / 2, NULL);
			LineTo(hDC, x0 -= dd, y0 -= dd);
			LineTo(hDC, x0, y0 += 2 * dd);
			LineTo(hDC, x0 += dd, y0 -= dd);
		} else {
			bw = r.right; dd = bw / 4;  gw = r.bottom - 2 * bw;
			Rectangle(hDC, 0, 0, bw, bw);
			Rectangle(hDC, 0, r.bottom - bw, bw, r.bottom);
			MoveToEx(hDC, x0 = bw / 2, y0 = dd, NULL);
			LineTo(hDC, x0 += dd, y0 += dd);
			LineTo(hDC, x0 -= 2 * dd, y0);
			LineTo(hDC, x0 += dd, y0 -= dd);
			MoveToEx(hDC, x0 = bw / 2, y0 = r.bottom - dd - 1, NULL);
			LineTo(hDC, x0 -= dd, y0 -= dd);
			LineTo(hDC, x0 += 2 * dd, y0);
			LineTo(hDC, x0 -= dd, y0 += dd);
		}

		SelectObject (hDC, GetStockObject (NULL_PEN));
		SelectObject (hDC, (flag & 0x40) ? g_GDI.hBrush1 : GetStockObject (enabled ? BLACK_BRUSH : GRAY_BRUSH));
		if (rmax > rmin) {
			switch (flag & 3) { // direction flag
			case 0: // left to right
				Rectangle (hDC, bw, 0, bw+(gw*(pos-rmin))/(rmax-rmin) + 1, bw + 1);
				break;
			case 1: // right to left
				Rectangle (hDC, bw+gw, 0, bw+gw-(gw*(pos-rmin))/(rmax-rmin) + 1, bw + 1);
				break;
			case 2: // top to bottom
				Rectangle (hDC, 0, bw, bw + 1, bw+(gw*(pos-rmin))/(rmax-rmin) + 1);
				break;
			case 3: // bottom to top
				Rectangle (hDC, 0, bw+gw, bw + 1, bw+gw-(gw*(pos-rmin))/(rmax-rmin) + 1);
				break;
			}
		}

		EndPaint (hWnd, &ps);
		} return 0;

	case WM_LBUTTONDOWN: {
		int x = LOWORD (lParam);
		int y = HIWORD (lParam);
		RECT r;
		DWORD bw, gw;
		DWORD flag = GetWindowLongPtr (hWnd, WINOFS_FLAG);
		bool horz = ((flag & 2) == 0);
		GetClientRect (hWnd, &r);
		if (horz) {
			bw = r.bottom; gw = r.right - 2*bw;
			if (x < (int)bw) {
				flag |= 4;                        // flag for left button pressed
				flag |= ((flag & 1) ? 0x20:0x10); // flag for inc/dec button pressed
			} else if (x >= (int)(gw+bw)) {
				flag |= 8;                        // flag for right button pressed
				flag |= ((flag & 1) ? 0x10:0x20); // flag for inc/dec button pressed
			} else {
                flag |= 12;                       // flag for slider area pressed
			}
		} else {
			bw = r.right; gw = r.bottom - 2*bw;
			if (y < (int)bw) {
				flag |= 4;                        // flag for top button pressed
				flag |= ((flag & 1) ? 0x20:0x10); // flag for inc/dec button pressed
			} else if (y >= (int)(gw+bw)) {
				flag |= 8;                        // flag for bottom button pressed
				flag |= ((flag & 1) ? 0x10:0x20); // flag for inc/dec button pressed
			} else {
				flag |= 12;                       // flag for slider area pressed
			}
		}
		hPrevCapt = SetCapture (hWnd);
		SetWindowLongPtr (hWnd, WINOFS_FLAG, flag);
		if ((flag & 12) == 12) {
			DragSlider (hWnd, x, y);
			SendMessage (GetParent (hWnd), WM_HSCROLL, MAKEWPARAM (SB_THUMBTRACK, GetWindowLongPtr (hWnd, WINOFS_POS)), LPARAM (hWnd));
		} else {
			int rmin = GetWindowLongPtr (hWnd, WINOFS_RMIN);
			int rmax = GetWindowLongPtr (hWnd, WINOFS_RMAX);
			g_timer = SetTimer (hWnd, 1, min(1000,max(1,3000/(rmax-rmin))), NULL);
			PostMessage (hWnd, WM_TIMER, 1, 0);
		}
		} return 0;
		
	case WM_LBUTTONUP: {
		ReleaseCapture();
		if (hPrevCapt) SetCapture (hPrevCapt);
		if (g_timer) {
			KillTimer (hWnd, 1);
			g_timer = 0;
		}
		DWORD flag = GetWindowLongPtr (hWnd, WINOFS_FLAG);
		flag &= 0xFFFFFFC3; // clear mouse selection state
		SetWindowLongPtr (hWnd, WINOFS_FLAG, flag);
		} return 0;

	case WM_MOUSEMOVE: {
		DWORD flag = GetWindowLongPtr (hWnd, WINOFS_FLAG);
		if ((flag & 12) == 12) {
			DragSlider (hWnd, (short)LOWORD(lParam), (short)HIWORD(lParam));
			SendMessage (GetParent (hWnd), WM_HSCROLL, MAKEWPARAM (SB_THUMBTRACK, GetWindowLongPtr (hWnd, WINOFS_POS)), (LPARAM)hWnd);
		}
		} return 0;

	case WM_ENABLE:
		InvalidateRect(hWnd, 0, TRUE);
		return TRUE;

	case BM_GETSTATE:
		return (GetWindowLongPtr (hWnd, WINOFS_FLAG) >> 4) & 3;

	case WM_TIMER:
		int pos    = GetWindowLongPtr (hWnd, WINOFS_POS);
		int rmin   = GetWindowLongPtr (hWnd, WINOFS_RMIN);
		int rmax   = GetWindowLongPtr (hWnd, WINOFS_RMAX);
		DWORD flag = GetWindowLongPtr (hWnd, WINOFS_FLAG);

		switch ((flag >> 4) & 3) {
		case 1: // decrease
			if (pos > rmin) SetWindowLongPtr (hWnd, WINOFS_POS, --pos);
			InvalidateRect (hWnd, NULL, TRUE);
			SendMessage (GetParent (hWnd), WM_HSCROLL, MAKEWPARAM (SB_LINELEFT, pos), (LPARAM)hWnd);
			break;
		case 2: // increase
			if (pos < rmax) SetWindowLongPtr (hWnd, WINOFS_POS, ++pos);
			InvalidateRect (hWnd, NULL, TRUE);
			SendMessage (GetParent (hWnd), WM_HSCROLL, MAKEWPARAM (SB_LINERIGHT, pos), (LPARAM)hWnd);
			break;
		}
		return 0;
	}
	return DefWindowProc (hWnd, uMsg, wParam, lParam);
}

void DragSlider (HWND hWnd, int x, int y)
{
	RECT r;
	DWORD bw, gw;
	int pos;
	DWORD flag = GetWindowLongPtr (hWnd, WINOFS_FLAG);
	GetClientRect (hWnd, &r);

	bool horz = ((flag & 2) == 0);
	if (horz) {
		bw = r.bottom; gw = r.right - 2*bw;
		pos = x-(int)bw;
	} else {
		bw = r.right; gw = r.bottom - 2*bw;
		pos = y-(int)bw;
	}
	if (pos < 0) pos = 0; else if (pos >= (int)gw) pos = (int)gw;
	if (flag & 1) pos = gw-pos;

	int rmin = GetWindowLongPtr (hWnd, WINOFS_RMIN);
	int rmax = GetWindowLongPtr (hWnd, WINOFS_RMAX);
	if (rmax > rmin) {
		SetWindowLongPtr (hWnd, WINOFS_POS, (pos*(rmax-rmin))/gw+rmin);
		InvalidateRect (hWnd, NULL, TRUE);
	}
}

void oapiSetGaugeParams (HWND hCtrl, GAUGEPARAM *gp, bool redraw)
{
	SetWindowLongPtr (hCtrl, WINOFS_RMIN, gp->rangemin);
	SetWindowLongPtr (hCtrl, WINOFS_RMAX, gp->rangemax);

	int pos = (int)GetWindowLongPtr (hCtrl, WINOFS_POS);
	if      (pos < gp->rangemin) SetWindowLongPtr (hCtrl, WINOFS_POS, gp->rangemin);
	else if (pos > gp->rangemax) SetWindowLongPtr (hCtrl, WINOFS_POS, gp->rangemax);

	DWORD flag = 0;
	switch (gp->base) {
	case GAUGEPARAM::LEFT:   flag = 0; break;
	case GAUGEPARAM::RIGHT:  flag = 1; break;
	case GAUGEPARAM::TOP:    flag = 2; break;
	case GAUGEPARAM::BOTTOM: flag = 3; break;
	}
	switch (gp->color) {
	case GAUGEPARAM::BLACK:                break;
	case GAUGEPARAM::RED:    flag |= 0x40; break;
	}
	SetWindowLongPtr (hCtrl, WINOFS_FLAG, flag);

	if (redraw) InvalidateRect (hCtrl, NULL, TRUE);
}

void oapiSetGaugeRange (HWND hCtrl, int rmin, int rmax, bool redraw)
{
	SetWindowLongPtr (hCtrl, WINOFS_RMIN, rmin);
	SetWindowLongPtr (hCtrl, WINOFS_RMAX, rmax);

	int pos = (int)GetWindowLongPtr (hCtrl, WINOFS_POS);
	if      (pos < rmin) SetWindowLongPtr (hCtrl, WINOFS_POS, rmin);
	else if (pos > rmax) SetWindowLongPtr (hCtrl, WINOFS_POS, rmax);

	if (redraw) InvalidateRect (hCtrl, NULL, TRUE);
}

int oapiSetGaugePos (HWND hCtrl, int pos, bool redraw)
{
	int rmin = GetWindowLongPtr(hCtrl, WINOFS_RMIN);
	int rmax = GetWindowLongPtr(hCtrl, WINOFS_RMAX);
	if      (pos < rmin) pos = rmin;
	else if (pos > rmax) pos = rmax;

	int oldPos = GetWindowLongPtr(hCtrl, WINOFS_POS);
	if (pos != oldPos) {
		SetWindowLongPtr(hCtrl, WINOFS_POS, pos);
		if (redraw) InvalidateRect(hCtrl, NULL, TRUE);
	}

	return pos;
}

int oapiIncGaugePos (HWND hCtrl, int dpos, bool redraw)
{
	int rmin = GetWindowLongPtr (hCtrl, WINOFS_RMIN);
	int rmax = GetWindowLongPtr (hCtrl, WINOFS_RMAX);
	int pos  = GetWindowLongPtr (hCtrl, WINOFS_POS) + dpos;

	if      (pos < rmin) pos = rmin;
	else if (pos > rmax) pos = rmax;

	SetWindowLongPtr (hCtrl, WINOFS_POS, pos);
	if (redraw) InvalidateRect (hCtrl, NULL, TRUE);

	return pos;
}

int oapiGetGaugePos (HWND hCtrl)
{
	return GetWindowLongPtr (hCtrl, WINOFS_POS);
}

// ==================================================================================
// ==================================================================================

PropertyItem::PropertyItem (PropertyGroup *grp)
{
	label = NULL;
	value = NULL;
	labelw = -1;
	valuew = -1;
	label_dirty = false;
	value_dirty = false;
	group = grp;
}

PropertyItem::~PropertyItem ()
{
	if (label) delete []label;
	if (value) delete []value;
}

void PropertyItem::SetLabel (const char *newlabel)
{
	label_dirty = false;
	if (newlabel && label) {
		label_dirty = (strcmp (label, newlabel) != 0);
	} else if (newlabel || label) {
		label_dirty = true;
	}
	if (label_dirty) {
		if (label) delete []label;
		if (newlabel) {
			label = new char[strlen(newlabel)+1];
			strcpy (label, newlabel);
		} else label = NULL;
		if (!group->IsExpanded()) label_dirty = false;
	}
}

void PropertyItem::SetValue (const char *newvalue)
{
	value_dirty = false;
	if (newvalue && value) {
		value_dirty = (strcmp (value, newvalue) != 0);
	} else if (newvalue || value) {
		value_dirty = true;
	}
	if (value_dirty) {
		if (value) delete []value;
		if (newvalue) {
			value = new char[strlen(newvalue)+1];
			strcpy (value, newvalue);
		} else value = NULL;
		if (!group->IsExpanded()) value_dirty = false;
	}
}

// ==================================================================================

PropertyGroup::PropertyGroup (PropertyList *list, bool expand)
{
	plist = list;
	item = NULL;
	nitem = 0;
	title = NULL;
	expanded = expand;
}

PropertyGroup::~PropertyGroup ()
{
	if (nitem) {
		for (int i = 0; i < nitem; i++)
			delete item[i];
		delete []item;
	}
	if (title) delete []title;
}

PropertyItem *PropertyGroup::AppendItem ()
{
	PropertyItem *newitem = new PropertyItem (this);
	PropertyItem **tmp = new PropertyItem*[nitem+1];
	if (nitem) {
		memcpy (tmp, item, nitem*sizeof(PropertyItem*));
		delete []item;
	}
	item = tmp;
	item[nitem++] = newitem;
	return newitem;
}

PropertyItem *PropertyGroup::GetItem (int idx)
{
	if (idx >= 0 && idx < nitem)
		return item[idx];
	else
		return NULL;
}

void PropertyGroup::SetTitle (const char *t)
{
	if (title) delete []title;
	if (t) {
		title = new char[strlen(t)+1];
		strcpy (title, t);
	} else
		title = NULL;
}

void PropertyGroup::Expand (bool expand)
{
	if (expand != expanded) {
		expanded = expand;
		// more stuff
	}
}

// ==================================================================================

int PropertyList::titleh = 20;
int PropertyList::itemh = 18;
int PropertyList::gaph = 6;
HBITMAP PropertyList::hBmpArrows = NULL;

PropertyList::PropertyList ()
{
	npg = 0;
	listh = 0;
	yofs = 0;
	valx0 = 0;
	hFontTitle = NULL;
	hFontItem = NULL;
	hPenLine = NULL;
	hBrushTitle = NULL;
}

PropertyList::~PropertyList ()
{
	if (npg) {
		for (int i = 0; i < npg; i++)
			delete pg[i];
		delete []pg;
	}
	if (hFontTitle) DeleteObject (hFontTitle);
	if (hFontItem) DeleteObject (hFontItem);
	if (hPenLine) DeleteObject (hPenLine);
	if (hBrushTitle) DeleteObject (hBrushTitle);
}

void PropertyList::OnInitDialog (HWND hWnd, int nIDDlgItem)
{
	hDlg = hWnd;
	dlgid = nIDDlgItem;
	hItem = GetDlgItem (hDlg, dlgid);
	SetWindowLongPtr (hItem, GWLP_USERDATA, (LONG_PTR)this);
	RECT cr;
	GetClientRect (hItem, &cr);
	winw = cr.right;
	winh = cr.bottom;
	if (!valx0) valx0 = winw/2;
	hFontTitle = CreateFont (15, 0, 0, 0, FW_BOLD, FALSE, FALSE, FALSE, ANSI_CHARSET,
		OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FF_SWISS,
		"Arial");
	hFontItem = CreateFont (15, 0, 0, 0, FW_NORMAL, FALSE, FALSE, FALSE, ANSI_CHARSET,
		OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FF_SWISS,
		"Arial");
	hPenLine = CreatePen (PS_SOLID, 1, 0xD0D0D0);
	LOGBRUSH lb = { BS_SOLID, 0xFFE0E0, 0 };
	hBrushTitle = CreateBrushIndirect (&lb);
	SetScrollRange (hItem, SB_VERT, 0, 0, FALSE);
	SetScrollPos (hItem, SB_VERT, 0, FALSE);
}

void PropertyList::OnPaint (HWND hWnd)
{
	HDC hDC;
	RECT rupd;
	PAINTSTRUCT ps;
	HFONT hOldFont;
	HPEN hOldPen;
	HBRUSH hOldBrush;
	int i, j, y;

	if (!GetUpdateRect (hWnd, &rupd, FALSE)) return;
	hDC = BeginPaint (hWnd, &ps);
	
	hOldFont = (HFONT)SelectObject (hDC, hFontItem);
	hOldPen = (HPEN)SelectObject (hDC, hPenLine);
	hOldBrush = (HBRUSH)SelectObject (hDC, hBrushTitle);
	HDC hDCmem = CreateCompatibleDC (hDC);
	SelectObject (hDCmem, hBmpArrows);

	y = -yofs;
	for (i = 0; i < npg; i++) {
		bool expanded = pg[i]->IsExpanded();
		const char *title = pg[i]->GetTitle();
		if (title) {
			Rectangle (hDC, 18, y, winw-4, y+titleh-2);
			SelectObject (hDC, hFontTitle);
			SetTextColor (hDC, 0xB00000);
			SetBkColor (hDC, 0xFFE0E0);
			TextOut (hDC, 20, y+1, title, strlen(title));
			BitBlt (hDC, 2, y+2, 14, 14, hDCmem, 28+(expanded ? 0:14), 0, SRCCOPY);
			y += titleh;
			SetTextColor (hDC, 0x000000);
			SelectObject (hDC, hFontItem);
			SetBkColor (hDC, 0xFFFFFF);
		}
		if (expanded) {
			for (j = 0; j < pg[i]->ItemCount(); j++) {
				const char *item = pg[i]->GetItem (j)->GetLabel();
				if (item)
					TextOut (hDC, 20, y, item, strlen(item));
				const char *value = pg[i]->GetItem (j)->GetValue();
				if (value)
					TextOut (hDC, valx0, y, value, strlen(value));
				if (j < pg[i]->ItemCount()-1) {
					MoveToEx (hDC, 20, y+itemh-2, NULL);
					LineTo (hDC, winw-4, y+itemh-2);
				}
				y += itemh;
			}
			y += gaph;
		}
	}

	DeleteDC (hDCmem);
	SelectObject (hDC, hOldFont);
	SelectObject (hDC, hOldPen);
	SelectObject (hDC, hOldBrush);
	EndPaint (hWnd, &ps);
}

void PropertyList::OnSize (int w, int h)
{
	winw = w;
	winh = h;
	SetListHeight (listh, true);
}

void PropertyList::OnVScroll (unsigned int cmd, int p)
{
	int pmin, pmax, pos, dpos = 0;
	pos = GetScrollPos (hItem, SB_VERT);
	GetScrollRange (hItem, SB_VERT, &pmin, &pmax);
	pmax -= winh/itemh-1;
	switch (cmd) {
	case SB_LINEUP:
		dpos = -1;
		break;
	case SB_LINEDOWN:
		dpos = 1;
		break;
	case SB_PAGEUP:
		dpos = -winh/itemh;
		break;
	case SB_PAGEDOWN:
		dpos = winh/itemh;
		break;
	case SB_TOP:
		dpos = -pos;
		break;
	case SB_BOTTOM:
		dpos = pmax-pos;
		break;
	case SB_THUMBTRACK:
		dpos = p-pos;
		break;
	}
	if (dpos) {
		int newpos = max (pmin, min (pmax, pos+dpos));
		if (newpos != pos) {
			yofs = newpos * itemh;
			ScrollWindow (hItem, 0, (pos-newpos)*itemh, NULL, NULL);
			SetScrollPos (hItem, SB_VERT, newpos, TRUE);
			UpdateWindow (hItem);
		}
	}
}

void PropertyList::OnLButtonDown (int x, int y)
{
	if (x >= 18) return;
	int i, ytitle = 0;
	y += yofs;
	for (i = 0; i < npg; i++) {
		if (y >= ytitle && y < ytitle+titleh) {
			ExpandGroup (pg[i], !pg[i]->IsExpanded());
			break;
		}
		ytitle += titleh;
		if (pg[i]->IsExpanded())
			ytitle += itemh * pg[i]->ItemCount() + gaph;
	}	
}

void PropertyList::VScrollTo (int pos)
{
	int oldpos = yofs/itemh;
	yofs = pos * itemh;
	ScrollWindow (hItem, 0, (oldpos-pos)*itemh, NULL, NULL);
	SetScrollPos (hItem, SB_VERT, pos, TRUE);
	UpdateWindow (hItem);
}

void PropertyList::Move (int x, int y, int w, int h)
{
	MoveWindow (hItem, x, y, w, h, TRUE);
}

void PropertyList::Redraw ()
{
	InvalidateRect (hItem, NULL, TRUE);
}

void PropertyList::Update ()
{
	HDC hDC = NULL;
	HFONT hOldFont = NULL;

	int i, j, y = -yofs;
	SIZE ext;

	for (i = 0; i < npg; i++) {
		y += titleh;
		if (pg[i]->IsExpanded()) {
			for (j = 0; j < pg[i]->ItemCount(); j++) {
				PropertyItem *item = pg[i]->GetItem (j);
				if (item->label_dirty || item->value_dirty) {
					if (y >= -itemh && y < listh) {
						if (!hDC) {
							hDC = GetDC (hItem);
							hOldFont = (HFONT)SelectObject (hDC, hFontItem);
							SelectObject (hDC, GetStockObject (WHITE_PEN));
							SelectObject (hDC, GetStockObject (WHITE_BRUSH));
						}
						if (item->label_dirty) {
							const char *label = item->GetLabel();
							int oldw = item->labelw;
							if (oldw < 0) oldw = winw;
							if (label) {
								int n = strlen(label);
								TextOut (hDC, 20, y, label, n);
								GetTextExtentPoint32 (hDC, label, n, &ext);
								item->labelw = ext.cx;
							} else {
								item->labelw = 0;
							}
							if (item->labelw < oldw) {
								Rectangle (hDC, 20+item->labelw, y, valx0-1, y+itemh-2);
							}
							item->label_dirty = false;
						}
						if (item->value_dirty) {
							const char *value = item->GetValue();
							int oldw = item->valuew;
							if (oldw < 0) oldw = winw;
							if (value) {
								int n = strlen(value);
								TextOut (hDC, valx0, y, value, n);
								GetTextExtentPoint32 (hDC, value, n, &ext);
								item->valuew = ext.cx;
							} else {
								item->valuew = 0;
							}
							if (item->valuew < oldw) {
								Rectangle (hDC, valx0+item->valuew, y, winw, y+itemh-2);
							}
							item->value_dirty = false;
						}
					}
				}
				y += itemh;
			}
			y += gaph;
		}
	}
	if (hDC) {
		if (hOldFont) SelectObject (hDC, hOldFont);
		ReleaseDC (hItem, hDC);
	}
}

void PropertyList::SetColWidth (int col, int w)
{
	if (col) return;        // only col 0 is supported for now
	if (w == valx0) return; // nothing to do
	valx0 = w;
	Redraw();
}

PropertyGroup *PropertyList::AppendGroup (bool expand)
{
	PropertyGroup **tmp = new PropertyGroup*[npg+1];
	if (npg) {
		memcpy (tmp, pg, npg*sizeof(PropertyGroup*));
		delete []pg;
	}
	pg = tmp;
	pg[npg] = new PropertyGroup (this, expand);
	SetListHeight (listh + titleh + gaph);
	return pg[npg++];
}

bool PropertyList::DeleteGroup (PropertyGroup *g)
{
	int i, j, k, h;
	for (i = 0; i < npg; i++) {
		if (pg[i] == g) {
			h = listh - titleh - gaph;
			if (g->IsExpanded()) h -= g->ItemCount()*itemh;
			delete g;
			PropertyGroup **tmp;
			if (npg > 1) {
				tmp = new PropertyGroup*[npg-1];
				for (j = k = 0; j < npg; j++)
					if (j != i) tmp[k++] = pg[j];
			} else tmp = NULL;
			delete []pg;
			pg = tmp;
			npg--;
			SetListHeight (h);
			return true;
		}
	}
	return false;
}

PropertyGroup *PropertyList::GetGroup (int idx)
{
	if (idx >= 0 && idx < npg)
		return pg[idx];
	else
		return NULL;
}

bool PropertyList::ExpandGroup (PropertyGroup *g, bool expand)
{
	if (expand == g->IsExpanded()) return false; // nothing to do

	g->Expand (expand);
	int h = listh, dh = itemh * g->ItemCount() + gaph;
	if (expand) h += dh;
	else        h -= dh;
	SetListHeight (h);
	Redraw();

	return true;
}

void PropertyList::ExpandAll (bool expand)
{
	int h = 0;
	for (int i = 0; i < npg; i++) {
		pg[i]->Expand (expand);
		h += titleh;
		if (expand)
			h += gaph + itemh * pg[i]->ItemCount();
	}
	SetListHeight (h);
	Redraw();
}

void PropertyList::ClearGroups ()
{
	if (npg) {
		for (int i = 0; i < npg; i++)
			delete pg[i];
		delete []pg;
		npg = 0;
		SetListHeight (0);
	}
}

PropertyItem *PropertyList::AppendItem (PropertyGroup *g)
{
	PropertyItem *item = g->AppendItem ();
	if (g->IsExpanded())
		SetListHeight (listh + itemh);
	return item;
}

void PropertyList::SetListHeight (int h, bool force)
{
	if (h == listh && !force) return; // nothing to do
	listh = h;
	int rmax = 0;
	if (listh > winh)
		rmax = (listh-winh+itemh-1)/itemh;
	int pos = GetScrollPos (hItem, SB_VERT);
	SCROLLINFO si;
	si.cbSize = sizeof(SCROLLINFO);
	si.fMask = SIF_PAGE | SIF_RANGE;
	si.nPage = winh/itemh;
	si.nMin = 0;
	si.nMax = winh/itemh-1+rmax;
	SetScrollInfo (hItem, SB_VERT, &si, TRUE);

	int pos2 = GetScrollPos (hItem, SB_VERT);
	if (pos2 != pos)
		VScrollTo (pos2);
}