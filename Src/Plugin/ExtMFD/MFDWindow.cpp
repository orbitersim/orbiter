// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                  ORBITER MODULE: ExtMFD
//                  Part of the ORBITER SDK
//            Copyright (C) 2006 Martin Schweiger
//                   All rights reserved
//
// MFDWindow.cpp
//
// Class implementation for MFDWindow. Defines the properties and
// state of an MFD display in a dialog box
// ==============================================================

#include "MFDWindow.h"
#include "resource.h"
#include <stdio.h> // temporary

// ==============================================================
// prototype definitions

INT_PTR CALLBACK DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

// ==============================================================
// class MFDWindow

MFDWindow::MFDWindow (HINSTANCE _hInst, const MFDSPEC &spec): ExternMFD (spec), hInst(_hInst)
{
	hBtnFnt = 0;
	fnth = 0;
	vstick = false;
	oapiOpenDialogEx (hInst, IDD_MFD, DlgProc,
		DLG_ALLOWMULTI, this);
}

MFDWindow::~MFDWindow ()
{
	oapiCloseDialog (hDlg);
	if (hBtnFnt) DeleteObject (hBtnFnt);
}

void MFDWindow::Initialise (HWND _hDlg)
{
	extern HBITMAP g_hPin;

	hDlg = _hDlg;
	hDsp = GetDlgItem (hDlg, IDC_DISPLAY);
	for (int i = 0; i < 15; i++)
		SetWindowLongPtr (GetDlgItem (hDlg, IDC_BUTTON1+i), GWLP_USERDATA, i);
	oapiAddTitleButton (IDSTICK, g_hPin, DLG_CB_TWOSTATE);
	SetTitle ();
	gap = 3;
	Resize (false);
}

void MFDWindow::SetVessel (OBJHANDLE hV)
{
	ExternMFD::SetVessel (hV);
	SetTitle ();
}

void MFDWindow::SetTitle ()
{
	char cbuf[256] = "MFD [";
	oapiGetObjectName (hVessel, cbuf+5, 250);
	strcat (cbuf, "]");
	SetWindowText (hDlg, cbuf);
}

void MFDWindow::Resize (bool fixaspect)
{
	RECT r;
	GetClientRect (hDlg, &r);
	int bw = (r.right*35)/300;
	BW = max (30, min (60, bw));
	BH = max (15, min (40, (bw*2)/3));
	int dspw = r.right-2*(BW+gap), dsph = r.bottom-(BH+2*gap);
	int ds = min (dspw, dsph);
	if (fixaspect && dspw != dsph) { // force aspect adjustment
		RECT wr;
		ds = max (50, ds);
		GetWindowRect (hDlg, &wr);
		SetWindowPos (hDlg, NULL, 0, 0, wr.right-wr.left-r.right+ds+2*(BW+gap), wr.bottom-wr.top-r.bottom+ds+(BH+2*gap), SWP_NOMOVE|SWP_NOOWNERZORDER);
		return;
	}

	int fh = BW/3;
	if (fh != fnth) {
		if (hBtnFnt) DeleteObject (hBtnFnt);
		hBtnFnt = CreateFont (fnth = fh, 0, 0, 0, 0, 0, 0, 0, DEFAULT_CHARSET,
			OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
			DEFAULT_PITCH|FF_DONTCARE, "Arial");
	}

	r.right = ds + (r.left = (r.right-ds)/2);
	r.bottom = ds + (r.top = gap);
	SetWindowPos (hDsp, NULL, r.left, r.top, DW = (r.right-r.left), DH = (r.bottom-r.top), SWP_SHOWWINDOW);
	
	int x1 = r.left-BW-gap;
	int x2 = r.right+gap;
	int dy = (DH*2)/13, y0 = DH/7, y1 = r.top+y0-BH/2;
	for (int i = 0; i < 6; i++) {
		SetWindowPos (GetDlgItem (hDlg, IDC_BUTTON1+i), NULL, x1, y1+i*dy, BW, BH, SWP_SHOWWINDOW);
		SetWindowPos (GetDlgItem (hDlg, IDC_BUTTON7+i), NULL, x2, y1+i*dy, BW, BH, SWP_SHOWWINDOW);
	}
	y1 = r.top+DH+gap;
	SetWindowPos (GetDlgItem (hDlg, IDC_BUTTON_PWR), NULL, r.left + DW/2 - (BW*7)/4, y1, BW, BH, SWP_SHOWWINDOW);
	SetWindowPos (GetDlgItem (hDlg, IDC_BUTTON_SEL), NULL, r.left + DW/2 - BW/2, y1, BW, BH, SWP_SHOWWINDOW);
	SetWindowPos (GetDlgItem (hDlg, IDC_BUTTON_MNU), NULL, r.left + DW/2 + (BW*3)/4, y1, BW, BH, SWP_SHOWWINDOW);

	MFDSPEC spec = {{0,0,DW,DH},6,6,y0,dy};
	ExternMFD::Resize (spec);
	InvalidateRect (hDlg, NULL, FALSE);
}

void MFDWindow::RepaintDisplay (HWND hWnd)
{
	PAINTSTRUCT ps;
	HDC hDCtgt = BeginPaint (hWnd, &ps);
	SURFHANDLE surf = GetDisplaySurface();
	if (surf) {
		HDC hDCsrc = oapiGetDC (surf);
		BitBlt (hDCtgt, 0, 0, DW, DH, hDCsrc, 0, 0, SRCCOPY);
		oapiReleaseDC (surf, hDCsrc);
	} else {
		SelectObject (hDCtgt, GetStockObject (BLACK_BRUSH));
		Rectangle (hDCtgt, 0, 0, DW, DH);
	}
	EndPaint (hWnd, &ps);
}

void MFDWindow::RepaintButton (HWND hWnd)
{
	int id = GetWindowLongPtr (hWnd, GWLP_USERDATA);
	PAINTSTRUCT ps;
	HDC hDC = BeginPaint (hWnd, &ps);
	SelectObject (hDC, GetStockObject (BLACK_PEN));
	Rectangle (hDC, 0, 0, BW, BH);
	SetTextAlign (hDC, TA_CENTER);
	const char *label;
	if (id < 12) {
		label = GetButtonLabel (id);
	} else {
		static const char *lbl[3] = {"PWR","SEL","MNU"};
		label = lbl[id-12];
		if (id == 12) SetTextColor (hDC, 0x0000FF);
	}
	if (label) {
		SetBkMode (hDC, TRANSPARENT);
		HFONT pFont = (HFONT)SelectObject (hDC, hBtnFnt);
		TextOut (hDC, BW/2, (BH-fnth)/2, label, strlen(label));
		SelectObject (hDC, pFont);
	}
	EndPaint (hWnd, &ps);
}

void MFDWindow::ProcessButton (int bt, int event)
{
	switch (bt) {
	case 12:
		if (event == PANEL_MOUSE_LBDOWN)
			SendKey (OAPI_KEY_ESCAPE);
		break;
	case 13:
		if (event == PANEL_MOUSE_LBDOWN)
			SendKey (OAPI_KEY_F1);
		break;
	case 14:
		if (event == PANEL_MOUSE_LBDOWN)
			SendKey (OAPI_KEY_GRAVE);
		break;
	default:
		ExternMFD::ProcessButton (bt, event);
		break;
	}
}

void MFDWindow::clbkRefreshDisplay (SURFHANDLE)
{
	InvalidateRect (hDsp, NULL, FALSE);
}

void MFDWindow::clbkRefreshButtons ()
{
	for (int i = 0; i < 12; i++)
		InvalidateRect (GetDlgItem (hDlg, IDC_BUTTON1+i), NULL, FALSE);
}

void MFDWindow::clbkFocusChanged (OBJHANDLE hFocus)
{
	if (!vstick) {
		ExternMFD::clbkFocusChanged (hFocus);
		//SetTitle ();
	}
}

void MFDWindow::StickToVessel (bool stick)
{
	vstick = stick;
	if (!vstick) {
		SetVessel (oapiGetFocusObject());
		//SetTitle ();
	}
}

// ==============================================================
// Windows message handler for the dialog box

INT_PTR CALLBACK DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((MFDWindow*)lParam)->Initialise (hDlg);
		return TRUE;
	case WM_SIZE:
		((MFDWindow*)oapiGetDialogContext(hDlg))->Resize (true);
		return TRUE;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDCANCEL:
			oapiUnregisterExternMFD ((MFDWindow*)oapiGetDialogContext (hDlg));
			return TRUE;
		case IDHELP:
			((MFDWindow*)oapiGetDialogContext(hDlg))->OpenModeHelp ();
			return TRUE;
		case IDSTICK:
			((MFDWindow*)oapiGetDialogContext(hDlg))->StickToVessel (HIWORD(wParam) != 0);
			return TRUE;
		}
		break;
	}
	return oapiDefDialogProc (hDlg, uMsg, wParam, lParam);
}

long FAR PASCAL MFD_WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_PAINT: {
		MFDWindow *mfdw = (MFDWindow*)oapiGetDialogContext (GetParent (hWnd));
		mfdw->RepaintDisplay (hWnd);
		} return 0;
	}

	return DefWindowProc (hWnd, uMsg, wParam, lParam);
}

long FAR PASCAL MFD_BtnProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_PAINT: {
		MFDWindow *mfdw = (MFDWindow*)oapiGetDialogContext (GetParent (hWnd));
		mfdw->RepaintButton (hWnd);
		} return 0;
	case WM_LBUTTONDOWN: {
		MFDWindow *mfdw = (MFDWindow*)oapiGetDialogContext (GetParent (hWnd));
		mfdw->ProcessButton (GetWindowLongPtr (hWnd, GWLP_USERDATA), PANEL_MOUSE_LBDOWN);
		SetCapture (hWnd);
		} return 0;
	case WM_LBUTTONUP: {
		MFDWindow *mfdw = (MFDWindow*)oapiGetDialogContext (GetParent (hWnd));
		mfdw->ProcessButton (GetWindowLongPtr (hWnd, GWLP_USERDATA), PANEL_MOUSE_LBUP);
		ReleaseCapture();
		} return 0;
	}

	return DefWindowProc (hWnd, uMsg, wParam, lParam);
}