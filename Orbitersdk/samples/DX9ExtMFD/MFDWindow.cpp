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
#include "gcConst.h"

#define IDSTICK 999

// ==============================================================
// prototype definitions

INT_PTR CALLBACK DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

// ==============================================================
// class MFDWindow

MFDWindow::MFDWindow (HINSTANCE _hInst, const MFDSPEC &spec): ExternMFD (spec), hInst(_hInst)
{
	hSwap = NULL;
	hBtnFnt = 0;
	fnth = 0;
	vstick = false;
	bFailed = false;
	bDrvMode = false; // D3D

	oapiOpenDialogEx (hInst, IDD_MFD, DlgProc, DLG_ALLOWMULTI|DLG_CAPTIONCLOSE|DLG_CAPTIONHELP, this);
}

MFDWindow::~MFDWindow ()
{
	gcCore *pCore = gcGetCoreInterface();
	if (pCore && hSwap) pCore->ReleaseSwap(hSwap);
	oapiCloseDialog (hDlg);
	if (hBtnFnt) DeleteObject (hBtnFnt);
}

void MFDWindow::Initialise (HWND _hDlg)
{
	extern HBITMAP g_hPin;

	hDlg = _hDlg;
	hDsp = GetDlgItem (hDlg, IDC_DISPLAY);
	
	for (int i = 0; i < 16; i++)
		SetWindowLongPtr (GetDlgItem (hDlg, IDC_BUTTON1+i), GWLP_USERDATA, i);

	oapiAddTitleButton (IDSTICK, g_hPin, DLG_CB_TWOSTATE);
	SetTitle ();
	gap = 3;

	GetWindowRect(hDlg, &wr);
	CheckAspect(&wr, 0);
	Resize();
}

void MFDWindow::SetVessel (OBJHANDLE hV)
{
	ExternMFD::SetVessel (hV);
	SetTitle ();
}

void MFDWindow::SetTitle ()
{
	char cbuf[256] = "DX9 MFD [";
	oapiGetObjectName (hVessel, cbuf+9, 200);
	strcat_s (cbuf, 250, "]");
	SetWindowText (hDlg, cbuf);		//<<--- Very odd runtime check failure here why now ???  :jarmonik 5-Aug-2021
}

void MFDWindow::CheckAspect(LPRECT r, DWORD q)
{
	RECT c,b;

	GetClientRect(hDlg, &c);
	GetWindowRect(hDlg, &b);

	int ew = (b.right - b.left) - (c.right - c.left);
	int eh = (b.bottom - b.top) - (c.bottom - c.top);

	int bw = (c.right * 35) / 300;

	BW = max(30, min(60, bw));
	BH = max(15, min(40, (bw * 2) / 3));

	int dspw = (r->right - r->left) - (2 * (BW + gap) + 2 * gap + ew);
	int dsph = (r->bottom - r->top) - ((BH + 2 * gap) + 2 * gap + ew);

	ds = (dspw + dsph) / 2;

	int w = (gap + BW) * 2 + ds + gap * 2 + ew;
	int h = (gap + BH) + ds + gap * 2 + eh;

	switch (q) {
	case WMSZ_BOTTOM:
	case WMSZ_RIGHT:
	case WMSZ_BOTTOMRIGHT:
		r->right = r->left + w;
		r->bottom = r->top + h;
		break;
	default:
		r->left = r->right - w;
		r->top = r->bottom - h;
		break;
	}
	return;
}

void MFDWindow::Resize()
{
	RECT r;
	GetClientRect (hDlg, &r);
	int bw = (r.right*35)/300;
	BW = max (30, min (60, bw));
	BH = max (15, min (40, (bw*2)/3));
	ds = r.right - (2 * (BW + gap) + 2 * gap);
	
	int fh = BW/3;

	if (fh != fnth) {
		if (hBtnFnt) DeleteObject (hBtnFnt);
		hBtnFnt = CreateFont (fnth = fh, 0, 0, 0, 0, 0, 0, 0, DEFAULT_CHARSET,
			OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
			DEFAULT_PITCH|FF_DONTCARE, "Arial");
	}

	DH = DW = ds;

	SetWindowPos(hDsp, NULL, BW + gap * 2, gap, ds, ds, SWP_SHOWWINDOW);
	
	int x1 = gap;
	int x2 = r.right-gap-BW;
	int dy = (DH*2)/13, y0 = DH/7, y1 = r.top+y0-BH/2;
	for (int i = 0; i < 6; i++) {
		SetWindowPos (GetDlgItem (hDlg, IDC_BUTTON1+i), NULL, x1, y1+i*dy, BW, BH, SWP_SHOWWINDOW);
		SetWindowPos (GetDlgItem (hDlg, IDC_BUTTON7+i), NULL, x2, y1+i*dy, BW, BH, SWP_SHOWWINDOW);
	}
	y1 = r.top+DH+gap*2;
	SetWindowPos (GetDlgItem (hDlg, IDC_BUTTON_DRV), NULL, r.left + DW/2 + (BW*12)/4, y1, BW, BH, SWP_SHOWWINDOW);
	SetWindowPos (GetDlgItem (hDlg, IDC_BUTTON_PWR), NULL, r.left + DW/2 - (BW*7)/4, y1, BW, BH, SWP_SHOWWINDOW);
	SetWindowPos (GetDlgItem (hDlg, IDC_BUTTON_SEL), NULL, r.left + DW/2 - BW/2, y1, BW, BH, SWP_SHOWWINDOW);
	SetWindowPos (GetDlgItem (hDlg, IDC_BUTTON_MNU), NULL, r.left + DW/2 + (BW*3)/4, y1, BW, BH, SWP_SHOWWINDOW);

	if (!bFailed) {
		gcCore *pCore = gcGetCoreInterface();
		if (pCore) hSwap = pCore->RegisterSwap(hDsp, hSwap, 0);
	}

	MFDSPEC spec = {{0,0,DW,DH},6,6,y0,dy};
	ExternMFD::Resize (spec);

	InvalidateRect(hDlg, NULL, FALSE);
}


void MFDWindow::RepaintDisplay(HWND hWnd)
{
	PAINTSTRUCT ps;
	HDC hDCtgt = BeginPaint(hWnd, &ps);
	EndPaint(hWnd, &ps);
}

void MFDWindow::RepaintButton (HWND hWnd)
{
	int id = (int)GetWindowLongPtr (hWnd, GWLP_USERDATA);
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
		static const char *drv[2] = {"N/A","N/A"};
		if (id == 15) label = drv[bDrvMode];
		else {
			label = lbl[id - 12];
			if (id == 12) SetTextColor(hDC, 0x0000FF);
		}
	}
	if (label) {
		SetBkMode (hDC, TRANSPARENT);
		HFONT pFont = (HFONT)SelectObject (hDC, hBtnFnt);
		TextOut (hDC, BW/2, (BH-fnth)/2, label, lstrlen(label));
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
	case 15:
		if (event == PANEL_MOUSE_LBDOWN) {
			//bDrvMode = !bDrvMode;
			//InvalidateRect(GetDlgItem(hDlg, IDC_BUTTON_DRV), NULL, FALSE);
			//ToggleDrvMode();
		}
		break;
	default:
		ExternMFD::ProcessButton (bt, event);
		break;
	}
}


void MFDWindow::ToggleDrvMode()
{
	gcCore *pCore = gcGetCoreInterface();
	if (!pCore) return;

	SURFHANDLE surf = GetDisplaySurface();

	if (bDrvMode) pCore->ConvertSurface(surf, OAPISURFACE_SYSMEM); // GDI
	else pCore->ConvertSurface(surf, OAPISURFACE_RENDERTARGET); // D3D
}


void MFDWindow::clbkRefreshDisplay (SURFHANDLE)
{
	if (bFailed) return;

	if (bDrvMode) {
		// GDI Mode
		InvalidateRect(hDsp, NULL, false);
		return;
	}

	gcCore *pCore = gcGetCoreInterface();
	if (!pCore) return;

	if (!hSwap) hSwap = pCore->RegisterSwap(hDsp, hSwap, 0);
	if (!hSwap) {
		bFailed = true;
		return;
	}

	SURFHANDLE tgt = pCore->GetRenderTarget(hSwap);
	SURFHANDLE surf = GetDisplaySurface();

	if (surf) oapiBlt(tgt, surf, 0, 0, 0, 0, DW, DH);
	else oapiClearSurface(tgt);

	pCore->FlipSwap(hSwap);
}

void MFDWindow::clbkRefreshButtons ()
{
	InvalidateRect(GetDlgItem(hDlg, IDC_BUTTON_DRV), NULL, FALSE);
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
	case WM_SIZING:
		((MFDWindow*)oapiGetDialogContext(hDlg))->CheckAspect(LPRECT(lParam), (DWORD)wParam);
		return TRUE;
	case WM_SIZE:
		((MFDWindow*)oapiGetDialogContext(hDlg))->Resize();
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


LRESULT CALLBACK MFD_WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{	
	switch (uMsg) {
		case WM_ERASEBKGND:
			return 1;
		case WM_PAINT: 
		{
			MFDWindow *mfdw = (MFDWindow*)oapiGetDialogContext(GetParent(hWnd));
			mfdw->RepaintDisplay(hWnd);
			return 0;
		} 
	}
	return DefWindowProc (hWnd, uMsg, wParam, lParam);
}


LRESULT FAR PASCAL MFD_BtnProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_PAINT: {
		MFDWindow *mfdw = (MFDWindow*)oapiGetDialogContext (GetParent (hWnd));
		mfdw->RepaintButton (hWnd);
		} return 0;
	case WM_LBUTTONDOWN: {
		MFDWindow *mfdw = (MFDWindow*)oapiGetDialogContext (GetParent (hWnd));
		mfdw->ProcessButton ((int)GetWindowLongPtr(hWnd, GWLP_USERDATA), PANEL_MOUSE_LBDOWN);
		SetCapture (hWnd);
		} return 0;
	case WM_LBUTTONUP: {
		MFDWindow *mfdw = (MFDWindow*)oapiGetDialogContext (GetParent (hWnd));
		mfdw->ProcessButton ((int)GetWindowLongPtr(hWnd, GWLP_USERDATA), PANEL_MOUSE_LBUP);
		ReleaseCapture();
		} return 0;
	}

	return DefWindowProc (hWnd, uMsg, wParam, lParam);
}