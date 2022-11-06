// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Time acceleration dialog
// ======================================================================

#define STRICT 1

#include "DlgTacc.h"
#include "Orbiter.h"
#include "resource.h"
#include "resource2.h"

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern HELPCONTEXT DefHelpContext;

// ======================================================================

DlgTacc::DlgTacc (HINSTANCE hInstance, HWND hParent, void *context)
: DialogWin (hInstance, hParent, IDD_TIMEWARP, 0, 0, context)
{
	pos = &g_pOrbiter->Cfg()->CfgWindowPos.DlgTacc;
}

// ======================================================================

void DlgTacc::Message (DWORD msg, void *data)
{
	bool paused = (data != 0);
	SetWindowText (GetDlgItem (hWnd, IDC_WARP_PAUSE), paused ? "Resume" : "Pause");
}

// ======================================================================

void DlgTacc::RegisterWarp (HWND hDlg, double warp, bool commit, bool edit, bool slide)
{
	if (commit) g_pOrbiter->SetWarpFactor (warp);
	if (slide) {
		int sliderpos;
		if      (warp <=   1.0) sliderpos = (int)(warp*10.0);
		else if (warp <=  10.0) sliderpos = (int)(warp+9.0);
		else if (warp <= 100.0) sliderpos = (int)(0.1*warp+18.0);
		else                    sliderpos = (int)(0.01*warp+27.0);
		SendDlgItemMessage (hDlg, IDC_WARP_SLIDER, TBM_SETPOS, TRUE, (LONG)sliderpos);
	}
	if (edit) {
		char cbuf[256];
		double pwarp;
		GetWindowText (GetDlgItem (hDlg, IDC_WARP_EDIT), cbuf, 256);
		int res = sscanf (cbuf, "%lf", &pwarp);
		if (res != 1 || fabs (warp-pwarp) > 1e-8) {
			sprintf (cbuf, "%0.1f", warp);
			SetWindowText (GetDlgItem (hDlg, IDC_WARP_EDIT), cbuf);
		}
	}
}

// ======================================================================

BOOL DlgTacc::OnInitDialog (HWND hDlg, WPARAM wParam, LPARAM lParam)
{
	SendDlgItemMessage (hDlg, IDC_WARP_SLIDER, TBM_SETRANGE, FALSE, MAKELONG(1,37));
	SendDlgItemMessage (hDlg, IDC_WARP_SLIDER, TBM_SETPAGESIZE, 0, 9L);
	SendDlgItemMessage (hDlg, IDC_WARP_SLIDER, TBM_SETTICFREQ, 9, 0);
	RegisterWarp (hDlg, td.Warp(), false);
	SetWindowText (GetDlgItem (hDlg, IDC_WARP_PAUSE), g_pOrbiter->IsRunning() ? "Pause": "Resume");
	return TRUE;
}

// ======================================================================

BOOL DlgTacc::OnCommand (HWND hDlg, WORD id, WORD code, HWND hControl)
{
	switch (id) {
	case IDHELP:
		DefHelpContext.topic = "/timeacc.htm";
		g_pOrbiter->OpenHelp (&DefHelpContext);
		return TRUE;
	case IDC_WARP_01:
		RegisterWarp (hDlg, 0.1);
		return TRUE;
	case IDC_WARP_1:
		RegisterWarp (hDlg, 1.0);
		return TRUE;
	case IDC_WARP_10:
		RegisterWarp (hDlg, 10.0);
		return TRUE;
	case IDC_WARP_100:
		RegisterWarp (hDlg, 100.0);
		return TRUE;
	case IDC_WARP_1000:
		RegisterWarp (hDlg, 1000.0);
		return TRUE;
	case IDC_WARP_EDIT:
		if (code == EN_UPDATE) {
			double warp;
			char cbuf[256];
			GetWindowText (hControl, cbuf, 256);
			warp = atof (cbuf);
			if (warp >= 0.1 && warp <= 1e5)
				RegisterWarp (hDlg, warp, true, false);
			return TRUE;
		}
		break;
	case IDC_WARP_PAUSE:
		g_pOrbiter->TogglePause();
		return TRUE;
	}
	return DialogWin::OnCommand (hDlg, id, code, hControl);
}

// ======================================================================

BOOL DlgTacc::OnHScroll (HWND hDlg, WORD request, WORD curpos, HWND hControl)
{
	if (request == TB_THUMBTRACK) {
		int sliderpos = SendDlgItemMessage (hDlg, IDC_WARP_SLIDER, TBM_GETPOS, 0, 0);
		if      (sliderpos <= 10) RegisterWarp (hDlg, sliderpos*0.1);
		else if (sliderpos <= 19) RegisterWarp (hDlg, sliderpos-9);
		else if (sliderpos <= 28) RegisterWarp (hDlg, (sliderpos-18)*10);
		else                      RegisterWarp (hDlg, (sliderpos-27)*100);
		return TRUE;
	}
	return DialogWin::OnHScroll (hDlg, request, curpos, hControl);
}
