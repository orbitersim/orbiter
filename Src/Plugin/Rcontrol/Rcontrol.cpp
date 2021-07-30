// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                  ORBITER MODULE: Rcontrol
//                  Part of the ORBITER SDK
//
// Rcontrol.cpp
//
// A small plugin which allows to manipulate spacecraft main and
// retro controls via a dialog box.
// ==============================================================

#define STRICT 1
#define ORBITER_MODULE
#include "Orbitersdk.h"
#include "DlgCtrl.h"
#include "resource.h"
#include <stdio.h>

// ==============================================================
// The module interface class

namespace oapi {

class RControl: public Module {
public:
	RControl (HINSTANCE hDLL): Module (hDLL) {}
	~RControl () {}
	void clbkSimulationStart (RenderMode mode);
	void clbkPreStep (double simt, double simdt, double mjd);
};

}; // namespace oapi

// ==============================================================
// Global parameters

using namespace oapi;

static struct {
	RControl *rcontrol;
	HINSTANCE hDLL;
	HWND      hDlg;
	DWORD     dwCmd;
	VESSEL    *vessel;
	int       maingauge, retrogauge, hovergauge;
} g_Param;

void OpenDlgClbk (void *context);
LRESULT CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
void CreateVesselList (HWND);

// ==============================================================
// RControl implementation

void RControl::clbkSimulationStart (RenderMode mode)
{
	g_Param.hDlg = NULL;
}

// Frame update
void RControl::clbkPreStep (double simt, double simdt, double mjd)
{
	if (!g_Param.hDlg) return;

	int slider;
	slider = (int)(g_Param.vessel->GetThrusterGroupLevel (THGROUP_MAIN) * 100.0 + 0.5);
	if (slider != g_Param.maingauge)
		oapiSetGaugePos (GetDlgItem (g_Param.hDlg, IDC_MAIN_GAUGE), g_Param.maingauge = slider);
	slider = (int)(g_Param.vessel->GetThrusterGroupLevel (THGROUP_RETRO) * 100.0 + 0.5);
	if (slider != g_Param.retrogauge)
		oapiSetGaugePos (GetDlgItem (g_Param.hDlg, IDC_RETRO_GAUGE), g_Param.retrogauge = slider);
	slider = (int)(g_Param.vessel->GetThrusterGroupLevel (THGROUP_HOVER) * 100.0 + 0.5);
	if (slider != g_Param.hovergauge)
		oapiSetGaugePos (GetDlgItem (g_Param.hDlg, IDC_HOVER_GAUGE), g_Param.hovergauge = slider);

	// RCS button status
	double rcslevel = oapiGetGaugePos (GetDlgItem (g_Param.hDlg, IDC_RCSLEVEL)) * 0.01;
	if (SendDlgItemMessage (g_Param.hDlg, IDC_RCS_PITCHUP, BM_GETSTATE, 0, 0) & BST_PUSHED)
		g_Param.vessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_PITCHUP, rcslevel);
	else if (SendDlgItemMessage (g_Param.hDlg, IDC_RCS_PITCHDOWN, BM_GETSTATE, 0, 0) & BST_PUSHED)
		g_Param.vessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_PITCHDOWN, rcslevel);
	else if (SendDlgItemMessage (g_Param.hDlg, IDC_RCS_YAWLEFT, BM_GETSTATE, 0, 0) & BST_PUSHED)
		g_Param.vessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_YAWLEFT, rcslevel);
	else if (SendDlgItemMessage (g_Param.hDlg, IDC_RCS_YAWRIGHT, BM_GETSTATE, 0, 0) & BST_PUSHED)
		g_Param.vessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_YAWRIGHT, rcslevel);
	else if (SendDlgItemMessage (g_Param.hDlg, IDC_RCS_BANKLEFT, BM_GETSTATE, 0, 0) & BST_PUSHED)
		g_Param.vessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_BANKLEFT, rcslevel);
	else if (SendDlgItemMessage (g_Param.hDlg, IDC_RCS_BANKRIGHT, BM_GETSTATE, 0, 0) & BST_PUSHED)
		g_Param.vessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_BANKRIGHT, rcslevel);
	else if (SendDlgItemMessage (g_Param.hDlg, IDC_RCS_UP, BM_GETSTATE, 0, 0) & BST_PUSHED)
		g_Param.vessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_UP, rcslevel);
	else if (SendDlgItemMessage (g_Param.hDlg, IDC_RCS_DOWN, BM_GETSTATE, 0, 0) & BST_PUSHED)
		g_Param.vessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_DOWN, rcslevel);
	else if (SendDlgItemMessage (g_Param.hDlg, IDC_RCS_LEFT, BM_GETSTATE, 0, 0) & BST_PUSHED)
		g_Param.vessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_LEFT, rcslevel);
	else if (SendDlgItemMessage (g_Param.hDlg, IDC_RCS_RIGHT, BM_GETSTATE, 0, 0) & BST_PUSHED)
		g_Param.vessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_RIGHT, rcslevel);
	else if (SendDlgItemMessage (g_Param.hDlg, IDC_RCS_FORWARD, BM_GETSTATE, 0, 0) & BST_PUSHED)
		g_Param.vessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_FORWARD, rcslevel);
	else if (SendDlgItemMessage (g_Param.hDlg, IDC_RCS_BACK, BM_GETSTATE, 0, 0) & BST_PUSHED)
		g_Param.vessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_BACK, rcslevel);
}

// ==============================================================
// DLL entry and exit points

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	g_Param.hDLL = hDLL;

	// Register the custom function in Orbiter
	g_Param.dwCmd = oapiRegisterCustomCmd ("Remote Vessel Control",
		"Operate the engines of any spacecraft from a dialog box.",
		OpenDlgClbk, NULL);

	// Register custom dialog controls
	oapiRegisterCustomControls (hDLL);

	// Register the module
	g_Param.rcontrol = new RControl (hDLL);
	oapiRegisterModule (g_Param.rcontrol);
}

// DLL cleanup
DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Unregister the custom function in Orbiter
	oapiUnregisterCustomCmd (g_Param.dwCmd);

	// Unregister custom dialog controls
	oapiUnregisterCustomControls (hDLL);

	delete g_Param.rcontrol;
}

// Open the dialog box
void OpenDlgClbk (void *context)
{
	HWND hDlg = oapiOpenDialog (g_Param.hDLL, IDD_INTERFACE, DlgProc);
	if (hDlg) {
		g_Param.hDlg = hDlg;

		GAUGEPARAM gp = { 0, 100, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
		oapiSetGaugeParams (GetDlgItem (hDlg, IDC_RCSLEVEL), &gp);
		oapiSetGaugePos (GetDlgItem (hDlg, IDC_RCSLEVEL), 100);
		gp.color = GAUGEPARAM::RED;
		oapiSetGaugeParams (GetDlgItem (hDlg, IDC_MAIN_GAUGE), &gp);
		oapiSetGaugePos (GetDlgItem (hDlg, IDC_MAIN_GAUGE), g_Param.maingauge = 0);
		gp.base = GAUGEPARAM::RIGHT;
		oapiSetGaugeParams (GetDlgItem (hDlg, IDC_RETRO_GAUGE), &gp);
		oapiSetGaugePos (GetDlgItem (hDlg, IDC_RETRO_GAUGE), g_Param.retrogauge = 0);
		gp.base = GAUGEPARAM::BOTTOM;
		oapiSetGaugeParams (GetDlgItem (hDlg, IDC_HOVER_GAUGE), &gp);
		oapiSetGaugePos (GetDlgItem (hDlg, IDC_HOVER_GAUGE), g_Param.hovergauge = 0);
	}
}

// Create vessel list
void CreateVesselList (HWND hDlg)
{
	char cbuf[128];
	DWORD i, n = oapiGetVesselCount();
	SendDlgItemMessage (hDlg, IDC_VESSELLIST, CB_RESETCONTENT, 0, 0);
	for (i = 0; i < n; i++) {
		OBJHANDLE hVessel = oapiGetVesselByIndex (i);
		oapiGetObjectName (hVessel, cbuf, 128);
		SendDlgItemMessage (hDlg, IDC_VESSELLIST, CB_ADDSTRING, 0, (LPARAM)cbuf);
	}
	g_Param.vessel = oapiGetFocusInterface();
	SendDlgItemMessage (hDlg, IDC_VESSELLIST, CB_SELECTSTRING, 0, (LPARAM)g_Param.vessel->GetName());
}

// Dialog message callback function
LRESULT CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	char cbuf[128];
	int idx;
	OBJHANDLE hVessel;

	switch (uMsg) {
	case WM_INITDIALOG:
		CreateVesselList (hWnd);
		return 0;
	case WM_DESTROY:
		g_Param.hDlg = NULL;
		return 0;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDCANCEL:
			oapiCloseDialog (hWnd);
			return TRUE;
		case IDC_FOCUS:
			oapiSetFocusObject (g_Param.vessel->GetHandle());
			break;
		case IDC_VESSELLIST:
			if (HIWORD(wParam) == CBN_SELCHANGE) {
				idx = SendDlgItemMessage (hWnd, IDC_VESSELLIST, CB_GETCURSEL, 0, 0);
				SendDlgItemMessage (hWnd, IDC_VESSELLIST, CB_GETLBTEXT, idx, (LPARAM)cbuf);
				hVessel = oapiGetVesselByName (cbuf);
				if (hVessel) g_Param.vessel = oapiGetVesselInterface (hVessel);
			}
			break;

		case IDC_KILLROT:
		case IDC_KILLROT2:
			g_Param.vessel->ToggleNavmode (NAVMODE_KILLROT);
			return TRUE;
		case IDC_HLEVEL:
			g_Param.vessel->ToggleNavmode (NAVMODE_HLEVEL);
			return TRUE;
		case IDC_PROGRADE:
			g_Param.vessel->ToggleNavmode (NAVMODE_PROGRADE);
			return TRUE;
		case IDC_RETROGRADE:
			g_Param.vessel->ToggleNavmode (NAVMODE_RETROGRADE);
			return TRUE;
		case IDC_NORMAL:
			g_Param.vessel->ToggleNavmode (NAVMODE_NORMAL);
			return TRUE;
		case IDC_ANTINORMAL:
			g_Param.vessel->ToggleNavmode (NAVMODE_ANTINORMAL);
			return TRUE;
		}
		break;
	case WM_HSCROLL:
		switch (GetDlgCtrlID ((HWND)lParam)) {
		case IDC_MAIN_GAUGE:
			switch (LOWORD(wParam)) {
			case SB_THUMBTRACK:
			case SB_LINELEFT:
			case SB_LINERIGHT:
				g_Param.vessel->SetThrusterGroupLevel (THGROUP_MAIN, HIWORD(wParam) * 0.01);
				return 0;
			}
			break;
		case IDC_RETRO_GAUGE:
			switch (LOWORD(wParam)) {
			case SB_THUMBTRACK:
			case SB_LINELEFT:
			case SB_LINERIGHT:
				g_Param.vessel->SetThrusterGroupLevel (THGROUP_RETRO, HIWORD(wParam) * 0.01);
				return 0;
			}
			break;
		case IDC_HOVER_GAUGE:
			switch (LOWORD(wParam)) {
			case SB_THUMBTRACK:
			case SB_LINELEFT:
			case SB_LINERIGHT:
				g_Param.vessel->SetThrusterGroupLevel (THGROUP_HOVER, HIWORD(wParam) * 0.01);
				return 0;
			}
			break;
		}
		break;
	}
	return oapiDefDialogProc (hWnd, uMsg, wParam, lParam);
}