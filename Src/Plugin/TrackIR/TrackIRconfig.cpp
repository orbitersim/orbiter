#include "TrackIR.h"
#include "TrackIRconfig.h"
#include "resource.h"
#include <string.h>
#include <stdio.h>
#include <commctrl.h>

extern GPARAMS gParams;

TrackIRconfig *TrackIRconfig::tirc = 0;

char *TrackIRconfig::Description()
{
	static char *desc = "Configure and test the NaturalPoint® TrackIR(tm) Optical Headtracker.";
	return desc;
}

bool TrackIRconfig::clbkOpen (HWND hLaunchpad)
{
	// respond to user double-clicking the item in the list
	tirc = this; // keep a global pointer to be used by the message handlers
	return OpenDialog (gParams.hInst, hLaunchpad, IDD_CONFIG, DlgProc);
}

int TrackIRconfig::clbkWriteConfig ()
{
	char cbuf[256];
	FILEHANDLE hFile = oapiOpenFile (cfgfile, FILE_OUT, ROOT);
	oapiWriteItem_int (hFile, "CMODE", trackir->GetCameraMode());
	sprintf (cbuf, "%d %f", trackir->vcmode.trackrotation, trackir->vcmode.rotationrange);
	oapiWriteItem_string (hFile, "VCROT", cbuf);
	sprintf (cbuf, "%d %f", trackir->vcmode.trackposition, trackir->vcmode.positionrange);
	oapiWriteItem_string (hFile, "VCPOS", cbuf);
	sprintf (cbuf, "%d %f", trackir->vcmode.freezeonmouse, trackir->vcmode.freeze_t);
	oapiWriteItem_string (hFile, "FREEZE", cbuf);
	sprintf (cbuf, "%d %d", trackir->trkmode.trackrotation, trackir->trkmode.rotationdata);
	oapiWriteItem_string (hFile, "TRKROT", cbuf);
	sprintf (cbuf, "%d", trackir->trkmode.trackzoom);
	oapiWriteItem_string (hFile, "TRKZOOM", cbuf);
	sprintf (cbuf, "%0.2f %0.2f", trackir->trkmode.deadzone, trackir->trkmode.speed);
	oapiWriteItem_string (hFile, "TRKPRM", cbuf);
	oapiCloseFile (hFile, FILE_OUT);
	return 0;
}

void TrackIRconfig::InitDialog (HWND hDlg)
{
	char cbuf[256];
	TC_ITEM tie;
	tie.mask = TCIF_TEXT;
	tie.iImage = -1;
	tie.pszText = cbuf;

	strcpy (cbuf, "Mode");
	SendDlgItemMessage (hDlg, IDC_TAB1, TCM_INSERTITEM, 0, (LPARAM)&tie);
	hTab[0] = CreateDialog (gParams.hInst, MAKEINTRESOURCE(IDD_CFG_MODE), hDlg, TabProc_mode);

	strcpy (cbuf, "VC");
	SendDlgItemMessage (hDlg, IDC_TAB1, TCM_INSERTITEM, 1, (LPARAM)&tie);
	hTab[1] = CreateDialog (gParams.hInst, MAKEINTRESOURCE(IDD_CFG_VC), hDlg, TabProc_cfg);

	strcpy (cbuf, "Track");
	SendDlgItemMessage (hDlg, IDC_TAB1, TCM_INSERTITEM, 2, (LPARAM)&tie);
	hTab[2] = CreateDialog (gParams.hInst, MAKEINTRESOURCE(IDD_CFG_TRACK), hDlg, TabProc_trk);

	strcpy (cbuf, "Diagnostic");
	SendDlgItemMessage (hDlg, IDC_TAB1, TCM_INSERTITEM, 3, (LPARAM)&tie);
	hTab[3] = CreateDialog (gParams.hInst, MAKEINTRESOURCE(IDD_CFG_DIAGNOSTIC), hDlg, TabProc_diag);

	SwitchTab (hDlg);
}

void TrackIRconfig::CloseDialog (HWND hDlg)
{
	int i;
	for (i = 0; i < NTAB; i++)
		DestroyWindow (hTab[i]);
	EndDialog (hDlg, 0);
}

void TrackIRconfig::SwitchTab (HWND hDlg)
{
	int pg, cpg = TabCtrl_GetCurSel (GetDlgItem (hDlg, IDC_TAB1));
	for (pg = 0; pg < NTAB; pg++)
		if (pg != cpg) ShowWindow (hTab[pg], SW_HIDE);
	ShowWindow (hTab[cpg], SW_SHOW);
}

void TrackIRconfig::Apply (HWND hDlg)
{
	char cbuf[256];
	double t;
	DWORD cmode = 0;

	// main page
	if (SendDlgItemMessage (hTab[0], IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED) cmode |= CAMMODE_VC;
	if (SendDlgItemMessage (hTab[0], IDC_CHECK2, BM_GETCHECK, 0, 0) == BST_CHECKED) cmode |= CAMMODE_PANELCOCKPIT;
	if (SendDlgItemMessage (hTab[0], IDC_CHECK3, BM_GETCHECK, 0, 0) == BST_CHECKED) cmode |= CAMMODE_GENERICCOCKPIT;
	if (SendDlgItemMessage (hTab[0], IDC_CHECK4, BM_GETCHECK, 0, 0) == BST_CHECKED) cmode |= CAMMODE_TRACK;
	if (SendDlgItemMessage (hTab[0], IDC_CHECK5, BM_GETCHECK, 0, 0) == BST_CHECKED) cmode |= CAMMODE_GROUND;
	trackir->SetCameraMode (cmode);

	// VC config page
	trackir->vcmode.trackrotation = (SendDlgItemMessage (hTab[1], IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED);
	GetWindowText (GetDlgItem (hTab[1], IDC_EDIT1), cbuf, 256);
	if (sscanf (cbuf, "%lf", &t)) trackir->vcmode.rotationrange = max (1.0*RAD, min (PI, t*RAD));
	trackir->vcmode.trackposition = (SendDlgItemMessage (hTab[1], IDC_CHECK2, BM_GETCHECK, 0, 0) == BST_CHECKED);
	GetWindowText (GetDlgItem (hTab[1], IDC_EDIT2), cbuf, 256);
	if (sscanf (cbuf, "%lf", &t)) trackir->vcmode.positionrange = max (0.0, min (2.0, t));
	trackir->vcmode.freezeonmouse = (SendDlgItemMessage (hTab[1], IDC_CHECK3, BM_GETCHECK, 0, 0) == BST_CHECKED);
	GetWindowText (GetDlgItem (hTab[1], IDC_EDIT3), cbuf, 256);
	if (sscanf (cbuf, "%lf", &t)) trackir->vcmode.freeze_t = t;

	// Track config mode
	trackir->trkmode.trackrotation = (SendDlgItemMessage (hTab[2], IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED);
	trackir->trkmode.trackzoom = (SendDlgItemMessage (hTab[2], IDC_CHECK2, BM_GETCHECK, 0, 0) == BST_CHECKED);
	trackir->trkmode.rotationdata = (SendDlgItemMessage (hTab[2], IDC_RADIO1, BM_GETCHECK, 0, 0) == BST_CHECKED ?
		ExternalCameraControl::TrackMode::BYROTATION : ExternalCameraControl::TrackMode::BYPOSITION);
	GetWindowText (GetDlgItem (hTab[2], IDC_EDIT1), cbuf, 256);
	if (sscanf (cbuf, "%lf", &t)) trackir->trkmode.deadzone = max (0, min (100, t))*0.01;
	GetWindowText (GetDlgItem (hTab[2], IDC_EDIT2), cbuf, 256);
	if (sscanf (cbuf, "%lf", &t)) trackir->trkmode.speed = max(1, min (100, t))*0.05;
}

INT_PTR CALLBACK TrackIRconfig::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	LPNMHDR nmh;

	switch (uMsg) {
	case WM_INITDIALOG:
		tirc->InitDialog (hDlg);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDOK:
			tirc->Apply (hDlg);
			// fall through
		case IDCANCEL:
			tirc->CloseDialog (hDlg);
			return 0;
		}
		break;
	case WM_NOTIFY:
		nmh = (LPNMHDR)lParam;
		if (nmh->idFrom == IDC_TAB1) {
			if (nmh->code == TCN_SELCHANGE)
				tirc->SwitchTab (hDlg);
			return TRUE;
		}
		break;
	}
	return 0;
}

INT_PTR CALLBACK TrackIRconfig::TabProc_mode (HWND hTab, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	TrackIR *trackir = tirc->trackir;
	switch (uMsg) {
	case WM_INITDIALOG: {
		DWORD cmode = trackir->GetCameraMode();
		SendDlgItemMessage (hTab, IDC_CHECK1, BM_SETCHECK, cmode & CAMMODE_VC ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hTab, IDC_CHECK2, BM_SETCHECK, cmode & CAMMODE_PANELCOCKPIT ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hTab, IDC_CHECK3, BM_SETCHECK, cmode & CAMMODE_GENERICCOCKPIT ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hTab, IDC_CHECK4, BM_SETCHECK, cmode & CAMMODE_TRACK ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hTab, IDC_CHECK5, BM_SETCHECK, cmode & CAMMODE_GROUND ? BST_CHECKED:BST_UNCHECKED, 0);
		} break;
	}
	return 0;
}

INT_PTR CALLBACK TrackIRconfig::TabProc_cfg (HWND hTab, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	TrackIR *trackir = tirc->trackir;
	TrackIR::VCMode &vcmode = trackir->vcmode;

	switch (uMsg) {
	case WM_INITDIALOG: {
		char cbuf[256];
		SendDlgItemMessage (hTab, IDC_CHECK1, BM_SETCHECK, vcmode.trackrotation ? BST_CHECKED:BST_UNCHECKED, 0);
		sprintf (cbuf, "%0.0f", trackir->vcmode.rotationrange * DEG);
		SetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf);
		SendDlgItemMessage (hTab, IDC_CHECK2, BM_SETCHECK, vcmode.trackposition ? BST_CHECKED:BST_UNCHECKED, 0);
		sprintf (cbuf, "%0.2f", trackir->vcmode.positionrange);
		SetWindowText (GetDlgItem (hTab, IDC_EDIT2), cbuf);
		SendDlgItemMessage (hTab, IDC_CHECK3, BM_SETCHECK, vcmode.freezeonmouse ? BST_CHECKED:BST_UNCHECKED, 0);
		sprintf (cbuf, "%0.1f", trackir->vcmode.freeze_t);
		SetWindowText (GetDlgItem (hTab, IDC_EDIT3), cbuf);
		} break;
	}
	return 0;
}

INT_PTR CALLBACK TrackIRconfig::TabProc_trk (HWND hTab, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	TrackIR *trackir = tirc->trackir;
	TrackIR::TrackMode &trkmode = trackir->trkmode;

	switch (uMsg) {
	case WM_INITDIALOG: {
		char cbuf[256];
		SendDlgItemMessage (hTab, IDC_CHECK1, BM_SETCHECK, trkmode.trackrotation ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hTab, IDC_CHECK2, BM_SETCHECK, trkmode.trackzoom ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hTab, IDC_RADIO1, BM_SETCHECK, trkmode.rotationdata == ExternalCameraControl::TrackMode::BYROTATION ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hTab, IDC_RADIO2, BM_SETCHECK, trkmode.rotationdata == ExternalCameraControl::TrackMode::BYPOSITION ? BST_CHECKED:BST_UNCHECKED, 0);
		sprintf (cbuf, "%d", (int)(trkmode.deadzone*100.0));
		SetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf);
		sprintf (cbuf, "%d", (int)(trkmode.speed*20));
		SetWindowText (GetDlgItem (hTab, IDC_EDIT2), cbuf);
		} break;
	}
	return 0;
}

INT_PTR CALLBACK TrackIRconfig::TabProc_diag (HWND hTab, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	TrackIR *trackir = tirc->trackir;
	switch (uMsg) {
	case WM_INITDIALOG:
		SetWindowText (GetDlgItem (hTab, IDC_EDIT1), trackir->dllPath);
		SetWindowText (GetDlgItem (hTab, IDC_EDIT2), trackir->cVersion);
		break;
	}
	return 0;
}