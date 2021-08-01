// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// D3D7Extra.cpp
// Management of the configuration dialogs under the "Extra"
// Launchpad tab
// ==============================================================

#include <windows.h>
#include <commctrl.h>
#include "D3D7Extra.h"
#include "D3D7Config.h"
#include "resource.h"

char *D3D7ClientCfg::Name ()
{
	static char *name = "D3D7 Graphics Configuration";
	return name;
}

char *D3D7ClientCfg::Description ()
{
	static char *desc = "Configure the D3D7 graphics client plugin.\r\n\r\nThis allows to fine-tune rendering options and visual quality.";
	return desc;
}


D3D7PlanetRenderCfg::D3D7PlanetRenderCfg (oapi::D3D7Client *_gc, D3D7Config *_cfg)
: LaunchpadItem (), gc(_gc), cfg(_cfg)
{}

char *D3D7PlanetRenderCfg::Name ()
{
	static char *name = "Planet Rendering Options";
	return name;
}

char *D3D7PlanetRenderCfg::Description ()
{
	static char *desc = "Configure the rendering options for planets and other celestial objects.";
	return desc;
}

bool D3D7PlanetRenderCfg::clbkOpen (HWND hLaunchpad)
{
	extern HINSTANCE g_hInst;
	return OpenDialog (g_hInst, hLaunchpad, IDD_EXTRA_PLANETRENDER, DlgProc);
}

void D3D7PlanetRenderCfg::InitDialog (HWND hDlg)
{
	int i, j, mode;
	char cbuf[64];

	mode = cfg->PlanetPreloadMode;
	for (i = 0; i < 2; i++)
		SendDlgItemMessage (hDlg, IDC_RADIO4+i, BM_SETCHECK, (i==mode?BST_CHECKED:BST_UNCHECKED), 0);
	mode = cfg->PlanetMipmapMode;
	for (i = 0; i < 3; i++)
		SendDlgItemMessage (hDlg, IDC_RADIO1+i, BM_SETCHECK, (i==mode?BST_CHECKED:BST_UNCHECKED), 0);
	SendDlgItemMessage (hDlg, IDC_SLIDER1, TBM_SETRANGE, FALSE, MAKELONG(-10,10));
	SendDlgItemMessage (hDlg, IDC_SLIDER1, TBM_SETTICFREQ, 1, 0);
	SendDlgItemMessage (hDlg, IDC_SLIDER1, TBM_SETPOS, TRUE, (LONG)(cfg->PlanetMipmapBias*10.0));
	SendDlgItemMessage (hDlg, IDC_SLIDER2, TBM_SETRANGE, FALSE, MAKELONG(-10,10));
	SendDlgItemMessage (hDlg, IDC_SLIDER2, TBM_SETTICFREQ, 1, 0);
	SendDlgItemMessage (hDlg, IDC_SLIDER2, TBM_SETPOS, TRUE, (LONG)(*(double*)gc->GetConfigParam (CFGPRM_RESOLUTIONBIAS)*5.0));
	SendDlgItemMessage (hDlg, IDC_SLIDER3, TBM_SETRANGE, FALSE, MAKELONG(4,6));
	SendDlgItemMessage (hDlg, IDC_SLIDER3, TBM_SETTICFREQ, 1, 0);
	SendDlgItemMessage (hDlg, IDC_SLIDER3, TBM_SETPOS, TRUE, (LONG)(*(int*)gc->GetConfigParam (CFGPRM_TILEPATCHRES)));
	sprintf (cbuf, "%d", cfg->PlanetLoadFrequency);
	SetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf);

	SendDlgItemMessage (hDlg, IDC_COMBO1, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hDlg, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)"None");
	for (i = 2; i <= 16; i*=2) {
		sprintf (cbuf, "%dx", i);
		SendDlgItemMessage (hDlg, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)cbuf);
	}
	mode = min (cfg->PlanetAnisoMode, 16);
	for (i = 0, j = 1; j < mode; i++, j*=2);
	SendDlgItemMessage (hDlg, IDC_COMBO1, CB_SETCURSEL, i, 0);

	SendDlgItemMessage (hDlg, IDC_COMBO2, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hDlg, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM)"Load from tile cache only");
	SendDlgItemMessage (hDlg, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM)"Load from compressed archive only");
	SendDlgItemMessage (hDlg, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM)"Try cache first, then archive");
	DWORD m = cfg->PlanetTileLoadFlags;
	SendDlgItemMessage (hDlg, IDC_COMBO2, CB_SETCURSEL, m-1, 0);

	Update (hDlg);
}

void D3D7PlanetRenderCfg::Update (HWND hDlg)
{
	bool enable;
	int i;

	enable = (SendDlgItemMessage (hDlg, IDC_RADIO1, BM_GETCHECK, 0, 0) != BST_CHECKED);
	static int CtrlId1[5] = {IDC_SLIDER1, IDC_STATIC1, IDC_STATIC2, IDC_STATIC3, IDC_STATIC4};
	for (i = 0; i < 5; i++)
		EnableWindow (GetDlgItem (hDlg, CtrlId1[i]), enable ? TRUE:FALSE);

	enable = (SendDlgItemMessage (hDlg, IDC_RADIO4, BM_GETCHECK, 0, 0) == BST_CHECKED);
	static int CtrlId2[2] = {IDC_EDIT1, IDC_STATIC5};
	for (i = 0; i < 2; i++)
		EnableWindow (GetDlgItem (hDlg, CtrlId2[i]), enable ? TRUE:FALSE);
}

void D3D7PlanetRenderCfg::Apply (HWND hDlg)
{
	int i, mode;
	char cbuf[64];

	for (mode = 0; mode < 1; mode++)
		if (SendDlgItemMessage (hDlg, IDC_RADIO4+mode, BM_GETCHECK, 0, 0) == BST_CHECKED) break;
	cfg->PlanetPreloadMode = mode;
	for (mode = 0; mode < 2; mode++)
		if (SendDlgItemMessage (hDlg, IDC_RADIO1+mode, BM_GETCHECK, 0, 0) == BST_CHECKED) break;
	cfg->PlanetMipmapMode = mode;
	cfg->PlanetMipmapBias = 0.1 * SendDlgItemMessage (hDlg, IDC_SLIDER1, TBM_GETPOS, 0, 0);
	*(double*)gc->GetConfigParam (CFGPRM_RESOLUTIONBIAS) = 0.2 * SendDlgItemMessage (hDlg, IDC_SLIDER2, TBM_GETPOS, 0, 0);
	*(int*)gc->GetConfigParam (CFGPRM_TILEPATCHRES) = SendDlgItemMessage (hDlg, IDC_SLIDER3, TBM_GETPOS, 0, 0);

	GetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf, 64);
	if (sscanf (cbuf, "%d", &cfg->PlanetLoadFrequency) == 0)
		cfg->PlanetLoadFrequency = 20;

	i = SendDlgItemMessage (hDlg, IDC_COMBO1, CB_GETCURSEL, 0, 0);
	for (mode = 1; i > 0; mode *= 2, i--);
	cfg->PlanetAnisoMode = mode;

	i = SendDlgItemMessage (hDlg, IDC_COMBO2, CB_GETCURSEL, 0, 0);
	cfg->PlanetTileLoadFlags = (DWORD)i+1;
}

void D3D7PlanetRenderCfg::CloseDialog (HWND hDlg)
{
	EndDialog (hDlg, 0);
}

INT_PTR CALLBACK D3D7PlanetRenderCfg::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	D3D7PlanetRenderCfg *pCfg = (D3D7PlanetRenderCfg*)GetWindowLongPtr (hDlg, DWLP_USER);

	switch (uMsg) {
	case WM_INITDIALOG:
		pCfg = (D3D7PlanetRenderCfg*)lParam;
		SetWindowLongPtr (hDlg, DWLP_USER, (LONG)pCfg);
		pCfg->InitDialog (hDlg);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDOK:
			pCfg->Apply (hDlg);
			// fall through
		case IDCANCEL:
			pCfg->CloseDialog (hDlg);
			return 0;
		case IDC_RADIO1:
		case IDC_RADIO2:
		case IDC_RADIO3:
		case IDC_RADIO4:
		case IDC_RADIO5:
			pCfg->Update (hDlg);
			return 0;
		}
		break;
	}
	return 0;
}
