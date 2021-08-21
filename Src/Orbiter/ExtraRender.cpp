// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ExtraTab item: Planet render options
//=============================================================================

#include <windows.h>
#include <commctrl.h>
#include "Launchpad.h"
#include "ExtraRender.h"
#include "Help.h"
#include "resource.h"

//=============================================================================

char *Extra_RenderOptions::Name ()
{
	return "Visualisation parameters";
}

char *Extra_RenderOptions::Description ()
{
	static char *desc = "Configure advanced rendering options affecting the appearance of the 3D world.\r\n\r\nCommon options can be accessed under the 'Visual effects' tab.";
	return desc;
}

//=============================================================================

char *Extra_PlanetRenderOptions::Name ()
{
	return "Planet rendering options";
}

char *Extra_PlanetRenderOptions::Description ()
{
	static char *desc = "Options affecting the visual quality and appearance of planetary bodies.";
	return desc;
}

bool Extra_PlanetRenderOptions::clbkOpen (HWND hParent)
{
	OpenDialog (hParent, IDD_EXTRA_PLANETRENDER, DlgProc);
	return true;
}

void Extra_PlanetRenderOptions::InitDialog (HWND hWnd)
{
	Config *cfg = pTab->Cfg();
	SetDialog (hWnd, pTab->Cfg()->CfgPRenderPrm);
}

void Extra_PlanetRenderOptions::ResetDialog (HWND hWnd)
{
	extern CFG_PLANETRENDERPRM CfgPRenderPrm_default;
	SetDialog (hWnd, CfgPRenderPrm_default);
}

void Extra_PlanetRenderOptions::SetDialog (HWND hWnd, const CFG_PLANETRENDERPRM &prm)
{
	int i, j, mode;
	char cbuf[64];

	mode = prm.PreloadMode;
	for (i = 0; i < 2; i++)
		SendDlgItemMessage (hWnd, IDC_RADIO4+i, BM_SETCHECK, (i==mode?BST_CHECKED:BST_UNCHECKED), 0);
	mode = prm.MipmapMode;
	for (i = 0; i < 3; i++)
		SendDlgItemMessage (hWnd, IDC_RADIO1+i, BM_SETCHECK, (i==mode?BST_CHECKED:BST_UNCHECKED), 0);
	SendDlgItemMessage (hWnd, IDC_SLIDER1, TBM_SETRANGE, FALSE, MAKELONG(-10,10));
	SendDlgItemMessage (hWnd, IDC_SLIDER1, TBM_SETTICFREQ, 1, 0);
	SendDlgItemMessage (hWnd, IDC_SLIDER1, TBM_SETPOS, TRUE, (LONG)(prm.MipmapBias*10.0));
	SendDlgItemMessage (hWnd, IDC_SLIDER2, TBM_SETRANGE, FALSE, MAKELONG(-10,10));
	SendDlgItemMessage (hWnd, IDC_SLIDER2, TBM_SETTICFREQ, 1, 0);
	SendDlgItemMessage (hWnd, IDC_SLIDER2, TBM_SETPOS, TRUE, (LONG)(prm.ResolutionBias*5.0));
	SendDlgItemMessage (hWnd, IDC_SLIDER3, TBM_SETRANGE, FALSE, MAKELONG(4,6));
	SendDlgItemMessage (hWnd, IDC_SLIDER3, TBM_SETTICFREQ, 1, 0);
	SendDlgItemMessage (hWnd, IDC_SLIDER3, TBM_SETPOS, TRUE, (LONG)(prm.PatchRes));
	sprintf (cbuf, "%d", prm.LoadFrequency);
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf);
	sprintf (cbuf, "%d", prm.CacheSize);
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT2), cbuf);
	SendDlgItemMessage (hWnd, IDC_CHECK1, BM_SETCHECK, (prm.bLoadOnThread ? BST_CHECKED : BST_UNCHECKED), 0);

	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)"None");
	for (i = 2; i <= 16; i*=2) {
		sprintf (cbuf, "%dx", i);
		SendDlgItemMessage (hWnd, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)cbuf);
	}
	mode = min (prm.AnisoMode, 16);
	for (i = 0, j = 1; j < mode; i++, j*=2);
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_SETCURSEL, i, 0);

	SendDlgItemMessage (hWnd, IDC_COMBO2, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hWnd, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM)"Load from tile cache only");
	SendDlgItemMessage (hWnd, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM)"Load from compressed archive only");
	SendDlgItemMessage (hWnd, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM)"Try cache first, then archive");
	DWORD m = prm.TileLoadFlags;
	SendDlgItemMessage (hWnd, IDC_COMBO2, CB_SETCURSEL, m-1, 0);

	Update (hWnd);
}

bool Extra_PlanetRenderOptions::StoreParams (HWND hWnd)
{
	Config *cfg = pTab->Cfg();
	int i, mode;
	char cbuf[64];
	CFG_PLANETRENDERPRM &prm = cfg->CfgPRenderPrm;

	for (mode = 0; mode < 1; mode++)
		if (SendDlgItemMessage (hWnd, IDC_RADIO4+mode, BM_GETCHECK, 0, 0) == BST_CHECKED) break;
	prm.PreloadMode = mode;
	for (mode = 0; mode < 2; mode++)
		if (SendDlgItemMessage (hWnd, IDC_RADIO1+mode, BM_GETCHECK, 0, 0) == BST_CHECKED) break;
	prm.MipmapMode = mode;
	prm.MipmapBias = 0.1 * SendDlgItemMessage (hWnd, IDC_SLIDER1, TBM_GETPOS, 0, 0);
	prm.ResolutionBias = 0.2 * SendDlgItemMessage (hWnd, IDC_SLIDER2, TBM_GETPOS, 0, 0);
	prm.PatchRes = SendDlgItemMessage (hWnd, IDC_SLIDER3, TBM_GETPOS, 0, 0);

	GetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf, 64);
	if (sscanf (cbuf, "%d", &prm.LoadFrequency) == 0)
		prm.LoadFrequency = 20;
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT2), cbuf, 64);
	if (sscanf (cbuf, "%d", &prm.CacheSize) == 0)
		prm.CacheSize = 40;
	prm.bLoadOnThread = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED);

	i = SendDlgItemMessage (hWnd, IDC_COMBO1, CB_GETCURSEL, 0, 0);
	for (mode = 1; i > 0; mode *= 2, i--);
	prm.AnisoMode = mode;

	i = SendDlgItemMessage (hWnd, IDC_COMBO2, CB_GETCURSEL, 0, 0);
	prm.TileLoadFlags = (DWORD)i+1;

	return true;
}

void Extra_PlanetRenderOptions::Update (HWND hWnd)
{
	bool enable;
	int i;

	enable = (SendDlgItemMessage (hWnd, IDC_RADIO1, BM_GETCHECK, 0, 0) != BST_CHECKED);
	static int CtrlId1[5] = {IDC_SLIDER1, IDC_STATIC1, IDC_STATIC2, IDC_STATIC3, IDC_STATIC4};
	for (i = 0; i < 5; i++)
		EnableWindow (GetDlgItem (hWnd, CtrlId1[i]), enable ? TRUE:FALSE);

	enable = (SendDlgItemMessage (hWnd, IDC_RADIO4, BM_GETCHECK, 0, 0) == BST_CHECKED);
	static int CtrlId2[2] = {IDC_EDIT1, IDC_STATIC5};
	for (i = 0; i < 2; i++)
		EnableWindow (GetDlgItem (hWnd, CtrlId2[i]), enable ? TRUE:FALSE);
}

bool Extra_PlanetRenderOptions::OpenHelp (HWND hWnd)
{
	OpenDefaultHelp (hWnd, "extra_planetrender");
	return true;
}

INT_PTR CALLBACK Extra_PlanetRenderOptions::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((Extra_PlanetRenderOptions*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BUTTON1:
			((Extra_PlanetRenderOptions*)GetWindowLongPtr (hWnd, DWLP_USER))->ResetDialog (hWnd);
			return 0;
		case IDC_BUTTON2:
			((Extra_PlanetRenderOptions*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
			return 0;
		case IDOK:
			if (((Extra_PlanetRenderOptions*)GetWindowLongPtr (hWnd, DWLP_USER))->StoreParams (hWnd))
				EndDialog (hWnd, 0);
			break;
		case IDC_RADIO1:
		case IDC_RADIO2:
		case IDC_RADIO3:
		case IDC_RADIO4:
		case IDC_RADIO5:
			((Extra_PlanetRenderOptions*)GetWindowLongPtr (hWnd, DWLP_USER))->Update (hWnd);
			return 0;
		}
		break;
	}
	return BuiltinLaunchpadItem::DlgProc (hWnd, uMsg, wParam, lParam);
}
