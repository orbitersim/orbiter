#include <windows.h>
#include <commctrl.h>
#include "D3D9Extra.h"
#include "D3D9Config.h"
#include "resource.h"




char *D3D9ClientCfg::Name ()
{
	static char *name = "D3D9 Graphics Configuration\0";
	return name;
}

char *D3D9ClientCfg::Description ()
{
	static char *desc = "Configure the D3D9 graphics client plugin.\r\n\r\nThis allows to fine-tune rendering options and visual quality.\0";
	return desc;
}

char *D3D9PlanetRenderCfg::Name ()
{
	static char *name = "Planet Rendering Options\0";
	return name;
}

char *D3D9PlanetRenderCfg::Description ()
{
	static char *desc = "Configure the rendering options for planets and other celestial objects.\0";
	return desc;
}

bool D3D9PlanetRenderCfg::clbkOpen (HWND hLaunchpad)
{
	extern HINSTANCE g_hInst;
	return OpenDialog (g_hInst, hLaunchpad, IDD_EXTRA_PLANETRENDER, DlgProc);
}

void D3D9PlanetRenderCfg::InitDialog(HWND hDlg)
{
	int i, j, mode;
	char cbuf[64];

	mode = Config->PlanetPreloadMode;
	for (i = 0; i < 2; i++)	SendDlgItemMessage (hDlg, IDC_RADIO4+i, BM_SETCHECK, (i==mode?BST_CHECKED:BST_UNCHECKED), 0);
	mode = Config->PlanetMipmapMode;
	for (i = 0; i < 3; i++)	SendDlgItemMessage (hDlg, IDC_RADIO1+i, BM_SETCHECK, (i==mode?BST_CHECKED:BST_UNCHECKED), 0);
	SendDlgItemMessage (hDlg, IDC_SLIDER1, TBM_SETRANGE, FALSE, MAKELONG(-10,10));
	SendDlgItemMessage (hDlg, IDC_SLIDER1, TBM_SETTICFREQ, 1, 0);
	SendDlgItemMessage (hDlg, IDC_SLIDER1, TBM_SETPOS, TRUE, (LONG)(Config->PlanetMipmapBias*10.0));
	sprintf_s(cbuf, 64, "%d", Config->PlanetLoadFrequency);
	SetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf);

	SendDlgItemMessage (hDlg, IDC_COMBO1, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hDlg, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)"None");
	for (i = 2; i <= 16; i*=2) {
		sprintf_s(cbuf, 64, "%dx", i);
		SendDlgItemMessage (hDlg, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)cbuf);
	}
	mode = min (Config->PlanetAnisoMode, 16);
	for (i = 0, j = 1; j < mode; i++, j*=2);
	SendDlgItemMessage (hDlg, IDC_COMBO1, CB_SETCURSEL, i, 0);
	
	Update (hDlg);
}

void D3D9PlanetRenderCfg::Update (HWND hDlg)
{

	bool enable;
	int i;

	enable = (SendDlgItemMessage (hDlg, IDC_RADIO1, BM_GETCHECK, 0, 0) != BST_CHECKED);
	static int CtrlId1[5] = {IDC_SLIDER1, IDC_STATIC1, IDC_STATIC2, IDC_STATIC3, IDC_STATIC4};
	for (i = 0; i < 5; i++)	EnableWindow (GetDlgItem (hDlg, CtrlId1[i]), enable ? TRUE:FALSE);

	enable = (SendDlgItemMessage (hDlg, IDC_RADIO4, BM_GETCHECK, 0, 0) == BST_CHECKED);
	static int CtrlId2[2] = {IDC_EDIT1, IDC_STATIC5};
	for (i = 0; i < 2; i++)	EnableWindow (GetDlgItem (hDlg, CtrlId2[i]), enable ? TRUE:FALSE);
}

void D3D9PlanetRenderCfg::Apply (HWND hDlg)
{
	
	int i, mode;
	char cbuf[64];

	for (mode = 0; mode < 1; mode++) if (SendDlgItemMessage (hDlg, IDC_RADIO4+mode, BM_GETCHECK, 0, 0) == BST_CHECKED) break;
	Config->PlanetPreloadMode = mode;

	//for (mode = 0; mode < 2; mode++) if (SendDlgItemMessage (hDlg, IDC_RADIO1+mode, BM_GETCHECK, 0, 0) == BST_CHECKED) break;
	//Config->PlanetMipmapMode = mode;

	Config->PlanetMipmapBias = 0.1 * SendDlgItemMessage (hDlg, IDC_SLIDER1, TBM_GETPOS, 0, 0);

	GetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf, 64);
	if (sscanf(cbuf, "%d", &Config->PlanetLoadFrequency) == 0)
		Config->PlanetLoadFrequency = 20;

	i = SendDlgItemMessage (hDlg, IDC_COMBO1, CB_GETCURSEL, 0, 0);
	for (mode = 1; i > 0; mode *= 2, i--);
	Config->PlanetAnisoMode = mode;
	
}

void D3D9PlanetRenderCfg::CloseDialog (HWND hDlg)
{
	EndDialog (hDlg, 0);
}

BOOL CALLBACK D3D9PlanetRenderCfg::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	
	D3D9PlanetRenderCfg *pCfg = (D3D9PlanetRenderCfg*)GetWindowLong (hDlg, DWL_USER);

	switch (uMsg) {
	case WM_INITDIALOG:
		pCfg = (D3D9PlanetRenderCfg*)lParam;
		SetWindowLong (hDlg, DWL_USER, (LONG)pCfg);
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
