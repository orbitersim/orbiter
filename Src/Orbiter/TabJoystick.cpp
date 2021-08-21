// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// JoystickTab class
//=============================================================================

#include <windows.h>
#include <commctrl.h>
#include <io.h>
#include "Orbiter.h"
#include "Launchpad.h"
#include "TabJoystick.h"
#include "resource.h"

//-----------------------------------------------------------------------------
// JoystickTab class

orbiter::JoystickTab::JoystickTab (const LaunchpadDialog *lp): LaunchpadTab (lp)
{
}

//-----------------------------------------------------------------------------

void orbiter::JoystickTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_JOY);

	static int item[] = {
		IDC_JOY_STATIC1, IDC_JOY_DEVICE, IDC_JOY_STATIC2, IDC_JOY_STATIC3,
		IDC_JOY_DEAD, IDC_JOY_STATIC4, IDC_JOY_SAT, IDC_JOY_THROTTLE,
		IDC_JOY_STATIC5, IDC_JOY_INIT
	};

	RegisterItemPositions (item, 10);
}

//-----------------------------------------------------------------------------

void orbiter::JoystickTab::GetConfig (const Config *cfg)
{
	DWORD ndev;
	DIDEVICEINSTANCE *joylist;
	pLp->App()->GetDInput()->GetJoysticks (&joylist, &ndev);

	static char *nodev = "<Disabled>";
	static char *thmode[4] = {"<Keyboard only>", "Z-axis", "Slider 0", "Slider 1"};

	DWORD i;
	SendMessage (GetDlgItem (hTab, IDC_JOY_DEVICE), CB_ADDSTRING, 0, (LPARAM)nodev);
	for (i = 0; i < ndev; i++)
		SendMessage (GetDlgItem (hTab, IDC_JOY_DEVICE), CB_ADDSTRING, 0,
			(LPARAM)(joylist[i].tszProductName));
	SendDlgItemMessage (hTab, IDC_JOY_DEVICE, CB_SETCURSEL, (WPARAM)cfg->CfgJoystickPrm.Joy_idx, 0);

	for (i = 0; i < 4; i++)
		SendMessage (GetDlgItem (hTab, IDC_JOY_THROTTLE), CB_ADDSTRING, 0, (LPARAM)thmode[i]);
	SendDlgItemMessage (hTab, IDC_JOY_THROTTLE, CB_SETCURSEL, (WPARAM)cfg->CfgJoystickPrm.ThrottleAxis, 0);

	SendDlgItemMessage (hTab, IDC_JOY_DEAD, TBM_SETRANGE, FALSE, MAKELONG(0,1000));
	SendDlgItemMessage (hTab, IDC_JOY_DEAD, TBM_SETTICFREQ, 100, 0);
	SendDlgItemMessage (hTab, IDC_JOY_DEAD, TBM_SETPOS, TRUE, (LONG)cfg->CfgJoystickPrm.Deadzone/10);
	SendDlgItemMessage (hTab, IDC_JOY_INIT, BM_SETCHECK, cfg->CfgJoystickPrm.bThrottleIgnore ? BST_CHECKED : BST_UNCHECKED, 0);
	PostMessage (hTab, WM_HSCROLL, 0, (LPARAM)GetDlgItem(hTab, IDC_JOY_DEAD)); // force update
	SendDlgItemMessage (hTab, IDC_JOY_SAT,  TBM_SETRANGE, FALSE, MAKELONG(0,1000));
	SendDlgItemMessage (hTab, IDC_JOY_SAT,  TBM_SETTICFREQ, 100, 0);
	SendDlgItemMessage (hTab, IDC_JOY_SAT,  TBM_SETPOS, TRUE, (LONG)cfg->CfgJoystickPrm.ThrottleSaturation/10);
	PostMessage (hTab, WM_HSCROLL, 0, (LPARAM)GetDlgItem(hTab, IDC_JOY_SAT)); // force update
	JoystickChanged (cfg->CfgJoystickPrm.Joy_idx);
}

//-----------------------------------------------------------------------------

void orbiter::JoystickTab::SetConfig (Config *cfg)
{
	cfg->CfgJoystickPrm.Joy_idx = SendDlgItemMessage (hTab, IDC_JOY_DEVICE, CB_GETCURSEL, 0, 0);
	cfg->CfgJoystickPrm.ThrottleAxis = SendDlgItemMessage (hTab, IDC_JOY_THROTTLE, CB_GETCURSEL, 0, 0);
	cfg->CfgJoystickPrm.Deadzone = 10*SendDlgItemMessage (hTab, IDC_JOY_DEAD, TBM_GETPOS, 0, 0);
	cfg->CfgJoystickPrm.ThrottleSaturation = 10*SendDlgItemMessage (hTab, IDC_JOY_SAT, TBM_GETPOS, 0, 0);
	cfg->CfgJoystickPrm.bThrottleIgnore = (SendDlgItemMessage (hTab, IDC_JOY_INIT, BM_GETCHECK, 0, 0) == BST_CHECKED);
}

//-----------------------------------------------------------------------------

bool orbiter::JoystickTab::OpenHelp ()
{
	OpenTabHelp ("tab_joystick");
	return true;
}

//-----------------------------------------------------------------------------

INT_PTR orbiter::JoystickTab::TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_JOY_DEVICE:
			if (HIWORD(wParam) == CBN_SELCHANGE) {
				DWORD idx;
				idx = SendDlgItemMessage (hWnd, IDC_JOY_DEVICE, CB_GETCURSEL, 0, 0);
				JoystickChanged (idx);
				return TRUE;
			}
			break;
		}
		break;
	case WM_HSCROLL:
		switch (GetDlgCtrlID ((HWND)lParam)) {
		case IDC_JOY_DEAD: {
			char cbuf[64];
			int pos = SendDlgItemMessage (hWnd, IDC_JOY_DEAD, TBM_GETPOS, 0, 0);
			sprintf (cbuf, "Deadzone [%d]", pos);
			SetWindowText (GetDlgItem (hWnd, IDC_JOY_STATIC3), cbuf);
			} break;
		case IDC_JOY_SAT: {
			char cbuf[64];
			int pos = SendDlgItemMessage (hWnd, IDC_JOY_SAT, TBM_GETPOS, 0, 0);
			sprintf (cbuf, "Throttle saturation [%d]", pos);
			SetWindowText (GetDlgItem (hWnd, IDC_JOY_STATIC4), cbuf);
			} break;
		}
		break;
	}
	return FALSE;
}

//-----------------------------------------------------------------------------

void orbiter::JoystickTab::JoystickChanged (DWORD idx)
{
	BOOL bJoy = (idx != 0);
	for (DWORD i = IDC_JOY_STATIC2; i <= IDC_JOY_INIT; i++)
		EnableWindow (GetDlgItem (hTab, i), bJoy);
}

