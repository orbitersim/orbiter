// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ParameterTab class
//=============================================================================

#include <windows.h>
#include <commctrl.h>
#include "TabParam.h"
#include "resource.h"

//-----------------------------------------------------------------------------

orbiter::ParameterTab::ParameterTab (const LaunchpadDialog *_lp): LaunchpadTab (_lp)
{
}

//-----------------------------------------------------------------------------

void orbiter::ParameterTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_OPT);

	static int item[] = {
		IDC_OPT_STATIC1, IDC_OPT_STATIC2, IDC_OPT_STATIC4, IDC_OPT_STATIC8,
		IDC_OPT_STATIC9, IDC_OPT_STATIC10, IDC_OPT_STATIC11, IDC_OPT_STATIC14,
		IDC_RADIO1, IDC_RADIO2, IDC_OPT_COMPLEXMODEL, IDC_OPT_DAMAGE, IDC_OPT_LIMFUEL,
		IDC_OPT_PADFUEL, IDC_OPT_COMPLEXGRAV, IDC_OPT_DISTMASS, IDC_OPT_WIND,
		IDC_OPT_RPRESSURE, IDC_OPT_FOCUS, IDC_OPT_MFDTRANSP, IDC_OPT_REFRESH,
		IDC_OPT_MFDSIZE, IDC_OPT_PANELSCALE, IDC_OPT_PANELSPD
	};

	RegisterItemPositions (item, ARRAYSIZE(item));
}

//-----------------------------------------------------------------------------

void orbiter::ParameterTab::GetConfig (const Config *cfg)
{
	char cbuf[20];
	SendDlgItemMessage (hTab, IDC_OPT_MFDTRANSP, BM_SETCHECK,
		pCfg->CfgLogicPrm.bMfdTransparent ? BST_CHECKED : BST_UNCHECKED, 0);
	sprintf (cbuf, "%0.2f", pCfg->CfgLogicPrm.InstrUpdDT);
	SetWindowText (GetDlgItem (hTab, IDC_OPT_REFRESH), cbuf);
	sprintf (cbuf, "%d", pCfg->CfgLogicPrm.MFDSize);
	SetWindowText (GetDlgItem (hTab, IDC_OPT_MFDSIZE), cbuf);
	sprintf (cbuf, "%0.2f", pCfg->CfgLogicPrm.PanelScale);
	SetWindowText (GetDlgItem (hTab, IDC_OPT_PANELSCALE), cbuf);
	sprintf (cbuf, "%0.0f", pCfg->CfgLogicPrm.PanelScrollSpeed*0.1);
	SetWindowText (GetDlgItem (hTab, IDC_OPT_PANELSPD), cbuf);
	SendDlgItemMessage (hTab, IDC_RADIO1, BM_SETCHECK,
		pCfg->CfgVisualPrm.StarPrm.map_log ? BST_UNCHECKED : BST_CHECKED, 0);
	SendDlgItemMessage (hTab, IDC_RADIO2, BM_SETCHECK,
		pCfg->CfgVisualPrm.StarPrm.map_log ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_OPT_COMPLEXMODEL, BM_SETCHECK,
		pCfg->CfgLogicPrm.FlightModelLevel ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_OPT_DAMAGE, BM_SETCHECK,
		pCfg->CfgLogicPrm.DamageSetting ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_OPT_LIMFUEL, BM_SETCHECK,
		pCfg->CfgLogicPrm.bLimitedFuel ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_OPT_PADFUEL, BM_SETCHECK,
		pCfg->CfgLogicPrm.bPadRefuel ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_OPT_DISTMASS, BM_SETCHECK,
		pCfg->CfgPhysicsPrm.bDistributedMass ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_OPT_COMPLEXGRAV, BM_SETCHECK,
		pCfg->CfgPhysicsPrm.bNonsphericalGrav ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_OPT_RPRESSURE, BM_SETCHECK,
		pCfg->CfgPhysicsPrm.bRadiationPressure ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_OPT_WIND, BM_SETCHECK,
		pCfg->CfgPhysicsPrm.bAtmWind ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_OPT_FOCUS, BM_SETCHECK,
		pCfg->CfgUIPrm.bFocusFollowsMouse ? BST_CHECKED : BST_UNCHECKED, 0);
}

//-----------------------------------------------------------------------------

void orbiter::ParameterTab::SetConfig (Config *cfg)
{
	DWORD i;
	char cbuf[128];
	double d;

	pCfg->CfgLogicPrm.bMfdTransparent = (SendDlgItemMessage (hTab, IDC_OPT_MFDTRANSP, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgUIPrm.bFocusFollowsMouse = (SendDlgItemMessage (hTab, IDC_OPT_FOCUS, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgLogicPrm.FlightModelLevel = (SendDlgItemMessage (hTab, IDC_OPT_COMPLEXMODEL, BM_GETCHECK, 0, 0) == BST_CHECKED ? 1:0);
	pCfg->CfgLogicPrm.DamageSetting = (SendDlgItemMessage (hTab, IDC_OPT_DAMAGE, BM_GETCHECK, 0, 0) == BST_CHECKED ? 1:0);
	pCfg->CfgLogicPrm.bLimitedFuel = (SendDlgItemMessage (hTab, IDC_OPT_LIMFUEL, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgLogicPrm.bPadRefuel = (SendDlgItemMessage (hTab, IDC_OPT_PADFUEL, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgPhysicsPrm.bDistributedMass = (SendDlgItemMessage (hTab, IDC_OPT_DISTMASS, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgPhysicsPrm.bNonsphericalGrav = (SendDlgItemMessage (hTab, IDC_OPT_COMPLEXGRAV, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgPhysicsPrm.bRadiationPressure = (SendDlgItemMessage (hTab, IDC_OPT_RPRESSURE, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgPhysicsPrm.bAtmWind = (SendDlgItemMessage (hTab, IDC_OPT_WIND, BM_GETCHECK, 0, 0) == BST_CHECKED);

	GetWindowText (GetDlgItem (hTab, IDC_OPT_REFRESH), cbuf, 127);
	if (!sscanf (cbuf, "%lf", &d) || d < 0.01 || d > 10) d = 1.0;
	pCfg->CfgLogicPrm.InstrUpdDT = d;

	GetWindowText (GetDlgItem (hTab, IDC_OPT_MFDSIZE), cbuf, 127);
	if (!sscanf (cbuf, "%lu", &i)) i = 6; else if (i < 1) i = 1; else if (i > 10) i = 10;
	pCfg->CfgLogicPrm.MFDSize = i;

	GetWindowText (GetDlgItem (hTab, IDC_OPT_PANELSCALE), cbuf, 127);
	if (!sscanf (cbuf, "%lf", &d)) d = 1.0; else if (d < 0.25) d = 0.25; else if (d > 4.0) d = 4.0;
	pCfg->CfgLogicPrm.PanelScale = d;

	GetWindowText (GetDlgItem (hTab, IDC_OPT_PANELSPD), cbuf, 127);
	if (!sscanf (cbuf, "%lf", &d)) d = 30.0; else if (d < -100.0) d = -100.0; else if (d > 100) d = 100.0;
	pCfg->CfgLogicPrm.PanelScrollSpeed = d*10.0;
}

//-----------------------------------------------------------------------------

bool orbiter::ParameterTab::OpenHelp ()
{
	OpenTabHelp ("tab_param");
	return true;
}