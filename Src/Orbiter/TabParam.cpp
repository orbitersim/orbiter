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
		IDC_OPT_STATIC1, IDC_OPT_STATIC2, IDC_OPT_STATIC14,
		IDC_RADIO1, IDC_RADIO2, IDC_OPT_COMPLEXMODEL, IDC_OPT_DAMAGE, IDC_OPT_LIMFUEL,
		IDC_OPT_PADFUEL, IDC_OPT_COMPLEXGRAV, IDC_OPT_DISTMASS, IDC_OPT_WIND,
		IDC_OPT_RPRESSURE, IDC_OPT_FOCUS
	};

	RegisterItemPositions (item, ARRAYSIZE(item));
}

//-----------------------------------------------------------------------------

void orbiter::ParameterTab::GetConfig (const Config *cfg)
{
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

	pCfg->CfgUIPrm.bFocusFollowsMouse = (SendDlgItemMessage (hTab, IDC_OPT_FOCUS, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgLogicPrm.FlightModelLevel = (SendDlgItemMessage (hTab, IDC_OPT_COMPLEXMODEL, BM_GETCHECK, 0, 0) == BST_CHECKED ? 1:0);
	pCfg->CfgLogicPrm.DamageSetting = (SendDlgItemMessage (hTab, IDC_OPT_DAMAGE, BM_GETCHECK, 0, 0) == BST_CHECKED ? 1:0);
	pCfg->CfgLogicPrm.bLimitedFuel = (SendDlgItemMessage (hTab, IDC_OPT_LIMFUEL, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgLogicPrm.bPadRefuel = (SendDlgItemMessage (hTab, IDC_OPT_PADFUEL, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgPhysicsPrm.bDistributedMass = (SendDlgItemMessage (hTab, IDC_OPT_DISTMASS, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgPhysicsPrm.bNonsphericalGrav = (SendDlgItemMessage (hTab, IDC_OPT_COMPLEXGRAV, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgPhysicsPrm.bRadiationPressure = (SendDlgItemMessage (hTab, IDC_OPT_RPRESSURE, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgPhysicsPrm.bAtmWind = (SendDlgItemMessage (hTab, IDC_OPT_WIND, BM_GETCHECK, 0, 0) == BST_CHECKED);
}

//-----------------------------------------------------------------------------

bool orbiter::ParameterTab::OpenHelp ()
{
	OpenTabHelp ("tab_param");
	return true;
}