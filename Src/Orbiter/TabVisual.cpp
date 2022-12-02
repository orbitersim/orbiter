// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// VisualTab class
//=============================================================================

#include <windows.h>
#include <commctrl.h>
#include "TabVisual.h"
#include "resource.h"
#include <fstream>

//-----------------------------------------------------------------------------

orbiter::VisualTab::VisualTab (const LaunchpadDialog *lp): LaunchpadTab (lp)
{
}

//-----------------------------------------------------------------------------

orbiter::VisualTab::~VisualTab ()
{
}

//-----------------------------------------------------------------------------

void orbiter::VisualTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_VIS);
}

//-----------------------------------------------------------------------------

void orbiter::VisualTab::GetConfig (const Config *cfg)
{
	char cbuf[256];

	SendDlgItemMessage(hTab, IDC_OPT_COMPLEXMODEL, BM_SETCHECK,
		pCfg->CfgLogicPrm.FlightModelLevel ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_OPT_DAMAGE, BM_SETCHECK,
		pCfg->CfgLogicPrm.DamageSetting ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_OPT_DISTMASS, BM_SETCHECK,
		pCfg->CfgPhysicsPrm.bDistributedMass ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_OPT_COMPLEXGRAV, BM_SETCHECK,
		pCfg->CfgPhysicsPrm.bNonsphericalGrav ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_OPT_RPRESSURE, BM_SETCHECK,
		pCfg->CfgPhysicsPrm.bRadiationPressure ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_OPT_WIND, BM_SETCHECK,
		pCfg->CfgPhysicsPrm.bAtmWind ? BST_CHECKED : BST_UNCHECKED, 0);
}

//-----------------------------------------------------------------------------

void orbiter::VisualTab::SetConfig (Config *cfg)
{
	DWORD i;
	char cbuf[128];
	double d;

	pCfg->CfgLogicPrm.FlightModelLevel = (SendDlgItemMessage(hTab, IDC_OPT_COMPLEXMODEL, BM_GETCHECK, 0, 0) == BST_CHECKED ? 1 : 0);
	pCfg->CfgLogicPrm.DamageSetting = (SendDlgItemMessage(hTab, IDC_OPT_DAMAGE, BM_GETCHECK, 0, 0) == BST_CHECKED ? 1 : 0);
	pCfg->CfgPhysicsPrm.bDistributedMass = (SendDlgItemMessage(hTab, IDC_OPT_DISTMASS, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgPhysicsPrm.bNonsphericalGrav = (SendDlgItemMessage(hTab, IDC_OPT_COMPLEXGRAV, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgPhysicsPrm.bRadiationPressure = (SendDlgItemMessage(hTab, IDC_OPT_RPRESSURE, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgPhysicsPrm.bAtmWind = (SendDlgItemMessage(hTab, IDC_OPT_WIND, BM_GETCHECK, 0, 0) == BST_CHECKED);
}

//-----------------------------------------------------------------------------

bool orbiter::VisualTab::OpenHelp ()
{
	OpenTabHelp ("tab_visual");
	return true;
}

//-----------------------------------------------------------------------------

BOOL orbiter::VisualTab::OnMessage (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return FALSE;
}
