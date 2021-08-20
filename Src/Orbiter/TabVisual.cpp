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
	ncsphere_img = 0;
}

//-----------------------------------------------------------------------------

orbiter::VisualTab::~VisualTab ()
{
	EmptyCSphereList();
}

//-----------------------------------------------------------------------------

void orbiter::VisualTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_VIS);

	static int item[] = {
		IDC_VIS_STATIC1, IDC_VIS_STATIC2, IDC_VIS_STATIC3, IDC_VIS_STATIC4,
		IDC_VIS_STATIC5, IDC_VIS_STATIC6, IDC_VIS_STATIC7, IDC_VIS_STATIC8,
		IDC_VIS_CLOUD, IDC_VIS_CSHADOW, IDC_VIS_HAZE, IDC_VIS_FOG,
		IDC_VIS_REFWATER, IDC_VIS_RIPPLE, IDC_VIS_LIGHTS, IDC_VIS_LTLEVEL,
		IDC_VIS_MAXLEVEL, IDC_VIS_VSHADOW, IDC_VIS_SHADOW, IDC_VIS_SPECULAR,
		IDC_VIS_REENTRY, IDC_VIS_PARTICLE, IDC_VIS_LOCALLIGHT, IDC_VIS_AMBIENT,
		IDC_VIS_BGIMAGE, IDC_VIS_BGINTENS,
		IDC_VIS_ELEV, IDC_VIS_ELEVMODE
	};

	RegisterItemPositions (item, 28);
}

//-----------------------------------------------------------------------------

void orbiter::VisualTab::GetConfig (const Config *cfg)
{
	char cbuf[256];

	SendDlgItemMessage (hTab, IDC_VIS_SHADOW, BM_SETCHECK,
		pCfg->CfgVisualPrm.bShadows ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_VSHADOW, BM_SETCHECK,
		pCfg->CfgVisualPrm.bVesselShadows ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_CLOUD, BM_SETCHECK,
		pCfg->CfgVisualPrm.bClouds ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_CSHADOW, BM_SETCHECK,
		pCfg->CfgVisualPrm.bCloudShadows ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_LIGHTS, BM_SETCHECK,
		pCfg->CfgVisualPrm.bNightlights ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_REFWATER, BM_SETCHECK,
		pCfg->CfgVisualPrm.bWaterreflect ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_RIPPLE, BM_SETCHECK,
		pCfg->CfgVisualPrm.bSpecularRipple ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_HAZE, BM_SETCHECK,
		pCfg->CfgVisualPrm.bHaze ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_FOG, BM_SETCHECK,
		pCfg->CfgVisualPrm.bFog ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_SPECULAR, BM_SETCHECK,
		pCfg->CfgVisualPrm.bSpecular ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_REENTRY, BM_SETCHECK,
		pCfg->CfgVisualPrm.bReentryFlames ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_PARTICLE, BM_SETCHECK,
		pCfg->CfgVisualPrm.bParticleStreams ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_LOCALLIGHT, BM_SETCHECK,
		pCfg->CfgVisualPrm.bLocalLight ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VIS_ELEV, BM_SETCHECK,
		pCfg->CfgVisualPrm.ElevMode ? BST_CHECKED : BST_UNCHECKED, 0);
	SetWindowText (GetDlgItem (hTab, IDC_VIS_AMBIENT), _itoa (pCfg->CfgVisualPrm.AmbientLevel, cbuf, 10));
	sprintf (cbuf, "%0.2f", pCfg->CfgVisualPrm.LightBrightness);
	SetWindowText (GetDlgItem (hTab, IDC_VIS_LTLEVEL), cbuf);
	SetWindowText (GetDlgItem (hTab, IDC_VIS_MAXLEVEL), _itoa (pCfg->CfgVisualPrm.PlanetMaxLevel, cbuf, 10));
	sprintf (cbuf, "%0.2f", pCfg->CfgVisualPrm.CSphereBgIntens);
	SetWindowText (GetDlgItem (hTab, IDC_VIS_BGINTENS), cbuf);

	SendDlgItemMessage (hTab, IDC_VIS_ELEVMODE, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_VIS_ELEVMODE, CB_ADDSTRING, 0, (LPARAM)"linear interpolation");
	SendDlgItemMessage (hTab, IDC_VIS_ELEVMODE, CB_ADDSTRING, 0, (LPARAM)"cubic interpolation");
	SendDlgItemMessage (hTab, IDC_VIS_ELEVMODE, CB_SETCURSEL, pCfg->CfgVisualPrm.ElevMode < 2 ? 0:1, 0);

	EmptyCSphereList();

	SendDlgItemMessage (hTab, IDC_VIS_BGIMAGE, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_VIS_BGIMAGE, CB_ADDSTRING, 0, (LPARAM)"<none>");

	std::ifstream ifs(cfg->ConfigPath ("CSphere\\bkgimage"));
	if (ifs) {
		char *c;
		while (ifs.getline(cbuf, 256)) {
			c = strtok (cbuf, "|");
			if (c) {
				SendDlgItemMessage(hTab, IDC_VIS_BGIMAGE, CB_ADDSTRING, 0, (LPARAM)c);
				c = strtok (NULL, "\n");
				AddCSphereList (c);
			}
		}
	}
	int idx = SendDlgItemMessage (hTab, IDC_VIS_BGIMAGE, CB_FINDSTRINGEXACT, -1, (LPARAM)cfg->CfgVisualPrm.CSphereBgImage);
	if (idx == CB_ERR) idx = 0;
	SendDlgItemMessage (hTab, IDC_VIS_BGIMAGE, CB_SETCURSEL, idx, 0);

	VisualsChanged ();
}

//-----------------------------------------------------------------------------

void orbiter::VisualTab::SetConfig (Config *cfg)
{
	DWORD i;
	char cbuf[128];
	double d;

	pCfg->CfgVisualPrm.bShadows = (SendDlgItemMessage (hTab, IDC_VIS_SHADOW, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgVisualPrm.bVesselShadows = (SendDlgItemMessage (hTab, IDC_VIS_VSHADOW, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgVisualPrm.bClouds = (SendDlgItemMessage (hTab, IDC_VIS_CLOUD, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgVisualPrm.bCloudShadows = (SendDlgItemMessage (hTab, IDC_VIS_CSHADOW, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgVisualPrm.bNightlights = (SendDlgItemMessage (hTab, IDC_VIS_LIGHTS, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgVisualPrm.bWaterreflect = (SendDlgItemMessage (hTab, IDC_VIS_REFWATER, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgVisualPrm.bSpecularRipple = (SendDlgItemMessage (hTab, IDC_VIS_RIPPLE, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgVisualPrm.bHaze = (SendDlgItemMessage (hTab, IDC_VIS_HAZE, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgVisualPrm.bFog = (SendDlgItemMessage (hTab, IDC_VIS_FOG, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgVisualPrm.bSpecular = (SendDlgItemMessage (hTab, IDC_VIS_SPECULAR, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgVisualPrm.bReentryFlames = (SendDlgItemMessage (hTab, IDC_VIS_REENTRY, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgVisualPrm.bParticleStreams = (SendDlgItemMessage (hTab, IDC_VIS_PARTICLE, BM_GETCHECK, 0, 0) == BST_CHECKED);
	pCfg->CfgVisualPrm.bLocalLight = (SendDlgItemMessage (hTab, IDC_VIS_LOCALLIGHT, BM_GETCHECK, 0, 0) == BST_CHECKED);
	GetWindowText (GetDlgItem (hTab, IDC_VIS_AMBIENT), cbuf, 127);
	if (!sscanf (cbuf, "%lu", &i)) i = 15; else if (i > 255) i = 255;
	pCfg->SetAmbientLevel (i);
	GetWindowText (GetDlgItem (hTab, IDC_VIS_LTLEVEL), cbuf, 127);
	if (!sscanf (cbuf, "%lf", &d)) d = 0.5; else if (d < 0) d = 0.0; else if (d > 1) d = 1.0;
	pCfg->CfgVisualPrm.LightBrightness = d;
	GetWindowText (GetDlgItem (hTab, IDC_VIS_MAXLEVEL), cbuf, 127);
	if (!sscanf (cbuf, "%lu", &i)) i = SURF_MAX_PATCHLEVEL2;
	pCfg->CfgVisualPrm.PlanetMaxLevel = max (1, min (SURF_MAX_PATCHLEVEL2, i));
	pCfg->CfgVisualPrm.ElevMode = (SendDlgItemMessage (hTab, IDC_VIS_ELEV, BM_GETCHECK, 0, 0) != BST_CHECKED ?
		0 : SendDlgItemMessage (hTab, IDC_VIS_ELEVMODE, CB_GETCURSEL, 0, 0) + 1);
	GetWindowText (GetDlgItem (hTab, IDC_VIS_BGIMAGE), cbuf, 127);
	strncpy (pCfg->CfgVisualPrm.CSphereBgImage, cbuf, 64);
	i = SendDlgItemMessage (hTab, IDC_VIS_BGIMAGE, CB_GETCURSEL, 0, 0);
	if (i) strncpy (pCfg->CfgVisualPrm.CSphereBgPath, csphere_img_path[i-1], 128);
	else pCfg->CfgVisualPrm.CSphereBgPath[0] = '\0';

	GetWindowText (GetDlgItem (hTab, IDC_VIS_BGINTENS), cbuf, 127);
	if (!sscanf (cbuf, "%lf", &d)) d = 0.5; else if (d < 0) d = 0.0; else if (d > 1) d = 1.0;
	pCfg->CfgVisualPrm.CSphereBgIntens = d;
}

//-----------------------------------------------------------------------------

bool orbiter::VisualTab::OpenHelp ()
{
	OpenTabHelp ("tab_visual");
	return true;
}

//-----------------------------------------------------------------------------

INT_PTR orbiter::VisualTab::TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_VIS_CLOUD:
		case IDC_VIS_REFWATER:
			if (HIWORD (wParam) == BN_CLICKED) {
				VisualsChanged();
				return TRUE;
			}
			break;
		}
		break;
	}
	return FALSE;
}

//-----------------------------------------------------------------------------

void orbiter::VisualTab::VisualsChanged ()
{
	EnableWindow (GetDlgItem (hTab, IDC_VIS_CSHADOW),
		SendDlgItemMessage (hTab, IDC_VIS_CLOUD, BM_GETCHECK, 0, 0) == BST_CHECKED);
	EnableWindow (GetDlgItem (hTab, IDC_VIS_RIPPLE),
		SendDlgItemMessage (hTab, IDC_VIS_REFWATER, BM_GETCHECK, 0, 0) == BST_CHECKED);
}

//-----------------------------------------------------------------------------

void orbiter::VisualTab::EmptyCSphereList ()
{
	if (ncsphere_img) {
		for (int i = 0; i < ncsphere_img; i++)
			delete []csphere_img_path[i];
		delete []csphere_img_path;
		ncsphere_img = 0;
	}
}

//-----------------------------------------------------------------------------

void orbiter::VisualTab::AddCSphereList (const char *c)
{
	int len = strlen(c);
	char **tmp = new char*[ncsphere_img+1];
	if (ncsphere_img) {
		memcpy (tmp, csphere_img_path, ncsphere_img*sizeof(char*));
		delete []csphere_img_path;
	}
	csphere_img_path = tmp;
	csphere_img_path[ncsphere_img] = new char[len+1];
	strcpy (csphere_img_path[ncsphere_img++], c);
}
