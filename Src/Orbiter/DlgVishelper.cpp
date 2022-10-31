// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Visual helper options dialog
// ======================================================================

#define STRICT 1
#define OEMRESOURCE

#include <io.h>
#include "Orbiter.h"
#include "Camera.h"
#include "Psys.h"
#include "DlgMgr.h"
#include "DlgVishelper.h"
#include "Resource.h"
#include "DlgCtrl.h"
#include "Uxtheme.h"
#include <Commctrl.h>

extern Orbiter *g_pOrbiter;
extern PlanetarySystem *g_psys;
extern Camera *g_camera;
extern HELPCONTEXT DefHelpContext;

// ======================================================================

DlgVishelper::DlgVishelper (HINSTANCE hInstance, HWND hParent, void *context)
: DialogWin (hInstance, hParent, IDD_VISHELPER, 0, 0, context)
{
	hcontext = 0;
	pos = &g_pOrbiter->Cfg()->CfgWindowPos.DlgCamera;
}

// ======================================================================

DlgVishelper::~DlgVishelper ()
{
	Clear ();
}

// ======================================================================

void DlgVishelper::Update()
{
	for (auto it = m_pTab.begin(); it != m_pTab.end(); it++)
		(*it)->UpdateControls((*it)->Tab());
}

// ======================================================================

void DlgVishelper::AddTab (HWND hDlg, VhelperTab *tab, const char *label)
{
	char cbuf[256];
	strcpy (cbuf, label);
	TC_ITEM tie;
	tie.mask = TCIF_TEXT;
	tie.iImage = -1;
	tie.pszText = cbuf;
	SendDlgItemMessage (hDlg, IDC_TAB1, TCM_INSERTITEM, m_pTab.size(), (LPARAM)&tie);
	m_pTab.push_back(tab);
	tab->CreateInterface();
}

// ======================================================================
// Display new tab page

void DlgVishelper::SwitchTab (HWND hDlg)
{
	size_t cpg = (size_t)TabCtrl_GetCurSel (GetDlgItem (hDlg, IDC_TAB1));
	for (size_t pg = 0; pg < m_pTab.size(); pg++)
		if (pg != cpg) m_pTab[pg]->Show (false);
	m_pTab[cpg]->Show (true);
	hcontext = m_pTab[cpg]->HelpContext();
}

// ======================================================================

void DlgVishelper::Clear ()
{
	for (auto it = m_pTab.begin(); it != m_pTab.end(); it++)
		delete (*it);
	m_pTab.clear();
}

// ======================================================================

BOOL DlgVishelper::OnInitDialog (HWND hDlg, WPARAM wParam, LPARAM lParam)
{
	HWND hTabFrame = hDlg;

	AddTab (hDlg, new TabPlanetarium (hTabFrame), "Planetarium");
	AddTab(hDlg, new TabLabels(hTabFrame), "Labels");
	AddTab (hDlg, new TabForces (hTabFrame), "Forces");
	AddTab (hDlg, new TabAxes (hTabFrame), "Axes");

	SwitchTab (hDlg);
	return TRUE;
}

// ======================================================================

BOOL DlgVishelper::OnCommand (HWND hDlg, WORD ctrlId, WORD notification, HWND hCtrl)
{
	switch (ctrlId) {
	case IDHELP:
		DefHelpContext.topic = hcontext;
		g_pOrbiter->OpenHelp (&DefHelpContext);
		return TRUE;
	}
	return DialogWin::OnCommand (hDlg, ctrlId, notification, hCtrl);
}

// ======================================================================

BOOL DlgVishelper::OnNotify (HWND hDlg, int idCtrl, LPNMHDR pnmh)
{
	if (pnmh->idFrom == IDC_TAB1) {
		if (pnmh->code == TCN_SELCHANGE) SwitchTab (hDlg);
		return TRUE;
	}
	return MSG_DEFAULT;
}


// ======================================================================
// ======================================================================

VhelperTab::VhelperTab (HWND hParentTab)
{
	m_hParent = hParentTab;
}

// ======================================================================

VhelperTab::~VhelperTab()
{
	SetWindowLongPtr(m_hTab, DWLP_USER, 0);
}

// ======================================================================

void VhelperTab::MakeTab(int dlgId)
{
	m_hTab = CreateDialogParam(g_pOrbiter->GetInstance(), MAKEINTRESOURCE(dlgId), m_hParent, s_DlgProc, (LPARAM)this);
}

// ======================================================================

void VhelperTab::Show (bool show)
{
	ShowWindow (m_hTab, show ? SW_SHOW : SW_HIDE);
}

// ======================================================================

INT_PTR VhelperTab::DlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		return OnInitDialog(hWnd, wParam, lParam);
	case WM_COMMAND:
		return OnCommand(hWnd, LOWORD(wParam), HIWORD(wParam), (HWND)lParam);
	case WM_HSCROLL:
		return OnHScroll(hWnd, wParam, lParam);
	default:
		return OnMessage(hWnd, uMsg, wParam, lParam);
	}
}

// ======================================================================

INT_PTR CALLBACK VhelperTab::s_DlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		EnableThemeDialogTexture(hWnd, ETDT_ENABLETAB);
		SetWindowLongPtr(hWnd, DWLP_USER, lParam);
		break;
	}
	VhelperTab* pTab = (VhelperTab*)(uMsg == WM_INITDIALOG ? lParam : GetWindowLongPtr(hWnd, DWLP_USER));
	return (pTab ? pTab->DlgProc(hWnd, uMsg, wParam, lParam) : DefWindowProc(hWnd, uMsg, wParam, lParam));
}


// ======================================================================
// ======================================================================

TabPlanetarium::TabPlanetarium (HWND hParentTab): VhelperTab (hParentTab)
{
}

// ======================================================================

void TabPlanetarium::CreateInterface()
{
	MakeTab(IDD_VHELP_PLANETARIUM);
}

// ======================================================================

void TabPlanetarium::UpdateControls(HWND hTab)
{
	DWORD& plnFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagPlanetarium;
	bool enable = plnFlag & PLN_ENABLE;
	SendDlgItemMessage(hTab, IDC_VH_PLN, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
	for (int i = IDC_VH_PLN_CELGRID; i <= IDC_VH_PLN_MKRLIST; i++) 
		EnableWindow(GetDlgItem(hTab, i), enable ? TRUE : FALSE);
	if (enable && !(plnFlag & IDC_VH_PLN_CNSTLABEL)) {
		for (int i = IDC_VH_PLN_CNSTLABEL_FULL; i <= IDC_VH_PLN_CNSTLABEL_SHORT; i++)
			EnableWindow(GetDlgItem(hTab, i), FALSE);
	}
	SendDlgItemMessage(hTab, IDC_VH_PLN_CELGRID,     BM_SETCHECK, plnFlag & PLN_CGRID     ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_PLN_ECLGRID,     BM_SETCHECK, plnFlag & PLN_EGRID     ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_PLN_GALGRID,     BM_SETCHECK, plnFlag & PLN_GGRID     ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_PLN_EQU,         BM_SETCHECK, plnFlag & PLN_EQU       ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_PLN_CNSTLABEL,   BM_SETCHECK, plnFlag & PLN_CNSTLABEL ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_PLN_CNSTBND,     BM_SETCHECK, plnFlag & PLN_CNSTBND   ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_PLN_CNSTPATTERN, BM_SETCHECK, plnFlag & PLN_CONST     ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_PLN_MARKER,      BM_SETCHECK, plnFlag & PLN_CCMARK    ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_PLN_CNSTLABEL_FULL,  BM_SETCHECK, plnFlag & PLN_CNSTLONG ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_PLN_CNSTLABEL_SHORT, BM_SETCHECK, plnFlag & PLN_CNSTLONG ? BST_UNCHECKED : BST_CHECKED, 0);
}

// ======================================================================

void TabPlanetarium::RescanMarkerList(HWND hTab)
{
	SendDlgItemMessage(hTab, IDC_VH_PLN_MKRLIST, LB_RESETCONTENT, 0, 0);

	const std::vector< oapi::GraphicsClient::LABELLIST>& list = g_psys->LabelList();
	if (!list.size()) return;

	char cbuf[256];
	_finddata_t fdata;
	intptr_t fh = g_psys->FindFirst(FILETYPE_MARKER, &fdata, cbuf);
	if (fh >= 0) {
		int n = 0;
		do {
			SendDlgItemMessage(hTab, IDC_VH_PLN_MKRLIST, LB_ADDSTRING, 0, (LPARAM)trim_string(cbuf));
			if (n < list.size() && list[n].active)
				SendDlgItemMessage(hTab, IDC_VH_PLN_MKRLIST, LB_SETSEL, TRUE, n);
			n++;
		} while (!g_psys->FindNext(fh, &fdata, cbuf));
		_findclose(fh);
	}
}

// ======================================================================

BOOL TabPlanetarium::OnInitDialog(HWND hTab, WPARAM wParam, LPARAM lParam)
{
	UpdateControls(hTab);
	RescanMarkerList(hTab);

	return TRUE;
}

// ======================================================================

BOOL TabPlanetarium::OnCommand(HWND hTab, WORD ctrlId, WORD notification, HWND hCtrl)
{
	switch (ctrlId) {
	case IDC_VH_PLN:
		if (notification == BN_CLICKED) {
			g_pOrbiter->TogglePlanetariumMode();
			return TRUE;
		}
		break;
	case IDC_VH_PLN_CELGRID:
	case IDC_VH_PLN_ECLGRID:
	case IDC_VH_PLN_GALGRID:
	case IDC_VH_PLN_EQU:
	case IDC_VH_PLN_CNSTLABEL:
	case IDC_VH_PLN_CNSTBND:
	case IDC_VH_PLN_CNSTPATTERN:
	case IDC_VH_PLN_MARKER:
	case IDC_VH_PLN_CNSTLABEL_FULL:
	case IDC_VH_PLN_CNSTLABEL_SHORT:
		if (notification == BN_CLICKED) {
			OnItemClicked(hTab, ctrlId);
			return TRUE;
		}
		break;
	case IDC_VH_PLN_MKRLIST:
		if (notification == LBN_SELCHANGE)
			return OnMarkerSelectionChanged(hTab);
		break;
	}
	return FALSE;
}

// ======================================================================

void TabPlanetarium::OnItemClicked(HWND hTab, WORD ctrlId)
{
	bool check = (SendDlgItemMessage(hTab, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
	DWORD flag;
	switch (ctrlId) {
	case IDC_VH_PLN:                 flag = PLN_ENABLE;    break;
	case IDC_VH_PLN_CELGRID:         flag = PLN_CGRID;     break;
	case IDC_VH_PLN_ECLGRID:         flag = PLN_EGRID;     break;
	case IDC_VH_PLN_GALGRID:         flag = PLN_GGRID;     break;
	case IDC_VH_PLN_EQU:             flag = PLN_EQU;       break;
	case IDC_VH_PLN_CNSTLABEL:       flag = PLN_CNSTLABEL; break;
	case IDC_VH_PLN_CNSTBND:         flag = PLN_CNSTBND;   break;
	case IDC_VH_PLN_CNSTPATTERN:     flag = PLN_CONST;     break;
	case IDC_VH_PLN_MARKER:          flag = PLN_CCMARK;    break;
	case IDC_VH_PLN_CNSTLABEL_FULL:  flag = PLN_CNSTLONG;  break;
	case IDC_VH_PLN_CNSTLABEL_SHORT: flag = PLN_CNSTLONG; check = !check; break;
	default:                         flag = 0;             break;
	}
	DWORD& plnFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagPlanetarium;
	if (check) plnFlag |=  flag;
	else       plnFlag &= ~flag;

	UpdateControls(hTab);
}

// ======================================================================

BOOL TabPlanetarium::OnMarkerSelectionChanged(HWND hTab)
{
	std::vector<oapi::GraphicsClient::LABELLIST>& list = g_psys->LabelList();
	if (list.size()) {
		for (int i = 0; i < list.size(); i++) {
			int sel = SendDlgItemMessage(hTab, IDC_VH_PLN_MKRLIST, LB_GETSEL, i, 0);
			list[i].active = (sel ? true : false);
		}

		std::ifstream cfg(g_pOrbiter->Cfg()->ConfigPath(g_psys->Name()));
		g_psys->ScanLabelLists(cfg);
	}

	return 0;
}

// ======================================================================

char *TabPlanetarium::HelpContext () const
{
	static char *context = "/vh_planetarium.htm";
	return context;
}


// ======================================================================
// ======================================================================

TabLabels::TabLabels(HWND hParentTab) : VhelperTab(hParentTab)
{
}

// ======================================================================

void TabLabels::CreateInterface()
{
	MakeTab(IDD_VHELP_LABELS);
}

// ======================================================================

void TabLabels::UpdateControls(HWND hTab)
{
	DWORD& plnFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagPlanetarium;
	bool enable = plnFlag & PLN_ENABLE;
	SendDlgItemMessage(hTab, IDC_VH_PLN, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
	for (int i = IDC_VH_MKR_VESSEL; i <= IDC_VH_MKR_FEATURELIST; i++)
		EnableWindow(GetDlgItem(hTab, i), enable ? TRUE : FALSE);
	SendDlgItemMessage(hTab, IDC_VH_MKR_VESSEL,   BM_SETCHECK, plnFlag & PLN_VMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_MKR_CELBODY,  BM_SETCHECK, plnFlag & PLN_CMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_MKR_BASE,     BM_SETCHECK, plnFlag & PLN_BMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_MKR_BEACON,   BM_SETCHECK, plnFlag & PLN_RMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_MKR_FEATURES, BM_SETCHECK, plnFlag & PLN_LMARK ? BST_CHECKED : BST_UNCHECKED, 0);
}

// ======================================================================

char* TabLabels::HelpContext() const
{
	static char* context = "/vh_planetarium.htm"; // fix
	return context;
}

// ======================================================================

BOOL TabLabels::OnInitDialog(HWND hTab, WPARAM wParam, LPARAM lParam)
{
	UpdateControls(hTab);
	ScanPsysBodies(hTab);

	return TRUE;
}

// ======================================================================

BOOL TabLabels::OnCommand(HWND hTab, WORD ctrlId, WORD notification, HWND hCtrl)
{
	switch (ctrlId) {
	case IDC_VH_PLN:
		if (notification == BN_CLICKED) {
			g_pOrbiter->TogglePlanetariumMode();
			return TRUE;
		}
		break;
	case IDC_VH_MKR_VESSEL:
	case IDC_VH_MKR_CELBODY:
	case IDC_VH_MKR_BASE:
	case IDC_VH_MKR_BEACON:
	case IDC_VH_MKR_FEATURES:
		if (notification == BN_CLICKED) {
			OnItemClicked(hTab, ctrlId);
			return TRUE;
		}
		break;
	case IDC_VH_MKR_FEATUREBODY:
		if (notification == CBN_SELCHANGE)
			UpdateFeatureList(hTab);
		return TRUE;
	case IDC_VH_MKR_FEATURELIST:
		if (notification == LBN_SELCHANGE)
			RescanFeatures(hTab);
		return TRUE;
	}
	return FALSE;
}

// ======================================================================

void TabLabels::OnItemClicked(HWND hTab, WORD ctrlId)
{
	bool check = (SendDlgItemMessage(hTab, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
	DWORD flag;
	switch (ctrlId) {
	case IDC_VH_PLN:                 flag = PLN_ENABLE;    break;
	case IDC_VH_MKR_VESSEL:          flag = PLN_VMARK;     break;
	case IDC_VH_MKR_CELBODY:         flag = PLN_CMARK;     break;
	case IDC_VH_MKR_BASE:            flag = PLN_BMARK;     break;
	case IDC_VH_MKR_BEACON:          flag = PLN_RMARK;     break;
	case IDC_VH_MKR_FEATURES:        flag = PLN_LMARK;     break;
	default:                         flag = 0;             break;
	}
	DWORD& plnFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagPlanetarium;
	if (check) plnFlag |=  flag;
	else       plnFlag &= ~flag;

	if (ctrlId == IDC_VH_MKR_FEATURES)
		g_psys->ActivatePlanetLabels(plnFlag & PLN_ENABLE && plnFlag & PLN_LMARK);

	UpdateControls(hTab);
}

// ======================================================================

void TabLabels::ScanPsysBodies(HWND hTab)
{
	const Body* sel = nullptr;
	for (int i = 0; i < g_psys->nPlanet(); i++) {
		Planet* planet = g_psys->GetPlanet(i);
		if (planet == g_camera->Target())
			sel = planet;
		if (planet->isMoon())
			continue;
		SendDlgItemMessage(hTab, IDC_VH_MKR_FEATUREBODY, CB_ADDSTRING, 0, (LPARAM)planet->Name());
		for (int j = 0; j < planet->nSecondary(); j++) {
			char cbuf[256] = "    ";
			strncpy(cbuf + 4, planet->Secondary(j)->Name(), 252);
			SendDlgItemMessage(hTab, IDC_VH_MKR_FEATUREBODY, CB_ADDSTRING, 0, (LPARAM)cbuf);
		}
	}
	if (!sel) {
		Body* tgt = g_camera->Target();
		if (tgt->Type() == OBJTP_VESSEL)
			sel = ((Vessel*)tgt)->GetSurfParam()->ref;
	}
	int idx = (sel ? SendDlgItemMessage(hTab, IDC_VH_MKR_FEATUREBODY, CB_FINDSTRINGEXACT, -1, (LPARAM)sel->Name()) : 0);
	SendDlgItemMessage(hTab, IDC_VH_MKR_FEATUREBODY, CB_SETCURSEL, idx, 0);
	UpdateFeatureList(hTab);
}

// ======================================================================

void TabLabels::UpdateFeatureList(HWND hTab)
{
	int n, nlist;
	char cbuf[256], cpath[256];
	int idx = SendDlgItemMessage(hTab, IDC_VH_MKR_FEATUREBODY, CB_GETCURSEL, 0, 0);
	SendDlgItemMessage(hTab, IDC_VH_MKR_FEATUREBODY, CB_GETLBTEXT, idx, (LPARAM)cbuf);
	SendDlgItemMessage(hTab, IDC_VH_MKR_FEATURELIST, LB_RESETCONTENT, 0, 0);
	Planet* planet = g_psys->GetPlanet(trim_string(cbuf), true);
	if (!planet) return;

	if (planet->LabelFormat() < 2) {
		oapi::GraphicsClient::LABELLIST* list = planet->LabelList(&nlist);
		if (!nlist) return;
		_finddata_t fdata;
		long fh = planet->FindFirst(FILETYPE_MARKER, &fdata, cpath, cbuf);
		if (fh >= 0) {
			n = 0;
			do {
				SendDlgItemMessage(hTab, IDC_VH_MKR_FEATURELIST, LB_ADDSTRING, 0, (LPARAM)trim_string(cbuf));
				if (n < nlist && list[n].active)
					SendDlgItemMessage(hTab, IDC_VH_MKR_FEATURELIST, LB_SETSEL, TRUE, n);
				n++;
			} while (!planet->FindNext(fh, &fdata, cbuf));
			_findclose(fh);
		}
	}
	else {
		int nlabel = planet->NumLabelLegend();
		if (nlabel) {
			const oapi::GraphicsClient::LABELTYPE* lspec = planet->LabelLegend();
			for (int i = 0; i < nlabel; i++) {
				SendDlgItemMessage(hTab, IDC_VH_MKR_FEATURELIST, LB_ADDSTRING, 0, (LPARAM)lspec[i].name);
				if (lspec[i].active)
					SendDlgItemMessage(hTab, IDC_VH_MKR_FEATURELIST, LB_SETSEL, TRUE, i);
			}
		}
	}
}

// ======================================================================

void TabLabels::RescanFeatures(HWND hTab)
{
	char cbuf[256];
	int nlist;

	int idx = SendDlgItemMessage(hTab, IDC_VH_MKR_FEATUREBODY, CB_GETCURSEL, 0, 0);
	SendDlgItemMessage(hTab, IDC_VH_MKR_FEATUREBODY, CB_GETLBTEXT, idx, (LPARAM)cbuf);
	Planet* planet = g_psys->GetPlanet(trim_string(cbuf), true);
	if (!planet) return;

	if (planet->LabelFormat() < 2) {
		oapi::GraphicsClient::LABELLIST* list = planet->LabelList(&nlist);
		if (!nlist) return;

		for (int i = 0; i < nlist; i++) {
			BOOL sel = SendDlgItemMessage(hTab, IDC_VH_MKR_FEATURELIST, LB_GETSEL, i, 0);
			list[i].active = (sel ? true : false);
		}

		std::ifstream cfg(g_pOrbiter->Cfg()->ConfigPath(planet->Name()));
		planet->ScanLabelLists(cfg);
	}
	else {
		nlist = planet->NumLabelLegend();
		for (int i = 0; i < nlist; i++) {
			BOOL sel = SendDlgItemMessage(hTab, IDC_VH_MKR_FEATURELIST, LB_GETSEL, i, 0);
			planet->SetLabelActive(i, sel ? true : false);
		}
	}
}


// ======================================================================
// ======================================================================

TabForces::TabForces (HWND hParentTab): VhelperTab (hParentTab)
{
}

// ======================================================================

void TabForces::CreateInterface()
{
	MakeTab(IDD_VHELP_BODYFORCE);
}

// ======================================================================

void TabForces::UpdateControls(HWND hTab)
{
	DWORD vecFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagBodyforce;
	bool enable = (vecFlag & BF_ENABLE);
	SendDlgItemMessage(hTab, IDC_VH_VEC, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
	for (int i = IDC_VH_VEC_WEIGHT; i <= IDC_VH_VEC_OPACITY; i++)
		EnableWindow(GetDlgItem(hTab, i), enable ? TRUE : FALSE);
	SendDlgItemMessage(hTab, IDC_VH_VEC_WEIGHT, BM_SETCHECK, vecFlag & BF_WEIGHT   ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_VEC_THRUST, BM_SETCHECK, vecFlag & BF_THRUST   ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_VEC_LIFT,   BM_SETCHECK, vecFlag & BF_LIFT     ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_VEC_DRAG,   BM_SETCHECK, vecFlag & BF_DRAG     ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_VEC_TOTAL,  BM_SETCHECK, vecFlag & BF_TOTAL    ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_VEC_TORQUE, BM_SETCHECK, vecFlag & BF_TORQUE   ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_VEC_LINSCL, BM_SETCHECK, vecFlag & BF_LOGSCALE ? BST_UNCHECKED : BST_CHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_VEC_LOGSCL, BM_SETCHECK, vecFlag & BF_LOGSCALE ? BST_CHECKED : BST_UNCHECKED, 0);

	int scalePos = (int)(25.0 * (1.0 + 0.5 * log(g_pOrbiter->Cfg()->CfgVisHelpPrm.scaleBodyforce) / log(2.0)));
	oapiSetGaugePos(GetDlgItem(hTab, IDC_VH_VEC_SCALE), scalePos);
	int opacPos = (int)(g_pOrbiter->Cfg()->CfgVisHelpPrm.opacBodyforce * 50.0);
	oapiSetGaugePos(GetDlgItem(hTab, IDC_VH_VEC_OPACITY), opacPos);
}

// ======================================================================

char *TabForces::HelpContext () const
{
	static char *context = "/vh_force.htm";
	return context;
}

// ======================================================================

BOOL TabForces::OnInitDialog(HWND hTab, WPARAM wParam, LPARAM lParam)
{
	GAUGEPARAM gp = { 0, 50, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
	oapiSetGaugeParams(GetDlgItem(hTab, IDC_VH_VEC_SCALE), &gp);
	oapiSetGaugeParams(GetDlgItem(hTab, IDC_VH_VEC_OPACITY), &gp);

	UpdateControls(hTab);

	return TRUE;
}

// ======================================================================

BOOL TabForces::OnCommand(HWND hTab, WORD ctrlId, WORD notification, HWND hCtrl)
{
	switch (ctrlId) {
	case IDC_VH_VEC:
	case IDC_VH_VEC_WEIGHT:
	case IDC_VH_VEC_THRUST:
	case IDC_VH_VEC_LIFT:
	case IDC_VH_VEC_DRAG:
	case IDC_VH_VEC_TOTAL:
	case IDC_VH_VEC_TORQUE:
	case IDC_VH_VEC_LINSCL:
	case IDC_VH_VEC_LOGSCL:
		if (notification == BN_CLICKED) {
			OnItemClicked(hTab, ctrlId);
			return FALSE;
		}
		break;
	}
	return FALSE;
}

// ======================================================================

void TabForces::OnItemClicked(HWND hTab, WORD ctrlId)
{
	bool check = (SendDlgItemMessage(hTab, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
	DWORD flag;
	switch (ctrlId) {
	case IDC_VH_VEC:        flag = BF_ENABLE;  break;
	case IDC_VH_VEC_WEIGHT: flag = BF_WEIGHT;  break;
	case IDC_VH_VEC_THRUST: flag = BF_THRUST;  break;
	case IDC_VH_VEC_LIFT:   flag = BF_LIFT;    break;
	case IDC_VH_VEC_DRAG:   flag = BF_DRAG;    break;
	case IDC_VH_VEC_TOTAL:  flag = BF_TOTAL;   break;
	case IDC_VH_VEC_TORQUE: flag = BF_TORQUE;  break;
	case IDC_VH_VEC_LINSCL: flag = BF_LOGSCALE; check = false; break;
	case IDC_VH_VEC_LOGSCL: flag = BF_LOGSCALE; check = true;  break;
	default:                flag = 0;          break;
	}
	DWORD& vecFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagBodyforce;
	if (check) vecFlag |=  flag;
	else       vecFlag &= ~flag;

	UpdateControls(hTab);
}

// ======================================================================

BOOL TabForces::OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam)
{
	switch (GetDlgCtrlID((HWND)lParam)) {
	case IDC_VH_VEC_SCALE:
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			g_pOrbiter->Cfg()->CfgVisHelpPrm.scaleBodyforce = (float)pow(2.0, (HIWORD(wParam) - 25) * 0.08);
			return 0;
		}
		break;
	case IDC_VH_VEC_OPACITY:
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			g_pOrbiter->Cfg()->CfgVisHelpPrm.opacBodyforce = (float)(HIWORD(wParam) * 0.02);
			return 0;
		}
		break;
	}
	return FALSE;
}


// ======================================================================
// ======================================================================

TabAxes::TabAxes (HWND hParentTab): VhelperTab (hParentTab)
{
}

// ======================================================================

void TabAxes::CreateInterface()
{
	MakeTab(IDD_VHELP_COORDINATES);
}

// ======================================================================

void TabAxes::UpdateControls(HWND hTab)
{
	DWORD crdFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagCrdAxes;
	bool enable = (crdFlag & CA_ENABLE);
	SendDlgItemMessage(hTab, IDC_VH_CRD, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
	for (int i = IDC_VH_CRD_VESSEL; i <= IDC_VH_CRD_OPACITY; i++)
		EnableWindow(GetDlgItem(hTab, i), enable ? TRUE : FALSE);
	SendDlgItemMessage(hTab, IDC_VH_CRD_VESSEL,  BM_SETCHECK,  crdFlag & CA_VESSEL ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_CRD_CELBODY, BM_SETCHECK,  crdFlag & CA_CBODY  ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_CRD_BASE, BM_SETCHECK,     crdFlag & CA_BASE   ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hTab, IDC_VH_CRD_NEGATIVE, BM_SETCHECK, crdFlag & CA_NEG    ? BST_CHECKED : BST_UNCHECKED, 0);

	int scalePos = (int)(25.0 * (1.0 + 0.5 * log(g_pOrbiter->Cfg()->CfgVisHelpPrm.scaleCrdAxes) / log(2.0)));
	oapiSetGaugePos(GetDlgItem(hTab, IDC_VH_CRD_SCALE), scalePos);
	int opacPos = (int)(g_pOrbiter->Cfg()->CfgVisHelpPrm.opacCrdAxes * 50.0);
	oapiSetGaugePos(GetDlgItem(hTab, IDC_VH_CRD_OPACITY), opacPos);
}

// ======================================================================

char *TabAxes::HelpContext () const
{
	static char *context = "/vh_coord.htm";
	return context;
}

// ======================================================================

BOOL TabAxes::OnInitDialog(HWND hTab, WPARAM wParam, LPARAM lParam)
{
	GAUGEPARAM gp = { 0, 50, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
	oapiSetGaugeParams(GetDlgItem(hTab, IDC_VH_CRD_SCALE), &gp);
	oapiSetGaugeParams(GetDlgItem(hTab, IDC_VH_CRD_OPACITY), &gp);

	UpdateControls(hTab);

	return TRUE;
}

// ======================================================================

BOOL TabAxes::OnCommand(HWND hTab, WORD ctrlId, WORD notification, HWND hCtrl)
{
	switch (ctrlId) {
	case IDC_VH_CRD:
	case IDC_VH_CRD_VESSEL:
	case IDC_VH_CRD_CELBODY:
	case IDC_VH_CRD_BASE:
	case IDC_VH_CRD_NEGATIVE:
		if (notification == BN_CLICKED) {
			OnItemClicked(hTab, ctrlId);
			return FALSE;
		}
		break;
	}
	return FALSE;
}

// ======================================================================

void TabAxes::OnItemClicked(HWND hTab, WORD ctrlId)
{
	bool check = (SendDlgItemMessage(hTab, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
	DWORD flag;
	switch (ctrlId) {
	case IDC_VH_CRD:          flag = CA_ENABLE; break;
	case IDC_VH_CRD_VESSEL:   flag = CA_VESSEL; break;
	case IDC_VH_CRD_CELBODY:  flag = CA_CBODY;  break;
	case IDC_VH_CRD_BASE:     flag = CA_BASE;   break;
	case IDC_VH_CRD_NEGATIVE: flag = CA_NEG;    break;
	default:                  flag = 0;         break;
	}
	DWORD& crdFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagCrdAxes;
	if (check) crdFlag |=  flag;
	else       crdFlag &= ~flag;

	UpdateControls(hTab);
}

// ======================================================================

BOOL TabAxes::OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam)
{
	switch (GetDlgCtrlID((HWND)lParam)) {
	case IDC_VH_CRD_SCALE:
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			g_pOrbiter->Cfg()->CfgVisHelpPrm.scaleCrdAxes = (float)pow(2.0, (HIWORD(wParam) - 25) * 0.08);
			return 0;
		}
		break;
	case IDC_VH_CRD_OPACITY:
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			g_pOrbiter->Cfg()->CfgVisHelpPrm.opacCrdAxes = (float)(HIWORD(wParam) * 0.02);
			return 0;
		}
		break;
	}
	return FALSE;
}
