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
	nTab = 0;
	hcontext = 0;
	pos = &g_pOrbiter->Cfg()->CfgWindowPos.DlgCamera;
}

// ======================================================================

DlgVishelper::~DlgVishelper ()
{
	Clear ();
}

// ======================================================================

void DlgVishelper::Update ()
{
	for (int i = 0; i < nTab; i++)
		pTab[i]->Update ();
}

// ======================================================================

int DlgVishelper::AddTab (HWND hDlg, VhelperTab *tab, const char *label)
{
	char cbuf[256];
	strcpy (cbuf, label);
	TC_ITEM tie;
	tie.mask = TCIF_TEXT;
	tie.iImage = -1;
	tie.pszText = cbuf;
	SendDlgItemMessage (hDlg, IDC_TAB1, TCM_INSERTITEM, nTab, (LPARAM)&tie);

	VhelperTab **tmp = new VhelperTab*[nTab+1];
	if (nTab) {
		memcpy (tmp, pTab, nTab*sizeof(VhelperTab*));
		delete []pTab;
	}
	pTab = tmp;
	pTab[nTab] = tab;
	return nTab++;
}

// ======================================================================
// Display new tab page

void DlgVishelper::SwitchTab (HWND hDlg)
{
	int pg, cpg = TabCtrl_GetCurSel (GetDlgItem (hDlg, IDC_TAB1));
	for (pg = 0; pg < nTab; pg++)
		if (pg != cpg) pTab[pg]->Show (false);
	pTab[cpg]->Show (true);
	hcontext = pTab[cpg]->HelpContext();
}

// ======================================================================

void DlgVishelper::Clear ()
{
	if (nTab) {
		for (int i = 0; i < nTab; i++)
			delete pTab[i];
		delete []pTab;
		nTab = 0;
	}
}

// ======================================================================

BOOL DlgVishelper::OnInitDialog (HWND hDlg, WPARAM wParam, LPARAM lParam)
{
	HWND hTabFrame = hDlg;

	AddTab (hDlg, new TabPlanetarium (hTabFrame), "Planetarium");
	AddTab (hDlg, new TabForces (hTabFrame), "Forces");
	AddTab (hDlg, new TabAxes (hTabFrame), "Axes");

	SwitchTab (hDlg);
	return TRUE;
}

// ======================================================================

BOOL DlgVishelper::OnCommand (HWND hDlg, WORD id, WORD code, HWND hControl)
{
	switch (id) {
	case IDHELP:
		DefHelpContext.topic = hcontext;
		g_pOrbiter->OpenHelp (&DefHelpContext);
		return TRUE;
	}
	return DialogWin::OnCommand (hDlg, id, code, hControl);
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

VhelperTab::VhelperTab (HWND hParentTab, int dlgId, DLGPROC dlgProc)
{
	active = false;
	hParent = hParentTab;
	hTab = CreateDialogParam (g_pOrbiter->GetInstance(), MAKEINTRESOURCE(dlgId), hParentTab, dlgProc, (LPARAM)this);
}

// ======================================================================

void VhelperTab::Show (bool show)
{
	ShowWindow (hTab, show ? SW_SHOW : SW_HIDE);
	active = show;
}

// ======================================================================

LRESULT CALLBACK VhelperTab::DlgProcInit (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		EnableThemeDialogTexture (hWnd, ETDT_ENABLETAB);
		SetWindowLongPtr (hWnd, DWLP_USER, lParam);
		return TRUE;
	}
	return FALSE;
}


// ======================================================================
// ======================================================================

TabPlanetarium::TabPlanetarium (HWND hParentTab): VhelperTab (hParentTab, IDD_VHELP_PLANETARIUM, DlgProc)
{
}

// ======================================================================

void TabPlanetarium::Update ()
{
	Refresh (hTab);
}

// ======================================================================

void TabPlanetarium::Refresh (HWND hDlg)
{
	int i;
	bool enable = g_pOrbiter->Cfg()->PlanetariumItem (IDC_PLANETARIUM);
	SendDlgItemMessage (hDlg, IDC_PLANETARIUM, BM_SETCHECK, enable ? BST_CHECKED:BST_UNCHECKED, 0);
	for (i = IDC_PLN_CELGRID; i <= IDC_PLN_SHORT; i++)
		EnableWindow (GetDlgItem (hDlg, i), enable ? TRUE:FALSE);
	if (enable && !g_pOrbiter->Cfg()->PlanetariumItem (IDC_PLN_CNSTLABEL)) {
		for (i = IDC_PLN_FULL; i <= IDC_PLN_SHORT; i++)
			EnableWindow (GetDlgItem (hDlg, i), FALSE);
	}
}

// ======================================================================

char *TabPlanetarium::HelpContext () const
{
	static char *context = "/vh_planetarium.htm";
	return context;
}

// ======================================================================

LRESULT CALLBACK TabPlanetarium::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	VhelperTab::DlgProcInit (hWnd, uMsg, wParam, lParam);
	TabPlanetarium *pTab = (TabPlanetarium*)(uMsg == WM_INITDIALOG ? lParam : GetWindowLongPtr(hWnd,DWLP_USER));

	switch (uMsg) {
	case WM_INITDIALOG: {
		pTab->Refresh (hWnd);
		for (int i = IDC_PLN_CELGRID; i <= IDC_PLN_SHORT; i++)
			SendDlgItemMessage (hWnd, i, BM_SETCHECK, g_pOrbiter->Cfg()->PlanetariumItem (i) ? BST_CHECKED:BST_UNCHECKED, 0);
		} return TRUE;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_PLANETARIUM:
		case IDC_PLN_CELGRID:
		case IDC_PLN_ECLGRID:
		case IDC_PLN_ECLIPTIC:
		case IDC_PLN_EQUATOR:
		case IDC_PLN_CONST:
		case IDC_PLN_CNSTLABEL:
		case IDC_PLN_CMARKER:
		case IDC_PLN_VMARKER:
		case IDC_PLN_BMARKER:
		case IDC_PLN_RMARKER:
		case IDC_PLN_LMARKER:
		case IDC_PLN_CCMARKER:
			if (HIWORD(wParam) == BN_CLICKED) {
				bool check = (SendDlgItemMessage (hWnd, LOWORD(wParam), BM_GETCHECK, 0, 0) == TRUE);
				g_pOrbiter->Cfg()->SetPlanetariumItem (LOWORD(wParam), check);
				pTab->Refresh (hWnd);
				if (LOWORD(wParam) == IDC_PLANETARIUM || LOWORD(wParam) == IDC_PLN_LMARKER)
					g_psys->ActivatePlanetLabels(g_pOrbiter->Cfg()->PlanetariumItem(IDC_PLANETARIUM) && g_pOrbiter->Cfg()->PlanetariumItem(IDC_PLN_LMARKER));
				return TRUE;
			}
			break;
		case IDC_PLN_FULL:
		case IDC_PLN_SHORT:
			if (HIWORD(wParam) == BN_CLICKED) {
				bool check = (SendDlgItemMessage (hWnd, LOWORD(wParam), BM_GETCHECK, 0, 0) == TRUE);
				g_pOrbiter->Cfg()->SetPlanetariumItem (LOWORD(wParam), check);
				g_pOrbiter->Cfg()->SetPlanetariumItem (LOWORD(wParam) == IDC_PLN_FULL ? IDC_PLN_SHORT : IDC_PLN_FULL, !check);
				return TRUE;
			}
			break;
		case IDC_PLN_CONFIG:
			g_pOrbiter->DlgMgr()->EnsureEntry<DlgCustomLabels> ();
			break;
		case IDC_PLN_CONFIG2:
			g_pOrbiter->DlgMgr()->EnsureEntry<DlgCustomCLabels> ();
			break;
		}
		break;
	}
	return FALSE;
}


// ======================================================================
// ======================================================================

TabForces::TabForces (HWND hParentTab): VhelperTab (hParentTab, IDD_VHELP_BODYFORCE, DlgProc)
{
}

// ======================================================================

void TabForces::Update ()
{
	Refresh (hTab, true);
}

// ======================================================================

void TabForces::Refresh (HWND hDlg, bool tick)
{
	int i;
	DWORD flag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagBodyforce;
	bool active = (flag & BF_ENABLE);
	SendDlgItemMessage (hDlg, IDC_BODYFORCE, BM_SETCHECK, active ? BST_CHECKED:BST_UNCHECKED, 0);
	if (tick) {
		SendDlgItemMessage (hDlg, IDC_WEIGHT,   BM_SETCHECK, (flag & BF_WEIGHT)   ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hDlg, IDC_THRUST,   BM_SETCHECK, (flag & BF_THRUST)   ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hDlg, IDC_LIFT,     BM_SETCHECK, (flag & BF_LIFT)     ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hDlg, IDC_DRAG,     BM_SETCHECK, (flag & BF_DRAG)     ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hDlg, IDC_TOTAL,    BM_SETCHECK, (flag & BF_TOTAL)    ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hDlg, IDC_TORQUE,   BM_SETCHECK, (flag & BF_TORQUE)   ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hDlg, IDC_LINSCALE, BM_SETCHECK, (flag & BF_LOGSCALE) ? BST_UNCHECKED:BST_CHECKED, 0);
		SendDlgItemMessage (hDlg, IDC_LOGSCALE, BM_SETCHECK, (flag & BF_LOGSCALE) ? BST_CHECKED:BST_UNCHECKED, 0);
	}
	for (i = IDC_WEIGHT; i <= IDC_LOGSCALE; i++)
		EnableWindow (GetDlgItem (hDlg, i), active ? TRUE:FALSE);
}


// ======================================================================

char *TabForces::HelpContext () const
{
	static char *context = "/vh_force.htm";
	return context;
}

// ======================================================================

LRESULT CALLBACK TabForces::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	VhelperTab::DlgProcInit (hWnd, uMsg, wParam, lParam);
	TabForces *pTab = (TabForces*)(uMsg == WM_INITDIALOG ? lParam : GetWindowLongPtr(hWnd,DWLP_USER));

	switch (uMsg) {
	case WM_INITDIALOG: {
		GAUGEPARAM gp = { 0, 50, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
		oapiSetGaugeParams (GetDlgItem (hWnd, IDC_SCALE), &gp);
		int scl = (int)(25.0*(1.0+0.5*log(g_pOrbiter->Cfg()->CfgVisHelpPrm.scaleBodyforce)/log(2.0)));
		oapiSetGaugePos (GetDlgItem (hWnd, IDC_SCALE), scl);
		oapiSetGaugeParams (GetDlgItem (hWnd, IDC_OPACITY), &gp);
		scl = (int)(g_pOrbiter->Cfg()->CfgVisHelpPrm.opacBodyforce * 50.0);
		oapiSetGaugePos (GetDlgItem (hWnd, IDC_OPACITY), scl);
		pTab->Refresh (hWnd, true);
		} return TRUE;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_BODYFORCE:
		case IDC_WEIGHT:
		case IDC_THRUST:
		case IDC_LIFT:
		case IDC_DRAG:
		case IDC_TOTAL:
		case IDC_TORQUE:
		case IDC_LINSCALE:
		case IDC_LOGSCALE:
			if (HIWORD(wParam) == BN_CLICKED) {
				bool check = (SendDlgItemMessage (hWnd, LOWORD(wParam), BM_GETCHECK, 0, 0) == TRUE);
				g_pOrbiter->Cfg()->SetBodyforceItem (LOWORD(wParam), check);
				pTab->Refresh (hWnd, false);
				return TRUE;
			}
			break;
		}
		break;
	case WM_HSCROLL:
		switch (GetDlgCtrlID ((HWND)lParam)) {
		case IDC_SCALE:
			switch (LOWORD(wParam)) {
			case SB_THUMBTRACK:
			case SB_LINELEFT:
			case SB_LINERIGHT:
				g_pOrbiter->Cfg()->CfgVisHelpPrm.scaleBodyforce = (float)pow (2.0, (HIWORD(wParam)-25)*0.08);
				return 0;
			}
			break;
		case IDC_OPACITY:
			switch (LOWORD(wParam)) {
			case SB_THUMBTRACK:
			case SB_LINELEFT:
			case SB_LINERIGHT:
				g_pOrbiter->Cfg()->CfgVisHelpPrm.opacBodyforce = (float)(HIWORD(wParam)*0.02);
				return 0;
			}
			break;
		}
		break;
	}
	return FALSE;
}


// ======================================================================
// ======================================================================

TabAxes::TabAxes (HWND hParentTab): VhelperTab (hParentTab, IDD_VHELP_COORDINATES, DlgProc)
{
}

// ======================================================================

void TabAxes::Update ()
{
	Refresh (hTab, true);
}

// ======================================================================
// Coordinate axes tab: Refresh

void TabAxes::Refresh (HWND hDlg, bool tick)
{
	DWORD flag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagCrdAxes;
	bool active = (flag & CA_ENABLE);
	SendDlgItemMessage (hDlg, IDC_COORDINATES, BM_SETCHECK, active ? BST_CHECKED:BST_UNCHECKED, 0);
	if (tick) {
		SendDlgItemMessage (hDlg, IDC_CRD_VESSEL,   BM_SETCHECK, (flag & CA_VESSEL) ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hDlg, IDC_CRD_CBODY,    BM_SETCHECK, (flag & CA_CBODY)  ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hDlg, IDC_CRD_BASE,     BM_SETCHECK, (flag & CA_BASE)   ? BST_CHECKED:BST_UNCHECKED, 0);
		SendDlgItemMessage (hDlg, IDC_CRD_NEGATIVE, BM_SETCHECK, (flag & CA_NEG)    ? BST_CHECKED:BST_UNCHECKED, 0);
	}
	for (int i = IDC_CRD_VESSEL; i <= IDC_CRD_OPAC; i++)
		EnableWindow (GetDlgItem (hDlg, i), active ? TRUE:FALSE);
}

// ======================================================================

char *TabAxes::HelpContext () const
{
	static char *context = "/vh_coord.htm";
	return context;
}

// ======================================================================

LRESULT CALLBACK TabAxes::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	VhelperTab::DlgProcInit (hWnd, uMsg, wParam, lParam);
	TabAxes *pTab = (TabAxes*)(uMsg == WM_INITDIALOG ? lParam : GetWindowLongPtr(hWnd,DWLP_USER));

	switch (uMsg) {
	case WM_INITDIALOG: {
		GAUGEPARAM gp = {0, 50, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
		oapiSetGaugeParams (GetDlgItem (hWnd, IDC_CRD_SCALE), &gp);
		int scl = (int)(25.0*(1.0+0.5*log(g_pOrbiter->Cfg()->CfgVisHelpPrm.scaleCrdAxes)/log(2.0)));
		oapiSetGaugePos (GetDlgItem (hWnd, IDC_CRD_SCALE), scl);
		oapiSetGaugeParams (GetDlgItem (hWnd, IDC_CRD_OPAC), &gp);
		scl = (int)(g_pOrbiter->Cfg()->CfgVisHelpPrm.opacCrdAxes * 50.0);
		oapiSetGaugePos (GetDlgItem (hWnd, IDC_CRD_OPAC), scl);
		pTab->Refresh (hWnd, true);
		} return TRUE;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_COORDINATES:
		case IDC_CRD_VESSEL:
		case IDC_CRD_CBODY:
		case IDC_CRD_BASE:
		case IDC_CRD_NEGATIVE:
			if (HIWORD(wParam) == BN_CLICKED) {
				bool check = (SendDlgItemMessage (hWnd, LOWORD(wParam), BM_GETCHECK, 0, 0) == TRUE);
				g_pOrbiter->Cfg()->SetCoordinateAxesItem (LOWORD(wParam), check);
				pTab->Refresh (hWnd, false);
				return TRUE;
			}
			break;
		}
		break;
	case WM_HSCROLL:
		switch (GetDlgCtrlID ((HWND)lParam)) {
		case IDC_CRD_SCALE:
			switch (LOWORD(wParam)) {
			case SB_THUMBTRACK:
			case SB_LINELEFT:
			case SB_LINERIGHT:
				g_pOrbiter->Cfg()->CfgVisHelpPrm.scaleCrdAxes = (float)pow (2.0, (HIWORD(wParam)-25)*0.08);
				return 0;
			}
			break;
		case IDC_CRD_OPAC:
			switch (LOWORD(wParam)) {
			case SB_THUMBTRACK:
			case SB_LINELEFT:
			case SB_LINERIGHT:
				g_pOrbiter->Cfg()->CfgVisHelpPrm.opacCrdAxes = (float)(HIWORD(wParam)*0.02);
				return 0;
			}
			break;
		}
		break;
	}
	return FALSE;
}

// ======================================================================
// ======================================================================

DlgCustomLabels::DlgCustomLabels (HINSTANCE hInstance, HWND hParent, void *context)
: DialogWin (hInstance, hParent, IDD_CUSTOMLABELS, 0, 0, context)
{
}

// ======================================================================

void DlgCustomLabels::Refresh (HWND hDlg)
{
	int i, n, nlist;
	char cbuf[256], cpath[256];

	i = SendDlgItemMessage (hDlg, IDC_CLBL_OBJECT, CB_GETCURSEL, 0, 0);
	SendDlgItemMessage (hDlg, IDC_CLBL_OBJECT, CB_GETLBTEXT, i, (LPARAM)cbuf);
	SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_RESETCONTENT, 0, 0);
	Planet *planet = g_psys->GetPlanet (cbuf, true);
	if (!planet) return;

	if (planet->LabelFormat() < 2) {
		oapi::GraphicsClient::LABELLIST *list = planet->LabelList (&nlist);
		if (!nlist) return;

		_finddata_t fdata;
		long fh = planet->FindFirst (FILETYPE_MARKER, &fdata, cpath, cbuf);
		if (fh >= 0) {
			n = 0;
			do {
				SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_ADDSTRING, 0, (LPARAM)trim_string(cbuf));
				if (n < nlist && list[n].active)
					SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_SETSEL, TRUE, n);
				n++;
			} while (!planet->FindNext (fh, &fdata, cbuf));
			_findclose (fh);
		}
	} else {
		int nlabel = planet->NumLabelLegend();
		if (nlabel) {
			const oapi::GraphicsClient::LABELTYPE *lspec = planet->LabelLegend();
			for (i = 0; i < nlabel; i++) {
				SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_ADDSTRING, 0, (LPARAM)lspec[i].name);
				if (lspec[i].active)
					SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_SETSEL, TRUE, i);
			}
		}
	}
}

// ======================================================================

void DlgCustomLabels::Select (HWND hDlg)
{
	int i, sel, nlist;
	char cbuf[256];

	i = SendDlgItemMessage (hDlg, IDC_CLBL_OBJECT, CB_GETCURSEL, 0, 0);
	SendDlgItemMessage (hDlg, IDC_CLBL_OBJECT, CB_GETLBTEXT, i, (LPARAM)cbuf);
	Planet *planet = g_psys->GetPlanet (cbuf, true);
	if (!planet) return;

	if (planet->LabelFormat() < 2) {
		oapi::GraphicsClient::LABELLIST *list = planet->LabelList (&nlist);
		if (!nlist) return;

		for (i = 0; i < nlist; i++) {
			sel = SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_GETSEL, i, 0);
			list[i].active = (sel ? true : false);
		}

		std::ifstream cfg (g_pOrbiter->Cfg()->ConfigPath (planet->Name()));
		planet->ScanLabelLists (cfg);
	} else {
		nlist = planet->NumLabelLegend();
		for (i = 0; i < nlist; i++) {
			sel = SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_GETSEL, i, 0);
			planet->SetLabelActive(i, sel ? true : false);
		}
	}
}

// ======================================================================

void DlgCustomLabels::SelectAll(HWND hDlg, bool active)
{
	int i, nlist;
	char cbuf[256];

	i = SendDlgItemMessage (hDlg, IDC_CLBL_OBJECT, CB_GETCURSEL, 0, 0);
	SendDlgItemMessage (hDlg, IDC_CLBL_OBJECT, CB_GETLBTEXT, i, (LPARAM)cbuf);
	Planet *planet = g_psys->GetPlanet (cbuf, true);
	if (!planet) return;

	if (planet->LabelFormat() < 2) {
		oapi::GraphicsClient::LABELLIST *list = planet->LabelList (&nlist);
		if (!nlist) return;
		for (i = 0; i < nlist; i++) {
			SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_SETSEL, active ? TRUE:FALSE, i);
			list[i].active = active;
		}
	} else {
		nlist = planet->NumLabelLegend();
		for (i = 0; i < nlist; i++) {
			SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_SETSEL, active ? TRUE:FALSE, i);
			planet->SetLabelActive(i, active);
		}
	}
}

// ======================================================================

BOOL DlgCustomLabels::OnInitDialog (HWND hDlg, WPARAM wParam, LPARAM lParam)
{
	int i, j;
	const Body *sel = 0;
	for (i = 0; i < g_psys->nPlanet(); i++) {
		Planet *planet = g_psys->GetPlanet(i);
		if (planet == g_camera->Target()) sel = planet;
		if (planet->isMoon()) continue;
		SendDlgItemMessage (hDlg, IDC_CLBL_OBJECT, CB_ADDSTRING, 0, (LPARAM)planet->Name());
		for (j = 0; j < planet->nSecondary(); j++)
			SendDlgItemMessage (hDlg, IDC_CLBL_OBJECT, CB_ADDSTRING, 0, (LPARAM)planet->Secondary(j)->Name());
	}
	if (!sel) {
		Body *tgt = g_camera->Target();
		if (tgt->Type() == OBJTP_VESSEL)
			sel = ((Vessel*)tgt)->GetSurfParam()->ref;
	}
	i = (sel ? SendDlgItemMessage (hDlg, IDC_CLBL_OBJECT, CB_FINDSTRINGEXACT, -1, (LPARAM)sel->Name()) : 0);
	SendDlgItemMessage (hDlg, IDC_CLBL_OBJECT, CB_SETCURSEL, i, 0);
	Refresh (hDlg);
	return TRUE;
}

// ======================================================================

BOOL DlgCustomLabels::OnCommand (HWND hDlg, WORD id, WORD code, HWND hControl)
{
	switch (id) {
	case IDC_CLBL_OBJECT:
		if (code == CBN_SELCHANGE)
			Refresh (hDlg);
		return TRUE;
	case IDC_CLBL_LIST:
		if (code == LBN_SELCHANGE)
			Select (hDlg);
		return TRUE;
	case IDC_BUTTON1:
		SelectAll(hDlg, true);
		return TRUE;
	case IDC_BUTTON2:
		SelectAll(hDlg, false);
		return TRUE;
	}
	return DialogWin::OnCommand (hDlg, id, code, hControl);
}


// ======================================================================
// ======================================================================

DlgCustomCLabels::DlgCustomCLabels (HINSTANCE hInstance, HWND hParent, void *context)
: DialogWin (hInstance, hParent, IDD_CUSTOMCLABELS, 0, 0, context)
{
}

// ======================================================================

void DlgCustomCLabels::Refresh (HWND hDlg)
{
	int n, nlist;
	char cbuf[256], cpath[256];

	SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_RESETCONTENT, 0, 0);

	oapi::GraphicsClient::LABELLIST *list = g_psys->LabelList (&nlist);
	if (!nlist) return;

	_finddata_t fdata;
	long fh = g_psys->FindFirst (FILETYPE_MARKER, &fdata, cpath, cbuf);
	if (fh >= 0) {
		n = 0;
		do {
			SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_ADDSTRING, 0, (LPARAM)trim_string(cbuf));
			if (n < nlist && list[n].active)
				SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_SETSEL, TRUE, n);
			n++;
		} while (!g_psys->FindNext (fh, &fdata, cbuf));
		_findclose (fh);
	}
}

// ======================================================================

void DlgCustomCLabels::Select (HWND hDlg)
{
	int i, sel, nlist;

	oapi::GraphicsClient::LABELLIST *list = g_psys->LabelList (&nlist);
	if (!nlist) return;

	for (i = 0; i < nlist; i++) {
		sel = SendDlgItemMessage (hDlg, IDC_CLBL_LIST, LB_GETSEL, i, 0);
		list[i].active = (sel ? true:false);
	}

	std::ifstream cfg (g_pOrbiter->Cfg()->ConfigPath (g_psys->Name()));
	g_psys->ScanLabelLists (cfg);
}

// ======================================================================

BOOL DlgCustomCLabels::OnInitDialog (HWND hDlg, WPARAM wParam, LPARAM lParam)
{
	Refresh (hDlg);
	return TRUE;
}

// ======================================================================

BOOL DlgCustomCLabels::OnCommand (HWND hDlg, WORD id, WORD code, HWND hControl)
{
	switch (id) {
	case IDC_CLBL_LIST:
		if (code == LBN_SELCHANGE)
			Select (hDlg);
		return TRUE;
	}
	return DialogWin::OnCommand (hDlg, id, code, hControl);
}

