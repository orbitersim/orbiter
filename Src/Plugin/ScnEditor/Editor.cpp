// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//              ORBITER MODULE: Scenario Editor
//                  Part of the ORBITER SDK
//
// Editor.cpp
//
// Implementation of ScnEditor class and editor tab subclasses
// derived from ScnEditorTab.
// ==============================================================

#include "orbitersdk.h"
#include "resource.h"
#include "Editor.h"
#include "DlgCtrl.h"
#include <commctrl.h>
#include <stdio.h>

using std::min;
using std::max;

extern ScnEditor *g_editor;
extern HBITMAP g_hPause;

// ==============================================================
// Local prototypes
// ==============================================================

void OpenDialog (void *context);
void Crt2Pol (VECTOR3 &pos, VECTOR3 &vel);
void Pol2Crt (VECTOR3 &pos, VECTOR3 &vel);
INT_PTR CALLBACK EditorProc (HWND, UINT, WPARAM, LPARAM);

static double lengthscale[4] = {1.0, 1e-3, 1.0/AU, 1.0};
static double anglescale[2] = {DEG, 1.0};

static HELPCONTEXT g_hc = {
	(char*)"html/plugin/ScnEditor.chm",
	0,
	(char*)"html/plugin/ScnEditor.chm::/ScnEditor.hhc",
	(char*)"html/plugin/ScnEditor.chm::/ScnEditor.hhk"
};

// ==============================================================
// ScnEditor class definition
// ==============================================================

ScnEditor::ScnEditor (HINSTANCE hDLL)
{
	hInst  = hDLL;
	hEdLib = NULL;
	hDlg   = NULL;

	imglist = ImageList_Create (16, 16, ILC_COLOR8, 4, 0);
	treeicon_idx[0] = ImageList_Add (imglist, LoadBitmap (hInst, MAKEINTRESOURCE (IDB_TREEICON_FOLDER1)), 0);
	treeicon_idx[1] = ImageList_Add (imglist, LoadBitmap (hInst, MAKEINTRESOURCE (IDB_TREEICON_FOLDER2)), 0);
	treeicon_idx[2] = ImageList_Add (imglist, LoadBitmap (hInst, MAKEINTRESOURCE (IDB_TREEICON_FILE1)), 0);
	treeicon_idx[3] = ImageList_Add (imglist, LoadBitmap (hInst, MAKEINTRESOURCE (IDB_TREEICON_FILE2)), 0);

	dwCmd = oapiRegisterCustomCmd (
		(char*)"Scenario Editor",
		(char*)"Create, delete and configure spacecraft",
		::OpenDialog, this);

	dwMenuCmd = oapiRegisterCustomMenuCmd("ScnEdit", "MenuInfoBar/ScnEdit.png", ::OpenDialog, this);
}

ScnEditor::~ScnEditor ()
{
	CloseDialog();
	oapiUnregisterCustomCmd (dwCmd);
	oapiUnregisterCustomMenuCmd (dwMenuCmd);
	ImageList_Destroy (imglist);
}

void ScnEditor::OpenDialog ()
{
	nTab  = 0;
	cTab  = NULL;
	hDlg  = oapiOpenDialogEx (hInst, IDD_EDITOR, EditorProc, 0, this);
}

void ScnEditor::CloseDialog ()
{
	if (hDlg) {
		oapiCloseDialog (hDlg);
		hDlg = NULL;
		cTab = NULL;
		if (nTab) {
			for (DWORD i = 0; i < nTab; i++) delete pTab[i];
			delete []pTab;
			nTab = 0;
		}
	}
	if (hEdLib) {
		FreeLibrary (hEdLib);
		hEdLib = 0;
	}
}

void ScnEditor::ScanCBodyList (HWND hDlg, int hList, OBJHANDLE hSelect)
{
	// populate a list of celestial bodies
	char cbuf[256];
	SendDlgItemMessage (hDlg, hList, CB_RESETCONTENT, 0, 0);
	for (DWORD n = 0; n < oapiGetGbodyCount(); n++) {
		oapiGetObjectName (oapiGetGbodyByIndex (n), cbuf, 256);
		SendDlgItemMessage (hDlg, hList, CB_ADDSTRING, 0, (LPARAM)cbuf);
	}
	// select the requested body
	oapiGetObjectName (hSelect, cbuf, 256);
	SendDlgItemMessage (hDlg, hList, CB_SELECTSTRING, -1, (LPARAM)cbuf);
}

void ScnEditor::ScanPadList (HWND hDlg, int hList, OBJHANDLE hBase)
{
	SendDlgItemMessage (hDlg, hList, CB_RESETCONTENT, 0, 0);
	if (hBase) {
		DWORD n, npad = oapiGetBasePadCount (hBase);
		char cbuf[16];
		for (n = 1; n <= npad; n++) {
			sprintf (cbuf, "%d", n);
			SendDlgItemMessage (hDlg, hList, CB_ADDSTRING, 0, (LPARAM)cbuf);
		}
		SendDlgItemMessage (hDlg, hList, CB_SETCURSEL, 0, 0);
	}
}

void ScnEditor::SelectBase (HWND hDlg, int hList, OBJHANDLE hRef, OBJHANDLE hBase)
{
	char cbuf[256];
	GetWindowText (GetDlgItem (hDlg, hList), cbuf, 256);
	OBJHANDLE hOldBase = oapiGetBaseByName (hRef, cbuf);
	if (hBase) {
		oapiGetObjectName (hBase, cbuf, 256);
		SendDlgItemMessage (hDlg, hList, CB_SELECTSTRING, -1, (LPARAM)cbuf);
	}
	if (hOldBase != hBase) ScanPadList (hDlg, IDC_PAD, hBase);
}

void ScnEditor::SetBasePosition (HWND hDlg)
{
	char cbuf[256];
	DWORD pad;
	double lng, lat;
	GetWindowText (GetDlgItem (hDlg, IDC_REF), cbuf, 256);
	OBJHANDLE hRef = oapiGetGbodyByName (cbuf);
	if (!hRef) return;
	GetWindowText (GetDlgItem (hDlg, IDC_BASE), cbuf, 256);
	OBJHANDLE hBase = oapiGetBaseByName (hRef, cbuf);
	if (!hBase) return;
	GetWindowText (GetDlgItem (hDlg, IDC_PAD), cbuf, 256);
	if (sscanf (cbuf, "%d", &pad) && pad >= 1 && pad <= oapiGetBasePadCount (hBase))
		oapiGetBasePadEquPos (hBase, pad-1, &lng, &lat);
	else
		oapiGetBaseEquPos (hBase, &lng, &lat);
	sprintf (cbuf, "%lf", lng * DEG);
	SetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf);
	sprintf (cbuf, "%lf", lat * DEG);
	SetWindowText (GetDlgItem (hDlg, IDC_EDIT2), cbuf);
}

bool ScnEditor::SaveScenario (HWND hDlg)
{
	char fname[256], title[256], text[4096], desc[4096];
	GetWindowText(GetDlgItem(hDlg, IDC_EDIT1), fname, 256);
	GetWindowText(GetDlgItem(hDlg, IDC_EDIT3), title, 256);
	GetWindowText(GetDlgItem(hDlg, IDC_EDIT2), text, 4096);

	if (strlen(title)) {
		sprintf(desc, "<h1>%s</h1>\n", title);
	}
	else {
		desc[0] = '\0';
	}
	if (strlen(text)) {
		strcat(desc, "<p>");
		int j = strlen(desc);
		for (int i = 0; i < strlen(text) && j < 4090; i++) {
			if (text[i] == '\r') {
				continue;
			}
			else if (text[i] == '\n') {
				strcpy(desc + j, "</p>\n<p>");
				j = strlen(desc);
			}
			else
				desc[j++] = text[i];
		}
		strcpy(desc + j, "</p>");
	}
	return oapiSaveScenario(fname, desc);
}

void ScnEditor::InitDialog (HWND _hDlg)
{
	hDlg = _hDlg;
	AddTab (new EditorTab_Vessel (this));
	AddTab (new EditorTab_New (this));
	AddTab (new EditorTab_Save (this));
	AddTab (new EditorTab_Edit (this));
	AddTab (new EditorTab_Elements (this));
	AddTab (new EditorTab_Statevec (this));
	AddTab (new EditorTab_Landed (this));
	AddTab (new EditorTab_Orientation (this));
	AddTab (new EditorTab_AngularVel (this));
	AddTab (new EditorTab_Propellant (this));
	AddTab (new EditorTab_Docking (this));
	AddTab (new EditorTab_Date (this));
	nTab0 = nTab;
	oapiAddTitleButton (IDPAUSE, g_hPause, DLG_CB_TWOSTATE);
	Pause (oapiGetPause());
	ShowTab (0);
}

DWORD ScnEditor::AddTab (ScnEditorTab *newTab)
{
	if (newTab->TabHandle()) {
		ScnEditorTab **tmpTab = new ScnEditorTab*[nTab+1];
		if (nTab) {
			memcpy (tmpTab, pTab, nTab*sizeof(ScnEditorTab*));
			delete []pTab;
		}
		pTab = tmpTab;
		pTab[nTab] = newTab;
		return nTab++;
	} else return (DWORD)-1;
}

void ScnEditor::DelCustomTabs ()
{
	if (!nTab || nTab == nTab0) return; // nothing to do
	for (DWORD i = nTab0; i < nTab; i++) delete pTab[i];
	ScnEditorTab **tmpTab = new ScnEditorTab*[nTab0];
	memcpy (tmpTab, pTab, nTab0*sizeof(ScnEditorTab*));
	delete []pTab;
	pTab = tmpTab;
	nTab = nTab0;
}

void ScnEditor::ShowTab (DWORD t)
{
	if (t < nTab) {
		cTab = pTab[t];
		cTab->Show();
	}
}

INT_PTR ScnEditor::MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		InitDialog (hDlg);
		return TRUE;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDCANCEL:
			CloseDialog();
			return TRUE;
		case IDHELP:
			if (cTab) cTab->OpenHelp();
			return TRUE;
		case IDPAUSE:
			oapiSetPause (HIWORD(wParam) != 0);
			return TRUE;
		}
		break;
	}
	return oapiDefDialogProc (hDlg, uMsg, wParam, lParam);
}

bool ScnEditor::CreateVessel (char *name, char *classname)
{
	// define an arbitrary status (the user will have to edit this after creation)
	VESSELSTATUS2 vs;
	memset (&vs, 0, sizeof(vs));
	vs.version = 2;
	vs.rbody = oapiGetGbodyByName ((char*)"Earth");
	if (!vs.rbody) vs.rbody = oapiGetGbodyByIndex (0);
	double rad = 1.1 * oapiGetSize (vs.rbody);
	double vel = sqrt (GGRAV * oapiGetMass (vs.rbody) / rad);
	vs.rpos = _V(rad,0,0);
	vs.rvel = _V(0,0,vel);
	// create the vessel
	OBJHANDLE newvessel = oapiCreateVesselEx (name, classname, &vs);
	if (!newvessel) return false; // failure
	hVessel = newvessel;
	return true;
}

void ScnEditor::VesselDeleted (OBJHANDLE hV)
{
	if (!hDlg) return; // no action required

	// update editor after vessel has been deleted
	if (hVessel == hV) { // vessel is currently being edited
		for (DWORD i = 0; i < nTab; i++)
			pTab[i]->Hide();
		ShowTab (0);
	}
	((EditorTab_Vessel*)pTab[0])->VesselDeleted (hV);
}

char *ScnEditor::ExtractVesselName (char *str)
{
	int i;
	for (i = 0; str[i] && str[i] != '\t'; i++);
	str[i] = '\0';
	return str;
}

void ScnEditor::Pause (bool pause)
{
	if (hDlg) oapiSetTitleButtonState (hDlg, IDPAUSE, pause ? 1:0);
}

HINSTANCE ScnEditor::LoadVesselLibrary (const VESSEL *vessel)
{
	// load vessel-specific editor extensions
	char cbuf[256];
	if (hEdLib) FreeLibrary (hEdLib); // remove previous library
	if (vessel->GetEditorModule (cbuf))
		hEdLib = LoadLibrary (cbuf);
	else hEdLib = 0;
	return hEdLib;
}

// ==============================================================
// nonmember functions

void OpenDialog (void *context)
{
	((ScnEditor*)context)->OpenDialog();
}

void Crt2Pol (VECTOR3 &pos, VECTOR3 &vel)
{
	// position in polar coordinates
	double r      = sqrt  (pos.x*pos.x + pos.y*pos.y + pos.z*pos.z);
	double lng    = atan2 (pos.z, pos.x);
	double lat    = asin  (pos.y/r);
	// derivatives in polar coordinates
	double drdt   = (vel.x*pos.x + vel.y*pos.y + vel.z*pos.z) / r;
	double dlngdt = (vel.z*pos.x - pos.z*vel.x) / (pos.x*pos.x + pos.z*pos.z);
	double dlatdt = (vel.y*r - pos.y*drdt) / (r*sqrt(r*r - pos.y*pos.y));
	pos.data[0] = r;
	pos.data[1] = lng;
	pos.data[2] = lat;
	vel.data[0] = drdt;
	vel.data[1] = dlngdt;
	vel.data[2] = dlatdt;
}

void Pol2Crt (VECTOR3 &pos, VECTOR3 &vel)
{
	double r   = pos.data[0];
	double lng = pos.data[1], clng = cos(lng), slng = sin(lng);
	double lat = pos.data[2], clat = cos(lat), slat = sin(lat);
	// position in cartesian coordinates
	double x   = r * cos(lat) * cos(lng);
	double z   = r * cos(lat) * sin(lng);
	double y   = r * sin(lat);
	// derivatives in cartesian coordinates
	double dxdt = vel.data[0]*clat*clng - vel.data[1]*r*clat*slng - vel.data[2]*r*slat*clng;
	double dzdt = vel.data[0]*clat*slng + vel.data[1]*r*clat*clng - vel.data[2]*r*slat*slng;
	double dydt = vel.data[0]*slat + vel.data[2]*r*clat;
	pos.data[0] = x;
	pos.data[1] = y;
	pos.data[2] = z;
	vel.data[0] = dxdt;
	vel.data[1] = dydt;
	vel.data[2] = dzdt;
}

INT_PTR CALLBACK EditorProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return g_editor->MsgProc (hDlg, uMsg, wParam, lParam);
}

// ==============================================================
// ScnEditorTab class definition
// ==============================================================

ScnEditorTab::ScnEditorTab (ScnEditor *editor)
{
	ed = editor;
	HWND hDlg = ed->DlgHandle();
	hTab = 0;
}

ScnEditorTab::~ScnEditorTab ()
{
	DestroyTab ();
}

HWND ScnEditorTab::CreateTab (HINSTANCE hInst, WORD ResId,  DLGPROC TabProc)
{
	hTab = CreateDialogParam (hInst, MAKEINTRESOURCE(ResId), ed->DlgHandle(), TabProc, (LPARAM)this);
	SetWindowLongPtr (hTab, DWLP_USER, (LONG_PTR)this);
	return hTab;
}

HWND ScnEditorTab::CreateTab (WORD ResId, DLGPROC TabProc)
{
	return CreateTab (ed->InstHandle(), ResId, TabProc);
}

void ScnEditorTab::DestroyTab ()
{
	if (hTab) {
		DestroyWindow (hTab);
		hTab = 0;
	}
}

void ScnEditorTab::SwitchTab (int newtab)
{
	ShowWindow (hTab, SW_HIDE);
	ed->ShowTab (newtab);
}

void ScnEditorTab::Show ()
{
	ShowWindow (hTab, SW_SHOW);
	g_hc.topic = HelpTopic();
	InitTab ();
}

void ScnEditorTab::Hide ()
{
	ShowWindow (hTab, SW_HIDE);
}

char *ScnEditorTab::HelpTopic ()
{
	return (char*)"/ScnEditor.htm";
}

void ScnEditorTab::OpenHelp ()
{
	static HELPCONTEXT hc = {
		(char*)"html/plugin/ScnEditor.chm",
		0,
		(char*)"html/plugin/ScnEditor.chm::/ScnEditor.hhc",
		(char*)"html/plugin/ScnEditor.chm::/ScnEditor.hhk"
	};
	hc.topic = HelpTopic();
	oapiOpenHelp (&hc);
}

INT_PTR ScnEditorTab::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDHELP:
			OpenHelp();
			return TRUE;
		}
		break;
	}
	return FALSE;
}

ScnEditorTab *ScnEditorTab::TabPointer (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	if (uMsg == WM_INITDIALOG) return (ScnEditorTab*)lParam;
	else return (ScnEditorTab*)GetWindowLongPtr (hDlg, DWLP_USER);
}

void ScnEditorTab::ScanVesselList (int ResId, bool detail, OBJHANDLE hExclude)
{
	char cbuf[256], *cp;

	// populate vessel list
	SendDlgItemMessage (hTab, ResId, LB_RESETCONTENT, 0, 0);
	for (DWORD i = 0; i < oapiGetVesselCount(); i++) {
		OBJHANDLE hR, hV = oapiGetVesselByIndex (i);
		if (hV == hExclude) continue;
		VESSEL *vessel = oapiGetVesselInterface (hV);
		strcpy (cbuf, vessel->GetName());
		if (detail) {
			if (cp = vessel->GetClassName())
				sprintf (cbuf+strlen(cbuf), "\t(%s)", cp);
			else strcat (cbuf, "\t");
			if (hR = vessel->GetGravityRef()) {
				char rname[256];
				oapiGetObjectName (hR, rname, 256);
				sprintf (cbuf+strlen(cbuf), "\t%s", rname);
			}
		}
		SendDlgItemMessage (hTab, ResId, LB_ADDSTRING, 0, (LPARAM)cbuf);
	}
}

OBJHANDLE ScnEditorTab::GetVesselFromList (int ResId)
{
	char cbuf[256];
	int idx = SendDlgItemMessage (hTab, ResId, LB_GETCURSEL, 0, 0);
	SendDlgItemMessage (hTab, ResId, LB_GETTEXT, idx, (LPARAM)cbuf);
	OBJHANDLE hV = oapiGetVesselByName (ed->ExtractVesselName (cbuf));
	return hV;
}


// ==============================================================
// EditorTab_Vessel class definition
// ==============================================================

EditorTab_Vessel::EditorTab_Vessel (ScnEditor *editor) : ScnEditorTab (editor)
{
	CreateTab (IDD_TAB_VESSEL, EditorTab_Vessel::DlgProc);
}

void EditorTab_Vessel::InitTab ()
{
	UINT tbs[1] = {70};
	SendDlgItemMessage (hTab, IDC_LIST1, LB_SETTABSTOPS, 1, (LPARAM)tbs);
	ScanVesselList ();
}

void EditorTab_Vessel::ScanVesselList ()
{
	ScnEditorTab::ScanVesselList (IDC_LIST1, true);
	// update vessel list
	SelectVessel (0);
}

char *EditorTab_Vessel::HelpTopic ()
{
	return (char*)"/ScnEditor.htm";
}

INT_PTR EditorTab_Vessel::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	int i;
	char cbuf[256];

	switch (uMsg) {
	case WM_INITDIALOG:
		SendDlgItemMessage (hDlg, IDC_TRACK, BM_SETCHECK, BST_CHECKED, 0);
		return TRUE;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDCANCEL:
			ed->CloseDialog();
			return TRUE;
		case IDC_VESSELNEW:
			SwitchTab (1);
			return TRUE;
		case IDC_VESSELEDIT:
			i = SendDlgItemMessage (hTab, IDC_LIST1, LB_GETCURSEL, 0, 0);
			SendDlgItemMessage (hTab, IDC_LIST1, LB_GETTEXT, i, (LPARAM)cbuf);
			ed->hVessel = oapiGetVesselByName (ed->ExtractVesselName(cbuf));
			if (ed->hVessel) SwitchTab (3);
			return TRUE;
		case IDC_VESSELDEL:
			DeleteVessel();
			return TRUE;
		case IDC_SAVE:
			SwitchTab (2);
			return TRUE;
		case IDC_DATE:
			SwitchTab (11);
			return TRUE;
		case IDC_LIST1:
			if (HIWORD (wParam) == LBN_SELCHANGE) {
				VesselSelected ();
				return TRUE;
			}
			break;
		}
		break;
	}
	return ScnEditorTab::TabProc (hDlg, uMsg, wParam, lParam);
}

void EditorTab_Vessel::SelectVessel (OBJHANDLE hV)
{
	char cbuf[256];
	if (!hV) hV = oapiCameraTarget();
	oapiGetObjectName (hV, cbuf, 256);
	int idx = SendDlgItemMessage (hTab, IDC_LIST1, LB_FINDSTRING, -1, (LPARAM)cbuf);
	if (idx == LB_ERR) { // last resort: focus object
		hV = oapiGetFocusObject();
		oapiGetObjectName (hV, cbuf, 256);
		idx = SendDlgItemMessage (hTab, IDC_LIST1, LB_FINDSTRING, -1, (LPARAM)cbuf);
	}
	if (idx != LB_ERR) {
		SendDlgItemMessage (hTab, IDC_LIST1, LB_SETCURSEL, idx, 0);
		ed->hVessel = hV;
	}
}

void EditorTab_Vessel::VesselSelected ()
{
	OBJHANDLE hV = GetVesselFromList (IDC_LIST1);
	if (!hV) return;
	bool track = (SendDlgItemMessage (hTab, IDC_TRACK, BM_GETCHECK, 0, 0) == BST_CHECKED);
	if (track) oapiCameraAttach (hV, 1);
}

void EditorTab_Vessel::VesselDeleted (OBJHANDLE hV)
{
	char cbuf[256];
	oapiGetObjectName (hV, cbuf, 256);
	int idx = SendDlgItemMessage (hTab, IDC_LIST1, LB_FINDSTRING, -1, (LPARAM)cbuf);
	if (idx == LB_ERR) return;
	SendDlgItemMessage (hTab, IDC_LIST1, LB_DELETESTRING, idx, 0);
	idx = SendDlgItemMessage (hTab, IDC_LIST1, LB_GETCURSEL, 0, 0);
	if (idx == LB_ERR) { // deleted current selection
		SendDlgItemMessage (hTab, IDC_LIST1, LB_SETCURSEL, 0, 0);
		VesselSelected ();
	}
}

// Orbiter closes when there is no focusable vessels in the scenario
// Check if there's another focusable object present
bool EditorTab_Vessel::CanDelete(OBJHANDLE hVessel)
{
	for(int i = 0; i < oapiGetVesselCount(); i++) {
		OBJHANDLE obj = oapiGetVesselByIndex (i);
		if(obj == hVessel)
			continue;
		VESSEL *v = oapiGetVesselInterface (obj);
		if(v->GetEnableFocus())
			return true;
	}
	return false;
}

bool EditorTab_Vessel::DeleteVessel ()
{
	char cbuf[256];
	int idx = SendDlgItemMessage (hTab, IDC_LIST1, LB_GETCURSEL, 0, 0);
	if (idx == LB_ERR) return false;
	SendDlgItemMessage (hTab, IDC_LIST1, LB_GETTEXT, idx, (LPARAM)cbuf);
	OBJHANDLE hV = oapiGetVesselByName (ed->ExtractVesselName (cbuf));
	if (!hV) return false;
	//ed->hVessel = hV;
	if(CanDelete(hV)) {
		oapiDeleteVessel (hV);
	} else {
		MessageBox(hTab, "Cannot delete the last focusable vessel in a scenario", NULL, MB_OK);
		return false;
	}
	return true;
}

INT_PTR EditorTab_Vessel::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_Vessel *pTab = (EditorTab_Vessel*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}

// ==============================================================
// EditorTab_New class definition
// ==============================================================

EditorTab_New::EditorTab_New (ScnEditor *editor) : ScnEditorTab (editor)
{
	hVesselBmp = NULL;
	CreateTab (IDD_TAB_NEW, EditorTab_New::DlgProc);
}

EditorTab_New::~EditorTab_New ()
{
	if (hVesselBmp) DeleteObject (hVesselBmp);
}

void EditorTab_New::InitTab ()
{
	SendDlgItemMessage (hTab, IDC_CAMERA, BM_SETCHECK, BST_CHECKED, 0);

	// populate vessel type list
	RefreshVesselTpList ();
	SendDlgItemMessage (hTab, IDC_VESSELTP, TVM_SETIMAGELIST, (WPARAM)TVSIL_NORMAL, (LPARAM)ed->imglist);

	RECT r;
	GetClientRect (GetDlgItem (hTab, IDC_VESSELBMP), &r);
	imghmax = r.bottom;
}

char *EditorTab_New::HelpTopic ()
{
	return (char*)"/NewVessel.htm";
}

INT_PTR EditorTab_New::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	NM_TREEVIEW *pnmtv;

	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDCANCEL:
			SwitchTab (0);
			return TRUE;
		case IDC_CREATE:
			if (CreateVessel ()) SwitchTab (3); // switch to editor page
			return TRUE;
		}
		break;
	case WM_NOTIFY:
		switch (LOWORD(wParam)) {
		case IDC_VESSELTP:
			pnmtv = (NM_TREEVIEW FAR *)lParam;
			switch (pnmtv->hdr.code) {
			case TVN_SELCHANGED:
				VesselTpChanged ();
				return TRUE;
			case NM_DBLCLK:
				PostMessage (hDlg, WM_COMMAND, IDC_CREATE, 0);
				return TRUE;
			}
			break;
		}
		break;
	case WM_PAINT:
		DrawVesselBmp ();
		break;
	}
	return ScnEditorTab::TabProc (hDlg, uMsg, wParam, lParam);
}

bool EditorTab_New::CreateVessel ()
{
	char name[256], classname[256];

	GetWindowText (GetDlgItem (hTab, IDC_NAME), name, 256);
	if (name[0] == '\0') return false;            // no name provided
	if (oapiGetVesselByName (name)) return false; // vessel name already in use

	if (GetSelVesselTp (classname, 256) != 1) return false; // no type selected
	if (!ed->CreateVessel (name, classname)) return false; // creation failed
	if (SendDlgItemMessage (hTab, IDC_FOCUS, BM_GETCHECK, 0, 0) == BST_CHECKED)
		oapiSetFocusObject (ed->hVessel);
	if (SendDlgItemMessage (hTab, IDC_CAMERA, BM_GETSTATE, 0, 0) == BST_CHECKED)
		oapiCameraAttach (ed->hVessel, 1);
	//	ed->VesselSelection();
	return true;
}

void EditorTab_New::ScanConfigDir (const fs::path& dir, HTREEITEM hti)
{
	// recursively scans a directory tree and adds to the list
	TV_INSERTSTRUCT tvis;
	HTREEITEM ht, hts0, ht0;
	char cbuf[256];

	tvis.hParent = hti;
	tvis.item.mask = TVIF_TEXT | TVIF_CHILDREN | TVIF_IMAGE | TVIF_SELECTEDIMAGE;
	tvis.item.pszText = cbuf;
	tvis.hInsertAfter = TVI_SORT;
	tvis.item.cChildren = 1;
	tvis.item.iImage = ed->treeicon_idx[0];
	tvis.item.iSelectedImage = ed->treeicon_idx[0];

	// scan for subdirectories
	for (const auto& entry : fs::directory_iterator(dir)) {
		if (entry.is_directory()) {
			strcpy(cbuf, entry.path().filename().string().c_str());
			ht = (HTREEITEM)SendDlgItemMessage(hTab, IDC_VESSELTP, TVM_INSERTITEM, 0, (LPARAM)&tvis);
			ScanConfigDir(entry.path(), ht);
		}
	}

	hts0 = (HTREEITEM)SendDlgItemMessage (hTab, IDC_VESSELTP, TVM_GETNEXTITEM, TVGN_CHILD, (LPARAM)hti);
	// the first subdirectory entry in this folder

	// scan for files
	tvis.hInsertAfter = TVI_FIRST;
	tvis.item.cChildren = 0;
	tvis.item.iImage = ed->treeicon_idx[2];
	tvis.item.iSelectedImage = ed->treeicon_idx[3];
	for (const auto& entry : fs::directory_iterator(dir)) {
		if (entry.is_regular_file() && entry.path().extension().string() == ".cfg") {
			bool skip = false;
			FILEHANDLE hFile = oapiOpenFile(entry.path().string().c_str(), FILE_IN);
			if (hFile) {
				bool b;
				skip = (oapiReadItem_bool(hFile, (char*)"EditorCreate", b) && !b);
				oapiCloseFile(hFile, FILE_IN);
			}
			if (skip) continue;

			strcpy(cbuf, entry.path().stem().string().c_str());

			char ch[256];
			TV_ITEM tvi = { TVIF_HANDLE | TVIF_TEXT, 0, 0, 0, ch, 256 };

			ht0 = (HTREEITEM)SendDlgItemMessage(hTab, IDC_VESSELTP, TVM_GETNEXTITEM, TVGN_CHILD, (LPARAM)hti);
			for (tvi.hItem = ht0; tvi.hItem && tvi.hItem != hts0; tvi.hItem = (HTREEITEM)SendDlgItemMessage(hTab, IDC_VESSELTP, TVM_GETNEXTITEM, TVGN_NEXT, (LPARAM)tvi.hItem)) {
				SendDlgItemMessage(hTab, IDC_VESSELTP, TVM_GETITEM, 0, (LPARAM)&tvi);
				if (strcmp(tvi.pszText, cbuf) > 0) break;
			}
			if (tvi.hItem) {
				ht = (HTREEITEM)SendDlgItemMessage(hTab, IDC_VESSELTP, TVM_GETNEXTITEM, TVGN_PREVIOUS, (LPARAM)tvi.hItem);
				tvis.hInsertAfter = (ht ? ht : TVI_FIRST);
			}
			else {
				tvis.hInsertAfter = (hts0 ? TVI_FIRST : TVI_LAST);
			}
			(HTREEITEM)SendDlgItemMessage(hTab, IDC_VESSELTP, TVM_INSERTITEM, 0, (LPARAM)&tvis);
		}
	}
}

void EditorTab_New::RefreshVesselTpList ()
{
	SendDlgItemMessage (hTab, IDC_VESSELTP, TVM_DELETEITEM, 0, (LPARAM)TVI_ROOT);
	ScanConfigDir ("Config/Vessels/", NULL);
}

int EditorTab_New::GetSelVesselTp (char *name, int len)
{
	TV_ITEM tvi;
	char cbuf[256];
	int type;

	tvi.mask = TVIF_HANDLE | TVIF_TEXT | TVIF_CHILDREN;
	tvi.hItem = TreeView_GetSelection (GetDlgItem (hTab, IDC_VESSELTP));
	tvi.pszText = name;
	tvi.cchTextMax = len;

	if (!TreeView_GetItem (GetDlgItem (hTab, IDC_VESSELTP), &tvi)) return 0;
	type = (tvi.cChildren ? 2 : 1);

	// build path
	tvi.pszText = cbuf;
	tvi.cchTextMax = 256;
	while (tvi.hItem = TreeView_GetParent (GetDlgItem (hTab, IDC_VESSELTP), tvi.hItem)) {
		if (TreeView_GetItem (GetDlgItem (hTab, IDC_VESSELTP), &tvi)) {
			strcat (cbuf, "\\");
			strcat (cbuf, name);
			strcpy (name, cbuf);
		}
	}
	return type;
}

void EditorTab_New::VesselTpChanged ()
{
	UpdateVesselBmp();
}

bool EditorTab_New::UpdateVesselBmp ()
{
	char classname[256], pathname[256], imagename[256];

	if (hVesselBmp) {
		DeleteObject (hVesselBmp);
		hVesselBmp = NULL;
	}

	if (GetSelVesselTp (classname, 256) == 1) {
		sprintf (pathname, "Vessels\\%s.cfg", classname);
		FILEHANDLE hFile = oapiOpenFile (pathname, FILE_IN, CONFIG);
		if (!hFile) return false;
		if (oapiReadItem_string (hFile, (char*)"ImageBmp", imagename)) {
			hVesselBmp = (HBITMAP)LoadImage (ed->InstHandle(), imagename, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE);
		}
		oapiCloseFile (hFile, FILE_IN);
	}
	DrawVesselBmp();
	return (hVesselBmp != NULL);
}

void EditorTab_New::DrawVesselBmp ()
{
	HWND hImgWnd = GetDlgItem (hTab, IDC_VESSELBMP);
	InvalidateRect (hImgWnd, NULL, TRUE);
	UpdateWindow (hImgWnd);
	if (hVesselBmp) {
	    BITMAP bm;
		RECT r;
		int dx, dy, h;
		HDC hDC = GetDC (hImgWnd);
		HDC hBmpDC = CreateCompatibleDC (hDC);
		SelectObject (hBmpDC, hVesselBmp);
		GetClientRect (hImgWnd, &r);
	    GetObject(hVesselBmp, sizeof(bm), &bm);
		dx = bm.bmWidth, dy = bm.bmHeight;
		h = min (imghmax, (int)(r.right*dy)/dx);
		SetWindowPos (hImgWnd, NULL, 0, 0, r.right, h, SWP_NOMOVE|SWP_NOZORDER);
		StretchBlt (hDC, 0, 0, r.right, h, hBmpDC, 0, 0, dx, dy, SRCCOPY);
		DeleteDC (hBmpDC);
		ReleaseDC (hImgWnd, hDC);
		ShowWindow (hImgWnd, SW_SHOW);
	} else {
		ShowWindow (hImgWnd, SW_HIDE);
	}
}

INT_PTR EditorTab_New::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_New *pTab = (EditorTab_New*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}

// ==============================================================
// EditorTab_Save class definition
// ==============================================================

EditorTab_Save::EditorTab_Save (ScnEditor *editor) : ScnEditorTab (editor)
{
	CreateTab (IDD_TAB_SAVE, EditorTab_Save::DlgProc);
	SendDlgItemMessage(hTab, IDC_RADIO1, BM_SETCHECK, BST_CHECKED, 0);
	SendDlgItemMessage(hTab, IDC_RADIO2, BM_SETCHECK, BST_UNCHECKED, 0);
}

char *EditorTab_Save::HelpTopic ()
{
	return (char*)"/SaveScenario.htm";
}

INT_PTR EditorTab_Save::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BACK:
			SwitchTab (0);
			return TRUE;
		case IDOK:
			if (ed->SaveScenario (hTab))
				SwitchTab (0);
			return TRUE;
		}
		break;
	}
	return ScnEditorTab::TabProc (hDlg, uMsg, wParam, lParam);
}

INT_PTR EditorTab_Save::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_Save *pTab = (EditorTab_Save*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}


// ==============================================================
// EditorTab_Date class definition
// ==============================================================

EditorTab_Date::EditorTab_Date (ScnEditor *editor) : ScnEditorTab (editor)
{
	CreateTab (IDD_TAB_DATE, EditorTab_Date::DlgProc);
}

void EditorTab_Date::InitTab ()
{
	Refresh();
}

char *EditorTab_Date::HelpTopic ()
{
	return (char*)"/Date.htm";
}

void EditorTab_Date::Apply ()
{
	int OrbitalMode[3] = {PROP_ORBITAL_FIXEDSTATE, PROP_ORBITAL_FIXEDSURF, PROP_ORBITAL_ELEMENTS};
	int SOrbitalMode[4] = {PROP_SORBITAL_FIXEDSTATE, PROP_SORBITAL_FIXEDSURF, PROP_SORBITAL_ELEMENTS, PROP_SORBITAL_DESTROY};
	int omode = OrbitalMode[SendDlgItemMessage (hTab, IDC_PROP_ORBITAL, CB_GETCURSEL, 0, 0)];
	int smode = SOrbitalMode[SendDlgItemMessage (hTab, IDC_PROP_SORBITAL, CB_GETCURSEL, 0, 0)];
	oapiSetSimMJD (mjd, omode | smode);
}

void EditorTab_Date::Refresh ()
{
	SetMJD (oapiGetSimMJD (), true);
}

void EditorTab_Date::UpdateDateTime ()
{
	char cbuf[256];

	sprintf (cbuf, "%02d", date.tm_mday);
	bIgnore = true;
	SetWindowText (GetDlgItem (hTab, IDC_UT_DAY), cbuf);
	bIgnore = false;

	sprintf (cbuf, "%02d", date.tm_mon);
	bIgnore = true;
	SetWindowText (GetDlgItem (hTab, IDC_UT_MONTH), cbuf);
	bIgnore = false;

	sprintf (cbuf, "%04d", date.tm_year+1900);
	bIgnore = true;
	SetWindowText (GetDlgItem (hTab, IDC_UT_YEAR), cbuf);
	bIgnore = false;

	sprintf (cbuf, "%02d", date.tm_hour);
	bIgnore = true;
	SetWindowText (GetDlgItem (hTab, IDC_UT_HOUR), cbuf);
	bIgnore = false;

	sprintf (cbuf, "%02d", date.tm_min);
	bIgnore = true;
	SetWindowText (GetDlgItem (hTab, IDC_UT_MIN), cbuf);
	bIgnore = false;

	sprintf (cbuf, "%02d", date.tm_sec);
	bIgnore = true;
	SetWindowText (GetDlgItem (hTab, IDC_UT_SEC), cbuf);
	bIgnore = false;
}

void EditorTab_Date::UpdateMJD (void)
{
	char cbuf[256];
	sprintf (cbuf, "%0.6f", mjd);
	bIgnore = true;
	SetWindowText (GetDlgItem (hTab, IDC_MJD), cbuf);
	bIgnore = false;
}

void EditorTab_Date::UpdateJD (void)
{
	char cbuf[256];
	sprintf (cbuf, "%0.6f", mjd + 2400000.5);
	bIgnore = true;
	SetWindowText (GetDlgItem (hTab, IDC_JD), cbuf);
	bIgnore = false;
}

void EditorTab_Date::UpdateJC (void)
{
	char cbuf[256];
	sprintf (cbuf, "%0.10f", MJD2JC(mjd));
	bIgnore = true;
	SetWindowText (GetDlgItem (hTab, IDC_JC2000), cbuf);
	bIgnore = false;
}

void EditorTab_Date::UpdateEpoch (void)
{
	char cbuf[256];
	sprintf (cbuf, "%0.8f", MJD2Jepoch (mjd));
	bIgnore = true;
	SetWindowText (GetDlgItem (hTab, IDC_EPOCH), cbuf);
	bIgnore = false;
}

void EditorTab_Date::SetUT (struct tm *new_date, bool reset_ut)
{
	mjd = date2mjd (new_date);
	UpdateMJD();
	UpdateJD();
	UpdateJC();
	UpdateEpoch();
	if (reset_ut) UpdateDateTime();
}

void EditorTab_Date::SetMJD (double new_mjd, bool reset_mjd)
{
	mjd = new_mjd;
	memcpy (&date, mjddate(mjd), sizeof (date));

	UpdateDateTime();
	UpdateJD();
	UpdateJC();
	UpdateEpoch();
	if (reset_mjd) UpdateMJD();
}

void EditorTab_Date::SetJD (double new_jd, bool reset_jd)
{
	mjd = new_jd - 2400000.5;
	memcpy (&date, mjddate(mjd), sizeof (date));

	UpdateDateTime();
	UpdateMJD();
	UpdateJC();
	UpdateEpoch();
	if (reset_jd) UpdateJD();
}

void EditorTab_Date::SetJC (double new_jc, bool reset_jc)
{
	mjd = JC2MJD (new_jc);
	memcpy (&date, mjddate(mjd), sizeof (date));

	UpdateDateTime();
	UpdateMJD();
	UpdateJD();
	UpdateEpoch();
	if (reset_jc) UpdateJC();
}

void EditorTab_Date::SetEpoch (double new_epoch, bool reset_epoch)
{
	mjd = Jepoch2MJD (new_epoch);
	memcpy (&date, mjddate(mjd), sizeof (date));

	UpdateDateTime();
	UpdateMJD();
	UpdateJD();
	UpdateJC();
	if (reset_epoch) UpdateEpoch();
}

void EditorTab_Date::OnChangeDateTime() 
{
	if (bIgnore) return;
	char cbuf[256];
	int day, month, year, hour, min, sec;

	GetWindowText (GetDlgItem (hTab, IDC_UT_DAY), cbuf, 256);
	if (sscanf (cbuf, "%d", &day) != 1 || day < 1 || day > 31) return;
	GetWindowText (GetDlgItem (hTab, IDC_UT_MONTH), cbuf, 256);
	if (sscanf (cbuf, "%d", &month) != 1 || month < 1 || month > 12) return;
	GetWindowText (GetDlgItem (hTab, IDC_UT_YEAR), cbuf, 256);
	if (sscanf (cbuf, "%d", &year) != 1) return;
	year -= 1900;
	GetWindowText (GetDlgItem (hTab, IDC_UT_HOUR), cbuf, 256);
	if (sscanf (cbuf, "%d", &hour) != 1) return;
	GetWindowText (GetDlgItem (hTab, IDC_UT_MIN), cbuf, 256);
	if (sscanf (cbuf, "%d", &min) != 1) return;
	GetWindowText (GetDlgItem (hTab, IDC_UT_SEC), cbuf, 256);
	if (sscanf (cbuf, "%d", &sec) != 1) return;

	if (day == date.tm_mday && month == date.tm_mon && year == date.tm_year &&
		hour == date.tm_hour && min == date.tm_min && sec == date.tm_sec) return;

	date.tm_mday = day;
	date.tm_mon  = month;
	date.tm_year = year;
	date.tm_hour = hour;
	date.tm_min  = min;
	date.tm_sec  = sec;
	SetUT (&date);
}

void EditorTab_Date::OnChangeMjd() 
{
	if (bIgnore) return;
	char cbuf[256];
	double new_mjd;
	GetWindowText (GetDlgItem (hTab, IDC_MJD), cbuf, 256);
	if (sscanf (cbuf, "%lf", &new_mjd) == 1 && fabs (new_mjd-mjd) > 1e-6)
		SetMJD (new_mjd);
}

void EditorTab_Date::OnChangeJd() 
{
	if (bIgnore) return;
	char cbuf[256];
	double new_jd;
	GetWindowText (GetDlgItem (hTab, IDC_JD), cbuf, 256);
	if (sscanf (cbuf, "%lf", &new_jd) == 1)
		SetJD (new_jd);
}

void EditorTab_Date::OnChangeJc() 
{
	if (bIgnore) return;
	char cbuf[256];
	double new_jc;
	GetWindowText (GetDlgItem (hTab, IDC_JC2000), cbuf, 256);
	if (sscanf (cbuf, "%lf", &new_jc) == 1)
		SetJC (new_jc);
}

void EditorTab_Date::OnChangeEpoch() 
{
	if (bIgnore) return;
	char cbuf[256];
	double new_epoch;
	GetWindowText (GetDlgItem (hTab, IDC_EPOCH), cbuf, 256);
	if (sscanf (cbuf, "%lf", &new_epoch) == 1)
		SetEpoch (new_epoch);
}

INT_PTR EditorTab_Date::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG: {
		int i;
		SendDlgItemMessage (hDlg, IDC_PROP_ORBITAL, CB_RESETCONTENT, 0, 0);
		for (i = 0; i < 3; i++) {
			char cbuf[128];
			LoadString (ed->InstHandle(), IDS_PROP1+i, cbuf, 128);
			SendDlgItemMessage (hDlg, IDC_PROP_ORBITAL, CB_ADDSTRING, 0, (LPARAM)cbuf);
		}
		SendDlgItemMessage (hDlg, IDC_PROP_ORBITAL, CB_SETCURSEL, 2, 0);
		SendDlgItemMessage (hDlg, IDC_PROP_SORBITAL, CB_RESETCONTENT, 0, 0);
		for (i = 0; i < 4; i++) {
			char cbuf[128];
			LoadString (ed->InstHandle(), IDS_PROP1+i, cbuf, 128);
			SendDlgItemMessage (hDlg, IDC_PROP_SORBITAL, CB_ADDSTRING, 0, (LPARAM)cbuf);
		}
		SendDlgItemMessage (hDlg, IDC_PROP_SORBITAL, CB_SETCURSEL, 1, 0);
		} return TRUE;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BACK:
			SwitchTab (0);
			return TRUE;
		case IDC_REFRESH:
			Refresh();
			return TRUE;
		case IDC_NOW:
			SetMJD (oapiGetSysMJD(), true);
			// fall through
		case IDC_APPLY:
			Apply();
			return TRUE;
		case IDC_UT_DAY:
		case IDC_UT_MONTH:
		case IDC_UT_YEAR:
		case IDC_UT_HOUR:
		case IDC_UT_MIN:
		case IDC_UT_SEC:
			OnChangeDateTime ();
			return TRUE;
		case IDC_MJD:
			OnChangeMjd ();
			return TRUE;
		case IDC_JD:
			OnChangeJd ();
			return TRUE;
		case IDC_JC2000:
			OnChangeJc ();
			return TRUE;
		case IDC_EPOCH:
			OnChangeEpoch ();
			return TRUE;
		}
		break;
	case WM_NOTIFY:
		if (((NMHDR*)lParam)->code == UDN_DELTAPOS) {
			NMUPDOWN *nmud = (NMUPDOWN*)lParam;
			double dmjd = 0;
			bool dut = false;
			switch (((NMHDR*)lParam)->idFrom) {
			case IDC_SPIN_DAY:
				dmjd = -nmud->iDelta;
				break;
			case IDC_SPIN_HOUR:
				dmjd = -nmud->iDelta/24.0;
				break;
			case IDC_SPIN_MINUTE:
				dmjd = -nmud->iDelta/(24.0*60.0);
				break;
			case IDC_SPIN_SECOND:
				dmjd = -nmud->iDelta/(24.0*3600.0);
				break;
			case IDC_SPIN_MONTH:
				if (nmud->iDelta > 0) {
					date.tm_mon--;
					if (date.tm_mon < 1) date.tm_year--, date.tm_mon = 12;
				} else {
					date.tm_mon++;
					if (date.tm_mon > 12) date.tm_year++, date.tm_mon = 1;
				}
				dut = true;
				break;
			case IDC_SPIN_YEAR:
				date.tm_year -= nmud->iDelta;
				dut = true;
				break;
			}
			if (dmjd || dut) {
				if (dmjd) SetMJD (mjd+dmjd, true);
				else SetUT (&date, true);
				Apply();
			}
		}
		break;
	}
	return ScnEditorTab::TabProc (hDlg, uMsg, wParam, lParam);
}

INT_PTR EditorTab_Date::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_Date *pTab = (EditorTab_Date*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}


// ==============================================================
// EditorTab_Edit class definition
// ==============================================================

EditorTab_Edit::EditorTab_Edit (ScnEditor *editor) : ScnEditorTab (editor)
{
	hVessel = 0;
	CreateTab (IDD_TAB_EDIT1, EditorTab_Edit::DlgProc);
}

void EditorTab_Edit::InitTab ()
{
	VESSEL *vessel = Vessel();

	if (hVessel != ed->hVessel) {
		hVessel = ed->hVessel;

		// fill vessel and class name boxes
		SetWindowText (GetDlgItem (hTab, IDC_EDIT1), vessel->GetName());
		SetWindowText (GetDlgItem (hTab, IDC_EDIT2), vessel->GetClassName());
		SetWindowText (GetDlgItem (hTab, IDC_STATIC1), vessel->GetClassName());

		BOOL bFuel = (vessel->GetPropellantCount() > 0 && vessel->GetMaxFuelMass () > 0.0);
		EnableWindow (GetDlgItem (hTab, IDC_PROPELLANT), bFuel);

		BOOL bDocking = (vessel->DockCount() > 0);
		EnableWindow (GetDlgItem (hTab, IDC_DOCKING), bDocking);

		// disable custom buttons by default
		nCustom = 0;
		ed->DelCustomTabs();
		ShowWindow (GetDlgItem (hTab, IDC_STATIC1), SW_HIDE);
		for (int i = 0; i < 6; i++) {
			CustomPage[i] = 0;
			ShowWindow (GetDlgItem (hTab, IDC_EXTRA1+i), SW_HIDE);
		}

		// now load vessel-specific interface
		HINSTANCE hLib = ed->LoadVesselLibrary (vessel);
		if (hLib) {
			typedef void (*SEC_Init)(HWND,OBJHANDLE);
			SEC_Init secInit = (SEC_Init)GetProcAddress (hLib, "secInit");
			if (secInit) secInit (hTab, hVessel);
		}
	}
	SetWindowText (GetDlgItem (hTab, IDC_EDIT3),
		vessel->GetFlightStatus() & 1 ? "Inactive (Landed)":"Active (Flight)");
}

char *EditorTab_Edit::HelpTopic ()
{
	return (char*)"/EditVessel.htm";
}

BOOL EditorTab_Edit::AddFuncButton (EditorFuncSpec *efs)
{
	if (nCustom == 6) return FALSE;
	ShowWindow (GetDlgItem (hTab, IDC_STATIC1), SW_SHOW);
	SetWindowText (GetDlgItem (hTab, IDC_EXTRA1+nCustom), efs->btnlabel);
	ShowWindow (GetDlgItem (hTab, IDC_EXTRA1+nCustom), SW_SHOW);
	funcCustom[nCustom++] = efs->func;
	return TRUE;

}

BOOL EditorTab_Edit::AddPageButton (EditorPageSpec *eps)
{
	if (nCustom == 6) return FALSE;
	ShowWindow (GetDlgItem (hTab, IDC_STATIC1), SW_SHOW);
	SetWindowText (GetDlgItem (hTab, IDC_EXTRA1+nCustom), eps->btnlabel);
	ShowWindow (GetDlgItem (hTab, IDC_EXTRA1+nCustom), SW_SHOW);
	CustomPage[nCustom++] = ed->AddTab (new EditorTab_Custom (ed, eps->hDLL, eps->ResId, eps->TabProc));
	return TRUE;
}

INT_PTR EditorTab_Edit::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BACK:
			SwitchTab (0);
			return TRUE;
		case IDC_ELEMENTS:
			SwitchTab (4);
			return TRUE;
		case IDC_STATEVEC:
			SwitchTab (5);
			return TRUE;
		case IDC_GROUND:
			SwitchTab (6);
			return TRUE;
		case IDC_ORIENT:
			SwitchTab (7);
			return TRUE;
		case IDC_ANGVEL:
			SwitchTab (8);
			return TRUE;
		case IDC_PROPELLANT:
			SwitchTab (9);
			return TRUE;
		case IDC_DOCKING:
			SwitchTab (10);
			return TRUE;
		case IDC_EXTRA1:
		case IDC_EXTRA2:
		case IDC_EXTRA3:
		case IDC_EXTRA4:
		case IDC_EXTRA5:
		case IDC_EXTRA6: {
			int id = LOWORD(wParam)-IDC_EXTRA1;
			if (CustomPage[id])
				SwitchTab (CustomPage[id]);
			else
				funcCustom[id](ed->hVessel);
			} return TRUE;
		}
		break;
	case WM_SCNEDITOR:
		switch (LOWORD (wParam)) {
		case SE_ADDFUNCBUTTON:
			return AddFuncButton ((EditorFuncSpec*)lParam);
		case SE_ADDPAGEBUTTON:
			return AddPageButton ((EditorPageSpec*)lParam);
		}
		break;
	}
	return ScnEditorTab::TabProc (hDlg, uMsg, wParam, lParam);
}

INT_PTR EditorTab_Edit::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_Edit *pTab = (EditorTab_Edit*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}


// ==============================================================
// EditorTab_Elements class definition
// ==============================================================

EditorTab_Elements::EditorTab_Elements (ScnEditor *editor) : ScnEditorTab (editor)
{
	CreateTab (IDD_TAB_EDIT2, EditorTab_Elements::DlgProc);
	elmjd = oapiGetSimMJD(); // initial reference date
}

void EditorTab_Elements::InitTab ()
{
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	OBJHANDLE hRef = vessel->GetGravityRef();
	SendDlgItemMessage (hTab, IDC_COMBO1, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)"m");
	SendDlgItemMessage (hTab, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)"km");
	SendDlgItemMessage (hTab, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)"AU");
	SendDlgItemMessage (hTab, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)"planet rad.");
	SendDlgItemMessage (hTab, IDC_COMBO1, CB_SETCURSEL, 0, 0);

	SendDlgItemMessage (hTab, IDC_COMBO2, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM)"deg");
	SendDlgItemMessage (hTab, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM)"rad");
	SendDlgItemMessage (hTab, IDC_COMBO2, CB_SETCURSEL, 0, 0);

	SendDlgItemMessage (hTab, IDC_COMBO3, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_COMBO3, CB_ADDSTRING, 0, (LPARAM)"deg");
	SendDlgItemMessage (hTab, IDC_COMBO3, CB_ADDSTRING, 0, (LPARAM)"rad");
	SendDlgItemMessage (hTab, IDC_COMBO3, CB_SETCURSEL, 0, 0);

	SendDlgItemMessage (hTab, IDC_COMBO4, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_COMBO4, CB_ADDSTRING, 0, (LPARAM)"deg");
	SendDlgItemMessage (hTab, IDC_COMBO4, CB_ADDSTRING, 0, (LPARAM)"rad");
	SendDlgItemMessage (hTab, IDC_COMBO4, CB_SETCURSEL, 0, 0);

	SendDlgItemMessage (hTab, IDC_COMBO5, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_COMBO5, CB_ADDSTRING, 0, (LPARAM)"deg");
	SendDlgItemMessage (hTab, IDC_COMBO5, CB_ADDSTRING, 0, (LPARAM)"rad");
	SendDlgItemMessage (hTab, IDC_COMBO5, CB_SETCURSEL, 0, 0);

	SendDlgItemMessage (hTab, IDC_COMBO6, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_COMBO6, CB_ADDSTRING, 0, (LPARAM)"current");
	SendDlgItemMessage (hTab, IDC_COMBO6, CB_ADDSTRING, 0, (LPARAM)"MJD");
	SendDlgItemMessage (hTab, IDC_COMBO6, CB_SETCURSEL, 1, 0);

	SendDlgItemMessage (hTab, IDC_FRM, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_FRM, CB_ADDSTRING, 0, (LPARAM)"ecliptic");
	SendDlgItemMessage (hTab, IDC_FRM, CB_ADDSTRING, 0, (LPARAM)"ref. equator");
	SendDlgItemMessage (hTab, IDC_FRM, CB_SETCURSEL, 0, 0);

	ed->ScanCBodyList (hTab, IDC_REF, hRef);

	char cbuf[256];
	sprintf (cbuf, "%0.5f", elmjd);
	SetWindowText (GetDlgItem (hTab, IDC_EDIT7), cbuf);

	Refresh ();
}

char *EditorTab_Elements::HelpTopic ()
{
	return (char*)"/Elements.htm";
}

void EditorTab_Elements::Apply ()
{
	const double eps = 1e-7;
	char cbuf[256];
	int i;
	double mjd;
	GetWindowText (GetDlgItem (hTab, IDC_REF), cbuf, 256);
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	OBJHANDLE hRef = oapiGetGbodyByName (cbuf);
	if (hRef) {
		int frm = SendDlgItemMessage (hTab, IDC_FRM, CB_GETCURSEL, 0, 0);
		int epc = SendDlgItemMessage (hTab, IDC_COMBO6, CB_GETCURSEL, 0, 0);
		if (!epc) mjd = 0;
		else {
			GetWindowText (GetDlgItem (hTab, IDC_EDIT7), cbuf, 256);
			sscanf (cbuf, "%lf", &elmjd);
			mjd = (elmjd ? elmjd : 1e-10);
		}
		GetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf, 256);
		sscanf (cbuf, "%lf", &el.a);
		i = SendDlgItemMessage (hTab, IDC_COMBO1, CB_GETCURSEL, 0, 0);
		el.a /= lengthscale[i];
		GetWindowText (GetDlgItem (hTab, IDC_EDIT2), cbuf, 256);
		sscanf (cbuf, "%lf", &el.e);
		if (el.e >= 1 && el.e < 1+eps) el.e = 1+eps; // e=1 causes problems
		GetWindowText (GetDlgItem (hTab, IDC_EDIT3), cbuf, 256);
		sscanf (cbuf, "%lf", &el.i);
		i = SendDlgItemMessage (hTab, IDC_COMBO2, CB_GETCURSEL, 0, 0);
		el.i /= anglescale[i];
		GetWindowText (GetDlgItem (hTab, IDC_EDIT4), cbuf, 256);
		sscanf (cbuf, "%lf", &el.theta);
		i = SendDlgItemMessage (hTab, IDC_COMBO3, CB_GETCURSEL, 0, 0);
		el.theta /= anglescale[i];
		GetWindowText (GetDlgItem (hTab, IDC_EDIT5), cbuf, 256);
		sscanf (cbuf, "%lf", &el.omegab);
		i = SendDlgItemMessage (hTab, IDC_COMBO4, CB_GETCURSEL, 0, 0);
		el.omegab /= anglescale[i];
		GetWindowText (GetDlgItem (hTab, IDC_EDIT6), cbuf, 256);
		sscanf (cbuf, "%lf", &el.L);
		i = SendDlgItemMessage (hTab, IDC_COMBO5, CB_GETCURSEL, 0, 0);
		el.L /= anglescale[i];
		el.a = fabs (el.a);
		if (el.e > 1.0) el.a = -el.a;
		if (vessel->SetElements (hRef, el, &prm, mjd, frm))
			RefreshSecondaryParams (el, prm);
		else
			MessageBeep (-1);
		Refresh ();
	}
}

void EditorTab_Elements::Refresh ()
{
	char cbuf[256];
	double scale, mjd;
	GetWindowText (GetDlgItem (hTab, IDC_REF), cbuf, 256);
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	OBJHANDLE hRef = oapiGetGbodyByName (cbuf);
	if (!hRef) return;
	int frm = SendDlgItemMessage (hTab, IDC_FRM, CB_GETCURSEL, 0, 0);
	int epc = SendDlgItemMessage (hTab, IDC_COMBO6, CB_GETCURSEL, 0, 0);
	if (!epc) mjd = 0;
	else {
		GetWindowText (GetDlgItem (hTab, IDC_EDIT7), cbuf, 256);
		sscanf (cbuf, "%lf", &elmjd);
		mjd = (elmjd ? elmjd : 1e-10);
	}
	if (!vessel->GetElements (hRef, el, &prm, mjd, frm)) return;
	bool closed = (el.e < 1.0);
	int prec = (closed ? 6:10);
	lengthscale[3] = 1.0/oapiGetSize (hRef);
	scale = lengthscale[SendDlgItemMessage (hTab, IDC_COMBO1, CB_GETCURSEL, 0, 0)];
	sprintf (cbuf, "%0.10g", el.a*scale);
	SetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf);
	sprintf (cbuf, "%0.*g", prec, el.e);
	SetWindowText (GetDlgItem (hTab, IDC_EDIT2), cbuf);
	scale = anglescale[SendDlgItemMessage (hTab, IDC_COMBO2, CB_GETCURSEL, 0, 0)];
	sprintf (cbuf, "%0.*g", prec, el.i*scale);
	SetWindowText (GetDlgItem (hTab, IDC_EDIT3), cbuf);
	scale = anglescale[SendDlgItemMessage (hTab, IDC_COMBO3, CB_GETCURSEL, 0, 0)];
	sprintf (cbuf, "%0.*g", prec, el.theta*scale);
	SetWindowText (GetDlgItem (hTab, IDC_EDIT4), cbuf);
	scale = anglescale[SendDlgItemMessage (hTab, IDC_COMBO4, CB_GETCURSEL, 0, 0)];
	sprintf (cbuf, "%0.*g", prec, el.omegab*scale);
	SetWindowText (GetDlgItem (hTab, IDC_EDIT5), cbuf);
	scale = anglescale[SendDlgItemMessage (hTab, IDC_COMBO5, CB_GETCURSEL, 0, 0)];
	sprintf (cbuf, "%0.*g", prec, el.L*scale);
	SetWindowText (GetDlgItem (hTab, IDC_EDIT6), cbuf);
	RefreshSecondaryParams (el, prm);
}

void EditorTab_Elements::RefreshSecondaryParams (const ELEMENTS &el, const ORBITPARAM &prm)
{
	char cbuf[256];
	bool closed = (el.e < 1.0); // closed orbit?

	sprintf (cbuf, "%g m", prm.PeD); SetWindowText (GetDlgItem (hTab, IDC_PERIAPSIS), cbuf);
	sprintf (cbuf, "%g s", prm.PeT); SetWindowText (GetDlgItem (hTab, IDC_PET), cbuf);
	sprintf (cbuf, "%0.3f °", prm.MnA*DEG); SetWindowText (GetDlgItem (hTab, IDC_MNANM), cbuf);
	sprintf (cbuf, "%0.3f °", prm.TrA*DEG); SetWindowText (GetDlgItem (hTab, IDC_TRANM), cbuf);
	sprintf (cbuf, "%0.3f °", prm.MnL*DEG); SetWindowText (GetDlgItem (hTab, IDC_MNLNG), cbuf);
	sprintf (cbuf, "%0.3f °", prm.TrL*DEG); SetWindowText (GetDlgItem (hTab, IDC_TRLNG), cbuf);
	if (closed) {
		sprintf (cbuf, "%g s", prm.T);   SetWindowText (GetDlgItem (hTab, IDC_PERIOD), cbuf);
		sprintf (cbuf, "%g m", prm.ApD); SetWindowText (GetDlgItem (hTab, IDC_APOAPSIS), cbuf);
		sprintf (cbuf, "%g s", prm.ApT); SetWindowText (GetDlgItem (hTab, IDC_APT), cbuf);
	} else {
		SetWindowText (GetDlgItem (hTab, IDC_PERIOD), "N/A");
		SetWindowText (GetDlgItem (hTab, IDC_APOAPSIS), "N/A");
		SetWindowText (GetDlgItem (hTab, IDC_APT), "N/A");
	}
}
INT_PTR EditorTab_Elements::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	char cbuf[256];
	int i;

	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BACK:
			SwitchTab (3);
			return TRUE;
		case IDC_APPLY:
			Apply ();
			return TRUE;
		case IDC_REFRESH:
			Refresh ();
			return TRUE;
		case IDC_REF:
			if (HIWORD (wParam) == CBN_SELCHANGE || HIWORD (wParam) == CBN_EDITCHANGE) {
				PostMessage (hDlg, WM_COMMAND, IDC_REFRESH, 0);
				return TRUE;
			}
			break;
		case IDC_FRM:
			if (HIWORD (wParam) == CBN_SELCHANGE) {
				PostMessage (hDlg, WM_COMMAND, IDC_REFRESH, 0);
				return TRUE;
			}
			break;
		case IDC_COMBO1:
			if (HIWORD (wParam) == CBN_SELCHANGE) {
				i = SendDlgItemMessage (hDlg, IDC_COMBO1, CB_GETCURSEL, 0, 0);
				sprintf (cbuf, "%g", el.a * lengthscale[i]);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf);
				return TRUE;
			}
			break;
		case IDC_COMBO2:
			if (HIWORD (wParam) == CBN_SELCHANGE) {
				i = SendDlgItemMessage (hDlg, IDC_COMBO2, CB_GETCURSEL, 0, 0);
				sprintf (cbuf, "%g", el.i * anglescale[i]);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT3), cbuf);
				return TRUE;
			}
			break;
		case IDC_COMBO3:
			if (HIWORD (wParam) == CBN_SELCHANGE) {
				i = SendDlgItemMessage (hDlg, IDC_COMBO3, CB_GETCURSEL, 0, 0);
				sprintf (cbuf, "%g", el.theta * anglescale[i]);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT4), cbuf);
				return TRUE;
			}
			break;
		case IDC_COMBO4:
			if (HIWORD (wParam) == CBN_SELCHANGE) {
				i = SendDlgItemMessage (hDlg, IDC_COMBO4, CB_GETCURSEL, 0, 0);
				sprintf (cbuf, "%g", el.omegab * anglescale[i]);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT5), cbuf);
				return TRUE;
			}
			break;
		case IDC_COMBO5:
			if (HIWORD (wParam) == CBN_SELCHANGE) {
				i = SendDlgItemMessage (hDlg, IDC_COMBO5, CB_GETCURSEL, 0, 0);
				sprintf (cbuf, "%g", el.L * anglescale[i]);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT6), cbuf);
				return TRUE;
			}
			break;
		case IDC_COMBO6:
			if (HIWORD (wParam) == CBN_SELCHANGE) {
				i = SendDlgItemMessage (hDlg, IDC_COMBO6, CB_GETCURSEL, 0, 0);
				EnableWindow (GetDlgItem (hDlg, IDC_EDIT7), i != 0);
				return TRUE;
			}
			break;
		}
		break;
	case WM_NOTIFY:
		if (((NMHDR*)lParam)->code == UDN_DELTAPOS) {
			NMUPDOWN *nmud = (NMUPDOWN*)lParam;
			switch (((NMHDR*)lParam)->idFrom) {
			case IDC_SPIN1:
				el.a *= (1.0 - nmud->iDelta*1e-4);
				i = SendDlgItemMessage (hDlg, IDC_COMBO1, CB_GETCURSEL, 0, 0);
				sprintf (cbuf, "%g", el.a * lengthscale[i]);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf);
				Apply ();
				return TRUE;
			case IDC_SPIN2:
				el.e *= (1.0 - nmud->iDelta*0.001);
				if (el.e < 0.0) el.e = 0.0;
				sprintf (cbuf, "%g", el.e);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT2), cbuf);
				Apply ();
				return TRUE;
			case IDC_SPIN3:
				el.i -= nmud->iDelta*RAD*0.1;
				if      (el.i >  PI) el.i -= 2.0*PI;
				else if (el.i < -PI) el.i += 2.0*PI;
				i = SendDlgItemMessage (hDlg, IDC_COMBO2, CB_GETCURSEL, 0, 0);
				sprintf (cbuf, "%g", el.i * anglescale[i]);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT3), cbuf);
				Apply ();
				return TRUE;
			case IDC_SPIN4:
				el.theta -= nmud->iDelta*RAD*0.1;
				if      (el.theta >= 2.0*PI) el.theta -= 2.0*PI;
				else if (el.theta <  0.0)    el.theta += 2.0*PI;
				i = SendDlgItemMessage (hDlg, IDC_COMBO3, CB_GETCURSEL, 0, 0);
				sprintf (cbuf, "%g", el.theta * anglescale[i]);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT4), cbuf);
				Apply ();
				return TRUE;
			case IDC_SPIN5:
				el.omegab -= nmud->iDelta*RAD*0.1;
				if      (el.omegab >= 2.0*PI) el.omegab -= 2.0*PI;
				else if (el.omegab <  0.0)    el.omegab += 2.0*PI;
				i = SendDlgItemMessage (hDlg, IDC_COMBO4, CB_GETCURSEL, 0, 0);
				sprintf (cbuf, "%g", el.omegab * anglescale[i]);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT5), cbuf);
				Apply ();
				return TRUE;
			case IDC_SPIN6:
				el.L -= nmud->iDelta*RAD*0.1;
				if      (el.L >= 2.0*PI) el.L -= 2.0*PI;
				else if (el.L <  0.0)    el.L += 2.0*PI;
				i = SendDlgItemMessage (hDlg, IDC_COMBO5, CB_GETCURSEL, 0, 0);
				sprintf (cbuf, "%g", el.L * anglescale[i]);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT6), cbuf);
				Apply ();
				return TRUE;
			}
		}
		break;
	}
	return ScnEditorTab::TabProc (hDlg, uMsg, wParam, lParam);
}

INT_PTR EditorTab_Elements::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_Elements *pTab = (EditorTab_Elements*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}

// ==============================================================
// EditorTab_Statevec class definition
// ==============================================================

EditorTab_Statevec::EditorTab_Statevec (ScnEditor *editor) : ScnEditorTab (editor)
{
	CreateTab (IDD_TAB_EDIT3, EditorTab_Statevec::DlgProc);
}

void EditorTab_Statevec::InitTab ()
{
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	OBJHANDLE hRef = vessel->GetGravityRef();

	SendDlgItemMessage (hTab, IDC_FRM, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_FRM, CB_ADDSTRING, 0, (LPARAM)"ecliptic");
	SendDlgItemMessage (hTab, IDC_FRM, CB_ADDSTRING, 0, (LPARAM)"ref. equator (fixed)");
	SendDlgItemMessage (hTab, IDC_FRM, CB_ADDSTRING, 0, (LPARAM)"ref. equator (rotating)");
	SendDlgItemMessage (hTab, IDC_FRM, CB_SETCURSEL, 0, 0);

	SendDlgItemMessage (hTab, IDC_CRD, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_CRD, CB_ADDSTRING, 0, (LPARAM)"cartesian");
	SendDlgItemMessage (hTab, IDC_CRD, CB_ADDSTRING, 0, (LPARAM)"polar");
	SendDlgItemMessage (hTab, IDC_CRD, CB_SETCURSEL, 0, 0);

	DlgLabels ();
	ed->ScanCBodyList (hTab, IDC_REF, hRef);
	ScanVesselList ();
	Refresh ();
}

void EditorTab_Statevec::ScanVesselList ()
{
	char cbuf[256];

	// populate vessel list
	SendDlgItemMessage (hTab, IDC_STATECPY, LB_RESETCONTENT, 0, 0);
	for (DWORD i = 0; i < oapiGetVesselCount(); i++) {
		OBJHANDLE hV = oapiGetVesselByIndex (i);
		if (hV == ed->hVessel) continue;                  // skip myself
		VESSEL *vessel = oapiGetVesselInterface (hV);
		if ((vessel->GetFlightStatus() & 1) == 1) continue; // skip landed vessels
		strcpy (cbuf, vessel->GetName());
		SendDlgItemMessage (hTab, IDC_STATECPY, LB_ADDSTRING, 0, (LPARAM)cbuf);
	}
}

char *EditorTab_Statevec::HelpTopic ()
{
	return (char*)"/Statevec.htm";
}

void EditorTab_Statevec::DlgLabels ()
{
	int crd = SendDlgItemMessage (hTab, IDC_CRD, CB_GETCURSEL, 0, 0);
	SetWindowText (GetDlgItem (hTab, IDC_STATIC1A), crd ? "radius" : "x");
	SetWindowText (GetDlgItem (hTab, IDC_STATIC2A), crd ? "longitude" : "y");
	SetWindowText (GetDlgItem (hTab, IDC_STATIC3A), crd ? "latitude" : "z");
	SetWindowText (GetDlgItem (hTab, IDC_STATIC4A), crd ? "d radius / dt" : "dx / dt");
	SetWindowText (GetDlgItem (hTab, IDC_STATIC5A), crd ? "d longitude / dt" : "dy / dt");
	SetWindowText (GetDlgItem (hTab, IDC_STATIC6A), crd ? "d latitude / dt" : "dz / dt");
	SetWindowText (GetDlgItem (hTab, IDC_STATIC2), crd ? "deg" : "m");
	SetWindowText (GetDlgItem (hTab, IDC_STATIC3), crd ? "deg" : "m");
	SetWindowText (GetDlgItem (hTab, IDC_STATIC5), crd ? "deg/s" : "m/s");
	SetWindowText (GetDlgItem (hTab, IDC_STATIC6), crd ? "deg/s" : "m/s");
}

INT_PTR EditorTab_Statevec::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BACK:
			SwitchTab (3);
			return TRUE;
		case IDC_APPLY:
			Apply ();
			return TRUE;
		case IDC_REFRESH:
			Refresh ();
			return TRUE;
		case IDC_REF:
			if (HIWORD (wParam) == CBN_SELCHANGE || HIWORD (wParam) == CBN_EDITCHANGE) {
				PostMessage (hDlg, WM_COMMAND, IDC_REFRESH, 0);
				return TRUE;
			}
			break;
		case IDC_FRM:
			if (HIWORD (wParam) == CBN_SELCHANGE) {
				PostMessage (hDlg, WM_COMMAND, IDC_REFRESH, 0);
				return TRUE;
			}
			break;
		case IDC_CRD:
			if (HIWORD (wParam) == CBN_SELCHANGE) {
				DlgLabels ();
				PostMessage (hDlg, WM_COMMAND, IDC_REFRESH, 0);
				return TRUE;
			}
			break;
		case IDC_STATECPY: {
			OBJHANDLE hV = GetVesselFromList (IDC_STATECPY);
			switch (HIWORD(wParam)) {
			case LBN_SELCHANGE:
				Refresh (hV);
				break;
			case LBN_DBLCLK:
				Refresh (hV);
				Apply ();
				break;
			}
			} break;
		}
		break;
	case WM_NOTIFY:
		if (((NMHDR*)lParam)->code == UDN_DELTAPOS) {
			char cbuf[256];
			int crd = SendDlgItemMessage (hTab, IDC_CRD, CB_GETCURSEL, 0, 0);
			int prec, idx = 0;
			double val, dv;
			NMUPDOWN *nmud = (NMUPDOWN*)lParam;
			switch (((NMHDR*)lParam)->idFrom) {
			case IDC_SPIN1:
				idx = IDC_EDIT1; prec = 1; dv = -nmud->iDelta*1.0;
				break;
			case IDC_SPIN1A:
				idx = IDC_EDIT1; prec = 1; dv = -nmud->iDelta*1000.0;
				break;
			case IDC_SPIN2:
				idx = IDC_EDIT2; prec = (crd?6:1); dv = -nmud->iDelta*(crd?0.0001:1.0);
				break;
			case IDC_SPIN2A:
				idx = IDC_EDIT2; prec = (crd?6:1); dv = -nmud->iDelta*(crd?0.1:1000.0);
				break;
			case IDC_SPIN3:
				idx = IDC_EDIT3; prec = (crd?6:1); dv = -nmud->iDelta*(crd?0.0001:1.0);
				break;
			case IDC_SPIN3A:
				idx = IDC_EDIT3; prec = (crd?6:1); dv = -nmud->iDelta*(crd?0.1:1000.0);
				break;
			case IDC_SPIN4:
				idx = IDC_EDIT4; prec = 2; dv = -nmud->iDelta*0.1;
				break;
			case IDC_SPIN4A:
				idx = IDC_EDIT4; prec = 2; dv = -nmud->iDelta*100.0;
				break;
			case IDC_SPIN5:
				idx = IDC_EDIT5; prec = (crd?7:2); dv = -nmud->iDelta*(crd?1e-5:0.1);
				break;
			case IDC_SPIN5A:
				idx = IDC_EDIT5; prec = (crd?7:2); dv = -nmud->iDelta*(crd?1e-2:100.0);
				break;
			case IDC_SPIN6:
				idx = IDC_EDIT6; prec = (crd?7:2); dv = -nmud->iDelta*(crd?1e-5:0.1);
				break;
			case IDC_SPIN6A:
				idx = IDC_EDIT6; prec = (crd?7:2); dv = -nmud->iDelta*(crd?1e-2:100.0);
				break;
			}
			if (idx) {
				GetWindowText (GetDlgItem (hDlg, idx), cbuf, 256);
				sscanf (cbuf, "%lf", &val);
				val += dv;
				sprintf (cbuf, "%0.*f", prec, val);
				SetWindowText (GetDlgItem (hDlg, idx), cbuf);
				Apply ();
				return TRUE;
			}
		}
		break;
	}
	return ScnEditorTab::TabProc (hDlg, uMsg, wParam, lParam);
}

void EditorTab_Statevec::Refresh (OBJHANDLE hV)
{
	if (!hV) hV = ed->hVessel;
	char cbuf[256];
	GetWindowText (GetDlgItem (hTab, IDC_REF), cbuf, 256);
	VESSEL *vessel = oapiGetVesselInterface (hV);
	OBJHANDLE hRef = oapiGetGbodyByName (cbuf);
	if (!hRef) return;
	int frm = SendDlgItemMessage (hTab, IDC_FRM, CB_GETCURSEL, 0, 0);
	int crd = SendDlgItemMessage (hTab, IDC_CRD, CB_GETCURSEL, 0, 0);
	VECTOR3 pos, vel;
	oapiGetRelativePos (hV, hRef, &pos);
	oapiGetRelativeVel (hV, hRef, &vel);
	// map ecliptic -> equatorial frame
	if (frm) {
		MATRIX3 rot;
		if (frm == 1) oapiGetPlanetObliquityMatrix (hRef, &rot);
		else          oapiGetRotationMatrix (hRef, &rot);
		pos = tmul (rot, pos);
		vel = tmul (rot, vel);
	}
	// map cartesian -> polar coordinates
	if (crd) {
		Crt2Pol (pos, vel);
		pos.data[1] *= DEG; pos.data[2] *= DEG;
		vel.data[1] *= DEG; vel.data[2] *= DEG;
	}
	// in the rotating reference frame we need to subtract the angular
	// velocity of the planet
	if (frm == 2) {
		double T = oapiGetPlanetPeriod (hRef);
		if (crd) {
			vel.data[1] -= 360.0/T;
		} else { // map back to cartesian
			double r   = std::hypot (pos.x, pos.z);
			double phi = atan2 (pos.z, pos.x);
			double v   = 2.0*PI*r/T;
			vel.x     += v*sin(phi);
			vel.z     -= v*cos(phi);
		}
	}
	sprintf (cbuf, "%0.1f", pos.x); SetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf);
	sprintf (cbuf, "%0.*f", (crd?6:1), pos.y); SetWindowText (GetDlgItem (hTab, IDC_EDIT2), cbuf);
	sprintf (cbuf, "%0.*f", (crd?6:1), pos.z); SetWindowText (GetDlgItem (hTab, IDC_EDIT3), cbuf);
	sprintf (cbuf, "%0.2f", vel.x); SetWindowText (GetDlgItem (hTab, IDC_EDIT4), cbuf);
	sprintf (cbuf, "%0.*f", (crd?7:2), vel.y); SetWindowText (GetDlgItem (hTab, IDC_EDIT5), cbuf);
	sprintf (cbuf, "%0.*f", (crd?7:2), vel.z); SetWindowText (GetDlgItem (hTab, IDC_EDIT6), cbuf);
}

void EditorTab_Statevec::Apply ()
{
	char cbuf[256];
	GetWindowText (GetDlgItem (hTab, IDC_REF), cbuf, 256);
	VESSEL *vessel = Vessel();
	OBJHANDLE hRef = oapiGetGbodyByName (cbuf);
	if (hRef) {
		bool needrefresh = false;
		MATRIX3 rot;
		VECTOR3 pos, vel, refpos, refvel;
		VESSELSTATUS vs;
		GetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf, 256);
		sscanf (cbuf, "%lf", &pos.x);
		GetWindowText (GetDlgItem (hTab, IDC_EDIT2), cbuf, 256);
		sscanf (cbuf, "%lf", &pos.y);
		GetWindowText (GetDlgItem (hTab, IDC_EDIT3), cbuf, 256);
		sscanf (cbuf, "%lf", &pos.z);
		GetWindowText (GetDlgItem (hTab, IDC_EDIT4), cbuf, 256);
		sscanf (cbuf, "%lf", &vel.x);
		GetWindowText (GetDlgItem (hTab, IDC_EDIT5), cbuf, 256);
		sscanf (cbuf, "%lf", &vel.y);
		GetWindowText (GetDlgItem (hTab, IDC_EDIT6), cbuf, 256);
		sscanf (cbuf, "%lf", &vel.z);
		int frm = SendDlgItemMessage (hTab, IDC_FRM, CB_GETCURSEL, 0, 0);
		int crd = SendDlgItemMessage (hTab, IDC_CRD, CB_GETCURSEL, 0, 0);
		// in the rotating reference frame we need to add the angular
		// velocity of the planet
		if (frm == 2) {
			double T = oapiGetPlanetPeriod (hRef);
			if (crd) {
				vel.data[1] += 360.0/T;
			} else { // map back to cartesian
				double r   = std::hypot (pos.x, pos.z);
				double phi = atan2 (pos.z, pos.x);
				double v   = 2.0*PI*r/T;
				vel.x     -= v*sin(phi);
				vel.z     += v*cos(phi);
			}
		}
		// map polar -> cartesian coordinates
		if (crd) {
			pos.data[1] *= RAD, pos.data[2] *= RAD;
			vel.data[1] *= RAD, vel.data[2] *= RAD;
			Pol2Crt (pos, vel);
		}
		// map from celestial/equatorial frame of reference
		if (frm) {
			if (frm == 1) oapiGetPlanetObliquityMatrix (hRef, &rot);
			else          oapiGetRotationMatrix (hRef, &rot);
			pos = mul (rot, pos);
			vel = mul (rot, vel);
		}
		// change reference in case the selected reference object is
		// not the same as the VESSELSTATUS reference
		vessel->GetStatus (vs);
		oapiGetGlobalPos (hRef, &refpos);     pos += refpos;
		oapiGetGlobalVel (hRef, &refvel);     vel += refvel;
		oapiGetGlobalPos (vs.rbody, &refpos); pos -= refpos;
		oapiGetGlobalVel (vs.rbody, &refvel); vel -= refvel;
		if (vs.status != 0) { // enforce freeflight mode
			vs.status = 0;
			vessel->GetRotationMatrix (rot);
			vs.arot.x = atan2(rot.m23, rot.m33);
			vs.arot.y = -asin(rot.m13);
			vs.arot.z = atan2(rot.m12, rot.m11);
			vessel->GetAngularVel(vs.vrot);
		}
#ifdef UNDEF
		// sanity check
		double rad = length(pos);
		double rad0 = oapiGetSize (vs.rbody) + vessel->GetCOG_elev();
		if (rad < rad0) {
			Crt2Pol (pos, vel);
			pos.x = rad0;
			vel.x = 0.0;
			Pol2Crt (pos, vel);
			needrefresh = true;
		}
#endif
		veccpy (vs.rpos, pos);
		veccpy (vs.rvel, vel);
		vessel->DefSetState (&vs);
		if (needrefresh) Refresh();
	}
}

INT_PTR EditorTab_Statevec::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_Statevec *pTab = (EditorTab_Statevec*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}

// ==============================================================
// EditorTab_Landed class definition
// ==============================================================

EditorTab_Landed::EditorTab_Landed (ScnEditor *editor) : ScnEditorTab (editor)
{
	CreateTab (IDD_TAB_EDIT4, EditorTab_Landed::DlgProc);
}

void EditorTab_Landed::InitTab ()
{
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	OBJHANDLE hRef = vessel->GetGravityRef();

	ScanCBodyList (hTab, IDC_REF, hRef);
	ScanBaseList (hTab, IDC_BASE, hRef);
	ScanVesselList ();
	Refresh ();
}

void EditorTab_Landed::ScanVesselList ()
{
	char cbuf[256];

	// populate vessel list
	SendDlgItemMessage (hTab, IDC_STATECPY, LB_RESETCONTENT, 0, 0);
	for (DWORD i = 0; i < oapiGetVesselCount(); i++) {
		OBJHANDLE hV = oapiGetVesselByIndex (i);
		if (hV == ed->hVessel) continue;                  // skip myself
		VESSEL *vessel = oapiGetVesselInterface (hV);
		if ((vessel->GetFlightStatus() & 1) == 0) continue; // skip vessels in flight
		strcpy (cbuf, vessel->GetName());
		SendDlgItemMessage (hTab, IDC_STATECPY, LB_ADDSTRING, 0, (LPARAM)cbuf);
	}
}

char *EditorTab_Landed::HelpTopic ()
{
	return (char*)"/Location.htm";
}

INT_PTR EditorTab_Landed::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BACK:
			SwitchTab (3);
			return TRUE;
		case IDC_APPLY:
			Apply ();
			return TRUE;
		case IDC_REFRESH:
			if (lParam == 1) { // rescan bases
				char cbuf[256];
				GetWindowText (GetDlgItem (hDlg, IDC_REF), cbuf, 256);
				OBJHANDLE hRef = oapiGetGbodyByName (cbuf);
				ScanBaseList (hDlg, IDC_BASE, hRef);
			}
			Refresh ();
			return TRUE;
		case IDC_REF:
			if (HIWORD (wParam) == CBN_SELCHANGE || HIWORD (wParam) == CBN_EDITCHANGE) {
				PostMessage (hDlg, WM_USER+0, 0, 0);
				return TRUE;
			}
		case IDC_BASE:
			if (HIWORD (wParam) == CBN_SELCHANGE || HIWORD (wParam) == CBN_EDITCHANGE) {
				PostMessage (hDlg, WM_USER+1, 0, 0);
				return TRUE;
			}
			break;
		case IDC_PAD:
			if (HIWORD (wParam) == CBN_SELCHANGE || HIWORD (wParam) == CBN_EDITCHANGE) {
				ed->SetBasePosition (hDlg);
				PostMessage (hDlg, WM_COMMAND, IDC_APPLY, 0);
				return TRUE;
			}
			break;
		case IDC_STATECPY: {
			OBJHANDLE hV = GetVesselFromList (IDC_STATECPY);
			switch (HIWORD(wParam)) {
			case LBN_SELCHANGE:
				Refresh (hV);
				break;
			case LBN_DBLCLK:
				Refresh (hV);
				Apply ();
				break;
			}
			} break;
		}
		break;
	case WM_USER+0: { // reference body changed
		char cbuf[256];
		GetWindowText (GetDlgItem (hDlg, IDC_REF), cbuf, 256);
		OBJHANDLE hRef = oapiGetGbodyByName (cbuf);
		if (!hRef) break;
		ScanBaseList (hDlg, IDC_BASE, hRef);
		PostMessage (hDlg, WM_USER+1, 0, 0);
		} return TRUE;
	case WM_USER+1: { // base changed
		char cbuf[256];
		GetWindowText (GetDlgItem (hDlg, IDC_REF), cbuf, 256);
		OBJHANDLE hRef = oapiGetGbodyByName (cbuf);
		if (!hRef) break;
		GetWindowText (GetDlgItem (hDlg, IDC_BASE), cbuf, 256);
		OBJHANDLE hBase = oapiGetBaseByName (hRef, cbuf);
		ed->ScanPadList (hDlg, IDC_PAD, hBase);
		ed->SetBasePosition (hDlg);
		PostMessage (hDlg, WM_COMMAND, IDC_APPLY, 0);
		} return TRUE;
	case WM_NOTIFY:
		if (((NMHDR*)lParam)->code == UDN_DELTAPOS) {
			NMUPDOWN *nmud = (NMUPDOWN*)lParam;
			char cbuf[256];
			double val;
			int id = ((NMHDR*)lParam)->idFrom;
			switch (id) {
			case IDC_SPIN1:
			case IDC_SPIN1A:
				GetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf, 256);
				sscanf (cbuf, "%lf", &val);
				val -= nmud->iDelta * (id == IDC_SPIN1 ? 0.00001 : 0.001);
				if      (val < -180.0) val += 360.0;
				else if (val > +180.0) val -= 360.0;
				sprintf (cbuf, "%lf", val);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf);
				Apply ();
				return TRUE;
			case IDC_SPIN2:
			case IDC_SPIN2A:
				GetWindowText (GetDlgItem (hDlg, IDC_EDIT2), cbuf, 256);
				sscanf (cbuf, "%lf", &val);
				val -= nmud->iDelta * (id == IDC_SPIN2 ? 0.00001 : 0.001);
				val = min (90.0, max (-90.0, val));
				sprintf (cbuf, "%lf", val);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT2), cbuf);
				Apply ();
				return TRUE;
			case IDC_SPIN3:
			case IDC_SPIN3A:
				GetWindowText (GetDlgItem (hDlg, IDC_EDIT3), cbuf, 256);
				sscanf (cbuf, "%lf", &val);
				val -= nmud->iDelta * (id == IDC_SPIN3 ? 0.01 : 1.0);
				if      (val <   0.0) val += 360.0;
				else if (val > 360.0) val -= 360.0;
				sprintf (cbuf, "%lf", val);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT3), cbuf);
				Apply ();
				return TRUE;
			}
		}
		break;
	}
	return ScnEditorTab::TabProc (hDlg, uMsg, wParam, lParam);
}

void EditorTab_Landed::Refresh (OBJHANDLE hV)
{
	char cbuf[256], lngstr[64], latstr[64];
	bool scancbody = ((hV != NULL) && (hV != ed->hVessel));
	if (!hV) hV = ed->hVessel;
	VESSEL *vessel = oapiGetVesselInterface (hV);
	if (scancbody) SelectCBody (vessel->GetSurfaceRef());
	GetWindowText (GetDlgItem (hTab, IDC_REF), cbuf, 256);
	OBJHANDLE hRef = oapiGetGbodyByName (cbuf);
	if (!hRef) return;

	VESSELSTATUS2 vs;
	memset (&vs, 0, sizeof(vs)); vs.version = 2;
	vessel->GetStatusEx (&vs);
	if (vs.rbody == hRef) {
		sprintf (lngstr, "%lf", vs.surf_lng * DEG);
		sprintf (latstr, "%lf", vs.surf_lat * DEG);
		ed->SelectBase (hTab, IDC_BASE, hRef, vs.base);
	} else {
		// calculate ground position
		VECTOR3 pos;
		MATRIX3 rot;
		oapiGetRelativePos (ed->hVessel, hRef, &pos);
		oapiGetRotationMatrix (hRef, &rot);
		pos = tmul (rot, pos);
		VECTOR3 vel = _V(0,0,0);
		Crt2Pol (pos, vel);
		sprintf (lngstr, "%lf", pos.data[1] * DEG);
		sprintf (latstr, "%lf", pos.data[2] * DEG);
	}
	SetWindowText (GetDlgItem (hTab, IDC_EDIT1), lngstr);
	SetWindowText (GetDlgItem (hTab, IDC_EDIT2), latstr);

	sprintf (cbuf, "%lf", vs.surf_hdg * DEG);
	SetWindowText (GetDlgItem (hTab, IDC_EDIT3), cbuf);
}

void EditorTab_Landed::Apply ()
{
	char cbuf[256];
	GetWindowText (GetDlgItem (hTab, IDC_REF), cbuf, 256);
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	OBJHANDLE hRef = oapiGetGbodyByName (cbuf);
	if (!hRef) return;
	VESSELSTATUS2 vs;
	memset (&vs, 0, sizeof(vs));
	vs.version = 2;
	vs.rbody = hRef;
	vs.status = 1; // landed
	vs.arot.x = 10; // use default touchdown orientation
	GetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf, 256);
	sscanf (cbuf, "%lf", &vs.surf_lng); vs.surf_lng *= RAD;
	GetWindowText (GetDlgItem (hTab, IDC_EDIT2), cbuf, 256);
	sscanf (cbuf, "%lf", &vs.surf_lat); vs.surf_lat *= RAD;
	GetWindowText (GetDlgItem (hTab, IDC_EDIT3), cbuf, 256);
	sscanf (cbuf, "%lf", &vs.surf_hdg); vs.surf_hdg *= RAD;
	vessel->DefSetStateEx (&vs);
}

void EditorTab_Landed::ScanCBodyList (HWND hDlg, int hList, OBJHANDLE hSelect)
{
	// populate a list of celestial bodies
	char cbuf[256];
	SendDlgItemMessage (hDlg, hList, CB_RESETCONTENT, 0, 0);
	for (DWORD n = 0; n < oapiGetGbodyCount(); n++) {
		OBJHANDLE hBody = oapiGetGbodyByIndex (n);
		if (oapiGetObjectType (hBody) == OBJTP_STAR) continue; // skip stars
		oapiGetObjectName (hBody, cbuf, 256);
		SendDlgItemMessage (hDlg, hList, CB_ADDSTRING, 0, (LPARAM)cbuf);
	}
	// select the requested body
	oapiGetObjectName (hSelect, cbuf, 256);
	SendDlgItemMessage (hDlg, hList, CB_SELECTSTRING, -1, (LPARAM)cbuf);
}

void EditorTab_Landed::ScanBaseList (HWND hDlg, int hList, OBJHANDLE hRef)
{
	char cbuf[256];
	DWORD n;

	SendDlgItemMessage (hDlg, hList, CB_RESETCONTENT, 0, 0);
	for (n = 0; n < oapiGetBaseCount (hRef); n++) {
		oapiGetObjectName (oapiGetBaseByIndex (hRef, n), cbuf, 256);
		SendDlgItemMessage (hDlg, hList, CB_ADDSTRING, 0, (LPARAM)cbuf);
	}
}

void EditorTab_Landed::SelectCBody (OBJHANDLE hBody)
{
	char cbuf[256];
	oapiGetObjectName (hBody, cbuf, 256);
	int idx = SendDlgItemMessage (hTab, IDC_REF, CB_FINDSTRING, -1, (LPARAM)cbuf);
	if (idx != LB_ERR) {
		SendDlgItemMessage (hTab, IDC_REF, CB_SETCURSEL, idx, 0);
		PostMessage (hTab, WM_USER+0, 0, 0);
	}
}

INT_PTR EditorTab_Landed::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_Landed *pTab = (EditorTab_Landed*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}


// ==============================================================
// EditorTab_Orientation class definition
// ==============================================================

EditorTab_Orientation::EditorTab_Orientation (ScnEditor *editor) : ScnEditorTab (editor)
{
	CreateTab (IDD_TAB_EDIT5, EditorTab_Orientation::DlgProc);
}

void EditorTab_Orientation::InitTab ()
{
	Refresh();
}

char *EditorTab_Orientation::HelpTopic ()
{
	return (char*)"/Orientation.htm";
}

INT_PTR EditorTab_Orientation::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BACK:
			SwitchTab (3);
			return TRUE;
		case IDC_REFRESH:
			Refresh ();
			return TRUE;
		case IDC_APPLY:
			Apply ();
			return TRUE;
		}
		break;
	case WM_NOTIFY:
		if (((NMHDR*)lParam)->code == UDN_DELTAPOS) {
			NMUPDOWN *nmud = (NMUPDOWN*)lParam;
			char cbuf[256];
			double val;
			int id = ((NMHDR*)lParam)->idFrom;
			switch (id) {
			case IDC_SPIN1:
			case IDC_SPIN1A:
				GetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf, 256);
				sscanf (cbuf, "%lf", &val);
				val -= nmud->iDelta * (id == IDC_SPIN1 ? 0.001 : 0.1);
				if      (val < -180.0) val += 360.0;
				else if (val > +180.0) val -= 360.0;
				sprintf (cbuf, "%lf", val);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf);
				Apply ();
				return TRUE;
			case IDC_SPIN2:
			case IDC_SPIN2A:
				GetWindowText (GetDlgItem (hDlg, IDC_EDIT2), cbuf, 256);
				sscanf (cbuf, "%lf", &val);
				val -= nmud->iDelta * (id == IDC_SPIN2 ? 0.001 : 0.1);
				val = min (90.0, max (-90.0, val));
				sprintf (cbuf, "%lf", val);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT2), cbuf);
				Apply ();
				return TRUE;
			case IDC_SPIN3:
			case IDC_SPIN3A:
				GetWindowText (GetDlgItem (hDlg, IDC_EDIT3), cbuf, 256);
				sscanf (cbuf, "%lf", &val);
				val -= nmud->iDelta * (id == IDC_SPIN3 ? 0.001 : 0.1);
				if      (val <   0.0) val += 360.0;
				else if (val > 360.0) val -= 360.0;
				sprintf (cbuf, "%lf", val);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT3), cbuf);
				Apply ();
				return TRUE;
			case IDC_SPIN4:
			case IDC_SPIN5:
			case IDC_SPIN6:
				Rotate (id-IDC_SPIN4, nmud->iDelta * -0.005);
				return TRUE;
			}
		}
		break;
	}
	return ScnEditorTab::TabProc (hDlg, uMsg, wParam, lParam);
}

void EditorTab_Orientation::Refresh ()
{
	int i;
	char cbuf[256];
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	VECTOR3 arot;
	vessel->GetGlobalOrientation (arot);
	for (i = 0; i < 3; i++) {
		sprintf (cbuf, "%lf", arot.data[i] * DEG);
		SetWindowText (GetDlgItem (hTab, IDC_EDIT1+i), cbuf);
	}
}

void EditorTab_Orientation::Apply ()
{
	int i;
	char cbuf[256];
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	VECTOR3 arot;
	for (i = 0; i < 3; i++) {
		GetWindowText (GetDlgItem (hTab, IDC_EDIT1+i), cbuf, 256);
		sscanf (cbuf, "%lf", &arot.data[i]);
		arot.data[i] *= RAD;
	}
	vessel->SetGlobalOrientation (arot);
}

void EditorTab_Orientation::ApplyAngularVel ()
{
	int i;
	char cbuf[256];
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	VECTOR3 avel;
	for (i = 0; i < 3; i++) {
		GetWindowText (GetDlgItem (hTab, IDC_EDIT4+i), cbuf, 256);
		sscanf (cbuf, "%lf", &avel.data[i]);
		avel.data[i] *= RAD;
	}
	vessel->SetAngularVel (avel);
}

void EditorTab_Orientation::Rotate (int axis, double da)
{
	MATRIX3 R, R2;
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	vessel->GetRotationMatrix (R);
	double sina = sin(da), cosa = cos(da);
	switch (axis) {
	case 0: // pitch
		R2 = _M(1,0,0,  0,cosa,sina,  0,-sina,cosa);
		break;
	case 1: // yaw
		R2 = _M(cosa,0,sina,  0,1,0,  -sina,0,cosa);
		break;
	case 2: // bank
		R2 = _M(cosa,sina,0,  -sina,cosa,0,  0,0,1);
		break;
	}
	vessel->SetRotationMatrix (mul (R,R2));
	Refresh ();
}

INT_PTR EditorTab_Orientation::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_Orientation *pTab = (EditorTab_Orientation*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}

// ==============================================================
// EditorTab_AngularVel class definition
// ==============================================================

EditorTab_AngularVel::EditorTab_AngularVel (ScnEditor *editor) : ScnEditorTab (editor)
{
	CreateTab (IDD_TAB_EDIT6, EditorTab_AngularVel::DlgProc);
}

void EditorTab_AngularVel::InitTab ()
{
	Refresh();
}

char *EditorTab_AngularVel::HelpTopic ()
{
	return (char*)"/AngularVel.htm";
}

INT_PTR EditorTab_AngularVel::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BACK:
			SwitchTab (3);
			return TRUE;
		case IDC_REFRESH:
			Refresh ();
			return TRUE;
		case IDC_APPLY:
			Apply ();
			return TRUE;
		case IDC_KILLROT:
			Killrot ();
			return TRUE;
		}
		break;
	case WM_NOTIFY:
		if (((NMHDR*)lParam)->code == UDN_DELTAPOS) {
			NMUPDOWN *nmud = (NMUPDOWN*)lParam;
			char cbuf[256];
			double val;
			int id = ((NMHDR*)lParam)->idFrom;
			switch (id) {
			case IDC_SPIN1:
			case IDC_SPIN1A:
				GetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf, 256);
				sscanf (cbuf, "%lf", &val);
				val -= nmud->iDelta * (id == IDC_SPIN4 ? 0.001 : 0.1);
				sprintf (cbuf, "%lf", val);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf);
				Apply ();
				return TRUE;
			case IDC_SPIN2:
			case IDC_SPIN2A:
				GetWindowText (GetDlgItem (hDlg, IDC_EDIT2), cbuf, 256);
				sscanf (cbuf, "%lf", &val);
				val -= nmud->iDelta * (id == IDC_SPIN2 ? 0.001 : 0.1);
				sprintf (cbuf, "%lf", val);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT2), cbuf);
				Apply ();
				return TRUE;
			case IDC_SPIN3:
			case IDC_SPIN3A:
				GetWindowText (GetDlgItem (hDlg, IDC_EDIT3), cbuf, 256);
				sscanf (cbuf, "%lf", &val);
				val -= nmud->iDelta * (id == IDC_SPIN3 ? 0.001 : 0.1);
				sprintf (cbuf, "%lf", val);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT3), cbuf);
				Apply ();
				return TRUE;
			}
		}
		break;
	}
	return ScnEditorTab::TabProc (hDlg, uMsg, wParam, lParam);
}

void EditorTab_AngularVel::Refresh ()
{
	char cbuf[256];
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	VECTOR3 avel;
	vessel->GetAngularVel (avel);
	for (int i = 0; i < 3; i++) {
		sprintf (cbuf, "%lf", avel.data[i] * DEG);
		SetWindowText (GetDlgItem (hTab, IDC_EDIT1+i), cbuf);
	}
}

void EditorTab_AngularVel::Apply ()
{
	char cbuf[256];
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	VECTOR3 avel;
	for (int i = 0; i < 3; i++) {
		GetWindowText (GetDlgItem (hTab, IDC_EDIT1+i), cbuf, 256);
		sscanf (cbuf, "%lf", &avel.data[i]);
		avel.data[i] *= RAD;
	}
	vessel->SetAngularVel (avel);
}

void EditorTab_AngularVel::Killrot ()
{
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	VECTOR3 avel;
	for (int i = 0; i < 3; i++) {
		avel.data[i] = 0.0;
		SetWindowText (GetDlgItem (hTab, IDC_EDIT1+i), "0");
	}
	vessel->SetAngularVel (avel);

}

INT_PTR EditorTab_AngularVel::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_AngularVel *pTab = (EditorTab_AngularVel*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}


// ==============================================================
// EditorTab_Propellant class definition
// ==============================================================

EditorTab_Propellant::EditorTab_Propellant (ScnEditor *editor) : ScnEditorTab (editor)
{
	CreateTab (IDD_TAB_EDIT7, EditorTab_Propellant::DlgProc);
}

void EditorTab_Propellant::InitTab ()
{
	GAUGEPARAM gp = { 0, 100, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
	oapiSetGaugeParams (GetDlgItem (hTab, IDC_PROPLEVEL), &gp);
	lastedit = IDC_EDIT2;
	Refresh();
}

char *EditorTab_Propellant::HelpTopic ()
{
	return (char*)"/Propellant.htm";
}

INT_PTR EditorTab_Propellant::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BACK:
			SwitchTab (3);
			return TRUE;
		case IDC_REFRESH:
			Refresh ();
			return TRUE;
		case IDC_APPLY:
			Apply ();
			return TRUE;
		case IDC_EMPTY:
			SetLevel (0);
			return TRUE;
		case IDC_FULL:
			SetLevel (1);
			return TRUE;
		case IDC_EMPTYALL:
			SetLevel (0, true);
			return TRUE;
		case IDC_FULLALL:
			SetLevel (1, true);
			return TRUE;
		case IDC_EDIT2:
		case IDC_EDIT3:
			if (HIWORD(wParam) == EN_CHANGE)
				lastedit = LOWORD(wParam);
			return TRUE;
		}
		break;
	case WM_NOTIFY:
		if (((NMHDR*)lParam)->code == UDN_DELTAPOS) {
			NMUPDOWN *nmud = (NMUPDOWN*)lParam;
			char cbuf[256];
			DWORD i, n;
			int id = ((NMHDR*)lParam)->idFrom;
			switch (id) {
			case IDC_SPIN1:
				GetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf, 256);
				i = sscanf (cbuf, "%d", &n);
				if (!i || !n || n > ntank) n = 1;
				n += (nmud->iDelta < 0 ? 1 : -1);
				n = max ((DWORD)1, min (ntank, n));
				sprintf (cbuf, "%d", n);
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf);
				Refresh ();
				return TRUE;
			}
		}
		break;
	case WM_HSCROLL:
		switch (GetDlgCtrlID ((HWND)lParam)) {
		case IDC_PROPLEVEL:
			switch (LOWORD (wParam)) {
			case SB_THUMBTRACK:
			case SB_LINELEFT:
			case SB_LINERIGHT:
				SetLevel (HIWORD(wParam)*0.01);
				return TRUE;
			}
			break;
		}
		break;
	}
	return ScnEditorTab::TabProc (hDlg, uMsg, wParam, lParam);
}

void EditorTab_Propellant::Refresh ()
{
	int i;
	DWORD n;
	double m, m0;
	char cbuf[256];
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	ntank = vessel->GetPropellantCount();
	sprintf (cbuf, "of %d", ntank);
	SetWindowText (GetDlgItem (hTab, IDC_STATIC1), cbuf);
	GetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf, 256);
	i = sscanf (cbuf, "%d", &n);
	if (i != 1 || n > ntank) {
		SetWindowText (GetDlgItem (hTab, IDC_EDIT1), "1");
		n = 0;
	} else n--;
	PROPELLANT_HANDLE hP = vessel->GetPropellantHandleByIndex (n);
	if (!hP) return;
	m0 = vessel->GetPropellantMaxMass (hP);
	m  = vessel->GetPropellantMass (hP);
	sprintf (cbuf, "Mass (0-%0.2f kg)", m0);
	SetWindowText (GetDlgItem (hTab, IDC_STATIC2), cbuf);
	sprintf (cbuf, "%0.4f", m/m0);
	SetWindowText (GetDlgItem (hTab, IDC_EDIT2), cbuf);
	sprintf (cbuf, "%0.2f", m);
	SetWindowText (GetDlgItem (hTab, IDC_EDIT3), cbuf);
	oapiSetGaugePos (GetDlgItem (hTab, IDC_PROPLEVEL), (int)(m/m0*100+0.5));
	RefreshTotals();
}

void EditorTab_Propellant::RefreshTotals ()
{
	double m;
	char cbuf[256];
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	m = vessel->GetTotalPropellantMass ();
	sprintf (cbuf, "%0.2f kg", m);
	SetWindowText (GetDlgItem (hTab, IDC_STATIC3), cbuf);
	m = vessel->GetMass ();
	sprintf (cbuf, "%0.2f kg", m);
	SetWindowText (GetDlgItem (hTab, IDC_STATIC4), cbuf);
}

void EditorTab_Propellant::Apply ()
{
	char cbuf[256];
	double level;
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	if (lastedit == IDC_EDIT2) {
		GetWindowText (GetDlgItem (hTab, IDC_EDIT2), cbuf, 256);
		sscanf (cbuf, "%lf", &level);
		level = max (0.0, min (1.0, level));
	} else {
		VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
		GetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf, 256);
		DWORD n, i = sscanf (cbuf, "%d", &n);
		if (!i || --n >= ntank) return;
		PROPELLANT_HANDLE hP = vessel->GetPropellantHandleByIndex (n);
		double m, m0 = vessel->GetPropellantMaxMass (hP);
		GetWindowText (GetDlgItem (hTab, IDC_EDIT3), cbuf, 256);
		sscanf (cbuf, "%lf", &m);
		level = max (0.0, min (1.0, m/m0));
	}
	SetLevel (level);
}

void EditorTab_Propellant::SetLevel (double level, bool setall)
{
	char cbuf[256];
	int i, j;
	DWORD n, k, k0, k1;
	double m0;
	VESSEL *vessel = oapiGetVesselInterface (ed->hVessel);
	ntank = vessel->GetPropellantCount();
	if (!ntank) return;
	GetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf, 256);
	i = sscanf (cbuf, "%d", &n);
	if (!i || --n >= ntank) return;
	if (setall) k0 = 0, k1 = ntank;
	else        k0 = n, k1 = n+1;
	for (k = k0; k < k1; k++) {
		PROPELLANT_HANDLE hP = vessel->GetPropellantHandleByIndex (k);
		m0 = vessel->GetPropellantMaxMass (hP);
		vessel->SetPropellantMass (hP, level*m0);
		if (k == n) {
			sprintf (cbuf, "%f", level);
			SetWindowText (GetDlgItem (hTab, IDC_EDIT2), cbuf);
			sprintf (cbuf, "%0.2f", level*m0);
			SetWindowText (GetDlgItem (hTab, IDC_EDIT3), cbuf);
			i = oapiGetGaugePos (GetDlgItem (hTab, IDC_PROPLEVEL));
			j = (int)(level*100.0+0.5);
			if (i != j) oapiSetGaugePos (GetDlgItem (hTab, IDC_PROPLEVEL), j);
		}
	}
	RefreshTotals();
}

INT_PTR EditorTab_Propellant::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_Propellant *pTab = (EditorTab_Propellant*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}


// ==============================================================
// EditorTab_Docking class definition
// ==============================================================

EditorTab_Docking::EditorTab_Docking (ScnEditor *editor) : ScnEditorTab (editor)
{
	CreateTab (IDD_TAB_EDIT8, EditorTab_Docking::DlgProc);
	SendDlgItemMessage (hTab, IDC_RADIO1, BM_SETCHECK, BST_CHECKED, 0);
	SendDlgItemMessage (hTab, IDC_RADIO2, BM_SETCHECK, BST_UNCHECKED, 0);
}

void EditorTab_Docking::InitTab ()
{
	SetWindowText (GetDlgItem (hTab, IDC_EDIT1), "1");
	ScanTargetList();
	Refresh ();
}

char *EditorTab_Docking::HelpTopic ()
{
	return (char*)"/Docking.htm";
}

INT_PTR EditorTab_Docking::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BACK:
			SwitchTab (3);
			return TRUE;
		case IDC_REFRESH:
			Refresh ();
			return TRUE;
		case IDC_DOCK:
			Dock ();
			return TRUE;
		case IDC_UNDOCK:
			Undock ();
			Refresh ();
			return TRUE;
		case IDC_IDS:
			ToggleIDS();
			Refresh ();
			return TRUE;
		case IDC_EDIT1:
			if (HIWORD (wParam) == EN_CHANGE)
				if (DockNo()) Refresh();
			return TRUE;
		case IDC_EDIT2:
			if (HIWORD (wParam) == EN_CHANGE)
				IncIDSChannel (0);
			return TRUE;
		case IDC_COMBO1:
			if (HIWORD (wParam) == CBN_SELCHANGE) {
				SetTargetDock (1);
			}
			return TRUE;
		}
		break;
	case WM_NOTIFY:
		if (((NMHDR*)lParam)->code == UDN_DELTAPOS) {
			NMUPDOWN *nmud = (NMUPDOWN*)lParam;
			char cbuf[256];
			DWORD n;
			switch (((NMHDR*)lParam)->idFrom) {
			case IDC_SPIN1:
				GetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf, 256);
				if (!sscanf (cbuf, "%d", &n)) n = 1;
				sprintf (cbuf, "%d", n + (nmud->iDelta < 0 ? 1 : -1));
				SetWindowText (GetDlgItem (hDlg, IDC_EDIT1), cbuf);
				Refresh ();
				return TRUE;
			case IDC_SPIN2:
				IncIDSChannel (nmud->iDelta < 0 ? 1 : -1);
				Refresh ();
				return TRUE;
			case IDC_SPIN2A:
				IncIDSChannel (nmud->iDelta < 0 ? 20 : -20);
				Refresh ();
				return TRUE;
			case IDC_SPIN3:
				GetWindowText (GetDlgItem (hDlg, IDC_EDIT4), cbuf, 256);
				if (!sscanf (cbuf, "%d", &n)) n = 1;
				SetTargetDock (n + (nmud->iDelta < 0 ? 1 : -1));
				return TRUE;
			}
		}
		break;
	}
	return ScnEditorTab::TabProc (hDlg, uMsg, wParam, lParam);
}

UINT EditorTab_Docking::DockNo ()
{
	// returns the id of the dock currently being processed
	// (>= 1, or 0 if invalid)
	char cbuf[256];
	UINT dock;
	GetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf, 256);
	int res = sscanf (cbuf, "%d", &dock);
	if (!res || dock > Vessel()->DockCount()) dock = 0;
	return dock;
}

void EditorTab_Docking::ScanTargetList ()
{
	// populate docking target list
	SendDlgItemMessage (hTab, IDC_COMBO1, CB_RESETCONTENT, 0, 0);
	for (DWORD i = 0; i < oapiGetVesselCount(); i++) {
		OBJHANDLE hV = oapiGetVesselByIndex (i);
		VESSEL *v = oapiGetVesselInterface (hV);
		if (v != Vessel() && v->DockCount() > 0) { // only vessels with docking ports make sense here
			SendDlgItemMessage (hTab, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)v->GetName());
		}
	}
}

void EditorTab_Docking::ToggleIDS ()
{
	UINT dock = DockNo();
	if (!dock) return;
	DOCKHANDLE hDock = Vessel()->GetDockHandle (dock-1);
	bool enable = (SendDlgItemMessage (hTab, IDC_IDS, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Vessel()->EnableIDS (hDock, enable);
}

void EditorTab_Docking::IncIDSChannel (int dch)
{
	char cbuf[256];
	double freq;
	UINT dock = DockNo();
	if (!dock) return;
	DOCKHANDLE hDock = Vessel()->GetDockHandle (dock-1);
	GetWindowText (GetDlgItem (hTab, IDC_EDIT2), cbuf, 256);
	if (sscanf (cbuf, "%lf", &freq)) {
		int ch = (int)((freq-108.0)*20.0+0.5);
		ch = max(0, min (639, ch+dch));
		Vessel()->SetIDSChannel (hDock, ch);
	}
}

void EditorTab_Docking::Dock ()
{
	char cbuf[256];
	DWORD n, ntgt, mode;
	VESSEL *vessel = Vessel();

	GetWindowText (GetDlgItem (hTab, IDC_COMBO1), cbuf, 256);
	OBJHANDLE hTarget = oapiGetVesselByName (cbuf);
	if (!hTarget) return;
	VESSEL *target = oapiGetVesselInterface (hTarget);
	n = DockNo();
	if (!n) return;
	GetWindowText (GetDlgItem (hTab, IDC_EDIT4), cbuf, 256);
	if (!sscanf (cbuf, "%d", &ntgt) || ntgt < 1 || ntgt > target->DockCount()) {
		DisplayErrorMsg (IDS_ERR4);
		return;
	}
	mode = (SendDlgItemMessage (hTab, IDC_RADIO1, BM_GETCHECK, 0, 0) == BST_CHECKED ? 1:2);
	int res = vessel->Dock (hTarget, n-1, ntgt-1, mode);
	Refresh ();
	if (res) {
		static UINT errmsgid[3] = {IDS_ERR1, IDS_ERR2, IDS_ERR3};
		DisplayErrorMsg (errmsgid[res-1]);
	}
}

void EditorTab_Docking::Undock ()
{
	UINT dock = DockNo();
	if (!dock) return;
	Vessel()->Undock (dock-1);
}

void EditorTab_Docking::Refresh ()
{
	static const int dockitem[7] = {IDC_DOCK, IDC_COMBO1, IDC_EDIT4, IDC_STATIC3, IDC_SPIN3, IDC_RADIO1, IDC_RADIO2};
	static const int undockitem[2] = {IDC_UNDOCK, IDC_EDIT3};

	VESSEL *vessel = Vessel();
	char cbuf[256];
	int i;
	DWORD n, ndock = vessel->DockCount();
	SetWindowText (GetDlgItem (hTab, IDC_ERRMSG), "");

	sprintf (cbuf, "of %d", vessel->DockCount());
	SetWindowText (GetDlgItem (hTab, IDC_STATIC1), cbuf);

	GetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf, 256);
	if (!sscanf (cbuf, "%d", &n)) n = 0;
	if (n < 1 || n > ndock) {
		n = max ((DWORD)1, min (ndock, n));
		sprintf (cbuf, "%d", n);
		SetWindowText (GetDlgItem (hTab, IDC_EDIT1), cbuf);
	}
	n--; // zero-based

	DOCKHANDLE hDock = vessel->GetDockHandle (n);
	NAVHANDLE hIDS = vessel->GetIDS (hDock);
	if (hIDS) sprintf (cbuf, "%0.2f", oapiGetNavFreq (hIDS));
	else      strcpy (cbuf, "<none>");
	SetWindowText (GetDlgItem (hTab, IDC_EDIT2), cbuf);
	SendDlgItemMessage (hTab, IDC_IDS, BM_SETCHECK, hIDS ? BST_CHECKED : BST_UNCHECKED, 0);
	EnableWindow (GetDlgItem (hTab, IDC_EDIT2), hIDS ? TRUE:FALSE);
	EnableWindow (GetDlgItem (hTab, IDC_SPIN2), hIDS ? TRUE:FALSE);

	OBJHANDLE hMate = vessel->GetDockStatus (hDock);
	if (hMate) { // dock is engaged
		SetWindowText (GetDlgItem (hTab, IDC_STATIC2), "Currently docked to");
		for (i = 0; i < 7; i++) ShowWindow (GetDlgItem (hTab, dockitem[i]), SW_HIDE);
		for (i = 0; i < 2; i++) ShowWindow (GetDlgItem (hTab, undockitem[i]), SW_SHOW);
		oapiGetObjectName (hMate, cbuf, 256);
		SetWindowText (GetDlgItem (hTab, IDC_EDIT3), cbuf);
	} else { // dock is free
		SetWindowText (GetDlgItem (hTab, IDC_STATIC2), "Establish docking connection with");
		for (i = 0; i < 2; i++) ShowWindow (GetDlgItem (hTab, undockitem[i]), SW_HIDE);
		for (i = 0; i < 7; i++) ShowWindow (GetDlgItem (hTab, dockitem[i]), SW_SHOW);
	}
}

void EditorTab_Docking::SetTargetDock (DWORD dock)
{
	char cbuf[256];
	DWORD n = 0;
	GetWindowText (GetDlgItem (hTab, IDC_COMBO1), cbuf, 256);
	OBJHANDLE hTarget = oapiGetVesselByName (cbuf);
	if (hTarget) {
		VESSEL *v = oapiGetVesselInterface (hTarget);
		DWORD ndock = v->DockCount();
		if (ndock) n = max ((DWORD)1, min (ndock, dock));
	}
	if (n) sprintf (cbuf, "%d", n);
	else   cbuf[0] = '\0';
	SetWindowText (GetDlgItem (hTab, IDC_EDIT4), cbuf);
}

void EditorTab_Docking::DisplayErrorMsg (UINT err)
{
	char cbuf[256] = "Error: ";
	LoadString (ed->InstHandle(), err, cbuf+7, 249);
	SetWindowText (GetDlgItem (hTab, IDC_ERRMSG), cbuf);
}

INT_PTR EditorTab_Docking::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_Docking *pTab = (EditorTab_Docking*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}


// ==============================================================
// EditorTab_Custom class definition
// ==============================================================

EditorTab_Custom::EditorTab_Custom (ScnEditor *editor, HINSTANCE hInst, WORD ResId, DLGPROC UserProc) : ScnEditorTab (editor)
{
	usrProc = UserProc;
	CreateTab (hInst, ResId, EditorTab_Custom::DlgProc);
}

void EditorTab_Custom::OpenHelp ()
{
	if (!usrProc (hTab, WM_COMMAND, IDHELP, 0))
		ScnEditorTab::OpenHelp();
}

INT_PTR EditorTab_Custom::TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		return usrProc (hDlg, uMsg, wParam, (LPARAM)ed->hVessel);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BACK:
			SwitchTab (3);
			return TRUE;
		}
		break;
	case WM_SCNEDITOR:
		switch (LOWORD (wParam)) {
		case SE_GETVESSEL:
			*(OBJHANDLE*)lParam = ed->hVessel;
			return TRUE;
		}
		break;
	}
	return usrProc (hDlg, uMsg, wParam, lParam);
}

INT_PTR EditorTab_Custom::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	EditorTab_Custom *pTab = (EditorTab_Custom*)TabPointer (hDlg, uMsg, wParam, lParam);
	if (!pTab) return FALSE;
	else return pTab->TabProc (hDlg, uMsg, wParam, lParam);
}

