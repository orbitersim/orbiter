// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: Meshdebug
//                  Part of the ORBITER SDK
//
// Meshdebug.cpp
//
// A simple tool for marking mesh groups during a simulation.
// May be useful for debugging meshes.
// ==============================================================

#ifndef __linux__
#define STRICT
#else // __linux__
// STRICT left out: windows.h handle type-checking switch
#endif // __linux__
#define ORBITER_MODULE
#ifndef __linux__
#include <windows.h>
#include <commctrl.h>
#include "orbitersdk.h"
#else // __linux__
// windows.h, commctrl.h left out: the dialog controls are Qt widgets
#include "Orbitersdk.h"
#include "OrbiterResource.h"
#endif // __linux__
#include "resource.h"
#include <cstdio>
#ifdef __linux__
#include <cstring>
#include <cmath>
#include <functional>
#include <QKeyEvent>
#endif // __linux__

using std::min;
using std::max;

// ==============================================================
// Global variables
// ==============================================================

#ifndef __linux__
HINSTANCE g_hInst;    // module instance handle
HWND g_hDlg = 0;      // dialog window handle
#else // __linux__
void *g_hInst;        // module instance handle
QWidget *g_hDlg = 0;  // dialog window handle
#endif // __linux__
DWORD g_dwCmd;        // custom function identifier
VESSEL *g_vessel;     // focus vessel interface
VISHANDLE *g_visptr;  // pointer to visual of current focus object
MESHHANDLE g_mesh;    // current mesh handle
MATERIAL matOrig;
DWORD g_nmesh, g_ngrp;
int   g_imesh, g_igrp;
DWORD g_flag;
DWORD g_origmat;
int g_tmpmat;
float g_opac = 0.5f;
double g_t;
bool blink;

// ==============================================================
// Local prototypes
// ==============================================================

void ChangeMesh (int idx);
void ChangeGroup (int idx);
void OpenDlgClbk (void *context);
void SetMaterialOpacity (MESHHANDLE hMesh, float opac);
#ifndef __linux__
INT_PTR CALLBACK MsgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
void MsgProc (QWidget*, void*);
#endif // __linux__

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// This function is called when Orbiter starts or when the module
// is activated.

#ifndef __linux__
DLLCLBK void InitModule (HINSTANCE hModule)
#else // __linux__
DLLCLBK void InitModule (void *hModule)
#endif // __linux__
{
	g_hInst = hModule;
	g_dwCmd = oapiRegisterCustomCmd ((char*)"Mesh debugger",
		(char*)"Mark individual mesh groups in a vessel mesh",
		OpenDlgClbk, NULL);
}


// ==============================================================
// This function is called when Orbiter shuts down or when the
// module is deactivated

#ifndef __linux__
DLLCLBK void ExitModule (HINSTANCE hModule)
#else // __linux__
DLLCLBK void ExitModule (void *hModule)
#endif // __linux__
{
	oapiUnregisterCustomCmd (g_dwCmd);
	// Unregister the custom function in Orbiter
}


// ==============================================================
// frame step

DLLCLBK void opcPreStep (double simt, double simdt, double mjd)
{
	if (!g_hDlg) return;
	double d, t = oapiGetSysTime();
	bool bshow = (modf (t-g_t, &d) > 0.5);
	if (bshow != blink) {
		blink = bshow;
		if (g_igrp >= 0) {
			MESHGROUP *grp = oapiMeshGroup (g_mesh, g_igrp);
			grp->UsrFlag = (blink ? g_flag:3);
		}
	}
}

// ==============================================================
// Open the dialog window

void OpenDlgClbk (void *context)
{
	g_hDlg = oapiOpenDialog (g_hInst, IDD_DBGDIALOG, MsgProc);
}


// ==============================================================
// Close the dialog

#ifndef __linux__
void CloseDlg (HWND hDlg)
#else // __linux__
void CloseDlg (QWidget *hDlg)
#endif // __linux__
{
	ChangeMesh (-1);
	oapiCloseDialog (hDlg);
	g_hDlg = 0;
}


// ==============================================================

void GetMeshParams ()
{
	g_vessel = oapiGetFocusInterface();
	g_visptr = oapiObjectVisualPtr (oapiGetFocusObject());
	g_mesh = 0;
	g_nmesh = g_ngrp =  0;
	g_imesh = g_igrp = -1;
	g_tmpmat = -1;
	if (*g_visptr) {
		for (; g_vessel->GetMesh (*g_visptr, g_nmesh); g_nmesh++);
		ChangeMesh (0);
	}
}

void ChangeMesh (int idx)
{
	if (*g_visptr && idx != g_imesh) {
		if (g_mesh) {
			ChangeGroup (-1);
			SetMaterialOpacity (g_mesh, 1.0f/g_opac);
			oapiDeleteMaterial (g_mesh, g_tmpmat);
			oapiSetMeshProperty (g_mesh, MESHPROPERTY_MODULATEMATALPHA, 0);
		}
		g_imesh = idx;
		g_mesh = (idx >= 0 ? g_vessel->GetMesh (*g_visptr, g_imesh) : 0);
		if (g_mesh) {
			g_ngrp = oapiMeshGroupCount (g_mesh);
			SetMaterialOpacity (g_mesh, g_opac);
			oapiSetMeshProperty (g_mesh, MESHPROPERTY_MODULATEMATALPHA, 1);
			MATERIAL mat;
			g_tmpmat = oapiAddMaterial (g_mesh, &mat);
			ChangeGroup (0);
		}
	}
}

void ChangeGroup (int idx)
{
	if (*g_visptr) {
		MESHGROUP *grp;
		g_t = oapiGetSysTime();
		blink = true;
		if (g_igrp >= 0) {
			grp = oapiMeshGroup (g_mesh, g_igrp);
			grp->UsrFlag = g_flag;
			grp->MtrlIdx = g_origmat;
		}
		g_igrp = idx;
		if (g_igrp >= 0) {
			grp = oapiMeshGroup (g_mesh, g_igrp);
			g_flag = grp->UsrFlag; // save original flag
			grp->UsrFlag = 3;      // hide group
			g_origmat = grp->MtrlIdx;
			if (g_origmat >= oapiMeshMaterialCount (g_mesh)) g_origmat = 0;
			MATERIAL *mat1 = oapiMeshMaterial (g_mesh, g_tmpmat);
			MATERIAL *mat2 = oapiMeshMaterial (g_mesh, g_origmat);
			memcpy (mat1, mat2, sizeof(MATERIAL));
			mat1->diffuse.a /= g_opac;
			mat1->ambient.a /= g_opac;
			mat1->specular.a /= g_opac;
			mat1->emissive.a /= g_opac;
			grp->MtrlIdx = g_tmpmat;
		}
	}
}

void SetMaterialOpacity (MESHHANDLE hMesh, float opac)
{
	DWORD i, nmat = oapiMeshMaterialCount (hMesh);
	for (i = 0; i < nmat; i++) {
		MATERIAL *mat = oapiMeshMaterial (hMesh, i);
		mat->diffuse.a  *= opac;
		mat->ambient.a  *= opac;
		mat->specular.a *= opac;
		mat->emissive.a *= opac;
	}
}

// ==============================================================

#ifndef __linux__
void RefreshDialog (HWND hDlg)
#else // __linux__
void RefreshDialog (QWidget *hDlg)
#endif // __linux__
{
	VISHANDLE vis = *g_visptr;
	char cbuf[256];
	if (vis) {
#ifndef __linux__
		SendMessage (GetDlgItem (hDlg, IDC_MESHSPIN), UDM_SETRANGE, 0, MAKELONG (g_nmesh-1, 0));
		//SendMessage (GetDlgItem (hDlg, IDC_MESHSPIN), UDM_SETPOS, 0, MAKELONG (g_imesh, 0));
		SendMessage (GetDlgItem (hDlg, IDC_GROUPSPIN), UDM_SETRANGE, 0, MAKELONG (g_ngrp-1, 0));
		SendMessage (GetDlgItem (hDlg, IDC_GROUPSPIN), UDM_SETPOS, 0, MAKELONG (g_igrp, 0));
#else // __linux__
		oapiSetUpDownRange (oapiResDlgItem (hDlg, IDC_MESHSPIN), 0, g_nmesh-1);
		//oapiSetUpDownPos (oapiResDlgItem (hDlg, IDC_MESHSPIN), g_imesh);
		oapiSetUpDownRange (oapiResDlgItem (hDlg, IDC_GROUPSPIN), 0, g_ngrp-1);
		oapiSetUpDownPos (oapiResDlgItem (hDlg, IDC_GROUPSPIN), g_igrp);
#endif // __linux__
	}
	sprintf (cbuf, "(0 to %d)", g_nmesh-1);
#ifndef __linux__
	SetWindowText (GetDlgItem (hDlg, IDC_NMESH), cbuf);
#else // __linux__
	oapiSetDlgItemText (hDlg, IDC_NMESH, cbuf);
#endif // __linux__
	sprintf (cbuf, "(0 to %d)", g_ngrp-1);
#ifndef __linux__
	SetWindowText (GetDlgItem (hDlg, IDC_NGRP), cbuf);
#else // __linux__
	oapiSetDlgItemText (hDlg, IDC_NGRP, cbuf);
#endif // __linux__
}

#ifdef __linux__
// not upstream: DefDlgProc's WM_CLOSE (and Esc) -> IDCANCEL to the dialog procedure, as an event filter
class DlgEvents: public QObject {
public:
	DlgEvents (QWidget *hWnd, std::function<void()> cancel): QObject (hWnd), onCancel (cancel) { hWnd->installEventFilter (this); }
	bool eventFilter (QObject *o, QEvent *e) override {
		if (e->type() == QEvent::Close || (e->type() == QEvent::KeyPress && static_cast<QKeyEvent*>(e)->key() == Qt::Key_Escape)) {
			e->ignore();
			onCancel();
			return true;
		}
		return false;
	}
private:
	std::function<void()> onCancel;
};

#endif // __linux__
// ==============================================================
// Windows message handler for the dialog box

#ifndef __linux__
INT_PTR CALLBACK MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void MsgProc (QWidget *hDlg, void *context)
#endif // __linux__
{
#ifndef __linux__
	switch (uMsg) {
	case WM_INITDIALOG:          // initialise dialog
#else // __linux__
	// WM_INITDIALOG: initialise dialog
#endif // __linux__
		GetMeshParams();
		RefreshDialog (hDlg);
#ifndef __linux__
		return TRUE;
	case WM_NOTIFY:
		switch (wParam) {
		case IDC_MESHSPIN:
			if (((LPNMHDR)lParam)->code == UDN_DELTAPOS) {
				int m = ((LPNMUPDOWN)lParam)->iPos + ((LPNMUPDOWN)lParam)->iDelta;
				m = min ((int)g_nmesh-1, max (0, m));
				if (m != g_imesh) ChangeMesh (m);
				RefreshDialog (hDlg);
			}
			return 0;
		case IDC_GROUPSPIN:
			if (((LPNMHDR)lParam)->code == UDN_DELTAPOS) {
				int g = ((LPNMUPDOWN)lParam)->iPos + ((LPNMUPDOWN)lParam)->iDelta;
				g = min ((int)g_ngrp-1, max (0, g));
				if (g != g_igrp) ChangeGroup (g);
			}
			return 0;
		}
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
#else // __linux__
	// WM_NOTIFY
	oapiConnectDlgDeltaPos (hDlg, [hDlg](int idFrom, int iPos, int iDelta) { // UDN_DELTAPOS
		switch (idFrom) {
		case IDC_MESHSPIN: {
				int m = iPos + iDelta;
				m = min ((int)g_nmesh-1, max (0, m));
				if (m != g_imesh) ChangeMesh (m);
				RefreshDialog (hDlg);
			} return;
		case IDC_GROUPSPIN: {
				int g = iPos + iDelta;
				g = min ((int)g_ngrp-1, max (0, g));
				if (g != g_igrp) ChangeGroup (g);
			} return;
		}
	});
	// WM_COMMAND
	auto command = [hDlg](int id, int code, QWidget *hCtrl) {
		switch (id) {
#endif // __linux__

		case IDCANCEL: // dialog closed by user
			CloseDlg (hDlg);
#ifndef __linux__
			return TRUE;
#else // __linux__
			return;
#endif // __linux__
		}
#ifndef __linux__
		break;
	}
	return oapiDefDialogProc (hDlg, uMsg, wParam, lParam);
#else // __linux__
	};
	oapiConnectDlgCommands (hDlg, command);
	new DlgEvents (hDlg, [command]() { command (IDCANCEL, RESN_CLICKED, NULL); });
	// oapiDefDialogProc left out: oapiOpenDialog wires the default dialog behaviour
#endif // __linux__
}
