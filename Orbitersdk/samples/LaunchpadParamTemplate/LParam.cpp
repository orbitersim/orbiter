// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//           ORBITER MODULE: LaunchpadParamTemplate
//                  Part of the ORBITER SDK
//
// LParam.cpp
//
// This module demonstrates the ability to add custom interfaces
// for module-specific global parameter settings into the "Extra"
// tab of the Orbiter Launchpad startup dialog.
// This particular example doesn't do anything useful, but can
// be used as a starting point for real applications.
// ==============================================================

#ifndef __linux__
#define STRICT 1
#else // __linux__
// STRICT left out: windows.h handle type-checking switch
#endif // __linux__
#define ORBITER_MODULE
#ifndef __linux__
#include "orbitersdk.h"
#else // __linux__
#include "Orbitersdk.h"
#include "OrbiterResource.h"
#endif // __linux__
#include "resource.h"
#include <stdio.h>
#ifdef __linux__
#include <QDialog>
#endif // __linux__

// ==============================================================
// Some global parameters

// file name for storing custom parameters
const char *cfgfile = "myparam.cfg";
char *myitemtag = "MyParam";

class MyRootItem;
class MyItem;

struct {
#ifndef __linux__
	HINSTANCE hInst;
#else // __linux__
	void *hInst;
#endif // __linux__
	MyRootItem *root_item;
	MyItem *sub_item;
	double my_param;
} gParams;

// ==============================================================
// A class defining a new root item in the Launchpad "Extra" list
// This doesn't do anything other than display an item in the list
// and a description when selected.
// ==============================================================

class MyRootItem: public LaunchpadItem {
public:
	MyRootItem(): LaunchpadItem() {}
	char *Name() { return "My root item"; }
	char *Description() { return "Example 'Launchpad parameter template' from Orbiter SDK"; }
};

// ==============================================================
// A class defining the new launchpad parameter item
// This opens a dialog box for a user-defined item, and writes
// the value to a file to be read next time.
// ==============================================================

class MyItem: public LaunchpadItem {
public:
	MyItem();
	char *Name() { return "My sub-item"; }
	char *Description() { return "This item is an example from the Orbiter SDK. It doesn't do anything useful, but provides a source example for developers on how to write Launchpad plugins."; }
#ifndef __linux__
	bool clbkOpen (HWND hLaunchpad);
#else // __linux__
	bool clbkOpen (QWidget *hLaunchpad);
#endif // __linux__
	int clbkWriteConfig ();
#ifndef __linux__
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	static void DlgProc (QWidget*, void*);
#endif // __linux__
};

MyItem::MyItem (): LaunchpadItem ()
{
	// Read the current parameter value from file
	FILEHANDLE hFile = oapiOpenFile (cfgfile, FILE_IN, ROOT);
	if (!oapiReadItem_float (hFile, myitemtag, gParams.my_param)) {
		gParams.my_param = 0;
	}
	oapiCloseFile (hFile, FILE_IN);
}

#ifndef __linux__
bool MyItem::clbkOpen (HWND hLaunchpad)
#else // __linux__
bool MyItem::clbkOpen (QWidget *hLaunchpad)
#endif // __linux__
{
	// respond to user double-clicking the item in the list
#ifndef __linux__
	DialogBox (gParams.hInst, MAKEINTRESOURCE (IDD_MYPARAM), hLaunchpad, DlgProc);
#else // __linux__
	QDialog *dlg = qobject_cast<QDialog*> (oapiCreateResDialog (gParams.hInst, IDD_MYPARAM, hLaunchpad)); // DialogBox
	if (dlg) {
		DlgProc (dlg, NULL);
		dlg->exec();
		delete dlg;
	}
#endif // __linux__
	return true;
}

int MyItem::clbkWriteConfig ()
{
	// called when orbiter needs to write its configuration to disk
	FILEHANDLE hFile = oapiOpenFile (cfgfile, FILE_OUT, ROOT);
	oapiWriteItem_float (hFile, myitemtag, gParams.my_param);
	oapiCloseFile (hFile, FILE_OUT);
	return 0;
}

#ifndef __linux__
INT_PTR CALLBACK MyItem::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void MyItem::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
	// the dialog message handler
	char cbuf[32];

#ifndef __linux__
	switch (uMsg) {
	case WM_INITDIALOG: // display the current value
#else // __linux__
	// WM_INITDIALOG: display the current value
#endif // __linux__
		sprintf (cbuf, "%f", gParams.my_param);
#ifndef __linux__
		SetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf);
		return TRUE;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDOK:    // store the value
			GetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf, 32);
			if (sscanf (cbuf, "%lf", &gParams.my_param) != 1)
				gParams.my_param = 0;
			EndDialog (hWnd, 0);
			return 0;
		case IDCANCEL:
			EndDialog (hWnd, 0);
			return 0;
		}
		break;
	}
	return 0;
#else // __linux__
		oapiSetDlgItemText (hWnd, IDC_EDIT1, cbuf);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd](int id, int code, QWidget *hCtrl) {
		char cbuf[32];
		switch (id) {
		case IDOK:    // store the value
			oapiGetDlgItemText (hWnd, IDC_EDIT1, cbuf, 32);
			if (sscanf (cbuf, "%lf", &gParams.my_param) != 1)
				gParams.my_param = 0;
			qobject_cast<QDialog*> (hWnd)->done (0); // EndDialog
			return;
		case IDCANCEL:
			qobject_cast<QDialog*> (hWnd)->done (0); // EndDialog
			return;
		}
	});
#endif // __linux__
}

// ==============================================================
// The DLL entry point
// ==============================================================

#ifndef __linux__
DLLCLBK void InitModule (HINSTANCE hDLL)
#else // __linux__
DLLCLBK void InitModule (void *hDLL)
#endif // __linux__
{
	gParams.hInst = hDLL;
	gParams.my_param = 0;

	gParams.root_item = new MyRootItem;
	LAUNCHPADITEM_HANDLE hRoot = oapiRegisterLaunchpadItem (gParams.root_item);
	// register the new root item with orbiter

	gParams.sub_item = new MyItem;
	oapiRegisterLaunchpadItem (gParams.sub_item, hRoot);
	// register the new sub-item with Orbiter
}

// ==============================================================
// The DLL exit point
// ==============================================================

#ifndef __linux__
DLLCLBK void ExitModule (HINSTANCE hDLL)
#else // __linux__
DLLCLBK void ExitModule (void *hDLL)
#endif // __linux__
{
	// Unregister the launchpad items
	oapiUnregisterLaunchpadItem (gParams.sub_item);
	delete gParams.sub_item;
	oapiUnregisterLaunchpadItem (gParams.root_item);
	delete gParams.root_item;
}
