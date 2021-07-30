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

#define STRICT 1
#define ORBITER_MODULE
#include "orbitersdk.h"
#include "resource.h"
#include <stdio.h>

// ==============================================================
// Some global parameters

// file name for storing custom parameters
const char *cfgfile = "myparam.cfg";
char *myitemtag = "MyParam";

class MyRootItem;
class MyItem;

struct {
	HINSTANCE hInst;
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
	bool clbkOpen (HWND hLaunchpad);
	int clbkWriteConfig ();
	static LRESULT CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
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

bool MyItem::clbkOpen (HWND hLaunchpad)
{
	// respond to user double-clicking the item in the list
	DialogBox (gParams.hInst, MAKEINTRESOURCE (IDD_MYPARAM), hLaunchpad, DlgProc);
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

LRESULT CALLBACK MyItem::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	// the dialog message handler
	char cbuf[32];

	switch (uMsg) {
	case WM_INITDIALOG: // display the current value
		sprintf (cbuf, "%f", gParams.my_param);
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
}

// ==============================================================
// The DLL entry point
// ==============================================================

DLLCLBK void InitModule (HINSTANCE hDLL)
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

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Unregister the launchpad items
	oapiUnregisterLaunchpadItem (gParams.sub_item);
	delete gParams.sub_item;
	oapiUnregisterLaunchpadItem (gParams.root_item);
	delete gParams.root_item;
}