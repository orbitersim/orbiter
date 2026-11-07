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

#define ORBITER_MODULE
#include "orbitersdk.h"
#include "imgui.h"
#include <stdio.h>

ORBITER_MODULE_INFO("SDK sample plugins",
	"LAUNCHPAD PARAMETER TEMPLATE:\r\n\r\nThis SDK sample creates a root entry (\"My root entry\") and sub-entry (\"My sub-item\") in the Extra tab of the Orbiter launchpad.\r\n\r\nThis addon is not very useful - it is intended as a starting point for developers.");

// ==============================================================
// Some global parameters

// file name for storing custom parameters
const char *cfgfile = "myparam.cfg";
char myitemtag[] = "MyParam";

class MyRootItem;
class MyItem;

struct {
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
// The dialog box opened by the launchpad item. Without a running
// simulation session, Orbiter draws it in the Launchpad window.
// ==============================================================

class MyDialog: public ImGuiDialog {
public:
	MyDialog (): ImGuiDialog ("My parameters") {}

	void Open ()
	{
		if (!IsActive()) value = gParams.my_param; // display the current value
		oapiOpenDialog (this);
	}

	void Display () override
	{
		// Override Display instead of OnDraw to control the window flags
		if (ImGui::Begin (name.c_str(), &active, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoCollapse)) {
			ImGui::PushTextWrapPos (ImGui::GetFontSize() * 20.0f);
			ImGui::TextUnformatted ("This example demonstrates the possibility of configuring addon modules from the Orbiter Launchpad dialog.");
			ImGui::PopTextWrapPos ();
			ImGui::SetNextItemWidth (ImGui::GetFontSize() * 8.0f);
			ImGui::InputDouble ("Custom parameter", &value);
			if (ImGui::Button ("OK")) {
				gParams.my_param = value; // store the value
				active = false;
			}
			ImGui::SameLine ();
			if (ImGui::Button ("Cancel"))
				active = false;
		}
		ImGui::End ();
	}

protected:
	void OnDraw () override {}

private:
	double value = 0.0;
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
	bool clbkOpen ();
	int clbkWriteConfig ();

private:
	MyDialog dlg;
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

bool MyItem::clbkOpen ()
{
	// respond to user double-clicking the item in the list
	dlg.Open ();
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

// ==============================================================
// The DLL entry point
// ==============================================================

DLLCLBK void InitModule (HINSTANCE hDLL)
{
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