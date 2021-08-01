// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __DIALOGS_H
#define __DIALOGS_H

#include "DlgCamera.h"
#include "DlgFocus.h"
#include "DlgTacc.h"
#include "DlgFunction.h"
#include "DlgMap.h"
#include "DlgInfo.h"
#include "DlgRecorder.h"
#include "DlgVishelper.h"
#include "DlgCapture.h"

//INT_PTR CALLBACK Navaid_DlgProc (HWND, UINT, WPARAM, LPARAM);

INT_PTR CALLBACK FRecorderMsg_DlgProc (HWND, UINT, WPARAM, LPARAM);

// =========================================================================
// Set up a combo box dialog control for selecting a celestial body

class CBodySelectComboBox {
public:
	CBodySelectComboBox () {}
	static void Init (HWND hDlg, int resid);
	static void BuildListFromNode (HWND hDlg, int resid, const CelestialBody *node = 0);
	static CelestialBody *OnSelectionChanged (HWND hDlg, int resid);
};

#endif // !__DIALOGS_H