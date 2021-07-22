// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Custom function selection dialog
// ======================================================================

#ifndef __DLGFUNCTION_H
#define __DLGFUNCTION_H

#include "DialogWin.h"

class DlgFunction: public DialogWin {
public:
	DlgFunction (HINSTANCE hInstance, HWND hParent, void *context);
	BOOL OnInitDialog (HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand (HWND hWnd, WORD id, WORD code, HWND hControl);

protected:
	void ScanFunctions (HWND hDlg);
	void RunFunction (HWND hDlg);
	void ShowDescription (HWND hDlg);
};

#endif // !__DLGFUNCTION_H