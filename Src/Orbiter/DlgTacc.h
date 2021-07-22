// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Time acceleration dialog
// ======================================================================

#ifndef __DLGTACC_H
#define __DLGTACC_H

#include "DialogWin.h"

class DlgTacc: public DialogWin {
public:
	DlgTacc (HINSTANCE hInstance, HWND hParent, void *context);
	void Message (DWORD msg, void *data);
	void RegisterWarp (HWND hDlg, double warp, bool commit = true, bool edit = true, bool slide = true);
	BOOL OnInitDialog (HWND hDlg, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand (HWND hDlg, WORD id, WORD code, HWND hControl);
	BOOL OnHScroll (HWND hDlg, WORD request, WORD curpos, HWND hControl);
};

#endif // !__DLGTACC_H