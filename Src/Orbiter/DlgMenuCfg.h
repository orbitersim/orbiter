// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Menu bar configuration dialog
// ======================================================================

#ifndef __DLGMENUCFG_H
#define __DLGMENUCFG_H

#include "DialogWin.h"

class DlgMenuCfg: public DialogWin {
public:
	DlgMenuCfg (HINSTANCE hInstance, HWND hParent, void *context);
	~DlgMenuCfg ();
	BOOL OnInitDialog (HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand (HWND hWnd, WORD id, WORD code, HWND hControl);
	BOOL OnHScroll (HWND hWnd, WORD request, WORD curpos, HWND hControl);
};

#endif // !__DLGMENUCFG_H