// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Screen capture window
// ======================================================================

#ifndef __DLGCAPTURE_H
#define __DLGCAPTURE_H

#include "DialogWin.h"

// ======================================================================
// Class for screen capture dialog

class DlgCapture: public DialogWin {
public:
	DlgCapture (HINSTANCE hInstance, HWND hParent, void *context);
	~DlgCapture ();
	void Update ();
	BOOL OnInitDialog (HWND hDlg, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand (HWND hDlg, WORD id, WORD code, HWND hControl);

protected:
	bool Take (HWND hDlg);
	bool AutoIncrement (HWND hDlg);
};

#endif // !__DLGCAPTURE_H