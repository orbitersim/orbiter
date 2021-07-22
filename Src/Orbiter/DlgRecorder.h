// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Flight recorder dialog
// ======================================================================

#ifndef __DLGRECORDER_H
#define __DLGRECORDER_H

#include "DialogWin.h"

class DlgRecorder: public DialogWin {
public:
	DlgRecorder (HINSTANCE hInstance, HWND hParent, void *context);
	BOOL OnInitDialog (HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand (HWND hWnd, WORD id, WORD code, HWND hControl);
	BOOL OnUser1 (HWND hWnd, WPARAM wParam, LPARAM lParam);
	void GetRecordName (char *str, int maxlen) const;

protected:
	void SetupDialog (HWND hDlg);
	void ShowAdvancedRec (HWND hDlg);
	void HideAdvancedRec (HWND hDlg);

private:
	int RecorderDlg_w, RecorderDlg_h1, RecorderDlg_h2;
};

#endif // !__DLGRECORDER_H