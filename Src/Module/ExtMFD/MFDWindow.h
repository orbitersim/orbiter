// ==============================================================
//                  ORBITER MODULE: ExtMFD
//                  Part of the ORBITER SDK
//            Copyright (C) 2006 Martin Schweiger
//                   All rights reserved
//
// MFDWindow.h
//
// Class interface for MFDWindow. Defines the properties and state
// of an MFD display in a dialog box
// ==============================================================

#ifndef __MFDWINDOW_H
#define __MFDWINDOW_H

#define STRICT 1
#include <windows.h>
#include "orbitersdk.h"

class MFDWindow: public ExternMFD {
public:
	MFDWindow (HINSTANCE _hInst, const MFDSPEC &spec);
	~MFDWindow ();
	void Initialise (HWND _hDlg);
	void SetVessel (OBJHANDLE hV);
	void SetTitle ();
	void Resize (bool fixaspect);
	void RepaintDisplay (HWND hWnd);
	void RepaintButton (HWND hWnd);
	void ProcessButton (int bt, int event);
	void StickToVessel (bool stick);

	void clbkRefreshDisplay (SURFHANDLE);
	void clbkRefreshButtons ();
	void clbkFocusChanged (OBJHANDLE hFocus);

private:
	HINSTANCE hInst;  // instance handle
	HWND hDlg, hDsp;  // dialog and MFD display handles
	HFONT hBtnFnt;    // button font
	int BW, BH;       // button width and height
	int gap;          // geometry parameters
	int fnth;         // button font height
	bool vstick;      // stick to vessel
};

#endif // !__MFDWINDOW_H