// ==============================================================          
// Copyright (C) 2006-2026 Martin Schweiger
// Licensed under the MIT License
// ==============================================================

#ifndef __MFDWINDOW_H
#define __MFDWINDOW_H

// STRICT left out: windows.h handle type-checking switch
// windows.h left out: the Win32 types come from OrbiterPlatform.h
#include "Orbitersdk.h"
#include "gcCoreAPI.h"

class MFDWindow: public ExternMFD {
public:
	MFDWindow (void *_hInst, const MFDSPEC &spec);
	~MFDWindow ();
	void Initialise (QWidget *_hDlg);
	void SetVessel (OBJHANDLE hV);
	void SetTitle ();
	void Resize();
	void CheckAspect (LPRECT, DWORD);
	void RepaintButton (QWidget *hWnd);
	// RepaintDisplay left out: the display is a Vulkan window (see MFDWindow.cpp)
	void ProcessButton (int bt, int event);
	void StickToVessel (bool stick);

	void clbkRefreshDisplay (SURFHANDLE);
	void clbkRefreshButtons ();
	void clbkFocusChanged (OBJHANDLE hFocus);

private:
	RECT wr;
	HSWAP hSwap;
	void *hInst;      // instance handle
	QWidget *hDlg, *hDsp; // dialog and MFD display handles
	QFont *hBtnFnt;   // button font
	int BW, BH, ds;   // button width and height, display size
	int gap;          // geometry parameters
	int fnth;         // button font height
	bool vstick;      // stick to vessel
	bool bFailed;
};

#endif // !__MFDWINDOW_H

