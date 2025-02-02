// Copyright (c) Martin Schweiger
// Licensed under the MIT License

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

#include "GraphicsAPI.h"

class MFDWindow : public ExternMFD {
public:
	MFDWindow(const MFDSPEC& spec);
	virtual ~MFDWindow();
	void SetVessel(OBJHANDLE hV) override;
	void ProcessButton(int bt, int event);
	void ToggleStickToVessel();
	bool GetStickToVessel() { return vstick; }
	void clbkFocusChanged(OBJHANDLE hFocus) override;

private:
	int BW, BH;       // button width and height
	int fnth;         // button font height
	bool vstick;      // stick to vessel
	std::unique_ptr<ImGuiDialog> m_window;
};

#endif // !__MFDWINDOW_H