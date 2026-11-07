// ==============================================================
// Copyright (C) 2006-2026 Martin Schweiger
// Licensed under the MIT License
// ==============================================================

#ifndef __MFDWINDOW_H
#define __MFDWINDOW_H

#include "orbitersdk.h"
#include <memory>

class MFDWindow: public ExternMFD {
public:
	MFDWindow (const MFDSPEC &spec);
	~MFDWindow ();
	void SetVessel (OBJHANDLE hV) override;
	void ProcessButton (int bt, int event);
	void StickToVessel (bool stick);
	bool GetStickToVessel () const { return vstick; }
	void SetDisplaySize (int size);
	int GetDisplaySize () const { return DW; }

	void clbkFocusChanged (OBJHANDLE hFocus) override;

private:
	int windowId;
	bool vstick;
	std::unique_ptr<ImGuiDialog> window;
};

#endif // !__MFDWINDOW_H
