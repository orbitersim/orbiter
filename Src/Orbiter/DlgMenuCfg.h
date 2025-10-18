// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Menu bar configuration dialog
// ======================================================================

#ifndef __DLGMENUCFG_H
#define __DLGMENUCFG_H

#include "OrbiterAPI.h"

class DlgMenuCfg: public ImGuiDialog {
public:
	DlgMenuCfg ();
	void OnDraw();
};

#endif // !__DLGMENUCFG_H