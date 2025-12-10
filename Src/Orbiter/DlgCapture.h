// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Screen capture window
// ======================================================================

#ifndef __DLGCAPTURE_H
#define __DLGCAPTURE_H

#include "OrbiterAPI.h"
// ======================================================================
// Class for screen capture dialog

class DlgCapture : public ImGuiDialog {
	void AutoIncrement(char *);
public:
    DlgCapture();
    void OnDraw() override;
};

#endif // !__DLGCAPTURE_H