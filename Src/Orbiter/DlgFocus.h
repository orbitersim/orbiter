// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Focus vessel selection dialog
// ======================================================================

#ifndef __DLGFOCUS_H
#define __DLGFOCUS_H

#include "OrbiterAPI.h"

class DlgFocus : public ImGuiDialog {
public:
    DlgFocus();
    void OnDraw() override;
    void DrawAll();
    void DrawNearby();
    void DrawLocation();
    void DrawClass();
    std::string m_SelectedShip;
    float m_Range = 1.0;
};

#endif // !__DLGFOCUS_H