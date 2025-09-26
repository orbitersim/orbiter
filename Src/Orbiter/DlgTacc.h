// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Time acceleration dialog
// ======================================================================

#ifndef __DLGTACC_H
#define __DLGTACC_H

#include "OrbiterAPI.h"

class DlgTacc : public ImGuiDialog {
public:
    DlgTacc();
    void OnDraw() override;
};

#endif // !__DLGTACC_H