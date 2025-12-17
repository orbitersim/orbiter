// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Help window
// ======================================================================

#ifndef __DLGHELP_H
#define __DLGHELP_H
#include "OrbiterAPI.h"


class DlgHelp: public ImGuiDialog
{
public:
    DlgHelp();
    void OnDraw() override;
	void Display() override;
	void OpenHelp(const HELPCONTEXT *hc);
};
#endif // !__DLGHELP_H
