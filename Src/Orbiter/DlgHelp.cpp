// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Help window
// ======================================================================
#include "DlgHelp.h"
#include "Help.h"

// The help pages are shown in the native help window; this dialog draws nothing itself
DlgHelp::DlgHelp():ImGuiDialog("Orbiter: Help") {}
void DlgHelp::Display() {}
void DlgHelp::OnDraw() {}

void DlgHelp::OpenHelp(const HELPCONTEXT *hc)
{
	::OpenHelp(hc->helpfile, hc->topic);
}
