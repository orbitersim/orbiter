// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Help window
// ======================================================================
#include "DlgHelp.h"
#ifndef __linux__
#include <htmlhelp.h>
#include <io.h>
#else // __linux__
#include "ChmHelp.h"
#endif // __linux__
#include "imgui.h"

// This is just a placeholder to call the HtmlHelp API
// We draw nothing ourselves
DlgHelp::DlgHelp():ImGuiDialog("Orbiter: Help") {}
void DlgHelp::Display() {}
void DlgHelp::OnDraw() {}

void DlgHelp::OpenHelp(const HELPCONTEXT *hc)
{
	char buf[256];
#ifndef __linux__
	HWND hWnd = (HWND)(ImGui::GetMainViewport()->PlatformHandle);
#else // __linux__
	// the help window is a top-level window of its own (the render window is a QWindow, not a widget)
#endif // __linux__
	if(hc->topic)
		snprintf(buf, 256, "%s::%s", hc->helpfile, hc->topic);
	else
		snprintf(buf, 256, "%s", hc->helpfile);

	buf[255] = '\0';

#ifndef __linux__
	if(!HtmlHelp (hWnd, buf, HH_DISPLAY_TOPIC, NULL)) {
#else // __linux__
	if(!HtmlHelp (NULL, buf, NULL)) {
#endif // __linux__
		oapiAddNotification(OAPINOTIF_ERROR, "Failed to open help", buf);
	}
}
