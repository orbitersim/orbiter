// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Help window
// ======================================================================
#include "DlgHelp.h"
#include <htmlhelp.h>
#include <io.h>
#include "imgui.h"

// This is just a placeholder to call the HtmlHelp API
// We draw nothing ourselves
DlgHelp::DlgHelp():ImGuiDialog("Orbiter: Help") {}
void DlgHelp::Display() {}
void DlgHelp::OnDraw() {}

void DlgHelp::OpenHelp(const HELPCONTEXT *hc)
{
	char buf[256];
	HWND hWnd = (HWND)(ImGui::GetMainViewport()->PlatformHandle);
	if(hc->topic)
		snprintf(buf, 256, "%s::%s", hc->helpfile, hc->topic);
	else
		snprintf(buf, 256, "%s", hc->helpfile);

	buf[255] = '\0';

	if(!HtmlHelp (hWnd, buf, HH_DISPLAY_TOPIC, NULL)) {
		oapiAddNotification(OAPINOTIF_ERROR, "Failed to open help", buf);
	}
}
