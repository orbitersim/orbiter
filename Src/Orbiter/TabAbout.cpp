// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// AboutTab class
//=============================================================================

#include "TabAbout.h"
#include "Help.h"
#include "Orbiter.h"
#include "Util.h"
#include "about.hpp"
#include "resource.h"
#include <commctrl.h>
#include <io.h>
#include <windows.h>

//-----------------------------------------------------------------------------
// AboutTab class

orbiter::AboutTab::AboutTab(const LaunchpadDialog2 *lp)
    : LaunchpadTab2(lp, "About"), savedWidth(0) {
    icon =
        std::make_shared<LpImage>(lp->Win(), "Textures/OrbiterCore/Icon64.png");
}

//-----------------------------------------------------------------------------

void orbiter::AboutTab::RenderCentered(oapi::WithImCtx<LpImCtx> &ctx) {
    ImGui::BeginGroup();

    ImGui::Image(icon->TexID(), ImVec2(64, 64));
    ImGui::SameLine();
    ImGui::SetCursorPosY(ImGui::GetFrameHeight() / 2);
    ImGui::BeginGroup();
    ImGui::Text(NAME1);
    ImGui::Text(SIG4);
    ImGui::Text(SIG1B);
    ImGui::EndGroup();

    ImGui::EndGroup();
}

void orbiter::AboutTab::OnDraw(oapi::WithImCtx<LpImCtx> &ctx) {
    ImVec2 center = ImGui::GetContentRegionAvail();
    ImGui::SetNextWindowPos(center, ImGuiCond_Always, ImVec2(0.5f, 0.5f));
    if (ImGui::BeginChild("##About", ImVec2(512, 512), ImGuiChildFlags_Border,
                          ImGuiWindowFlags_None)) {
        // HACK: center this window by rendering first with 0 alpha, then saving
        // the width. Content is not dynamic width so it won't matter.
        if (savedWidth == 0) {
            ImGui::PushStyleVar(ImGuiStyleVar_Alpha, 0.0f);
            RenderCentered(ctx);
            ImGui::PopStyleVar();
            savedWidth = ImGui::GetItemRectSize().x;
        }
        ImGui::SetCursorPosX(ImGui::GetContentRegionAvail().x / 2 -
                             savedWidth / 2);
        RenderCentered(ctx);

        if (ImGui::CollapsingHeader("Installed components")) {
            for (const auto &module : m_lp->App()->LoadedModules()) {
                if (module.pModule) {
                    if (const auto copy =
                            module.pModule->clbkGetModuleCopyright()) {
                        ImGui::BulletText(copy);
                    }
                }
            }
        }
    }
    ImGui::EndChild();
}

//
// void orbiter::AboutTab::Create ()
// {
// 	hTab = CreateTab (IDD_PAGE_ABT);
//
// 	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_NAME), NAME1);
// 	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_BUILDDATE), SIG4);
// 	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_CPR), SIG1B);
// 	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_WEBADDR), SIG2 "\n" SIG5
// "\n" SIG6); 	SendDlgItemMessage(hTab, IDC_ABT_LBOX_COMPONENT, LB_ADDSTRING,
// 0, 		(LPARAM)"D3D9Client module by Jarmo Nikkanen and Peter
// Schneider"
// 	);
// 	SendDlgItemMessage(hTab, IDC_ABT_LBOX_COMPONENT, LB_ADDSTRING, 0,
// 		(LPARAM)"XRSound module Copyright (c) Doug Beachy"
// 	);
// }
//
// //-----------------------------------------------------------------------------
//
// BOOL orbiter::AboutTab::OnMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM
// lParam)
// {
// 	switch (uMsg) {
// 	case WM_COMMAND:
// 		switch (LOWORD(wParam)) {
// 		case IDC_ABT_WEB:
// 			ShellExecute (NULL, "open",
// "http://orbit.medphys.ucl.ac.uk/", NULL, NULL, SW_SHOWNORMAL);
// return true; 		case IDC_ABT_DISCLAIM:
// DialogBoxParam (AppInstance(), MAKEINTRESOURCE(IDD_MSG), LaunchpadWnd(),
// AboutProc, IDT_DISCLAIMER); 			return TRUE; 		case
// IDC_ABT_CREDIT:
// 			::OpenHelp(hWnd, "html\\Credit.chm", "Credit");
// 			return TRUE;
// 		}
// 		break;
// 	}
// 	return FALSE;
// }
//
// //-----------------------------------------------------------------------------
// // Name: AboutProc()
// // Desc: Minimal message proc function for the about box
// //-----------------------------------------------------------------------------
// INT_PTR CALLBACK orbiter::AboutTab::AboutProc (HWND hWnd, UINT uMsg, WPARAM
// wParam, LPARAM lParam)
// {
// 	switch (uMsg) {
// 	case WM_INITDIALOG:
// 		SetWindowText(GetDlgItem(hWnd, IDC_MSG),
// 			(char*)LockResource(LoadResource(NULL,
// FindResource(NULL, MAKEINTRESOURCE(lParam), "TEXT")))
// 		);
// 		return TRUE;
// 	case WM_COMMAND:
// 		if (IDOK == LOWORD(wParam) || IDCANCEL == LOWORD(wParam))
// 			EndDialog (hWnd, TRUE);
// 		return TRUE;
// 	}
//     return FALSE;
// }
//
