// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ScenarioTab class
//=============================================================================

#include <windows.h>
#include <direct.h>
#include <string>
#include "Orbiter.h"
#include "TabScenario.h"
#include "Launchpad.h"
//#include "Log.h"
#include <imgui_internal.h>

#include "Help.h"
#include "htmlctrl.h"
#include "resource.h"
#include "UIUtil.h"
#include <sstream>

using namespace std;

//-----------------------------------------------------------------------------

orbiter::ScenarioTab::ScenarioTab(const LaunchpadDialog2 *lp): LaunchpadTab2(lp, "Scenarios") {
    scnhelp[0] = '\0';
    htmldesc = m_lp->App()->UseHtmlInline();
}

//-----------------------------------------------------------------------------

orbiter::ScenarioTab::~ScenarioTab() {
    if (img_folder1) {
        delete img_folder1;
        delete img_folder2;
        delete img_scn1;
    }
    for (const auto i: loadedImages) {
        delete i;
    }
    loadedImages.clear();
    // TerminateThread (hThread, 0);
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::Create() {
    img_folder1 = new Image(m_lp->Device(), m_lp->Window(), "Textures/OrbiterCore/Folder1.png");
    img_folder2 = new Image(m_lp->Device(), m_lp->Window(), "Textures/OrbiterCore/Folder2.png");
    img_scn1 = new Image(m_lp->Device(), m_lp->Window(), "Textures/OrbiterCore/Scn1.png");

    RefreshList(false);
    // if (pLp->App()->UseHtmlInline()) {
    // 	ShowWindow (GetDlgItem (hTab, IDC_SCN_DESC), SW_HIDE);
    // 	ShowWindow (GetDlgItem (hTab, IDC_SCN_HTML), SW_SHOW);
    // 	ShowWindow (GetDlgItem (hTab, IDC_SCN_INFO), SW_HIDE);
    // 	infoId = IDC_SCN_HTML;
    // } else {
    // 	ShowWindow (GetDlgItem (hTab, IDC_SCN_HTML), SW_HIDE);
    // 	ShowWindow (GetDlgItem (hTab, IDC_SCN_DESC), SW_SHOW);
    // 	ShowWindow (GetDlgItem (hTab, IDC_SCN_INFO), SW_SHOW);
    // 	infoId = IDC_SCN_DESC;
    // }
    //
    // splitListDesc.SetHwnd (GetDlgItem (hTab, IDC_SCN_SPLIT1), GetDlgItem (hTab, IDC_SCN_LIST), GetDlgItem (hTab, infoId));

    // create a thread to monitor changes to the scenario list
    // hThread = CreateThread (NULL, NULL, threadWatchScnList, this, NULL, NULL);
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::GetConfig(const Config *cfg) {
    startPaused = cfg->CfgLogicPrm.bStartPaused;
    scnListW = cfg->CfgWindowPos.LaunchpadScnListWidth;
    // if (!listw) {
    // 	RECT r;
    // 	GetClientRect (GetDlgItem (hTab, IDC_SCN_LIST), &r);
    // 	listw = r.right-r.left;
    // }
    // splitListDesc.SetStaticPane (SplitterCtrl::PANE1, listw);
}

//-----------------------------------------------------------------------------

// TODO: Use Description.txt for folder
void orbiter::ScenarioTab::SetConfig(Config *cfg) {
    cfg->CfgLogicPrm.bStartPaused = startPaused;
    cfg->CfgWindowPos.LaunchpadScnListWidth = scnListW;
}

void orbiter::ScenarioTab::RenderTree(const ScenarioTree &tree) {
    bool selected = tree.item.path == selection;
    bool prev_selected = selected;
    if (tree.children.empty()) {
        ImGui::SetNextItemAllowOverlap();

        ImGui::PushID(tree.item.path.u8string().c_str());
        ImGui::Selectable("##Select", &selected);
        ImGui::PopID();
        ImGui::SameLine(0.0, 0.0);
        ImGui::Bullet();

        if (selected && !prev_selected) {
            selection = tree.item.path;
            ScenarioChanged();
        }
        ImGui::SameLine();
        ImGui::Image(selected
                         ? reinterpret_cast<ImTextureID>(tree.item.selIcon->Binding())
                         : reinterpret_cast<ImTextureID>(tree.item.icon->Binding()), ImVec2(16, 16));
        ImGui::SameLine();
        ImGui::Text(tree.item.name.c_str());
        return;
    }

    ImGui::PushID(tree.item.path.u8string().c_str());
    bool wasOpen = ImGui::TreeNodeGetOpen(ImGui::GetID("##TreeNode"));
    bool treeNode = ImGui::TreeNodeEx("##TreeNode", ImGuiTreeNodeFlags_SpanAvailWidth);
    if (treeNode != wasOpen) {
        selection = tree.item.path;
        ScenarioChanged();
    }
    ImGui::SameLine(0, 0);
    ImGui::Image(treeNode
                     ? reinterpret_cast<ImTextureID>(tree.item.selIcon->Binding())
                     : reinterpret_cast<ImTextureID>(tree.item.icon->Binding()), ImVec2(16, 16));
    ImGui::SameLine();
    ImGui::Text(tree.item.name.c_str());
    if (treeNode) {
        for (const auto &subtree: tree.children) {
            RenderTree(subtree);
        }
        ImGui::TreePop();
    }
    ImGui::PopID();
}

void orbiter::ScenarioTab::OnDraw(WithLocalContext &ctx) {
    ImGui::BeginChild("ScnList", ImVec2(static_cast<float>(scnListW), 0),
                      ImGuiChildFlags_Border | ImGuiChildFlags_ResizeX,
                      ImGuiWindowFlags_HorizontalScrollbar);
    scnListW = ImGui::GetWindowWidth();
    for (const auto &subtree: tree.children) {
        RenderTree(subtree);
    }
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("ScnDesc", ImVec2(0, 0),
                      ImGuiChildFlags_None,
                      ImGuiWindowFlags_HorizontalScrollbar);
    Markdown(ctx, desc, loadedImages);
    ImGui::EndChild();
}

// TODO: Recursion limit?
orbiter::ScenarioTree orbiter::ScenarioTab::BuildScnTree(const ScenarioTreeItem &root) {
    auto tree = ScenarioTree{};
    tree.item = root;
    for (const auto &elem: fs::directory_iterator(std::filesystem::path(root.path))) {
        if (elem.is_directory()) {
            auto item = ScenarioTreeItem{};
            item.icon = img_folder1;
            item.selIcon = img_folder2;
            item.path = elem.path();
            item.name = elem.path().filename().u8string();
            tree.children.push_back(BuildScnTree(item));
        } else {
            if (elem.path().filename().u8string() == "Description.txt") {
                continue;
            }
            auto item = ScenarioTreeItem{};
            item.icon = img_scn1;
            item.selIcon = img_scn1;
            item.path = elem.path();
            item.name = elem.path().stem().u8string();
            auto subtree = ScenarioTree{};
            subtree.item = item;
            tree.children.push_back(subtree);
        }
    }
    return tree;
}

void orbiter::ScenarioTab::RefreshList(bool preserveSelection) {
    if (!preserveSelection)
        selection = fs::path();
    ScenarioTreeItem root = {};
    root.icon = img_folder1;
    root.selIcon = img_folder2;
    root.path = fs::path(m_cfg->CfgDirPrm.ScnDir);
    root.name = "Scenarios";
    tree = BuildScnTree(root);
}

void orbiter::ScenarioTab::ScenarioChanged() {
    std::ifstream file;
    if (is_directory(selection)) {
        file.open(fs::path(selection).append("Description.txt").c_str());
    } else {
        file.open(selection);
    }
    std::string line;
    if (!file.is_open()) {
        // TODO: log error?
        return;
    }
    bool in_desc = false;
    std::stringstream desc;
    while (std::getline(file, line)) {
        if (line.find("BEGIN_DESC") != std::string::npos) {
            in_desc = true;
        } else if (line.find("END_DESC") != std::string::npos) {
            break;
        } else if (in_desc) {
            desc << line << "\n";
        }
    }
    this->desc = desc.str();
}


// //-----------------------------------------------------------------------------
//
// char *ScanFileDesc (std::istream &is, const char *blockname)
// {
// 	char *buf = 0;
// 	char blockbegin[256] = "BEGIN_";
// 	char blockend[256] = "END_";
// 	strncpy (blockbegin+6, blockname, 240);
// 	strncpy (blockend+4, blockname, 240);
//
// 	if (FindLine (is, blockbegin)) {
// 		int i, len, buflen = 0;
// 		const int linelen = 256;
// 		char line[linelen];
// 		for(i = 0;; i++) {
// 			if (!is.getline(line, linelen-2)) {
// 				if (is.eof()) break;
// 				else is.clear();
// 			}
// 			if (_strnicmp (line, blockend, strlen(blockend))) {
// 				len = strlen(line);
// 				if (len) strcat (line, " "), len++;    // convert newline to space
// 				else     strcpy (line, "\r\n"), len=2; // convert empty line to CR
// 				char *tmp = new char[buflen+len+1];
// 				if (buflen) {
// 					memcpy (tmp, buf, buflen*sizeof(char));
// 					delete []buf;
// 				}
// 				memcpy (tmp+buflen, line, len*sizeof(char));
// 				buflen += len;
// 				tmp[buflen] = '\0';
// 				buf = tmp;
// 			} else {
// 				break;
// 			}
// 		}
// 	}
// 	return buf;
// }
//

// //-----------------------------------------------------------------------------
//
// void orbiter::ScenarioTab::ScenarioChanged ()
// {
// 	const int linelen = 256;
// 	bool have_info = false;
// 	char cbuf[256], path[256], *pc;
// 	ifstream ifs;
// 	scnhelp[0] = '\0';
//
// 	switch (GetSelScenario (cbuf, 256)) {
// 	case 0: // error
// 		return;
// 	case 1: // scenario file
// 		ifs.open (pLp->App()->ScnPath (cbuf));
// 		pLp->EnableLaunchButton (true);
// 		break;
// 	case 2: // subdirectory
// 		strcpy (path, pCfg->CfgDirPrm.ScnDir);
// 		strcat (path, cbuf);
// 		strcat (path, "\\Description.txt");
// 		ifs.open (path, ios::in);
// 		pLp->EnableLaunchButton (false);
// 		break;
// 	}
// 	if (ifs) {
// 		if (!have_info) {
// 			char *buf;
// 			if (htmldesc) {
// 				buf = ScanFileDesc(ifs, "URLDESC");
// 				if (buf) {
// 					char url_ref[256], url[256], *path, *topic;
// 					strncpy(url_ref, trim_string(buf), 255);
// 					path = strtok(url_ref, ",");
// 					topic = strtok(NULL, "\n");
// 					if (topic)
// 						sprintf(url, "its:Html\\Scenarios\\%s.chm::%s.htm", path, topic);
// 					else
// 						sprintf(url, "%s\\Html\\Scenarios\\%s.htm", _getcwd(url, 256), path);
// 					DisplayHTMLPage(GetDlgItem(hTab, IDC_SCN_HTML), url);
// 					have_info = true;
// 				}
// 				else {
// 					buf = ScanFileDesc(ifs, "HYPERDESC");
// 					if (!buf) {
// 						buf = ScanFileDesc(ifs, "DESC");
// 						if (buf) {
// 							std::string str(buf);
// 							Text2Html(str);
// 							delete[]buf;
// 							buf = new char[str.size() + 1];
// 							strcpy(buf, str.c_str());
// 						}
// 					}
// 					if (buf) { // prepend style preamble
// 						char* buf2 = new char[strlen(htmlstyle) + strlen(buf) + 1];
// 						strcpy(buf2, htmlstyle); strcat(buf2, buf);
// 						delete[]buf;
// 						buf = buf2;
// 						DisplayHTMLStr(GetDlgItem(hTab, IDC_SCN_HTML), buf);
// 						have_info = true;
// 					}
// 				}
// 			} else {
// 				if (buf = ScanFileDesc (ifs, "DESC")) {
// 					SetWindowText(GetDlgItem(hTab, IDC_SCN_DESC), buf);
// 					have_info = true;
// 				} else if (buf = ScanFileDesc (ifs, "HYPERDESC")) {
// 					std::string str(buf);
// 					Html2Text(str);
// 					SetWindowText(GetDlgItem(hTab, IDC_SCN_DESC), str.c_str());
// 					have_info = true;
// 				}
// 			}
// 			if (buf) {
// 				delete []buf;
// 				buf = NULL;
// 			}
// 		}
// 	}
//
// 	if (!have_info) {
// 		if (htmldesc) DisplayHTMLStr (GetDlgItem (hTab, IDC_SCN_HTML), "");
// 		else          SetWindowText (GetDlgItem (hTab, IDC_SCN_DESC), "");
// 	}
//
// 	if (!htmldesc) {
// 		bool enable_info = false;
// 		for (int i = 0; scnhelp[i]; i++)
// 			if (scnhelp[i] == ',') {
// 				enable_info = true;
// 				break;
// 			}
// 		EnableWindow (GetDlgItem (hTab, IDC_SCN_INFO), enable_info ? TRUE:FALSE);
// 	}
// }
//
// //-----------------------------------------------------------------------------
//
// int orbiter::ScenarioTab::GetSelScenario (char *scn, int len)
// {
// 	TV_ITEM tvi;
// 	char cbuf[256];
// 	int type;
//
// 	tvi.mask = TVIF_HANDLE | TVIF_TEXT | TVIF_CHILDREN;
// 	tvi.hItem = TreeView_GetSelection (GetDlgItem (hTab, IDC_SCN_LIST));
// 	tvi.pszText = scn;
// 	tvi.cchTextMax = len;
//
// 	if (!TreeView_GetItem (GetDlgItem (hTab, IDC_SCN_LIST), &tvi)) return 0;
// 	type = (tvi.cChildren ? 2 : 1);
//
// 	// build path
// 	tvi.pszText = cbuf;
// 	tvi.cchTextMax = 256;
// 	while (tvi.hItem = TreeView_GetParent (GetDlgItem (hTab, IDC_SCN_LIST), tvi.hItem)) {
// 		if (TreeView_GetItem (GetDlgItem (hTab, IDC_SCN_LIST), &tvi)) {
// 			strcat (cbuf, "\\");
// 			strcat (cbuf, scn);
// 			strcpy (scn, cbuf);
// 		}
// 	}
// 	return type;
// }
//
// //-----------------------------------------------------------------------------
//
// void orbiter::ScenarioTab::SaveCurScenario ()
// {
// 	ifstream ifs (pLp->App()->ScnPath (CurrentScenario), ios::in);
// 	if (ifs) {
// 		DialogBoxParam (AppInstance(), MAKEINTRESOURCE(IDD_SAVESCN), LaunchpadWnd(), SaveProc, (LPARAM)this);
// 	} else {
// 		MessageBox (LaunchpadWnd(), "No current simulation state available", "Save Error", MB_OK|MB_ICONEXCLAMATION);
// 	}
// }
//
// //-----------------------------------------------------------------------------
// // Name: SaveCurScenarioAs()
// // Desc: copy current scenario file into 'name', replacing description with 'desc'.
// //		 return value: 0=ok, 1=failed, 2=file exists (only checked if replace=false)
// //-----------------------------------------------------------------------------
// int orbiter::ScenarioTab::SaveCurScenarioAs (const char *name, char *desc, bool replace)
// {
// 	string cbuf;
// 	bool skip = false;
// 	const char *path = pLp->App()->ScnPath (name);
// 	if (!replace) { // check if exists
// 		ifstream ifs (path, ios::in);
// 		if (ifs) return 2;
// 	}
// 	ofstream ofs (path);
// 	if (!ofs) return 1;
// 	ifstream ifs (pLp->App()->ScnPath (CurrentScenario));
// 	if (!ifs) return 1;
// 	int i, len = strlen(desc);
// 	for (i = 0; i < len-1; i++)
// 		if (desc[i] == '\r' && desc[i+1] == '\n') desc[i] = '\n';
// 	ofs << "BEGIN_DESC" << endl;
// 	ofs << desc << endl;
// 	ofs << "END_DESC" << endl;
// 	while (std::getline( ifs, cbuf ))
// 	{
// 		if (cbuf == "BEGIN_DESC")
// 			skip = true;
// 		else if (cbuf == "END_DESC")
// 			skip = false;
// 		else if (!skip)
// 			ofs << cbuf << endl;
// 	}
// 	return 0;
// }
//
// //-----------------------------------------------------------------------------
// // Name: SaveProc()
// // Desc: Scenario save dialog message proc
// //-----------------------------------------------------------------------------
// INT_PTR CALLBACK orbiter::ScenarioTab::SaveProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
// {
// 	static ScenarioTab *pTab;
// 	int res, name_len, desc_len;
// 	static char name[64], *desc;
//
// 	switch (uMsg) {
// 	case WM_INITDIALOG:
// 		pTab = (ScenarioTab*)lParam;
// 		return TRUE;
// 	case WM_COMMAND:
// 		switch (LOWORD(wParam)) {
// 		case IDOK:
// 			name_len = SendDlgItemMessage (hWnd, IDC_SAVE_NAME, WM_GETTEXTLENGTH, 0, 0);
// 			desc_len = SendDlgItemMessage (hWnd, IDC_SAVE_DESC, WM_GETTEXTLENGTH, 0, 0);
// 			if (name_len > 63) {
// 				MessageBox (hWnd, "Scenario name too long (max 63 characters)", "Save Error", MB_OK|MB_ICONEXCLAMATION);
// 				return TRUE;
// 			}
// 			desc = new char[desc_len+1];
// 			SendDlgItemMessage (hWnd, IDC_SAVE_NAME, WM_GETTEXT, 64, (LPARAM)name);
// 			SendDlgItemMessage (hWnd, IDC_SAVE_DESC, WM_GETTEXT, desc_len+1, (LPARAM)desc);
// 			res = pTab->SaveCurScenarioAs (name, desc);
// 			if (res == 2) {
// 				if (MessageBox (hWnd, "File exists. Overwrite?", "Warning", MB_YESNO|MB_ICONQUESTION) == IDYES)
// 					res = pTab->SaveCurScenarioAs (name, desc, true);
// 				else return TRUE;
// 			}
// 			if (res == 1) {
// 				MessageBox (hWnd, "Error writing scenario file.", "Save Error", MB_OK|MB_ICONEXCLAMATION);
// 				return TRUE;
// 			}
// 			delete []desc;
// 			desc = NULL;
// 			// fall through
// 		case IDCANCEL:
// 			EndDialog (hWnd, TRUE);
// 			return TRUE;
// 		}
// 	}
//     return FALSE;
// }

//-----------------------------------------------------------------------------
// Name: ClearQSFolder()
// Desc: Delete all scenarios in the Quicksave folder
//-----------------------------------------------------------------------------
void orbiter::ScenarioTab::ClearQSFolder() {
    fs::path scnpath{m_lp->App()->ScnPath("Quicksave")};
    scnpath.replace_extension(); // remove ".scn"

    std::error_code ec;
    fs::remove_all(scnpath, ec);
    if (!ec) {
        fs::create_directory(scnpath);
    }
}

// //-----------------------------------------------------------------------------
// // Name: OpenScenarioHelp()
// // Desc: Opens the help file associated with the scenario
// //-----------------------------------------------------------------------------
// void orbiter::ScenarioTab::OpenScenarioHelp ()
// {
// 	if (!scnhelp[0]) return;
// 	char str[256], path[256], *scenario, *topic;
// 	strncpy (str, scnhelp, 256);
// 	scenario = strtok (str, ",");
// 	topic = strtok (NULL, "\n");
// 	sprintf(path, "html\\scenarios\\%s.chm", scenario);
// 	::OpenHelp(LaunchpadWnd(), path, topic);
// }
//
// //-----------------------------------------------------------------------------
// // Thread function for scenario directory tree watcher
// //-----------------------------------------------------------------------------
// DWORD WINAPI orbiter::ScenarioTab::threadWatchScnList (LPVOID pPrm)
// {
// 	ScenarioTab *tab = (ScenarioTab*)pPrm;
// 	HANDLE dwChangeHandle;
// 	DWORD dwWaitStatus;
//
// 	dwChangeHandle = FindFirstChangeNotification (
// 		tab->m_cfg->CfgDirPrm.ScnDir,
// 		TRUE, FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME);
//
// 	while (true) {
// 		dwWaitStatus = WaitForSingleObject (dwChangeHandle, INFINITE);
// 		switch (dwWaitStatus) {
// 			case WAIT_OBJECT_0:
// 				tab->RefreshList(true);
// 				FindNextChangeNotification (dwChangeHandle);
// 				break;
// 		}
// 	}
// 	FindCloseChangeNotification(dwChangeHandle);
// 	return 0;
// }
