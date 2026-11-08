// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __DLGMGR_H
#define __DLGMGR_H

#define STRICT 1
#include <windows.h>

#include "Orbiter.h"
#include <list>
#include "imgui.h"
#include "imgui_extras.h"
class ImGuiDialog;

namespace oapi { class GraphicsClient; }
extern Orbiter *g_pOrbiter;

class DialogManager {
public:
	DialogManager(Orbiter *orbiter, HWND hAppWnd);
	~DialogManager();

private:
	Orbiter *pOrbiter;
	oapi::GraphicsClient *gc;
	HWND hWnd;

	// ====================================================================
	// ImGui management
	// ====================================================================
	std::list<ImGuiDialog*> DlgImGuiList;
public:
	// Make sure that a dialog of type DlgType is open and return a pointer to it.
	// This opens the dialog if not yet present.
	// Use this function for dialogs that should only have a single instance
	template<typename T, std::enable_if_t<std::is_base_of_v<ImGuiDialog, T>, bool> = true>
	T* EnsureEntry()
	{
		T* dlg = EntryExists<T>();
		if(!dlg)
			dlg = MakeEntry<T>();
		if(dlg)
			dlg->Activate();
		return dlg;
	}

	// Create a new instance of dialog type DlgType and return a pointer to it.
	// This opens a new dialog, even if one of this type was open already.
	// Use this function for dialogs that can have multiple instances.
	template<typename T, std::enable_if_t<std::is_base_of_v<ImGuiDialog, T>, bool> = true>
	T* MakeEntry()
	{
		T* pDlg = new T();
		AddEntry(pDlg);
		return pDlg;
	}

	// Returns a pointer to the first instance of dialog type DlgType,
	// or 0 if no instance exists.
	template<typename T, std::enable_if_t<std::is_base_of_v<ImGuiDialog, T>, bool> = true>
	T* EntryExists()
	{
		for (auto& e : DlgImGuiList) {
			T *dlg = dynamic_cast<T *>(e);
			if(dlg) return dlg;
		}
		return nullptr;
	}

	void AddEntry(ImGuiDialog* dlg)
	{
		for (auto& e : DlgImGuiList) {
			if (e == dlg) {
				return;
			}
		}
		DlgImGuiList.push_back(dlg);
	}

	bool DelEntry(ImGuiDialog* dlg)
	{
		for (auto it = DlgImGuiList.begin(); it != DlgImGuiList.end(); ) {
			if (*it == dlg) {
				it = DlgImGuiList.erase(it);
				return true;
			}
			else {
				++it;
			}
		}
		return false;
	}

	void ImGuiNewFrame();
	struct ImGuiContext *m_imguiCtx = nullptr;
	ImFont *GetFont(ImGuiFont f);

	void SetMainColor(COLORREF col);
private:
	void InitImGui();
	void ShutdownImGui();
	ImFont *defaultFont;
	ImFont *consoleFont;
	ImFont *monoFont;
	ImFont *manuscriptFont;
};

#endif // !__DLGMGR_H