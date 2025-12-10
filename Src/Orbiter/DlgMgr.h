// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __DLGMGR_H
#define __DLGMGR_H

#define STRICT 1
#include <windows.h>

#include "DialogWin.h"
#include "Orbiter.h"
#include <list>
#include "imgui.h"
#include "imgui_extras.h"
class ImGuiDialog;

class oapi::GraphicsClient;
extern Orbiter *g_pOrbiter;

struct DIALOGENTRY {
	DialogWin *dlg;
	struct DIALOGENTRY *prev, *next;
};

class DialogManager {
public:
	DialogManager(Orbiter *orbiter, HWND hAppWnd);
	~DialogManager();

	void Init (HWND hAppWnd);
	void Clear ();

	inline HWND OpenDialog (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, void *context)
	{ return OpenDialogEx (hInst, id, hParent, pDlg, 0, context); }

	HWND OpenDialogEx (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, DWORD flag, void *context);

	bool CloseDialog (HWND hDlg);

	void *GetDialogContext (HWND hDlg);
	inline DWORD Size() const { return nEntry; }
	HWND GetNextEntry (HWND hWnd) const;

	bool AddTitleButton (DWORD msg, HBITMAP hBmp, DWORD flag);
	DWORD GetTitleButtonState (HWND hDlg, DWORD msg);
	bool SetTitleButtonState (HWND hDlg, DWORD msg, DWORD state);

	DIALOGENTRY *AddWindow (HINSTANCE hInst, HWND hWnd, HWND hParent, DWORD flag);

	HWND AddEntry (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, DWORD flag, void *context);
	HWND AddEntry (DialogWin *dlg);

	bool DelEntry (HWND hDlg, HINSTANCE hInst, int id);
	// remove dialog entry. If either 'hDlg' or 'id' is 0,
	// only the other component is checked

	HWND IsEntry (HINSTANCE hInst, int id);
	// Returns window handle of dialog with identifier 'id' if it is in the list
	// Otherwise returns 0

	inline DWORD GetDlgList (const HWND **hDlgList) const
	{ *hDlgList = DlgList; return nList; }
	// Returns current dialog window list

	void UpdateDialogs();
	// periodic dialog updates

	void BroadcastMessage (DWORD msg, void *data);
	// broadcast a message to all open dialog windows, using the WM_USER+10 channel

private:
	void AddList (HWND hWnd);
	void DelList (HWND hWnd);
	// add/remove window handle from window list

	DWORD nEntry;
	DIALOGENTRY *firstEntry, *lastEntry;
	mutable DIALOGENTRY *searchEntry;

	HWND *DlgList;
	DWORD nList, nListBuf;

	Orbiter *pOrbiter;
	oapi::GraphicsClient *gc;
	HWND hWnd;

	static LRESULT FAR PASCAL OrbiterCtrl_Level_MsgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	// MessageHandler for slider custom control

	// ====================================================================
	// Tread management for dialog thread
	// ====================================================================

public:
	void OpenDialogAsync (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, DWORD flag, void *context);

protected:
	void StartDialogThread ();
	// start the dialog handler thread (during render window creation)

	void DestroyDialogThread ();
	// kill the dialog handler thread (during render window destruction)

	void AddEntryAsync (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, DWORD flag, void *context);

private:
	HANDLE hThread;          // thread handle
	DWORD thid;              // dialog thread id

	// ====================================================================
	// End tread management
	// ====================================================================


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

INT_PTR OrbiterDefDialogProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

#endif // !__DLGMGR_H