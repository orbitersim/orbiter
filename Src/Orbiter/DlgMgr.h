// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __DLGMGR_H
#define __DLGMGR_H

#ifndef __linux__
#define STRICT 1
#include <windows.h>
#else // __linux__
#include "OrbiterPlatform.h"
#endif // __linux__

#include "DialogWin.h"
#include "Orbiter.h"
#include <list>
#include "imgui.h"
#include "imgui_extras.h"
class ImGuiDialog;

#ifndef __linux__
class oapi::GraphicsClient;
#else // __linux__
namespace oapi { class GraphicsClient; }
#endif // __linux__
extern Orbiter *g_pOrbiter;

struct DIALOGENTRY {
	DialogWin *dlg;
	struct DIALOGENTRY *prev, *next;
};

class DialogManager {
public:
#ifndef __linux__
	DialogManager(Orbiter *orbiter, HWND hAppWnd);
#else // __linux__
	DialogManager(Orbiter *orbiter, QWindow *hAppWnd);
#endif // __linux__
	~DialogManager();

#ifndef __linux__
	void Init (HWND hAppWnd);
#else // __linux__
	void Init (QWindow *hAppWnd);
#endif // __linux__
	void Clear ();

#ifndef __linux__
	inline HWND OpenDialog (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, void *context)
#else // __linux__
	inline QWidget *OpenDialog (void *hInst, int id, QWindow *hParent, DLGINIT pDlg, void *context)
#endif // __linux__
	{ return OpenDialogEx (hInst, id, hParent, pDlg, 0, context); }

#ifndef __linux__
	HWND OpenDialogEx (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, DWORD flag, void *context);
#else // __linux__
	QWidget *OpenDialogEx (void *hInst, int id, QWindow *hParent, DLGINIT pDlg, DWORD flag, void *context);
#endif // __linux__

#ifndef __linux__
	bool CloseDialog (HWND hDlg);
#else // __linux__
	bool CloseDialog (QWidget *hDlg);
#endif // __linux__

#ifndef __linux__
	void *GetDialogContext (HWND hDlg);
#else // __linux__
	void *GetDialogContext (QWidget *hDlg);
#endif // __linux__
	inline DWORD Size() const { return nEntry; }
#ifndef __linux__
	HWND GetNextEntry (HWND hWnd) const;
#else // __linux__
	QWidget *GetNextEntry (QWidget *hWnd) const;
#endif // __linux__

#ifndef __linux__
	bool AddTitleButton (DWORD msg, HBITMAP hBmp, DWORD flag);
	DWORD GetTitleButtonState (HWND hDlg, DWORD msg);
	bool SetTitleButtonState (HWND hDlg, DWORD msg, DWORD state);
#else // __linux__
	bool AddTitleButton (DWORD msg, QImage *hBmp, DWORD flag);
	DWORD GetTitleButtonState (QWidget *hDlg, DWORD msg);
	bool SetTitleButtonState (QWidget *hDlg, DWORD msg, DWORD state);
#endif // __linux__

#ifndef __linux__
	DIALOGENTRY *AddWindow (HINSTANCE hInst, HWND hWnd, HWND hParent, DWORD flag);
#else // __linux__
	DIALOGENTRY *AddWindow (void *hInst, QWidget *hWnd, QWindow *hParent, DWORD flag);
#endif // __linux__

#ifndef __linux__
	HWND AddEntry (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, DWORD flag, void *context);
	HWND AddEntry (DialogWin *dlg);
#else // __linux__
	QWidget *AddEntry (void *hInst, int id, QWindow *hParent, DLGINIT pDlg, DWORD flag, void *context);
	QWidget *AddEntry (DialogWin *dlg);
#endif // __linux__

#ifndef __linux__
	bool DelEntry (HWND hDlg, HINSTANCE hInst, int id);
#else // __linux__
	bool DelEntry (QWidget *hDlg, void *hInst, int id);
#endif // __linux__
	// remove dialog entry. If either 'hDlg' or 'id' is 0,
	// only the other component is checked

#ifndef __linux__
	HWND IsEntry (HINSTANCE hInst, int id);
#else // __linux__
	QWidget *IsEntry (void *hInst, int id);
#endif // __linux__
	// Returns window handle of dialog with identifier 'id' if it is in the list
	// Otherwise returns 0

#ifndef __linux__
	inline DWORD GetDlgList (const HWND **hDlgList) const
#else // __linux__
	inline DWORD GetDlgList (QWidget *const **hDlgList) const
#endif // __linux__
	{ *hDlgList = DlgList; return nList; }
	// Returns current dialog window list

	void UpdateDialogs();
	// periodic dialog updates

	void BroadcastMessage (DWORD msg, void *data);
	// broadcast a message to all open dialog windows, using the WM_USER+10 channel

private:
#ifndef __linux__
	void AddList (HWND hWnd);
	void DelList (HWND hWnd);
#else // __linux__
	void AddList (QWidget *hWnd);
	void DelList (QWidget *hWnd);
#endif // __linux__
	// add/remove window handle from window list

	DWORD nEntry;
	DIALOGENTRY *firstEntry, *lastEntry;
	mutable DIALOGENTRY *searchEntry;

#ifndef __linux__
	HWND *DlgList;
#else // __linux__
	QWidget **DlgList;
#endif // __linux__
	DWORD nList, nListBuf;

	Orbiter *pOrbiter;
	oapi::GraphicsClient *gc;
#ifndef __linux__
	HWND hWnd;

	static LRESULT FAR PASCAL OrbiterCtrl_Level_MsgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	// MessageHandler for slider custom control
#else // __linux__
	QWindow *hWnd;
#endif // __linux__

	// ====================================================================
	// Tread management for dialog thread
	// ====================================================================

public:
#ifndef __linux__
	void OpenDialogAsync (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, DWORD flag, void *context);
#else // __linux__
	void OpenDialogAsync (void *hInst, int id, QWindow *hParent, DLGINIT pDlg, DWORD flag, void *context);
#endif // __linux__

protected:
	void StartDialogThread ();
	// start the dialog handler thread (during render window creation)

	void DestroyDialogThread ();
	// kill the dialog handler thread (during render window destruction)

#ifndef __linux__
	void AddEntryAsync (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, DWORD flag, void *context);

private:
	HANDLE hThread;          // thread handle
	DWORD thid;              // dialog thread id
#else // __linux__
	void AddEntryAsync (void *hInst, int id, QWindow *hParent, DLGINIT pDlg, DWORD flag, void *context);
#endif // __linux__

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

#ifndef __linux__
INT_PTR OrbiterDefDialogProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
#else // __linux__
bool OrbiterDefDialogProc (QWidget *hDlg, QEvent *event);
#endif // __linux__

#endif // !__DLGMGR_H
