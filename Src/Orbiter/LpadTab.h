// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __LPADTAB_H
#define __LPADTAB_H

#include <windows.h>
#include "Config.h"
#include "Launchpad.h"

// Property page indices
#define PG_SCN  0
#define PG_OPT  1
#define PG_VIS  2
#define PG_MOD  3
#define PG_VID  4
#define PG_JOY  5
#define PG_EXT  6
#define PG_ABT  7
#define PG_WAIT 8
#define PG_NET  9
#define PG_NET2 10
#define PG_NET3 11

namespace orbiter {

	class LaunchpadDialog;

	//-----------------------------------------------------------------------------
	// Name: class LaunchpadTab
	// Desc: Base class for main dialog tabs
	//-----------------------------------------------------------------------------
	class LaunchpadTab {
	public:
		LaunchpadTab(const LaunchpadDialog* lp);
		virtual ~LaunchpadTab();
		virtual void Create() {}

		inline const LaunchpadDialog* Launchpad() const { return pLp; }
		inline Config* Cfg() const { return pCfg; }

		virtual void GetConfig(const Config* cfg) {}
		// Read config parameters

		virtual void SetConfig(Config* cfg) {}
		// Write config parameters back

		virtual BOOL InitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam) { return FALSE; }

		virtual bool OpenHelp() { return false; }

		void OpenTabHelp(const char* topic);

		virtual BOOL Size(int w, int h);
		// by default, this re-centers the items if RegisterItemPositions has been called

		virtual void Show();
		virtual void Hide();
		virtual void LaunchpadShowing(bool show) {}
		inline bool IsActive() const { return bActive; }
		inline HWND TabWnd() const { return hTab; }
		inline HWND LaunchpadWnd() const { return pLp->hDlg; }
		inline HINSTANCE AppInstance() const { return pLp->hInst; }

		virtual INT_PTR TabProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
		// generic message handler

	protected:
		HWND CreateTab(int resid);

		void RegisterItemPositions(int* _item, int _nitem);
		// Keep a record of the positions of dialog items
		// (for auto-recentering)

		static INT_PTR CALLBACK TabProcHook(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

		const LaunchpadDialog* pLp;
		Config* pCfg;
		HWND hTab;
		RECT pos0;  // initial position in Launchpad dialog
		bool bActive;

		int* item;
		POINT* itempos;  // registered dialog item postions
		int nitem;    // list length
	};

}

#endif // !__LPADTAB_H