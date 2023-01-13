// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: class DefVideoTab
// Tab for default video device parameters
//-----------------------------------------------------------------------------

#ifndef __TABVIDEO_H
#define __TABVIDEO_H

#include "LpadTab.h"

namespace orbiter {

	class DefVideoTab : public LaunchpadTab {
	public:
		DefVideoTab(const LaunchpadDialog* lp);
		~DefVideoTab();

		void Create();

		BOOL OnInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam);

		void OnGraphicsClientLoaded(oapi::GraphicsClient* gc, const PSTR moduleName);

		void SetConfig(Config* cfg);

		bool OpenHelp();

		BOOL OnMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	protected:
		void ShowInterface(HWND hTab, bool show);

		void EnumerateClients(HWND hTab);

		void ScanDir(HWND hTab, const PSTR dir);
		// scan directory dir (relative to Orbiter root) for graphics clients
		// and enter them in the combo box

		void SelectClientIndex(UINT idx);

		void SetInfoString(PSTR str);

		static INT_PTR CALLBACK InfoProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	private:
		UINT idxClient;
		char* strInfo;
	};

}

#endif // !__TABVIDEO_H