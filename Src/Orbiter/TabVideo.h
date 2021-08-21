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

		void Create();

		BOOL InitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam);

		void SetConfig(Config* cfg);

		bool OpenHelp();

		INT_PTR TabProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	protected:
		void EnumerateClients();

		void ScanDir(const PSTR dir);
		// scan directory dir (relative to Orbiter root) for graphics clients
		// and enter them in the combo box

		void SelectClientIndex(UINT idx);

	private:
		UINT idxClient;
	};

}

#endif // !__TABVIDEO_H