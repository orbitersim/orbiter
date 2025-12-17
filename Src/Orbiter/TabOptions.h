// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: OptionsTab
// Simulation options
//-----------------------------------------------------------------------------

#ifndef __TABOPTIONS_H
#define __TABOPTIONS_H

#include "LpadTab.h"
#include "OptionsPages.h"

namespace orbiter {

	class OptionsTab : public orbiter::LaunchpadTab, public OptionsPageContainer {
	public:
		OptionsTab(const orbiter::LaunchpadDialog* lp);
		void Create();

		bool DynamicSize() const { return true; }

		void LaunchpadShowing(bool show);

		void SetConfig(Config* cfg);

		bool OpenHelp();

		BOOL OnInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam);
		BOOL OnSize(int w, int h);
		BOOL OnNotify(HWND hDlg, int idCtrl, LPNMHDR pnmh);
	};
}

#endif // !__TABOPTIONS_H
