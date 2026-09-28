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

#ifndef __linux__
		BOOL OnInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam);
#else // __linux__
		BOOL OnInitDialog(QWidget *hWnd);
#endif // __linux__
		BOOL OnSize(int w, int h);
#ifndef __linux__
		BOOL OnNotify(HWND hDlg, int idCtrl, LPNMHDR pnmh);
#endif // !__linux__
	};
}

#endif // !__TABOPTIONS_H
