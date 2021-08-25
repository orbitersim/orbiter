// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab definition: class AboutTab
// Tab for "about" page
//-----------------------------------------------------------------------------

#ifndef __TABABOUT_H
#define __TABABOUT_H

#include "LpadTab.h"

namespace orbiter {

	class AboutTab : public LaunchpadTab {
	public:
		AboutTab(const LaunchpadDialog* lp);

		void Create();
		bool OpenHelp();

		INT_PTR TabProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	private:
		static INT_PTR CALLBACK AboutProc(HWND, UINT, WPARAM, LPARAM);
	};

}

#endif // !__TABABOUT_H
