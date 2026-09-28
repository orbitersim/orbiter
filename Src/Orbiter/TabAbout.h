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

#ifndef __linux__
		BOOL OnMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
#else // __linux__
		BOOL OnInitDialog(QWidget *hWnd);
#endif // __linux__

	private:
#ifndef __linux__
		static INT_PTR CALLBACK AboutProc(HWND, UINT, WPARAM, LPARAM);
#else // __linux__
		static void AboutProc(QWidget *hWnd, int textId);
#endif // __linux__
	};

}

#endif // !__TABABOUT_H
