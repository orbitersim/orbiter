// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: VisualTab
// Tab for visual parameters
//-----------------------------------------------------------------------------

#ifndef __TABVISUAL_H
#define __TABVISUAL_H

#include "LpadTab.h"

namespace orbiter {

	class VisualTab : public LaunchpadTab {
	public:
		VisualTab(const LaunchpadDialog* lp);
		~VisualTab();

		void Create();

		void GetConfig(const Config* cfg);
		void SetConfig(Config* cfg);

		bool OpenHelp();

		INT_PTR TabProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	protected:
		void VisualsChanged();
		// Something on the visual params tab has changed which requires a reset

		void EmptyCSphereList();
		void AddCSphereList(const char* c);

	private:
		char** csphere_img_path;
		int ncsphere_img;
	};

}

#endif // !__TABVISUAL_H