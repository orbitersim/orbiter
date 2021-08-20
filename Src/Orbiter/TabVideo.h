// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: class DefVideoTab
// Tab for default video device parameters
//-----------------------------------------------------------------------------

#ifndef __TABVIDEO_H
#define __TABVIDEO_H

#include "LpadTab.h"

class DefVideoTab: public LaunchpadTab {
public:
	DefVideoTab (const orbiter::LaunchpadDialog *lp);

	void Create ();

	void SetConfig (Config *cfg);

	bool OpenHelp ();

	INT_PTR TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

#endif // !__TABVIDEO_H