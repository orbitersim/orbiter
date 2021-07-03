//-----------------------------------------------------------------------------
// Launchpad tab declaration: class DefVideoTab
// Tab for default video device parameters
//-----------------------------------------------------------------------------

#ifndef __TABVIDEO_H
#define __TABVIDEO_H

#include "LpadTab.h"

class DefVideoTab: public LaunchpadTab {
public:
	DefVideoTab (const MainDialog *lp);

	void Create ();

	void SetConfig (Config *cfg);

	bool OpenHelp ();

	BOOL TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

#endif // !__TABVIDEO_H