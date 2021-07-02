//-----------------------------------------------------------------------------
// Launchpad tab declaration: class JoystickTab
// Tab for joystick parameters
//-----------------------------------------------------------------------------

#ifndef __TABJOYSTICK_H
#define __TABJOYSTICK_H

#include "LpadTab.h"

class JoystickTab: public LaunchpadTab {
public:
	JoystickTab (const MainDialog *lp);

	void Create ();

	void GetConfig (const Config *cfg);
	void SetConfig (Config *cfg);

	bool OpenHelp ();

	BOOL TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

protected:
	void JoystickChanged (DWORD idx);
	// Respond to user joystick selection
};

#endif // !__TABJOYSTICK_H