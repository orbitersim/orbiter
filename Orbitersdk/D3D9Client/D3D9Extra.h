// Management of the configuration dialogs under the "Extra"
// Launchpad tab

#ifndef __D3D9EXTRA_H
#define __D3D9EXTRA_H

#include "orbitersdk.h"

class D3D9Config;

class D3D9ClientCfg: public LaunchpadItem {
public:
	D3D9ClientCfg (): LaunchpadItem () {}
	char *Name ();
	char *Description ();
};

class D3D9PlanetRenderCfg: public LaunchpadItem {
public:
	D3D9PlanetRenderCfg (): LaunchpadItem () {}
	char *Name ();
	char *Description ();
	void InitDialog (HWND hDlg);
	void Update (HWND hDlg);
	void Apply (HWND hDlg);
	void CloseDialog (HWND hDlg);
	bool clbkOpen (HWND hLaunchpad);

private:
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
};

#endif // !__D3D9EXTRA_H