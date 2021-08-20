// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad Extra item:
// Render options
//-----------------------------------------------------------------------------

#ifndef __EXTRARENDER_H
#define __EXTRARENDER_H

#include "TabExtra.h"

//=============================================================================

class Extra_RenderOptions: public BuiltinLaunchpadItem {
public:
	Extra_RenderOptions (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
};

//=============================================================================

class Extra_PlanetRenderOptions: public BuiltinLaunchpadItem {
public:
	Extra_PlanetRenderOptions (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hParent);

private:
	void InitDialog (HWND hWnd);
	void ResetDialog (HWND hWnd);
	void SetDialog (HWND hWnd, const CFG_PLANETRENDERPRM &prm);
	bool StoreParams (HWND hWnd);
	void Update (HWND hWnd);
	bool OpenHelp (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

#endif // !__EXTRARENDER_H