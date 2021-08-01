// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// D3D7Extra.h
// Management of the configuration dialogs under the "Extra"
// Launchpad tab
// ==============================================================

#ifndef __D3D7EXTRA_H
#define __D3D7EXTRA_H

#include "orbitersdk.h"
#include "D3D7Client.h"

class D3D7Config;

class D3D7ClientCfg: public LaunchpadItem {
public:
	D3D7ClientCfg (): LaunchpadItem () {}
	char *Name ();
	char *Description ();
};

class D3D7PlanetRenderCfg: public LaunchpadItem {
public:
	D3D7PlanetRenderCfg (oapi::D3D7Client *_gc, D3D7Config *_cfg);
	char *Name ();
	char *Description ();
	void InitDialog (HWND hDlg);
	void Update (HWND hDlg);
	void Apply (HWND hDlg);
	void CloseDialog (HWND hDlg);
	bool clbkOpen (HWND hLaunchpad);

private:
	oapi::D3D7Client *gc;
	D3D7Config *cfg;
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
};

#endif // !__D3D7EXTRA_H