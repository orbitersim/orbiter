// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// VideoTab.h
// Class VideoTab (interface)
// Manages the user selections in the "Video" tab of the Orbiter
// Launchpad dialog.
// ==============================================================

#ifndef __VIDEOTAB_H
#define __VIDEOTAB_H

#include "D3D7Enum.h"

namespace oapi { class D3D7Client; }

// ==============================================================

class VideoTab {
public:
	VideoTab (oapi::D3D7Client *gc, HINSTANCE _hInst, HINSTANCE _hOrbiterInst, HWND hVideoTab);

	BOOL WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	// Video tab message handler

	void UpdateConfigData ();
	// copy dialog state back to parameter structure

protected:
	void Initialise (D3D7Enum_DeviceInfo *dev);
	// Initialise dialog elements

	void SelectDevice (D3D7Enum_DeviceInfo *dev);
	// Update dialog after user device selection

	void SelectDispmode (D3D7Enum_DeviceInfo *dev, BOOL bWindow);
	// Update dialog after user fullscreen/window selection

	void SelectMode (D3D7Enum_DeviceInfo *dev, DWORD idx);
	// Update dialog after fullscreen mode selection

	void SelectBPP (D3D7Enum_DeviceInfo *dev, DWORD idx);
	// Update dialog after fullscreen colour depth selection

	void SelectPageflip ();
	// Flip hardware pageflip on/off

	void SelectWidth ();
	// Update dialog after window width selection

	void SelectHeight ();
	// Update dialog after window height selection

	void SelectFixedAspect ();
	// Flip fixed window aspect ratio on/off

private:
	static INT_PTR CALLBACK AboutDlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	oapi::D3D7Client *gclient;
	HINSTANCE hOrbiterInst; // orbiter instance handle
	HINSTANCE hInst;        // module instance handle
	HWND hTab;              // window handle of the video tab
	int aspect_idx;         // fixed aspect ratio index
};

#endif // !__VIDEOTAB_H