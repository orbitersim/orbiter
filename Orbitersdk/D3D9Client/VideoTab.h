// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 Martin Schweiger
//				 2012 Jarmo Nikkanen
// ==============================================================

#ifndef __VIDEOTAB_H
#define __VIDEOTAB_H

// ==============================================================

class VideoTab {
public:
	VideoTab(oapi::D3D9Client *gc, HINSTANCE _hInst, HINSTANCE _hOrbiterInst, HWND hVideoTab);
	~VideoTab();

	BOOL WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	// Video tab message handler

	void UpdateConfigData();
	// copy dialog state back to parameter structure

protected:

	void Initialise();
	// Initialise dialog elements

	void SelectFullscreen(bool);
	void SelectMode(DWORD index);
	void SelectAdapter(DWORD index);
	// Update dialog after user device selection

	void SelectWidth();
	// Update dialog after window width selection

	void SelectHeight();
	// Update dialog after window height selection

private:
	static BOOL CALLBACK SetupDlgProcWrp(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK CreditsDlgProcWrp(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	BOOL CALLBACK SetupDlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	BOOL CALLBACK CreditsDlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	void InitCreditsDialog(HWND hWnd);
	void CreateSymbolicLinks();
	void InitSetupDialog(HWND hWnd);
	void SaveSetupState(HWND hWnd);
	
	oapi::D3D9Client *gclient;
	HINSTANCE hOrbiterInst; // orbiter instance handle
	HINSTANCE hInst;        // module instance handle
	HWND hTab;              // window handle of the video tab
	int aspect_idx;
	DWORD SelectedAdapterIdx;
	bool bHasMultiSample;
};

//};

#endif // !__VIDEOTAB_H