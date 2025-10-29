// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
// ==============================================================

#ifndef __VIDEOTAB_H
#define __VIDEOTAB_H
#include <vector>
#include <map>

// ==============================================================

class VideoTab {

	struct _AtmoCfg { string cfg, file; };
public:
	VideoTab(oapi::vkClient *gc, HINSTANCE _hInst, HINSTANCE _hOrbiterInst, HWND hVideoTab);
	~VideoTab();

	BOOL WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	// Video tab message handler

	void UpdateConfigData();
	// copy dialog state back to parameter structure

	bool Initialise();
	// Initialise dialog elements

protected:
	void SelectFullscreen(bool);
	void SelectMode(DWORD index);
	bool SelectAdapter(DWORD index);
	// Update dialog after user device selection

	void SelectWidth();
	// Update dialog after window width selection

	void SelectHeight();
	// Update dialog after window height selection

private:
	static INT_PTR CALLBACK SetupDlgProcWrp(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK CreditsDlgProcWrp(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	INT_PTR CALLBACK SetupDlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	INT_PTR CALLBACK CreditsDlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	void InitCreditsDialog(HWND hWnd);
	void CreateSymbolicLinks();
	void InitSetupDialog(HWND hWnd);
	void SaveSetupState(HWND hWnd);
	void ScanAtmoCfgs();
	bool GetConfigName(const char* file, string& cfg, string& planet);
	
	oapi::vkClient *gclient;
	HINSTANCE hOrbiterInst; // orbiter instance handle
	HINSTANCE hInst;        // module instance handle
	HWND hTab;              // window handle of the video tab
	int aspect_idx;
	DWORD SelectedAdapterIdx;
	bool bHasMultiSample;
	std::map<string, std::vector<_AtmoCfg>> AtmoCfgs;
};

//};

#endif // !__VIDEOTAB_H
