// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2026 Martin Schweiger
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
	VideoTab(oapi::D3D9Client *gc, void *_hInst, void *_hOrbiterInst, QWidget *hVideoTab);
	~VideoTab();

	void WndProc (QWidget *hWnd);
	// Video tab message handler: connects the video controls (called once, from LaunchpadVideoWndProc)

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
	static void SetupDlgProcWrp(QWidget *hWnd, void *context);   // DLGINIT, context = lParam (the VideoTab)
	static void CreditsDlgProcWrp(QWidget *hWnd, void *context); // DLGINIT, context = lParam (the VideoTab)
	void SetupDlgProc(QWidget *hWnd);   // connects the WM_COMMAND and WM_HSCROLL handlers
	void CreditsDlgProc(QWidget *hWnd); // connects the WM_COMMAND handler
	void InitCreditsDialog(QWidget *hWnd);
	void CreateSymbolicLinks();
	void InitSetupDialog(QWidget *hWnd);
	void SaveSetupState(QWidget *hWnd);
	void ScanAtmoCfgs();
	bool GetConfigName(const char* file, string& cfg, string& planet);
	
	oapi::D3D9Client *gclient;
	void *hOrbiterInst;     // orbiter instance handle
	void *hInst;            // module instance handle
	QWidget *hTab;          // window handle of the video tab
	int aspect_idx;
	DWORD SelectedAdapterIdx;
	bool bHasMultiSample;
	std::map<string, std::vector<_AtmoCfg>> AtmoCfgs;
};

//};

#endif // !__VIDEOTAB_H

