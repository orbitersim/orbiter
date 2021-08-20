// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: class ExtraTab
// Tab for additional/advanced parameters
//-----------------------------------------------------------------------------

#ifndef __TABEXTRA_H
#define __TABEXTRA_H

#include "LpadTab.h"
#include "OrbiterAPI.h"
#include "CustomControls.h"

namespace orbiter {

	class ExtraTab : public LaunchpadTab {
		friend class LaunchpadDialog;

	public:
		ExtraTab(const LaunchpadDialog* lp);
		~ExtraTab();

		void Create();

		void GetConfig(const Config* cfg);
		void SetConfig(Config* cfg);

		bool OpenHelp();

		BOOL Size(int w, int h);

		INT_PTR TabProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	protected:
		HTREEITEM RegisterExtraParam(LaunchpadItem* item, HTREEITEM parent = 0);
		// Register an item in the "Extra" list. If parent=0, the item is registered
		// as a root (top level) item. Otherwise it appears as a sub-item under
		// the parent item.

		bool UnregisterExtraParam(LaunchpadItem* item);
		// Unregister an item in the "Extra" list.

		HTREEITEM FindExtraParam(const char* name, const HTREEITEM parent = 0);
		// Return item 'name' below parent 'parent', or NULL if not found

		HTREEITEM FindExtraParamChild(const HTREEITEM parent = 0);
		// Return first child of entry 'parent' (or first root item if root==0)
		// or 0 if parent has no children

		void WriteExtraParams();
		// allow all externally registered "Extra" items to write their data to file
		// (internal "extra" items use the Config class to write to Orbiter.cfg)

	private:
		LaunchpadItem** ExtPrm;  // list of parameter items on "Extra" list
		DWORD nExtPrm;

		SplitterCtrl splitListDesc;  // splitter control for extras list(left) and description(right)
		RECT r_pane, r_edit0;
		RECT r_lst0, r_dsc0; // REMOVE!
	};

}

// ****************************************************************************

//-----------------------------------------------------------------------------
// Additional built-in parameter settings (under the "Extra" tab)
//-----------------------------------------------------------------------------

class BuiltinLaunchpadItem: public LaunchpadItem {
public:
	BuiltinLaunchpadItem (const orbiter::ExtraTab *tab);
	bool OpenDialog (HWND hParent, int resid, DLGPROC pDlg);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

protected:
	void Error (const char *msg);
	const orbiter::ExtraTab *pTab;
};

//=============================================================================
// Physics engine: Parameters for dynamic state propagation
//=============================================================================

class ExtraPropagation: public BuiltinLaunchpadItem {
public:
	ExtraPropagation (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
};

//-----------------------------------------------------------------------------
// Physics engine: Parameters for linear state propagation
//-----------------------------------------------------------------------------

class ExtraDynamics: public BuiltinLaunchpadItem {
public:
	ExtraDynamics (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hParent);

private:
	void InitDialog (HWND hWnd);
	void ResetDialog (HWND hWnd);
	void SetDialog (HWND hWnd, const CFG_PHYSICSPRM &prm);
	void Activate (HWND hWnd, int which);
	bool StoreParams (HWND hWnd);
	bool OpenHelp (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static int PropId[NPROP_METHOD];
};

//-----------------------------------------------------------------------------
// Physics engine: Parameters for angular state propagation
//-----------------------------------------------------------------------------

#ifdef UNDEF
class ExtraAngDynamics: public BuiltinLaunchpadItem {
public:
	ExtraAngDynamics (const ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hParent);

private:
	void InitDialog (HWND hWnd);
	void ResetDialog (HWND hWnd);
	void SetDialog (HWND hWnd, const CFG_PHYSICSPRM &prm);
	void Activate (HWND hWnd, int which);
	bool StoreParams (HWND hWnd);
	bool OpenHelp (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static int PropId[NAPROP_METHOD];
};
#endif

//-----------------------------------------------------------------------------
// Physics engine: Parameters for orbit stabilisation
//-----------------------------------------------------------------------------

class ExtraStabilisation: public BuiltinLaunchpadItem {
public:
	ExtraStabilisation (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hParent);

private:
	void InitDialog (HWND hWnd);
	void ResetDialog (HWND hWnd);
	void SetDialog (HWND hWnd, const CFG_PHYSICSPRM &prm);
	bool StoreParams (HWND hWnd);
	void ToggleEnable (HWND hWnd);
	bool OpenHelp (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};


//=============================================================================
// Instruments and panels: configuration parameters for instrumentation
//=============================================================================

class ExtraInstruments: public BuiltinLaunchpadItem {
public:
	ExtraInstruments (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
};

//-----------------------------------------------------------------------------
// Instruments and panels: MFDs
//-----------------------------------------------------------------------------

class ExtraMfdConfig: public BuiltinLaunchpadItem {
public:
	ExtraMfdConfig (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hParent);

private:
	void InitDialog (HWND hWnd);
	void ResetDialog (HWND hWnd);
	void SetDialog (HWND hWnd, const CFG_INSTRUMENTPRM &prm);
	bool StoreParams (HWND hWnd);
	void ToggleEnable (HWND hWnd);
	bool OpenHelp (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};


//=============================================================================
// Root item for vessel configurations (sub-items to be added by modules)
//=============================================================================

class ExtraVesselConfig: public BuiltinLaunchpadItem {
public:
	ExtraVesselConfig (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
};

//=============================================================================
// Root item for planet configurations (sub-items to be added by modules)
//=============================================================================

class ExtraPlanetConfig: public BuiltinLaunchpadItem {
public:
	ExtraPlanetConfig (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
};

//=============================================================================
// Debugging parameters
//=============================================================================

class ExtraDebug: public BuiltinLaunchpadItem {
public:
	ExtraDebug (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
};

//-----------------------------------------------------------------------------
// Debugging parameters: shutdown options
//-----------------------------------------------------------------------------

class ExtraShutdown: public BuiltinLaunchpadItem {
public:
	ExtraShutdown (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hParent);

private:
	void InitDialog (HWND hWnd);
	void ResetDialog (HWND hWnd);
	void SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm);
	bool StoreParams (HWND hWnd);
	bool OpenHelp (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

//-----------------------------------------------------------------------------
// Debugging parameters: fixed time steps
//-----------------------------------------------------------------------------

class ExtraFixedStep: public BuiltinLaunchpadItem {
public:
	ExtraFixedStep (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hParent);

private:
	void InitDialog (HWND hWnd);
	void ResetDialog (HWND hWnd);
	void SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm);
	bool StoreParams (HWND hWnd);
	void ToggleEnable (HWND hWnd);
	bool OpenHelp (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

//-----------------------------------------------------------------------------
// Debugging parameters: rendering options
//-----------------------------------------------------------------------------

class ExtraRenderingOptions: public BuiltinLaunchpadItem {
public:
	ExtraRenderingOptions (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hParent);

private:
	void InitDialog (HWND hWnd);
	void ResetDialog (HWND hWnd);
	void SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm);
	bool StoreParams (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

//-----------------------------------------------------------------------------
// Debugging parameters: timer settings
//-----------------------------------------------------------------------------

class ExtraTimerSettings: public BuiltinLaunchpadItem {
public:
	ExtraTimerSettings (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hParent);

private:
	void InitDialog (HWND hWnd);
	void ResetDialog (HWND hWnd);
	void SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm);
	bool StoreParams (HWND hWnd);
	bool OpenHelp (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

//-----------------------------------------------------------------------------
// Debugging parameters: performance options
//-----------------------------------------------------------------------------

class ExtraPerformanceSettings: public BuiltinLaunchpadItem {
public:
	ExtraPerformanceSettings (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hParent);

private:
	void InitDialog (HWND hWnd);
	void ResetDialog (HWND hWnd);
	void SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm);
	bool StoreParams (HWND hWnd);
	bool OpenHelp (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

//-----------------------------------------------------------------------------
// Debugging parameters: launchpad options
//-----------------------------------------------------------------------------

class ExtraLaunchpadOptions: public BuiltinLaunchpadItem {
public:
	ExtraLaunchpadOptions (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hParent);

private:
	void InitDialog (HWND hWnd);
	void ResetDialog (HWND hWnd);
	void SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm);
	bool StoreParams (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

//-----------------------------------------------------------------------------
// Debugging parameters: log file options
//-----------------------------------------------------------------------------

class ExtraLogfileOptions: public BuiltinLaunchpadItem {
public:
	ExtraLogfileOptions (const orbiter::ExtraTab *tab): BuiltinLaunchpadItem (tab) {}
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hParent);

private:
	void InitDialog (HWND hWnd);
	void ResetDialog (HWND hWnd);
	void SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm);
	bool StoreParams (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

#endif // !__TABEXTRA_H