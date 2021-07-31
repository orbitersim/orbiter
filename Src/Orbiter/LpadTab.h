// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __LPADTAB_H
#define __LPADTAB_H

#include <windows.h>
#include "Config.h"

// Property page indices
#define PG_SCN  0
#define PG_OPT  1
#define PG_VIS  2
#define PG_MOD  3
#define PG_VID  4
#define PG_JOY  5
#define PG_EXT  6
#define PG_ABT  7
#define PG_WAIT 8
#define PG_NET  9
#define PG_NET2 10
#define PG_NET3 11

//-----------------------------------------------------------------------------
// Forward declarations
//-----------------------------------------------------------------------------
class MainDialog;

//-----------------------------------------------------------------------------
// Name: class LaunchpadTab
// Desc: Base class for main dialog tabs
//-----------------------------------------------------------------------------
class LaunchpadTab {
public:
	LaunchpadTab (const MainDialog *lp);
	virtual ~LaunchpadTab ();
	virtual void Create () {}

	inline const MainDialog *Launchpad () const { return pLp; }
	inline Config *Cfg () const { return pCfg; }

	virtual void GetConfig (const Config *cfg) {}
	// Read config parameters

	virtual void SetConfig (Config *cfg) {}
	// Write config parameters back

	virtual BOOL InitDialog (HWND hWnd, WPARAM wParam, LPARAM lParam) { return FALSE; }

	virtual bool OpenHelp () { return false; }

	virtual BOOL Size (int w, int h);
	// by default, this re-centers the items if RegisterItemPositions has been called

	virtual void Show ();
	virtual void Hide ();
	inline bool IsActive () const { return bActive; }
	inline HWND TabWnd () const { return hTab; }

	virtual BOOL TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	// generic message handler

protected:
	HWND CreateTab (int resid);

	void RegisterItemPositions (int *_item, int _nitem);
	// Keep a record of the positions of dialog items
	// (for auto-recentering)

	static INT_PTR CALLBACK TabProcHook (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	const MainDialog *pLp;
	Config *pCfg;
	HWND hTab;
	RECT pos0;  // initial position in Launchpad dialog
	bool bActive;

	int *item;
	POINT *itempos;  // registered dialog item postions
	int nitem;    // list length
};

#endif // !__LPADTAB_H