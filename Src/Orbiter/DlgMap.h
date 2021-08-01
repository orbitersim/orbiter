// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Map window
// ======================================================================

#ifndef __DLGMAP_H
#define __DLGMAP_H

#include "DialogWin.h"
#include "VectorMap.h"

// ======================================================================
// Class for map window inside the dialog box

class MapWin: public VectorMap {
	friend class DlgMap;

public:
	MapWin (DlgMap *pDlg);
	~MapWin ();

	int Create ();
	int Destroy ();
	void PostCreation (HWND hw);
	void Update (bool force = false);
	bool SetCBody (const CelestialBody *body);
	int Paint ();
	int Size (DWORD w, DWORD h);
	void Pan (int dx, int dy);

protected:
	enum MouseMode {
		MOUSE_DRAG, MOUSE_SELECT
	};

	void RegisterWindow (HINSTANCE hInst);
	void UnregisterWindow (HINSTANCE hInst);

	void OnLButtonDown (int mx, int my);
	void OnLButtonUp (int mx, int my);
	void OnMouseMove (int mx, int my);
	void OnMouseWheel (int wdelta);
	void SetMouseMode (MouseMode mode);
	bool FindTarget (int mx, int my);
	static MapWin *GetMapInstance (HWND hw);
	static MapWin *map_in_creation;
	static LRESULT FAR PASCAL Map_WndProc (HWND, UINT, WPARAM, LPARAM);

private:
	DlgMap *dlg;
	HWND hWnd;
	HFONT font;
	HPEN pen[3];
	bool bPaintPending;
	bool bForceUpdate;
	bool bDragActive;
	int mousex, mousey;
	MouseMode mousemode;
	DWORD storeflag;
};

// ======================================================================
// Map dialog

class DlgMap: public DialogWin {
	friend class MapWin;
	friend class DlgMapOpt;

public:
	DlgMap (HINSTANCE hInstance, HWND hParent, void *context);
	~DlgMap ();
	static void GlobalInit ();
	void SetSelection (const Body *body);
	HWND OpenWindow ();
	void Update ();
	void VesselDeleting (Vessel *v);
	void SetCBody (CelestialBody *cbody);
	void SetPlanet (const char *name);
	void EchoSelection (const VectorMap::OBJTYPE &obj);
	void SetSelection (const VectorMap::OBJTYPE &obj);
	bool SetSelection (const char *name, int type = 0);
	void ToggleDispFlags (DWORD dflag);
	void SetDispFlags (DWORD flag);
	void SetDragMode (bool drag);
	void EnableInfo (bool enable);
	void OpenInfo ();
	MapWin *GetMapWin() { return map; }

	BOOL OnInitDialog (HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand (HWND hWnd, WORD id, WORD code, HWND hControl);
	BOOL OnNotify (HWND hWnd, int idCtrl, LPNMHDR pnmh);
	BOOL OnUserMessage (HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL OnMouseWheel (HWND hWnd, int vk, int dist, int x, int y);

	struct MapPrmPersist {
		const CelestialBody *cbody;
		VectorMap::OBJTYPE sel;
		int zoom;
		double lngcnt, latcnt;
		bool track;
	};

	struct MAP_PARAM {
		int dlgw, dlgh, mapw, maph;
		POINT btlp[1], btrp[4];
	};

protected:
	BOOL OnSize (HWND hDlg, WPARAM wParam, int w, int h);
	void SetZoom (int zoom);
	void ZoomIn ();
	void ZoomOut ();
	void SetFindMask ();

private:
	MapWin *map;
	MAP_PARAM MapPrm;
	CFG_MAPPRM *prm;
	static MapPrmPersist prm_store;
};

// ======================================================================
// Map options dialog

class DlgMapOpt: public DialogWin {
public:
	DlgMapOpt (HINSTANCE hInstance, HWND hParent, void *context);
	DlgMap *GetMapDlg() { return dlgmap; }
	void ToggleDispFlag (DWORD flag);
	void SetOrbitMode (DWORD flag);
	void SetVesselMode (DWORD flag);

	BOOL OnInitDialog (HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand (HWND hWnd, WORD id, WORD code, HWND hControl);

private:
	DlgMap *dlgmap;
};

#endif // !__DLGMAP_H