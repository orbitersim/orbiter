// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __DLGCTRL_H
#define __DLGCTRL_H

#define STRICT 1
#include "windows.h"

void oapiRegisterCustomControls (HINSTANCE hInst);
void oapiUnregisterCustomControls (HINSTANCE hInst);

LRESULT FAR PASCAL MsgProc_Gauge (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
LRESULT FAR PASCAL MsgProc_Switch (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

struct GAUGEPARAM {
	int rangemin, rangemax;
	enum GAUGEBASE { LEFT, RIGHT, TOP, BOTTOM } base;
	enum GAUGECOLOR { BLACK, RED } color;
};

void oapiSetGaugeParams (HWND hCtrl, GAUGEPARAM *gp, bool redraw = true);
void oapiSetGaugeRange (HWND hCtrl, int rmin, int rmax, bool redraw = true);
int  oapiSetGaugePos (HWND hCtrl, int pos, bool redraw = true);
int  oapiIncGaugePos (HWND hCtrl, int dpos, bool redraw = true);
int  oapiGetGaugePos (HWND hCtrl);

struct SWITCHPARAM {
	enum SWITCHMODE { TWOSTATE, THREESTATE } mode;
	enum ORIENTATION { HORIZONTAL, VERTICAL } align;
};

void oapiSetSwitchParams (HWND hCtrl, SWITCHPARAM *sp, bool redraw);
int oapiSetSwitchState (HWND hCtrl, int state, bool redraw);
int oapiGetSwitchState (HWND hCtrl);

// ==================================================================================
// ==================================================================================

class PropertyItem {
	friend class PropertyGroup;
	friend class PropertyList;
public:
	PropertyItem (PropertyGroup *grp);
	~PropertyItem ();

	void SetLabel (const char *newlabel);
	const char *GetLabel () const { return label; }
	void SetValue (const char *newvalue);
	const char *GetValue () const { return value; }

private:
	PropertyGroup *group;
	char *label;       // label string
	char *value;       // value string
	int labelw;        // width of label string
	int valuew;        // width of value string
	bool label_dirty;  // label string awaiting redraw
	bool value_dirty;  // value string awaiting redraw
};

// ==================================================================================

class PropertyGroup {
	friend class PropertyList;
public:
	PropertyGroup (PropertyList *list, bool expand = true);
	~PropertyGroup ();
	PropertyItem *GetItem (int idx);
	int ItemCount () const { return nitem; }
	void SetTitle (const char *t);
	const char *GetTitle () const { return title; }
	void Expand (bool expand);
	bool IsExpanded () const { return expanded; }

protected:
	PropertyItem *AppendItem ();

private:
	PropertyList *plist;
	PropertyItem **item;
	int nitem;
	char *title;
	bool expanded;
};

// ==================================================================================

class PropertyList {
public:
	PropertyList ();
	~PropertyList ();
	void OnInitDialog (HWND hWnd, int nIDDlgItem);
	void OnPaint (HWND hWnd);
	void OnSize (int w, int h);
	void OnVScroll (unsigned int cmd, int p);
	void OnLButtonDown (int x, int y);
	void Move (int x, int y, int w, int h);
	void Redraw ();
	void Update ();
	void SetColWidth (int col, int w);

	PropertyGroup *AppendGroup (bool expand = true);
	bool DeleteGroup (PropertyGroup *g);
	PropertyGroup *GetGroup (int idx);
	int GroupCount () const { return npg; }
	bool ExpandGroup (PropertyGroup *g, bool expand);
	void ExpandAll (bool expand);
	void ClearGroups ();

	PropertyItem *AppendItem (PropertyGroup *g);

	static HBITMAP hBmpArrows;

protected:
	void SetListHeight (int h, bool force = false);
	void VScrollTo (int pos);

private:
	HWND hDlg;          // window handle for dialog box
	HWND hItem;         // window handle for list control
	HFONT hFontTitle;   // group title font
	HFONT hFontItem;    // item font
	HPEN hPenLine;      // pen for cell borders
	HBRUSH hBrushTitle; // brush for title backgrounds
	int dlgid;          // dialog id for list control
	int winw, winh;     // width and height of list window
	int listh;          // logical height of list
	int valx0;          // width of first column
	int yofs;           // scroll position [pixel]

	PropertyGroup **pg;
	int npg;

	static int titleh;
	static int itemh;
	static int gaph;
};

#endif // !__DLGCTRL_H
