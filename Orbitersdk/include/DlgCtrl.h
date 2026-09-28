// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __DLGCTRL_H
#define __DLGCTRL_H

#ifndef __linux__
#define STRICT 1
#include "windows.h"

void oapiRegisterCustomControls (HINSTANCE hInst);
void oapiUnregisterCustomControls (HINSTANCE hInst);
#else // __linux__
#include "OrbiterPlatform.h"
#include <QAbstractScrollArea>
#include <QWidget>

class QTimer;
struct GAUGEPARAM;
struct SWITCHPARAM;

// registers the control classes (OrbiterCtrl_Gauge, OrbiterCtrl_Switch, OrbiterCtrl_PropertyList) for dialogs built from resources
void oapiRegisterCustomControls (void *hInst);
void oapiUnregisterCustomControls (void *hInst);

// gauge notification request (WM_HSCROLL SB_LINELEFT/SB_LINERIGHT/SB_THUMBTRACK counterparts)
enum GAUGEREQUEST { GAUGE_LINEDEC, GAUGE_LINEINC, GAUGE_THUMBTRACK };

// OrbiterCtrl_Gauge: level indicator with inc/dec buttons at both ends and a draggable bar
class GaugeCtrl: public QWidget {
	Q_OBJECT
	friend void oapiSetGaugeParams (QWidget *hCtrl, GAUGEPARAM *gp, bool redraw);
	friend void oapiSetGaugeRange (QWidget *hCtrl, int rmin, int rmax, bool redraw);
	friend int  oapiSetGaugePos (QWidget *hCtrl, int pos, bool redraw);
	friend int  oapiIncGaugePos (QWidget *hCtrl, int dpos, bool redraw);
	friend int  oapiGetGaugePos (QWidget *hCtrl);
public:
	GaugeCtrl (QWidget *parent);
	int State () const { return (flag >> 4) & 3; } // BM_GETSTATE: 1 = decrease, 2 = increase button pressed
signals:
	void scrolled (int request, int pos); // GAUGEREQUEST
protected:
	void paintEvent (QPaintEvent *event) override;
	void mousePressEvent (QMouseEvent *event) override;
	void mouseReleaseEvent (QMouseEvent *event) override;
	void mouseMoveEvent (QMouseEvent *event) override;
	void changeEvent (QEvent *event) override;
private:
	void DragSlider (int x, int y);
	void OnTimer ();
	int pos = 0, rmin = 0, rmax = 0;
	DWORD flag = 0;
	QTimer *timer;
};
#endif // __linux__

#ifndef __linux__
LRESULT FAR PASCAL MsgProc_Gauge (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
LRESULT FAR PASCAL MsgProc_Switch (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
#else // __linux__
// OrbiterCtrl_Switch: two- or three-state lever switch
class SwitchCtrl: public QWidget {
	Q_OBJECT
	friend void oapiSetSwitchParams (QWidget *hCtrl, SWITCHPARAM *sp, bool redraw);
	friend int  oapiSetSwitchState (QWidget *hCtrl, int state, bool redraw);
	friend int  oapiGetSwitchState (QWidget *hCtrl);
public:
	SwitchCtrl (QWidget *parent);
signals:
	void clicked (int state); // BN_CLICKED with the new state
protected:
	void paintEvent (QPaintEvent *event) override;
	void mousePressEvent (QMouseEvent *event) override;
private:
	int pos = 0;
	DWORD flag = 0;
};
#endif // __linux__

struct GAUGEPARAM {
	int rangemin, rangemax;
	enum GAUGEBASE { LEFT, RIGHT, TOP, BOTTOM } base;
	enum GAUGECOLOR { BLACK, RED } color;
};

#ifndef __linux__
void oapiSetGaugeParams (HWND hCtrl, GAUGEPARAM *gp, bool redraw = true);
void oapiSetGaugeRange (HWND hCtrl, int rmin, int rmax, bool redraw = true);
int  oapiSetGaugePos (HWND hCtrl, int pos, bool redraw = true);
int  oapiIncGaugePos (HWND hCtrl, int dpos, bool redraw = true);
int  oapiGetGaugePos (HWND hCtrl);
#else // __linux__
void oapiSetGaugeParams (QWidget *hCtrl, GAUGEPARAM *gp, bool redraw = true);
void oapiSetGaugeRange (QWidget *hCtrl, int rmin, int rmax, bool redraw = true);
int  oapiSetGaugePos (QWidget *hCtrl, int pos, bool redraw = true);
int  oapiIncGaugePos (QWidget *hCtrl, int dpos, bool redraw = true);
int  oapiGetGaugePos (QWidget *hCtrl);
#endif // __linux__

struct SWITCHPARAM {
	enum SWITCHMODE { TWOSTATE, THREESTATE } mode;
	enum ORIENTATION { HORIZONTAL, VERTICAL } align;
};

#ifndef __linux__
void oapiSetSwitchParams (HWND hCtrl, SWITCHPARAM *sp, bool redraw);
int oapiSetSwitchState (HWND hCtrl, int state, bool redraw);
int oapiGetSwitchState (HWND hCtrl);
#else // __linux__
void oapiSetSwitchParams (QWidget *hCtrl, SWITCHPARAM *sp, bool redraw);
int oapiSetSwitchState (QWidget *hCtrl, int state, bool redraw);
int oapiGetSwitchState (QWidget *hCtrl);
#endif // __linux__

// ==================================================================================
// ==================================================================================

#ifdef __linux__
class PropertyItem;
class PropertyGroup;
class PropertyList;

#endif // __linux__
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

#ifdef __linux__
// OrbiterCtrl_PropertyList: the scrolling window a PropertyList draws into
class PropertyListCtrl: public QAbstractScrollArea {
	Q_OBJECT
	friend class PropertyList;
public:
	PropertyListCtrl (QWidget *parent);
protected:
	void paintEvent (QPaintEvent *event) override;
	void resizeEvent (QResizeEvent *event) override;
	void mousePressEvent (QMouseEvent *event) override;
	void scrollContentsBy (int dx, int dy) override;
private:
	class PropertyList *plist = nullptr;
};

#endif // __linux__
class PropertyList {
public:
	PropertyList ();
	~PropertyList ();
#ifndef __linux__
	void OnInitDialog (HWND hWnd, int nIDDlgItem);
	void OnPaint (HWND hWnd);
#else // __linux__
	void OnInitDialog (QWidget *hWnd, int nIDDlgItem);
	void OnPaint (QWidget *hWnd);
#endif // __linux__
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

#ifndef __linux__
	static HBITMAP hBmpArrows;
#else // __linux__
	static QImage *hBmpArrows;
#endif // __linux__

protected:
	void SetListHeight (int h, bool force = false);
	void VScrollTo (int pos);

private:
#ifndef __linux__
	HWND hDlg;          // window handle for dialog box
	HWND hItem;         // window handle for list control
	HFONT hFontTitle;   // group title font
	HFONT hFontItem;    // item font
	HPEN hPenLine;      // pen for cell borders
	HBRUSH hBrushTitle; // brush for title backgrounds
#else // __linux__
	QWidget *hDlg;      // window handle for dialog box
	PropertyListCtrl *hItem; // window handle for list control
	QFont *hFontTitle;  // group title font
	QFont *hFontItem;   // item font
	QPen *hPenLine;     // pen for cell borders
	QBrush *hBrushTitle; // brush for title backgrounds
#endif // __linux__
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
