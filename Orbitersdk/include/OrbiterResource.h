// not upstream: resource tables compiled from .rc scripts (cmake/rc2cpp.py) and the Qt dialogs built from them

#ifndef __ORBITERRESOURCE_H
#define __ORBITERRESOURCE_H

#include "OrbiterAPI.h"
#include <functional>

// control kinds (rc2cpp.py classifies each .rc control into one of these)
enum RESKIND {
	RES_STATIC, RES_STATICIMAGE, RES_STATICFRAME, RES_BUTTON, RES_CHECKBOX, RES_RADIOBUTTON, RES_GROUPBOX,
	RES_EDIT, RES_COMBOBOX, RES_LISTBOX, RES_SCROLLBAR, RES_UPDOWN, RES_TRACKBAR, RES_TREEVIEW, RES_TABCONTROL,
	RES_PROGRESS, RES_RICHEDIT, RES_LISTVIEW, RES_CUSTOM
};

enum RESIMAGEKIND { RES_BITMAP, RES_ICON, RES_PNG };

// one control of a dialog template; coordinates in dialog units, styles keep the Win32 bit values of the .rc
struct RESCONTROL {
	int kind;
	int id;
	const char *text;     // UTF-8, or nullptr
	const char *cls;      // window class for RES_CUSTOM, else nullptr
	int imgid;            // image resource for image statics, else -1
	int x, y, cx, cy;
	DWORD style, exstyle;
	const char *idname;   // symbolic id from the .rc, or nullptr
};

struct RESDIALOG {
	int id;
	const char *name;
	const char *caption;
	const char *font;
	int fontsize, weight, italic;
	int x, y, cx, cy;
	DWORD style, exstyle;
	int nctrl;
	const RESCONTROL *ctrl;
	int menu;             // MENU statement: menu resource id, -1 if none
};

struct RESIMAGE {
	int kind;
	int id;
	const char *name;
	const unsigned char *data;
	int size;
};

// resource of a user-defined type (TEXT, IMAGE, RCDATA, ...) read from a file; data is zero-terminated after size bytes
struct RESDATA {
	const char *type;
	int id;
	const char *name;
	const unsigned char *data;
	int size;
};

// MENU/MENUEX item; a popup's nsub items follow it, each nested popup followed by its own items
struct RESMENUITEM {
	int id;               // command id (MENUEX popups may have one too)
	const char *text;     // UTF-8, '&' marks the mnemonic, '\t' starts the shortcut text; nullptr for a separator
	DWORD flags;          // MF_GRAYED 0x1, MF_DISABLED 0x2, MF_CHECKED 0x8, MF_POPUP 0x10, MF_MENUBARBREAK 0x20, MF_MENUBREAK 0x40, MF_HELP 0x4000
	int nsub;
};

struct RESMENU {
	int id;
	const char *name;
	int nitem;            // all items in .rc order (a popup's items right after it)
	const RESMENUITEM *item;
};

// STRINGTABLE entry (UTF-8)
struct RESSTRING {
	int id;
	const char *text;
};

struct RESTABLE {
	size_t ndlg;
	const RESDIALOG *dlg;
	size_t nimg;
	const RESIMAGE *img;
	size_t ndata;
	const RESDATA *data;
	size_t nstr;
	const RESSTRING *str;
	size_t nmenu;
	const RESMENU *menu;
};

class QWidget;
class QImage;
class QMenuBar;

// resource table of a module (dlopen handle, via its generated oapiModuleResources), or of Orbiter for hModule == 0
OAPIFUNC const RESTABLE *oapiResourceTable (void *hModule);
OAPIFUNC const RESDIALOG *oapiFindResDialog (void *hModule, int resId);
OAPIFUNC const RESIMAGE *oapiFindResImage (void *hModule, int resId);

// image resource as a QImage (LoadBitmap/LoadIcon counterpart); caller owns the image
OAPIFUNC QImage *oapiLoadResImage (void *hModule, int resId);

// not upstream: makes a bitmap's surround (its corner colour, joined to the edge) transparent, for trees that follow the desktop theme
OAPIFUNC void oapiClearImageBackground (QImage *img);

// MENU/MENUEX resource (FindResource counterpart)
OAPIFUNC const RESMENU *oapiFindResMenu (void *hModule, int resId);

// resource of a user-defined type (FindResource/LoadResource counterpart), type compared case-insensitively
OAPIFUNC const RESDATA *oapiFindResData (void *hModule, const char *type, int resId);

// STRINGTABLE string (LoadString counterpart): copies at most buflen-1 bytes plus a terminating zero,
// returns the number of bytes copied, 0 if there is no such string
OAPIFUNC int oapiLoadResString (void *hModule, int id, char *buf, int buflen);

// builds the Qt widgets of a dialog template (CreateDialogParam counterpart, without the message procedure); owner: hWndParent of a popup
OAPIFUNC QWidget *oapiCreateResDialog (void *hModule, int resId, QWidget *parent, QWindow *owner = nullptr);

// runs a modal dialog owned by a window that isn't a widget, e.g. the render window (MessageBox/GetOpenFileName hWndOwner)
class QDialog;
OAPIFUNC int oapiExecOwned (QDialog *dlg, QWindow *owner);

// LoadMenu + SetMenu counterpart (also used for a template's MENU statement): bar on top, window grows; items reach oapiConnectDlgCommands
OAPIFUNC QMenuBar *oapiCreateResMenu (void *hModule, int resId, QWidget *hWnd);

// dialog control by resource id (GetDlgItem counterpart)
OAPIFUNC QWidget *oapiResDlgItem (QWidget *hDlg, int id);

// resource id of a dialog or control widget (GetDlgCtrlID counterpart), 0 if none
OAPIFUNC int oapiResId (const QWidget *hWnd);

// custom control classes (RegisterClass/UnregisterClass counterparts for controls the .rc names by class);
// a dialog looks the class up for its own module first, then for any module
typedef QWidget *(*RESCTRLFACTORY)(const RESCONTROL *ctrl, QWidget *parent);
OAPIFUNC void oapiRegisterResControl (void *hModule, const char *cls, RESCTRLFACTORY create);
OAPIFUNC void oapiUnregisterResControl (void *hModule, const char *cls);

class QComboBox;

// command notifications of dialog controls (WM_COMMAND notification code counterparts)
enum RESNOTIFY {
	RESN_CLICKED,    // button clicked by the user (BN_CLICKED)
	RESN_CHANGE,     // edit box text changed, also when set by the program (EN_CHANGE)
	RESN_KILLFOCUS,  // edit box left, or Return pressed (EN_KILLFOCUS)
	RESN_SELCHANGE,  // combo box selection changed by the user (CBN_SELCHANGE); list box selection changed (LBN_SELCHANGE)
	RESN_DBLCLK,     // list box item double-clicked (LBN_DBLCLK)
	RESN_EDITCHANGE  // editable combo box text changed by the user (CBN_EDITCHANGE)
};

// WM_COMMAND switch of a dialog procedure: one handler for its controls and menu items (resource id, RESNOTIFY code, control or nullptr)
typedef std::function<void (int id, int code, QWidget *hCtrl)> RESCOMMAND;
OAPIFUNC void oapiConnectDlgCommands (QWidget *hDlg, RESCOMMAND handler);

// up-down (spin) controls: UDN_DELTAPOS of all up-down controls of a dialog (id, position before the change, requested change),
// UDM_SETRANGE and UDM_SETPOS
typedef std::function<void (int id, int iPos, int iDelta)> RESDELTAPOS;
OAPIFUNC void oapiConnectDlgDeltaPos (QWidget *hDlg, RESDELTAPOS handler);
OAPIFUNC void oapiSetUpDownRange (QWidget *hCtrl, int lower, int upper);
OAPIFUNC void oapiSetUpDownPos (QWidget *hCtrl, int pos);

// SetWindowText / GetWindowText counterparts for dialogs and their controls (labels, edit boxes, buttons, group boxes,
// editable combo boxes, window titles). Text is UTF-8; text that is not valid UTF-8 is taken as Latin-1.
// "\r\n" line ends become "\n". The getter returns the number of bytes copied (buf is zero-terminated).
OAPIFUNC void oapiSetDlgText (QWidget *hWnd, const char *text);
OAPIFUNC int  oapiGetDlgText (QWidget *hWnd, char *buf, int buflen);
OAPIFUNC void oapiSetDlgItemText (QWidget *hDlg, int id, const char *text);
OAPIFUNC int  oapiGetDlgItemText (QWidget *hDlg, int id, char *buf, int buflen);

// CB_ADDSTRING counterpart: appends the string, or inserts it in case-insensitive order in a sorted (CBS_SORT)
// combo box; returns the index of the new item
OAPIFUNC int oapiComboAddString (QComboBox *cb, const char *str);

#ifdef QT_WIDGETS_LIB
#include <QWidget>
// typed control lookup: DlgItem<QComboBox>(hDlg, IDC_X)
template<class T> inline T *DlgItem (QWidget *hDlg, int id)
{
	return qobject_cast<T*>(oapiResDlgItem (hDlg, id));
}
#endif

#endif // !__ORBITERRESOURCE_H
