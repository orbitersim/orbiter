#ifndef __SELECT_H
#define __SELECT_H

#define STRICT 1
#include <windows.h>
#include <d3d.h>
#include "OrbiterAPI.h"
#include "GraphicsAPI.h"

#define ITEM_SUBMENU 0x01
#define ITEM_NOHILIGHT 0x02
#define ITEM_SEPARATOR 0x04

const int select_strlen = 100;
const int select_chunk = 32;
const int select_maxlevel = 10;
const int maxcol = 20;

// =======================================================================
// Base class for inline dialog box resources

class InlineDialog {
public:
	InlineDialog (oapi::GraphicsClient *gclient, HWND hwnd);
	virtual ~InlineDialog();

	// initialise global drawing resources
	static void GlobalInit (oapi::GraphicsClient *gclient);

	// release global drawing resources
	static void GlobalExit (oapi::GraphicsClient *gclient);

protected:
	static struct DRAWRESRC {  // global drawing resources
		oapi::Pen *pen1, *pen2;
		oapi::Brush *brushMark;
		oapi::Font *fontNorm, *fontInactive, *fontInput;
		int fontH;
	} draw;

	oapi::GraphicsClient *gc;  // graphics client instance
	HWND hWnd;                 // render window handle
};

// =======================================================================
// menu dialog box

class Select: public InlineDialog {
public:
	Select (oapi::GraphicsClient *gclient, HWND hwnd);
	~Select ();
	void Activate ();
	void Clear (bool resetlevel = false);
	void Display (LPDIRECTDRAWSURFACE7 pdds);
	bool IsActive() { return (len > 0); }
	int Len() const { return len; }
	int Cur() const { return cur; }
	char *Str (int i) const { return buf[i]+1; }
	int ParentCur () const { return (level > 0 ? lcur[level-1] : 0); }
	const char *ParentStr () const { return (level > 0 ? lbuf[level-1]+1 : 0); }
	const char *StackStr (int lev) { return (lev >= 0 && lev < level ? lbuf[lev]+1 : 0); }
	bool HasSubmenu (int i) const { return buf[i][0] & 0x01; }
	int Level() const { return level; }
	int Append (const char *_str, BYTE flag = 0);
	void AppendSeparator ();
	void SetTitle (char *_title);
	const char *Title () const { return title; }
	int ConsumeKey (UINT uMsg, WPARAM wParam, WORD mod = 0);
	enum { key_ignore, key_consume, key_ok, key_cancel };
	typedef bool (*Callbk)(Select*, int, char*, void*);
	void SetSubmenuCallback (Callbk cbk) { cbk_submenu = cbk; }
	void SetEnterCallback (Callbk cbk) { cbk_enter = cbk; }
	
	void Open (char *_title = 0, Callbk submenu_cbk = 0, Callbk enter_cbk = 0,
		void *_userdata = 0, int cntx = -1, int cnty = -1);
	// activates the menu with the specified parameters (menu items must have
	// been added with Append before)

private:
	void RefreshSurface ();
	void MarkItem (int item);
	void Push ();
	void Pop ();

	SURFHANDLE surf;  // drawing surface
	int surfw, surfh; // surface dimensions
	int cntx, cnty;   // centre coords
	int X0, Y0;       // upper left corner of input box
	int lineh;    // font height
	int arrow;    // arrow size
	int listw;    // pixel width of longest list entry
	int listh;    // pixel height of full list (w/o title)
	int len;      // number of active entries
	int alen;     // number of allocated entries
	struct COL {  // column specs
		int nitem; //  # items in column
		int w;     //   column width
	} col[maxcol];
	int ncol;     // # columns
	int llen;     // level stack size
	int level;    // current submenu level (0=top level)
	int cur;      // selected entry index
	int *lcur;    // entry index stack (length llen)
	char **buf;   // array of string buffers (length alen)
	int *buflen;  // array buffer sizes (length alen);
	char **lbuf;  // entry stack (length llen);
	int *lbuflen; // array of stack entry sizes (length llen)
	char title[select_strlen];
	void *userdata;
	Callbk cbk_submenu;
	Callbk cbk_enter;
};

// =======================================================================
// selection list

class SelectionList: public InlineDialog {
public:
	SelectionList (oapi::GraphicsClient *gclient, HWND hwnd);

	void Open (LISTENTRY *list, DWORD nlist, char *_title, Listentry_clbk _clbk,
		DWORD flag = 0, void *_userdata = 0);

	void Clear();

private:
	void AllocSurface();
	void RefreshSurface();

	Listentry_clbk clbk;
	void *userdata;
	DWORD listflag;
	LISTENTRY *list;  // entries in selection list
	DWORD nlist;      // number of entries
	char *title;
	SURFHANDLE surf;  // drawing surface
	int surfw, surfh; // surface dimensions
	int surfx, surfy; // display location
	int lineh;        // font height
	int arrow;        // arrow size
	int cur;      // selected entry index
	int ncol;         // number of columns
	struct COL {      // column specs
		int nitem;      //  # items in column
		int w;          //   column width
	} col[maxcol];
};

// =======================================================================
// text input dialog box

class InputBox: public InlineDialog {
public:
	InputBox (oapi::GraphicsClient *gclient, HWND hwnd, int _buflen);
	// construct an input box resource for hwnd, allocating
	// an input buffer of size _buflen

	~InputBox ();

	typedef bool (*Callbk)(InputBox*, char*, void*);
	// template for callback function. Returns pointer to calling input box,
	// input box string and user-defined data

	void SetTitle (const char *_title);
	void InitBuffer (int _vislen, char *str);

	void Activate (int cntx = -1, int cnty = -1);
	// activate intput box centered at position cntx, cnty (default is viewport centre)

	bool IsActive() { return (surf != 0); }

	void Display (LPDIRECTDRAWSURFACE7 pdds);
	// copy input box onto viewport surface

	enum { key_ignore, key_consume, key_ok, key_cancel };

	int ConsumeKey (UINT uMsg, WPARAM wParam, WORD mod = 0);
	// Processes WM_CHAR and WM_KEYDOWN messages

	void SetEnterCallback (Callbk cbk) { cbk_enter = cbk; }
	// set callback function for accepting input

	void Open (char *_title = 0, char *_buf = 0, int _vislen = 20,
		Callbk cbk = 0, void *_userdata = 0, int cntx = -1, int cnty = -1);
	// activates the input box with the specified parameters
	// combines SetTitle, InitBuffer, SetEnterCallback and Activate

	bool OpenEx (const char *_title = 0, char *_buf = 0, int _vislen = 20,
		Callbk enter_cbk = 0, Callbk cancel_cbk = 0, void *_userdata = 0,
		DWORD flags = 0, int cntx = -1, int cnty = -1);
	// This version also provides a callback function for cancelling the
	// input box

	bool Close (bool onEnter = false);
	// de-activates the input box

private:
	void RefreshSurface ();

	SURFHANDLE surf;  // drawing surface
	int surfw, surfh; // surface dimensions
	int X0, Y0;       // upper left corner of input box
	int lineh;        // font height
	int boxw;         // input box width
	int cw;           // fixed character width
	char *title, *buf;
	int titlelen, buflen, vislen, vis0, slen;
	int cur;          // cursor position
	void *userdata;
	DWORD flag;
	Callbk cbk_enter;
	Callbk cbk_cancel;
};

#endif // !__SELECT_H