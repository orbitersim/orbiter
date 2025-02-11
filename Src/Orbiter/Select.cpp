// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1

#include <stdio.h>
#include <string.h>
#include "Orbiter.h"
#include "Config.h"
#include "Select.h"
#include "Util.h"
#include "Log.h"

using std::max;

extern Orbiter *g_pOrbiter;
extern char DBG_MSG[256];
COLORREF titlecol   = RGB(255,255,255);
COLORREF enablecol  = RGB(210,210,210);
COLORREF disablecol = RGB(130,130,130);
COLORREF enablehi   = RGB(255,255,255);
COLORREF disablehi  = RGB(150,150,150);
COLORREF markcol    = RGB(96,96,96);

// =======================================================================
// local prototypes

//SURFHANDLE AllocSurface (oapi::GraphicsClient *gc, int w, int h);
void ReleaseSurface (LPDIRECTDRAWSURFACE7 &surf);
void ClearSurface (oapi::GraphicsClient *gc, SURFHANDLE surf, int h = 0);

// =======================================================================
// Base class for inline dialog box resources

struct InlineDialog::DRAWRESRC InlineDialog::draw = {0,0,0,0,0};

InlineDialog::InlineDialog (oapi::GraphicsClient *gclient, HWND hwnd)
{
	gc = gclient;
	hWnd = hwnd;
};

InlineDialog::~InlineDialog()
{
}

void InlineDialog::GlobalInit (oapi::GraphicsClient *gclient)
{
	if (!gclient) return; // sanity check

	draw.pen1 = gclient->clbkCreatePen (1, 0, 0x808080);
	draw.pen2 = gclient->clbkCreatePen (1, 0, 0xFFFFFF);
	draw.brushMark = gclient->clbkCreateBrush (markcol);

	DWORD viewH = g_pOrbiter->ViewH();
	double scale = g_pOrbiter->Cfg()->CfgFontPrm.dlgFont_Scale;
	char *face1 = g_pOrbiter->Cfg()->CfgFontPrm.dlgFont1_Face;

	draw.fontH = (int)(viewH*0.02*scale);
	if (draw.fontH < 8) draw.fontH = 8;
	draw.fontNorm = gclient->clbkCreateFont (draw.fontH, true, face1);
	draw.fontInactive = gclient->clbkCreateFont (draw.fontH, true, face1, FONT_ITALIC);
	draw.fontInput = gclient->clbkCreateFont (draw.fontH, false, "Courier New");
}

void InlineDialog::GlobalExit (oapi::GraphicsClient *gclient)
{
	if (draw.pen1)         gclient->clbkReleasePen (draw.pen1);
	if (draw.pen2)         gclient->clbkReleasePen (draw.pen2);
	if (draw.brushMark)    gclient->clbkReleaseBrush (draw.brushMark);
	if (draw.fontNorm)     gclient->clbkReleaseFont (draw.fontNorm);
	if (draw.fontInactive) gclient->clbkReleaseFont (draw.fontInactive);
	if (draw.fontInput)    gclient->clbkReleaseFont (draw.fontInput);
}

// =======================================================================
// menu dialog box

Select::Select (oapi::GraphicsClient *gclient, HWND hwnd): InlineDialog (gclient, hwnd)
{
	alen = llen = level = 0;
	cbk_submenu = 0;
	cbk_enter = 0;
	cntx = cnty = -1;
	surf = 0;
	Clear ();
	lineh = draw.fontH;
	arrow = lineh/3;
}

Select::~Select ()
{
	if (alen) {
		for (int i = 0; i < alen; i++) {
			delete []buf[i];
			buf[i] = NULL;
		}
		delete []buf;
		buf = NULL;
		delete []buflen;
		buflen = 0;
	}
	if (llen) {
		for (int i = 0; i < llen; i++) {
			delete []lbuf[i];
			lbuf[i] = NULL;
		}
		delete []lbuf;
		lbuf = NULL;
		delete []lbuflen;
		lbuflen = 0;
		delete []lcur;
		lcur = 0;
	}
}

void Select::Activate ()
{
	DWORD viewW = g_pOrbiter->ViewW(), viewH = g_pOrbiter->ViewH();
	int i, colh, itemh;
	bool submenu = false;
	DWORD textW;

	// remove trailing separator
	buf[len-1][0] &= ~ITEM_SEPARATOR;

	// find width of longest entry
	oapi::Sketchpad *skp = gc->clbkGetSketchpad (NULL);
	if (skp) {
		oapi::Font *nfnt, *ofnt = 0;

		colh = 0;
		surfw = lineh; // left and right frame borders
		surfh = 0;
		ncol = 0;
		col[ncol].nitem = 0;
		col[ncol].w = 0;
		for (i = 0; i < len; i++) {
			nfnt = (buf[i][0] & ITEM_NOHILIGHT ? draw.fontInactive : draw.fontNorm);
			if (nfnt != ofnt) skp->SetFont (ofnt = nfnt);
			textW = skp->GetTextWidth (buf[i]+1);
			itemh = (buf[i][0] & ITEM_SEPARATOR ? lineh+3 : lineh);
			if (colh + itemh + 2*lineh > viewH) { // new column
				if (submenu) col[ncol].w += lineh/2+arrow;
				surfw += col[ncol].w + lineh; // column and vertical divider
				ncol++;
				col[ncol].w = textW;
				surfh = max (surfh, colh);
				colh = itemh;
				submenu = false;
			} else {
				col[ncol].w = max (col[ncol].w, (int)textW);
				colh += itemh;
			}
			col[ncol].nitem++;
			if (buf[i][0] & ITEM_SUBMENU) submenu = true;
		}
		if (submenu) col[ncol].w += lineh/2+arrow;
		surfw += col[ncol].w;
		surfh = max (surfh, colh) + lineh + 4;

		if (ofnt != draw.fontNorm) skp->SetFont (draw.fontNorm);
		textW = skp->GetTextWidth (title);
		if (textW+lineh > surfw) surfw = textW+lineh;
		gc->clbkReleaseSketchpad (skp);
	}

	X0 = (cntx >= 0 ? cntx : viewW/2) - surfw/2;
	if      (X0 < 0)              X0 = 0;
	else if (X0+surfw >= viewW) X0 = viewW-surfw-1;
	Y0 = (cnty >= 0 ? cnty : viewH/2) - surfh/2;
	if      (Y0 < 0)              Y0 = 0;
	else if (Y0+surfh >= viewH) Y0 = viewH-surfh-1;

	// allocate and init surface
	surf = gc->clbkCreateSurface (surfw, surfh);
	RefreshSurface ();
	SetFocus (g_pOrbiter->GetRenderWnd());
}

void Select::Open (const char *_title, Callbk submenu_cbk, Callbk enter_cbk,
				   void *_userdata, int _cntx, int _cnty)
{
	if (surf) Clear (true); // this line was commented. why?
	SetTitle (_title);
	SetSubmenuCallback (submenu_cbk);
	SetEnterCallback (enter_cbk);
	userdata = _userdata;
	cntx = _cntx, cnty = _cnty;
	submenu_cbk (this, 0, 0, userdata);
	Activate ();
}

void Select::Clear (bool resetlevel)
{
	len = 0; // keep buffered entries around
	cur = 0;
	listw = listh = 0;
	if (resetlevel) level = 0;
	if (surf) {
		gc->clbkReleaseSurface (surf);
		surf = 0;
	}
}

void Select::MarkItem (int item)
{
	gc->clbkFillSurface (surf, 0, item*lineh, surfw, lineh,
		gc->clbkGetDeviceColour (0x80, 0x80, 0x80));
	//RECT r = {0, item*lineh, surfw, item*lineh+lineh};
	//DDBLTFX bltfx;
	//ZeroMemory (&bltfx, sizeof(bltfx));
	//bltfx.dwSize = sizeof(bltfx);
	//bltfx.dwFillColor = GetSurfColour (0x80,0x80,0x80);
	//surf->Blt (&r, NULL, NULL, DDBLT_COLORFILL, &bltfx);
}

void Select::Display (LPDIRECTDRAWSURFACE7 pdds)
{
	if (!surf) return; // sanity check
	gc->clbkBlt (0, X0, Y0, surf, 0);
}

void Select::RefreshSurface ()
{
	if (!surf) return; // sanity check
	ClearSurface (gc, surf, lineh);

	// draw list
	int i, cl, item0, item1, ay, x = lineh/2, y = 2;
	int subx = surfw-lineh/2-arrow;
	oapi::Font *ofnt, *nfnt;
	oapi::Sketchpad *skp = gc->clbkGetSketchpad (surf);
	if (skp) {
		skp->SetFont (ofnt = draw.fontNorm);
		skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);
		skp->SetTextColor (titlecol);
		skp->SetPen (draw.pen1);
		skp->Text (x, y-1, title, strlen(title)), y += lineh;
		cl = 0;
		item0 = 0; item1 = col[0].nitem;
		for (i = 0; i < len; i++) {
			nfnt = (buf[i][0] & ITEM_NOHILIGHT ? draw.fontInactive : draw.fontNorm);
			if (nfnt != ofnt) skp->SetFont (ofnt = nfnt);
			if (i == item1) {
				skp->Line (x + col[cl].w + lineh/2, lineh+2, x + col[cl].w + lineh/2, surfh);
				x += col[cl].w + lineh;
				subx += col[cl].w + lineh;
				y = 2+lineh;
				item0 = item1;
				item1 += col[++cl].nitem;
			}
			if (i == cur) {
				int x0 = x-lineh/2;
				int x1 = (cl == ncol ? surfw : x0+col[cl].w+lineh);
				oapi::Brush *pbrush = skp->SetBrush (draw.brushMark);
				skp->Rectangle (x0+2, y, x1-2, y+lineh);
				skp->SetBrush (pbrush);
			}
			if (i == cur) skp->SetTextColor (buf[i][0] & ITEM_NOHILIGHT ? disablehi : enablehi);
			else          skp->SetTextColor (buf[i][0] & ITEM_NOHILIGHT ? disablecol : enablecol);
			skp->Text (x, y, buf[i]+1, strlen (buf[i]+1));

			if (buf[i][0] & ITEM_SUBMENU) { // submenu indicator
				ay = y+lineh/2;
				skp->MoveTo (subx, ay-arrow);
				if (i == cur) skp->SetPen (draw.pen2);
				skp->LineTo (subx+arrow, ay);
				skp->LineTo (subx, ay+arrow);
				skp->LineTo (subx, ay-arrow);
				if (i == cur) skp->SetPen (draw.pen1);
			}
			y += lineh;
			if (buf[i][0] & ITEM_SEPARATOR) { // separator indicator
				skp->Line (0, y+1, surfw, y+1);
				y += 3;
			}
		}
		skp->Rectangle (0, 0, surfw, surfh);
		gc->clbkReleaseSketchpad (skp);
	}
}

int Select::Append (const char *_str, BYTE flag)
{
	if (len == alen) { // need to allocate new entry
		char **ctmp = new char*[alen+1]; TRACENEW
		memcpy (ctmp, buf, alen*sizeof(char*));
		if (alen) delete []buf;
		buf = ctmp;
		int *itmp = new int[alen+1]; TRACENEW
		memcpy (itmp, buflen, alen*sizeof(int));
		if (alen) delete []buflen;
		buflen = itmp;
		buflen[alen++] = 0;
	}
	int nlen = strlen (_str)+2; // reserve space for flag and EOL
	if (buflen[len] < nlen) { // need to (re)allocate space for entry
		if (buflen[len]) delete []buf[len];
		buf[len] = new char[buflen[len] = nlen]; TRACENEW
	}
	memcpy (buf[len]+1, _str, nlen-1);
	buf[len][0] = (char)flag;
	listh += lineh;
	if (flag & ITEM_SEPARATOR) listh += 3;
	return len++;
}

void Select::AppendSeparator ()
{
	if (!len) return;
	if (buf[len-1][0] & ITEM_SEPARATOR) return; // got a separator already
	buf[len-1][0] |= ITEM_SEPARATOR;
	listh += 3;
}

void Select::SetTitle (const char *_title)
{
	strncpy(title, _title, select_strlen);
	title[select_strlen - 1] = '\0';
}

void Select::Push ()
{
	if (level == llen) { // grow stack
		char **ctmp = new char*[llen+1]; TRACENEW
		memcpy (ctmp, lbuf, llen*sizeof(char*));
		if (llen) delete []lbuf;
		lbuf = ctmp;
		int *itmp = new int[llen+1]; TRACENEW
		memcpy (itmp, lbuflen, llen*sizeof(int));
		if (llen) delete []lbuflen;
		lbuflen = itmp;
		lbuflen[llen] = 0;
		itmp = new int[llen+1]; TRACENEW
		memcpy (itmp, lcur, llen*sizeof(int));
		if (llen) delete []lcur;
		lcur = itmp;
		llen++;
	}
	if (lbuflen[level] < buflen[cur]) { // grow stack entry
		if (lbuflen[level]) delete lbuf[level];
		lbuf[level] = new char[lbuflen[level] = buflen[cur]]; TRACENEW
	}
	memcpy (lbuf[level], buf[cur], buflen[cur]);
	lcur[level++] = cur;
}

void Select::Pop ()
{
	if (!level) return; // sanity check
	cur = lcur[--level];
}

int Select::ConsumeKey (UINT uMsg, WPARAM wParam, WORD mod)
{
	if (!IsActive() || uMsg != WM_KEYDOWN || mod) return key_ignore;

	switch (wParam) {
	case VK_RETURN:
		if (!(buf[cur][0] & ITEM_NOHILIGHT) &&
			(!cbk_enter || cbk_enter (this, cur, buf[cur]+1, userdata))) {
			Clear (true);
			return key_ok;
		} else return key_consume;
	case VK_ESCAPE:
		Clear (true);
		return key_cancel;
	case VK_DOWN:
		if (cur < len-1) {
			cur++;
			RefreshSurface ();
		}
		return key_consume;
	case VK_UP:
		if (cur > 0) {
			cur--;
			RefreshSurface ();
		}
		return key_consume;
	case VK_RIGHT:
		if ((buf[cur][0] & ITEM_SUBMENU) && cbk_submenu) {
			Push (); // push current level to stack
			Clear ();
			if (cbk_submenu (this, lcur[level-1], lbuf[level-1]+1, userdata)) {
				Activate ();
				return key_consume;
			}
		} else return key_ignore;
		// fall through
	case VK_LEFT:
		if (level && cbk_submenu) {
			Clear ();
			Pop (); // get previous level from stack
			if (level) cbk_submenu (this, lcur[level-1], lbuf[level-1]+1, userdata);
			else       cbk_submenu (this, 0, 0, userdata);  // main menu
			Activate ();
		}
		return key_consume;
	default:
		return key_ignore;
	}
}

// =======================================================================

SelectionList::SelectionList (oapi::GraphicsClient *gclient, HWND hwnd): InlineDialog (gclient, hwnd)
{
	clbk = 0;
	userdata = 0;
	listflag = 0;
	list = 0;
	nlist = 0;
	title = 0;
	lineh = draw.fontH;
	arrow = lineh/3;
	surf = 0;
	cur = 0;
}

void SelectionList::Open (LISTENTRY *_list, DWORD _nlist, char *_title, Listentry_clbk _clbk,
	DWORD flag, void *_userdata)
{
	Clear ();

	clbk = _clbk;
	userdata = _userdata;
	listflag = flag;
	list = new LISTENTRY[nlist = _nlist];
	memcpy (list, _list, nlist*sizeof(LISTENTRY));

	if (_title) {
		title = new char[strlen(_title)+1];
		strcpy (title, _title);
	}

	AllocSurface();
}

void SelectionList::Clear ()
{
	clbk = 0;
	userdata = 0;
	listflag = 0;
	if (nlist) {
		delete []list;
		list = 0;
		nlist = 0;
	}
	if (title) {
		delete []title;
		title = 0;
	}
	if (surf) {
		gc->clbkReleaseSurface (surf);
		surf = 0;
	}
}

void SelectionList::AllocSurface ()
{
	DWORD viewW = g_pOrbiter->ViewW(), viewH = g_pOrbiter->ViewH();
	int colh, itemh;
	DWORD i, textw;
	bool submenu = false;

	// remove trailing separator
	list[nlist-1].flag &= ~LISTENTRY_SEPARATOR;

	// find width of longest entry
	oapi::Sketchpad *skp = gc->clbkGetSketchpad (NULL);
	if (skp) {
		oapi::Font *nfnt, *ofnt = 0;
		colh = 0;
		surfw = lineh;
		surfh = 0;
		ncol = 0;
		col[ncol].nitem = 0;
		col[ncol].w = 0;
		for (i = 0; i < nlist; i++) {
			nfnt = (list[i].flag & LISTENTRY_INACTIVE ? draw.fontInactive : draw.fontNorm);
			if (nfnt != ofnt) skp->SetFont (ofnt = nfnt);
			textw = skp->GetTextWidth (list[i].name);
			itemh = (list[i].flag & LISTENTRY_SEPARATOR ? lineh+3 : lineh);
			if (colh + itemh + 2*lineh > viewH) { // new column
				if (submenu) col[ncol].w += lineh/2+arrow;
				surfw += col[ncol].w + lineh; // column and vertical divider
				ncol++;
				col[ncol].w = textw;
				surfh = max (surfh, colh);
				colh = itemh;
				submenu = false;
			} else {
				col[ncol].w = max ((DWORD)col[ncol].w, textw);
				colh += itemh;
			}
			col[ncol].nitem++;
			if (list[i].flag & LISTENTRY_SUBITEM) submenu = true;
		}
		if (submenu) col[ncol].w += lineh/2+arrow;
		surfw += col[ncol].w;
		surfh = max (surfh, colh) + lineh + 4;

		if (ofnt != draw.fontNorm) skp->SetFont (draw.fontNorm);
		textw = skp->GetTextWidth (title);
		if (textw+lineh > surfw) surfw = textw+lineh;
		gc->clbkReleaseSketchpad (skp);
	}

	surfx = viewW/2 - surfw/2;
	if      (surfx < 0)            surfx = 0;
	else if (surfx+surfw >= viewW) surfx = viewW-surfw-1;
	surfy = viewH/2 - surfh/2;
	if      (surfy < 0)            surfy = 0;
	else if (surfy+surfh >= viewH) surfy = viewH-surfh-1;

	if (surf) gc->clbkReleaseSurface (surf);
	surf = gc->clbkCreateSurface (surfw, surfh);
}

void SelectionList::RefreshSurface ()
{
	if (!surf) return; // sanity check
	ClearSurface (gc, surf, lineh);

	// draw list
	int i, cl, item0, item1, ay, x = lineh/2, y = 2;
	int subx = surfw-lineh/2-arrow;
	oapi::Font *ofnt, *nfnt;
	oapi::Sketchpad *skp = gc->clbkGetSketchpad (surf);
	if (skp) {
		skp->SetFont (ofnt = draw.fontNorm);
		skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);
		skp->SetTextColor (titlecol);
		skp->SetPen (draw.pen1);
		skp->Text (x, y-1, title, strlen(title)), y += lineh;
		cl = 0;
		item0 = 0; item1 = col[0].nitem;
		for (i = 0; i < nlist; i++) {
			nfnt = (list[i].flag & LISTENTRY_INACTIVE ? draw.fontInactive : draw.fontNorm);
			if (nfnt != ofnt) skp->SetFont (ofnt = nfnt);
			if (i == item1) {
				skp->Line (x + col[cl].w + lineh/2, lineh+2, x + col[cl].w + lineh/2, surfh);
				x += col[cl].w + lineh;
				subx += col[cl].w + lineh;
				y = 2+lineh;
				item0 = item1;
				item1 += col[++cl].nitem;
			}
			if (i == cur) {
				int x0 = x-lineh/2;
				int x1 = (cl == ncol ? surfw : x0+col[cl].w+lineh);
				oapi::Brush *pbrush = skp->SetBrush (draw.brushMark);
				skp->Rectangle (x0+2, y, x1-2, y+lineh);
				skp->SetBrush (pbrush);
			}
			if (i == cur) skp->SetTextColor (list[i].flag & LISTENTRY_INACTIVE ? disablehi : enablehi);
			else          skp->SetTextColor (list[i].flag & LISTENTRY_INACTIVE ? disablecol : enablecol);
			skp->Text (x, y, list[i].name, strlen (list[i].name));

			if (list[i].flag & LISTENTRY_SUBITEM) { // submenu indicator
				ay = y+lineh/2;
				skp->MoveTo (subx, ay-arrow);
				if (i == cur) skp->SetPen (draw.pen2);
				skp->LineTo (subx+arrow, ay);
				skp->LineTo (subx, ay+arrow);
				skp->LineTo (subx, ay-arrow);
				if (i == cur) skp->SetPen (draw.pen1);
			}
			y += lineh;
			if (list[i].flag & LISTENTRY_SEPARATOR) { // separator indicator
				skp->Line (0, y+1, surfw, y+1);
				y += 3;
			}
		}
		skp->Rectangle (0, 0, surfw, surfh);
		gc->clbkReleaseSketchpad (skp);
	}
}

// =======================================================================

InputBox::InputBox (oapi::GraphicsClient *gclient, HWND hwnd, int _buflen): InlineDialog (gclient, hwnd)
{
	buflen = _buflen;
	titlelen = 20; // can grow as required
	title = new char[titlelen+1];  title[0] = '\0'; TRACENEW
	buf   = new char[buflen+1];    buf[0] = '\0';   TRACENEW
	slen = vis0 = cur = 0;
	vislen = 20;   // arbitrary default
	surf = 0;
	cbk_enter = 0;
	cbk_cancel = 0;
	userdata = 0;
	lineh  = draw.fontH;
	cw     = g_gdires->dlgF2W;  // replace this!
}

InputBox::~InputBox ()
{
	delete []buf;
	buf = NULL;
	delete []title;
	title = NULL;
}

void InputBox::SetTitle (const char *_title)
{
	int len = (_title ? strlen(_title) : 0);
	if (len > titlelen) {
		if (titlelen) delete []title;
		title = new char[(titlelen=len)+1]; TRACENEW
	}
	if (len) strcpy (title, _title);
	else title[0] = '\0';
}

void InputBox::InitBuffer (int _vislen, char *_buf)
{
	vislen = _vislen;
	vis0 = 0;
	if (_buf) {
		slen = cur = strlen(_buf);
		strcpy (buf, _buf);
	} else {
		slen = cur = 0;
		buf[0] = '\0';
	}
}

void InputBox::Activate (int cntx, int cnty)
{
	DWORD viewW = g_pOrbiter->ViewW(), viewH = g_pOrbiter->ViewH();
	DWORD textW;
	oapi::Sketchpad *skp = gc->clbkGetSketchpad (NULL);
	if (skp) {
		skp->SetFont (draw.fontNorm);
		textW = skp->GetTextWidth (title);
		surfw = textW + lineh;
		boxw = cw*vislen;
		if (boxw+lineh > surfw) surfw = boxw+lineh;
		surfh = 4*lineh;
		gc->clbkReleaseSketchpad (skp);
	}
	X0 = (cntx >= 0 ? cntx : viewW/2) - surfw/2;
	if      (X0 < 0)              X0 = 0;
	else if (X0+surfw >= viewW) X0 = viewW-surfw-1;
	Y0 = (cnty >= 0 ? cnty : viewH/2) - surfh/2;
	if      (Y0 < 0)              Y0 = 0;
	else if (Y0+surfh >= viewH) Y0 = viewH-surfh-1;

	// allocate and init surface
	if (surf) gc->clbkReleaseSurface (surf);
	surf = gc->clbkCreateSurface (surfw, surfh);
	RefreshSurface ();
}

void InputBox::Open (const char *_title, char *_buf, int _vislen,
					 Callbk cbk, void *_userdata, int cntx, int cnty)
{
	OpenEx (_title, _buf, _vislen, cbk, 0, _userdata, 0, cntx, cnty);
}

bool InputBox::OpenEx (const char *_title, char *_buf, int _vislen,
					 Callbk enter_cbk, Callbk cancel_cbk, void *_userdata, DWORD flags, int cntx, int cnty)
{
	if (surf) {
		if (flags & USRINPUT_NEEDANSWER) return false;
		// can't open new input box if currently open one requires an answer
		if (cbk_cancel) cbk_cancel (this, buf, userdata);
		Close();
	}
	SetTitle (_title);
	InitBuffer (_vislen, _buf);
	SetEnterCallback (enter_cbk);
	cbk_cancel = cancel_cbk;
	flag = flags;
	userdata = _userdata;
	Activate (cntx, cnty);
	return true;
}

bool InputBox::Close (bool onEnter)
{
	if (!onEnter && (flag & USRINPUT_NEEDANSWER)) return false;
	if (surf) {
		gc->clbkReleaseSurface (surf);
		surf = 0;
	}
	userdata = 0;
	return true;
}

void InputBox::RefreshSurface ()
{
	if (!surf) return; // sanity check
	ClearSurface (gc, surf, lineh);

	oapi::Sketchpad *skp = gc->clbkGetSketchpad (surf);
	if (skp) {
		int x = lineh/2, y = 2;
		skp->SetFont (draw.fontNorm);
		skp->SetPen (draw.pen1);
		skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);
		skp->SetTextColor (titlecol);
		skp->Text (x, y-1, title, strlen(title)); y += 2*lineh;
		skp->SetTextColor (enablecol);
		skp->SetFont (draw.fontInput);
		int n = slen-vis0;
		if (n > vislen) n = vislen;
		skp->Text (x, y, buf+vis0, n);
		skp->SetBackgroundMode (oapi::Sketchpad::BK_OPAQUE);
		skp->SetTextColor (0);
		skp->Text (x+(cur-vis0)*cw, y, cur < slen ? buf+cur : " ", 1);
		skp->Rectangle (0, 0, surfw, surfh);
		skp->Rectangle (x-2, y-2, x+boxw+2, y+lineh+2);
		gc->clbkReleaseSketchpad (skp);
	}
}

void InputBox::Display (LPDIRECTDRAWSURFACE7 pdds)
{
	if (!surf) return; // sanity check
	gc->clbkBlt (0, X0, Y0, surf, 0);
	//pdds->BltFast (X0, Y0, surf, NULL, DDBLTFAST_WAIT);
}

int InputBox::ConsumeKey (UINT uMsg, WPARAM wParam, WORD mod)
{
	int i;

	if (!IsActive()) return key_ignore;

	switch (uMsg) {
	case WM_CHAR:
		if (isgraph(wParam) || wParam == VK_SPACE) {
			if (slen < buflen) {
				for (i = slen; i >= cur; i--) buf[i+1] = buf[i];
				buf[cur] = wParam;
				slen++;
				if (++cur == vislen+vis0) vis0++;
				RefreshSurface();
			}
			return key_consume;
		}
		return key_ignore;
	case WM_KEYDOWN:
		if (!mod) {
			switch (wParam) {
			case VK_RETURN:
				if (!cbk_enter || cbk_enter (this, buf, userdata)) {
					Close (true);
					return key_ok;
				} else {
					MessageBeep (-1);
					return key_consume;
				}
			case VK_ESCAPE:
				if (flag & USRINPUT_NEEDANSWER) {
					return key_consume;
				} else {
					if (cbk_cancel) cbk_cancel (this, buf, userdata);
					Close ();
					return key_cancel;
				}
			case VK_LEFT:
				if (cur > 0) {
					if (--cur < vis0) vis0 = cur;
					RefreshSurface();
				}
				return key_consume;
			case VK_RIGHT:
				if (cur < slen) {
					if (++cur == vislen+vis0) vis0++;
					RefreshSurface();
				}
				return key_consume;
			case VK_BACK:
				if (cur > 0) {
					for (i = cur; i <= slen; i++) buf[i-1] = buf[i];
					slen--;
					if (--cur < vis0) vis0 = cur;
					RefreshSurface();
				}
				return key_consume;
			case VK_DELETE:
				if (cur < slen) {
					for (i = cur+1; i <= slen; i++) buf[i-1] = buf[i];
					slen--;
					RefreshSurface();
				}
				return key_consume;
			case VK_HOME:
				if (cur > 0) {
					cur = 0;
					if (vis0 > 0) vis0 = 0;
					RefreshSurface();
				}
				return key_consume;
			case VK_END:
				if (cur < slen) {
					if ((cur = slen) >= vislen+vis0) vis0 = cur-vislen+1;
					RefreshSurface();
				}
				return key_consume;
			default:
				// trap unmodified and shift keys
				if (!(mod & 0x02)) return key_consume;
				break;
			}
		}
	}
	return key_ignore;
}

// =======================================================================
// auxiliary routines

#ifdef UNDEF
SURFHANDLE AllocSurface (oapi::GraphicsClient *gc, int w, int h)
{
	return NULL;
}
#endif

void ReleaseSurface (LPDIRECTDRAWSURFACE7 &surf)
{
	if (surf) {
		surf->Release ();
		surf = 0;
	}
}

void ClearSurface (oapi::GraphicsClient *gc, SURFHANDLE surf, int h)
{
	gc->clbkFillSurface (surf, gc->clbkGetDeviceColour (0x50, 0x50, 0x50));
	//DDBLTFX bltfx;
	//ZeroMemory (&bltfx, sizeof(bltfx));
	//bltfx.dwSize = sizeof(bltfx);
	//bltfx.dwFillColor = GetSurfColour (0x50,0x50,0x50);
	//surf->Blt (NULL, NULL, NULL, DDBLT_COLORFILL, &bltfx);

	if (h) { // paint title bar
		DWORD width, height;
		gc->clbkGetSurfaceSize (surf, &width, &height);
		gc->clbkFillSurface (surf, 0, 0, width, h+1,
			gc->clbkGetDeviceColour (0x60, 0x20, 0x20));
		//DDSURFACEDESC2 ddsd;
		//ddsd.dwSize = sizeof(ddsd);
		//surf->GetSurfaceDesc (&ddsd);
		//RECT r = {0, 0, ddsd.dwWidth, h+1};
		//bltfx.dwFillColor = GetSurfColour (0x60,0x20,0x20);
		//surf->Blt (&r, NULL, NULL, DDBLT_COLORFILL, &bltfx);
	}
}
