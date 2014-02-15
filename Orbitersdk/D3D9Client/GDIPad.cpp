// ==============================================================
// GDIClient.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2006 Martin Schweiger
// ==============================================================

#include "GDIPad.h"
#include "D3D9Pad.h"
#include "D3D9Client.h"
#include "D3D9Surface.h"
#include "D3D9Util.h"
#include "D3D9Config.h"
#include "Log.h"

using namespace oapi;


// ======================================================================
// class GDIPad
// ======================================================================

GDIPad::GDIPad (SURFHANDLE s, HDC hdc): Sketchpad (s)
{
	LogOk("Creating GDI SketchPad...");

	hDC    = hdc;
	cfont  = NULL;
	cpen   = NULL;
	cbrush = NULL;
	hFont0 = NULL;
	hFontA = NULL;

	// Default initial drawing settings
	SetBkMode (hDC, TRANSPARENT); // transparent text background
	SelectObject(hDC, GetStockObject (NULL_PEN));
	SelectObject(hDC, GetStockObject (NULL_BRUSH));
}

GDIPad::~GDIPad ()
{
	// make sure to deselect custom resources before destroying the DC
	if (hFont0) SelectObject (hDC, hFont0);
	SelectObject (hDC, GetStockObject (NULL_PEN));
	SelectObject (hDC, GetStockObject (NULL_BRUSH));
	if (hFontA) DeleteObject(hFontA);
	LogOk("...GDI SketchPad Released");
}

HDC GDIPad::GetDC()
{
	SURFHANDLE srf = GetSurface();
	if (!SURFACE(srf)->bSkpGetDCEr) {
		//LogWrn("GDIPad::GetDC() called for surface 0x%X",srf);
		SURFACE(srf)->bSkpGetDCEr = true;
	}

	return hDC;
}

Font *GDIPad::SetFont (Font *font) const
{
	//LogErr("GDIPad::SetFont(0x%X) surface(%u,%u)",font,SURFACE(GetSurface())->GetWidth(), SURFACE(GetSurface())->GetHeight());
	Font *pfont = cfont;
	if (font) {
		HFONT hFont = (HFONT)SelectObject (hDC, ((D3D9PadFont*)font)->hFont);
		if (!cfont) hFont0 = hFont; // Disabled for ShuttleFleet Hack
	} else if (hFont0) { // restore original font
		SelectObject (hDC, hFont0);
		hFont0 = 0;
	}
	cfont = font;
	return pfont;
}

Pen *GDIPad::SetPen (Pen *pen) const
{
	Pen *ppen = cpen;
	if (pen) cpen = pen;
	else     cpen = NULL;
	if (cpen) SelectObject (hDC, ((D3D9PadPen*)cpen)->hPen);
	else      SelectObject (hDC, GetStockObject (NULL_BRUSH));
	return ppen;
}

Brush *GDIPad::SetBrush (Brush *brush) const
{
	Brush *pbrush = cbrush;
	cbrush = brush;
	if (brush) SelectObject (hDC, ((D3D9PadBrush*)cbrush)->hBrush);
	else SelectObject (hDC, GetStockObject (NULL_BRUSH));
	return pbrush;
}

void GDIPad::SetTextAlign (TAlign_horizontal tah, TAlign_vertical tav)
{
	UINT align = 0;
	switch (tah) {
		case LEFT:     align |= TA_LEFT;     break;
		case CENTER:   align |= TA_CENTER;   break;
		case RIGHT:    align |= TA_RIGHT;    break;
	}
	switch (tav) {
		case TOP:      align |= TA_TOP;      break;
		case BASELINE: align |= TA_BASELINE; break;
		case BOTTOM:   align |= TA_BOTTOM;   break;
	}
	::SetTextAlign (hDC, align);
}

DWORD GDIPad::SetTextColor (DWORD col)
{
	return (DWORD)::SetTextColor (hDC, (COLORREF)(col&0xFFFFFF));
}

DWORD GDIPad::SetBackgroundColor (DWORD col)
{
	return (DWORD)SetBkColor (hDC, (COLORREF)(col&0xFFFFFF));
}

void GDIPad::SetBackgroundMode (BkgMode mode)
{
	int bkmode;
	switch (mode) {
		case BK_TRANSPARENT: bkmode = TRANSPARENT; break;
		case BK_OPAQUE:      bkmode = OPAQUE; break;
		default: return;
	}
	SetBkMode (hDC, bkmode);
}

DWORD GDIPad::GetCharSize ()
{
	TEXTMETRIC tm;
	GetTextMetrics (hDC, &tm);
	return MAKELONG(tm.tmHeight-tm.tmInternalLeading, tm.tmAveCharWidth);
}

DWORD GDIPad::GetTextWidth (const char *str, int len)
{
	SIZE size;
	if (!len) len = (int)strlen(str);
	GetTextExtentPoint32 (hDC, str, len, &size);
	return (DWORD)size.cx;
}

void GDIPad::SetOrigin (int x, int y)
{
	SetViewportOrgEx (hDC, x, y, NULL);
}

bool GDIPad::Text (int x, int y, const char *str, int len)
{
	return (TextOut (hDC, x, y, str, len) != FALSE);
}

bool GDIPad::TextBox (int x1, int y1, int x2, int y2, const char *str, int len)
{
	RECT r;
	r.left =   x1;
	r.top =    y1;
	r.right =  x2;
	r.bottom = y2;
	return (DrawText (hDC, str, len, &r, DT_LEFT|DT_NOPREFIX|DT_WORDBREAK) != 0);
}

void GDIPad::Pixel (int x, int y, DWORD col)
{
	SetPixel (hDC, x, y, (COLORREF)col);
}

void GDIPad::MoveTo (int x, int y)
{
	MoveToEx (hDC, x, y, NULL);
}

void GDIPad::LineTo (int x, int y)
{
	::LineTo (hDC, x, y);
}

void GDIPad::Line (int x0, int y0, int x1, int y1)
{
	MoveToEx (hDC, x0, y0, NULL);
	::LineTo (hDC, x1, y1);
}

void GDIPad::Rectangle (int x0, int y0, int x1, int y1)
{
	::Rectangle (hDC, x0, y0, x1, y1);
}

void GDIPad::Ellipse (int x0, int y0, int x1, int y1)
{
	::Ellipse (hDC, x0, y0, x1, y1);
}

void GDIPad::Polygon (const IVECTOR2 *pt, int npt)
{
	::Polygon (hDC, (const POINT*)pt, npt);
}

void GDIPad::Polyline (const IVECTOR2 *pt, int npt)
{
	::Polyline (hDC, (const POINT*)pt, npt);
}

void GDIPad::PolyPolygon (const IVECTOR2 *pt, const int *npt, const int nline)
{
	::PolyPolygon (hDC, (const POINT*)pt, npt, nline);
}

void GDIPad::PolyPolyline (const IVECTOR2 *pt, const int *npt, const int nline)
{
	::PolyPolyline (hDC, (const POINT*)pt, (const DWORD*)npt, nline);
}