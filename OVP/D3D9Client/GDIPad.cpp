// ==============================================================
// GDIClient.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006 - 2016 Martin Schweiger
// ==============================================================

#include "GDIPad.h"
#include "D3D9Pad.h"
#include "D3D9Client.h"
#include "D3D9Surface.h"
#include "D3D9Util.h"
#include "D3D9Config.h"
#include "Log.h"

using namespace oapi;


static std::string UTF8ToCP1252(const char *utf8, int ulen)
{
	// Convert UTF-8 to Windows-1252
	std::wstring utf16(ulen, L'\0');
	MultiByteToWideChar(CP_UTF8, 0, utf8,
                        ulen, utf16.data(), ulen);

	int wlen = WideCharToMultiByte(28591, 0, utf16.c_str(),
                                  utf16.length(), nullptr, 0,
                                  nullptr, nullptr);
	// In case of problem, return the original string
	// to help with backward compatibility
	if (wlen == 0) return std::string(utf8, ulen);

	// The string will be at most ulen in length since
	// it won't contain multibyte characters
	std::string str(ulen, '\0');
	int len = WideCharToMultiByte(28591, 0, utf16.c_str(),
                        utf16.length(), &str[0], wlen, 
                        nullptr, nullptr);

	// Resize to proper length
	str.resize(len);
	return str;
}

// ===============================================================================================
// class GDIPad
// ===============================================================================================

GDIPad::GDIPad (SURFHANDLE s, HDC hdc): Sketchpad (s)
{
	LogOk("Creating GDI SketchPad... for Surface %s", _PTR(s));

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

// ===============================================================================================
//
GDIPad::~GDIPad ()
{
	// make sure to deselect custom resources before destroying the DC
	if (hFont0) SelectObject (hDC, hFont0);
	SelectObject (hDC, GetStockObject (NULL_PEN));
	SelectObject (hDC, GetStockObject (NULL_BRUSH));
	if (hFontA) DeleteObject(hFontA);
	LogOk("...GDI SketchPad Released for surface %s", _PTR(GetSurface()));
}

// ===============================================================================================
//
HDC GDIPad::GetDC()
{
	return hDC;
}

// ===============================================================================================
//
Font *GDIPad::SetFont (Font *font)
{
	Font *pfont = cfont;
	if (font) {
		HFONT hFont = (HFONT)SelectObject (hDC, static_cast<D3D9PadFont*>(font)->hFont);
		if (!cfont) hFont0 = hFont;
	} else if (hFont0) { // restore original font
		SelectObject (hDC, hFont0);
		hFont0 = 0;
	}
	cfont = font;
	return pfont;
}

// ===============================================================================================
//
Pen *GDIPad::SetPen (Pen *pen)
{
	Pen *ppen = cpen;
	if (pen) cpen = pen;
	else     cpen = NULL;
	if (cpen) SelectObject (hDC, static_cast<D3D9PadPen*>(cpen)->hPen);
	else      SelectObject (hDC, GetStockObject (NULL_PEN));
	return ppen;
}

// ===============================================================================================
//
Brush *GDIPad::SetBrush (Brush *brush)
{
	Brush *pbrush = cbrush;
	cbrush = brush;
	if (brush) SelectObject (hDC, static_cast<D3D9PadBrush*>(cbrush)->hBrush);
	else SelectObject (hDC, GetStockObject (NULL_BRUSH));
	return pbrush;
}

// ===============================================================================================
//
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

// ===============================================================================================
//
DWORD GDIPad::SetTextColor (DWORD col)
{
	return (DWORD)::SetTextColor (hDC, (COLORREF)(col&0xFFFFFF));
}

// ===============================================================================================
//
DWORD GDIPad::SetBackgroundColor (DWORD col)
{
	return (DWORD)SetBkColor (hDC, (COLORREF)(col&0xFFFFFF));
}

// ===============================================================================================
//
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

// ===============================================================================================
//
DWORD GDIPad::GetCharSize ()
{
	TEXTMETRIC tm;
	GetTextMetrics (hDC, &tm);
	return MAKELONG(tm.tmHeight-tm.tmInternalLeading, tm.tmAveCharWidth);
}

// ===============================================================================================
//
DWORD GDIPad::GetTextWidth (const char *utf8, int len)
{
	if (utf8) if (utf8[0] == '_') if (strcmp(utf8, "_SkpVerInfo") == 0) return 1;
	SIZE size;
	if (!len) len = lstrlen(utf8);
	std::string str = UTF8ToCP1252(utf8, len);

	GetTextExtentPoint32 (hDC, str.c_str(), str.length(), &size);
	return (DWORD)size.cx;
}

// ===============================================================================================
//
void GDIPad::SetOrigin (int x, int y)
{
	SetViewportOrgEx (hDC, x, y, NULL);
}

// ===============================================================================================
//
void GDIPad::GetOrigin (int *x, int *y) const
{
	POINT point;
	GetViewportOrgEx (hDC, &point);
	if (x) *x = point.x;
	if (y) *y = point.y;
}

// ===============================================================================================
//
bool GDIPad::Text (int x, int y, const char *utf8, int len)
{
	std::string str = UTF8ToCP1252(utf8, len);
	return (TextOut (hDC, x, y, str.c_str(), str.length()) != FALSE);
}

// ===============================================================================================
//
bool GDIPad::TextW (int x, int y, const LPWSTR str, int len)
{
	return (TextOutW(hDC, x, y, str, len) != FALSE);
}

// ===============================================================================================
//
bool GDIPad::TextBox (int x1, int y1, int x2, int y2, const char *utf8, int len)
{
	std::string str = UTF8ToCP1252(utf8, len);
	RECT r;
	r.left =   x1;
	r.top =    y1;
	r.right =  x2;
	r.bottom = y2;
	return (DrawText (hDC, str.c_str(), str.length(), &r, DT_LEFT|DT_NOPREFIX|DT_WORDBREAK) != 0);
}

// ===============================================================================================
//
void GDIPad::Pixel (int x, int y, DWORD col)
{
	SetPixel (hDC, x, y, (COLORREF)col);
}

// ===============================================================================================
//
void GDIPad::MoveTo (int x, int y)
{
	MoveToEx (hDC, x, y, NULL);
}

// ===============================================================================================
//
void GDIPad::LineTo (int x, int y)
{
	::LineTo (hDC, x, y);
}

// ===============================================================================================
//
void GDIPad::Line (int x0, int y0, int x1, int y1)
{
	MoveToEx (hDC, x0, y0, NULL);
	::LineTo (hDC, x1, y1);
}

// ===============================================================================================
//
void GDIPad::Rectangle (int x0, int y0, int x1, int y1)
{
	::Rectangle (hDC, x0, y0, x1, y1);
}

// ===============================================================================================
//
void GDIPad::Ellipse (int x0, int y0, int x1, int y1)
{
	::Ellipse (hDC, x0, y0, x1, y1);
}

// ===============================================================================================
//
void GDIPad::Polygon (const IVECTOR2 *pt, int npt)
{
	::Polygon (hDC, (const POINT*)pt, npt);
}

// ===============================================================================================
//
void GDIPad::Polyline (const IVECTOR2 *pt, int npt)
{
	::Polyline (hDC, (const POINT*)pt, npt);
}

// ===============================================================================================
//
void GDIPad::PolyPolygon (const IVECTOR2 *pt, const int *npt, const int nline)
{
	::PolyPolygon (hDC, (const POINT*)pt, npt, nline);
}

// ===============================================================================================
//
void GDIPad::PolyPolyline (const IVECTOR2 *pt, const int *npt, const int nline)
{
	::PolyPolyline (hDC, (const POINT*)pt, (const DWORD*)npt, nline);
}
