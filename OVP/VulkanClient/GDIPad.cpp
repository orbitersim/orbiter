// ==============================================================
// GDIPad.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2026 Martin Schweiger
// ==============================================================

#include "GDIPad.h"
#include "D3D9Pad.h"
#include "D3D9Client.h"
#include "D3D9Surface.h"
#include "D3D9Util.h"
#include "D3D9Config.h"
#include "Log.h"
#include <QPainter>
#include <QPainterPath>
#include <QFontMetrics>
#include <vector>

using namespace oapi;

// not upstream: the wingdi.h TextAlign flags GDIPad keeps
#define TA_LEFT			0
#define TA_RIGHT		2
#define TA_CENTER		6
#define TA_TOP			0
#define TA_BOTTOM		8
#define TA_BASELINE		24


static std::string UTF8ToCP1252(const char *utf8, int ulen)
{
	// Convert UTF-8 to Windows-1252
	QString utf16 = QString::fromUtf8(utf8, ulen); // MultiByteToWideChar(CP_UTF8); upstream converted all ulen wide slots (NULs after multibyte text)

	QByteArray lat = utf16.toLatin1(); // WideCharToMultiByte(28591): ISO-8859-1, unmapped characters become '?'
	int wlen = int(lat.size());
	// In case of problem, return the original string
	// to help with backward compatibility
	if (wlen == 0) return std::string(utf8, ulen);

	// The string will be at most ulen in length since
	// it won't contain multibyte characters
	std::string str(ulen, '\0');
	int len = std::min(wlen, ulen);
	memcpy(&str[0], lat.constData(), len);

	// Resize to proper length
	str.resize(len);
	return str;
}

// ===============================================================================================
// class GDIPad
// ===============================================================================================

GDIPad::GDIPad (SURFHANDLE s, QPainter *hdc): Sketchpad (s)
{
	LogOk("Creating GDI SketchPad... for Surface %s", _PTR(s));

	hDC    = hdc;
	cfont  = NULL;
	cpen   = NULL;
	cbrush = NULL;
	hFont0 = NULL;
	hFontA = NULL;
	textcol = 0x000000;			// not upstream: a new DC's text colour, background colour, alignment and position
	bkcol  = 0xFFFFFF;
	textalign = TA_LEFT | TA_TOP;
	cpos   = QPoint(0, 0);

	hDC->setBackground (QColor (255, 255, 255)); // not upstream: bkcol on the painter

	// Default initial drawing settings
	hDC->setBackgroundMode (Qt::TransparentMode); // transparent text background
	hDC->setPen (Qt::NoPen); // NULL_PEN
	hDC->setBrush (Qt::NoBrush); // NULL_BRUSH
}

// ===============================================================================================
//
GDIPad::~GDIPad ()
{
	// make sure to deselect custom resources before destroying the DC
	if (hFont0) hDC->setFont (*hFont0), delete hFont0; // the painter held a copy of the original font
	hDC->setPen (Qt::NoPen);
	hDC->setBrush (Qt::NoBrush);
	if (hFontA) delete hFontA;
	LogOk("...GDI SketchPad Released for surface %s", _PTR(GetSurface()));
}

// ===============================================================================================
//
QPainter *GDIPad::GetDC()
{
	return hDC;
}

// ===============================================================================================
//
Font *GDIPad::SetFont (Font *font)
{
	Font *pfont = cfont;
	if (font) {
		QFont *hFont = new QFont (hDC->font()); // SelectObject returns the previous font; QPainter holds fonts by value
		hDC->setFont (*static_cast<D3D9PadFont*>(font)->hFont);
		if (!cfont) hFont0 = hFont;
		else delete hFont;
	} else if (hFont0) { // restore original font
		hDC->setFont (*hFont0);
		delete hFont0;
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
	if (cpen) hDC->setPen (*static_cast<D3D9PadPen*>(cpen)->hPen);
	else      hDC->setPen (Qt::NoPen); // NULL_PEN
	return ppen;
}

// ===============================================================================================
//
Brush *GDIPad::SetBrush (Brush *brush)
{
	Brush *pbrush = cbrush;
	cbrush = brush;
	if (brush) hDC->setBrush (*static_cast<D3D9PadBrush*>(cbrush)->hBrush);
	else hDC->setBrush (Qt::NoBrush); // NULL_BRUSH
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
	textalign = align; // ::SetTextAlign: kept for TextOut
}

// ===============================================================================================
//
DWORD GDIPad::SetTextColor (DWORD col)
{
	DWORD prev = textcol; // ::SetTextColor returns the previous colour; QPainter draws text with the pen, so it is kept apart
	textcol = (COLORREF)(col&0xFFFFFF);
	return prev;
}

// ===============================================================================================
//
DWORD GDIPad::SetBackgroundColor (DWORD col)
{
	DWORD prev = bkcol; // SetBkColor
	bkcol = (COLORREF)(col&0xFFFFFF);
	hDC->setBackground (QColor (GetRValue(bkcol), GetGValue(bkcol), GetBValue(bkcol)));
	return prev;
}

// ===============================================================================================
//
void GDIPad::SetBackgroundMode (BkgMode mode)
{
	Qt::BGMode bkmode;
	switch (mode) {
		case BK_TRANSPARENT: bkmode = Qt::TransparentMode; break;
		case BK_OPAQUE:      bkmode = Qt::OpaqueMode; break;
		default: return;
	}
	hDC->setBackgroundMode (bkmode); // SetBkMode
}

// ===============================================================================================
//
DWORD GDIPad::GetCharSize ()
{
	QFontMetrics tm = hDC->fontMetrics (); // GetTextMetrics
	int px = hDC->font().pixelSize();
	int tmInternalLeading = std::max (0, tm.height() - (px > 0 ? px : tm.height()));
	return MAKELONG(tm.height()-tmInternalLeading, tm.averageCharWidth());
}

// ===============================================================================================
//
DWORD GDIPad::GetTextWidth (const char *utf8, int len)
{
	if (utf8) if (utf8[0] == '_') if (strcmp(utf8, "_SkpVerInfo") == 0) return 1;
	SIZE size;
	if (!len) len = int(strlen(utf8));
	std::string str = UTF8ToCP1252(utf8, len);

	size.cx = hDC->fontMetrics().horizontalAdvance (QString::fromLatin1 (str.c_str(), str.length())); // GetTextExtentPoint32
	return (DWORD)size.cx;
}

// ===============================================================================================
//
void GDIPad::SetOrigin (int x, int y)
{
	hDC->setTransform (QTransform::fromTranslate (x, y)); // SetViewportOrgEx
}

// ===============================================================================================
//
void GDIPad::GetOrigin (int *x, int *y) const
{
	POINT point;
	QTransform t = hDC->transform (); // GetViewportOrgEx
	point.x = LONG(t.dx());
	point.y = LONG(t.dy());
	if (x) *x = point.x;
	if (y) *y = point.y;
}

// ===============================================================================================
//
bool GDIPad::Text (int x, int y, const char *utf8, int len)
{
	std::string str = UTF8ToCP1252(utf8, len);
	return (TextOut (x, y, QString::fromLatin1 (str.c_str(), str.length())) != FALSE);
}

// ===============================================================================================
//
bool GDIPad::TextW (int x, int y, const LPWSTR str, int len)
{
	return (TextOut (x, y, QString::fromWCharArray (str, len)) != FALSE); // TextOutW
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
	QRect br; // DrawText: text colour apart from the pen, clipped to the rectangle as without DT_NOCLIP
	QPen pen = hDC->pen ();
	hDC->setPen (QColor (GetRValue(textcol), GetGValue(textcol), GetBValue(textcol)));
	hDC->drawText (QRect (r.left, r.top, r.right-r.left, r.bottom-r.top), Qt::AlignLeft|Qt::AlignTop|Qt::TextWordWrap, QString::fromLatin1 (str.c_str(), str.length()), &br); // DT_LEFT|DT_NOPREFIX|DT_WORDBREAK
	hDC->setPen (pen);
	return (br.height() != 0);
}

// ===============================================================================================
//
void GDIPad::Pixel (int x, int y, DWORD col)
{
	QPen pen = hDC->pen (); // SetPixel
	hDC->setPen (QColor (GetRValue(col), GetGValue(col), GetBValue(col)));
	hDC->drawPoint (x, y);
	hDC->setPen (pen);
}

// ===============================================================================================
//
void GDIPad::MoveTo (int x, int y)
{
	cpos = QPoint (x, y); // MoveToEx
}

// ===============================================================================================
//
void GDIPad::LineTo (int x, int y)
{
	hDC->drawLine (cpos, QPoint (x, y)); // ::LineTo
	cpos = QPoint (x, y);
}

// ===============================================================================================
//
void GDIPad::Line (int x0, int y0, int x1, int y1)
{
	cpos = QPoint (x0, y0); // MoveToEx
	hDC->drawLine (cpos, QPoint (x1, y1)); // ::LineTo
	cpos = QPoint (x1, y1);
}

// ===============================================================================================
//
void GDIPad::Rectangle (int x0, int y0, int x1, int y1)
{
	hDC->drawRect (x0, y0, x1-x0-1, y1-y0-1); // ::Rectangle: right and bottom edges are exclusive
}

// ===============================================================================================
//
void GDIPad::Ellipse (int x0, int y0, int x1, int y1)
{
	hDC->drawEllipse (x0, y0, x1-x0-1, y1-y0-1); // ::Ellipse: right and bottom edges are exclusive
}

// ===============================================================================================
//
void GDIPad::Polygon (const IVECTOR2 *pt, int npt)
{
	std::vector<QPoint> p (npt); // (const POINT*)pt
	for (int i = 0; i < npt; i++) p[i] = QPoint (pt[i].x, pt[i].y);
	hDC->drawPolygon (p.data(), npt); // ::Polygon (ALTERNATE fill = Qt::OddEvenFill)
}

// ===============================================================================================
//
void GDIPad::Polyline (const IVECTOR2 *pt, int npt)
{
	std::vector<QPoint> p (npt); // (const POINT*)pt
	for (int i = 0; i < npt; i++) p[i] = QPoint (pt[i].x, pt[i].y);
	hDC->drawPolyline (p.data(), npt); // ::Polyline
}

// ===============================================================================================
//
void GDIPad::PolyPolygon (const IVECTOR2 *pt, const int *npt, const int nline)
{
	QPainterPath path; // ::PolyPolygon: one ALTERNATE fill over all polygons
	path.setFillRule (Qt::OddEvenFill);
	for (int j = 0; j < nline; pt += npt[j], j++) {
		QPolygon poly;
		for (int i = 0; i < npt[j]; i++) poly << QPoint (pt[i].x, pt[i].y);
		path.addPolygon (poly);
		path.closeSubpath ();
	}
	hDC->drawPath (path);
}

// ===============================================================================================
//
void GDIPad::PolyPolyline (const IVECTOR2 *pt, const int *npt, const int nline)
{
	for (int j = 0; j < nline; pt += npt[j], j++) Polyline (pt, npt[j]); // ::PolyPolyline
}

// ===============================================================================================
// not upstream: GDI TextOut on a QPainter (alignment point, text colour, background box and escapement kept by GDIPad)
bool GDIPad::TextOut (int x, int y, const QString &str)
{
	QFontMetrics fm = hDC->fontMetrics ();
	int w = fm.horizontalAdvance (str);
	int dx = 0, dy = fm.ascent(); // TA_TOP: GDI places the top edge at y, QPainter the baseline
	if ((textalign & TA_CENTER) == TA_CENTER) dx = -w/2;
	else if (textalign & TA_RIGHT) dx = -w;
	if ((textalign & TA_BASELINE) == TA_BASELINE) dy = 0;
	else if (textalign & TA_BOTTOM) dy = -fm.descent();
	float rot = (cfont ? static_cast<D3D9PadFont*>(cfont)->rotation : 0.0f); // the font's escapement: QFont has none
	hDC->save ();
	hDC->translate (x, y);
	if (rot) hDC->rotate (-rot);
	hDC->setPen (QColor (GetRValue(textcol), GetGValue(textcol), GetBValue(textcol)));
	hDC->drawText (dx, dy, str); // OpaqueMode fills the background box as OPAQUE does
	hDC->restore ();
	return true;
}

