// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// GDIClient.cpp
// Defines class GDIClient, acts as a base class for GDI-based
// graphics clients
// ==============================================================

#define OAPI_IMPLEMENTATION

#include "GDIClient.h"

using namespace oapi;

// ======================================================================
// class GDIClient
// ======================================================================

GDIClient::GDIClient (HINSTANCE hInstance): GraphicsClient (hInstance)
{
	refCountPen = 0;
	refCountBrush = 0;
	refCountFont = 0;
}

oapi::Sketchpad *GDIClient::clbkGetSketchpad (SURFHANDLE surf)
{
	HDC hDC = clbkGetSurfaceDC (surf);
	if (hDC) return new GDIPad (surf, hDC);
	else return NULL; // client does not support GDI
}

void GDIClient::clbkReleaseSketchpad (oapi::Sketchpad *sp)
{
	if (sp) {
		GDIPad *gdip = (GDIPad*)sp;
		clbkReleaseSurfaceDC (gdip->GetSurface(), gdip->GetDC());
		delete sp;
	}
}

Font *GDIClient::clbkCreateFont (int height, bool prop, const char *face, FontStyle style, int orientation) const
{
	refCountFont++;
	return new GDIFont (height, prop, face, style, orientation);
}

Font* GDIClient::clbkCreateFontEx(int height, char* face, int width, int weight, FontStyle style, float spacing) const
{
	refCountFont++;
	return new GDIFont(height, face, width, weight, style, spacing);
}


void GDIClient::clbkReleaseFont (Font *font) const
{
	refCountFont--;
	delete font;
}

Pen *GDIClient::clbkCreatePen (int style, int width, DWORD col) const
{
	refCountPen++;
	return new GDIPen (style, width, col);
}

void GDIClient::clbkReleasePen (Pen *pen) const
{
	refCountPen--;
	delete pen;
}

Brush *GDIClient::clbkCreateBrush (DWORD col) const
{
	refCountBrush++;
	return new GDIBrush (col);
}

void GDIClient::clbkReleaseBrush (Brush *brush) const
{
	refCountBrush--;
	delete brush;
}

bool GDIClient::clbkSaveSurfaceToImage (SURFHANDLE surf, const char *fname, ImageFileFormat fmt, float quality)
{
	HDC hdc = clbkGetSurfaceDC (surf);
	if (!hdc) return false;

	DWORD w, h;
	if (surf) clbkGetSurfaceSize (surf, &w, &h);
	else      clbkGetViewportSize (&w, &h);

	HDC hdcmem = CreateCompatibleDC (hdc);
	HBITMAP hbm = CreateCompatibleBitmap (hdc, w, h);
	SelectObject (hdcmem, hbm);
	BitBlt (hdcmem, 0, 0, w, h, hdc, 0, 0, SRCCOPY);

	if (fname == NULL) {
		// copy device-dependent bitmap to clipboard
		if (OpenClipboard (GetRenderWindow())) {
		    EmptyClipboard();
			SetClipboardData(CF_BITMAP,hbm);
			CloseClipboard(); 
		}
	} else {
		BITMAP bmp;
		BITMAPINFOHEADER bi;
		GetObject (hbm, sizeof(BITMAP), &bmp);

		// map to device-independent bitmap
		bi.biSize = sizeof(BITMAPINFOHEADER);    
		bi.biWidth = bmp.bmWidth;    
		bi.biHeight = -bmp.bmHeight;
		bi.biPlanes = 1;    
		bi.biBitCount = 24;    
		bi.biCompression = BI_RGB;    
		bi.biSizeImage = 0;  
		bi.biXPelsPerMeter = 0;
		bi.biYPelsPerMeter = 0;
		bi.biClrUsed = 0;    
		bi.biClrImportant = 0;

		oapi::ImageData imgdata;
		imgdata.width = (UINT)bmp.bmWidth;
		imgdata.height = (UINT)bmp.bmHeight;
		imgdata.bpp = bi.biBitCount;
		imgdata.stride = ((imgdata.width * imgdata.bpp + 31) & ~31) >> 3;
		imgdata.bufsize = imgdata.stride * imgdata.height;

		HANDLE hDIB = GlobalAlloc(GHND,imgdata.bufsize);
		imgdata.data = (BYTE*)GlobalLock(hDIB);

		int res = GetDIBits(hdc, hbm, 0, imgdata.height, imgdata.data, (BITMAPINFO*)&bi, DIB_RGB_COLORS);

		WriteImageDataToFile (imgdata, fname, fmt, quality);

		GlobalUnlock(hDIB);    
		GlobalFree(hDIB);
	}
    DeleteObject(hbm);
    DeleteObject(hdcmem);

	clbkReleaseSurfaceDC (surf, hdc);
	return true;
}

// ======================================================================
// class GDIPad
// ======================================================================

GDIPad::GDIPad (SURFHANDLE s, HDC hdc): Sketchpad (s)
{
	hDC = hdc;
	hFont0 = NULL;
	cfont = NULL;
	cpen = NULL;
	cbrush = NULL;

	// Default initial drawing settings
	SetBkMode (hDC, TRANSPARENT); // transparent text background
	SelectObject (hDC, GetStockObject (NULL_BRUSH)); // no fill
	SelectObject (hDC, GetStockObject (NULL_PEN));   // no outline
}

GDIPad::~GDIPad ()
{
	// make sure to deselect custom resources before destroying the DC
	if (hFont0) SelectObject (hDC, hFont0);
	if (cpen)   SelectObject (hDC, GetStockObject (NULL_PEN));
	if (cbrush) SelectObject (hDC, GetStockObject (NULL_BRUSH));
}

Font *GDIPad::SetFont (Font *font)
{
	Font *pfont = cfont;
	if (font) {
		HFONT hFont = (HFONT)SelectObject (hDC, ((GDIFont*)font)->hFont);
		if (!cfont) hFont0 = hFont;
	} else if (hFont0) { // restore original font
		SelectObject (hDC, hFont0);
		hFont0 = 0;
	}
	cfont = font;
	return pfont;
}

Pen *GDIPad::SetPen (Pen *pen)
{
	Pen *ppen = cpen;
	if (pen) {
		SelectObject (hDC, ((GDIPen*)pen)->hPen);
	} else { // disable outlines
		SelectObject (hDC, GetStockObject (NULL_PEN));
	}
	cpen = pen;
	return ppen;
}

Brush *GDIPad::SetBrush (Brush *brush)
{
	Brush *pbrush = cbrush;
	if (brush) {
		SelectObject (hDC, ((GDIBrush*)brush)->hBrush);
	} else { // disable filled brush
		SelectObject (hDC, GetStockObject (NULL_BRUSH));
	}
	cbrush = brush;
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
	return (DWORD)::SetTextColor (hDC, (COLORREF)col);
}

DWORD GDIPad::SetBackgroundColor (DWORD col)
{
	return (DWORD)SetBkColor (hDC, (COLORREF)col);
}

void GDIPad::SetBackgroundMode (BkgMode mode)
{
	int bkmode;
	switch (mode) {
		case BK_TRANSPARENT: bkmode = TRANSPARENT; break;
		case BK_OPAQUE:      bkmode = OPAQUE; break;
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

void GDIPad::GetOrigin (int *x, int *y) const
{
	POINT p;
	GetViewportOrgEx (hDC, &p);
	*x = p.x;
	*y = p.y;
}

bool GDIPad::Text (int x, int y, const char *str, int len)
{
	return (TextOut (hDC, x, y, str, len) != FALSE);
}

bool GDIPad::TextW (int x, int y, const LPWSTR str, int len)
{
	return (TextOutW (hDC, x, y, str, len) != FALSE);
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

// ======================================================================
// class GDIFont
// ======================================================================

GDIFont::GDIFont (int height, bool prop, const char *face, FontStyle style, int orientation): oapi::Font (height, prop, face, style, orientation)
{
	char *def_fixedface = "Courier New";
	char *def_sansface = "Arial";
	char *def_serifface = "Times New Roman";

	if (!_stricmp (face, "fixed")) {
		face = def_fixedface;
	} else if (!_stricmp (face, "sans")) {
		face = def_sansface;
	} else if (!_stricmp (face, "serif")) {
		face = def_serifface;
	} else if (_stricmp (face, def_fixedface) &&
			   _stricmp (face, def_sansface) &&
			   _stricmp (face, def_serifface)) {
		face = (prop ? def_sansface : def_fixedface);
	}
	int weight = (style & FONT_BOLD ? FW_BOLD : FW_NORMAL);
	DWORD italic = (style & FONT_ITALIC ? TRUE : FALSE);
	DWORD underline = (style & FONT_UNDERLINE ? TRUE : FALSE);
	DWORD strikeout = (style & FONT_STRIKEOUT) ? TRUE : FALSE;
	hFont = CreateFont (height, 0, orientation, orientation, weight, italic, underline, strikeout, 0, 3, 2, 1, 49, face);
}

GDIFont::GDIFont(int height, char* face, int width, int weight, FontStyle style, float spacing) : oapi::Font(height, false, face, style, 0)
{
	DWORD italic = (style & FontStyle::FONT_ITALIC ? TRUE : FALSE);
	DWORD underline = (style & FontStyle::FONT_UNDERLINE ? TRUE : FALSE);
	DWORD strikeout = (style & FontStyle::FONT_STRIKEOUT ? TRUE : FALSE);
	hFont = CreateFont(height, width, 0, 0, weight, italic, underline, strikeout, 0, 3, 2, 1, 49, face);
}

GDIFont::~GDIFont ()
{
	DeleteObject (hFont);
}


// ======================================================================
// class GDIPen
// ======================================================================

GDIPen::GDIPen (int style, int width, DWORD col): oapi::Pen (style, width, col)
{
	int pstyle;
	switch (style) {
		case 0:  pstyle = PS_NULL;  break;
		case 2:  pstyle = PS_DOT;   break;
		default: pstyle = PS_SOLID; break;
	}
	hPen = CreatePen (pstyle, width, (COLORREF)col);
}

GDIPen::~GDIPen ()
{
	DeleteObject (hPen);
}


// ======================================================================
// class GDIBrush
// ======================================================================

GDIBrush::GDIBrush (DWORD col): oapi::Brush (col)
{
	hBrush = CreateSolidBrush ((COLORREF)col);
}

GDIBrush::~GDIBrush ()
{
	DeleteObject (hBrush);
}
