// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define OAPI_IMPLEMENTATION

#include "DrawAPI.h"

using namespace oapi;

// ======================================================================
// ======================================================================
// class Sketchpad

Sketchpad::Sketchpad (SURFHANDLE s)
{
	surf = s;
}

// ==================================================================

Sketchpad::~Sketchpad ()
{
}

// ==================================================================

void Sketchpad::Rectangle (int x0, int y0, int x1, int y1)
{
	MoveTo (x0, y0);
	LineTo (x1, y0);
	LineTo (x1, y1);
	LineTo (x0, y1);
	LineTo (x0, y0);
}

// ==================================================================

void Sketchpad::PolyPolyline (const oapi::IVECTOR2 *pt, const int *npt, const int nline)
{
	int i, ofs = 0;
	for (i = 0; i < nline; i++) {
		Polyline (pt+ofs, npt[i]);
		ofs += npt[i];
	}
}

// ==================================================================

void Sketchpad::PolyPolygon (const oapi::IVECTOR2 *pt, const int *npt, const int nline)
{
	int i, ofs = 0;
	for (i = 0; i < nline; i++) {
		Polygon (pt+ofs, npt[i]);
		ofs += npt[i];
	}
}

// ==================================================================

bool Sketchpad::TextBox (int x1, int y1, int x2, int y2, const char *str, int len)
{
	static int buflen = 1024;
	static char *buf = new char[buflen];
	
	if (len >= buflen) {
		delete []buf;
		buf = new char[buflen = len+1];
	}
	strncpy (buf, str, len);  buf[len] = '\0';

	int w0 = x2-x1, w;

	DWORD h = GetCharSize() & 0xFFFF;

	const char *p0 = str, *p, *pp;
	for (p0 = str, pp = str, p = str; *p0; *p && p < str+len) {
		if (*p == '\0' || *p == '\n' || *p == '\r' || *p == ' ' || *p == '-') {
			w = (p > p0 ? GetTextWidth (p0, p-p0) : 0);
			if (w <= w0 && *p != '\0' && *p != '\n' && *p != '\r') {
				pp = p;
				p++;
			} else {
				if ((*p == '\0' || *p == '\r' || *p == '\n') && w <= w0)
					pp = p;
				if (pp == p0) pp = p;
				if (pp > p0) Text (x1, y1, p0, pp-p0);
				p0 = pp;
				if (*p0 == '\r') p0++;
				if (*p0 == '\n') p0++;
				pp = p0;
				p = p0;
				y1 += h;
			}
		} else p++;
	}
	return true;
}