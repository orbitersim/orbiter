#include "Orbiter.h"
#include "ScreenNote.h"
#include "Texture.h"
#include "OGraphics.h"

ScreenNote::ScreenNote (Orbiter *po, int ScreenW, int ScreenH)
{
	orbiter = po;
	sw = ScreenW;
	sh = ScreenH;
	SetPos (0.25, 0.1, 0.75, 0.9);
	txtlen = 0;
	txt[0] = txt[1023] = '\0';
	txtscale = 1.0;
	hf = sh/35;
	hFont = CreateFont (-hf, 0, 0, 0, 400, 0, 0, 0, 0, 3, 2, 1, 49, "Arial");
	txtcol = 0x3280ff;
}

ScreenNote::~ScreenNote ()
{
	DeleteObject (hFont);
}

void ScreenNote::SetNote (char *note)
{
	strncpy (txt, note, 1023);
	txtlen = strlen (txt);
}

void ScreenNote::ClearNote ()
{
	txtlen = 0;
}

void ScreenNote::SetColour (COLORREF col)
{
	txtcol = col;
	DWORD r = col & 8;
	DWORD g = (col >> 8) & 8;
	DWORD b = (col >> 16) & 8;
	txtcol2 = (r/2) | ((g/2) << 8) | ((b/2) << 16);
}

void ScreenNote::SetSize (double scale)
{
	if (scale != txtscale) {
		txtscale = scale;
		hf = (int)(sh*txtscale/35.0);
		DeleteObject (hFont);
		hFont = CreateFont (-hf, 0, 0, 0, 400, 0, 0, 0, 0, 3, 2, 1, 49, "Arial");
	}
}

void ScreenNote::SetPos (double x1, double y1, double x2, double y2)
{
	nx1 = (int)(x1*sw), nx2 = (int)(x2*sw), nw = nx2-nx1;
	ny1 = (int)(y1*sh), ny2 = (int)(y2*sh), nh = ny2-ny1;
}

void ScreenNote::Render ()
{
	if (!txtlen) return;

	double x = 0.25, y = 0.1, w = 0.5, h = 0.8;
	HDC hDC;
	HFONT pFont;
	if (SUCCEEDED (orbiter->GetInlineGraphicsClient()->GetRenderTarget()->GetDC (&hDC))) {
		pFont = (HFONT)SelectObject (hDC, hFont);
		SetTextColor (hDC, txtcol2);
		SetBkMode (hDC, TRANSPARENT);
		RECT r;
		r.left =   nx1;
		r.top =    ny1;
		r.right =  nx2;
		r.bottom = ny2;
		DrawText (hDC, txt, txtlen, &r, DT_LEFT|DT_NOPREFIX|DT_WORDBREAK);
		r.left--, r.top--, r.right--, r.bottom--;
		SetTextColor (hDC, txtcol);
		DrawText (hDC, txt, txtlen, &r, DT_LEFT|DT_NOPREFIX|DT_WORDBREAK);
		orbiter->GetInlineGraphicsClient()->GetRenderTarget()->ReleaseDC (hDC);
		SelectObject (hDC, pFont);
	}
}