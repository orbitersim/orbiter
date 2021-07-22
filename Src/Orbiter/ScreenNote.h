// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __SCREENNOTE_H
#define __SCREENNOTE_H

class Orbiter;

class ScreenNote {
public:
	ScreenNote (Orbiter *po, int ScreenW, int ScreenH);
	~ScreenNote();
	void SetNote (char *note);
	void ClearNote();
	void SetColour (COLORREF col);
	void SetSize (double scale);
	void SetPos (double x1, double y1, double x2, double y2);
	void Render();

private:
	Orbiter *orbiter;
	int sw, sh;
	int nw, nh, nx1, ny1, nx2, ny2, hf;
	int txtlen;
	double txtscale;
	char txt[1024];
	HFONT hFont;
	COLORREF txtcol, txtcol2;
};

#endif // !__SCREENNOTE_H