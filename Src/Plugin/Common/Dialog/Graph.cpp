// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: FlightData
//                  Part of the ORBITER SDK
//
// Graph.cpp
// Generic data graph class implementation.
// ==============================================================

#define STRICT
#include "Graph.h"
#include <stdio.h>
#include <math.h>
#include <strstream>
#include "orbitersdk.h"

using namespace std;

static COLORREF plotcol[MAXPLOT] = {0x0000ff, 0xff0000, 0x00ff00};

Graph::Graph (int _nplot): nplot(_nplot)
{
	data = new float*[nplot];
	for (int i = 0; i < nplot; i++)
		data[i] = new float[NDATA];
	ResetData();
	title = 0;
	xlabel = 0;
	ylabel = 0;
	legend = 0;
	legend_idx = 0;
}

Graph::~Graph()
{
	for (int i = 0; i < nplot; i++)
		delete []data[i];
	delete []data;

	if (title) delete []title;
	if (xlabel) delete []xlabel;
	if (ylabel) delete []ylabel;
	if (legend) delete []legend;
	if (legend_idx) delete[]legend_idx;
}

void Graph::InitGDI ()
{
	if (!gdi.nref) {
	 	gdi.font[0] = CreateFont (-10, 0, 0, 0, 400, 0, 0, 0, 0, 3, 2, 1, 49, "Arial");
		gdi.font[1] = CreateFont (-10, 0, 900, 900, 400, 0, 0, 0, 0, 3, 2, 1, 49, "Arial");
		gdi.pen[0]  = CreatePen (PS_SOLID, 0, 0x808080);
		gdi.pen[1]  = CreatePen (PS_SOLID, 0, 0xD0D0D0);
		for (int i = 0; i < MAXPLOT; i++)
			gdi.pen[2+i]  = CreatePen (PS_SOLID, 1, plotcol[i]);
	}
	gdi.nref++;
}

void Graph::FreeGDI ()
{
	gdi.nref--;
	if (!gdi.nref) {
		DWORD i;
		for (i = 0; i < 2; i++) DeleteObject (gdi.font[i]);
		for (i = 0; i < 2+MAXPLOT; i++) DeleteObject (gdi.pen[i]);
	}
}

void Graph::SetTitle (const char *_title)
{
	if (title) delete []title;
	title = new char[strlen (_title)+1];
	strcpy (title, _title);
}

void Graph::SetXLabel (const char *_label)
{
	if (xlabel) delete []xlabel;
	xlabel = new char[strlen (_label)+1];
	strcpy (xlabel, _label);
}

void Graph::SetYLabel (const char *_label)
{
	if (ylabel) delete []ylabel;
	ylabel = new char[strlen (_label)+1];
	strcpy (ylabel, _label);
}

void Graph::SetLegend (const char *_legend)
{
	if (legend) delete[]legend;
	legend = new char[strlen (_legend)+1];
	strcpy (legend, _legend);
	if (legend_idx) delete []legend_idx;
	legend_idx = new int[nplot];
	legend_idx[0] = 0;
	int i, k, n = strlen(_legend);
	for (i = 0, k = 1; i < n && k < nplot; i++) {
		if (_legend[i] == '&')
			legend[i] = '\0', legend_idx[k++] = i+1;
	}
}

void Graph::ResetData ()
{
	ndata = idx = 0;
	vmin = vmax = data_tickmin = 0.0;
	data_dtick = 1.0;
}

void Graph::AppendDataPoint (float val)
{
	data[0][idx] = val;
	idx = (idx+1)%NDATA;
	if (ndata < NDATA) ndata++;
	float vmn = vmin, vmx = vmax;
	SetAutoRange ();
	if (vmn != vmin || vmx != vmax) SetAutoTicks();
}

void Graph::AppendDataPoints (float *val)
{
	for (int p = 0; p < nplot; p++)
		data[p][idx] = val[p];
	idx = (idx+1)%NDATA;
	if (ndata < NDATA) ndata++;
	float vmn = vmin, vmx = vmax;
	SetAutoRange ();
	if (vmn != vmin || vmx != vmax) SetAutoTicks();
}

void Graph::SetAutoRange ()
{
	int p, i;

	vmin = (ndata ? data[0][0] : 0.0f);
	vmax = (ndata ? data[0][0] : 0.0f);

	for (p = 0; p < nplot; p++) {
		for (i = 0; i < ndata; i++) {
			if (data[p][i] < vmin) vmin = data[p][i];
			if (data[p][i] > vmax) vmax = data[p][i];
		}
	}

	if (vmax-vmin < 1e-6) vmin -= 0.5f, vmax += 0.5f;
}

void Graph::SetAutoTicks ()
{
	float rmin, rmax, dr, scale;
	int mintick = 1;

	rmin = vmin;
	rmax = vmax;
	dr = rmax-rmin;
	scale = 1.0f;
	while (dr <= 1.0f) dr *= 10.0f, scale *= 10.0f;
	while (dr > 10.0f) dr *= 0.1f, scale *= 0.1f;
	if (dr < 2.0) mintick = 10;
	else if (dr < 4.0) mintick = 5;
	else if (dr < 8.0) mintick = 2;
	data_tickscale = scale;
	data_dtick = 1.0f/scale;
	data_tickmin = (float)ceil(rmin*scale)/scale;
	data_minortick = mintick;
}

//#pragma optimize("g", off)

void Graph::Refresh (HDC hDC, int w, int h)
{
	int    x0 = w/10,   x1 = w-w/20, dx = x1-x0;
	int y, y0 = h-h/10, y1 = h/20,   dy = y0-y1;
	int i, p;
	char cbuf[256];

	HFONT pfont = (HFONT)SelectObject (hDC, gdi.font[0]);

	if (ndata >= 2) {
		double f;
		float ys = dy/(vmax-vmin);
		SelectObject (hDC, gdi.pen[0]);

		// draw grid lines and ordinate labels
		SetTextAlign (hDC, TA_RIGHT);
		for (f = data_tickmin; f <= vmax; f += data_dtick) {
			y = y0 - (int)((f-vmin)*ys+0.5);
			MoveToEx (hDC, x0, y, 0); LineTo (hDC, x1, y);
			sprintf (cbuf, "%0.0f", f*data_tickscale);
			TextOut (hDC, x0, y-5, cbuf, strlen(cbuf));
		}
		if (data_minortick > 1) {
			SelectObject (hDC, gdi.pen[1]);
			for (f = data_tickmin, i = 0; f > vmin; f -= data_dtick/data_minortick) {
				if (i++ % data_minortick) {
					y = y0 - (int)((f-vmin)*ys+0.5);
					MoveToEx (hDC, x0, y, 0); LineTo (hDC, x1, y);
				}
			}
			for (f = data_tickmin, i = 0; f < vmax; f += data_dtick/data_minortick) {
				if (i++ % data_minortick) {
					y = y0 - (int)((f-vmin)*ys+0.5);
					MoveToEx (hDC, x0, y, 0); LineTo (hDC, x1, y);
				}
			}
		}
		// draw data
		for (p = 0; p < nplot; p++) {
			SelectObject (hDC, gdi.pen[(p%MAXPLOT)+2]);
			int j, i = idx-1; if (i < 0) i += NDATA;
			MoveToEx (hDC, x1, y0 - (int)((data[p][i]-vmin)*ys+0.5), NULL);
			for (j = 1; j < ndata; j++) {
				i = idx-j-1; if (i < 0) i += NDATA;
				LineTo (hDC, x1 - (dx*j)/NDATA, y0 - (int)((data[p][i]-vmin)*ys+0.5));
			}
		}
	}

	// Draw axes
	SelectObject (hDC, GetStockObject (BLACK_PEN));
	MoveToEx (hDC, x0, y1, NULL);
	LineTo (hDC, x0, y0); LineTo (hDC, x1, y0);

	if (legend) {
		SetTextAlign (hDC, TA_LEFT);
		for (p = 0; p < nplot; p++) {
			SetTextColor (hDC, plotcol[p%MAXPLOT]);
			TextOut (hDC, x0+5, y1+p*10, legend+legend_idx[p], strlen(legend+legend_idx[p]));
		}
		SetTextColor (hDC, 0x000000);
	}
	SetTextAlign (hDC, TA_CENTER);
	if (title) {
		TextOut (hDC, w/2, 0, title, strlen(title));
	}
	
	SelectObject (hDC, gdi.font[1]);
	ostrstream oss(cbuf,64);
	if (ylabel) {
		oss << ylabel;
		if (data_tickscale && data_tickscale != 1.0f) oss << " x " << 1.0/data_tickscale;
		oss << '\0';
		TextOut (hDC, 0, (y0+y1)/2, oss.str(), strlen(oss.str()));
	}

	SelectObject (hDC, pfont);
}

GDIres Graph::gdi = {0,0,0};