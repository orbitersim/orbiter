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

static COLORREF plotcol[MAXPLOT] = { 0x0000ff, 0xff0000, 0x00ff00 };

Graph::Graph (int nplot)
{
	m_data.resize(nplot);
	for (int i = 0; i < nplot; i++)
		m_data[i].resize(NDATA);
	ResetData();
}

Graph::Graph(const Graph& graph)
{
	m_data = graph.m_data;
	m_vmin = graph.m_vmin;
	m_vmax = graph.m_vmax;
	m_idx = graph.m_idx;
	m_ndata = graph.m_ndata;
	m_tickscale = graph.m_tickscale;
	m_dtick = graph.m_dtick;
	m_tickmin = graph.m_tickmin;
	m_minortick = graph.m_minortick;
	m_title = graph.m_title;
	m_xlabel = graph.m_xlabel;
	m_ylabel = graph.m_ylabel;
	m_legend = graph.m_legend;
}

Graph::~Graph()
{
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

void Graph::SetTitle (const std::string &title)
{
	m_title = title;
}

void Graph::SetXLabel (const std::string &label)
{
	m_xlabel = label;
}

void Graph::SetYLabel (const std::string &label)
{
	m_ylabel = label;
}

void Graph::SetLegend (const std::string &legend)
{
	// individual legend entries separated by '&'
	m_legend.clear();
	size_t p0 = 0, p1;
	do {
		p1 = legend.find('&', p0);
		if (p1 == std::string::npos)
			p1 = legend.size();
		m_legend.push_back(legend.substr(p0, p1 - p0));
		p0 = p1 + 1;
	} while (p1 < legend.size());
}

void Graph::ResetData ()
{
	m_ndata = m_idx = 0;
	m_vmin = m_vmax = m_tickmin = 0.0;
	m_dtick = 1.0;
}

void Graph::AppendDataPoint (float val)
{
	m_data[0][m_idx] = val;
	m_idx = (m_idx + 1) % NDATA;
	if (m_ndata < NDATA) m_ndata++;
	float vmn = m_vmin, vmx = m_vmax;
	SetAutoRange ();
	if (vmn != m_vmin || vmx != m_vmax) SetAutoTicks();
}

void Graph::AppendDataPoints (float *val)
{
	for (int p = 0; p < m_data.size(); p++)
		m_data[p][m_idx] = val[p];
	m_idx = (m_idx + 1) % NDATA;
	if (m_ndata < NDATA) m_ndata++;
	float vmn = m_vmin, vmx = m_vmax;
	SetAutoRange ();
	if (vmn != m_vmin || vmx != m_vmax) SetAutoTicks();
}

void Graph::SetAutoRange ()
{
	int p, i;

	m_vmin = (m_ndata ? m_data[0][0] : 0.0f);
	m_vmax = (m_ndata ? m_data[0][0] : 0.0f);

	for (p = 0; p < m_data.size(); p++) {
		for (i = 0; i < m_ndata; i++) {
			if (m_data[p][i] < m_vmin) m_vmin = m_data[p][i];
			if (m_data[p][i] > m_vmax) m_vmax = m_data[p][i];
		}
	}

	if (m_vmax - m_vmin < 1e-6) m_vmin -= 0.5f, m_vmax += 0.5f;
}

void Graph::SetAutoTicks ()
{
	float rmin, rmax, dr, scale;
	int mintick = 1;

	rmin = m_vmin;
	rmax = m_vmax;
	dr = rmax-rmin;
	scale = 1.0f;
	while (dr <= 1.0f) dr *= 10.0f, scale *= 10.0f;
	while (dr > 10.0f) dr *= 0.1f, scale *= 0.1f;
	if (dr < 2.0) mintick = 10;
	else if (dr < 4.0) mintick = 5;
	else if (dr < 8.0) mintick = 2;
	m_tickscale = scale;
	m_dtick = 1.0f/scale;
	m_tickmin = (float)ceil(rmin*scale)/scale;
	m_minortick = mintick;
}

void Graph::Refresh (HDC hDC, int w, int h)
{
	int    x0 = w/10,   x1 = w-w/20, dx = x1-x0;
	int y, y0 = h-h/10, y1 = h/20,   dy = y0-y1;
	int i, p;
	char cbuf[256];

	HFONT pfont = (HFONT)SelectObject (hDC, gdi.font[0]);

	if (m_ndata >= 2) {
		double f;
		float ys = dy / (m_vmax - m_vmin);
		SelectObject (hDC, gdi.pen[0]);

		// draw grid lines and ordinate labels
		SetTextAlign (hDC, TA_RIGHT);
		for (f = m_tickmin; f <= m_vmax; f += m_dtick) {
			y = y0 - (int)((f - m_vmin) * ys + 0.5);
			MoveToEx (hDC, x0, y, 0); LineTo (hDC, x1, y);
			sprintf (cbuf, "%0.0f", f * m_tickscale);
			TextOut (hDC, x0, y-5, cbuf, strlen(cbuf));
		}
		if (m_minortick > 1) {
			SelectObject (hDC, gdi.pen[1]);
			for (f = m_tickmin, i = 0; f > m_vmin; f -= m_dtick/m_minortick) {
				if (i++ % m_minortick) {
					y = y0 - (int)((f - m_vmin) * ys + 0.5);
					MoveToEx (hDC, x0, y, 0); LineTo (hDC, x1, y);
				}
			}
			for (f = m_tickmin, i = 0; f < m_vmax; f += m_dtick/m_minortick) {
				if (i++ % m_minortick) {
					y = y0 - (int)((f - m_vmin) * ys + 0.5);
					MoveToEx (hDC, x0, y, 0); LineTo (hDC, x1, y);
				}
			}
		}
		// draw data
		for (p = 0; p < m_data.size(); p++) {
			SelectObject (hDC, gdi.pen[(p%MAXPLOT)+2]);
			int j, i = m_idx - 1; if (i < 0) i += NDATA;
			MoveToEx (hDC, x1, y0 - (int)((m_data[p][i] - m_vmin) * ys + 0.5), NULL);
			for (j = 1; j < m_ndata; j++) {
				i = m_idx - j - 1; if (i < 0) i += NDATA;
				LineTo (hDC, x1 - (dx*j)/NDATA, y0 - (int)((m_data[p][i] - m_vmin) * ys + 0.5));
			}
		}
	}

	// Draw axes
	SelectObject (hDC, GetStockObject (BLACK_PEN));
	MoveToEx (hDC, x0, y1, NULL);
	LineTo (hDC, x0, y0); LineTo (hDC, x1, y0);

	if (m_legend.size()) {
		SetTextAlign (hDC, TA_LEFT);
		for (p = 0; p < m_legend.size(); p++) {
			SetTextColor (hDC, plotcol[p%MAXPLOT]);
			TextOut (hDC, x0+5, y1+p*10, m_legend[p].c_str(), m_legend[p].size());
		}
		SetTextColor (hDC, 0x000000);
	}
	SetTextAlign (hDC, TA_CENTER);
	if (m_title.size()) {
		TextOut (hDC, w/2, 0, m_title.c_str(), m_title.size());
	}
	
	SelectObject (hDC, gdi.font[1]);
	std::ostrstream oss(cbuf,64);
	if (m_ylabel.size()) {
		oss << m_ylabel;
		if (m_tickscale && m_tickscale != 1.0f) oss << " x " << 1.0/m_tickscale;
		oss << '\0';
		TextOut (hDC, 0, (y0+y1)/2, oss.str(), strlen(oss.str()));
	}

	SelectObject (hDC, pfont);
}

GDIres Graph::gdi = {0,0,0};