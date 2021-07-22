// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define OAPI_IMPLEMENTATION

#include "Orbiter.h"
#include "MfdUser.h"
#include "Config.h"
#include "Astro.h"
#include "Select.h"
#include "Planet.h"
#include "Base.h"
#include "Psys.h"
#include "Nav.h"
#include <stdio.h>
#include <strstream>
#include <iomanip>
#include "Log.h"
#include "Util.h"
#include <windows.h>

using namespace std;

extern InputBox *g_input;
extern Select *g_select;
extern PlanetarySystem *g_psys;
extern Instrument *g_instr_passkey;
extern Orbiter *g_pOrbiter;
extern TimeData td;
extern char Key2Char[256];
extern char DBG_MSG[256];

// =======================================================================
// class Instrument_User
// user-defined MFD mode (implemented in module)

Instrument_User::Instrument_User (Pane *_pane, int _id, const Spec &spec, Vessel *_vessel,
	int _type, const MFDMODE &mode)
: Instrument (_pane, _id, spec, _vessel, true)
{
	type = _type;
	msgproc = mode.spec->msgproc;
	name = mode.spec->name;
	selkey = Key2Char[mode.spec->key];
	MFDMODEOPENSPEC ospec = {IW, IH, mode.spec};
	mfd = (MFD*)msgproc (OAPI_MSG_MFD_OPENEDEX, _id, (WPARAM)&ospec, (LPARAM)vessel->GetModuleInterface());
	if (!mfd)
		mfd = (MFD*)msgproc (OAPI_MSG_MFD_OPENED, _id, MAKEWPARAM(IW, IH), (LPARAM)vessel->GetModuleInterface());
	try {
		mfd2 = dynamic_cast<MFD2*>(mfd); // will return 0 if not an MFD2 instance
	}
	catch(...) {
		mfd2 = 0;
	}
	mfd->instr = this;
	mfd->cw = cw;
	mfd->ch = ch;
	use_skp_interface = (mfd2 != 0);
	AllocSurface (IW, IH);
	mfd->RecallStatus();
}

Instrument_User::Instrument_User (Pane *_pane, int _id, const Spec &spec, Vessel *_vessel)
: Instrument (_pane, _id, spec, _vessel, true)
{
	type    = MFD_USERTYPE; // generic
	msgproc = 0;
	name    = 0;
	selkey  = '.';
	mfd = mfd2 = 0;
}

Instrument_User::~Instrument_User ()
{
	if (mfd) mfd->StoreStatus();
	if (msgproc) msgproc (OAPI_MSG_MFD_CLOSED, id, 0, 0);
	if (mfd) delete mfd;
}

void Instrument_User::UpdateDraw (oapi::Sketchpad *skp)
{
	if (mfd2) {
		mfd2->Update (skp);
	} else if (mfd) {
		HDC hDC = skp->GetDC ();
		if (hDC) mfd->Update (hDC); // this should directly use skp
	}
}

void Instrument_User::UpdateDraw (HDC hDC)
{
	if (hDC && mfd) mfd->Update (hDC);
}

bool Instrument_User::ReadParams (ifstream &ifs)
{
	char cbuf[256], *pc, *modestr;
	MFDMODESPECEX *spec;
	if (!ifs.getline (cbuf, 256)) return false;
	pc = trim_string (cbuf);
	if (_strnicmp (pc, "MODE", 4)) return false;
	modestr = trim_string (pc+4);
	int tp = ModeFromName (modestr, &spec);
	if (tp <= BUILTIN_MFD_MODES)
		tp = VesselModeFromName (modestr, &spec);
	if (tp <= BUILTIN_MFD_MODES) return false;
	if (mfd) delete mfd;
	type = tp;
	msgproc = spec->msgproc;
	name = spec->name;
	selkey = Key2Char[spec->key];
	MFDMODEOPENSPEC ospec = {IW, IH, spec};
	mfd = (MFD*)msgproc (OAPI_MSG_MFD_OPENEDEX, id, (WPARAM)&ospec, (LPARAM)vessel->GetModuleInterface());
	if (!mfd)
		mfd = (MFD*)msgproc (OAPI_MSG_MFD_OPENED, id, MAKEWPARAM(IW, IH), (LPARAM)vessel->GetModuleInterface());
	try {
		mfd2 = dynamic_cast<MFD2*>(mfd); // will return 0 if not an MFD2 instance
	}
	catch(...) {
		mfd2 = 0;
	}
	mfd->instr = this;
	mfd->cw = cw;
	mfd->ch = ch;
	mfd->ReadStatus ((FILEHANDLE)&ifs);
	use_skp_interface = (mfd2 != 0);
	AllocSurface (IW, IH);
	return true;
}

void Instrument_User::WriteParams (ostream &ofs) const
{
	ofs << "  TYPE User" << endl;
	if (name) ofs << "  MODE " << name << endl;
	if (mfd) {
		mfd->WriteStatus ((FILEHANDLE)&ofs);
	}
}

// =======================================================================
// =======================================================================

// =======================================================================
// Impementation of module interface class MFD
// This class is used for defining user MFD modes
// See MFDAPI.h for interface
// =======================================================================

MFD::MFD (DWORD w, DWORD h, VESSEL *vessel)
{
	instr = 0;
	W = w, H = h;
	pV = vessel;
}

MFD::~MFD ()
{}

void MFD::Title (HDC hDC, const char *title) const
{
	// GDI legacy code
	instr->DisplayTitle (hDC, title);
}

void MFD::InvalidateDisplay ()
{
	instr->Refresh();
}

void MFD::InvalidateButtons ()
{
	instr->RepaintButtons ();
}

HPEN MFD::SelectDefaultPen (HDC hDC, DWORD i) const
{
	// GDI legacy code
	return instr->SelectDefaultPen (hDC, i);
}

HFONT MFD::SelectDefaultFont (HDC hDC, DWORD i) const
{
	// GDI legacy code
	return instr->SelectDefaultFont (hDC, i);
}


// =======================================================================
// Impementation of module interface class MFD
// This class is used for defining user GDI-independent MFD modes
// See MFDAPI.h for interface
// =======================================================================

bool MFD2::Update (oapi::Sketchpad *skp)
{
	return false;
}

void MFD2::Title (oapi::Sketchpad *skp, const char *title) const
{
	instr->DisplayTitle (skp, title);
}

oapi::Pen *MFD2::GetDefaultPen (DWORD colidx, DWORD intens, DWORD style) const
{
	return instr->GetDefaultPen (colidx, intens, style);
}

oapi::Font *MFD2::GetDefaultFont (DWORD fontidx) const
{
	return instr->GetDefaultFont (fontidx);
}

DWORD MFD2::GetDefaultColour (DWORD colidx, DWORD intens) const
{
	return instr->GetDefaultColour (colidx, intens);
}


// =======================================================================
// Impementation of module interface class GraphMFD
// This class is used for defining MFD modes containing 2D graphs
// See orbitersdk.h for interface
// =======================================================================

GraphMFD::GraphMFD (DWORD w, DWORD h, VESSEL *vessel)
: MFD (w, h, vessel)
{
	ngraph  = 0;
}

GraphMFD::~GraphMFD ()
{
	if (ngraph) {
		for (int g = 0; g < ngraph; g++) {
			if (graph[g].nplot)
				delete []graph[g].plot;
		}
		delete []graph;
	}
}

int GraphMFD::AddGraph (void)
{
	GRAPH *tmp = new GRAPH[ngraph+1]; TRACENEW
	if (ngraph) {
		memcpy (tmp, graph, ngraph*sizeof(GRAPH));
		delete []graph;
	}
	graph = tmp;
	graph[ngraph].nplot = 0;
	graph[ngraph].absc_title[0] = graph[ngraph].absc_title[63] = '\0';
	graph[ngraph].data_title[0] = graph[ngraph].data_title[63] = '\0';
	SetRange (ngraph, 0, -1.0, 1.0); // arbitrary default
	SetRange (ngraph, 1, -1.0, 1.0); // arbitrary default
	return ngraph++;
}

void GraphMFD::AddPlot (int g, float *absc, float *data, int ndata, int col, int *ofs)
{
	int np = graph[g].nplot;
	GRAPH::PLOT *tmp_plot = new GRAPH::PLOT[np+1]; TRACENEW
	if (np) {
		memcpy (tmp_plot, graph[g].plot, np*sizeof(GRAPH::PLOT));
		delete []graph[g].plot;
	}
	tmp_plot[np].absc  = absc;
	tmp_plot[np].data  = data;
	tmp_plot[np].ndata = ndata;
	tmp_plot[np].col   = col;
	tmp_plot[np].ofs   = ofs;
	graph[g].plot      = tmp_plot;
	graph[g].nplot++;
}

void GraphMFD::SetRange (int g, int axis, float rmin, float rmax)
{
	switch (axis) {
	case 0:
		graph[g].absc_min = rmin;
		graph[g].absc_max = rmax;
		break;
	case 1:
		graph[g].data_min = rmin;
		graph[g].data_max = rmax;
		break;
	}
	SetAutoTicks (g, axis);
}

void GraphMFD::SetAutoRange (int g, int axis, int p)
{
	float rmin, rmax, r1, r2;
	int i;
	int p0 = (p >= 0 ? p : 0);
	int p1 = (p >= 0 ? p+1 : graph[g].nplot);

	switch (axis) {
	case 0:
		for (i = p0; i < p1; i++) {
			FindRange (graph[g].plot[i].absc, graph[g].plot[i].ndata, r1, r2);
			if (i == p0 || r1 < rmin) rmin = r1;
			if (i == p0 || r2 > rmax) rmax = r2;
		}
		if (rmin == rmax) rmin -= 0.5f, rmax += 0.5f;
		SetRange (g, axis, rmin, rmax);
		break;
	case 1:
		for (i = p0; i < p1; i++) {
			FindRange (graph[g].plot[i].data, graph[g].plot[i].ndata, r1, r2);
			if (i == p0 || r1 < rmin) rmin = r1;
			if (i == p0 || r2 > rmax) rmax = r2;
		}
		if (rmin == rmax) rmin -= 0.5f, rmax += 0.5f;
		SetRange (g, axis, rmin, rmax);
		break;
	}
}

void GraphMFD::SetAutoTicks (int g, int axis)
{
	GRAPH &gf = graph[g];
	float rmin, rmax, dr, scale;
	int mintick = 1;

	rmin = (axis ? gf.data_min : gf.absc_min);
	rmax = (axis ? gf.data_max : gf.absc_max);
	dr = rmax-rmin;
	scale = 1.0f;
	while (dr <= 1.0f) dr *= 10.0f, scale *= 10.0f;
	while (dr > 10.0f) dr *= 0.1f, scale *= 0.1f;
	if (dr < 2.0) mintick = 10;
	else if (dr < 4.0) mintick = 5;
	else if (dr < 8.0) mintick = 2;
	if (axis) {
		gf.data_tickscale = scale;
		gf.data_dtick = 1.0f/scale;
		gf.data_tickmin = (float)ceil(rmin*scale)/scale;
		gf.data_minortick = mintick;
	} else {
		gf.absc_tickscale = scale;
		gf.absc_dtick = 1.0f/scale;
		gf.absc_tickmin = (float)ceil(rmin*scale)/scale;
		gf.absc_minortick = mintick;
	}
}

void GraphMFD::SetAxisTitle (int g, int axis, char *title)
{
	if (axis)
		strncpy (graph[g].data_title, title, 63);
	else
		strncpy (graph[g].absc_title, title, 63);
}

void GraphMFD::Plot (HDC hDC, int g, int h0, int h1, const char *title)
{
	GRAPH &gf = graph[g];
	char cbuf[64];
	float minx, maxx, miny, maxy, ixrange, iyrange, f;
	int i, j, pl, x0, y0, x1, y1, x, y;
	minx = gf.absc_min; maxx = gf.absc_max;
	miny = gf.data_min; maxy = gf.data_max;
	x0 = cw*3, x1 = W-cw;
	y0 = h1-(3*ch)/2, y1 = h0+ch/2;
	ixrange = (x1-x0)/(maxx-minx);
	iyrange = (y0-y1)/(maxy-miny);

	SelectDefaultFont (hDC, 1);
	SetTextColor (hDC, 0x00A000);

	// abscissa ticks/labels
	SelectDefaultPen (hDC, 3);
	SetTextAlign (hDC, TA_CENTER);
	for (f = gf.absc_tickmin; f <= gf.absc_max; f += gf.absc_dtick) {
		x = x0 + (int)((f-minx)*ixrange+0.5);
		MoveToEx (hDC, x, y0, 0);
		LineTo (hDC, x, y1);
		sprintf (cbuf, "%0.0f", f*gf.absc_tickscale);
		TextOut (hDC, x, y0, cbuf, strlen(cbuf));
	}
	if (gf.absc_minortick > 1) {
		SelectDefaultPen (hDC, 4);
		for (f = gf.absc_tickmin, i = 0; f > gf.absc_min; f -= gf.absc_dtick/(float)gf.absc_minortick, i++) {
			if (!(i%gf.absc_minortick)) continue;
			x = x0 + (int)((f-minx)*ixrange+0.5);
			MoveToEx (hDC, x, y0, 0); LineTo (hDC, x, y1);
		}
		for (f = gf.absc_tickmin, i = 0; f < gf.absc_max; f += gf.absc_dtick/(float)gf.absc_minortick, i++) {
			if (!(i%gf.absc_minortick)) continue;
			x = x0 + (int)((f-minx)*ixrange+0.5);
			MoveToEx (hDC, x, y0, 0); LineTo (hDC, x, y1);
		}
	}
	if (gf.absc_title[0]) {
		ostrstream oss(cbuf, 64);
		oss << gf.absc_title;
		if (gf.absc_tickscale != 1.0f) oss << " x " << 1.0/gf.absc_tickscale;
		oss << '\0';
		TextOut (hDC, (x0+x1)/2, y0+(3*ch)/4, oss.str(), strlen(oss.str()));
	}

	// ordinate ticks/labels
	SelectDefaultPen (hDC, 3);
	SetTextAlign (hDC, TA_RIGHT);
	for (f = gf.data_tickmin; f <= gf.data_max; f += gf.data_dtick) {
		y = y0 - (int)((f-miny)*iyrange+0.5);
		MoveToEx (hDC, x0, y, 0);
		LineTo (hDC, x1, y);
		sprintf (cbuf, "%0.0f", f*gf.data_tickscale);
		TextOut (hDC, x0, y-ch/2, cbuf, strlen(cbuf));
	}
	if (gf.data_minortick > 1) {
		SelectDefaultPen (hDC, 4);
		for (f = gf.data_tickmin, i = 0; f > gf.data_min; f -= gf.data_dtick/(float)gf.data_minortick, i++) {
			if (!(i%gf.data_minortick)) continue;
			y = y0 - (int)((f-miny)*iyrange+0.5);
			MoveToEx (hDC, x0, y, 0); LineTo (hDC, x1, y);
		}
		for (f = gf.data_tickmin, i = 0; f < gf.data_max; f += gf.data_dtick/(float)gf.data_minortick, i++) {
			if (!(i%gf.data_minortick)) continue;
			y = y0 - (int)((f-miny)*iyrange+0.5);
			MoveToEx (hDC, x0, y, 0); LineTo (hDC, x1, y);
		}
	}
	SetTextAlign (hDC, TA_CENTER);
	if (gf.data_title[0]) {
		SelectDefaultFont (hDC, 2);
		ostrstream oss(cbuf, 64);
		oss << gf.data_title;
		if (gf.data_tickscale != 1.0f) oss << " x " << 1.0/gf.data_tickscale;
		oss << '\0';
		TextOut (hDC, 0, (y0+y1)/2, oss.str(), strlen(oss.str()));
	}

	// plot frame
	SelectDefaultPen (hDC, 2);
	Rectangle (hDC, x0, y1, x1+1, y0+1);

	// plot line
	for (pl = 0; pl < gf.nplot; pl++) {
		GRAPH::PLOT &p = gf.plot[pl];
		SelectDefaultPen (hDC, p.col);
		float fx, fy, pfx=0, pfy=0, xa, ya, xb, yb;
		bool clip, vis;
		int sample = (p.ofs ? *p.ofs : 0);
		for (i = 0; i < p.ndata; i++) {
			j = (i+sample) % p.ndata;
			fx = xb = p.absc[j];	xa = pfx;
			fy = yb = p.data[j];	ya = pfy;

			if (!i) {
				clip = true;
			} else {
				if (clip) { // clip first point
					vis = true;
					if (ya < miny) {        // clip at lower edge
						if (yb < miny) vis = false;
						else xa += (miny-ya)/(yb-ya)*(xb-xa), ya = miny;
					} else if (ya > maxy) { // clip at upper edge
						if (yb > maxy) vis = false;
						else xa += (maxy-ya)/(yb-ya)*(xb-xa), ya = maxy;
					}
					if (xa < minx) {        // clip at left edge
						if (xb < minx) vis = false;
						else ya += (minx-xa)/(xb-xa)*(yb-ya), xa = minx;
					} else if (xa > maxx) { // clip at right edge
						if (xb > maxx) vis = false;
						else ya += (maxx-xa)/(xb-xa)*(yb-ya), xa = maxx;
					}
					if (vis) MoveToEx (hDC, x0 + (int)((xa-minx)*ixrange), y0 - (int)((ya-miny)*iyrange), 0);
				} else vis = true;

				if (vis) { // clip second point
					clip = false;
					if (yb < miny) {        // clip at lower edge
						if (ya < miny) vis = false;
						else xb += (miny-yb)/(ya-yb)*(xa-xb), yb = miny;
						clip = true;
					} else if (yb > maxy) { // clip at upper edge
						if (ya > maxy) vis = false;
						else xb += (maxy-yb)/(ya-yb)*(xa-xb), yb = maxy;
						clip = true;
					}
					if (xb < minx) {        // clip at left edge
						if (xa < minx) vis = false;
						else yb += (minx-xb)/(xa-xb)*(ya-yb), xb = minx;
						clip = true;
					} else if (xb > maxx) { // clip at right edge
						if (xa > maxx) vis = false;
						else yb += (maxx-xb)/(xa-xb)*(ya-yb), xb = maxx;
						clip = true;
					}
					if (vis) LineTo (hDC, x0 + (int)((xb-minx)*ixrange), y0 - (int)((yb-miny)*iyrange));
				} else clip = true;
			}
			pfx = fx, pfy = fy;
		}
	}

	if (title) {
		SelectDefaultFont (hDC, 1);
		SetTextColor (hDC, 0x00FF00);
		TextOut (hDC, (x0+x1)/2, y1, title, strlen(title));
	}
}

void GraphMFD::FindRange (float *d, int ndata, float &dmin, float &dmax) const
{
	dmin = dmax = d[0];
	for (int i = 1; i < ndata; i++) {
		if      (d[i] < dmin) dmin = d[i];
		else if (d[i] > dmax) dmax = d[i];
	}
}

