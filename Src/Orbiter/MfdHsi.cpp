// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "MfdHsi.h"
#include "Nav.h"
#include "Base.h"
#include "Astro.h"
#include "Planet.h"
#include "Config.h"
#include <stdio.h>
#include <dinput.h>

using namespace std;

// =======================================================================
// class Instrument_HSI

struct Instrument_HSI::SavePrm Instrument_HSI::saveprm = {0, {{0,0}, {0,0}}};

Instrument_HSI::Instrument_HSI (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel)
: Instrument (_pane, _id, spec, _vessel)
{
	int i;

	focus = 0;
	for (i = 0; i < 2; i++) {
		if (_vessel == saveprm.usr) {
			hsi[i].nv = saveprm.hsi[i].nv;
			hsi[i].obs = saveprm.hsi[i].obs;
		} else {
			hsi[i].nv = 0;
			hsi[i].obs = 0.0;
		}
	}
		
	SetSize (spec);
	if (gc) {
		pen[0] = gc->clbkCreatePen (1, 1, RGB(160,160,160));
		pen[1] = gc->clbkCreatePen (1, 1, RGB(255,128,0));
		brush  = gc->clbkCreateBrush (col_yellow1);
	} else {
		for (i = 0; i < 2; i++) pen[i] = 0;
		brush = 0;
	}
	if (instrDT > 0.25) instrDT = 0.25; // force at least 4 Hz refresh rate
}

Instrument_HSI::~Instrument_HSI ()
{
	int i;

	if (gc) {
		for (i = 0; i < 2; i++)
			if (pen[i]) gc->clbkReleasePen (pen[i]);
		if (brush) gc->clbkReleaseBrush (brush);
	}
	saveprm.usr = vessel;
	for (i = 0; i < 2; i++) {
		saveprm.hsi[i].nv = hsi[i].nv;
		saveprm.hsi[i].obs = hsi[i].obs;
	}
}

HELPCONTEXT *Instrument_HSI::HelpTopic () const
{
	extern HELPCONTEXT DefHelpContext;
	DefHelpContext.topic = "/mfd_hsi.htm";
	return &DefHelpContext;
}

void Instrument_HSI::UpdateDraw (oapi::Sketchpad *skp)
{
	int i, j, tk, tl, dx1, dy1, dx2, dy2, x0, xorig, yorig;
	oapi::IVECTOR2 *p;
	static oapi::IVECTOR2 arrow[22];
	const SurfParam *sp;
	double dir, alpha, cosa, sina, r1, tlng, tlat, adist, bdir, brg, crsdev, apprd, slope;
	char cbuf[64];
	bool bsignal, bglideslope;
	static int nseg = 44, seg[44] = {
		2, 2, 2, 4, 4, 4, 4, 4, // outer markers
		2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 // gyro ticks
	};
	static const INT narrowseg = 3, arrowseg[3] = {7,4,4};

	static char *label[12] = {"N", "3", "6", "E", "12", "15", "S", "21", "24", "W", "30", "33"};
	static int llen[12] = {1,1,1,1,2,2,1,2,2,1,2,2};
	
	// gyro direction
	if (sp = vessel->GetSurfParam()) dir = sp->dir;
	else                             dir = 0.0;
	
	// update gyro ring markers
	for (tk = tl = 0, alpha = dir+Pi05, p = pgyro; tk < 9; tk++, alpha -= 10*RAD) {
		cosa = cos(alpha), sina = sin(alpha);
		dx1 = (int)(cosa*R0+0.5), dy1 = (int)(sina*R0+0.5);
		r1  = R0*(tk % 3 ? 0.9:0.85);
		dx2 = (int)(cosa*r1+0.5), dy2 = (int)(sina*r1+0.5);
		p->x =  dx1, p->y = -dy1;  p++;
		p->x =  dx2, p->y = -dy2;  p++;
		p->x = -dx1, p->y =  dy1;  p++;
		p->x = -dx2, p->y =  dy2;  p++;
		p->x =  dy1, p->y =  dx1;  p++;
		p->x =  dy2, p->y =  dx2;  p++;
		p->x = -dy1, p->y = -dx1;  p++;
		p->x = -dy2, p->y = -dx2;  p++;
		if (!(tk%3)) {
			r1 = R0*0.75;
			dx2 = (int)(cosa*r1+0.5), dy2 = (int)(sina*r1+0.5);
			plabel[tl].x   =  dx2, plabel[tl].y   = -dy2;
			plabel[tl+6].x = -dx2, plabel[tl+6].y =  dy2;
			plabel[tl+3].x =  dy2, plabel[tl+3].y =  dx2;
			plabel[tl+9].x = -dy2, plabel[tl+9].y = -dx2;
			tl++;
		}
	}

	DisplayTitle (skp, "HSI");
	skp->GetOrigin (&xorig, &yorig);
	for (i = 0; i < 2; i++) {
		skp->SetOrigin (CNTX[i], CNTY[i]);
		x0 = -R0-U1;
		NavRadioSpec *nav = (hsi[i].nv < vessel->nnav ? vessel->nav+hsi[i].nv : 0);
		bsignal = bglideslope = false;
		skp->SetFont (GetDefaultFont (1));
		skp->SetTextColor (draw[0][0].col);
		skp->SetTextAlign (oapi::Sketchpad::LEFT);
		if (nav && nav->sender) {
			switch (nav->sender->Type()) {
			case TRANSMITTER_VOR: {
				Nav_VOR *vor = (Nav_VOR*)nav->sender;
				strcpy (cbuf, "VOR "); strcat (cbuf, vor->GetId());
				skp->Text (x0, YLN2, cbuf, strlen(cbuf));
				vor->GetEquPos (tlng, tlat);
				bsignal = true;
				} break;
			case TRANSMITTER_ILS: {
				Nav_ILS *ils = (Nav_ILS*)nav->sender;
				sprintf (cbuf, "ILS Rwy %02d ", (int)((apprd = ils->ApprDir())*DEG*0.1+0.5));
				strncat (cbuf, ils->GetBase()->Name(), 16);
				skp->Text (x0, YLN2, cbuf, strlen(cbuf));
				ils->GetEquPos (tlng, tlat);
				hsi[i].obs = ils->ApprDir();
				bsignal = bglideslope = true;
				} break;
			}
		} else {
			skp->Text (x0, YLN2, "[no signal]", 11);
		}
		if (sp && bsignal) {
			Orthodome (sp->lng, sp->lat, tlng, tlat, adist, bdir);
			Body * planet = vessel->ProxyPlanet();
			if (planet) adist *= planet->Size();
			brg = posangle (bdir);
			bdir -= hsi[i].obs;
			if      (bdir <  -Pi) bdir += Pi2;
			else if (bdir >=  Pi) bdir -= Pi2;
			crsdev = bdir;
			if      (crsdev < -Pi05) crsdev = -Pi-crsdev;
			else if (crsdev >= Pi05) crsdev =  Pi-crsdev;
			if (bglideslope) {
				double dst = cos(brg-apprd) * adist;
				slope = atan2 (sp->alt, dst);
			}
		} else bdir = brg = crsdev = adist = 0.0, bglideslope = false;

		// draw gyro with all markers
		skp->SetTextColor (draw[2][0].col);
		skp->SetPen (draw[2][0].solidpen);
		skp->PolyPolyline (pt, seg, nseg);
		skp->SetTextAlign (oapi::Sketchpad::CENTER);
		for (j = 0; j < 12; j++)
			skp->Text (plabel[j].x, plabel[j].y-U2, label[j], llen[j]);

		skp->SetPen (pen[0]);
		skp->SetBrush (brush);

		// draw glide slope marker
		if (bglideslope) {
			const double glslope = 4.0;
			double dslope = slope*DEG-glslope;
			dy1 = (int)(max(-2, min(2, dslope))*0.3*R0);
			skp->Rectangle (-R0/2, dy1-U4, R0/2, dy1+U4);
		}

		// draw obs arrows
		dy1 = (int)(max(-10, min (10, DEG*crsdev))*0.06*R0)+U4;
		dy2 = dy1-2*U4;
		parrow[11].y = parrow[12].y = dy2;
		parrow[13].y = parrow[14].y = dy1;
		alpha = -Pi05-dir+hsi[i].obs;
		cosa = cos(alpha), sina = sin(alpha);
		for (j = 0; j < 15; j++) {
			arrow[j].x = (int)(cosa*parrow[j].x - sina*parrow[j].y + 0.5);
			arrow[j].y = (int)(sina*parrow[j].x + cosa*parrow[j].y + 0.5);
		}
		skp->PolyPolygon (arrow, arrowseg, narrowseg);

		double ddx = U1*cosa, ddy = U1*sina;
		for (j = 0; j <= 10; j++) {
			double d = (j-5)*0.6*0.2*R0;
			double dx = d*sina;
			double dy = d*cosa;
			arrow[j*2].x   = (int)(dx+ddx+0.5), arrow[j*2].y   = (int)(-dy+ddy+0.5);
			arrow[j*2+1].x = (int)(dx-ddx+0.5), arrow[j*2+1].y = (int)(-dy-ddy+0.5);
		}
		skp->SetPen (draw[2][0].solidpen);
		skp->PolyPolyline (arrow, seg+8, 11);

		// draw focus marker
		skp->SetPen (draw[0][i == focus ? 0:1].solidpen);
		skp->SetBrush (0);
		skp->Rectangle (-R0-2*U1, -R0-2*U1, R0+2*U1, R0+2*U1);

		// draw vessel symbol
		skp->SetPen (pen[1]);
		skp->Polyline (psym, 5);

		// text output
		skp->SetTextAlign (oapi::Sketchpad::LEFT);
		cosa = cos(bdir);
		if      (cosa >  0.1) skp->Text (x0, R0+U1-U3, "TO", 2);
		else if (cosa < -0.1) skp->Text (x0, R0+U1-U3, "FROM", 4);
		skp->SetTextColor (draw[0][0].col);
		if (nav) {
			sprintf (cbuf, "NAV%d %06.2fMHz", hsi[i].nv+1, nav->freq);
			skp->Text (x0, YLN1, cbuf, strlen(cbuf));
		}
		sprintf (cbuf, "CRS %03.0f°", hsi[i].obs*DEG);
		skp->Text (x0, YLN3, cbuf, 8);
		sprintf (cbuf, "DEV %02.0f°", fabs(crsdev)*DEG);
		skp->Text (x0, YLN4, cbuf, 7);
		skp->SetTextAlign (oapi::Sketchpad::RIGHT);
		sprintf (cbuf, "BRG %03.0f°", brg*DEG);
		skp->Text (R0+U1, YLN3, cbuf, 8);
		sprintf (cbuf, "DST%s", DistStr (adist));
		skp->Text (R0+U1, YLN4, cbuf, strlen(cbuf));
	}
	skp->SetOrigin (xorig, yorig);
}

void Instrument_HSI::SetSize (const Spec &spec)
{
	static double sqrt2 = sqrt(2.0);
	int d1, d2;

	CNTX[0] = spec.w/4;     CNTY[0] = spec.h/2;
	CNTX[1] = (3*spec.w)/4; CNTY[1] = spec.h/2;
	U1 = spec.w/42, U2 = spec.w/48, U3 = spec.w/24, U4 = spec.w/128;
	R0 = spec.w/5,  R1 = spec.w/6;
	YLN1 = -R0 - 3*U1 - 2*U3;
	YLN2 = -R0 - 3*U1 - U3;
	YLN3 =  R0 + 3*U1;
	YLN4 =  R0 + 3*U1 + U3;
	pt[0].x = (pt[1].x = R0) + U1;
	pt[0].y =  pt[1].y = 0;
	pt[2].x = (pt[3].x = -R0) - U1;
	pt[2].y =  pt[3].y = 0;
	pt[4].x =  pt[5].x = 0;
	pt[4].y = (pt[5].y = R0) + U1;
	pt[6].x = pt[9].x = 0; pt[7].x = -U1; pt[8].x = U1;
	pt[7].y = pt[8].y = (pt[6].y = pt[9].y = -R0) - U1;
	d1 = (int)(R0/sqrt2+0.5); d2 = (int)(U1*sqrt2+0.5);
	pt[10].x = pt[11].x = pt[13].x =  d1; pt[12].x =  d1+d2;
	pt[10].y = pt[12].y = pt[13].y = -d1; pt[11].y = -d1-d2;
	pt[14].x = pt[15].x = pt[17].x = -d1; pt[16].x = -d1-d2;
	pt[14].y = pt[16].y = pt[17].y = -d1; pt[15].y = -d1-d2;
	pt[18].x = pt[19].x = pt[21].x =  d1; pt[20].x =  d1+d2;
	pt[18].y = pt[20].y = pt[21].y =  d1; pt[19].y =  d1+d2;
	pt[22].x = pt[23].x = pt[25].x = -d1; pt[24].x = -d1-d2;
	pt[22].y = pt[24].y = pt[25].y =  d1; pt[23].y =  d1+d2;
	pgyro = pt+26;
	parrow = pt+98;
	pt[98].x = R1; pt[98].y = 0;
	pt[99].x = pt[100].x = pt[103].x = pt[104].x = R1-U3;
	pt[101].x = pt[102].x = R1-2*U3;
	pt[99].y = U2; pt[104].y = -U2;
	pt[100].y = pt[101].y = U4; pt[102].y = pt[103].y = -U4;
	pt[105].x = pt[108].x = -R1+2*U3;
	pt[106].x = pt[107].x = -R1;
	pt[105].y = pt[106].y =  U4;
	pt[107].y = pt[108].y = -U4;
	pt[109].x = pt[112].x =  R1-2*U3;
	pt[110].x = pt[111].x = -R1+2*U3;
	pt[109].y = pt[110].y =  U4;
	pt[111].y = pt[112].y = -U4;
	psym = pt+113;
	psym[0].x = psym[1].x = psym[4].x = 0;
	psym[2].x = U1, psym[3].x = -U1;
	psym[0].y = -U3, psym[1].y = psym[4].y = 0;
	psym[2].y = psym[3].y = U3;
}

bool Instrument_HSI::KeyBuffered (DWORD key)
{
	switch (key) {
	case DIK_F:
		focus = 1-focus;
		Refresh();
		return true;
	case DIK_N:
		if (vessel->nnav) hsi[focus].nv = (hsi[focus].nv+1) % vessel->nnav;
		Refresh();
		return true;
	}
	return false;
}

bool Instrument_HSI::KeyImmediate (char *kstate)
{
	if (KEYDOWN (kstate, DIK_LBRACKET)) {
		if (BufKey (DIK_LBRACKET, 0.05)) {
			hsi[focus].obs = posangle (hsi[focus].obs-RAD);
			Refresh();
		}
		return true;
	}
	if (KEYDOWN (kstate, DIK_RBRACKET)) {
		if (BufKey (DIK_RBRACKET, 0.05)) {
			hsi[focus].obs = posangle (hsi[focus].obs+RAD);
			Refresh();
		}
		return true;
	}
	return false;
}

bool Instrument_HSI::ProcessButton (int bt, int event)
{
	static const DWORD btkey[4] = { DIK_F, DIK_N, DIK_LBRACKET, DIK_RBRACKET };
	if (event & PANEL_MOUSE_LBDOWN) {
		if (bt < 2) return KeyBuffered (btkey[bt]);
	} else if (event & PANEL_MOUSE_LBPRESSED) {
		if (bt >= 2 && bt < 4) return KeyImmediate (KstateSet (btkey[bt]));
	}
	return false;
}

const char *Instrument_HSI::BtnLabel (int bt) const
{
	static const char *label[4] = { "L/R", "NAV", "OB-", "OB+" };
	return (bt < 4 ? label[bt] : 0);
}

int Instrument_HSI::BtnMenu (const MFDBUTTONMENU **menu) const
{
	static const MFDBUTTONMENU mnu[4] = {
		{"Switch HSI focus", 0, 'F'},
		{"NAV receiver", "select", 'N'},
		{"OBS indicator", "left", '['},
		{"OBS indicator", "right", ']'}
	};
	if (menu) *menu = mnu;
	return 4;
}

bool Instrument_HSI::ReadParams (ifstream &ifs)
{
	char cbuf[256], *pc;
	if (!FindScnHeader (ifs)) return false;
	for (;;) {
		if (!ifs.getline (cbuf, 256)) return false;
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_MFD", 7)) {
			break;
		} else if (!_strnicmp (pc, "NAV", 3)) {
			sscanf (pc+3, "%d%d", &hsi[0].nv, &hsi[1].nv);
		} else if (!_strnicmp (pc, "OBS", 3)) {
			sscanf (pc+3, "%lf%lf", &hsi[0].obs, &hsi[1].obs);
		}
	}
	return true;
}

void Instrument_HSI::WriteParams (ostream &ofs) const
{
	ofs << "  TYPE HSI" << endl;
	ofs << "  NAV " << hsi[0].nv << ' ' << hsi[1].nv << endl;
	ofs << "  OBS " << hsi[0].obs << ' ' << hsi[1].obs << endl;
}

