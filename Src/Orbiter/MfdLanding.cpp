// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "MfdLanding.h"
#include "Nav.h"
#include "Astro.h"
#include "Config.h"
#include "Celbody.h"
#include <stdio.h>
#include <dinput.h>

using namespace std;

// =======================================================================
// class Instrument_Landing

struct Instrument_Landing::SavePrm Instrument_Landing::saveprm = {
	0, 0
};

Instrument_Landing::Instrument_Landing (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel, bool restore)
: Instrument (_pane, _id, spec, _vessel)
{
	nv = 0;

	if (gc) {
		brush[0] = gc->clbkCreateBrush (col_green2);
		brush[1] = gc->clbkCreateBrush (col_yellow1);
		brush[2] = gc->clbkCreateBrush (col_red1);
	} else {
		brush[0] = brush[1] = brush[2] = 0;
	}

	strcpy (title, "Launch/Land: ");
	if (restore && vessel == saveprm.usr) { // restore
		nv = saveprm.nv;
	}
	SetSize (spec);
}

Instrument_Landing::~Instrument_Landing ()
{
	// save status
	saveprm.usr = vessel;
	saveprm.nv  = nv;

	int i;
	if (gc) {
		for (i = 0; i < 3; i++)
			if (brush[i]) gc->clbkReleaseBrush (brush[i]);
	}
}

HELPCONTEXT *Instrument_Landing::HelpTopic () const
{
	extern HELPCONTEXT DefHelpContext;
	DefHelpContext.topic = "/mfd_vtol.htm";
	return &DefHelpContext;
}

bool Instrument_Landing::KeyBuffered (DWORD key)
{
	switch (key) {
	case DIK_N: // NAV receiver select
		if (vessel->nnav) nv = (nv+1) % vessel->nnav;
		Refresh();
		return true;
	}
	return false;
}

bool Instrument_Landing::ProcessButton (int bt, int event)
{
	static const DWORD btkey[1] = { DIK_N };
	if (event & PANEL_MOUSE_LBDOWN) {
		if (bt < 1) return KeyBuffered (btkey[bt]);
	}
	return false;
}

const char *Instrument_Landing::BtnLabel (int bt) const
{
	static const char *label[1] = { "NAV" };
	return (bt < 1 ? label[bt] : 0);
}

int Instrument_Landing::BtnMenu (const MFDBUTTONMENU **menu) const
{
	static const MFDBUTTONMENU mnu[1] = {
		{"NAV receiver", "select", 'N'}
	};
	if (menu) *menu = mnu;
	return 1;
}

void Instrument_Landing::UpdateDraw (oapi::Sketchpad *skp)
{
	oapi::Pen *ppen;
	char cbuf[256];
	int len, i, rd, x = cw/2, y = ch;
	double tlng, tlat; // target location
	bool draw_cone = false; // draw VTOL cone?

	DisplayTitle (skp, "VOR/VTOL");

	if (nv >= vessel->nnav) return; // sanity check
	NavRadioSpec *nav = vessel->nav+nv;

	skp->SetTextColor (draw[0][0].col);
	sprintf (cbuf, "NAV%d %06.2fMHz", nv+1, nav->freq);
	skp->Text (IW-cw*15, 0, cbuf, strlen (cbuf));

	if (nav->sender) {
		switch (nav->sender->Type()) {
		case TRANSMITTER_VOR: {
			len = nav->sender->IdString (cbuf, 256);
			skp->Text (x, y, cbuf, min (len, 35));
			Nav_VOR *vor = (Nav_VOR*)nav->sender;
			vor->GetEquPos (tlng, tlat);
			} break;
		case TRANSMITTER_VTOL: {
			len = nav->sender->IdString (cbuf, 256);
			skp->Text (x, y, cbuf, min (len, 35));
			Nav_VTOL *vtol = (Nav_VTOL*)nav->sender;
			vtol->GetEquPos (tlng, tlat);
			draw_cone = true;
			} break;
		default:
			skp->Text (x, y, "[no signal]", 11);
			return;
		}
	} else {
		skp->Text (x, y, "[no signal]", 11);
		return;
	}

	const SurfParam *sp = vessel->GetSurfParam();
	if (!sp) return;

	// target indicator circle
	skp->SetPen (draw[2][1].solidpen);
	for (i = 1; i <= 4; i++) {
		rd = (i*circr)/4;
		skp->Ellipse (circx-rd, circy-rd, circx+rd+1, circy+rd+1);
	}
	skp->Line (circx, circy-circr, circx, circy+circr+1);
	skp->Line (circx-circr, circy, circx+circr+1, circy);

	// target distance
	for (i = 1; i <= 4; i++) {
		rd = (i*circr)/4;
		cbuf[0] = '0'+i;
		skp->Text (circx+1, circy-rd-ch, cbuf, 1);
	}
	double adist, dist, ldist, hspd, lhspd, bdir, vdir, r;
	int dx, dy, xc, yc;
	Orthodome (sp->lng, sp->lat, tlng, tlat, adist, bdir);
	bdir -= sp->dir;
	if      (bdir <  0.0) bdir += Pi2;
	else if (bdir >= Pi2) bdir -= Pi2;
	dist = adist * sp->ref->Size();
	sprintf (cbuf, "DIST%s  DIR %03.0f°", DistStr(dist), DEG*bdir);
	skp->Text (x, ch*2, cbuf, strlen(cbuf));

	// horizontal airspeed
	skp->SetTextColor (draw[1][0].col);
	for (i = 1; i <= 4; i++) {
		rd = (i*circr)/4;
		cbuf[0] = '0'+i-1;
		skp->Text (circx+1, circy+rd-2, cbuf, 1);
	}
	Vector hvel (tmul (sp->ref->GRot(), sp->groundvel_glob));
	hvel.Set (mul (sp->L2H, hvel));
	hspd = _hypot (hvel.x, hvel.z);
	vdir = atan2 (hvel.x, hvel.z) - sp->dir;
	if      (vdir <= -Pi) vdir += Pi2;
	else if (vdir >=  Pi) vdir -= Pi2;
	sprintf (cbuf, "HSPD%s", FloatStr(hspd));
	skp->Text (x, ch*3, cbuf, strlen(cbuf));

	// altitude bar
	static double lalt_min = 0.0;
	static double lalt_max = 4.0;
	double lalt = log10(sp->alt);
	if (lalt > lalt_max) lalt = lalt_max;
	dx = (IW*9)/12-(barw*3)/2;
	dy = (int)((lalt-lalt_min)/(lalt_max-lalt_min)*barh);
	xc = dx+barw/2;
	yc = bar0-barh-(3*ch)/2;
	skp->SetBrush (brush[0]);
	skp->Rectangle (dx, bar0-dy, dx+barw, bar0+1);
	skp->SetBrush (0);
	skp->Rectangle (dx, bar0-barh, dx+barw, bar0+1);
	skp->SetTextColor (draw[0][0].col);
	skp->Text (xc-(cw*3)/2, yc-ch, "ALT", 3);
	strcpy (cbuf, DistStr(sp->alt)+1); i = strlen(cbuf);
	skp->Text (xc-(i*cw)/2, yc, cbuf, i);
	for (i = 0; i <= 4; i++) {
		dy = (barh*i)/4;
		skp->Line (dx, bar0-dy, dx+barw+2, bar0-dy);
		char no = '0' + i;
		skp->Text (dx+barw+3, bar0-dy-ch/2, &no, 1);
	}

	// vertical velocity bar
	static double lvel_min = -1.0;
	static double lvel_max =  3.0;
	double lvel = log10(fabs(hvel.y));
	if (lvel < lvel_min) lvel = lvel_min;
	else if (lvel > lvel_max) lvel = lvel_max;
	dx = (IW*11)/12-barw;
	dy = (int)((lvel-lvel_min)/(lvel_max-lvel_min)*barh);
	xc = dx + barw/2;
	skp->SetBrush (brush[hvel.y > 0.0 ? 0 : lvel+1.0 <= lalt ? 1 : 2]);
	skp->Rectangle (dx, bar0-dy, dx+barw, bar0+1);
	skp->SetBrush (0);
	skp->Rectangle (dx, bar0-barh, dx+barw, bar0+1);
	skp->Text (xc-cw*2, yc-ch, "VSPD", 4);
	strcpy (cbuf, DistStr (fabs(hvel.y))+1); i = strlen(cbuf);
	skp->Text (xc-(i*cw)/2, yc, cbuf, i);
	for (i = 0; i <= 4; i++) {
		dy = (barh*i)/4;
		skp->Line (dx, bar0-dy, dx+barw+2, bar0-dy);
		_itoa (i-1, cbuf, 10);
		skp->Text (dx+barw+3, bar0-dy-ch/2, cbuf, strlen(cbuf));
	}

	skp->SetFont (GetDefaultFont (2));
	skp->SetTextAlign (oapi::Sketchpad::CENTER);
	skp->Text ((IW* 9)/12-(5*barw)/2, bar0-barh/2, "10 ^ x m", 8);
	skp->Text ((IW*11)/12-2*barw, bar0-barh/2, "10 ^ x m/s", 10);
	skp->Text (circx-barw, circy-circr/2, "10 ^ x m", 8);
	skp->SetTextColor (draw[1][0].col);
	skp->Text (circx-barw, circy+circr/2, "10 ^ x m/s", 10);

	// VTOL cone
	if (draw_cone) {
		double tgtrad = 20.0 + 0.2*sp->alt;
		ldist = log10(tgtrad);
		if (ldist <= 4.0) {
			rd = (int)(ldist * circr * 0.25);
			ppen = skp->SetPen (draw[dist > tgtrad ? 3:0][0].solidpen);
			skp->Ellipse (circx-rd, circy-rd, circx+rd+1, circy+rd+1);
			skp->SetPen (ppen);
		}
	}

	// velocity vector
	double sinv = sin(vdir), cosv = cos(vdir);
	lhspd = log10(hspd) + 1.0;
	if (lhspd < 0.0) lhspd = 0.0;
	else if (lhspd > 6.0) lhspd = 6.0;
	r = lhspd * circr * 0.25;
	dx = (int)(r*sinv);
	dy = (int)(r*cosv);
	xc = circx + dx;
	yc = circy - dy;
	double s = (double)ch, u = s*0.3;
	double sx = s*sinv, sy = s*cosv;
	ppen = skp->SetPen (draw[1][0].solidpen);
	skp->MoveTo (circx, circy);
	skp->LineTo (xc, yc);
	skp->LineTo (xc-(int)(sx+u*cosv), yc+(int)(sy-u*sinv));
	skp->LineTo (xc-(int)(sx-u*cosv), yc+(int)(sy+u*sinv));
	skp->LineTo (xc, yc);

	// NAV transmitter location indicator
	ldist = log10(dist);
	if (ldist < 0.0) ldist = 0.0;
	else if (ldist > 6.0) ldist = 6.0;
	r = ldist * circr * 0.25;
	dx = (int)(r*sin(bdir));
	dy = (int)(r*cos(bdir));
	xc = circx + dx;
	yc = circy - dy;
	ppen = skp->SetPen (draw[0][0].solidpen);
	skp->Line (circx, circy, xc, yc);
	skp->Line (xc-ch, yc, xc+ch+1, yc);
	skp->Line (xc, yc-ch, xc, yc+ch+1);
	skp->SetPen (ppen);
}

void Instrument_Landing::SetSize (const Spec &spec)
{
	circx = spec.w/3;
	circy = spec.h - spec.w/3 - ch;
	circr = spec.w/3 - spec.w/40;
	bar0 = circy+circr;
	barh = spec.h - 6*ch;
	barw = circr/8;
}

bool Instrument_Landing::ReadParams (ifstream &ifs)
{
	char cbuf[256], *pc;
	if (!FindScnHeader (ifs)) return false;
	for (;;) {
		if (!ifs.getline (cbuf, 256)) return false;
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_MFD", 7)) break;
		if (!_strnicmp (pc, "NAV", 3)) {
			sscanf (pc+3, "%d", &nv);
		}
	}
	return true;
}

void Instrument_Landing::WriteParams (ostream &ofs) const
{
	ofs << "  TYPE Launch" << endl;
	ofs << "  NAV " << nv << endl;
}

