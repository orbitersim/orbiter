// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "MfdSurface.h"
#include "Orbiter.h"
#include "Pane.h"
#include "Celbody.h"
#include "Psys.h"
#include "Select.h"
#include "Util.h"
#include "Log.h"

using namespace std;

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern Pane *g_pane;

// =======================================================================
// class Instrument_Surface

COLORREF Instrument_Surface::tapelabelcol[2] = {0xFFFFFF,0x60A0FF}; // make configurable
struct Instrument_Surface::SavePrm Instrument_Surface::saveprm = { 1 };

Instrument_Surface::Instrument_Surface (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel)
: Instrument (_pane, _id, spec, _vessel)
{
	sp = 0;
	atmc = 0;
	alt = spd = vspd = lng = lat = aoa = acc = vacc = 0.0;
	as_plimit = 1e-3; // pressure limit for pitot airspeed measurement
	spdmode = saveprm.spdmode;
	psimt = 0.0;
	strcpy (title, "Surface: ");
	SetSize (spec);
	if (instrDT > 0.25) instrDT = 0.25; // force at least 4 Hz refresh rate

	tapecol = g_pOrbiter->GetDeviceColour (0,0,160);
	hrzcol[0] = g_pOrbiter->GetDeviceColour( 99, 93,231);
	hrzcol[1] = g_pOrbiter->GetDeviceColour(173,125, 37);

	if (gc) {
		horizonpen = gc->clbkCreatePen (1, 3, 0xFFFFFF);
		brush[0] = gc->clbkCreateBrush (RGB( 99, 93,231));
		brush[1] = gc->clbkCreateBrush (RGB(173,125, 37));
		brush[2] = gc->clbkCreateBrush (RGB(0,0,0));
	} else {
		int i;
		horizonpen = 0;
		for (i = 0; i < 3; i++) brush[i] = 0;
	}

	horizon = 0; //CreateSurface (hrzr*2, hrzr*2);
	heading = 0; //CreateSurface (hdgbmpw, hdgbmph);
	tapes1  = 0; //CreateSurface (cw*17, hrzr*2);
	ticks1  = 0; //CreateSurface (cw*2,  hrzr*3);
	tapes2  = 0; //CreateSurface (cw*14, accr*2);
	ticks2  = 0; //CreateSurface (cw*2,  accr*3);
	InitDeviceObjects ();

}

Instrument_Surface::~Instrument_Surface ()
{
	int i;
	if (gc) {
		if (horizonpen) gc->clbkReleasePen (horizonpen);
		for (i = 0; i < 3; i++)
			if (brush[i]) gc->clbkReleaseBrush (brush[i]);
	}
	if (horizon) g_pOrbiter->ReleaseSurface (horizon);
	if (heading) g_pOrbiter->ReleaseSurface (heading);
	if (tapes1)  g_pOrbiter->ReleaseSurface (tapes1);
	if (ticks1)  g_pOrbiter->ReleaseSurface (ticks1);
	if (tapes2)  g_pOrbiter->ReleaseSurface (tapes2);
	if (ticks2)  g_pOrbiter->ReleaseSurface (ticks2);

	// save status
	saveprm.spdmode = spdmode;
}

void Instrument_Surface::InitDeviceObjects ()
{
	if (!gc) return;

	int i, j, x0, x1, y, y0, y1, y2, dy;

	if (!horizon) horizon = gc->clbkCreateSurfaceEx (hrzr*2, hrzr*2, OAPISURFACE_RENDERTARGET | OAPISURFACE_SKETCHPAD);
	if (!heading) heading = gc->clbkCreateSurfaceEx (hdgbmpw, hdgbmph, OAPISURFACE_RENDERTARGET | OAPISURFACE_SKETCHPAD);
	if (!tapes1)  tapes1  = gc->clbkCreateSurfaceEx (cw*17, hrzr*2, OAPISURFACE_RENDERTARGET | OAPISURFACE_SKETCHPAD);
	if (!ticks1)  ticks1  = gc->clbkCreateSurfaceEx (cw*2,  hrzr*3, OAPISURFACE_RENDERTARGET | OAPISURFACE_SKETCHPAD);
	if (!tapes2)  tapes2  = gc->clbkCreateSurfaceEx (cw*14, accr*2, OAPISURFACE_RENDERTARGET | OAPISURFACE_SKETCHPAD);
	if (!ticks2)  ticks2  = gc->clbkCreateSurfaceEx (cw*2,  accr*3, OAPISURFACE_RENDERTARGET | OAPISURFACE_SKETCHPAD);

	oapi::Sketchpad *skp;
	oapi::Pen *wpen = gc->clbkCreatePen (1, 1, 0xffffff);

	// Create heading indicator tape
	if (heading) {
		g_pOrbiter->FillSurface (heading, tapecol);
		if (skp = gc->clbkGetSketchpad (heading)) {
			skp->SetFont (GetDefaultFont (3));
			skp->SetTextColor (tapelabelcol[0]);
			skp->SetTextAlign (oapi::Sketchpad::CENTER);
			static char hdglabel[15][4] =
				{"330"," N ","030","060"," E ","120","150"," S ","210","240"," W ","300","330"," N ","030"};
			skp->SetPen (wpen);
			y0 = (3*ch)/2, y1 = ch, y2 = (5*ch)/4;
			for (i = 0; i <= 44; i++) {
				x0 = i*hdgtick;
				skp->Line (x0, y0, x0, y1);
				if (i < 44) for (j = 1; j < 5; j++) {
					x1 = x0 + (j*hdgtick)/5;
					skp->Line (x1, y0, x1, y2);
				}
			}
			for (i = 0; i < 15; i++)
				skp->Text (hdgtick * (i*3+1), 0, hdglabel[i], 3);
			gc->clbkReleaseSketchpad (skp);
		}
	}

	// Create tick strip for top row of tapes
	if (ticks1) {
		g_pOrbiter->FillSurface (ticks1, tapecol);
		if (skp = gc->clbkGetSketchpad (ticks1)) {
			skp->SetPen (wpen);
			y0 = (hrzr*3)/2;
			dy = (hrzr*2)/5;
			x0 = cw/2+1;
			x1 = cw/4+1;
			for (y = y0; y >= 0; y -= dy) {
				skp->Line (0, y, cw*2, y);
				skp->Line (cw-x1, y-dy/4, cw+x1, y-dy/4);
				skp->Line (cw-x0, y-dy/2, cw+x0, y-dy/2);
				skp->Line (cw-x1, y-(dy*3)/4, cw+x1, y-(dy*3)/4);
			}
			for (y = y0; y < 3*(int)hrzr; y += dy) {
				skp->Line (0, y, cw*2, y);
				skp->Line (cw-x1, y+dy/4, cw+x1, y+dy/4);
				skp->Line (cw-x0, y+dy/2, cw+x0, y+dy/2);
				skp->Line (cw-x1, y+(dy*3)/4, cw+x1, y+(dy*3)/4);
			}
			gc->clbkReleaseSketchpad (skp);
		}
	}

	if (ticks2) {
		// Create tick strip for bottom row of tapes
		g_pOrbiter->FillSurface (ticks2, tapecol);
		if (skp = gc->clbkGetSketchpad (ticks2)) {
			skp->SetPen (wpen);
			y0 = (accr*3)/2;
			dy = (accr*2)/5;
			x0 = cw/2+1;
			x1 = cw/4+1;
			for (y = y0; y >= 0; y -= dy) {
				skp->Line (0, y, cw*2, y);
				skp->Line (cw-x1, y-dy/4, cw+x1, y-dy/4);
				skp->Line (cw-x0, y-dy/2, cw+x0, y-dy/2);
				skp->Line (cw-x1, y-(dy*3)/4, cw+x1, y-(dy*3)/4);
			}
			for (y = y0; y < 3*hrzr; y += dy) {
				skp->Line (0, y, cw*2, y);
				skp->Line (cw-x1, y+dy/4, cw+x1, y+dy/4);
				skp->Line (cw-x0, y+dy/2, cw+x0, y+dy/2);
				skp->Line (cw-x1, y+(dy*3)/4, cw+x1, y+(dy*3)/4);
			}
			gc->clbkReleaseSketchpad (skp);
		}
	}
	gc->clbkReleasePen (wpen);
}

HELPCONTEXT *Instrument_Surface::HelpTopic () const
{
	extern HELPCONTEXT DefHelpContext;
	DefHelpContext.topic = (char*)"/mfd_surf.htm";
	return &DefHelpContext;
}

bool Instrument_Surface::KeyBuffered (DWORD key)
{
	switch (key) {
	case OAPI_KEY_G: // ground-relative speed (GRS)
		spdmode = 1;
		Refresh();
		return true;
	case OAPI_KEY_H: // copy data to HUD
		CopyToHUD ();
		return true;
	case OAPI_KEY_I: // indicated airspeed (IAS)
		spdmode = 3;
		Refresh();
		return true;
	case OAPI_KEY_O: // orbital speed (OS)
		spdmode = 4;
		Refresh();
		return true;
	case OAPI_KEY_T: // true airspeed (TAS)
		spdmode = 2;
		Refresh();
		return true;
	}
	return false;
}

bool Instrument_Surface::ProcessButton (int bt, int event)
{
	static const DWORD btkey[5] = { OAPI_KEY_I, OAPI_KEY_T, OAPI_KEY_G, OAPI_KEY_O, OAPI_KEY_H };
	if (event & PANEL_MOUSE_LBDOWN) {
		if (bt < 5) return KeyBuffered (btkey[bt]);
	}
	return false;
}

const char *Instrument_Surface::BtnLabel (int bt) const
{
	static const char *label[5] = { "IAS", "TAS", "GS", "OS", "HUD" };
	return (bt < 5 ? label[bt] : 0);
}

int Instrument_Surface::BtnMenu (const MFDBUTTONMENU **menu) const
{
	static const MFDBUTTONMENU mnu[5] = {
		{"Indicated airspeed", 0, 'I'},
		{"True airspeed", 0, 'T'},
		{"Ground-relative", "speed", 'G'},
		{"Orbital speed", 0, 'O'},
		{"Copy data to HUD", 0, 'H'}
	};
	if (menu) *menu = mnu;
	return 5;
}

void Instrument_Surface::UpdateHorizon ()
{
	if (!sp) return;

	oapi::IVECTOR2 pt[4];
	static double prange = RAD*30.0;
	int size = hrzr, size2 = size*2;
	int extent = (int)(size*prange);
	double bank = sp->bank;
	double pitch = sp->pitch;
	double pfrac = pitch/prange;
	double sinb = sin(bank), cosb = cos(bank);
	double a = tan(bank);
	double yl, yr, xb, xt, xlr, xll, ylr, yll;
	int i, iphi, n = 0;
	bool bl, br, bb, bt, bblue;
	if (cosb) { // horizon not vertical
		double b = pfrac/cosb;
		bl = (fabs(yl = -a+b) < 1.0); // left edge
		br = (fabs(yr =  a+b) < 1.0);  // right edge
		if (a) { // horizon not horizontal
			bb = (fabs(xb = ( 1.0-b)/a) < 1.0); // bottom edge
			bt = (fabs(xt = (-1.0-b)/a) < 1.0); // top edge
		} else { // horizon horizontal
			bb = bt = false;
		}
	} else { // horizon vertical
		bl = br = false;
		bb = bt = (fabs(xb = xt = pfrac) < 1.0);
	}
	if (bl) {
		pt[0].x = 0;
		pt[0].y = (int)(yl*size)+size;
		if (bt) {
			pt[1].x = (int)(xt*size)+size;
			pt[1].y = 0;
			pt[2].x = 0;
			pt[2].y = 0;
			n = 3;
			bblue = (cosb > 0.0);
		} else if (br) {
			pt[1].x = size2;
			pt[1].y = (int)(yr*size)+size;
			pt[2].x = size2;
			pt[2].y = 0;
			pt[3].x = 0;
			pt[3].y = 0;
			n = 4;
			bblue = (cosb > 0.0);
		} else if (bb) {
			pt[1].x = (int)(xb*size)+size;
			pt[1].y = size2;
			pt[2].x = 0;
			pt[2].y = size2;
			n = 3;
			bblue = (cosb < 0.0);
		}
	} else if (br) {
		pt[0].x = size2;
		pt[0].y = (int)(yr*size)+size;
		if (bt) {
			pt[1].x = (int)(xt*size)+size;
			pt[1].y = 0;
			pt[2].x = size2;
			pt[2].y = 0;
			n = 3;
			bblue = (cosb > 0.0);
		} else if (bb) {
			pt[1].x = (int)(xb*size)+size;
			pt[1].y = size2;
			pt[2].x = size2;
			pt[2].y = size2;
			n = 3;
			bblue = (cosb < 0.0);
		}
	} else if (bt && bb) {
		pt[0].x = (int)(xt*size)+size;
		pt[0].y = 0;
		pt[1].x = (int)(xb*size)+size;
		pt[1].y = size2;
		pt[2].x = 0;
		pt[2].y = size2;
		pt[3].x = 0;
		pt[3].y = 0;
		n = 4;
		bblue = ((xt-xb)*cosb > 0.0);
	}
	if (!n) bblue = (pitch < 0.0);

	g_pOrbiter->FillSurface (horizon, hrzcol[bblue?1:0]);

	oapi::Sketchpad *skp;
	if (gc && (skp = gc->clbkGetSketchpad (horizon))) {
		if (n >= 3) {
			skp->SetBrush (brush[bblue ? 0:1]);
			skp->Polygon (pt, n);
			skp->SetPen (horizonpen);
			skp->Line (pt[0].x, pt[0].y, pt[1].x, pt[1].y);
		}

		// bank indicator
		double r1 = hrzr-5, r2 = ((hrzr-5)*8)/10;
		double sinb1 = sin(bank-0.1), cosb1 = cos(bank-0.1);
		double sinb2 = sin(bank+0.1), cosb2 = cos(bank+0.1);
		pt[0].x = (int)(r2*sinb1+0.5)+size; pt[0].y = -(int)(r2*cosb1+0.5)+size;
		pt[1].x = (int)(r1*sinb+0.5)+size;  pt[1].y = -(int)(r1*cosb+0.5)+size;
		pt[2].x = (int)(r2*sinb2+0.5)+size; pt[2].y = -(int)(r2*cosb2+0.5)+size;

		skp->SetBrush (0);
		skp->SetPen (draw[2][1].solidpen);
		skp->Ellipse (5, 5, 2*hrzr-5, 2*hrzr-5);
		skp->SetPen (draw[2][0].solidpen);
		skp->Polygon (pt, 3);
		for (i = 0; i < 12; i++)
			skp->Rectangle (bnkmarkx[i]-2, bnkmarky[i]-2, bnkmarkx[i]+2, bnkmarky[i]+2);

		// pitch ladder
		double d = size*(10.0*RAD)/prange;
		double ladderw = hrzr/4;
		double lwcosa = ladderw*cosb, lwsina = ladderw*sinb;
		double dsinb = d*sinb, dcosb = d*cosb;
		double phi0 = floor(pitch*DEG*0.1);
		double d0 = (pitch*DEG*0.1-phi0) * d, d1 = d0-4*d;
		// ladder main ticks
		xlr = lwcosa-d1*sinb, xll = -lwcosa-d1*sinb;
		ylr = lwsina+d1*cosb, yll = -lwsina+d1*cosb;
		for (iphi = (int)phi0+4, i = 0; i < 8; i++, iphi--) {
			if (iphi) {
				skp->Line (size+(int)xll, size+(int)yll, size+(int)xlr, size+(int)ylr);
			}
			xlr -= dsinb, ylr += dcosb;
			xll -= dsinb, yll += dcosb;
		}
		// labels
		skp->SetFont (GetDefaultFont (1));
		skp->SetTextColor (tapelabelcol[0]);
		lwcosa *= 1.6, lwsina *= 1.6;
		xlr = lwcosa-d1*sinb, xll = -lwcosa-d1*sinb;
		ylr = lwsina+d1*cosb, yll = -lwsina+d1*cosb;
		char cbuf[4] = "00";
		int ofs = size-((ch*3)/8+1);
		for (iphi = (int)phi0+4, i = 0; i < 8; i++, iphi--) {
			if (iphi) {
				int lb = abs(iphi); if (lb > 9) lb = 18-lb;
				cbuf[0] = lb+'0';
				skp->Text (ofs+(int)xlr, ofs+(int)ylr, cbuf, 2);
				skp->Text (ofs+(int)xll, ofs+(int)yll, cbuf, 2);
			}
			xlr -= dsinb, ylr += dcosb;
			xll -= dsinb, yll += dcosb;
		}

		// ladder minor ticks
		lwcosa *= 0.35, lwsina *= 0.35;
		d1 = d0 - 3.5*d;
		xlr = lwcosa-d1*sinb, xll = -lwcosa-d1*sinb;
		ylr = lwsina+d1*cosb, yll = -lwsina+d1*cosb;
		for (iphi = (int)phi0+3, i = 0; i < 7; i++, iphi--) {
			skp->Line (size+(int)xll, size+(int)yll, size+(int)xlr, size+(int)ylr);
			xlr -= dsinb, ylr += dcosb;
			xll -= dsinb, yll += dcosb;
		}

		skp->SetBrush (brush[2]);
		skp->Rectangle (hrzr/4, hrzr-3, hrzr/2, hrzr+3);
		skp->Rectangle ((3*hrzr)/2, hrzr-3, (7*hrzr)/4, hrzr+3);
		skp->Rectangle (hrzr-3, hrzr-3, hrzr+3, hrzr+3);
		gc->clbkReleaseSketchpad (skp);
	}
}

void Instrument_Surface::UpdateTapes ()
{
	g_pOrbiter->FillSurface (tapes1, tapecol);
	g_pOrbiter->FillSurface (tapes2, tapecol);

	char cbuf[20] = "";
	int i, y, xofs, tcki;
	int dy1 = (hrzr*2)/5, dy2 = (accr*2)/5;
	RECT r = {0, 0, cw, 0};

	// speed tape calculations
	double spd100 = spd*0.01;
	int spd0 = (int)spd100;
	int spd1 = spd0+1;
	int spd_y0 = hrzr + (int)((spd100-spd0)*dy1+0.5);

	// speed tape ticks
	r.top = (hrzr*3)/2 - spd_y0;
	r.bottom = r.top + 2*hrzr;
	g_pOrbiter->Blt (tapes1, cw*5, 0, ticks1, 0, r.top, cw, 2*hrzr);

	// altitude tape calculations
	double alt = (sp->alt < 1e4 ? sp->alt : sp->alt0);
	double altkm = alt*1e-3;
	double altkm_min = altkm - 2.6;
	double altkm_max = altkm + 2.6;
	int alt0 = (int)altkm;
	int alt1 = alt0+1;
	int alt_y0 = hrzr + (int)((altkm-alt0)*dy1+0.5);

	// altitude tape ticks
	r.left = cw; r.right = 2*cw;
	r.top = (hrzr*3)/2 - alt_y0;
	r.bottom = r.top + 2*hrzr;
	g_pOrbiter->Blt (tapes1, cw*6, 0, ticks1, cw, r.top, cw, 2*hrzr);

	// VSI tape calculations
	double vsp10 = vspd*0.1;
	int vsp0 = (int)(vsp10 >= 0 ? vsp10 : vsp10-1);
	int vsp1 = vsp0+1;
	int vsi_y0 = hrzr + (int)((vsp10-vsp0)*dy1+0.5);

	// VSI tape ticks
	r.top = (hrzr*3)/2 - vsi_y0;
	r.bottom = r.top + 2*hrzr;
	g_pOrbiter->Blt (tapes1, cw*12, 0, ticks1, cw, r.top, cw, 2*hrzr);

	// acceleration calculations
	int acc0 = (int)(acc >= 0 ? acc : acc-1);
	int acc1 = acc0+1;
	int acc_y0 = accr + (int)((acc-acc0)*dy2+0.5);

	// acceleration tape ticks
	r.left = 0, r.right = cw;
	r.top = (accr*3)/2 - acc_y0;
	r.bottom = r.top + 2*accr;
	g_pOrbiter->Blt (tapes2, cw*4, 0, ticks2, 0, r.top, cw, 2*accr);

	// vertical acceleration calculations
	int vac0 = (int)(vacc >= 0 ? vacc : vacc-1);
	int vac1 = vac0+1;
	int vac_y0 = accr + (int)((vacc-vac0)*dy2+0.5);

	// vertical acceleration tape ticks
	r.left = cw, r.right = 2*cw;
	r.top = (accr*3)/2 - vac_y0;
	r.bottom = r.top + 2*accr;
	g_pOrbiter->Blt (tapes2, cw*5, 0, ticks2, cw, r.top, cw, 2*accr);

	// AOA calculations
	int aoa0 = (int)(aoa >= 0 ? aoa : aoa-1);
	int aoa1 = aoa0+1;
	int aoa_y0 = accr + (int)((aoa-aoa0)*dy2+0.5);

	// AOA tape ticks
	r.top = (accr*3)/2 - aoa_y0;
	r.bottom = r.top + 2*accr;
	g_pOrbiter->Blt (tapes2, cw*10, 0, ticks2, cw, r.top, cw, 2*accr);

	oapi::Sketchpad *skp;
	if (skp = gc->clbkGetSketchpad (tapes1)) {
		// speed tape
		skp->SetFont (GetDefaultFont (3));
		skp->SetTextColor (tapelabelcol[0]);
		skp->SetTextAlign (oapi::Sketchpad::RIGHT);
		y = spd_y0 + 2*dy1-ch/2 - 1;
		for (i = -2; i <= 3; i++) {
			if ((tcki = i+spd0) >= 0) {
				sprintf (cbuf, "%03d00", tcki % 1000);
				skp->Text (cw*5, y, cbuf, 5);
			}
			y -= dy1;
		}

		// altitude tape
		skp->SetTextAlign (oapi::Sketchpad::LEFT);
		xofs = cw*6;
		y = alt_y0 + 2*dy1-ch/2 - 1;
		for (i = -2; i <= 3; i++) {
			if ((tcki = i+alt0) >= 0) {
				sprintf (cbuf, "%05d", tcki % 100000);
				skp->Text (xofs+cw, y, cbuf, 5);
			}
			y -= dy1;
		}
		// aphel, perihel distance markers
		const Elements *el = vessel->Els();
		double h = (el->ApDist() - vessel->ProxyBody()->Size()) * 1e-3;
		if (h >= altkm_min && h <= altkm_max) {
			y = (int)(hrzr-(h-altkm)*dy1+0.5);
			skp->SetPen (draw[3][0].solidpen);
			skp->Line (xofs, y, xofs+cw, y);
			skp->SetBackgroundColor (0x0000ff);
			skp->SetBackgroundMode (oapi::Sketchpad::BK_OPAQUE);
			skp->Text (xofs+cw, y-ch/2-1, "AP", 2);
		}
		h = (el->PeDist() - vessel->ProxyBody()->Size()) * 1e-3;
		if (h >= altkm_min && h <= altkm_max) {
			y = (int)(hrzr-(h-altkm)*dy1+0.5);
			skp->SetPen (draw[3][0].solidpen);
			skp->Line (xofs, y, xofs+cw, y);
			skp->SetBackgroundColor (0x0000ff);
			skp->SetBackgroundMode (oapi::Sketchpad::BK_OPAQUE);
			skp->Text (xofs+cw, y-ch/2-1, "PE", 2);
		}

		// VSI tape
		xofs = cw*12+(9*cw)/2;
		skp->SetFont (GetDefaultFont (1));
		skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);
		skp->SetTextColor (tapelabelcol[vsp0 >= 2 ? 0:1]);
		skp->SetTextAlign (oapi::Sketchpad::RIGHT);
		y = vsi_y0 + 2*dy1-(3*ch)/8 - 1;
		for (i = -2; i <= 3; i++) {
			tcki = i+vsp0;
			if (!tcki) skp->SetTextColor (tapelabelcol[0]);
			sprintf (cbuf, "%+04d0", tcki % 1000);
			skp->Text (xofs, y, cbuf, 5);
			y -= dy1;
		}
		gc->clbkReleaseSketchpad (skp);
	}

	if (skp = gc->clbkGetSketchpad (tapes2)) {
		// acceleration tape
		skp->SetFont (GetDefaultFont (1));
		skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);
		skp->SetTextAlign (oapi::Sketchpad::RIGHT);
		skp->SetTextColor (tapelabelcol[acc0 >= 2 ? 0:1]);
		xofs = (7*cw)/2;
		y = acc_y0 + 2*dy2-(3*ch)/8 - 1;
		for (i = -2; i <= 3; i++) {
			tcki = i+acc0;
			if (!tcki) skp->SetTextColor (tapelabelcol[0]);
			sprintf (cbuf, "%+03d", tcki % 100);
			skp->Text (xofs, y, cbuf, 3);
			y -= dy2;
		}

		// vertical acceleration tape
		skp->SetTextColor (tapelabelcol[vac0 >= 2 ? 0:1]);
		xofs = (17*cw)/2;
		y = vac_y0 + 2*dy2-(3*ch)/8 - 1;
		for (i = -2; i <= 3; i++) {
			tcki = i+vac0;
			if (!tcki) skp->SetTextColor (tapelabelcol[0]);
			sprintf (cbuf, "%+03d", tcki % 100);
			skp->Text (xofs, y, cbuf, 3);
			y -= dy2;
		}

		// AOA tape
		skp->SetTextColor (tapelabelcol[aoa0 >= 2 ? 0:1]);
		xofs = (55*cw)/4;
		y = aoa_y0 + 2*dy2-(3*ch)/8 - 1;
		for (i = -2; i <= 3; i++) {
			tcki = i+aoa0;
			if (!tcki) skp->SetTextColor (tapelabelcol[0]);
			sprintf (cbuf, "%+04d", tcki);
			skp->Text (xofs, y, cbuf, 4);
			y -= dy2;
		}
		gc->clbkReleaseSketchpad (skp);
	}
}

void Instrument_Surface::UpdateBlt ()
{
	if (!g_pOrbiter->GetGraphicsClient()) return; // no graphics available

	sp = vessel->GetSurfParam();
	if (!sp) return;
	if (sp && sp->ref->Type() == OBJTP_PLANET)
		atmc = ((Planet*)sp->ref)->AtmParams();
	else
		atmc = 0;

	if (horizon && heading) {
		UpdateHorizon();
		g_pOrbiter->Blt (surf, hrzx0, hrzy0, horizon);

		static RECT r = {0,0,0,0};
		r.left   = (int)((DEG*sp->dir + 40.0)/440.0 * hdgbmpw) - hrzr;
		r.right  = r.left+2*hrzr;
		r.bottom = (3*ch)/2;
		g_pOrbiter->Blt (surf, hrzx0, diry0, heading, r.left, 0, 2*hrzr, r.bottom);

		UpdateTapes();
		r.left = 0, r.right = cw*6, r.top = 0, r.bottom = hrzr*2;
		g_pOrbiter->Blt (surf, spdx0, hrzy0, tapes1, 0, 0, r.right, r.bottom);
		//surf->BltFast (spdx0, hrzy0, tapes1, &r, DDBLTFAST_WAIT);
		r.left = cw*6, r.right = cw*12;
		g_pOrbiter->Blt (surf, altx0, hrzy0, tapes1, cw*6, 0, cw*6, r.bottom);
		//surf->BltFast (altx0, hrzy0, tapes1, &r, DDBLTFAST_WAIT);
		r.left = cw*12, r.right = cw*17;
		g_pOrbiter->Blt (surf, vspx0, hrzy0, tapes1, cw*12, 0, cw*5, r.bottom);
		//surf->BltFast (vspx0, hrzy0, tapes1, &r, DDBLTFAST_WAIT);
		r.left = 0, r.right = cw*5, r.top = 0, r.bottom = accr*2;
		g_pOrbiter->Blt (surf, accx0, accy0, tapes2, 0, 0, cw*5, accr*2);
		//surf->BltFast (accx0, accy0, tapes2, &r, DDBLTFAST_WAIT);
		r.left = cw*5, r.right = cw*10;
		g_pOrbiter->Blt (surf, vacx0, accy0, tapes2, cw*5, 0, cw*5, accr*2);
		//surf->BltFast (vacx0, accy0, tapes2, &r, DDBLTFAST_WAIT);
		r.left = cw*10, r.right = cw*14;
		g_pOrbiter->Blt (surf, aoax0, accy0, tapes2, cw*10, 0, cw*4, accr*2);
		//surf->BltFast (aoax0, accy0, tapes2, &r, DDBLTFAST_WAIT);
	}
}

#ifdef UNDEF
bool Instrument_Surface::GRS (const CelestialBody *body, double &grs) const
{
	// ground-relative vessel speed
	double lng, lat, rad, vref;
	body->GlobalToEquatorial (vessel->GPos(), lng, lat, rad);
	vref = Pi2/body->RotT() * rad*cos(lat);
	Vector GRS (vessel->GVel() - body->GVel() - mul (body->GRot(), Vector(-vref*sin(lng),0,vref*cos(lng))));
	grs = GRS.length();
	return true;
}
#endif

bool Instrument_Surface::IAS (const CelestialBody *body, double &ias) const
{
	if (sp && atmc && body->Type() == OBJTP_PLANET && atm_p > as_plimit && atm_M >= 0.0) {
		Planet *planet = (Planet*)body;
		double p0 = atm_p * pow (atm_M*atm_M*0.5*(atmc->gamma-1.0) + 1.0, atmc->gamma/(atmc->gamma-1.0));
		ATMPARAM ap;
		planet->GetAtmParam (0, lng, lat, &ap);
		double as = ((Planet*)body)->AtmSoundSpeed (ap.T);
		ias = as * sqrt (2.0/(atmc->gamma-1.0) * (pow ((p0-atm_p)/atmc->p0+1.0,(atmc->gamma-1.0)/atmc->gamma) - 1.0));
		return true;
	} else {
		ias = 0.0;
		return false;
	}
}

void Instrument_Surface::CopyToHUD () const
{
	g_pane->SetHUDMode (HUD_SURFACE);
	// TO DO: copy surface reference once this is made selectable
}

void Instrument_Surface::UpdateDraw (oapi::Sketchpad *skp)
{
	static char spdstr[4][4] = {"GS","TAS","IAS","OS"};

	//double dt = SimT - updT + instrDT; // last step interval
	double v, dt = td.SimT1 - psimt;
	double pspd = spd;
	bool spd_valid = false;
	psimt = td.SimT1;

	char cbuf[32];

	if (sp) {

		strcpy (title+9, sp->ref->Name());

		vacc = (sp->vspd-vspd)/dt;
		vspd = sp->vspd;
		alt = sp->alt0;
		dir   = sp->dir;
		pitch = sp->pitch;
		bank  = sp->bank;
		aoa   = -atan2 (sp->airvel_ship.y, sp->airvel_ship.z)*DEG;
		vessel->AtmPressureAndDensity (atm_p, atm_rho);
		if (!vessel->AtmTemperature(atm_T)) atm_T = -1.0;
		if (!vessel->MachNumber(atm_M))     atm_M = -1.0;
		// equatorial position
		vlng = (sp->lng-lng)/dt; lng = sp->lng;
		vlat = (sp->lat-lat)/dt; lat = sp->lat;

	} else {
		strcpy (title+9, "N/A");
	}

	switch (spdmode) {
	case 1: // ground-relative speed
		if (sp) {
			spd = sp->groundspd;
			spd_valid = true;
		} else {
			spd = 0.0;
			spd_valid = false;
		}
		break;
	case 2: // true airspeed
		if (sp && atm_p > as_plimit) {
			spd = sp->airspd;
			spd_valid = true;
		} else {
			spd = 0.0;
			spd_valid = false;
		}
		break;
	case 3: // indicated airspeed
		spd_valid = IAS (vessel->ProxyBody(), spd);
		break;
	case 4: // orbital speed
		spd = (vessel->GVel()-vessel->ProxyBody()->GVel()).length();
		spd_valid = true;
		break;
	}
	acc = (spd_valid ? (spd-pspd)/dt : 0.0);

	int x = cw/2, y = (3*ch)/2, dy = ch;
	int x0, y0, x1, y1, x2;
	DisplayTitle (skp, title);
	if (!sp) return;

	y = hrzy1+ch;
	x0 = accx0+6*cw;
	x1 = x0 + 8*cw;
	x2 = aoax0-cw/2-1;
	skp->SetFont (GetDefaultFont (1));
	skp->Line (cw/2, y-1, IW-cw/2, y-1);
	skp->Line (x0-cw/2, y-1, x0-cw/2, IW-ch/2);
	skp->Line (x2, y-1, x2, IW-ch/2);
	skp->Line (x0-cw/2, y+(7*ch)/2, x2, y+(7*ch)/2);
	skp->Text (x0, y, "ATM DATA", 8);
	skp->Text (x0, y+ch, "OAT", 3);
	skp->Text (x0, y+2*ch, "M", 1);
	skp->Text (x1, y, "DNS", 3);
	skp->Text (x1, y+ch, "STP", 3);
	skp->Text (x1, y+2*ch, "DNP", 3);
	skp->Text (x0, y+4*ch, "EQU POS", 7);
	skp->Text (x1, y+4*ch, "RATE", 4);

	skp->SetTextColor (draw[0][0].col);
	x0 += (7*cw)/2;

	// temperature
	if (atm_T >= 0 && atm_rho >= 1e-7)
		sprintf (cbuf, "%03.0fK", atm_T);
	else
		strcpy (cbuf, "N/A");
	skp->Text (x0, y+ch, cbuf, strlen (cbuf));

	// Mach number
	if (atm_M >= 0 && atm_rho >= 1e-7)
		sprintf (cbuf, "%0.2f", atm_M);
	else
		strcpy (cbuf, "N/A");
	skp->Text (x0, y+2*ch, cbuf, strlen (cbuf));

	x1 += (7*cw)/2;
	// ambient density
	sprintf (cbuf, "%0.3f", atm_rho);
	skp->Text (x1, y, cbuf, strlen(cbuf));

	// static pressure
	sprintf (cbuf, "%sPa", DistStr(atm_p));
	skp->Text (x1, y+ch, cbuf+1, strlen(cbuf+1));

	// dynamic pressure [kg/m/s^2]
	if (vessel->DynPressure (v)) sprintf (cbuf, "%sPa", DistStr(v));
	else strcpy (cbuf, " N/A");
	skp->Text (x1, y+2*ch, cbuf+1, strlen (cbuf+1));

	// position data -> move to Map MFD
	y = hrzy1+6*ch;
	x0 = accx0+6*cw; x1 = x0 + 8*cw; x = x0 + 4*cw;
	if (lng >= 0.0)  sprintf (cbuf, "%07.3lfº E", Deg(lng));
	else             sprintf (cbuf, "%07.3lfº W", Deg(-lng));
	skp->Text (x0, y, cbuf, strlen(cbuf));
	if (vlng >= 0.0) sprintf (cbuf, "[%0.4lfº/s E]", Deg(vlng));
	else             sprintf (cbuf, "[%0.4lfº/s W]", Deg(-vlng));
	skp->Text (x1, y, cbuf, strlen(cbuf)); y += ch;
	if (lat >= 0.0)  sprintf (cbuf, "%07.3lfº N", Deg(lat));
	else             sprintf (cbuf, "%07.3lfº S", Deg(-lat));
	skp->Text (x0, y, cbuf, strlen(cbuf));
	if (vlat >= 0.0) sprintf (cbuf, "[%0.4lfº/s N]", Deg(vlat));
	else             sprintf (cbuf, "[%0.4lfº/s S]", Deg(-vlat));
	skp->Text (x1, y, cbuf, strlen(cbuf)); y += ch;

	// heading indicator
	skp->SetFont (GetDefaultFont (3));
	sprintf (cbuf, "%03d", (int)(DEG*sp->dir+0.5));
	y1 = hrzy0-ch-2, y0 = y1-ch-2;
	skp->SetBrush (brush[2]);
	skp->Rectangle (hrzx-2*cw, y0, hrzx+2*cw+1, y1);
	skp->SetTextAlign (oapi::Sketchpad::CENTER);
	skp->Text (hrzx, y0, cbuf, 3);
	skp->Line (hrzx, y1, hrzx, hrzy0-2);

	// ground speed indicator
	x = spdx0+(21*cw)/8;
	skp->Polygon (spdpt, 7);
	if (spd_valid && spd < 1e6) sprintf (cbuf, "%0.*f", spd < 10 ? 2 : spd < 100 ? 1 : 0, spd);
	else strcpy (cbuf, "----");
	skp->Text (x, hrzy-ch/2-1, cbuf, strlen(cbuf));

	// altitude indicator
	x = altx0+(29*cw)/8;
	bool radaralt = (sp->alt < 1e4);
	double alt = (radaralt ? sp->alt : sp->alt0);
	double altkm = alt*1e-3;
	double altkm0 = fmod (altkm, 1e5);
	skp->Polygon (altpt, 7);
	sprintf (cbuf, "%0.*f", altkm0 < 100 ? 2 : altkm0 < 1000 ? 1 : 0, altkm0);
	skp->Text (x, hrzy-ch/2-1, cbuf, strlen(cbuf));

	// labels
	skp->SetFont (GetDefaultFont (1));
	x = spdx0+(6*cw)/2;
	skp->Text (x, hrzy0-(7*ch)/4, spdstr[spdmode-1], strlen(spdstr[spdmode-1]));
	skp->Text (x, hrzy0-ch, "m/s", 3);

	x = altx0+3*cw;
	skp->Text (x, hrzy0-(7*ch)/4, radaralt ? "ALT-R" : "ALT", radaralt ? 5 : 3);
	skp->Text (x, hrzy0-ch, "km", 2);
	if (altkm >= 1e5) {
		sprintf (cbuf, "+%0.1fMkm", (altkm-altkm0+1)*1e-6);
		skp->Text (x, hrzy1, cbuf, strlen(cbuf));
	}

	// vspeed indicator
	skp->Polygon (vsipt, 7);
	double avspd = fabs(vspd);
	sprintf (cbuf, "%+0.*f", avspd < 10 ? 2 : avspd < 100 ? 1 : 0, vspd);
	x = vspx0+(25*cw)/8;
	skp->Text (x, hrzy-(3*ch)/8-1, cbuf, strlen(cbuf));
	x = vspx0+(5*cw)/2;
	skp->Text (x, hrzy0-(7*ch)/4, "VS", 2);
	skp->Text (x, hrzy0-ch, "m/s", 3);

	// vacc indicator
	x = vacx0 + (5*cw)/2;
	double avacc = fabs(vacc);
	skp->Polygon (vacpt, 7);
	sprintf (cbuf, "%0.*f", avacc < 10 ? 2 : avacc < 100 ? 1 : 0, vacc);
	skp->Text (vacx0+(25*cw)/8, accy-(3*ch)/8-1, cbuf, strlen(cbuf));
	skp->Text (x, accy0-(7*ch)/4, "VACC", 4);
	skp->Text (x, accy0-ch, "m/s²", 4);

	// aoa indicator
	x = aoax0+2*cw;
	skp->Polygon (aoapt, 7);
	sprintf (cbuf, "%0.*f", aoa < 10 ? 1 : 0, aoa);
	skp->Text (aoax0+(21*cw)/8, accy-(3*ch)/8-1, cbuf, strlen(cbuf));
	skp->Text (x, accy0-(7*ch)/4, "AOA", 3);
	skp->Text (x, accy0-ch, "deg", 3);

	// acceleration indicator
	x = accx0 + (5*cw)/2;
	skp->Polygon (accpt, 7);
	if (spd_valid) sprintf (cbuf, "%0.*f", acc < 10 ? 2 : acc < 100 ? 1 : 0, acc);
	else strcpy (cbuf, "----");
	skp->Text (accx0+(15*cw)/8, accy-(3*ch)/8-1, cbuf, strlen(cbuf));
	skp->Text (x, accy0-(7*ch)/4, "ACC", 3);
	skp->Text (x, accy0-ch, "m/s²", 4);

	// pitch and bank values
	skp->SetTextAlign (oapi::Sketchpad::RIGHT);
	sprintf (cbuf, "BNK %03.0fº%c", DEG*fabs(bank), bank >= 0 ? 'L':'R');
	skp->Text (hrzx1, hrzy1, cbuf, 9);
	skp->SetTextAlign (oapi::Sketchpad::LEFT);
	sprintf (cbuf, "PTCH %+03.0fº", DEG*pitch);
	skp->Text (hrzx0, hrzy1, cbuf, 9);

}

void Instrument_Surface::SetSize (const Spec &spec)
{
	int i, x0, x1, y0, y1, ym, gap = (3*cw)/4;

	spdx0 = cw;
	hrzx0 = spdx0 + 6*cw + gap;
	hrzr  = (IW - 19*cw - 3*gap)/2;
	hrzx  = hrzx0 + hrzr;
	hrzx1 = hrzx + hrzr;
	hrzy0 = (7*ch)/2;
	hrzy  = hrzy0 + hrzr;
	hrzy1 = hrzy + hrzr;
	altx0 = hrzx1 + gap;
	vspx0 = altx0 + 6*cw + gap;
	accx0 = spdx0;
	vacx0 = vspx0;
	aoax0 = vacx0 - 4*cw - gap;
	accy0 = hrzy1 + (11*ch)/4;
	accr  = (IH - ch/2 - accy0)/2;
	accy  = accy0 + accr;
	diry0 = hrzy0 - (3*ch)/2-2;

	for (i = 0; i < 12; i++) {
		double alpha = i*Pi2/12.0;
		bnkmarkx[i] = hrzr + (int)((hrzr-2)*cos(alpha));
		bnkmarky[i] = hrzr - (int)((hrzr-2)*sin(alpha));
	}

	hdgtick = (hrzr+3)/4;
	hdgbmpw = hdgtick*44+1;
	hdgbmph = (3*ch)/2;

	x0 = spdx0-cw/2;  x1 = spdx0+(21*cw)/4;
	y0 = hrzy-ch/2-1; y1 = hrzy+ch/2+2; ym = (y0+y1)/2;
	spdpt[0].x = x0;      spdpt[0].y = y0;
	spdpt[1].x = x1;      spdpt[1].y = y0;
	spdpt[2].x = x1;      spdpt[2].y = ym-cw/2;
	spdpt[3].x = x1+cw/2; spdpt[3].y = ym;
	spdpt[4].x = x1;      spdpt[4].y = ym+cw/2;
	spdpt[5].x = x1;      spdpt[5].y = y1;
	spdpt[6].x = x0;      spdpt[6].y = y1;

	x0 = altx0+cw*6+cw/2; x1 = altx0+(3*cw)/4;
	altpt[0].x = x0;      altpt[0].y = y0;
	altpt[1].x = x1;      altpt[1].y = y0;
	altpt[2].x = x1;      altpt[2].y = ym-cw/2;
	altpt[3].x = x1-cw/2; altpt[3].y = ym;
	altpt[4].x = x1;      altpt[4].y = ym+cw/2;
	altpt[5].x = x1;      altpt[5].y = y1;
	altpt[6].x = x0;      altpt[6].y = y1;

	x0 = vspx0+cw*5+cw/2; x1 = vspx0+(3*cw)/4;
	vsipt[0].x = x0;      vsipt[0].y = y0;
	vsipt[1].x = x1;      vsipt[1].y = y0;
	vsipt[2].x = x1;      vsipt[2].y = ym-cw/2;
	vsipt[3].x = x1-cw/2; vsipt[3].y = ym;
	vsipt[4].x = x1;      vsipt[4].y = ym+cw/2;
	vsipt[5].x = x1;      vsipt[5].y = y1;
	vsipt[6].x = x0;      vsipt[6].y = y1;

	x0 = accx0-cw/2;  x1 = accx0+(17*cw)/4;
	y0 = accy-ch/2-1; y1 = accy+ch/2+2; ym = (y0+y1)/2;
	accpt[0].x = x0;      accpt[0].y = y0;
	accpt[1].x = x1;      accpt[1].y = y0;
	accpt[2].x = x1;      accpt[2].y = ym-cw/2;
	accpt[3].x = x1+cw/2; accpt[3].y = ym;
	accpt[4].x = x1;      accpt[4].y = ym+cw/2;
	accpt[5].x = x1;      accpt[5].y = y1;
	accpt[6].x = x0;      accpt[6].y = y1;

	x0 = vacx0+cw*5+cw/2; x1 = vacx0+(3*cw)/4;
	vacpt[0].x = x0;      vacpt[0].y = y0;
	vacpt[1].x = x1;      vacpt[1].y = y0;
	vacpt[2].x = x1;      vacpt[2].y = ym-cw/2;
	vacpt[3].x = x1-cw/2; vacpt[3].y = ym;
	vacpt[4].x = x1;      vacpt[4].y = ym+cw/2;
	vacpt[5].x = x1;      vacpt[5].y = y1;
	vacpt[6].x = x0;      vacpt[6].y = y1;

	x0 = aoax0+cw*4+cw/2; x1 = aoax0+(3*cw)/4;
	aoapt[0].x = x0;      aoapt[0].y = y0;
	aoapt[1].x = x1;      aoapt[1].y = y0;
	aoapt[2].x = x1;      aoapt[2].y = ym-cw/2;
	aoapt[3].x = x1-cw/2; aoapt[3].y = ym;
	aoapt[4].x = x1;      aoapt[4].y = ym+cw/2;
	aoapt[5].x = x1;      aoapt[5].y = y1;
	aoapt[6].x = x0;      aoapt[6].y = y1;
}

void Instrument_Surface::OptionChanged(DWORD cat, DWORD item)
{
	if (cat == OPTCAT_INSTRUMENT) {
		switch (item) {
		case OPTITEM_INSTRUMENT_MFDUPDATEINTERVAL:
			instrDT = min(0.25, g_pOrbiter->Cfg()->CfgLogicPrm.InstrUpdDT);
			break;
		}
	}
}

bool Instrument_Surface::ReadParams (ifstream &ifs)
{
	char cbuf[256], *pc;
	int n = 0;
	if (!FindScnHeader (ifs)) return false;
	for (;;) {
		if (!ifs.getline (cbuf, 256)) return false;
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_MFD", 7)) break;
		if (!_strnicmp (pc, "SPDMODE", 7)) {
			n = sscanf (pc+7, "%d", &spdmode);
			if (spdmode <= 0 || spdmode > 4) spdmode = 1;
		}
	}
	return true;
}

void Instrument_Surface::WriteParams (ostream &ofs) const
{
	ofs << "  TYPE Surface" << endl;
	ofs << "  SPDMODE " << spdmode << endl;
}

