// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "MfdDocking.h"
#include "Pane.h"
#include "Psys.h"
#include "Nav.h"

using namespace std;

extern PlanetarySystem *g_psys;
extern TimeData td;
extern Pane *g_pane;
extern InputBox *g_input;
extern Select *g_select;

// =======================================================================
// class Instrument_Docking

struct Instrument_Docking::SavePrm Instrument_Docking::saveprm = {
	0,Instrument_Docking::NAV,0,0,-1
};

Instrument_Docking::Instrument_Docking (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel, bool restore)
: Instrument (_pane, _id, spec, _vessel)
{
	int i;

	nv          =  0;
	Legacy_ref  =  0;
	Legacy_port = -1;
	smode       = NAV;
	refdock     =  0;
	scale       =  0;

	if (gc) {
		brush[0] = gc->clbkCreateBrush (col_green2);
		brush[1] = gc->clbkCreateBrush (col_yellow1);
		brush[2] = gc->clbkCreateBrush (col_red1);
		brush[3] = gc->clbkCreateBrush (RGB(255,255,255));
	} else {
		for (i = 0; i < 4; i++) brush[i] = 0;
	}

	strcpy (title, "Docking: ");
	dst = -1.0;
	SetupVessel ();
	if (restore && vessel == saveprm.usr) { // restore
		smode       = saveprm.smode;
		nv          = saveprm.nv;
		Legacy_ref  = saveprm.Legacy_ref;
		Legacy_port = saveprm.Legacy_port;
	}
	SetSize (spec);
}

Instrument_Docking::~Instrument_Docking ()
{
	// save status
	saveprm.usr         = vessel;
	saveprm.smode       = smode;
	saveprm.nv          = nv;
	saveprm.Legacy_ref  = Legacy_ref;
	saveprm.Legacy_port = Legacy_port;

	int i;
	if (gc) {
		for (i = 0; i < 4; i++)
			if (brush[i]) gc->clbkReleaseBrush (brush[i]);
	}
}

HELPCONTEXT *Instrument_Docking::HelpTopic () const
{
	extern HELPCONTEXT DefHelpContext;
	DefHelpContext.topic = "/mfd_dock.htm";
	return &DefHelpContext;
}

void Instrument_Docking::SetupVessel ()
{
	if (vessel && vessel->ndock && refdock < vessel->ndock) {
		ndock = vessel->ndock;
		// rotation matrix ship local -> ship approach frame
		DirRotToMatrix(vessel->dock[refdock]->dir, vessel->dock[refdock]->rot, dockframe);
	}
}

void Instrument_Docking::SetReference (Vessel *_vessel, DWORD dock)
{
	vessel = _vessel;
	refdock = dock;
	SetupVessel();
	Refresh();
}

int Instrument_Docking::ProcessMessage (int msg, void *data)
{
	struct REFTAG {
		Vessel *v;
		DWORD dock;
	} *ref;

	switch (msg) {
	case 0:
		ref = (REFTAG*)data;
		SetReference (ref->v, ref->dock);
		return 1;
	case MSG_KILLVESSEL:
		if ((Body*)data == Legacy_ref) { // current target object was destroyed
			Legacy_ref = 0;
			Legacy_port = -1;
			smode = NAV;
		}
		return 1;
	}
	return 0;
}

void Instrument_Docking::UpdateDraw (oapi::Sketchpad *skp)
{
	int i, len, rd, x0, x1, y0, y1, xc, yc, x = cw/2, y = ch;
	char cbuf[256], modestr[10] = "";

	strcpy (cbuf, "APPR/DOCK");
	if (ndock > 1) sprintf (cbuf+9, " %d", refdock+1);
	DisplayTitle (skp, cbuf);

	// approach indicator circle
	skp->SetPen (draw[2][1].solidpen);
	for (i = 1; i <= 4; i++) {
		rd = (i*circr)/4;
		skp->Ellipse (circx-rd, circy-rd, circx+rd+1, circy+rd+1);
	}
	skp->Line (circx, circy-circr, circx, circy+circr+1);
	skp->Line (circx-circr, circy, circx+circr+1, circy);

	double rdd = circr * 1.05;
	for (i = -3; i <= 3; i++) {
		double tht = 10.0 * i * RAD;
		double sint = sin(tht), cost = cos(tht);
		x0 = (int)(sint * circr);
		y0 = (int)(cost * circr);
		x1 = (int)(sint * rdd);
		y1 = (int)(cost * rdd);
		skp->Line(circx + x0, circy - y0, circx + x1, circy - y1);
	}

	skp->SetFont (GetDefaultFont (2));
	skp->SetTextAlign (oapi::Sketchpad::CENTER);
	skp->SetTextColor (draw[0][0].col);
	skp->Text (bar0x + barw, bar0-barh/2, "10 ^ x m", 8);
	skp->Text (bar1x + barw, bar0-barh/2, "10 ^ x m/s", 10);
	skp->Text (circx+1, circy-circr/2, "10 ^ x m", 8);
	skp->SetTextColor (draw[1][0].col);
	skp->Text (circx+1, circy+circr/2, "10 ^ x m/s", 10);
	skp->SetTextAlign (oapi::Sketchpad::RIGHT);
	skp->SetFont(GetDefaultFont(1));
	skp->SetBackgroundColor(0);
	skp->SetBackgroundMode(oapi::Sketchpad::BK_OPAQUE);

	for (i = 1; i <= 4; i++) {
		sprintf (cbuf, "%d", i-2-scale);
		skp->Text (circx-cw/8, circy+(i*circr)/4-(ch*4)/10, cbuf, strlen(cbuf));
	}
	skp->SetTextColor (draw[0][0].col);
	for (i = 1; i <= 4; i++) {
		sprintf(cbuf, "%d", i - 1 - scale);
		skp->Text (circx-cw/8, circy-(i*circr)/4-(ch*4)/10, cbuf, strlen(cbuf));
	}

	skp->SetTextColor(draw[2][0].col);
	skp->SetTextAlign(oapi::Sketchpad::CENTER);

	skp->Text(circx, circy - (circr * 116) / 100, "0�", 2);
	if (scale) {
		skp->Text(circx + circr / 2, circy+1, "2�", 2);
		skp->Text(circx + circr, circy+1, "4�", 2);
		skp->Text(circx - (circr * 54) / 100, circy - (circr * 102) / 100, "3�", 2);
		skp->Text(circx + (circr * 54) / 100, circy - (circr * 102) / 100, "3�", 2);
	}
	else {
		skp->Text(circx + circr / 2, circy+1, "10�", 3);
		skp->Text(circx + circr, circy+1, "20�", 3);
		skp->Text(circx - (circr*54)/100, circy - (circr*102)/100, "30�", 3);
		skp->Text(circx + (circr*54)/100, circy - (circr*102)/100, "30�", 3);
	}

	skp->SetTextAlign(oapi::Sketchpad::LEFT);
	skp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT);

	if (scale) {
		skp->SetTextColor(draw[2][0].col);
		skp->Text(cw / 2, IH - ch * 4, "FINE", 4);
	}

	skp->SetFont(GetDefaultFont(0));
	if (!vessel) {
		skp->SetBackgroundMode (oapi::Sketchpad::BK_OPAQUE);
		skp->SetBackgroundColor (col_yellow1);
		skp->SetTextColor (0);
		skp->Text (x, 2*ch, "NO DATA", 7);
		return;
	}

	if (!vessel->ndock) { // sanity check
		skp->SetBackgroundMode (oapi::Sketchpad::BK_OPAQUE);
		skp->SetBackgroundColor (col_yellow1);
		skp->SetTextColor (0);
		skp->Text (x, 2*ch, "Device not available", 20);
		return;
	}
	skp->SetTextColor (draw[0][0].col);

	static oapi::IVECTOR2 pt[3];
	const Body *tgt = 0;
	const PortSpec *ps = 0;
	static PortSpec local_ps;

	double dt = td.SimT1 - updT + instrDT; // last step interval

	switch (smode) {

	case NAV: { // slaved to NAV receiver

		if (nv >= vessel->nnav) return; // sanity check
		NavRadioSpec *nav = vessel->nav+nv;

		sprintf (cbuf, "NAV%d %06.2fMHz", nv+1, nav->freq);
		skp->Text (IW-cw*15, 0, cbuf, strlen (cbuf));

		if (nav->sender) {
			switch (nav->sender->Type()) {
			case TRANSMITTER_IDS: {
				len = nav->sender->IdString (cbuf, 256);
				skp->Text (x, y, cbuf, min (len, 35));
				Nav_IDS *ids = (Nav_IDS*)nav->sender;
				tgt = ids->GetVessel();
				ps  = ids->GetPortSpec();
				strcpy (modestr, " IDS ");
				} break;
			case TRANSMITTER_XPDR: {
				len = nav->sender->IdString (cbuf, 256);
				skp->Text (x, y, cbuf, min (len, 35));
				Nav_XPDR *xpdr = (Nav_XPDR*)nav->sender;
				tgt = xpdr->GetVessel();
				strcpy (modestr, " XPDR ");
				} break;
			}
		}
		} break;

	case VIS: { // video acquisition

		const Vessel *vtgt;
		if (vtgt = vessel->DockMate (refdock)) {
			ps = vtgt->GetDockParams (vessel->dock[refdock]->matedock);
		} else if (vessel->closedock.dist < 100) {
			vtgt = vessel->closedock.vessel;
			ps = vtgt->GetDockParams (vessel->closedock.dock);
		}
		tgt = vtgt;
		strcpy (modestr, " VIS ");
		} break;

	case DIRECT: // direct specification (legacy)

		tgt = Legacy_ref;
		if (Legacy_port >= 0) {
			if (tgt->Type() == OBJTP_VESSEL) {
				ps = ((const Vessel*)tgt)->GetDockParams (Legacy_port);
			}
			strcpy (modestr, " IDS ");
			strcpy (cbuf, "IDS ");
		} else {
			strcpy (modestr, " XPDR ");
			strcpy (cbuf, "XPDR ");
		}
		strncat (cbuf, tgt->Name(), 20);
		skp->Text (x, y, cbuf, strlen(cbuf));

		break;
	}

	// "docked" marker
	if (vessel->DockMate (refdock)) {
		skp->SetBackgroundMode (oapi::Sketchpad::BK_OPAQUE);
		skp->SetBackgroundColor (col_yellow1);
		skp->SetTextColor (0);
		skp->Text (IW - cw * 9, ch, " DOCKED ", 8);
		skp->SetTextColor (draw[0][0].col);
		skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);
	}

	if (!tgt) {
		skp->SetBackgroundMode (oapi::Sketchpad::BK_OPAQUE);
		skp->SetBackgroundColor (col_yellow1);
		skp->SetTextColor (0);
		skp->Text (x, 2*ch, "NO SIGNAL", 9);
		return;
	}

	if (modestr[0]) {
		skp->SetBackgroundMode (oapi::Sketchpad::BK_OPAQUE);
		skp->SetBackgroundColor (col_yellow1);
		skp->SetTextColor (0);
		skp->Text (13*cw, 0, modestr, strlen(modestr));
		skp->SetTextColor (draw[0][0].col);
		skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);
	}

	if (!tgt) return;

	Vector dpos;
	double dstold = dst;

	if (!ps) { // no dock information

		// target object in ship coords
		dpos.Set (tmul (vessel->GRot(), tgt->GPos() - vessel->GPos()));
		dst = dpos.length();

	} else {

		// dock position in ship coords
		dpos.Set (tmul (vessel->GRot(), mul (tgt->GRot(), ps->ref) + tgt->GPos() - vessel->GPos()));
		dst = dpos.dist (vessel->dock[refdock]->ref);
		// dock position in ship approach frame:
		Vector adposold(adpos); // store for velocity calculation
		adpos.Set (mul (dockframe, dpos));
		// dock approach direction in ship coords:
		Vector ddir(tmul(vessel->GRot(), mul(tgt->GRot(), -ps->dir)));
		// dock "up" direction in ship coords:
		Vector drot(tmul(vessel->GRot(), mul(tgt->GRot(), ps->rot)));
		// dock approach direction in ship approach frame:
		Vector addir (mul (dockframe, ddir));
		// dock "up" direction in ship approach frame:
		Vector adrot(mul(dockframe, drot));

		// Euler angles (yaw, pitch, bank) of vessel with respect to correct docking attitude
		double yaw   = -atan2(addir.x, addir.z);
		double pitch = -asin(addir.y / hypot(addir.y, addir.z));
		double bank  = -atan2(adrot.x, adrot.y);

		// dock reference point in ship approach frame
		Vector adockref (mul (dockframe, vessel->dock[refdock]->ref));
		// intersection of approach vector with ship's approach xy plane
		double s = (adockref.z - adpos.z) / addir.z;
		Vector Z (adpos - adockref + addir*s);

		double z = _hypot (Z.x, Z.y);
		double lz = (log10(z)+1.0+scale) * 0.25;
		if (lz < 0.0) {
			x = y = 0;
		} else {
			if (lz > 1.0) lz = 1.0;
			lz *= circr;
			double dir = atan2 (Z.y, Z.x);
			x = (int)(lz*cos(dir));
			y = (int)(lz*sin(dir));
		}

		// approach cone
		double r = 0.0875*dst + 0.3;
		if (r < (scale ? 1e2 : 1e3)) {
			double lr = (log10(r)+1.0+scale) * 0.25;
			rd = (int)(lr*circr);
			skp->SetPen (draw[r>z?0:3][0].solidpen);
			skp->Ellipse (circx-rd, circy-rd, circx+rd+1, circy+rd+1);
		}

		// Linear displacement
		int rlimit = (9*circr)/8, msize = circr/2;
		x0 = x - msize; if (x0 < -rlimit) x0 = -rlimit;
		x1 = x + msize; if (x1 >  rlimit) x1 =  rlimit;
		y0 = y - msize; if (y0 < -rlimit) y0 = -rlimit;
		y1 = y + msize; if (y1 >  rlimit) y1 =  rlimit;
		skp->SetPen (draw[r>z?0:3][0].solidpen);
		skp->Line (circx+x0, circy-y, circx+x1+1, circy-y);
		skp->Line (circx+x, circy-y0, circx+x, circy-y1-1);

		skp->SetPen(draw[0][0].solidpen);
		sprintf (cbuf, "hD %s", DistStr(fabs(Z.x), 4));
		x0 = cw / 2;
		y0 = (ch * 5) / 2;
		skp->Text(x0, y0, cbuf, strlen(cbuf)); // horizontal distance from approach path
		if (fabs(Z.x) > 2e-3)
			DrawArrow(x0 + (cw * 5) / 2, y0, Z.x < 0 ? 3 : 1, skp);
		sprintf(cbuf, "vD %s", DistStr(fabs(Z.y), 4));
		x0 += cw * 11;
		skp->Text(x0, y0, cbuf, strlen(cbuf)); // vertical distance from approach path
		if (fabs(Z.y) > 2e-3)
			DrawArrow(x0 + (cw * 5) / 2, y0, Z.y < 0 ? 2 : 0, skp);

		// Ship's relative velocity projected into ship's approach xy plane
		double dvx = (adposold.x-adpos.x)/dt;
		double dvy = (adposold.y-adpos.y)/dt;
		r = _hypot(dvx, dvy);

		sprintf(cbuf, "hV %s", DistStr(fabs(dvx), 5));
		x0 = cw / 2;
		y0 = (ch * 7) / 2;
		skp->Text(x0, y0, cbuf, strlen(cbuf)); // horizontal velocity relative to approach path
		if (fabs(dvx) > 2e-4)
			DrawArrow(x0 + (cw * 5) / 2, y0, dvx < 0 ? 3 : 1, skp);
		sprintf(cbuf, "vV %s", DistStr(fabs(dvy), 5));
		x0 += cw * 11;
		skp->Text(x0, y0, cbuf, strlen(cbuf)); // horizontal velocity relative to approach path
		if (fabs(dvy) > 2e-4)
			DrawArrow(x0 + (cw * 5) / 2, y0, dvy < 0 ? 2 : 0, skp);

		double r_scaled = (scale ? r*10.0 : r);
		if (r_scaled > 1e-2) {
			double alpha = atan2 (dvy, dvx);
			double sina = sin(alpha), cosa = cos(alpha);
			double s = (double)ch, u = s*0.3;
			double sx = s*cosa, sy = s*sina;
			double lr = (r_scaled < 1e2 ? (log10(r_scaled)+2.0)*0.25 : 1.0) * circr;
			x = circx + (int)(lr*cosa);
			y = circy - (int)(lr*sina);
			skp->SetPen (draw[1][0].solidpen);
			skp->MoveTo (circx, circy);
			skp->LineTo (x, y);
			skp->LineTo (x-(int)(sx+u*sina), y+(int)(sy-u*cosa));
			skp->LineTo (x-(int)(sx-u*sina), y+(int)(sy+u*cosa));
			skp->LineTo (x, y);
		}

		// Angular displacement
		double alpha = acos (addir.z);
		const double amax = RAD*(scale ? 4.0 : 20.0); // displacement at outer instrument radius
		if (fabs(pitch) <= amax && fabs(yaw) <= amax) {
			x = (int)(-yaw/amax*circr);
			y = (int)(-pitch/amax*circr);
			msize = (int)(circr/2.83);
			x0 = x - msize; //if (x0 < -rlimit) x0 = -rlimit;
			x1 = x + msize; //if (x1 >  rlimit) x1 =  rlimit;
			y0 = y - msize; //if (y0 < -rlimit) y0 = -rlimit;
			y1 = y + msize; //if (y1 >  rlimit) y1 =  rlimit;
			skp->SetPen (draw[alpha<RAD*2.5 ? 2:3][0].solidpen);
			skp->Line (circx+x0, circy-y0, circx+x1+1, circy-y1-1);
			skp->Line (circx+x0, circy-y1-1, circx+x1+1, circy-y0);
		}
		skp->SetTextColor (draw[0][0].col);

		// longitudinal rotation alignment (roll)
		if (alpha < RAD*5.0) {
			if (fabs(bank) < RAD*2.5) {
				skp->SetPen (draw[2][0].solidpen);
				skp->SetBrush (brush[3]);
			} else {
				skp->SetPen (draw[3][0].solidpen);
				skp->SetBrush (brush[2]);
			}
			double beta = -bank; // scaled bank angle as displayed by instrument
			if (scale) {
				beta *= 10.0;
				if (beta < -PI) beta = -PI;
				else if (beta > PI) beta = PI;
			}
			pt[0].x = circx + (int)(circr*sin(beta));
			pt[0].y = circy - (int)(circr*cos(beta));
			pt[1].x = circx + (int)((circr-ch)*sin(beta-0.07));
			pt[1].y = circy - (int)((circr-ch)*cos(beta-0.07));
			pt[2].x = circx + (int)((circr-ch)*sin(beta+0.07));
			pt[2].y = circy - (int)((circr-ch)*cos(beta+0.07));
			skp->Polygon (pt, 3);
			skp->SetBrush (0);
		}

		skp->SetPen(draw[0][0].solidpen);
		y0 = IH - (ch*3) / 2;
		x0 = cw / 2;
		double absYdeg = fabs(yaw)*DEG;
		sprintf(cbuf, "Y  %0.1lf", absYdeg);
		skp->Text(x0, y0, cbuf, strlen(cbuf));
		if (absYdeg > 2e-2)
			DrawArrow(x0 + (cw*3)/2, y0, yaw < 0 ? 1 : 3, skp);
		x0 += cw * 11;
		double absPdeg = fabs(pitch)*DEG;
		sprintf(cbuf, "P  %0.1lf", absPdeg);
		skp->Text(x0, y0, cbuf, strlen(cbuf));
		if (absPdeg > 2e-2)
			DrawArrow(x0 + (cw * 3) / 2, y0, pitch < 0 ? 0 : 2, skp);
		x0 += cw * 11;
		double absBdeg = fabs(bank)*DEG;
		sprintf(cbuf, "B  %0.1lf", absBdeg);
		skp->Text(x0, y0, cbuf, strlen(cbuf));
		if (absBdeg > 2e-2)
			DrawArrow(x0 + (cw * 3) / 2, y0, bank < 0 ? 1 : 3, skp);
	}

	// distance bar
	double ldst_min = -1.0-scale;
	double ldst_max =  3.0-scale;
	double ldst = log10 (dst);
	if (ldst > ldst_max) ldst = ldst_max;
	xc = bar0x+barw/2;
	yc = bar0-barh-(3*ch)/2;
	skp->SetPen (draw[2][1].solidpen);
	if (ldst >= ldst_min) {
		y0 = (int)((ldst-ldst_min)/(ldst_max-ldst_min)*barh);
		skp->SetBrush (brush[0]);
		skp->Rectangle (bar0x, bar0-y0, bar0x+barw, bar0+1);
		skp->SetBrush (0);
	}
	skp->Rectangle (bar0x, bar0-barh, bar0x+barw, bar0+1);
	skp->SetTextColor (draw[0][0].col);
	skp->Text (xc-(cw*3)/2, (5*ch)/2, "DST", 3);
	strcpy (cbuf, DistStr (dst)+1); i = strlen (cbuf);
	skp->Text (xc-(cw*i)/2, (7*ch)/2, cbuf, i);
	skp->SetFont(GetDefaultFont(1));
	skp->SetTextAlign(oapi::Sketchpad::RIGHT);
	for (i = 0; i <= 4; i++) {
		y0 = (barh*i)/4;
		skp->Line (bar0x-cw/4, bar0-y0, bar0x+barw, bar0-y0);
		sprintf (cbuf, "%d", i-1-scale);
		skp->Text (bar0x-cw/4, bar0-y0-(ch*4)/10, cbuf, strlen(cbuf));
	}

	// closing velocity bar
	double lvel_min = -2.0-scale;
	double lvel_max =  2.0-scale;
	xc = bar1x + barw/2;
	yc = bar0-barh-(3*ch)/2;
	if (dstold >= 0.0) {
		double cvel = (dstold-dst)/dt;
		double avel = fabs (cvel);
		double lvel = log10 (avel);
		if (lvel >= lvel_min) {
			if (lvel > lvel_max) lvel = lvel_max;
			y0 = (int)((lvel-lvel_min)/(lvel_max-lvel_min)*barh);
			skp->SetBrush (brush[cvel>=0.0?1:0]);
			skp->Rectangle (bar1x, bar0-y0, bar1x+barw, bar0+1);
			skp->SetBrush (0);
		}
		strcpy (cbuf, DistStr (cvel)); 
	} else {
		strcpy (cbuf, "-");
	}
	skp->Rectangle (bar1x, bar0-barh, bar1x+barw, bar0+1);
	skp->SetTextColor (draw[0][0].col);
	skp->SetFont(GetDefaultFont(0));
	skp->SetTextAlign(oapi::Sketchpad::LEFT);
	skp->Text (xc-cw*2, (5*ch)/2, "CVEL", 4);
	i = strlen (cbuf);
	skp->Text (xc-(cw*i)/2, (7*ch)/2, cbuf, i);
	yc = bar0+ch/2;
	skp->SetFont(GetDefaultFont(1));
	skp->SetTextAlign(oapi::Sketchpad::RIGHT);
	for (i = 0; i <= 4; i++) {
		y0 = (barh*i)/4;
		skp->Line (bar1x-cw/4, bar0-y0, bar1x+barw, bar0-y0);
		sprintf (cbuf, "%d", i-2-scale);
		skp->Text (bar1x-cw/4, bar0-y0-(ch*4)/10, cbuf, strlen(cbuf));
	}
	dstold = dst;
}

void Instrument_Docking::DrawArrow(int x0, int y0, int dir, oapi::Sketchpad *skp)
{
	int xr, yr, d = cw/2;
	y0 += (ch * 11) / 20 - cw / 2; // vertical offset of arrow box from top of line
	switch (dir) {
	case 0: // up
		xr = x0 + d, yr = y0;
		skp->Line(xr, yr, xr, yr + cw);
		skp->Line(xr, yr, xr - d, yr + d);
		skp->Line(xr, yr, xr + d, yr + d);
		break;
	case 1: // right
		xr = x0 + cw, yr = y0 + d;
		skp->Line(xr, yr, xr - cw, yr);
		skp->Line(xr, yr, xr - d, yr - d);
		skp->Line(xr, yr, xr - d, yr + d);
		break;
	case 2: // down
		xr = x0 + d, yr = y0 + cw;
		skp->Line(xr, yr, xr, yr - cw);
		skp->Line(xr, yr, xr - d, yr - d);
		skp->Line(xr, yr, xr + d, yr - d);
		break;
	case 3: // left
		xr = x0, yr = y0 + d;
		skp->Line(xr, yr, xr + cw, yr);
		skp->Line(xr, yr, xr + d, yr - d);
		skp->Line(xr, yr, xr + d, yr + d);
		break;
	}
}

bool Instrument_Docking::KeyBuffered (DWORD key)
{
	switch (key) {
	case DIK_D:  // docking port
		SetReference (vessel, (refdock+1)%ndock);
		Refresh();
		return true;
	case DIK_H:  // copy reference to HUD
		CopyToHUD ();
		return true;
	case DIK_N:  // NAV receiver select
		if (smode == NAV) {
			if (vessel->nnav) nv = (nv+1) % vessel->nnav;
		} else {
			smode = NAV;
		}
		Refresh();
		return true;
	case DIK_V: // visual mode
		smode = VIS;
		Refresh();
		return true;
	case DIK_S: // scale resolution
		scale = 1 - scale;
		Refresh();
		return true;
	case DIK_T:  // select target - OBSOLETE
		g_select->Open ("Docking MFD: Target", ClbkSelection_Target, ClbkEnter_Target, (void*)this);
		return true;
	}
	return false;
}

bool Instrument_Docking::ProcessButton (int bt, int event)
{
	static const DWORD btkey[6] = { DIK_D, DIK_N, DIK_V, DIK_T, DIK_S, DIK_H };
	if (event & PANEL_MOUSE_LBDOWN) {
		if (ndock > 1) {
			if (bt < 6) return KeyBuffered (btkey[bt]);
		} else {
			if (bt < 5) return KeyBuffered (btkey[bt+1]);
		}
	}
	return false;
}

const char *Instrument_Docking::BtnLabel (int bt) const
{
	static const char *label[6] = { "DCK", "NAV", "VIS", "TGT", "SCL", "HUD" };
	if (ndock > 1) return (bt < 6 ? label[bt] : 0);
	else           return (bt < 5 ? label[bt+1] : 0);
}

int Instrument_Docking::BtnMenu (const MFDBUTTONMENU **menu) const
{
	static const MFDBUTTONMENU mnu[6] = {
		{"Select docking port", 0, 'D'},
		{"NAV receiver", "select", 'N'},
		{"Visual acquisition", "mode", 'V'},
		{"Select target", 0, 'T'},
		{"Toggle coarse/fine", "scales", 'S'},
		{"Copy data to HUD", 0, 'H'}
	};
	if (menu) *menu = (ndock > 1 ? mnu : mnu+1);
	return (ndock > 1 ? 6 : 5);
}

bool Instrument_Docking::ClbkSelection_Target (Select *menu, int item, char *str, void *data)
{
	DWORD i;
	Instrument_Docking *mfd = (Instrument_Docking*)data;
	Vessel *vessel;

	if (!str) { // main menu
		menu->Append ("By name ...");
		menu->AppendSeparator ();
		for (i = 0; i < g_psys->nVessel(); i++) {
			vessel = g_psys->GetVessel (i);
			if (vessel == mfd->vessel) continue;
			menu->Append (vessel->Name(), vessel->nDock() ? ITEM_SUBMENU : 0);
		}
		return true;
	} else { // submenu
		char cbuf[256];
		if (vessel = g_psys->GetVessel (str, true)) {
			for (i = 0; i < vessel->nDock(); i++) {
				sprintf (cbuf, "%s, dock %d", vessel->Name(), i+1);
				menu->Append (cbuf);
			}
			return true;
		}
		return false;
	}
}

bool Instrument_Docking::ClbkEnter_Target (Select *menu, int item, char *str, void *data)
{
	Instrument_Docking *mfd = (Instrument_Docking*)data;
	if (!strcmp (str, "By name ...")) {
		g_input->Open ("Docking MFD: Enter target (e.g. 'ISS' or 'ISS 1')", 0, 30, CallbackTarget, data);
		return true;
	} else
		return mfd->SetTarget (str);
}

bool Instrument_Docking::CallbackTarget (InputBox*, char *str, void *data)
{
	Instrument_Docking* instr = (Instrument_Docking*)data;
	return instr->SetTarget (str);
}

bool Instrument_Docking::SetTarget (char *str)
{
	char name[256];
	int dock;
	bool bdock = (sscanf (str, "%s%d", name, &dock) == 2);
	int len = strlen(name);
	if (name[len-1] == ',') {
		name[len-1] = '\0';
		bdock = (sscanf (str+len+5, "%d", &dock) == 1);
	}
	Vessel *obj = g_psys->GetVessel (name, true);
	if (!obj) return false;
	if (bdock && (dock < 1 || (DWORD)dock > obj->nDock())) return false;
	Legacy_ref = obj;
	Legacy_port = (bdock ? dock : 0) - 1;
	smode = DIRECT;
	return true;
}

void Instrument_Docking::CopyToHUD () const
{
	g_pane->SetHUDMode (HUD_DOCKING);
	HUD *hud = g_pane->GetHUD();
	if (hud && hud->Mode() == HUD_DOCKING) {
		HUD_Docking *dhud = (HUD_Docking*)hud;
		switch (smode) {
		case NAV:
			dhud->SetNav (nv);
			break;
		case VIS: {
			int port = -1;
			const Vessel *vtgt = vessel->DockMate (refdock);
			if (vtgt) port = vessel->dock[refdock]->matedock;
			else if (vessel->closedock.dist < 100) {
				vtgt = vessel->closedock.vessel;
				port = vessel->closedock.dock;
			}
			if (vtgt)
				dhud->SetReference (vtgt, port);
			} break;
		case DIRECT:
			dhud->SetReference (Legacy_ref, Legacy_port);
			break;
		}
	}
}

void Instrument_Docking::SetSize (const Spec &spec)
{
	circx = spec.w/3;
	circy = spec.h - spec.w/3 - ch*2;
	circr = spec.w/3 - spec.w/40;
	bar0  = circy+circr;
	barh  = spec.h - (15*ch)/2;
	barw  = circr/8;
	bar0x = (IW*9)/12-(barw*3)/2;
	bar1x = (IW*11)/12-barw;
}

bool Instrument_Docking::ReadParams (ifstream &ifs)
{
	char cbuf[256], *pc;
	if (!FindScnHeader (ifs)) return false;
	for (;;) {
		if (!ifs.getline (cbuf, 256)) return false;
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_MFD", 7)) break;
		if (!_strnicmp (pc, "NAV", 3)) {
			smode = NAV;
			sscanf (trim_string (pc+3), "%d", &nv);
		} else if (!_strnicmp (pc, "VIS", 3)) {
			smode = VIS;
		} else if (!_strnicmp (pc, "TARGET", 6)) {
			SetTarget (trim_string (pc+6));
		}
		else if (!_strnicmp(pc, "SCALE", 5)) {
			sscanf(trim_string(pc + 5), "%d", &scale);
		}
	}
	return true;
}

void Instrument_Docking::WriteParams (ostream &ofs) const
{
	ofs << "  TYPE Docking" << endl;
	switch (smode) {
	case NAV:
		ofs << "  NAV " << nv << endl;
		break;
	case VIS:
		ofs << "  VIS" << endl;
		break;
	case DIRECT:
		if (Legacy_ref) {
			ofs << "  TARGET " << Legacy_ref->Name();
			if (Legacy_port >= 0) ofs << ' ' << Legacy_port;
			ofs << endl;
		}
		break;
	}
	if (scale)
		ofs << "SCALE " << scale << endl;
}

