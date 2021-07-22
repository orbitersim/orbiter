// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "MfdTransfer.h"
#include "Pane.h"
#include "Celbody.h"
#include "Psys.h"
#include "Log.h"
#include "Select.h"
#include "Orbiter.h"
#include <dinput.h>

using namespace std;

extern TimeData td;
extern PlanetarySystem *g_psys;
extern InputBox *g_input;
extern Select *g_select;

// =======================================================================
// class Instrument_Transfer

struct Instrument_Transfer::SavePrm Instrument_Transfer::saveprm = {0,0,0,0,0.0,0.0,false,false};

Instrument_Transfer::Instrument_Transfer (Pane *_pane, int _id, const Spec &spec, Vessel *_vessel)
: Instrument (_pane, _id, spec, _vessel)
{
	if (_vessel == saveprm.usr) {
		src     = (saveprm.src ? saveprm.src : _vessel);
		elref   = (saveprm.elref ? saveprm.elref : src->ElRef());
		enable_hyp = saveprm.enable_hyp;
		enable_num = saveprm.enable_num;
		deltav     = saveprm.deltav;
		l_eject    = saveprm.l_eject;
		if (saveprm.tgt && saveprm.tgt->ElRef() == elref)
			tgt = saveprm.tgt;
		else
			tgt = 0;
	} else {
		src   = _vessel;
		elref = _vessel->ElRef();
		enable_hyp = false;
		enable_num = false;
		deltav  = 0.0;
		l_eject = 0.0;
		tgt     = 0;
	}
	shpel = new Elements(); TRACENEW
	shpel2 = new Elements(); TRACENEW
	if (elref) {
		shpel->Set (*src->Els());
		shpel->Calculate (src->GPos()-elref->GPos(), src->GVel()-elref->GVel(), td.SimT0);
		shpel2->Set (*shpel);
	}
	tbdown = tbpress = -1e10;
	hto_a = 0.0;
	nstep = 2000; // should be variable
	step_scale = 10.0;
	path = new Vector[nstep]; TRACENEW
	pathp = new oapi::IVECTOR2[100]; TRACENEW
	process_num = false;

	SetSize (spec);
}

Instrument_Transfer::~Instrument_Transfer ()
{
	// save status
	saveprm.usr        = vessel;
	saveprm.src        = src;
	saveprm.elref      = elref;
	saveprm.tgt        = tgt;
	saveprm.enable_hyp = enable_hyp;
	saveprm.enable_num = enable_num;
	saveprm.deltav     = deltav;
	saveprm.l_eject    = l_eject;

	delete shpel;
	delete shpel2;
	delete []path;
	delete []pathp;
}

HELPCONTEXT *Instrument_Transfer::HelpTopic () const
{
	extern HELPCONTEXT DefHelpContext;
	DefHelpContext.topic = "/mfd_transfer.htm";
	return &DefHelpContext;
}

void Instrument_Transfer::SetSize (const Spec &spec)
{
	pixrad = (spec.w*4)/9;
	ICNTX  = spec.w/2;
	ICNTY  = spec.h/2;
}

bool Instrument_Transfer::Update (double upDTscale)
{
	if (enable_num && process_num)
		process_num = CalcStep();
	return Instrument::Update(upDTscale);
}

void Instrument_Transfer::UpdateDraw (oapi::Sketchpad *skp)
{
	char cbuf[128];
	int y, x0 = cw/2, x1 = IW-12*cw, y0 = 1+(ch*3)/2;
	double scale; // scaling factor for orbit display
	const Elements *tgtel, *workel;
	Vector sp, sv;
	Matrix rot2, *workr;
	oapi::IVECTOR2 pt[ELN+5], p0;
	double p1, p2, A, B, C, d, p, q, arg, sinr, cosr, rlng;
	double myta, ea, rma, cma, escal, T0, dt0, dte, tanm, tlng;
	bool bValid, bTarget;

	DisplayTitle (skp, "Transfer");
	skp->SetTextColor (draw[0][0].col);
	y = y0;

	// sanity checks
	if (elref) {
		sp.Set (src->GPos()-elref->GPos());	// ship in planet coords
		sv.Set (src->GVel()-elref->GVel());
		shpel->Calculate (sp, sv, td.SimT1);
		bValid = true;
	} else bValid = false;
	if (bValid) {
		skp->SetTextColor (draw[2][1].col);
		sprintf (cbuf, "[Ref: %s]", elref->Name());
		skp->Text (cw/2+cw*9, 1, cbuf, strlen(cbuf));
		skp->SetTextColor (draw[0][0].col);
	} else {
		if (blink) skp->Text (x0, y, "Select reference (Shift-R)", 26);
		return;
	}
	bTarget = (tgt && tgt->ElRef() == elref && (tgtel = tgt->Els()) != 0);
	if (enable_hyp) {
		//adelta = (bTarget && tgtel->a > shpel->a ? tgtel->a : shpel->a) * 0.005;
		if (dv_manip) {
			CalcElements (shpel, shpel2, l_eject, 0);
			hto_a = shpel2->a;
		} else
			deltav = CalcElements (shpel, shpel2, l_eject, hto_a);
	}
	dv_manip = false;
	scale = pixrad / (!bTarget || shpel->ApDist() > tgtel->ApDist() ? shpel->ApDist() : tgtel->ApDist());
	Matrix irot (IRotMatrix (shpel->cost, shpel->sint, shpel->cosi, shpel->sini));

	// ship orbit
	Matrix rot1 (RotMatrix (shpel->coso, shpel->sino, shpel->cost, shpel->sint, shpel->cosi, shpel->sini));
	rot1.premul (irot);
	UpdateOrbitGraph (ICNTX, ICNTY, IW, IH, scale, shpel, rot1, irot, pt);
	skp->SetPen (draw[0][1].solidpen);
	DisplayOrbit (skp, pt);
	MapScreen (ICNTX, ICNTY, pixrad/shpel->Radius(), mul (irot, shpel->RVec()), &p0);
	skp->Line (ICNTX, ICNTY, p0.x, p0.y);

	// ship -> src location indicator
	if (src != vessel) {
		int x, y;
		Vector dp (mul (irot, vessel->GPos() - src->GPos()));
		double r = _hypot (dp.x, dp.z);
		x = (int)(IW*0.1/r*dp.x);
		y = (int)(IW*0.1/r*dp.z);
		MapScreen (ICNTX, ICNTY, scale, mul (irot, shpel->RVec()), &p0);
		skp->Line (p0.x, p0.y, p0.x+x, p0.y-y);
	}

	// target orbit
	if (bTarget) {
		rot2.Set (RotMatrix (tgtel->coso, tgtel->sino, tgtel->cost, tgtel->sint, tgtel->cosi, tgtel->sini));
		rot2.premul (irot);
		UpdateOrbitGraph (ICNTX, ICNTY, IW, IH, scale, tgtel, rot2, irot, pt);
		skp->SetPen (draw[1][1].solidpen);
		DisplayOrbit (skp, pt);
		MapScreen (ICNTX, ICNTY, pixrad/tgtel->Radius(), mul (irot, tgtel->RVec()), &p0);
		skp->Line (ICNTX, ICNTY, p0.x, p0.y);
	}

	// transfer orbit
	if (enable_hyp) {
		Matrix rot3 (RotMatrix (shpel2->coso, shpel2->sino, shpel2->cost, shpel2->sint, shpel2->cosi, shpel2->sini));
		rot3.premul (irot);
		UpdateOrbitGraph (ICNTX, ICNTY, IW, IH, scale, shpel2, rot3, irot, pt);
		skp->SetPen (draw[0][1].dashpen);
		DisplayOrbit (skp, pt);
		MapScreen (ICNTX, ICNTY, pixrad/shpel2->Radius(), mul (irot, shpel2->RVec()), &p0);
		skp->Line (ICNTX, ICNTY, p0.x, p0.y);
		workel = shpel2;
		workr  = &rot3;
	} else {
		workel = shpel;
		workr  = &rot1;
	}

	// simulated orbit parameters
	if (enable_hyp) {
		myta = shpel->TrueAnm();                                        // calc ship time to ejection point
		escal = sqrt ((1.0-shpel->e)/(1.0+shpel->e));
		ea  = 2.0 * atan (escal * tan (0.5*(l_eject-shpel->omegab)));   // ship's eccentric anomaly at ref. axis
		rma = ea - shpel->e * sin(ea);                                  // ship's mean anomaly at ref. axis
		ea  = 2.0 * atan (escal * tan (0.5*myta));                      // ship's current eccentric anomaly
		cma = ea - shpel->e * sin(ea);                                  // ship's current mean anomaly
		T0  = shpel->OrbitT();                                          // ship's orbit period
		dte = T0/Pi2 * (rma-cma);                                       // ship's time to ref. axis
		if (dte < 0.0) dte += T0; else if (dte >= T0) dte -= T0;
	}

	// find intersection of current or simulated orbit with target orbit
	dt0 = -1.0;
	if (bTarget) {
		//double domega = tgtel->ArgPer() - workel->ArgPer(); // angle between periapses
		double domega = tgtel->omegab - workel->omegab;
		p1 = workel->a * (1.0-workel->e*workel->e);
		p2 = tgtel->a * (1.0-tgtel->e*tgtel->e);
		A = workel->e/p1 - tgtel->e/p2 * cos(domega);
		B = -tgtel->e/p2 * sin(domega);
		C = 1.0/p1 - 1.0/p2;
		d = A*A + B*B;
		p = 2.0*B*C/d;
		q = (C*C-A*A)/d;
		arg = p*p*0.25-q;
		if (arg >= 0) { // intersections exist
			double s = sqrt (arg);
			sinr = -0.5*p+s;
			rlng = asin(sinr);
			cosr = cos(rlng);
			// resolve ambiguity in sine
			if (fabs (A*cosr + B*sinr + C) > fabs (-A*cosr + B*sinr + C)) rlng = Pi-rlng, cosr = -cosr;
			Vector v(mul (*workr, Vector(cosr, 0.0, sinr)));
			skp->SetPen (draw[2][1].solidpen);
			skp->Line (ICNTX, ICNTY, ICNTX+(int)(v.x*pixrad), ICNTY-(int)(v.z*pixrad));

			// time from ref (current TrL or ejection point) to target orbit intersect
			escal = sqrt ((1.0-workel->e)/(1.0+workel->e));
			myta = (enable_hyp ? l_eject : shpel->TrueLng()) - workel->omegab; // ship's true anomaly at ref
			ea = 2.0 * atan (escal * tan (0.5*rlng));                          // ship's eccentric anomaly at tgt intersect
			rma = ea - workel->e * sin(ea);                                    // ship's mean anomaly at tgt intersect
			ea  = 2.0 * atan (escal * tan (0.5*myta));                         // ship's eccentric anomaly at ref
			cma = ea - workel->e * sin(ea);                                    // ship's mean anomaly at ref
			T0  = workel->OrbitT();                                            // ship's transit orbit period
			dt0 = T0/Pi2 * (rma-cma);                                          // ship's transit time to tgt intersect
			if (dt0 < 0.0) dt0 += T0; else if (dt0 >= T0) dt0 -= T0;
			if (enable_hyp) dt0 += dte;

			// target postition at intersect time
			tanm = tgtel->TrueAnomaly (tgtel->MeanAnomaly (td.SimT1+dt0));
			tlng = tanm + tgtel->omegab;
			v.Set (mul (rot2, Vector (cos(tanm), 0.0, sin(tanm))));
			skp->SetPen (draw[1][1].dashpen);
			skp->Line (ICNTX, ICNTY, ICNTX+(int)(v.x*pixrad), ICNTY-(int)(v.z*pixrad));
		} else {
			if (blink) skp->Text ((IW-cw*15)/2, IH/2, "No intersection", 15);
		}
	}

	y = y0;
	sprintf (cbuf, "Src %s", src == vessel ? "[self]" : src->Name());
	skp->Text (x0, y, cbuf, strlen(cbuf)); y += ch;
	sprintf (cbuf, "TrL% 7.2f�", DEG*shpel->TrueLng());
	skp->Text (x0, y, cbuf, strlen(cbuf)); y += ch;
	if (!enable_hyp && dt0 >= 0.0) {
		sprintf (cbuf, "TLi% 7.2f�", DEG*posangle(rlng+shpel->omegab));
		skp->Text (x0, y, cbuf, strlen(cbuf)); y += ch;
		sprintf (cbuf, "DTi%s", DistStr (dt0));
		skp->Text (x0, y, cbuf, strlen(cbuf)); y += ch;
	}
	
	// simulated orbit parameters
	if (enable_hyp) {
		y += ch;
		skp->Text (x0, y, "HTO", 3); y += ch;
		sprintf (cbuf, "SMa%s", DistStr (shpel2->a));          // semi-major axis
		skp->Text (x0, y, cbuf, strlen(cbuf)); y += ch;
		sprintf (cbuf, "TLe% 7.2f�", DEG*l_eject);             // longitude of ejection point
		skp->Text (x0, y, cbuf, strlen(cbuf)); y += ch;
		sprintf (cbuf, "DTe%s", DistStr(dte));                 // time to ejection point
		skp->Text (x0, y, cbuf, strlen(cbuf)); y += ch;
		sprintf (cbuf, "Dv %s", DistStr (deltav));             // Dvel at ejection point
		skp->Text (x0, y, cbuf, strlen(cbuf)); y += ch;
		if (dt0 >= 0.0) {
			sprintf (cbuf, "TLi% 7.2f�", DEG*posangle(rlng+shpel2->omegab));
			skp->Text (x0, y, cbuf, strlen(cbuf)); y += ch;
			sprintf (cbuf, "DTi%s", DistStr (dt0));
			skp->Text (x0, y, cbuf, strlen(cbuf)); y += ch;
		}
	}


	skp->SetTextColor (draw[1][0].col); y = y0;
	if (bTarget) {
		strcpy (cbuf, "Tgt "); strcat (cbuf, tgt->Name());
		skp->Text (x1, y, cbuf, strlen(cbuf)); y += ch;
		sprintf (cbuf, "TrL% 7.2f�", DEG*tgtel->TrueLng());
		skp->Text (x1, y, cbuf, strlen(cbuf)); y += ch;
		if (dt0 >= 0.0) sprintf (cbuf, "TLi% 7.2f�", DEG*tlng);
		else            sprintf (cbuf, "TLi  N/A");
		skp->Text (x1, y, cbuf, strlen(cbuf)); y += ch;
	} else {
		strcpy (cbuf, "Tgt N/A");
		skp->Text (x1, y, cbuf, strlen(cbuf)); y += ch;
	}
	skp->SetTextColor (draw[0][0].col);

	// numerical trajectory
	if (enable_num) {
		int i, ii, np = step_curr/10;
		for (i = 0; i < 100; i++) {
			if ((ii = (i*nstep)/100) >= step_curr) break;
			MapScreen (ICNTX, ICNTY, scale, mul (irot, path[ii]), pathp+i);
		}
		skp->SetPen (draw[1][0].solidpen);
		skp->Polyline (pathp, i);
		y = y0+ch*4;
		skp->SetTextColor (draw[1][0].col);
		skp->Text (x1, y, "Num orbit", 9); y += ch;
		sprintf (cbuf, "Stp %d", step_curr);
		skp->Text (x1, y, cbuf, strlen(cbuf)); y += ch;
		sprintf (cbuf, "T  %s", DistStr (step_t-step_0));
		skp->Text (x1, y, cbuf, strlen(cbuf)); y += ch;
	}

	if (bTarget) {
		// normals of the two orbital planes
		Vector nm1 = crossp (sv, sp);
		Vector nm2 = crossp (tgt->GVel()-elref->GVel(), tgt->GPos()-elref->GPos());
		nm1.unify();
		nm2.unify();
		// relative inclination between ship's and target's orbital planes
		double reli = xangle (nm1, nm2);

		// relative inclination ship orbit <-> target orbit
		skp->SetTextColor (draw[reli < RAD*1.0 ? 0:1][1].col);
		sprintf (cbuf, "RInc%7.2f�", Deg(reli));
		skp->Text (x0, IH-(3*ch)/2, cbuf, strlen(cbuf));
	}
}

void Instrument_Transfer::DisplayOrbit (oapi::Sketchpad *skp, oapi::IVECTOR2 *p) const
{
	if (p[ELN+2].x != -1) { // closed orbit
		// ellipse
		skp->Polygon (p, ELN);
		// apoapsis
		//oapi::IVECTOR2 &pa = p[ELN+2];
		//skp->Ellipse (pa.x-3, pa.y-3, pa.x+4, pa.y+4);
	} else {                // open orbit
		// hyperbola
		skp->Polyline (p, ELN-1);
	}
}

double Instrument_Transfer::CalcElements (const Elements *el1, Elements *el2, double lng, double a)
{
	// first calculate pos & vel of orbit el1 at ejection point
	Vector P, V;
	double ta = l_eject - el1->omegab; // true anomaly at ejection point
	el1->PosVel_TA (P, V, ta);

	// add thrust
	double dv;
	if (a) { // rescale deltav so as to maintain a
		double ip2 = 2.0/P.length();
		dv = sqrt (el1->Mu() * (ip2 - 1.0/a)) - sqrt (el1->Mu() * (ip2 - 1.0/el1->a));
		V *= 1.0 + dv/V.length();
	} else { // rescale a so as to maintain deltav
		dv = deltav;
		V *= 1.0 + dv/V.length();
	}

	// calculate new elements
	el2->Calculate (P, V, td.SimT1);
	return dv;
}

bool Instrument_Transfer::CalcStep ()
{
	Vector refpos, a0, a1, a2, v1, v2;
	double tstep, tstep_i2, tstep_i6, tb, tc;

	a0 = g_psys->GaccAt (step_t, step_gpos, src);
	tstep = step_scale/a0.length();
	tstep_i2 = tstep*0.5;
	tstep_i6 = tstep/6.0;
	tb = step_t + tstep_i2;
	tc = step_t + tstep;
	v1 = step_gvel + a0*tstep_i2;
	a1 = g_psys->GaccAt (tb, step_gpos + step_gvel*tstep_i2, src);
	v2 = step_gvel + a1*tstep_i2;
	a2 = g_psys->GaccAt (tb, step_gpos + v1*tstep_i2, src);
	step_gpos += (step_gvel + (v1+v2)*2.0 + step_gvel + a2*tstep)*tstep_i6;
	step_gvel += (a0 + (a1+a2)*2.0 + g_psys->GaccAt (tc, step_gpos+v2*tstep, src))*tstep_i6;
	if (elref->Type() == OBJTP_PLANET)
		((Planet*)elref)->PositionAtTime (tc, &refpos);
	path[step_curr] = step_gpos-refpos;
	step_t = tc;
	return ++step_curr < nstep;
}

bool Instrument_Transfer::InitNumTrajectory (const Elements *el)
{
	Vector refpos, refvel, pos, vel;
	step_t = step_0 = td.SimT0;
	if (elref->Type() == OBJTP_PLANET) 
		if (!((Planet*)elref)->PosVelAtTime (td.SimT0, &refpos, &refvel))
			return false;
	el->PosVel (pos, vel, td.SimT0);
	path[0] = pos;
	step_gpos = pos+refpos;
	step_gvel = vel+refvel;
	step_curr = 1;
	process_num = true;
	return true;
}

bool Instrument_Transfer::KeyBuffered (DWORD key)
{
	switch (key) {
	case DIK_F:  // decrease step scale for numerical trajectory
		step_scale *= 0.5;
		return true;
	case DIK_G:  // increase step scale for numerical trajectory
		step_scale *= 2.0;
		return true;
	case DIK_M:  // toggle numerical trajectory
		enable_num = !enable_num;
		if (enable_num) InitNumTrajectory (enable_hyp ? shpel2 : shpel);
		Refresh();
		return true;
	case DIK_N:  // deselect target
		tgt = 0;
		Refresh();
		return true;
	case DIK_R:  // select reference
		OpenSelect_CelBody ("Transfer MFD: Reference", ClbkEnter_Ref);
		return true;
	case DIK_S:  // select source
		OpenSelect_Tgt ("Transfer MFD: Source", ClbkEnter_Src, elref, 0);
		//g_input->Open ("Enter source orbit body:", 0, 20, Instrument_Transfer::ClbkName_Src, (void*)this);
		return true;
	case DIK_T:  // select target
		OpenSelect_Tgt ("Transfer MFD: Target", ClbkEnter_Tgt, elref, 0);
		return true;
	case DIK_U:  // update numerical trajectory
		if (enable_num) InitNumTrajectory (enable_hyp ? shpel2 : shpel);
		Refresh();
		return true;
	case DIK_X:  // toggle HTO orbit
		enable_hyp = !enable_hyp;
		Refresh();
		return true;
	case DIK_Z:  // change number of trajectory steps
		g_input->Open ("Enter # time steps:", 0, 20, Instrument_Transfer::ClbkNstep, (void*)this);
		return true;
	}
	return false;
}

bool Instrument_Transfer::KeyImmediate (char *kstate)
{
	if (KEYDOWN (kstate, DIK_COMMA)) {
		if (BufKey (DIK_COMMA, 0.1)) {
			l_eject = posangle (l_eject-RAD);
			dv_manip = true;
			Refresh();
		}
		return true;
	}
	if (KEYDOWN (kstate, DIK_PERIOD)) {
		if (BufKey (DIK_PERIOD, 0.1)) {
			l_eject = posangle (l_eject+RAD);
			dv_manip = true;
			Refresh();
		}
		return true;
	}
	if (KEYDOWN (kstate, DIK_MINUS)) {
		if (BufKey (DIK_MINUS, 0.1)) {
			double da = 1.0;
			if (td.SysT0-tbpress < 0.2)
				da = (td.SysT0-tbdown < 1.0 ? 1.0:10.0);
			else
				tbdown = td.SysT0;
			tbpress = td.SysT0;
			deltav -= da;
			dv_manip = true;
			Refresh();
		}
		return true;
	}
	if (KEYDOWN (kstate, DIK_EQUALS)) {
		if (BufKey (DIK_EQUALS, 0.1)) {
			double da = 1.0;
			if (td.SysT0-tbpress < 0.2)
				da = (td.SysT0-tbdown < 1.0 ? 1.0:10.0);
			else
				tbdown = td.SysT0;
			tbpress = td.SysT0;
			deltav += da;
			dv_manip = true;
			Refresh();
		}
		return true;
	}
	return false;
}

bool Instrument_Transfer::ProcessButton (int bt, int event)
{
	static const DWORD btkey[14] = { DIK_R, DIK_S, DIK_T, DIK_N, DIK_X, DIK_M, DIK_U, DIK_Z,
		DIK_COMMA, DIK_PERIOD, DIK_MINUS, DIK_EQUALS, DIK_F, DIK_G };
	if (event & PANEL_MOUSE_LBDOWN) {
		if (bt < 8 || bt >= 12) return KeyBuffered (btkey[bt]);
	} else if (event & PANEL_MOUSE_LBPRESSED) {
		if (bt >= 8 && bt < 12) return KeyImmediate (KstateSet (btkey[bt]));
	}
	return false;
}

const char *Instrument_Transfer::BtnLabel (int bt) const
{
	static const char *label[14] = { "REF", "SRC", "TGT", "NT", "HTO", "NUM", "UPD", "STP",
		"EJ-", "EJ+", "DV-", "DV+", "ST-", "ST+" };
	return (bt < 14 ? label[bt] : 0);
}

int Instrument_Transfer::BtnMenu (const MFDBUTTONMENU **menu) const
{
	static const MFDBUTTONMENU mnu[14] = {
		{"Enter orbit", "reference", 'R'},
		{"Enter source", "('x' for self)", 'S'},
		{"Enter target", 0, 'T'},
		{"Unselect target", 0, 'N'},
		{"Toggle transfer", "orbit", 'X'},
		{"Toggle numerical", "trajectory", 'M'},
		{"Update numerical", "trajectory", 'U'},
		{"Adjust time steps", 0, 'Z'},
		{"Rotate", "eject point", '<'},
		{"Rotate", "eject point", '>'},
		{"Decrease dV", 0, '-'},
		{"Increase dV", 0, '+'},
		{"Decrease step", "scale", 'F'},
		{"Increase step", "scale", 'G'}
	};
	if (menu) *menu = mnu;
	return 14;
}

bool Instrument_Transfer::ClbkEnter_Tgt (Select *menu, int item, char *str, void *data)
{
	Instrument_Transfer *instr = (Instrument_Transfer*)data;
	return instr->SelectTarget (str);
}

bool Instrument_Transfer::ClbkEnter_Ref (Select *menu, int item, char *str, void *data)
{
	Instrument_Transfer* instr = (Instrument_Transfer*)data;
	return instr->SelectRef (str);
}

bool Instrument_Transfer::ClbkEnter_Src (Select *menu, int item, char *str, void *data)
{
	Instrument_Transfer* instr = (Instrument_Transfer*)data;
	return instr->SelectSrc (str);
}

bool Instrument_Transfer::ClbkNstep (InputBox*, char *str, void *data)
{
	Instrument_Transfer* instr = (Instrument_Transfer*)data;
	int np;
	if (sscanf (str, "%d", &np) != 1) return false;
	return instr->SetNstep (np);
}

bool Instrument_Transfer::SelectRef (char *str)
{
	CelestialBody *obj = g_psys->GetGravObj (str, true);
	if (!obj) return false;
	if (obj == elref) return true; // nothing to do

	elref = obj;
	shpel->SetMasses (0.0, elref->Mass());
	shpel->Calculate (src->GPos()-elref->GPos(), src->GVel()-elref->GVel(), td.SimT0);
	shpel2->Set (*shpel);
	l_eject = 0.0;
	deltav = 0.0;
	//a_hyp = shpel->a;
	Refresh();
	return true;
}

bool Instrument_Transfer::SelectSrc (char *str)
{
	RigidBody *obj;
	if (!_stricmp (str, "x")) {
		obj = vessel;
	} else {
		obj = (RigidBody*)g_psys->GetObj (str, true);
		if (!obj) return false;
	}
	if (obj == src) return true; // nothing to do

	src = obj;
	shpel->Calculate (src->GPos()-elref->GPos(), src->GVel()-elref->GVel(), td.SimT0);
	shpel2->Set (*shpel);
	l_eject = 0.0;
	deltav = 0.0;
	//a_hyp = shpel->a;
	Refresh();
	return true;
}

bool Instrument_Transfer::SelectTarget (char *str)
{
	RigidBody *body = (RigidBody*)g_psys->GetObj (str, true);
	if (body && body->ElRef() == elref) {
		tgt = body;
		Refresh();
		return true;
	} else return false;
}

bool Instrument_Transfer::SetNstep (int np)
{
	if (np < 2) return false;
	if (np == nstep) return true; // nothing to do
	Vector *path_tmp = new Vector[np]; TRACENEW
	memcpy (path_tmp, path, min (np, nstep)*sizeof(Vector));
	delete []path;
	path = path_tmp;
	if (enable_num && np > nstep)
		process_num = true;
	nstep = np;
	return true;
}

int Instrument_Transfer::ProcessMessage (int msg, void *data)
{
	switch (msg) {
	case MSG_KILLVESSEL:
		if ((Body*)data == tgt) tgt = 0; // current target object was destroyed
		if ((Body*)data == src) SelectSrc (vessel->Name()); // current source object was destroyed
		return 1;
	}
	return 0;
}

bool Instrument_Transfer::ReadParams (ifstream &ifs)
{
	char cbuf[256], *pc;
	char cref[128] = "", ctgt[128] = "", csrc[128] = "";
	double le, dv;
	bool xferprm = false;

	if (!FindScnHeader (ifs)) return false;
	for (;;) {
		if (!ifs.getline (cbuf, 256)) return false;
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_MFD", 7)) break;
		if (!_strnicmp (pc, "REF", 3)) {
			strcpy (cref, trim_string (pc+3));
		} else if (!_strnicmp (pc, "SOURCE", 6)) {
			strcpy (csrc, trim_string (pc+6));
		} else if (!_strnicmp (pc, "TARGET", 6)) {
			strcpy (ctgt, trim_string (pc+6));
		} else if (!_strnicmp (pc, "SIMORBIT", 8)) {
			char flag[32];
			int res = sscanf (pc+8, "%lf%lf%s", &le, &dv, flag);
			enable_hyp = (res == 3 && !_stricmp (flag, "SHOW"));
			xferprm = true;
		}
	}
	if (csrc[0]) SelectSrc (csrc);
	if (cref[0]) SelectRef (cref);
	if (ctgt[0]) SelectTarget (ctgt);
	if (xferprm) l_eject = le, deltav = dv;
	return true;
}

void Instrument_Transfer::WriteParams (ostream &ofs) const
{
	ofs << "  TYPE Transfer" << endl;
	if (elref) ofs << "  REF " << elref->Name() << endl;
	if (src) ofs << "  SOURCE " << src->Name() << endl;
	if (tgt) ofs << "  TARGET " << tgt->Name() << endl;
	ofs << "  SIMORBIT " << l_eject << ' ' << deltav;
	if (enable_hyp) ofs << " SHOW";
	ofs << endl;
}

