// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <dinput.h>
#include "MfdOrbit.h"
#include "Pane.h"
#include "Celbody.h"
#include "Psys.h"
#include "Select.h"
#include "Log.h"

using namespace std;

extern PlanetarySystem *g_psys;
extern TimeData td;
extern Pane *g_pane;
extern InputBox *g_input;
extern Select *g_select;
extern char DBG_MSG[256];

// =======================================================================
// class Instrument_Orbit

struct Instrument_Orbit::SavePrm Instrument_Orbit::saveprm = {
	0, 0, 0,
	Instrument_Orbit::PRJ_FRM,
	Instrument_Orbit::FRM_ECL,
	Instrument_Orbit::DIST_RAD,
	Instrument_Orbit::DISP_BOTH
};

Instrument_Orbit::Instrument_Orbit (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel)
: Instrument (_pane, _id, spec, _vessel)
{
	projmode = saveprm.projmode;
	frmmode  = saveprm.frmmode;
	dstmode  = saveprm.dstmode;
	dispmode = saveprm.dispmode;
	if (_vessel == saveprm.usr) {
		elref = (saveprm.elref ? saveprm.elref : _vessel->ElRef());
		tgt = saveprm.tgt;
	} else {
		elref = _vessel->ElRef();
		tgt = 0;
	}
	shpel = new Elements(); TRACENEW
	tgtel = new Elements(); TRACENEW
	if (elref) {
		shpel->Setup (_vessel->Mass(), elref->Mass(), _vessel->Els()->MJDepoch());
		if (tgt)
			tgtel->Setup (tgt->Mass(), elref->Mass(), _vessel->Els()->MJDepoch());
	}

	SetSize (spec);
	strcpy (title_str, "Orbit: ");
	strcpy (proj_str, "Proj: ");

	// init drawing resources
	if (gc) {
		brush[0]    = gc->clbkCreateBrush (col_green2);
		brush[1]    = gc->clbkCreateBrush (col_yellow2);
	} else {
		int i;
		for (i = 0; i < 2; i++) brush[i] = 0;
	}
}

Instrument_Orbit::~Instrument_Orbit ()
{
	int i;
	if (gc) {
		for (i = 0; i < 2; i++)
			if (brush[i]) gc->clbkReleaseBrush (brush[i]);
	}
	delete shpel;
	delete tgtel;

	// save status
	saveprm.usr      = vessel;
	saveprm.elref    = elref;
	saveprm.tgt      = tgt;
	saveprm.projmode = projmode;
	saveprm.frmmode  = frmmode;
	saveprm.dstmode  = dstmode;
	saveprm.dispmode = dispmode;
}

HELPCONTEXT *Instrument_Orbit::HelpTopic () const
{
	extern HELPCONTEXT DefHelpContext;
	DefHelpContext.topic = "/mfd_orbit.htm";
	return &DefHelpContext;
}

bool Instrument_Orbit::KeyBuffered (DWORD key)
{
	switch (key) {
	case DIK_A:  // auto reference
		SelectAutoRef ();
		return true;
	case DIK_D:  // toggle distance display mode
		dstmode = (dstmode == DIST_RAD ? DIST_ALT : DIST_RAD);
		Refresh();
		return true;
	case DIK_F:  // reference frame
		frmmode = (frmmode == FRM_ECL ? FRM_EQU : FRM_ECL);
		Refresh();
		return true;
	case DIK_H:  // copy reference to HUD
		CopyToHUD ();
		return true;
	case DIK_M:  // display mode
		dispmode = (DisplayMode)((((int)dispmode)+1) % 3);
		Refresh();
		return true;
	case DIK_N:  // deselect target
		UnselectTarget ();
		return true;
	case DIK_P:  // projection plane
		projmode = (ProjectionMode)((((int)projmode)+1) % 3);
		if (projmode == PRJ_TGT && !tgt) projmode = PRJ_FRM;
		Refresh();
		return true;
	case DIK_R:  // select reference
		OpenSelect_CelBody ("Orbit MFD: Reference", ClbkEnter_Ref);
		return true;
	case DIK_T:  // select target
		OpenSelect_Tgt ("Orbit MFD: Target", ClbkEnter_Tgt, elref, 0);
		return true;
	}
	return false;
}

bool Instrument_Orbit::ProcessButton (int bt, int event)
{
	static const DWORD btkey[9] = { DIK_R, DIK_A, DIK_T, DIK_N, DIK_M, DIK_F, DIK_P, DIK_D, DIK_H };
	if (event & PANEL_MOUSE_LBDOWN) {
		if (bt < 9) return KeyBuffered (btkey[bt]);
	}
	return false;
}

const char *Instrument_Orbit::BtnLabel (int bt) const
{
	static const char *label[9] = { "REF", "AR", "TGT", "NT", "MOD", "FRM", "PRJ", "DST", "HUD" };
	return (bt < 9 ? label[bt] : 0);
}

int Instrument_Orbit::BtnMenu (const MFDBUTTONMENU **menu) const
{
	static const MFDBUTTONMENU mnu[9] = {
		{"Orbit reference", 0, 'R'},
		{"Auto reference", 0, 'A'},
		{"Select target", 0, 'T'},
		{"Unselect target", 0, 'N'},
		{"Display mode", 0, 'M'},
		{"Reference frame", "ECL/EQU", 'F'},
		{"Projection plane", 0, 'P'},
		{"Distance display", "Radius/Altitude", 'D'},
		{"Copy data to HUD", 0, 'H'}
	};
	if (menu) *menu = mnu;
	return 9;
}

bool Instrument_Orbit::ClbkEnter_Tgt (Select *menu, int item, char *str, void *data)
{
	Instrument_Orbit *instr = (Instrument_Orbit*)data;
	return instr->SelectTarget (str);
}

bool Instrument_Orbit::ClbkEnter_Ref (Select *menu, int item, char *str, void *data)
{
	Instrument_Orbit* instr = (Instrument_Orbit*)data;
	return instr->SelectRef (str);
}

void Instrument_Orbit::SetRef (const CelestialBody *ref)
{
	if (ref && ref != elref) {
		elref = ref;
		shpel->SetMasses (0.0, elref->Mass());
		tgtel->SetMasses (0.0, elref->Mass());
		if (tgt == (const Body*)ref) UnselectTarget();
		Refresh();
	}
}

bool Instrument_Orbit::SelectRef (char *str)
{
	CelestialBody *obj = g_psys->GetGravObj (str, true);
	if (!obj) return false;
	SetRef (obj);
	return true;
}

void Instrument_Orbit::SelectAutoRef ()
{
	SetRef (vessel->ElRef());
}

bool Instrument_Orbit::SelectTarget (char *str)
{
	Body *body = g_psys->GetObj (str, true);
	if (body == (Body*)elref) return false;
	if (body) {
		tgt = body;
		if (elref)
			tgtel->Setup (tgt->Mass(), elref->Mass(), shpel->MJDepoch());
		Refresh();
		return true;
	} else return false;
}

void Instrument_Orbit::UnselectTarget ()
{
	tgt = 0;
	if (projmode == PRJ_TGT) projmode = PRJ_FRM;
	Refresh();
}

void Instrument_Orbit::CopyToHUD () const
{
	g_pane->SetHUDMode (HUD_ORBIT);
	HUD *hud = g_pane->GetHUD();
	if (hud && hud->Mode() == HUD_ORBIT) {
		((HUD_Orbit*)hud)->SetReference (elref);
	}
}

void Instrument_Orbit::DisplayOrbit (oapi::Sketchpad *skp, int which, oapi::IVECTOR2 *p)
{
	skp->SetPen (GetDefaultPen (which));
	if (p[ELN+2].x != -1) { // closed orbit
		// ellipse
		skp->Polygon (p, ELN);
		// apoapsis
		oapi::IVECTOR2 &pa = p[ELN+2];
		skp->Ellipse (pa.x-3, pa.y-3, pa.x+4, pa.y+4);
	} else {                // open orbit
		// hyperbola
		skp->Polyline (p, ELN-1);
	}
	// radius vector
	skp->Line (ICNTX, ICNTY, p[ELN].x, p[ELN].y);
	// periapsis
	oapi::IVECTOR2 &pe = p[ELN+1];
	skp->SetBrush (brush[which]);
	skp->Ellipse (pe.x-3, pe.y-3, pe.x+4, pe.y+4);
	// ascending node
	oapi::IVECTOR2 &asc = p[ELN+3];
	skp->Rectangle (asc.x-3, asc.y-3, asc.x+4, asc.y+4);
	skp->SetBrush (0);
	// descending node
	oapi::IVECTOR2 &desc = p[ELN+4];
	skp->Rectangle (desc.x-3, desc.y-3, desc.x+4, desc.y+4);
	// line of nodes
	skp->SetPen (GetDefaultPen (which, 0, 2));
	skp->Line (asc.x, asc.y, desc.x, desc.y);
}

void Instrument_Orbit::UpdateDraw (oapi::Sketchpad *skp)
{
	static char *projstr[3] = {"Ecliptic", "Ship", "Target"};
	Matrix rot1, rot2, irot;

	bool bValidShpEl = (elref != 0);
	bool bValidTgtEl = (elref && tgt);

	if (bValidShpEl) {
		Vector pos = vessel->GPos()-elref->GPos();
		Vector vel = vessel->GVel()-elref->GVel();
		if (frmmode == FRM_EQU) { // convert to equatorial frame
			pos = tmul (elref->RotObliq(), pos);
			vel = tmul (elref->RotObliq(), vel);
		}
		shpel->Calculate (pos, vel, td.SimT1);
		strcpy (title_str+7, elref->Name());
		strcpy (proj_str+6, projstr[projmode]);
		instable = (shpel->PeDist() < elref->Size());
		scale = pixrad / (shpel->e < 1.0 ? shpel->ApDist() :
						  max (2.0*shpel->PeDist(), shpel->Radius()));
	}
	if (bValidTgtEl) {
		Vector pos = tgt->GPos()-elref->GPos();
		Vector vel = tgt->GVel()-elref->GVel();
		if (frmmode == FRM_EQU) { // convert to equatorial frame
			pos = tmul (elref->RotObliq(), pos);
			vel = tmul (elref->RotObliq(), vel);
		}
		tgtel->Calculate (pos, vel, td.SimT1);
		double scale2 = (tgtel->e < 1.0 ? pixrad / tgtel->ApDist() : scale);
		if (scale2 < scale) scale = scale2;
	} else {
		if (projmode == PRJ_TGT) projmode = PRJ_SHIP;
	}

	if (elref)
		elref_rad = (int)(elref->Size() * scale + 0.5);

	if (dispmode != DISP_LIST) { // draw orbit graphs
		bool eclproj;
		skp->SetTextColor (draw[2][0].col);
		skp->Text (IW-(cw*23)/2, 1, frmmode == FRM_ECL ? "ECL":"EQU", 3);
		skp->Text (IW-(cw*7)/2,  1, projmode == PRJ_SHIP ? "SHP" : projmode == PRJ_TGT ? "TGT" : frmmode == FRM_ECL ? "ECL":"EQU", 3);
		skp->SetTextColor (draw[2][1].col);
		skp->Text (IW - cw*15, 1, "Frm     Prj", 11);
		if (bValidShpEl && projmode == PRJ_SHIP) {
			irot = IRotMatrix (shpel->cost, shpel->sint, shpel->cosi, shpel->sini);
			eclproj = false;
		} else if (bValidTgtEl && projmode == PRJ_TGT) {
			irot = IRotMatrix (tgtel->cost, tgtel->sint, tgtel->cosi, tgtel->sini);
			eclproj = false;
		} else {
			irot.Set (IMatrix());
			eclproj = true;
		}
		skp->SetPen (GetDefaultPen (2, 1));
		skp->Ellipse (ICNTX-elref_rad, ICNTY-elref_rad, ICNTX+elref_rad, ICNTY+elref_rad);
		if (bValidShpEl) {
			rot1 = RotMatrix (shpel->coso, shpel->sino, shpel->cost, shpel->sint, shpel->cosi, shpel->sini);
			if (!eclproj) rot1.premul (irot);
			UpdateOrbitGraph (ICNTX, ICNTY, IW, IH, scale, shpel, rot1, irot, o_pt1);
			DisplayOrbit (skp, 0, o_pt1);
		}
		if (bValidTgtEl) {
			rot2 = RotMatrix (tgtel->coso, tgtel->sino, tgtel->cost, tgtel->sint, tgtel->cosi, tgtel->sini);
			if (!eclproj) rot2.premul (irot);
			UpdateOrbitGraph (ICNTX, ICNTY, IW, IH, scale, tgtel, rot2, irot, o_pt2);
			DisplayOrbit (skp, 1, o_pt2);
		}
	} else {
		skp->SetTextColor (draw[2][0].col);
		skp->Text (IW-(cw*23)/2, 1, frmmode == FRM_ECL ? "ECL":"EQU", 3);
		skp->SetTextColor (draw[2][1].col);
		skp->Text (IW - cw*15, 1, "Frm", 3);
	}

	DisplayTitle (skp, title_str);
	// output elref's contribution to gravity field
	bool dominant;
	double gfrac = g_psys->GetGravityContribution (elref, vessel->GPos(), &dominant);
	char cbuf[20];
	sprintf (cbuf, "G %0.2f", gfrac);
	skp->SetTextColor (draw[!dominant ? 3 : gfrac < 0.0 ? 1 : 0][0].col);
	skp->Text ((IW-cw*6)/2, IH-(3*ch)/2, cbuf, 6);

	if (dispmode != DISP_GRAPH) { // list ship and target orbit parameters
		if (bValidShpEl) {
			skp->SetTextColor (draw[0][0].col);
			skp->Text (cw/2, (ch*3)/2, "--OSC.EL.--", 11);
			DisplayElements (skp, shpel, cw/2, (ch*5)/2);
		}
		if (bValidTgtEl) {
			skp->SetTextColor (draw[1][0].col);
			int len = strlen (tgt->Name());
			if (len >= 11)
				strncpy (cbuf, tgt->Name(), 11);
			else {
				memset (cbuf, '-', 11);
				strncpy (cbuf+(11-len)/2, tgt->Name(), len);
			}
			skp->Text (IW-(cw*23)/2, (ch*3)/2, cbuf, 11);
			DisplayElements (skp, tgtel, IW-(cw*23)/2, (ch*5)/2);
		}
	}
}

void Instrument_Orbit::DisplayElements (oapi::Sketchpad *skp, const Elements *el, int x, int y)
{
	char cbuf[16];
	int dy = ch;
	if (el) {
		if (el->e < 1.0) { // closed orbit
			sprintf (cbuf, "SMa%s", el->a < 1e5*AU ? DistStr (el->a) : " N/A");
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "SMi%s", el->SMi() < 1e5*AU ? DistStr (el->SMi()) : " N/A");
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			if (dstmode == DIST_RAD) {
				sprintf (cbuf, "PeR%s", DistStr (el->PeDist()));
				skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
				sprintf (cbuf, "ApR%s", el->ApDist() < 1e5*AU ? DistStr (el->ApDist()) : " N/A");
				skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
				sprintf (cbuf, "Rad%s", DistStr (el->Radius()));
				skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			} else {
				double r = elref->Size();
				sprintf (cbuf, "PeA%s", DistStr (el->PeDist()-r));
				skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
				sprintf (cbuf, "ApA%s", el->ApDist()-r < 1e5*AU ? DistStr (el->ApDist()-r) : " N/A");
				skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
				sprintf (cbuf, "Alt%s", DistStr (el->Radius()-r));
				skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			}
			sprintf (cbuf, "Ecc% 0.4f", el->e);
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "T  %s", FloatStr (el->OrbitT()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "PeT%s", FloatStr (el->PeT()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "ApT%s", FloatStr (el->ApT()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "Vel%s", FloatStr (el->Vel()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "Inc% 7.2fº", Deg(el->i));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "LAN% 7.2fº", Deg(el->theta));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "LPe% 7.2fº", Deg(el->omegab));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "AgP% 7.2fº", Deg(el->ArgPer()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "TrA% 7.2fº", Deg(el->TrueAnm()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "TrL% 7.2fº", Deg(el->TrueLng()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "MnA% 7.2fº", Deg(posangle(el->MeanAnm())));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "MnL% 7.2fº", Deg(el->MeanLng()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
		} else { // open orbit
			sprintf (cbuf, "SMa%s", -el->a < 1e5*AU ? DistStr (el->a) : " N/A");
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "SMi%s", -el->SMi() < 1e5*AU ? DistStr (-el->SMi()) : " N/A");
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			if (dstmode == DIST_RAD) {
				sprintf (cbuf, "PeR%s", DistStr (el->PeDist()));
				skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
				skp->Text (x, y, "ApR N/A", 7); y += dy;
				sprintf (cbuf, "Rad%s", DistStr (el->Radius()));
				skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			} else {
				double r = elref->Size();
				sprintf (cbuf, "PeA%s", DistStr (el->PeDist()-r));
				skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
				skp->Text (x, y, "ApA N/A", 7); y += dy;
				sprintf (cbuf, "Alt%s", DistStr (el->Radius()-r));
				skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			}
			if      (el->e < 1e2) sprintf (cbuf, "Ecc% 0.4f", el->e);
			else if (el->e < 1e3) sprintf (cbuf, "Ecc% 0.3f", el->e);
			else if (el->e < 1e4) sprintf (cbuf, "Ecc% 0.2f", el->e);
			else if (el->e < 1e5) sprintf (cbuf, "Ecc% 0.1f", el->e);
			else {
				double e = el->e;
				int eexp = 0;
				while (e >= 10.0) e *= 0.1, eexp += 1;
				sprintf (cbuf, "Ecc% 0.3fe%d", e, eexp);
			}
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			skp->Text (x, y, "T   N/A", 7); y += dy;
			sprintf (cbuf, "PeT%s", DistStr (el->PeT()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			skp->Text (x, y, "ApT N/A", 7); y += dy;
			sprintf (cbuf, "Vel%s", DistStr (el->Vel()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "Inc% 7.2fº", Deg(el->i));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "LAN% 7.2fº", Deg(el->theta));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "LPe% 7.2fº", Deg(el->omegab));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "AgP% 7.2fº", Deg(el->ArgPer()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "TrA% 7.2fº", Deg(el->TrueAnm()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "TrL% 7.2fº", Deg(el->TrueLng()));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			sprintf (cbuf, "MnA%sº", FloatStr (DEG*el->MeanAnm(),4));
			skp->Text (x, y, cbuf, strlen(cbuf)); y += dy;
			skp->Text (x, y, "MnL N/A", 7); y += dy;
		}
	} else {
		skp->Text (x, y, "Orbital", 7); y += dy;
		skp->Text (x, y, "Elements", 8); y += dy;
		skp->Text (x, y, "not", 3); y += dy;
		skp->Text (x, y, "available", 9);
	}
}

int Instrument_Orbit::ProcessMessage (int msg, void *data)
{
	switch (msg) {
	case MSG_KILLVESSEL:
		if ((Body*)data == tgt) {
			tgt = 0; // current target object was destroyed
			if (projmode == PRJ_TGT) projmode = PRJ_FRM;
		}
		return 1;
	}
	return 0;
}

void Instrument_Orbit::SetSize (const Spec &spec)
{
	pixrad = (spec.w*4)/9;
	ICNTX = spec.w/2;
	ICNTY = spec.h/2;
}

bool Instrument_Orbit::ReadParams (ifstream &ifs)
{
	char cbuf[256], *pc;
	char cref[128] = "", ctgt[128] = "", cprj[128] = "";
	dstmode = DIST_RAD;
	if (!FindScnHeader (ifs)) return false;
	for (;;) {
		if (!ifs.getline (cbuf, 256)) return false;
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_MFD", 7)) break;
		if (!_strnicmp (pc, "PROJ", 4)) {
			strcpy (cprj, trim_string (pc+4));
		} else if (!_strnicmp (pc, "ALT", 3)) {
			dstmode = DIST_ALT;
		} else if (!_strnicmp (pc, "REF", 3)) {
			strcpy (cref, trim_string (pc+3));
		} else if (!_strnicmp (pc, "TARGET", 6)) {
			strcpy (ctgt, trim_string (pc+6));
		} else if (!_strnicmp (pc, "FRAME", 5)) {
			pc = trim_string (pc+5);
			if (!_stricmp (pc, "Ecliptic"))
				frmmode = FRM_ECL;
			else if (!_stricmp (pc, "Equator"))
				frmmode = FRM_EQU;
		}
	}
	if (cref[0]) SelectRef (cref);
	if (ctgt[0]) SelectTarget (ctgt);
	if (cprj[0]) {
		if (!_strnicmp (cprj, "Ship", 4))
			projmode = PRJ_SHIP;
		else if (!_strnicmp (cprj, "Target", 6) && tgt)
			projmode = PRJ_TGT;
		else if (!_strnicmp (cprj, "Ecliptic", 8) || !_strnicmp (cprj, "Frame", 5))
			projmode = PRJ_FRM;
	}
	return true;
}

void Instrument_Orbit::WriteParams (ostream &ofs) const
{
	ofs << "  TYPE Orbit" << endl;
	ofs << "  PROJ " <<
		(projmode == PRJ_FRM ? "Frame" :
		 projmode == PRJ_SHIP ? "Ship" : "Target") << endl;
	ofs << "  FRAME " <<
		(frmmode == FRM_ECL ? "Ecliptic" : "Equator") << endl;
	if (dstmode == DIST_ALT)
		ofs << "  ALT" << endl;
	if (elref) ofs << "  REF " << elref->Name() << endl;
	if (tgt) ofs << "  TARGET " << tgt->Name() << endl;
}
