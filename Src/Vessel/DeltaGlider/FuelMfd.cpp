// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// FuelMfd.cpp
// Fuel status display
// ==============================================================

#define STRICT 1
#include "FuelMfd.h"
#include "DeltaGlider.h"
#include "ScramSubsys.h"
#include "meshres_p0.h"
#include "meshres_vc.h"

// ==============================================================

// constants for texture coordinates
static const float texw = (float)INSTR3D_TEXW;
static const float texh = (float)INSTR3D_TEXH;
static const float fd_y0 = 395.5f;
static const float fuelh =  86.0f;
static const float fuelw =  28.0f;
static const float fuely = fd_y0+29.5f+fuelh;

// ==============================================================

FuelMFD::FuelMFD (VESSEL3 *v): PanelElement (v)
{
	isScram = false;
	Mmain = Mrcs = Mscram = 0.0;
	memset (&vc_grp, 0, sizeof(GROUPREQUESTSPEC));
}

// ==============================================================

FuelMFD::~FuelMFD ()
{
	if (vc_grp.Vtx) delete []vc_grp.Vtx;
}

// ==============================================================

void FuelMFD::Reset2D (int panelid, MESHHANDLE hMesh)
{
	if (panelid != 0) return;

	DeltaGlider *dg = (DeltaGlider*)vessel;
	isScram = dg->ScramVersion();

	oapiMeshGroup(hMesh, GRP_FUEL_DISP_SCRAM_P0)->UsrFlag = (isScram ? 0 : 3);
	oapiMeshGroup(hMesh, GRP_FUEL_DISP_NOSCRAM_P0)->UsrFlag = (isScram ? 3 : 0);
	grp = oapiMeshGroup (hMesh, isScram ? GRP_FUEL_DISP_SCRAM_P0 : GRP_FUEL_DISP_NOSCRAM_P0);
	vtxofs = 0;
	NTVERTEX *Vtx = grp->Vtx+vtxofs;
	crd_2D[0] = 511.5f;   crd_2D[1] = 425.5f;
	crd_2D[2] =   0.0f;   crd_2D[3] =   0.0f;
	for (int i = 0; i < 9; i++)
		memset(sout[i], 0, 5 * sizeof(char));

	Mmain = dg->GetPropellantMass(dg->ph_main);
	Mrcs = dg->GetPropellantMass(dg->ph_rcs);
	if (isScram) Mscram = dg->SubsysScram()->GetPropellantMass();
}

// ==============================================================

void FuelMFD::ResetVC (DEVMESHHANDLE hMesh)
{
	// NEED TO DO VERTEX TRANSFORMATIONS HERE!

	DeltaGlider *dg = (DeltaGlider*)vessel;
	isScram = dg->ScramVersion();

	vc_grp.nVtx = 20;
	if (!vc_grp.Vtx) vc_grp.Vtx = new NTVERTEX[vc_grp.nVtx];

	GROUPEDITSPEC ges = { GRPEDIT_SETUSERFLAG, (DWORD)(isScram ? 3 : 0), 0, 0, 0 };
	oapiEditMeshGroup(hMesh, GRP_PROPELLANT_STATUS_NOSCRAM_VC, &ges);
	ges.UsrFlag = (isScram ? 0 : 3);
	oapiEditMeshGroup(hMesh, GRP_PROPELLANT_STATUS_SCRAM_VC, &ges);

	grpId = (isScram ? GRP_PROPELLANT_STATUS_SCRAM_VC : GRP_PROPELLANT_STATUS_NOSCRAM_VC);
	if (oapiGetMeshGroup(hMesh, grpId, &vc_grp) != 0) { // problems
		delete[]vc_grp.Vtx;
		vc_grp.Vtx = 0;
	}
	else {
		NTVERTEX *Vtx = vc_grp.Vtx;
		crd_VC[0] = Vtx[8].y;   crd_VC[1] = Vtx[10].y;
		crd_VC[2] = Vtx[8].z;   crd_VC[3] = Vtx[10].z;
	}
	for (int i = 0; i < 9; i++)
		memset(sout[i], 0, 5 * sizeof(char));

	Mmain = dg->GetPropellantMass(dg->ph_main);
	Mrcs = dg->GetPropellantMass(dg->ph_rcs);
	if (isScram) Mscram = dg->SubsysScram()->GetPropellantMass();
}

// ==============================================================

void FuelMFD::Redraw (NTVERTEX *Vtx, SURFHANDLE surf, float crd[4])
{
	DeltaGlider *dg = (DeltaGlider*)vessel;

	static const int xofs = INSTR3D_TEXW-424, yofs = 0;
	double m, m0, lvl, dv, isp;
	float y, z;
	int vofs;
	char cbuf[16];
	double T = oapiGetSimTime();
	double dT = T-Tsample;
	m0 = dg->GetMass();

	// main level
	m = dg->GetPropellantMass (dg->ph_main);
	lvl = m / max (1.0, dg->max_rocketfuel);
	isp = dg->GetThrusterIsp (dg->th_main[0]);
	dv = isp * log(m0/(m0-m));
	//y1 = (float)(fuely - lvl * fuelh);
	y = crd[0] + (float)lvl*(crd[1]-crd[0]);
	z = crd[2] + (float)lvl*(crd[3]-crd[2]);
	vofs = 8;
	Vtx[vofs+2].y = Vtx[vofs+3].y = y;
	Vtx[vofs+2].z = Vtx[vofs+3].z = z;
	sprintf (cbuf, "% 6d", (int)(m+0.5));
	BltString (cbuf+1, sout[0], 5, xofs+42, yofs+78, surf);
	sprintf (cbuf, "% 6d", (int)(dv+0.5));
	BltString (cbuf+1, sout[6], 5, xofs+42, yofs+106, surf);
	if (dT > 0.0) {
		sprintf (cbuf, "% 5.2f", (Mmain-m)/(T-Tsample));
		BltString (cbuf, sout[3], 5, xofs+42, yofs+156, surf);
		Mmain = m;
	}

	// rcs level
	m = dg->GetPropellantMass (dg->ph_rcs);
	lvl = m / RCS_FUEL_CAPACITY;
	isp = ISP;
	dv = isp * log(m0/(m0-m));
	//y1 = (float)(fuely - lvl * fuelh);
	y = crd[0] + (float)lvl*(crd[1]-crd[0]);
	z = crd[2] + (float)lvl*(crd[3]-crd[2]);
	vofs = 12;
	Vtx[vofs+2].y = Vtx[vofs+3].y = y;
	Vtx[vofs+2].z = Vtx[vofs+3].z = z;
	sprintf (cbuf, "% 6d", (int)(m+0.5));
	BltString (cbuf+1, sout[1], 5, xofs+134, yofs+78, surf);
	sprintf (cbuf, "% 6d", (int)(dv+0.5));
	BltString (cbuf+1, sout[7], 5, xofs+134, yofs+106, surf);
	if (dT > 0.0) {
		sprintf (cbuf, "% 5.2f", (Mrcs-m)/(T-Tsample));
		BltString (cbuf, sout[4], 5, xofs+134, yofs+156, surf);
		Mrcs = m;
	}

	if (isScram) {
		// scram level
		m = dg->SubsysScram()->GetPropellantMass ();
		lvl = m / max (1.0, dg->SubsysScram()->GetPropellantMaxMass());
		isp = dg->SubsysScram()->GetThrusterIsp (0);
		dv = isp * log(m0/(m0-m));
		//y1 = (float)(fuely - lvl * fuelh);
		y = crd[0] + (float)lvl*(crd[1]-crd[0]);
		z = crd[2] + (float)lvl*(crd[3]-crd[2]);
		vofs = 16;
		Vtx[vofs+2].y = Vtx[vofs+3].y = y;
		Vtx[vofs+2].z = Vtx[vofs+3].z = z;
		sprintf (cbuf, "% 6d", (int)(m+0.5));
		BltString (cbuf+1, sout[2], 5, xofs+226, yofs+78, surf);
		sprintf (cbuf, "% 6d", (int)(dv+0.5));
		BltString (cbuf+1, sout[8], 5, xofs+226, yofs+106, surf);
		if (dT > 0.0) {
			sprintf (cbuf, "% 5.2f", (Mscram-m)/(T-Tsample));
			BltString (cbuf, sout[5], 5, xofs+226, yofs+156, surf);
			Mscram = m;
		}
	}
	Tsample = T;
}

// ==============================================================

bool FuelMFD::Redraw2D (SURFHANDLE surf)
{
	Redraw (grp->Vtx+vtxofs, surf, crd_2D);
	return false;
}

// ==============================================================

bool FuelMFD::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	if (hMesh && surf) {
		Redraw (vc_grp.Vtx, surf, crd_VC);
		GROUPEDITSPEC ges = {GRPEDIT_VTXCRDY|GRPEDIT_VTXCRDZ, 0, vc_grp.Vtx, vc_grp.nVtx, 0};
		oapiEditMeshGroup (hMesh, grpId, &ges);
	}
	return false;
}

// ==============================================================

void FuelMFD::BltString (char *str, char *pstr, int maxlen, int x, int y, SURFHANDLE surf)
{
	int i, xsrc, xofs = INSTR3D_TEXW-293, ysrc = 1;
	char *c = str;
	for (i = 0; i < maxlen && *c; i++, c++) {
		if (*c != pstr[i]) {
			if (*c >= '0' && *c <= '9') {
				xsrc = xofs+(*c-'0')*8;
			} else switch(*c) {
				case '.': xsrc = xofs+80; break;
				default:  xsrc = xofs+88; break;
			}
			oapiBlt (surf, surf, x, y, xsrc, ysrc, 7, 9);
			pstr[i] = *c;
		}
		x += 7;
	}
}