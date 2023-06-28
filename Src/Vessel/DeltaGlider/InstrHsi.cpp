// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// InstrHsi.cpp
// HSI (Horizonal situation indicator) for the Delta-Glider
// ==============================================================

#define STRICT 1
#include "InstrHsi.h"
#include "DeltaGlider.h"
#include "meshres_vc.h"
#include "meshres_p0.h"
#include "dg_vc_anim.h"

using std::min;

// ==============================================================

InstrHSI::InstrHSI (VESSEL3 *v): PanelElement (v)
{
	crs = 0.0;
	dev = 0.0;
	nav = NULL;
	navType = TRANSMITTER_NONE;
	memset (&vc_grp, 0, sizeof(GROUPREQUESTSPEC));
}

// ==============================================================

InstrHSI::~InstrHSI ()
{
	if (vc_grp.Vtx) delete []vc_grp.Vtx;
}

// ==============================================================

void InstrHSI::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_BELOW_P0);
	vtxofs = 76;
}

// ==============================================================

void InstrHSI::ResetVC (DEVMESHHANDLE hMesh)
{
	vc_grp.nVtx = 48;
	if (!vc_grp.Vtx) vc_grp.Vtx = new NTVERTEX[vc_grp.nVtx];
	if (oapiGetMeshGroup (hMesh, GRP_HSI_VC, &vc_grp) != 0) { // problems
		delete []vc_grp.Vtx;
		vc_grp.Vtx = 0;
	}
}

// ==============================================================

void InstrHSI::SetCrs (double newcrs)
{
	if (navType != TRANSMITTER_ILS) { // course indicator fixed for ILS approach
		crs = newcrs;
		while (crs < 0.0) crs += PI2;
		while (crs >= PI2) crs -= PI2;
	}
}

// ==============================================================

double InstrHSI::GetCrs () const
{
	return crs;
}

// ==============================================================

void InstrHSI::Redraw (NTVERTEX *Vtx)
{
	int i, j, vofs;
	DWORD tp;
	double yaw = vessel->GetYaw();   if (yaw < 0.0) yaw += PI2;
	double siny = sin(yaw), cosy = cos(yaw);
	double dev = 0.0, brg = 0.0, slope, c, sinc, cosc;

	static double texw = INSTR3D_TEXW, texh = INSTR3D_TEXH;

	NAVHANDLE nv = vessel->GetNavSource (0);
	if (nv) {
		tp = oapiGetNavType(nv);
		if (tp != TRANSMITTER_VOR && tp != TRANSMITTER_ILS)
			nv = NULL;
	}
	if (nv != nav) {
		if (nav = nv) {
			navRef = vessel->GetSurfaceRef();
			navType = tp;
			if (navRef) {
				VECTOR3 npos;
				NAVDATA data;
				double rad;
				oapiGetNavPos (nav, &npos);
				oapiGlobalToEqu (navRef, npos, &navlng, &navlat, &rad);
				oapiGetNavData (nav, &data);
				if (navType == TRANSMITTER_ILS) crs = data.ils.appdir;
			} else nav = NULL;
		} else {
			navType = TRANSMITTER_NONE;
		}
	}

	// hide/show glideslope background and indicator
	static float gs_tu[2] = {(float)((texw-311.5)/texw), (float)((texw-156.5)/texw)};
	vofs = 8;
	for (i = 2; i < 4; i++)
		Vtx[vofs+i].tu = (navType == TRANSMITTER_ILS ? gs_tu[1] : gs_tu[0]);
	vofs = 28;
	static float gi_tu[4] = {(float)((texw-217.5)/texw), (float)((texw-204.5)/texw), (float)((texw-217.5)/texw), (float)((texw-204.5)/texw)};
	static float gi_tu_off = (float)((texw-315.5)/texw);
	for (i = 0; i < 4; i++)
		Vtx[vofs+i].tu = (navType == TRANSMITTER_ILS ? gi_tu[i] : gi_tu_off);

	// apply glideslope
	float yshift = 0.0;
	if (nav) {
		double vlng, vlat, vrad, adist;
		OBJHANDLE hRef = vessel->GetEquPos (vlng, vlat, vrad);
		if (hRef && hRef == navRef) {
			Orthodome (vlng, vlat, navlng, navlat, adist, brg);
			adist *= oapiGetSize (hRef);
			dev = brg-crs;
			if      (dev < -PI) dev += PI2;
			else if (dev >= PI) dev -= PI2;
			if      (dev < -PI05) dev = -PI-dev;
			else if (dev >= PI05) dev =  PI-dev;

			// calculate slope
			if (navType == TRANSMITTER_ILS) {
				double s = adist * cos(crs-brg);
				double alt = vessel->GetAltitude();
				slope = atan2 (alt, s) * DEG;

				// transform glideslope indicator
				const double tgtslope = 4.0;
				double dslope = slope - tgtslope;
				yshift = (float)min(fabs(dslope)*20.0,45.0);
				if (dslope > 0.0) yshift = -yshift;
			}
		}
	}
	static float gs_x[4] = {64.0f,76.5f,64.0f,76.5f};
	static float gs_y[4] = {4.0f,4.0f,-4.0f,-4.0f};
	vofs = 28;
	for (i = 0; i < 4; i++) {
		Vtx[vofs+i].x = gs_x[i];
		Vtx[vofs+i].y = gs_y[i]+yshift;
	}

	// rotate compass rose
	static double xp[4] = {-60.5,60.5,-60.5,60.5};
	static double yp[4] = {60.5,60.5,-60.5,-60.5};
	vofs = 0;
	for (i = 0; i < 4; i++) {
		Vtx[vofs+i].x = (float)(cosy*xp[i] - siny*yp[i]);
		Vtx[vofs+i].y = (float)(siny*xp[i] + cosy*yp[i]);
	}

	// rotate source bearing indicator
	vofs = 12;
	if (nav) {
		c = yaw-brg;
		sinc = sin(c), cosc = cos(c);
		static double xs[4] = {-6.2,6.2,-6.2,6.2};
		static double ys[4] = {61,61,45,45};
		for (i = 0; i < 4; i++) {
			Vtx[i+vofs].x = (float)(cosc*xs[i] - sinc*ys[i]);
			Vtx[i+vofs].y = (float)(sinc*xs[i] + cosc*ys[i]);
		}
	} else { // hide indicator
		for (i = 0; i < 4; i++) {
			Vtx[i+vofs].x = -65.0f;
			Vtx[i+vofs].y = 0.0f;
		}
	}

	// rotate course+deviation indicator and scale
	c = yaw-crs;
	sinc = sin(c), cosc = cos(c);
	static double xd[4] = {-3.65,3.65,-3.65,3.65};
	static double yd[4] = {25.82,25.82,-25.82,-25.82};
	static double xc[12] = {-32.2,32.2,-32.2,32.2,-6.2, 6.2, -6.2,  6.2, 0,0,0,0};
	static double yc[12] = {  4.7, 4.7, -4.7,-4.7,60.5,60.5,-60.5,-60.5, 26.82,26.82,-26.82,-26.82};
	vofs = 16;
	double dx = min(8.0,fabs(dev)*DEG)*5.175;
	if (dev < 0.0) dx = -dx;
	for (i = 0; i < 4; i++) xc[i+8] = xd[i]+dx;
	for (i = 0; i < 12; i++) {
		Vtx[i+vofs].x = (float)(cosc*xc[i] - sinc*yc[i]);
		Vtx[i+vofs].y = (float)(sinc*xc[i] + cosc*yc[i]);
	}

	// course readout
	int icrs = (int)(crs*DEG+0.5) % 360;
	char *cc, cbuf[16];
	sprintf (cbuf, "%03d", icrs);
	vofs = 32;
	static double numw = 10.0, num_ofs = texw-311.0;
	static double tu_num[4] = {0,numw/texw,0,numw/texw};
	for (cc = cbuf, i = 0; i < 3; cc++, i++) {
		double x = ((*cc-'0') * numw + num_ofs)/texw;
		for (j = 0; j < 4; j++)
			Vtx[i*4+j+vofs].tu = (float)(tu_num[j]+x);
	}
}

// ==============================================================

bool InstrHSI::Redraw2D (SURFHANDLE surf)
{
	if (grp) {
		Redraw (grp->Vtx+vtxofs);

		// transform vertices to 2D panel location
		static const float xcnt = 0.5f*PANEL2D_WIDTH+1.0f, ycnt = 473.0f;
		for (int i = 0; i < 32; i++) {
			if (i < 4 || i >= 12) {
				grp->Vtx[i+vtxofs].x += xcnt;
				grp->Vtx[i+vtxofs].y = ycnt - grp->Vtx[i+vtxofs].y;
			}
		}
	}
	return false;
}

// ==============================================================

bool InstrHSI::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	NTVERTEX *Vtx = vc_grp.Vtx;
	if (hMesh && Vtx) {
		Redraw (Vtx);

		// transform vertices to VC location
		static const double rad = 0.06;           // display size param
		static const double ycnt = VC_HSI_ref.y;  // y-position of display centre (x is assumed 0)
		static const double zcnt = VC_HSI_ref.z;  // z-position of display centre
		static const double cosa = VC_HSI_axis.z, sina = VC_HSI_axis.y;
		static const float scale = (float)(rad/108.0);
		double y, z;
		for (int i = 0; i < 32; i++) {
			if (i < 4 || i >= 12) {
				// scale and rotate for bank
				Vtx[i].x *= scale;
				y = Vtx[i].y*scale;
				z = (i < 8 ? 0.0 : i < 20 ? -0.0005 : -0.001);
				// tilt to panel inclination
				Vtx[i].y = (float)(ycnt - y*cosa - z*sina);
				Vtx[i].z = (float)(zcnt + y*sina - z*cosa);
			}
		}

		// write back to mesh group
		GROUPEDITSPEC ges;
		ges.flags = GRPEDIT_VTXCRD | GRPEDIT_VTXTEX;
		ges.nVtx = vc_grp.nVtx;
		ges.Vtx  = vc_grp.Vtx;
		ges.vIdx = 0;
		oapiEditMeshGroup (hMesh, GRP_HSI_VC, &ges);
	}
	return false;
}

// ==============================================================

void InstrHSI::Orthodome (double lng1, double lat1, double lng2, double lat2,
				double &dist, double &dir)
{
	double A     = lng2-lng1;
	double sinA  = sin(A),    cosA  = cos(A);
	double slat1 = sin(lat1), clat1 = cos(lat1);
	double slat2 = sin(lat2), clat2 = cos(lat2);
	double cosa  = slat2*slat1 + clat2*clat1*cosA;
	dist = acos (cosa);
	dir  = asin (clat2*sinA/sin(dist));
	if (lat2 < lat1) dir = PI-dir; // point 2 further south than point 1
	if (dir < 0.0) dir += PI2;     // quadrant 4
}
