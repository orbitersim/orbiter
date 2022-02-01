// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// Horizon.cpp
// Artificial horizon instrument for the Delta-Glider
// ==============================================================

#define STRICT 1
#include "Horizon.h"
#include "DeltaGlider.h"
#include "meshres_vc.h"
#include "meshres_p0.h"
#include <math.h>

extern GDIParams g_Param;

using namespace std;

// ==============================================================

InstrAtt::InstrAtt (VESSEL3 *v): PanelElement (v)
{
	memset (&vc_grp, 0, sizeof(GROUPREQUESTSPEC));
}

// ==============================================================

InstrAtt::~InstrAtt ()
{
	if (vc_grp.Vtx) delete []vc_grp.Vtx;
}

// ==============================================================

void InstrAtt::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_BELOW_P0);
	vtxofs = 0;
}

// ==============================================================

void InstrAtt::ResetVC (DEVMESHHANDLE hMesh)
{
	vc_grp.nVtx = 80;
	if (!vc_grp.Vtx) vc_grp.Vtx = new NTVERTEX[vc_grp.nVtx];
	if (oapiGetMeshGroup (hMesh, GRP_HORIZON_VC, &vc_grp) != 0) { // problems
		delete []vc_grp.Vtx;
		vc_grp.Vtx = 0;
	}
}

// ==============================================================

void InstrAtt::Redraw (NTVERTEX *Vtx)
{
	int i, j;
	double bank  = vessel->GetBank();
	double pitch = vessel->GetPitch();
	double yaw   = vessel->GetYaw();   if (yaw < 0.0) yaw += PI2;
	double alt   = vessel->GetAltitude(ALTMODE_GROUND);
	double spd   = vessel->GetAirspeed();
	double sinb = sin(bank), cosb = cos(bank);

	static double texw = INSTR3D_TEXW, texh = INSTR3D_TEXH;
	static double scaleh = 900.0, scalew = 154.0;
	static const double horzx2 = (double)(texw-312);

	// transform articfical horizon
	static double pitchscale = 315.0/(texh*PI05);  // texcrd/rad
	static double dy = pitchscale * (PI05/2.0), dy2 = dy*0.5;
	static double scalecnt = (texh-scaleh*0.5)/texh;
	static double xp[12] = {-108.0,-54.0,54.0,108.0,-108.0,-54.0,54.0,108.0,-6,6,-6,6};
	static double yp[12] = {54.0,108.0,108.0,54.0,-54.0,-108.0,-108.0,-54.0,49,49,37,37};
	static double tv[8] = {scalecnt-dy2,scalecnt-dy,scalecnt-dy,scalecnt-dy2,scalecnt+dy2,scalecnt+dy,scalecnt+dy,scalecnt+dy2};

	for (i = 0; i < 12; i++) {
		Vtx[i+vtxofs].x = (float)( cosb*xp[i] + sinb*yp[i]);
		Vtx[i+vtxofs].y = (float)(-sinb*xp[i] + cosb*yp[i]);
		if (i < 8)
			Vtx[i+vtxofs].tv = (float)(tv[i]-pitch*pitchscale);
	}

	// transform compass ribbon
	static double yawrange = 121.0/(double)texh;
	static double yawscale   = 864.0/(texh*PI2);
	static double tv_ofs[4] = {1.0,1.0-yawrange,1.0,1.0-yawrange};
	for (i = 0; i < 4; i++)
		Vtx[i+12+vtxofs].tv = (float)(tv_ofs[i] - yaw*yawscale);

	// speed and altitude readout
	for (int disp = 0; disp < 3; disp++) {
		char *c, *str, cbuf[20] = "";
		int vofs, maxnum;
		switch (disp) {
		case 0: vofs = 16; maxnum = 6; str = DispStr (alt)+1; break;
		case 1: vofs = 40; maxnum = 6; str = DispStr (spd)+1; break;
		case 2: vofs = 64; maxnum = 3;
			    sprintf (cbuf, "%03d", (int)(yaw*DEG+0.5));
				str = cbuf;
				break;
		}
		vofs += vtxofs;
		static double numw = 10.0, num_ofs = texw-311.0;
		static double tu_num[4] = {0,numw/texw,0,numw/texw};
		for (c = str, i = 0; *c && (i < maxnum); c++, i++) {
			if (*c >= '0' && *c <= '9') {
				double x = ((*c-'0') * numw + num_ofs)/texw;
				for (j = 0; j < 4; j++) {
					Vtx[i*4+j+vofs].tu = (float)(tu_num[j]+x);
				}
			} else {
				double ofs;
				switch (*c) {
				case ' ': ofs = horzx2+107.5; break;
				case '.': ofs = horzx2+101.0; break;
				case 'k': ofs = horzx2+116.0; break;
				case 'M': ofs = horzx2+127.0; break;
				case 'G': ofs = horzx2+137.0; break;
				default:  ofs = horzx2+0.0; break;
				}
				if (ofs > 0.0) {
					Vtx[i*4+vofs+1].tu = Vtx[i*4+vofs+3].tu = (float)(numw/texw) +
					(Vtx[i*4+vofs].tu = Vtx[i*4+vofs+2].tu = (float)(ofs/texw));
				}
			}
		}
	}
}

// ==============================================================

bool InstrAtt::Redraw2D (SURFHANDLE surf)
{
	if (grp) {
		Redraw (grp->Vtx);

		// transform vertices to 2D panel location
		static const float xcnt = 0.5f*PANEL2D_WIDTH+1.0f, ycnt = 150.0f;
		for (int i = 0; i < 12; i++) {
			grp->Vtx[i+vtxofs].x += xcnt;
			grp->Vtx[i+vtxofs].y = ycnt - grp->Vtx[i+vtxofs].y;
		}
	}
	return false;
}

// ==============================================================

bool InstrAtt::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	NTVERTEX *Vtx = vc_grp.Vtx;
	if (hMesh && Vtx) {
		Redraw (Vtx);

		// transform vertices to VC location
		static const double rad = 0.055;      // display size param
		static const double tilt = 20.0*RAD;  // display tilt around x-axis
		static const double ycnt = 1.189;     // y-position of display centre (x is assumed 0)
		static const double zcnt = 7.285;     // z-position of display centre
		static const double cosa = cos(tilt), sina = sin(tilt);
		static const float scale = (float)(rad/108.0);
		double y, z;
		for (int i = 0; i < 12; i++) {
			// scale and rotate for bank
			Vtx[i].x *= scale;
			y = Vtx[i].y*scale;
			z = (i < 8 ? -0.0005f : -0.001f);
			// tilt to panel inclination
			Vtx[i].y = (float)(ycnt + y*cosa - z*sina);
			Vtx[i].z = (float)(zcnt + y*sina + z*cosa);
		}

		// write back to mesh group
		GROUPEDITSPEC ges;
		ges.flags = GRPEDIT_VTXCRD | GRPEDIT_VTXTEX;
		ges.nVtx = vc_grp.nVtx;
		ges.Vtx  = vc_grp.Vtx;
		ges.vIdx = 0;
		oapiEditMeshGroup (hMesh, GRP_HORIZON_VC, &ges);
	}
	return false;
}
