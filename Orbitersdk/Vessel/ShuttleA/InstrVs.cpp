// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2011 Martin Schweiger
//                   All rights reserved
//
// InstrVs.cpp
// Vertical speed tape instrument for the ShuttleA
// ==============================================================

#define STRICT 1
#include "InstrVs.h"
#include "ShuttleA.h"

const int label_srcx = 103;
const int wheel_srcx = 111;
const float vtape_xcnt = 694.0f, vtape_ycnt = 76.0f;
const float atape_xcnt = 626.0f, atape_ycnt = 76.0f;
const float stape_xcnt = 550.0f, stape_ycnt = 76.0f;

// ==============================================================

InstrSpd::InstrSpd (VESSEL3 *v): PanelElement (v)
{
	psmin = 100000; // invalidate
}

// ==============================================================

void InstrSpd::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	const DWORD texw = PANELEL_TEXW, texh = PANELEL_TEXH;
	const float tapex0 = 68.0f, tapew = 32.0f;
	const float tapey0 = (float)(texh-422), tapeh = 333.0f;

	const DWORD NVTX = 22;
	const DWORD NIDX = 30;

	static NTVERTEX VTX[NVTX] = {
		// Spd tape
		{stape_xcnt-16,stape_ycnt-48,0,  0,0,0,  tapex0/(float)texw,        0},
		{stape_xcnt+16,stape_ycnt-48,0,  0,0,0,  (tapex0+tapew)/(float)texw,0},
		{stape_xcnt-16,stape_ycnt+48,0,  0,0,0,  tapex0/(float)texw,        0},
		{stape_xcnt+16,stape_ycnt+48,0,  0,0,0,  (tapex0+tapew)/(float)texw,0},
		// Spd*1000 readouts
		{stape_xcnt-39,stape_ycnt-7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{stape_xcnt-31,stape_ycnt-7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{stape_xcnt-39,stape_ycnt+7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{stape_xcnt-31,stape_ycnt+7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{stape_xcnt-31,stape_ycnt-7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{stape_xcnt-23,stape_ycnt-7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{stape_xcnt-31,stape_ycnt+7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{stape_xcnt-23,stape_ycnt+7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{stape_xcnt-23,stape_ycnt-7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{stape_xcnt-15,stape_ycnt-7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{stape_xcnt-23,stape_ycnt+7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{stape_xcnt-15,stape_ycnt+7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		// apoapsis/periapsis velocity bugs
		{stape_xcnt+16,stape_ycnt   ,0,  0,0,0,  77.5f/(float)texw, (float)(texh-13.5f)/(float)texh},
		{stape_xcnt+ 5,stape_ycnt   ,0,  0,0,0,  65.0f/(float)texw, (float)(texh-13.5f)/(float)texh},
		{stape_xcnt+16,stape_ycnt-11,0,  0,0,0,  77.5f/(float)texw, (float)(texh-26)/(float)texh},
		{stape_xcnt+16,stape_ycnt   ,0,  0,0,0,  77.5f/(float)texw, (float)(texh-12.5f)/(float)texh},
		{stape_xcnt+ 5,stape_ycnt   ,0,  0,0,0,  65.0f/(float)texw, (float)(texh-12.5f)/(float)texh},
		{stape_xcnt+16,stape_ycnt+11,0,  0,0,0,  77.5f/(float)texw, (float)(texh)/(float)texh}
	};
	static WORD IDX[NIDX] = {
		0,1,2, 3,2,1,
		4,5,6, 7,6,5,
		8,9,10, 11,10,9,
		12,13,14, 15,14,13,
		16,17,18,  19,21,20
	};

	AddGeometry (hMesh, grpidx, VTX, NVTX, IDX, NIDX);
}

// ==============================================================

bool InstrSpd::Redraw2D (SURFHANDLE surf)
{
	double spd = vessel->GetGroundspeed()*0.1;
	double vspd, aspd = fabs(spd);

	static double texw = PANELEL_TEXW, texh = PANELEL_TEXH;
	static double scalecnt = texh-422.0+152.0;
	static int scaleunit = 15;
	static double viewh = 50.0;
	double ycnt, y0, y1, dy, ddy;
	char *c, cbuf[12];
	bool centered = (fabs(spd) <= 4.0);

	dy = spd-floor(spd);
	if (centered) {
		ycnt = scalecnt - spd*scaleunit;
	} else {
		if (spd > 0.0) ycnt = scalecnt - (5.0+dy)*scaleunit;
		else           ycnt = scalecnt + (5.0-dy)*scaleunit;
	}
	y0 = ycnt-viewh;
	y1 = ycnt+viewh;
	grp->Vtx[0+vtxofs].tv = grp->Vtx[1+vtxofs].tv = (float)(y0/texh);
	grp->Vtx[2+vtxofs].tv = grp->Vtx[3+vtxofs].tv = (float)(y1/texh);

	// copy labels onto scale
	const int labelx = 70;
	int i, j, n, smin, smax, iy, len, ysrc;
	int s0 = (int)floor(spd);
	smin = s0-3;
	if (smin != psmin) {
		psmin = smin;
		smax = smin+7;
		for (i = smin; i <= smax; i++) {
			sprintf (cbuf, "%03d", abs((i%100)*10));
			len = 3;
			if (centered) {
				if (!i) continue;
				iy = (int)scalecnt-i*scaleunit-5;
			} else {
				if (i > 0) iy = (int)scalecnt-(2+i-smin)*scaleunit-5;
				else       iy = (int)scalecnt+(8-i+smin)*scaleunit-5;
			}
			for (j = 0, c = cbuf; j < 3; c++, j++) {
				n = *c-'0';
				ysrc = (int)texh-265+n*8;
				if (i < 0) ysrc += 88;
				oapiBlt (surf, surf, labelx+j*6, iy, label_srcx, ysrc, 6, 8);
			}
		}
	}

#ifdef UNDEF
	// apoapsis/periapsis altitude indicators
	OBJHANDLE hRef = vessel->GetSurfaceRef();
	if (hRef) {
		double rad = oapiGetSize (hRef);
		double apalt, pealt;
		vessel->GetApDist(apalt); apalt -= rad; apalt *= 1e-3;
		vessel->GetPeDist(pealt); pealt -= rad; pealt *= 1e-3;

		float yofs;
		if (apalt < alt+4 && apalt > alt-4)
			yofs = (float)((apalt-alt)*scaleunit*(48.0/50.0));
		else yofs = -60;
		static const float y0[3] = {atape_ycnt, atape_ycnt, atape_ycnt-11};
		for (i = 0; i < 3; i++)
			grp->Vtx[vtxofs+i+16].y = y0[i]-yofs;

		if (pealt < alt+4 && pealt > alt-4)
			yofs = (float)((pealt-alt)*scaleunit*(48.0/50.0));
		else yofs = -60;
		static const float y1[3] = {atape_ycnt, atape_ycnt, atape_ycnt+11};
		for (i = 0; i < 3; i++)
			grp->Vtx[vtxofs+i+19].y = y1[i]-yofs;
	}
#endif

	// km*1000 indicator wheels
	sprintf (cbuf, "%06d", (((int)aspd)%100000)*10);
	for (i = 0; i < 3; i++) {
		float yofs = (float)(texh-111 - (cbuf[i]-'0')*15);
		if (aspd > 50.0) {
			const double scl[3] = {1e4,1e3,1e2};
			vspd = aspd/scl[i];
			ddy = (vspd-floor(vspd))*scl[i];
			// number dials in rotation phase
			if (ddy < 0.5) yofs += (float)((0.5-ddy)*15.0);
			else if (ddy > scl[i]-0.5) yofs += (float)((scl[i]-0.5-ddy)*15.0);
		}
		for (j = 0; j < 4; j++) {
			grp->Vtx[4+i*4+vtxofs].tv = grp->Vtx[5+i*4+vtxofs].tv = yofs/(float)texh;
			grp->Vtx[6+i*4+vtxofs].tv = grp->Vtx[7+i*4+vtxofs].tv = (yofs+17.0f)/(float)texh;
		}
	}

	return false;
}

// ==============================================================
// ==============================================================

InstrAlt::InstrAlt (VESSEL3 *v): PanelElement (v)
{
	pamin = 100000; // invalidate
}

// ==============================================================

void InstrAlt::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	const DWORD texw = PANELEL_TEXW, texh = PANELEL_TEXH;
	const float tapex0 = 34.0f, tapew = 32.0f;
	const float tapey0 = (float)(texh-422), tapeh = 333.0f;

	const DWORD NVTX = 22;
	const DWORD NIDX = 30;

	static NTVERTEX VTX[NVTX] = {
		// VS tape
		{atape_xcnt-16,atape_ycnt-48,0,  0,0,0,  tapex0/(float)texw,        0},
		{atape_xcnt+16,atape_ycnt-48,0,  0,0,0,  (tapex0+tapew)/(float)texw,0},
		{atape_xcnt-16,atape_ycnt+48,0,  0,0,0,  tapex0/(float)texw,        0},
		{atape_xcnt+16,atape_ycnt+48,0,  0,0,0,  (tapex0+tapew)/(float)texw,0},
		// Alt*1000 readouts
		{atape_xcnt-39,atape_ycnt-7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{atape_xcnt-31,atape_ycnt-7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{atape_xcnt-39,atape_ycnt+7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{atape_xcnt-31,atape_ycnt+7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{atape_xcnt-31,atape_ycnt-7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{atape_xcnt-23,atape_ycnt-7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{atape_xcnt-31,atape_ycnt+7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{atape_xcnt-23,atape_ycnt+7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{atape_xcnt-23,atape_ycnt-7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{atape_xcnt-15,atape_ycnt-7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{atape_xcnt-23,atape_ycnt+7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{atape_xcnt-15,atape_ycnt+7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		// apoapsis/periapsis altitude bugs
		{atape_xcnt+16,atape_ycnt   ,0,  0,0,0,  77.5f/(float)texw, (float)(texh-13.5f)/(float)texh},
		{atape_xcnt+ 5,atape_ycnt   ,0,  0,0,0,  65.0f/(float)texw, (float)(texh-13.5f)/(float)texh},
		{atape_xcnt+16,atape_ycnt-11,0,  0,0,0,  77.5f/(float)texw, (float)(texh-26)/(float)texh},
		{atape_xcnt+16,atape_ycnt   ,0,  0,0,0,  77.5f/(float)texw, (float)(texh-12.5f)/(float)texh},
		{atape_xcnt+ 5,atape_ycnt   ,0,  0,0,0,  65.0f/(float)texw, (float)(texh-12.5f)/(float)texh},
		{atape_xcnt+16,atape_ycnt+11,0,  0,0,0,  77.5f/(float)texw, (float)(texh)/(float)texh}
	};
	static WORD IDX[NIDX] = {
		0,1,2, 3,2,1,
		4,5,6, 7,6,5,
		8,9,10, 11,10,9,
		12,13,14, 15,14,13,
		16,17,18,  19,21,20
	};

	AddGeometry (hMesh, grpidx, VTX, NVTX, IDX, NIDX);
}

// ==============================================================

bool InstrAlt::Redraw2D (SURFHANDLE surf)
{
	double alt = vessel->GetAltitude ()*1e-3;  // altitude in km
	double valt, aalt = fabs(alt);

	static double texw = PANELEL_TEXW, texh = PANELEL_TEXH;
	static double scalecnt = texh-422.0+152.0;
	static int scaleunit = 15;
	static double viewh = 50.0;
	double ycnt, y0, y1, dy, ddy;
	char *c, cbuf[12];
	bool centered = (fabs(alt) <= 4.0);

	dy = alt-floor(alt);
	if (centered) {
		ycnt = scalecnt - alt*scaleunit;
	} else {
		if (alt > 0.0) ycnt = scalecnt - (5.0+dy)*scaleunit;
		else           ycnt = scalecnt + (5.0-dy)*scaleunit;
	}
	y0 = ycnt-viewh;
	y1 = ycnt+viewh;
	grp->Vtx[0+vtxofs].tv = grp->Vtx[1+vtxofs].tv = (float)(y0/texh);
	grp->Vtx[2+vtxofs].tv = grp->Vtx[3+vtxofs].tv = (float)(y1/texh);

	// copy labels onto scale
	const int labelx = 36;
	int i, j, n, amin, amax, iy, len, ysrc;
	int a0 = (int)floor(alt);
	amin = a0-3;
	if (amin != pamin) {
		pamin = amin;
		amax = amin+7;
		for (i = amin; i <= amax; i++) {
			sprintf (cbuf, "%03d", abs(i%1000));
			len = 3;
			if (centered) {
				if (!i) continue;
				iy = (int)scalecnt-i*scaleunit-5;
			} else {
				if (i > 0) iy = (int)scalecnt-(2+i-amin)*scaleunit-5;
				else       iy = (int)scalecnt+(8-i+amin)*scaleunit-5;
			}
			for (j = 0, c = cbuf; j < 3; c++, j++) {
				n = *c-'0';
				ysrc = (int)texh-265+n*8;
				if (i < 0) ysrc += 88;
				oapiBlt (surf, surf, labelx+j*6, iy, label_srcx, ysrc, 6, 8);
			}
		}
	}

	// apoapsis/periapsis altitude indicators
	OBJHANDLE hRef = vessel->GetSurfaceRef();
	if (hRef) {
		double rad = oapiGetSize (hRef);
		double apalt, pealt;
		vessel->GetApDist(apalt); apalt -= rad; apalt *= 1e-3;
		vessel->GetPeDist(pealt); pealt -= rad; pealt *= 1e-3;

		float yofs;
		if (apalt < alt+4 && apalt > alt-4)
			yofs = (float)((apalt-alt)*scaleunit*(48.0/50.0));
		else yofs = -60;
		static const float y0[3] = {atape_ycnt, atape_ycnt, atape_ycnt-11};
		for (i = 0; i < 3; i++)
			grp->Vtx[vtxofs+i+16].y = y0[i]-yofs;

		if (pealt < alt+4 && pealt > alt-4)
			yofs = (float)((pealt-alt)*scaleunit*(48.0/50.0));
		else yofs = -60;
		static const float y1[3] = {atape_ycnt, atape_ycnt, atape_ycnt+11};
		for (i = 0; i < 3; i++)
			grp->Vtx[vtxofs+i+19].y = y1[i]-yofs;
	}


	// km*1000 indicator wheels
	sprintf (cbuf, "%06d", ((int)aalt)%1000000);
	for (i = 0; i < 3; i++) {
		float yofs = (float)(texh-111 - (cbuf[i]-'0')*15);
		if (aalt > 500.0) {
			const double scl[3] = {1e5,1e4,1e3};
			valt = aalt/scl[i];
			ddy = (valt-floor(valt))*scl[i];
			// number dials in rotation phase
			if (ddy < 0.5) yofs += (float)((0.5-ddy)*15.0);
			else if (ddy > scl[i]-0.5) yofs += (float)((scl[i]-0.5-ddy)*15.0);
		}
		for (j = 0; j < 4; j++) {
			grp->Vtx[4+i*4+vtxofs].tv = grp->Vtx[5+i*4+vtxofs].tv = yofs/(float)texh;
			grp->Vtx[6+i*4+vtxofs].tv = grp->Vtx[7+i*4+vtxofs].tv = (yofs+17.0f)/(float)texh;
		}
	}
	return false;
}

// ==============================================================
// ==============================================================

InstrVS::InstrVS (VESSEL3 *v): PanelElement (v)
{
	pvmin = 100000; // invalidate
}

// ==============================================================

void InstrVS::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	const DWORD texw = PANELEL_TEXW, texh = PANELEL_TEXH;
	const float tapex0 = 0.0f, tapew = 32.0f;
	const float tapey0 = (float)(texh-422), tapeh = 333.0f;

	const DWORD NVTX = 18;
	const DWORD NIDX = 24;

	static NTVERTEX VTX[NVTX] = {
		// VS tape
		{vtape_xcnt-16,vtape_ycnt-48,0,  0,0,0,  tapex0/(float)texw,        tapey0/(float)texh},
		{vtape_xcnt+16,vtape_ycnt-48,0,  0,0,0,  (tapex0+tapew)/(float)texw,tapey0/(float)texh},
		{vtape_xcnt-16,vtape_ycnt+48,0,  0,0,0,  tapex0/(float)texw,        (tapey0+tapeh)/(float)texh},
		{vtape_xcnt+16,vtape_ycnt+48,0,  0,0,0,  (tapex0+tapew)/(float)texw,(tapey0+tapeh)/(float)texh},
		// VS*1000 readouts
		{vtape_xcnt-31,vtape_ycnt-7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{vtape_xcnt-23,vtape_ycnt-7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{vtape_xcnt-31,vtape_ycnt+7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{vtape_xcnt-23,vtape_ycnt+7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{vtape_xcnt-23,vtape_ycnt-7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{vtape_xcnt-15,vtape_ycnt-7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		{vtape_xcnt-23,vtape_ycnt+7,0, 0,0,0,  (float)wheel_srcx/(float)texw, 0},
		{vtape_xcnt-15,vtape_ycnt+7,0, 0,0,0,  (float)(wheel_srcx+10)/(float)texw, 0},
		// max vspeed bugs
		{vtape_xcnt+16,vtape_ycnt   ,0,  0,0,0,  77.5f/(float)texw, (float)(texh-13.5f)/(float)texh},
		{vtape_xcnt+ 5,vtape_ycnt   ,0,  0,0,0,  65.0f/(float)texw, (float)(texh-13.5f)/(float)texh},
		{vtape_xcnt+16,vtape_ycnt-11,0,  0,0,0,  77.5f/(float)texw, (float)(texh-26)/(float)texh},
		{vtape_xcnt+16,vtape_ycnt   ,0,  0,0,0,  77.5f/(float)texw, (float)(texh-12.5f)/(float)texh},
		{vtape_xcnt+ 5,vtape_ycnt   ,0,  0,0,0,  65.0f/(float)texw, (float)(texh-12.5f)/(float)texh},
		{vtape_xcnt+16,vtape_ycnt+11,0,  0,0,0,  77.5f/(float)texw, (float)(texh)/(float)texh}
	};
	static WORD IDX[NIDX] = {
		0,1,2, 3,2,1,
		4,5,6, 7,6,5,
		8,9,10, 11,10,9,
		12,13,14,  15,17,16
	};

	AddGeometry (hMesh, grpidx, VTX, NVTX, IDX, NIDX);
}

// ==============================================================

bool InstrVS::Redraw2D (SURFHANDLE surf)
{
	VECTOR3 V;
	double vspd, avspd;
	if (vessel->GetAirspeedVector (FRAME_HORIZON, V)) {
		vspd = V.y*0.1; // unit is 10m
		avspd = fabs(vspd);
	} else {
		vspd = avspd = 0.0;
	}

	static double texw = PANELEL_TEXW, texh = PANELEL_TEXH;
	static double scalecnt = texh-422.0+152.0;
	static int scaleunit = 15;
	static double viewh = 50.0;
	double ycnt, y0, y1, dy, vvspd, ddy;
	char *c, cbuf[12];
	bool centered = (fabs(vspd) <= 4.0);

	dy = vspd-floor(vspd);
	if (centered) {
		ycnt = scalecnt - vspd*scaleunit;
	} else {
		if (vspd > 0.0) ycnt = scalecnt - (5.0+dy)*scaleunit;
		else            ycnt = scalecnt + (5.0-dy)*scaleunit;
	}
	y0 = ycnt-viewh;
	y1 = ycnt+viewh;
	grp->Vtx[0+vtxofs].tv = grp->Vtx[1+vtxofs].tv = (float)(y0/texh);
	grp->Vtx[2+vtxofs].tv = grp->Vtx[3+vtxofs].tv = (float)(y1/texh);

	// copy labels onto scale
	const int labelx = 2;
	int i, j, n, vmin, vmax, iy, len, ysrc;
	int v0 = (int)floor(vspd);
	vmin = v0-3;
	if (vmin != pvmin) {
		pvmin = vmin;
		vmax = vmin+7;
		for (i = vmin; i <= vmax; i++) {
			sprintf (cbuf, "%03d", abs((i%100)*10));
			len = 3; // strlen(cbuf);
			if (centered) {
				if (!i) continue;
				iy = (int)scalecnt-i*scaleunit-5;
			} else {
				if (i > 0) iy = (int)scalecnt-(2+i-vmin)*scaleunit-5;
				else       iy = (int)scalecnt+(8-i+vmin)*scaleunit-5;
			}
			for (j = 0, c = cbuf; j < 3; c++, j++) {
				n = *c-'0';
				ysrc = (int)texh-265+n*8;
				if (i < 0) ysrc += 88;
				oapiBlt (surf, surf, labelx+j*6, iy, label_srcx, ysrc, 6, 8);
			}
		}
	}

	// max VS indicator
	OBJHANDLE hRef = vessel->GetSurfaceRef();
	if (hRef) {
		const double EPS = 1e-10;
		const double G = 6.67259e-11;
		double mu = G*oapiGetMass(hRef);
		ELEMENTS el;
		vessel->GetElements (hRef, el, 0);
		if (el.e == 1.0) el.e += EPS; // hack; what is the maximum radial velocity of a parabolic orbit?
		double vr_max = (el.e == 1.0 ? 0.0 : el.e * sqrt (mu/(el.a*(1.0-el.e*el.e))));

		vr_max *= 0.1;
		float yofs;
		if (vr_max < vspd+4 && vr_max > vspd-4)
			yofs = (float)((vr_max-vspd)*scaleunit*(48.0/50.0));
		else yofs = -60;
		static const float y0[3] = {vtape_ycnt, vtape_ycnt, vtape_ycnt-11};
		for (i = 0; i < 3; i++)
			grp->Vtx[vtxofs+i+12].y = y0[i]-yofs;

		if (-vr_max < vspd+4 && -vr_max > vspd-4)
			yofs = (float)((-vr_max-vspd)*scaleunit*(48.0/50.0));
		else yofs = -60;
		static const float y1[3] = {vtape_ycnt, vtape_ycnt, vtape_ycnt+11};
		for (i = 0; i < 3; i++)
			grp->Vtx[vtxofs+i+15].y = y1[i]-yofs;
	}

	// km/s indicator wheels
	sprintf (cbuf, "%05d", (((int)avspd)%10000)*10);
	for (i = 0; i < 2; i++) {
		float yofs = (float)(texh-111 - (cbuf[i]-'0')*15);
		if (avspd > 50.0) {
			const double scl[2] = {1e3,1e2};
			vvspd = avspd/scl[i];
			ddy = (vvspd-floor(vvspd))*scl[i];
			// number dials in rotation phase
			if (ddy < 0.5) yofs += (float)((0.5-ddy)*15.0);
			else if (ddy > scl[i]-0.5) yofs += (float)((scl[i]-0.5-ddy)*15.0);
		}
		for (j = 0; j < 4; j++) {
			grp->Vtx[4+i*4+vtxofs].tv = grp->Vtx[5+i*4+vtxofs].tv = yofs/(float)texh;
			grp->Vtx[6+i*4+vtxofs].tv = grp->Vtx[7+i*4+vtxofs].tv = (yofs+17.0f)/(float)texh;
		}
	}

	return false;
}

// ==============================================================
// ==============================================================

InstrVAcc::InstrVAcc (VESSEL3 *v): PanelElement (v)
{
	VECTOR3 V;
	pt = oapiGetSimTime();
	if (v->GetAirspeedVector (FRAME_HORIZON, V))
		pvspd = V.y;
	else
		pvspd = 0.0;
}

// ==============================================================

void InstrVAcc::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	const DWORD texw = PANELEL_TEXW, texh = PANELEL_TEXH;
	const float tapex0 = 0.0f, tapew = 32.0f;
	const float tapey0 = (float)(texh-422), tapeh = 333.0f;

	const DWORD NVTX = 3;
	const DWORD NIDX = 3;

	static NTVERTEX VTX[NVTX] = {
		// VAcc needle
		{vtape_xcnt+24,vtape_ycnt  ,0,  0,0,0,  66.5f/(float)texw, (texh-6.5f)/(float)texh},
		{vtape_xcnt+31,vtape_ycnt-6,0,  0,0,0,  71.5f/(float)texw, (texh-1.0f)/(float)texh},
		{vtape_xcnt+31,vtape_ycnt+6,0,  0,0,0,  61.5f/(float)texw, (texh-1.0f)/(float)texh},
	};
	static WORD IDX[NIDX] = {
		0,1,2
	};

	AddGeometry (hMesh, grpidx, VTX, NVTX, IDX, NIDX);
}

// ==============================================================

bool InstrVAcc::Redraw2D (SURFHANDLE surf)
{
	int i;
	float yofs;
	VECTOR3 V;
	double vspd;
	double t = oapiGetSimTime();
	if (vessel->GetAirspeedVector (FRAME_HORIZON, V)) {
		vspd = V.y; // unit is 10m
	} else {
		vspd = 0.0;
	}
	if (t > pt) {
		const double yscale = 6.2455;
		double vacc = (vspd-pvspd)/(t-pt);
		yofs = -(float)((vacc >= 0.0 ? sqrt(min(40,vacc)) : -sqrt(min(40,-vacc))) * yscale);
		static const float y0[3] = {vtape_ycnt, vtape_ycnt-6, vtape_ycnt+6};
		for (i = 0; i < 3; i++)
			grp->Vtx[vtxofs+i].y = y0[i] + yofs;
	}
	pvspd = vspd;
	pt = t;

	return false;
}