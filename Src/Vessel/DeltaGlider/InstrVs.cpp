// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// InstrVs.cpp
// Vertical speed tape instrument for the Delta-Glider
// ==============================================================

#define STRICT 1
#include "InstrVs.h"
#include "DeltaGlider.h"
#include "meshres_p0.h"
#include "meshres_vc.h"
#include "dg_vc_anim.h"

// ==============================================================

InstrVS::InstrVS (VESSEL3 *v): PanelElement (v)
{
	pvmin = 100000; // invalidate

	memset (&vc_grp, 0, sizeof(GROUPREQUESTSPEC));
	for (int i = 0; i < 4; i++)
		vperm[i] = (WORD)(i+VC_VSTAPE_vofs);
	vc_grp.VtxPerm = vperm;
	vc_grp.nVtx = 4;

	memset (&vc_grp_readout, 0, sizeof(GROUPREQUESTSPEC));
	for (int i = 0; i < 20; i++)
		vperm_readout[i] = (WORD)(i+VC_VS_READOUT_vofs);
	vc_grp_readout.VtxPerm = vperm_readout;
	vc_grp_readout.nVtx = 20;
}

// --------------------------------------------------------------

InstrVS::~InstrVS()
{
	if (vc_grp.Vtx) delete []vc_grp.Vtx;
	if (vc_grp_readout.Vtx) delete []vc_grp_readout.Vtx;
}

// --------------------------------------------------------------

void InstrVS::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_BELOW_P0);
	sf = oapiGetTextureHandle (hMesh, grp->TexIdx+1);
	vtxofs = 160;
}

// --------------------------------------------------------------

void InstrVS::ResetVC (DEVMESHHANDLE hMesh)
{
	if (!vc_grp.Vtx) vc_grp.Vtx = new NTVERTEX[vc_grp.nVtx];
	if (!vc_grp_readout.Vtx) vc_grp_readout.Vtx = new NTVERTEX[vc_grp_readout.nVtx];
	if (oapiGetMeshGroup (hMesh, GRP_VC_INSTR_VC, &vc_grp) != 0) { // problems
		delete []vc_grp.Vtx;
		vc_grp.Vtx = 0;
	}
	if (oapiGetMeshGroup (hMesh, GRP_VC_INSTR_VC, &vc_grp_readout) != 0) { // problems
		delete []vc_grp_readout.Vtx;
		vc_grp_readout.Vtx = 0;
	}
	sf = oapiGetTextureHandle (((DeltaGlider*)vessel)->vcmesh_tpl, 19);
}

// --------------------------------------------------------------

void InstrVS::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	const DWORD texw = INSTR3D_TEXW, texh = INSTR3D_TEXH;
	const float tapex0 = (float)texw-197.5f, tapew = 41.0f;
	const float tapey0 = (float)(texh-764), tapeh = 512.0f;
	const float xcnt = 682.0f, ycnt = 311.0f;

	const DWORD NVTX = 24;
	const DWORD NIDX = 36;
	static NTVERTEX VTX[NVTX] = {
		// VS tape
		{xcnt-22,ycnt-59,0,  0,0,0,  tapex0/(float)texw,        tapey0/(float)texh},
		{xcnt+22,ycnt-59,0,  0,0,0,  (tapex0+tapew)/(float)texw,tapey0/(float)texh},
		{xcnt-22,ycnt+59,0,  0,0,0,  tapex0/(float)texw,        (tapey0+tapeh)/(float)texh},
		{xcnt+22,ycnt+59,0,  0,0,0,  (tapex0+tapew)/(float)texw,(tapey0+tapeh)/(float)texh},
		// VS readout
		{xcnt+ 6.0f,ycnt-7.0f,0,  0,0,0,  0, 0},
		{xcnt+13.0f,ycnt-7.0f,0,  0,0,0,  0, 0},
		{xcnt+ 6.0f,ycnt+7.0f,0,  0,0,0,  0, 0},
		{xcnt+13.0f,ycnt+7.0f,0,  0,0,0,  0, 0},
		{xcnt+13.0f,ycnt-7.0f,0,  0,0,0,  0, 0},
		{xcnt+20.0f,ycnt-7.0f,0,  0,0,0,  0, 0},
		{xcnt+13.0f,ycnt+7.0f,0,  0,0,0,  0, 0},
		{xcnt+20.0f,ycnt+7.0f,0,  0,0,0,  0, 0},
		{xcnt+20.0f,ycnt-7.0f,0,  0,0,0,  0, 0},
		{xcnt+27.0f,ycnt-7.0f,0,  0,0,0,  0, 0},
		{xcnt+20.0f,ycnt+7.0f,0,  0,0,0,  0, 0},
		{xcnt+27.0f,ycnt+7.0f,0,  0,0,0,  0, 0},
		{xcnt+27.0f,ycnt-7.0f,0,  0,0,0,  0, 0},
		{xcnt+34.0f,ycnt-7.0f,0,  0,0,0,  0, 0},
		{xcnt+27.0f,ycnt+7.0f,0,  0,0,0,  0, 0},
		{xcnt+34.0f,ycnt+7.0f,0,  0,0,0,  0, 0},
		{xcnt+34.0f,ycnt-7.0f,0,  0,0,0,  0, 0},
		{xcnt+41.0f,ycnt-7.0f,0,  0,0,0,  0, 0},
		{xcnt+34.0f,ycnt+7.0f,0,  0,0,0,  0, 0},
		{xcnt+41.0f,ycnt+7.0f,0,  0,0,0,  0, 0}
	};
	static WORD IDX[NIDX] = {
		0,1,2, 3,2,1,
		4,5,6, 7,6,5,
		8,9,10, 11,10,9,
		12,13,14, 15,14,13,
		16,17,18, 19,18,17,
		20,21,22, 23,22,21,
	};

#ifdef UNDEF
	// DEBUG
	VFS::ofstream ofs("tmp.dat");
	for (int j = 0; j < NVTX; j++) {
			ofs << VTX[j].x << ' ' << VTX[j].y << ' ' << 0 << ' ';
			ofs << VTX[j].nx << ' ' << VTX[j].ny << ' '  << -1 << ' ';
			ofs << VTX[j].tu << ' ' << VTX[j].tv << std::endl;
	}
#endif
	//AddGeometry (hMesh, grpidx, VTX, NVTX, IDX, NIDX);
}

// --------------------------------------------------------------

void InstrVS::Redraw (NTVERTEX *vtx, NTVERTEX *vtxr)
{
	VECTOR3 V;
	double vspd;
	if (vessel->GetAirspeedVector (FRAME_HORIZON, V))
		vspd = V.y*0.1; // unit is 10m
	else
		vspd = 0.0;

	static double texw = INSTR3D_TEXW, texh = INSTR3D_TEXH;
	static double scalecnt = texh-764.0+152.0;
	static int scaleunit = 15;
	static double viewh = 50.0;
	double ycnt, y0, y1, dx, dy;
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
	vtx[0].tv = vtx[1].tv = (float)(y0/texh);
	vtx[2].tv = vtx[3].tv = (float)(y1/texh);

	// copy labels onto scale
	const int labelx = (int)texw-185;
	int i, j, n, vmin, vmax, iy, len, xsrc, ysrc;
	vmin = (int)floor(vspd)-3;
	if (vmin != pvmin) {
		pvmin = vmin;
		vmax = vmin+7;
		for (i = vmin; i <= vmax; i++) {
			sprintf (cbuf, "%d", abs((i%1000)*10));
			len = strlen(cbuf);
			if (centered) {
				if (!i) continue;
				iy = (int)scalecnt-i*scaleunit-5;
			} else {
				if (i > 0) iy = (int)scalecnt-(2+i-vmin)*scaleunit-5;
				else       iy = (int)scalecnt+(8-i+vmin)*scaleunit-5;
			}
			for (j = 0, c = cbuf; j < 4; c++, j++) {
				if (j < len) {
					n = *c-'0';
					xsrc = (int)texw-184;
					ysrc = (int)texh-428+n*8;
				} else {
					xsrc = (int)texw-184;
					ysrc = (int)texh-348;
				}
				if (i < 0) ysrc += 88;
				oapiBlt (sf, sf, labelx+j*6, iy, xsrc, ysrc, 6, 8);
			}
		}
	}

	// VS readout
	if (fabs(vspd) < 1e3)
		sprintf (cbuf, fabs(vspd) < 10.0 ? "%+0.1f" : "%+0.0f", vspd*10.0);
	else if (fabs(vspd) < 1e6)
		sprintf (cbuf, "%+3.0fk", vspd > 0.0 ? floor(vspd*0.01) : ceil(vspd*0.01));
	else
		strcpy (cbuf, "----");

	static double numx = texw-177.0, numy = texh-423.5, numw = 10.0, numh = 19.0;
	static double tu_num[4] = {numx/texw,(numx+numw)/texw,numx/texw,(numx+numw)/texw};
	static double tv_num[4] = {(numy+numh)/texh,(numy+numh)/texh,numy/texh,numy/texh};
	int vofs = 4+vtxofs;
	for (c = cbuf, i = 0; i < 5; c++, i++) {
		if (*c >= '0' && *c <= '9') {
			dx = 0.0;
			dy = ((*c-'0') * 17.0)/texh;
		} else {
			dx = 10.0f/texw;
			switch (*c) {
				case '.': dy =  0.0;      break;
				case '-': dy = 34.0/texh; break;
				case '+': dy = 51.0/texh; break;
				case 'k': dy = 68.0/texh; break;
				default:  dy = 17.0/texh; break;
			}
		}
		for (j = 0; j < 4; j++) {
			vtxr[i*4+j].tu = (float)(tu_num[j]+dx);
			vtxr[i*4+j].tv = (float)(tv_num[j]+dy);
		}
	}
}

// --------------------------------------------------------------

bool InstrVS::Redraw2D (SURFHANDLE surf)
{
	VECTOR3 V;
	double vspd;
	if (vessel->GetAirspeedVector (FRAME_HORIZON, V))
		vspd = V.y*0.1; // unit is 10m
	else
		vspd = 0.0;

	static double texw = INSTR3D_TEXW, texh = INSTR3D_TEXH;
	static double scalecnt = texh-764.0+152.0;
	static int scaleunit = 15;
	static double viewh = 50.0;
	double ycnt, y0, y1, dx, dy;
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
	const int labelx = (int)texw-185;
	int i, j, n, vmin, vmax, iy, len, xsrc, ysrc;
	vmin = (int)floor(vspd)-3;
	if (vmin != pvmin) {
		pvmin = vmin;
		vmax = vmin+7;
		for (i = vmin; i <= vmax; i++) {
			sprintf (cbuf, "%d", abs((i%1000)*10));
			len = strlen(cbuf);
			if (centered) {
				if (!i) continue;
				iy = (int)scalecnt-i*scaleunit-5;
			} else {
				if (i > 0) iy = (int)scalecnt-(2+i-vmin)*scaleunit-5;
				else       iy = (int)scalecnt+(8-i+vmin)*scaleunit-5;
			}
			for (j = 0, c = cbuf; j < 4; c++, j++) {
				if (j < len) {
					n = *c-'0';
					xsrc = (int)texw-184;
					ysrc = (int)texh-428+n*8;
				} else {
					xsrc = (int)texw-184;
					ysrc = (int)texh-348;
				}
				if (i < 0) ysrc += 88;
				oapiBlt (sf, sf, labelx+j*6, iy, xsrc, ysrc, 6, 8);
			}
		}
	}

	// VS readout
	if (fabs(vspd) < 1e3)
		sprintf (cbuf, fabs(vspd) < 10.0 ? "%+0.1f" : "%+0.0f", vspd*10.0);
	else if (fabs(vspd) < 1e6)
		sprintf (cbuf, "%+3.0fk", vspd > 0.0 ? floor(vspd*0.01) : ceil(vspd*0.01));
	else
		strcpy (cbuf, "----");

	static double numx = texw-177.0, numy = texh-423.5, numw = 10.0, numh = 19.0;
	static double tu_num[4] = {numx/texw,(numx+numw)/texw,numx/texw,(numx+numw)/texw};
	static double tv_num[4] = {numy/texh,numy/texh,(numy+numh)/texh,(numy+numh)/texh};
	int vofs = 4+vtxofs;
	for (c = cbuf, i = 0; i < 5; c++, i++) {
		if (*c >= '0' && *c <= '9') {
			dx = 0.0;
			dy = ((*c-'0') * 17.0)/texh;
		} else {
			dx = 10.0f/texw;
			switch (*c) {
				case '.': dy =  0.0;      break;
				case '-': dy = 34.0/texh; break;
				case '+': dy = 51.0/texh; break;
				case 'k': dy = 68.0/texh; break;
				default:  dy = 17.0/texh; break;
			}
		}
		for (j = 0; j < 4; j++) {
			grp->Vtx[i*4+j+vofs].tu = (float)(tu_num[j]+dx);
			grp->Vtx[i*4+j+vofs].tv = (float)(tv_num[j]+dy);
		}
	}

	return false;
}

// --------------------------------------------------------------

bool InstrVS::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	NTVERTEX *Vtx = vc_grp.Vtx, *VtxR = vc_grp_readout.Vtx;
	if (hMesh && Vtx && VtxR) {
		Redraw (Vtx, VtxR);

		GROUPEDITSPEC ges = {GRPEDIT_VTXTEXV,0,vc_grp.Vtx,vc_grp.nVtx,vperm};
		oapiEditMeshGroup (hMesh, GRP_VC_INSTR_VC, &ges);

		GROUPEDITSPEC gesr = {GRPEDIT_VTXTEX,0,vc_grp_readout.Vtx,vc_grp_readout.nVtx,vperm_readout};
		oapiEditMeshGroup (hMesh, GRP_VC_INSTR_VC, &gesr);
	}
	return false;
}