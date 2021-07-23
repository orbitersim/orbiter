// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// sliderpair.cpp
// User interface: throttle slider pair
// ==============================================================

#define STRICT 1
#include "sliderpair.h"

static const float texw = (float)PANELEL_TEXW;
static const float texh = (float)PANELEL_TEXH;

// ==============================================================

SliderPair::SliderPair (VESSEL3 *v, float basex, float basey, float rangey, int colidx): PanelElement (v)
{
	bx = basex;  // centre line of slider pair
	by = basey-152;  // bottom position of slider pair
	ry = rangey; // slider vertical range
	cidx = colidx;
	sliderlvl[0] = sliderlvl[1] = 0.0;
}

// ==============================================================

void SliderPair::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	static const DWORD nvtx =  8*2;
	static const DWORD nidx = 30*2;
	float tu0 = (float)( 6+cidx*31)/texw;
	float tu1 = (float)(26+cidx*31)/texw;
	float tu2 = (float)( 1+cidx*31)/texw;
	float tu3 = (float)(31+cidx*31)/texw;
	float tv0 = (texh-61.0f)/texh;
	float tv1 = (texh-51.0f)/texh;
	float tv2 = (texh-66.0f)/texh;
	float tv3 = (texh-46.0f)/texh;
	float vofs = 22.0f/texh;
	NTVERTEX vtx[nvtx] = {
		{0,0,0,  0,0,0,  tu0,tv0-vofs},
		{0,0,0,  0,0,0,  tu1,tv0-vofs},
		{0,0,0,  0,0,0,  tu0,tv1-vofs},
		{0,0,0,  0,0,0,  tu1,tv1-vofs},
		{0,0,0,  0,0,0,  tu2,tv2-vofs},
		{0,0,0,  0,0,0,  tu3,tv2-vofs},
		{0,0,0,  0,0,0,  tu2,tv3-vofs},
		{0,0,0,  0,0,0,  tu3,tv3-vofs},

		{0,0,0,  0,0,0,  tu0,tv0},
		{0,0,0,  0,0,0,  tu1,tv0},
		{0,0,0,  0,0,0,  tu0,tv1},
		{0,0,0,  0,0,0,  tu1,tv1},
		{0,0,0,  0,0,0,  tu2,tv2},
		{0,0,0,  0,0,0,  tu3,tv2},
		{0,0,0,  0,0,0,  tu2,tv3},
		{0,0,0,  0,0,0,  tu3,tv3}
	};
	static const WORD idx[nidx] = {
		0,1,2, 3,2,1,
		4,5,0, 1,0,5,
		5,7,1, 3,1,7,
		7,6,3, 2,3,6,
		6,4,2, 0,2,4,

		8,9,10, 11,10,9,
		12,13,8, 9,8,13,
		13,15,9, 11,9,15,
		15,14,11, 10,11,14,
		14,12,10, 8,10,12
	};
	AddGeometry (hMesh, grpidx, vtx, nvtx, idx, nidx);
	for (int slider = 0; slider < 2; slider++)
		SetVertices (slider, sliderlvl[slider]);
}

// ==============================================================

bool SliderPair::Redraw2D (SURFHANDLE surf, double level[2])
{
	for (int slider = 0; slider < 2; slider++) {
		if (level[slider] != sliderlvl[slider])
			SetVertices (slider, sliderlvl[slider] = level[slider]);
	}
	return false;
}

// ==============================================================

void SliderPair::SetVertices (int which, double pos)
{
	// Pseudo-3D transformation for the throttle sliders

	int i;
	static double rad = 105.0;
	static double alpha0 =   0*RAD;
	static double alpha1 =  70*RAD;
	static double x_obs  =  625;
	static double y_obs  =  145;
	static double z_obs  = -2000;
	static double scale = 2000;
	static double x0[8] = {-8,8,-8,8,-10,10,-10,10};
	static double y0[8] = {4,4,-4,-4,6,6,-6,-6};
	static double z0[8] = {-rad-20,-rad-20,-rad-20,-rad-20,-rad,-rad,-rad,-rad};

	double alpha = alpha0 + (alpha1-alpha0)*pos;
	double sina = sin(alpha), cosa = cos(alpha);

	DWORD vofs = vtxofs + which*8;

	for (i = 0; i < 8; i++) {
		double x = x0[i] + (which-0.5)*25+bx;
		double y = y0[i]*cosa - z0[i]*sina;
		double z = y0[i]*sina + z0[i]*cosa;
		double xd = x-x_obs;
		double zd = z-z_obs;
		double yd = y-y_obs;
		double tanb = yd/zd;
		double tang = xd/zd;
		grp->Vtx[vofs+i].x = (float)(x_obs+tang*scale);
		grp->Vtx[vofs+i].y = (float)(by-tanb*scale);
	}
}

// ==============================================================

int SliderPair::ProcessMouse2D (int event, int mx, int my, double *level)
{
	static int ctrl = 0;
	if (event & PANEL_MOUSE_LBDOWN) { // record which slider to operate
		if      (mx <  12) ctrl = 1; // left engine
		else if (mx >= 41) ctrl = 2; // right engine
		else               ctrl = 3; // both
	}
	double lvl;
	if ((my -= 9) < 0)     lvl = 1.0;
	else if (my > (int)ry) lvl = 0.0;
	else                   lvl = 1.0-my/ry;
	if (ctrl & 1) level[0] = lvl;
	if (ctrl & 2) level[1] = lvl;
	return ctrl;
}


// ==============================================================
// ==============================================================

ThrottlePair::ThrottlePair (VESSEL3 *v, float basex, float basey, float rangey, int colidx, THRUSTER_HANDLE *hthrust)
: SliderPair (v, basex, basey, rangey, colidx)
{
	th = hthrust;
}

// ==============================================================

bool ThrottlePair::Redraw2D (SURFHANDLE surf)
{
	double level[2];
	for (int i = 0; i < 2; i++)
		level[i] = vessel->GetThrusterLevel (th[i]);
	return SliderPair::Redraw2D (surf, level);
}

// ==============================================================

bool ThrottlePair::ProcessMouse2D (int event, int mx, int my)
{
	double level[2];
	int mode = SliderPair::ProcessMouse2D (event, mx, my, level);
	if (mode & 1) vessel->SetThrusterLevel (th[0], level[0]);
	if (mode & 2) vessel->SetThrusterLevel (th[1], level[1]);
	return true;
}
