// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Defpanel.h"
#include "Pane.h"
#include "Util.h"
#include "Vessel.h"

using std::min;
using std::max;

extern Orbiter *g_pOrbiter;
extern Vessel *g_focusobj;
extern char DBG_MSG[256];

static const char *btmlbl[3] = {"PWR","SEL","MNU"};
int nnv0 = 7; // number of nav modes available

// texture dimensions
static float texw = 512.0f, texh = 256.0f;

void BtnRect (oapi::Sketchpad *skp, int x1, int y1, int x2, int y2);
void NumOut (char *str, char *pstr, int len, NTVERTEX *vtx);
char *FmtNum (double f);

float CHX[256] = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,/*#*/400.8f,0,0,0,0,0,0,0,/*+*/300.7f,/*,*/274.4f,/*-*/308.7f,/*.*/270.8f,/* / */276.0f,
	/*0*/206.6f,/*1*/213.0f,/*2*/219.4f,/*3*/225.8f,/*4*/232.2f,/*5*/238.6f,/*6*/245.0f,/*7*/251.4f,/*8*/257.8f,/*9*/264.2f,0,0,/*<*/280.3f,/*=*/316.5f,/*>*/286.5f,0,
	0,/*A*/1,/*B*/10,/*C*/18,/*D*/26,/*E*/34,/*F*/42,/*G*/49,/*H*/58,/*I*/66,/*J*/69,/*K*/76,/*L*/84,/*M*/91,/*N*/101,/*O*/109,
	/*P*/118,/*Q*/126,/*R*/134,/*S*/143,/*T*/150.5f,/*U*/157.5f,/*V*/166,/*W*/173.5f,/*X*/184,/*Y*/192,/*Z*/199.5f,0,0,0,0,0,
	0,/*a*/1,/*b*/7.5f,/*c*/14.5f,/*d*/21,/*e*/28,/*f*/34,/*g*/39,/*h*/46.5f,/*i*/53.5f,/*j*/56,/*k*/59.8f,/*l*/66,/*m*/69.5f,/*n*/79.5f,/*o*/86.8f,
	/*p*/94,/*q*/100.5f,/*r*/107.8f,/*s*/112,/*t*/118,/*u*/122.5f,/*v*/129.1f,/*w*/135.5f,/*x*/144.5f,/*y*/151,/*z*/157.2f,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};
float CHW[256] = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,/*#*/6.4f,0,0,0,0,0,0,0,/*+*/7.6f,/*,*/2.2f,/*-*/7.6f,/*.*/2.4f,/* / */4,
	/*0*/6.4f,/*1*/6.4f,/*2*/6.4f,/*3*/6.4f,/*4*/6.4f,/*5*/6.4f,/*6*/6.4f,/*7*/6.4f,/*8*/6.4f,/*9*/6.4f,0,0,/*<*/6.5f,/*=*/6.5f,/*>*/6.5f,0,
	0,/*A*/8.7f,/*B*/8,/*C*/8,/*D*/8,/*E*/8,/*F*/7,/*G*/9,/*H*/8,/*I*/3,/*J*/7,/*K*/8,/*L*/7,/*M*/9.5f,/*N*/8,/*O*/9,
	/*P*/7.8f,/*Q*/8,/*R*/9,/*S*/7.5f,/*T*/7,/*U*/8,/*V*/7.5f,/*W*/10.5f,/*X*/8,/*Y*/7.5f,/*Z*/7,0,0,0,0,0,
	0,/*a*/6.5f,/*b*/7,/*c*/6.5f,/*d*/7,/*e*/6,/*f*/5,/*g*/7.2f,/*h*/6.5f,/*i*/2.5f,/*j*/2.5f,/*k*/6.2f,/*l*/3,/*m*/9.6f,/*n*/6.7f,/*o*/6.6f,
	/*p*/6.5f,/*q*/7,/*r*/4.2f,/*s*/6,/*t*/4.2f,/*u*/6.5f,/*v*/6.8f,/*w*/8.7f,/*x*/6,/*y*/6,/*z*/5.8f,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};

// =======================================================================
// class DefaultPanel

DefaultPanel::DefaultPanel (Pane *_pane, int cidx): pane(_pane)
{
	gc = NULL;
	shRenderTarget = NULL; // should be queried from graphics client
	surf = NULL;
	activemfd = activebtn = -1;
	transpmfd = g_pOrbiter->Cfg()->CfgLogicPrm.bMfdTransparent;
	compact_layout = g_pOrbiter->Cfg()->CfgLogicPrm.bGlasspitCompact;
	colidx = cidx;

	elevtrim = 100;

	SetGeometry ();

	fw = pane->f1W, fh = pane->f1H;
	int i, edge = (fw*9)/2;
	mfdx[0] = edge;
	mfdx[1] = pane->W - mfdw - edge;
	mfdy = pane->H - mfdh - btnh - fw/2;

	// set up navmode geometry
	navdispmode = 1;
	for (i = 0; i < nnv0; i++) nvbstate[i] = false;
	nnvb = nnv0;
	nvby = pane->H - btnh - fw/4;

	rcsmode = -1;
	rcsdispmode = 1;
	
	mfdFont = NULL;
	mfdPen = NULL;

	InitDeviceObjects ();
}

DefaultPanel::~DefaultPanel ()
{
	DestroyDeviceObjects();
}

void DefaultPanel::SetGeometry ()
{
	int i, j, k, m, n, vofs;

	ResetGeometry();

	// viewport dimensions
	viewW = pane->Width(), viewH = pane->Height();
	scale = max(0.625f,viewH/800.0f); //viewW/1280.0f;

	// MFD display dimensions
	mfdw = mfdh = (int)(scale*35.0*(g_pOrbiter->Cfg()->CfgLogicPrm.MFDSize+5));//(viewH*(g_pOrbiter->Cfg()->CfgLogicPrm.MFDSize+5))/23;
	btnw = max(28,viewH/18) /*viewW/28*/, btnh = (33*btnw)/44;
	gapw = btnw/20;
	float mfdy0 = (float)(viewH-mfdh-btnh-2*gapw);
	float mfdy1 = mfdy0 + (float)mfdh;

	blockdx = (float)gapw+0.5f;
	if (compact_layout)
		blockdx = max (blockdx, viewW/2 - viewH*0.45f - mfdw - 2*btnw - 3*gapw + 0.5f);
	blockdy = viewH*0.06f;
	for (i = 0; i < 6; i++)
		btny[i] = mfdy0 + mfdh/6.0f + (i*mfdh)/7.0f - btnh/2.0f;
	bbtny = (float)(viewH-btnh-gapw);

	WORD idx[6] = {0,1,2,3,2,1}; // default billboard index list

	int nVtx, nIdx;
	NTVERTEX *Vtx;
	WORD *Idx;

	// MFD display billboards
	for (n = 0; n < 2; n++) { // left/right MFD
		float mfdx0 = (float)(n == 0 ? btnw+2*gapw : viewW-mfdw-btnw-2*gapw)-0.5f;
		if (compact_layout) {
			if (n == 0)
				mfdx0 = max (mfdx0, viewW/2 - (viewH*45)/100 - mfdw - btnw - gapw + 0.5f);
			else
				mfdx0 = min (mfdx0, viewW/2 + (viewH*45)/100 + btnw + gapw + 0.5f);
		}
		float mfdx1 = mfdx0 + (float)mfdw;
		Vtx = new NTVERTEX[nVtx = 4];
		Idx = new WORD[nIdx = 6];
		memset (Vtx, 0, nVtx*sizeof(NTVERTEX));
		for (i = 0; i < 4; i++) {
			Vtx[i].x = (i%2 ? mfdx1:mfdx0);
			Vtx[i].y = (i/2 ? mfdy1:mfdy0);
			Vtx[i].tu = (i%2 ? 1.0f:0.0f);
			Vtx[i].tv = (i/2 ? 1.0f:0.0f);
		}
		memcpy (Idx, idx, 6*sizeof(WORD));

		mesh.AddGroup (Vtx, nVtx, Idx, nIdx, 0, TEXIDX_MFD0+n);

		// MFD buttons
		Vtx = new NTVERTEX[nVtx = 4*54];
		Idx = new WORD[nIdx = 6*54];
		btnx[n][0] = mfdx0-btnw-gapw;
		btnx[n][1] = mfdx1+gapw;
		for (k = vofs = 0; k < 2; k++) { // left/right button row
			for (i = 0; i < 6; i++) {
				for (j = 0; j < 4; j++) {
					Vtx[vofs+j].x = btnx[n][k] + (j%2)*btnw;
					Vtx[vofs+j].y = btny[i] + (j/2)*btnh;
					Vtx[vofs+j].z = 0;
					Vtx[vofs+j].tu = (316.0f+(j%2)*44.0f)/texw;
					Vtx[vofs+j].tv = (texh-34.0f+(j/2)*33.0f)/texh;
				}
				vofs += 4;
				for (m = 0; m < 3; m++) {
					for (j = 0; j < 4; j++) {
						Vtx[vofs+j].x = btnx[n][k] + btnw*0.5f;
						Vtx[vofs+j].y = btny[i] + (j/2 ? 0.75f:0.30f)*btnh;
						Vtx[vofs+j].z = 0;
						Vtx[vofs+j].tu = 350.0f/texw;
						Vtx[vofs+j].tv = (texh-231.0f+(j/2)*14.0f)/texh;
					}
					vofs += 4;
				}
			}
		}
		for (i = 0; i < 3; i++) {
			bbtnx[n][i] = floor((mfdx0+mfdx1)*0.5f - 2.0f*btnw + i*(1.5f*btnw))+0.5f;
			for (j = 0; j < 4; j++) {
				Vtx[vofs+j].x = bbtnx[n][i]+(j%2)*btnw;
				Vtx[vofs+j].y = bbtny+(j/2)*btnh;
				Vtx[vofs+j].z = 0;
				Vtx[vofs+j].tu = (316.0f+(j%2)*44.0f)/texw;
				Vtx[vofs+j].tv = (texh-34.0f+(j/2)*33.0f)/texh;
			}
			vofs += 4;
			for (j = 0; j < 4; j++) {
				Vtx[vofs+j].x = bbtnx[n][i] + (j%2 ? 0.85f:0.15f)*btnw;
				Vtx[vofs+j].y = bbtny + (j/2 ? 0.7f:0.3f)*btnh;
				Vtx[vofs+j].z = 0;
				Vtx[vofs+j].tu = (197.0f+i*29.0f+(j%2)*27.0f)/texw;
				Vtx[vofs+j].tv = (texh-101.5f+(j/2)*11.0f)/texh;
			}
			vofs += 4;
		}
		for (i = 0; i < 54; i++)
			for (j = 0; j < 6; j++)
				Idx[i*6+j] = i*4+idx[j];

		mesh.AddGroup (Vtx, nVtx, Idx, nIdx, 0, 0);
	}

	// MFD power button (when MFD display is disabled)
	for (n = 0; n < 2; n++) {
		Vtx = new NTVERTEX[nVtx = 4];
		Idx = new WORD[nIdx = 6];
		memset (Vtx, 0, nVtx*sizeof(NTVERTEX));
		for (j = vofs = 0; j < 4; j++) {
			Vtx[vofs+j].x = bbtnx[n][0]+(j%2)*btnw;
			Vtx[vofs+j].y = bbtny+(j/2)*btnh;
			Vtx[vofs+j].z = 0;
			Vtx[vofs+j].tu = (316.0f+(j%2)*44.0f)/texw;
			Vtx[vofs+j].tv = (texh-102.0f+(j/2)*33.0f)/texh;
		}
		for (j = 0; j < 6; j++)
			Idx[j] = idx[j];

		mesh.AddGroup (Vtx, nVtx, Idx, nIdx, 0, 0, 0, 2);
	}

	// navigation buttons
	Vtx = new NTVERTEX[nVtx = 4*7];
	Idx = new WORD[nIdx = 6*7];
	memset (Vtx, 0, nVtx*sizeof(NTVERTEX));
	float xb[4] = {0,(float)btnw,0,(float)btnw};
	float yb[4] = {0,0,(float)btnh,(float)btnh};
	float ub[4] = {1.0f/texw,45.0f/texw,1.0f/texw,45.0f/texw};
	float vb[4] = {(texh-34.0f)/texh,(texh-34.0f)/texh,(texh-1.0f)/texh,(texh-1.0f)/texh};
	for (i = 0; i < 7; i++) {
		for (j = 0; j < 4; j++) {
			Vtx[i*4+j].x = 0.5f*viewW-3.5f*btnw-3.0f*gapw+1.0f*i*(btnw+gapw)+xb[j];
			Vtx[i*4+j].y = bbtny+yb[j];
			Vtx[i*4+j].z = 0;
			Vtx[i*4+j].tu = (45.0f*i)/texw+ub[j];
			Vtx[i*4+j].tv = vb[j];
		}
		for (j = 0; j < 6; j++)
			Idx[i*6+j] = i*4+idx[j];
	}
	navgrp = mesh.AddGroup (Vtx, nVtx, Idx, nIdx, 0, 0);
	
	// engine status display
	Vtx = new NTVERTEX[nVtx = 27*4];
	Idx = new WORD[nIdx = 27*6];
	memset (Vtx, 0, nVtx*sizeof(NTVERTEX));
	float dx = blockdx, dy = blockdy;
	float xe[4] = {0,195*scale,0,195*scale};
	float ye[4] = {0,0,121*scale,121*scale};
	float ue[4] = {0,195.0f/texw,0,195.0f/texw};
	float ve[4] = {(texh-200.0f)/texh,(texh-200.0f)/texh,(texh-79.0f)/texh,(texh-79.0f)/texh};
	vofs = 0;
	// status background
	for (j = 0; j < 4; j++) {
		Vtx[j+vofs].x = dx+xe[j];
		Vtx[j+vofs].y = dy+ye[j];
		Vtx[j+vofs].z = 0;
		Vtx[j+vofs].tu = ue[j];
		Vtx[j+vofs].tv = ve[j];
	}
	// rcs buttons
	float xr[4] = {8*scale,37*scale,8*scale,37*scale};
	float yr[4] = {95*scale,95*scale,113*scale,113*scale};
	vofs += 4;
	for (i = 0; i < 3; i++)
	    for (j = 0; j < 4; j++) {
		Vtx[i*4+j+vofs].x = dx+i*36.0f*scale+xr[j];
		Vtx[i*4+j+vofs].y = dy+yr[j];
		Vtx[i*4+j+vofs].z = 0;
		Vtx[i*4+j+vofs].tu = (196.0f+i*29.0f+(j%2)*29.0f)/texw;
		Vtx[i*4+j+vofs].tv = (texh-121.0f+(j/2)*18.0f)/texh;
	    }
	rcsx = (int)(dx+xr[0]);
	rcsw = (int)(36.0f*scale);
	rcsy = (int)(dy+yr[0]);
	rcsh = (int)(18.0f*scale);

	// fuel bar
	float xe1[4] = {46*scale,46*scale,46*scale,46*scale};
	float ye1[4] = {8*scale,8*scale,22*scale,22*scale};
	vofs += 12;
	for (j = 0; j < 4; j++) {
		Vtx[j+vofs].x = dx+xe1[j];
		Vtx[j+vofs].y = dy+ye1[j];
		Vtx[j+vofs].z = 0;
		Vtx[j+vofs].tu = 4.0f/texw;
		Vtx[j+vofs].tv = (texh-77.0f)/texh;
	}
	// main engine bar
	vofs += 4;
	for (j = 0; j < 4; j++) {
		Vtx[j+vofs].x = dx+xe1[j];
		Vtx[j+vofs].y = dy+ye1[j]+24.0f*scale;
		Vtx[j+vofs].z = 0;
		Vtx[j+vofs].tu = 4.0f/texw;
		Vtx[j+vofs].tv = (texh-77.0f)/texh;
	}
	// hover engine bar
	vofs += 4;
	for (j = 0; j < 4; j++) {
		Vtx[j+vofs].x = dx+xe1[j];
		Vtx[j+vofs].y = dy+ye1[j]+48.0f*scale;
		Vtx[j+vofs].z = 0;
		Vtx[j+vofs].tu = 4.0f/texw;
		Vtx[j+vofs].tv = (texh-77.0f)/texh;
	}
	// engine data readouts
	vofs += 4;
	for (n = 0; n < 3; n++) {
		for (i = 0; i < 5; i++) {
			for (j = 0; j < 4; j++) {
				Vtx[i*4+j+vofs].x = dx+(145.0f+9.0f*(i+j%2))*scale;
				Vtx[i*4+j+vofs].y = dy + (8+12.6f*(j/2)+24.0f*n)*scale;
				Vtx[i*4+j+vofs].z = 0;
				Vtx[i*4+j+vofs].tu = (325.0f+(j%2)*10.0f)/texw;
				Vtx[i*4+j+vofs].tv = (texh-154.5f+(j/2)*14.0f)/texh;
			}
		}
		vofs += 4*5;
	}
	// trim indicator
	for (j = 0; j < 4; j++) {
		Vtx[j+vofs].x = dx + (121.0f + 10.0f*(j%2))*scale;
		Vtx[j+vofs].y = dy + (95.0f + 5.0f*(j/2))*scale;
		Vtx[j+vofs].z = 0;
		Vtx[j+vofs].tu = 2.0f/texw;
		Vtx[j+vofs].tv = (texh-79.0f+(j/2)*4.0f)/texh;
	}
	vofs += 4;
	// trim readout
	for (i = 0; i < 3; i++) {
		for (j = 0; j < 4; j++) {
			Vtx[i*4+j+vofs].x = dx+(163.0f+9.0f*(i+j%2))*scale;
			Vtx[i*4+j+vofs].y = dy+(93.0f+12.6f*(j/2))*scale;
			Vtx[i*4+j+vofs].z = 0;
			Vtx[i*4+j+vofs].tu = (337.1f+(j%2)*10.0f)/texw;
			Vtx[i*4+j+vofs].tv = (texh-154.5f+(j/2)*14.0f)/texh;
		}
	}
	vofs += 4*3;
	for (j = 0; j < 4; j++) {
		Vtx[j+vofs].x = dx+(151.0f+12.0f*(j%2))*scale;
		Vtx[j+vofs].y = dy+(97.0f+8.6f*(j/2))*scale;
		Vtx[j+vofs].z = 0;
		Vtx[j+vofs].tu = (314.0f+(j%2)*14.0f)/texw;
		Vtx[j+vofs].tv = (texh-138.5f+(j/2)*10.0f)/texh;
	}

	for (i = 0; i < 27; i++)
		for (j = 0; j < 6; j++)
			Idx[i*6+j] = i*4+idx[j];

	enggrp = mesh.AddGroup (Vtx, nVtx, Idx, nIdx, 0, 0);
}

void DefaultPanel::ResetGeometry()
{
	mesh.Clear();
	fuel = engmain = enghovr = -2; // invalidate
	rcsmode = -1; // invalidate
	elevtrim = 100;
	for (size_t i = 0; i < nnv0; i++)
		nvbstate[i] = false;
	memset(engstat_str, 0, 4 * 8 * sizeof(char));
}

void DefaultPanel::InitDeviceObjects ()
{
	gc = g_pOrbiter->GetGraphicsClient();
	if (!gc) return;

	surf = LoadTexture (colidx);

	int fh1 = max(12, min(20, pane->H/50));
	int fh2 = max(10, min(18, pane->H/55));
	int fh3 = (4*fh)/5;
	mfdPen  = gc->clbkCreatePen (1, 0, RGB(0,255,0));
	mfdFont = gc->clbkCreateFont (fh1, true, "Sans");
}

void DefaultPanel::RestoreDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev)
{
	// This method is currently only supported by the inline client
	rcsmode = -1;
}

void DefaultPanel::DestroyDeviceObjects ()
{
	if (gc) {
		if (mfdFont) gc->clbkReleaseFont (mfdFont);
		if (mfdPen)  gc->clbkReleasePen (mfdPen);
		if (surf)    gc->clbkReleaseSurface (surf);
	}
}

SURFHANDLE DefaultPanel::LoadTexture (int idx)
{
	char cbuf[64] = "Cockpit\\Glasspit";
	switch (idx) {
		case 1: strcat (cbuf, "_red"); break;
		case 2: strcat (cbuf, "_yellow"); break;
		case 3: strcat (cbuf, "_blue"); break;
	}
	strcat (cbuf, ".dds");
	return gc->clbkLoadTexture (cbuf, 0x4);
}

void DefaultPanel::SwitchColour (int idx)
{
	if (!gc) return;
	if (idx == colidx) return;
	colidx = idx % 4;
	if (surf) gc->clbkReleaseSurface (surf);
	surf = LoadTexture (colidx);
}

void DefaultPanel::Render ()
{
	if (!gc) return;

	int i, j, vofs, visflag;
	char *cb;
	GroupSpec *grp;
	static MATRIX3 transf = {1,-0.5,0, 0,1,-0.5, 0,0,1};
	bool upd_fuel;

	for (i = 0; i < 2; i++) {
		visflag = (pane->mfd[i].instr ? 0:2);
		for (j = 0; j < 2; j++)
			mesh.GetGroup(i*2+j)->UsrFlag = visflag; // hide/show MFDs
		mesh.GetGroup(4+i)->UsrFlag = 2-visflag;     // show/hide power button
	}
	for (i = 0; i < nnvb; i++) {
		bool active = (g_focusobj->NavModeActive(i+1));
		if (active != nvbstate[i]) {
			GroupSpec *grp = mesh.GetGroup(navgrp);
			static float vb[4] = {(texh-34.0f)/texh,(texh-34.0f)/texh,(texh-1.0f)/texh,(texh-1.0f)/texh};
			for (j = 0; j < 4; j++) {
				grp->Vtx[i*4+j].tv = (active ? vb[j]-34.0f/texh : vb[j]);
			}
			nvbstate[i] = active;
		}
	}
	visflag = (pane->hud && navdispmode ? 0:2);
	mesh.GetGroup(navgrp)->UsrFlag = visflag; // hide/show navbuttons

	// Update RCS buttons
	grp = mesh.GetGroup(enggrp);  // engine status group
	float dx = blockdx, dy = blockdy;
	vofs = 4;
	if (rcsdispmode) {
		if (g_focusobj->AttMode() != rcsmode) {
			rcsmode = g_focusobj->AttMode();
			for (i = 0; i < 3; i++) {
				float dy = texh - (rcsmode == i ? 139.0f : 121.0f);
				for (j = 0; j < 4; j++)
					grp->Vtx[vofs+i*4+j].tv = (dy+(j/2)*18.0f)/texh;
			}
		}
	}

	// Update fuel status
	const TankSpec *ts;
	vofs += 12;
	ts = g_focusobj->DefaultPropellantHandle();
	double newfuel = (ts ? g_focusobj->GetPropellantLevel (ts) : 0.0);
	if (upd_fuel = (newfuel != fuel)) {
		fuel = newfuel;
		grp->Vtx[vofs+1].x = grp->Vtx[vofs+3].x = dx+(float)((46.0+fuel*94.0)*scale);
	}
	// Update main engine status
	vofs += 4;
	double th = g_focusobj->GetThrusterGroupLevel(THGROUP_MAIN);
	if (!th) th = -g_focusobj->GetThrusterGroupLevel(THGROUP_RETRO);
	if (th != engmain) {
		engmain = th;
		if (th >= 0.0) {
			grp->Vtx[vofs].x = grp->Vtx[vofs+2].x = dx+46.0f*scale;
			grp->Vtx[vofs+1].x = grp->Vtx[vofs+3].x = dx+(float)((46.0+engmain*94.0)*scale);
			for (i = 0; i < 4; i++) grp->Vtx[vofs+i].tu = 4.0f/texw;
		} else {
			grp->Vtx[vofs].x = grp->Vtx[vofs+2].x = dx+(140.0f+(float)engmain*94.0f)*scale;
			grp->Vtx[vofs+1].x = grp->Vtx[vofs+3].x = dx+140.0f*scale;
			for (i = 0; i < 4; i++) grp->Vtx[vofs+i].tu = 6.0f/texw;
		}
	}
	// Update hover engine status
	vofs += 4;
	if (g_focusobj->NumThrusters(THGROUP_HOVER)) {
		th = g_focusobj->GetThrusterGroupLevel (THGROUP_HOVER);
		if (th != enghovr) {
			enghovr = th;
			grp->Vtx[vofs+1].x = grp->Vtx[vofs+3].x = dx+(float)((46.0+enghovr*94.0)*scale);
		}
	}
	// Update fuel readout
	vofs += 4;
	if (upd_fuel) {
		cb = FmtNum (ts ? fuel*ts->maxmass : 0.0);
		NumOut (cb, engstat_str[0], min(strlen(cb),(size_t)5), grp->Vtx+vofs);
	}
	// Update main engine readout
	vofs += 4*5;
	th = (engmain >= 0.0 ? engmain*g_focusobj->GetThrusterGroupMaxth(THGROUP_MAIN) :
		-engmain*g_focusobj->GetThrusterGroupMaxth(THGROUP_RETRO));
	cb = FmtNum (th);
	NumOut (cb, engstat_str[1], min(strlen(cb),(size_t)5), grp->Vtx+vofs);
	// Update hover engine readout
	vofs += 4*5;
	cb = FmtNum (enghovr*g_focusobj->GetThrusterGroupMaxth(THGROUP_HOVER));
	NumOut (cb, engstat_str[2], min(strlen(cb),(size_t)5), grp->Vtx+vofs);
	// Update trim indicator and readout
	vofs += 4*5;
	if (g_focusobj->bElevTrim) {
		double trim = g_focusobj->ctrlsurf_level[AIRCTRL_ELEVATORTRIM].curr;
		if (trim != elevtrim) {
			for (j = 0; j < 4; j++)
				grp->Vtx[vofs+j].y = dy + (95.0f + 5.0f*(j/2) - (float)trim*14.5f)*scale;
			char cbuf[16];
			sprintf (cbuf, "%3.1f", fabs(trim));
			NumOut (cbuf, engstat_str[3], min(strlen(cbuf),(size_t)3), grp->Vtx+vofs+4);
			float ofs = (fabs(trim) < 0.01 ? 0.0f : trim < 0 ? 15.0f : 31.0f);
			for (j = 0; j < 4; j++)
				grp->Vtx[vofs+16+j].tu = (314.0f+ofs+14.0f*(j%2))/texw;
			elevtrim = trim;
		}
	}
	vofs += 4*5;

	gc->clbkRender2DPanel (&surf, (MESHHANDLE)&mesh, &transf, transpmfd);
}

bool DefaultPanel::ProcessMouse (UINT event, DWORD state, int x, int y)
{
	switch (event) {
	case WM_LBUTTONDOWN:
		mstate = PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED;
		break;
	case WM_RBUTTONDOWN:
		mstate = PANEL_MOUSE_RBDOWN | PANEL_MOUSE_RBPRESSED;
		break;
	case WM_LBUTTONUP:
		mstate = PANEL_MOUSE_LBUP;
		break;
	case WM_RBUTTONUP:
		mstate = PANEL_MOUSE_RBUP;
		break;
	default:
		mstate = 0;
		break;
	}
	if (mstate & PANEL_MOUSE_DOWN) { // locate mouse event
		int mfd, btn;
		if (GetMFDButton (x, y, mfd, btn)) {
			PressMFDButton (mfd, btn);
			return true;
		} else if (GetNavButton (x, y, btn)) {
			PressNavButton (btn);
			return true;
		} else if (GetRCSButton (x, y, btn)) {
			PressRCSButton (btn);
			return true;
		} else if (GetTrimButton (x, y, btn)) {
			PressTrimButton (btn);
			return true;
		}
	} else if (mstate & PANEL_MOUSE_UP) {
		if (activemfd >= 0) {
			ActivateMFDButton (activemfd, activebtn, false);
			activemfd = activebtn = -1;
		}
	}
	return false;
}

bool DefaultPanel::GetMFDButton (int mx, int my, int &mfd, int &btn) const
{
	int i, j, k;
	for (i = 0; i < 2; i++) {
		if (pane->mfd[i].instr) {
			for (j = 0; j < 2; j++) { // test for left and right buttons
				if (mx > btnx[i][j] && mx < btnx[i][j]+btnw) {
					if (my < btny[0] || my >= btny[5]+btnh) return false;
					k = (int)((my-btny[0])/(btny[1]-btny[0]));
					if (k > 5 || my-btny[k] > btnh) return false;
					else {
						mfd = i;
						btn = k+j*6;
						return true;
					}
				}
			}
			// test for bottom buttons
			if (my > bbtny && my < bbtny+btnh) {
				if (mx > bbtnx[i][0] && mx < bbtnx[i][2]+btnw) {
					k = (int)((mx-bbtnx[i][0])/(bbtnx[i][1]-bbtnx[i][0]));
					if (k > 2 || mx-bbtnx[i][k] > btnw) return false;
					else {
						mfd = i;
						btn = k+12;
						return true;
					}
				}
			}
		} else { // if MFD is off, test for power button
			if (my > bbtny && my < bbtny+btnh && mx > bbtnx[i][0] && mx < bbtnx[i][0]+btnw) {
				mfd = i;
				btn = 12;
				return true;
			}
		}
	}
	return false;
}

bool DefaultPanel::GetNavButton (int mx, int my, int &btn) const
{
	if (!pane->hud || navdispmode != 1) return false;
	if (my >= nvby && my < nvby+btnh) {
		int x0 = (pane->W - nnvb*btnw - (nnvb-1)*gapw)/2;
		for (btn = 0; btn < nnvb; btn++) {
			if (mx >= x0 && mx < x0+btnw) return true;
			x0 += btnw + gapw;
		}
	}
	return false;
}

bool DefaultPanel::GetRCSButton (int mx, int my, int &btn) const
{
	if (!pane->hud || !rcsdispmode) return false;
	if (mx >= rcsx && mx < rcsx+3*rcsw && my >= rcsy && my < rcsy+rcsh) {
		btn = (mx-rcsx)/rcsw;
		return (mx-rcsx-btn*rcsw < rcsw-7);
	}
	return false;
}

bool DefaultPanel::GetTrimButton (int mx, int my, int &btn) const
{
	if (!pane->hud || !g_focusobj->bElevTrim) return false;
	float dx = (float)gapw, dy = blockdy;
	if (mx >= dx + 135.0f*scale && mx <= dx + 148.0f*scale &&
		my >= dy +  80.0f*scale && my <= dy + 115.0f*scale) {
		btn = min(2, max (0, (int)((my - dy - 80.0f*scale)*(3.0f/(35.0f*scale)))));
		return true;
	}
	return false;
}

void DefaultPanel::PressMFDButton (int mfd, int btn)
{
	ActivateMFDButton (activemfd = mfd, activebtn = btn, true);
	switch (btn) { // standard buttom actions
		case 12: pane->ToggleMFD_on (mfd); break;
		case 13: pane->mfd[mfd].instr->ConsumeKeyBuffered (OAPI_KEY_F1); break;
		case 14: pane->mfd[mfd].instr->ConsumeKeyBuffered (OAPI_KEY_GRAVE); break;
	}
}

void DefaultPanel::PressNavButton (int btn)
{
	g_focusobj->TglNavMode (btn+1);
}

void DefaultPanel::PressRCSButton (int btn)
{
	g_focusobj->SetAttMode (btn);
}

void DefaultPanel::PressTrimButton (int btn)
{
	double tgt, ptgt = g_focusobj->ctrlsurf_level[AIRCTRL_ELEVATORTRIM].ptgt;
	switch (btn) {
		case 0: tgt = min ( 1.0, ptgt+0.2); break;
		case 1: tgt = 0.0; break;
		case 2: tgt = max (-1.0, ptgt-0.2); break;
	}
	if (tgt != ptgt) {
		g_focusobj->SetControlSurfaceLevel (AIRCTRL_ELEVATORTRIM, tgt, false);
	}
}

void DefaultPanel::GetButtonState (int &state, int &mfd, int &btn)
{
	if (activemfd >= 0 && mstate & PANEL_MOUSE_PRESSED) {
		POINT pt;
		GetCursorPos (&pt);
		g_pOrbiter->ScreenToClient (&pt);
		if (GetMFDButton (pt.x, pt.y, mfd, btn) && mfd == activemfd && btn == activebtn) {
			if (btn < 12) {
				state = mstate;
				return;
			}
		} else {
			ActivateMFDButton (activemfd, activebtn, false);
			activemfd = activebtn = -1;
			mstate = 0;
		}
	}
	state = mfd = btn = 0;
}

void DefaultPanel::MFDModeChanged(int mfd, int mode)
{
	mstate = 0;
	RepaintMFDButtons (mfd);
}

void DefaultPanel::RepaintMFDButtons (int id)
{
	if (id >= 2) return;
	if (!gc || !pane->mfd[id].instr) return;

	static char label0[3] = {0,0,0};
	GroupSpec *grp = mesh.GetGroup (id*2+1);
	int i, j, k, m, len, vofs;
	float w, cw, y, scl = scale*1.15f;
	char c;
	for (i = 0; i < 2; i++) {
		for (j = 0; j < 6; j++) {
			vofs = (i*6+j)*16+4;
			const char *label = pane->mfd[id].instr->ButtonLabel (i*6+j);
			if (!label) label = label0;
			len = strlen(label);
			for (k = 0, w = 0.0f; k < 3; k++) w += (k < len ? CHW[label[k]] : 0);
			w = btnx[id][i] + btnw/2 - w*0.5f*scl;
			for (k = 0; k < 3; k++) {
				c = (k < len ? label[k] : 0);
				cw = CHW[c]*scl;
				y  = texh-231.0f;
				if (c >= 97 && c <= 122) y += 14.0f;
				for (m = 0; m < 4; m++) {
					grp->Vtx[vofs+m].x = w + (m%2)*cw;
					grp->Vtx[vofs+m].tu = (CHX[c]+(m%2)*CHW[c])/texw;
					grp->Vtx[vofs+m].tv = (y+(m/2)*14.0f)/texh;
				}
				vofs += 4;
				w += cw;
			}
		}
	}
}

void DefaultPanel::ActivateMFDButton (int mfd, int btn, bool active)
{
	if (!gc || !pane->mfd[mfd].instr) return;
	int vofs, j;
	GroupSpec *grp = mesh.GetGroup (mfd*2+1);
	if (btn < 12) vofs = btn*4*4;
	else          vofs = 12*4*4+(btn-12)*8;
	float yofs = (active ? 68.0f : 34.0f);
	for (j = 0; j < 4; j++)
		grp->Vtx[vofs+j].tv = (texh-yofs+(j/2)*33.0f)/texh;
}

Instrument::Spec DefaultPanel::GetMFDSpec () const
{
	Instrument::Spec spec;
	spec.w = mfdw, spec.h = mfdh;
	spec.nbtl = spec.nbtr = 6;
	spec.bt_y0 = spec.h/6;
	spec.bt_dy = spec.h/7;
	spec.flag = 0;
	return spec;
}

void DefaultPanel::OptionChanged(DWORD cat, DWORD item)
{
	if (cat == OPTCAT_INSTRUMENT) {
		switch (item) {
		case OPTITEM_INSTRUMENT_MFDGENERICSIZE:
			SetGeometry();
			for (int i = 0; i < 2; i++)
				RepaintMFDButtons(i);
			break;
		case OPTITEM_INSTRUMENT_MFDGENERICTRANSP:
			transpmfd = g_pOrbiter->Cfg()->CfgLogicPrm.bMfdTransparent;
			break;
		}
	}
}

// =======================================================================
// nonmember functions

void BtnRect (oapi::Sketchpad *skp, int x1, int y1, int x2, int y2)
{
	skp->Line (x1+1, y1, x2-1, y1);
	skp->Line (x1+1, y2-1, x2-1, y2-1);
	skp->Line (x1, y1+1, x1, y2-1);
	skp->Line (x2-1, y1+1, x2-1, y2-1);
}

void NumOut (char *str, char *pstr, int len, NTVERTEX *vtx)
{
	int i, j;
	float u;
	char *s, *ps;
	for (i = 0, s = str, ps = pstr; i < len; i++, s++, ps++) {
		if (*s != *ps) {
			if (*s >= '0' && *s <= '9') {
				u = 195.5f + (*s-'0')*10.1f;
			} else switch(*s) {
				case '.': u = 296.5f; break;
				case 'k': u = 305.9f; break;
				case 'M': u = 316.0f; break;
				case 'G': u = 327.0f; break;
				default:  u = 337.1f; break;
			}
			for (j = 0; j < 4; j++) {
				vtx[i*4+j].tu = (u+(j%2)*10.1f)/texw;
			}
			*ps = *s;
		}
	}
}

char *FmtNum (double f)
{
	static char strbuf[16];
	double absf = fabs (f);
	if (absf < 1e4) {
		if		(absf < 1e2)  sprintf (strbuf, "%5.2f", f);
		else if (absf < 1e3)  sprintf (strbuf, "%5.1f", f);
		else                  sprintf (strbuf, "%4.2fk", f*1e-3);
	} else if (absf < 1e7) {
		if      (absf < 1e5)  sprintf (strbuf, "%4.1fk", f*1e-3);
		else if (absf < 1e6)  sprintf (strbuf, "%4.0fk", f*1e-3);
		else                  sprintf (strbuf, "%4.2fM", f*1e-6);
	} else if (absf < 1e10) {
		if      (absf < 1e8)  sprintf (strbuf, "%4.1fM", f*1e-6);
		else if (absf < 1e9)  sprintf (strbuf, "%4.0fM", f*1e-6);
		else                  sprintf (strbuf, "%4.2fG", f*1e-9);
	} else {
		if      (absf < 1e11) sprintf (strbuf, "%4.1fG", f*1e-9);
		else if (absf < 1e12) sprintf (strbuf, "%4.0fG", f*1e-9);
		else                  sprintf (strbuf, "%5e", f);
	}
	return strbuf;
}
