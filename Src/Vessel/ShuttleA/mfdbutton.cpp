// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// mfdbutton.cpp
// User interface for MFD buttons
// ==============================================================

#define STRICT 1
#include "mfdbutton.h"

// MFD button font geometry

const int MFD_font_xpos[256] = {   // character x-positions in texture
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,80/*(*/,87/*)*/,93/***/,101/*+*/,111/*,*/,116/*-*/,125/*.*/,130/*/*/,
	137/*0*/,147/*1*/,156/*2*/,166/*3*/,175/*4*/,185/*5*/,195/*6*/,204/*7*/,214/*8*/,224/*9*/,234/*:*/,240/*;*/,247/*<*/,256/*=*/,267/*>*/,278/*?*/,
	288/*@*/,304/*A*/,316/*B*/,328/*C*/,340/*D*/,352/*E*/,362/*F*/,373/*G*/,386/*H*/,398/*I*/,404/*J*/,414/*K*/,425/*L*/,435/*M*/,449/*N*/,461/*O*/,
	474/*P*/,485/*Q*/,497/*R*/,2/*S*/,13/*T*/,24/*U*/,36/*V*/,48/*W*/,64/*X*/,76/*Y*/,87/*Z*/,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
};

const int MFD_font_width[256] = {   // character widths in texture
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,6/*(*/,6/*)*/,7/***/,9/*+*/,5/*,*/,9/*-*/,5/*.*/,7/*/*/,
	9/*0*/,9/*1*/,9/*2*/,9/*3*/,9/*4*/,9/*5*/,9/*6*/,9/*7*/,9/*8*/,9/*9*/,5/*:*/,5/*;*/,8/*<*/,10/*=*/,8/*>*/,9/*?*/,
	14/*@*/,11/*A*/,11/*B*/,10/*C*/,10/*D*/,9/*E*/,9/*F*/,11/*G*/,10/*H*/,4/*I*/,8/*J*/,10/*K*/,9/*L*/,13/*M*/,10/*N*/,11/*O*/,
	9/*P*/,11/*Q*/,11/*R*/,10/*S*/,10/*T*/,10/*U*/,10/*V*/,14/*W*/,10/*X*/,9/*Y*/,10/*Z*/,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
};

const int MFD_font_ypos[2] = {0,16};
const int MFD_font_height = 16;

const int texlabelw = 44;

// horizontal MFD button centres for left/right button column of left/right MFD
const int lblx[2][2] = {{LMFD_X-26,LMFD_X+284},{RMFD_X-26,RMFD_X+284}};

// button top edges
const int lbly[6] = {PANEL2D_MAINH-272, PANEL2D_MAINH-234, PANEL2D_MAINH-196, PANEL2D_MAINH-158, PANEL2D_MAINH-120, PANEL2D_MAINH-82};

// ==============================================================

MFDButtonCol::MFDButtonCol (VESSEL3 *v, DWORD _mfdid, DWORD _lr): PanelElement (v)
{
	mfdid = _mfdid;
	lr = _lr;
	xcnt = PANELEL_TEXW - texlabelw/2;
};

// ==============================================================

void MFDButtonCol::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	static const DWORD nbtn = 6;
	static const DWORD nvtx = 4*nbtn;
	static const DWORD nidx = 6*nbtn;
	static const WORD base_idx[6] = { 0,1,2, 3,2,1 };

	DWORD btn, i;
	int x0 = lblx[mfdid][lr]-15;
	NTVERTEX vtx[nvtx];
	WORD idx[nidx];
	memset (vtx, 0, nvtx*sizeof(NTVERTEX));
	for (btn = 0; btn < nbtn; btn++) {
		for (i = 0; i < 4; i++) {
			vtx[btn*4+i].x = (float)(x0+(i%2)*30);
			vtx[btn*4+i].y = (float)(lbly[btn] + (i/2)*13 + 1.5);
			vtx[btn*4+i].tu = (float)(PANELEL_TEXW - (i%2 ? 0 : texlabelw))/(float)PANELEL_TEXW;
			vtx[btn*4+i].tv = (float)(PANELEL_TEXH - MFD_font_height*(24-mfdid*12-lr*6-btn-(i/2?1:0)))/(float)PANELEL_TEXH;
		}
		for (i = 0; i < 6; i++)
			idx[btn*6+i] = (WORD)btn*4 + base_idx[i];
	}
	AddGeometry (hMesh, grpidx, vtx, nvtx, idx, nidx);
}

// ==============================================================

bool MFDButtonCol::Redraw2D (SURFHANDLE surf)
{
	int btn, x, y, ysrc, len, i, w;
	const char *label;

	y = PANELEL_TEXH - MFD_font_height*(24-mfdid*12-lr*6);

	// blank buttons
	oapiBlt (surf, surf, xcnt-texlabelw/2, y, xcnt-texlabelw/2, PANELEL_TEXH-MFD_font_height*30, texlabelw, MFD_font_height*6);

	// write labels
	for (btn = 0; btn < 6; btn++) {
		if (label = oapiMFDButtonLabel (mfdid, btn+lr*6)) {
			len = strlen(label);
			for (w = i = 0; i < len; i++) w += MFD_font_width[label[i]];
			for (i = 0, x = xcnt-w/2; i < len; i++) {
				w = MFD_font_width[label[i]];
				ysrc = (label[i] < 'S' ? 0:16);
				if (w) {
					oapiBlt (surf, surf, x, y, MFD_font_xpos[label[i]], ysrc, w, MFD_font_height);
					x += w;
				}
			}
		} else break;
		y += MFD_font_height;
	}
    return false;
}

// ==============================================================

bool MFDButtonCol::ProcessMouse2D (int event, int mx, int my)
{
	if (my%38 < 27) {
		int bt = my/38 + lr*6;
		oapiProcessMFDButton (mfdid, bt, event);
		return true;
	} else
		return false;
}


// ==============================================================
// ==============================================================

MFDButtonRow::MFDButtonRow (VESSEL3 *v, DWORD _mfdid): PanelElement (v)
{
	mfdid = _mfdid;
}

// ==============================================================

bool MFDButtonRow::ProcessMouse2D (int event, int mx, int my)
{
	bool proc = false;
	if (mx % 50 < 37) {
		int bt = mx/50;
		switch (bt) {
		case 0: oapiToggleMFD_on (mfdid); return true;
		case 1: oapiSendMFDKey (mfdid, OAPI_KEY_F1); return true;
		case 2: oapiSendMFDKey (mfdid, OAPI_KEY_GRAVE); return true;
		}
	}
	return false;
}
