// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define OAPI_IMPLEMENTATION

#include "hud.h"
#include "Pane.h"
#include "VCockpit.h"
#include "Camera.h"
#include "Psys.h"
#include "Pane.h"
#include "Log.h"
#include <fstream>

using namespace std;

extern Orbiter *g_pOrbiter;
extern Camera *g_camera;
extern Vessel *g_focusobj;
extern PlanetarySystem *g_psys;
extern Pane *g_pane;
extern InputBox *g_input;
extern char DBG_MSG[256];

const double MAXALT_HUD = 1e15; // make user-selectable

void BoxCoord (const Vector &dockpos, const Vector &dockdir,
			   const Vector &refdir, double dist, Vector *crd);

// =======================================================================
// class HUD

static double hudtexw = 512.0;
static double hudtexh = 256.0;
static WORD rectidx[6] = {0,2,1, 2,3,1};

Mesh HUD::hudMesh;

HUD::HUD (const Pane *_pane): pane(_pane)
{
	colidx = pane->colidx;
	gc = pane->gc;
	font = NULL;
	font2 = NULL;
	Resize (pane->GetVC() != NULL && pane->GetVC()->GetHUDSurf() != NULL);
	hudTex = (gc ? LoadTexture (colidx) : NULL);
	transp = g_pOrbiter->Cfg()->CfgLogicPrm.bMfdTransparent;
}

HUD::~HUD ()
{
	if (gc) {
		if (font) gc->clbkReleaseFont (font);
		if (font2) gc->clbkReleaseFont(font2);
		if (hudTex) gc->clbkReleaseSurface (hudTex);
	}
}

void HUD::SwitchColour (int idx)
{
	if (!gc) return;
	if (idx == colidx) return;
	colidx = idx % 4;
	if (hudTex) gc->clbkReleaseSurface (hudTex);
	hudTex = LoadTexture (colidx);
}

SURFHANDLE HUD::LoadTexture (int idx)
{
	char cbuf[64] = "Cockpit\\hud";
	switch (idx) {
		case 1: strcat (cbuf, "_red"); break;
		case 2: strcat (cbuf, "_yellow"); break;
		case 3: strcat (cbuf, "_blue"); break;
	}
	strcat (cbuf, ".dds");
	return gc->clbkLoadSurface(cbuf, OAPISURFACE_RENDERTARGET | OAPISURFACE_TEXTURE);
}

void HUD::Draw (oapi::Sketchpad *skp)
{
	static const double onedeg = tan (RAD*1.0);
	if (g_camera->IsExternal()) return; // nothing to do

	if (bVC) {
		HUDofs.Set ((VCcnt - g_camera->CockpitPos()) * VCscale);
		spec.Scale = HUDofs.z * onedeg;
	} else {
		spec.Scale = g_camera->Scale() * onedeg;
	}

	// Determine HUD centre coordinates
	if (bVC || g_camera->IsCockpitForward()) {
		spec.CX = HRES05 - (int)HUDofs.x, spec.CY = VRES05 + (int)HUDofs.y;
		bCNTvis = (spec.CX >= 0 && spec.CX < spec.W && spec.CY >= 0 && spec.CY < spec.H);
	} else {
		D3DVECTOR homog;
		bCNTvis = pane->GlobalToHomog (mul (g_focusobj->GRot(), Vector(0,0,1)), homog);
		if (bCNTforward = (homog.z > 0.0)) {
			spec.CX = (int)(HRES05*(1.0f+homog.x));
			spec.CY = (int)(VRES05*(1.0f-homog.y));
		} else {
			spec.CX = -10000;
			spec.CY = -10000;
		}
	}

	skp->SetFont (font);
	skp->SetPen (pane->hudpen);
	skp->SetTextColor (pane->hudCol);
	g_focusobj->DrawHUD (this, skp);
}

void HUD::DrawDefault (oapi::Sketchpad *skp)
{
	if (bVC) {
		Display (skp);
		DrawCenterMarker (skp);
  	    skp->Text (10, 0, ModeIDString(), strlen (ModeIDString()));
	}
}

void HUD::RenderDefault ()
{
	int ivtx = 0, iidx = 0;
	AddMesh_CenterMarker (ivtx, iidx);
	UpdateMesh (ivtx, iidx);

	if (ivtx && iidx) {
		static MATRIX3 transf = {1,-0.5,0, 0,1,-0.5, 0,0,1};
		GroupSpec *gs = hudMesh.GetGroup(0);
		DWORD nvtx = gs->nVtx;
		DWORD nidx = gs->nIdx;
		gs->nVtx = ivtx;
		gs->nIdx = iidx;
		float intens = (float)g_pane->HudIntens();
		gc->clbkRender2DPanel (&hudTex, (MESHHANDLE)&hudMesh, &transf, intens, transp);
		gs->nVtx = nvtx;
		gs->nIdx = nidx;
	}
}

void HUD::RenderCustom (MESHHANDLE hMesh, SURFHANDLE *hTex)
{
	static MATRIX3 transf = {1,-0.5,0, 0,1,-0.5, 0,0,1};
	float intens = (float)g_pane->HudIntens();
	gc->clbkRender2DPanel (hTex, hMesh, &transf, intens, transp);
}

void HUD::Render ()
{
	if (bVC) return;   // don't use render method in VC

	// Determine HUD centre coordinates
	if (g_camera->IsCockpitForward()) {
		spec.CX = HRES05 - (int)HUDofs.x, spec.CY = VRES05 + (int)HUDofs.y;
		bCNTvis = (spec.CX >= 0 && spec.CX < spec.W && spec.CY >= 0 && spec.CY < spec.H);
	} else {
		D3DVECTOR homog;
		bCNTvis = pane->GlobalToHomog (mul (g_focusobj->GRot(), Vector(0,0,1)), homog);
		if (bCNTforward = (homog.z > 0.0)) {
			spec.CX = (int)(HRES05*(1.0f+homog.x));
			spec.CY = (int)(VRES05*(1.0f-homog.y));
		} else {
			spec.CX = -10000;
			spec.CY = -10000;
		}
	}
	g_focusobj->RenderHUD (this, Mode(), &spec, hudTex);
}

void HUD::Resize (bool isVC)
{
	int h;
	bVC = isVC;

	fW = pane->f1W;
	fH = pane->f1H;

	if (bVC) {
		spec.W = spec.H = g_pOrbiter->Cfg()->CfgInstrumentPrm.PanelMFDHUDSize;
		HRES05 = VRES05 = (spec.W/2);
		ladder_width = (HRES05*60)/128;
		ladder_range = 250;
		const VECTOR3 &cnt = pane->GetVC()->GetHUDParams()->hudcnt;
		VCcnt.Set (cnt.x, cnt.y, cnt.z);
		VCscale = spec.H / pane->GetVC()->GetHUDParams()->size;
		spec.Markersize = 6+(10*HRES05)/128;
		boxx = fW*3;
		h = 4+(8*HRES05)/128;
	} else {
		HRES05 = (spec.W = pane->W)/2;
		VRES05 = (spec.H = pane->H)/2;
		ladder_width = 0.12*spec.W;
		ladder_range = 0.43*spec.H;
		HUDofs.Set (0, 0, g_camera->Scale());
		spec.Markersize = max(20,spec.H/25); // spec.W/40;
		boxx = spec.Markersize*10; //HRES05 >> 1;
		h = spec.H/50;
	}

	if (font)
		gc->clbkReleaseFont (font);
	font = gc->clbkCreateFont (-h, false, "Fixed");
	if (font2) 
		gc->clbkReleaseFont(font2);
	font2 = gc->clbkCreateFont(-(h*2)/3, false, "Arial");

	if (bVC) {
		SURFHANDLE hudsurf = pane->GetVC()->GetHUDSurf();
		oapi::GraphicsClient *gc = g_pOrbiter->GetGraphicsClient();
		if (gc) {
			oapi::Sketchpad *skp = gc->clbkGetSketchpad (hudsurf);
			if (skp) {
				skp->SetFont (font);
				DWORD charsize = skp->GetCharSize();
				fW = HIWORD(charsize);
				fH = LOWORD(charsize);
				gc->clbkReleaseSketchpad (skp);
			}
		}
	}

	// Precompute mesh render parameters
	if (!bVC) {
		renderprm.scal = spec.Markersize*0.024; // HRES05*0.0012; // size scaling parameters
	}
}

GroupSpec *HUD::EnsureMeshBuffer(int grp, int nvtx, int nidx)
{
	GroupSpec *gs = hudMesh.GetGroup (grp);
	while (!gs) {
		hudMesh.AddGroup(NULL,0,NULL,0,0,0);
		gs = hudMesh.GetGroup(grp);
	}

	if (gs->nVtx < nvtx) {
		NTVERTEX *vtx = new NTVERTEX[nvtx];
		memset(vtx, 0, nvtx*sizeof(NTVERTEX));
		if (gs->nVtx) {
			memcpy (vtx, gs->Vtx, gs->nVtx*sizeof(NTVERTEX));
			delete []gs->Vtx;
		}
		gs->Vtx = vtx;
		gs->nVtx = nvtx;
	}
	if (gs->nIdx < nidx) {
		WORD *idx = new WORD[nidx];
		memset(idx, 0, nidx*sizeof(WORD));
		if (gs->nIdx) {
			memcpy (idx, gs->Idx, gs->nIdx*sizeof(WORD));
			delete []gs->Idx;
		}
		gs->Idx = idx;
		gs->nIdx = nidx;
	}
	return gs;
}

int HUD::TexBltString (const char *str, int tgtx, int tgty)
{
	static const int x0[256] = {
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		341/* */,0,0,0,0,0,0,0,0,0,0,0,0,329/*-*/,186/*.*/,0,
		234/*0*/,241/*1*/,248/*2*/,256/*3*/,263/*4*/,270/*5*/,277/*6*/,284/*7*/,292/*8*/,299/*9*/,0,0,0,0,0,0,
		0,1/*A*/,11/*B*/,20/*C*/,30/*D*/,39/*E*/,48/*F*/,56/*G*/,66/*H*/,75/*I*/,79/*J*/,86/*K*/,95/*L*/,103/*M*/,114/*N*/,123/*O*/,
		134/*P*/,142/*Q*/,152/*R*/,162/*S*/,170/*T*/,178/*U*/,187/*V*/,196/*W*/,208/*X*/,217/*Y*/,226/*Z*/,300/*[*/,0,304/*]*/,0,0,
		0,1/*a*/,0,0,0,0,0,0,0,0,0,66/*k*/,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,177/*z*/,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	};
	static const int dx[256] = {
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		7/* */,0,0,0,0,0,0,0,0,0,0,0,0,6/*-*/,5/*.*/,0,
		7/*0*/,7/*1*/,7/*2*/,7/*3*/,7/*4*/,7/*5*/,7/*6*/,7/*7*/,7/*8*/,7/*9*/,0,0,0,0,0,0,
		0,10/*A*/,9/*B*/,10/*C*/,9/*D*/,9/*E*/,8/*F*/,10/*G*/,9/*H*/,4/*I*/,7/*J*/,9/*K*/,8/*L*/,11/*M*/,9/*N*/,11/*O*/,
		9/*P*/,10/*Q*/,10/*R*/,8/*S*/,8/*T*/,9/*U*/,9/*V*/,12/*W*/,9/*X*/,9/*Y*/,8/*Z*/,4/*[*/,0,4/*]*/,0,0,
		0,8/*a*/,0,0,0,0,0,0,0,0,0,8/*k*/,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,7/*z*/,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	};
	static const int y0[256] = {
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		78/* */,0,0,0,0,0,0,0,0,0,0,0,0,78/*-*/,91/*.*/,0,
		78/*0*/,78/*1*/,78/*2*/,78/*3*/,78/*4*/,78/*5*/,78/*6*/,78/*7*/,78/*8*/,78/*9*/,0,0,0,0,0,0,
		0,78/*A*/,78/*B*/,78/*C*/,78/*D*/,78/*E*/,78/*F*/,78/*G*/,78/*H*/,78/*I*/,78/*J*/,78/*K*/,78/*L*/,78/*M*/,78/*N*/,78/*O*/,
		78/*P*/,78/*Q*/,78/*R*/,78/*S*/,78/*T*/,78/*U*/,78/*V*/,78/*W*/,78/*X*/,78/*Y*/,78/*Z*/,91/*[*/,0,91/*]*/,0,0,
		0,91/*a*/,0,0,0,0,0,0,0,0,0,91/*k*/,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,91/*z*/,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	};
	static const int dy = 13;
	int tgtx0 = tgtx;
	const char *c;
	for (c = str; *c; c++) {
		gc->clbkBlt (hudTex, tgtx, tgty, hudTex, x0[*c], y0[*c], dx[*c], dy);
		tgtx += dx[*c];
	}
	return tgtx-tgtx0;
}

void HUD::AddMesh_Billboard (int &ivtx, int &iidx, double x0, double y0, double dx, double dy, int texx, int texy, int texw, int texh)
{
	const int nvtx = 4;
	const int nidx = 6;
	int i;
	GroupSpec *gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);
	for (i = 0; i < nidx; i++) gs->Idx[iidx++] = ivtx+rectidx[i];
	NTVERTEX *vtx = gs->Vtx+ivtx;
	vtx[2].x = vtx[3].x = (vtx[0].x = vtx[1].x = (float)x0) + (float)dx;
	vtx[1].y = vtx[3].y = (vtx[0].y = vtx[2].y = (float)y0) + (float)dy;
	vtx[0].tu = vtx[1].tu = (float)(texx/hudtexw);
	vtx[2].tu = vtx[3].tu = (float)((texx+texw)/hudtexw);
	vtx[0].tv = vtx[2].tv = (float)(texy/hudtexh);
	vtx[1].tv = vtx[3].tv = (float)((texy+texh)/hudtexh);
	ivtx += nvtx;
}

void HUD::AddMesh_CenterMarker (int &ivtx, int &iidx)
{
	const int nvtx = 4;
	const int nidx = 6;
	int i;

	GroupSpec *gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);

	for (i = 0; i < nidx; i++) gs->Idx[iidx++] = ivtx+rectidx[i];

	// center marker geometry
	float x0, dx, y0, dy, u0, u1, v0, v1;
	x0 = (float)(spec.CX - spec.Markersize*1.0);
	dx = (float)(spec.Markersize*2.0);
	y0 = (float)(spec.CY - spec.Markersize*0.07936);
	dy = (float)(spec.Markersize*0.53968);
	u0 = (float)(316.0/hudtexw);
	u1 = (float)(379.0/hudtexw);
	v0 = 0.0f;
	v1 = (float)(17.0/hudtexh);
	NTVERTEX *cntmkr = gs->Vtx+ivtx;
	cntmkr[2].x = cntmkr[3].x = (cntmkr[0].x = cntmkr[1].x = x0) + dx;
	cntmkr[1].y = cntmkr[3].y = (cntmkr[0].y = cntmkr[2].y = y0) + dy;
	cntmkr[0].tu = cntmkr[1].tu = u0;
	cntmkr[2].tu = cntmkr[3].tu = u1;
	cntmkr[0].tv = cntmkr[2].tv = v0;
	cntmkr[1].tv = cntmkr[3].tv = v1;
	ivtx += nvtx;
}

bool HUD::AddMesh_Marker (int &ivtx, int &iidx, const Vector &dir, int markerid, double *xcnt, double *ycnt)
{
	double x, y;

	if (GlobalToHUD (dir, x, y)) {
		int i;
		double dx = spec.Markersize*0.7, u0, v0;
		double dt = 48.0;
		const int nvtx = 4;
		const int nidx = 6;
		GroupSpec *gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);

		for (i = 0; i < nidx; i++) gs->Idx[iidx++] = ivtx+rectidx[i];

		gs->Vtx[ivtx+0].x = gs->Vtx[ivtx+1].x = (float)(x-dx);
		gs->Vtx[ivtx+2].x = gs->Vtx[ivtx+3].x = (float)(x+dx);
		gs->Vtx[ivtx+0].y = gs->Vtx[ivtx+2].y = (float)(y-dx);
		gs->Vtx[ivtx+1].y = gs->Vtx[ivtx+3].y = (float)(y+dx);
		u0 = 463.5/hudtexw;
		v0 = (0.5 + (markerid*49))/hudtexh;
		gs->Vtx[ivtx+0].tu = gs->Vtx[ivtx+1].tu = (float)u0;
		gs->Vtx[ivtx+2].tu = gs->Vtx[ivtx+3].tu = (float)(u0+dt/hudtexw);
		gs->Vtx[ivtx+0].tv = gs->Vtx[ivtx+2].tv = (float)v0;
		gs->Vtx[ivtx+1].tv = gs->Vtx[ivtx+3].tv = (float)(v0+dt/hudtexh);
		ivtx += 4;
		if (xcnt) *xcnt = x;
		if (ycnt) *ycnt = y;
		return true;
	} else {
		return false;
	}
}

void HUD::AddMesh_DirectionMarker (int &ivtx, int &iidx, const Vector &dir, bool prograde, double *xcnt, double *ycnt)
{
	int i;
	//double scal = VRES05*0.00075; // HRES05*0.0012;
	double dx = spec.Markersize*0.55;//scal*26;
	double dy = spec.Markersize*0.83;//scal*39;
	double rad = dy*4.0;

	Vector d;
	if (bVC) d.Set (tmul (g_focusobj->GRot(), dir));
	else     d.Set (tmul (g_camera->GRot(), dir));
	double len = std::hypot (d.x, d.y);
	if (!len) return;
	double cosa = d.y/len, sina = -d.x/len;

	const int nvtx = 4;
	const int nidx = 6;
	GroupSpec *gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);

	for (i = 0; i < nidx; i++) gs->Idx[iidx++] = ivtx+rectidx[i];

	double vofs = (prograde ? 58.5:0.5);
	double vtxx[nvtx] = {-dx,-dx,dx,dx};
	double vtxy[nvtx] = {-dy-rad,dy-rad,-dy-rad,dy-rad};
	double vtxu[nvtx] = {423.5/hudtexw, 423.5/hudtexw, 462.5/hudtexw, 462.5/hudtexw};
	double vtxv[nvtx] = {vofs/hudtexh, (vofs+57.0)/hudtexh, vofs/hudtexh, (vofs+57.0)/hudtexh};

	for (i = 0; i < nvtx; i++, ivtx++) {
		gs->Vtx[ivtx].x = (float)(spec.CX + vtxx[i]*cosa + vtxy[i]*sina);
		gs->Vtx[ivtx].y = (float)(spec.CY - vtxx[i]*sina + vtxy[i]*cosa);
		gs->Vtx[ivtx].tu = (float)vtxu[i];
		gs->Vtx[ivtx].tv = (float)vtxv[i];
	}

	if (xcnt) *xcnt = spec.CX - rad*sina;
	if (ycnt) *ycnt = spec.CY - rad*cosa;
}

void HUD::AddMesh_Readout (int &ivtx, int &iidx, int side, const char *str, int label)
{
	int i, j, vtxofs = ivtx;
	double cw, ch, dx, dy, x0, y0, lx0, ldx, ly0, ldy, uofs = 0.0, vofs;
	int len = strlen(str);
	int cidx = len-1;
	bool rmarker = (str[cidx] == 'R');
	if (rmarker) cidx--;
	int nbox = 1;
	const int nvtx = (len+2)*4;
	const int nidx = (len+2)*6;

	cw = renderprm.scal*14;
	ch = renderprm.scal*21;
	dx = cw*6 + renderprm.scal*9;
	dy = ch + renderprm.scal*8;
	x0 = (side ? HRES05+boxx-dx : HRES05-boxx);
	//x0 = (side ? spec.W-boxx-dx : boxx);
	y0 = fH*3;

	GroupSpec *gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);

	gs->Vtx[ivtx+2].x = gs->Vtx[ivtx+3].x = (gs->Vtx[ivtx+0].x = gs->Vtx[ivtx+1].x = (float)x0) + (float)dx;
	gs->Vtx[ivtx+1].y = gs->Vtx[ivtx+3].y = (gs->Vtx[ivtx+0].y = gs->Vtx[ivtx+2].y = (float)y0) + (float)dy;
	gs->Vtx[ivtx+0].tu = gs->Vtx[ivtx+1].tu = (float)(353.5/hudtexw);
	gs->Vtx[ivtx+2].tu = gs->Vtx[ivtx+3].tu = (float)(422.5/hudtexw);
	gs->Vtx[ivtx+0].tv = gs->Vtx[ivtx+2].tv = (float)(62.5/hudtexh);
	gs->Vtx[ivtx+1].tv = gs->Vtx[ivtx+3].tv = (float)(85.5/hudtexh);
	ivtx += 4;

	if (label) {
		ldx = renderprm.scal*26;
		ldy = renderprm.scal*11;
		lx0 = (side ? x0 + dx + renderprm.scal*2 : x0 - ldx - renderprm.scal*2);
		ly0 = y0 + renderprm.scal*14;
		gs->Vtx[ivtx+2].x = gs->Vtx[ivtx+3].x = (gs->Vtx[ivtx+0].x = gs->Vtx[ivtx+1].x = (float)lx0) + (float)ldx;
		gs->Vtx[ivtx+1].y = gs->Vtx[ivtx+3].y = (gs->Vtx[ivtx+0].y = gs->Vtx[ivtx+2].y = (float)ly0) + (float)ldy;
		gs->Vtx[ivtx+2].tu = gs->Vtx[ivtx+3].tu = (gs->Vtx[ivtx+0].tu = gs->Vtx[ivtx+1].tu = (float)((146+label*26)/hudtexw)) + (float)(26/hudtexw);
		gs->Vtx[ivtx+1].tv = gs->Vtx[ivtx+3].tv = (gs->Vtx[ivtx+0].tv = gs->Vtx[ivtx+2].tv = (float)(62.5/hudtexh)) + (float)(11/hudtexh);
		ivtx += 4;
		nbox++;
	}
	if (rmarker) {
		ldx = renderprm.scal*8;
		ldy = renderprm.scal*11;
		lx0 = (side ? x0 - ldx - renderprm.scal*2 : x0 + dx + renderprm.scal*2);
		ly0 = y0 + renderprm.scal*14;
		gs->Vtx[ivtx+2].x = gs->Vtx[ivtx+3].x = (gs->Vtx[ivtx+0].x = gs->Vtx[ivtx+1].x = (float)lx0) + (float)ldx;
		gs->Vtx[ivtx+1].y = gs->Vtx[ivtx+3].y = (gs->Vtx[ivtx+0].y = gs->Vtx[ivtx+2].y = (float)ly0) + (float)ldy;
		gs->Vtx[ivtx+2].tu = gs->Vtx[ivtx+3].tu = (gs->Vtx[ivtx+0].tu = gs->Vtx[ivtx+1].tu = (float)(198/hudtexw)) + (float)(8/hudtexw);
		gs->Vtx[ivtx+1].tv = gs->Vtx[ivtx+3].tv = (gs->Vtx[ivtx+0].tv = gs->Vtx[ivtx+2].tv = (float)(62.5/hudtexh)) + (float)(11/hudtexh);
		ivtx += 4;
		nbox++;
	}

	x0 += cw*5 + renderprm.scal*4.5;
	y0 += renderprm.scal*4;
	vofs = 61.5/hudtexh;

	switch (str[cidx]) {
	case 'k':
		uofs = 130/hudtexw;
		break;
	case 'M':
		uofs = 140/hudtexw;
		break;
	case 'G':
		uofs = 150/hudtexw;
		break;
	case 'T':
		uofs = 160/hudtexw;
		break;
	}
	if (uofs) {
		gs->Vtx[ivtx+2].x = gs->Vtx[ivtx+3].x = (gs->Vtx[ivtx+0].x = gs->Vtx[ivtx+1].x = (float)x0) + (float)cw;
		gs->Vtx[ivtx+1].y = gs->Vtx[ivtx+3].y = (gs->Vtx[ivtx+0].y = gs->Vtx[ivtx+2].y = (float)y0) + (float)ch;
		gs->Vtx[ivtx+0].tu = gs->Vtx[ivtx+1].tu = (float)uofs;
		gs->Vtx[ivtx+2].tu = gs->Vtx[ivtx+3].tu = (float)(uofs+10/hudtexw);
		gs->Vtx[ivtx+0].tv = gs->Vtx[ivtx+2].tv = (float)vofs;
		gs->Vtx[ivtx+1].tv = gs->Vtx[ivtx+3].tv = (float)(vofs+15/hudtexh);
		ivtx += 4;
		nbox++;
		cidx--;
	}
	x0 -= cw;
	while (cidx >= 0) {
		if (str[cidx] >= '0' && str[cidx] <= '9') {
			uofs = (str[cidx]-'0')*10/hudtexw;
		} else if (str[cidx] == '.') {
			uofs = 120/hudtexw;
		} else if (str[cidx] == '-') {
			uofs = 110/hudtexw;
		} else {
			uofs = 330/hudtexw;
		}
		gs->Vtx[ivtx+2].x = gs->Vtx[ivtx+3].x = (gs->Vtx[ivtx+0].x = gs->Vtx[ivtx+1].x = (float)x0) + (float)cw;
		gs->Vtx[ivtx+1].y = gs->Vtx[ivtx+3].y = (gs->Vtx[ivtx+0].y = gs->Vtx[ivtx+2].y = (float)y0) + (float)ch;
		gs->Vtx[ivtx+0].tu = gs->Vtx[ivtx+1].tu = (float)uofs;
		gs->Vtx[ivtx+2].tu = gs->Vtx[ivtx+3].tu = (float)(uofs+10/hudtexw);
		gs->Vtx[ivtx+0].tv = gs->Vtx[ivtx+2].tv = (float)vofs;
		gs->Vtx[ivtx+1].tv = gs->Vtx[ivtx+3].tv = (float)(vofs+15/hudtexh);
		ivtx += 4;
		nbox++;
		x0 -= cw;
		cidx--;
	}

	for (j = 0; j < nbox; j++)
		for (i = 0; i < 6; i++)
			gs->Idx[iidx++] = (vtxofs+j*4)+rectidx[i];
}

void HUD::AddMesh_LadderBar (int &ivtx, int &iidx, double sina, double cosa,
	double d, int phi10, bool mark_subzero)
{
	GroupSpec *gs;
	double scal = renderprm.scal; //HRES05*0.0012;
	int i, j, k;

	if (phi10 > 9)       phi10 =  18-phi10;
	else if (phi10 < -9) phi10 = -18-phi10;
	int lab = abs (phi10);
	if (lab != 9) {
		if (lab) {
			static const int nvtx = 22;
			static const int nidx = 36;
			static WORD idx1[nidx] = {0,1,2, 2,1,3, 2,3,4, 4,3,5,  6,8,7, 8,9,7, 10,12,11, 12,13,11, 14,16,15, 16,17,15,  18,20,19, 20,21,19};
			static WORD idx2[nidx] = {0,2,1, 2,3,1, 2,4,3, 4,5,3,  6,8,7, 8,9,7, 10,12,11, 12,13,11, 14,16,15, 16,17,15,  18,20,19, 20,21,19};
			WORD *idx;
			double x0, x2l, x3l, x4l, x2r, x3r, x4r, y0, y1, y2;
			x0 = scal*120.0;
			x2l = -scal*160.0, x3l = -scal*150.0, x4l = -scal*130.0;
			x2r =  scal*130.0, x3r =  scal*140.0, x4r =  scal*160.0;
			y2 = -scal*8.0;
			if (phi10 > 0) {
				y0 = -scal*3.0;
				y1 = scal*15.0;
				idx = idx2;
			} else {
				y0 = scal*3.0;
				y1 = -scal*15.0;
				idx = idx1;
			}
			double vtxx[nvtx] = {-x0,-x0,0,0,x0,x0,                                               // bar
				x2l,x2l,x3l,x3l, x3l,x3l,x4l,x4l,                                                 // left label
				x2r,x2r,x3r,x3r, x3r,x3r,x4r,x4r                                                  // right label
			};
			double vtxy[nvtx] = {y0,y1,y0,y1,y0,y1,                                               // bar
				y2,-y2,y2,-y2, y2,-y2,y2,-y2,                                                     // left label
				y2,-y2,y2,-y2, y2,-y2,y2,-y2                                                      // right label
			};
			i = 53+lab*20;           // select pitch label
			j = 254+(phi10>0 ? 0:10); // select pitch label sign
			k = (mark_subzero && phi10<0 ? 19:1);
			double vtxu[nvtx] = {0,0,60.0/hudtexw,60.0/hudtexw,0,0,                                                   // bar
				j/hudtexw,j/hudtexw,(j+10)/hudtexw,(j+10)/hudtexw,i/hudtexw,i/hudtexw,(i+20)/hudtexw,(i+20)/hudtexw,  // left label
				j/hudtexw,j/hudtexw,(j+10)/hudtexw,(j+10)/hudtexw,i/hudtexw,i/hudtexw,(i+20)/hudtexw,(i+20)/hudtexw   // right label
			};
			double vtxv[nvtx] = {k/hudtexh,(18+k)/hudtexh,k/hudtexh,(18+k)/hudtexh,k/hudtexh,(18+k)/hudtexh,          // bar
				1/hudtexh,17/hudtexh,1/hudtexh,17/hudtexh,1/hudtexh,17/hudtexh,1/hudtexh,17/hudtexh,                  // left label
				1/hudtexh,17/hudtexh,1/hudtexh,17/hudtexh,1/hudtexh,17/hudtexh,1/hudtexh,17/hudtexh                   // right label
			};
			gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);
			for (i = 0; i < nidx; i++) {
				gs->Idx[iidx++] = idx[i]+ivtx;
			}
			for (i = 0; i < nvtx; i++, ivtx++) {
				gs->Vtx[ivtx].x = (float)(spec.CX + vtxx[i]*cosa - (vtxy[i]+d)*sina);
				gs->Vtx[ivtx].y = (float)(spec.CY + vtxx[i]*sina + (vtxy[i]+d)*cosa);
				gs->Vtx[ivtx].tu = (float)vtxu[i];
				gs->Vtx[ivtx].tv = (float)vtxv[i];
			}
		} else {
			static const int nvtx = 4;
			static const int nidx = 6;
			double x0 = scal*220;
			double y0 = -scal*4.0;
			double vtxx[nvtx] = {-x0,-x0,x0,x0};
			double vtxy[nvtx] = {y0,-y0,y0,-y0};
			double vtxu[nvtx] = {16.0/hudtexw,16.0/hudtexw,16.0/hudtexw,16.0/hudtexw};
			double vtxv[nvtx] = {0,8/hudtexh,0,8/hudtexh};
			gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);
			gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);
			for (i = 0; i < nidx; i++) {
				gs->Idx[iidx++] = rectidx[i]+ivtx;
			}
			for (i = 0; i < nvtx; i++, ivtx++) {
				gs->Vtx[ivtx].x = (float)(spec.CX + vtxx[i]*cosa - (vtxy[i]+d)*sina);
				gs->Vtx[ivtx].y = (float)(spec.CY + vtxx[i]*sina + (vtxy[i]+d)*cosa);
				gs->Vtx[ivtx].tu = (float)vtxu[i];
				gs->Vtx[ivtx].tv = (float)vtxv[i];
			}
		}
	} else {
		static const int nvtx = 20;
		static const int nidx = 30;
		double x0, x2l, x3l, x4l, x2r, x3r, x4r, y2;
		x0 = scal*35;
		x2l = -scal*75, x3l = -scal*65, x4l = -scal*45;
		x2r =  scal*45,  x3r = scal*55, x4r =  scal*75;
		y2 = -scal*8.0;
		double vtxx[nvtx] = {-x0,-x0,x0,x0,    // cross
			x2l,x2l,x3l,x3l, x3l,x3l,x4l,x4l,  // left label
			x2r,x2r,x3r,x3r, x3r,x3r,x4r,x4r   // right label
		};
		double vtxy[nvtx] = {
			-x0,x0,-x0,x0,                     // cross
			y2,-y2,y2,-y2, y2,-y2,y2,-y2,      // left label
			y2,-y2,y2,-y2, y2,-y2,y2,-y2       // right label
		};
		i = 53+9*20;           // select pitch label
		j = 254+(phi10>0 ? 0:10); // select pitch label sign
		double vtxu[nvtx] = {463.5/hudtexw, 463.5/hudtexw, 511.5/hudtexw, 511.5/hudtexw,                      // cross
			j/hudtexw,j/hudtexw,(j+10)/hudtexw,(j+10)/hudtexw,i/hudtexw,i/hudtexw,(i+20)/hudtexw,(i+20)/hudtexw,  // left label
			j/hudtexw,j/hudtexw,(j+10)/hudtexw,(j+10)/hudtexw,i/hudtexw,i/hudtexw,(i+20)/hudtexw,(i+20)/hudtexw   // right label
		};
		double vtxv[nvtx] = {147.5/hudtexh, 195.5/hudtexh, 147.5/hudtexh, 195.5/hudtexh,                      // cross
			1/hudtexh,17/hudtexh,1/hudtexh,17/hudtexh,1/hudtexh,17/hudtexh,1/hudtexh,17/hudtexh,                  // left label
			1/hudtexh,17/hudtexh,1/hudtexh,17/hudtexh,1/hudtexh,17/hudtexh,1/hudtexh,17/hudtexh                   // right label
		};
		gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);
		for (j = 0; j < 5; j++)
			for (i = 0; i < 6; i++)
				gs->Idx[iidx++] = ivtx + j*4 + rectidx[i];
		for (i = 0; i < nvtx; i++, ivtx++) {
			gs->Vtx[ivtx].x = (float)(spec.CX + vtxx[i]*cosa - (vtxy[i]+d)*sina);
			gs->Vtx[ivtx].y = (float)(spec.CY + vtxx[i]*sina + (vtxy[i]+d)*cosa);
			gs->Vtx[ivtx].tu = (float)vtxu[i];
			gs->Vtx[ivtx].tv = (float)vtxv[i];
		}
	}
}

void HUD::AddMesh_HeadingTape (int &ivtx, int &iidx, double hdg, bool marker, double markerhdg)
{
	int i;
	const int nvtx = 8 + 6*4;
	const int nidx = 12 + 6*6;
	const double range05 = 30; // half-range in deg
	const WORD idx[nidx] = {
		0,2,1, 2,3,1, 4,6,5, 6,7,5, // tape
		8,10,9, 10,11,9, 12,14,13, 14,15,13, 16,18,17, 18,19,17, 20,22,21, 22,23,21, 24,26,25, 26,27,25, 28,30,29, 30,31,29 // labels
	};
	double scal = renderprm.scal; //HRES05*0.0012;

	double hdg0 = hdg-range05;
	double hdg0_10 = hdg0*0.1;
	double hdg10 = floor(hdg0_10)*10.0;
	double ofs = hdg0-hdg10;
	// tape
	double vtxx[nvtx] = {
		HRES05-scal*250, HRES05-scal*250, HRES05+scal*250, HRES05+scal*250,  // tape
		HRES05-scal*12, HRES05-scal*12, HRES05+scal*12, HRES05+scal*12       // indicator needle
	};
	double vtxy[nvtx] = {
		scal*40,scal*60,scal*40,scal*60,                                     // tape
		scal*32,scal*62,scal*32,scal*62                                        // indicator needle
	};
	double vtxu[nvtx] = {
		(70+5*ofs)/hudtexw, (70+5*ofs)/hudtexw, (70+range05*10+5*ofs)/hudtexw, (70+range05*10+5*ofs)/hudtexw,  // tape
		276/hudtexw, 276/hudtexw, 290/hudtexw, 290/hudtexw                   // indicator needle
	};
	double vtxv[nvtx] = {
		17.5/hudtexh, 29/hudtexh, 17.5/hudtexh, 29/hudtexh,                      // tape
		0,17/hudtexh,0,17/hudtexh                                            // indicator needle
	};
	// labels
	int lbl, lbl0 = (int)ceil(hdg0_10);
	const double labelw = 23.333;
	const double labelsize = scal*16;
	double dx = scal*250.0/(range05*0.1);
	ofs = (lbl0-hdg0_10)*dx-labelsize+HRES05-scal*250;
	if (lbl0 < 0) lbl0 += 36;
	for (i = 0; i < 6; i++) {
		lbl = (i+lbl0)%36;
		vtxx[(i+2)*4+2] = vtxx[(i+2)*4+3] = (vtxx[(i+2)*4+0] = vtxx[(i+2)*4+1] = ofs + i*dx) + 2*labelsize;
		vtxy[(i+2)*4+1] = vtxy[(i+2)*4+3] = (vtxy[(i+2)*4+0] = vtxy[(i+2)*4+2] = scal*62) + labelsize;
		vtxu[(i+2)*4+2] = vtxu[(i+2)*4+3] = (vtxu[(i+2)*4+0] = vtxu[(i+2)*4+1] = (0.8+(lbl%18)*labelw)/hudtexw) + labelw/hudtexw;
		vtxv[(i+2)*4+1] = vtxv[(i+2)*4+3] = (vtxv[(i+2)*4+0] = vtxv[(i+2)*4+2] = (38+(lbl/18)*12)/hudtexh) + 12/hudtexh;
	}
	GroupSpec *gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);
	for (i = 0; i < nidx; i++) {
		gs->Idx[iidx++] = idx[i]+ivtx;
	}
	for (i = 0; i < nvtx; i++, ivtx++) {
		gs->Vtx[ivtx].x = (float)vtxx[i];
		gs->Vtx[ivtx].y = (float)vtxy[i];
		gs->Vtx[ivtx].tu = (float)vtxu[i];
		gs->Vtx[ivtx].tv = (float)vtxv[i];
	}

	if (marker) {
		const int nvtx = 4;
		const int nidx = 6;
		static const WORD idx[nidx] = {0,2,1, 2,3,1};
		gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);
		double ddir = markerhdg-hdg;
		while (fabs(ddir) > 180.0) {
			if (ddir > 0.0) ddir -= 360.0;
			else            ddir += 360.0;
		}
		double *vtxx, *vtxy, *vtxu, *vtxv;
		if (ddir > range05) {
			double vtxx1[nvtx] = {HRES05+scal*260,HRES05+scal*260,HRES05+scal*272,HRES05+scal*272};
			double vtxy1[nvtx] = {scal*40,scal*62,scal*40,scal*62};
			double vtxu1[nvtx] = {300/hudtexw,300/hudtexw,305/hudtexw,305/hudtexw};
			double vtxv1[nvtx] = {0,9/hudtexh,0,9/hudtexh};
			vtxx = vtxx1, vtxy = vtxy1, vtxu = vtxu1, vtxv = vtxv1;
		} else if (ddir < -range05) {
			double vtxx2[nvtx] = {HRES05-scal*272,HRES05-scal*272,HRES05-scal*260,HRES05-scal*260};
			double vtxy2[nvtx] = {scal*40,scal*62,scal*40,scal*62};
			double vtxu2[nvtx] = {293/hudtexw,293/hudtexw,298/hudtexw,298/hudtexw};
			double vtxv2[nvtx] = {0,9/hudtexh,0,9/hudtexh};
			vtxx = vtxx2, vtxy = vtxy2, vtxu = vtxu2, vtxv = vtxv2;
		} else {
			double ofs = HRES05+ddir*dx*0.1;
			double vtxx3[nvtx] = {ofs-scal*15,ofs-scal*15,ofs+scal*15,ofs+scal*15};
			double vtxy3[nvtx] = {scal*49,scal*62,scal*49,scal*62};
			double vtxu3[nvtx] = {292/hudtexw,292/hudtexw,306/hudtexw,306/hudtexw};
			double vtxv3[nvtx] = {10/hudtexh,16/hudtexh,10/hudtexh,16/hudtexh};
			vtxx = vtxx3, vtxy = vtxy3, vtxu = vtxu3, vtxv = vtxv3;
		}
		for (i = 0; i < nidx; i++) {
			gs->Idx[iidx++] = idx[i]+ivtx;
		}
		for (i = 0; i < nvtx; i++, ivtx++) {
			gs->Vtx[ivtx].x = (float)vtxx[i];
			gs->Vtx[ivtx].y = (float)vtxy[i];
			gs->Vtx[ivtx].tu = (float)vtxu[i];
			gs->Vtx[ivtx].tv = (float)vtxv[i];
		}
	}
}

void HUD::AddMesh_AzimuthTape (int &ivtx, int &iidx, double phi, double alpha)
{
	static double step = tan (Rad(2.0));          // 2deg tick separation
	double cosa = cos(alpha), sina = sin(alpha);
	double d = g_camera->Scale() * step;          // tick separation (pixel)
	if (d < ladder_range*0.01)
		return;            // don't draw if ticks are too close

	double scal = renderprm.scal; //HRES05*0.0012;
	const double labelw = 23.333;
	const double labelsize = scal*16;
	int iphin;
	double x0, y0, x1, y1, x2, y2;
	double s = scal*30.0;    // tick length
	double h = scal*4;       // tick width
	double dsx = s*sina;
	double dsy = s*cosa;
	double dhx = h*cosa;
	double dhy = h*sina;
	double dlx = labelsize*cosa;
	double dly = labelsize*sina;
	double d1;
	double phi0 = floor (phi*DEG*0.5);
	double d0   = (phi0 - phi*DEG*0.5) * d;
	int iphi0, iphi, lbl;
	iphi0 = (int)phi0;

	const int nvtx = 4*(int)(ladder_range*(1.4*1.2)/d)+8;
	const int nidx = (nvtx*6)/4;

	GroupSpec *gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);
	int vtxofs = ivtx;
	double tuofs;

	for (d1 = d0, iphi = iphi0; d1 > -ladder_range*0.7; d1 -= d, iphi--) {
		iphin = iphi*2;
		if (iphin >= 360)   iphin -= 360;
		else if (iphin < 0) iphin += 360;
		x0 = spec.CX + d1*cosa;
		y0 = spec.CY + d1*sina;
		x1 = x0+dsx, y1 = y0-dsy;
		x2 = x0-dsx, y2 = y0+dsy;
		if (iphin%10) tuofs = 311; // minor tick
		else {
			lbl = iphin/10;
			tuofs = 307; // major tick
			gs->Vtx[ivtx].x = (float)(x1-dlx+dly);
			gs->Vtx[ivtx].y = (float)(y1-dly-dlx);
			gs->Vtx[ivtx].tu = (float)((0.5+(lbl%18)*labelw)/hudtexw);
			gs->Vtx[ivtx].tv = (float)((38+(lbl/18)*12)/hudtexh);
			ivtx++;
			gs->Vtx[ivtx].x = (float)(x1-dlx);
			gs->Vtx[ivtx].y = (float)(y1-dly);
			gs->Vtx[ivtx].tu = (float)((0.5+(lbl%18)*labelw)/hudtexw);
			gs->Vtx[ivtx].tv = (float)((38+(lbl/18)*12)/hudtexh + 12/hudtexh);
			ivtx++;
			gs->Vtx[ivtx].x = (float)(x1+dlx+dly);
			gs->Vtx[ivtx].y = (float)(y1+dly-dlx);
			gs->Vtx[ivtx].tu = (float)((0.5+(lbl%18)*labelw)/hudtexw + labelw/hudtexw);
			gs->Vtx[ivtx].tv = (float)((38+(lbl/18)*12)/hudtexh);
			ivtx++;
			gs->Vtx[ivtx].x = (float)(x1+dlx);
			gs->Vtx[ivtx].y = (float)(y1+dly);
			gs->Vtx[ivtx].tu = (float)((0.5+(lbl%18)*labelw)/hudtexw + labelw/hudtexw);
			gs->Vtx[ivtx].tv = (float)((38+(lbl/18)*12)/hudtexh + 12/hudtexh);
			ivtx++;
		}
		gs->Vtx[ivtx].x = (float)(x1-dhx);
		gs->Vtx[ivtx].y = (float)(y1-dhy);
		gs->Vtx[ivtx].tu = (float)(tuofs/hudtexw);
		gs->Vtx[ivtx].tv = 0.0f;
		ivtx++;
		gs->Vtx[ivtx].x = (float)(x2-dhx);
		gs->Vtx[ivtx].y = (float)(y2-dhy);
		gs->Vtx[ivtx].tu = (float)(tuofs/hudtexw);
		gs->Vtx[ivtx].tv = (float)(17.0/hudtexh);
		ivtx++;
		gs->Vtx[ivtx].x = (float)(x1+dhx);
		gs->Vtx[ivtx].y = (float)(y1+dhy);
		gs->Vtx[ivtx].tu = (float)((tuofs+4.0)/hudtexw);
		gs->Vtx[ivtx].tv = 0.0f;
		ivtx++;
		gs->Vtx[ivtx].x = (float)(x2+dhx);
		gs->Vtx[ivtx].y = (float)(y2+dhy);
		gs->Vtx[ivtx].tu = (float)((tuofs+4.0)/hudtexw);
		gs->Vtx[ivtx].tv = (float)(17.0/hudtexh);
		ivtx++;
	}
	for (d1 = d0+d, iphi = iphi0+1; d1 < ladder_range*0.7; d1 += d, iphi++) {
		iphin = iphi*2;
		if (iphin >= 360)   iphin -= 360;
		else if (iphin < 0) iphin += 360;
		x0 = spec.CX + d1*cosa;
		y0 = spec.CY + d1*sina;
		x1 = x0+dsx, y1 = y0-dsy;
		x2 = x0-dsx, y2 = y0+dsy;
		if (iphin%10) tuofs = 311; // minor tick
		else {
			lbl = iphin/10;
			tuofs = 307; // major tick
			gs->Vtx[ivtx].x = (float)(x1-dlx+dly);
			gs->Vtx[ivtx].y = (float)(y1-dly-dlx);
			gs->Vtx[ivtx].tu = (float)((0.5+(lbl%18)*labelw)/hudtexw);
			gs->Vtx[ivtx].tv = (float)((38+(lbl/18)*12)/hudtexh);
			ivtx++;
			gs->Vtx[ivtx].x = (float)(x1-dlx);
			gs->Vtx[ivtx].y = (float)(y1-dly);
			gs->Vtx[ivtx].tu = (float)((0.5+(lbl%18)*labelw)/hudtexw);
			gs->Vtx[ivtx].tv = (float)((38+(lbl/18)*12)/hudtexh + 12/hudtexh);
			ivtx++;
			gs->Vtx[ivtx].x = (float)(x1+dlx+dly);
			gs->Vtx[ivtx].y = (float)(y1+dly-dlx);
			gs->Vtx[ivtx].tu = (float)((0.5+(lbl%18)*labelw)/hudtexw + labelw/hudtexw);
			gs->Vtx[ivtx].tv = (float)((38+(lbl/18)*12)/hudtexh);
			ivtx++;
			gs->Vtx[ivtx].x = (float)(x1+dlx);
			gs->Vtx[ivtx].y = (float)(y1+dly);
			gs->Vtx[ivtx].tu = (float)((0.5+(lbl%18)*labelw)/hudtexw + labelw/hudtexw);
			gs->Vtx[ivtx].tv = (float)((38+(lbl/18)*12)/hudtexh + 12/hudtexh);
			ivtx++;
		}
		gs->Vtx[ivtx].x = (float)(x1-dhx);
		gs->Vtx[ivtx].y = (float)(y1-dhy);
		gs->Vtx[ivtx].tu = (float)(tuofs/hudtexw);
		gs->Vtx[ivtx].tv = 0.0f;
		ivtx++;
		gs->Vtx[ivtx].x = (float)(x2-dhx);
		gs->Vtx[ivtx].y = (float)(y2-dhy);
		gs->Vtx[ivtx].tu = (float)(tuofs/hudtexw);
		gs->Vtx[ivtx].tv = (float)(17.0/hudtexh);
		ivtx++;
		gs->Vtx[ivtx].x = (float)(x1+dhx);
		gs->Vtx[ivtx].y = (float)(y1+dhy);
		gs->Vtx[ivtx].tu = (float)((tuofs+4.0)/hudtexw);
		gs->Vtx[ivtx].tv = 0.0f;
		ivtx++;
		gs->Vtx[ivtx].x = (float)(x2+dhx);
		gs->Vtx[ivtx].y = (float)(y2+dhy);
		gs->Vtx[ivtx].tu = (float)((tuofs+4.0)/hudtexw);
		gs->Vtx[ivtx].tv = (float)(17.0/hudtexh);
		ivtx++;
	}

	int i, nb = (ivtx-vtxofs)/4;
	for (i = 0; i < nb; i++) {
		gs->Idx[iidx++] = vtxofs;
		gs->Idx[iidx++] = vtxofs+2;
		gs->Idx[iidx++] = vtxofs+1;
		gs->Idx[iidx++] = vtxofs+2;
		gs->Idx[iidx++] = vtxofs+3;
		gs->Idx[iidx++] = vtxofs+1;
		vtxofs += 4;
	}
}

void HUD::AddMesh_DockApproachGates (int &ivtx, int &iidx, const Body *tgt, const PortSpec *ps)
{
	int i, j;
	double x[16], y[16], dx1, dy1, dx2, dy2, rx, ry;
	double linew = renderprm.scal*3;
	Vector dpos (mul (tgt->GRot(), ps->ref) + tgt->GPos() - g_camera->GPos());
	Vector ddir (mul (tgt->GRot(), -ps->dir));
	Vector ydir (mul (tgt->GRot(), ps->rot));
	Vector p[4];
	double dist = 50;
	float dx = -(float)(ddir.x*dist);
	float dy = -(float)(ddir.y*dist);
	float dz = -(float)(ddir.z*dist);
	float ddx = -(float)ydir.x;
	float ddy = -(float)ydir.y;
	float ddz = -(float)ydir.z;
	BoxCoord (dpos, ddir, ydir, dist, p);

	const int ngate = 10;
	const int nvtx = ngate*16;
	const int nidx = ngate*30;
	GroupSpec *gs = EnsureMeshBuffer (0, ivtx+nvtx, iidx+nidx);
	const float tu = (float)(2.0/hudtexw);
	const float tv = (float)(109.0/hudtexh);
	const WORD boxidx1[30] = {0,1,4, 1,5,4, 6,7,3, 7,2,3, 4,8,6, 8,11,6, 9,5,10, 5,7,10, 12,13,15, 13,14,15};
	const WORD boxidx2[30] = {0,4,1, 1,4,5, 6,3,7, 7,3,2, 4,6,8, 8,6,11, 9,10,5, 5,10,7, 12,15,13, 13,15,14};
	NTVERTEX *vtx = gs->Vtx+ivtx;
	bool need_setup = true;
	for (i = 0; i < 10; i++) {
		for (j = 0; j < 4; j++) {
			if (!GlobalToHUD (p[j], x[j], y[j])) break;
		}
		if (j == 4) {
			if (need_setup) {
				rx = x[3]-x[0];
				ry = y[3]-y[0];
				double d  = std::hypot(rx,ry);
				dx1 = rx*linew/d, dy1 = ry*linew/d;
				rx = x[1]-x[0];
				ry = y[1]-y[0];
				d = std::hypot(rx,ry);
				dx2 = rx*linew/d, dy2 = ry*linew/d;
				need_setup = false;
			}
			x[4] = x[0]+dx1; x[5] = x[1]+dx1;
			y[4] = y[0]+dy1; y[5] = y[1]+dy1;
			x[6] = x[3]-dx1; x[7] = x[2]-dx1;
			y[6] = y[3]-dy1; y[7] = y[2]-dy1;
			x[8] = x[4]+dx2; x[9] = x[5]-dx2;
			y[8] = y[4]+dy2; y[9] = y[5]-dy2;
			x[11]= x[6]+dx2; x[10]= x[7]-dx2;
			y[11]= y[6]+dy2; y[10]= y[7]-dy2;
			rx = 0.2*(x[3]-x[0])-0.5*dx1; ry = 0.2*(y[3]-y[0])-0.5*dy1;
			x[12]= x[0]+rx; x[13]= x[1]+rx;
			y[12]= y[0]+ry; y[13]= y[1]+ry;
			x[14]= x[13]+dx1;x[15]= x[12]+dx1;
			y[14]= y[13]+dy1;y[15]= y[12]+dy1;
			if (dotp(dpos.unit(), ddir) >= 0.0) {
				for (j = 0; j < 30; j++)
					gs->Idx[iidx++] = ivtx + boxidx1[j];
			} else {
				for (j = 0; j < 30; j++)
					gs->Idx[iidx++] = ivtx + boxidx2[j];
			}
			for (j = 0; j < 16; j++) {
				vtx[j].x = (float)x[j];
				vtx[j].y = (float)y[j];
				vtx[j].tu = tu;
				vtx[j].tv = tv;
			}
			ivtx += 16;
			vtx += 16;
		}
		for (j = 0; j < 4; j++)
			p[j].x += dx, p[j].y += dy, p[j].z += dz;
		dpos -= ddir*dist;
	}
}

bool HUD::DrawCenterMarker (oapi::Sketchpad *skp) const
{
	// orientation marker
	int d = spec.W/32;
	skp->Line (spec.CX-2*d, spec.CY, spec.CX-d, spec.CY);
	skp->Line (spec.CX+2*d, spec.CY, spec.CX+d, spec.CY);
	skp->MoveTo (spec.CX-d, spec.CY+d); skp->LineTo (spec.CX, spec.CY); skp->LineTo (spec.CX+d+1, spec.CY+d+1);
	return true;
}

void HUD::DrawLadderBar (oapi::Sketchpad *skp, double lwcosa, double lwsina,
	double dcosa, double dsina, int phi10, bool mark_subzero)
{
	int x1, y1, x2, y2, dx, dy, dx1, dy1, dx2=0, dy2=0;
	char cbuf[5];

	bool is_subzero = (phi10 < 0);
	bool revert = false;
	if (phi10 > 9)       phi10 =  18-phi10, revert = true;
	else if (phi10 < -9) phi10 = -18-phi10, revert = true;
	int lab = abs (phi10);

	if (!lab) lwcosa *= 1.4, lwsina *= 1.4;
	x1 = spec.CX + (int)( lwcosa - dsina + 0.5);
	x2 = spec.CX + (int)(-lwcosa - dsina + 0.5);
	y1 = spec.CY + (int)( lwsina + dcosa + 0.5);
	y2 = spec.CY + (int)(-lwsina + dcosa + 0.5);
	dx1 = ((dx = (x2-x1))*3)/8, dy1 = ((dy = (y2-y1))*3)/8;
	if (lab != 9) {
		if (lab) {
			if (is_subzero) {
				dx2 = -dy / 20, dy2 = dx / 20;
				if (mark_subzero) {
					skp->Line(x1 + dx1, y1 + dy1, x1 + dx / 4, y1 + dy / 4);
					skp->Line(x2 - dx1, y2 - dy1, x2 - dx / 4, y2 - dy / 4);
					dx1 = dx / 8;
					dy1 = dy / 8;
				}
			}
			else {
				dx2 = dy / 20, dy2 = -dx / 20;
			}
		}
		if (revert) {
			dx2 = -dx2, dy2 = -dy2;
		}
		skp->Line (x1+dx1, y1+dy1, x1, y1);
		if (lab) skp->LineTo (x1+dx2, y1+dy2);
		skp->Line (x2-dx1, y2-dy1, x2, y2);
		if (lab) skp->LineTo (x2+dx2, y2+dy2);
	}
	else {
		skp->Line(x1 + dx1, y1 + dy1, x1 + dx / 4, y1 + dy / 4);
		skp->Line(x2 - dx1, y2 - dy1, x2 - dx / 4, y2 - dy / 4);
		skp->Line(x1 + dx / 2 + dy / 8, y1 + dy / 2 - dx / 8, x1 + dx / 2 + dy / 4, y1 + dy / 2 - dx / 4);
		skp->Line(x1 + dx / 2 - dy / 8, y1 + dy / 2 + dx / 8, x1 + dx / 2 - dy / 4, y1 + dy / 2 + dx / 4);
	}
	if (mark_subzero) {
		sprintf (cbuf, "%+d0", phi10);
		skp->Text (x1+dx/2-(3*fW)/2, y1+dy/2-fH/2, cbuf, 3);
	} else {
		sprintf (cbuf, "%d0", lab);
		skp->Text (x1+dx/2-fW, y1+dy/2-fH/2, cbuf, 2);
	}
}

void HUD::DrawTopRibbon(oapi::Sketchpad *skp, double value)
{
	const int x0 = HRES05;
	const int xmin = x0 - spec.W / 5;
	const int xmax = x0 + spec.W / 5;
	const int ticksep2 = 16;
	int x, d, d2;
	char cbuf[10];
	skp->MoveTo(x0 - 5, 5); skp->LineTo(x0, 10); skp->LineTo(x0 + 6, 4);
	double val2 = floor(value *= 0.5); // work in units of 2deg
	int dx0 = (int)((value - val2) * ticksep2 + 0.5);
	d2 = (int)(val2 + 0.5);
	skp->SetFont(font2);
	skp->SetTextAlign(oapi::Sketchpad::CENTER);
	for (x = x0 - dx0, d = d2; x >= xmin; x -= ticksep2, d--) {
		skp->MoveTo(x, 10);
		if (d % 5) skp->LineTo(x, 15);
		else {
			skp->LineTo(x, 20);
			if (d < 0) d += 180;
			sprintf(cbuf, "%03d", d * 2);  skp->Text(x, 20, cbuf, 3);
		}
	}
	for (x = x0 - dx0 + ticksep2, d = d2 + 1; x <= xmax; x += ticksep2, d++) {
		skp->MoveTo(x, 10);
		if (d % 5) skp->LineTo(x, 15);
		else {
			skp->LineTo(x, 20);
			if (d >= 180) d -= 180;
			sprintf(cbuf, "%03d", d * 2); skp->Text(x, 20, cbuf, 3);
		}
	}
	skp->SetFont(font);
	skp->SetTextAlign(oapi::Sketchpad::LEFT);
}

void HUD::DrawTiltedRibbon (oapi::Sketchpad *skp, double phi, double alpha)
{
	static double step = tan (Rad(2.0));   // 2deg tick separation
	double cosa = cos(alpha), sina = sin(alpha);
	double d   = (bVC ? HUDofs.z : g_camera->Scale()) * step;      // tick separation (pixel)
	if (d < ladder_range*0.01) return;
	double s   = ladder_width * 0.094; // major tick size                    
	int    dsx = (int)(s*sina);
	int    dsy = (int)(s*cosa);
	int    dtx = (int)(0.5*s*sina);
	int    dty = (int)(0.5*s*cosa);
	int x0, y0, iphin;
	char cbuf[16];
	
	skp->SetTextAlign (oapi::Sketchpad::CENTER);
	double d1;
	double phi0 = floor (phi*DEG*0.5);
	double d0   = (phi0 - phi*DEG*0.5) * d;

	int iphi0, iphi;
	iphi0 = (int)phi0;
	for (d1 = d0, iphi = iphi0; d1 > -ladder_range*0.7; d1 -= d, iphi--) {
		iphin = iphi*2;
		if (iphin >= 360)   iphin -= 360;
		else if (iphin < 0) iphin += 360;
		x0 = spec.CX + (int)(d1*cosa);
		y0 = spec.CY + (int)(d1*sina);
		if (iphin % 10) {  // minor tick
			skp->Line (x0-dtx, y0+dty, x0+dtx, y0-dty);
		} else {
			skp->Line (x0-dsx, y0+dsy, x0+dsx, y0-dsy);
			sprintf(cbuf, "%d", iphin);
			skp->Text (x0+dsx*2, y0-dsy*2-fH/2, cbuf, strlen(cbuf));
		}
	}
	for (d1 = d0+d, iphi = iphi0+1; d1 < ladder_range*0.7; d1 += d, iphi++) {
		iphin = iphi*2;
		if (iphin >= 360)   iphin -= 360;
		else if (iphin < 0) iphin += 360;
		x0 = spec.CX + (int)(d1*cosa);
		y0 = spec.CY + (int)(d1*sina);
		if (iphin % 10) {  // minor tick
			skp->Line (x0-dtx, y0+dty, x0+dtx, y0-dty);
		} else {
			skp->Line (x0-dsx, y0+dsy, x0+dsx, y0-dsy);
			sprintf(cbuf, "%d", iphin);
			skp->Text (x0+dsx*2, y0-dsy*2-fH/2, cbuf, strlen(cbuf));
		}
	}
	skp->SetTextAlign (oapi::Sketchpad::LEFT);
}

bool HUD::GlobalToHUD (const Vector &dir, int &x, int &y) const
{
	bool vis;

	if (bVC) {
		Vector ldir = tmul (g_focusobj->GRot(), dir);
		if (ldir.z <= 0.0) vis = false;
		else {
			double fac = HUDofs.z/ldir.z;
			x = spec.CX + (int)(fac * ldir.x);
			y = spec.CY - (int)(fac * ldir.y);
			vis = (x >= 0 && x < spec.W && y >= 0 && y < spec.H);
		}
	} else {
		vis = pane->GlobalToScreen (dir, x, y);
	}
	return vis;
}

bool HUD::GlobalToHUD (const Vector &dir, double &x, double &y) const
{
	bool vis;

	if (bVC) {
		Vector ldir = tmul (g_focusobj->GRot(), dir);
		if (ldir.z <= 0.0) vis = false;
		else {
			double fac = HUDofs.z/ldir.z;
			x = spec.CX + fac * ldir.x;
			y = spec.CY - fac * ldir.y;
			vis = (x >= 0.0 && x < 256.0 && y >= 0.0 && y < 256.0);
		}
	} else {
		vis = pane->GlobalToScreen (dir, x, y);
	}
	return vis;
}

bool HUD::GlobalDrawMarker (oapi::Sketchpad *skp, const Vector &dir, int style) const
{
	int x, y;
	if (GlobalToHUD (dir, x, y)) {
		if (style & 1) {
			skp->Rectangle (x-spec.Markersize, y-spec.Markersize, x+spec.Markersize+1, y+spec.Markersize+1);
		}
		if (style & 2) {
			skp->Ellipse (x-spec.Markersize, y-spec.Markersize, x+spec.Markersize+1, y+spec.Markersize+1);
		}
		if (style & 4) {
			skp->Line (x-spec.Markersize, y, x+spec.Markersize+1, y);
			skp->Line (x, y-spec.Markersize, x, y+spec.Markersize+1);
		}
		return true;
	}
	return false;
}

oapi::IVECTOR2 *HUD::OffscreenDirMarker (const Vector &dir) const
{
	static oapi::IVECTOR2 pt[4];
	Vector d;
	if (bVC) {
		d.Set (tmul (g_focusobj->GRot(), dir));
	} else {
		d.Set (tmul (g_camera->GRot(), dir));
	}
	double len = std::hypot (d.y, d.x);
	double scale = spec.Markersize * 0.6;
	double dx = d.x/len*scale;
	double dy = d.y/len*scale;
	pt[0].x = spec.CX + (int)(10.0*dx);
	pt[0].y = spec.CY - (int)(10.0*dy);
	pt[1].x = spec.CX + (int)(7.0*dx - dy);
	pt[1].y = spec.CY - (int)(7.0*dy + dx);
	pt[2].x = spec.CX + (int)(7.0*dx + dy);
	pt[2].y = spec.CY - (int)(7.0*dy - dx);
	pt[3].x = spec.CX + (int)(8.5*dx);  // centre
	pt[3].y = spec.CY - (int)(8.5*dy);
	return pt;
}

HUD *HUD::Create (ifstream &ifs, const Pane *_pane, oapi::GraphicsClient *gc)
{
	HUD *hud = 0;
	char cbuf[256], *pc;
	if (!FindLine (ifs, "BEGIN_HUD")) return 0;
	for (;hud == 0;) {
		if (!ifs.getline (cbuf, 256)) return 0;
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_HUD", 7)) return 0;
		if (!_strnicmp (pc, "TYPE", 4)) {
			pc = trim_string (pc+4);
			if (!_strnicmp (pc, "Orbit", 5)) { hud = new HUD_Orbit (_pane); TRACENEW }
			else if (!_strnicmp (pc, "Surface", 7)) { hud = new HUD_Surface (_pane); TRACENEW }
			else if (!_strnicmp (pc, "Docking", 7)) { hud = new HUD_Docking (_pane); TRACENEW }
		}
	}
	if (hud) hud->ReadParams (ifs);
	return hud;
}

void HUD::Write (ostream &ofs) const
{
	ofs << "BEGIN_HUD" << endl;
	WriteParams (ofs);
	ofs << "END_HUD" << endl;
}

// =======================================================================
// class HUD_Orbit

HUD_Orbit::SavePrm HUD_Orbit::saveprm = {0,0};

HUD_Orbit::HUD_Orbit (const Pane *_pane)
: HUD (_pane)
{
	// restore status
	if (saveprm.vessel == g_focusobj) {
		ref = saveprm.ref;
	} else {
		ref = 0;
	}
	refname[0] = '\0';
	refwidth = 0;
}

HUD_Orbit::~HUD_Orbit ()
{
	// save status
	saveprm.vessel = g_focusobj;
	saveprm.ref = ref;
}

void HUD_Orbit::SwitchColour (int idx)
{
	HUD::SwitchColour (idx);
	refname[0] = '\0';
	refwidth = 0;
}

void HUD_Orbit::Display (oapi::Sketchpad *skp)
{
	int px, py, iphi, iphi0;
	int xbl, xbr, y, dx, len;
	double fac, a, b, c, phi, phi0, alpha, sina, cosa, d, d0, d1, v;
	double lwcosa, lwsina;
	char cbuf[64];

	const Vessel *self = g_focusobj;
	const Body *cntobj = (ref ? ref : self->ElRef());
	if (!cntobj) return; // not in orbit - nothing to draw

	sprintf (cbuf, "[%s]", cntobj->Name());
	skp->Text (10+6*fW, 0, cbuf, strlen (cbuf));

	Vector V = self->GVel() - cntobj->GVel();
	Vector Vunit = V.unit();
	Vector Vrel = tmul (self->GRot(), V).unit();
	if (!GlobalDrawMarker (skp, Vunit, 6) &&
		!GlobalDrawMarker (skp, -Vunit, 4)) {
		oapi::IVECTOR2 *pt = OffscreenDirMarker (Vunit);
		skp->Polygon (pt, 3);
		skp->Text (pt[3].x-fW, pt[3].y-spec.Markersize-fH-2, "PG", 2);
	}

	Vector P = self->GPos() - cntobj->GPos();

	// radius indicator
	dx = fW*7;
	xbl = boxx;
	xbr = spec.W - boxx - dx;
	y = fH*3;
	char spd_mag = 0; // speed magnitude character
	char rad_mag = 0; // radius magnitude character
	if ((v=P.length()) < MAXALT_HUD) {
		strcpy (cbuf, FloatStr (v));
		len = strlen(cbuf + 1);
		if (cbuf[len] >= 'M') {
			rad_mag = cbuf[len];
			cbuf[len] = ' ';
		}
	}
	else {
		strcpy(cbuf, " ----");
		len = 4;
	}
	skp->Text (xbr+((7-len)*fW)/2, y, cbuf+1, len);
	skp->Rectangle (xbr, y, xbr+dx, y+2+fH);

	// orbital velocity indicator
	if ((v = V.length()) >= 0.0) {
		if (v < MAXALT_HUD) {
			strcpy (cbuf, FloatStr (v));
			len = strlen(cbuf + 1);
			if (cbuf[len] >= 'M') {
				spd_mag = cbuf[len];
				cbuf[len] = ' ';
			}
		}
		else {
			strcpy(cbuf, " ----");
			len = 4;
		}
		skp->Text (xbl + ((7-len)*fW)/2, y, cbuf+1, len);
		skp->Rectangle (xbl, y, xbl+dx, y+2+fH);
	}

	// output stuff in small font
	skp->SetFont(font2);
	skp->Text(xbr + dx + 2, y + fH / 5, "RAD", 3);
	if (rad_mag)
		skp->Text(xbr + (11 * fW) / 2, y + fH / 5, &rad_mag, 1);
	if (spd_mag)
		skp->Text(xbl + (11 * fW) / 2, y + fH / 5, &spd_mag, 1);
	skp->SetTextAlign(oapi::Sketchpad::RIGHT);
	skp->Text(xbl - 1, y + fH / 5, "OS", 2);
	skp->SetTextAlign(oapi::Sketchpad::LEFT);
	skp->SetFont(font);

	Vector Prel = tmul (self->GRot(), P).unit();
	//if (Prel.z < 0.0) Prel = -Prel;
	fac = VRES05 / (Prel.z * g_camera->TanAperture());
	px = HRES05 + (int)(Prel.x * fac);
	py = VRES05 - (int)(Prel.y * fac);
	// equation of orbital plane in ship's relative coords: ax + by + cz = 0
	a = Prel.z*Vrel.y - Prel.y*Vrel.z;
	b = Prel.x*Vrel.z - Prel.z*Vrel.x;
	c = Prel.y*Vrel.x - Prel.x*Vrel.y;
	// angle between ship's longitudinal axis and orbital plane
	double h = 1.0 / sqrt (a*a + b*b + c*c);
	phi = asin (c * h);
	// ship's banking angle w.r.t. orbital plane
	alpha = atan2 (a, b);
	sina = sin (alpha), cosa = cos (alpha);
	lwcosa = ladder_width*cosa, lwsina = ladder_width*sina;

	phi0 = floor (Deg(phi)*0.1);
	static double step = tan (RAD*10.0);
	d = (bVC ? HUDofs.z : g_camera->Scale()) * step;
	if (d > ladder_range*0.1) {
		d0 = (0.1*Deg(phi) - phi0) * d;

		iphi0 = iphi = (int)phi0;
		for (d1 = d0; d1 < ladder_range; d1 += d)
			DrawLadderBar (skp, lwcosa, lwsina, d1*cosa, d1*sina, iphi--, true);
		iphi = iphi0+1;
		for (d1 = d0-d; d1 > -ladder_range; d1 -= d)
			DrawLadderBar (skp, lwcosa, lwsina, d1*cosa, d1*sina, iphi++, true);
	}

	// Orbital plane azimuth angle ribbon
	Vector Z0 (-c*a, -c*b, a*a+b*b); // projection of vessel forward direction into orbital plane
	if (Z0.x || Z0.y || Z0.z) {
		Z0.unify();
		double cosa = dotp (Vrel, Z0);
		double phi = acos(cosa);
		Vector VV (Vrel.y*c - Vrel.z*b, Vrel.z*a - Vrel.x*c, Vrel.x*b - Vrel.y*a); // Vector perpendicular to V in orbital plane
		if (dotp (Z0, VV) > 0.0) phi = Pi2-phi;
		DrawTiltedRibbon (skp, phi, alpha);
	}
}

void HUD_Orbit::UpdateMesh (int &ivtx, int &iidx)
{
	if (bVC) return; // apply render mode only to glass cockpit and 2D instrument panel

	double a, b, c, h, phi, phi0, alpha, d, d0, d1, cosa, sina, v;
	int iphi, iphi0;

	const Vessel *self = g_focusobj;
	const Body *cntobj = (ref ? ref : self->ElRef());
	if (!cntobj) return; // not in orbit - nothing to draw

	// Output HUD mode and reference object
	const char *name = cntobj->Name();
	if (_strnicmp(name, refname, 63)) {
		int i;
		for (i = 0; name[i] && i<63; i++) refname[i] = toupper(name[i]);
		refname[i] = '\0';
		refwidth = TexBltString (refname, 50, 243) + 50;
	}
	AddMesh_Billboard (ivtx, iidx, spec.CX-renderprm.scal*refwidth*0.7, renderprm.scal*10, renderprm.scal*refwidth*1.4, renderprm.scal*19.6, 0, 242, refwidth, 14);

	Vector V = self->GVel() - cntobj->GVel();
	Vector Vunit = V.unit();
	if (!AddMesh_Marker (ivtx, iidx, Vunit, 1) &&
		!AddMesh_Marker (ivtx, iidx, -Vunit, 0)) {
			AddMesh_DirectionMarker (ivtx, iidx, Vunit, true);
	}

	if (g_camera->IsCockpitForward()) {
		static double step = tan (RAD*10.0);
		Vector Vrel = tmul (self->GRot(), V).unit();
		Vector P = self->GPos() - cntobj->GPos();
		Vector Prel = tmul (self->GRot(), P).unit();

		// Radius readout
		char cbuf[64];
		strcpy (cbuf, (v=P.length()) < MAXALT_HUD ? FloatStr (v) : " ----");
		AddMesh_Readout (ivtx, iidx, 1, cbuf+1, 2);

		// Orbital velocity readout
		strcpy (cbuf, (v=V.length()) < MAXALT_HUD ? FloatStr (v) : " ----");
		AddMesh_Readout (ivtx, iidx, 0, cbuf+1, 4);

		a = Prel.z*Vrel.y - Prel.y*Vrel.z;
		b = Prel.x*Vrel.z - Prel.z*Vrel.x;
		c = Prel.y*Vrel.x - Prel.x*Vrel.y;
		h = 1.0 / sqrt (a*a + b*b + c*c);
		phi = asin (c * h);
		phi0 = floor (Deg(phi)*0.1);
		alpha = atan2 (a, b);
		sina = sin (alpha), cosa = cos (alpha);
		d = g_camera->Scale() * step;
		if (d > ladder_range*0.1) {
			d0 = (0.1*Deg(phi) - phi0) * d;
			iphi0 = iphi = (int)phi0;
			for (d1 = d0; d1 < ladder_range; d1 += d)
				AddMesh_LadderBar (ivtx, iidx, sina, cosa, d1, iphi--, false);
			iphi = iphi0+1;
			for (d1 = d0-d; d1 > -ladder_range; d1 -= d)
				AddMesh_LadderBar (ivtx, iidx, sina, cosa, d1, iphi++, false);
		}

		// Orbital plane azimuth angle ribbon
		Vector Z0 (-c*a, -c*b, a*a+b*b); // projection of vessel forward direction into orbital plane
		if (Z0.x || Z0.y || Z0.z) {
			Z0.unify();
			double cosa = dotp (Vrel, Z0);
			double phi = acos(cosa);
			Vector VV (Vrel.y*c - Vrel.z*b, Vrel.z*a - Vrel.x*c, Vrel.x*b - Vrel.y*a); // Vector perpendicular to V in orbital plane
			if (dotp (Z0, VV) > 0.0) phi = Pi2-phi;
			AddMesh_AzimuthTape (ivtx, iidx, phi, alpha);
		}
	}
}

void HUD_Orbit::SetReference (const Body *bref)
{
	ref = bref;
}

void HUD_Orbit::SelectReference ()
{
	g_input->Open ("HUD reference body (a for auto):", 0, 20, HUD_Orbit::ClbkName_Ref, (void*)this);
}

bool HUD_Orbit::ClbkName_Ref (InputBox*, char *str, void *data)
{
	HUD_Orbit* hud = (HUD_Orbit*)data;
	return hud->SelectRef (str);
}

bool HUD_Orbit::SelectRef (char *str)
{
	if (toupper(str[0]) == 'A' && str[1] == '\0') {
		ref = 0;
	} else {
		Body *obj = g_psys->GetGravObj (str, true);
		if (!obj) return false;
		ref = obj;
	}
	return true;
}

void HUD_Orbit::WriteParams (ostream &ofs) const
{
	ofs << "  TYPE Orbit" << endl;
	ofs << "  REF " << (ref ? ref->Name() : "AUTO") << endl;
}

void HUD_Orbit::ReadParams (ifstream &ifs)
{
	char cbuf[256], *pc;
	for (;;) {
		if (!ifs.getline (cbuf, 256)) {
			ifs.clear(); break;
		}
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_HUD", 7)) break;
		if (!_strnicmp (pc, "REF", 3)) {
			pc = trim_string (pc+3);
			if (!_strnicmp (pc, "AUTO", 4)) ref = 0;
			else ref = g_psys->GetObj (pc, true);
		}
	}
}

// =======================================================================
// class HUD_Surface

double HUD_Surface::as_plimit = 1e-3; // pressure limit for airspeed availability [Pa]

HUD_Surface::HUD_Surface (const Pane *_pane)
: HUD (_pane)
{
	refname[0] = '\0';
	refwidth = 0;
}

void HUD_Surface::SwitchColour (int idx)
{
	HUD::SwitchColour (idx);
	refname[0] = '\0';
	refwidth = 0;
}

void HUD_Surface::Display (oapi::Sketchpad *skp)
{
	const SurfParam *sp;
	const Vessel *self = g_focusobj;
	double sina, cosa, lwsina, lwcosa, phi0, d, d0, d1;
	int iphi, iphi0, xbl, xbr, dx, y, len;
	char cbuf[40];

	if (!(sp = self->GetSurfParam())) return;

	// velocity marker
	if (sp->groundspd > 1.0)
		GlobalDrawMarker (skp, sp->groundvel_glob.unit(), 6);

	// altitude indicator
	dx = fW*7;
	xbl = boxx;
	xbr = spec.W - boxx - dx;
	y = fH*3;
	char spd_mag = 0; // speed magnitude character
	char alt_mag = 0; // altitude magnitude character
	int spd_mode = 0; // speed mode id (0 = none, 1 = TAS, 2 = GS)
	bool radaralt = false; // radar altitude
	if (sp->alt0 < MAXALT_HUD) {
		radaralt = (sp->alt < 1e4);
		strcpy (cbuf, FloatStr (radaralt ? sp->alt : sp->alt0));
		len = strlen(cbuf + 1);
		if (cbuf[len] >= 'M') {
			alt_mag = cbuf[len];
			cbuf[len] = ' ';
		}
	}
	else {
		strcpy(cbuf, " ----");
		len = 4;
	}
	skp->Text (xbr+((7-len)*fW)/2, y, cbuf+1, len);
	skp->Rectangle (xbr, y, xbr+dx, y+2+fH);

	// airspeed indicator
	spd_mode = (sp->atmp > as_plimit ? 1 : 2);
	double spd = (spd_mode == 1 ? sp->airspd : sp->groundspd);
	if (spd >= 0.0) {
		if (spd < MAXALT_HUD) {
			strcpy (cbuf, FloatStr (spd));
			len = strlen(cbuf + 1);
			if (cbuf[len] >= 'M') {
				spd_mag = cbuf[len];
				cbuf[len] = ' ';
			}
		}
		else {
			spd_mode = 0;
			strcpy(cbuf, " ----");
			len = 4;
		}
		skp->Text (xbl + ((7 - len)*fW) / 2, y, cbuf+1, len);
		skp->Rectangle (xbl, y, xbl+dx, y+2+fH);
	}

	// output stuff in small font
	skp->SetFont(font2);
	skp->Text(xbr + dx + 2, y + fH / 5, "ALT", 3);
	if (alt_mag)
		skp->Text(xbr + (11*fW)/2, y + fH / 5, &alt_mag, 1);
	if (spd_mag)
		skp->Text(xbl + (11*fW)/2, y + fH / 5, &spd_mag, 1);
	if (radaralt || spd_mode) {
		skp->SetTextAlign(oapi::Sketchpad::RIGHT);
		if (radaralt)
			skp->Text(xbr - 1, y + fH / 5, "R", 1);
		if (spd_mode)
			skp->Text(xbl - 1, y + fH / 5, sp->atmp > as_plimit ? "TAS" : " GS", 3);
		skp->SetTextAlign(oapi::Sketchpad::LEFT);
	}
	skp->SetFont(font);


	// compass ribbon
	DrawTopRibbon (skp, Deg(sp->dir));

	// draw target direction index
	const Base *base = self->LandingTarget();
	if (base) {
		static const double scalerange = Rad(spec.W/40.0);
		double blng, blat, adist, bdir, ddir;
		int x;
		base->EquPos (blng, blat);
		Orthodome (sp->lng, sp->lat, blng, blat, adist, bdir);
		ddir = bdir - sp->dir;
		if      (ddir <= -Pi) ddir += Pi2;
		else if (ddir >=  Pi) ddir -= Pi2;
		if (ddir > scalerange) {
			x = HRES05 + spec.W/5 + 5;
			skp->MoveTo (x, 10); skp->LineTo (x, 22);
			skp->LineTo (x+12, 16); skp->LineTo (x, 10);
		} else if (ddir < -scalerange) {
			x = HRES05 - spec.W/5 - 5;
			skp->MoveTo (x, 10); skp->LineTo (x, 22);
			skp->LineTo (x-12, 16); skp->LineTo (x, 10);
		} else {
			x = HRES05 + (int)(Deg(ddir)*8.0+0.5);
			skp->MoveTo (x, 10); skp->LineTo (x-6, 22);
			skp->LineTo (x+6, 22); skp->LineTo (x, 10);
		}
	}

	// horizon elevation ladder
	sina = sin (sp->bank), cosa = cos (sp->bank);
	lwcosa = ladder_width*cosa, lwsina = ladder_width*sina;

	phi0 = floor (Deg(sp->pitch)*0.1);
	static double step = tan (RAD*10.0);
	d = (bVC ? HUDofs.z : g_camera->Scale()) * step;
	if (d > ladder_range*0.1) {
		d0 = (0.1*Deg(sp->pitch) - phi0) * d;

		iphi0 = iphi = (int)phi0;
		for (d1 = d0; d1 < ladder_range; d1 += d)
			DrawLadderBar (skp, lwcosa, lwsina, d1*cosa, d1*sina, iphi--, true);
		iphi = iphi0+1;
		for (d1 = d0-d; d1 > -ladder_range; d1 -= d)
			DrawLadderBar (skp, lwcosa, lwsina, d1*cosa, d1*sina, iphi++, true);
	}
}

void HUD_Surface::UpdateMesh (int &ivtx, int &iidx)
{
	if (bVC) return; // Render mode only for glass cockpit and 2D instrument panel

	const SurfParam *sp;
	const Vessel *self = g_focusobj;
	if (!(sp = self->GetSurfParam())) return;  // no data available

	// Output HUD mode and reference object
	const char *name = sp->ref->Name();
	if (_strnicmp(name, refname, 63)) {
		int i;
		for (i = 0; name[i] && i<63; i++) refname[i] = toupper(name[i]);
		refname[i] = '\0';
		refwidth = TexBltString (refname, 310, 243) + 54;
	}
	AddMesh_Billboard (ivtx, iidx, spec.CX-renderprm.scal*refwidth*0.7, renderprm.scal*10, renderprm.scal*refwidth*1.4, renderprm.scal*19.6, 256, 242, refwidth, 14);

	// velocity marker
	if (sp->groundspd > 1.0)
		AddMesh_Marker (ivtx, iidx, sp->groundvel_glob.unit(), 1);

	if (g_camera->IsCockpitForward()) {
		// Altitude readout
		char cbuf[64];
		bool groundalt = (sp->alt < 1e4);
		strcpy (cbuf, sp->alt0 < MAXALT_HUD ? FloatStr (groundalt ? sp->alt : sp->alt0) : " ----");
		if (groundalt) strcat (cbuf, "R");
		AddMesh_Readout (ivtx, iidx, 1, cbuf+1, 1);

		// Speed readout
		double spd = (sp->atmp > as_plimit ? sp->airspd : sp->groundspd);
		if (spd >= 0.0) {
			strcpy(cbuf, spd < MAXALT_HUD ? FloatStr(spd) : " ----");
			AddMesh_Readout(ivtx, iidx, 0, cbuf + 1, sp->atmp > as_plimit ? 5 : 3);
		}

		// Heading tape
		const Base *base = self->LandingTarget();
		double bdir = 0.0;
		if (base) {
			static const double scalerange = Rad(spec.W/40.0);
			double blng, blat, adist;
			base->EquPos (blng, blat);
			Orthodome (sp->lng, sp->lat, blng, blat, adist, bdir);
		}
		AddMesh_HeadingTape (ivtx, iidx, sp->dir*DEG, base != 0, bdir*DEG);

		// Pitch ladder
		double sina, cosa, phi0, d, d0, d1;
		int iphi, iphi0;
		sina = sin (sp->bank), cosa = cos (sp->bank);
		phi0 = floor (Deg(sp->pitch)*0.1);
		static double step = tan (RAD*10.0);
		d = g_camera->Scale() * step;
		if (d > ladder_range*0.1) {
			d0 = (0.1*Deg(sp->pitch) - phi0) * d;
			iphi0 = iphi = (int)phi0;
			for (d1 = d0; d1 < ladder_range; d1 += d)
				AddMesh_LadderBar (ivtx, iidx, sina, cosa, d1, iphi--, true);
			iphi = iphi0+1;
			for (d1 = d0-d; d1 > -ladder_range; d1 -= d)
				AddMesh_LadderBar (ivtx, iidx, sina, cosa, d1, iphi++, true);
		}
	}
}

void HUD_Surface::WriteParams (ostream &ofs) const
{
	ofs << "  TYPE Surface" << endl;
}

// =======================================================================
// class HUD_Docking

struct HUD_Docking::SavePrm HUD_Docking::saveprm = {0,0,0,-1,false};

HUD_Docking::HUD_Docking (const Pane *_pane)
: HUD (_pane)
{
	// restore status
	if (g_focusobj == saveprm.vessel) {
		nv = saveprm.nv;
		Legacy_ref = saveprm.lref;
		Legacy_port = saveprm.lport;
		bUseLegacyReference = saveprm.luse;
	} else {
		nv = 0;
		Legacy_ref  =  0;
		Legacy_port = -1;
		bUseLegacyReference = false;
	}
	refname[0] = navname[0] = vstr1[0] = pstr1[0] = '\0';
	navwidth = vstr1width = pstr1width = 0;
	refwidth = 47;
}

HUD_Docking::~HUD_Docking ()
{
	// save status
	saveprm.vessel = g_focusobj;
	saveprm.nv     = nv;
	saveprm.lref   = Legacy_ref;
	saveprm.lport  = Legacy_port;
	saveprm.luse   = bUseLegacyReference;
}

void HUD_Docking::SwitchColour (int idx)
{
	HUD::SwitchColour (idx);
	refname[0] = navname[0] = vstr1[0] = pstr1[0] = '\0';
	navwidth = vstr1width = pstr1width = 0;
	refwidth = 47;
}

void HUD_Docking::Display (oapi::Sketchpad *skp)
{
	int x, y, slen;
	char cbuf[128];
	const Vessel *self = g_focusobj;
	const PortSpec *ps = 0;
	PortSpec local_ps; // only used for legacy stations
	const Body *tgt = 0;

	if (bUseLegacyReference) { // use legacy reference

		tgt = Legacy_ref;
		strcpy (cbuf, tgt->Name());
		if (Legacy_port >= 0) {
			sprintf (cbuf+strlen(cbuf), " [port %d]", Legacy_port+1);
			if (tgt->Type() == OBJTP_VESSEL) {
				const Vessel *vessel = (const Vessel*)tgt;
				if ((DWORD)Legacy_port < vessel->nDock())
					ps = vessel->GetDockParams (Legacy_port);
			}
		}

	} else { // use NAV-based reference selection

		if (nv >= self->nnav) return; // sanity check
		NavRadioSpec *nav = self->nav+nv;

		sprintf (cbuf, "NAV%d %0.2fMHz", nv+1, nav->freq);
		skp->Text (10, fH, cbuf, strlen(cbuf));
		if (!nav->sender) return;
		switch (nav->sender->Type()) {
		case TRANSMITTER_XPDR:
			tgt = ((Nav_XPDR*)nav->sender)->GetVessel();
			sprintf (cbuf, "[XPDR %s]", tgt->Name());
			break;
		case TRANSMITTER_IDS: {
			Nav_IDS *ids = (Nav_IDS*)nav->sender;
			tgt = ids->GetVessel();
			ps  = ids->GetPortSpec();
			sprintf (cbuf, "[IDS %s]", tgt->Name());
			} break;
		default:
			return;
		}
	}

	if (!tgt) return; // no docking target

	skp->Text (10+5*fW, 0, cbuf, strlen (cbuf));

	// rel velocity marker
	Vector vrel = self->GVel() - tgt->GVel();
	double len = vrel.length(); vrel /= len;
	sprintf (cbuf, "V[%s]", tgt->Name());

	if (GlobalToHUD (vrel, x, y)) {
		skp->Ellipse (x-spec.Markersize, y-spec.Markersize, x+spec.Markersize+1, y+spec.Markersize+1);
		skp->Line (x-spec.Markersize, y, x+spec.Markersize+1, y);
		skp->Line (x, y-spec.Markersize, x, y+spec.Markersize+1);
	} else if (GlobalToHUD (-vrel, x, y)) { // negative vel. marker
		skp->Line (x-spec.Markersize, y, x+spec.Markersize+1, y);
		skp->Line (x, y-spec.Markersize, x, y+spec.Markersize+1);
		sprintf (cbuf, "-V[%s]", tgt->Name());
	} else { // offscreen velocity dir marker
		oapi::IVECTOR2 *pt = OffscreenDirMarker (vrel);
		skp->Polygon (pt, 3);
		x = pt[3].x, y = pt[3].y;
	}
	slen = strlen (cbuf);
	skp->Text (x-(slen*fW)/2, y-spec.Markersize-fH-2, cbuf, slen);
	strcpy (cbuf, DistStr(len));
	slen = strlen (cbuf+1);
	skp->Text (x-(slen*fW)/2, y+spec.Markersize, cbuf+1, slen);

	// target marker
	Vector prel = tgt->GPos() - self->GPos();
	len = prel.length(); prel /= len;
	if (GlobalToHUD (prel, x, y)) {
		skp->Rectangle (x-spec.Markersize, y-spec.Markersize, x+spec.Markersize+1, y+spec.Markersize+1);
	} else {
		oapi::IVECTOR2 *pt = OffscreenDirMarker (prel);
		skp->Polygon (pt, 3);
		x = pt[3].x, y = pt[3].y;
	}
	slen = strlen (tgt->Name());
	skp->Text (x-(slen*fW)/2, y-spec.Markersize-fH-2, tgt->Name(), slen);
	strcpy (cbuf, DistStr(len));
	slen = strlen (cbuf+1);
	skp->Text (x-(slen*fW)/2, y+spec.Markersize, cbuf+1, slen);

	// approach path indicator
	if (ps) {
		int i, j, x, y;
		Vector dpos (mul (tgt->GRot(), ps->ref) + tgt->GPos() - g_camera->GPos());
		Vector ddir (mul (tgt->GRot(), -ps->dir));
		Vector ydir (mul (tgt->GRot(), ps->rot));
		Vector p[4], pp[2];
		oapi::IVECTOR2 pt[4];
		double dist = 50.0;
		float dx = -(float)(ddir.x*dist);
		float dy = -(float)(ddir.y*dist);
		float dz = -(float)(ddir.z*dist);
		float ddx = -(float)ydir.x;
		float ddy = -(float)ydir.y;
		float ddz = -(float)ydir.z;
		BoxCoord (dpos, ddir, ydir, dist, p);
		//double zlimit = g_camera->Farplane() / (g_camera->Farplane()-g_camera->Nearplane());
		for (i = 0; i < 10; i++) {
			for (j = 0; j < 4; j++) {
				if (!GlobalToHUD (p[j], x, y)) break;
				pt[j].x = x, pt[j].y = y;
			}
			if (j == 4) {
				skp->Polygon (pt, 4);
				for (j = 0; j < 2; j++) {
					pp[j].x = p[j].x + ddx, pp[j].y = p[j].y + ddy, pp[j].z = p[j].z + ddz;
					GlobalToHUD (pp[j], x, y);
					pt[j].x = x, pt[j].y = y;
				}
				skp->Line (pt[0].x, pt[0].y, pt[1].x, pt[1].y);
			}
			for (j = 0; j < 4; j++)
				p[j].x += dx, p[j].y += dy, p[j].z += dz;
		}
	}
}

void HUD_Docking::UpdateMesh (int &ivtx, int &iidx)
{
	if (bVC) return; // apply render mode only to glass cockpit and 2D instrument panel

	int i;
	char refstr[128] = "", cbuf[128];
	const Vessel *self = g_focusobj;
	const PortSpec *ps = 0;
	PortSpec local_ps; // only used for legacy stations
	const Body *tgt = 0;

	if (bUseLegacyReference) { // use legacy reference

		tgt = Legacy_ref;
		strcpy (refstr, tgt->Name());
		if (Legacy_port >= 0) {
			sprintf (refstr+strlen(refstr), " [port %d]", Legacy_port+1);
			if (tgt->Type() == OBJTP_VESSEL) {
				const Vessel *vessel = (const Vessel*)tgt;
				if ((DWORD)Legacy_port < vessel->nDock())
					ps = vessel->GetDockParams (Legacy_port);
			}
		}

	} else if (nv < self->nnav) { // use NAV-based reference selection

		NavRadioSpec *nav = self->nav+nv;

		sprintf (cbuf, "NAV%d %0.2fMHz", nv+1, nav->freq);
		if (_strnicmp(cbuf, navname, 127)) {
			for (i = 0; cbuf[i] && i<127; i++) navname[i] = cbuf[i];
			navname[i] = '\0';
			navwidth = TexBltString (navname, 256, 230);
		}
		AddMesh_Billboard (ivtx, iidx, spec.CX-renderprm.scal*navwidth*0.7, renderprm.scal*30, renderprm.scal*navwidth*1.4, renderprm.scal*19.6, 256, 229, navwidth, 13);

		if (nav->sender) {
			switch (nav->sender->Type()) {
			case TRANSMITTER_XPDR:
				nav->sender->IdString (refstr, 128);
				tgt = ((Nav_XPDR*)nav->sender)->GetVessel();
				break;
			case TRANSMITTER_IDS: {
				nav->sender->IdString (refstr, 128);
				Nav_IDS *ids = (Nav_IDS*)nav->sender;
				tgt = ids->GetVessel();
				ps  = ids->GetPortSpec();
				} break;
			}
		}
	}

	// Output HUD mode and reference object
	if (_strnicmp(refstr, refname, 127)) {
		for (i = 0; refstr[i] && i<127; i++) refname[i] = toupper(refstr[i]);
		refname[i] = '\0';
		refwidth = TexBltString (refname, 47, 230) + 47;
	}
	AddMesh_Billboard (ivtx, iidx, spec.CX-renderprm.scal*refwidth*0.7, renderprm.scal*10, renderprm.scal*refwidth*1.4, renderprm.scal*19.6, 0, 229, refwidth, 13);

	if (!tgt) return; // no docking target

	// rel velocity marker
	double xcnt, ycnt;
	Vector vrel = self->GVel() - tgt->GVel();
	double len = vrel.length();
	Vector Vunit = vrel/len;

	sprintf (cbuf, "-V[%s]%s", tgt->Name(), DistStr(len));
	char *pc = cbuf+1;

	if (!AddMesh_Marker (ivtx, iidx, Vunit, 1, &xcnt, &ycnt)) {
		if (AddMesh_Marker (ivtx, iidx, -Vunit, 0, &xcnt, &ycnt)) 
			pc = cbuf;
		else
			AddMesh_DirectionMarker (ivtx, iidx, Vunit, false, &xcnt, &ycnt);
	}

	if (_strnicmp (pc,vstr1, 127)) {
		strncpy (vstr1, pc, 127);
		vstr1width = TexBltString (vstr1, 0, 217);
	}
	AddMesh_Billboard (ivtx, iidx, xcnt-0.7*renderprm.scal*vstr1width, ycnt-renderprm.scal*60, 1.4*renderprm.scal*vstr1width, renderprm.scal*19.6, 0, 216, vstr1width, 13);

	// target marker
	Vector prel = tgt->GPos() - self->GPos();
	len = prel.length(); prel /= len;
	if (!AddMesh_Marker (ivtx, iidx, prel, 2, &xcnt, &ycnt))
		AddMesh_DirectionMarker (ivtx, iidx, prel, false, &xcnt, &ycnt);
	sprintf (cbuf, "D[%s]%s", tgt->Name(), DistStr(len));
	if (_strnicmp (cbuf, pstr1, 127)) {
		strncpy (pstr1, cbuf, 127);
		pstr1width = TexBltString (pstr1, 256, 217);
	}
	AddMesh_Billboard (ivtx, iidx, xcnt-0.7*renderprm.scal*pstr1width, ycnt+renderprm.scal*40.4, 1.4*renderprm.scal*pstr1width, renderprm.scal*19.6, 256, 216, pstr1width, 13);

	// approach path gates
	if (ps) {
		AddMesh_DockApproachGates (ivtx, iidx, tgt, ps);
	}
}

void HUD_Docking::SetNav (DWORD setnav)
{
	nv = setnav;
	bUseLegacyReference = false;
}

void HUD_Docking::SetReference (const Body *ref, int port)
{
	Legacy_ref = ref;
	Legacy_port = port;
	bUseLegacyReference = true;
}

void HUD_Docking::SelectReference ()
{
	bUseLegacyReference = false;
	if (++nv >= g_focusobj->nnav) nv = 0;
}

void HUD_Docking::SelectReferenceOld ()
{
	// legacy method for docking target selection
	// this should probably be dropped at some stage
	g_input->Open ("Docking target (+ optional port #), e.g. 'ISS 1':", 0, 20, HUD_Docking::ClbkName_Ref, (void*)this);
}

void HUD_Docking::ProcessMessage (int msg, void *data)
{
	switch (msg) {
	case MSG_KILLVESSEL: {
		Vessel *v = (Vessel*)data;
		if (v == Legacy_ref) {
			Legacy_ref = 0;
			Legacy_port = -1;
			bUseLegacyReference = false;
		}
		} break;
	}
}

bool HUD_Docking::ClbkName_Ref (InputBox*, char *str, void *data)
{
	HUD_Docking* hud = (HUD_Docking*)data;
	return hud->SelectRef (str);
}

bool HUD_Docking::SelectRef (char *str)
{
	char name[256];
	DWORD dock;
	bool bdock = (sscanf (str, "%s%d", name, &dock) == 2);
	Vessel *obj = g_psys->GetVessel (name, true);
	if (!obj) return false;
	if (bdock && (dock < 1 || dock > obj->nDock())) return false;
	Legacy_ref  = obj;
	Legacy_port = (bdock ? (int)dock : 0) - 1;
	bUseLegacyReference = true;
	return true;
}

void HUD_Docking::WriteParams (ostream &ofs) const
{
	ofs << "  TYPE Docking" << endl;
	if (bUseLegacyReference && Legacy_ref) {
		ofs << "  REF " << Legacy_ref->Name();
		if (Legacy_port >= 0) ofs << ' ' << Legacy_port;
		ofs << endl;
	} else {
		ofs << "  NAV " << nv << endl;
	}
}

void HUD_Docking::ReadParams (ifstream &ifs)
{
	char cbuf[256], *pc;
	for (;;) {
		if (!ifs.getline (cbuf, 256)) {
			ifs.clear(); break;
		}
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_HUD", 7)) break;
		if (!_strnicmp (pc, "NAV", 3)) {
			sscanf (pc+3, "%d", &nv);
			bUseLegacyReference = false;
		} else if (!_strnicmp (pc, "REF", 3)) {
			char name[256];
			int res, port;
			res = sscanf (pc+3, "%s%d", name, &port);
			Legacy_ref = g_psys->GetObj (name, true);
			if (res >= 2) Legacy_port = port;
			bUseLegacyReference = true;
		}
	}
}

void BoxCoord (const Vector &dockpos, const Vector &dockdir,
			   const Vector &refdir, double dist, Vector *crd)
{
	const double size1 = 7.0, size2 = 5.0;
	Vector xdir (crossp (dockdir, refdir));
	Vector s (dockpos - dockdir*dist);
	for (int i = 0; i < 4; i++)
		crd[i] = s + refdir * (i <= 1 ? size2 : -size2) + xdir * (i == 0 || i == 3 ? size1 : -size1);
}
