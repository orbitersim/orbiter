// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Orbiter.h"
#include "Panel2D.h"
#include "Pane.h"
#include "Vessel.h"
#include <zmouse.h>

using namespace std;

extern Orbiter *g_pOrbiter;
extern Vessel *g_focusobj;
extern TimeData td;
extern char DBG_MSG[256];

Panel2D::Panel2D (int _id, Pane *_pane, double scale)
{
	int i;

	gc   = g_pOrbiter->GetGraphicsClient();
	if (gc) {
		gc->clbkGetViewportSize (&viewW, &viewH);
	} else {
		viewW = viewH = 0;
	}
	pane = _pane;
	hBkgMesh = NULL;
	nSurf = 0;
	panelW = panelH = 0;
	tgtW = tgtH = 0.0;
	panelscale = userscale = scale;
	x0 = y0 = 0.0;
	id   = _id;
	visible = true;
	allowMFDNudge = true;
	shiftflag = PANEL_ATTACH_BOTTOM | PANEL_MOVEOUT_BOTTOM;
	zoomaction = NONE;
	narea = nareabuf = 0;
	idx_mfocus = aid_mfocus = -1;
	mstate = 0;

	if (g_pOrbiter->IsFullscreen()) cwnd = 0;
	else                            cwnd = g_pOrbiter->GetRenderWnd();

	for (i = 0; i < 4; i++)
		connect[i] = -1;

	for (i = 0; i < MAXMFD; i++)
		mfdspec[i].nmsh = -1; // 'disabled' flag
}

Panel2D::~Panel2D ()
{
	ReleaseAreas();
	ReleaseSurfaces();
}

void Panel2D::ReleaseSurfaces ()
{
	if (nSurf) {
		for (int i = 0; i < nSurf; i++)
			gc->clbkReleaseSurface (hSurf[i]);
		delete []hSurf;
		nSurf = 0;
	}
}

void Panel2D::Setup ()
{
	RedrawAllAreas (PANEL_REDRAW_INIT);
}

void Panel2D::SetConnections (int left, int right, int top, int bottom)
{
	connect[0] = left;
	connect[1] = right;
	connect[2] = top;
	connect[3] = bottom;
}

int Panel2D::SetBackground (SURFHANDLE *hSurface, DWORD nsurf, MESHHANDLE hMesh, DWORD width, DWORD height, DWORD baseline, DWORD scrollflag)
{
	ReleaseSurfaces();
	if (nSurf = nsurf) {
		hSurf = new SURFHANDLE[nSurf];
		for (DWORD i = 0; i < nsurf; i++) {
			hSurf[i] = hSurface[i];
			if (hSurf[i]) gc->clbkIncrSurfaceRef (hSurface[i]);
		}
	} else hSurf = 0;
	hBkgMesh = hMesh;
	shiftflag = scrollflag;
	panelW   = width;
	panelH   = height;
	ybase    = baseline;
	panelscale = minscale = maxscale = userscale;
	tgtW = (double)width * panelscale;
	tgtH = (double)height * panelscale;
	x0 = 0.5 * ((double)viewW - tgtW);
	if (scrollflag & PANEL_ATTACH_TOP) y0 = (double)baseline * panelscale;
	else                               y0 = ((double)viewH + baseline*panelscale - tgtH);
	SetTransformation();
	zoomaction = NONE;
	return 0;
}

int Panel2D::SetScaling (double scale1, double scale2)
{
	minscale = min (scale1,scale2)*userscale;
	maxscale = max (scale1,scale2)*userscale;
	panelscale = scale1*userscale;
	tgtW = (double)panelW * panelscale;
	tgtH = (double)panelH * panelscale;
	x0 = 0.5 * ((double)viewW - tgtW);
	if (shiftflag & PANEL_ATTACH_TOP) y0 = (double)ybase * panelscale;
	else                              y0 = ((double)viewH + ybase*panelscale - tgtH);
	refx = refy = 0;
	zoomaction = NONE;
	SetActiveScale (panelscale, true);
	return 0;
}

void Panel2D::SetActiveScale (double scale, bool force)
{
	if (scale == panelscale && !force) return; // nothing to do

	double sr = scale/panelscale;
	panelscale = scale;
	tgtW = (double)panelW * panelscale;
	tgtH = (double)panelH * panelscale;
	if (tgtW < viewW)
		x0 = 0.5 * ((double)viewW - tgtW);
	else {
		x0 = refx - sr*(refx-x0);
		x0 = max (min(0,x0), viewW-tgtW);
	}
	y0 = refy - sr*(refy-y0);
	if      (shiftflag & PANEL_ATTACH_BOTTOM) y0 = max (y0, viewH-tgtH);
	else if (shiftflag & PANEL_ATTACH_TOP)    y0 = min (y0, 0);

	if (panelscale == 1.0) { // pixel alignment 
		x0 = floor(x0+0.5);
		y0 = floor(y0+0.5);
	}
	SetTransformation();
}

int Panel2D::RegisterMFDGeometry (int MFD_id, int nmesh, int ngroup)
{
	mfdspec[MFD_id].nmsh = nmesh; // for now, nmesh is ignored
	mfdspec[MFD_id].ngrp = ngroup;
	mfdspec[MFD_id].flag = 0;
	if (hBkgMesh) {
		Mesh *mesh = (Mesh*)hBkgMesh;
		GroupSpec *grp = mesh->GetGroup (ngroup);
		if (!grp) return 0;
		grp->TexIdx = TEXIDX_MFD0 + MFD_id;
		float xmin, xmax, ymin, ymax;
		xmin = xmax = grp->Vtx[0].x, ymin = ymax = grp->Vtx[0].y;
		for (DWORD i = 1; i < grp->nVtx; i++) {
			xmin = min (xmin, grp->Vtx[i].x);
			xmax = max (xmax, grp->Vtx[i].x);
			ymin = min (ymin, grp->Vtx[i].y);
			ymax = max (ymax, grp->Vtx[i].y);
		}
		mfdspec[MFD_id].left   = xmin;
		mfdspec[MFD_id].right  = xmax;
		mfdspec[MFD_id].top    = ymin;
		mfdspec[MFD_id].bottom = ymax;
		MFDSPEC dummy; memset (&dummy, 0, sizeof(MFDSPEC));
		pane->RegisterMFD (MFD_id, dummy);
	}
	return 0;
}

Instrument::Spec Panel2D::GetMFDSpec (int MFD_id) const
{
	Instrument::Spec spec;
	if (hBkgMesh) {
		int ngrp = mfdspec[MFD_id].ngrp;
		Mesh *mesh = (Mesh*)hBkgMesh;
		GroupSpec *grp = mesh->GetGroup (ngrp);
		spec.w = (int)((mfdspec[MFD_id].right-mfdspec[MFD_id].left)*panelscale+0.5);
		spec.h = (int)((mfdspec[MFD_id].bottom-mfdspec[MFD_id].top)*panelscale+0.5);
		// make multiple of 4
		spec.w = ((spec.w+2) >> 2) << 2;
		spec.h = ((spec.h+2) >> 2) << 2;
	} else {
		spec.w = 256; spec.h = 256;
	}
	spec.nbtl = spec.nbtr = 6;  // for now
	spec.bt_y0 = spec.h/6;
	spec.bt_dy = spec.h/7;
	spec.flag = 0;
	return spec;
}

void Panel2D::Move (double dx, double dy)
{
	bool moved = false;

	if (dx && tgtW > viewW) { // horizontal panning only if panel wider than viewport
		x0 += dx;
		x0 = max (min (x0, 0), viewW-tgtW);
		moved = true;
	}
	if (dy) {
		y0 += dy;
		//if (y0 < 0 && !(shiftflag & PANEL_MOVEOUT_TOP)) y0 = 0;
		if (y0 > 0 &&  (shiftflag & PANEL_ATTACH_TOP))  y0 = 0;
		//if (y0+tgtH > viewH && !(shiftflag & PANEL_MOVEOUT_BOTTOM)) y0 = viewH-tgtH;
		if (y0+tgtH < viewH &&  (shiftflag & PANEL_ATTACH_BOTTOM))  y0 = viewH-tgtH;

		if      (y0 > viewH) y0 = viewH, visible = false; // moved out through bottom
		else if (y0 < -tgtH) y0 = -tgtH, visible = false; // moved out through top
		else                             visible = true;
		moved = true;
	}
	if (moved) {
		if (panelscale == 1.0) { // pixel alignment
			x0 = floor(x0+0.5);
			y0 = floor(y0+0.5);
		}
		SetTransformation();
	}
}

void Panel2D::SetTransformation ()
{
	transf = _M(panelscale, 0, x0-0.5, 0, panelscale, y0-0.5, 0, 0, 1);

	if (allowMFDNudge && hBkgMesh) {
		Mesh *mesh = (Mesh*)hBkgMesh;
		int mfd, w, h;
		float xcnt, ycnt, xcnt_t, ycnt_t, wdsp, hdsp;
		double dx, dy;
		for (mfd = 0; mfd < MAXMFD; mfd++) {
			if (mfdspec[mfd].nmsh >= 0) {
				GroupSpec *grp = mesh->GetGroup (mfdspec[mfd].ngrp);
				if (!grp) return;
				w = (int)((mfdspec[mfd].right-mfdspec[mfd].left)*panelscale+0.5);
				h = (int)((mfdspec[mfd].bottom-mfdspec[mfd].top)*panelscale+0.5);
				// make multiple of 4
				w = ((w+2) >> 2) << 2;
				h = ((h+2) >> 2) << 2;
				// original MFD centre
				xcnt = 0.5f * (mfdspec[mfd].left + mfdspec[mfd].right);
				ycnt = 0.5f * (mfdspec[mfd].top + mfdspec[mfd].bottom);
				// transformed MFD centre
				xcnt_t = (float)(xcnt*panelscale+x0);
				ycnt_t = (float)(ycnt*panelscale+y0);
				// nudge to integer
				dx = xcnt_t-floor(xcnt_t); if (dx > 0.5) dx -= 1.0;
				dy = ycnt_t-floor(ycnt_t); if (dy > 0.5) dy -= 1.0;
				xcnt -= (float)(dx/panelscale);
				ycnt -= (float)(dy/panelscale);
				//xcnt = (int)(xcnt*panelscale+0.5)/panelscale;
				//ycnt = (int)(ycnt*panelscale+0.5)/panelscale;
				//dy = y0-floor(y0); if (dy > 0.5) dy -= 1.0;
				//ycnt -= (float)dy;
				wdsp = (float)(0.5*w/panelscale);
				hdsp = (float)(0.5*h/panelscale);
				grp->Vtx[0].x = grp->Vtx[2].x = xcnt-wdsp;
				grp->Vtx[1].x = grp->Vtx[3].x = xcnt+wdsp;
				grp->Vtx[0].y = grp->Vtx[1].y = ycnt-hdsp;
				grp->Vtx[2].y = grp->Vtx[3].y = ycnt+hdsp;
			}
		}
	}
}

void Panel2D::Render ()
{
	if (gc && hBkgMesh) {
		// use graphics client to render surface as billboard
		gc->clbkRender2DPanel (hSurf, hBkgMesh, &transf);
		extern TimeData td;
		if (pane->blinkmesh.mesh2d && fmod(td.SysT0,1.0) < 0.7)
			gc->clbkRender2DPanel (&pane->blinkmesh.tex, pane->blinkmesh.mesh2d, &transf);
	}
}

bool Panel2D::ProcessMouse_System(UINT event, DWORD state, int x, int y, const char *kstate)
{
	// Windows event handler for mouse events
	switch (event) {
	case WM_MOUSEWHEEL:
		if ((KEYMOD_CONTROL(kstate))) {
			short zDelta = (short)HIWORD(state);
			zoomaction = (zDelta < 0 ? ZOOM_OUT : ZOOM_IN);
			refx = x, refy = y;
			return true;
		}
		break;
	}
	return false;
}

bool Panel2D::ProcessMouse_OnRunning (UINT event, DWORD state, int x, int y, const char *kstate)
{
	mstate = 0;

	// Windows event handler for mouse events
	switch (event) {
	case WM_LBUTTONDOWN:
		state = PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED;
		break;
	case WM_RBUTTONDOWN:
		state = PANEL_MOUSE_RBDOWN | PANEL_MOUSE_RBPRESSED;
		break;
	case WM_LBUTTONUP:
		state = PANEL_MOUSE_LBUP;
		break;
	case WM_RBUTTONUP:
		state = PANEL_MOUSE_RBUP;
		break;
	//case WM_MOUSEWHEEL:
	//	if ((KEYMOD_CONTROL(kstate))) {
	//		short zDelta = (short)HIWORD(state);
	//		zoomaction = (zDelta < 0 ? ZOOM_OUT : ZOOM_IN);
	//		refx = x, refy = y;
	//		return true;
	//	}
	//	break;
	}

	// mouse state event handler (button-down events only)
	if (state & PANEL_MOUSE_DOWN) {
	    int i;
	    double tx, ty;  // transformed coordinates
		idx_mfocus = -1;
	    tx = ((double)x-x0)/panelscale;
	    ty = ((double)y-y0)/panelscale;
	    if (tx < 0.0 || tx > panelW || ty < 0.0 || ty > panelH)
			return false;
	    for (i = 0; i < narea; i++) {
			if (tx >= area[i]->pos.left && tx <= area[i]->pos.right &&
				ty >= area[i]->pos.top && ty <= area[i]->pos.bottom)
				break;
	    }
	    if (i == narea) return false;
		idx_mfocus = i;
		aid_mfocus = area[i]->id;
		mousex = (int)(tx+0.5) - area[i]->pos.left;
		mousey = (int)(ty+0.5) - area[i]->pos.top;
		if (!(mstate = state & area[i]->mouse)) return false;
		if (g_focusobj->bPlayback() && !(area[i]->mouse & PANEL_MOUSE_ONREPLAY)) {
			mstate = 0;
			return false;
		}
		return true;
	} else {
		if (idx_mfocus < 0 || !(mstate = state & area[idx_mfocus]->mouse)) return false;
		return true;
	}
}

void Panel2D::GetMouseState (int &idx, int &state, int &mx, int &my) const
{
	if (mstate & PANEL_MOUSE_PRESSED) {
		POINT pt;
		GetCursorPos (&pt);
		if (cwnd) // need to subtract client window offset
			ScreenToClient (cwnd, &pt);
	    double tx, ty;  // transformed coordinates
	    tx = ((double)pt.x-x0)/panelscale;
	    ty = ((double)pt.y-y0)/panelscale;
		mousex = (int)(tx+0.5) - area[idx_mfocus]->pos.left;
		mousey = (int)(ty+0.5) - area[idx_mfocus]->pos.top;
	}
	idx = idx_mfocus; state = mstate; mx = mousex, my = mousey;
}

void Panel2D::Update (double SimT, double SysT)
{
	if (zoomaction != NONE) {
		const double zoomspeed = 1.0;
		double newscale;
		if (zoomaction == ZOOM_IN) {
			newscale = min (maxscale, panelscale + zoomspeed*SysT);
			if (newscale == maxscale) zoomaction = NONE;
		} else {
			newscale = max (minscale, panelscale - zoomspeed*SysT);
			if (newscale == minscale) zoomaction = NONE;
		}
		if (newscale != panelscale) {
			SetActiveScale (newscale);
			if (zoomaction == NONE) { // finished rescaling
				for (int i = 0; i < MAXMFD; i++)
					if (mfdspec[i].nmsh >= 0) pane->RefreshMFD (i);
			}
		}
	}
}

int Panel2D::DefineArea (int aid, const RECT &pos, int draw_mode, int mouse_mode, SURFHANDLE surf, void *context)
{
	// grow area buffer
	if (narea == nareabuf) {
		Area **tmp = new Area*[nareabuf += 32];
		if (narea) {
			memcpy (tmp, area, narea*sizeof(Area*));
			delete []area;
		}
		area = tmp;
	}
	// allocate new area
	area[narea] = new Area;
	area[narea]->id      = aid;
	area[narea]->texid   = 0;  // check this!
	area[narea]->pos     = pos;
	area[narea]->texpos  = _R(0,0,0,0);
	area[narea]->w       = 0;
	area[narea]->h       = 0;
	area[narea]->bltmode = PANEL_MAP_NONE;
	area[narea]->surf    = surf;
	if (surf && gc) gc->clbkIncrSurfaceRef (surf);
	area[narea]->bksurf  = NULL;
	area[narea]->redraw  = draw_mode & 0xFF;
	area[narea]->mouse   = mouse_mode;
	area[narea]->context = context;
	narea++;
	return 0;
}

int Panel2D::DefineArea (int aid, const RECT &pos, int texid, const RECT &texpos, int draw_mode, int mouse_mode, int bkmode)
{
	int draw_event_mode = draw_mode & 0xFF; // strip access flags

	// sanity-checks
	if (draw_event_mode == PANEL_REDRAW_NEVER) bkmode = PANEL_MAP_NONE;

	if (narea == nareabuf) {
		Area **tmp = new Area*[nareabuf += 32];
		if (narea) {
			memcpy (tmp, area, narea*sizeof(Area*));
			delete []area;
		}
		area = tmp;
	}
	area[narea] = new Area;
	area[narea]->id      = aid;
	area[narea]->texid   = texid;
	area[narea]->pos     = pos;
	area[narea]->texpos  = texpos;
	area[narea]->w       = pos.right-pos.left;
	area[narea]->h       = pos.bottom-pos.top;
	area[narea]->bltmode = bkmode;
	area[narea]->redraw  = draw_event_mode;
	area[narea]->mouse   = mouse_mode;
	area[narea]->context = 0;

	// allocate surfaces for area
	if (draw_event_mode == PANEL_REDRAW_NEVER) {
		area[narea]->surf = NULL;
		area[narea]->bksurf = NULL;
	} else {
		DWORD attrib = OAPISURFACE_RENDERTARGET;
		if (draw_mode & PANEL_REDRAW_GDI) attrib |= OAPISURFACE_GDI;
		if (draw_mode & PANEL_REDRAW_SKETCHPAD) attrib |= OAPISURFACE_SKETCHPAD;
		area[narea]->surf = gc->clbkCreateSurfaceEx (area[narea]->w, area[narea]->h, attrib);
		switch (bkmode) {
		case PANEL_MAP_NONE:
			area[narea]->bksurf = NULL;
			area[narea]->bltmode = PANEL_MAP_NONE;
			break;
		case PANEL_MAP_CURRENT:
			area[narea]->bksurf = NULL;
			area[narea]->bltmode = PANEL_MAP_NONE;
			if (area[narea]->surf) {
				RECT &r = area[narea]->texpos;
				gc->clbkBlt (area[narea]->surf, 0, 0, hSurf[texid], r.left, r.top, r.right-r.left, r.bottom-r.top);
			}
			break;
		case PANEL_MAP_BACKGROUND:
		case PANEL_MAP_BGONREQUEST:
			if (!(area[narea]->bksurf = gc->clbkCreateSurfaceEx (area[narea]->w, area[narea]->h, OAPISURFACE_RENDERTARGET))) {
				area[narea]->bltmode = PANEL_MAP_NONE;
			} else {
				RECT &r = area[narea]->texpos;
				gc->clbkBlt (area[narea]->bksurf, 0, 0, hSurf[texid], r.left, r.top, r.right-r.left, r.bottom-r.top);
			}
			break;
		}
	}
	narea++;
	return 0;
}

void Panel2D::ReleaseAreas ()
{
	if (!nareabuf) return;
	for (int i = 0; i < narea; i++) {
		if (area[i]->surf) {
			gc->clbkReleaseSurface (area[i]->surf);
			area[i]->surf = NULL;
		}
		if (area[i]->bksurf) {
			gc->clbkReleaseSurface (area[i]->bksurf);
			area[i]->bksurf = NULL;
		}
		delete area[i];
	}
	delete []area;
	area = NULL;
	narea = nareabuf = 0;
}

void Panel2D::SetArea (int idx, SURFHANDLE s)
{
	if (!gc) return; // should be redundant
	Area *a = area[idx];
	if (!a->w || !a->h) return; // zero texture area
	gc->clbkBlt (hSurf[a->texid], a->texpos.left, a->texpos.top, s, 0, 0, a->w, a->h);
}

SURFHANDLE Panel2D::GetArea (int idx)
{
	if (!gc) return NULL; // should be redundant
	Area *a = area[idx];
	switch (a->bltmode) {
	case PANEL_MAP_NONE:
	case PANEL_MAP_BGONREQUEST:
		return a->surf;
	case PANEL_MAP_BACKGROUND: {
		gc->clbkBlt (a->surf, 0, 0, a->bksurf, 0, 0, a->w, a->h);
		return a->surf; }
	case PANEL_MAP_CURRENT: {
		gc->clbkBlt (a->surf, 0, 0, hSurf[a->texid], a->texpos.left, a->texpos.top, a->texpos.right-a->texpos.left, a->texpos.bottom-a->texpos.top);
		return a->surf; }
	default:
		return NULL;
	}
}

void Panel2D::RedrawArea (int idx, int event)
{
	SURFHANDLE s = GetArea(idx);
	if (g_focusobj->PanelRedrawEvent (area[idx]->id, event, s, area[idx]->context))
		SetArea (idx, s);
}

void Panel2D::RedrawAllAreas (int event)
{
	for (int i = 0; i < narea; i++)
		if ((area[i]->redraw & event) ||
			(area[i]->redraw && event == PANEL_REDRAW_INIT))
		RedrawArea (i, event);
}

bool Panel2D::Read (ifstream &ifs)
{
	if (!FindLine (ifs, "BEGIN_PANEL")) return false;
	// read panel parameters
	return true;
}

void Panel2D::Write (ostream &ofs) const
{
	ofs << "BEGIN_PANEL" << endl;
	ofs << "END_PANEL" << endl;
}
