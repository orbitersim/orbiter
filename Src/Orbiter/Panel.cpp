// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Orbiter.h"
#include "Panel.h"
#include "Pane.h"
#include "Vessel.h"
#include "Texture.h"
#include "Log.h"

using namespace std;

extern Orbiter *g_pOrbiter;
extern Vessel *g_focusobj;
extern char DBG_MSG[256];

// =======================================================================
// class Panel

Panel::Panel (int _id, const Pane *_pane, double _scale)
{
	int i;

	gc      = g_pOrbiter->GetGraphicsClient();
	id      = _id;
	pane    = _pane;
	scale   = _scale;
	iscale  = 1.0/scale;
	scaled  = (scale != 1.0);
	surf    = NULL;
	visible = false;
	has_ck  = false;
	if (g_pOrbiter->IsFullscreen())
		cwnd = 0;
	else
		cwnd = g_pOrbiter->GetRenderWnd();
	narea   = nareabuf = 0;
	idx_mfocus = aid_mfocus = mstate = 0;

	shiftflag = 0;
	for (i = 0; i < MAXMFD; i++)
		mfd[i].exist = false;
	for (i = 0; i < 4; i++)
		connect[i] = -1;
}

Panel::~Panel ()
{
	if (surf) gc->clbkReleaseSurface (surf);
	ReleaseAreas ();
}

void Panel::Setup ()
{
	MFDMoved();
}

void Panel::SetConnections (int left, int right, int top, int bottom)
{
	connect[0] = left;
	connect[1] = right;
	connect[2] = top;
	connect[3] = bottom;
}

void Panel::Display (SURFHANDLE pdds)
{
	if (visible && gc) {
		// panel background
		if (has_ck) gc->clbkSetSurfaceColourKey (surf, ck);
		if (scaled) gc->clbkScaleBlt (pdds, tgtRect.left, tgtRect.top, tgtRect.right-tgtRect.left, tgtRect.bottom-tgtRect.top, surf, srcRect.left, srcRect.top, srcRect.right-srcRect.left, srcRect.bottom-srcRect.top, bltflag);
		else        gc->clbkBlt (pdds, tgtRect.left, tgtRect.top, surf, srcRect.left, srcRect.top, srcRect.right-srcRect.left, srcRect.bottom-srcRect.top, bltflag);
		if (has_ck) /*gc->clbkUnsetSurfaceColourKey (surf)*/; // surf->SetColorKey (DDCKEY_SRCBLT, 0);

		// MFDs
		for (int i = 0; i < MAXMFD; i++)
			if (pane->mfd[i].instr && mfd[i].visible)
				gc->clbkBlt (pdds, mfd[i].tgtx, mfd[i].tgty, pane->mfd[i].instr->Surface(), mfd[i].src_vis.left, mfd[i].src_vis.top,
					mfd[i].src_vis.right-mfd[i].src_vis.left, mfd[i].src_vis.bottom-mfd[i].src_vis.top);
	}
}

void Panel::Move (LONG dx, LONG dy)
{
	// we only allow horizontal shifts if panel is wider than screen
	if (dx && tgtW > pane->W) {
		X0 += dx;
		if (X0 > 0) X0 = 0;
		if (tgtW+X0 < pane->W) X0 = pane->W-tgtW;
		tgtRect.left   = 0;
		tgtRect.right  = min (tgtRect.left+tgtW, (LONG)pane->W);
		srcRect.left   = -X0;
		srcRect.right  = srcRect.left + (tgtRect.right-tgtRect.left);
		if (scaled) {
			srcRect.left   = max((LONG)(srcRect.left  *iscale), (LONG)0);
			srcRect.right  = min((LONG)(srcRect.right * iscale), srcW);
		}
	}
	if (dy) {
		Y0 += dy;
		if (Y0 < 0 && !(shiftflag & PANEL_MOVEOUT_TOP)) Y0 = 0;
		if (Y0 > 0 &&  (shiftflag & PANEL_ATTACH_TOP)) Y0 = 0;

		if (Y0+tgtH > pane->H && !(shiftflag & PANEL_MOVEOUT_BOTTOM)) Y0 = pane->H-tgtH;
		if (Y0+tgtH < pane->H &&  (shiftflag & PANEL_ATTACH_BOTTOM))  Y0 = pane->H-tgtH;

		if      (Y0 > pane->H) Y0 = pane->H, visible = false; // moved out through bottom
		else if (Y0 < -tgtH)   Y0 = -tgtH,   visible = false; // moved out through top
		else                                 visible = true;

		tgtRect.top    = (Y0 >= 0 ? Y0 : 0);
		tgtRect.bottom = min (Y0+tgtH, (LONG)pane->H);
		srcRect.top    = tgtRect.top-Y0;
		srcRect.bottom = min (tgtH, srcRect.top  + (tgtRect.bottom-tgtRect.top));
		if (scaled) {
			srcRect.top    = max((LONG)(srcRect.top   *iscale), (LONG)0);
			srcRect.bottom = min((LONG)(srcRect.bottom* iscale), srcH);
		}
	}
	MFDMoved();
}

void Panel::MFDMoved ()
{
	for (int i = 0; i < MAXMFD; i++) {
		if (!mfd[i].exist) continue;
		mfd[i].tgtx = mfd[i].panel_pos.left + X0;
		mfd[i].tgty = mfd[i].panel_pos.top  + Y0;
		mfd[i].src_vis.right  = (mfd[i].tgtx + mfd[i].w <= pane->W ? mfd[i].w : pane->W - mfd[i].tgtx);
		mfd[i].src_vis.bottom = (mfd[i].tgty + mfd[i].h <= pane->H ? mfd[i].h : pane->H - mfd[i].tgty);
		if (mfd[i].tgtx >= 0) mfd[i].src_vis.left = 0;
		else                  mfd[i].src_vis.left = -mfd[i].tgtx, mfd[i].tgtx = 0;
		if (mfd[i].tgty >= 0) mfd[i].src_vis.top  = 0;
		else                  mfd[i].src_vis.top  = -mfd[i].tgty, mfd[i].tgty = 0;
		mfd[i].visible = (mfd[i].src_vis.bottom >= 0 && mfd[i].src_vis.top < mfd[i].src_vis.bottom &&
			              mfd[i].src_vis.right  >= 0 && mfd[i].src_vis.left < mfd[i].src_vis.right);
	}
}

void Panel::DefineBackground (HBITMAP hBmp, DWORD flag, DWORD _ck)
{
	if (!gc) return;

	//HRESULT res;
	BITMAP bm;

	if (surf) gc->clbkReleaseSurface (surf);

	// bitmap size
    GetObject (hBmp, sizeof(bm), &bm);
    srcW = bm.bmWidth;
	srcH = bm.bmHeight;
	tgtW = (int)(scale*srcW);
	tgtH = (int)(scale*srcH);

	shiftflag = flag & 0x00FF;

	// some sanity checks
	if (shiftflag & PANEL_ATTACH_BOTTOM) shiftflag |= PANEL_MOVEOUT_TOP;
	if (shiftflag & PANEL_ATTACH_TOP)    shiftflag |= PANEL_MOVEOUT_BOTTOM;
	if (shiftflag & PANEL_ATTACH_LEFT)   shiftflag |= PANEL_MOVEOUT_RIGHT;
	if (shiftflag & PANEL_ATTACH_RIGHT)  shiftflag |= PANEL_MOVEOUT_LEFT;

	if (shiftflag & PANEL_ATTACH_TOP && shiftflag & PANEL_ATTACH_BOTTOM) { // stretch to full height
		if (tgtH < pane->H) {
			scale = (double)pane->H/(double)srcH;
			tgtW = (int)(scale*srcW);
			tgtH = (int)(scale*srcH);
			iscale  = 1.0/scale;
			scaled  = (scale != 1.0);
		}
	}

	if (!(surf = gc->clbkCreateSurface (srcW, srcH))) return;
	if (!gc->clbkCopyBitmap (surf, hBmp, 0, 0, 0, 0)) {
		gc->clbkReleaseSurface (surf);
		surf = NULL;
		return;
	}

	X0 = ((LONG)pane->W - tgtW)/2;

	if ((shiftflag & 0x0003) == PANEL_ATTACH_BOTTOM) // panel attached to screen bottom
		Y0 = (LONG)pane->H - tgtH;
	else if ((shiftflag & 0x0003) == PANEL_ATTACH_TOP) // panel attached to screen top
		Y0 = 0;
	else // pane either attached to top and bottom or not attached
		Y0 = ((LONG)pane->H - tgtH)/2;

	tgtRect.left   = (X0 >= 0 ? X0 : 0);
	tgtRect.top    = (Y0 >= 0 ? Y0 : 0);
	tgtRect.right  = min (tgtRect.left+tgtW, (LONG)pane->W);
	tgtRect.bottom = min (tgtRect.top+tgtH, (LONG)pane->H);

	srcRect.left   = tgtRect.left-X0;
    srcRect.top    = tgtRect.top-Y0;
	srcRect.right  = srcRect.left + (tgtRect.right-tgtRect.left);
	srcRect.bottom = srcRect.top  + (tgtRect.bottom-tgtRect.top);

	if (scaled) {
		srcRect.left   = max((LONG)(srcRect.left  *iscale), (LONG)0);
		srcRect.top    = max((LONG)(srcRect.top   *iscale), (LONG)0);
		srcRect.right  = min((LONG)(srcRect.right * iscale), srcW);
		srcRect.bottom = min((LONG)(srcRect.bottom* iscale), srcH);
	}

	// define color key for blitting
	bltflag = 0;
	if (has_ck = (_ck != (DWORD)-1)) {
		ck = _ck;
		//ck.dwColorSpaceLowValue = ck.dwColorSpaceHighValue = _ck;
		bltflag |= BLT_SRCCOLORKEY;
	}

	visible = true;
}

void Panel::DefineArea (int aid, const RECT &pos, int draw_mode, int mouse_mode, int bkmode)
{
	// sanity-checks
	if (draw_mode == PANEL_REDRAW_NEVER) bkmode = PANEL_MAP_NONE;

	if (narea == nareabuf) {
		Area **tmp = new Area*[nareabuf += 32]; TRACENEW
		if (narea) {
			memcpy (tmp, area, narea*sizeof(Area*));
			delete []area;
		}
		area = tmp;
	}
	area[narea] = new Area; TRACENEW
	area[narea]->id      = aid;
	area[narea]->pos     = pos;
	area[narea]->w       = pos.right-pos.left;
	area[narea]->h       = pos.bottom-pos.top;
	area[narea]->bltmode = bkmode;
	area[narea]->redraw  = draw_mode;
	area[narea]->mouse   = mouse_mode;

	// allocate surfaces for area
	if (draw_mode == PANEL_REDRAW_NEVER) {
		area[narea]->surf = NULL;
		area[narea]->bksurf = NULL;
	} else {
		area[narea]->surf = gc->clbkCreateSurface (area[narea]->w, area[narea]->h);
		switch (bkmode) {
		case PANEL_MAP_NONE:
			area[narea]->bksurf = NULL;
			area[narea]->bltmode = PANEL_MAP_NONE;
			break;
		case PANEL_MAP_CURRENT:
			area[narea]->bksurf = NULL;
			area[narea]->bltmode = PANEL_MAP_NONE;
			if (area[narea]->surf) {
				RECT &r = area[narea]->pos;
				gc->clbkBlt (area[narea]->surf, 0, 0, surf, r.left, r.top, r.right-r.left, r.bottom-r.top);
			}
			break;
		case PANEL_MAP_BACKGROUND:
		case PANEL_MAP_BGONREQUEST:
			if (!(area[narea]->bksurf = gc->clbkCreateSurface (area[narea]->w, area[narea]->h))) {
				area[narea]->bltmode = PANEL_MAP_NONE;
			} else {
				RECT &r = area[narea]->pos;
				gc->clbkBlt (area[narea]->bksurf, 0, 0, surf, r.left, r.top, r.right-r.left, r.bottom-r.top);
			}
			break;
		}
	}
	narea++;
}

void Panel::ReleaseAreas ()
{
	if (!nareabuf) return;
	for (int i = 0; i < narea; i++) {
		if (area[i]->surf) {
			gc->clbkReleaseSurface (area[i]->surf); //area[i]->surf->Release();
			area[i]->surf = NULL;
		}
		if (area[i]->bksurf) {
			gc->clbkReleaseSurface (area[i]->bksurf); //area[i]->bksurf->Release();
			area[i]->bksurf = NULL;
		}
		delete area[i];
	}
	delete []area;
	area = NULL;
	narea = nareabuf = 0;
}

SURFHANDLE Panel::GetArea (int idx)
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
		gc->clbkBlt (a->surf, 0, 0, surf, a->pos.left, a->pos.top, a->pos.right-a->pos.left, a->pos.bottom-a->pos.top);
		return a->surf; }
	default:
		return NULL;
	}
}

bool Panel::BltAreaBackground (int idx, SURFHANDLE s)
{
	if (!gc) return false; // should be redundant
	Area *a = area[idx];
	if (a->bltmode == PANEL_MAP_BGONREQUEST) {
		gc->clbkBlt (s, 0, 0, a->bksurf, 0, 0, a->w, a->h);
		return true;
	} else {
		return false;
	}
}

void Panel::SetArea (int idx, SURFHANDLE s)
{
	if (!gc) return; // should be redundant
	Area *a = area[idx];
	gc->clbkBlt (surf, a->pos.left, a->pos.top, s, 0, 0, a->w, a->h);
}

void Panel::RedrawArea (int idx, int event)
{
	SURFHANDLE s = GetArea(idx);
	if (g_focusobj->PanelRedrawEvent (area[idx]->id, event, s, 0))
		SetArea (idx, s);
}

void Panel::RedrawAllAreas (int event)
{
	for (int i = 0; i < narea; i++)
		if ((area[i]->redraw & event) ||
			(area[i]->redraw && event == PANEL_REDRAW_INIT))
		RedrawArea (i, event);
}

void Panel::RegisterMFD (int id, const MFDSPEC &spec)
{
	RECT &r = mfd[id].panel_pos;
	r = spec.pos;
	if (scaled) {
		r.left        = (int)(scale*r.left);
		r.right       = (int)(scale*r.right);
		r.top         = (int)(scale*r.top);
		r.bottom      = (int)(scale*r.bottom);
	}
	mfd[id].w = r.right-r.left;
	mfd[id].h = r.bottom-r.top;
	mfd[id].exist = true;
}

void Panel::Point2Screen (long srcX, long srcY, long &tgtX, long &tgtY) const
{
	if (scaled) {
		srcX = (long)(srcX*scale);
		srcY = (long)(srcY*scale);
	}
	tgtX = srcX+X0;
	tgtY = srcY+Y0;
}

void Panel::Area2Screen (const RECT &srcR, RECT &tgtR) const
{
	Point2Screen (srcR.left, srcR.top, tgtR.left, tgtR.top);
	Point2Screen (srcR.right, srcR.bottom, tgtR.right, tgtR.bottom);
}

bool Panel::ProcessMouse (UINT event, DWORD state, int x, int y)
{
	mstate = 0;
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
	}
	if (state & PANEL_MOUSE_DOWN) { // locate mouse event
		int i;
		x -= X0, y -= Y0;
		idx_mfocus = -1;
		if (scaled) x = (int)(x*iscale), y = (int)(y*iscale);
		if (x < 0 || x >= srcW || y < 0 || y >= srcH)
			return false;
		for (i = 0; i < narea; i++)
			if (x >= area[i]->pos.left && x < area[i]->pos.right &&
				y >= area[i]->pos.top  && y < area[i]->pos.bottom) break;
		if (i == narea) return false;
		idx_mfocus = i;
		aid_mfocus = area[i]->id;
		mousex = x - area[i]->pos.left;
		mousey = y - area[i]->pos.top;
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

void Panel::GetMouseState (int &idx, int &state, int &mx, int &my) const
{
	if (mstate & PANEL_MOUSE_PRESSED) {
		POINT pt;
		GetCursorPos (&pt);
		if (cwnd) // need to subtract client window offset
			ScreenToClient (cwnd, &pt);
		pt.x -= X0, pt.y -= Y0;
		if (scaled) pt.x = (int)(pt.x*iscale), pt.y = (int)(pt.y*iscale);
		mousex = pt.x - area[idx_mfocus]->pos.left;
		mousey = pt.y - area[idx_mfocus]->pos.top;
	}
	idx = idx_mfocus; state = mstate; mx = mousex, my = mousey;
}

bool Panel::Read (ifstream &ifs)
{
	if (!FindLine (ifs, "BEGIN_PANEL")) return false;
	// read panel parameters
	return true;
}

void Panel::Write (ostream &ofs) const
{
	ofs << "BEGIN_PANEL" << endl;
	ofs << "END_PANEL" << endl;
}
