// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "MfdMap_old.h"
#include "Pane.h"
#include "Psys.h"
#include "Celbody.h"
#include "Planet.h"
#include "Base.h"

using namespace std;

extern Orbiter *g_pOrbiter;
extern PlanetarySystem *g_psys;
extern InputBox *g_input;
extern Select *g_select;

// =======================================================================
// class Instrument_Map

struct Instrument_MapOld::SavePrm Instrument_MapOld::saveprm = {0,0,0,0,false,true};

Instrument_MapOld::Instrument_MapOld (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel)
: Instrument (_pane, _id, spec, _vessel)
{
	map = bgmap = 0;
	mapw = IW, maph = IW/2;
	mapx = mapy = 0;
	refplanet = 0;
	btgt      = 0;
	otgt      = 0;
	zoom      = false;
	track     = true;
	if (_vessel == saveprm.usr) {
		refplanet = saveprm.ref;
		btgt      = saveprm.btgt;
		otgt      = saveprm.otgt;
		zoom      = saveprm.zoom;
		track     = saveprm.track;
		if (!track) {
			mapx = (int)(saveprm.cx*IW);
			mapy = (int)(saveprm.cy*(IW/2));
		}
	}
	if (!refplanet) refplanet = _vessel->ProxyPlanet();
	if (!btgt)      btgt      = _vessel->LandingTarget();

	strcpy (title, "Map: ");
	strcpy (datastr[0], "Target base: ");
	strcpy (datastr[1], "Target orbit: ");

	if (gc) {
		//pen[0] = gc->clbkCreatePen (1, 1, col_yellow1);
		//pen[1] = gc->clbkCreatePen (1, 1, col_green1);
		//pen[2] = gc->clbkCreatePen (1, 1, RGB(255,128,128));
		//pen[3] = gc->clbkCreatePen (1, 1, RGB(255,255,255));
		brush[0] = gc->clbkCreateBrush (col_green1);
		brush[1] = gc->clbkCreateBrush (col_yellow1);
	}

	double dphi = Pi2/(double)INSTRMAP_NPROJPT;
	for (int i = 0; i < INSTRMAP_NPROJPT; i++) {
		double phi = i*dphi;
		cosp[i] = cos(phi), sinp[i] = sin(phi);
	}
	needmap = (refplanet != 0);
	SetSize (spec);
}

Instrument_MapOld::~Instrument_MapOld ()
{
	int i;

	// save status
	saveprm.usr   = vessel;
	saveprm.ref   = refplanet;
	saveprm.btgt  = btgt;
	saveprm.otgt  = otgt;
	saveprm.zoom  = zoom;
	saveprm.track = track;
	saveprm.cx    = (float)mapx/(float)mapw;
	saveprm.cy    = (float)mapy/(float)maph;
	if (map)   gc->clbkReleaseSurface (map);
	if (bgmap) gc->clbkReleaseSurface (bgmap);

	if (gc) {
		//for (i = 0; i < 4; i++)
		//	if (pen[i]) gc->clbkReleasePen (pen[i]);
		for (i = 0; i < 2; i++)
			if (brush[i]) gc->clbkReleaseBrush (brush[i]);
	}
}

HELPCONTEXT *Instrument_MapOld::HelpTopic () const
{
	extern HELPCONTEXT DefHelpContext;
	DefHelpContext.topic = (char*)"/mfd_map.htm";
	return &DefHelpContext;
}

bool Instrument_MapOld::KeyBuffered (DWORD key)
{
	switch (key) {
	case OAPI_KEY_K:  // track mode on/off
		ToggleTrack ();
		Refresh();
		return true;
	case OAPI_KEY_R:  // select reference
		OpenSelect_CelBody ("Map MFD: Reference", ClbkEnter_Map, 1);
		return true;
	case OAPI_KEY_T:  // select target
		g_select->Open ("Map MFD: Target", ClbkSubmn_Target, ClbkEnter_Target, (void*)this);
		return true;
	case OAPI_KEY_Z:  // zoom in/out
		SetZoom (!zoom);
		Refresh();
		return true;
	}
	return false;
}

bool Instrument_MapOld::KeyImmediate (char *kstate)
{
	if (KEYDOWN (kstate, OAPI_KEY_LBRACKET)) { // scroll left
		if (!track && BufKey (OAPI_KEY_LBRACKET, 0.05)) {
			if ((mapx -= 1) < 0) mapx += mapw;
			Refresh();
		}
		return true;
	}
	if (KEYDOWN (kstate, OAPI_KEY_RBRACKET)) { // scroll left
		if (!track && BufKey (OAPI_KEY_RBRACKET, 0.05)) {
			if ((mapx += 1) > mapw-IW) mapx -= mapw;
			Refresh();
		}
		return true;
	}
	if (KEYDOWN (kstate, OAPI_KEY_MINUS)) { // scroll down
		if (!track && BufKey (OAPI_KEY_MINUS, 0.05)) {
			if ((mapy -= 1) < 0) mapy = 0;
			Refresh();
		}
		return true;
	}
	if (KEYDOWN (kstate, OAPI_KEY_EQUALS)) { // scroll down
		if (!track && BufKey (OAPI_KEY_EQUALS, 0.05)) {
			if ((mapy += 1) > maph-IW/2) mapy = maph-IW/2;
			Refresh();
		}
		return true;
	}
	return false;
}

bool Instrument_MapOld::ProcessButton (int bt, int event)
{
	static const DWORD btkey[8] = { OAPI_KEY_R, OAPI_KEY_T, OAPI_KEY_K, OAPI_KEY_Z, OAPI_KEY_LBRACKET, OAPI_KEY_RBRACKET, OAPI_KEY_MINUS, OAPI_KEY_EQUALS };
	if (event & PANEL_MOUSE_LBDOWN) {
		if (bt < 4) return KeyBuffered (btkey[bt]);
	} else if (event & PANEL_MOUSE_LBPRESSED) {
		if (bt >= 4 && bt < 8) return KeyImmediate (KstateSet (btkey[bt]));
	}
	return false;
}

const char *Instrument_MapOld::BtnLabel (int bt) const
{
	static const char *label[8] = { "REF", "TGT", "TRK", "ZM", "<<", ">>", "UP", "DN" };
	return (bt < 8 ? label[bt] : 0);
}

int Instrument_MapOld::BtnMenu (const MFDBUTTONMENU **menu) const
{
	static const MFDBUTTONMENU mnu[8] = {
		{"Orbit reference", 0, 'R'},
		{"Select target", 0, 'T'},
		{"Track mode on/off", 0, 'K'},
		{"Zoom in/out", 0, 'Z'},
		{"Scroll left", 0, '['},
		{"Scroll right", 0, ']'},
		{"Scroll up", 0, '-'},
		{"Scroll down", 0, '='}
	};
	if (menu) *menu = mnu;
	return 8;
}

void Instrument_MapOld::UpdateMap ()
{
	static oapi::IVECTOR2 p[INSTRMAP_NPROJPT+4], pi0, pi1;
	bool have_shppos = false;

	gc->clbkBlt (map, 0, 0, bgmap); // copy background
	if (!refplanet) return;
	const SurfParam *sp = vessel->GetSurfParam();
	const Elements *el = vessel->Els();

	oapi::Sketchpad *skp = gc->clbkGetSketchpad(map);
	if (skp) {
		oapi::Pen *ppen;
		if (btgt) {  // mark target base in map
			ppen = skp->SetPen (draw[1][0].solidpen);
			skp->Ellipse (tgtx-3, tgty-3, tgtx+4, tgty+4);
			skp->SetPen (ppen);
		}
		if (otgt) {  // draw orbit projection
			const Elements *tel = otgt->Els();
			if (tel && otgt->ElRef() == refplanet) {
				double lng, lat, r;
				int xo, yo;
				CalcOrbitProj (tel, refplanet, p);
				ppen = skp->SetPen (draw[1][0].solidpen);
				skp->Polyline (p, INSTRMAP_NPROJPT+2);
				if (CalcIntersect (tel, refplanet, refplanet->Size(), p+0, p+1)) {
					skp->Rectangle (p[0].x-4, p[0].y-4, p[0].x+5, p[0].y+5);
					skp->SetBrush (brush[1]);
					skp->Rectangle (p[1].x-4, p[1].y-4, p[1].x+5, p[1].y+5);
					skp->SetBrush (0);
				}
				refplanet->GlobalToEquatorial (otgt->GPos(), lng, lat, r);
				CalcCoords (lng, lat, xo, yo);
				skp->Line (xo, yo-4, xo, yo+5);
				skp->Line (xo-4, yo, xo+5, yo);
				skp->SetPen (ppen);
			}
		}
		if (sp && vessel->ProxyPlanet() == refplanet) {
			CalcCoords (sp->lng, sp->lat, shpx, shpy);
			have_shppos = true;
		}
		if (el && vessel->ElRef() == refplanet) {
			// draw ship's orbital plane
			CalcOrbitProj (el, refplanet, p);
			ppen = skp->SetPen (draw[0][0].solidpen);
			skp->Polyline (p, INSTRMAP_NPROJPT+2);
			if (have_shppos && CalcIntersect (el, refplanet, refplanet->Size(), &pi0, &pi1)) {
				skp->SetPen (draw[3][0].solidpen);
				int i, j;
				if (p[INSTRMAP_NPROJPT].x-p[0].x >= 0) { // prograde motion
					if (pi1.x > pi0.x) {
						for (i = 0; i < INSTRMAP_NPROJPT+1 && p[i+1].x <= pi0.x; i++);
						for (j = INSTRMAP_NPROJPT+1; j > 0 && p[j-1].x >= pi1.x; j--);
						skp->Polyline (p, i+1);
						skp->Line (p[i].x, p[i].y, pi0.x, pi0.y);
						skp->Polyline (p+j, INSTRMAP_NPROJPT+2-j);
						skp->Line (pi1.x, pi1.y, p[j].x, p[j].y);
					} else if (pi1.x < pi0.x) {
						for (i = 0; i < INSTRMAP_NPROJPT+1 && p[i].x <= pi1.x; i++);
						for (j = i+1; j < INSTRMAP_NPROJPT+1 && p[j+1].x < pi0.x; j++);
						skp->Line (pi1.x, pi1.y, p[i].x, p[i].y);
						skp->Polyline (p+i, j-i+1);
						skp->Line (p[j].x, p[j].y, pi0.x, pi0.y);
					} else { // x values of intersections equal: just check completely inside/outside
						if (0.5*(el->ApDist() + el->PeDist()) < refplanet->Size()) // inside
							skp->Polyline (p, INSTRMAP_NPROJPT+2);
					}
				} else { // retrograde
					if (pi1.x < pi0.x) {
						for (i = 0; i < INSTRMAP_NPROJPT+1 && p[i+1].x >= pi0.x; i++);
						skp->Polyline (p, i+1);
						skp->Line (p[i].x, p[i].y, pi0.x, pi0.y);
						for (j = INSTRMAP_NPROJPT+1; j > 0 && p[j-1].x <= pi1.x; j--);
						skp->Polyline (p+j, INSTRMAP_NPROJPT+2-j);
						skp->Line (pi1.x, pi1.y, p[j].x, p[j].y);
					} else if (pi1.x > pi0.x) {
						for (i = 0; i < INSTRMAP_NPROJPT+1 && p[i].x >= pi1.x; i++);
						for (j = i+1; j < INSTRMAP_NPROJPT+1 && p[j+1].x >= pi0.x; j++);
						skp->Polyline (p+i, j-i+1);
						skp->Line (pi1.x, pi1.y, p[i].x, p[i].y);
						skp->Line (p[j].x, p[j].y, pi0.x, pi0.y);
					} else { // x values of intersections equal: just check completely inside/outside
						if (0.5*(el->ApDist() + el->PeDist()) < refplanet->Size()) // inside
							skp->Polyline (p, INSTRMAP_NPROJPT+2);
					}
				}

				skp->Rectangle (pi1.x-2, pi1.y-2, pi1.x+3, pi1.y+3);
				skp->Rectangle (pi1.x-1, pi1.y-1, pi1.x+2, pi1.y+2);
				skp->SetPen (draw[0][0].solidpen);
				skp->Rectangle (pi0.x-2, pi0.y-2, pi0.x+3, pi0.y+3);
				skp->Rectangle (pi0.x-1, pi0.y-1, pi0.x+2, pi0.y+2);
			}
			skp->SetPen (ppen);
		}
		if (have_shppos) {
			// mark ship's location
			skp->SetPen (draw[2][0].solidpen);
			skp->Line (shpx, shpy-4, shpx, shpy+5);
			skp->Line (shpx-4, shpy, shpx+5, shpy);
		}
		gc->clbkReleaseSketchpad (skp);
	}
}

void Instrument_MapOld::UpdateBlt ()
{
	if (needmap) {
		LoadMap (refplanet);
		CalcTargetCoords();
		needmap = false;
	}
	if (map) {
		UpdateMap ();
		if (track) {
			mapx = shpx - IW/2;
			mapy = min (maph-IW/2, (max (0, shpy - IW/4)));
		}
		RECT r = {mapx, mapy, mapx+IW, mapy+IW/2};
		if (mapx < 0) {
			r.left = mapw+mapx; r.right = mapw;
			gc->clbkBlt (surf, 0, IH-IW/2, map, mapw+mapx, mapy, -mapx, IW/2);
			r.left = 0; r.right = IW+mapx;
			if (r.right > 0)
				gc->clbkBlt (surf, -mapx, IH-IW/2, map, 0, mapy, IW+mapx, IW/2);
		} else if (mapx+IW > mapw) {
			r.right = mapw;
			gc->clbkBlt (surf, 0, IH-IW/2, map, mapx, mapy, mapw-mapx, IW/2);
			r.left = 0; r.right = IW-mapw+mapx;
			gc->clbkBlt (surf, mapw-mapx, IH-IW/2, map, 0, mapy, IW-mapw+mapx, IW/2);
		} else {
			gc->clbkBlt (surf, 0, IH-IW/2, map, mapx, mapy, IW, IW/2);
		}
	}
}

void Instrument_MapOld::UpdateDraw (oapi::Sketchpad *skp)
{
	//static POINT p[INSTRMAP_NPROJPT+4], pi0, pi1;

	if (refplanet) {

		int x1 = cw/2, x2 = IW/2, dy = ch, y = dy/2;
		double blng, blat, adist, hdg, rad;
		char cbuf[40];
		const SurfParam *sp = vessel->GetSurfParam();
		const Elements *el = vessel->Els();

		skp->SetTextColor (draw[0][0].col);
		if (btgt) {
			// output base data
			strcpy (datastr[0]+13, btgt->Name());
			skp->Text (x1, y+dy, datastr[0], strlen(datastr[0]));
			btgt->EquPos (blng, blat);
			sprintf (cbuf, "  Pos: %6.2fº%c %6.2fº%c",
				Deg(fabs(blng)), blng >= 0.0 ? 'E':'W', Deg(fabs(blat)), blat >= 0.0 ? 'N':'S');
			skp->Text (x1, y+2*dy, cbuf, strlen (cbuf));
			if (sp) {
				rad	= refplanet->Size();
				Orthodome (sp->lng, sp->lat, blng, blat, adist, hdg);
				sprintf (cbuf, "  Dst: %s (%0.2fº)", DistStr (adist*rad), Deg(adist));
				skp->Text (x1, y+3*dy, cbuf, strlen (cbuf));
				sprintf (cbuf, "  Dir: %6.2fº", Deg(hdg));
				skp->Text (x1, y+4*dy, cbuf, strlen (cbuf));
			}
		} else {
			strcpy (datastr[0]+13, "N/A");
			skp->Text (x1, y+dy, datastr[0], strlen(datastr[0]));
		}

		if (otgt) {
			// output target orbit data
			skp->SetTextColor (draw[1][0].col); y += 6*dy;
			strcpy (datastr[1]+14, otgt->Name());
			skp->Text (x1, y, datastr[1], strlen(datastr[1])); y += dy;
			const Elements *tel = otgt->Els();
			if (tel && otgt->ElRef() == refplanet) {
				double lng, lat, r;
				refplanet->GlobalToEquatorial (otgt->GPos(), lng, lat, r);
				sprintf (cbuf, "  Pos: %6.2fº%c %6.2fº%c",
					Deg(fabs(lng)), lng >= 0.0 ? 'E':'W', Deg(fabs(lat)), lat >= 0.0 ? 'N':'S');
				skp->Text (x1, y, cbuf, strlen (cbuf)); y += dy;
				sprintf (cbuf, "  Alt: %s", DistStr (r - refplanet->Size()));
				skp->Text (x1, y, cbuf, strlen (cbuf)); y += dy;
			}
		} else {
			strcpy (datastr[1]+14, "N/A");
			skp->Text (x1, y+6*dy, datastr[1], strlen(datastr[1]));
		}
		if (zoom) {
			skp->SetTextColor (draw[2][0].col);
			skp->Text (IW-(cw*13)/2, 0, "ZM", 2);
		}
		if (track) {
			skp->SetTextColor (draw[2][0].col);
			skp->Text (IW-(cw*7)/2, 0, "TRK", 3);
		}
	}
	DisplayTitle (skp, title);
}

int Instrument_MapOld::ProcessMessage (int msg, void *data)
{
	switch (msg) {
	case MSG_KILLVESSEL:
		if ((Body*)data == otgt) otgt = 0; // current target object was destroyed
		return 1;
	}
	return 0;
}

void Instrument_MapOld::SetSize (const Spec &spec)
{
	double cx = (double)mapx/(double)mapw;
	double cy = (double)mapy/(double)maph;

	maph = (mapw = IW) >> 1;
	if (zoom) mapw <<= 1, maph <<= 1; // 2x zoom

	mapx = (int)(cx*mapw);
	mapy = min (maph - IW/2, max (0, (int)(cy*maph)));

	//LoadMap (refplanet);
	//CalcTargetCoords();
}

void Instrument_MapOld::LoadMap (Planet *p)
{
	if (!gc) return;

	if (map)   gc->clbkReleaseSurface (map);
	if (bgmap) gc->clbkReleaseSurface (bgmap);
	bgmap = gc->clbkCreateSurfaceEx (mapw, maph, OAPISURFACE_RENDERTARGET | OAPISURFACE_SKETCHPAD);
	map   = gc->clbkCreateSurfaceEx (mapw, maph, OAPISURFACE_RENDERTARGET | OAPISURFACE_SKETCHPAD);

	if (refplanet = p) {
		DWORD i, x, y;
		int bmw, bmh;
		char cbuf[256];
		sprintf (cbuf, "%sM", p->Name());
		SURFHANDLE rawmap = LoadBitmap (cbuf, &bmw, &bmh);
		if (rawmap) {
			gc->clbkScaleBlt (bgmap, 0, 0, mapw, maph, rawmap, 0, 0, bmw, bmh);
			gc->clbkReleaseSurface (rawmap);
		} else {
			gc->clbkFillSurface (bgmap, 0);
		}
		oapi::Sketchpad *skp = gc->clbkGetSketchpad(bgmap);
		if (skp) {
			oapi::Pen *pen1 = gc->clbkCreatePen (1, 1, 0x006000);
			oapi::Pen *pen2 = gc->clbkCreatePen (1, 1, 0x0000FF);
			skp->SetPen (pen1);
			for (i = 0; i <= 6; i++) {
				y = (i*maph)/6;
				skp->Line (0, y, mapw, y);
			}
			for (i = 0; i <= 12; i++) {
				x = (i*mapw)/12;
				skp->Line (x, 0, x, maph);
			}
			// mark all surface bases
			skp->SetPen (pen2);
			for (DWORD j = 0; j < p->nBase(); j++) {
				double lng, lat;
				Base *base = p->GetBase(j);
				base->EquPos (lng, lat);
				if (lng >= Pi) lng -= Pi2;
				x = (int)(mapw * (lng+Pi)/Pi2);
				y = (int)((0.5-lat/Pi)*maph);
				skp->Rectangle (x-2, y-2, x+3, y+3);
			}
			gc->clbkReleaseSketchpad (skp);
			gc->clbkReleasePen (pen1);
			gc->clbkReleasePen (pen2);
		}
	} else {
		strcpy (title+5, "N/A");
		map = bgmap = 0;
	}
}

SURFHANDLE Instrument_MapOld::LoadBitmap (const char *cbuf, int *w, int *h)
{
	if (!gc) return NULL;

	// Load bitmap
	char *path = g_pOrbiter->TexPath (cbuf, ".bmp");
	HBITMAP hbm = (HBITMAP)LoadImage (GetModuleHandle(NULL), path, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION);
	if (!hbm)
		hbm = (HBITMAP)LoadImage (NULL, path, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE|LR_CREATEDIBSECTION);
	if (!hbm)
		return NULL;
	// Get bitmap size
	BITMAP bm;
	GetObject (hbm, sizeof(bm), &bm);
	*w = bm.bmWidth;
	*h = bm.bmHeight;
	// Create surface
	SURFHANDLE surf = gc->clbkCreateSurfaceEx (*w, *h, OAPISURFACE_RENDERTARGET);
	// Copy bitmap
	if (surf) {
		if (!gc->clbkCopyBitmap (surf, hbm, 0, 0, *w, *h)) {
			gc->clbkReleaseSurface (surf);
			surf = NULL;
		}
	}
	DeleteObject (hbm);
	return surf;
}

void Instrument_MapOld::SetZoom (bool newzoom)
{
	zoom = newzoom;
	Spec spec = { 10, 10, 0, 0, 0, 10, 0 }; // dummy
	SetSize (spec);
	needmap = true;
}

void Instrument_MapOld::ToggleTrack ()
{
	track = !track;
	if (!zoom) mapx = mapy = 0;
}

void Instrument_MapOld::CalcCoords (double lng, double lat, int &x, int &y)
{
	if (lng >= Pi) lng -= Pi2;
	x = (int)(mapw * (lng+Pi)/Pi2);
	y = (int)((0.5-lat/Pi)*maph);
}

void Instrument_MapOld::CalcTargetCoords()
{
	if (btgt) {
		double lng, lat;
		btgt->EquPos (lng, lat);
		CalcCoords (lng, lat, tgtx, tgty);
	} else {
		tgtx = tgty = -1;
	}
}

void Instrument_MapOld::CalcOrbitProj (const Elements *el, const Planet *planet, oapi::IVECTOR2 *p)
{
	const double EPS = 1e-4;
	int i, wrap=0;
	double x, y, z, lng, lat;
	double obl = planet->Obliquity();
	double rot = planet->Rotation();
	double sino = sin(obl), coso = cos(obl);
	double eqlng = planet->EqLng();
	double sine = sin(eqlng), cose= cos(eqlng);
	double f1 = (double)mapw/Pi2;
	static POINT sp[INSTRMAP_NPROJPT];
	int ilat, mapw05 = mapw/2, yofs = maph/2;
	int npt05 = INSTRMAP_NPROJPT/2;

	Matrix R (1,  0,        0,
		      0,  el->cosi, el->sini,
			  0, -el->sini, el->cosi);
	R.premul (Matrix (el->cost, 0, -el->sint,
		              0,        1,  0,
                      el->sint, 0,  el->cost));
	R.tpremul (planet->GRot());

	Vector rg, rl;

	for (i = 0; i < npt05; i++) {
		rl.Set (mul (R, Vector(cosp[i],0,sinp[i])));
		x = rl.x, y = rl.y, z = rl.z;
		lng = atan2 (z,x) + Pi;  // maps start at -Pi (180°W)
		lat = atan(y/std::hypot(x,z));
		sp[i].x = (int)(lng*f1);
		if ((sp[i+npt05].x = sp[i].x + mapw05) >= mapw) sp[i+npt05].x -= mapw;
		ilat = (int)(lat*f1);
		sp[i].y = yofs - ilat;
		sp[i+npt05].y = yofs + ilat;
	}
	for (i = 1; i < INSTRMAP_NPROJPT; i++)
		if (abs (sp[i-1].x-sp[i].x) > mapw05) { wrap = i; break; }

	// unwrap
	memcpy (p+1, sp+wrap, (INSTRMAP_NPROJPT-wrap)*sizeof(POINT));
	memcpy (p+INSTRMAP_NPROJPT+1-wrap, sp, wrap*sizeof(POINT));
	if (p[1].x < mapw05) p[0].x = 0, p[INSTRMAP_NPROJPT+1].x = mapw;
	else                 p[0].x = mapw, p[INSTRMAP_NPROJPT+1].x = 0; // retrograde
	p[0].y = p[INSTRMAP_NPROJPT+1].y =
		p[1].y - ((p[1].y-p[INSTRMAP_NPROJPT].y)*p[1].x) / (p[1].x-p[INSTRMAP_NPROJPT].x+mapw);
}

bool Instrument_MapOld::CalcIntersect (const Elements *el, const Planet *planet, double rad, oapi::IVECTOR2 *is1, oapi::IVECTOR2 *is2)
{
	if (!el->e) return false;
	double arg = (el->P()/rad - 1.0)/el->e;
	if (fabs(arg) > 1.0) return false;
	double lng, lat, r, ta = acos(arg);
	double f1 = (double)mapw/Pi2;
	int yofs = maph/2;
	Vector pos;
	el->Pol2Crt (rad, ta, pos);
	planet->GlobalToEquatorial (pos+planet->GPos(), lng, lat, r);
	is1->x = (int)((lng+Pi)*f1);
	is1->y = yofs - (int)(lat*f1);
	el->Pol2Crt (rad, -ta, pos);
	planet->GlobalToEquatorial (pos+planet->GPos(), lng, lat, r);
	is2->x = (int)((lng+Pi)*f1);
	is2->y = yofs - (int)(lat*f1);
	return true;
}

bool Instrument_MapOld::SelectTarget (const char *str)
{
	Base *base = 0;
	Body *body = g_psys->GetObj (str, true);
	if (body) {
		if (body->ElRef() == refplanet) {
			otgt = (RigidBody*)body;
		} else body = 0;
	} else {
		if (base = refplanet->GetBase (str, true)) {
			vessel->SetLandingTarget (base), btgt = base;
			CalcTargetCoords();
		}
	}
	Refresh();
	return (body || base);
}

bool Instrument_MapOld::SelectMap (char *str)
{
	Planet *planet = g_psys->GetPlanet (str, true);
	if (planet) {
		if (planet == refplanet) return true; // no change
		refplanet = planet;
		btgt = 0;
		otgt = 0;
		needmap = true;
		Refresh();
		return true;
	} else
		return false;
}

bool Instrument_MapOld::ClbkSubmn_Target (Select *menu, int item, char *str, void *data)
{
	DWORD i, j;
	Instrument_MapOld *map = (Instrument_MapOld*)data;

	if (!str) { // main menu
		menu->Append ("By name ...");
		menu->AppendSeparator ();
		menu->Append ("Spaceports", ITEM_SUBMENU | ITEM_NOHILIGHT);
		menu->Append ("Spacecraft", ITEM_SUBMENU | ITEM_NOHILIGHT);
		menu->Append ("Moons", ITEM_SUBMENU | ITEM_NOHILIGHT);
		return true;
	} else {    // submenu
		switch (item) {
		case 1: // spaceports
			for (j = 0; j < map->refplanet->nBase(); j++)
				menu->Append (map->refplanet->GetBase(j)->Name());
			return (j > 0);
		case 2: // ships
			for (i = 0, j = 0; i < g_psys->nVessel(); i++) {
				Vessel *v = g_psys->GetVessel(i);
				if (v == map->vessel || v->GetStatus() != FLIGHTSTATUS_FREEFLIGHT) continue;
				if (v->ElRef() == map->refplanet)
					menu->Append (v->Name()), j++;
			}
			return (j > 0);
		case 3: // moons
			for (i = 0, j = 0; i < g_psys->nGrav(); i++) {
				Body *body = g_psys->GetGravObj(i);
				if (body->Type() == OBJTP_PLANET && body->ElRef() == map->refplanet)
					menu->Append (body->Name()), j++;
			}
			return (j > 0);
		}
		return false;
	}
}

bool Instrument_MapOld::ClbkEnter_Target (Select *menu, int item, char *str, void *data)
{
	Instrument_MapOld *map = (Instrument_MapOld*)data;
	if (!_stricmp (str, "By name ...")) {
		g_input->Open ("Enter target:", 0, 20, Instrument_MapOld::ClbkName_Target,
			map);
		return true;
	} else
		return map->SelectTarget (str);
}

bool Instrument_MapOld::ClbkName_Target (InputBox*, char *str, void *data)
{
	Instrument_MapOld *map = (Instrument_MapOld*)data;
	return map->SelectTarget (str);
}

bool Instrument_MapOld::ClbkEnter_Map (Select *menu, int item, char *str, void *data)
{
	Instrument_MapOld *map = (Instrument_MapOld*)data;
	return map->SelectMap (str);
}

bool Instrument_MapOld::ReadParams (ifstream &ifs)
{
	char cbuf[256], cref[128] = "", cbtgt[128] = "", cotgt[128] = "", *pc;
	if (!FindScnHeader (ifs)) return false;
	zoom = false;
	track = true;

	for (;;) {
		if (!ifs.getline (cbuf, 256)) return false;
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_MFD", 7)) break;
		if (!_strnicmp (pc, "REF", 3)) {
			strcpy (cref, trim_string (pc+3));
		} else if (!_strnicmp (pc, "BTARGET", 7)) {
			strcpy (cbtgt, trim_string (pc+7));
		} else if (!_strnicmp (pc, "OTARGET", 7)) {
			strcpy (cotgt, trim_string (pc+7));
		} else if (!_strnicmp (pc, "ZOOM", 4)) {
			zoom = true;
		} else if (!_strnicmp (pc, "TRACK", 5)) {
			if (!_stricmp (trim_string (pc+5), "ON")) {
				track = true;
			} else {
				track = false;
				double cx, cy;
				int mw = IW * (zoom ? 2:1);
				sscanf (pc+5, "%lf%lf", &cx, &cy);
				mapx = (int)(cx*IW);
				mapy = (int)(cy*(IW/2));
			}
		}
	}
	SetZoom (zoom);
	if (cref[0]) SelectMap (cref);
	if (cbtgt[0]) SelectTarget (cbtgt);
	if (cotgt[0]) SelectTarget (cotgt);
	return true;
}

void Instrument_MapOld::WriteParams (ostream &ofs) const
{
	ofs << "  TYPE Map" << endl;
	if (refplanet) ofs << "  REF " << refplanet->Name() << endl;
	if (btgt) ofs << "  BTARGET " << btgt->Name() << endl;
	if (otgt) ofs << "  OTARGET " << otgt->Name() << endl;
	if (zoom) ofs << "  ZOOM" << endl;
	if (track) ofs << "  TRACK ON" << endl;
	else       ofs << "  TRACK " << (double)mapx/(double)mapw
		           << " " << (double)mapy/(double)maph << endl;
}
