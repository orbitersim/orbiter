// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "MfdMap.h"
#include "Pane.h"
#include "Psys.h"
#include "Celbody.h"
#include "Planet.h"
#include "Base.h"

using namespace std;

extern Orbiter *g_pOrbiter;
extern PlanetarySystem *g_psys;
extern Vessel *g_focusobj;
extern InputBox *g_input;
extern Select *g_select;
extern TimeData td;
extern char DBG_MSG[256];

struct Instrument_Map::SavePrm Instrument_Map::saveprm = {0,0,{0,0},1,true};
DWORD dispflag_default = DISP_GRIDLINE | DISP_COASTLINE | DISP_CONTOURS | DISP_HORIZONLINE | DISP_VESSEL | DISP_BASE | DISP_GROUNDTRACK | DISP_TERMINATOR | DISP_ORBITFOCUS | DISP_ORBITSEL;

// =======================================================================
// class Instrument_Map

Instrument_Map::Instrument_Map (Pane *_pane, INT_PTR _id, const Spec &spec, Vessel *_vessel)
: Instrument (_pane, _id, spec, _vessel)
{
	strcpy (title, "Map: ");
	zoom = 1;
	track = false;
	refplanet = NULL;
	VectorMap::OBJTYPE sel = {0,0};
	dispflag = dispflag_default;
	DWORD mkrflag = 0;
	if (_vessel == saveprm.usr) {
		refplanet = saveprm.ref;
		sel       = saveprm.sel;
		zoom      = saveprm.zoom;
		track     = saveprm.track;
		dispflag  = saveprm.dispflag;
		mkrflag   = saveprm.mkrflag;
	}
	if (!refplanet) refplanet = _vessel->ProxyPlanet();
	map = new VectorMap();
	map->SetLabelSize(max(9, IH / 40));
	map->SetCBody (refplanet);
	if (refplanet) strcpy (title+5, refplanet->Name());
	else           title[5] = '\0';
	SetSize (spec);
	map->SetCenterMode (track ? 1:0);
	map->SetSelection (sel);
	map->SetZoom (zoom);
	map->SetDisplayFlags (dispflag);
	if (mkrflag) {
		CustomMkrSet &set = map->GetCustomMarkerSet();
		for (int i = 0; mkrflag; i++) {
			if (mkrflag & 1) set.set[i].active = true;
			mkrflag >>= 1;
		}
	}
	if (!track && _vessel == saveprm.usr) {
		map->SetCenter (saveprm.lng, saveprm.lat);
	}
	disp_mode = 0;
	ndispprm = 0;
	dispprm_top = 0;
}

// =======================================================================

Instrument_Map::~Instrument_Map ()
{
	// save status
	saveprm.usr      = vessel;
	saveprm.ref      = refplanet;
	saveprm.sel      = map->GetSelection();
	saveprm.zoom     = zoom;
	saveprm.track    = track;
	saveprm.lng      = map->CntLng();
	saveprm.lat      = map->CntLat();
	saveprm.dispflag = dispflag;
	saveprm.mkrflag  = 0;
	CustomMkrSet &set = map->GetCustomMarkerSet();
	for (int i = set.nset-1; i >= 0; i--) {
		saveprm.mkrflag <<= 1;
		if (set.set[i].active) saveprm.mkrflag |= 1;
	}
	delete map;
}

// =======================================================================

HELPCONTEXT *Instrument_Map::HelpTopic () const
{
	extern HELPCONTEXT DefHelpContext;
	DefHelpContext.topic = (char*)"/mfd_map.htm";
	return &DefHelpContext;
}

// =======================================================================

int Instrument_Map::ProcessMessage (int msg, void *data)
{
	switch (msg) {
	case MSG_KILLVESSEL:
		if (data == map->GetSelection().obj)
			UnselectTarget();
		return 1;
	}
	return 0;
}

// =======================================================================

bool Instrument_Map::KeyBuffered (DWORD key)
{
	switch (disp_mode) {
	case 0: // default display
		switch (key) {
		case OAPI_KEY_R:  // select reference
			OpenSelect_CelBody ("Map MFD: Reference", ClbkEnter_Map, 1);
			return true;
		case OAPI_KEY_T:  // select target
			g_select->Open ("Map MFD: Target", ClbkSubmn_Target, ClbkEnter_Target, (void*)this);
			return true;
		case OAPI_KEY_D:  // switch to display parameters page
			disp_mode = 1;
			disp_sel = 0;
			btnpage = 0;
			RepaintButtons();
			Refresh();
			return true;
		case OAPI_KEY_K:
			ToggleTrack();
			RepaintButtons();
			return true;
		case OAPI_KEY_X:  ZoomOut(); return true;
		case OAPI_KEY_Z:  ZoomIn();  return true;
		case OAPI_KEY_LBRACKET:
		case OAPI_KEY_RBRACKET:
		case OAPI_KEY_MINUS:
		case OAPI_KEY_EQUALS:
			scroll_t0 = scroll_tp = td.SysT0;
			break;
		}
		break;
	case 1: // display parameters
		switch (key) {
		case OAPI_KEY_O:
			disp_mode = 0;
			RepaintButtons();
			Refresh();
			return true;
		case OAPI_KEY_MINUS:
			if (--disp_sel < 0) disp_sel = ndispprm-1;
			Refresh();
			return true;
		case OAPI_KEY_EQUALS:
			disp_sel = (disp_sel+1)%ndispprm;
			Refresh();
			return true;
		case OAPI_KEY_M:
			if (ToggleDispParam (disp_sel)) {
				map->SetDisplayFlags (dispflag);
				Refresh();
			}
			return true;
		}
		break;
	}
	return false;
}

// =======================================================================

bool Instrument_Map::KeyImmediate (char *kstate)
{
	double t = td.SysT0;
	double dt = max(t-scroll_t0, 0.0);
	double mag = min(dt*0.5, 8.0);
	double step = (t-scroll_tp) * mag / zoom;

	if (KEYDOWN (kstate, OAPI_KEY_LBRACKET)) { // scroll left
		if (!track && BufKey (OAPI_KEY_LBRACKET, 0.05)) {
			map->SetCenter (map->CntLng()-step, map->CntLat());
			scroll_tp = t;
			Refresh();
		}
		return true;
	}
	if (KEYDOWN (kstate, OAPI_KEY_RBRACKET)) { // scroll right
		if (!track && BufKey (OAPI_KEY_RBRACKET, 0.05)) {
			map->SetCenter (map->CntLng()+step, map->CntLat());
			scroll_tp = t;
			Refresh();
		}
		return true;
	}
	if (KEYDOWN (kstate, OAPI_KEY_MINUS)) { // scroll down
		if (!track && BufKey (OAPI_KEY_MINUS, 0.05)) {
			map->SetCenter (map->CntLng(), min (Pi, map->CntLat()+step));
			scroll_tp = t;
			Refresh();
		}
		return true;
	}
	if (KEYDOWN (kstate, OAPI_KEY_EQUALS)) { // scroll up
		if (!track && BufKey (OAPI_KEY_EQUALS, 0.05)) {
			map->SetCenter (map->CntLng(), max (-Pi, map->CntLat()-step));
			scroll_tp = t;
			Refresh();
		}
		return true;
	}
	return false;
}

// =======================================================================

bool Instrument_Map::ProcessButton (int bt, int event)
{
	switch (disp_mode) {
	case 0: {
		static const DWORD btkey[10] = { OAPI_KEY_R, OAPI_KEY_T, OAPI_KEY_X, OAPI_KEY_Z, OAPI_KEY_K, OAPI_KEY_D, OAPI_KEY_MINUS, OAPI_KEY_EQUALS, OAPI_KEY_LBRACKET, OAPI_KEY_RBRACKET };
		if (event & PANEL_MOUSE_LBDOWN) {
			if (bt < (track ? 6:10)) return KeyBuffered (btkey[bt]);
		}
		if (event & PANEL_MOUSE_LBPRESSED) {
			if (!track && bt >= 6 && bt < 10) return KeyImmediate (KstateSet (btkey[bt]));
		}
		} break;
	case 1: {
		static const DWORD btkey[4] = { OAPI_KEY_MINUS, OAPI_KEY_EQUALS, OAPI_KEY_M, OAPI_KEY_O };
		if (event & PANEL_MOUSE_LBDOWN) {
			if (bt < 4) return KeyBuffered (btkey[bt]);
		}
		} break;
	}
	return false;
}

// =======================================================================

const char *Instrument_Map::BtnLabel (int bt) const
{
	switch (disp_mode) {
	case 0: { // standard view
		static const char *label[10] = {"REF", "TGT", "ZM-", "ZM+", "TRK", "DSP", "UP", "DN", "<", ">"};
		return (bt < (track ? 6 : 10) ? label[bt] : 0);
		} break;
	case 1: { // display parameters view
		static const char *label[4] = {"UP", "DN", "MOD", "OK"};
		return (bt < 4 ? label[bt] : 0);
		} break;
	}
	return 0;
}

// =======================================================================

int Instrument_Map::BtnMenu (const MFDBUTTONMENU **menu) const
{
	switch (disp_mode) {
	case 0: {
		static const MFDBUTTONMENU mnu[10] = {
			{"Map reference", 0, 'R'},
			{"Select target", 0, 'T'},
			{"Zoom out", 0, 'X'},
			{"Zoom in", 0, 'Z'},
			{"Track mode on/off", 0, 'K'},
			{"Display parameters", 0, 'D'},
			{"Scroll up", 0, '-'},
			{"Scroll down", 0, '='},
			{"Scroll left", 0, '['},
			{"Scroll right", 0, ']'}
		};
		if (menu) *menu = mnu;
		} return (track ? 6 : 10);
	case 1: {
		static const MFDBUTTONMENU mnu[4] = {
			{"Move selection up", 0, '-'},
			{"Move selection down", 0, '='},
			{"Modify selected", "parameter", 'M'},
			{"Return to map", 0, 'O'}
		};
		if (menu) *menu = mnu;
		} return 4;
	}
	return 0;
}

// =======================================================================

void Instrument_Map::SetSize (const Spec &spec)
{
	if (!gc) return;

	oapi::Sketchpad *skp = gc->clbkGetSketchpad (surf);
	map->SetCanvas (skp, IW, IH);
	gc->clbkReleaseSketchpad (skp);
}

// =======================================================================

bool Instrument_Map::Update (double upDTscale)
{
	//map->UpdateGroundtrack();
	return Instrument::Update (upDTscale);
}

// =======================================================================

void Instrument_Map::UpdateDraw (oapi::Sketchpad *skp)
{
	switch (disp_mode) {
	case 0:
		UpdateDraw_Map (skp);
		break;
	case 1:
		UpdateDraw_Dispprm (skp);
		break;
	}
}

// =======================================================================

void Instrument_Map::UpdateDraw_Map (oapi::Sketchpad *skp)
{
	if (refplanet) {
		const SurfParam *sp = (g_focusobj->ProxyPlanet() == refplanet ? vessel->GetSurfParam() : NULL);
		char cbuf[128];
		oapi::Font *pfont = 0;
		int y = IH-ch;
		if (track) {
			skp->SetTextColor (draw[2][0].col);
			skp->Text (IW-(cw*22)/2, 0, "TRK", 3);
		}
		if (zoom > 1) {
			sprintf (cbuf, "ZM %d", zoom);
			skp->SetTextColor (draw[2][0].col);
			skp->Text (IW-(cw*13)/2, 0, cbuf, strlen(cbuf));
		}
		VectorMap::OBJTYPE sel = map->GetSelection();
		switch (sel.type) {
		case DISP_MOON:
		case DISP_VESSEL: {
			const Body *b = (const Body*)sel.obj;
			double lng, lat, rad;
			refplanet->GlobalToEquatorial (b->GPos(), lng, lat, rad);
			sprintf (cbuf, "%s [%6.2fº%c %6.2fº%c, Alt %s]",
				b->Name(), fabs(lng)*DEG, lng>=0.0 ? 'E':'W', fabs(lat)*DEG,
				lat>=0.0 ? 'N':'S', DistStr(rad-refplanet->Size()));
			skp->SetTextColor (draw[1][0].col);
			if (!pfont) pfont = skp->SetFont (GetDefaultFont (1));
			skp->Text (cw/2, y, "TGT:", 4);
			skp->Text (cw*4, y, cbuf, strlen(cbuf));
			y -= ch;
			} break;
		case DISP_BASE: {
			const Base *base = (const Base*)sel.obj;
			double lng, lat, lng0, lat0, rad, adist, hdg;
			base->EquPos (lng, lat);
			sprintf (cbuf, "%s [%6.2fº%c %6.2fº%c]",
				base->Name(), fabs(lng)*DEG, lng>=0.0 ? 'E':'W', fabs(lat)*DEG,
				lat>=0.0 ? 'N':'S');
			if (sp) lng0 = sp->lng, lat0 = sp->lat;
			else    refplanet->GlobalToEquatorial (g_focusobj->GPos(), lng0, lat0, rad);
			rad = refplanet->Size();
			Orthodome (lng0, lat0, lng, lat, adist, hdg);
			sprintf (cbuf+strlen(cbuf)-1, ", Dst%s, Brg %05.1fº]", DistStr(adist*rad), Deg(posangle(hdg)));
			skp->SetTextColor (draw[2][0].col);
			if (!pfont) pfont = skp->SetFont (GetDefaultFont (1));
			skp->Text (cw/2, y, "BSE:", 4);
			skp->Text (cw*4, y, cbuf, strlen(cbuf));
			y -= ch;
			} break;
		}
		double lng, lat, rad;
		refplanet->GlobalToEquatorial (g_focusobj->GPos(), lng, lat, rad);
		sprintf (cbuf, "%s [%6.2fº%c %6.2fº%c, Alt %s]",
			g_focusobj->Name(), fabs(lng)*DEG, lng>=0.0 ? 'E':'W', fabs(lat)*DEG,
			lat>=0.0 ? 'N':'S', DistStr(rad-refplanet->Size()));
		if (sp) {
			Vector hvel (tmul (sp->ref->GRot(), sp->groundvel_glob));
			hvel.Set (mul (sp->L2H, hvel));
			double crs = atan2 (hvel.x, hvel.z);
			sprintf (cbuf+strlen(cbuf)-1, ", Crs %05.1fº]", Deg(posangle(crs)));
		}
		skp->SetTextColor (draw[0][0].col);
		if (!pfont) pfont = skp->SetFont (GetDefaultFont (1));
		skp->Text (cw/2, y, "SHP:", 4);
		skp->Text (cw*4, y, cbuf, strlen(cbuf));
		y -= ch;
		if (pfont) skp->SetFont (pfont);	
	}
	DisplayTitle (skp, title);
}

// =======================================================================

void Instrument_Map::UpdateDraw_Dispprm (oapi::Sketchpad *skp)
{
	const int ndefault = 9; // default items
	const int nlist = 16;   // max number of items displayed
	int i, idx = 0, x0 = (cw*5)/2, x1 = cw*25, y = ch*3, dy = ch;

	int scl = IW/60, xcnt=cw*4, ycnt=ch*(7+nlist*2)/2;
	oapi::IVECTOR2 arrowdn[8] = {
		{-scl+xcnt,0+ycnt},
		{-scl+xcnt,scl+ycnt},
		{-2*scl+xcnt,scl+ycnt},
		{0+xcnt,2*scl+ycnt},
		{2*scl+xcnt,scl+ycnt},
		{scl+xcnt,scl+ycnt},
		{scl+xcnt,0+ycnt},
		{-scl+xcnt,0+ycnt}
	};
	ycnt = (ch*5)/2;
	oapi::IVECTOR2 arrowup[8] = {
		{-scl+xcnt,0+ycnt},
		{-scl+xcnt,-scl+ycnt},
		{-2*scl+xcnt,-scl+ycnt},
		{0+xcnt,-2*scl+ycnt},
		{2*scl+xcnt,-scl+ycnt},
		{scl+xcnt,-scl+ycnt},
		{scl+xcnt,0+ycnt},
		{-scl+xcnt,0+ycnt}
	};

	if      (disp_sel < dispprm_top)        dispprm_top = disp_sel;
	else if (disp_sel >= dispprm_top+nlist) dispprm_top = disp_sel-nlist+1;

	DisplayTitle (skp, "Map: Display parameters");
	oapi::Font *pfont = skp->SetFont (GetDefaultFont (1));
	skp->SetTextColor (draw[2][0].col);

	if (dispprm_top <= idx++) {
		skp->Text (x0, y, "Orbit lines", 11);
		skp->Text (x1, y, dispflag & DISP_GROUNDTRACK ? "Groundtrack" : dispflag & DISP_ORBITPLANE ? "Orbit plane" : "OFF        ", 11);
		y += dy;
	}

	if (dispprm_top <= idx++) {
		skp->Text (x0, y, "Horizon lines", 13);
		skp->Text (x1, y, dispflag & DISP_HORIZONLINE ? "ON ":"OFF", 3);
		y += dy;
	}

	if (dispprm_top <= idx++) {
		skp->Text (x0, y, "Terminator", 10);
		switch (dispflag & DISP_TERMINATOR) {
			case DISP_TERMINATOR_NONE: skp->Text (x1, y, "OFF", 3); break;
			case DISP_TERMINATOR_LINE: skp->Text (x1, y, "Line", 4); break;
			case DISP_TERMINATOR_SHADE: skp->Text (x1, y, "Shading", 7); break;
			case DISP_TERMINATOR_BOTH: skp->Text (x1, y, "Line+shading", 12); break;
		}
		y += dy;
	}

	if (dispprm_top <= idx++) {
		skp->Text (x0, y, "Grid lines", 10);
		skp->Text (x1, y, dispflag & DISP_GRIDLINE ? "ON ":"OFF", 3);
		y += dy;
	}

	if (dispprm_top <= idx++) {
		skp->Text (x0, y, "Coastlines", 10);
		skp->Text (x1, y, dispflag & DISP_COASTLINE ? "ON ":"OFF", 3);
		y += dy;
	}

	if (dispprm_top <= idx++) {
		skp->Text (x0, y, "Contour lines", 13);
		skp->Text (x1, y, dispflag & DISP_CONTOURS ? "ON ":"OFF", 3);
		y += dy;
	}

	if (dispprm_top <= idx++) {
		skp->Text (x0, y, "Base markers", 12);
		skp->Text (x1, y, dispflag & DISP_BASE ? "ON ":"OFF", 3);
		y += dy;
	}

	if (dispprm_top <= idx++) {
		skp->Text (x0, y, "Navaid markers", 14);
		skp->Text (x1, y, dispflag & DISP_NAVAID ? "ON ":"OFF", 3);
		y += dy;
	}

	if (dispprm_top <= idx++) {
		skp->Text (x0, y, "Natural satellites", 18);
		skp->Text (x1, y, dispflag & DISP_MOON ? "ON ":"OFF", 3);
		y += dy;
	}

	CustomMkrSet &set = map->GetCustomMarkerSet();
	for (i = max(0,dispprm_top-ndefault); i < min((int)set.nset,nlist-ndefault+dispprm_top); i++) {
		std::string& name = set.set[i].list->name;
		bool active = set.set[i].active;
		skp->Text (x0, y, name.c_str(), name.size());
		skp->Text (x1, y, active ? "ON ":"OFF", 3);
		y += dy;
	}
	ndispprm = ndefault+set.nset;

	skp->Rectangle (cw*2, (disp_sel-dispprm_top)*dy+ch*3, IW-2*cw, (disp_sel-dispprm_top+1)*dy+ch*3);
	if (dispprm_top) skp->Polyline (arrowup, 8);
	if (ndispprm-dispprm_top > nlist) skp->Polyline (arrowdn, 8);
	skp->SetFont (pfont);
}

// =======================================================================

void Instrument_Map::UpdateBlt ()
{
	if (disp_mode) return;
	if (!gc) return;
	map->Update ();
	map->DrawMap ();
	oapiBlt (surf, map->GetMap(), 0, 0, 0, 0, IW, IH);
}

// =======================================================================

bool Instrument_Map::SelectMap (char *str)
{
	Planet *planet = g_psys->GetPlanet (str, true);
	if (planet) {
		if (planet == refplanet) return true; // no change
		refplanet = planet;
		map->SetCBody (refplanet);
		if (refplanet) strcpy (title+5, refplanet->Name());
		else           title[5] = '\0';
		Refresh();
		return true;
	} else
		return false;
}

// =======================================================================

bool Instrument_Map::SelectTarget (char *str)
{
	const Body *body = g_psys->GetObj (str, true);
	if (!body) {
		body = refplanet->GetBase (str, true);
		if (body) vessel->SetLandingTarget((Base*)body);
	}
	if (body) {
		VectorMap::OBJTYPE sel;
		sel.obj = body;
		switch (body->Type()) {
		case OBJTP_VESSEL: sel.type = DISP_VESSEL; break;
		case OBJTP_SURFBASE: sel.type = DISP_BASE; break;
		case OBJTP_PLANET: sel.type = DISP_MOON; break;
		default: sel.type = 0; break;
		}
		map->SetSelection (sel);
		return true;
	}
	return false;
}

// =======================================================================

bool Instrument_Map::UnselectTarget ()
{
	map->UnsetSelection ();
	Refresh();
	return true;
}

// =======================================================================

void Instrument_Map::ZoomIn ()
{
	if (zoom < 128) {
		zoom = zoom*2;
		map->SetZoom (zoom);
		Refresh();
	}
}

// =======================================================================

void Instrument_Map::ZoomOut ()
{
	if (zoom > 1) {
		zoom = zoom/2;
		map->SetZoom (zoom);
		Refresh();
	}
}

// =======================================================================

void Instrument_Map::ToggleTrack ()
{
	track = !track;
	map->SetCenterMode (track ? 1:0);
	Refresh();
}

// =======================================================================

bool Instrument_Map::ToggleDispParam (int which)
{
	switch (which) {
	case 0:
		if (dispflag & DISP_GROUNDTRACK)
			dispflag = (dispflag & ~DISP_GROUNDTRACK) | DISP_ORBITPLANE;
		else if (dispflag & DISP_ORBITPLANE)
			dispflag = (dispflag & ~DISP_ORBITPLANE);
		else
			dispflag = dispflag | DISP_GROUNDTRACK;
		return true;
	case 1:
		dispflag = dispflag ^ DISP_HORIZONLINE;
		return true;
	case 2:
		dispflag = (((dispflag & DISP_TERMINATOR) + 0x4000) % (DISP_TERMINATOR+0x4000)) | (dispflag & ~DISP_TERMINATOR);
		return true;
	case 3:
		dispflag = dispflag ^ DISP_GRIDLINE;
		return true;
	case 4:
		dispflag = dispflag ^ DISP_COASTLINE;
		return true;
	case 5:
		dispflag = dispflag ^ DISP_CONTOURS;
		return true;
	case 6:
		dispflag = dispflag ^ DISP_BASE;
		return true;
	case 7:
		dispflag = dispflag ^ DISP_NAVAID;
		return true;
	case 8:
		dispflag = dispflag ^ DISP_MOON;
		return true;
	default:
		if (which-9 < map->GetCustomMarkerSet().nset) {
			CustomMkrSet &cms = map->GetCustomMarkerSet();
			cms.set[which-9].active = !cms.set[which-9].active;
			int i;
			for (i = 0; i < cms.nset; i++)
				if (cms.set[i].active) break;
			if (i < cms.nset) dispflag = dispflag | DISP_CUSTOMMARKER;
			else              dispflag = dispflag & ~DISP_CUSTOMMARKER;
		}
		return true;
	}
	return false;
}

// =======================================================================

bool Instrument_Map::ReadParams (std::ifstream &ifs)
{
	char cbuf[256], cref[128] = "", ctgt[128] = "", *pc;
	char otgt[128] = "", btgt[128] = "";
	zoom = 1;
	double lng, lat;
	DWORD dflag = dispflag;
	track = true;

	for (;;) {
		if (!ifs.getline (cbuf, 256)) return false;
		pc = trim_string (cbuf);
		if (!_strnicmp (pc, "END_MFD", 7)) break;
		if (!_strnicmp (pc, "REF", 3)) {
			strcpy (cref, trim_string (pc+3));
		} else if (!_strnicmp (pc, "TARGET", 6)) {
			strcpy (ctgt, trim_string (pc+6));
		} else if (!_strnicmp (pc, "OTARGET", 7)) { // backward compatibility
			strcpy (otgt, trim_string (pc+7));
		} else if (!_strnicmp (pc, "BTARGET", 7)) { // backward compatibility
			strcpy (btgt, trim_string (pc+7));
		} else if (!_strnicmp (pc, "ZOOM", 4)) {
			int res = sscanf (trim_string(pc+4), "%d", &zoom);
			if (res < 1) zoom = 2;
		} else if (!_strnicmp (pc, "POS", 3)) {
			int res = sscanf (pc+4, "%lf%lf", &lng, &lat);
			if (res == 2) track = false;
		} else if (!_strnicmp (pc, "DISP", 4)) {
			sscanf (trim_string(pc+4), "%d", &dflag);
		}
	}
	if (cref[0]) SelectMap (cref);
	if (!ctgt[0]) strcpy (ctgt, otgt[0] ? otgt : btgt);
	if (ctgt[0]) SelectTarget (ctgt);
	map->SetZoom (zoom);
	map->SetCenterMode (track ? 1:0);
	if (dflag != dispflag) map->SetDisplayFlags (dispflag = dflag);
	if (!track)
		map->SetCenter (lng, lat);

	return true;
}

// =======================================================================

void Instrument_Map::WriteParams (std::ostream &ofs) const
{
	ofs << "  TYPE Map" << endl;
	if (refplanet) ofs << "  REF " << refplanet->Name() << endl;
	const VectorMap::OBJTYPE sel = map->GetSelection();
	if (sel.type == DISP_VESSEL || sel.type == DISP_BASE || sel.type == DISP_MOON) {
		const Body *body = (const Body*)sel.obj;
		ofs << "  TARGET " << body->Name() << endl;
	}
	if (zoom > 1)  ofs << "  ZOOM " << zoom << endl;
	if (!track)    ofs << "  POS " << map->CntLng() << ' ' << map->CntLat() << endl;
	if (dispflag != dispflag_default)
		           ofs << "  DISP " << dispflag << endl;
}

// =======================================================================

bool Instrument_Map::ClbkEnter_Map (Select *menu, int item, char *str, void *data)
{
	Instrument_Map *map = (Instrument_Map*)data;
	return map->SelectMap (str);
}

// =======================================================================

bool Instrument_Map::ClbkSubmn_Target (Select *menu, int item, char *str, void *data)
{
	DWORD i, j;
	Instrument_Map *map = (Instrument_Map*)data;

	if (!str) { // main menu
		menu->Append ("By name ...");
		menu->AppendSeparator ();
		menu->Append ("Spaceports", ITEM_SUBMENU | ITEM_NOHILIGHT);
		menu->Append ("Spacecraft", ITEM_SUBMENU | ITEM_NOHILIGHT);
		menu->Append ("Moons", ITEM_SUBMENU | ITEM_NOHILIGHT);
		menu->Append ("[none]");
		return true;
	} else {    // submenu
		switch (item) {
		case 1: // spaceports
			for (i = 0; i < map->refplanet->nBase(); i++)
				menu->Append (map->refplanet->GetBase(i)->Name());
			return (i > 0);
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

// =======================================================================

bool Instrument_Map::ClbkEnter_Target (Select *menu, int item, char *str, void *data)
{
	Instrument_Map *map = (Instrument_Map*)data;
	if (!_stricmp (str, "By name ...")) {
		g_input->Open ("Enter target:", 0, 20, Instrument_Map::ClbkName_Target,
			map);
		return true;
	} else if (!_stricmp (str, "[none]")) {
		map->UnselectTarget ();
		return true;
	} else
		return map->SelectTarget (str);
}

// =======================================================================

bool Instrument_Map::ClbkName_Target (InputBox*, char *str, void *data)
{
	Instrument_Map *map = (Instrument_Map*)data;
	return map->SelectTarget (str);
}

