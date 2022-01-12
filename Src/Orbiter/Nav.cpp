// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// class Nav
// Navigation signal transmitter
// =======================================================================

#include <stdio.h>
#include "Config.h"
#include "Nav.h"
#include "Planet.h"
#include "Base.h"
#include "Orbitersdk.h"
#include "Log.h"

using namespace std;

// =======================================================================
// standalone methods

Nav *ParseNav (const char *line, const Planet *planet)
{
	char typestr[32];
	sscanf (line, "%s", typestr);
	if (!_stricmp (typestr, "VOR") && planet) {
		TRACENEW; return new Nav_VOR (planet, line+3);
	}
	return NULL;
}

// =======================================================================
// class Nav

Nav::Nav (float _freq, float _range)
{
	range = _range;
	memset (id, '\0', 8);
	SetFreq (_freq);
}

void Nav::SetFreq (float _freq)
{
	freq = _freq;
	step = (DWORD)((freq - NAV_RADIO_FREQ_MIN)*20.0 + 0.5);
}

void Nav::SetStep (DWORD _step)
{
	step = _step;
	freq = (float)(step*0.05 + NAV_RADIO_FREQ_MIN);
}

int Nav::IdString (char *str, int len) const
{
	if (len > 0) str[0] = '\0';
	return 0;
}

double Nav::Dist (const Vector &gpos) const
{
	Vector gp;
	GPos (gp);
	return gp.dist (gpos);
}

double Nav::FieldStrength (const Vector &gpos) const
{
	// field strength in arbitrary units
	Vector gp;
	GPos (gp);
	double dist2 = max(gp.dist2(gpos), 1.0);
	return (range*range)/dist2;
}

bool Nav::InRange (const Vector &gpos) const
{
	// Note "InRange" corresponds to FieldStrength > 1

	Vector gp;
	GPos (gp);
	return gp.dist2 (gpos) < range*range;
}

void Nav::GetData (NAVDATA *data) const
{
	data->type  = Type();
	data->ch    = step;
	double rng  = (double)range;
	data->power = rng*rng;
	data->descr = id;
}

// =======================================================================
// class Nav_VOR
// surface-based omnidirectional transmitter

Nav_VOR::Nav_VOR (const Planet *_planet, double _lng, double _lat)
: Nav()
{
	planet = _planet;
	lng = _lng;
	lat = _lat;
	planet->EquatorialToLocal (lng, lat, planet->Size() + planet->Elevation (lng, lat), lpos);
}

Nav_VOR::Nav_VOR (const Planet *_planet, double _lng, double _lat, float _freq, float _range)
: Nav (_freq, _range)
{
	planet = _planet;
	lng = _lng;
	lat = _lat;
	planet->EquatorialToLocal (lng, lat, planet->Size() + planet->Elevation (lng, lat), lpos);
}

Nav_VOR::Nav_VOR (const Planet *_planet, const char *str)
: Nav()
{
	planet = _planet;
	sscanf (str, "%s%lf%lf%f%f", id, &lng, &lat, &freq, &range);
	lng *= RAD, lat *= RAD, range *= 1e3;
	step = (DWORD)((freq - NAV_RADIO_FREQ_MIN)*20.0 + 0.5);
	planet->EquatorialToLocal (lng, lat, planet->Size() + planet->Elevation (lng, lat), lpos);
}

int Nav_VOR::IdString (char *str, int len) const
{
	return _snprintf (str, len, "VOR %s", GetId());
}

void Nav_VOR::GPos (Vector &gp) const
{
	gp.Set (mul (planet->GRot(), lpos) + planet->GPos());
	//planet->EquatorialToGlobal (lng, lat, planet->Size(), gp);
}

void Nav_VOR::GetData (NAVDATA *data) const
{
	Nav::GetData (data);
	data->vor.hPlanet = (OBJHANDLE)planet;
	data->vor.lng = lng;
	data->vor.lat = lat;
}

// =======================================================================
// class Nav_VTOL
// transmitter for vertical takeoff/landing support

Nav_VTOL::Nav_VTOL (const Base *_base, int _npad, double _lng, double _lat, float _freq, float _range)
: Nav_VOR (_base->RefPlanet(), _lng, _lat, _freq, _range)
{
	base  = _base;
	npad  = _npad;
}

int Nav_VTOL::IdString (char *str, int len) const
{
	return _snprintf (str, len, "VTOL Pad-%02d %s", GetPad()+1, GetBase()->Name());
}

void Nav_VTOL::GetData (NAVDATA *data) const
{
	Nav::GetData (data);
	data->vtol.hBase = (OBJHANDLE)base;
	data->vtol.npad = npad;
}

// =======================================================================
// class Nav_ILS
// Instrument landing system: runway instrument landing

Nav_ILS::Nav_ILS (const Base *_base, double _dir, double _lng, double _lat, float _freq, float _range)
: Nav_VOR (_base->RefPlanet(), _lng, _lat, _freq, _range)
{
	base = _base;
	dir = _dir;
}

int Nav_ILS::IdString (char *str, int len) const
{
	return _snprintf (str, len, "ILS Rwy %02d %s", (int)(ApprDir()*DEG*0.1+0.5), GetBase()->Name());
}

void Nav_ILS::GetData (NAVDATA *data) const
{
	Nav::GetData (data);
	data->ils.hBase = (OBJHANDLE)base;
	data->ils.appdir = dir;
}

// =======================================================================
// class Nav_IDS
// Instrument docking system: vessel-mounted transmitter for docking approach

Nav_IDS::Nav_IDS (const Vessel *_vessel, const PortSpec *_ps, float _freq, float _range)
: Nav (_freq, _range)
{
	vessel = _vessel;
	ps = _ps;
}

int Nav_IDS::IdString (char *str, int len) const
{
	DWORD i;
	for (i = 0; i < vessel->nDock(); i++)
		if (vessel->GetDockParams(i) == ps) break;
	if (i < vessel->nDock())
		return _snprintf (str, len, "IDS D-%02d %s", i+1, vessel->Name());
	else // should not happen
		return _snprintf (str, len, "IDS %s", vessel->Name());
}

void Nav_IDS::GPos (Vector &gp) const
{
	gp.Set (vessel->GetDockGPos (ps));
}

void Nav_IDS::GetData (NAVDATA *data) const
{
	Nav::GetData (data);
	data->ids.hVessel = (OBJHANDLE)vessel;
	data->ids.hDock = (DOCKHANDLE)ps;
}

// =======================================================================
// class Nav_XPDR
// Transponder (vessel-mounted)

Nav_XPDR::Nav_XPDR (const Vessel *_vessel, float _freq, float _range)
: Nav (_freq, _range)
{
	vessel = _vessel;
}

int Nav_XPDR::IdString (char *str, int len) const
{
	return _snprintf (str, len, "XPDR %s", vessel->Name());
}

void Nav_XPDR::GetData (NAVDATA *data) const
{
	Nav::GetData (data);
	data->xpdr.hVessel = (OBJHANDLE)vessel;
}

// =======================================================================
// class NavManager
// maintains a list of transmitters

NavManager::NavManager ()
{
	nnav = nbuf = 0;
}

NavManager::~NavManager ()
{
	Clear ();
}

void NavManager::Clear ()
{
	if (nbuf) {
		for (DWORD i = 0; i < nnav; i++) {
			delete []nav[i];
			nav[i] = NULL;
		}
		delete []nav;
		nav = NULL;
		nbuf = nnav = 0;
	}
}

void NavManager::AddNav (Nav *_nav)
{
	if (nbuf == nnav) { // grow buffer
		Nav **tmp = new Nav*[nbuf += 16]; TRACENEW
		if (nnav) {
			memcpy (tmp, nav, nnav*sizeof(Nav*));
			delete []nav;
		}
		nav = tmp;
	}
	// sort new entry in
	DWORD i;
	for (i = nnav; i > 0; i--) {
		if (nav[i-1]->freq > _nav->freq) nav[i] = nav[i-1];
		else break;
	}
	nav[i] = _nav;
	nnav++;
}

DWORD NavManager::Read (ifstream &ifs, const Planet *planet, bool append)
{
	if (nnav && !append) Clear ();

	if (FindLine (ifs, "BEGIN_NAVBEACON")) {
		char cbuf[256];
		for (;;) {
			if (!ifs.getline (cbuf, 256) || !_strnicmp (cbuf, "END_NAVBEACON", 13)) break;
			Nav *nv = ParseNav (cbuf, planet);
			if (nv) AddNav (nv);
		}
	}
	return nnav;
}
