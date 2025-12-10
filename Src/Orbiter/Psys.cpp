// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Implementation of class PlanetarySystem

#include <fstream>
#include <stdio.h>
#include <string.h>
#include <algorithm>

#include "Config.h"
#include "Psys.h"
#include "TimeData.h"
#include "Element.h"
#include "Vessel.h"
#include "SuperVessel.h"
#include "Log.h"

using namespace std;

extern TimeData td;
extern bool g_bForceUpdate;
extern char DBG_MSG[256];

PlanetarySystem::PlanetarySystem (char *fname, const Config* config, OutputLoadStatusCallback outputLoadStatus, void* callbackContext)
{
	Read (fname, config, outputLoadStatus, callbackContext);
}

PlanetarySystem::~PlanetarySystem ()
{
	Clear ();
}

void PlanetarySystem::Clear ()
{
	DestroyDeviceObjects ();
	m_Name.clear();

	//Vessel destructor broadcasts messages to every other vessel in 'vessels'.
	//We remove it from the collection as soon as we deleted it to prevent the next Vessel to broadcast to the free'd one.
	while (vessels.size()) {
		DelBody(vessels.back());
		vessels.pop_back();
	}

	//Every element in vessels, stars, planets and celestials is also present in bodies.
	//By deallocating all bodies we can then just clear all the other collections.
	//NOTE: vessels are a special case! See comment above.
	for (size_t i = 0; i < bodies.size(); ++i)
		delete bodies[i];

	bodies    .clear();
	stars     .clear();
	planets   .clear();
	celestials.clear();

	g_bForceUpdate = true;

	//Supervessels are not stored in 'bodies' so we have to take care of them manually
	for (size_t i = 0; i < supervessels.size(); ++i) {
		delete supervessels[i];
	}
	supervessels.clear();
}

void PlanetarySystem::InitState (const char *fname)
{
	char cbuf[256], *pc, *pd;
	ifstream ifs (fname);
	if (!ifs) return;
	if (FindLine (ifs, "BEGIN_SHIPS")) {
		for (;;) {
			if (!ifs.getline (cbuf, 256)) break;
			pc = trim_string (cbuf);
			if (!_stricmp (pc, "END_SHIPS")) break;
			for (pd = pc; *pd != '\0' && *pd != ':'; pd++);
			if (*pd) *pd++ = '\0';
			else pd = 0;
			AddVessel (new Vessel (this, pc, pd, ifs)); TRACENEW
		}
	}
}

void PlanetarySystem::PostCreation ()
{
	for (size_t i = 0; i < vessels.size(); ++i) vessels[i]->PostCreation();
	for (size_t i = 0; i < vessels.size(); ++i) vessels[i]->InitSupervessel();
	for (size_t i = 0; i < vessels.size(); ++i) vessels[i]->ModulePostCreation();
}

void PlanetarySystem::Write (ostream &os)
{
	os << "BEGIN_SHIPS" << endl;
	for (size_t i = 0; i < vessels.size(); ++i)
		vessels[i]->Write (os);
	os << "END_SHIPS" << endl;
}

Body *PlanetarySystem::GetObj (const char *name, bool ignorecase)
{
	for (DWORD i = 0; i < bodies.size(); i++)
		if (!StrComp (bodies[i]->Name(), name, ignorecase)) return bodies[i];
	return 0;
}

CelestialBody *PlanetarySystem::GetGravObj (const char *name, bool ignorecase) const
{
	for (DWORD i = 0; i < celestials.size(); i++)
		if (!StrComp (celestials[i]->Name(), name, ignorecase)) return celestials[i];
	return 0;
}

Planet *PlanetarySystem::GetPlanet (const char *name, bool ignorecase)
{
	for (size_t i = 0; i < planets.size(); i++)
		if (!StrComp (planets[i]->Name(), name, ignorecase)) return planets[i];
	return 0;
}

Vessel *PlanetarySystem::GetVessel (const char *name, bool ignorecase) const
{
	for (DWORD i = 0; i < vessels.size(); i++)
		if (!StrComp (vessels[i]->Name(), name, ignorecase)) return vessels[i];
	return 0;
}

bool PlanetarySystem::isObject (const Body *obj) const
{
	for (DWORD i = 0; i < bodies.size(); i++)
		if (bodies[i] == obj) return true;
	return false;
}

bool PlanetarySystem::isVessel (const Vessel *v) const
{
	for (DWORD i = 0; i < vessels.size(); i++)
		if (vessels[i] == v) return true;
	return false;
}

Base *PlanetarySystem::GetBase (const Planet *planet, const char *name, bool ignorecase)
{
	for (DWORD i = 0; i < planet->nBase(); i++)
		if (!StrComp (planet->GetBase(i)->Name(), name, ignorecase))
			return planet->GetBase(i);
	return 0;
}

Base *PlanetarySystem::GetBase (const char *name, bool ignorecase)
{
	for (DWORD i = 0; i < celestials.size(); i++) {
		if (celestials[i]->Type() != OBJTP_PLANET) continue;
		Planet *planet = (Planet*)celestials[i];
		Base *base = GetBase (planet, name, ignorecase);
		if (base) return base;
	}
	return 0;
}

void PlanetarySystem::OptionChanged(DWORD cat, DWORD item)
{
	for (size_t i = 0; i < vessels.size(); i++)
		vessels[i]->OptionChanged(cat, item);
}

bool PlanetarySystem::Read (char *fname, const Config* config, OutputLoadStatusCallback outputLoadStatus, void* callbackContext)
{
	int i;
	DWORD j;
	char cbuf[256], label[128];
	
	ifstream ifs (config->ConfigPath(fname));
	if (!ifs) return false;
	Clear();
	if (GetItemString (ifs, "Name", cbuf)) {
		m_Name = cbuf;
	}

	// read stars
	for (;;) {
		sprintf (label, "Star%zd", stars.size() + 1);
		if (!GetItemString (ifs, label, cbuf)) break;
		OutputLoadStatus (cbuf, outputLoadStatus, callbackContext);
		AddStar (new Star (cbuf)); TRACENEW
	}

	// read planets
	for (i = 0;; i++) {
		sprintf (label, "Planet%d", i+1);
		if (!GetItemString (ifs, label, cbuf)) break;
		OutputLoadStatus (cbuf, outputLoadStatus, callbackContext);
		Planet *planet = new Planet (cbuf); TRACENEW
		AddPlanet (planet, stars[0]);
		ScanMoons (ifs, planet, planet->Name());
	}

	for (j = 0; j < stars.size(); j++) stars[j]->RelTrueAndBaryState();
	for (j = 0; j < stars.size(); j++) stars[j]->AbsTrueState();

	if (GetItemString (ifs, "MarkerPath", cbuf)) {
		m_labelPath = cbuf;
		if (m_labelPath.back() != '\\')
			m_labelPath.push_back('\\');
	}
	else {
		m_labelPath = std::string(config->CfgDirPrm.ConfigDir) + m_Name + std::string("\\Marker\\");
	}
	m_labelList.clear();
	ScanLabelLists(ifs, true);

	return true;
}

void PlanetarySystem::OutputLoadStatus (const char *bname, OutputLoadStatusCallback outputLoadStatus, void* callbackContext)
{
	char cbuf[256];
	sprintf (cbuf, "%s: %s", m_Name.c_str(), bname);
	outputLoadStatus(cbuf, 0, callbackContext);
}

void PlanetarySystem::ScanLabelLists (ifstream &cfg, bool bScanHeaders)
{
	int i;
	char cbuf[256];
	oapi::GraphicsClient::LABELLIST* ll;
	int idx = 0;
	ForEach(FILETYPE_MARKER, [&](const fs::directory_entry& entry) {
		// open marker file
		ifstream ulf(entry.path());
		// read label header
		if (bScanHeaders) {
			oapi::GraphicsClient::LABELLIST list;
			//list.marker.clear();
			list.colour = 1;
			list.shape = 0;
			list.size = 1.0f;
			list.distfac = 1.0f;
			list.active = false;
			list.flag = 0;
			list.name = entry.path().stem().string();
			if (FindLine(ulf, "BEGIN_HEADER")) {
				char item[256], value[256];
				for (;;) {
					if (!ulf.getline(cbuf, 256) || !_strnicmp(cbuf, "END_HEADER", 10)) break;
					sscanf(cbuf, "%s %s", item, value);
					if (!_stricmp(item, "InitialState")) {
						if (!_stricmp(value, "on")) list.active = true;
					}
					else if (!_stricmp(item, "ColourIdx")) {
						int col;
						sscanf(value, "%d", &col);
						list.colour = max(0, min(5, col));
					}
					else if (!_stricmp(item, "ShapeIdx")) {
						int shape;
						sscanf(value, "%d", &shape);
						list.shape = max(0, min(6, shape));
					}
					else if (!_stricmp(item, "Size")) {
						float size;
						sscanf(value, "%f", &size);
						list.size = max(0.1f, min(2.0f, size));
					}
					else if (!_stricmp(item, "DistanceFactor")) {
						float distfac;
						sscanf(value, "%f", &distfac);
						list.distfac = max(1e-5f, min(1e3f, distfac));
					}
					else if (!_stricmp(item, "Frame")) {
						if (_stricmp(value, "Ecliptic"))
							list.flag = 1; // flag for celestial position data
					}
				}
			}
			m_labelList.push_back(list);
			ll = &m_labelList.back();
		}
		else {
			ll = &m_labelList[idx++];
		}

		// check if positions are in celestial or ecliptic frame
		bool celestialpos = ((ll->flag & 1) != 0);

		// read label list for active labels, if not already present
		if (ll->active && !ll->marker.size()) {
			double lng, lat;
			int nl;
			char* pc;
			FindLine(ulf, "BEGIN_DATA");
			for (nl = 0;; nl++) {
				if (!ulf.getline(cbuf, 256)) break;
				pc = strtok(cbuf, ":");
				if (!pc || sscanf(pc, "%lf%lf", &lng, &lat) != 2) continue;
				lng = Rad(lng);
				lat = Rad(lat);
				if (celestialpos) {
					static double eps = 0.4092797095927;
					static double coseps = cos(eps), sineps = sin(eps);
					double ra = lng, dc = lat;
					Equ2Ecl(coseps, sineps, ra, dc, lng, lat);
				}
				oapi::GraphicsClient::LABELSPEC ls;
				double xz = cos(lat);
				ls.pos.y = sin(lat);
				ls.pos.x = xz * cos(lng);
				ls.pos.z = xz * sin(lng);
				for (i = 0; i < 2; i++) {
					if (pc = strtok(NULL, ":")) {
						ls.label[i] = trim_string(pc);
					}
				}
				ll->marker.push_back(ls);
			}
		}
	});
}

void PlanetarySystem::ActivatePlanetLabels(bool activate)
{
	for (size_t i = 0; i < planets.size(); i++)
		planets[i]->ActivateLabels(activate);
}

void PlanetarySystem::AddBody (Body *_body)
{
	bodies.emplace_back(_body);
}

bool PlanetarySystem::DelBody (Body *_body)
{
	size_t i;
	for (i = 0; i < bodies.size(); i++)
		if (bodies[i] == _body)
			break;

	if (i == bodies.size())
		return false; // bodies not found in list

	//if (bodies[i]->s0) bodies[i]->s0 = NULL; // s0 is used in vessels destructor due to undocking of
	//if (bodies[i]->s1) bodies[i]->s1 = NULL; // vessels before deletion.
	
	delete bodies[i]; // delete actual bodies/vessels

	std::iter_swap(bodies.begin() + i, bodies.end() - 1);
	bodies.pop_back();

	return true;
}

size_t PlanetarySystem::AddStar (Star *_star)
{
	stars.emplace_back(_star);
	AddGrav (_star); // register in list of massive objects
	AddBody (_star); // register in general list
	return stars.size();
}

size_t PlanetarySystem::AddPlanet (Planet *_planet, CelestialBody *cbody)
{
	planets.emplace_back(_planet);
	AddGrav (_planet); // register in list of massive objects
	AddBody (_planet); // register in general list
	_planet->SetPsys (this);
	_planet->Attach (cbody);
	return planets.size();
}

void PlanetarySystem::ScanMoons (istream &is, CelestialBody *cbody, const char *id)
{
	char cbuf[256], name[256];
	for (int i = 0;; i++) {
		sprintf (cbuf, "%s:Moon%d", id, i+1);
		if (!GetItemString (is, cbuf, name)) return;
		Planet *moon = new Planet (name); TRACENEW
		AddPlanet (moon, cbody);
		ScanMoons (is, moon, cbuf); // recursion
	}
}

void PlanetarySystem::AddGrav (CelestialBody *newBody)
{
	//(alektron) The old method sorted by mass directly while inserting into the newly allocated list.
	//While the sorting was technically more efficient this way (since most of the list was already sorted anyways),
	//it doesn't really matter much since this function is not called very often.
	//And this is just so much more readable.
	celestials.emplace_back(newBody);
	std::sort(celestials.begin(), celestials.end(), [](CelestialBody* a, CelestialBody* b) { return a->Mass() > b->Mass(); });
}

size_t PlanetarySystem::AddVessel (Vessel *_vessel)
{
	vessels.emplace_back(_vessel);
	AddBody (_vessel); // register in general list
	g_bForceUpdate = true;
	return vessels.size();
}

bool PlanetarySystem::DelVessel (Vessel *_vessel)
{
	size_t i;
	for (i = 0; i < vessels.size(); i++) {
		if (vessels[i] == _vessel)
			break;
	}

	if (i == vessels.size())
		return false; // vessels not found in list

	DelBody (_vessel); //DelBody takes care of freeing the vessel
	std::iter_swap(vessels.begin() + i, vessels.end() - 1);
	vessels.pop_back();

	g_bForceUpdate = true;
	return true;
}

void PlanetarySystem::AddSuperVessel (SuperVessel *sv)
{
	supervessels.emplace_back(sv);
}

bool PlanetarySystem::DelSuperVessel (SuperVessel *sv)
{
	size_t i;
	for (i = 0; i < supervessels.size(); i++) {
		if (supervessels[i] == sv)
			break;
	}

	if (i == supervessels.size())
		return false; // vessels not found in list

	delete supervessels[i];

	//Note that supervesslels unlike other bodies are NOT also present in the 'bodies'
	std::iter_swap(supervessels.begin() + i, supervessels.end() - 1);
	supervessels.pop_back();

	return false;
}

void PlanetarySystem::DockVessels (Vessel *vessel1, Vessel *vessel2, int port1, int port2, bool mixmoments)
{
	// if the vessels are already complex structures, store a reference to
	// the superstructures so we can remove them after merging
	SuperVessel *sv1 = vessel1->SuperStruct();
	SuperVessel *sv2 = vessel2->SuperStruct();

	if (!sv1 && !sv2) {    // create a new super-structure
		AddSuperVessel (new SuperVessel (vessel1, vessel2, port1, port2, mixmoments)); TRACENEW
	} else if (sv1 && sv2) { // merge sv2 into sv1
		sv1->Merge (vessel1, port1, vessel2, port2);
		DelSuperVessel (sv2);
	} else if (sv1) {        // add vessel2 into sv1
		sv1->Add (vessel1, port1, vessel2, port2, mixmoments);
	} else {                 // add vessel1 into sv2
		sv2->Add (vessel2, port2, vessel1, port1, mixmoments);
	}
}

void PlanetarySystem::UndockVessel (SuperVessel *sv, Vessel *_vessel, int port, double vsep)
{
	sv->Detach (_vessel, port, vsep);
	if (!sv->nVessel()) {
		DelSuperVessel(sv);
	}
}

void PlanetarySystem::ScanGFieldSources (const Vector *gpos, const Body *exclude, GFieldData *gfd) const
{
	const double min_contrib = 1e-6; // min. rel. g-field contribution threshold
	DWORD i, j, idx;
	double a, atot = 0.0;
	gfd->ngrav = 0; // reset gravitation source list
	for (i = 0; i < celestials.size(); i++)
		if (celestials[i] != exclude)
			atot += celestials[i]->Mass() / gpos->dist2 (celestials[i]->GPos());
	for (i = 0; i < celestials.size(); i++) {
		if (celestials[i] != exclude) {
			a = celestials[i]->Mass() / gpos->dist2 (celestials[i]->GPos());
			if (a > min_contrib*atot) {
				if (gfd->ngrav < MAXGFIELDLIST)
					gfd->gravidx[gfd->ngrav++] = i;
				else { // find current smallest contribution
					double a2, amin = 1e100;
					DWORD jmin = MAXGFIELDLIST;
					// this search is not very efficient, but shouldn't be
					// called very often
					for (j = 0; j < MAXGFIELDLIST; j++) {
						idx = gfd->gravidx[j];
						a2 = celestials[idx]->Mass() / gpos->dist2 (celestials[idx]->GPos());
						if (a2 < amin) amin = a2, jmin = j;
					}
					if (amin < a) gfd->gravidx[jmin] = i; // replace
				}
			}
		}
	}
	gfd->testidx = 0;
}

void PlanetarySystem::UpdateGFieldSources (const Vector *gpos, const Body *exclude, GFieldData *gfd) const
{
	extern const DWORD MAXGFIELDLIST;
	const double min_contrib = 1e-6; // min. g-field contribution threshold
	DWORD i, idx, imin;
	double a, atot = 0.0, amin = 1e100;
	Vector acc;
	bool testnext = true;

	// First, check we can drop the weakest member of the current list
	for (i = 0; i < gfd->ngrav; i++) {
		idx = gfd->gravidx[i];
		if (celestials[idx] != exclude) {
			Vector acci = SingleGacc (celestials[idx]->GPos() - *gpos, celestials[idx]);
			acc += acci;
			a = acci.length();
			atot += a;
			if (a < amin) amin = a, imin = i;
		}
		if (gfd->testidx == idx) testnext = false;
	}
	if (amin < min_contrib*atot) {
		for (i = imin+1; i < gfd->ngrav; i++)
			gfd->gravidx[i-1] = gfd->gravidx[i];
		gfd->ngrav--;
	}

	// Test next candidate for inclusion in the list
	if (testnext && exclude != celestials[idx = gfd->testidx]) {
		Vector acci = SingleGacc (celestials[idx]->GPos() - *gpos, celestials[idx]);
		a = acci.length();
		if (a > min_contrib*atot) {
			if (gfd->ngrav < MAXGFIELDLIST) gfd->gravidx[gfd->ngrav++] = idx;
			else if (a > amin) gfd->gravidx[imin] = idx;
		}
	}
	if (++gfd->testidx == celestials.size()) gfd->testidx = 0;
}

Vector PlanetarySystem::GaccAt (double t, const Vector &gpos, const Body *exclude) const
{
	Vector r, acc, pos, closepos;
	CelestialBody *closep = 0;
	const CelestialBody *sec;
	DWORD i;
	double d, dmin = 1e100;

	// pass 1: sun and primary planets
	for (i = 0; i < celestials.size(); i++) {
		if (exclude == celestials[i]) continue;
		if (celestials[i]->Primary() &&
			celestials[i]->Primary()->Type() == OBJTP_PLANET) continue;
		if (celestials[i]->Type() == OBJTP_PLANET) {
			if (!celestials[i]->PositionAtTime (t, &pos)) continue;
		} else pos.Set(0,0,0); // sun assumed in origin
		r.Set(pos - gpos);
		d = r.length();
		acc += r * (Ggrav * celestials[i]->Mass() / (d*d*d));
		if (d < dmin) dmin = d, closep = celestials[i], closepos.Set(pos);
	}
	// pass 2: moons of closest planet
	if (closep && closep->Type() == OBJTP_PLANET) {
		for (i = 0; i < closep->nSecondary(); i++) {
			sec = closep->Secondary(i);
			if (!sec->PositionAtTime (t, &pos)) continue;
			pos += closepos;
			r.Set(pos - gpos);
			d = r.length();
			acc += r * (Ggrav * sec->Mass() / (d*d*d));
		}
	}
	return acc;
}

Vector SingleGacc_perturbation (const Vector &rpos, const CelestialBody *body)
{
	// Calculate perturbation of gravitational acceleration due to nonspherical
	// shape of the body.
	// rpos: relative position of 'bodies' wrt. r (global frame)

	Vector dg;

	if (body->UseComplexGravity() && body->usePines()) {
		
		//Rotate position vector into the planet's local frame
		Matrix rot = body->GRot();
		Vector lpos = -tmul(rot,rpos)/1000.0;

		//Convert to right-handed
		double temp_y;
		temp_y = lpos.y;
		lpos.y = lpos.z; 
		lpos.z = temp_y;

		unsigned int maxDegreeOrder = body->GetPinesCutoff();
		//get aceleration vector from spherical harmonics
		{
			//Limit scope of the const cast. the internal state of body.PinesGravProp does need to change when this finction is called.
			CelestialBody* unconstbody = const_cast<CelestialBody*>(body);
			dg = unconstbody->pinesAccel(lpos, maxDegreeOrder, maxDegreeOrder);
		}

		//Convert back to Orbiter's lefthandedness
		temp_y = dg.y;
		dg.y = dg.z;
		dg.z = temp_y;

		//rotate back into global frame
		dg = mul(rot, dg) * 1000.0;
		
		//Useful debug string. Make sure you only have one vessel in your scenerio if you us it...
		//double radial = dg.length() * 100000.0 * dotp(rpos.unit(), dg.unit());
		//sprintf(DBG_MSG, "<%lf %lf %lf> Magnitude: %lf mGal Radial: %lf Radialness: %lf", dg.x * 100000.0, dg.y * 100000.0, dg.z * 100000.0, dg.length()*100000.0, radial,  dotp(rpos.unit(), dg.unit()));
		return dg;
	}
	else if (body->UseComplexGravity() && body->nJcoeff() > 0) {

		const double eps = 1e-10; // perturbation limit
		double d  = rpos.length();
		double Rr = body->Size() / d;
		double Rrn = Rr*Rr;
		double gacc_r = 0.0, gacc_p = 0.0;

		// J2 perturbation term
		double Jn_Rrn = body->Jcoeff(0) * Rrn;  // relative influence of J2 term
		if (fabs (Jn_Rrn) > eps) {
			Vector er (rpos.unit());  // radial unit vector
			Vector loc (tmul (body->GRot(), er));
			double lat = asin(-loc.y), slat = sin(lat), clat = cos(lat); // latitude
			gacc_r += 1.5 * Jn_Rrn * (1.0 - 3.0*slat*slat);
			gacc_p += 3.0 * Jn_Rrn * clat*slat;

			// J3 perturbation term
			if (body->nJcoeff() > 1) {
				Rrn *= Rr;
				Jn_Rrn = body->Jcoeff(1) * Rrn;  // relative influence of J3 term
				if (fabs (Jn_Rrn) > eps) {
					gacc_r += 2.0 * Jn_Rrn * slat * (3.0 - 5.0*slat*slat);
					gacc_p += 1.5 * Jn_Rrn * clat * (-1.0 + 5.0*slat*slat);

					// J4 perturbation term
					if (body->nJcoeff() > 2) {
						Rrn *= Rr;
						Jn_Rrn = body->Jcoeff(2) * Rrn;
						if (fabs (Jn_Rrn) > eps) {
							gacc_r += -0.625 * Jn_Rrn * (3.0 + slat*slat*(-30.0 + 35.0*slat*slat));
							gacc_p += 2.5 * Jn_Rrn * clat*slat * (-3.0 + 7.0*slat*slat);
						}
					}
				}
			}
			double GM = Ggrav * body->Mass();
			double T0 = GM / (d*d);

			Vector ep, ea (crossp (er, body->RotAxis())); // azimuth vector
			double lea = ea.length();
			if (lea > eps) ep.Set (crossp (er, ea/lea));  // polar unit vector
			else ep.Set (0,0,0);
			dg = er * (T0*gacc_r) + ep * (T0*gacc_p);
		}
	}
	return dg;
}

Vector SingleGacc (const Vector &rpos, const CelestialBody *body)
{
	// Calculate gravitational acceleration at a position r from a single source
	// rpos: relative position of 'body' wrt. r (global coords)

	double d  = rpos.length();
	return rpos * (Ggrav * body->Mass() / (d*d*d)) + SingleGacc_perturbation (rpos, body);
}

Vector PlanetarySystem::Gacc (const Vector &gpos, const Body *exclude, const GFieldData *gfd) const
{
	Vector acc;
	DWORD i, j;

	if (gfd) {
		for (j = 0; j < gfd->ngrav; j++) {
			i = gfd->gravidx[j];
			if (exclude == celestials[i]) continue;
			acc += SingleGacc (celestials[i]->s0->pos - gpos, celestials[i]);
		}
	} else {
		for (i = 0; i < celestials.size(); i++) {
			if (exclude == celestials[i]) continue;
			acc += SingleGacc (celestials[i]->s0->pos - gpos, celestials[i]);
		}
	}
	return acc;
}

Vector PlanetarySystem::Gacc_intermediate (const Vector &gpos, double n, const Body *exclude, GFieldData *gfd) const
{
	Vector acc;
	DWORD i, j;
	
	if (gfd) { // use body's source list
		for (j = 0; j < gfd->ngrav; j++) {
			i = gfd->gravidx[j];
			if (exclude == celestials[i]) continue;
			acc += SingleGacc (celestials[i]->InterpolatePosition (n) - gpos, celestials[i]);
		}
	} else { // use full gbody list
		for (i = 0; i < celestials.size(); i++) {
			if (exclude == celestials[i]) continue;
			acc += SingleGacc (celestials[i]->InterpolatePosition (n) - gpos, celestials[i]);
		}
	}
	return acc;
}

Vector PlanetarySystem::Gacc_intermediate_pert (const CelestialBody *cbody, const Vector &relpos, double n, const Body *exclude, GFieldData *gfd) const
{
	Vector acc;
	DWORD i, j;
	Vector gpos = relpos + cbody->InterpolatePosition (n);

	if (gfd) { // use body's source list
		for (j = 0; j < gfd->ngrav; j++) {
			i = gfd->gravidx[j];
			if (exclude == celestials[i]) continue;
			if (cbody == celestials[i])
				acc += SingleGacc_perturbation (-relpos, celestials[i]);
			else
				acc += SingleGacc (celestials[i]->InterpolatePosition (n) - gpos, celestials[i]);
		}
	} else { // use full gbody list
		for (i = 0; i < celestials.size(); i++) {
			if (exclude == celestials[i]) continue;
			if (cbody == celestials[i])
				acc += SingleGacc_perturbation (-relpos, celestials[i]);
			else
				acc += SingleGacc (celestials[i]->InterpolatePosition (n) - gpos, celestials[i]);
		}
	}
	return acc;
}

Vector PlanetarySystem::GaccRel (const Vector &rpos, const CelestialBody *cbody, double n, const Body *exclude, GFieldData *gfd) const
{
	Vector acc;
	DWORD i, j;

	Vector gpos (rpos + cbody->InterpolatePosition (n));

	if (gfd) { // use bodies's source list
		for (j = 0; j < gfd->ngrav; j++) {
			i = gfd->gravidx[j];
			if (exclude == celestials[i]) continue;
			acc += SingleGacc (celestials[i]->InterpolatePosition (n) - gpos, celestials[i]);
		}
	} else { // use full gbody list
		for (i = 0; i < celestials.size(); i++) {
			if (exclude == celestials[i]) continue;
			acc += SingleGacc (celestials[i]->InterpolatePosition (n) - gpos, celestials[i]);
		}
	}
	return acc;
}

Vector PlanetarySystem::GaccPn_perturbation (const Vector &gpos, double n, const CelestialBody *cbody) const
{
	return SingleGacc_perturbation (cbody->InterpolatePosition (n) - gpos, cbody);
}

CelestialBody *PlanetarySystem::GetDominantGravitySource (const Vector &gpos, double &gfrac)
{
	// assumes spherical gravity sources
	DWORD i;
	double g, gtot = 0.0, gmax = 0.0;
	CelestialBody *body = 0;

	for (i = 0; i < celestials.size(); i++) {
		Vector r(celestials[i]->GPos() - gpos);
		gtot += (g = celestials[i]->Mass() / r.length2());
		if (g > gmax) {
			gmax = g;
			body = celestials[i];
		}
	}
	gfrac = (body ? gmax/gtot : 0.0);
	return body;
}

double PlanetarySystem::GetGravityContribution (const Body *body, const Vector &gpos, bool *dominant)
{
	// assumes spherical gravity sources
	DWORD i;
	double g, gtot = 0.0, gbody = 0.0, gmax = 0.0;

	for (i = 0; i < celestials.size(); i++) {
		Vector r(celestials[i]->GPos() - gpos);
		gtot += (g = celestials[i]->Mass() / r.length2());
		if (celestials[i] == body) gbody = g;
		if (g > gmax) gmax = g;
	}
	if (dominant) *dominant = (gmax == gbody);
	return (gtot ? gbody/gtot : 0.0);
}

Vector PlanetarySystem::GetMomentumFlux (const Vector &gpos) const
{
	const double L = 3.846e26;      // sun luminosity [W] - SHOULD BE CONFIGURABLE!
	const double F = L/(4.0*PI*C0); // radiation force at unit distance
	double r2 = gpos.length2();
	double p  = F/r2;               // radiation pressure at distance r
	return gpos * (p/sqrt(r2));
}

void PlanetarySystem::Update (bool force)
{
	DWORD i;
	for (i = 0; i < bodies      .size(); i++) bodies      [i]->BeginStateUpdate ();
	for (i = 0; i < stars       .size(); i++) stars       [i]->RelTrueAndBaryState();
	for (i = 0; i < stars       .size(); i++) stars       [i]->AbsTrueState();
	for (i = 0; i < celestials  .size(); i++) celestials  [i]->Update (force);
	for (i = 0; i < vessels     .size(); i++) vessels     [i]->UpdateBodyForces ();
	for (i = 0; i < supervessels.size(); i++) supervessels[i]->Update (force);
	for (i = 0; i < vessels     .size(); i++) vessels     [i]->Update (force);
}

void PlanetarySystem::FinaliseUpdate ()
{
	DWORD i;
	for (i = 0; i < bodies.size(); i++) bodies[i]->EndStateUpdate ();
	for (i = 0; i < supervessels.size(); i++) supervessels[i]->PostUpdate ();
	for (i = 0; i < vessels.size(); i++) vessels[i]->PostUpdate ();
}

void PlanetarySystem::Timejump (const TimeJumpData& jump)
{
	DWORD i;
	for (i = 0; i < bodies.size(); i++) bodies[i]->BeginStateUpdate ();
	for (i = 0; i < stars.size(); i++) stars[i]->RelTrueAndBaryState();
	for (i = 0; i < stars.size(); i++) stars[i]->AbsTrueState();
	for (i = 0; i < celestials.size(); i++) celestials[i]->Update (true);
	for (i = 0; i < bodies.size(); i++) bodies[i]->EndStateUpdate ();

	for (i = 0; i < vessels.size(); i++)
		vessels[i]->Timejump(jump.dt, jump.mode);
}

void PlanetarySystem::InitDeviceObjects ()
{
	for (DWORD i = 0; i < bodies.size(); i++)
		bodies[i]->InitDeviceObjects();
}

void PlanetarySystem::DestroyDeviceObjects ()
{
	for (DWORD i = 0; i < bodies.size(); i++)
		bodies[i]->DestroyDeviceObjects();
}

void PlanetarySystem::BroadcastVessel (DWORD msg, void *data)
{
	for (DWORD i = 0; i < vessels.size(); i++)
		vessels[i]->ProcessMessage (msg, data);
}