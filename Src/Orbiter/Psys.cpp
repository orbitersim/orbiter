// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Implementation of class PlanetarySystem

#include <fstream>
#include <stdio.h>
#include <string.h>
#include <io.h>
#include "Orbiter.h"
#include "Config.h"
#include "Psys.h"
#include "Astro.h"
#include "Element.h"
#include "Vessel.h"
#include "SuperVessel.h"
#include "Log.h"

using namespace std;

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern bool g_bForceUpdate;
extern char DBG_MSG[256];

PlanetarySystem::PlanetarySystem (char *fname)
{
	name = 0;
	nbody = 0;
	nstar = 0;
	nplanet = 0;
	ngrav = 0;
	nvessel = 0;
	nsupervessel = 0;
	//nuserlabel   = 0;
	labellist    = 0;
	nlabellist   = 0;
	labelpath = 0;
	Read (fname);
}

PlanetarySystem::~PlanetarySystem ()
{
	Clear ();
}

void PlanetarySystem::Clear ()
{
	int i, j;
	DWORD k;

	DestroyDeviceObjects ();
	if (name) {
		delete []name;
		name = 0;
	}
	if (labelpath) {
		delete []labelpath;
		labelpath = 0;
	}
	if (labellist) {
		for (i = 0; i < nlabellist; i++) {
			if (labellist[i].list) {
				for (j = 0; j < labellist[i].length; j++)
					for (k = 0; k < 2; k++)
						if (labellist[i].list[j].label[k]) delete []labellist[i].list[j].label[k];
				delete []labellist[i].list;
			}
		}
		delete []labellist;
		labellist = NULL;
		nlabellist = 0;
	}
	while (nvessel) DelVessel (vessel[0], 0);
	if (nbody) {
		for (k = 0; k < nbody; k++) delete body[k]; // delete actual objects
		delete []body; // delete list
		nbody = 0;
	}
	if (nstar) {
		delete []star;
		nstar = 0;
	}
	if (nplanet) {
		delete []planet;
		nplanet = 0;
	}
	if (ngrav) {
		delete []grav;
		ngrav = 0;
	}
	if (nsupervessel) {
		for (DWORD k = 0; k < nsupervessel; k++) delete supervessel[k];
		delete []supervessel;
		nsupervessel = 0;
	}
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
	DWORD i;
	for (i = 0; i < nvessel; i++) vessel[i]->PostCreation();
	for (i = 0; i < nvessel; i++) vessel[i]->InitSupervessel();
	for (i = 0; i < nvessel; i++) vessel[i]->ModulePostCreation();
}

void PlanetarySystem::Write (ostream &os)
{
	os << "BEGIN_SHIPS" << endl;
	for (DWORD i = 0; i < nvessel; i++)
		vessel[i]->Write (os);
	os << "END_SHIPS" << endl;
}

Body *PlanetarySystem::GetObj (const char *name, bool ignorecase)
{
	for (DWORD i = 0; i < nbody; i++)
		if (!StrComp (body[i]->Name(), name, ignorecase)) return body[i];
	return 0;
}

CelestialBody *PlanetarySystem::GetGravObj (const char *name, bool ignorecase) const
{
	for (DWORD i = 0; i < ngrav; i++)
		if (!StrComp (grav[i]->Name(), name, ignorecase)) return grav[i];
	return 0;
}

Planet *PlanetarySystem::GetPlanet (const char *name, bool ignorecase)
{
	for (int i = 0; i < nplanet; i++)
		if (!StrComp (planet[i]->Name(), name, ignorecase)) return planet[i];
	return 0;
}

Vessel *PlanetarySystem::GetVessel (const char *name, bool ignorecase) const
{
	for (DWORD i = 0; i < nvessel; i++)
		if (!StrComp (vessel[i]->Name(), name, ignorecase)) return vessel[i];
	return 0;
}

bool PlanetarySystem::isObject (const Body *obj) const
{
	for (DWORD i = 0; i < nbody; i++)
		if (body[i] == obj) return true;
	return false;
}

bool PlanetarySystem::isVessel (const Vessel *v) const
{
	for (DWORD i = 0; i < nvessel; i++)
		if (vessel[i] == v) return true;
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
	for (DWORD i = 0; i < ngrav; i++) {
		if (grav[i]->Type() != OBJTP_PLANET) continue;
		Planet *planet = (Planet*)grav[i];
		Base *base = GetBase (planet, name, ignorecase);
		if (base) return base;
	}
	return 0;
}

bool PlanetarySystem::Read (char *fname)
{
	int i;
	DWORD j;
	char cbuf[256], label[128];
	Planet *body;
	
	ifstream ifs (g_pOrbiter->ConfigPath (fname));
	if (!ifs) return false;
	Clear();
	if (GetItemString (ifs, "Name", cbuf)) {
		name = new char[strlen(cbuf)+1]; TRACENEW
		strcpy (name, cbuf);
	}

	// read stars
	for (;;) {
		sprintf (label, "Star%d", nstar+1);
		if (!GetItemString (ifs, label, cbuf)) break;
		OutputLoadStatus (cbuf);
		AddStar (new Star (cbuf)); TRACENEW
	}

	// read planets
	for (i = 0;; i++) {
		sprintf (label, "Planet%d", i+1);
		if (!GetItemString (ifs, label, cbuf)) break;
		OutputLoadStatus (cbuf);
		body = new Planet (cbuf); TRACENEW
		AddPlanet (body, star[0]);
		ScanMoons (ifs, body, body->Name());
	}

	for (j = 0; j < nstar; j++) star[j]->RelTrueAndBaryState();
	for (j = 0; j < nstar; j++) star[j]->AbsTrueState();

	// read celestial markers
	if (labelpath) {
		delete []labelpath;
		labelpath = 0;
	}
	labellist = 0;
	nlabellist = 0;
	if (GetItemString (ifs, "MarkerPath", cbuf)) {
		if (cbuf[strlen(cbuf)-1] != '\\') strcat (cbuf, "\\");
		labelpath = new char[strlen(cbuf)+1]; TRACENEW
		strcpy (labelpath, cbuf);
	}
	ScanLabelLists (ifs);

	return true;
}

void PlanetarySystem::OutputLoadStatus (const char *bname)
{
	char cbuf[256];
	sprintf (cbuf, "%s: %s", name, bname);
	g_pOrbiter->OutputLoadStatus (cbuf, 0);
}

intptr_t PlanetarySystem::FindFirst (int type, _finddata_t *fdata, char *path, char *fname)
{
	intptr_t fh;
	char cbuf[256];

	switch (type) {
	case FILETYPE_MARKER:
		if (labelpath) strcpy (path, labelpath);
		else           sprintf (path, "%s%s\\Marker\\", g_pOrbiter->Cfg()->CfgDirPrm.ConfigDir, name);
		break;
	}
	sprintf (cbuf, "%s*.mkr", path);
	if ((fh = _findfirst (cbuf, fdata)) != -1) {
		strncpy (fname, fdata->name, strlen(fdata->name)-4);
		fname[strlen(fdata->name)-4] = '\0';
	}
	return fh;
}

intptr_t PlanetarySystem::FindNext (intptr_t fh, _finddata_t *fdata, char *fname)
{
	intptr_t fn = _findnext (fh, fdata);
	if (!fn) {
		strncpy (fname, fdata->name, strlen(fdata->name)-4);
		fname[strlen(fdata->name)-4] = '\0';
	}
	return fn;
}

void PlanetarySystem::ScanLabelLists (ifstream &cfg)
{
	int i;
	char cbuf[256], fname[256], lbpath[256];
	int nlabellistbuf = 0;
	nlabellist = 0;

	_finddata_t fdata;
	intptr_t fh = FindFirst (FILETYPE_MARKER, &fdata, lbpath, fname);
	if (fh >= 0) {

		oapi::GraphicsClient::LABELLIST *ll;
		bool scanheader = (labellist == 0); // only need to parse the headers for the initial scan
		
		do {
			// open marker file
			sprintf (cbuf, "%s%s.mkr", lbpath, fname);
			ifstream ulf (cbuf);

			// read label header
			if (scanheader) {
				if (nlabellist == nlabellistbuf) { // increase buffer
					oapi::GraphicsClient::LABELLIST *tmp = new oapi::GraphicsClient::LABELLIST[nlabellistbuf += 8]; TRACENEW
					memcpy (tmp, labellist, nlabellist*sizeof(oapi::GraphicsClient::LABELLIST));
					if (nlabellist) delete []labellist;
					labellist = tmp;
				}
				ll = labellist+nlabellist;
				ll->list    = NULL;
				ll->length  = 0;
				ll->colour  = 1;
				ll->shape   = 0;
				ll->size    = 1.0f;
				ll->distfac = 1.0f;
				ll->active  = false;
				ll->flag    = 0;
				if (FindLine (ulf, "BEGIN_HEADER")) {
					char item[256], value[256];
					for (;;) {
						if (!ulf.getline (cbuf, 256) || !_strnicmp (cbuf, "END_HEADER", 10)) break;
						sscanf (cbuf, "%s %s", item, value);
						if (!_stricmp (item, "InitialState")) {
							if (!_stricmp (value, "on")) ll->active = true;
						} else if (!_stricmp (item, "ColourIdx")) {
							int col;
							sscanf (value, "%d", &col);
							ll->colour = max (0, min (5, col));
						} else if (!_stricmp (item, "ShapeIdx")) {
							int shape;
							sscanf (value, "%d", &shape);
							ll->shape = max (0, min (6, shape));
						} else if (!_stricmp (item, "Size")) {
							float size;
							sscanf (value, "%f", &size);
							ll->size = max (0.1f, min (2.0f, size));
						} else if (!_stricmp (item, "DistanceFactor")) {
							float distfac;
							sscanf (value, "%f", &distfac);
							ll->distfac = max (1e-5f, min (1e3f, distfac));
						} else if (!_stricmp (item, "Frame")) {
							if (_stricmp (value, "Ecliptic"))
								ll->flag = 1; // flag for celestial position data
						}
					}
				}
			} else {
				ll = labellist+nlabellist;
			}

			// check if positions are in celestial or ecliptic frame
			bool celestialpos = ((ll->flag & 1) != 0);

			// read label list for active labels, if not already present
			if (ll->active && !ll->list) {
				ll->length = 0;
				int nlistbuf = 0;
				double lng, lat;
				int nl;
				char *pc, *pc2;
				FindLine (ulf, "BEGIN_DATA");
				for (nl = 0;; nl++) {
					if (!ulf.getline (cbuf, 256)) break;
					pc = strtok (cbuf, ":");
					if (!pc || sscanf (pc, "%lf%lf", &lng, &lat) != 2) continue;
					if (ll->length == nlistbuf) {
						oapi::GraphicsClient::LABELSPEC *tmp = new oapi::GraphicsClient::LABELSPEC[nlistbuf += 64]; TRACENEW
						if (ll->length) {
							memcpy (tmp, ll->list, ll->length*sizeof(oapi::GraphicsClient::LABELSPEC));
							delete []ll->list;
						}
						ll->list = tmp;
					}
					lng = Rad(lng);
					lat = Rad(lat);
					if (celestialpos) {
						static double eps = 0.4092797095927;
						static double coseps = cos(eps), sineps = sin(eps);
						double ra = lng, dc = lat;
						Equ2Ecl (coseps, sineps, ra, dc, lng, lat);
					}
					double xz = cos(lat);
					ll->list[ll->length].pos.y = sin(lat);
					ll->list[ll->length].pos.x = xz * cos(lng);
					ll->list[ll->length].pos.z = xz * sin(lng);
					for (i = 0; i < 2; i++) {
						ll->list[ll->length].label[i] = 0;
						if (pc = strtok (NULL, ":")) {
							pc2 = trim_string (pc);
							int len = strlen(pc2);
							if (len) {
								ll->list[ll->length].label[i] = new char[len+1]; TRACENEW
								strcpy (ll->list[ll->length].label[i], pc2);
							}
						}
					}
					ll->length++;
				}
			}
			nlabellist++;

		} while (!FindNext (fh, &fdata, fname));
		_findclose (fh);
	}
}

void PlanetarySystem::ActivatePlanetLabels(bool activate)
{
	for (int i = 0; i < nplanet; i++)
		planet[i]->ActivateLabels(activate);
}

void PlanetarySystem::AddBody (Body *_body)
{
	Body **tmp = new Body*[nbody+1]; TRACENEW
	if (nbody) {
		memcpy (tmp, body, nbody*sizeof(Body*));
		delete []body;
	}
	body = tmp;
	body[nbody++] = _body;
}

bool PlanetarySystem::DelBody (Body *_body)
{
	DWORD i, j, k;
	Body **tmp;
	for (i = 0; i < nbody; i++)
		if (body[i] == _body) break;
	if (i == nbody) return false;
	delete body[i]; // delete actual body
	if (nbody > 1) {
		tmp = new Body*[nbody-1]; TRACENEW
		for (j = k = 0; j < nbody; j++)
			if (j != i) tmp[k++] = body[j];
	} else tmp = 0;
	delete []body;
	body = tmp;
	nbody--;
	return true;
}

int PlanetarySystem::AddStar (Star *_star)
{
	Star **tmp = new Star*[nstar+1]; TRACENEW
	if (nstar) {
		memcpy (tmp, star, nstar*sizeof(Star*));
		delete []star;
	}
	star = tmp;
	star[nstar] = _star;
	AddGrav (_star); // register in list of massive objects
	AddBody (_star); // register in general list
	_star->SetPsys (this); // somewhat ugly
	return nstar++;
}

int PlanetarySystem::AddPlanet (Planet *_planet, CelestialBody *cbody)
{
	Planet **tmp = new Planet*[nplanet+1]; TRACENEW
	if (nplanet) {
		memcpy (tmp, planet, nplanet*sizeof(Planet*));
		delete []planet;
	}
	planet = tmp;
	planet[nplanet] = _planet;
	AddGrav (_planet); // register in list of massive objects
	AddBody (_planet); // register in general list
	_planet->SetPsys (this);
	_planet->Attach (cbody);
	return nplanet++;
}

void PlanetarySystem::ScanMoons (istream &is, CelestialBody *cbody, char *id)
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

void PlanetarySystem::AddGrav (CelestialBody *body)
{
	DWORD i, j;

	CelestialBody **tmp = new CelestialBody*[ngrav+1]; TRACENEW
	for (i = j = 0; i < ngrav;) {
		if (i == j && body->Mass() > grav[i]->Mass())
			tmp[j++] = body;
		tmp[j++] = grav[i++];
	}
	if (j == ngrav) tmp[j] = body;
	if (ngrav) delete []grav;
	grav = tmp;
	ngrav++;
}

int PlanetarySystem::AddVessel (Vessel *_vessel)
{
	Vessel **tmp = new Vessel*[nvessel+1]; TRACENEW
	if (nvessel) {
		memcpy (tmp, vessel, nvessel*sizeof(Vessel*));
		delete []vessel;
	}
	vessel = tmp;
	vessel[nvessel] = _vessel;
	AddBody (_vessel); // register in general list
	g_bForceUpdate = true;
	return nvessel++;
}

bool PlanetarySystem::DelVessel (Vessel *_vessel, Body *_alt_cam_tgt)
{
	//if (!g_pOrbiter->RequestDelete (_vessel, _alt_cam_tgt)) return false;
	DWORD i, j, k;
	for (i = 0; i < nvessel; i++)
		if (vessel[i] == _vessel) break;
	if (i == nvessel) return false; // vessel not found in list
	DelBody (_vessel);
	Vessel **tmp;
	if (nvessel > 1) {
		tmp = new Vessel*[nvessel-1]; TRACENEW
		for (j = k = 0; j < nvessel; j++)
			if (j != i) tmp[k++] = vessel[j];
	} else tmp = 0;
	delete []vessel;
	vessel = tmp;
	nvessel--;
	g_bForceUpdate = true;
	return true;
}

void PlanetarySystem::AddSuperVessel (SuperVessel *sv)
{
	SuperVessel **tmp = new SuperVessel*[nsupervessel+1]; TRACENEW
	if (nsupervessel) {
		memcpy (tmp, supervessel, nsupervessel*sizeof(SuperVessel*));
		delete []supervessel;
	}
	supervessel = tmp;
	supervessel[nsupervessel++] = sv;
}

bool PlanetarySystem::DelSuperVessel (SuperVessel *sv)
{
	DWORD i, j, k;

	for (i = 0; i < nsupervessel; i++) {
		if (supervessel[i] == sv) {
			delete sv;
			SuperVessel **tmp;
			if (nsupervessel > 1) {
				tmp = new SuperVessel*[nsupervessel-1]; TRACENEW
				for (j = k = 0; j < nsupervessel; j++)
					if (j != i) tmp[k++] = supervessel[j];
			} else tmp = 0;
			delete []supervessel;
			supervessel = tmp;
			nsupervessel--;
			return true;
		}
	}
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
		// remove supervessel from list
		DWORD i, j, k;
		for (i = 0; i < nsupervessel; i++) {
			if (supervessel[i] == sv) {
				SuperVessel **tmp;
				if (nsupervessel > 1) {
					tmp = new SuperVessel*[nsupervessel-1]; TRACENEW
					for (j = k = 0; j < nsupervessel; j++)
						if (j != i) tmp[k++] = supervessel[j];
				} else tmp = 0;
				delete []supervessel;
				supervessel = tmp;
				nsupervessel--;
				delete sv;
				break;
			}
		}
	}
}

void PlanetarySystem::ScanGFieldSources (const Vector *gpos, const Body *exclude, GFieldData *gfd) const
{
	const double min_contrib = 1e-6; // min. rel. g-field contribution threshold
	DWORD i, j, idx;
	double a, atot = 0.0;
	gfd->ngrav = 0; // reset gravitation source list
	for (i = 0; i < ngrav; i++)
		if (grav[i] != exclude)
			atot += grav[i]->Mass() / gpos->dist2 (grav[i]->GPos());
	for (i = 0; i < ngrav; i++) {
		if (grav[i] != exclude) {
			a = grav[i]->Mass() / gpos->dist2 (grav[i]->GPos());
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
						a2 = grav[idx]->Mass() / gpos->dist2 (grav[idx]->GPos());
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
		if (grav[idx] != exclude) {
			Vector acci = SingleGacc (grav[idx]->GPos() - *gpos, grav[idx]);
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
	if (testnext && exclude != grav[idx = gfd->testidx]) {
		Vector acci = SingleGacc (grav[idx]->GPos() - *gpos, grav[idx]);
		a = acci.length();
		if (a > min_contrib*atot) {
			if (gfd->ngrav < MAXGFIELDLIST) gfd->gravidx[gfd->ngrav++] = idx;
			else if (a > amin) gfd->gravidx[imin] = idx;
		}
	}
	if (++gfd->testidx == ngrav) gfd->testidx = 0;
}

Vector PlanetarySystem::GaccAt (double t, const Vector &gpos, const Body *exclude) const
{
	Vector r, acc, pos, closepos;
	CelestialBody *closep = 0;
	const CelestialBody *sec;
	DWORD i;
	double d, dmin = 1e100;

	// pass 1: sun and primary planets
	for (i = 0; i < ngrav; i++) {
		if (exclude == grav[i]) continue;
		if (grav[i]->Primary() &&
			grav[i]->Primary()->Type() == OBJTP_PLANET) continue;
		if (grav[i]->Type() == OBJTP_PLANET) {
			if (!grav[i]->PositionAtTime (t, &pos)) continue;
		} else pos.Set(0,0,0); // sun assumed in origin
		r.Set(pos - gpos);
		d = r.length();
		acc += r * (Ggrav * grav[i]->Mass() / (d*d*d));
		if (d < dmin) dmin = d, closep = grav[i], closepos.Set(pos);
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
	// rpos: relative position of 'body' wrt. r (global frame)

	Vector dg;

	if (body->UseComplexGravity() && body->nJcoeff() > 0) {

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
			if (exclude == grav[i]) continue;
			acc += SingleGacc (grav[i]->s0->pos - gpos, grav[i]);
		}
	} else {
		for (i = 0; i < ngrav; i++) {
			if (exclude == grav[i]) continue;
			acc += SingleGacc (grav[i]->s0->pos - gpos, grav[i]);
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
			if (exclude == grav[i]) continue;
			acc += SingleGacc (grav[i]->InterpolatePosition (n) - gpos, grav[i]);
		}
	} else { // use full gbody list
		for (i = 0; i < ngrav; i++) {
			if (exclude == grav[i]) continue;
			acc += SingleGacc (grav[i]->InterpolatePosition (n) - gpos, grav[i]);
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
			if (exclude == grav[i]) continue;
			if (cbody == grav[i])
				acc += SingleGacc_perturbation (-relpos, grav[i]);
			else
				acc += SingleGacc (grav[i]->InterpolatePosition (n) - gpos, grav[i]);
		}
	} else { // use full gbody list
		for (i = 0; i < ngrav; i++) {
			if (exclude == grav[i]) continue;
			if (cbody == grav[i])
				acc += SingleGacc_perturbation (-relpos, grav[i]);
			else
				acc += SingleGacc (grav[i]->InterpolatePosition (n) - gpos, grav[i]);
		}
	}
	return acc;
}

Vector PlanetarySystem::GaccRel (const Vector &rpos, const CelestialBody *cbody, double n, const Body *exclude, GFieldData *gfd) const
{
	Vector acc;
	DWORD i, j;

	Vector gpos (rpos + cbody->InterpolatePosition (n));

	if (gfd) { // use body's source list
		for (j = 0; j < gfd->ngrav; j++) {
			i = gfd->gravidx[j];
			if (exclude == grav[i]) continue;
			acc += SingleGacc (grav[i]->InterpolatePosition (n) - gpos, grav[i]);
		}
	} else { // use full gbody list
		for (i = 0; i < ngrav; i++) {
			if (exclude == grav[i]) continue;
			acc += SingleGacc (grav[i]->InterpolatePosition (n) - gpos, grav[i]);
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

	for (i = 0; i < ngrav; i++) {
		Vector r(grav[i]->GPos() - gpos);
		gtot += (g = grav[i]->Mass() / r.length2());
		if (g > gmax) {
			gmax = g;
			body = grav[i];
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

	for (i = 0; i < ngrav; i++) {
		Vector r(grav[i]->GPos() - gpos);
		gtot += (g = grav[i]->Mass() / r.length2());
		if (grav[i] == body) gbody = g;
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
	for (i = 0; i < nbody; i++) body[i]->BeginStateUpdate ();
	for (i = 0; i < nstar; i++) star[i]->RelTrueAndBaryState();
	for (i = 0; i < nstar; i++) star[i]->AbsTrueState();
	for (i = 0; i < ngrav; i++) grav[i]->Update (force);
	for (i = 0; i < nvessel; i++) vessel[i]->UpdateBodyForces ();
	for (i = 0; i < nsupervessel; i++) supervessel[i]->Update (force);
	for (i = 0; i < nvessel; i++) vessel[i]->Update (force);
}

void PlanetarySystem::FinaliseUpdate ()
{
	DWORD i;
	for (i = 0; i < nbody; i++) body[i]->EndStateUpdate ();
	for (i = 0; i < nsupervessel; i++) supervessel[i]->PostUpdate ();
	for (i = 0; i < nvessel; i++) vessel[i]->PostUpdate ();
}

void PlanetarySystem::Timejump ()
{
	DWORD i;
	for (i = 0; i < nbody; i++) body[i]->BeginStateUpdate ();
	for (i = 0; i < nstar; i++) star[i]->RelTrueAndBaryState();
	for (i = 0; i < nstar; i++) star[i]->AbsTrueState();
	for (i = 0; i < ngrav; i++) grav[i]->Update (true);
	for (i = 0; i < nbody; i++) body[i]->EndStateUpdate ();

	for (i = 0; i < nvessel; i++)
		vessel[i]->Timejump(g_pOrbiter->tjump.dt, g_pOrbiter->tjump.mode);
}

void PlanetarySystem::InitDeviceObjects ()
{
	for (DWORD i = 0; i < nbody; i++)
		body[i]->InitDeviceObjects();
}

void PlanetarySystem::DestroyDeviceObjects ()
{
	for (DWORD i = 0; i < nbody; i++)
		body[i]->DestroyDeviceObjects();
}

void PlanetarySystem::BroadcastVessel (DWORD msg, void *data)
{
	for (DWORD i = 0; i < nvessel; i++)
		vessel[i]->ProcessMessage (msg, data);
}