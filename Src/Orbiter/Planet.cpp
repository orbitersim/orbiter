// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Implementation of class Planet

#define OAPI_IMPLEMENTATION

#include <fstream>
#include <stdio.h>
#include <string.h>
#include "Orbiter.h"
#include "Config.h"
#include "State.h"
#include "Astro.h"
#include "Element.h"
#include "Planet.h"
#include "elevmgr.h"
#include "Base.h"
#include "Camera.h"
#include "Log.h"
#include "Util.h"

#define UPDMODE_ANALYTIC 0
#define UPDMODE_DYNAMIC  1
#define UPDMODE_CUSTOM   2

using namespace std;

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern Camera *g_camera;
extern char DBG_MSG[256];

int patchidx[9] = {0, 1, 2, 3, 5, 13, 37, 137, 501};
// index to the first texture of a given surface patch level

void InterpretEphemeris (double *data, int format, Vector *pos, Vector *vel);


const double OAPI_RAND_MAX = 65536.0;
static DWORD g_seed = 0;

static const DWORD g_rseed[100] = {
	84351070,
  2926063127,
  1629858562,
  3572536693,
  2159564893,
  3047156429,
  1842078683,
  1308321628,
   814556643,
   830780491,
  2930126434,
  1300363200,
  2326471488,
   647994498,
  2997451156,
  1625099663,
  3693721717,
  3666420868,
  2549333297,
  2132676532,
  3864479181,
  3528870375,
  2769869009,
  3513173043,
  2835655763,
  1468752622,
  1244363248,
  1465415222,
  2293851914,
  3122927487,
  1328391121,
  3601313091,
  2439852642,
  1590914112,
  3018244945,
  2347505222,
  1910745930,
  2983143583,
  2668506692,
  3413730546,
  4109611318,
  2244508459,
  3780181997,
   742840970,
  4207980880,
  1165857098,
  1083746293,
  3761282819,
  3166705108,
   586343533,
    50494588,
  3839262532,
   855291486,
  1283005568,
  2840874234,
  1221525592,
  2015302959,
   278232805,
  4244866237,
  2503071213,
  1818902573,
  2214106116,
  1434310685,
  1859319673,
   970447294,
  2490251559,
  3265742850,
  2275572959,
  2751040365,
   897946255,
  1631307479,
  3364370933,
  2924210236,
  1980388489,
  2438805750,
  3411108774,
   254187303,
  2589303007,
   215902868,
  1784021441,
  1309959343,
  3755378407,
    64465306,
  3298321810,
  4169747264,
  4252372356,
  3388135169,
  1884024057,
  2140230752,
   918965512,
  2763778332,
  1374542339,
  4123592090,
  3120859674,
  1769325557,
  3197885688,
  1150824679,
  1889460522,
  4008837039,
  2934889985
  };

inline void oapi_srand (DWORD seed) { g_seed = seed; }
inline DWORD oapi_rand()
{ g_seed = 1103515245*g_seed + 12345; return g_seed >> 16; }

bool Planet::bEnableWind = true;

Planet::Planet (double _mass, double _mean_radius)
: CelestialBody (_mass, _mean_radius)
{
	psys         = 0;
	ncloudtex    = 0;
	cloudtex     = 0;
	nringtex     = 0;
	ringtex      = 0;
	tmgr         = NULL;
	smgr2        = NULL;
	cmgr2        = NULL;
	emgr         = NULL;
	elev_res     = 1.0;
	fog.dens_0 = fog.dens_ref = fog.alt_ref = 0.0;
	nbase        = 0;
	nnav         = 0;
	nobserver    = 0;
	labellist    = 0;
	nlabellist   = 0;
	labelpath    = 0;
	vislimit     = spotlimit = 2e-5;
	horizon_excess = 0.002;
	bb_excess    = 0.0;
	max_patch_level = 1;
	min_cloud_level = 100;
	max_cloud_level = 0;
	minelev      = 0.0;
	maxelev = 0.0;
	AtmInterface = 0;
	atm_attenuationalt = 0.0;
	bHasCloudlayer = false;
	bBrightClouds  = false;
	bCloudMicrotex = false;
	bWaterMicrotex = false;
	bHasRings = false;
	labelLegend = NULL;
	nLabelLegend = 0;
	Setup ();
}

Planet::Planet (char *fname)
: CelestialBody (fname)
{
	int i, n;
	double d;
	char cbuf[256], *pc;

	psys         = 0;
	ncloudtex    = 0;
	cloudtex     = 0;
	nringtex     = 0;
	ringtex      = 0;
	nbase        = 0;
	tmgr         = NULL;
	smgr2        = NULL;
	cmgr2        = NULL;
	emgr         = NULL;
	elev_res     = 1.0;
	fog.dens_0 = fog.dens_ref = fog.alt_ref = 0.0;
	vislimit     = spotlimit = 2e-5;
	horizon_excess = 0.002;
	bb_excess    = 0.0;
	minelev      = 0.0;
	maxelev = 0.0;
	labelLegend  = NULL;
	nLabelLegend = 0;
	ifstream ifs (g_pOrbiter->ConfigPath (fname));
	if (!ifs) return;

	AtmInterface = 0;
	memset (&atm, 0, sizeof(ATMCONST));
	// Check module for atmospheric parameter interface
	if (module) {
		ATMPARAM prm;
		if (module->Version() >= 2 && !((CELBODY2*)module)->LegacyAtmosphereInterface()) {
			ATMOSPHERE *a = ((CELBODY2*)module)->GetAtmosphere();
			if (a) {
				a->clbkConstants (&atm);
				AtmInterface = 4;
			}
		} else if (module->clbkAtmParam (0, &prm)) {
			atm.p0   = prm.p;
			atm.rho0 = prm.rho;
			AtmInterface = 3;
		} else if (modIntf.oplanetAtmPrm) {
			modIntf.oplanetAtmPrm (0, &prm);
			atm.p0   = prm.p;
			atm.rho0 = prm.rho;
			AtmInterface = 2;
		}
	}
	if (!AtmInterface) {
		if (GetItemReal (ifs, "AtmPressure0", atm.p0) && GetItemReal (ifs, "AtmDensity0", atm.rho0))
			AtmInterface = 1; // atmosphere defined via config parameters
	}

	// Override parameters from configuration file
	if (AtmInterface) {
		if (!GetItemReal (ifs, "AtmAltLimit", atm.altlimit) && !atm.altlimit) {
			atm.altlimit = 200e3;
		} else {
			atm_attenuationalt = atm.altlimit;
		}
		GetItemReal (ifs, "AtmAttenuationAlt", atm_attenuationalt);
		if (!GetItemReal (ifs, "AtmHorizonAlt", atm.horizonalt) && !atm.horizonalt)
			atm.horizonalt = atm.altlimit * 0.3;
		if (!GetItemReal (ifs, "AtmGasConstant", atm.R) && !atm.R)
			atm.R = 286.91; // default to air
		if (!GetItemReal (ifs, "AtmGamma", atm.gamma) && !atm.gamma)
			atm.gamma = 1.4;  // default to air
		if (!GetItemReal (ifs, "AtmHazeExtent", hazerange)) hazerange = 0.1;
		hazerange = max (0.0, min (0.9, hazerange));
		if (!GetItemReal (ifs, "AtmHazeShift", hazeshift)) hazeshift = 0.0;
		if (!GetItemReal (ifs, "AtmHazeDensity", hazedens)) hazedens = 1.0;
		Vector col0;
		if (GetItemVector (ifs, "AtmColor0", col0)) {
			atm.color0.x = col0.x; atm.color0.y = col0.y; atm.color0.z = col0.z;
		}
		if (!GetItemVector (ifs, "AtmHazeColor", hazecol))
			hazecol.Set (atm.color0.x, atm.color0.y, atm.color0.z);

		if (GetItemString (ifs, "AtmFogParam", cbuf)) {
			i = sscanf (cbuf, "%lf%lf%lf", &fog.dens_0, &fog.dens_ref, &fog.alt_ref);
			if (GetItemVector (ifs, "AtmFogColor", col0)) fog.col = MakeVECTOR3 (col0);
			else fog.col = atm.color0;
		}
		if (!GetItemVector (ifs, "AtmTintColor", tintcol))
			tintcol.Set (fog.col.x*0.2, fog.col.y*0.2, fog.col.z*0.2);
	}

	GetItemReal (ifs, "HorizonExcess", horizon_excess);
	GetItemReal (ifs, "BBExcess", bb_excess);
	if (GetItemReal (ifs, "ShadowDepth", shadowalpha)) {
		shadowalpha = max(0.0, min (1.0, shadowalpha));
	} else {
		shadowalpha = (AtmInterface ? min (0.8, exp (-0.6*atm.rho0)) : 1.0);
		shadowalpha = min (1.0, max (0.1, shadowalpha));
	}
	shadowcol = 1.0-shadowalpha;
	GetItemReal (ifs, "MinElevation", minelev);
	GetItemReal (ifs, "MaxElevation", maxelev);
	GetItemReal (ifs, "ElevationResolution", elev_res);

	bHasRings = (GetItemReal (ifs, "RingMinRadius", ringmin) &&
		         GetItemReal (ifs, "RingMaxRadius", ringmax));

	if (!GetItemInt (ifs, "TileFormat", tmgr_version) || tmgr_version < 1 || tmgr_version > 2)
		tmgr_version = 1;
	if (!GetItemInt (ifs, "CloudFormat", cmgr_version) || cmgr_version < 1 || cmgr_version > 2)
		cmgr_version = 1;
	if (!GetItemInt (ifs, "LabelFormat", label_version) || label_version < 1 || label_version > 2)
		label_version = 1;

	if (GetItemInt (ifs, "MaxPatchResolution", i))
		max_patch_level = (DWORD)i;
	else
		max_patch_level = 8;
	max_patch_level = min (max_patch_level, (DWORD)SURF_MAX_PATCHLEVEL2);
	max_patch_level = min (max_patch_level, g_pOrbiter->Cfg()->CfgVisualPrm.PlanetMaxLevel);

	bHasCloudlayer = g_pOrbiter->Cfg()->CfgVisualPrm.bClouds &&
		GetItemInt (ifs, "MinCloudResolution", min_cloud_level) && min_cloud_level >= 1;
	if (bHasCloudlayer) {
		if (!GetItemInt (ifs, "MaxCloudResolution", max_cloud_level))
			max_cloud_level = min (8, (int)max_patch_level);
		if (!GetItemBool (ifs, "BrightenClouds", bBrightClouds))
			bBrightClouds = false;
		if (GetItemString (ifs, "CloudMicrotextureAlt", cbuf) &&
			sscanf (cbuf, "%lf%lf", &cloud_micro_alt0, &cloud_micro_alt1) == 2 &&
			cloud_micro_alt0 >= 0 && cloud_micro_alt1 >= cloud_micro_alt0)
				bCloudMicrotex = true;
		if (!GetItemReal (ifs, "CloudAlt", cloudalt)) cloudalt = size * 0.001;
		if (g_pOrbiter->Cfg()->CfgVisualPrm.bCloudShadows && GetItemReal (ifs, "CloudShadowDepth", d))
			cloudshadowcol = (float)d;
		else
			cloudshadowcol = 1.0f;
		if (GetItemReal (ifs, "CloudRotPeriod", crot_t)) {
			crot_offset = fmod (td.MJD0*86400.0, crot_t) * Pi2/crot_t; // arbitrary reference mjd=0
		} else {
			crot_t = -1.0;
			cloudrot = 0.0;
		}
	} else {
		cloudalt = 0.0;
	}

	if (!GetItemBool (ifs, "SpecularRipple", bWaterMicrotex))
		bWaterMicrotex = false;

	navlist.Read (ifs, this);

	// read surface bases from list
	if (FindLine (ifs, "BEGIN_SURFBASE")) {
		char cbuf[256], *nm, *ps;
		double lng, lat;
		for (;;) {
			if (!ifs.getline (cbuf, 256) || !_strnicmp (cbuf, "END_SURFBASE", 12)) break;
			pc = trim_string (cbuf);
			if (!pc[0]) continue;
			if (!_strnicmp (pc, "DIR", 3)) { // scan folder
				ScanBases (trim_string (pc+3));
			} else {                        // read single base definition
				nm = strtok (trim_string(cbuf), ":");
				ps = strtok (NULL, ";");
				if (nm && strlen(nm) && ps && sscanf (ps, "%lf%lf", &lng, &lat) == 2) {
					Base *base = new Base (nm, this, Rad(lng), Rad(lat)); TRACENEW
					if (!AddBase (base))
						delete base;
				} else {
					LOGOUT_WARN("Parse error in configuration file for %s:\nSyntax error in SURFBASE list. Skipping line.", fname);
				}
			}
		}
	} else { // by default, scan folder 'Config/<pname>/Base'
		sprintf (cbuf, "%s/Base", name.c_str());
		ScanBases (cbuf);
	}

	// old style surface basis list
	if (GetItemInt (ifs, "NumBases", n)) { // link bases as children of the planet
		for (i = 0; i < n; i++) {
			char bstr[10], cbuf[256], nm[128];
			double lng, lat;
			sprintf (bstr, "Base%d", i+1);
			if (GetItemString (ifs, bstr, cbuf) &&
				sscanf (cbuf, "%s%lf%lf", nm, &lng, &lat) == 3) {
				Base *base = new Base (nm, this, Rad(lng), Rad(lat)); TRACENEW
				base->Attach (this);
			}
		}
	}
	nnav = navlist.nNav();

	nobserver = 0;
	if (FindLine (ifs, "BEGIN_OBSERVER")) { // read observer positions
		char cbuf[256], *site, *addr, *equp;
		double lng, lat, alt;
		for (;;) {
			if (!ifs.getline (cbuf, 256) || !_strnicmp (cbuf, "END_OBSERVER", 12)) break;
			site = strtok (trim_string(cbuf), ":");
			addr = strtok (NULL, ":");
			equp = strtok (NULL, ";");
			if (!site || !addr || !equp) continue;
			if (sscanf (equp, "%lf%lf%lf", &lng, &lat, &alt) != 3) continue;
			lng *= RAD, lat *= RAD;
			AddObserverSite (lng, lat, alt, site, addr);
		}
	}

	// read user label list
	labellist = 0;
	nlabellist = 0;
	labelpath = 0;
	if (GetItemString (ifs, "MarkerPath", cbuf)) {
		if (cbuf[strlen(cbuf)-1] != '\\') strcat (cbuf, "\\");
		labelpath = new char[strlen(cbuf)+1]; TRACENEW
		strcpy (labelpath, cbuf);
	}
	//if (label_version <= 1)
		ScanLabelLists (ifs);
	//else
		if (label_version > 1)
		ScanLabelLegend();

	Setup();
}

Planet::~Planet ()
{
	int i, j, k;
	DWORD d;

	if (nbase) {
		for (d = 0; d < nbase; d++)
			delete baselist[d];
		delete []baselist;
		baselist = NULL;
	}
	if (nobserver) {
		for (i = 0; i < nobserver; i++) {
			delete []observer[i]->site;
			observer[i]->site = NULL;
			delete observer[i];
		}
		delete []observer;
		observer = NULL;
	}
	if (labellist) {
		delete []labellist;
		labellist = NULL;
		nlabellist = 0;
	}
	if (labelpath) {
		delete []labelpath;
		labelpath = NULL;
	}
	if (nLabelLegend) {
		for (i = 0; i < nLabelLegend; i++) {
			delete []labelLegend[i].name;
			labelLegend[i].name = NULL;
		}
		delete []labelLegend;
		labelLegend = NULL;
	}
	g_pOrbiter->UpdateDeallocationProgress();

	delete emgr;
}

void Planet::ScanBases (char *path)
{
	char cbuf[256], spath[256], *pc, *cut = 0;

	// check for period limiter
	if ((pc = strstr (path, "PERIOD")) != NULL) {
		if (sscanf (pc+6, "%s%s", cbuf, cbuf+128) == 2) {
			double dt;
			if (sscanf (cbuf, "%lf", &dt) == 1 && dt > td.MJD_ref) return;
			if (sscanf (cbuf+128, "%lf", &dt) == 1 && dt < td.MJD_ref) return;
		}
		cut = pc;
	}
	// check for context limiter
	if ((pc = strstr (path, "CONTEXT")) != NULL) {
		if (sscanf (pc+7, "%s", cbuf) == 1) {
			const char *context = g_pOrbiter->PState()->Context();
			if (!context) return;
			if (_stricmp (cbuf, context)) return;
		}
		cut = (cut ? min (cut,pc) : pc);
	}
	if (cut) {
		*cut = '\0';
		trim_string (path);
	}

	sprintf (spath, "%s/dummy", path);
	strcpy (cbuf, g_pOrbiter->ConfigPath(spath));
	fs::path configdir = fs::path(cbuf).parent_path();
	std::error_code ec;
	for (const auto& entry : fs::directory_iterator(configdir, ec)) {
		if (entry.path().extension().string() == ".cfg") {
			ifstream ifs(entry.path());
			if (!ifs) continue;
			do {
				if (!ifs.getline(cbuf, 256)) break;
				pc = trim_string(cbuf);
			} while (!pc[0]);
			if (_strnicmp(pc, "BASE-V2.0", 9)) continue;
			sprintf(spath, "%s\\%s", path, entry.path().stem().string().c_str());
			Base* base = new Base(spath, this); TRACENEW
			if (!AddBase(base))
				delete base;
		}
	}
}

bool Planet::AddBase (Base *base)
{
	if (GetBase (base->Name(), true))
		return false;  // a base of this name exists already

	base->Attach (this);

	Base **tmp = new Base*[nbase+1];
	if (nbase) {
		for (DWORD i = 0; i < nbase; i++)
			tmp[i] = baselist[i];
		delete []baselist;
	}
	baselist = tmp;
	baselist[nbase++] = base;
	return true;
}

void Planet::ScanLabelLists (ifstream &cfg)
{
	int i;
	int nlabellistbuf = 0;
	nlabellist = 0;

	oapi::GraphicsClient::LABELLIST* ll;
	bool scanheader = (labellist == 0); // only need to parse the headers for the initial scan
	ForEach(FILETYPE_MARKER, [&](const fs::directory_entry& entry) {
		char cbuf[256];
		// open marker file
		ifstream ulf(entry.path());

		// read label header
		if (scanheader) {
			if (nlabellist == nlabellistbuf) { // increase buffer
				oapi::GraphicsClient::LABELLIST* tmp = new oapi::GraphicsClient::LABELLIST[nlabellistbuf += 8];
				for (int i = 0; i < nlabellist; i++)
					tmp[i] = labellist[i];
				if (nlabellist) delete[]labellist;
				labellist = tmp;
			}
			ll = labellist + nlabellist;
			ll->name = entry.path().filename().string();
			ll->marker.clear();
			ll->colour = 1;
			ll->shape = 0;
			ll->size = 1.0f;
			ll->distfac = 1.0f;
			ll->active = false;
			ll->flag = 0;
			if (FindLine(ulf, "BEGIN_HEADER")) {
				char item[256], value[256];
				for (;;) {
					if (!ulf.getline(cbuf, 256) || !_strnicmp(cbuf, "END_HEADER", 10)) break;
					sscanf(cbuf, "%s %s", item, value);
					if (!_stricmp(item, "InitialState")) {
						if (!_stricmp(value, "on")) ll->active = true;
					}
					else if (!_stricmp(item, "ColourIdx")) {
						int col;
						sscanf(value, "%d", &col);
						ll->colour = max(0, min(5, col));
					}
					else if (!_stricmp(item, "ShapeIdx")) {
						int shape;
						sscanf(value, "%d", &shape);
						ll->shape = max(0, min(6, shape));
					}
					else if (!_stricmp(item, "Size")) {
						float size;
						sscanf(value, "%f", &size);
						ll->size = max(0.1f, min(2.0f, size));
					}
					else if (!_stricmp(item, "DistanceFactor")) {
						float distfac;
						sscanf(value, "%f", &distfac);
						ll->distfac = max(1e-5f, min(1e3f, distfac));
					}
				}
			}
		}
		else {
			ll = labellist + nlabellist;
		}

		// read label list for active labels, if not already present
		if (ll->active && !ll->marker.size()) {
			int nlistbuf = 0;
			double lng, lat;
			int nl;
			char* pc;
			Vector pos;
			FindLine(ulf, "BEGIN_DATA");
			for (nl = 0;; nl++) {
				if (!ulf.getline(cbuf, 256)) break;
				pc = strtok(cbuf, ":");
				if (!pc || sscanf(pc, "%lf%lf", &lng, &lat) != 2) continue;
				EquatorialToLocal(RAD * lng, RAD * lat, size, pos);
				oapi::GraphicsClient::LABELSPEC ls;
				ls.pos = _V(pos.x, pos.y, pos.z);
				for (i = 0; i < 2; i++) {
					if (pc = strtok(NULL, ":"))
						ls.label[i] = trim_string(pc);
				}
				ll->marker.push_back(ls);
			}
		}
		nlabellist++;

	});
}

void Planet::ScanLabelLegend()
{
	char path[256];
	if (labelpath) strncpy (path, labelpath, 256);
	else           sprintf (path, "%s%s/", g_pOrbiter->Cfg()->CfgDirPrm.ConfigDir, name.c_str());
	strcat (path, "Label.cfg");
	std::ifstream ifs(path);
	while (ifs.good()) {
		char typestr[16], activestr[16], markerstr[16], namebuf[256], *name;
		int r,g,b;
		ifs >> typestr >> activestr >> markerstr >> r >> g >> b;
		if (ifs.good()) {
			ifs.getline(namebuf, 255);
			name = trim_string(namebuf);
			oapi::GraphicsClient::LABELTYPE *tmp = new oapi::GraphicsClient::LABELTYPE[nLabelLegend+1];
			if (nLabelLegend) {
				memcpy(tmp, labelLegend, nLabelLegend*sizeof(oapi::GraphicsClient::LABELTYPE));
				delete []labelLegend;
			}
			labelLegend = tmp;
			labelLegend[nLabelLegend].active = (toupper(activestr[0]) == 'X');
			labelLegend[nLabelLegend].col = RGB(r,g,b);
			labelLegend[nLabelLegend].labelId = typestr[0];
			labelLegend[nLabelLegend].markerId = markerstr[0];
			labelLegend[nLabelLegend].name = new char[strlen(name)+1];
			strcpy(labelLegend[nLabelLegend].name, name);
			nLabelLegend++;
		}
	}
}

void Planet::ActivateLabels(bool activate)
{
}

void Planet::Setup ()
{
	CelestialBody::Setup ();

	if (AtmInterface) {
		atm.C = atm.rho0 * E_grav (mass, size*size) / atm.p0;
		atm.radlimit = atm.altlimit + size;
	}

	bEnableWind = g_pOrbiter->Cfg()->CfgPhysicsPrm.bAtmWind;
	// should be done only once rather than for every planet

	if (tmgr_version == 2)
		emgr = new ElevationManager(this);
	for (DWORD i = 0; i < nbase; i++)
		baselist[i]->Setup();
}

const void *Planet::GetParam (DWORD paramtype) const
{
	switch (paramtype) {
	case OBJPRM_PLANET_SURFACEMAXLEVEL:
		return (const void*)&max_patch_level;
	case OBJPRM_PLANET_SURFACERIPPLE:
		return (const void*)&bWaterMicrotex;
	case OBJPRM_PLANET_HAZEEXTENT:
		return (const void*)&hazerange;
	case OBJPRM_PLANET_HAZEDENSITY:
		return (const void*)&hazedens;
	case OBJPRM_PLANET_HAZESHIFT:
		return (const void*)&hazeshift;
	case OBJPRM_PLANET_HAZECOLOUR:
		return (const void*)&hazecol;
	case OBJPRM_PLANET_FOGPARAM:
		return (const void*)&fog;
	case OBJPRM_PLANET_SHADOWCOLOUR:
		return (const void*)&shadowcol;
	case OBJPRM_PLANET_HASCLOUDS:
		return (const void*)&bHasCloudlayer;
	case OBJPRM_PLANET_CLOUDALT:
		return (const void*)&cloudalt;
	case OBJPRM_PLANET_CLOUDROTATION:
		return (const void*)&cloudrot;
	case OBJPRM_PLANET_CLOUDSHADOWCOL:
		return (const void*)&cloudshadowcol;
	case OBJPRM_PLANET_CLOUDMICROTEX:
		return (const void*)&bCloudMicrotex;
	case OBJPRM_PLANET_CLOUDMICROALTMIN:
		return (const void*)&cloud_micro_alt0;
	case OBJPRM_PLANET_CLOUDMICROALTMAX:
		return (const void*)&cloud_micro_alt1;
	case OBJPRM_PLANET_HASRINGS:
		return (const void*)&bHasRings;
	case OBJPRM_PLANET_RINGMINRAD:
		return (const void*)&ringmin;
	case OBJPRM_PLANET_RINGMAXRAD:
		return (const void*)&ringmax;
	case OBJPRM_PLANET_ATTENUATIONALT:
		return (const void*)&atm_attenuationalt;
	case OBJPRM_PLANET_TILEENGINE:
		return (const void*)&tmgr_version;
	case OBJPRM_PLANET_CLOUDTILEENGINE:
		return (const void*)&cmgr_version;
	case OBJPRM_PLANET_LABELENGINE:
		return (const void*)&label_version;
	case OBJPRM_PLANET_ATMTINTCOLOUR:
		return (const void*)&tintcol;
	case OBJPRM_PLANET_CLOUDMAXLEVEL:
		return (const void*)&max_cloud_level;
	case OBJPRM_PLANET_CLOUDOVERSATURATE:
		return (const void*)&bBrightClouds;
	case OBJPRM_PLANET_HORIZONEXCESS:
		return (const void*)&horizon_excess;
	case OBJPRM_PLANET_TILEBBEXCESS:
		return (const void*)&bb_excess;
	case OBJPRM_PLANET_MINELEVATION:
		return (const void*)&minelev;
	case OBJPRM_PLANET_MAXELEVATION:
		return (const void*)&maxelev;
	case OBJPRM_PLANET_ELEVRESOLUTION:
		return (const void*)&elev_res;
	}
	return Body::GetParam (paramtype);
}

void Planet::InitDeviceObjects ()
{
}

void Planet::DestroyDeviceObjects ()
{
	for (DWORD i = 0; i < nbase; i++)
		baselist[i]->DestroyDeviceObjects ();
	CelestialBody::DestroyDeviceObjects ();
}

void Planet::ElToEcliptic (const Elements *el_equ, Elements *el_ecl) const
{
	// this should be done by direct transform rather
	// than going via pos/vel

	Vector pos, vel;
	el_equ->PosVel (pos, vel, 0.0);
	el_ecl->Calculate (mul (GRot(), pos), mul (GRot(), vel), 0.0);
}

void Planet::BeginStateUpdate ()
{
	CelestialBody::BeginStateUpdate();
	for (DWORD i = 0; i < nbase; i++)
		baselist[i]->BeginStateUpdate();
}

void Planet::EndStateUpdate ()
{
	CelestialBody::EndStateUpdate();
	for (DWORD i = 0; i < nbase; i++)
		baselist[i]->EndStateUpdate();
}

void Planet::Update (bool force)
{
	if (bHasCloudlayer) {
		if (crot_t > 0)
			cloudrot = fmod (td.SimT1, crot_t) * Pi2/crot_t + crot_offset;
	}

	CelestialBody::Update (force);

	// Update bases
	for (DWORD i = 0; i < nbase; i++)
		baselist[i]->Update (force);

}

void Planet::AddObserverSite (double lng, double lat, double alt, char *site, char *addr)
{
	GROUNDOBSERVERSPEC **tmp = new GROUNDOBSERVERSPEC*[nobserver+1]; TRACENEW
	if (nobserver) {
		memcpy (tmp, observer, nobserver*sizeof(GROUNDOBSERVERSPEC*));
		delete []observer;
	}
	observer = tmp;
	observer[nobserver] = new GROUNDOBSERVERSPEC; TRACENEW
	observer[nobserver]->lng = lng;
	observer[nobserver]->lat = lat;
	observer[nobserver]->alt = alt;
	observer[nobserver]->site = new char[strlen(site)+strlen(addr)+2]; TRACENEW
	strcpy (observer[nobserver]->site, site);
	observer[nobserver]->addr = observer[nobserver]->site + (strlen(site)+1);
	strcpy (observer[nobserver]->addr, addr);
	nobserver++;
}

const GROUNDOBSERVERSPEC *Planet::GetGroundObserver (char *site, char *addr) const
{
	for (int i = 0; i < nobserver; i++) {
		if (!_stricmp (site, observer[i]->site) && !_stricmp (addr, observer[i]->addr)) {
			return observer[i];
		}
	}
	return 0;
}

Base *Planet::GetBase (const char *_name, bool ignorecase)
{
	for (DWORD i = 0; i < nbase; i++)
		if (!StrComp (baselist[i]->Name(), _name, ignorecase))
			return baselist[i];
	return 0;
}

const Base *Planet::GetBase (const char *_name, bool ignorecase) const
{
	for (DWORD i = 0; i < nbase; i++)
		if (!StrComp (baselist[i]->Name(), _name, ignorecase))
			return baselist[i];
	return 0;
}

bool Planet::GetAtmParam (double alt, double lng, double lat, ATMPARAM *prm) const
{
	if (!AtmInterface || alt > atm.altlimit) {
		prm->T = prm->p = prm->rho = 0.0;
		return false;
	} else {
		switch (AtmInterface) {
		case 1:
			prm->T = 288.16;
			prm->p = atm.p0 * exp (-atm.C * alt);
			prm->rho = prm->p * atm.rho0/atm.p0;
			return true;
		case 2:
			modIntf.oplanetAtmPrm (alt, prm);
			return true;
		case 3:
			module->clbkAtmParam (alt, prm);
			return true;
		case 4: {
			ATMOSPHERE::PRM_IN prm_in;
			ATMOSPHERE::PRM_OUT prm_out;
			prm_in.alt = alt;
			prm_in.lng = lng;
			prm_in.lat = lat;
			prm_in.flag = ATMOSPHERE::PRM_ALT | ATMOSPHERE::PRM_LNG | ATMOSPHERE::PRM_LAT;

#ifdef UNDEF
// Timing test!!!
static bool do_test=true;
if (do_test && SimT > 10) {
tic();
for (int i = 0; i < 1000; i++) {
	prm_in.alt = 1000e3+i;
	((CELBODY2*)module)->GetAtmosphere()->clbkParams (&prm_in, &prm_out);
}
sprintf (DBG_MSG, "timing=%0.8g", toc());
do_test=false;
}
#endif

			((CELBODY2*)module)->GetAtmosphere()->clbkParams (&prm_in, &prm_out);
			prm->p = prm_out.p;
			prm->rho = prm_out.rho;
			prm->T = prm_out.T;
			} return true;
		default:
			return false;
		}
	}
}

double Planet::Elevation (double lng, double lat) const
{
	return (emgr ? emgr->Elevation(lat,lng) : 0.0);
}

Vector Planet::GroundVelocity (double lng, double lat, double alt, int frame)
{
	if (frame < 2 || frame > 3) return Vector(0,0,0); // sanity check

	double vground = Pi2 * (size+alt) * cos(lat) / RotT();
	Vector v(-vground*sin(lng), 0.0, vground*cos(lng));
	if (frame == 2) return v;
	else            return mul (s0->R, v) + s0->vel;
}

Vector Planet::WindVelocity (double lng, double lat, double alt, int frame, WindPrm *prm, double *windspeed)
{
	Vector wv(0,0,0);

	if (bEnableWind && HasAtmosphere()) {
		
		int k, dim;
		DWORD r, rnd;
		Vector wv0[4];

		// cubic interpolation
		double alt_km = alt*1e-3;
		double alt0 = floor(alt_km);
		double mk0, mk1, t, t2, t3, h00, h10, h01, h11;
		t = alt_km-alt0;
		t3 = (t2 = t*t)*t;
		h00 = 2.0*t3 - 3.0*t2 + 1.0;
		h10 = t3 - 2.0*t2 + t;
		h01 = -2.0*t3 + 3.0*t2;
		h11 = t3-t2;
		r = (DWORD)alt0;
		for (k = 0; k < 4; k++) {
			oapi_srand(g_rseed[(r+k)%100]);
			for (dim = 0; dim < 3; dim+=2) {
				rnd = oapi_rand();
				wv0[k].data[dim] = ((double)rnd/OAPI_RAND_MAX-0.5)*50;
				//pert = ((double)(g_rseed[((DWORD)(t*100)+dim+k+r)%100]>>16)/OAPI_RAND_MAX-0.5)*20;
				//wv0[k].data[dim] += pert;
			}
		}
		for (dim = 0; dim < 3; dim+=2) {
			mk0 = 0.5 * (wv0[2].data[dim] - wv0[0].data[dim]);
			mk1 = 0.5 * (wv0[3].data[dim] - wv0[1].data[dim]);
			wv.data[dim] = h00*wv0[1].data[dim] + h10*mk0 + h01*wv0[2].data[dim] + h11*mk1;
		}

		// short-term temporal perturbations
		if (prm) {
			static const double corr_length = 1.0;   // should be planet-specific?
			static const double pert_amplitude = 10.0; // should be planet-specific, altitude-specific, etc.
			double tp = td.SimT1;
			double t_dist = fabs(tp-prm->pert_t);
			double corr = 1.0 - min (1.0, t_dist/corr_length);
			for (dim = 0; dim < 3; dim += 2) {
				double p = ((double)rand()/(double)RAND_MAX - 0.5)*pert_amplitude; // make this a normal distribution
				if (corr)
					p = corr * prm->pert_v.data[dim] + (1.0-corr) * p;
				prm->pert_v.data[dim] = p;
				wv.data[dim] += p;
			}
			prm->pert_t = tp;
		}

#ifdef UNDEF
		double fac1 = alt*1e-3 - alt0;
		double fac0 = 1.0-fac1;
		


		rnd0 = (DWORD)alt0+12345;
		rnd1 = rnd0+1;
		rnd0 = oapi_rand(rnd0); rnd1 = oapi_rand(rnd1);
		for (i = 0; i < 2; i++) {
			rnd0 = oapi_rand(rnd0);
			wva.data[i] = ((double)rnd0/OAPI_RAND_MAX-0.5)*100;
		}
		for (i = 0; i < 2; i++) {
			rnd1 = oapi_rand(rnd1);
			wv.data[i] = wva.data[i]*fac0 + ((double)rnd1/OAPI_RAND_MAX-0.5)*100 * fac1;
		}
#endif

	}

	if (windspeed) *windspeed = wv.length();
	if (frame == 0) return wv;  // surface-local frame

	double slng = sin(lng), clng = cos(lng), slat = sin(lat), clat = cos(lat);
	Vector wv_loc (tmul (Matrix (-slng,0,clng, clat*clng,slat,clat*slng, -slat*clng,clat,-slat*slng), wv));

	switch (frame) {
	case 1: // planet-local
		return wv_loc;
	case 2: // planet-local non-rotating
		return wv_loc + GroundVelocity(lng,lat,alt,2);
	case 3:  // global
		return mul (s0->R, wv_loc + GroundVelocity (lng,lat,alt,2)) + s0->vel;
	default:
		// this should not happen
		return wv;
	}

#ifdef UNDEF
	// arbitrary
	const ns = 5;
	static double a[ns] = {0, 1e3, 2e3, 5e3, 10e3};
	static Vector v[ns] = {
		Vector ((rand1()-0.5)*100,0,(rand1()-0.5)*100),
		Vector ((rand1()-0.5)*100,0,(rand1()-0.5)*100),
		Vector ((rand1()-0.5)*200,0,(rand1()-0.5)*200),
		Vector ((rand1()-0.5)*200,0,(rand1()-0.5)*200),
		Vector ((rand1()-0.5)*200,0,(rand1()-0.5)*200)
	};
	int i;
	for (i = 1; i < ns; i++) {
		if (alt < a[i]) return (v[i-1]*(a[i]-alt) + v[i]*(alt-a[i-1]))/(a[i]-a[i-1]);
	}
	return v[ns-1];
#endif
}

#ifdef UNDEF
void Planet::RegisterModule (char *dllname)
{
	char cbuf[256], funcname[256], *funcp;
	sprintf (cbuf, "Modules\\%s.dll", dllname);
	hMod = LoadLibrary (cbuf);
	if (!hMod) return;
	strcpy (funcname, name); funcp = funcname + strlen(name);

	strcpy (funcp, "_SetPrecision");
	modIntf.oplanetSetPrecision = (OPLANET_SetPrecision)GetProcAddress (hMod, funcname);

	strcpy (funcp, "_Ephemeris");
	modIntf.oplanetEphemeris = (OPLANET_Ephemeris)GetProcAddress (hMod, funcname);

	strcpy (funcp, "_FastEphemeris");
	modIntf.oplanetFastEphemeris = (OPLANET_FastEphemeris)GetProcAddress (hMod, funcname);

	strcpy (funcp, "_AtmPrm");
	modIntf.oplanetAtmPrm = (OPLANET_AtmPrm)GetProcAddress (hMod, funcname);
}

void Planet::ClearModule ()
{
	if (hMod) {
		FreeLibrary (hMod);
		hMod = 0;
	}
	memset (&modIntf, 0, sizeof (modIntf));
}
#endif