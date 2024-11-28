// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Planet - Logical interface
//
// Notes: Planets (including moons) are assumed top level objects, i.e.
// parent = NULL. Therefore rpos = gpos
// The Attach method only defines the central body (cbody) without linking
// to it as a child.
// Planets can be updated either directly from their elements or dynamically
// from the graviational fields. (definition file option ELLIPTIC_ORBIT)
// Independently, the initial position can be defined by elements or directly
// by a position/velocity vector. (definition file option HAS_ELEMENTS)
// =======================================================================

#ifndef __PLANET_H
#define __PLANET_H

#include "Celbody.h"
#include "Nav.h"
#include "GraphicsAPI.h"
#include "Orbiter.h"
#include <functional>
#include <filesystem>
namespace fs = std::filesystem;

#define FILETYPE_MARKER 1

class PlanetarySystem;
class Star;
class Elements;
class Base;
class TileManager;
class SurfTile;
class CloudTile;
class ElevationManager;

struct _finddata_t;

#pragma pack(push,1)

typedef struct {          // texture tile hierarchy element
	WORD nSubtile;        // number of children at next higher level
	bool bCover;          // children cover tile completely
	DWORD ChildFlag[4];   // subtile flags. bit0: subtile area contains land; bit1: subtile area contains water; bit2: subtile is present
	DWORD ChildIdx[4];    // texture indices for children ((DWORD)-1: child subtile not present)

	// the following fields are transient visibility flags which change
	// with each frame
	bool bVisibilityValid;// the visibility flags are up to date for current frame
	bool bVisible;        // the tile is visible
	bool bChildVis[4];    // child tile is visible in viewport
} TILESPEC;

#pragma pack(pop)

// bitflag masks for TILESPEC::ChildFlag
#define TILESPEC_ISOPAQUE 1        // tile area contains opaque regions
#define TILESPEC_ISTRANSPARENT 2   // tile area contains transparent regions
#define TILESPEC_TILEPRESENT 4     // child tile is present

typedef struct {
	double lng, lat, alt;
	char *site, *addr;
} GROUNDOBSERVERSPEC;

//typedef struct {
//	double lng, lat;
//	char *label[2];
//} USERLABELSPEC;

// =======================================================================
// Class Planet

class Planet: public CelestialBody {
	friend class VPlanet;
	friend class TileManager;
	template<class T> friend class TileManager2;

public:
	Planet (double _mass, double _mean_radius);
	// create a new planet

	Planet (char *fname);
	// create a planet from a config file

	virtual ~Planet ();

	void Setup ();

	int Type() const { return OBJTP_PLANET; }

	const void *GetParam (DWORD paramtype) const;

	void SetPsys (const PlanetarySystem *_psys)
	{ psys = _psys; }

	const PlanetarySystem *GetPsys () const
	{ return psys; }

	inline DWORD nNav() const { return nnav; }
	inline NavManager &NavMgr() { return navlist; }
	inline const NavManager &NavMgr() const { return navlist; }

	inline DWORD nBase() const { return nbase; }

	bool AddBase (Base *_base);
	// Add _base to planet's base list. Returns false if base with this name already exists

	Base *GetBase (DWORD i) const { return baselist[i]; }
	// return the planet's i'th surface base (assuming all children are bases)

	Base *GetBase (const char *_name, bool ignorecase = false);
	const Base *GetBase (const char *_name, bool ignorecase = false) const;
	// return base 'name' or 0 if doesn't exist

	void ScanBases (char *path);
	// create surface bases by scanning config files in directory 'path'

	void BeginStateUpdate ();
	void EndStateUpdate ();

	void Update (bool force = false);
	// Perform time step

	void ElToEcliptic (const Elements *el_equ, Elements *el_ecl) const;
	// Transforms orbital elements from planet equatorial
	// reference to ecliptic reference

	inline bool HasAtmosphere() const { return AtmInterface != 0; }
	inline const ATMCONST *AtmParams () const { return (AtmInterface ? &atm : 0); }
	// atmospheric parameters

	bool GetAtmParam (double alt, double lng, double lat, ATMPARAM *prm) const;
	// returns atmospheric parameters as a function of altitude from mean radius and
	// geographic position

	inline double AtmSoundSpeed (double T) const
	{ return (AtmInterface ? sqrt (atm.gamma * atm.R * T) : 0.0); }
	// returns speed of sound as a function of absolute temperature

	inline double AtmRadLimit () const { return atm.radlimit; }
	inline double AtmAltLimit () const { return atm.altlimit; }
	inline double AtmAttenuationLimit () const { return atm_attenuationalt; }
	inline double AtmHazeRange () const { return hazerange; }
	inline double AtmHazeShift () const { return hazeshift; }
	inline double AtmHazeDensity () const { return hazedens; }
	inline const Vector &AtmHazeColor () const { return hazecol; }
	inline double ShadowDepth () const { return shadowalpha; }
	inline double CloudRotationAngle () const { return cloudrot; }
	inline float CloudShadowDepth () const { return cloudshadowcol; }
	inline DWORD MaxPatchLevel() const { return max_patch_level; }
	inline double BBExcess() const { return bb_excess; }
	inline double ElevationResolution() const { return elev_res; }
	inline int LabelFormat() const { return label_version; }
	inline const oapi::GraphicsClient::LABELTYPE *LabelLegend() const { return labelLegend; }
	inline int NumLabelLegend() const { return nLabelLegend; }

	void SetLabelActive(int i, bool active)
	{ if (i >= 0 && i < nLabelLegend) labelLegend[i].active = active; }

	Vector GroundVelocity (double lng, double lat, double alt=0.0, int frame=2);
	Vector WindVelocity (double lng, double lat, double alt, int frame=0, WindPrm *prm=NULL, double *windspeed=NULL);
	// returns a velocity vector in local planet coordinates for ground/air at a point given
	// in equatorial coordinates

	bool CloudParam (double &_cloudalt) const
	{ _cloudalt = cloudalt; return bHasCloudlayer; }

	double Elevation (double lng, double lat) const;
	// returns surface elevation at lng/lat w.r.t. planet radius (size)

	GROUNDOBSERVERSPEC const *GetGroundObserver (char *site, char *addr) const;
	GROUNDOBSERVERSPEC const *GetGroundObserver (int idx) const { return observer[idx]; }
	int nGroundObserver() const { return nobserver; }
	// return the location of a ground observer site

	void InitDeviceObjects ();
	void DestroyDeviceObjects ();
	// Load and free textures required by the planet's visual

	TileManager *TileMgr() const { return tmgr; }
	TileManager2<SurfTile> *SurfMgr2() const { return smgr2; }
	ElevationManager *ElevMgr() const { return emgr; }
	TileManager2<CloudTile> *CloudMgr2() const { return cmgr2; }
	void ForEach(int type, std::function<void(const fs::directory_entry&)> callback) {
		extern Orbiter* g_pOrbiter;
		fs::path path;
		std::string ext;
		switch (type) {
		case FILETYPE_MARKER:
			ext = ".mkr";
			if (labelpath) path = labelpath;
			else           path = fs::path(g_pOrbiter->Cfg()->CfgDirPrm.ConfigDir) / name / "Marker";
			break;
		}
		std::error_code ec;
		for (const auto& entry : fs::directory_iterator(path, ec)) {
			if (entry.path().extension().string() == ext) {
				callback(entry);
			}
		}
	}

	//USERLABELSPEC const *GetUserLabel (int idx) const { return userlabel+idx; }
	//int nUserLabel () const { return nuserlabel; }
	//struct LABELLIST {
	//	int length;
	//	int colour;
	//	int shape;
	//	float size;
	//	float distfac;
	//	bool active;
	//};
	oapi::GraphicsClient::LABELLIST *LabelList (int *nlist = 0) const { if (nlist) *nlist = nlabellist; return labellist; }
	void ScanLabelLists (std::ifstream &cfg);
	// read surface label lists as defined in config file

	void ScanLabelLegend();

	void ActivateLabels(bool activate);

protected:

	int AtmInterface;        // atmospheric data interface:
	                         // 0: no atmosphere
	                         // 1: use simple exponential profile
							 // 2: use obsolete <Planet>_AtmPrm module function
	                         // 3: use CELBODY::clbkAtmParam interface
	                         // 4: use CELBODY2::clbkAtmParam interface
	ATMCONST atm;            // atmospheric parameters	
	double atm_attenuationalt; // altitude limit for calculation of light attenuation on vessels (should be moved into ATMCONST!)

	bool bHasCloudlayer;     // planet has separate cloud layer
	bool bBrightClouds;      // oversaturate cloud brightness?
	bool bCloudMicrotex;     // use micro-textures for clouds
	bool bWaterMicrotex;     // use micro-textures for water surfaces ("specular ripples")
	double cloudalt;         // altitude of cloud layer
	double cloudrot;         // cloud layer rotation state
	double cloud_micro_alt0, cloud_micro_alt1; // altitude range for cloud micro textures
	double hazerange;        // bleed-in factor of horizon haze into planet disc (0-0.9, 0=none)
	double hazeshift;        // shift of horizon haze reference radius (units of planet radius, default=0)
	double hazedens;         // horizon haze density factor
	Vector hazecol;          // horizon haze colour
	Vector tintcol;          // atmospheric tint colour
	double minelev;          // minimum elevation as read from config file
	double maxelev;          // maximum elevation as read from config file
	double crot_t, crot_offset; // cloud layer rotation time and offset
	double shadowalpha;      // depth of object shadows (1=black, 0=none)
	double shadowcol;        // depth of object shadows (0=black, 1=none) OBSOLETE
	float cloudshadowcol;    // depth of cloud shadows (0=black, 1=none)
	bool bHasRings;          // planet has ring system
	double ringmin, ringmax; // inner/outer ring radii
	double horizon_excess;   // specifies how far beyond the sphere-based horizon to render tiles (to avoid mountains to disappear) 0..infty
	double bb_excess;        // specifies how much to inflate the bounding box (1=double each side)
	FogParam fog;            // distance fog render parameters
	static bool bEnableWind; // allow atmospheric wind effects
	oapi::GraphicsClient::LABELTYPE *labelLegend;  // label type legend (label_version >= 2)
	int nLabelLegend;        // number of entries in legend

private:
	void AddObserverSite (double lng, double lat, double alt, char *site, char *addr);
	// add the position of a surface observer camera to the list

	const PlanetarySystem *psys; // system the planet belongs to

	DWORD nbase;
	Base **baselist;
	// list of surface bases

	int nnav;
	NavManager navlist;
	// list of nav transmitters

	int nobserver;
	GROUNDOBSERVERSPEC **observer;

	oapi::GraphicsClient::LABELLIST *labellist;
	int nlabellist;
	char *labelpath;

	DWORD max_patch_level;
	// resolution limit for sphere patch representation of planet's visual (<= 10)

	int min_cloud_level;
	int max_cloud_level;
	// separate cloud layer is rendered from this resolution

	LPDIRECTDRAWSURFACE7 *cloudtex;   // textures for cloud layer - TODO: replace with SURFHANDLE!
	int ncloudtex;

	LPDIRECTDRAWSURFACE7 *ringtex;    // textures for ring system (if bHasRings) - TODO: replace with SURFHANDLE!
	int nringtex;

	int tmgr_version;                 // which tile manager?
	int cmgr_version;                 // which cloud manager?
	int label_version;				  // what method to render surface labels?
	TileManager *tmgr;                // surface tile manager (legacy)
	TileManager2<SurfTile> *smgr2;    // surface tile manager
	TileManager2<CloudTile> *cmgr2;   // cloud layer manager
	ElevationManager *emgr;           // elevation manager
	double elev_res;                  // target elevation resolution [m]
};

#endif // !__PLANET_H