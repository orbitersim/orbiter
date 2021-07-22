// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Base - class for planetary surface bases

#ifndef __BASE_H
#define __BASE_H

#include "Body.h"
#include "Nav.h"

class Planet;
class PlanetarySystem;
class Vessel;
class Mesh;
class BaseObject;
class Base;
struct SurftileSpec;

typedef struct {
	Vector relpos;
	int status;           // 0=free, 1=occupied, 2=booked
	VesselBase *vessel;   // vessel assigned to pad
	Nav_VTOL *nav;        // pointer to NAV transmitter
} LpadSpec;

typedef struct {
	double lng1, lat1;    // runway end point 1 (and ILS reference point 1)
	double lng2, lat2;    // runway end point 2 (and ILS reference point 2)
	double appr1;         // approach angle to p1 (= angle to p2+pi) [rad]
	double length;        // runway length [m]
	Nav_ILS *ils1, *ils2; // ILS definitions (0 if N/A)
} RwySpec;

// =======================================================================
// Class Base

class Base: public Body {
	friend class VBase;

public:
	Base (char *fname, Planet *_planet, double _lng = 0, double _lat = 0);
	// create a Base from a config file
	// lng and lat are only used if not defined in the config file

	virtual ~Base ();

	int Type () const { return OBJTP_SURFBASE; }

	void Setup();

	virtual void Update (bool force = false);

	virtual void Attach (Planet *_parent);
	// Add *this as a child to "_parent"

	const Planet *RefPlanet () const { return (Planet*)cbody; }
	// The planet the base is located on

	void EquPos (double &_lng, double &_lat) const
	{ _lng = lng, _lat = lat; }
	// Return equatoral coordinates

	inline double Elevation() const { return elev; }
	// Return mean base elevation

	void Rel_EquPos (const Vector &relpos, double &_lng, double &_lat) const;
	// converts a base-relative position into longitude/latitude

	void Pad_EquPos (DWORD padno, double &_lng, double &_lat) const;
	// Return equatorial coordinates of landing pad 'padno'

	inline DWORD nPad() const { return npad; }
	inline LpadSpec *PadStatus (int padno) { return lspec+padno; }
	inline const LpadSpec *PadStatus (int padno) const { return lspec+padno; }
	// number of landing pads and pad status

	inline DWORD nRwy() const { return nrwy; }
	inline RwySpec *RwyStatus (DWORD rwyno) { return rwy+rwyno; }
	inline const RwySpec *RwyStatus (DWORD rwyno) const { return rwy+rwyno; }

	inline DWORD nNav() const { return nnav; }
	inline const NavManager &NavMgr() const { return navlist; }

	inline DWORD nVOR() const { return nvor; }
	inline Nav *VOR (DWORD i) { return vor[i]; }
	inline const Nav *VOR (DWORD i) const { return vor[i]; }

	int LandedAtPad (const Vessel *vessel) const;
	// returns padno at which 'vessel' is landed, or -1 if not

	int OccupyPad (Vessel *vessel, int pad = -1, bool forcepad = false);
	// Mark 'pad' as occupied by 'vessel'
	// if pad < 0, pick random free pad
	// return actual pad number, or -1 if no free pad
	// if forcepad=false and pad is occupied, pick random free pad

	int OccupyPad (Vessel *vessel, double _lng, double _lat);
	// select pad containing point (_lng,_lat) and mark as occupied.
	// return pad no, or -1 if pad not free or point not owned by any pad

	void ClearPad (Vessel *vessel);
	// mark the pad occupied by or reserved for 'vessel' as free

	int RequestLanding (Vessel *vessel, DWORD &padno);
	int RequestTakeoff ();
	// Request for landing/takeoff. Return value is result:
	// 1=denied, 2=still pending, 3=granted.
	// if landing clearance is given, padno contains allocated landing pad

	void ReportTakeoff (Vessel *vessel);
	// 'vessel' reports lift-off

	int ReportTouchdown (VesselBase *vessel, double vlng, double vlat);
	// 'vessel' reports touchdown at position (vlng,vlat)

	double CosSunAlt () const;
	// Return cosine of angle between surface normal and direction to
	// the sun (which is assumed in the centre of the global coord system)

	inline Vector SunDirection () const
	{ return (s1 ? tmul(s1->R, -s1->pos.unit()) : tmul (s0->R, -s0->pos.unit())); }
	// Return vector pointing towards sun (= world coordiate origin) in
	// base local coordinates

	inline const Vector *SunDirectionBuffered () const
	{ return &sundir; }
	// Return vector pointing towards sun (= world coordiate origin) in
	// base local coordinates.
	// This version returns a stored value that is updated in regular intervals

	inline D3DCOLOR ShadowColor () const { return D3DRGBA(0,0,0,0.7); }
	// colour and transparency of shadows. Make this planet-specific

	DWORD GetTileList (const SurftileSpec **_tile) const;

	bool GetGenericTexture (LONGLONG id, SURFHANDLE &daytex, SURFHANDLE &nighttex) const;
	// return day and night textures for given texture id
	// if not found, returns false and sets both textures to NULL

	DWORD GetGenericTextureIdx (LONGLONG id) const;
	// return index of generic textures corresponding to 'id'

	void ExportBaseStructures (Mesh ***mesh_us, DWORD *nmesh_us, Mesh ***mesh_os, DWORD *nmesh_os) const;
	// Export base structures as mesh lists.
	// Includes mesh objects as well as object primitives. Primitives are
	// compacted into a single mesh.
	// Mesh lists are separated into objects rendered under/above ground shadows

	void ExportShadowGeometry (Mesh ***mesh_shadow, double **elev, DWORD *nmesh_shadow) const;
	// Export base structures as mesh lists. This version only exports
	// "above shadow" objects, without textures and materials, and does not
	// restructure the object elements. Can be used for shadow projection
	// calculations

	inline bool MapObjectsToSphere() const { return bObjmapsphere; }
	// map base objects onto spherical planet surface?

	void DestroySurfaceTiles ();
	// Destroy the meshes and textures of the high-res surface tiles

	static void CreateStaticDeviceObjects ();
	static void DestroyStaticDeviceObjects ();

	void InitDeviceObjects ();
	void DestroyDeviceObjects ();
	// Load and free textures required by the planet's visual

private:
	//const Planet *planet;
	double rad, lng, lat;          // equatorial coords of the base
	double elev;                   // mean elevation of the base
	Vector rpos;                   // base position in local planet frame
	Matrix rrot;                   // rotation matrix in local planet frame
	double objscale;               // size of "typical" base object (for camera-distance dependent render cutoff)
	bool bObjmapsphere;            // map base objects onto spherical planet surface?
	Vector rotvel;                 // base velocity as result of planet rotation in local planet coords (rotvel.y=0)

	DWORD npad;                    // number of landing pads
	int padfree;                   // number of available (unoccupied) pads
	LpadSpec *lspec;               // list of landing pads

	DWORD nrwy;                    // number of runways
	RwySpec *rwy;                  // list of runways

	DWORD nnav;                    // number of nav transmitters
	NavManager navlist;            // list of nav transmitters

	DWORD nvor;                    // number of associated VOR transmitters
	Nav **vor;                     // list of VOR transmitters

	BaseObject **obj;              // list of base objects
	DWORD nobj;                    // number of base objects

	mutable Mesh *genmsh_os, *genmsh_us; // meshes for generic base structures (above/below shadows)
	mutable Mesh **objmsh_os, **objmsh_us; // meshes for base structures (above/below shadows)
	mutable Mesh **objmsh_sh;              // meshes for shadow projection calculations
	mutable double *sh_elev;               // object elevation (for shadow projection calculation)
	mutable DWORD nobjmsh_os, nobjmsh_us, nobjmsh_sh; // list lenghts
	mutable bool objmsh_valid;

	SurftileSpec *tile;            // list of surface tiles
	DWORD ntilebuf;                // list length
	DWORD ntile;                   // number of surface tiles

	Vector sundir;                 // sun direction in base coordinates
	double sundir_updt;            // time of next buffered sun direction update

	// common resources
	static char **generic_mesh_name;         // list of names for generic meshes
	static Mesh *generic_obj_mesh;           // meshes for generic objects
	static int ngenericmesh;                 // number of generic meshes
	static char **generic_tex_name;          // list of names for generic textures
	static LONGLONG *generic_tex_id;         // list of generic texture ids (only uses first 8 characters of texture name!)
	static SURFHANDLE *generic_dtex;         // generic daytime textures
	static SURFHANDLE *generic_ntex;         // generic nighttime textures
	static int ngenerictex;                  // number of generic textures

	bool InitSurfaceTiles () const;
	// Initialise/destroy meshes and textures for high-res base surface tiles

	void ScanObjectMeshes () const;
	// Import object meshes from individual base objects
};

#endif // !__BASE_H