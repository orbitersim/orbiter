// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Planetary system - Logical interface

#ifndef __PSYS_H
#define __PSYS_H

#include "Body.h"
#include "Star.h"
#include "Planet.h"
#include "Base.h"
#include "GraphicsAPI.h"
#include <iostream>

#define FILETYPE_MARKER 1

class Vessel;
class SuperVessel;

class PlanetarySystem {
	friend class Body;
	friend class SuperVessel;

public:
	PlanetarySystem (char *fname);
	// create a planetary system from a config file

	~PlanetarySystem ();

	const char *Name() const { return name; }

	void Clear ();
	// Remove all objects from the system

	void InitState (const char *fname);
	// Init psys from a scenario file

	void PostCreation ();

	void Write (std::ostream &os);
	// Write list of current vessel states to scenario stream

	DWORD nObj() const { return nbody; }
	Body *GetObj (const char *name, bool ignorecase = false);
	Body *GetObj (int i) const { return body[i]; }
	// Return pointer to object "name" (gravbody, station,
	// etc.) or 0 if not present

	DWORD nGrav() const { return ngrav; }
	CelestialBody *GetGravObj (const char *name, bool ignorecase = false) const;
	inline CelestialBody *GetGravObj (int i) const { return grav[i]; }
	// Return pointer to 'massive' object by name or index, or 0 if not present

	int nPlanet() const { return nplanet; }
	Planet *GetPlanet (const char *name, bool ignorecase = false);
	Planet *GetPlanet (int i) const { return planet[i]; }
	// Return pointer to planet-type object (planet or moon)

	DWORD nStar() const { return nstar; }
	Star *GetStar (int i) const { return star[i]; }

	inline DWORD nVessel() const { return nvessel; }
	Vessel *GetVessel (const char *name, bool ignorecase = false) const;
	inline Vessel *GetVessel (DWORD i) const { return vessel[i]; }
	// Return pointer to vessel by name or index, or 0 if not present

	bool isObject (const Body *obj) const;
	// returns true if obj is a registered object

	bool isVessel (const Vessel *v) const;
	// returns true if v is a registered vessel

	DWORD nBase(const Planet *planet) const { return planet->nBase(); }
	Base *GetBase (const Planet *planet, DWORD i) { return planet->GetBase(i); }
	Base *GetBase (const Planet *planet, const char *name, bool ignorecase = false);
	// Return pointer to planet's base by name or index

	Base *GetBase (const char *name, bool ignorecase = false);
	// search through the bases of all planets
	// NOTE: THIS SHOULD NOT BE NECESSARY!

	bool Read (char *fname);
	// Read specs from config file

	int AddStar (Star *_star);
	// add a new star to the system
	// return value is the star's id

	int AddPlanet (Planet *_planet, CelestialBody *cbody);
	// add a new planet or moon to the system with cbody as the central body
	// return value is the planet's id or -1 if error

	int AddVessel (Vessel *_vessel);
	// add a new vessel to the system
	// return value is the vessel's id

	bool DelVessel (Vessel *_vessel, Body *_alt_cam_tgt);
	// remove vessel from the system. This will fail for the focus object
	// if _vessel was camera target, then camera will switch to _alt_cam_tgt
	// (or Sun, if no target provided)

	void DockVessels (Vessel *vessel1, Vessel *vessel2, int port1 = 0, int port2 = 0, bool mixmoments = true);
	// create a composite vessel by connecting two vessels (or vessel groups)
	// at their respective docking ports port1 and port2

	void UndockVessel (SuperVessel *sv, Vessel *_vessel, int port, double vsep);
	// undock a vessel (or sub-structure) port from its super-structure
	// vsep is the separation velocity (>= 0)

	void ScanMoons (std::istream &is, CelestialBody *cbody, char *id);
	// Add moons recursively to the child list of "cbody",
	// using descriptions in "is" for description string "id"

	void Update (bool force = false);
	// Perform time step for the planetary system

	void FinaliseUpdate ();

	void Timejump ();
	// Discontinuous step

	void ScanGFieldSources (const Vector *gpos, const Body *exclude, GFieldData *gfd) const;
	// Build a list of significant gravity sources at point 'gpos',
	// excluding body 'exclude', and return results in 'gfd'.

	void UpdateGFieldSources (const Vector *gpos, const Body *exclude, GFieldData *gfd) const;
	// Update the existing list

	Vector GaccAt (double t, const Vector &gpos, const Body *exclude = 0) const;
	// gravity field at gpos for time t

	Vector Gacc (const Vector &gpos, const Body *exclude = 0, const GFieldData *gfd = 0) const;
	// Acceleration vector due to gravitational forces at global position gpos for current time t0.
	// If exclude != 0 then this object is omitted (to avoid objects interrogating themselves)
	// If gfd != 0 then only g-sources from this list are computed

	//Vector Gacc_t1 (const Vector &gpos, const Body *exclude = 0, const GFieldData *gfd = 0) const;
	// Acceleration vector due to gravitational forces at global position gpos for time t1
	// (update phase).
	// If exclude != 0 then this object is omitted (to avoid objects interrogating themselves)
	// if gfd != 0 then only objects from this source list are computed (plus an additional
	// one tested for inclusion in the list)

	Vector Gacc_intermediate (const Vector &gpos, double n, const Body *exclude = 0, GFieldData *gfd = 0) const;
	// Acceleration vector due to gravitational forces at global position gpos at intermediate
	// time t = t0+n*dt, where 0 <= n <= 1 is a fractional time step, n = (t-t0)/dt, and dt = t1-t0.
	// Uses linear interpolation of celestial body positions.
	// If gfd != 0 then only g-sources from this list are computed

	Vector Gacc_intermediate_pert (const CelestialBody *cbody, const Vector &gpos, double n, const Body *exclude, GFieldData *gfd) const;

	Vector GaccPn_perturbation (const Vector &gpos, double n, const CelestialBody *cbody) const;
	// returns the nonspherical perturbation of the gravity field from 'body' at global
	// position 'gpos' at fractional time n during current time step (0<=n<=1).

	Vector GaccRel (const Vector &rpos, const CelestialBody *cbody, double n, const Body *exclude, GFieldData *gfd) const;
	// this version calculates the gravitational acceleration vector at fractional time n during
	// current time step for position 'rpos' relative to 'cbody'

	CelestialBody *GetDominantGravitySource (const Vector &gpos, double &gfrac);
	// return the dominant object contributing to the gravity field
	// at position pos. gfrac is the fractional contribution of the
	// dominant body to the total field

	double GetGravityContribution (const Body *body, const Vector &gpos, bool *dominant = 0);
	// returns the fractional contribution of "body" to the gravity field at gpos.
	// if defined, dominant returns true if the body is the major contributor

	Vector GetMomentumFlux (const Vector &gpos) const;
	// returns the momentum flux [N/m^2] due to solar radiation at position gpos.
	// - assumes single radiation source at origin
	// - source luminosity is fixed to L=3.846e26 W (sun)
	// - does not take into account shadowing effects

	void InitDeviceObjects ();
	void DestroyDeviceObjects ();
	// Allocate/deallocate graphics device-specific objects for the individual
	// world objects.
	// These functions should eventually become obsolete because none of the
	// logical objects should contain any device objects

	void BroadcastVessel (DWORD msg, void *data);
	// Broadcast a message to all vessels

	friend Vector SingleGacc (const Vector &rpos, const CelestialBody *body);
	friend Vector SingleGacc_perturbation (const Vector &rpos, const CelestialBody *body);

	oapi::GraphicsClient::LABELLIST *LabelList (int *nlist = 0) { if (nlist) *nlist = nlabellist; return labellist; }
	void ScanLabelLists (std::ifstream &cfg);

	void ActivatePlanetLabels(bool activate);

	intptr_t FindFirst (int type, _finddata_t *fdata, char *path, char *fname);
	intptr_t FindNext (intptr_t fh, _finddata_t *fdata, char *fname);

private:
	char *name; // system's name

	DWORD nbody;     // number of bodies in the general object list
	Body **body;   // list of bodies

	DWORD nstar;     // number of stars in the system
	Star **star;   // list of stars

	int nplanet; // number of planets in the system
	Planet **planet; // list of planets

	DWORD ngrav;
	CelestialBody **grav;
	// List of "massive" objects (those producing a
	// gravitational field: stars, planets, moons)

	oapi::GraphicsClient::LABELLIST *labellist;
	int nlabellist;
	char *labelpath;

	DWORD nvessel;
	Vessel **vessel;
	// List of spacecraft

	DWORD nsupervessel;
	SuperVessel **supervessel;
	// List of spacecraft groups (composite vessels)

	void OutputLoadStatus (const char *bname);

	void AddBody (Body *_body);
	// Add "body" to the system's general list of objects

	bool DelBody (Body *_body);
	// Remove "body" from the system's general list of objects

	void AddGrav (CelestialBody *body);
	// Add "body" to the system's list of massive objects

	void AddSuperVessel (SuperVessel *sv);
	// add a vessel superstructure to the list

	bool DelSuperVessel (SuperVessel *sv);
	// remove a vessel superstructure from the list

};

#endif // !__PSYS_H