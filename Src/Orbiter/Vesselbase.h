// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __VESSELBASE_H
#define __VESSELBASE_H

#include "Rigidbody.h"
#include "elevmgr.h"

typedef enum {
	FLIGHTSTATUS_FREEFLIGHT,
	FLIGHTSTATUS_LANDED,
	FLIGHTSTATUS_TAXIING,
	FLIGHTSTATUS_DOCKED,
	FLIGHTSTATUS_CRASHED,
	FLIGHTSTATUS_UNDEFINED
} FlightStatus;

typedef struct { // touchdown vertex specifications
	Vector pos;          // touchdown point position (uncompressed) in vessel frame
	double stiffness;    // suspension stiffness
	double damping;      // suspension damping
	double compression;  // suspension compression factor
	double mu;           // isotropic/transversal friction coefficient
	double mu_lng;       // longitudinal friction coefficient (only used for first 3 points)
} TOUCHDOWN_VTX;

struct WindPrm;

struct SurfParam {//Surface-relative vessel state
	void Set (const StateVectors &s, const StateVectors &s_ref, const CelestialBody *ref,
		std::vector<ElevationTile> *etilecache=NULL, WindPrm *windprm=NULL);
	// Set surface parameters from object and reference state vectors

	static double ComputeAltitude(const StateVectors &s, const StateVectors &s_ref, const CelestialBody *ref,
		std::vector<ElevationTile> *etilecache=NULL);
	// recalculate altitude

	void SetLanded (double lng, double lat, double alt, double dir, const Vector &nml, const CelestialBody *ref);
	// Set surface parameters for a landed object

	const CelestialBody *ref; // body to which data refer
	Vector ploc;              // ship position in planet local cartesian coords
	Vector groundvel_glob;    // ground-relative velocity in global frame
	Vector groundvel_ship;    // ground-relative velocity in local vessel frame
	Vector airvel_glob;       // airspeed vector in global frame
	Vector airvel_ship;       // airspeed vector in local vessel frame
	double groundspd;         // ground-relative velocity magnitude
	double airspd;            // airspeed magnitude
	double vspd;              // vertical velocity (negative for descent)
	double lng, lat, rad;     // ship position in planet's local equatorial coords
	double slng, clng, slat, clat; // sines and cosines of lng and lat
	double alt;               // ship altitude over ground
	double alt0;              // ship altitude over mean radius
	double elev;              // ground elevation with respect to mean planet radius
	int elev_lvl;             // resolution level at which elevation was computed
	Vector surfnml;           // surface normal in local horizon frame (+y=up)
	double pitch, bank;       // vessel orientation w.r.t. horizon
	double dir;               // compass orientation
	bool is_in_atm;           // true if ship is within a planetary atmosphere
	double dynp;              // dynamic pressure: 1/2 rho * speed^2
	double atmp;              // atmospheric pressure [Pa]
	double atmrho;            // atmosphere density [kg/m^3]
	double atmT;              // atmospheric ambient temperature [K]
	double atmM;              // Mach number
	Matrix L2H;               // rotation matrix planet-local -> local horizon
};

struct WindPrm {           // per-vessel wind parameters
	double pert_t;            // time for wind perturbation vector
	Vector pert_v;            // wind perturbation vector
};

// =======================================================================

class Planet;
class Base;

// =======================================================================
// Class VesselBase

class VesselBase: public RigidBody {
public:
	VesselBase ();
	VesselBase (double _mass, double _size, const Vector &_pmi);

	virtual void Update (bool force = false) = 0;
	// Update vessel state by propagating across current time step

	virtual void PostUpdate ();

	inline CelestialBody *ProxyBody() { return proxybody; }
	inline Planet *ProxyPlanet() { return proxyplanet; }
	inline const Planet *ProxyPlanet() const { return proxyplanet; }
	// closest celestial object (star, planet, moon)

	inline Base *ProxyBase () const { return proxybase; }
	// closest surface base

	virtual void RPlace (const Vector &rpos, const Vector &rvel) {}
	// Set vessel position and velocity in parent coords
	// If the vessel is part of a superstructure, the complete structure
	// is moved accordingly

	virtual void Refuel () {}
	// Re-fill all tanks

	const char *CurPropagatorStr (bool verbose = true) const
	{
		const VesselBase *super = GetSuperStructure();
		return (super ? super->CurPropagatorStr(verbose) : RigidBody::CurPropagatorStr(verbose));
	}

	const int CurPropagatorSubsamples () const
	{
		const VesselBase *super = GetSuperStructure();
		return (super ? super->CurPropagatorSubsamples() : RigidBody::CurPropagatorSubsamples());
	}

	inline double Elevation() const
	{ return sp.elev; }
	// Surface elevation with respect to mean planet radius below the current
	// vessel position.

	inline double Altitude() const
	{ return sp.alt; }

	inline bool GroundContact() const
	{ return bSurfaceContact; }

protected:
	virtual void SetDefaultState ();
	// Reset all state parameters to default values

	virtual bool Activate (bool force = false);
	// Switch to active flight mode

	void UpdateSurfParams ();
	// update surface parameters

	virtual void InitLanded (Planet *planet, double lng, double lat, double dir,
		const Matrix *hrot=0, double cgelev=0.0, bool asComponent=false) {}
	// init vessel position on ground
	// Note: InitLanded must be called after the update phase (after EndStateUpdate)

	double LandedHeading (double lng, double lat) const;
	// Returns the heading of a vessel landed at lng/lat
	// using state vectors s0

	virtual void GetIntermediateMoments (Vector &acc, Vector &tau,
		const StateVectors &state, double tfrac, double dt);
	// Returns acceleration acc and torque tau, at time SimT0+tfrac*SimDT
	// and step size dt, given intermediate state in global frame

	virtual void UpdateProxies ();

	virtual void SetProxyplanet (Planet *p) { proxyplanet = p; }

	bool SurfaceProximity () const;
	// Returns true if vessel is withing 3 radii of planetary surface

	virtual bool CheckSurfaceContact () const = 0;
	// Returns true if any part of the vessel is in contact with a planet surface
	// Should be called only after update phase, and assumes that sp is up to date.

	virtual bool ThrustEngaged () const = 0;
	// Returns true if any thrusters are engaged at the current time step

	virtual bool IsComponent () const { return false; }
	// Returns true if *this is a component of a superstructure

	virtual const VesselBase *GetSuperStructure () const { return NULL; }

	virtual void SetPropagator (int &plevel, int &nstep) const;
	// set timestep propagator parameters; overrides defaults during ground contact

	virtual bool ValidateStateUpdate (StateVectors *s);

	virtual bool AddSurfaceForces (Vector *F, Vector *M,
		const StateVectors *s = NULL, double tfrac = 1.0, double dt = 0.0) const;
	// Return linear force F and angular moment M due to planet surface interaction,
	// given state vectors s at fraction tfrac (0..1) of the current time step, with
	// step interval dt.

	virtual TOUCHDOWN_VTX *HullvtxFirst () = 0;
	virtual TOUCHDOWN_VTX *HullvtxNext () = 0;
	// Hull vertex iterator

	// Behaviour flags
	bool bDynamicGroundContact;  // use dynamic model for ground contact forces

	mutable bool update_with_collision;   // include surface contact forces in dynamic update
	mutable bool collision_during_update; // surface contact during current step update?
	mutable bool collision_speed_checked; // checked for valid speed during impact?

	// Status flags
	FlightStatus fstatus;
	CelestialBody *proxybody;    // closest 'massive' object (star, planet, moon)
	Planet *proxyplanet;         // closest 'landable' object (planet or moon)
	Base *proxybase;             // closest surface base
	bool bSurfaceContact;        // signal vessel is in contact with planet surface

	SurfParam sp;      // ship parameters concerning planet surface
	Matrix land_rot;   // rotates ship's local into planet's local coords so that grot = grot(planet) * land_rot

	mutable std::vector<ElevationTile> etile;
	mutable WindPrm windp;

	struct LANDING_TEST {        // parameters for testing LANDED status eligibility
		bool testing;
		double testt;
		Vector surfpos;
		Quaternion surfrot;
	} LandingTest;

private:
	void CheckLanded ();
	// Switch to flight status "LANDED" if criteria are satisfied

	double proxyT;               // next check for proxybody
};

#endif // !__VESSELBASE_H