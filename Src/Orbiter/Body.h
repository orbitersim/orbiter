// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Class Body
// Body - Logical interface
// =======================================================================

#ifndef __BODY_H
#define __BODY_H

#include "OrbiterAPI.h"
#include "Config.h"

class Elements;
class CelestialBody;
class Body;
class VObject;

class Body {
	friend class PlanetarySystem;

public:
	Body ();
	Body (double _mass, double _size);
	Body (char *fname);
	// create a body from a config file

	virtual ~Body ();

	virtual int Type() const { return OBJTP_GENERIC; }

	inline char *Name() const { return name; }

	virtual const void *GetParam (DWORD paramtype) const { return 0; }

	void BroadcastVisMsg (DWORD msg, UINT content);
	// send a message to all existing visuals

	inline double Size() const { return size; }
	virtual void SetSize (double newsize);
	// Get/set current object size [m]

	inline double VisLimit () const { return vislimit; }
	inline double SpotLimit () const { return spotlimit; }

	virtual double ClipRadius() const { return size; }
	// used for near-plane definition

	inline double Mass() const { return mass; }
	// Current object mass [kg]

	inline const Vector &Albedo () const { return albedo; }
	// object albedo (RGB, 0-1)

	inline const Vector &GPos() const { return s0->pos; }
	inline const Vector &GVel() const { return s0->vel; }
	inline const Matrix &GRot() const { return s0->R; }
	inline const Quaternion &GQ() const { return s0->Q; }
	// Object position, velocity and orientation in global coords. To transform
	// a point between local object coords p_loc and global coords p_glob:
	// p_glob = GRot * p_loc + GPos

	void SetRPos (const Vector &p);
	void AddRPos (const Vector &dp);
	void FlushRPos ();

	void SetRVel (const Vector &v);
	void AddRVel (const Vector &dv);
	void FlushRVel ();

	inline Vector GlobalToLocal (const Vector &glob) const
	{ return tmul (s0->R, glob - s0->pos); }
	// Convert global position glob into body's local coordinate system

	inline void GlobalToLocal (const Vector &glob, Vector &loc) const
	{ loc.Set (tmul (s0->R, glob - s0->pos)); }
	// same with different interface

	inline void LocalToGlobal (const Vector &loc, Vector &glob) const
	{ glob.Set (mul (s0->R, loc) + s0->pos); }

	void LocalToEquatorial (const Vector &loc, double &lng, double &lat, double &rad) const;
	inline void GlobalToEquatorial (const Vector &glob, double &lng, double &lat, double &rad) const
	{ LocalToEquatorial (GlobalToLocal (glob), lng, lat, rad); }
	// Convert local/global position glob into equatorial coordinates of
	// the body (longitude, latitude and radial distance

	inline void EquatorialToLocal (double slng, double clng, double slat, double clat, double rad, Vector &loc) const
	{ double xz = rad*clat; loc.Set (xz*clng, rad*slat, xz*slng); }
	inline void EquatorialToLocal (double lng, double lat, double rad, Vector &loc) const
	{ EquatorialToLocal (sin(lng), cos(lng), sin(lat), cos(lat), rad, loc); }
	inline void EquatorialToGlobal (double lng, double lat, double rad, Vector &glob) const
	{ Vector loc; EquatorialToLocal (lng, lat, rad, loc); LocalToGlobal (loc, glob); }

	virtual bool SurfpointVisible (double lng, double lat, const Vector &gcam) const;
	// Returns true if a surface point given by longitude and latitude is visible from global point gcam
	// This base class method assumes a spherical object of radius Size()

	virtual void RPlace (const Vector &rpos, const Vector &rvel);
	// Set object position and velocity in parent coords

	virtual void Update (bool force = false);
	// Update object to current simulation time.
	// Called between the body's module clbkPreStep and clbkPostStep API calls

	virtual void BeginStateUpdate ();
	// Start the state update phase.
	// This function validates the s1 state to allow it being used for state
	// update calculations.

	virtual void EndStateUpdate ();
	// Make the updated state active. Should be called by the system to advance
	// the simulation state to the next time step, after the updated state has
	// been calculated via Update by all objects in the system, and at the same
	// time the simulation time is advanced from t0 to t0+dt.

	virtual bool SkipRender() const { return false; }
	// set this to true to suppress rendering of the object

	inline const CelestialBody *ElRef() const { return cbody; }
	// Orbit reference body (or NULL if none)

	virtual void InitDeviceObjects () {}
	// This allows objects to initialise any D3DDevice objects
	// that persist beyond the lifetime of the visual

	virtual void DestroyDeviceObjects ();
	// Clean up device objects before the D3DDevice is destroyed
	// Default action: Calls DestroyDeviceObjects() for all children

	virtual void RegisterVisual (VISHANDLE vis);
	// Called by a graphics client to notify the body of visual
	// creation.

	virtual void UnregisterVisual ();
	// Called by a graphics client to notify the body of visual
	// destruction.

	inline VISHANDLE GetVishandle() const { return hVis; }
	// return handle for object's visual (or NULL if doesn't exist)

	inline const VISHANDLE *GetVishandlePtr() const { return &hVis; }

	StateVectors *s0;    // body state at time t0
	StateVectors *s1;    // new body state at time t0+dt during update phase

	// Operations on updated state vectors (only accessible during update phase
	// AFTER the object has been updated)

	inline Vector GlobalToLocal_t1 (const Vector &glob) const
	{ return tmul (s1->R, glob - s1->pos); }
	// Convert global position glob into body's local coordinate system

	inline void GlobalToLocal_t1 (const Vector &glob, Vector &loc) const
	{ loc.Set (tmul (s1->R, glob - s1->pos)); }
	// same with different interface

	inline void LocalToGlobal_t1 (const Vector &loc, Vector &glob) const
	{ glob.Set (mul (s1->R, loc) + s1->pos); }

	inline const Vector &Acceleration() const { return acc; };

protected:
	double mass;         // current body mass [kg]
	double size;         // (mean) body radius [m]
	Vector albedo;       // object albedo (RGB, 0-1)
	double vislimit;     // total visibility limit (in units of viewport vertical)
	double spotlimit;    // spot visibility limit (in units of viewport vertical)

	Vector acc;          // current acceleration vector

	VISHANDLE hVis;      // visual identifier passed to messages (NULL=no visual)

	const CelestialBody *cbody; // orbit reference body

	void Setup ();       // initialise body with default params

	char *name;          // object name

	Vector rpos_base, rpos_add; // base and incremental parts of rpos
	Vector rvel_base, rvel_add; // base and incremental parts of rvel
	int updcount;               // update counter

private:
	void SetName (char *_name = 0);

	void FlipState ();
	// Switch the targets for state vector pointers s0 and s1.
	// This is normally used by EndStateUpdate to make the updated state active.
	// Note this assumes s0 to point to either sv[0] or sv[1]. After the function
	// returns, both s0 and s1 will be valid, where s1 will point to the address s0
	// was pointing to, while s0 will point to the other one.
	// If s1 should be set invalid (NULL), this has to be done by the caller after
	// the function returns.

	StateVectors sv[2];  // State vectors for current and updated state - don't use directly
};

#endif // !__BODY_H