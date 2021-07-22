// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Class RigidBody
// Objects that use a dynamic rigid body model for propagating their linear
// and rotational states.
// By default, rigid bodies experience gravitational forces. Other types
// of forces (atmospheric interaction, engines, etc. can be added by
// derived classes.
// RigidBody is the base class for all orbiting objects.
//
// Hierarchy:  Body --> RigidBody
// =======================================================================

#ifndef __RIGIDBODY_H
#define __RIGIDBODY_H

#include "Body.h"

class RigidBody;

// =======================================================================
// typdefs

const DWORD MAXGFIELDLIST = 20;

typedef struct {         // used for dynamic grav updates
	DWORD gravidx[MAXGFIELDLIST]; // index list for gravity source objects
	DWORD ngrav;                  // number of gravity sources
	DWORD testidx;                // index for next object to test
	double updt;                  // time of next list update
} GFieldData;

typedef struct {  // data for angular integrators
	int nsub;        // subdivisions of current time step (1=full step)
	double t1, dt;   // subdivision end time, time interval
	Vector p0, p1;   // orbital positions at start, end of subdivision step
	Vector (*AngAcc)(RigidBody *body, const Vector &tau, const Vector &omega);
	                 // full/simplified Euler's equations of angular motion
}  AngIntData;

typedef struct {  // data for linear perturbation integrators
	double t1, dt;     // subdivision end time, time interval
	Vector dv;         // applied non-gravitational delta v
	Vector p0;         // orbital position at start of sobdivision step
	bool nonspherical; // include nonspherical perturbations
} PertIntData;

typedef void (*Propagator)(RigidBody*,double);  // time propagation function template
typedef void (*AngPropagator)(RigidBody*,const AngIntData&); // angular time propagation template

typedef void (RigidBody::*LinAngPropagator)(double, int, int);
// state propagator function template

// =======================================================================

class RigidBody: public Body {
public:
	RigidBody ();
	RigidBody (double _mass, double _size, const Vector &_pmi);
	RigidBody (char *fname);

	virtual ~RigidBody();

	static void GlobalSetup ();
	// 1-time setup routines at beginning of simulation session

	inline const Vector &PMI () const { return pmi; }
	// principal moments of inertia

	//virtual void BeginStateUpdate ();
	//virtual void EndStateUpdate ();

	void SetOrbitReference (CelestialBody *body);
	// reset reference object for element calculation to "body"

	const Elements *Els() const;
	// Returns pointer to orbital elements (after updating them, if
	// necessary), or NULL if not supported

	virtual void Update (bool force = false);
	// Update object to current simulation time.
	// The default action is to update position and velocity vectors
	// according to graviational forces. Derived types which do their
	// own updates may override or augment this.

	virtual void SetPropagator (int &plevel, int &nstep) const;
	// return propagator level (0..nPropLevel-1) and substep number (1..PropSubMax)
	// for current step. Note that nstep > PropSubMax is valid, but should only be
	// used for immediate collision treatment

	virtual void GetIntermediateMoments (Vector &acc, Vector &tau,
		const StateVectors &state, double tfrac, double dt);
	// Returns acceleration acc and torque tau, at time SimT0+tfrac*SimDT
	// and step size dt, given intermediate state in global frame
	// Note: state.vel is not used by this method

	virtual void GetIntermediateMoments_pert (Vector &acc,
		Vector &tau, const StateVectors &state_rel, double tfrac, double dt,
		const CelestialBody *cbody);
	// As GetIntermediateMoments, but excludes the pointmass gravitational
	// acceleration of cbody. This is used by the Encke stabilised
	// propagators which add the effect of the primary by propagating
	// the 2-body elements
	// Note: state_rel.pos should be the position relative to cbody in the global frame
	// Note: state_rel.vel is not used by this method

	virtual bool ValidateStateUpdate (StateVectors *s) { return true; }

	virtual Vector GetPertAcc (const PertIntData &data, const Vector &pos, double tfrac);
	// returns the gravity perturbation (on top of the spherical gravity field
	// from cbody) at relative position pos, at fractional time tfrac within
	// the current time step

	virtual Vector GetTorque () const;
	// Returns mass-normalised torque vector for state s0. This only consists of
	// gravity gradient torque, and only if enabled.

	inline const Vector &AngularVelocity () const { return s0->omega; }
	// angular velocity - should be moved to Body class!

	virtual void SetAngVel (const Vector &omega);
	// set angular velocity to 'omega'

	inline Vector AngularMomentum () const
	{ return Vector(pmi.x*s0->omega.x, pmi.y*s0->omega.y, pmi.z*s0->omega.z); }
	// returns the vessel's current angular momentum (in local vessel coordinates)

	Vector Euler_full (const Vector &omegadot, const Vector &omega) const;
	// returns torque tau, given angular acceleration omegadot and angular velocity
	// omega.

	Vector EulerInv_full (const Vector &tau, const Vector &omega) const;
	// Solves Euler's equation:
	// returns angular acceleration, given torque tau and angular velocity
	// omega.

	Vector EulerInv_simple (const Vector &tau, const Vector &omega) const;
	// Simplified angular acceleration calculation ignoring cross-axis
	// coupling terms

	Vector EulerInv_zero (const Vector &tau, const Vector &omega) const;
	// Trivial angular acceleration calculation: ignores cross-axis
	// coupling terms and torque, and always returns zero

	static const char *PropagatorStr (DWORD idx, bool verbose = true);
	// Returns a string describing the dynamic state propagator
	// given by index idx.

	virtual const char *CurPropagatorStr (bool verbose = true) const;
	// Returns a string describing the dynamic state propagation
	// method at the current time step (e.g. "RK4")

	virtual const int CurPropagatorSubsamples () const
	{ return nPropSubsteps; }
	// Returns the number of subdivisions of the current frame interval
	// for the dynamic state integrator

	const char *RotationModel () const;
	// Returns a string describing the rotation model used

	void SetDynamicUpdate (bool dynamic);
	// enable disable dynamic position updates according to
	// gravitational forces

	virtual bool isOrbitStabilised () const { return bOrbitStabilised; }
	// return true if body uses orbit stabilisation for the current step

	inline bool canDynamicPosVel () const { return bDynamicPosVel; }
	// return true if body can update its position by state vector integration

	inline bool UseComplexGravity () const { return bGPerturb; }
	// creates or acts upon nonspherical gravity effects

	inline const GFieldData &GetGFieldData() const { return gfielddata; }

protected:
	virtual void SetDefaultState ();
	// Reset all state parameters to default values

	void ScanGFieldSources (const PlanetarySystem *psys);
	// Collect a list of gravity field sources affecting the body dynamics
	// at its current position

	void UpdateGFieldSources (const PlanetarySystem *psys);
	// Update an existing list of gravity sources

	Vector InterpolatePos (const Vector &p0, const Vector &p1, double t1, double dt, double tfrac) const;
	// interpolates orbital position between start point p1 and end point p2, given
	// end time t1 and interval dt, at fractional position tfrac [0..1]

	void SetDefaultCaps ();
	// Initialise parameters with default values

	void ReadGenericCaps (std::ifstream &ifs);
	// Read parameters from a config file

	inline int NumPropLevel() const { return nPropLevel; } // number of defined propagator levels
	inline int MaxSubStep() const { return PropSubMax; }   // max number of substeps per step update

	bool bDynamicPosVel;
	// Set this to false if an object handles its own position
	// and velocity update rather than relying on the default
	// dynamic model (e.g. analytic orbit calculation from elements)

	bool bCanUpdateStabilised;
	// If true, body can use "stabilised" state propagation if the step
	// length is too large for a direct integration. Stabilised updates
	// use Encke's method of propagating the perturbations to a 2-body orbit.

	bool bOrbitStabilised;
	// Indicates if the current step was updated by "orbit stabilisation",
	// i.e. Encke's method.

	bool bIgnoreGravTorque;
	// flag for suppressing gravity-gradient torque (to avoid numerical instability)

	mutable Elements *el;       // osculating elements for orbiting bodies
	mutable bool el_valid;      // flag for element update

	Vector cpos, cvel; // state vectors w.r.t. reference body
	Vector pcpos;      // refbody-relative position at previous step
	Vector pmi;        // principal moments of inertia tensor
	Vector arot;       // current angular acceleration
	Vector acc_pert;   // current acceleration excluding gravity from primary point mass (for Encke state integration, only valid during stabilised updates)
	Vector torque;     // current torque of CG
	double tidaldamp;  // damping factor for tidal torque
	double ostep;      // time step in terms of fractional orbit (approx.)
	AngIntData aidata; // data for angular integration
	static bool bDistmass;    // take into account spatial mass distribution
	static bool bGPerturb;    // nonspherical gravity effects

	GFieldData gfielddata;  // used for dynamic grav updates

private:
	static void SetupPropagationModes ();
	// set up the dynamic time propagation modes

	// -----------------------------------------------------------------------
	// Dynamic integrators for linear and angular state vectors
	// Implemented in BodyIntegrator.cpp

	void RKdrv_LinAng (double h, int nsub, int isub, int n, const double *alpha, const double *beta, const double *gamma); // RK engine for RK5-8, linear+angular
	void RK2_LinAng (double h, int nsub, int isub);  // RK2, linear+angular
	void RK4_LinAng (double h, int nsub, int isub);  // RK4, linear+angular
	void RK5_LinAng (double h, int nsub, int isub);  // RK5, linear+angular
	void RK6_LinAng (double h, int nsub, int isub);  // RK6, linear+angular
	void RK7_LinAng (double h, int nsub, int isub);  // RK7, linear+angular
	void RK8_LinAng (double h, int nsub, int isub);  // RK8, linear+angular
	void SY2_LinAng (double h, int nsub, int isub);  // symplectic, order 2, linear+angular
	void SY4_LinAng (double h, int nsub, int isub);  // symplectic, order 4, linear+angular
	void SY6_LinAng (double h, int nsub, int isub);  // symplectic, order 6, linear+angular
	void SY8_LinAng (double h, int nsub, int isub);  // symplectic, order 8, linear+angular

	// Propagators for 2-body orbit perturbations
	//void RK2_LinAng_Encke (double h, int nsub, int isub);

	//void RK2_Pert (const PertIntData &data); // RK2, perturbation
	//void RK4_Pert (const PertIntData &data); // RK4, perturbation
	//void RK8_Pert (const PertIntData &data); // RK8, perturbation
	//void RKdrv_Pert (const PertIntData &data, int n, const double *alpha, const double *beta, const double *gamma);

	void Encke ();

	// -----------------------------------------------------------------------

	static struct PROPMODE {
		LinAngPropagator propagator;
		int propidx;  // propagator method index
		double ttgt;  // time step target [s]
		double atgt;  // angular step target [rad]
		double tlim;  // time step limit [s]
		double alim;  // angular step limit [rad]
	} PropMode[MAX_PROP_LEVEL];
	static int nPropLevel; // number of propagator stages
	int PropLevel;         // current propagator stage
	int PropSubMax;        // upper limit for number of subsamples
	int nPropSubsteps;     // current number of subsamples

	friend Vector Call_EulerInv_full (RigidBody *body, const Vector &tau, const Vector &omega)
	{ return body->EulerInv_full (tau, omega); }
	friend Vector Call_EulerInv_simple (RigidBody *body, const Vector &tau, const Vector &omega)
	{ return body->EulerInv_simple (tau, omega); }
	friend Vector Call_EulerInv_zero (RigidBody *body, const Vector &tau, const Vector &omega)
	{ return body->EulerInv_zero (tau, omega); }
};

#endif // !__RIGIDBODY_H