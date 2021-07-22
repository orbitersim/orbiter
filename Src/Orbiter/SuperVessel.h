// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
// class SuperVessel
// A "vessel container" class which handles vessels which are grouped
// by docking. SuperVessels are created on the fly when vessels dock
// The SuperVessel manages position updates and interprets thrust
// events of the attached vessels.
// ==============================================================

// Some notes:
// -----------
// 1. The supervessel frame of reference is arbitrarily set to the local frame
//    of the first vessel in the original vessel list. This remains unchanged
//    even if the first vessel detaches from the assembly.
//
// 2. This means that the supervessel centre of gravity (CG) is not in the
//    origin of the supervessel frame. Instead, the CG can move around as vessels
//    are attached or detached, and individual vessel masses change.
//
// 3. The positions and orientations of individual vessels in the assembly relative
//    to the supervessel frame are defined by the rpos and rrot values of their
//    SubVesselData entries.
//
// 4. Mapping a point pv from the local frame of subvessel i to the supervessel
//    frame is done by
//                       ps = rrot_i * pv + rpos_i
//
// 5. The supervessel's global position, s0->pos, refers to its CG, not to the
//    origin of its local frame (is this really the correct convention?)
//
// 6. Mapping point ps from the supervessel frame to the global frame is done by
//                       pg = grot * (ps-cg) + gpos
//    where cg is the position of the CG in the supervessel frame, and grot and
//    gpos are the supervessel rotation matrix and global position, respectively


#ifndef __SUPERVESSEL_H
#define __SUPERVESSEL_H

#include "Vesselbase.h"
#include "Vessel.h"

typedef struct {     // vessel component specs
	Vessel *vessel;     // vessel pointer
	Vector rpos;        // rel vessel position in SuperVessel coords
	Matrix rrot;        // rel vessel orientation: vessel -> supervessel
	Quaternion rq;      // rel vessel orientation in quaternion representation
} SubVesselData;

// ==============================================================
// class SuperVessel

class SuperVessel: public VesselBase {
	friend class Vessel;

public:
	SuperVessel (Vessel *vessel);
	// Create a SuperVessel containing a single component
	// Usually this should be followed by adding one or more additional components

	SuperVessel (Vessel *vessel1, Vessel *vessel2, int port1 = 0, int port2 = 0, bool mixmoments = true);
	// create a SuperVessel by docking two vessels at specified ports
	// if mixmoments==true, angular velocities are merged, otherwise velocity of vessel1 is used

	~SuperVessel();

	inline int nVessel() const { return nv; }
	// number of individual vessels constituting the supervessel

	inline Vessel *GetVessel (int i) { return vlist[i].vessel; }
	// return pointer to i-th component vessel

	bool isComponent (const Vessel *v) const;
	// return true if vessel v is part of the supervessel

	bool Add (Vessel *vessel1, int port1, Vessel *vessel2, int port2, bool mixmoments = true);
	// Add simple vessel2 to *this by attaching to vessel1, which must be part of *this.
	// returns false if vessel1 is not part of *this

	bool Merge (Vessel *vessel1, int port1, Vessel *vessel2, int port2);
	// Merge the superstructures of vessel1 and vessel2. Vessel1 must be part of *this.
	// The superstructure of vessel2 is merged into *this and deleted

	void Attach (Vessel *vessel1, int port1, Vessel *vessel2, int port2);
	// Connect two vessels at the specified ports. Vessel1 must be part of superstructure
	// *this. If vessel2 is part of a superstructure, that superstructure is merged into
	// *this.

	void Detach (Vessel *vessel, DWORD port, double vsep = 0.2);
	// detach "vessel" from the super-structure

	void RPlace (const Vector &_rpos, const Vector &_rvel, const Vessel *ref = 0);
	// Sets the supervessel's position and velocity state vectors in parent coordinates
	// If ref is set, then rpos refers to that vessel. Otherwise it refers to the supervessel CG.

	void SetGlobalOrientation (const Vector &arot, const Vessel *ref = 0);
	// Set superstructure orientation from vector of Euler angles
	// If ref is set, then arot refers to that vessel.

	void SetRotationMatrix (const Matrix &R, const Vessel *ref = 0);
	// Set global superstructure rotation matrix to R
	// If ref is set, then arot refers to that vessel.

	void SetAngVel (const Vector &omega, const Vessel *ref = 0);
	// Set angular velocity components to omega [rad/s]
	// If ref is set, then avel refers to that vessel.

	void InitLanded (Planet *planet, double lng, double lat, double dir, const Matrix *hrot=0, double cgelev=0.0, bool asComponent=false);
	// init vessel position on ground
	// Note: InitLanded must be called after the update phase (after EndStateUpdate)

	void Refuel ();
	// Re-fill all tank in all components

	void GetIntermediateMoments (Vector &acc, Vector &tau,
		const StateVectors &state, double tfrac, double dt);
	// Returns acceleration acc and torque tau, at time SimT0+tfrac*SimDT
	// and step size dt, given intermediate state in global frame

	void Update (bool force);
	// per-frame update of supervessel parameters

	void PostUpdate ();

	bool AddSurfaceForces (Vector *F, Vector *M,
		const StateVectors *s = NULL, double tfrac = 1.0, double dt = 0.0) const;

	void NotifyShiftVesselOrigin (Vessel *vessel, const Vector &dr);
	// sent by a vessel to notify a shift of its local coordinate origin (i.e. its centre of mass)

	bool GetCG (const Vessel *vessel, Vector &vcg);
	// Sets 'vcg' to centre of gravity of super-structure in coordinates of 'vessel', if vessel is
	// part of the super-structure. Otherwise returns false

	bool GetPMI (const Vessel *vessel, Vector &vpmi);
	// Returns PMI values of supervessel in 'vpmi' rotated into coordinate frame of
	// vessel 'vessel'

	inline double Mass() const { return mass; }
	// Returns supervessel mass (sum of component masses)

	void SetStateFromComponent (const StateVectors *scomp, int comp) const;
	// Set the supervessel state from one of its component states

	void SetFlightStatus (FlightStatus fstatus);
	// set flightstatus of supervessel by propagating fstatus to all components

	// ===========================================================================
	// Flight recorder functions

	void FRecorder_EndPlayback();

protected:
	void SetDefaultState();

	bool Activate (bool force = false);
	// Switch to active flight mode

	bool CheckSurfaceContact () const;
	// Returns true if any part of the vessel is in contact with a planet surface
	// Should be called only after update phase, and assumes that sp is up to date.

	bool ThrustEngaged () const;

	TOUCHDOWN_VTX *HullvtxFirst ();
	TOUCHDOWN_VTX *HullvtxNext ();
	// hull vertex iterator: returns first/next hull vertex in supervessel frame

private:
	bool RemoveEntry (const Vessel *v);
	// remove vessel v from vlist

	void TransferAllDocked (Vessel *v, SuperVessel *sv, const Vessel *exclude);
	// Transfer all vessels docked to 'v' from *this to 'sv', excluding vessel 'exclude'

	void ResetMassAndCG();
	// re-calculates superstructure mass and centre of gravity.
	// Shifts global position to reflect CG change

	void ResetSize();

	void CalcPMI();
	// calculate PMI (principal axes of inertia for the superstructure,
	// given the sub-vessels and their relative orientation

	void UpdateProxies();
	// update reference body

	void SetOrbitReference (CelestialBody *body);
	// reset reference object for element calculation to "body"

	void ComponentStateVectors (const StateVectors *s, StateVectors *scomp, int comp) const;
	// Returns state vectors scomp for component vessel comp, given supervessel
	// state vectors s

	void AddComponentForceAndMoment (Vector *F, Vector *M,
		const Vector *Fcomp, const Vector *Mcomp, int comp) const;
	// Adds component force Fcomp and moment Mcomp from component vessel 'comp' to the
	// supervessel force F and moment M

	// *** component data ***
	SubVesselData *vlist; // list of associated vessels
	DWORD nv;             // length of vlist;

	// *** global data ***
	Vector cg;            // centre of gravity in supervessel coords
	// Note: The supervessel origin is the origin of the first vessel in the
	// list, not the CG of the composite structure.

	Vector Flin, Amom;
	// linear, angular forces on structure other than gravitational;
	// collected from vessel components

	bool bActivationPending; // switch to active mode

	double proxyT;         // update time for proxies

	// used for gravitation force updates
	int updcount;               // update counter
	//bool bOrbitStabilised;      // stabilised orbit

	TOUCHDOWN_VTX hullvtx; // used by hull vertex iterator
	DWORD next_hullvessel; // used by hull vertex iterator
};

#endif // !__SUPERVESSEL_H