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

#include "Orbiter.h"
#include "Rigidbody.h"
#include "Celbody.h"
#include "Psys.h"
#include "Element.h"
#include "Astro.h"
#include "Log.h"

using namespace std;

// =======================================================================
// Externals

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern PlanetarySystem *g_psys; // pointer to planetary system
extern char DBG_MSG[256];

bool       RigidBody::bDistmass = false;
bool       RigidBody::bGPerturb = false;
int        RigidBody::nPropLevel = 1;
RigidBody::PROPMODE RigidBody::PropMode[MAX_PROP_LEVEL] = {&RigidBody::RK2_LinAng, 0, 0.0, 0.0, 0.0, 0.0};

const double gfielddata_updt_interval = 60.0;

inline Vector Call_EulerInv_full (RigidBody *body, const Vector &tau, const Vector &omega)
{ return body->EulerInv_full (tau, omega); }
inline Vector Call_EulerInv_simple (RigidBody *body, const Vector &tau, const Vector &omega)
{ return body->EulerInv_simple (tau, omega); }
inline Vector Call_EulerInv_zero (RigidBody *body, const Vector &tau, const Vector &omega)
{ return body->EulerInv_zero (tau, omega); }

// =======================================================================
// class RigidBody

RigidBody::RigidBody (): Body ()
{
	SetDefaultCaps ();
}

RigidBody::RigidBody (double _mass, double _size, const Vector &_pmi): Body (_mass, _size)
{
	SetDefaultCaps ();
	pmi.Set (_pmi);
}

RigidBody::RigidBody (char *fname): Body (fname)
{
	SetDefaultCaps ();
	ifstream ifs (g_pOrbiter->ConfigPath (fname));
	if (ifs) ReadGenericCaps (ifs);
}

RigidBody::~RigidBody ()
{
	if (el) delete el;
}

void RigidBody::GlobalSetup ()
{
	SetupPropagationModes();
}

void RigidBody::SetDefaultCaps ()
{
	el          = 0;
	el_valid    = false;

	pmi.Set (-1,-1,-1); // "undef"
	bDynamicPosVel = true;
	bCanUpdateStabilised = g_pOrbiter->Cfg()->CfgPhysicsPrm.bOrbitStabilise;
	bDistmass = g_pOrbiter->Cfg()->CfgPhysicsPrm.bDistributedMass;
	bGPerturb = g_pOrbiter->Cfg()->CfgPhysicsPrm.bNonsphericalGrav;
	bOrbitStabilised = false;
	bIgnoreGravTorque = false;
	tidaldamp = 0.0;
	PropLevel = 0;
	PropSubMax = g_pOrbiter->Cfg()->CfgPhysicsPrm.PropSubMax;
	nPropSubsteps = 1;
	gfielddata.ngrav = 0;
	gfielddata.updt = -1e10; // invalidate
}

void RigidBody::ReadGenericCaps (ifstream &ifs)
{
	GetItemVector (ifs, "Inertia", pmi);
	GetItemReal (ifs, "GravityGradientDamping", tidaldamp);
}

void RigidBody::SetDefaultState ()
{
	arot.Set (0,0,0);
	torque.Set (0,0,0);
}

void RigidBody::SetOrbitReference (CelestialBody *body)
{
	if (body && body != cbody) { // otherwise nothing to do
		cbody = body;
		el->Setup (mass, cbody->Mass(), el->MJDepoch());
		el_valid = false;
	}
}

const Elements *RigidBody::Els () const
{
	extern bool g_bStateUpdate;
	if (cbody && el) {
		if (!el_valid) {
			el->Calculate (cpos, cvel, g_bStateUpdate ? td.SimT1 : td.SimT0);
			//el->Calculate (s0->pos-cbody->GPos(), s0->vel-cbody->GVel(), td.SimT0);
			el_valid = true;
		}
		return el;
	} else {
		return 0;
	}
}

void RigidBody::SetupPropagationModes ()
{
	int i;
	nPropLevel = g_pOrbiter->Cfg()->CfgPhysicsPrm.nLPropLevel;
	for (i = 0; i < nPropLevel; i++) {
		PropMode[i].ttgt = g_pOrbiter->Cfg()->CfgPhysicsPrm.PropTTgt[i];
		PropMode[i].atgt = g_pOrbiter->Cfg()->CfgPhysicsPrm.PropATgt[i];
		PropMode[i].tlim = g_pOrbiter->Cfg()->CfgPhysicsPrm.PropTLim[i];
		PropMode[i].alim = g_pOrbiter->Cfg()->CfgPhysicsPrm.PropALim[i];
		PropMode[i].propidx = g_pOrbiter->Cfg()->CfgPhysicsPrm.PropMode[i];
		switch (PropMode[i].propidx) {
		case PROP_RK2:  PropMode[i].propagator = &RigidBody::RK2_LinAng;  break;
		case PROP_RK4:  PropMode[i].propagator = &RigidBody::RK4_LinAng;  break;
		case PROP_RK5:  PropMode[i].propagator = &RigidBody::RK5_LinAng;  break;
		case PROP_RK6:  PropMode[i].propagator = &RigidBody::RK6_LinAng;  break;
		case PROP_RK7:  PropMode[i].propagator = &RigidBody::RK7_LinAng;  break;
		case PROP_RK8:  PropMode[i].propagator = &RigidBody::RK8_LinAng;  break;
		case PROP_SY2:  PropMode[i].propagator = &RigidBody::SY2_LinAng;  break;
		case PROP_SY4:  PropMode[i].propagator = &RigidBody::SY4_LinAng;  break;
		case PROP_SY6:  PropMode[i].propagator = &RigidBody::SY6_LinAng;  break;
		case PROP_SY8:  PropMode[i].propagator = &RigidBody::SY8_LinAng;  break;
		default:        PropMode[i].propagator = &RigidBody::RK4_LinAng;  break;
		}
	}
	PropMode[nPropLevel-1].tlim = 1e20;
	PropMode[nPropLevel-1].alim = 1e20;
}

// =======================================================================

void RigidBody::SetPropagator (int &plevel, int &nstep) const
{
	// 1. Time step limit
	for (plevel = 0; plevel < nPropLevel-1; plevel++)
		if (td.SimDT < PropMode[plevel].tlim)
			break;
	// 2. Angle step limit
	double astep = s0->omega.length()*td.SimDT; // angular step size
	for (; plevel < nPropLevel-1; plevel++)
		if (astep < PropMode[plevel].alim)
			break;

	nstep = min (PropSubMax, (int)ceil (max (td.SimDT / PropMode[plevel].ttgt, astep / PropMode[plevel].atgt)));
}

// =======================================================================

void RigidBody::Update (bool force)
{
	if (bDynamicPosVel) {

		// Update velocity and position according to
		// graviational field.
		// Notes:
		// 1. Dynamic updates are only allowed for toplevel
		//    objects, i.e. no parent and rpos=gpos
		// 2. We split rpos and rvel into a base and
		//    incremental part to minimise roundoff errors

		int i;
		Vector tau;
		pcpos.Set (cpos);
		ostep = cvel.length()*td.SimDT / (Pi2 * cpos.length());

		// approx. orbit step length (fraction of full orbit)
		bIgnoreGravTorque = (!bDistmass || ostep > 0.1 /*||
			vrot.length()*SimDT > Pi2*0.01*/);
		// flag for suppressing gravity-gradient torque (to avoid numerical instability)

		// Update the list of gravity field sources
		if (force || !gfielddata.ngrav) {
			ScanGFieldSources (g_psys);
			gfielddata.updt = td.SimT0 + (gfielddata_updt_interval*rand())/RAND_MAX;
			// randomize update times
		} else if (td.SimT0 > gfielddata.updt) {
			UpdateGFieldSources (g_psys);
			gfielddata.updt = td.SimT0 + gfielddata_updt_interval;
		}

		// First check if we should do a stabilised state update
		if (bCanUpdateStabilised &&
			ostep > g_pOrbiter->Cfg()->CfgPhysicsPrm.Stabilise_SLimit &&
			g_psys->GetGravityContribution (cbody, cpos+cbody->GPos()) > 1-g_pOrbiter->Cfg()->CfgPhysicsPrm.Stabilise_PLimit) {

			nPropSubsteps = 1; // for now
			double dt = td.SimDT/nPropSubsteps;

			// Update linear state with Encke's method
			s1->Set (*s0);
			if (!bOrbitStabilised) {
				FlushRPos();
				FlushRVel();
				s1->pos.Set (cpos);
				s1->vel.Set (cvel);
				GetIntermediateMoments_pert (acc_pert, tau, *s0, 0, dt, cbody);
				el->Calculate (cpos, cvel, td.SimT0); // get elements from previous step
			}
			Encke();
			s1->pos.Set (cpos + cbody->s1->pos);
			s1->vel.Set (cvel + cbody->s1->vel);
			FlushRPos();
			FlushRVel();
			s1->R.Set (s1->Q);
			GetIntermediateMoments (acc, tau, *s1, 1, dt);
			el_valid = bOrbitStabilised = true;

		} else { // do a dynamic state vector integration

			Vector acc0(acc), arot0(arot);
			Vector rpos_add0(rpos_add), rvel_add0(rvel_add);
			do {
				// Select propagator
				SetPropagator (PropLevel, nPropSubsteps);
				double dt = td.SimDT/nPropSubsteps;

				// Perform step propagation with sub-steps
				s1->Set (*s0);
				acc = acc0, arot = arot0;
				rpos_add = rpos_add0, rvel_add = rvel_add0;
				for (i = 0; i < nPropSubsteps; i++) {
					((*this).*(PropMode[PropLevel].propagator)) (dt, nPropSubsteps, i);
					s1->pos = rpos_base + rpos_add;
					s1->vel = rvel_base + rvel_add;
					s1->R.Set (s1->Q);
					GetIntermediateMoments (acc, tau, *s1, (i+1.0)/nPropSubsteps, dt);
					arot.Set (EulerInv_full (tau, s1->omega));
				}
			} while (!ValidateStateUpdate (s1));
			//s1->R.Set (s1->Q);

			if (updcount++ == 1000) { // flush increments
				rpos_base += rpos_add;
				rpos_add.Set (0,0,0);
				rvel_base += rvel_add;
				rvel_add.Set (0,0,0);
				updcount = 0;
			}
			el_valid = bOrbitStabilised = false;

		}

		// Limit angular velocity - this code is intended to prevent
		// numerical instabilities in the angular integration to
		// leading to a velocity explosion - a last resort
		const double vmag_max = 100.0*Pi; // limit angular velocities to 100Hz - make user-definable!
		extern int errno;
		errno = 0;
		double vmag = s1->omega.length();
		if (vmag > vmag_max || errno) {
			if (errno) {                  // fatal error - reset to arbitrary values
				s1->omega.Set (0,0,vmag_max);
				vmag = vmag_max;
				s1->Q.Set (0,0,0,1);
				s1->R.Set (s0->Q);
			} else {
				s1->omega *= vmag_max/vmag;
				vmag = vmag_max;
			}
			arot.Set(0,0,0);
		}
	}
	Body::Update (force);

	if (cbody) {
		cpos = s1->pos - cbody->s1->pos;
		cvel = s1->vel - cbody->s1->vel;
	}
}

// =======================================================================

void RigidBody::ScanGFieldSources (const PlanetarySystem *psys)
{
	psys->ScanGFieldSources (&s0->pos, this, &gfielddata);
}

// =======================================================================

void RigidBody::UpdateGFieldSources (const PlanetarySystem *psys)
{
	psys->UpdateGFieldSources (&s0->pos, this, &gfielddata);
}

// =======================================================================

Vector RigidBody::InterpolatePos (const Vector &p0, const Vector &p1, double t1, double dt, double tfrac) const
{
	if (ostep < 1e-3) {        // use simple linear interpolation
		return p0 + (p1-p0)*tfrac;
	} else if (ostep < 1e-2) { // slightly more sophisticated interpolation
		Vector dir (p0 + (p1-p0)*tfrac);
		return dir.unit() * (p0.length()*(1.0-tfrac) + p1.length()*tfrac);
	} else {                   // use Kepler orbit interpolation
		if (!el_valid) { 
			el->Calculate (cpos, cvel, td.SimT0);
			el_valid = true;
		}
		return el->Pos (t1 + (tfrac-1.0)*dt);
	}
}

// =======================================================================

void RigidBody::GetIntermediateMoments (Vector &acc, Vector &tau,
	const StateVectors &state, double tfrac, double dt)
{
	// linear acceleration due to graviational field
	acc = g_psys->Gacc_intermediate (state.pos, tfrac, this, &gfielddata);

	// angular acceleration due to gravity gradient torque
	if (!cbody || bIgnoreGravTorque) {
		tau.Set (0,0,0);
	} else {
		// map cbody into vessel frame
		Vector R0 (tmul (state.Q, cbody->InterpolatePosition (tfrac) - state.pos));
		double r0 = R0.length();
		Vector Re = R0/r0;
		double mag = 3.0 * Ggrav * cbody->Mass() / pow(r0,3.0);
		tau.Set (crossp (pmi*Re, Re) * mag);

		// damping of angular velocity
		if (tidaldamp) {
			double damp  = tidaldamp * mag;
			double scale = min (damp, dt*0.1);
			if (state.omega.x) tau.x -= scale * pmi.x*state.omega.x;
			if (state.omega.y) tau.y -= scale * pmi.y*state.omega.y;
			if (state.omega.z) tau.z -= scale * pmi.z*state.omega.z;
		}
	}
}

// =======================================================================

void RigidBody::GetIntermediateMoments_pert (Vector &acc, Vector &tau,
	const StateVectors &state_rel, double tfrac, double dt, const CelestialBody *cbody)
{
	// Note: Encke's method in the current implementation doesn't seem
	// very stable. Therefore we simply disable gravitational perturbations
	// altogether to revert to a simple 2-body solution
#define NO_GRAV_PERT
#ifdef NO_GRAV_PERT
	acc.Set (0,0,0);
	tau.Set (0,0,0);
#else
	Vector gpos = state_rel.pos + cbody->InterpolatePosition (tfrac);
	// linear acceleration due to graviational field
	acc = g_psys->Gacc_intermediate_pert (cbody, state_rel.pos, tfrac, this, &gfielddata);

	// angular acceleration due to gravity gradient torque
	if (!cbody || bIgnoreGravTorque) {
		tau.Set (0,0,0);
	} else {
		// map cbody into vessel frame
		Vector R0 (tmul (state_rel.Q, cbody->InterpolatePosition (tfrac) - gpos));
		double r0 = R0.length();
		Vector Re = R0/r0;
		double mag = 3.0 * Ggrav * cbody->Mass() / pow(r0,3.0);
		tau.Set (crossp (pmi*Re, Re) * mag);

		// damping of angular velocity
		if (tidaldamp) {
			double damp  = tidaldamp * mag;
			double scale = min (damp, dt*0.1);
			if (state_rel.omega.x) tau.x -= scale * pmi.x*state_rel.omega.x;
			if (state_rel.omega.y) tau.y -= scale * pmi.y*state_rel.omega.y;
			if (state_rel.omega.z) tau.z -= scale * pmi.z*state_rel.omega.z;
		}
	}
#endif
}

// =======================================================================

Vector RigidBody::GetPertAcc (const PertIntData &data, const Vector &pos, double tfrac)
{
	// acceleration perturbation: difference of the perturbation fields
	// between vessel position and central body position (at last step)

	static Vector zero(0,0,0);
	Vector accp (g_psys->GaccRel (pos, cbody, tfrac, cbody, &gfielddata) - g_psys->GaccRel (zero, cbody, tfrac, cbody, &gfielddata));

	if (data.nonspherical)
		accp += SingleGacc_perturbation (-pos, cbody);

	return accp;
}

// =======================================================================

Vector RigidBody::GetTorque () const
{
	if (!cbody || bIgnoreGravTorque) return Vector(0,0,0); // sanity check

	Vector R0;

	// map cbody into vessel frame
	R0.Set (tmul (s0->R, cbody->s0->pos - s0->pos));
	double r0 = R0.length();
	Vector Re = R0/r0;
	double mag = 3.0 * Ggrav * cbody->Mass() / pow(r0,3.0);
	Vector M (crossp (pmi*Re, Re) * mag);

	// damping of angular velocity
	if (tidaldamp) {
		double damp  = tidaldamp * mag;
		double scale = min (damp, td.iSimDT*0.1);
		if (s0->omega.x) M.x -= scale * pmi.x*s0->omega.x;
		if (s0->omega.y) M.y -= scale * pmi.y*s0->omega.y;
		if (s0->omega.z) M.z -= scale * pmi.z*s0->omega.z;
	}

	return M;
}

// =======================================================================

void RigidBody::SetAngVel (const Vector &omega)
{
	s0->omega.Set (omega);
}

// =======================================================================

Vector RigidBody::Euler_full (const Vector &omegadot, const Vector &omega) const
{
	return Vector (
		omegadot.x*pmi.x + (pmi.y-pmi.z)*omega.y*omega.z,
		omegadot.y*pmi.y + (pmi.z-pmi.x)*omega.z*omega.x,
		omegadot.z*pmi.z + (pmi.x-pmi.y)*omega.x*omega.y);
}

// =======================================================================

Vector RigidBody::EulerInv_full (const Vector &tau, const Vector &omega) const
{
	// Solves Euler's equation (in left-handed system):
	//
	//    I domega/dt + (I omega) x omega = tau
	//
	// for angular acceleration domega/dt, given torque tau, angular
	// velocity omega and inertia tensor I (assumed to be diagonal).

	return Vector (
		(tau.x - (pmi.y-pmi.z)*omega.y*omega.z) / pmi.x,
		(tau.y - (pmi.z-pmi.x)*omega.z*omega.x) / pmi.y,
		(tau.z - (pmi.x-pmi.y)*omega.x*omega.y) / pmi.z);
}

// =======================================================================

Vector RigidBody::EulerInv_simple (const Vector &tau, const Vector &omega) const
{
	// Simplified version of Euler's equation: ignores coupling terms,
	// and only solves
	//
	//    I domega/dt = tau
	//
	// for angular acceleration domega/dt, given torque tau and inertia
	// tensor I (assumed to be diagonal).
	// Used at high time acceleration to avoid instabilities.

	return Vector (tau.x/pmi.x, tau.y/pmi.y, tau.z/pmi.z);
}

// =======================================================================

Vector RigidBody::EulerInv_zero (const Vector &tau, const Vector &omega) const
{
	// Trivial version of Euler's equation: suppresses cross-axis
	// coupling terms and torque, i.e. solves
	//
	//    I domega/dt = 0
	//
	// for angular acceleration domega/dt, and thus returns zero.

	return Vector (0,0,0);
}

// =======================================================================

void RigidBody::SetDynamicUpdate (bool dynamic)
{
	if (bDynamicPosVel == dynamic) return; // nothing to do
	if (bDynamicPosVel = dynamic) {
		rpos_base.Set (s0->pos);
		rpos_add.Set (0,0,0);
	} else {
	}
}

const char *RigidBody::PropagatorStr (DWORD idx, bool verbose) {
	static const char *ShortPropModeStr[NPROP_METHOD] = {
		"RK2", "RK4", "RK5", "RK6", "RK7", "RK8",
		"SY2", "SY4", "SY6", "SY8"
	};
	static const char *LongPropModeStr[NPROP_METHOD] = {
		"Runge-Kutta, 2nd order (RK2)", "Runge-Kutta, 4th order (RK4)", "Runge-Kutta, 5th order (RK5)", "Runge-Kutta, 6th order (RK6)",
		"Runge-Kutta, 7th order (RK7)", "Runge-Kutta, 8th order (RK8)",
		"Symplectic, 2nd order (SY2)", "Symplectic, 4th order (SY4)", "Symplectic, 6th order (SY6)", "Symplectic, 8th order (SY8)"
	};
	return (idx < NPROP_METHOD ? (verbose ? LongPropModeStr[idx] : ShortPropModeStr[idx]) : "unknown");
}

const char *RigidBody::CurPropagatorStr (bool verbose) const
{
	if (!bDynamicPosVel) return "none";
	else return PropagatorStr (PropMode[PropLevel].propidx, verbose);
}

const char *RigidBody::RotationModel () const
{
	static char cbuf[64];
	if      (aidata.AngAcc == Call_EulerInv_full)   strcpy (cbuf, "Euler");
	else if (aidata.AngAcc == Call_EulerInv_simple) strcpy (cbuf, "decoupled");
	else                                         strcpy (cbuf, "none");
	return cbuf;
}
