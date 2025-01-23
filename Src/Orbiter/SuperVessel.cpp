// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
// class SuperVessel
// A "vessel container" class which handles vessels which are grouped
// by docking. SuperVessels are created on the fly when vessels dock
// The SuperVessel manages position updates and interprets thrust
// events of the attached vessels.
// ==============================================================

#include "Orbiter.h"
#include "Element.h"
#include "SuperVessel.h"
#include "Psys.h"
#include "Log.h"
#include <stdio.h>

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern PlanetarySystem *g_psys; // pointer to planetary system
extern Vessel *g_focusobj;
extern bool g_bStateUpdate;
extern char DBG_MSG[256];

// ==============================================================
// class SuperVessel

SuperVessel::SuperVessel (Vessel *vessel)
: VesselBase (vessel->mass, vessel->size, vessel->pmi)
{
	// Create a SuperVessel containing a single component
	// Usually this should be followed by adding one or more additional components

	vlist = new SubVesselData[nv = 1]; TRACENEW
	vlist[0].vessel = vessel;
	vlist[0].rrot.Set (1,0,0, 0,1,0, 0,0,1); // identity
	vlist[0].rpos.Set (0,0,0);
	cg.Set (0,0,0);
	s0->vel.Set (vessel->GVel());
	rvel_base.Set (s0->vel);
	rvel_add.Set (0,0,0);
	s0->R.Set (vessel->GRot());
	s0->Q.Set (vessel->GQ());
	bOrbitStabilised = false;
	cbody = 0;
	size = vessel->Size();
	el = new Elements; TRACENEW
	s0->omega.Set (vessel->AngularVelocity());
	s0->pos.Set (vessel->GPos());
	rpos_base.Set (s0->pos);
	rpos_add.Set (0,0,0);
	rvel_base.Set (s0->vel);
	rvel_add.Set (0,0,0);
	gfielddata.ngrav = 0;
	updcount = irand (100);
	acc = g_psys->Gacc (s0->pos, 0, &gfielddata);

	UpdateProxies();
	cpos = s0->pos - cbody->GPos();
	cvel = s0->vel - cbody->GVel();
	proxyT    = -(double)rand()*100.0/(double)RAND_MAX - 1.0;

	// register with vessel
	vessel->SetSuperStruct (this);
	//if (s1) { // is this necessary?
	//	vessel->FlushRPos();
	//	vessel->FlushRVel();
	//}

	SetDefaultState();
}

SuperVessel::SuperVessel (Vessel *vessel1, Vessel *vessel2, int port1, int port2, bool mixmoments)
: VesselBase ()
{
	dCHECK(!g_bStateUpdate, "SuperVessel constructor must not be called during state update") // only create supervessels outside update phase

	int i;

	vlist = new SubVesselData[nv = 2]; TRACENEW

	vlist[0].vessel = vessel1;
	vlist[1].vessel = vessel2;

	// by definition the SuperVessel has the first vessel's position and orientation
	vlist[0].rpos.Set (0,0,0);
	vlist[0].rrot.Set (1,0,0, 0,1,0, 0,0,1); // identity

	// calculate position and orientation of second vessel relative to first
	vessel1->RelDockingPos (vessel2, port1, port2, vlist[1].rpos, vlist[1].rrot);
	vlist[1].rq.Set (vlist[1].rrot);

	// total mass, centre of gravity and velocity
	cg.Set(0,0,0);
	mass = 0.0;
	for (i = 0; i < 2; i++) {
		mass += vlist[i].vessel->mass;
		cg += vlist[i].rpos * vlist[i].vessel->mass;
	}
	cg /= mass;

	// supervessel orientation
	s0->R.Set (vessel1->s0->R);
	s0->Q.Set (vessel1->s0->Q);
	bOrbitStabilised = false;
	cbody = 0;
	el = new Elements; TRACENEW

	// add up linear momentae
	if (mixmoments) {
		s0->vel.Set(0,0,0);
		for (i = 0; i < 2; i++) s0->vel += vlist[i].vessel->s0->vel * vlist[i].vessel->mass;
		s0->vel /= mass;
	} else s0->vel.Set (vlist[0].vessel->s0->vel);
	rvel_base.Set (s0->vel);
	rvel_add.Set (0,0,0);

	// total principal axes of inertia
	CalcPMI ();

	// add up angular momentae
	if (mixmoments) {
		// calculate linear and angular velocity from conservation of linear/angular momentum
		Vector am;
		for (i = 0; i < 2; i++) {
			// individual spin of each vessel
			am += mul (vlist[i].rrot, vlist[i].vessel->AngularMomentum()) * vlist[i].vessel->mass;
			// contribution of vessel motion to angular momentum
			am += crossp (tmul (s0->R, vlist[i].vessel->s0->vel - s0->vel), vlist[i].rpos-cg) * vlist[i].vessel->mass;
		}
		s0->omega.Set (am.x/pmi.x, am.y/pmi.y, am.z/pmi.z);
		s0->omega /= mass;
	} else {
		// set linear/angular velocity directly from reference vessel
		s0->omega.Set (vlist[0].vessel->AngularVelocity());
	}

	// supervessel position
	s0->pos.Set (vessel1->s0->pos + mul (s0->R, cg));
	rpos_base.Set (s0->pos);
	rpos_add.Set (0,0,0);
	gfielddata.ngrav = 0;
	updcount = irand (100); // spread update times

	acc = g_psys->Gacc (s0->pos, 0, &gfielddata);
	// init acc and gfielddata

	UpdateProxies();
	if (cbody) cpos = s0->pos - cbody->s0->pos; else cpos = { 0, 0, 0 };
	if (cbody) cvel = s0->vel - cbody->s0->vel; else cvel = { 0, 0, 0 };
	proxyT = -(double)rand()*100.0/(double)RAND_MAX - 1.0;

	// register with vessels
	for (i = 0; i < 2; i++) {
		Vessel *v = vlist[i].vessel;
		v->SetSuperStruct (this);
		ComponentStateVectors (s0, v->s0, i);
		v->rpos_base = v->s0->pos; v->rpos_add.Set (0,0,0);
		v->rvel_base = v->s0->vel; v->rvel_add.Set (0,0,0);
		if (v->s1) v->s1->Set(*v->s0);
		v->UpdateSurfParams();
	}

	SetDefaultState();
	ResetSize();
	Activate (true);
}

SuperVessel::~SuperVessel()
{
	if (nv) {
		delete []vlist;
		vlist = NULL;
	}
}

// =======================================================================

void SuperVessel::SetDefaultState ()
{
	VesselBase::SetDefaultState ();

	bActivationPending = false;
	next_hullvessel = 0;
	sp.is_in_atm        = false;
}

// =======================================================================

void SuperVessel::Attach (Vessel *vessel1, int port1, Vessel *vessel2, int port2)
{
	if (!vessel1->supervessel)
		Add (vessel1, port1, vessel2, port2);
	else
		Merge (vessel1, port1, vessel2, port2);

	Activate (true);
}

// =======================================================================

void SuperVessel::Detach (Vessel *vessel, DWORD port, double vsep)
{
	DWORD i, j, port2, idx1, idx2;
	bool atomic1, atomic2;

	Vessel *vessel2 = vessel->dock[port]->mate;
	if (!vessel2) return; // sanity check - is dock in use?
	port2 = vessel->dock[port]->matedock;

	// find vlist indices
	for (i = 0; i < nv; i++) {
		if      (vlist[i].vessel == vessel ) idx1 = i;
		else if (vlist[i].vessel == vessel2) idx2 = i;
	}

	// check whether vessel1 becomes an atomic vessel
	for (i = 0, atomic1 = true; i < vessel->ndock; i++)
		if (i != port && vessel->dock[i]->mate) { atomic1 = false; break; }

	// check whether vessel2 becomes an atomic vessel
	for (i = 0, atomic2 = true; i < vessel2->ndock; i++)
		if (i != port2 && vessel2->dock[i]->mate) { atomic2 = false; break; }

	// give it a separation push
	double vs = vsep * vessel->mass / mass; // structure velocity
	double vv = vsep - vs;                  // vessel velocity
	Vector sepdir (mul (vessel->GRot(), vessel->dock[port]->dir));
	Vector rotvel;

	if (atomic1) {
		// velocity component from rotation: v = omega x r
		rotvel = mul (s0->R, crossp (vlist[idx1].rpos-cg, s0->omega));
		vessel->RPlace_individual (mul (s0->R, vlist[idx1].rpos-cg) + s0->pos, s0->vel - sepdir*vv + rotvel);
		//vessel->acc = g_psys->Gacc (vessel->rpos, vessel, &vessel->gfielddata);
		vessel->SetSuperStruct (NULL);
		if (atomic2) { // disband superstructure
			// velocity component from rotation: v = omega x r
			rotvel = mul (s0->R, crossp (vlist[idx2].rpos-cg, s0->omega));
			vessel2->RPlace_individual (mul (s0->R, vlist[idx2].rpos-cg) + s0->pos, s0->vel + sepdir*vs + rotvel);
			//vessel2->acc = g_psys->Gacc (vessel2->rpos, vessel2, &vessel2->gfielddata);
			vessel2->SetSuperStruct (NULL);
			delete []vlist;
			vlist = NULL;
			nv = 0;
		} else { // remove vessel1 from superstructure
			SubVesselData *tmp = new SubVesselData[nv-1]; TRACENEW
			for (i = j = 0; i < nv; i++)
				if (i != idx1) tmp[j++] = vlist[i];
			delete []vlist;
			vlist = tmp;
			nv--;
			rvel_add += sepdir*vs;
		}
	} else {
		if (atomic2) {
			// velocity componet from rotation: v = omega x r
			rotvel = mul (s0->R, crossp (vlist[idx2].rpos-cg, s0->omega));
			vessel2->RPlace_individual (mul (s0->R, vlist[idx2].rpos-cg) + s0->pos, s0->vel + sepdir*vs + rotvel);
			vessel2->SetSuperStruct (NULL);
			SubVesselData *tmp = new SubVesselData[nv-1]; TRACENEW
			for (i = j = 0; i < nv; i++)
				if (i != idx2) tmp[j++] = vlist[i];
			delete []vlist;
			vlist = tmp;
			nv--;
			rvel_add -= sepdir*vv;
		} else {
			// split into 2 supervessels
			rotvel = mul (s0->R, crossp (vlist[idx2].rpos-cg, s0->omega));
			vessel2->RPlace_individual (mul (s0->R, vlist[idx2].rpos-cg) + s0->pos, s0->vel + sepdir*vs + rotvel);
			SuperVessel *sv2 = new SuperVessel (vessel2); TRACENEW
			g_psys->AddSuperVessel (sv2);
			RemoveEntry (vessel2);
			TransferAllDocked (vessel2, sv2, vessel);
		}
	}
	if (nv) {
		ResetMassAndCG();
		ResetSize();
		CalcPMI();
	}
	bOrbitStabilised = false;
}

// =======================================================================

bool SuperVessel::Activate (bool force)
{
	if (VesselBase::Activate (force)) {
		for (int i = 0; i < nv; i++) {
			vlist[i].vessel->fstatus = FLIGHTSTATUS_FREEFLIGHT;
			// none of the following should be necessary
			vlist[i].vessel->rpos_base = vlist[i].vessel->s0->pos;
			vlist[i].vessel->rpos_add.Set (0,0,0);
			vlist[i].vessel->s0->vel += vlist[i].vessel->rvel_add;
			vlist[i].vessel->rvel_base = vlist[i].vessel->s0->vel;
			vlist[i].vessel->rvel_add.Set (0,0,0);
			vlist[i].vessel->s0->omega.Set (0,0,0);
		}
		return true;
	} else
		return false;
}

// =======================================================================

void SuperVessel::RPlace (const Vector &_rpos, const Vector &_rvel, const Vessel *ref)
{
	DWORD i;
	Vector dp = _rpos-s0->pos;
	if (ref)
		for (i = 0; i < nv; i++)
			if (vlist[i].vessel == ref) {
				dp -= mul (s0->R, vlist[i].rpos-cg);
				break;
			}
	rpos_base = s0->pos += dp;
	rpos_add.Set (0,0,0);
	rvel_base = s0->vel = _rvel;
	rvel_add.Set (0,0,0);
	cpos = s0->pos - vlist[0].vessel->cbody->GPos();
	cvel = s0->vel - vlist[0].vessel->cbody->GVel();
	for (i = 0; i < nv; i++) // update component states
		vlist[i].vessel->RPlace_individual (vlist[i].vessel->s0->pos + dp, s0->vel);
	fstatus = FLIGHTSTATUS_FREEFLIGHT;
	bOrbitStabilised = false;
	UpdateProxies();
	UpdateSurfParams();
}

// =======================================================================

void SuperVessel::SetGlobalOrientation (const Vector &arot, const Vessel *ref)
{
	DWORD i;
	double sinx = sin(arot.x), cosx = cos(arot.x);
	double siny = sin(arot.y), cosy = cos(arot.y);
	double sinz = sin(arot.z), cosz = cos(arot.z);
	s0->R.Set (cosy*cosz,                cosy*sinz,               -siny,
	           sinx*siny*cosz-cosx*sinz, sinx*siny*sinz+cosx*cosz, sinx*cosy,
			   cosx*siny*cosz+sinx*sinz, cosx*siny*sinz-sinx*cosz, cosx*cosy);
	if (ref) {
		for (i = 0; i < nv; i++)
			if (vlist[i].vessel == ref) {
				s0->R.tpostmul (vlist[i].rrot);
				break;
			}
	}
	s0->Q.Set (s0->R);

	// update rotation for all sub-vessels
	for (i = 0; i < nv; i++) {
		Vessel *v = vlist[i].vessel;
		v->s0->pos.Set (mul (s0->R, vlist[i].rpos-cg) + s0->pos);
		v->s0->Q.Set (s0->Q);
		v->s0->Q.postmul (vlist[i].rq);
		v->s0->R.Set (v->s0->Q);
		v->UpdateAttachments();
	}
}

// =======================================================================

void SuperVessel::SetRotationMatrix (const Matrix &R, const Vessel *ref)
{
	DWORD i;
	s0->R.Set (R);
	if (ref) {
		for (i = 0; i < nv; i++)
			if (vlist[i].vessel == ref) {
				s0->R.tpostmul (vlist[i].rrot);
				break;
			}
	}
	s0->Q.Set (s0->R);

	// update rotation for all sub-vessels
	for (i = 0; i < nv; i++) {
		Vessel *v = vlist[i].vessel;
		v->s0->pos.Set (mul (s0->R, vlist[i].rpos-cg) + s0->pos);
		v->s0->R.Set (s0->R);
		v->s0->R.postmul (vlist[i].rrot);
		v->s0->Q.Set (v->s0->R);
		v->UpdateAttachments();
	}
}

// =======================================================================

void SuperVessel::SetAngVel (const Vector &omega, const Vessel *ref)
{
	DWORD i;
	s0->omega.Set (omega);
	if (ref) {
		for (i = 0; i < nv; i++)
			if (vlist[i].vessel == ref) {
				s0->omega.Set (mul (vlist[i].rrot, omega));		
			}
	}

	// update spin for all sub-vessels
	for (i = 0; i < nv; i++) {
		Vessel *v = vlist[i].vessel;
		v->SetAngVel_individual (tmul (vlist[i].rrot, s0->omega));
	}
}

// ==============================================================

void SuperVessel::GetIntermediateMoments (Vector &acc, Vector &tau,
	const StateVectors &state, double tfrac, double dt)
{
	// TODO: Move this up to VesselBase
	Vector F(Flin);
	Vector M(Amom);
	AddSurfaceForces (&F, &M, &state, tfrac, dt); // add ground contact forces and moments
	RigidBody::GetIntermediateMoments (acc, tau, state, tfrac, dt);  // get gravitational component
	acc += mul (state.Q, F/mass);
	tau += M/mass;
}

// ==============================================================

bool SuperVessel::RemoveEntry (const Vessel *v)
{
	DWORD i, j;
	bool found = false;
	if (nv <= 1) return false; // sanity check: can't remove last entry
	SubVesselData *tmp = new SubVesselData[nv-1]; TRACENEW
	for (i = j = 0; i < nv; i++) {
		if (vlist[i].vessel == v) found = true;
		else if (j < nv-1)        tmp[j++] = vlist[i];
	}
	if (found) {
		delete []vlist;
		vlist = tmp;
		nv--;
	}
	return found;
}

// =======================================================================

void SuperVessel::TransferAllDocked (Vessel *v, SuperVessel *sv, const Vessel *exclude)
{
	DWORD n;
	for (n = 0; n < v->ndock; n++) {
		Vessel *mate = v->dock[n]->mate;
		if (mate && mate != exclude && isComponent(mate)) {
			sv->Add (v, n, mate, v->dock[n]->matedock, false);
			RemoveEntry (mate);
			TransferAllDocked (mate, sv, v);
		}
	}
}

// =======================================================================

bool SuperVessel::isComponent (const Vessel *v) const
{
	for (DWORD i = 0; i < nv; i++)
		if (vlist[i].vessel == v) return true;
	return false;
}

// =======================================================================

void SuperVessel::SoftDockUpdate(Vessel* vessel2, PortSpec *pD)
{
	DWORD idx1;
	DWORD idx2;

	Vessel* vessel1 = pD->mate;
	DWORD port1 = pD->matedock;

	// make sure vessels are part of the same superstructure
	if (!vessel2->supervessel) return;
	if (vessel1->supervessel != vessel2->supervessel) return;

	// find index numbers in a superstructure
	for (idx1 = 0; idx1 < nv; idx1++) if (vessel1 == vlist[idx1].vessel) break;
	for (idx2 = 0; idx2 < nv; idx2++) if (vessel2 == vlist[idx2].vessel) break;
	
	// calculate orientation of new vessel relative to superstructure
	// 1. calc position of 2nd vessel relative to 1st
	Vector as(pD->dir);
	Vector bs(pD->rot);
	Vector cs(crossp(as, bs));
	Vector at(-vessel1->dock[port1]->dir);
	Vector bt(vessel1->dock[port1]->rot);
	Vector ct(crossp(at, bt));
	double den = cs.x * (as.y * bs.z - as.z * bs.y) +
				 cs.y * (as.z * bs.x - as.x * bs.z) +
				 cs.z * (as.x * bs.y - as.y * bs.x);
	vlist[idx2].rrot.m11 = (ct.x * (as.y * bs.z - as.z * bs.y) +
		bt.x * (as.z * cs.y - as.y * cs.z) +
		at.x * (bs.y * cs.z - bs.z * cs.y)) / den;
	vlist[idx2].rrot.m12 = (ct.x * (as.z * bs.x - as.x * bs.z) +
		bt.x * (as.x * cs.z - as.z * cs.x) +
		at.x * (bs.z * cs.x - bs.x * cs.z)) / den;
	vlist[idx2].rrot.m13 = (ct.x * (as.x * bs.y - as.y * bs.x) +
		bt.x * (as.y * cs.x - as.x * cs.y) +
		at.x * (bs.x * cs.y - bs.y * cs.x)) / den;
	vlist[idx2].rrot.m21 = (ct.y * (as.y * bs.z - as.z * bs.y) +
		bt.y * (as.z * cs.y - as.y * cs.z) +
		at.y * (bs.y * cs.z - bs.z * cs.y)) / den;
	vlist[idx2].rrot.m22 = (ct.y * (as.z * bs.x - as.x * bs.z) +
		bt.y * (as.x * cs.z - as.z * cs.x) +
		at.y * (bs.z * cs.x - bs.x * cs.z)) / den;
	vlist[idx2].rrot.m23 = (ct.y * (as.x * bs.y - as.y * bs.x) +
		bt.y * (as.y * cs.x - as.x * cs.y) +
		at.y * (bs.x * cs.y - bs.y * cs.x)) / den;
	vlist[idx2].rrot.m31 = (ct.z * (as.y * bs.z - as.z * bs.y) +
		bt.z * (as.z * cs.y - as.y * cs.z) +
		at.z * (bs.y * cs.z - bs.z * cs.y)) / den;
	vlist[idx2].rrot.m32 = (ct.z * (as.z * bs.x - as.x * bs.z) +
		bt.z * (as.x * cs.z - as.z * cs.x) +
		at.z * (bs.z * cs.x - bs.x * cs.z)) / den;
	vlist[idx2].rrot.m33 = (ct.z * (as.x * bs.y - as.y * bs.x) +
		bt.z * (as.y * cs.x - as.x * cs.y) +
		at.z * (bs.x * cs.y - bs.y * cs.x)) / den;

	// 2. premultipy with vessel1's rotation matrix
	vlist[idx2].rrot.premul(vlist[idx1].rrot);
	vlist[idx2].rq.Set(vlist[idx2].rrot);

	// position of vessel2 in superstructure
	vlist[idx2].rpos.Set(vlist[idx1].rpos + mul(vlist[idx1].rrot, vessel1->dock[port1]->ref) - mul(vlist[idx2].rrot, pD->ref));

	vessel2->SetSuperStruct(this);
	// flush the state increments
	vessel2->rpos_base = vessel2->s0->pos; vessel2->rpos_add.Set(0, 0, 0);
	vessel2->rvel_base = vessel2->s0->vel; vessel2->rvel_add.Set(0, 0, 0);
	vessel2->s0->Q.Set(s0->Q);
	vessel2->s0->Q.postmul(vlist[idx2].rq);
	vessel2->s0->R.Set(vessel2->s0->Q);

	ResetMassAndCG();
	ResetSize();
	CalcPMI();
	bOrbitStabilised = false;

	vessel2->s0->omega.Set(tmul(vlist[idx2].rrot, s0->omega));
	vessel2->UpdateSurfParams();
}

// =======================================================================

bool SuperVessel::Add (Vessel *vessel1, int port1, Vessel *vessel2, int port2, bool mixmoments)
{
	DWORD idx1;

	// make sure vessel1 is part of the superstructure
	for (idx1 = 0; idx1 < nv; idx1++)
		if (vessel1 == vlist[idx1].vessel) break;
	if (idx1 == nv) return false; // not found

	// create new entry in vessel list
	SubVesselData *tmp = new SubVesselData[nv+1]; TRACENEW
	if (nv) {
		memcpy (tmp, vlist, nv*sizeof(SubVesselData));
		delete []vlist;
	}
	vlist = tmp;
	vlist[nv].vessel = vessel2;

	// calculate orientation of new vessel relative to superstructure
	// 1. calc position of 2nd vessel relative to 1st
	Vector as(vessel2->dock[port2]->dir);
	Vector bs(vessel2->dock[port2]->rot);
	Vector cs(crossp(as,bs));
	Vector at(-vessel1->dock[port1]->dir);
	Vector bt(vessel1->dock[port1]->rot);
	Vector ct(crossp(at,bt));
	double den         =  cs.x * (as.y*bs.z - as.z*bs.y) +
		                  cs.y * (as.z*bs.x - as.x*bs.z) +
				          cs.z * (as.x*bs.y - as.y*bs.x);
	vlist[nv].rrot.m11 = (ct.x * (as.y*bs.z - as.z*bs.y) +
		                  bt.x * (as.z*cs.y - as.y*cs.z) +
				          at.x * (bs.y*cs.z - bs.z*cs.y)) / den;
	vlist[nv].rrot.m12 = (ct.x * (as.z*bs.x - as.x*bs.z) +
					      bt.x * (as.x*cs.z - as.z*cs.x) +
		                  at.x * (bs.z*cs.x - bs.x*cs.z)) / den;
	vlist[nv].rrot.m13 = (ct.x * (as.x*bs.y - as.y*bs.x) +
		                  bt.x * (as.y*cs.x - as.x*cs.y) +
					      at.x * (bs.x*cs.y - bs.y*cs.x)) / den;
	vlist[nv].rrot.m21 = (ct.y * (as.y*bs.z - as.z*bs.y) +
		                  bt.y * (as.z*cs.y - as.y*cs.z) +
					      at.y * (bs.y*cs.z - bs.z*cs.y)) / den;
	vlist[nv].rrot.m22 = (ct.y * (as.z*bs.x - as.x*bs.z) +
		                  bt.y * (as.x*cs.z - as.z*cs.x) +
					      at.y * (bs.z*cs.x - bs.x*cs.z)) / den;
	vlist[nv].rrot.m23 = (ct.y * (as.x*bs.y - as.y*bs.x) +
		                  bt.y * (as.y*cs.x - as.x*cs.y) +
					      at.y * (bs.x*cs.y - bs.y*cs.x)) / den;
	vlist[nv].rrot.m31 = (ct.z * (as.y*bs.z - as.z*bs.y) +
		                  bt.z * (as.z*cs.y - as.y*cs.z) +
					      at.z * (bs.y*cs.z - bs.z*cs.y)) / den;
	vlist[nv].rrot.m32 = (ct.z * (as.z*bs.x - as.x*bs.z) +
		                  bt.z * (as.x*cs.z - as.z*cs.x) +
					      at.z * (bs.z*cs.x - bs.x*cs.z)) / den;
	vlist[nv].rrot.m33 = (ct.z * (as.x*bs.y - as.y*bs.x) +
		                  bt.z * (as.y*cs.x - as.x*cs.y) +
					      at.z * (bs.x*cs.y - bs.y*cs.x)) / den;
	// 2. premultipy with vessel1's rotation matrix
	vlist[nv].rrot.premul (vlist[idx1].rrot);
	vlist[nv].rq.Set (vlist[nv].rrot);

	// position of vessel2 in superstructure
	vlist[nv].rpos.Set (vlist[idx1].rpos + mul (vlist[idx1].rrot, vessel1->dock[port1]->ref) - mul (vlist[nv].rrot, vessel2->dock[port2]->ref));

	vessel2->SetSuperStruct (this);
	// flush the state increments
	vessel2->rpos_base = vessel2->s0->pos; vessel2->rpos_add.Set(0,0,0);
	vessel2->rvel_base = vessel2->s0->vel; vessel2->rvel_add.Set(0,0,0);
	vessel2->s0->Q.Set (s0->Q);
	vessel2->s0->Q.postmul (vlist[nv].rq);
	vessel2->s0->R.Set (vessel2->s0->Q);

	nv++;
	ResetMassAndCG();
	ResetSize();
	CalcPMI();
	bOrbitStabilised = false;

	// calculate angular velocity from conservation of angular momentum
	if (mixmoments) {
		Vector am;
		for (DWORD i = 0; i < nv; i++) {
			// individual spin of each vessel
			am += mul (vlist[i].rrot, vlist[i].vessel->AngularMomentum()) * vlist[i].vessel->mass;
			// contribution of vessel motion to angular momentum
			am += crossp (tmul (s0->R, vlist[i].vessel->GVel()-s0->vel), vlist[i].rpos-cg) * vlist[i].vessel->mass;
		}
		s0->omega.Set (am.x/pmi.x, am.y/pmi.y, am.z/pmi.z);
		s0->omega /= mass;
	}

	vessel2->s0->omega.Set (tmul (vlist[nv-1].rrot, s0->omega));
	vessel2->UpdateSurfParams();

	return true;
}

// =======================================================================

bool SuperVessel::Merge (Vessel *vessel1, int port1, Vessel *vessel2, int port2)
{
	DWORD i, idx1, idx2, nv2;

	// make sure vessel1 is part of the superstructure
	for (idx1 = 0; idx1 < nv; idx1++)
		if (vessel1 == vlist[idx1].vessel) break;
	if (idx1 == nv) return false; // not found

	SuperVessel *sv2 = vessel2->supervessel;
	if (!sv2 || sv2 == this) return false;
	nv2 = sv2->nv;

	for (idx2 = 0; idx2 < nv2; idx2++)
		if (vessel2 == sv2->vlist[idx2].vessel) break;
	if (idx2 == nv2) return false;

	SubVesselData *tmp = new SubVesselData[nv+nv2]; TRACENEW
	memcpy (tmp, vlist, nv*sizeof(SubVesselData));
	delete []vlist;
	vlist = tmp;

	// define the rotation matrix from vessel2 to vessel1
	Matrix R;
	Vector as(vessel2->dock[port2]->dir);
	Vector bs(vessel2->dock[port2]->rot);
	Vector cs(crossp(as,bs));
	Vector at(-vessel1->dock[port1]->dir);
	Vector bt(vessel1->dock[port1]->rot);
	Vector ct(crossp(at,bt));
	double den = cs.x * (as.y*bs.z - as.z*bs.y) +
		     cs.y * (as.z*bs.x - as.x*bs.z) +
		     cs.z * (as.x*bs.y - as.y*bs.x);
	R.m11 = (ct.x * (as.y*bs.z - as.z*bs.y) +
		     bt.x * (as.z*cs.y - as.y*cs.z) +
	         at.x * (bs.y*cs.z - bs.z*cs.y)) / den;
	R.m12 = (ct.x * (as.z*bs.x - as.x*bs.z) +
			 bt.x * (as.x*cs.z - as.z*cs.x) +
		     at.x * (bs.z*cs.x - bs.x*cs.z)) / den;
	R.m13 = (ct.x * (as.x*bs.y - as.y*bs.x) +
		     bt.x * (as.y*cs.x - as.x*cs.y) +
			 at.x * (bs.x*cs.y - bs.y*cs.x)) / den;
	R.m21 = (ct.y * (as.y*bs.z - as.z*bs.y) +
		     bt.y * (as.z*cs.y - as.y*cs.z) +
			 at.y * (bs.y*cs.z - bs.z*cs.y)) / den;
	R.m22 = (ct.y * (as.z*bs.x - as.x*bs.z) +
		     bt.y * (as.x*cs.z - as.z*cs.x) +
			 at.y * (bs.z*cs.x - bs.x*cs.z)) / den;
	R.m23 = (ct.y * (as.x*bs.y - as.y*bs.x) +
		     bt.y * (as.y*cs.x - as.x*cs.y) +
			 at.y * (bs.x*cs.y - bs.y*cs.x)) / den;
	R.m31 = (ct.z * (as.y*bs.z - as.z*bs.y) +
		     bt.z * (as.z*cs.y - as.y*cs.z) +
             at.z * (bs.y*cs.z - bs.z*cs.y)) / den;
	R.m32 = (ct.z * (as.z*bs.x - as.x*bs.z) +
		     bt.z * (as.x*cs.z - as.z*cs.x) +
			 at.z * (bs.z*cs.x - bs.x*cs.z)) / den;
	R.m33 = (ct.z * (as.x*bs.y - as.y*bs.x) +
		     bt.z * (as.y*cs.x - as.x*cs.y) +
			 at.z * (bs.x*cs.y - bs.y*cs.x)) / den;

	R.premul (vlist[idx1].rrot);             // vessel2->supervessel1
	R.tpostmul (sv2->vlist[idx2].rrot);      // supervessel2->supervessel1

	for (i = 0; i < nv2; i++) {
		vlist[nv+i].vessel = sv2->vlist[i].vessel;
		vlist[nv+i].rrot.Set (sv2->vlist[i].rrot);
		vlist[nv+i].rrot.premul (R);
		vlist[nv+i].rq.Set (vlist[nv+i].rrot);
		vlist[nv+i].rpos.Set (vlist[idx1].rpos + mul (vlist[idx1].rrot, vessel1->dock[port1]->ref) +
			mul (R, sv2->vlist[i].rpos - sv2->vlist[idx2].rpos - mul (sv2->vlist[idx2].rrot, vessel2->dock[port2]->ref)));
		vlist[nv+i].vessel->SetSuperStruct (this);
		vlist[nv+i].vessel->FlushRPos();
		vlist[nv+i].vessel->FlushRVel();
		vlist[nv+i].vessel->s0->Q.Set (s0->Q);
		vlist[nv+i].vessel->s0->Q.postmul (vlist[nv+i].rq);
		vlist[nv+i].vessel->s0->R.Set (vlist[nv+i].vessel->s0->Q);
		vlist[nv + i].vessel->UpdateSurfParams();
	}
	nv += nv2;

	ResetMassAndCG();
	ResetSize();
	CalcPMI();
	return true;
}

// =======================================================================

bool SuperVessel::AddSurfaceForces (Vector *F, Vector *M, const StateVectors *s, double tfrac, double dt) const
{
	bool impact = false;
	DWORD comp;
	StateVectors scomp;
	for (comp = 0; comp < nv; comp++) {
		Vector Fcomp, Mcomp;
		ComponentStateVectors (s, &scomp, comp);
		if (vlist[comp].vessel->AddSurfaceForces (&Fcomp, &Mcomp, &scomp, tfrac, dt)) {
			AddComponentForceAndMoment (F, M, &Fcomp, &Mcomp, comp);
			impact = true;
		}
	}
	return impact;
}

// =======================================================================

void SuperVessel::Update (bool force)
{
	DWORD i;
	bool grot_changed = false;
	bool el_updated = false;;

	// centre of gravity and total mass
	ResetMassAndCG();

	if (vlist[0].vessel->bFRplayback) {

		vlist[0].vessel->FRecorder_Play();     // update lead vessel from playback stream
		SetStateFromComponent (vlist[0].vessel->s1, 0); // update supervessel from lead vessel
		arot.Set (mul (vlist[0].rrot, vlist[0].vessel->arot)); // is the vessel arot valid here?
		for (i = 1; i < nv; i++) {						// update all others from supervessel
			ComponentStateVectors (s1, vlist[i].vessel->s1, i);
			vlist[i].vessel->arot.Set (tmul (vlist[i].rrot, arot));
		}
		for (i = 0; i < nv; i++)
			vlist[i].vessel->el_valid = false;

	} else if (fstatus == FLIGHTSTATUS_FREEFLIGHT) {

		// Collect vessel thrust and atmospheric forces
		Flin.Set (0,0,0);
		Amom.Set (0,0,0);
		for (i = 0; i < nv; i++) {
			Vessel *v = vlist[i].vessel;
			Vector vAmom (mul (vlist[i].rrot, v->Amom_add));
			Vector vFlin (mul (vlist[i].rrot, v->Flin_add));
			Amom += vAmom + crossp (vFlin, vlist[i].rpos-cg);
			Flin += vFlin;
		}

		RigidBody::Update (force);

		// update state parameters for all sub-vessels
		for (i = 0; i < nv; i++) {
			ComponentStateVectors (s1, vlist[i].vessel->s1, i);
			vlist[i].vessel->arot.Set (tmul (vlist[i].rrot, arot));
			vlist[i].vessel->el_valid = false;
		}

	} else if (fstatus == FLIGHTSTATUS_LANDED) {

		proxyplanet->LocalToGlobal_t1 (sp.ploc, s1->pos);
		double vground = Pi2 * proxyplanet->Size() * sp.clat / proxyplanet->RotT();
		s1->vel.Set (-vground*sp.slng, 0.0, vground*sp.clng);
		s1->vel.Set (mul (proxyplanet->s1->R, s1->vel) + proxyplanet->s1->vel);
		s1->R.Set (land_rot);
		s1->R.premul (proxyplanet->s1->R);
		s1->Q.Set (s1->R);
		bActivationPending = false;
		for (i = 0; i < nv; i++) {
			ComponentStateVectors (s1, vlist[i].vessel->s1, i);
			bActivationPending = bActivationPending || vlist[i].vessel->bForceActive;
		}
	}

	// check periodically for proxy-bodies
	if (td.SimT1 > proxyT) {
		UpdateProxies();
		proxyT = td.SimT1 + 100.0; // update every 100 seconds
	}

	if (proxybody && fstatus != FLIGHTSTATUS_LANDED)
		UpdateSurfParams();

	// state vectors w.r.t. reference body
	cpos = s1->pos - cbody->s1->pos;
	cvel = s1->vel - cbody->s1->vel;
}

// =======================================================================

void SuperVessel::PostUpdate ()
{
	VesselBase::PostUpdate ();

	if (bActivationPending) {
		Activate ();
		bActivationPending = false;
	}
}

// =======================================================================

void SuperVessel::UpdateProxies ()
{
	VesselBase::UpdateProxies ();

	// MOVE THIS UP TO VesselBase::PostUpdate
	double gfrac;
	SetOrbitReference (g_psys->GetDominantGravitySource (s0->pos, gfrac));
}

// =======================================================================

void SuperVessel::SetOrbitReference (CelestialBody *body)
{
	if (body && body != cbody) {               // otherwise nothing to do
		cbody = body;
		el->Setup (mass, cbody->Mass(), el->MJDepoch());
		bOrbitStabilised = false;      // enforce recalculation of elements
		for (DWORD i = 0; i < nv; i++) // propagate to individual vessels
			vlist[i].vessel->SetOrbitReference (body);
	}
}

// =======================================================================

bool SuperVessel::CheckSurfaceContact () const
{
	for (DWORD comp = 0; comp < nv; comp++)
		if (vlist[comp].vessel->CheckSurfaceContact ())
			return true;
	return false;
}

// =======================================================================

void SuperVessel::InitLanded (Planet *planet, double lng, double lat, double dir, const Matrix *hrot, double cgelev, bool asComponent)
{
	dCHECK(!g_bStateUpdate, "SuperVessel::InitLanded must not be called during state update") // not valid during update phase
	dCHECK(hrot, "hrot parameter for SuperVessel::InitLanded must be supplied")               // only support dynamic impact model for now

	DWORD comp;
	proxybody = proxyplanet = planet;
	double slng = sin(lng), clng = cos(lng);
	double slat = sin(lat), clat = cos(lat);
	Matrix L2H (-slng,0,clng, clat*clng,slat,clat*slng, -slat*clng,clat,-slat*slng);
	Vector nml (tmul (*hrot, tmul (L2H, Vector(0,1,0))));
	sp.SetLanded (lng, lat, cgelev, dir, nml, planet);

	double vground = Pi2 * /*sp.rad*/planet->Size() * sp.clat / planet->RotT();
	s0->vel.Set (-vground*sp.slng, 0.0, vground*sp.clng);
	s0->pos.Set (mul (planet->GRot(), sp.ploc) + planet->GPos());
	s0->vel.Set (mul (planet->GRot(), s0->vel) + planet->GVel());
	land_rot.Set (*hrot);
	s0->Q.Set (land_rot);
	s0->Q.premul (planet->GQ());
	s0->R.Set (s0->Q);

	fstatus = FLIGHTSTATUS_LANDED;
	bSurfaceContact = true;
	for (comp = 0; comp < nv; comp++) {
		StateVectors scomp;
		double lng, lat, rad, dir, elev;
		ComponentStateVectors (s0, &scomp, comp);
		planet->GlobalToEquatorial (scomp.pos, lng, lat, rad);
		elev = planet->Elevation (lng, lat);
		dir = LandedHeading (lng, lat);
		Matrix hrotcomp (*hrot);
		hrotcomp.postmul (vlist[comp].rrot);
		vlist[comp].vessel->InitLanded (planet, lng, lat, dir, &hrotcomp, rad-elev-planet->Size(), true);
	}
}

// =======================================================================

void SuperVessel::Refuel ()
{
	for (DWORD comp = 0; comp < nv; comp++)
		vlist[comp].vessel->Refuel();	
}

// =======================================================================

bool SuperVessel::ThrustEngaged () const
{
	for (DWORD comp = 0; comp < nv; comp++)
		if (vlist[comp].vessel->ThrustEngaged ())
			return true;
	return false;
}

// =======================================================================

void SuperVessel::ComponentStateVectors (const StateVectors *s, StateVectors *scomp, int comp) const
{
	scomp->vel.Set (s->vel + mul (s->R, crossp (vlist[comp].rpos-cg, s->omega)));
	scomp->pos.Set (s->pos + mul (s->R, vlist[comp].rpos-cg));
	scomp->omega.Set (tmul (vlist[comp].rrot, s->omega));
	scomp->Q.Set (s->Q * vlist[comp].rq);
	scomp->R.Set (scomp->Q);
}

// =======================================================================

void SuperVessel::SetStateFromComponent (const StateVectors *scomp, int comp) const
{
	s0->R.Set (scomp->R);
	s0->R.postmul (vlist[comp].rrot);
	s0->Q.Set (s0->R);
	s0->omega.Set (mul (vlist[comp].rrot, scomp->omega));
	s0->vel.Set (scomp->vel - mul (s0->R, crossp (vlist[comp].rpos-cg, s0->omega)));
	s0->pos.Set (scomp->pos - mul (s0->R, vlist[comp].rpos-cg));
}

// =======================================================================

void SuperVessel::AddComponentForceAndMoment (Vector *F, Vector *M,
	const Vector *Fcomp, const Vector *Mcomp, int comp) const
{
	Vector Ftrans (mul (vlist[comp].rrot, *Fcomp));
	*F += Ftrans;
	*M += mul (vlist[comp].rrot, *Mcomp) + crossp (Ftrans, vlist[comp].rpos-cg);
}

// =======================================================================

void SuperVessel::NotifyShiftVesselOrigin (Vessel *vessel, const Vector &dr)
{
	for (DWORD i = 0; i < nv; i++) {
		if (vlist[i].vessel == vessel) {
			vlist[i].rpos += mul (vlist[i].rrot, dr);
			return;
		}
	}
}

// =======================================================================

bool SuperVessel::GetCG (const Vessel *vessel, Vector &vcg)
{
	for (DWORD i = 0; i < nv; i++) {
		if (vlist[i].vessel == vessel) {
			vcg.Set (tmul (vlist[i].rrot, cg-vlist[i].rpos));
			return true;
		}
	}
	return false;
}

// =======================================================================

bool SuperVessel::GetPMI (const Vessel *vessel, Vector &vpmi)
{
	for (DWORD i = 0; i < nv; i++) {
		if (vlist[i].vessel == vessel) {
			vpmi.Set (0,0,0);
			Vector r0[6], rt;
			double rtx2, rty2, rtz2;
			r0[1].x = -(r0[0].x = 0.5 * sqrt (fabs (-pmi.x + pmi.y + pmi.z)));
			r0[3].y = -(r0[2].y = 0.5 * sqrt (fabs ( pmi.x - pmi.y + pmi.z)));
			r0[5].z = -(r0[4].z = 0.5 * sqrt (fabs ( pmi.x + pmi.y - pmi.z)));
			for (DWORD j = 0; j < 6; j++) {
				rt.Set (tmul (vlist[i].rrot, r0[j] + cg - vlist[i].rpos));
				rtx2 = rt.x*rt.x, rty2 = rt.y*rt.y, rtz2 = rt.z*rt.z;
				vpmi.x += rty2 + rtz2;
				vpmi.y += rtx2 + rtz2;
				vpmi.z += rtx2 + rty2;
			}
			return true;
		}
	}
	return false;
}

// =======================================================================

void SuperVessel::SetFlightStatus (FlightStatus fstatus)
{
	for (DWORD i = 0; i < nv; i++)
		vlist[i].vessel->fstatus = fstatus;
}

// =======================================================================

void SuperVessel::ResetSize ()
{
	size = 0.0;
	for (DWORD i = 0; i < nv; i++) {
		Vector p (vlist[i].rpos - cg);
		double r = p.length() + vlist[i].vessel->Size();
		if (r > size) size = r;
	}
}

// =======================================================================

void SuperVessel::ResetMassAndCG ()
{
	// centre of gravity and total mass
	Vector cg_new;
	mass = 0.0;
	for (DWORD i = 0; i < nv; i++) {
		mass += vlist[i].vessel->mass;
		cg_new += vlist[i].rpos * vlist[i].vessel->mass;
	}
	cg_new /= mass;

	// shift CG
	Vector dp = mul (s0->R, cg_new-cg);
	s0->pos += dp;
	rpos_add += dp;
	cpos += dp;
	cg = cg_new;
}

// =======================================================================

void SuperVessel::CalcPMI ()
{
	// Calculates the PMI values for the supervessel from the layout and component PMI
	// values. For details see "Inertia calculations for composite vessels" in "Orbiter
	// Technical Reference".

	DWORD i, j;
	Vector r0[6], rt;
	double rtx2, rty2, rtz2, vmass;
	double vpmix, vpmiy, vpmiz;

	pmi.Set (0,0,0);

	for (i = 0; i < nv; i++) {
		Vessel *v = vlist[i].vessel;
		Vector &vpmi = v->pmi;
		vmass = v->mass/6.0;
		r0[1].x = -(r0[0].x = sqrt (1.5 * fabs (-vpmi.x + vpmi.y + vpmi.z)));
		r0[3].y = -(r0[2].y = sqrt (1.5 * fabs ( vpmi.x - vpmi.y + vpmi.z)));
		r0[5].z = -(r0[4].z = sqrt (1.5 * fabs ( vpmi.x + vpmi.y - vpmi.z)));

		vpmix = vpmiy = vpmiz = 0.0;
		for (j = 0; j < 6; j++) {
			rt.Set (mul (vlist[i].rrot, r0[j]) + vlist[i].rpos - cg);
			rtx2 = rt.x*rt.x, rty2 = rt.y*rt.y, rtz2 = rt.z*rt.z;
			vpmix += rty2 + rtz2;
			vpmiy += rtx2 + rtz2;
			vpmiz += rtx2 + rty2;
		}
		pmi.x += vmass * vpmix;
		pmi.y += vmass * vpmiy;
		pmi.z += vmass * vpmiz;
	}
	// normalise with total supervessel mass
	pmi.x /= mass;
	pmi.y /= mass;
	pmi.z /= mass;

	// we also need to update the damping term for the gravity gradient
	// torque. This is a weighted average of the vessel component values.
	tidaldamp = 0;
	for (i = 0; i < nv; i++) {
		Vessel *v = vlist[i].vessel;
		tidaldamp += v->tidaldamp * v->mass;
	}
	tidaldamp /= mass;
}

// =======================================================================

TOUCHDOWN_VTX *SuperVessel::HullvtxFirst ()
{
	next_hullvessel = 0;
	TOUCHDOWN_VTX *tdv = vlist[next_hullvessel].vessel->HullvtxFirst();
	if (tdv) {
		hullvtx = *tdv;
		hullvtx.pos.Set (mul(vlist[next_hullvessel].rrot, tdv->pos)+vlist[next_hullvessel].rpos-cg);
		return &hullvtx;
	} else
		return NULL;
}

// =======================================================================

TOUCHDOWN_VTX *SuperVessel::HullvtxNext ()
{
	if (next_hullvessel < nv) {
		TOUCHDOWN_VTX *tdv = vlist[next_hullvessel].vessel->HullvtxNext();
		if (!tdv && ++next_hullvessel < nv)
			tdv = vlist[next_hullvessel].vessel->HullvtxFirst();
		if (tdv) {
			hullvtx = *tdv;
			hullvtx.pos.Set (mul(vlist[next_hullvessel].rrot, tdv->pos)+vlist[next_hullvessel].rpos-cg);
			return &hullvtx;
		}
	}
	return NULL;
}

#ifdef UNDEF
// =======================================================================
// calculate angular moment induced by mass distributions of the separate
// vessels

void SuperVessel::AddGravityGradientTorque (Vector &Torque)
{
	if (!cbody) return;

	const double tidaldamp = 10; // justify this!
	// map cbody into vessel frame
	Vector R0 (tmul (rrot, cbody->GPos() - rpos));
	double r0 = R0.length();
	Vector Re = R0/r0;
	double mag = 3.0 * Ggrav * cbody->Mass() / pow(r0,3.0);
	Vector M = crossp (pmi*Re, Re) * mag;

	// damping of angular velocity
	double damp  = tidaldamp * mag;
	double scale = mag;
	if (vrot.x) M.x -= min (damp, iSimDT) * pmi.x*vrot.x;
	if (vrot.y) M.y -= min (damp, iSimDT) * pmi.y*vrot.y;
	if (vrot.z) M.z -= min (damp, iSimDT) * pmi.z*vrot.z;

	Torque += M*mass;

}
#endif

// ===========================================================================
// Flight recorder functions
// ===========================================================================

void SuperVessel::FRecorder_EndPlayback ()
{
	SetRotationMatrix (vlist[0].vessel->GRot(), vlist[0].vessel);
}
