// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Orbiter.h"
#include "Vesselbase.h"
#include "Psys.h"

extern PlanetarySystem *g_psys;
extern TimeData td;
extern char DBG_MSG[256];


// =======================================================================

void SurfParam::Set (const StateVectors &s, const StateVectors &s_ref, const CelestialBody *_ref,
					 std::vector<ElevationTile> *etilecache, WindPrm *windprm)
{
	// Calculate surface parameters for arbitrary state of object and reference planet

	static const double eps = 1e-4;
	static const double alt_max = 1e5; // for now

	ref = _ref;
	Planet *planet = (ref->Type() == OBJTP_PLANET ? (Planet*)ref : 0);

	Vector Prel = s.pos - s_ref.pos;
	ploc = tmul (s_ref.R, Prel);
	ref->LocalToEquatorial (ploc, lng, lat, rad);
	slng = sin(lng), clng = cos(lng);
	slat = sin(lat), clat = cos(lat);
	alt0 = alt = rad - ref->Size();
	elev = 0.0;
	surfnml.Set(0,1,0);
	if (etilecache && alt < alt_max) {
		if (ref->Type() == OBJTP_PLANET) {
			ElevationManager *emgr = ((Planet*)ref)->ElevMgr();
			if (emgr) {
				int reslvl = (int)(32.0-log(max(alt0,100))*LOG2);
				elev = emgr->Elevation (lat, lng, reslvl, etilecache, &surfnml, &elev_lvl);
				alt -= elev;
			}
		}
	}

	Vector Snm (tmul (s.R, Prel));
	Snm.unify(); // planet surface normal below ship in ship's local coords
	pitch = asin (Snm.z);
	if (fabs (Snm.x) > eps || fabs (Snm.y) > eps)
		bank = atan2 (Snm.x, Snm.y);
	else
		bank = 0.0;

	// ground speed
	Vector vrel = s.vel - s_ref.vel;
	double vref = Pi2/ref->RotT() * rad * clat; // speed of a point at vessel position fixed in planet frame
	groundvel_glob.Set (vrel - mul (s_ref.R, Vector(-vref*slng,0.0,vref*clng)));  // ground velocity in global frame
	groundvel_ship.Set (tmul (s.R, groundvel_glob));  // ground velocity in ship frame
	groundspd = groundvel_glob.length();

	// vertical velocity
	vspd = dotp(vrel, Prel.unit());

	// airspeed
	Vector windvel_glob(0,0,0);
	if (planet)
		windvel_glob.Set (mul (s_ref.R, planet->WindVelocity (lng, lat, alt, 1, windprm)));

	airvel_glob.Set (groundvel_glob - windvel_glob);
	airvel_ship.Set (tmul (s.R, airvel_glob));
	airspd = airvel_glob.length();

	// rotation from planet local coords to local horizon
	L2H.Set (-slng, 0, clng,
		     clat*clng, slat, clat*slng,
			 -slat*clng, clat, -slat*slng);

	// map ship's forward axis into local horizon frame
	Vector RZloc = mul(L2H, tmul(s_ref.R, Vector (s.R.m13, s.R.m23, s.R.m33)));
	if (fabs (RZloc.x) > eps || fabs(RZloc.z) > eps)
		dir = atan2 (RZloc.x, RZloc.z);
	else {  // ship is pointing vertically up - take ship's negative y-axis for direction
		Vector RYloc = mul(L2H, tmul(s_ref.R, Vector (-s.R.m12, -s.R.m22, -s.R.m32)));
		dir = atan2 (RYloc.x, RYloc.z);
	}
	if (dir < 0.0) dir += Pi2;

	// atmospheric parameters
	if (is_in_atm = (planet && planet->HasAtmosphere() && rad < planet->AtmRadLimit())) {
		ATMPARAM prm;
		planet->GetAtmParam (alt0, lng, lat, &prm);
		atmT   = prm.T;
		atmp   = prm.p;
		atmrho = prm.rho;
		dynp   = 0.5*atmrho*airspd*airspd;           // dynamic pressure
		atmM   = airspd / planet->AtmSoundSpeed (atmT); // Mach number
	} else {
		atmT = atmp = atmrho = atmM = 0.0;
		dynp = 0.0;
	}
}

// -----------------------------------------------------------------------

double SurfParam::ComputeAltitude(const StateVectors &s, const StateVectors &s_ref, const CelestialBody *_ref,
	std::vector<ElevationTile> *etilecache)
{
	static const double alt_max = 1e5; // for now

	double _lng, _lat, _rad;
	Vector ploc = tmul (s_ref.R, s.pos - s_ref.pos);
	_ref->LocalToEquatorial (ploc, _lng, _lat, _rad);
	double _alt = _rad - _ref->Size();
	if (etilecache && _alt < alt_max && _ref->Type() == OBJTP_PLANET) {
		ElevationManager *emgr = ((Planet*)_ref)->ElevMgr();
		if (emgr) {
			int reslvl = (int)(32.0-log(max(_alt,100))*LOG2);
			double elev = emgr->Elevation (_lat, _lng, reslvl, etilecache);
			_alt -= elev;
		}
	}
	return _alt;
}

// -----------------------------------------------------------------------

void SurfParam::SetLanded (double _lng, double _lat, double _alt, double _dir, const Vector &nml, const CelestialBody *_ref)
{
	static const double eps = 1e-6;

	ref = _ref;
	Planet *planet = (ref->Type() == OBJTP_PLANET ? (Planet*)ref : 0);

	lng = _lng;
	lat = _lat;
	alt = _alt;
	rad = ref->Size() + alt + (planet ? planet->Elevation (lng, lat) : 0.0);
	slng = sin(lng), clng = cos(lng), slat = sin(lat), clat = cos(lat);
	ref->EquatorialToLocal (slng, clng, slat, clat, rad, ploc);

	L2H.Set (-slng, 0, clng,
		     clat*clng, slat, clat*slng,
			 -slat*clng, clat, -slat*slng);

	pitch = atan2 (nml.z, nml.y);
	if (fabs (nml.x) > eps || fabs (nml.y) > eps)
		bank = atan2 (nml.x, nml.y);
	else
		bank = 0.0;
	dir = _dir;
	airvel_glob.Set (0,0,0);
	airvel_ship.Set (0,0,0);
	groundvel_glob.Set (0,0,0);
	groundvel_ship.Set (0,0,0);
	airspd = groundspd = 0.0;

	if (is_in_atm = (planet && planet->HasAtmosphere() && rad < planet->AtmRadLimit())) {
		ATMPARAM prm;
		planet->GetAtmParam (alt, lng, lat, &prm);
		atmT   = prm.T;
		atmp   = prm.p;
		atmrho = prm.rho;
		atmM   = airspd / planet->AtmSoundSpeed (atmT); // Mach number
	} else {
		atmT = atmp = atmrho = atmM = 0.0;
	}
	dynp = 0.0;
}


// =======================================================================
// class VesselBase
// =======================================================================

VesselBase::VesselBase (): RigidBody ()
{
	etile.resize(2);
}

// =======================================================================

VesselBase::VesselBase (double _mass, double _size, const Vector &_pmi)
: RigidBody (_mass, _size, _pmi)
{
	etile.resize(2);
}

// =======================================================================

void VesselBase::SetDefaultState ()
{
	RigidBody::SetDefaultState ();

	fstatus   = FLIGHTSTATUS_FREEFLIGHT;
	proxybody = 0;
	proxyplanet = 0;
	proxybase = 0;
	bDynamicGroundContact = true;
	bSurfaceContact = false;
	LandingTest.testing = false;
	proxyT    = -(double)rand()*100.0/(double)RAND_MAX - 1.0;
	// distribute update times

	windp.pert_t = 0;
	windp.pert_v.Set(0,0,0);
}

// =======================================================================

bool VesselBase::Activate (bool force)
{
	if (fstatus == FLIGHTSTATUS_LANDED || force) {
		fstatus = FLIGHTSTATUS_FREEFLIGHT;
		rpos_base = s0->pos;
		rpos_add.Set (0,0,0);
		rvel_base = s0->vel;
		rvel_add.Set (0,0,0);
		return true;
	} else
		return false;
}

// =======================================================================

void VesselBase::PostUpdate ()
{
	// Check periodically for proxy-bodies
	if (fstatus == FLIGHTSTATUS_FREEFLIGHT && td.SimT1 > proxyT) {
		UpdateProxies ();
		proxyT = td.SimT1 + 100.0;  // update every 100 seconds
	}

	// Check for surface contact
	bSurfaceContact = (fstatus == FLIGHTSTATUS_LANDED || CheckSurfaceContact());

	// Check for switching to LANDED status
	if (fstatus != FLIGHTSTATUS_LANDED && !IsComponent()) // only top-level vessels perform this check
		CheckLanded ();
}

// =======================================================================

void VesselBase::UpdateSurfParams ()
{
	if (proxybody) sp.Set (s1 ? *s1 : *s0, proxybody->s1 ? *proxybody->s1 : *proxybody->s0, proxybody, &etile, &windp);
}

// =======================================================================

void VesselBase::CheckLanded ()
{
	if (LandingTest.testing) {
		if (!bSurfaceContact || ThrustEngaged())
			LandingTest.testing = false;
		else {
			static const double eps = 1e-4;
			Vector prel (tmul(proxybody->s0->R, s0->pos - proxybody->s0->pos)); // vessel position in planet local coords
			double dt = td.SimT0 - LandingTest.testt;
			if (dt < 0.1) return;
			double dst2 = prel.dist2 (LandingTest.surfpos);
			double dangle = angle (s0->Q, LandingTest.surfrot);
			LandingTest.surfpos = prel;
			LandingTest.surfrot.Set (s0->Q);
			if (dst2/dt > 1e-4 || dangle/dt > 1e-4) {
				LandingTest.testt = td.SimT0; // still moving -> reset timer
			} else if (td.SimT0 - LandingTest.testt > 5.0) {
				// heading of landed vessel
				double lng, lat, rad, dir;
				proxybody->LocalToEquatorial (prel, lng, lat, rad);
				dir = LandedHeading (lng, lat);

				if (bDynamicGroundContact) {
					double alt = Altitude(); //rad - proxybody->Size();
					Matrix lrot (s0->R);
					lrot.tpremul (proxyplanet->s0->R);
					InitLanded (proxyplanet, lng, lat, dir, &lrot, alt);
				} else {
					InitLanded (proxyplanet, lng, lat, dir);
				}
				if (proxybase) {
					int pd = proxybase->ReportTouchdown (this, lng, lat);
				}
			}
		}
	} else {
		if (bSurfaceContact && !ThrustEngaged()) {
			LandingTest.testing = true;
			LandingTest.testt = td.SimT0;
			LandingTest.surfpos.Set (tmul(proxybody->s0->R, s0->pos - proxybody->s0->pos));
			LandingTest.surfrot.Set (s0->Q); // should really subtract proxybody rotation here
		}
	}
}

// =======================================================================

double VesselBase::LandedHeading (double lng, double lat) const
{
	static const double eps = 1e-4;
	double dir;
	double slng = sin(lng), clng = cos(lng);
	double slat = sin(lat), clat = cos(lat);
	Matrix L2H (-slng,0,clng, clat*clng,slat,clat*slng, -slat*clng,clat,-slat*slng);
	Vector RZloc = mul(L2H, tmul (proxybody->s0->R, Vector (s0->R.m13, s0->R.m23, s0->R.m33)));
	if (fabs(RZloc.x) > eps || fabs(RZloc.z) > eps)
		dir = atan2(RZloc.x, RZloc.z);
	else {
		Vector RYloc = mul(L2H, tmul(proxybody->s0->R, Vector(-s0->R.m12, -s0->R.m22, -s0->R.m32)));
		dir = atan2(RYloc.x, RYloc.z);
	}
	if (dir < 0.0) dir += Pi2;
	return dir;
}

// =======================================================================

void VesselBase::GetIntermediateMoments (Vector &acc, Vector &tau,
	const StateVectors &state, double tfrac, double dt)
{
}

// ==============================================================

void VesselBase::UpdateProxies ()
{
	DWORD i, ng = g_psys->nGrav();
	double dist2, proxydist2, proxypdist2;
	CelestialBody *grav;
	Planet *pl = 0;

	// Check for closest celestial body and planet
	for (i = 0, proxydist2 = proxypdist2 = 1e100; i < ng; i++) {
		grav = g_psys->GetGravObj(i);
		dist2 = s0->pos.dist2 (grav->s0->pos) - grav->Size(); // distance squared from body surface
		if (dist2 < proxydist2) {
			proxydist2 = dist2;
			proxybody = grav;
		}
		if (grav->Type() == OBJTP_PLANET && dist2 < proxypdist2) {
			proxypdist2 = dist2;
			pl = (Planet*)grav;
		}
	}
	if (pl != proxyplanet) SetProxyplanet (pl);

	// check for closest spaceport and update NAV reception status
	proxybase = 0;
	if (proxyplanet) {
		for (i = 0, proxydist2 = 1e100; i < proxyplanet->nBase(); i++) {
			Base *base = (Base*)proxyplanet->GetBase(i);
			if ((dist2 = s0->pos.dist2 (base->s0->pos)) < proxydist2) {
				proxydist2 = dist2;
				proxybase = base;
			}
		}
	}

	// check for orbit reference body
	double gfrac;
	SetOrbitReference (g_psys->GetDominantGravitySource (s0->pos, gfrac));
}

// ==============================================================

bool VesselBase::SurfaceProximity () const
{
	return sp.alt < 3.0*size;
#ifdef UNDEF
	if (!proxybody) return false;
	//if (bTouchdown) return true;  // TODO
	double rad = proxybody->Size();
	double dist = s0->pos.dist (proxybody->s0->pos);
	double alt = dist-rad-Elevation();
	if (alt < 3.0*size) return true;
	return false;
	// now extrapolate to end of time step
	Vector sp1 = s0->pos + (s0->vel + acc * 0.5*td.SimDT)*td.SimDT;
	dist = sp1.dist (proxybody->s1->pos);
	alt = dist-rad;
	return (alt < 3.0*size);
#endif
}

// ==============================================================

void VesselBase::SetPropagator (int &plevel, int &nstep) const
{
	RigidBody::SetPropagator (plevel, nstep);
	if (bSurfaceContact) {
		plevel = NumPropLevel()-1;
		nstep = MaxSubStep(); // should be set according to (vertical?) speed
		update_with_collision = true;
	} else {
		update_with_collision = false;
	}
	collision_during_update = false; // reset collision check
}

// =======================================================================

bool VesselBase::ValidateStateUpdate (StateVectors *s)
{
	if (collision_during_update) {
		if (!update_with_collision) {
			update_with_collision = true;
			collision_speed_checked = false;
			bSurfaceContact = true;
			return false;
		} else if (!collision_speed_checked) {
			collision_speed_checked = true;
			double vsmax = -0.1 * MaxSubStep()/td.SimDT; // allow max 0.1m / substep surface penetration
			Vector hn (tmul(GRot(), GPos()-proxybody->GPos()).unit());
			double vs = dotp (sp.groundvel_ship, hn);
			if (vs < vsmax) {
				Vector gv(sp.groundvel_ship * (vsmax/vs)); // rescaled surface-relative velocity
				gv = mul (GRot(), gv);                    // map to global
				s0->vel += (gv - sp.groundvel_glob);
				rvel_base += (gv - sp.groundvel_glob);
				return false;
			}
		}
	}
	return true;
}

// =======================================================================

bool VesselBase::AddSurfaceForces (Vector *F, Vector *M,
	const StateVectors *s, double tfrac, double dt) const
{
	return false;
}
