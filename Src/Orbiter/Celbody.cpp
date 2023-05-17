// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Class CelestialBody
// Class for stars, planets, moons
// Note: for precession and rotation calculations see Doc/Technotes/precession.pdf
// =======================================================================

#define OAPI_IMPLEMENTATION

#include "Orbiter.h"
#include "Element.h"
#include "Celbody.h"
#include "Log.h"
#include "Orbitersdk.h"
#include "PinesGrav.h"

using namespace std;

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern char DBG_MSG[256];

void Pol2Crt (double *pol, double *crt, bool dopos, bool dovel);
void InterpretEphemeris (double *data, int flg, Vector *pos, Vector *vel, Vector *bpos, Vector *bvel);

// =======================================================================
// class CelestialBody

CelestialBody::CelestialBody (double _mass, double _size)
: RigidBody (_mass, _size, Vector (1,1,1)), pinesgrav(NULL)
{
	DefaultParam();
	el = new Elements; TRACENEW
	ClearModule();
	usePinesGravity = false;
}

CelestialBody::CelestialBody (char *fname)
: RigidBody (fname), pinesgrav(this)
{
	char cbuf[256];
	int gravcoeff = 0;
	usePinesGravity = false;

	DefaultParam ();
	ClearModule ();

	ifstream ifs (g_pOrbiter->ConfigPath (fname));
	if (!ifs) {
		LOGOUT_ERR_FILENOTFOUND_MSG(g_pOrbiter->ConfigPath (fname), "while initialising celestial body");
		g_pOrbiter->TerminateOnError();
	}

	if (GetItemString (ifs, "Module", cbuf))
		RegisterModule (cbuf);

	GetItemReal (ifs, "SidRotPeriod", rot_T);
	GetItemReal (ifs, "SidRotOffset", Dphi);
	GetItemReal (ifs, "Obliquity", eps_rel);
	GetItemReal (ifs, "LAN", Lrel0);
	GetItemReal (ifs, "LAN_MJD", mjd_rel);
	GetItemReal (ifs, "PrecessionPeriod", prec_T);

	// precession parameters
	GetItemReal (ifs, "PrecessionObliquity", eps_ref);
	GetItemReal (ifs, "PrecessionLAN", lan_ref);

	if (GetItemString(ifs, "GravModelPath", cbuf) && GetItemInt(ifs, "GravCoeffCutoff", gravcoeff)) {
		char gravModelFileName[512];
		sprintf(gravModelFileName, "GravityModels\\%s",cbuf);
		int maxGravityTerms = 0;
		int	actualLoadedTerms = 0;
		int readResult = 0;
		readResult = pinesgrav.readGravModel(gravModelFileName, gravcoeff, actualLoadedTerms, maxGravityTerms);
		if (readResult == 0) {
			char logbuff[512];
			sprintf(logbuff, "GRAVITY MODEL: %s LOADED, Terms %d/%d", gravModelFileName, actualLoadedTerms, maxGravityTerms);
			LOGOUT(logbuff);
		}
		else if (readResult == 1) {
			char logbuff[512];
			sprintf(logbuff, "GRAVITY MODEL ERROR: COEFFICIENT FILE %s NOT FOUND", gravModelFileName);
			LOGOUT(logbuff);
		}
		else if (readResult == 2) {
			char logbuff[512];
			sprintf(logbuff, "GRAVITY MODEL ERROR: COULD NOT ALLOCATE SPACE FOR GRAVITY MODEL %s", gravModelFileName);
			LOGOUT(logbuff);
		}
		else if (readResult == 3) {
			char logbuff[512];
			sprintf(logbuff, "GRAVITY MODEL ERROR: BAD HEADDER LINE FORMAT %s", gravModelFileName);
			LOGOUT(logbuff);
		}
		else if (readResult == 4) {
			char logbuff[512];
			sprintf(logbuff, "GRAVITY MODEL ERROR: BAD COEFFICIENT LINE FORMAT %s", gravModelFileName);
			LOGOUT(logbuff);
		}

		if (readResult == 0) {
			usePinesGravity = true;
		}
	}

	if (GetItemString (ifs, "JCoeff", cbuf)) {
		char *str;
		str = strtok (cbuf, " \t");
		while (str) {
			double *tmp = new double[njcoeff+1]; TRACENEW
			if (njcoeff) {
				memcpy (tmp, jcoeff, njcoeff*sizeof(double));
				delete []jcoeff;
			}
			jcoeff = tmp;
			sscanf (str, "%lf", jcoeff + njcoeff++);
			str = strtok (NULL, " \t");
		}
	}

	if (GetItemBool (ifs, "HasElements", bInitFromElements) && bInitFromElements) {
		if (GetItemString (ifs, "ElReference", cbuf) &&
			!_stricmp (cbuf, "ParentEquator"))
			elframe = ELFRAME_PARENTEQU;
		el = new Elements (fname); TRACENEW
	}

	double prec;
	if (modIntf.oplanetSetPrecision && GetItemReal (ifs, "ErrorLimit", prec))
		modIntf.oplanetSetPrecision (prec); // old-style initialisation - OBSOLETE

	GetItemBool (ifs, "EllipticOrbit", bFixedElements);
	bDynamicPosVel = !bFixedElements;

	if (module) {
		module->clbkInit ((FILEHANDLE)&ifs);
		if (module->bEphemeris()) { // ephemerides calculated by module
			bDynamicPosVel = false;
			bFixedElements = false;
		}
	}
	if (modIntf.oplanetEphemeris) { // old module interface
		bDynamicPosVel = false;
		bFixedElements = false;
	}

	if (!el) { el = new Elements; TRACENEW }

	Dphi += (td.MJD0 - el->MJDepoch()) * (86400.0/rot_T)*Pi2; // merge rotation at t=0 into offset
	Dphi += Lrel0*cos(eps_rel); // merge precession state into offset
	Dphi = fmod (Dphi, Pi2);
}

CelestialBody::~CelestialBody ()
{
	ClearModule();
	if (nsecondary) {
		delete []secondary;
		secondary = NULL;
	}
	if (njcoeff) {
		delete []jcoeff;
		jcoeff = NULL;
	}
}

void CelestialBody::DefaultParam ()
{
	eps_ref           = 0.0;   // precession reference is ecliptic normal
	lan_ref           = 0.0;
	eps_rel           = 0.0;   // obliquity of axis against reference
	Lrel0             = 0.0;   // longitude of ascending node against reference at ref. time
	rotation_off      = 0.0;
	mjd_rel           = MJD2000; // J2000
	prec_T            = 0.0;   // no precession
	rot_T             = 1e100; // no planet rotation
	Dphi              = 0.0;
	njcoeff           = 0;     // shape for gravity calculations: spherical by default
	cbody             = 0;     // no parent body
	nsecondary        = 0;     // no child bodies
	el                = 0;     // elements undefined
	elframe           = ELFRAME_ECLIPTIC; // reference frame for elements
	bInitFromElements = false;
	hMod              = 0;
	module            = 0;
	bFixedElements = false;
}

void CelestialBody::Setup ()
{
	if (eps_ref) { // precession reference tilted against ecliptic normal
		double sine = sin(eps_ref), cose = cos(eps_ref);
		double sinl = sin(lan_ref), cosl = cos(lan_ref);
		R_ref.Set (1,0,0,  0,cose,-sine,  0,sine,cose);
		R_ref.premul (Matrix (cosl,0,-sinl,  0,1,0,  sinl,0,cosl));
	} else {
		R_ref = IMatrix();
	}

	// derived constants
	prec_omega = (prec_T ? Pi2/prec_T : 0.0);
	rot_omega = Pi2/rot_T;
	cos_eps = cos(eps_rel), sin_eps = sin(eps_rel);

	// Initialise precession parameters
	UpdatePrecession ();

	// init rotation matrix
	UpdateRotation ();
}

void CelestialBody::Attach (CelestialBody *_parent)
{
	// This overrides the standard Attach function
	// We do not link *this to *_parent as a child
	// because it would imply the child to be located
	// in the *rotating* parent system

	if (cbody = _parent) {
		_parent->AddSecondary (this);
		// we add ourselves to the parent's list of secondaries
		el->Setup (mass, cbody->Mass(), td.MJD_ref);

		int flg = ExternPosition ();
		if (flg) {
			if (!(flg & EPHEM_TRUEPOS)) s0->pos = bpos;   // forcing true pos == barycentre:
			if (!(flg & EPHEM_TRUEVEL)) s0->vel = bvel;   // not good! should do better
			if (!(flg & EPHEM_BARYPOS)) bpos = s0->pos;
			if (!(flg & EPHEM_BARYVEL)) bvel = s0->vel;
		} else if (bInitFromElements) {
			el->PosVel (s0->pos, s0->vel, td.SimT0);
			if (elframe == ELFRAME_PARENTEQU) {         // convert to ecliptic frame
				s0->pos.Set (mul (cbody->R_ecl, s0->pos));  // rotate radius vector by planet's obliquity
				s0->vel.Set (mul (cbody->R_ecl, s0->vel));  // rotate velocity vector by planet's obliquity
				el->Calculate (s0->pos, s0->vel, td.SimT0); // update elements with modified state vectors
			}
			bpos = s0->pos;
			bvel = s0->vel;
		}
		s0->pos += cbody->GPos();
		s0->vel += cbody->GVel();
		bpos += cbody->GPos();
		bvel += cbody->GVel();
	} 

	RPlace (s0->pos, s0->vel);

#ifdef UNDEF
	// temporary, for debugging
	if (!strcmp (name, "Jupiter"))
		bDynamicPosVel = true;
#endif
}

void CelestialBody::AddSecondary (CelestialBody *sec)
{
	CelestialBody **tmp = new CelestialBody*[nsecondary+1]; TRACENEW
	memcpy (tmp, secondary, nsecondary*sizeof(CelestialBody*));
	if (nsecondary) delete []secondary;
	secondary = tmp;
	secondary[nsecondary++] = sec;
}

const Elements *CelestialBody::Els () const
{
	if (!bFixedElements) {
		if (cbody) el->Calculate (s0->pos-cbody->GPos(), s0->vel-cbody->GVel(), td.SimT0);
		else       el->Calculate (s0->pos, s0->vel, td.SimT0);
	}
	return el;
}

Vector CelestialBody::Barycentre2Pos (const Vector &bary) const
{
	if (!nsecondary) return bary;
	else {
		Vector b2;
		double m, mtot = mass;
		for (DWORD i = 0; i < nsecondary; i++) {
			mtot += (m = secondary[i]->Mass());
			b2 += secondary[i]->GPos() * m;
		}
		return (bary * mtot - b2) / mass;
	}
}

Vector CelestialBody::Pos2Barycentre (const Vector &pos) const
{
	double m, mtot = mass;
	Vector b(pos*mass);
	for (DWORD i = 0; i < nsecondary; i++) {
		mtot += (m = secondary[i]->Mass());
		b += secondary[i]->GPos() * m;
	}
	return b/mtot;
}

int CelestialBody::RelTrueAndBaryState()
{
	// Calculate the body's true and barycentre state with respect to
	// the parent's position. This recursively steps through all the
	// body's secondary bodies to find the barycentre offset
	// Return value: 0 if state is calculated with respect to parent true state,
	// or EPHEM_PARENTBARY if calculated with respect to parent barycentre

	DWORD i;
	int flg = EPHEM_TRUEPOS | EPHEM_TRUEVEL;
	bool havevel;
	bool havetrue = false, havebary = false;
	static double state[12], *s;


	// recursive child updates
	for (i = 0; i < nsecondary; i++)
		secondary[i]->RelTrueAndBaryState();

	if (!bDynamicPosVel) { // analytic state update
		if (flg = ExternState (state)) { // state updated by module
			if (flg & EPHEM_BARYISTRUE) {
				if (flg & EPHEM_TRUEPOS) {
					s = state;
					havevel = ((flg & EPHEM_TRUEVEL) != 0);
				} else {
					s = state+6;
					havevel = ((flg & EPHEM_BARYVEL) != 0);
				}
				if (flg & EPHEM_POLAR)
					Pol2Crt (s, s, true, havevel);
				cpos = bpos = Vector (s[0], s[1], s[2]);
				havetrue = havebary = true;
				if (havevel)
					cvel = bvel = Vector (s[3], s[4], s[5]);
			} else {
				if (flg & EPHEM_TRUEPOS) {
					havetrue = true;
					havevel = ((flg & EPHEM_TRUEVEL) != 0);
					if (flg & EPHEM_POLAR) Pol2Crt (state, state, true, havevel);
					cpos = Vector (state[0], state[1], state[2]);
					if (havevel) cvel = Vector (state[3], state[4], state[5]);
				}
				if (flg & EPHEM_BARYPOS) {
					havebary = true;
					havevel = ((flg & EPHEM_BARYVEL) != 0);
					if (flg & EPHEM_POLAR) Pol2Crt (state+6, state+6, true, havevel);
					bpos = Vector (state[6], state[7], state[8]);
					if (havevel) bvel = Vector (state[9], state[10], state[11]);
				}
			}
			el_valid = false;

		} else {

			el->Update (bpos, bvel);
			havebary = true;

		}

	} else {
		// to be done!
	}

	// barycentre calculation
	if (havebary && havetrue) {
		bposofs = bpos - cpos;
		bvelofs = bvel - cvel;
	} else {
		// calculate the barycentre offset manually from child positions
		bposofs.Set(0,0,0); bvelofs.Set(0,0,0); // system barycentre
		double bmass = mass;
		for (i = 0; i < nsecondary; i++) {
			bposofs += secondary[i]->bpos * secondary[i]->Mass();
			bvelofs += secondary[i]->bvel * secondary[i]->Mass();
			// note: the mass term should actually include the total mass of
			// the child's subsystem (including its own secondaries)
			if (!secondary[i]->ephem_parentbary)
				bmass += secondary[i]->Mass();
		}
		bposofs /= bmass;
		bvelofs /= bmass;

		if (havetrue) {
			bpos = cpos + bposofs;
			bvel = cvel + bvelofs;
		} else {
			cpos = bpos - bposofs;
			cvel = bvel - bvelofs;
		}
	}

	// Now shift child positions for those children who have reported their
	// positions relative to my barycentre position

	for (i = 0; i < nsecondary; i++) {
		if (secondary[i]->ephem_parentbary) {
			secondary[i]->cpos += bposofs;
			secondary[i]->cvel += bvelofs;
			secondary[i]->bpos += bposofs;
			secondary[i]->bvel += bvelofs;
		}
	}

	return (ephem_parentbary = ((flg & EPHEM_PARENTBARY) != 0));
}

void CelestialBody::AbsTrueState()
{
	// Calculate the body's true position and velocity as the sum of the
	// parent state and its own relative state.
	// Recursively updates all secondary body states.

	int i;

	s1->pos = cpos;
	s1->vel = cvel;
	if (cbody) {
		s1->pos += cbody->s1->pos;
		s1->vel += cbody->s1->vel;
	}
	for (i = 0; i < nsecondary; i++)
		secondary[i]->AbsTrueState();
}

void CelestialBody::Update (bool force)
{
#ifdef UNDEF
	int flg = EPHEM_TRUEPOS | EPHEM_TRUEVEL;
	static Vector pbpos;
	if (nsecondary) pbpos = bpos;

	if (!bDynamicPosVel) {  // analytic update

		prpos.Set (rpos); // store last position
		prvel.Set (rvel); // store last velocity

		if (flg = ExternPosition()) { // state updated externally
			el_valid = false;         // need to re-calculate elements
		} else {                      // propagate elements: use 2-body approximation
			el->Update (*gpos, *gvel);
			flg = EPHEM_TRUEPOS | EPHEM_TRUEVEL;
		}

		if (cbody) { // move to global frame
			if (flg & EPHEM_PARENTBARY) {
				Vector pbary = cbody->Pos2Barycentre (cbody->GPos());
				if (flg & EPHEM_TRUEPOS) *gpos += pbary;
				if (flg & EPHEM_TRUEVEL) *gvel += cbody->GVel(); // don't know how to do that
				if (flg & EPHEM_BARYPOS)  bpos += pbary;
				if (flg & EPHEM_BARYVEL)  bvel += cbody->GVel(); // don't know how to do that
			} else {
				if (flg & EPHEM_TRUEPOS) *gpos += cbody->GPos();
				if (flg & EPHEM_TRUEVEL) *gvel += cbody->GVel();
				if (flg & EPHEM_BARYPOS)  bpos += cbody->GPos();
				if (flg & EPHEM_BARYVEL)  bvel += cbody->GVel();
			}
		}

		// update barycentre
		if (nsecondary) {
			if (!(flg & EPHEM_BARYPOS)) bpos = Pos2Barycentre (prpos);
			if (!(flg & EPHEM_BARYVEL)) bvel = *gvel; // don't know how to do that
			if (!(flg & EPHEM_TRUEPOS)) *gpos = bpos; // Barycentre2Pos (pbpos);
			if (!(flg & EPHEM_TRUEVEL)) *gvel = bvel; // don't know how to do that
		} else {
			if (!(flg & EPHEM_BARYPOS)) bpos = *gpos;
			if (!(flg & EPHEM_BARYVEL)) bvel = *gvel;
			if (!(flg & EPHEM_TRUEPOS)) *gpos = bpos;
			if (!(flg & EPHEM_TRUEVEL)) *gvel = bvel;
		}
	}
#endif

	// If planet supports precession, update precession parameters
	// (should not be necessary at each frame)
	if (prec_T) UpdatePrecession ();

	// Update rotation parameters
	UpdateRotation ();

	RigidBody::Update (force);   // dynamic update

	if (bDynamicPosVel) {
		bpos = (nsecondary ? Pos2Barycentre (s1->pos) : s1->pos);
		bvel = s1->vel;
	}

	acc = cpos * (-cvel.length2()/cpos.length2());
	//if (cbody) acc += cbody->acc;
}

void CelestialBody::UpdatePrecession ()
{
	// Tilt of rotation axis, including precession
	// See Doc/Technotes/precession.pdf for algorithm

	Lrel = Lrel0 + prec_omega*(td.MJD1-mjd_rel);
	double sinl = sin(Lrel), cosl = cos(Lrel);
	// R_rel component of R_ref*R_rel
	Matrix R_ref_rel (cosl, -sinl*sin_eps, -sinl*cos_eps,
		              0,    cos_eps,       -sin_eps,
			          sinl, cosl*sin_eps,  cosl*cos_eps);

	// R_ref component of R_ref*R_rel (tilt of precession reference)
	if (eps_ref) R_ref_rel.premul (R_ref);

	R_axis = mul (R_ref_rel, Vector(0,1,0));   // direction of rotation axis
	eps_ecl = acos (R_axis.y);                 // axis obliquity
	lan_ecl = atan2 (-R_axis.x, R_axis.z);     // axis LAN
	double sinL = sin(lan_ecl), cosL = cos(lan_ecl);
	double sine = sin(eps_ecl), cose = cos(eps_ecl);
	R_ecl.Set (cosL, -sinL*sine, -sinL*cose,   // precession matrix
		       0,    cose,       -sine,
			   sinL, cosL*sine,   cosL*cose);
	double cos_poff = cosL*R_ref_rel.m11 + sinL*R_ref_rel.m31;
	double sin_poff = -(cosL*R_ref_rel.m13 + sinL*R_ref_rel.m33);
	rotation_off = atan2(sin_poff,cos_poff);
}

void CelestialBody::UpdateRotation ()
{
	// Rotation of object around its local axis of rotation (y-axis)
	// See Doc/Technotes/precession.pdf for algorithm

	rotation = posangle (Dphi + td.SimT1*rot_omega - Lrel*cos_eps + rotation_off);
	double cosr = cos(rotation), sinr = sin(rotation);
	s1->R.Set (cosr, 0.0, -sinr,
	           0.0,  1.0,  0.0,
		       sinr, 0.0,  cosr);

	// apply axis tilt
	s1->R.premul (R_ecl);
	s1->Q.Set (s1->R);
}

void CelestialBody::GetRotation (double t, Matrix &rot) const
{
	// Note: this function assumes current precession, i.e. mjd sufficiently close to td.mjd
	double r = posangle (Dphi + t*rot_omega - Lrel*cos_eps + rotation_off);
	double cosr = cos(r), sinr = sin(r);
	rot.Set (cosr, 0.0, -sinr,
	 	     0.0,  1.0,  0.0,
		     sinr, 0.0,  cosr);

	// apply axis tilt
	rot.premul (R_ecl);
}

int CelestialBody::ExternEphemeris (double mjd, int req, double *res) const
{
	if (module)
		return module->clbkEphemeris (mjd, req, res); // new interface
	if (modIntf.oplanetEphemeris) {                   // OBSOLETE!
		int format;
		modIntf.oplanetEphemeris (mjd, res, format);
		return EPHEM_TRUEPOS | EPHEM_TRUEVEL | EPHEM_POLAR;
	}
	return 0;
}

int CelestialBody::ExternFastEphemeris (double simt, int req, double *res) const
{
	if (module) {
		return module->clbkFastEphemeris (simt, req, res); // new interface
	}

	if (modIntf.oplanetFastEphemeris) {
		int format;
		modIntf.oplanetFastEphemeris (simt, res, format);
		return EPHEM_TRUEPOS | EPHEM_TRUEVEL | EPHEM_POLAR;
	}
	return 0;
}

int CelestialBody::ExternPosition ()
{
	static double res[12];
	static int req = EPHEM_TRUEPOS | EPHEM_TRUEVEL | EPHEM_BARYPOS | EPHEM_BARYVEL;
	int flg;

	flg = ExternFastEphemeris (td.SimT1, req, res);
	if (!flg) flg = ExternEphemeris (td.MJD1, req, res);

	if (flg) InterpretEphemeris (res, flg, &s0->pos, &s0->vel, &bpos, &bvel);

	return flg;
}

int CelestialBody::ExternState (double *res)
{
	static int req = EPHEM_TRUEPOS | EPHEM_TRUEVEL | EPHEM_BARYPOS | EPHEM_BARYVEL;
	int flg;

	flg = ExternFastEphemeris (td.SimT1, req, res);
	if (!flg) flg = ExternEphemeris (td.MJD1, req, res);
	return flg;
}

bool CelestialBody::PositionAtTime (double t, Vector *p) const
{
	if (bDynamicPosVel) return false;
	// can't calc at arbitrary times if using dynamic updates

	double res[12];
	static Vector bp;

	int flg = ExternEphemeris (td.MJD_ref+Day(t), EPHEM_TRUEPOS, res);
	if (flg) {
		InterpretEphemeris (res, flg, p, 0, &bp, 0);
		if (!(flg & EPHEM_TRUEPOS)) *p = bp; // fudge: return barycentre pos
	} else {
		double r, ta, sinto, costo;
		el->RelPos (r, ta, t);
		sinto = sin (ta + el->ArgPer());
		costo = cos (ta + el->ArgPer());
		p->x = r * (el->cost*costo - el->sint*sinto*el->cosi);
		p->z = r * (el->sint*costo + el->cost*sinto*el->cosi);
		p->y = r * sinto * el->sini;
	}
	return true;
}

bool CelestialBody::PosVelAtTime (double t, Vector *p, Vector *v) const
{
	if (bDynamicPosVel) return false;
	// can't calc at arbitrary times if using dynamic updates

	double res[12];
	static Vector bp, bv;

	int flg = ExternEphemeris (td.MJD_ref+Day(t), EPHEM_TRUEPOS | EPHEM_TRUEVEL, res);
	if (flg) {
		InterpretEphemeris (res, flg, p, v, &bp, &bv);
		if (!(flg & EPHEM_TRUEPOS)) *p = bp;
		if (!(flg & EPHEM_TRUEVEL)) *v = bv;
	} else {
		el->PosVel (*p, *v, t);
	}
	return true;
}

Vector CelestialBody::InterpolatePosition (double n) const
{
	// Interpolate global position of body by iterative bisection

	if      (n == 0)   return s0->pos;
	else if (n == 1.0) return s1->pos;

	Vector refp0, refp1, refpm;
	const CelestialBody *ref = ElRef();
	if (ref) {
		// otherwise assume that reference position is origin
		refp0.Set (ref->s0->pos);
		refp1.Set (ref->s1->pos);
		refpm = ref->InterpolatePosition (n);
		// recursively get reference position at fractional step n
	}

	const double eps = 1e-2;
	Vector rp0 (s0->pos-refp0);        // rel. position at current step
	Vector rp1 (s1->pos-refp1);        // rel. position at next step
	double rd0 = rp0.length();         // radius at current step
	double rd1 = rp1.length();         // radius at next step
	double n0 = 0.0;                   // lower bound of search bracket
	double n1 = 1.0;                   // upper bound of search bracket
	double nm = 0.5, d = 0.5;          // current trial point
	double rdm = (rd0+rd1)*0.5;        // trial radius
	Vector rpm = (rp0+rp1).unit()*rdm; // trial position
	while (fabs (nm-n) > eps && d > eps) { // interval too large - continue
		d *= 0.5;                         // new interval width
		if (nm < n) {                     // cut away lower half of interval
			rp0 = rpm;
			rd0 = rdm;
			n0  = nm;
			nm += d;
		} else {                          // cut away upper half of interval
			rp1 = rpm;
			rd1 = rdm;
			n1  = nm;
			nm -= d;
		}
		rdm = (rd0+rd1)*0.5;              // new trial radius
		rpm = (rp0+rp1).unit()*rdm;       // new trial position
	}
	if (fabs (nm-n) > 1e-10) {        // linear interpolation of remaining step
		double scale = (n-n0)/(n1-n0);
		rdm = rd0 + (rd1-rd0)*scale;
		rpm = (rp0 + (rp1-rp0)*scale).unit() * rdm;
	}
	return rpm + refpm;
}

StateVectors CelestialBody::InterpolateState (double n) const
{
	// Celestial body state vectors at fractional time n [0..1] between
	// s0 at td.SimT0 and s1 at td.SimT1
	// Note: n > 0 is only allowed during the update phase (between 
	// BeginStateUpdate and EndStateUpdate)

	if (!n) return *s0;
	dCHECK(s1, "Update state not available")
	if (n == 1.0) return *s1;

	StateVectors sv;
	sv.pos = InterpolatePosition (n);
	sv.vel = s0->vel*(1.0-n) + s1->vel*n; // may need a better interpolation
	GetRotation (td.SimT0 + td.SimDT*n, sv.R);
	sv.Q.Set (sv.R);
	sv.omega.Set (s0->omega*(1.0-n) + s1->omega*n); // is this ok?
	return sv;
}

void CelestialBody::RegisterModule (char *dllname)
{
	char cbuf[256];
	module = 0;                              // reset new interface
	memset (&modIntf, 0, sizeof (modIntf));  // reset old interface
	sprintf (cbuf, "Modules\\Celbody\\%s.dll", dllname); // try new module location
	hMod = LoadLibrary (cbuf);
	if (!hMod) {
		sprintf (cbuf, "Modules\\%s.dll", dllname);  // try legacy module location
		hMod = LoadLibrary (cbuf);
	}
	if (!hMod) return;

	// Check if the module provides instance initialisation
	typedef CELBODY* (*INITPROC)(OBJHANDLE);
	INITPROC init_proc = (INITPROC)GetProcAddress (hMod, "InitInstance");
	if (init_proc) { // load interface class

		module = init_proc ((OBJHANDLE)this);

	} else {         // check for old-style interface
		
		char funcname[256], *funcp;
		strcpy (funcname, name); funcp = funcname + strlen(name);

		strcpy (funcp, "_SetPrecision");
		modIntf.oplanetSetPrecision = (OPLANET_SetPrecision)GetProcAddress (hMod, funcname);

		strcpy (funcp, "_Ephemeris");
		modIntf.oplanetEphemeris = (OPLANET_Ephemeris)GetProcAddress (hMod, funcname);

		strcpy (funcp, "_FastEphemeris");
		modIntf.oplanetFastEphemeris = (OPLANET_FastEphemeris)GetProcAddress (hMod, funcname);

		strcpy (funcp, "_AtmPrm");
		modIntf.oplanetAtmPrm = (OPLANET_AtmPrm)GetProcAddress (hMod, funcname);
	}
}

void CelestialBody::ClearModule ()
{
	if (hMod) {
		if (module) { // new interface
			typedef void (*EXITPROC)(CELBODY*);
			EXITPROC exit_proc = (EXITPROC)GetProcAddress (hMod, "ExitInstance");
			if (exit_proc) { // allow module to clean up
				exit_proc (module);
			} else {         // no cleanup - we delete the interface class here
				delete module;
			}
			module = 0;
		}
		FreeLibrary (hMod);
		hMod = 0;
	}
	memset (&modIntf, 0, sizeof (modIntf)); // old interface
}

// =======================================================================
// Nonmember functions
// =======================================================================

void Pol2Crt (double *pol, double *crt, bool dopos, bool dovel)
{
	double rad  = pol[2] * AU; // convert AU -> m (should be moved into module)
	double cosp = cos(pol[0]), sinp = sin(pol[0]); // cos, sin longitude
	double cost = cos(pol[1]), sint = sin(pol[1]); // cos, sin latitude
	double xz   = rad * cost;
	if (dopos) {
		crt[0] = xz  * cosp;
		crt[2] = xz  * sinp;
		crt[1] = rad * sint;
	}
	if (dovel) {
		double vl = xz  * pol[3]; // velocity in longitude [m/s]
		double vb = rad * pol[4]; // velocity in latitude [m/s]
		double vr = pol[5] * AU;  // radial velocity [m/s]
		// rotate velocity vector to current long/lat
		crt[3] = cosp*cost*vr - cosp*sint*vb - sinp*vl;
		crt[4] = sint*     vr + cost*     vb;
		crt[5] = sinp*cost*vr - sinp*sint*vb + cosp*vl;
	}
}

void InterpretEphemeris (double *data, int flg, Vector *pos, Vector *vel, Vector *bpos, Vector *bvel)
{
	static double crt[6], *p;

	if (flg & (EPHEM_TRUEPOS|EPHEM_TRUEVEL)) {
		if (flg & EPHEM_POLAR) {
			Pol2Crt (data, crt, (flg & EPHEM_TRUEPOS) && (pos != 0),
				                (flg & EPHEM_TRUEVEL) && (vel != 0));
			p = crt;
		} else {
			p = data;
		}
		if (pos) {
			pos->x = p[0];
			pos->y = p[1];
			pos->z = p[2];
		}
		if (vel) {
			vel->x = p[3];
			vel->y = p[4];
			vel->z = p[5];
		}
	}
	if (bpos || bvel) { // obtain barycentre
		if (!(flg & EPHEM_BARYISTRUE)) {
			if (flg & (EPHEM_BARYPOS|EPHEM_BARYVEL)) {
				if (flg & EPHEM_POLAR) {
					Pol2Crt (data+6, crt, (flg & EPHEM_BARYPOS) && (bpos != 0),
						                  (flg & EPHEM_BARYVEL) && (bvel != 0));
					p = crt;
				} else {
					p = data+6;
				}
			} else return; // no barycentre information
		}
		if (bpos) {
			bpos->x = p[0];
			bpos->y = p[1];
			bpos->z = p[2];
		}
		if (bvel) {
			bvel->x = p[3];
			bvel->y = p[4];
			bvel->z = p[5];
		}
	}
}

// =======================================================================
// Class CELBODY: API interface class

CELBODY::CELBODY ()
{ version = 1; }

bool CELBODY::bEphemeris () const
{ return false; }

void CELBODY::clbkInit (FILEHANDLE cfg)
{}

int CELBODY::clbkEphemeris (double mjd, int req, double *ret)
{ return 0; }

int CELBODY::clbkFastEphemeris (double simt, int req, double *ret)
{ return 0; }

bool CELBODY::clbkAtmParam (double alt, ATMPARAM *prm)
{ return false; }

void CELBODY::Pol2Crt (double *pol, double *crt)
{
	::Pol2Crt (pol, crt, true, true);
}


// =======================================================================
// class CELBODY2: API interface class

CELBODY2::CELBODY2 (OBJHANDLE hCBody): CELBODY ()
{
	version++;
	hBody = hCBody;
	atm = NULL;
	hAtmModule = NULL;
}

CELBODY2::~CELBODY2 ()
{
	FreeAtmosphere();
}

void CELBODY2::clbkInit (FILEHANDLE cfg)
{
	CELBODY::clbkInit (cfg);

	// Load external atmosphere modules
	if (!hAtmModule) {
		// 1: try Config\<Name>\Atmosphere.cfg for interactive setting
		char fname[256], name[256];
		oapiGetObjectName (hBody, name, 256);
		strcat (name, "\\Atmosphere.cfg");
		FILEHANDLE hFile = oapiOpenFile (name, FILE_IN, CONFIG);
		if (oapiReadItem_string (hFile, "MODULE_ATM", fname) || oapiReadItem_string (cfg, "MODULE_ATM", fname)) {
			if (_stricmp (fname, "[None]"))
				LoadAtmosphereModule (fname);
		}
		oapiCloseFile (hFile, FILE_IN);
	}
}

OBJHANDLE CELBODY2::GetParent () const
{
	return (OBJHANDLE)((CelestialBody*)hBody)->cbody;
}

OBJHANDLE CELBODY2::GetChild (DWORD idx) const
{
	CelestialBody *cbody = (CelestialBody*)hBody;
	if (idx < cbody->nsecondary) return (OBJHANDLE)cbody->secondary[idx];
	else                         return NULL;
}

void CELBODY2::SetAtmosphere (ATMOSPHERE *a)
{
	FreeAtmosphere(); // unload existing atmosphere instance
	atm = a;
}

bool CELBODY2::FreeAtmosphere ()
{
	if (!atm) return false;
	if (!FreeAtmosphereModule()) {
		delete atm;
		atm = 0;
	}
	return true;
}

bool CELBODY2::LoadAtmosphereModule (const char *fname)
{
	char path[256], name[256];
	oapiGetObjectName (hBody, name, 256);
	sprintf (path, "Modules\\Celbody\\%s\\Atmosphere", name);
	if (!(hAtmModule = g_pOrbiter->LoadModule (path, fname))) return false;
	ATMOSPHERE *(*func)(CELBODY2*) = (ATMOSPHERE*(*)(CELBODY2*))GetProcAddress (hAtmModule, "CreateAtmosphere");
	if (!func) {
		g_pOrbiter->UnloadModule (fname);
		hAtmModule = NULL;
		return false;
	}
	atm = func(this);
	return true;
}

bool CELBODY2::FreeAtmosphereModule ()
{
	if (!hAtmModule) return false;
	if (atm) {
		void (*func)(ATMOSPHERE*) = (void(*)(ATMOSPHERE*))GetProcAddress(hAtmModule, "DeleteAtmosphere");
		if (func) {
			func (atm);
		} else {
			// no instance destruction support in module - let's do it ourselves
			delete atm;
		}
		atm = 0;
	}
	g_pOrbiter->UnloadModule (hAtmModule);
	hAtmModule = NULL;
	return true;
}

double CELBODY2::SidRotPeriod () const
{
	return ((CelestialBody*)hBody)->rot_T;
}


// =======================================================================
// class ATMOSPHERE: API interface class

ATMOSPHERE::ATMOSPHERE (CELBODY2 *body)
{
	cbody =  body;
}

bool ATMOSPHERE::clbkConstants (ATMCONST *atmc) const
{
	// Some Earth defaults. Probably not very useful in general, but
	// we want to make sure they are set to physical values.
	atmc->R = 286.91;
	atmc->gamma = 1.4;
	return false;
}

bool ATMOSPHERE::clbkParams (const PRM_IN *prm_in, PRM_OUT *prm_out)
{
	return false;
}

