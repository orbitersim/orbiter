// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Implementation of Lieske GALSAT perturbation solutions for Galilean
// Jupiter moons. Converted from FORTRAN sources.
// ======================================================================

#include <stdio.h>
#include <memory.h>
#include <math.h>
#include "Galsat.h"

// Global variables

const double TWOPI = 6.28318530717958648;
const double DEGRAD = 57.2957795130823208;

struct {
    double earay[28], baray[22], paray[28];
} ebblok_1;

struct {
    double trmcod[53];
} trmblk_1;

struct {
    double angcod[99], ratcod[99], ang[22], rat[23];
} angblk_1;

struct {
    double axis[4], cxi1[10], argx1[10], ratx1[10], cz1[7], argz1[7], 
		ratz1[7], cv1[41], argv1[41], ratv1[41], cxi2[24], argx2[24], 
		ratx2[24], cz2[11], argz2[11], ratz2[11], cv2[66], argv2[66], 
		ratv2[66], cxi3[31], argx3[31], ratx3[31], cz3[13], argz3[13], 
		ratz3[13], cv3[75], argv3[75], ratv3[75], cxi4[49], argx4[49], 
		ratx4[49], cz4[18], argz4[18], ratz4[18], cv4[89], argv4[89], 
		ratv4[89], epsln;
    int nxi1t, nz1t, nv1t, nxi2t, nz2t, nv2t, nxi3t, nz3t, nv3t,
		nxi4t, nz4t, nv4t, kodx1[20], kodz1[14], kodv1[82], kodx2[48],
		kodz2[22], kodv2[132], kodx3[62], kodz3[26], kodv3[150],
		kodx4[98], kodz4[36], kodv4[178];
} theory_1;

struct {
    double t, rb[6], angbx[14], angbz[10], cofbx[7], cofbz[5];
} local_1;

struct {
    double tlast, tlastg, cj, sj, ci, si, cn, sn, cp, sp, phi, phidot,
	p[9], q[9], qdot[9], qmat[9], tlastp;
} svtloc_1;

// Prototypes

int qqdot (double t, double &qpsi11, double &qpsi21);
void unkod (int *kode, int *kod, int *kmin);
void barcor();
void updat (double *argx, int *kodx, double *argv, int *kodv, double *argz,
    int *kodz, int nmx, int nmv, int nmz);
int samjay (int nx, int nv, int nz, double *cx, double *cv, double *cz,
    int nmx, int nmv, int nmz, int nsat, double *ang, double *rat, int nflag);
inline double d_mod (double x, double y)
{ return x - (int)(x/y) * y; }

// Function galsat

void galsat (double *r__, double *rorb, double tjd, int ksat, int kflag)
{
    /* Initialized data */

    static double tolt = 1e-4;
    static double tolg = 50.;
    //static int first = 1;
    static double tref = 2443000.5;

    /* Local variables */
    static int kflg, nsat;
    static double qpsi11, qpsi21;
    static int i, j, nflag;
    extern int revizg_();


/* **************************************************************** */
/* this version calculates jupiter-centered (ksat=1,4) coordinates */
/* or barycentric (ksat=-1,-4) satellite coordinates, */
/* or coordinates of jupiter relative to barycenter (ksat=0) */
/* **************************************************************** */
/*  documented in "theory of motion of jupiter's galilean satellites," */
/*   astronomy & astrophysics 56, 333-352 (1977). */
/*  see also jpl engineering memorandum 314-112 (1 mar 1977). */


/*  input */
/*      tjd (double precision) = tdb julian date requested */
/*      nsat (integer) = satellite i.d. */
/*                nsat = 1 is  io */
/*                nsat = 2 is  europa */
/*                nsat = 3 is  ganymede */
/*                nsat = 4 is  callisto */

/*      kflag=1 means position only */
/*      kflag=2 means position and velocity */

/* output */
/*      r (dimension 6) = position and velocity in earth j2000 equatorial */
/*                        frame in au, au/day */
/* --set up equivalence arrays for ease in calling */
/* for galsat/galsap local communication */
/* these variables are for calculation of bary to jup vector */
/* they make use of fact that cofby=cofbx with cos replaced by sin */
/*  note that cofbx and cofbz were single precision in the @for version! */
/* --local common block for subroutines */

/*  note:  the above /local/ is the definition of the common block. */
/*         to strictly follow f77 standards, all occurrences of /local/ */
/*         must be declared the same length.  i let the compiler do it. */
/*         affected routines are samjay, barcor in galsat */
/*                           and samjap, dekod, barcop in galsap */
/*                           and updat, chkgal, revizg in satsap */
/* local variables to be saved for galsat/galsap and chkgal, qqdot */
/* tolt is tolerance for tlast updates, tolg is tolerance for tlastg updates */

/* ..start computation */
    nflag = kflag;
	kflg = (ksat > 0 ? 1 : -1);
    nsat = abs(ksat);
    local_1.t = tjd - tref;
	/*  note that chkgal will re-set values for tlast, tlastg, tlastp. */
	/*  galsap will check for previous use by galsat, but galsat won't check */
	/*  for galsap. */
	/*  hence, for rigorous check-out one should first call galsat before galsap or */
	/*  else always update the ancillary angles by setting tolt and tolg equal to */
	/*  zero. */

	// **** MS: chkgal is now called up-front from the initialisation routine
    //if (first) {
	//	first = 0;
	//	chkgal();
    //}

    if (fabs(local_1.t - svtloc_1.tlast) > tolt) {
		svtloc_1.tlast = local_1.t;
		/*  mark tlastp as being 'dirty' so can intermix galsat/galsap calls */
		/*  because galsap uses 'q' in a different sense for partials */
		/*  first call to galsap will initialize tlastp to -4.d20, so this */
		/*  is our way of telling whether or not galsap has been initialized, */
		/*  as well as being able to force a tlastg update */
		svtloc_1.tlastp = -6e20;
		qqdot (local_1.t, qpsi11, qpsi21);

		// **** MS: remove rotation from ecliptic to earth equator
		//mult3g_(svtloc_1.p, svtloc_1.qdot, svtloc_1.qmat);
		//mult3g_(svtloc_1.p, svtloc_1.q, svtloc_1.qdot);
		memcpy (svtloc_1.qmat, svtloc_1.qdot, 9*sizeof(double));
		memcpy (svtloc_1.qdot, svtloc_1.q, 9*sizeof(double));
    }
	/* now pq is in qdot (for position) and pqdot is in qmat (for vel) */
	/*  positions are r = qdot * rb, */
	/*  velocities are rdot = qdot * rbdot + qmat * rb */
	/* --revise g by adding dg now */
	/* --these update arguments for dg every 50 days */
    if (fabs(local_1.t - svtloc_1.tlastg) > tolg) {
		svtloc_1.tlastg = local_1.t;
		revizg_();
    }
	/* now compute satellite coordinates */
	/*   since do not have args subscripted, set up subr */
    for (i = 0; i < 6; ++i) {
		local_1.rb[i] = 0.;
		r__[i] = 0.;
    }
	switch (nsat) {
	case 0:         // Jupiter w.r.t. barycentre
		barcor();
		break;
	case 1:
		samjay (theory_1.nxi1t, theory_1.nv1t, theory_1.nz1t, theory_1.cxi1, theory_1.cv1,
			theory_1.cz1, 10, 41, 7, nsat, angblk_1.ang, angblk_1.rat, nflag);
		break;
	case 2:
		samjay (theory_1.nxi2t, theory_1.nv2t, theory_1.nz2t, theory_1.cxi2, theory_1.cv2,
			theory_1.cz2, 24, 66, 11, nsat, angblk_1.ang, angblk_1.rat, nflag);
		break;
	case 3:
		samjay (theory_1.nxi3t, theory_1.nv3t, theory_1.nz3t, theory_1.cxi3, theory_1.cv3,
			theory_1.cz3, 31, 75, 13, nsat, angblk_1.ang, angblk_1.rat, nflag);
		break;
	case 4:
		samjay (theory_1.nxi4t, theory_1.nv4t, theory_1.nz4t, theory_1.cxi4, theory_1.cv4,
			theory_1.cz4, 49, 89, 18, nsat, angblk_1.ang, angblk_1.rat, nflag);
		break;
    }

#ifdef UNDEF
	// MS: no longer needed
    if (kflg < 0) {
		barcor();
    }
	/* ..return the orbital plane state vector */
    for (i = 0; i < 6; ++i) {
		rorb[i] = local_1.rb[i];
    }
#endif

	/* --now go to earth's mean equator */
	// MS: this now just converts to ecliptic, since we have removed one of the
	// rotation matrices
    for (i = 0; i < 3; ++i) {
		for (j = 0; j < 3; ++j) {
			r__[i] += svtloc_1.qdot[i + j*3] * local_1.rb[j];
			if (kflag != 1) {
				r__[i + 3] += svtloc_1.qdot[i + j*3] * local_1.rb[j + 3] +
					          svtloc_1.qmat[i + j*3] * local_1.rb[j];
			}
		}
    }
}


/* = = = = = = = = = = = = = = = = = = = = = = = = = = = */
int samjay (int nx, int nv, int nz, double *cx, double *cv, double *cz,
    int nmx, int nmv, int nmz, int nsat, double *ang, double *rat, int nflag)
{
    /* System generated locals */
    double d__1;

    /* Local variables */
    static double angl, sdot, vdot, sdfac, s, v;
    static int k;
    static double xidot, q1, q2, q3, q4, ca, sa, dt, xi;

/* **************************************************** */
/* >> note:  the q1..q4 variables are employed for consistency with samjap in galsap */
/* >>        if they are not employed, the code would be somewhat shorter. */
/* >>        they are used here to maintain compatibility and ease of program revisions. */
/* >> for galsat/galsap local communication */
/* j*      double precision twopi,degrad,t,rb(6) */
/* j*c--local common block for subroutines */
/* j*      common /local/ twopi,degrad,t,rb */
/* >> */
/* > only twopi,degrad,t,rb(6) are needed here, but we include all to meet f77 standard */
/* ****************************************** */
/* --local common block for subroutines */
/* >> */
/* ****************************************** */
/* >> */
/* ****************************************** */
/* j*      double precision axis(4) */
/* j*      common /theory/ axis */
/* >  only axis is needed here, but we include all to meet f77 standard. */
/* ****************************************** */
/* ****************************************** */
/* > */
/* -- */
    /* Parameter adjustments */
    cx -= (1+nmx);
    cv -= (1+nmv);
    cz -= (1+nmz);
    --ang;
    --rat;

    /* Function Body */
    xi = 0.;
    v = 0.;
    s = 0.;
    xidot = 0.;
    vdot = 0.;
    sdot = 0.;
    for (k = 1; k <= nx; ++k) {
		d__1 = cx[k + (nmx << 1)] + cx[k + nmx * 3] * local_1.t;
		angl = d_mod(d__1, TWOPI);
		ca = cos(angl);
		q1 = cx[k + nmx] * ca;
		xi += q1;
		if (nflag != 1) {
			sa = sin(angl);
			q2 = cx[k + nmx] * sa;
			xidot -= q2 * cx[k + nmx * 3];
		}
    }
    for (k = 1; k <= nv; ++k) {
		d__1 = cv[k + (nmv << 1)] + cv[k + nmv * 3] * local_1.t;
		angl = d_mod(d__1, TWOPI);
		sa = sin(angl);
		q2 = cv[k + nmv] * sa;
		v += q2;
		if (nflag != 1) {
			ca = cos(angl);
			q1 = cv[k + nmv] * ca;
			vdot += q1 * cv[k + nmv * 3];
		}
    }
    dt = v / rat[nsat];
    sdfac = vdot / rat[nsat] + 1.;
/* >>  sdfac is irrelevant (its value will be 1) when nflag=1 (position-only) */
    for (k = 1; k <= nz; ++k) {
		d__1 = cz[k + (nmz << 1)] + cz[k + nmz * 3] * (local_1.t + dt);
		angl = d_mod(d__1, TWOPI);
		sa = sin(angl);
		q2 = cz[k + nmz] * sa;
		s += q2;
		if (nflag != 1) {
			ca = cos(angl);
			q1 = cz[k + nmz] * ca;
/* jay  put factor later      !sdot=sdot+q1*cz(k,3)*(1.d0+vdot/rat(nsat)) */
			sdot += q1 * cz[k + nmz * 3];
		}
    }
/* --this is l-psi+v */
    d__1 = ang[nsat] - ang[15] + (rat[nsat] - rat[15]) * local_1.t;
    angl = d_mod(d__1, TWOPI) + v;
    q1 = theory_1.axis[nsat - 1] * cos(angl);
    q2 = theory_1.axis[nsat - 1] * sin(angl);
    q3 = theory_1.axis[nsat - 1] * s;
    q4 = xi + 1.;
    local_1.rb[0] = q1 * q4;
    local_1.rb[1] = q2 * q4;
    local_1.rb[2] = q3 * q4;
    if (nflag == 1) {
		return 0;
    }
/* >> now correct for the sdot factor in time-completed: */
    sdot *= sdfac;
    ca = rat[nsat] - rat[15] + vdot;
    local_1.rb[3] = q1 * xidot - local_1.rb[1] * ca;
    local_1.rb[4] = q2 * xidot + local_1.rb[0] * ca;
    local_1.rb[5] = q3 * xidot + theory_1.axis[nsat - 1] * q4 * sdot;
    return 0;
} /* samjay_ */

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = */
void barcor()
{
    /* System generated locals */
    double d__1;

    /* Local variables */
    static double angl;
    static int i1;
    static double t1, t2, ca, sa;

/* ************************************************** */
/* --calculate barycenter-to-jupiter vector */
/* >> for galsat/galsap local communication */
/* --these variables are for calculation of bary to jup vector */
/* --they make use of fact that cofby=cofbx with cos replaced by sin */
/* --note that cofbx and cofbz were single precision in the @for version! */
/* --local common block for subroutines */
/* >> */
/* >> */
/*  this routine calculates the barycenter-to-jupiter shift */
/*  vector for cases when galsap is called with negative satellite */
/*  number or zero.  see lieske, jpl engineering memorandum 314-112 */
/*  "additional fortran subroutines for obtaining positions and */
/*  partial derivatives of jupiter's galilean satellites", 1 mar 1977. */
/*  the short series employed are stored in cofbx and cofbz for the coefficients */
/*  and in angbx and angbz for the angles.  the series are as follows: */
/*   10**10 xbj = (-1262 - 1267 e1) cos (l1 - psi) -1133 (1 + e2) cos (l2 - psi) */
/*               -5715 (1 + e3) cos (l3 - psi) -5668 (1 + e4) cos (l4 - psi) */
/*               +12 cos (pi3 - psi) + (68 + 68 e19 + 67 e4) cos (pi4 - psi) */
/*               -21 (1 + e4) cos (2l4 - pi4 - psi) */

/*   10**10 ybj = same as xbj with cos replaced by sin */

/*   10**10 zbj = -9 sin (l2 - omega2) -18 (1 + e3) sin (l3 - omega3) */
/*                -27 (1 + e4) sin (l4 - omega4) +9 sin (l3 - psi) */
/*                +(42 + 44 e4) sin (l4 - psi) */

/* >> */
/* -- */
    for (i1 = 1; i1 <= 7; ++i1) {
		d__1 = local_1.angbx[i1 - 1] + local_1.angbx[i1 + 6] * local_1.t;
		angl = d_mod(d__1, TWOPI);
		t1 = local_1.cofbx[i1 - 1];
		t2 = local_1.angbx[i1 + 6];
		ca = t1 * cos(angl) * 1e-10;
		sa = t1 * sin(angl) * 1e-10;
		local_1.rb[0] += ca;
		local_1.rb[1] += sa;
		local_1.rb[3] -= sa * t2;
		local_1.rb[4] += ca * t2;
    }
    for (i1 = 1; i1 <= 5; ++i1) {
		d__1 = local_1.angbz[i1 - 1] + local_1.angbz[i1 + 4] * local_1.t;
		angl = d_mod(d__1, TWOPI);
		t1 = local_1.cofbz[i1 - 1];
		t2 = local_1.angbz[i1 + 4];
		ca = t1 * cos(angl) * 1e-10;
		sa = t1 * sin(angl) * 1e-10;
		local_1.rb[2] += sa;
		local_1.rb[5] += ca * t2;
    }
}

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = */
void chkgal (void)
{
    /* Local variables */
    static int k;
    static double orbecl, orbequ;


/* ********************************************** */
/* >> check the common blocks to see if everything's loaded & print version */
/* ****************************************** */
/* ****************************************** */
/* ****************************************** */
/* ****************************************** */
/* ****************************************** */
/* >> for galsat/galsap local communication */
/* --these variables are for calculation of bary to jup vector */
/* --they make use of fact that cofby=cofbx with cos replaced by sin */
/* --note that cofbx and cofbz were single precision in the @for version! */
/* --local common block for subroutines */
/* >> */
/* ****************************************** */
/* >> local variables to be saved for galsat/galsap and chkgal, qqdot */
/* >> */
/* ****************************************** */
/* ****************************************** */

	/* --set up bary to jupiter shift coefficients first call */
    local_1.cofbx[0] = -1262. - ebblok_1.earay[0] * 1267.;
    local_1.cofbx[1] = (ebblok_1.earay[1] + 1.) * -1133.;
    svtloc_1.cj = ebblok_1.earay[2] + 1.;
    local_1.cofbx[2] = svtloc_1.cj * -5715.;
    svtloc_1.sj = ebblok_1.earay[3] + 1.;
    local_1.cofbx[3] = svtloc_1.sj * -5668.;
    local_1.cofbx[4] = 12.;
    local_1.cofbx[5] = (ebblok_1.earay[18] + 1.) * 68. + ebblok_1.earay[3] * 67.;
    local_1.cofbx[6] = svtloc_1.sj * -21.;
    local_1.cofbz[0] = -9.;
    local_1.cofbz[1] = svtloc_1.cj * -18.;
    local_1.cofbz[2] = svtloc_1.sj * -27.;
    local_1.cofbz[3] = 9.;
    local_1.cofbz[4] = ebblok_1.earay[3] * 44. + 42.;
    for (k = 1; k <= 4; ++k) {
		local_1.angbx[k - 1] = angblk_1.ang[k - 1] - angblk_1.ang[14];
		local_1.angbx[k + 6] = angblk_1.rat[k - 1] - angblk_1.rat[14];
		if (k != 1) {
			local_1.angbz[k - 2] = angblk_1.ang[k - 1] - angblk_1.ang[k + 9];
			local_1.angbz[k + 3] = angblk_1.rat[k - 1] - angblk_1.rat[k + 9];
		}
    }
    for (k = 1; k <= 2; ++k) {
		local_1.angbx[k + 3] = angblk_1.ang[k + 6] - angblk_1.ang[14];
		local_1.angbx[k + 10] = angblk_1.rat[k + 6] - angblk_1.rat[14];
		local_1.angbz[k + 2] = local_1.angbx[k + 1];
		local_1.angbz[k + 7] = local_1.angbx[k + 8];
    }
    local_1.angbx[6] = angblk_1.ang[3] * 2. - angblk_1.ang[8] - angblk_1.ang[14];
    local_1.angbx[13] = angblk_1.rat[3] * 2. - angblk_1.rat[8] - angblk_1.rat[14];
	/* --this completes setup of bary-to-jupiter arrays */
	/* --set up rotation matrices */
    orbecl = ebblok_1.paray[25] * (ebblok_1.earay[25] + 1.) / DEGRAD;
    svtloc_1.cj = cos(orbecl);
    svtloc_1.sj = sin(orbecl);
    orbequ = ebblok_1.paray[24] * (ebblok_1.earay[24] + 1.) / DEGRAD;
    svtloc_1.ci = cos(orbequ);
    svtloc_1.si = sin(orbequ);
	/* --note if node has rate, it should be calculated after stat 1 */
    svtloc_1.cn = cos(angblk_1.ang[21]);
    svtloc_1.sn = sin(angblk_1.ang[21]);
	/* -- set up rotation for ecliptic of 1950 to equator of 1950 */

#ifdef UNDEF
	// MS: disable rotation from ecliptic to earth equator
    double obl = ebblok_1.paray[26] * (ebblok_1.earay[26] + 1.) / DEGRAD;
    rotg_(&c__1, &obl, svtloc_1.p);
#endif

    svtloc_1.tlast = -2e20;
    svtloc_1.tlastp = -4e20;
    svtloc_1.tlastg = -6e20;
}

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = */
int qqdot (double t, double &qpsi11, double &qpsi21)
{
    /* Local variables */
    static int l;

/* **************************************************** */
/* >> calculate q and qdot matrices where */
/*   q    = r(-node) p(-j) r(-phi) p(-i) */
/*   qdot = r(-node) p(-j) rdot(-phi) p(-i) */
/*     the variables qpsi11 and qpsi21 are not used by galsat, */
/*     but are calculated here so that galsap can easily use them. */
/*     21 feb 91   jay lieske */
/* j*      implicit none */
/* ****************************************** */
/* ****************************************** */
/* >> local variables to be saved for galsat/galsap and chkgal, qqdot */
/* >> */
/* ****************************************** */
/* -- */
    svtloc_1.phidot = angblk_1.rat[14];
    svtloc_1.phi = svtloc_1.phidot * t + angblk_1.ang[14] - angblk_1.ang[21];
    svtloc_1.cp = cos(svtloc_1.phi);
    svtloc_1.sp = sin(svtloc_1.phi);
/* --set up matrix to go from jup equ to 1950 ecl */
    svtloc_1.q[0] = svtloc_1.cn * svtloc_1.cp - svtloc_1.sn * svtloc_1.cj * 
	    svtloc_1.sp;
    qpsi11 = -svtloc_1.cn * svtloc_1.sp - svtloc_1.sn * svtloc_1.cj * 
	    svtloc_1.cp;
    svtloc_1.q[3] = qpsi11 * svtloc_1.ci + svtloc_1.sn * svtloc_1.sj * 
	    svtloc_1.si;
    svtloc_1.q[6] = -qpsi11 * svtloc_1.si + svtloc_1.sn * svtloc_1.sj * 
	    svtloc_1.ci;
    svtloc_1.q[1] = svtloc_1.sn * svtloc_1.cp + svtloc_1.cn * svtloc_1.cj * 
	    svtloc_1.sp;
    svtloc_1.qdot[0] = qpsi11 * svtloc_1.phidot;
    qpsi21 = -svtloc_1.sn * svtloc_1.sp + svtloc_1.cn * svtloc_1.cj * 
	    svtloc_1.cp;
    svtloc_1.qdot[1] = qpsi21 * svtloc_1.phidot;
    svtloc_1.q[4] = qpsi21 * svtloc_1.ci - svtloc_1.cn * svtloc_1.sj * 
	    svtloc_1.si;
    svtloc_1.q[7] = -qpsi21 * svtloc_1.si - svtloc_1.cn * svtloc_1.sj * 
	    svtloc_1.ci;
    svtloc_1.q[2] = svtloc_1.sp * svtloc_1.sj;
    svtloc_1.q[5] = svtloc_1.cp * svtloc_1.sj * svtloc_1.ci + svtloc_1.cj * 
	    svtloc_1.si;
    svtloc_1.q[8] = -(svtloc_1.cp * svtloc_1.sj) * svtloc_1.si + svtloc_1.cj *
	     svtloc_1.ci;
    for (l = 1; l <= 3; ++l) {
	svtloc_1.qdot[l + 2] = -(svtloc_1.q[l - 1] * svtloc_1.phidot) * 
		svtloc_1.ci;
/* L3: */
	svtloc_1.qdot[l + 5] = svtloc_1.q[l - 1] * svtloc_1.phidot * 
		svtloc_1.si;
    }
    svtloc_1.qdot[2] = svtloc_1.cp * svtloc_1.sj * svtloc_1.phidot;
/* --note if node rate .ne. 0, then place cn and sn after stat 1 */
/* -- and define phidot=rat(15)-rat(22) (rad/day), and add */
/* --increments  qdot(1,1)=qdot(1,1)-q(2,1)*rat(22) */
/* --            qdot(2,1)=qdot(2,1)+q(1,1)*rat(22) */
/* --            qdot(1,2)=qdot(1,2)-q(2,2)*rat(22) */
/* --            qdot(2,2)=qdot(2,2)+q(1,2)*rat(22) */
/* --            qdot(1,3)=qdot(1,3)-q(2,3)*rat(22) */
/* --            qdot(2,3)=qdot(2,3)+q(1,3)*rat(22) */
/* -- */
    return 0;
}

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = */
/* Subroutine */ int revizg_()
{
    /* Local variables */
    static int k;
    static double dg, qx;

/* ********************************************** */
/* >> revise angles that depend on jupiter's g for jupiter/saturn inequality */
/*   see lieske, astronomy & astrophysics 56,333-352 (1977) table 3 footnote. */
/* ****************************************** */
/* ****************************************** */
/* ****************************************** */
/* >> for galsat/galsap local communication */
/* j*      double precision twopi,degrad,t */
/* j*c--local common block for subroutines */
/* j*      common /local/ twopi,degrad,t */
/* >  only twopi, degrad, t are needed, but we include all to meet f77 standard */
/* ****************************************** */
/* --local common block for subroutines */
/* >> */
/* ****************************************** */
/* >> */
/* -- */
/* j*      tlastg=t */
    qx = angblk_1.ang[15] * 2. - angblk_1.ang[16] + (float).76699 / 
	    DEGRAD + (angblk_1.rat[15] * 2. - angblk_1.rat[16]) * local_1.t;
    qx = d_mod(qx, TWOPI);
    dg = sin(qx) * .03439;
    qx = angblk_1.ang[15] * 5. - angblk_1.ang[16] * 2. + (float)64.26288 / 
	    DEGRAD + (angblk_1.rat[15] * 5. - angblk_1.rat[16] * 2.) *
	     local_1.t - .02276946941 / DEGRAD * local_1.t / 365.25;
    qx = d_mod(qx, TWOPI);
    dg = (dg + sin(qx) * .33033) / DEGRAD;
    for (k = 86; k <= 92; ++k) {
		qx = (double) (k - 90);
		if (qx >= 0.) {
			qx += 1.;
		}
		angblk_1.angcod[k - 1] = qx * (angblk_1.ang[16] + dg);
    }
    updat (theory_1.argx1, theory_1.kodx1, theory_1.argv1, theory_1.kodv1, 
	    theory_1.argz1, theory_1.kodz1, 10, 41, 7);
    updat (theory_1.argx2, theory_1.kodx2, theory_1.argv2, theory_1.kodv2, 
	    theory_1.argz2, theory_1.kodz2, 24, 66, 11);
    updat (theory_1.argx3, theory_1.kodx3, theory_1.argv3, theory_1.kodv3, 
	    theory_1.argz3, theory_1.kodz3, 31, 75, 13);
    updat (theory_1.argx4, theory_1.kodx4, theory_1.argv4, theory_1.kodv4, 
	    theory_1.argz4, theory_1.kodz4, 49, 89, 18);
    return 0;
} /* revizg_ */

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = */
void updat (double *argx, int *kodx, double *argv, int *kodv, double *argz, int *kodz, int nmx, int nmv, int nmz)
{
    /* Local variables */
    static int kmin;
    static int kl, km, km1, kod[8], kmz;

/* **************************************************** */
/*  called by revizg to update jupiter's mean anomaly for inequalities. */
/* --updates angles for dg changes */
/* **************************************************** */
/* >> for galsat/galsap local communication */
/* j*      double precision twopi */
/* j*      common /local/ twopi */
/* > only twopi is needed, but we include all to meet f77 standard */
/* ****************************************** */
/* --local common block for subroutines */
/* >> */
/* ****************************************** */
/* >> */
/* **************************************************** */
/* j*      double precision angcod */
/* j*      common /angblk/ angcod(99) */
/* >  only angcod is needed, but we include all for f77 standard. */
/* > */
/* ****************************************** */
/* ****************************************** */
/* > */
/* **************************************************** */
/* -- */
    /* Parameter adjustments */
    kodx -= 3;
    --argx;
    kodv -= 3;
    --argv;
    kodz -= 3;
    --argz;

    /* Function Body */
    for (kl = 1; kl <= nmx; ++kl) {
		if (kodx[(kl << 1) + 1] != 0 || kodx[(kl << 1) + 2] != 0) {
			unkod (&kodx[(kl << 1) + 1], kod, &kmin);
			for (km = kmin; km <= 8; ++km) {
				if (kod[km - 1] >= 86 && kod[km - 1] <= 92) {
					argx[kl] = 0.;
					for (km1 = kmin; km1 <= 8; ++km1) {
						kmz = kod[km1 - 1];
						argx[kl] += angblk_1.angcod[kmz - 1];
					}
					argx[kl] = d_mod(argx[kl], TWOPI);
				}
		    }
		}
    }

    for (kl = 1; kl <= nmv; ++kl) {
		if (kodv[(kl << 1) + 1] != 0 || kodv[(kl << 1) + 2] != 0) {
			unkod (&kodv[(kl << 1) + 1], kod, &kmin);
			for (km = kmin; km <= 8; ++km) {
				if (kod[km - 1] >= 86 && kod[km - 1] <= 92) {
					argv[kl] = 0.;
					for (km1 = kmin; km1 <= 8; ++km1) {
						kmz = kod[km1 - 1];
						argv[kl] += angblk_1.angcod[kmz - 1];
					}
					argv[kl] = d_mod(argv[kl], TWOPI);
				}
		    }
		}
    }

    for (kl = 1; kl <= nmz; ++kl) {
		if (kodz[(kl << 1) + 1] != 0 || kodz[(kl << 1) + 2] != 0) {
			unkod (&kodz[(kl << 1) + 1], kod, &kmin);
			for (km = kmin; km <= 8; ++km) {
				if (kod[km - 1] >= 86 && kod[km - 1] <= 92) {
					argz[kl] = 0.;
					for (km1 = kmin; km1 <= 8; ++km1) {
						kmz = kod[km1 - 1];
						argz[kl] += angblk_1.angcod[kmz - 1];
					}
					argz[kl] = d_mod(argz[kl], TWOPI);
				}
		    }
		}
    }
}

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = */
void unkod (int *kode, int *kod, int *kmin)
{
    int kb, kx;

    kod[0] = -kode[0] / 1000000;
    kx = -kode[0] - kod[0] * 1000000;
    kod[1] = kx / 10000;
    kx -= kod[1] * 10000;
    kod[2] = kx / 100;
    kod[3] = kx - kod[2] * 100;
    kod[4] = -kode[1] / 1000000;
    kx = -kode[1] - kod[4] * 1000000;
    kod[5] = kx / 10000;
    kx -= kod[5] * 10000;
    kod[6] = kx / 100;
    kod[7] = kx - kod[6] * 100;
    for (kb = 0; kb < 8; ++kb) {
		if (kod[kb] > 0) break;
    }
    *kmin = kb + 3;
}


/* = = = = = = = = = = = = = = = = = = = = = = = = = = = */
/* = = = = = = = = = = = = = = = = = = = = = = = = = = = */
int cd2com (const char *fname)
{
/* ********************************************** */
/* read the 'card' file (output as punch file from kodlod or the */
/* first 719 cards of kodlop) and fill the necessary theory */
/* common blocks /ebblok/ /trmblk/ /angblk/ /theory/ */

    int i;
    FILE *fdat = fopen(fname, "rt");

    if (fdat) {
        /* read ebblok */
        for (i = 0; i < 28; i++) fscanf(fdat, "%lf", ebblok_1.earay + i);
        for (i = 0; i < 22; i++) fscanf(fdat, "%lf", ebblok_1.baray + i);
        for (i = 0; i < 28; i++) fscanf(fdat, "%lf", ebblok_1.paray + i);

        /* read trmblk */
        for (i = 0; i < 53; i++) fscanf(fdat, "%lf", trmblk_1.trmcod + i);

        /* read angblk */
        for (i = 0; i < 99; i++) fscanf(fdat, "%lf", angblk_1.angcod + i);
        for (i = 0; i < 99; i++) fscanf(fdat, "%lf", angblk_1.ratcod + i);
        for (i = 0; i < 22; i++) fscanf(fdat, "%lf", angblk_1.ang + i);
        for (i = 0; i < 23; i++) fscanf(fdat, "%lf", angblk_1.rat + i);

        /* read theory */
        for (i = 0; i < 4; i++) fscanf(fdat, "%lf", theory_1.axis + i);
        for (i = 0; i < 10; i++) fscanf(fdat, "%lf", theory_1.cxi1 + i);
        for (i = 0; i < 10; i++) fscanf(fdat, "%lf", theory_1.argx1 + i);
        for (i = 0; i < 10; i++) fscanf(fdat, "%lf", theory_1.ratx1 + i);
        for (i = 0; i < 7; i++) fscanf(fdat, "%lf", theory_1.cz1 + i);
        for (i = 0; i < 7; i++) fscanf(fdat, "%lf", theory_1.argz1 + i);
        for (i = 0; i < 7; i++) fscanf(fdat, "%lf", theory_1.ratz1 + i);
        for (i = 0; i < 41; i++) fscanf(fdat, "%lf", theory_1.cv1 + i);
        for (i = 0; i < 41; i++) fscanf(fdat, "%lf", theory_1.argv1 + i);
        for (i = 0; i < 41; i++) fscanf(fdat, "%lf", theory_1.ratv1 + i);
        for (i = 0; i < 24; i++) fscanf(fdat, "%lf", theory_1.cxi2 + i);
        for (i = 0; i < 24; i++) fscanf(fdat, "%lf", theory_1.argx2 + i);
        for (i = 0; i < 24; i++) fscanf(fdat, "%lf", theory_1.ratx2 + i);
        for (i = 0; i < 11; i++) fscanf(fdat, "%lf", theory_1.cz2 + i);
        for (i = 0; i < 11; i++) fscanf(fdat, "%lf", theory_1.argz2 + i);
        for (i = 0; i < 11; i++) fscanf(fdat, "%lf", theory_1.ratz2 + i);
        for (i = 0; i < 66; i++) fscanf(fdat, "%lf", theory_1.cv2 + i);
        for (i = 0; i < 66; i++) fscanf(fdat, "%lf", theory_1.argv2 + i);
        for (i = 0; i < 66; i++) fscanf(fdat, "%lf", theory_1.ratv2 + i);
        for (i = 0; i < 31; i++) fscanf(fdat, "%lf", theory_1.cxi3 + i);
        for (i = 0; i < 31; i++) fscanf(fdat, "%lf", theory_1.argx3 + i);
        for (i = 0; i < 31; i++) fscanf(fdat, "%lf", theory_1.ratx3 + i);
        for (i = 0; i < 13; i++) fscanf(fdat, "%lf", theory_1.cz3 + i);
        for (i = 0; i < 13; i++) fscanf(fdat, "%lf", theory_1.argz3 + i);
        for (i = 0; i < 13; i++) fscanf(fdat, "%lf", theory_1.ratz3 + i);
        for (i = 0; i < 75; i++) fscanf(fdat, "%lf", theory_1.cv3 + i);
        for (i = 0; i < 75; i++) fscanf(fdat, "%lf", theory_1.argv3 + i);
        for (i = 0; i < 75; i++) fscanf(fdat, "%lf", theory_1.ratv3 + i);
        for (i = 0; i < 49; i++) fscanf(fdat, "%lf", theory_1.cxi4 + i);
        for (i = 0; i < 49; i++) fscanf(fdat, "%lf", theory_1.argx4 + i);
        for (i = 0; i < 49; i++) fscanf(fdat, "%lf", theory_1.ratx4 + i);
        for (i = 0; i < 18; i++) fscanf(fdat, "%lf", theory_1.cz4 + i);
        for (i = 0; i < 18; i++) fscanf(fdat, "%lf", theory_1.argz4 + i);
        for (i = 0; i < 18; i++) fscanf(fdat, "%lf", theory_1.ratz4 + i);
        for (i = 0; i < 89; i++) fscanf(fdat, "%lf", theory_1.cv4 + i);
        for (i = 0; i < 89; i++) fscanf(fdat, "%lf", theory_1.argv4 + i);
        for (i = 0; i < 89; i++) fscanf(fdat, "%lf", theory_1.ratv4 + i);
        fscanf(fdat, "%lf", &theory_1.epsln);

        fscanf(fdat, "%d", &theory_1.nxi1t);
        fscanf(fdat, "%d", &theory_1.nz1t);
        fscanf(fdat, "%d", &theory_1.nv1t);
        fscanf(fdat, "%d", &theory_1.nxi2t);
        fscanf(fdat, "%d", &theory_1.nz2t);
        fscanf(fdat, "%d", &theory_1.nv2t);
        fscanf(fdat, "%d", &theory_1.nxi3t);
        fscanf(fdat, "%d", &theory_1.nz3t);
        fscanf(fdat, "%d", &theory_1.nv3t);
        fscanf(fdat, "%d", &theory_1.nxi4t);
        fscanf(fdat, "%d", &theory_1.nz4t);
        fscanf(fdat, "%d", &theory_1.nv4t);
        for (i = 0; i < 20; i++) fscanf(fdat, "%d", theory_1.kodx1 + i);
        for (i = 0; i < 14; i++) fscanf(fdat, "%d", theory_1.kodz1 + i);
        for (i = 0; i < 82; i++) fscanf(fdat, "%d", theory_1.kodv1 + i);
        for (i = 0; i < 48; i++) fscanf(fdat, "%d", theory_1.kodx2 + i);
        for (i = 0; i < 22; i++) fscanf(fdat, "%d", theory_1.kodz2 + i);
        for (i = 0; i < 132; i++) fscanf(fdat, "%d", theory_1.kodv2 + i);
        for (i = 0; i < 62; i++) fscanf(fdat, "%d", theory_1.kodx3 + i);
        for (i = 0; i < 26; i++) fscanf(fdat, "%d", theory_1.kodz3 + i);
        for (i = 0; i < 150; i++) fscanf(fdat, "%d", theory_1.kodv3 + i);
        for (i = 0; i < 98; i++) fscanf(fdat, "%d", theory_1.kodx4 + i);
        for (i = 0; i < 36; i++) fscanf(fdat, "%d", theory_1.kodz4 + i);
        for (i = 0; i < 178; i++) fscanf(fdat, "%d", theory_1.kodv4 + i);

        fclose(fdat);
        return 0;
    }
    return -1;
}

// =================================================================
// MS: Functions no longer referenced

#ifdef UNDEF

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = */
int mult3g_(double *a, double *b, double *c__)
{
    static int i__, j, k;
    static double sum;

/* **************************************************** */
/* -- */
    /* Parameter adjustments */
    c__ -= 4;
    b -= 4;
    a -= 4;

    /* Function Body */
    for (i__ = 1; i__ <= 3; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    sum = 0.;
	    for (k = 1; k <= 3; ++k) {
/* L1: */
		sum += a[i__ + k * 3] * b[k + j * 3];
	    }
/* L2: */
	    c__[i__ + j * 3] = sum;
	}
    }
    return 0;
} /* mult3g_ */

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = */
int rotg_(int *id, double *arg, double *a)
{
    /* Local variables */
    static int j, k;

/* **************************************************** */
/* --these are set up for negative rotations */
/* j*      implicit none */
/*  set up rotation matrix for x,y,z rotations p=x=1, q=y=2, r=z=3 */
/*      id    11    12    13    21    22    23    31    32    33 */
/*  px  1     1     0     0     0     c    -s     0     s     c */
/*  qy  2     c     0     s     0     1     0    -s     0     c */
/*  rz  3     c    -s    0      s     c     0     0     0     1 */
/* -- */
    /* Parameter adjustments */
    a -= 4;

    /* Function Body */
    a[*id + *id * 3] = 1.;
    j = *id - 1;
    k = *id + 1;
    if (*id == 1) {
	j = 3;
    } else if (*id == 3) {
	k = 1;
    }
    a[j + j * 3] = cos(*arg);
    a[k + k * 3] = a[j + j * 3];
    a[j + k * 3] = sin(*arg);
    a[k + j * 3] = -a[j + k * 3];
    a[j + *id * 3] = 0.;
    a[*id + j * 3] = 0.;
    a[k + *id * 3] = 0.;
    a[*id + k * 3] = 0.;
    return 0;
} /* rotg_ */

#endif