/* eqncpv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b13 = 1.;

/* $Procedure EQNCPV (Equinoctial Elements to position and velocity) */
/* Subroutine */ int eqncpv_(doublereal *et, doublereal *epoch, doublereal *
	eqel, doublereal *rapol, doublereal *decpol, doublereal *state)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), sin(doublereal), cos(doublereal), d_mod(
	    doublereal *, doublereal *);

    /* Local variables */
    doublereal nfac, node, mldt, temp[3], a, b, h__, k, l, eecan, p, q, r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal dlpdt, prate;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal xhold[6];
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    doublereal trans[9]	/* was [3][3] */;
    extern doublereal twopi_(void);
    doublereal x1, y1;
    extern /* Subroutine */ int vlcom3_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    doublereal ca, cd, cf, di, cn, ra, sa, rb, sd, dt, sf, ml, dx, dy, vf[3], 
	    vg[3], sn, nodedt;
    extern doublereal kepleq_(doublereal *, doublereal *, doublereal *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    static doublereal pi2;
    doublereal dx1, dy1;
    extern logical return_(void);
    doublereal ecc, can, dlp, san;
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Compute the state (position and velocity) of an object whose */
/*     trajectory is described via equinoctial elements relative to some */
/*     fixed plane (usually the equatorial plane of some planet). */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     SPK */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Epoch in seconds past J2000 to find state */
/*     EPOCH      I   Epoch of elements in seconds past J2000 */
/*     EQEL       I   Array of equinoctial elements */
/*     RAPOL      I   Right Ascension of the pole of the reference plane */
/*     DECPOL     I   Declination of the pole of the reference plane */
/*     STATE      O   State of the object described by EQEL. */

/* $ Detailed_Input */

/*     ET       is the epoch (ephemeris time) at which the state */
/*              of the target body is to be computed. ET is measured */
/*              in seconds past the J2000 epoch. */

/*     EPOCH    is the epoch of the equinoctial elements in seconds */
/*              past the J2000 epoch. */

/*     EQEL     is an array of 9 double precision numbers that are the */
/*              equinoctial elements for some orbit expressed relative to */
/*              the equatorial frame of the central body defined as */

/*              -  The Z-axis of the equatorial frame is the direction */
/*                 of the pole of the central body relative to some */
/*                 inertial frame; */

/*              -  The X-axis is given by the cross product of the Z-axis */
/*                 of the inertial frame with the direction of the pole */
/*                 of the central body; and */

/*              -  The Y-axis completes a right handed frame. */

/*              If the X-axis of the equatorial frame is aligned with the */
/*              X-axis of the inertial frame, then the X-axis of the */
/*              equatorial frame will be located at 90 degrees + RAPOL in */
/*              the inertial frame. */

/*              The specific arrangement of the elements is spelled out */
/*              below: */

/*                 EQEL(1)   is the semi-major axis (A) of the orbit in */
/*                           km. */

/*                 EQEL(2)   is the value of H at the specified epoch. */
/*                           ( E*SIN(ARGP+NODE) ). */

/*                 EQEL(3)   is the value of K at the specified epoch */
/*                           ( E*COS(ARGP+NODE) ). */

/*                 EQEL(4)   is the mean longitude (MEAN0+ARGP+NODE) at */
/*                           the epoch of the elements measured in */
/*                           radians. */

/*                 EQEL(5)   is the value of P (TAN(INC/2)*SIN(NODE)) at */
/*                           the specified epoch. */

/*                 EQEL(6)   is the value of Q (TAN(INC/2)*COS(NODE)) at */
/*                           the specified epoch. */

/*                 EQEL(7)   is the rate of the longitude of periapse */
/*                           (dARGP/dt + dNODE/dt ) at the epoch of */
/*                           the elements. This rate is assumed to hold */
/*                           for all time. The rate is measured in */
/*                           radians per second. */

/*                 EQEL(8)   is the derivative of the mean longitude */
/*                           ( dM/dt + dARGP/dt + dNODE/dt ). This */
/*                           rate is assumed to be constant and is */
/*                           measured in radians/second. */

/*                 EQEL(9)   is the rate of the longitude of the */
/*                           ascending node ( dNODE/dt). This rate is */
/*                           measured in radians per second. */

/*              where */

/*                 INC       is the inclination of the orbit, */

/*                 ARGP      is the argument of periapse, */

/*                 NODE      is longitude of the ascending node, and */

/*                 E         is eccentricity of the orbit. */

/*     RAPOL    is the Right Ascension of the pole of the reference plane */
/*              with respect to some inertial frame (measured in */
/*              radians). */

/*     DECPOL   is the Declination of the pole of the reference plane */
/*              with respect to some inertial frame (measured in */
/*              radians). */

/* $ Detailed_Output */

/*     STATE    is the state of the object described by EQEL relative to */
/*              the inertial frame used to define RAPOL and DECPOL. Units */
/*              are in km and km/sec. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the eccentricity corresponding to the input elements is */
/*         greater than 0.9, the error SPICE(ECCOUTOFRANGE) is signaled. */

/*     2)  If the semi-major axis of the elements is non-positive, the */
/*         error SPICE(BADSEMIAXIS) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine evaluates the input equinoctial elements for */
/*     the specified epoch and return the corresponding state. */

/*     This routine was adapted from a routine provided by */
/*     Bob Jacobson of the Planetary Dynamics Group of */
/*     the Navigation and Flight Mechanics Section at JPL. */

/* $ Examples */

/*     Suppose you have classical elements and rates of */
/*     change of the ascending node and argument of periapse */
/*     for some satellite of the earth. */

/*     By transforming the classical elements */
/*     this routine can be used to compute the state of the */
/*     object at an arbitrary epoch. The code below illustrates */
/*     how you might do this. */

/*     The table below illustrates the meanings of the various */
/*     variables used in the discussion below. */

/*           Variable     Meaning */
/*           --------     ---------------------------------- */
/*           A            Semi-major axis in km */
/*           ECC          Eccentricity of orbit */
/*           INC          Inclination of orbit */
/*           NODE         Longitude of the ascending node at epoch */
/*           OMEGA        Argument of periapse at epoch */
/*           M            Mean anomaly at epoch */
/*           DMDT         Mean anomaly rate in radians/second */
/*           DNODE        Rate of change of longitude of ascending node */
/*                        in radians/second */
/*           DARGP        Rate of change of argument of periapse in */
/*                        radians/second */
/*           EPOCH        is the epoch of the elements in seconds past */
/*                        the J2000 epoch. */


/*        EQEL(1) = A */
/*        EQEL(2) = ECC * DSIN ( OMEGA + NODE ) */
/*        EQEL(3) = ECC * DCOS ( OMEGA + NODE ) */

/*        EQEL(4) = M + OMEGA + NODE */

/*        EQEL(5) = TAN(INC/2.0D0) * DSIN(NODE) */
/*        EQEL(6) = TAN(INC/2.0D0) * DCOS(NODE) */

/*        EQEL(7) = DARGP */
/*        EQEL(8) = DARGP + DMDT + DNODE */
/*        EQEL(9) = DNODE */


/*        We shall compute the state of the satellite in the */
/*        pole and equator reference system. */

/*        RAPOL   = -HALFPI() */
/*        DECPOL  =  HALFPI() */


/*        Now compute the state at the desired epoch ET. */

/*        CALL EQNCPV ( ET, EPOCH, EQEL, RAPOL, DECPOL, STATE ) */

/* $ Restrictions */

/*     1)  The equinoctial elements used by this routine are taken */
/*         from  "Tangent" formulation of equinoctial elements */

/*            p = tan(inclination/2) * sin(R.A. of ascending node) */
/*            q = tan(inclination/2) * cos(R.A. of ascending node) */

/*         Other formulations use Sine instead of Tangent. We shall */
/*         call these the "Sine" formulations. */

/*            p = sin(inclination/2) * sin(R.A. of ascending node) */
/*            q = sin(inclination/2) * cos(R.A. of ascending node) */

/*         If you have equinoctial elements from this alternative */
/*         formulation you should replace p and q  by the */
/*         expressions below. */

/*            P = P / DSQRT ( 1.0D0 - P*P - Q*Q ) */
/*            Q = Q / DSQRT ( 1.0D0 - P*P - Q*Q ) */

/*         This will convert the Sine formulation to the Tangent */
/*         formulation. */

/* $ Literature_References */

/*     [1]  W. Owen and R. Vaughan, "Optical Navigation Program */
/*          Mathematical Models," JPL Engineering Memorandum 314-513, */
/*          August 9, 1991. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     R.A. Jacobson      (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.3, 14-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added SPK */
/*        required reading. */

/* -    SPICELIB Version 1.0.2, 18-MAY-2010 (BVS) */

/*        Removed "C$" marker from text in the header. */

/* -    SPICELIB Version 1.0.1, 31-JAN-2008 (BVS) */

/*        Removed non-standard header section heading */
/*        'Declarations_of_external_functions'. */

/* -    SPICELIB Version 1.0.0, 08-JAN-1997 (WLT) (RAJ) */

/* -& */
/* $ Index_Entries */

/*     Compute a state from equinoctial elements */

/* -& */

/*     SPICELIB Functions. */


/*     LOCAL VARIABLES */


/*     Constants computed on first pass */


/*     Standard SPICE exception handling code. */

    if (return_()) {
	return 0;
    }
    chkin_("EQNCPV", (ftnlen)6);

/*     The first time through this routine we fetch the various */
/*     constants we need for this routine. */

    if (first) {
	first = FALSE_;
	pi2 = twopi_();
    }

/*     Take care of the various errors that can arise with the */
/*     input elements. */

    if (eqel[0] <= 0.) {
	setmsg_("The semi-major axis supplied to EQNCPV was non-positive. Th"
		"e value is required to be positive by this routine. The valu"
		"e supplied was #. ", (ftnlen)137);
	errdp_("#", eqel, (ftnlen)1);
	sigerr_("SPICE(BADSEMIAXIS)", (ftnlen)18);
	chkout_("EQNCPV", (ftnlen)6);
	return 0;
    }
    ecc = sqrt(eqel[1] * eqel[1] + eqel[2] * eqel[2]);
    if (ecc > .9) {
	setmsg_("The routine EQNCPV can reliably evaluate states from equino"
		"ctial elements if the eccentricity of the orbit associated w"
		"ith the elements is less than 0.9.  The eccentricity associa"
		"ted with the elements supplies is #.  The values of H and K "
		"are: # and # respectively. ", (ftnlen)266);
	errdp_("#", &ecc, (ftnlen)1);
	errdp_("#", &eqel[1], (ftnlen)1);
	errdp_("#", &eqel[2], (ftnlen)1);
	sigerr_("SPICE(ECCOUTOFRANGE)", (ftnlen)20);
	chkout_("EQNCPV", (ftnlen)6);
	return 0;
    }

/*     Form the transformation from planetary equator to the inertial */
/*     reference frame. */

    sa = sin(*rapol);
    ca = cos(*rapol);
    sd = sin(*decpol);
    cd = cos(*decpol);
    trans[0] = -sa;
    trans[3] = -ca * sd;
    trans[6] = ca * cd;
    trans[1] = ca;
    trans[4] = -sa * sd;
    trans[7] = sa * cd;
    trans[2] = 0.;
    trans[5] = cd;
    trans[8] = sd;

/*     Compute the offset of the input epoch (ET) from the */
/*     epoch of the elements. */

    dt = *et - *epoch;

/*     Obtain the elements, rates, and other parameters. First get */
/*     the semi-major axis. */

    a = eqel[0];

/*     Recall that H and K at the epoch of the elements are in */
/*     EQEL(2) and EQEL(3) respectively. */

/*        H_0 = E*Sin(ARGP_0 + NODE_0 ) */
/*        K_0 = E*Cos(ARGP_0 + NODE_0 ) */

/*     The values of H and K at the epoch of interest is */

/*        H_dt = E*Sin(ARGP_0 + NODE_0 + dt*d(ARGP+NODE)/dt ) */
/*        K_dt = E*Cos(ARGP_0 + NODE_0 + dt*d(ARGP+NODE)/dt ) */

/*     But using the identities Sin(A+B) = Sin(A)Cos(B) + Sin(B)Cos(A) */
/*                              Cos(A+B) = Cos(A)Cos(B) - Sin(A)Sin(B) */

/*     We can re-write the expression for H_dt and K_dt as */

/*        H_dt = E*Sin(ARGP_0 + NODE_0 )Cos(dt*d(ARGP+NODE)/dt ) */
/*             + E*Cos(ARGP_0 + NODE_0 )Sin(dt*d(ARGP+NODE)/dt ) */


/*             = H_0 * Cos(dt*d(ARGP+NODE)/dt ) */
/*             + K_0 * Sin(dt*d(ARGP+NODE)/dt ) */
/*     and */

/*        K_dt = E*Cos(ARGP_0 + NODE_0)Cos(dt*d(ARGP+NODE)/dt) */
/*             - E*Sin(ARGP_0 + NODE_0)Sin(dt*d(ARGP+NODE)/dt) */

/*             = K_0 * Cos(dt*d(ARGP+NODE)/dt) */
/*             - H_0 * Sin(dt*d(ARGP+NODE)/dt) */

/*     Thus we can easily compute H and K at the current epoch. */
/*     Recall that the derivative of the longitude of periapse is */
/*     in entry 7 of EQEL. */

    dlpdt = eqel[6];
    dlp = dt * dlpdt;
    can = cos(dlp);
    san = sin(dlp);
    h__ = eqel[1] * can + eqel[2] * san;
    k = eqel[2] * can - eqel[1] * san;

/*     The mean longitude at epoch is in the 4th element of EQEL. */

    l = eqel[3];

/*     The values for P and Q at epoch are stored in entries 5 and 6 */
/*     of the array EQEL.  Recall that */

/*        P_0 = TAN(INC/2)*SIN(NODE_0) */
/*        Q_0 = TAN(INC/2)*COS(NODE_0) */

/*     We need P and Q offset from the initial epoch by DT. */

/*        P   = TAN(INC/2)*SIN(NODE_0 + dt*dNODE/dt) */
/*        Q   = TAN(INC/2)*COS(NODE_0 + dt*dNODE/dt) */

/*     Applying the same identities as we did before we have */

/*        P    = P_0 * Cos( dt*dNODE/dt ) + Q_0 * Sin( dt*dNODE/dt ) */
/*        Q    = Q_0 * Cos( dt*dNODE/dt ) - P_0 * Sin( dt*dNODE/dt ) */

    nodedt = eqel[8];
    node = dt * nodedt;
    cn = cos(node);
    sn = sin(node);
    p = eqel[4] * cn + eqel[5] * sn;
    q = eqel[5] * cn - eqel[4] * sn;
    mldt = eqel[7];

/*     We compute the rate of change of the argument of periapse */
/*     by taking the difference between the rate of the longitude */
/*     of periapse and the rate of the node. */

    prate = dlpdt - nodedt;

/*     Form Broucke's beta parameter */

    b = sqrt(1. - h__ * h__ - k * k);
    b = 1. / (b + 1.);

/*     Construct the coordinate axes */

    di = 1. / (p * p + 1. + q * q);
    vf[0] = (1. - p * p + q * q) * di;
    vf[1] = p * 2. * q * di;
    vf[2] = p * -2. * di;
    vg[0] = p * 2. * q * di;
    vg[1] = (p * p + 1. - q * q) * di;
    vg[2] = q * 2. * di;

/*     Compute the mean longitude */

    d__1 = mldt * dt;
    ml = l + d_mod(&d__1, &pi2);

/*     Obtain the eccentric longitude from Kepler's equation */

    eecan = kepleq_(&ml, &h__, &k);

/*     Trigonometric functions of the eccentric longitude */

    sf = sin(eecan);
    cf = cos(eecan);

/*     Position in the orbit plane */

/* Computing 2nd power */
    d__1 = h__;
    x1 = a * ((1. - b * (d__1 * d__1)) * cf + (h__ * k * b * sf - k));
/* Computing 2nd power */
    d__1 = k;
    y1 = a * ((1. - b * (d__1 * d__1)) * sf + (h__ * k * b * cf - h__));

/*     Radial distance and functions of the radial distance */

    rb = h__ * sf + k * cf;
    r__ = a * (1. - rb);
    ra = mldt * a * a / r__;


/*     Velocity in the orbit plane */

    dx1 = ra * (-sf + h__ * b * rb);
    dy1 = ra * (cf - k * b * rb);

/*     Correction factor for periapsis rate */

    nfac = 1. - dlpdt / mldt;

/*     Include precession in velocity */

    dx = nfac * dx1 - prate * y1;
    dy = nfac * dy1 + prate * x1;

/*     Form the planetary mean equator position vector */

    vlcom_(&x1, vf, &y1, vg, xhold);

/*     Form the planetary mean equator velocity vector */

    temp[0] = -nodedt * xhold[1];
    temp[1] = nodedt * xhold[0];
    temp[2] = 0.;
    vlcom3_(&c_b13, temp, &dx, vf, &dy, vg, &xhold[3]);

/*     Transform to an inertial state vector */

    mxv_(trans, xhold, state);
    mxv_(trans, &xhold[3], &state[3]);
    chkout_("EQNCPV", (ftnlen)6);
    return 0;
} /* eqncpv_ */

