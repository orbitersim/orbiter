/* zznrddp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZNRDDP ( Shell for deep space entry points ) */
/* Subroutine */ int zznrddp_0_(int n__, doublereal *ao, doublereal *elems, 
	doublereal *em, doublereal *omgasm, doublereal *omgdot, doublereal *t,
	 doublereal *xinc, doublereal *xll, doublereal *xlldot, doublereal *
	xn, doublereal *xnodes, doublereal *xnodot, doublereal *xnodp)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), sin(doublereal), cos(doublereal), d_mod(
	    doublereal *, doublereal *), atan2(doublereal, doublereal);

    /* Local variables */
    static doublereal ctem, delt, pinc, sghl;
    static logical cont;
    static doublereal sghs, aqnv, cosq, temp, stem, eqsq, sinq, thgr, xmao, 
	    xnoi, zmol, zmos, pinc0, ainv2, sini2, temp1, cosq2, c__;
    extern /* Subroutine */ int zzsecprt_(integer *, doublereal *, doublereal 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    static integer i__;
    static doublereal bfact, alfdp, jdtdb;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal betdp, atime, theta, xfact, preep, jdut50, xincl, cosiq,
	     cosok, cosis, xlamo, a1, a2, a3, a4, a5, a6, a7, a8, a9, e3, f2, 
	    f3, siniq, sinis, sinok, sinzf, stepn, s1, s2, s3, s4, s5, s6, s7,
	     stepp, x1, x2, x3, x4, x5, x6, x7, x8, xldot, xnddt, xndot, 
	    xqncl, z1, z2, z3, zcosg, zcosh, zcosi, zsing, zsinh, zsini;
    extern doublereal twopi_(void);
    static doublereal a10, cc, dg[10], eo, pe, eq, ph, et, ft, se, pl, sh, si,
	     sl, z11, z12, z13, z21, xl, z22, omegao, z23, z31, z32, z33, ze, 
	    zf, zm, zn, xnodce;
    extern doublereal pi_(void);
    static doublereal zx, zy;
    static integer iresfl;
    static doublereal f220, f221, ee2, f311, f321, cosomo, f322, f330, f441, 
	    f442, f522, f523, f542, f543, g200, g201, g211, g300, g310, g322, 
	    g410, g422, g520, g521, g532, g533, oxnode, pe0, ph0, pl0, rteqsq,
	     se2, se3, sh2, sh3, si2, si3, sinomo, sl2, sl3, sl4, xh2, xh3, 
	    xi2, xi3, xl2, xl3, xl4, xnodeo, zcosgl, zcoshl, zcosil, zsingl, 
	    zsinhl, zsinil;
    static integer isynfl;
    static doublereal gam, del[3], eoc;
    extern doublereal j2000_(void), j1950_(void);
    extern logical return_(void);
    static doublereal ds50, day, pgh, sgh, sel, bsq, shl, sil;
    extern doublereal spd_(void);
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static doublereal ses, sll, xli, shs, sis, xni, sls, xmo, xls, xnq;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    static doublereal ssx[5], pgh0, sgh2, sgh3, sgh4, xgh2, xgh3, xgh4, pix1, 
	    pix2, xno2;

/* $ Abstract */

/*    This subroutine is a shell for the routines needed by DPSPCE */
/*    for calculating deep space effects on a vehicle. */

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

/*     None. */

/* $ Keywords */

/*     TWO LINE ELEMENTS */
/*     SPACETRACK */
/*     DEEP SPACE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     AO         I   Entry ZZDPINIT, Original semimajor axis */
/*     ELEMS      I   Entry ZZDPINIT, Array of orbit elements */
/*                      "   ZZDPSEC */
/*     EM         I   Entry ZZDPSEC,  Perturbed eccentricity of the orbit */
/*                      "   ZZDPPER   at time T */
/*     OMGASM     I   Entry ZZDPSEC   Perturbed argument of perigee */
/*                      "   ZZDPPER */
/*     OMGDOT     I   Entry ZZDPINIT, Time rate of change of arg of */
/*                      "   ZZDPSEC   perigee */
/*     T          I   Entry ZZDPSEC,  Time of state evaluation */
/*                      "   ZZDPPER */
/*     XINC       I   Entry ZZDPSEC,  Perturbed inclination of the orbit */
/*                      "   ZZDPPER   plane at time T */
/*     XLL        I   Entry ZZDPSEC   Long-period periodic term */
/*     XLLDOT     I   Entry ZZDPINIT, Time rate of change of XLL */
/*     XN         I   Entry ZZDPSEC   Perturbed mean motion of the orbit */
/*                                    at time T */
/*     XNODES     I   Entry ZZDPSEC,  Perturbed argument of ascending */
/*                      "   ZZDPPER   node */
/*     XNODOT     I   Entry ZZDPINIT, Time rate of change of mean motion */
/*     XNODP      I   Entry ZZDPINIT, Original mean motion */

/* $ Detailed_Input */

/*     AO          the original semimajor axis of the orbit. */

/*     ELEMS       is an array containing two-line element data */
/*                 as prescribed below. The elements XNDD6O and BSTAR */
/*                 must already be scaled by the proper exponent stored */
/*                 in the two line elements set.  Moreover, the */
/*                 various items must be converted to the units shown */
/*                 here. */

/*                    ELEMS (  1 ) = XNDT2O in radians/minute**2 */
/*                    ELEMS (  2 ) = XNDD6O in radians/minute**3 */
/*                    ELEMS (  3 ) = BSTAR */
/*                    ELEMS (  4 ) = XINCL  in radians */
/*                    ELEMS (  5 ) = XNODEO in radians */
/*                    ELEMS (  6 ) = EO */
/*                    ELEMS (  7 ) = OMEGAO in radians */
/*                    ELEMS (  8 ) = XMO    in radians */
/*                    ELEMS (  9 ) = XNO    in radians/minute */
/*                    ELEMS ( 10 ) = EPOCH of the elements in seconds */
/*                                   past ephemeris epoch J2000. */

/*     EM          is the perturbed eccentricity from the mean */
/*                 eccentricity at epoch at time T. */

/*     OMGASM      the value of the argument of perigee after the */
/*                 perturbations at the time of interest are */
/*                 added */

/*     OMGDOT      the time derivative of the argument of perigee */

/*     T           is the total time from the epoch, in minutes, of the */
/*                 element set at which to calculate the state. */

/*     XINC        is the perturbed inclination of the orbit plane from */
/*                 the mean inclination at the epoch at time T */

/*     XLL         a long-period periodic term dependent on inclination, */
/*                 eccentricity and argument of periapsis */

/*     XLLDOT      the time derivative of the XLL long-period term */

/*     XN          is the perturbed mean motion from the 'mean' mean */
/*                 motion at epoch at time T. */

/*     XNODES      is the value of the argument of the ascending node */
/*                 after the perturbations at the time of interest are */
/*                 added. */

/*     XNODOT      the time derivative of the mean motion */

/*     XNODP       original mean motion of the orbit. */

/* $ Detailed_Output */

/*     None */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This subroutine is a shell for the entry points used by the */
/*     propagator for deep space orbits, where a deep space orbit is one */
/*     which has a period greater the 225 minutes.  The entry points */
/*     are */

/*     ZZDPINIT - initialize variables for the deep space regime */
/*     ZZDPSEC  - calculates and updates secular perturbation terms */
/*     ZZDPPER  - calculates and updates periodic perturbation terms */
/*                particularly as caused by the sun and the moon */

/*     The names of several constants defined in the Spacetrack 3 report */
/*     have been changed. */

/*     D2201 to DG( 1  ) */
/*     D2211 to DG( 2  ) */
/*     D3210 to DG( 3  ) */
/*     D3222 to DG( 4  ) */
/*     D4410 to DG( 5  ) */
/*     D4422 to DG( 6  ) */
/*     D5220 to DG( 7  ) */
/*     D5232 to DG( 8  ) */
/*     D5421 to DG( 9  ) */
/*     D5433 to DG( 10 ) */

/*     The names of variables changed from the Spacetrack 3 report */

/*     DEL1  to  DEL( 1 ) */
/*     DEL2  to  DEL( 2 ) */
/*     DEL3  to  DEL( 3 ) */
/*     SSL   to  SSX( 1 ) */
/*     SSG   to  SSX( 2 ) */
/*     SSH   to  SSX( 3 ) */
/*     SSE   to  SSX( 4 ) */
/*     SSI   to  SSX( 5 ) */
/*     OMGDT to  OMGDOT */

/*     The entry point ZZDPPER was modified to insure that the */
/*     perturbations on the elements are zero at the epoch.  This was */
/*     not correctly handled in the Spacetrack 3 report. */

/* $ Examples */

/*     Never call this subroutine directly. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models */
/*     for Propagation of NORAD Element Sets". United States Department */
/*     of Defense Spacetrack Report (3). */

/*     Vallado, David A., Paul Crawford, Richard Hujsak, and */
/*     Kelso, T. S., "Revisiting Spacetrack Report #3," AIAA/AAS */
/*     Astrodynamics Specialist Conference, Keystone, CO, Aug 2006. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.2.0, 14-OCT-2021 (NJB) */

/*        Bug fix: fixed routine name in CHKIN/CHKOUT calls (NRDDP */
/*        -> ZZNRDDP). */

/*        Fixed typos in comments. */

/* -    SPICELIB Version 2.1.0, 03-JUL-2016 (EDW) */

/*        The "Revisiting Spacetrack Report #3" by Vallado et. al. */
/*        indicates the negative inclination modification as a mistake. */
/*        That code was removed. */

/*        Eliminated bug in ZZDPINIT that allowed NaN contamination */
/*        for zero inclination orbits. */

/* -    SPICELIB Version 2.0.0, 20-JAN-2012 (EDW) */

/*        Eliminated use of the DOPERT boolean in ZZDPINIT. */
/*        Refer to that entry point "Version" section for details. */

/*        Added proper citation for Spacetrack 3 (Hoots) and */
/*        Revisiting Spacetrak 3 (Vallado). */

/*        Added proper Declarations section. */

/* -    SPICELIB Version 1.5.1, 19-SEP-2006 (EDW) */

/*        Added text to previously empty Declarations section */
/*        in ZZDPINIT, ZZDPPER, ZZDPSEC. */

/* -    SPICELIB Version 1.5.0, 20-JAN-1999 (EDW) (WLT) */

/*        OMGDOT, named in an ENTRY point argument list */
/*        was not passed via an argument list. Solaris exhibited a */
/*        bus error because of this situation. All ENTRY point */
/*        arguments are passed only by argument lists and are declared */
/*        in the umbrella subroutine's, ZZNRDDP, argument list. */

/*        Combined the various SSL, SSG, SSH, SSI, SSE variables into */
/*        the vector SSX. */

/*        Removed the dependency upon the UTC/ET leapsecond kernel. */

/*        Alphabetized all variable declaration lists. */

/*        All arguments passed through entry points listed as arguments */
/*        of ZZNRDDP. OMGDT renamed OMGDOT to be consistent with other */
/*        deep space two line element routines. */

/* -    SPICELIB Version 1.0.0, 1-APR-1997 (EDW) */

/* -& */
/* $ Index_Entries */

/*     two line element set */

/* -& */

/*     Local variables */


/*     SPICELIB functions */


/*     Define rather a large number of local parameters. */


/*     Save everything just to be sure. */


/*     Standard SPICE error handling. */

    /* Parameter adjustments */
    if (elems) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzdpinit;
	case 2: goto L_zzdpsec;
	case 3: goto L_zzdpper;
	}

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZNRDDP", (ftnlen)7);
    }

/*     This routine should never be called. If this routine is called, */
/*     an error is signaled. */

    setmsg_("NRDDP: You called an entry which performs no run-time function."
	    " This may indicate a bug. Please check the documentation for the"
	    " subroutine ZZNRDDP.", (ftnlen)147);
    sigerr_("SPICE(EVILBOGUSENTRY)", (ftnlen)21);
    chkout_("ZZNRDDP", (ftnlen)7);
    return 0;
/* $Procedure ZZDPINIT (Initialize deep space algorithm and variables ) */

L_zzdpinit:
/* $ Abstract */

/*     Entrance for deep space initialization.  This section is called */
/*     once per element set. */

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

/*     None. */

/* $ Keywords */

/*     KEYWORD */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     AO         I   Original semimajor axis */
/*     XLLDOT     I   Time rate of change of XLL */
/*     OMGDOT     I   Time rate of change of argument of perigee */
/*     XNODOT     I   Time rate of change of mean motion */
/*     XNODP      I   Original mean motion */
/*     ELEMS      I   Array of orbit elements */

/* $ Detailed_Input */

/*     AO          the original semimajor axis of the orbit. */

/*     XLLDOT      the time derivative of the XLL long-period term */

/*     OMGDOT      the time derivative of the argument of perigee */

/*     XNODOT      the time derivative of the mean motion */

/*     XNODP       original mean motion of the elements */

/*     ELEMS       is an array containing two-line element data */
/*                 as prescribed below. The elements XNDD6O and BSTAR */
/*                 must already be scaled by the proper exponent stored */
/*                 in the two line elements set.  Moreover, the */
/*                 various items must be converted to the units shown */
/*                 here. */

/*                    ELEMS (  1 ) = XNDT2O in radians/minute**2 */
/*                    ELEMS (  2 ) = XNDD6O in radians/minute**3 */
/*                    ELEMS (  3 ) = BSTAR */
/*                    ELEMS (  4 ) = XINCL  in radians */
/*                    ELEMS (  5 ) = XNODEO in radians */
/*                    ELEMS (  6 ) = EO */
/*                    ELEMS (  7 ) = OMEGAO in radians */
/*                    ELEMS (  8 ) = XMO    in radians */
/*                    ELEMS (  9 ) = XNO    in radians/minute */
/*                    ELEMS ( 10 ) = EPOCH of the elements in seconds */
/*                                   past ephemeris epoch J2000. */

/* $ Detailed_Output */

/*     No direct output. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine only initializes non-time dependent variables and */
/*     sets flags concerning whether the orbit is synchronous or */
/*     experiences resonance effects.  It should be called once per */
/*     element set. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1)  This routine should only be called by DPSPCE when propagating */
/*         two line element sets. */

/* $ Literature_References */

/*     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models */
/*     for Propagation of NORAD Element Sets". United States Department */
/*     of Defense Spacetrack Report (3). */

/*     Vallado, David A., Paul Crawford, Richard Hujsak, and */
/*     Kelso, T. S., "Revisiting Spacetrack Report #3," AIAA/AAS */
/*     Astrodynamics Specialist Conference, Keystone, CO, Aug 2006. */

/* $ Author_and_Institution */

/*     E.D. Wright      (JPL) */
/*     W.L. Taber       (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.1, 01-OCT-2021 (NJB) */

/*        Fixed typo in comments. */

/* -    SPICELIB Version 2.1.0, 27-JUN-2016 (EDW) */

/*        Eliminated bug that allowed NaN contamination for zero */
/*        inclination orbits. New code avoids calculating 1/SINIQ for */
/*        SINIQ = 0. */

/* -    SPICELIB Version 2.0.0, 02-MAR-2011 (EDW) */

/*        Eliminated use of the DOPERT boolean. This algorithm used */
/*        that variable to zero-out the perturbations at epoch (T = 0). */
/*        The original code implementation introduced a logic error */
/*        such that the state of DOPERT was correct only for */
/*        runs involving a single vehicle TLE. */

/*        The "Revisiting Spacetrack Report #3" by Vallado et. al. */
/*        indicates the perturbation zeroing is a mistake. That */
/*        operation was removed. */

/* -    SPICELIB Version 1.5.1, 19-SEP-2006 (EDW) */

/*        Added text to previously empty Declarations section */
/*        in ZZDPINIT, ZZDPPER, ZZDPSEC. */

/* -    SPICELIB Version 1.5.0, 20-JAN-1999 (EDW) (WLT) */

/*        OMGDOT, named in an ENTRY point argument list */
/*        was not passed via an argument list.  Solaris exhibited a */
/*        bus error because of this situation.  All ENTRY point */
/*        arguments are passed only by argument lists and are declared */
/*        in the umbrella subroutine's, ZZNRDDP, argument list. */

/*        Combined the various SSL, SSG, SSH, SSI, SSE variables into */
/*        the vector SSX. */

/*        Removed the dependency upon the UTC/ET leapsecond kernel. */

/*        Alphabetized all variable declaration lists. */

/*        All arguments passed through entry points listed as arguments */
/*        of ZZNRDDP.  OMGDT renamed OMGDOT to be consistent with other */
/*        deep space two line element routines. */

/* -    SPICELIB Version 1.0.0, APR-30-1997 (EDW) */


/* -& */
/* $ Index_Entries */

/*     two line elements, deep space, initialize */

/* -& */
    pix1 = pi_();
    pix2 = twopi_();

/*     Unpack the elements array. */

    xincl = elems[3];
    xnodeo = elems[4];
    eo = elems[5];
    omegao = elems[6];
    xmo = elems[7];

/*     Calculate intermediate values */

/* Computing 2nd power */
    d__1 = eo;
    eqsq = d__1 * d__1;
    bsq = 1. - eqsq;
    rteqsq = sqrt(bsq);
    siniq = sin(xincl);
    cosiq = cos(xincl);
/* Computing 2nd power */
    d__1 = cosiq;
    cosq2 = d__1 * d__1;
    sinomo = sin(omegao);
    cosomo = cos(omegao);

/*     This section of code was previously performed by the THETAG */
/*     function.  The epoch of the elements is defined in seconds since */
/*     J2000.  It is necessary to calculate the number of days which have */
/*     elapsed since the Jan 0.0 1950 reference date which is */
/*     Dec 31 1949 00:00:00 UTC ( J1950 - 1 ).  First extract the epoch */
/*     from the ELEMS array and place it in the first entry of a working */
/*     array. */

    et = elems[9];

/*     Convert the ET seconds past the J2000 epoch to the Julian */
/*     date TDB. */

    jdtdb = j2000_() + et / spd_();

/*     How many days since the 1950 reference? Using SPICE standard */
/*     leapseconds the difference between TDB and UTC in 1950 is 32.184 */
/*     seconds.  So we compute JDTDB corresponding to the UTC 1950 */
/*     epoch.  We call this JDTDB epoch ---JDUT50. Then we get the days */
/*     since 1950 by simple arithmetic. */

    jdut50 = j1950_() - 1. + 32.184 / spd_();
    ds50 = jdtdb - jdut50;

/*     What is the Earth's right ascension of the epoch?  We know the */
/*     value at the JD1950-1 reference date, so add the number of radians */
/*     the Earth has rotated through since then.  MOD this value with */
/*     2*PI to get the right ascension for the epoch.  This technique may */
/*     not be the best way to get this value. */

    theta = ds50 * 6.3003880987 + 1.72944494;
    thgr = d_mod(&theta, &pix2);

/*     THGR should have a domain between 0 and 2 Pi. */

    if (thgr < 0.) {
	thgr += pix2;
    }

/*     Set some operation variables. */

    eq = eo;
    xnq = *xnodp;
    aqnv = 1. / *ao;
    xqncl = xincl;
    xmao = xmo;
    sinq = sin(xnodeo);
    cosq = cos(xnodeo);

/*     Initialize lunar solar terms */

    day = ds50 + 18261.5;
    if (day != preep) {
	preep = day;
	xnodce = 4.523602 - day * 9.2422029e-4;
	stem = sin(xnodce);
	ctem = cos(xnodce);
	zcosil = .91375164 - ctem * .03568096;
/* Computing 2nd power */
	d__1 = zcosil;
	zsinil = sqrt(1. - d__1 * d__1);
	zsinhl = stem * .089683511 / zsinil;
/* Computing 2nd power */
	d__1 = zsinhl;
	zcoshl = sqrt(1. - d__1 * d__1);
	c__ = day * .2299715 + 4.7199672;
	gam = day * .001944368 + 5.8351514;
	d__1 = c__ - gam;
	zmol = d_mod(&d__1, &pix2);
	if (zmol < 0.) {
	    zmol += pix2;
	}
	zx = stem * .39785416 / zsinil;
	zy = zcoshl * ctem + zsinhl * .91744867 * stem;

/*        Compute the angle from the x-axis of the point */

	if (zx != 0. || zy != 0.) {
	    zx = atan2(zx, zy);
	    if (zx < 0.) {
		zx += pix2;
	    }
	} else {
	    zx = 0.;
	}
	zx = gam + zx - xnodce;
	zcosgl = cos(zx);
	zsingl = sin(zx);
	zmos = day * .017201977 + 6.2565837;
	zmos = d_mod(&zmos, &pix2);
	if (zmos < 0.) {
	    zmos += pix2;
	}
    }

/*     Do solar terms.  Start with the constant values. */

    zcosg = .1945905;
    zsing = -.98088458;
    zcosi = .91744867;
    zsini = .39785416;
    zcosh = cosq;
    zsinh = sinq;
    cc = 2.9864797e-6;
    zn = 1.19459e-5;
    ze = .01675;
    xnoi = 1. / xnq;

/*     Initialize solar and lunar terms.  The procedure will */
/*     first initialize just the solar, then the lunar, then */
/*     reinitialize the solar with the added lunar effect. */

    for (i__ = 1; i__ <= 2; ++i__) {

/*        Solar. */

	a1 = zcosg * zcosh + zsing * zcosi * zsinh;
	a3 = -zsing * zcosh + zcosg * zcosi * zsinh;
	a7 = -zcosg * zsinh + zsing * zcosi * zcosh;
	a8 = zsing * zsini;
	a9 = zsing * zsinh + zcosg * zcosi * zcosh;
	a10 = zcosg * zsini;
	a2 = cosiq * a7 + siniq * a8;
	a4 = cosiq * a9 + siniq * a10;
	a5 = -siniq * a7 + cosiq * a8;
	a6 = -siniq * a9 + cosiq * a10;
	x1 = a1 * cosomo + a2 * sinomo;
	x2 = a3 * cosomo + a4 * sinomo;
	x3 = -a1 * sinomo + a2 * cosomo;
	x4 = -a3 * sinomo + a4 * cosomo;
	x5 = a5 * sinomo;
	x6 = a6 * sinomo;
	x7 = a5 * cosomo;
	x8 = a6 * cosomo;
/* Computing 2nd power */
	d__1 = x1;
/* Computing 2nd power */
	d__2 = x3;
	z31 = d__1 * d__1 * 12. - d__2 * d__2 * 3.;
	z32 = x1 * 24. * x2 - x3 * 6. * x4;
/* Computing 2nd power */
	d__1 = x2;
/* Computing 2nd power */
	d__2 = x4;
	z33 = d__1 * d__1 * 12. - d__2 * d__2 * 3.;
/* Computing 2nd power */
	d__1 = a1;
/* Computing 2nd power */
	d__2 = a2;
	z1 = (d__1 * d__1 + d__2 * d__2) * 3. + z31 * eqsq;
	z2 = (a1 * a3 + a2 * a4) * 6. + z32 * eqsq;
/* Computing 2nd power */
	d__1 = a3;
/* Computing 2nd power */
	d__2 = a4;
	z3 = (d__1 * d__1 + d__2 * d__2) * 3. + z33 * eqsq;
	z11 = a1 * -6. * a5 + eqsq * (x1 * -24. * x7 - x3 * 6. * x5);
	z12 = (a1 * a6 + a3 * a5) * -6. + eqsq * ((x2 * x7 + x1 * x8) * -24. 
		- (x3 * x6 + x4 * x5) * 6.);
	z13 = a3 * -6. * a6 + eqsq * (x2 * -24. * x8 - x4 * 6. * x6);
	z21 = a2 * 6. * a5 + eqsq * (x1 * 24. * x5 - x3 * 6. * x7);
	z22 = (a4 * a5 + a2 * a6) * 6. + eqsq * ((x2 * x5 + x1 * x6) * 24. - (
		x4 * x7 + x3 * x8) * 6.);
	z23 = a4 * 6. * a6 + eqsq * (x2 * 24. * x6 - x4 * 6. * x8);
	z1 = z1 + z1 + bsq * z31;
	z2 = z2 + z2 + bsq * z32;
	z3 = z3 + z3 + bsq * z33;
	s3 = cc * xnoi;
	s2 = s3 * -.5 / rteqsq;
	s4 = s3 * rteqsq;
	s1 = eq * -15. * s4;
	s5 = x1 * x3 + x2 * x4;
	s6 = x2 * x3 + x1 * x4;
	s7 = x2 * x4 - x1 * x3;
	se = s1 * zn * s5;
	si = s2 * zn * (z11 + z13);
	sl = -zn * s3 * (z1 + z3 - 14. - eqsq * 6.);
	sgh = s4 * zn * (z31 + z33 - 6.);
	sh = -zn * s2 * (z21 + z23);

/*        Check for, and adjust SH, at inclinations near 0 and 180 degs. */

	if (xqncl < .052359877 || xqncl > pi_() - .052359877) {
	    sh = 0.;
	}

/*        Secondary check, J.I.C. */

	if (siniq == 0.) {
	    sh = 0.;
	}
	ee2 = s1 * 2. * s6;
	e3 = s1 * 2. * s7;
	xi2 = s2 * 2. * z12;
	xi3 = s2 * 2. * (z13 - z11);
	xl2 = s3 * -2. * z2;
	xl3 = s3 * -2. * (z3 - z1);
	xl4 = s3 * -2. * (-21. - eqsq * 9.) * ze;
	xgh2 = s4 * 2. * z32;
	xgh3 = s4 * 2. * (z33 - z31);
	xgh4 = s4 * -18. * ze;
	xh2 = s2 * -2. * z22;
	xh3 = s2 * -2. * (z23 - z21);
	if (i__ == 1) {

/*           Do lunar terms after solar terms, but only once. */

	    ssx[0] = sl;

/*            Prevent evaluation of 1/SINIQ for SH = 0. */

	    if (sh == 0.) {
		ssx[2] = 0.;
	    }
	    if (sh != 0.) {
		ssx[2] = sh / siniq;
	    }
	    ssx[1] = sgh - cosiq * ssx[2];
	    ssx[3] = se;
	    ssx[4] = si;
	    se2 = ee2;
	    si2 = xi2;
	    sl2 = xl2;
	    sgh2 = xgh2;
	    sh2 = xh2;
	    se3 = e3;
	    si3 = xi3;
	    sl3 = xl3;
	    sgh3 = xgh3;
	    sh3 = xh3;
	    sl4 = xl4;
	    sgh4 = xgh4;
	    zcosg = zcosgl;
	    zsing = zsingl;
	    zcosi = zcosil;
	    zsini = zsinil;
	    zcosh = zcoshl * cosq + zsinhl * sinq;
	    zsinh = sinq * zcoshl - cosq * zsinhl;
	    zn = 1.5835218e-4;
	    cc = 4.7968065e-7;
	    ze = .0549;
	}
    }
    ssx[0] += sl;

/*      Prevent evaluation of 1/SINIQ for SH = 0. */

    if (sh == 0.) {
	ssx[1] += sgh;
    }
    if (sh != 0.) {
	ssx[1] = ssx[1] + sgh - cosiq / siniq * sh;
    }
    if (sh == 0.) {
	ssx[2] = ssx[2];
    }
    if (sh != 0.) {
	ssx[2] += sh / siniq;
    }
    ssx[3] += se;
    ssx[4] += si;

/*     Geopotential resonance initialization for 12 hour orbits */

    iresfl = 0;
    isynfl = 0;
    if (xnq < .0052359877 && xnq > .0034906585) {

/*        Synchronous resonance terms initialization */

	iresfl = 1;
	isynfl = 1;
	g200 = eqsq * (eqsq * .8125 - 2.5) + 1.;
	g310 = eqsq * 2. + 1.;
	g300 = eqsq * (eqsq * 6.60937 - 6.) + 1.;
/* Computing 2nd power */
	d__1 = cosiq + 1.;
	f220 = d__1 * d__1 * .75;
	f311 = siniq * .9375 * siniq * (cosiq * 3. + 1.) - (cosiq + 1.) * .75;
/* Computing 3rd power */
	d__1 = cosiq + 1.;
	f330 = d__1 * (d__1 * d__1) * 1.875;
/* Computing 2nd power */
	d__1 = xnq;
/* Computing 2nd power */
	d__2 = aqnv;
	del[0] = d__1 * d__1 * 3. * (d__2 * d__2);
	del[1] = del[0] * 2. * f220 * g200 * 1.7891679e-6;
	del[2] = del[0] * 3. * f330 * g300 * 2.2123015e-7 * aqnv;
	del[0] = del[0] * f311 * g310 * 2.1460748e-6 * aqnv;
	xlamo = xmao + xnodeo + omegao - thgr;
	bfact = *xlldot + *omgdot + *xnodot - .0043752691;
	bfact = bfact + ssx[0] + ssx[1] + ssx[2];
    } else {
	if (xnq < .00826 || xnq > .00924 || eq < .5) {
	    return 0;
	}
	iresfl = 1;
	eoc = eq * eqsq;
	g201 = -.306 - (eq - .64) * .44;

/*     Looks icky doesn't it? */

	if (eq > .65) {
	    g211 = eq * 331.819 - 72.099 - eqsq * 508.738 + eoc * 266.724;
	    g310 = eq * 1582.851 - 346.844 - eqsq * 2415.925 + eoc * 1246.113;
	    g322 = eq * 1554.908 - 342.585 - eqsq * 2366.899 + eoc * 1215.972;
	    g410 = eq * 4758.686 - 1052.797 - eqsq * 7193.992 + eoc * 
		    3651.957;
	    g422 = eq * 16178.11 - 3581.69 - eqsq * 24462.77 + eoc * 12422.52;

/*           Decide on the G520 coefficient. */

	    if (eq > .715) {
		g520 = eq * 29936.92 - 5149.66 - eqsq * 54087.36 + eoc * 
			31324.56;
	    } else {
		g520 = 1464.74 - eq * 4664.75 + eqsq * 3763.64;
	    }
	} else {
	    g211 = 3.616 - eq * 13.247 + eqsq * 16.29;
	    g310 = eq * 117.39 - 19.302 - eqsq * 228.419 + eoc * 156.591;
	    g322 = eq * 109.7927 - 18.9068 - eqsq * 214.6334 + eoc * 146.5816;
	    g410 = eq * 242.694 - 41.122 - eqsq * 471.094 + eoc * 313.953;
	    g422 = eq * 841.88 - 146.407 - eqsq * 1629.014 + eoc * 1083.435;
	    g520 = eq * 3017.977 - 532.114 - eqsq * 5740. + eoc * 3708.276;
	}
	if (eq >= .7) {
	    g533 = eq * 161616.52 - 37995.78 - eqsq * 229838.2 + eoc * 
		    109377.94;
	    g521 = eq * 218913.95 - 51752.104 - eqsq * 309468.16 + eoc * 
		    146349.42;
	    g532 = eq * 170470.89 - 40023.88 - eqsq * 242699.48 + eoc * 
		    115605.82;
	} else {
	    g533 = eq * 4988.61 - 919.2277 - eqsq * 9064.77 + eoc * 5542.21;
	    g521 = eq * 4568.6173 - 822.71072 - eqsq * 8491.4146 + eoc * 
		    5337.524;
	    g532 = eq * 4690.25 - 853.666 - eqsq * 8624.77 + eoc * 5341.4;
	}
	sini2 = siniq * siniq;
	f220 = (cosiq * 2. + 1. + cosq2) * .75;
	f221 = sini2 * 1.5;
	f321 = siniq * 1.875 * (1. - cosiq * 2. - cosq2 * 3.);
	f322 = siniq * -1.875 * (cosiq * 2. + 1. - cosq2 * 3.);
	f441 = sini2 * 35. * f220;
	f442 = sini2 * 39.375 * sini2;
	f522 = siniq * 9.84375 * (sini2 * (1. - cosiq * 2. - cosq2 * 5.) + (
		cosiq * 4. - 2. + cosq2 * 6.) * .33333333);
	f523 = siniq * (sini2 * 4.92187512 * (-2. - cosiq * 4. + cosq2 * 10.) 
		+ (cosiq * 2. + 1. - cosq2 * 3.) * 6.56250012);
	f542 = siniq * 29.53125 * (2. - cosiq * 8. + cosq2 * (cosiq * 8. - 
		12. + cosq2 * 10.));
	f543 = siniq * 29.53125 * (-2. - cosiq * 8. + cosq2 * (cosiq * 8. + 
		12. - cosq2 * 10.));
	xno2 = xnq * xnq;
	ainv2 = aqnv * aqnv;
	temp1 = xno2 * 3. * ainv2;
	temp = temp1 * 1.7891679e-6;
	dg[0] = temp * f220 * g201;
	dg[1] = temp * f221 * g211;
	temp1 *= aqnv;
	temp = temp1 * 3.7393792e-7;
	dg[2] = temp * f321 * g310;
	dg[3] = temp * f322 * g322;
	temp1 *= aqnv;
	temp = temp1 * 2. * 7.3636953e-9;
	dg[4] = temp * f441 * g410;
	dg[5] = temp * f442 * g422;
	temp1 *= aqnv;
	temp = temp1 * 1.1428639e-7;
	dg[6] = temp * f522 * g520;
	dg[7] = temp * f523 * g532;
	temp = temp1 * 2. * 2.1765803e-9;
	dg[8] = temp * f542 * g521;
	dg[9] = temp * f543 * g533;
	xlamo = xmao + xnodeo + xnodeo - thgr - thgr;
	bfact = *xlldot + *xnodot + *xnodot - .0043752691 - .0043752691;
	bfact = bfact + ssx[0] + ssx[2] + ssx[2];
    }
    xfact = bfact - xnq;

/*     Initialize integrator */

    xli = xlamo;
    xni = xnq;
    atime = 0.;
    return 0;
/* $Procedure ZZDPSEC (Calculate secular perturbations ) */

L_zzdpsec:
/* $ Abstract */

/*     Entrance for deep space secular effects */

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

/*     None. */

/* $ Keywords */

/*     SECULAR PERTURBATION */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     XLL        I    Long-period periodic term */
/*     OMGASM     I    Perturbed argument of perigee */
/*     XNODES     I    Perturbed argument of ascending node */
/*     T          I    Time to calculate perturbation */
/*     ELEMS      I    The two line elements array */
/*     XN         O    Perturbed mean motion of the orbit at time T */
/*     EM         O    Perturbed eccentricity of the orbit at time T */
/*     XINC       O    Perturbed inclination of the orbit plane at time T */

/* $ Detailed_Input */

/*     XLL         a long-period periodic term dependent on inclination, */
/*                 eccentricity and argument of periapsis */

/*     OMGASM      the value of the argument of perigee after the */
/*                 perturbations at the time of interest are */
/*                 added */

/*     XNODES      is the value of the argument of the ascending node */
/*                 after the perturbations at the time of interest are */
/*                 added. */

/*     T           is the total time from the epoch, in minutes, of the */
/*                 element set at which to calculate the perturbation. */

/*     ELEMS       is an array containing two-line element data */
/*                 as prescribed below. The elements XNDD6O and BSTAR */
/*                 must already be scaled by the proper exponent stored */
/*                 in the two line elements set.  Moreover, the */
/*                 various items must be converted to the units shown */
/*                 here. */

/*                    ELEMS (  1 ) = XNDT2O in radians/minute**2 */
/*                    ELEMS (  2 ) = XNDD6O in radians/minute**3 */
/*                    ELEMS (  3 ) = BSTAR */
/*                    ELEMS (  4 ) = XINCL  in radians */
/*                    ELEMS (  5 ) = XNODEO in radians */
/*                    ELEMS (  6 ) = EO */
/*                    ELEMS (  7 ) = OMEGAO in radians */
/*                    ELEMS (  8 ) = XMO    in radians */
/*                    ELEMS (  9 ) = XNO    in radians/minute */
/*                    ELEMS ( 10 ) = EPOCH of the elements in seconds */
/*                                   past ephemeris epoch J2000. */

/* $ Detailed_Output */

/*     XN          is the perturbed mean motion from the 'mean' mean */
/*                 motion at epoch at time T. */

/*     EM          is the perturbed eccentricity from the mean */
/*                 eccentricity at epoch at time T. */

/*     XINC        is the perturbed inclination of the orbit plane from */
/*                 the mean inclination at the epoch at time T */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The operation of this routine is to calculate the current secular */
/*     perturbations of the 'mean' orbit elements.  The extent of the */
/*     perturbations is determined by the state of the IRESFL flag.  This */
/*     flag indicates whether the resonance effects will or will not be */
/*     calculated for the vehicle.  Resonance will be calculated when */
/*     mean motion is between 0.8 to 1.2 orbits per day (approximately */
/*     geosynch), or between 1.9 and 2.1 orbits per days. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1)  This routine should only be called by DPSPCE when propagating */
/*         two line element sets. */

/* $ Literature_References */

/*     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models */
/*     for Propagation of NORAD Element Sets". United States Department */
/*     of Defense Spacetrack Report (3). */

/*     Vallado, David A., Paul Crawford, Richard Hujsak, and */
/*     Kelso, T. S., "Revisiting Spacetrack Report #3," AIAA/AAS */
/*     Astrodynamics Specialist Conference, Keystone, CO, Aug 2006. */

/* $ Author_and_Institution */

/*     E.D. Wright      (JPL) */
/*     W.L. Taber       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.6.0, 27-JUN-2016 (EDW) */

/*        The "Revisiting Spacetrack Report #3" by Vallado et. al. */
/*        indicates the negative inclination modification as a mistake. */
/*        That code was removed. */

/* -    SPICELIB Version 1.5.1, 19-SEP-2006 (EDW) */

/*        Added text to previously empty Declarations section */
/*        in ZZDPINIT, ZZDPPER, ZZDPSEC. */

/* -    SPICELIB Version 1.5.0, 20-JAN-1999 (EDW) (WLT) */

/*        OMGDOT, named in an ENTRY point argument list */
/*        was not passed via an argument list.  Solaris exhibited a */
/*        bus error because of this situation.  All ENTRY point */
/*        arguments are passed only by argument lists and are declared */
/*        in the umbrella subroutine's, ZZNRDDP, argument list. */

/*        Combined the various SSL, SSG, SSH, SSI, SSE variables into */
/*        the vector SSX. */

/*        Removed the dependency upon the UTC/ET leapsecond kernel. */

/*        Alphabetized all variable declaration lists. */

/*        All arguments passed through entry points listed as arguments */
/*        of ZZNRDDP.  OMGDT renamed OMGDOT to be consistent with other */
/*        deep space two line element routines. */

/* -    SPICELIB Version 1.0.0, MAY-2-1997 (EDW) */

/* -& */
/* $ Index_Entries */

/*     two line elements, secular perturbation */

/* -& */
    stepp = 720.;
    stepn = -720.;
    xincl = elems[3];
    eo = elems[5];
    *xll += ssx[0] * *t;
    *omgasm += ssx[1] * *t;
    *xnodes += ssx[2] * *t;
    *em = eo + ssx[3] * *t;
    *xinc = xincl + ssx[4] * *t;

/*     Check for the state of the resonance flag. */

    if (iresfl == 0) {
	return 0;
    }

/*     If we got down here then the resonance effects need to be */
/*     calculated.  Continue to loop until the CONT flag is set to false. */

    cont = TRUE_;
    while(cont) {
	if (atime == 0. || *t >= 0. && atime < 0. || *t < 0. && atime >= 0.) {

/*           Epoch restart */

	    if (*t >= 0.) {
		delt = stepp;
	    } else {
		delt = stepn;
	    }
	    atime = 0.;
	    xni = xnq;
	    xli = xlamo;
	    cont = FALSE_;
	} else if (abs(*t) >= abs(atime)) {
	    delt = stepn;
	    if (*t > 0.) {
		delt = stepp;
	    }
	    cont = FALSE_;
	} else {
	    delt = stepp;
	    if (*t >= 0.) {
		delt = stepn;
	    }
	    zzsecprt_(&isynfl, dg, del, &xni, &omegao, &atime, omgdot, &xli, &
		    xfact, &xldot, &xndot, &xnddt);
	    xli = xli + xldot * delt + xndot * 259200.;
	    xni = xni + xndot * delt + xnddt * 259200.;
	    atime += delt;
	    cont = TRUE_;
	}
    }

/*     Do this loop while the time interval is greater than STEPP */

    while((d__1 = *t - atime, abs(d__1)) >= stepp) {
	zzsecprt_(&isynfl, dg, del, &xni, &omegao, &atime, omgdot, &xli, &
		xfact, &xldot, &xndot, &xnddt);
	xli = xli + xldot * delt + xndot * 259200.;
	xni = xni + xndot * delt + xnddt * 259200.;
	atime += delt;
    }

/*     Calculate the time interval and determine the secular */
/*     perturbations */

    ft = *t - atime;
    zzsecprt_(&isynfl, dg, del, &xni, &omegao, &atime, omgdot, &xli, &xfact, &
	    xldot, &xndot, &xnddt);
    *xn = xni + xndot * ft + xnddt * ft * ft * .5;
    xl = xli + xldot * ft + xndot * ft * ft * .5;
    temp = -(*xnodes) + thgr + *t * .0043752691;
    *xll = xl - *omgasm + temp;
    if (isynfl == 0) {
	*xll = xl + temp + temp;
    }
    return 0;
/* $Procedure ZZDPPER ( Calculate periodic perturbations ) */

L_zzdpper:
/* $ Abstract */

/*     Entrances for lunar-solar periodics */

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

/*     None. */

/* $ Keywords */

/*     PERIODIC PERTURBATION */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     T          I   Time to calculate perturbations */
/*     EM         O   Perturbed eccentricity of the orbit at time T */
/*     XINC       O   Perturbed inclination of the orbit plane at time T */
/*     OMGASM     O   Perturbed argument of perigee */
/*     XNODES     O   Perturbed argument of ascending node */
/*     XLL        0   Long-period periodic term */

/* $ Detailed_Input */

/*     T           the time from the epoch in minutes of the element set */
/*                 at which to calculate the perturbation. */

/* $ Detailed_Output */

/*     EM          is the perturbed eccentricity from the mean */
/*                 eccentricity at epoch at time T. */

/*     XINC        is the perturbed inclination of the orbit plane from */
/*                 the mean inclination at the epoch at time T. */

/*     OMGASM      the value of the argument of perigee after the */
/*                 perturbations at the time of interest are */
/*                 added. */

/*     XNODES      is the value of the argument of the ascending node */
/*                 after the perturbations at the time of interest are */
/*                 added. */

/*     XLL         a long-period periodic term dependent on inclination, */
/*                 eccentricity and argument of periapsis. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine calculates the current time dependent periodic */
/*     perturbations values due to the sun and the moon.  The original */
/*     version, as taken from the Spacetrack 3 report, had a number of */
/*     bugs. */

/*     XNODES could be evaluated as being in the wrong quadrant due to */
/*     a failure to insure a domain of 0 to 2 Pi. */

/*     The SIN and COS of the perturbed inclination, XINCL, were */
/*     calculated before the perturbed value. */

/*     EM & XINC are input and output values.  The input value is updated */
/*     by the addition of a perturbation value. */

/*     The original report did not recalculate perturbation terms if two */
/*     consecutive epoch times were less than 30 minutes apart.  This */
/*     condition has been removed.  Perturbation terms are always */
/*     calculated. */

/* $ Examples */

/*     None needed. */

/* $ Restrictions */

/*     1)  This routine should only be called by DPSPCE when propagating */
/*         two line element sets. */

/*     2)  This routine should be initialized prior to use by making */
/*         a call with the time epoch set to 0.  Failure to do so */
/*         invalidates the perturbation calculation. */

/* $ Literature_References */

/*     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models */
/*     for Propagation of NORAD Element Sets". United States Department */
/*     of Defense Spacetrack Report (3). */

/* $ Author_and_Institution */

/*     E.D. Wright      (JPL) */
/*     W.L. Taber       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.5.1, 19-SEP-2006 (EDW) */

/*        Added text to previously empty Declarations section */
/*        in ZZDPINIT, ZZDPPER, ZZDPSEC. */

/* -    SPICELIB Version 1.5.0, 20-JAN-1999 (EDW) (WLT) */

/*        OMGDOT, named in an ENTRY point argument list */
/*        was not passed via an argument list.  Solaris exhibited a */
/*        bus error because of this situation.  All ENTRY point */
/*        arguments are passed only by argument lists and are declared */
/*        in the umbrella subroutine's, ZZNRDDP, argument list. */

/*        Combined the various SSL, SSG, SSH, SSI, SSE variables into */
/*        the vector SSX. */

/*        Removed the dependency upon the UTC/ET leapsecond kernel. */

/*        Alphabetized all variable declaration lists. */

/*        All arguments passed through entry points listed as arguments */
/*        of ZZNRDDP.  OMGDT renamed OMGDOT to be consistent with other */
/*        deep space two line element routines. */

/* -    SPICELIB Version 1.0.0, MAY-17-1997 (EDW) */


/* -& */
/* $ Index_Entries */

/*     two line elements, periodic perturbation */

/* -& */

/*     Time varying periodic terms. */


/*     Update for solar perts at time T. */

    zm = zmos + *t * 1.19459e-5;
    zf = zm + sin(zm) * .033500000000000002;
    sinzf = sin(zf);
    f2 = sinzf * .5 * sinzf - .25;
    f3 = sinzf * -.5 * cos(zf);
    ses = se2 * f2 + se3 * f3;
    sis = si2 * f2 + si3 * f3;
    sls = sl2 * f2 + sl3 * f3 + sl4 * sinzf;
    sghs = sgh2 * f2 + sgh3 * f3 + sgh4 * sinzf;
    shs = sh2 * f2 + sh3 * f3;

/*     Update for lunar perts at time T. */

    zm = zmol + *t * 1.5835218e-4;
    zf = zm + sin(zm) * .10979999999999999;
    sinzf = sin(zf);
    f2 = sinzf * .5 * sinzf - .25;
    f3 = sinzf * -.5 * cos(zf);
    sel = ee2 * f2 + e3 * f3;
    sil = xi2 * f2 + xi3 * f3;
    sll = xl2 * f2 + xl3 * f3 + xl4 * sinzf;
    sghl = xgh2 * f2 + xgh3 * f3 + xgh4 * sinzf;
    shl = xh2 * f2 + xh3 * f3;

/*     Sum of solar and lunar perts */

    pe = ses + sel;
    pinc = sis + sil;
    pl = sls + sll;
    pgh = sghs + sghl;
    ph = shs + shl;

/*     Subtract the epoch perturbations off the calculated values. */

    pe -= pe0;
    pinc -= pinc0;
    pl -= pl0;
    pgh -= pgh0;
    ph -= ph0;
    *xinc += pinc;
    *em += pe;

/*     Sin and Cos of the perturbed inclination.  The original */
/*     Spacetrack 3 report calculated the values before the */
/*     perturbation. */

    sinis = sin(*xinc);
    cosis = cos(*xinc);
    if (xqncl > .2) {
	ph /= siniq;
	pgh -= cosiq * ph;
	*omgasm += pgh;
	*xnodes += ph;
	*xll += pl;
    } else {

/*        Apply periodics with Lyddane modification */

	sinok = sin(*xnodes);
	cosok = cos(*xnodes);
	alfdp = sinis * sinok;
	betdp = sinis * cosok;
	alfdp = alfdp + ph * cosok + pinc * cosis * sinok;
	betdp = betdp - ph * sinok + pinc * cosis * cosok;

/*        Force a 0 - 2Pi domain on XNODES. */

	if (*xnodes < 0.) {
	    *xnodes += pix2;
	}
	xls = *xll + *omgasm + pl + pgh + cosis * *xnodes - sinis * *xnodes * 
		pinc;

/*        Compute the angle from the x-axis of the point */

	if (alfdp != 0. || betdp != 0.) {

/*           Save the old value of XNODES, then compute the current value */
/*           From ALFDP and BETDP */

	    oxnode = *xnodes;
	    *xnodes = atan2(alfdp, betdp);

/*           Force a 0 - 2Pi domain on XNODES */

	    if (*xnodes < 0.) {
		*xnodes += pix2;
	    }

/*           XNODES should be the angular difference between the previous */
/*           value of XNODES and that just calculated.  This is a */
/*           correction to the standard SDP4 routine which did not */
/*           calculate this term correctly if XNODES passes over the */
/*           branch cut at 2*Pi. */

	    if ((d__1 = *xnodes - oxnode, abs(d__1)) > pix1) {
		if (*xnodes > oxnode) {
		    *xnodes -= pix2;
		} else {
		    *xnodes += pix2;
		}
	    }
	} else {
	    *xnodes = 0.;
	}
	*xll += pl;
	*omgasm = xls - *xll - *xnodes * cos(*xinc);
    }
    return 0;
} /* zznrddp_ */

/* Subroutine */ int zznrddp_(doublereal *ao, doublereal *elems, doublereal *
	em, doublereal *omgasm, doublereal *omgdot, doublereal *t, doublereal 
	*xinc, doublereal *xll, doublereal *xlldot, doublereal *xn, 
	doublereal *xnodes, doublereal *xnodot, doublereal *xnodp)
{
    return zznrddp_0_(0, ao, elems, em, omgasm, omgdot, t, xinc, xll, xlldot, 
	    xn, xnodes, xnodot, xnodp);
    }

/* Subroutine */ int zzdpinit_(doublereal *ao, doublereal *xlldot, doublereal 
	*omgdot, doublereal *xnodot, doublereal *xnodp, doublereal *elems)
{
    return zznrddp_0_(1, ao, elems, (doublereal *)0, (doublereal *)0, omgdot, 
	    (doublereal *)0, (doublereal *)0, (doublereal *)0, xlldot, (
	    doublereal *)0, (doublereal *)0, xnodot, xnodp);
    }

/* Subroutine */ int zzdpsec_(doublereal *xll, doublereal *omgasm, doublereal 
	*xnodes, doublereal *em, doublereal *xinc, doublereal *xn, doublereal 
	*t, doublereal *elems, doublereal *omgdot)
{
    return zznrddp_0_(2, (doublereal *)0, elems, em, omgasm, omgdot, t, xinc, 
	    xll, (doublereal *)0, xn, xnodes, (doublereal *)0, (doublereal *)
	    0);
    }

/* Subroutine */ int zzdpper_(doublereal *t, doublereal *em, doublereal *xinc,
	 doublereal *omgasm, doublereal *xnodes, doublereal *xll)
{
    return zznrddp_0_(3, (doublereal *)0, (doublereal *)0, em, omgasm, (
	    doublereal *)0, t, xinc, xll, (doublereal *)0, (doublereal *)0, 
	    xnodes, (doublereal *)0, (doublereal *)0);
    }

