/* zzdsin.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZDSIN ( SGP4 deep space initialization ) */
/* Subroutine */ int zzdsin_(doublereal *geophs, doublereal *cosim, 
	doublereal *emsq, doublereal *argpo, doublereal *s1, doublereal *s2, 
	doublereal *s3, doublereal *s4, doublereal *s5, doublereal *sinim, 
	doublereal *ss1, doublereal *ss2, doublereal *ss3, doublereal *ss4, 
	doublereal *ss5, doublereal *sz1, doublereal *sz3, doublereal *sz11, 
	doublereal *sz13, doublereal *sz21, doublereal *sz23, doublereal *
	sz31, doublereal *sz33, doublereal *t, doublereal *tc, doublereal *
	gsto, doublereal *mo, doublereal *mdot, doublereal *no, doublereal *
	nodeo, doublereal *nodedot, doublereal *xpidot, doublereal *z1, 
	doublereal *z3, doublereal *z11, doublereal *z13, doublereal *z21, 
	doublereal *z23, doublereal *z31, doublereal *z33, doublereal *ecco, 
	doublereal *eccsq, doublereal *eccm, doublereal *argpm, doublereal *
	inclm, doublereal *mm, doublereal *xn, doublereal *nodem, integer *
	irez, doublereal *atime, doublereal *d2201, doublereal *d2211, 
	doublereal *d3210, doublereal *d3222, doublereal *d4410, doublereal *
	d4422, doublereal *d5220, doublereal *d5232, doublereal *d5421, 
	doublereal *d5433, doublereal *dedt, doublereal *didt, doublereal *
	dmdt, doublereal *dndt, doublereal *dnodt, doublereal *domdt, 
	doublereal *del1, doublereal *del2, doublereal *del3, doublereal *
	xfact, doublereal *xlamo, doublereal *xli, doublereal *xni)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_mod(doublereal *, doublereal *), pow_dd(doublereal *, doublereal 
	    *);

    /* Local variables */
    doublereal sghl, aonv, sghs, temp, ainv2, sini2, temp1;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal theta, emsqo, root22, root32, root52, root44, root54, rptim;
    extern doublereal twopi_(void);
    doublereal q22, q31, q33;
    extern doublereal pi_(void);
    doublereal g200, f220, f221, f311, f321, f322, f330, cosisq, f441, f442, 
	    f522, f523, f542, f543, g201, g211, g300, g310, g322, g410, g422, 
	    g520, g521, g532, g533;
    extern logical return_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal eoc, emo, shl, xke, ses, x2o3, sgs, shs, sis, sls, znl, zns, 
	    xno2;

/* $ Abstract */

/*     This Subroutine provides Deep Space contributions to Mean */
/*     Motion Dot due to geopotential resonance with half day and one */
/*     day orbits. */

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

/*     None. */

/* $ Declarations */
/* $Procedure ZZSGP4 ( SGP4 parameters ) */

/* $ Abstract */

/*      Parameter assignments for SGP4 algorithm as expressed */
/*      by Vallado [2]. */

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

/*     None. */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     J2    = GEOPHS(K_J2) */
/*     J3    = GEOPHS(K_J3) */
/*     J4    = GEOPHS(K_J4) */
/*     ER    = GEOPHS(K_ER) */
/*     XKE   = GEOPHS(K_KE) */

/*     TUMIN = 1.D0/XKE */
/*     J3OJ2 = J3/J2 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for */
/*       Propagation of the NORAD Element Sets." Spacetrack Report #3. */
/*       U.S. Air Force: Aerospace Defense Command. */

/*   [2] Vallado, David, Crawford, Paul, Hujsak, Richard, and Kelso, T.S. */
/*       2006. Revisiting Spacetrack Report #3. Paper AIAA 2006-6753 */
/*       presented at the AIAA/AAS Astrodynamics Specialist Conference, */
/*       August 21-24, 2006. Keystone, CO. */

/* $ Author_and_Institution */

/*     E. D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, MAY-27-2020 (EDW) */

/*        Updated descriptions of GEOPHS constants to be consistent */
/*        with what's used in other routines. */

/* -    SPICELIB Version 1.0.0 22-JUL-2014 (EDW) */

/* -& */
/* $ Index_Entries */

/*  SGP4 */

/* -& */

/*      WGS gravitational constants IDs. */


/*      Gravitational constant indices. */


/*     The following parameters give the indices in the GEOPHS */
/*     array of the various geophysical parameters needed for */
/*     the two line element sets. */

/*     K_J2  --- index of J2 gravitational harmonic for earth */
/*     K_J3  --- index of J3 gravitational harmonic for earth */
/*     K_J4  --- index of J4 gravitational harmonic for earth */
/*     K_KE  --- index of KE = sqrt(GM) in earth-radii**1.5/MIN */
/*     K_QO  --- index of high altitude bound for atmospheric */
/*               model in km */
/*     K_SO  --- index of low altitude bound for atmospheric */
/*               model in km */
/*     K_ER  --- index of earth equatorial radius in km */
/*     K_AE  --- index of distance units/earth radius */


/*     Operation mode values, OPMODE. */


/*     An enumeration of the various components of the */
/*     elements array---ELEMS */

/*     KNDT20  --- location of NDT20 */
/*     KNDD60  --- location of NDD60 */
/*     KBSTAR  --- location of BSTAR */
/*     KINCL   --- location of INCL */
/*     KNODE0  --- location of NODE0 */
/*     KECC    --- location of ECC */
/*     KOMEGA  --- location of OMEGA */
/*     KMO     --- location of MO */
/*     KNO     --- location of NO */

/* $ Brief_I/O */

/*    See Detailed_input and Detailed_Output. */

/* $ Detailed_Input */

/*    COSIM       COS of mean inclination. */

/*    SINIM       SIN of mean inclination. */

/*    EMSQ        Eccentricity squared */

/*    ARGPO       Argument of Perigee */

/*    S1          S coefficients */

/*    S2              ... */

/*    S3              ... */

/*    S4              ... */

/*    S5              ... */

/*    SS1             ... */

/*    SS2             ... */

/*    SS3             ... */

/*    SS4             ... */

/*    SS5             ... */

/*    SZ1             ... */

/*    SZ3             ... */

/*    SZ11            ... */

/*    SZ13            ... */

/*    SZ21            ... */

/*    SZ23            ... */

/*    SZ31            ... */

/*    SZ33            ... */

/*    T           Time */

/*    TC */

/*    GSTO        Greenwich sidereal time in radians */

/*    MO          Mean anomaly */

/*    MDOT        Mean anomaly rate */

/*    NO          Mean motion */

/*    NODEO       Right ascension of ascending node */

/*    NODEDOT     Right ascension of ascending node rate */

/*    XPIDOT */

/*    Z1          Z coefficients */

/*    Z3               ... */

/*    Z11              ... */

/*    Z13              ... */

/*    Z21              ... */

/*    Z23              ... */

/*    Z31              ... */

/*    Z33              ... */

/*    ECCM        Mean eccentricity */

/*    ARGPM       Mean argument of perigee */

/*    INCLM       Mean inclination */

/*    MM          Mean anomaly */

/*    XN          Mean motion */

/*    NODEM       Mean right ascension of ascending node */

/* $ Detailed_Output */

/*    ECCM        Mean eccentricity */

/*    ARGPM       Mean argument of perigee */

/*    INCLM       Mean inclination */

/*    MM          Mean anomaly */

/*    XN          Mean motion */

/*    NODEM       Right ascension of ascending node */

/*    IREZ        Resonance flags: 0-none, 1-one day, 2-half day */

/*    ATIME       Internal SGD4 parameter. */

/*    D2201       D COEFFCIENTS */

/*    D2211           ... */

/*    D3210           ... */

/*    D3222           ... */

/*    D4410           ... */

/*    D4422           ... */

/*    D5220           ... */

/*    D5232           ... */

/*    D5421           ... */

/*    D5433           ... */

/*    DEDT        Internal SGD4 parameter. */

/*    DIDT        Internal SGD4 parameter. */

/*    DMDT        Internal SGD4 parameter. */

/*    DNDT        Internal SGD4 parameter. */

/*    DNODT       Internal SGD4 parameter. */

/*    DOMDT       Internal SGD4 parameter. */

/*    DEL1        Internal SGD4 parameter. */

/*    DEL2        Internal SGD4 parameter. */

/*    DEL3        Internal SGD4 parameter. */

/*    SES         Internal SGD4 parameter. */

/*    SGHL        Internal SGD4 parameter. */

/*    SGHS        Internal SGD4 parameter. */

/*    SGS         Internal SGD4 parameter. */

/*    SHL         Internal SGD4 parameter. */

/*    SHS         Internal SGD4 parameter. */

/*    SIS         Internal SGD4 parameter. */

/*    SLS         Internal SGD4 parameter. */

/*    THETA       Internal SGD4 parameter. */

/*    XFACT       Internal SGD4 parameter. */

/*    XLAMO       Internal SGD4 parameter. */

/*    XLI         Internal SGD4 parameter. */

/*    XNI         Internal SGD4 parameter. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is based on the DSINIT code by David Vallado */
/*     corresponding to "Revisiting Spacetrack Report #3" [4]. */
/*     The intent is to maintain the original Vallado algorithm, */
/*     changing code only to meet NAIF format standards and to */
/*     integrate with SPICELIB. */

/*        Removed getgravconst call, replaced with GEOPHS array. */

/*        Capitalize all variables. */

/*        ENDIF replaced with END IF. */

/*        whichconst    eliminated, function provided by GEOPHS */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for */
/*       Propagation of the NORAD Element Sets." Spacetrack Report #3. */
/*       U.S. Air Force: Aerospace Defense Command. */

/*   [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation */
/*       of Space Command Element Sets." Space Command, */
/*       U. S. Air Force, CO. */

/*   [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004. */
/*       History of Analytical Orbit Modeling in the U. S. Space */
/*       Surveillance System. Journal of Guidance, Control, and */
/*       Dynamics. 27(2):174-185. */

/*   [4] Vallado, David, Crawford, Paul, Hujsak, Richard, */
/*       and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper */
/*       AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics */
/*       Specialist Conference, August 21-24, 2006. Keystone, CO. */

/* $ Author_and_Institution */

/*     David Vallado   (AGI) */
/*     E. D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 03-NOV-2014 (EDW) */

/*        Based on routine DSINIT, 28-JUN-2005, Vallado 2006 [4]. */

/* -& */
/* $ Index_Entries */

/*   SGP4 */

/* -& */

/*     Local Variables */


/*     SPICELIB routines. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZDSIN", (ftnlen)6);

/*     Constants */

    q22 = 1.7891679e-6;
    q31 = 2.1460748e-6;
    q33 = 2.2123015e-7;
    root22 = 1.7891679e-6;
    root44 = 7.3636953e-9;
    root54 = 2.1765803e-9;

/*     This equates to 7.29211514668855e-5 rad/sec */

    rptim = .00437526908801129966;
    root32 = 3.7393792e-7;
    root52 = 1.1428639e-7;
    x2o3 = .66666666666666663;
    znl = 1.5835218e-4;
    zns = 1.19459e-5;

/*     This code block replaces the call: */

/*     sgp4fix identify constants and allow alternate values. */

/*     CALL getgravconst( whichconst, tumin, */
/*     .                  mu, radiusearthkm, xke, */
/*     .                  j2, j3, j4, j3oj2 ) */

    xke = geophs[3];

/*     DEEP SPACE INITIALIZATION */

    *irez = 0;
    if (*xn < .0052359877 && *xn > .0034906585) {
	*irez = 1;
    }
    if (*xn >= .00826 && *xn <= .00924 && *eccm >= .5) {
	*irez = 2;
    }

/*     DO SOLAR TERMS */

    ses = *ss1 * zns * *ss5;
    sis = *ss2 * zns * (*sz11 + *sz13);
    sls = -zns * *ss3 * (*sz1 + *sz3 - 14. - *emsq * 6.);
    sghs = *ss4 * zns * (*sz31 + *sz33 - 6.);
    shs = -zns * *ss2 * (*sz21 + *sz23);

/*       sgp4fix for 180 deg incl */

    if (*inclm < .052359877 || *inclm > pi_() - .052359877) {
	shs = 0.;
    }
    if (*sinim != 0.) {
	shs /= *sinim;
    }
    sgs = sghs - *cosim * shs;

/*       DO LUNAR TERMS */

    *dedt = ses + *s1 * znl * *s5;
    *didt = sis + *s2 * znl * (*z11 + *z13);
    *dmdt = sls - znl * *s3 * (*z1 + *z3 - 14. - *emsq * 6.);
    sghl = *s4 * znl * (*z31 + *z33 - 6.);
    shl = -znl * *s2 * (*z21 + *z23);

/*       sgp4fix for 180 deg incl */

    if (*inclm < .052359877 || *inclm > pi_() - .052359877) {
	shl = 0.;
    }
    *domdt = sgs + sghl;
    *dnodt = shs;
    if (*sinim != 0.) {
	*domdt -= *cosim / *sinim * shl;
	*dnodt += shl / *sinim;
    }

/*       CALCULATE DEEP SPACE RESONANCE EFFECTS */

    *dndt = 0.;
    d__1 = *gsto + *tc * rptim;
    d__2 = twopi_();
    theta = d_mod(&d__1, &d__2);
    *eccm += *dedt * *t;
/* Computing 2nd power */
    d__1 = *eccm;
    *emsq = d__1 * d__1;
    *inclm += *didt * *t;
    *argpm += *domdt * *t;
    *nodem += *dnodt * *t;
    *mm += *dmdt * *t;

/*   sgp4fix for negative inclinations */
/*   the following if statement should be commented out */

/*           IF(Inclm .lt. 0.0D0) THEN */
/*             Inclm  = -Inclm */
/*             Argpm  = Argpm-PI */
/*             nodem = nodem+PI */
/*           END IF */


/*       Initialize the resonance terms */

    if (*irez != 0) {
	d__1 = *xn / xke;
	aonv = pow_dd(&d__1, &x2o3);

/*           GEOPOTENTIAL RESONANCE FOR 12 HOUR ORBITS */

	if (*irez == 2) {
	    cosisq = *cosim * *cosim;
	    emo = *eccm;
	    emsqo = *emsq;
	    *eccm = *ecco;
	    *emsq = *eccsq;
	    eoc = *eccm * *emsq;
	    g201 = -.306 - (*eccm - .64) * .44;
	    if (*eccm <= .65) {
		g211 = 3.616 - *eccm * 13.247 + *emsq * 16.29;
		g310 = *eccm * 117.39 - 19.302 - *emsq * 228.419 + eoc * 
			156.591;
		g322 = *eccm * 109.7927 - 18.9068 - *emsq * 214.6334 + eoc * 
			146.5816;
		g410 = *eccm * 242.694 - 41.122 - *emsq * 471.094 + eoc * 
			313.953;
		g422 = *eccm * 841.88 - 146.407 - *emsq * 1629.014 + eoc * 
			1083.435;
		g520 = *eccm * 3017.977 - 532.114 - *emsq * 5740.032 + eoc * 
			3708.276;
	    } else {
		g211 = *eccm * 331.819 - 72.099 - *emsq * 508.738 + eoc * 
			266.724;
		g310 = *eccm * 1582.851 - 346.844 - *emsq * 2415.925 + eoc * 
			1246.113;
		g322 = *eccm * 1554.908 - 342.585 - *emsq * 2366.899 + eoc * 
			1215.972;
		g410 = *eccm * 4758.686 - 1052.797 - *emsq * 7193.992 + eoc * 
			3651.957;
		g422 = *eccm * 16178.11 - 3581.69 - *emsq * 24462.77 + eoc * 
			12422.52;
		if (*eccm > .715) {
		    g520 = *eccm * 29936.92 - 5149.66 - *emsq * 54087.36 + 
			    eoc * 31324.56;
		} else {
		    g520 = 1464.74 - *eccm * 4664.75 + *emsq * 3763.64;
		}
	    }
	    if (*eccm < .7) {
		g533 = *eccm * 4988.61 - 919.2277 - *emsq * 9064.77 + eoc * 
			5542.21;
		g521 = *eccm * 4568.6173 - 822.71072 - *emsq * 8491.4146 + 
			eoc * 5337.524;
		g532 = *eccm * 4690.25 - 853.666 - *emsq * 8624.77 + eoc * 
			5341.4;
	    } else {
		g533 = *eccm * 161616.52 - 37995.78 - *emsq * 229838.2 + eoc *
			 109377.94;
		g521 = *eccm * 218913.95 - 51752.104 - *emsq * 309468.16 + 
			eoc * 146349.42;
		g532 = *eccm * 170470.89 - 40023.88 - *emsq * 242699.48 + eoc 
			* 115605.82;
	    }
	    sini2 = *sinim * *sinim;
	    f220 = (*cosim * 2. + 1. + cosisq) * .75;
	    f221 = sini2 * 1.5;
	    f321 = *sinim * 1.875 * (1. - *cosim * 2. - cosisq * 3.);
	    f322 = *sinim * -1.875 * (*cosim * 2. + 1. - cosisq * 3.);
	    f441 = sini2 * 35. * f220;
	    f442 = sini2 * 39.375 * sini2;
	    f522 = *sinim * 9.84375 * (sini2 * (1. - *cosim * 2. - cosisq * 
		    5.) + (*cosim * 4. - 2. + cosisq * 6.) * .33333333);
	    f523 = *sinim * (sini2 * 4.92187512 * (-2. - *cosim * 4. + cosisq 
		    * 10.) + (*cosim * 2. + 1. - cosisq * 3.) * 6.56250012);
	    f542 = *sinim * 29.53125 * (2. - *cosim * 8. + cosisq * (*cosim * 
		    8. - 12. + cosisq * 10.));
	    f543 = *sinim * 29.53125 * (-2. - *cosim * 8. + cosisq * (*cosim *
		     8. + 12. - cosisq * 10.));
	    xno2 = *xn * *xn;
	    ainv2 = aonv * aonv;
	    temp1 = xno2 * 3. * ainv2;
	    temp = temp1 * root22;
	    *d2201 = temp * f220 * g201;
	    *d2211 = temp * f221 * g211;
	    temp1 *= aonv;
	    temp = temp1 * root32;
	    *d3210 = temp * f321 * g310;
	    *d3222 = temp * f322 * g322;
	    temp1 *= aonv;
	    temp = temp1 * 2. * root44;
	    *d4410 = temp * f441 * g410;
	    *d4422 = temp * f442 * g422;
	    temp1 *= aonv;
	    temp = temp1 * root52;
	    *d5220 = temp * f522 * g520;
	    *d5232 = temp * f523 * g532;
	    temp = temp1 * 2. * root54;
	    *d5421 = temp * f542 * g521;
	    *d5433 = temp * f543 * g533;
	    d__1 = *mo + *nodeo + *nodeo - theta - theta;
	    d__2 = twopi_();
	    *xlamo = d_mod(&d__1, &d__2);
	    *xfact = *mdot + *dmdt + (*nodedot + *dnodt - rptim) * 2. - *no;
	    *eccm = emo;
	    *emsq = emsqo;
	}
	if (*irez == 1) {

/*           SYNCHRONOUS RESONANCE TERMS */

	    g200 = *emsq * (*emsq * .8125 - 2.5) + 1.;
	    g310 = *emsq * 2. + 1.;
	    g300 = *emsq * (*emsq * 6.60937 - 6.) + 1.;
	    f220 = (*cosim + 1.) * .75 * (*cosim + 1.);
	    f311 = *sinim * .9375 * *sinim * (*cosim * 3. + 1.) - (*cosim + 
		    1.) * .75;
	    f330 = *cosim + 1.;
	    f330 = f330 * 1.875 * f330 * f330;
	    *del1 = *xn * 3. * *xn * aonv * aonv;
	    *del2 = *del1 * 2. * f220 * g200 * q22;
	    *del3 = *del1 * 3. * f330 * g300 * q33 * aonv;
	    *del1 = *del1 * f311 * g310 * q31 * aonv;
	    d__1 = *mo + *nodeo + *argpo - theta;
	    d__2 = twopi_();
	    *xlamo = d_mod(&d__1, &d__2);
	    *xfact = *mdot + *xpidot - rptim + *dmdt + *domdt + *dnodt - *no;
	}

/*        FOR SGP4, INITIALIZE THE INTEGRATOR */

	*xli = *xlamo;
	*xni = *no;
	*atime = 0.;
	*xn = *no + *dndt;
    }
    chkout_("ZZDSIN", (ftnlen)6);
    return 0;
} /* zzdsin_ */

