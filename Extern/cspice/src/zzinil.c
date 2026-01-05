/* zzinil.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZINIL ( SGP4 initializer ) */
/* Subroutine */ int zzinil_(doublereal *geophs, integer *opmode, doublereal *
	ecco, doublereal *epoch, doublereal *inclo, doublereal *no, 
	doublereal *ainv, doublereal *ao, doublereal *con41, doublereal *
	con42, doublereal *cosio, doublereal *cosio2, doublereal *eccsq, 
	doublereal *omeosq, doublereal *posq, doublereal *rp, doublereal *
	rteosq, doublereal *sinio, doublereal *gsto)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), cos(doublereal), pow_dd(doublereal *, doublereal 
	    *), sin(doublereal), d_int(doublereal *), d_mod(doublereal *, 
	    doublereal *);

    /* Local variables */
    doublereal adel, c1p2p;
    integer ids70;
    doublereal temp;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal tfrac, thgr70, c1, d1, j2;
    extern doublereal twopi_(void);
    doublereal ak, po, radday;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    doublereal del, xke, x2o3, ts70, fk5r, tut1;

/* $ Abstract */

/*     This subroutine initializes the SGP4 propagator. All the */
/*     initialization is consolidated here instead of having multiple */
/*     loops inside other routines. */

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

/*     GEOPYS         Gephysical constants array. */

/*     ECCO           Eccentricity. */

/*     EPOCH          TLE epoch time in days from */
/*                    1950-00-00 00:00:00.000000 TDB, i.e. */
/*                    1949-12-31 00:00:00.000000 TDB. */

/*     INCLO          Inclination */

/*     OPMODE         Flag indicating which technique */
/*                    to use to calculate sidereal time. */

/*     NO             Mean motion. */

/* $ Detailed_Output */

/*     NO             Mean motion. */

/*     AINV           1.0/A0 */

/*     AO             Semi major axis. */

/*     CON41          Value -CON42-COSIO2-COSIO2. */

/*     CON42          1.0 - 5.0*cos(inclination). */

/*     COSIO          Cosine of inclination. */

/*     COSIO2         COSIO squared. */

/*     ECCSQ          Eccentricity squared. */

/*     OMEOSQ         1.0 - ECCO * ECCO. */

/*     POSQ           Semi-parameter squared. */

/*     RP             Radius of perigee. */

/*     RTEOSQ         Square root of (1.0 - ECCO*ECCO). */

/*     SINIO          Sine of inclination. */

/*     GSTO           GST at time of observation. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) SPICE(UNKNOWNMODE) signals when the value of OPMODE */
/*        does not equal an assigned OPMODE value listed in */
/*        zzsgp4.inc. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is based on the INITL code by David Vallado */
/*     corresponding to "Revisiting Spacetrack Report #3" [4]. */
/*     The intent is to maintain the original Vallado algorithm, */
/*     changing code only to meet NAIF format standards and to */
/*     integrate with SPICELIB. */

/*        Removed getgravconst call, replaced with GEOPHS array. */

/*        Capitalize all variables. */

/*        ENDIF replaced with END IF. */

/*        RadPerDay     replaced with RADDAY */

/*        Opsmode       replaced with OPMODE */

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

/* -    SPICELIB Version 1.0.0, OCT-09-2014 (EDW) */

/*        Based on routine INITL, 28-JUN-2005, Vallado 2006 [4]. */

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
    chkin_("ZZINIL", (ftnlen)6);

/*     This code block replaces the call: */

/*     sgp4fix identify constants and allow alternate values. */

/*     CALL getgravconst( whichconst, tumin, */
/*     .                  mu, radiusearthkm, xke, */
/*     .                  j2, j3, j4, j3oj2 ) */

    j2 = geophs[0];
    xke = geophs[3];
    x2o3 = .66666666666666663;

/*     Calculate auxillary epoch quantities */

    *eccsq = *ecco * *ecco;
    *omeosq = 1. - *eccsq;
    *rteosq = sqrt(*omeosq);
    *cosio = cos(*inclo);
    *cosio2 = *cosio * *cosio;

/*     Un-KOZAI the mean motion */

    d__1 = xke / *no;
    ak = pow_dd(&d__1, &x2o3);
    d1 = j2 * .75 * (*cosio2 * 3. - 1.) / (*rteosq * *omeosq);
    del = d1 / (ak * ak);
    adel = ak * (1. - del * del - del * (del * 134. * del / 81. + 
	    .33333333333333331));
    del = d1 / (adel * adel);
    *no /= del + 1.;
    d__1 = xke / *no;
    *ao = pow_dd(&d__1, &x2o3);
    *sinio = sin(*inclo);
    po = *ao * *omeosq;
    *con42 = 1. - *cosio2 * 5.;
    *con41 = -(*con42) - *cosio2 - *cosio2;
    *ainv = 1. / *ao;
    *posq = po * po;
    *rp = *ao * (1. - *ecco);

/*     Calculate greenwich location at epoch */


/*     sgp4fix Modern approach to finding sidereal time */

    if (*opmode == 2) {

/*        Radians per day, earth rotation, 6.30038809866574D0. */

	radday = twopi_() * 1.002737909350795;
	temp = *epoch + 2433281.5;
	d__1 = temp - .5;
	tut1 = (d_int(&d__1) + .5 - 2451545.) / 36525.;
	d__1 = temp - .5;
	*gsto = tut1 * 628.331970688841 + 1.75336855923327 + tut1 * 
		6.77071394490334e-6 * tut1 - tut1 * 4.50876723431868e-10 * 
		tut1 * tut1 + radday * (temp - .5 - d_int(&d__1));
    } else if (*opmode == 1) {

/*        sgp4fix Use old way of finding GST */

/*        Count integer number of days from 0 jan 1970 */

	ts70 = *epoch - 7305.;
	ids70 = (integer) (ts70 + 1e-8);
	tfrac = ts70 - ids70;

/*        Find greenwich location at epoch */

	c1 = .0172027916940703639;
	thgr70 = 1.7321343856509374;
	fk5r = 5.07551419432269442e-15;
	c1p2p = c1 + twopi_();
	*gsto = thgr70 + c1 * ids70 + c1p2p * tfrac + ts70 * ts70 * fk5r;
    } else {
	setmsg_("Unknown value for OPMODE. Value # not coded in zzsgp4.inc.", 
		(ftnlen)58);
	errint_("#", opmode, (ftnlen)1);
	sigerr_("SPICE(UNKNOWNMODE)", (ftnlen)18);
	chkout_("ZZINIL", (ftnlen)6);
	return 0;
    }

/*     Check quadrants */

    d__1 = twopi_();
    *gsto = d_mod(gsto, &d__1);
    if (*gsto < 0.) {
	*gsto += twopi_();
    }
    chkout_("ZZINIL", (ftnlen)6);
    return 0;
} /* zzinil_ */

