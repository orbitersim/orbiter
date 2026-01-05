/* zzdspr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZDSPR ( SGP4 deep space long period ) */
/* Subroutine */ int zzdspr_(integer *opmode, doublereal *e3, doublereal *ee2,
	 doublereal *peo, doublereal *pgho, doublereal *pho, doublereal *
	pinco, doublereal *plo, doublereal *se2, doublereal *se3, doublereal *
	sgh2, doublereal *sgh3, doublereal *sgh4, doublereal *sh2, doublereal 
	*sh3, doublereal *si2, doublereal *si3, doublereal *sl2, doublereal *
	sl3, doublereal *sl4, doublereal *t, doublereal *xgh2, doublereal *
	xgh3, doublereal *xgh4, doublereal *xh2, doublereal *xh3, doublereal *
	xi2, doublereal *xi3, doublereal *xl2, doublereal *xl3, doublereal *
	xl4, doublereal *zmol, doublereal *zmos, doublereal *inclo, logical *
	doinit, doublereal *eccp, doublereal *inclp, doublereal *nodep, 
	doublereal *argpp, doublereal *mp)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal), d_mod(doublereal *, doublereal *)
	    , atan2(doublereal, doublereal);

    /* Local variables */
    doublereal dalf, dbet, pinc, sghl, sghs, xnoh, alfdp;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal betdp, cosip, sinip, cosop, f2, f3, sinop, sinzf;
    extern doublereal twopi_(void);
    doublereal pe, ph;
    extern doublereal pi_(void);
    doublereal pl, zf, zm;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    doublereal pgh, dls, sel, shl, ses, sil, shs, sis, sll, zel, sls, zes, 
	    znl, xls, zns;

/* $ Abstract */

/*     Provide deep space long period periodic contributions to the mean */
/*     elements. By design, these periodics are zero at epoch. This used */
/*     to be dscom which included initialization, but it's really a */
/*     recurring function. */

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

/*     OPMODE     Flag indicating which technique */
/*                to use to calculate sidereal time. */

/*     E3         Internal SGD4 parameter. */

/*     EE2        Internal SGD4 parameter. */

/*     PEO        Internal SGD4 parameter. */

/*     PGHO       Internal SGD4 parameter. */

/*     PHO        Internal SGD4 parameter. */

/*     PINCO      Internal SGD4 parameter. */

/*     PLO        Internal SGD4 parameter. */

/*     SE2        Internal SGD4 parameter. */

/*     SE3        Internal SGD4 parameter. */

/*     SGH2       Internal SGD4 parameter. */

/*     SGH3       Internal SGD4 parameter. */

/*     SGH4       Internal SGD4 parameter. */

/*     SH2        Internal SGD4 parameter. */

/*     SH3        Internal SGD4 parameter. */

/*     SI2        Internal SGD4 parameter. */

/*     SI3        Internal SGD4 parameter. */

/*     SL2        Internal SGD4 parameter. */

/*     SL3        Internal SGD4 parameter. */

/*     SL4        Internal SGD4 parameter. */

/*     T          Time for state evaluation. */

/*     XGH2       Internal SGD4 parameter. */

/*     XGH3       Internal SGD4 parameter. */

/*     XGH4       Internal SGD4 parameter. */

/*     XH2        Internal SGD4 parameter. */

/*     XH3        Internal SGD4 parameter. */

/*     XI2        Internal SGD4 parameter. */

/*     XI3        Internal SGD4 parameter. */

/*     XL2        Internal SGD4 parameter. */

/*     XL3        Internal SGD4 parameter. */

/*     XL4        Internal SGD4 parameter. */

/*     ZMOL       Internal SGD4 parameter. */

/*     ZMOS       Internal SGD4 parameter. */

/*     INCLO      Unused argument. Maintained in call for historical */
/*                reference. */

/*     DOINIT     Flag indicating initialization state. True to */
/*                initialize, false otherwise. */

/*     ECCP       Eccentricity. */

/*     INCLP      Inclination. */

/*     NODEP      Right ascension of  ascending node. */

/*     ARGPP      Argument of periapsis. */

/*     MP         Mean anomaly. */

/* $ Detailed_Output */

/*     ECCP       Calculated perturbed eccentricity. */

/*     INCLP      Calculated perturbed inclination. */

/*     NODEP      Calculated perturbed right ascension of */
/*                ascending node. */

/*     ARGPP      Calculated perturbed argument of periapsis. */

/*     MP         Calculated perturbed mean anomaly. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is based on the DPPER code by David Vallado */
/*     corresponding to "Revisiting Spacetrack Report #3" [4]. */
/*     The intent is to maintain the original Vallado algorithm, */
/*     changing code only to meet NAIF format standards and to */
/*     integrate with SPICELIB. */

/*        Capitalize all variables. */

/*        ENDIF replaced with END IF. */

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

/* -    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB) */

/*        Tweaked wording of header Abstract section. */
/*        Fixed typos in comments. */

/* -    SPICELIB Version 1.0.0, SEP-15-2014 (EDW) */

/*        Based on routine DDPER, 28-JUN-2005, Vallado 2006 [4]. */

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
    chkin_("ZZDSPR", (ftnlen)6);

/*     Local constants. */

    zes = .01675;
    zel = .0549;
    zns = 1.19459e-5;
    znl = 1.5835218e-4;

/*     Calculate time varying periodics. */

    zm = *zmos + zns * *t;
    if (*doinit) {
	zm = *zmos;
    }
    zf = zm + zes * 2. * sin(zm);
    sinzf = sin(zf);
    f2 = sinzf * .5 * sinzf - .25;
    f3 = sinzf * -.5 * cos(zf);
    ses = *se2 * f2 + *se3 * f3;
    sis = *si2 * f2 + *si3 * f3;
    sls = *sl2 * f2 + *sl3 * f3 + *sl4 * sinzf;
    sghs = *sgh2 * f2 + *sgh3 * f3 + *sgh4 * sinzf;
    shs = *sh2 * f2 + *sh3 * f3;
    zm = *zmol + znl * *t;
    if (*doinit) {
	zm = *zmol;
    }
    zf = zm + zel * 2. * sin(zm);
    sinzf = sin(zf);
    f2 = sinzf * .5 * sinzf - .25;
    f3 = sinzf * -.5 * cos(zf);
    sel = *ee2 * f2 + *e3 * f3;
    sil = *xi2 * f2 + *xi3 * f3;
    sll = *xl2 * f2 + *xl3 * f3 + *xl4 * sinzf;
    sghl = *xgh2 * f2 + *xgh3 * f3 + *xgh4 * sinzf;
    shl = *xh2 * f2 + *xh3 * f3;
    pe = ses + sel;
    pinc = sis + sil;
    pl = sls + sll;
    pgh = sghs + sghl;
    ph = shs + shl;
    if (! (*doinit)) {
	pe -= *peo;
	pinc -= *pinco;
	pl -= *plo;
	pgh -= *pgho;
	ph -= *pho;
	*inclp += pinc;
	*eccp += pe;
	sinip = sin(*inclp);
	cosip = cos(*inclp);

/*        Apply periodics directly. */

/*        sgp4fix for lyddane choice */

/*        strn3 used original inclination - this is technically */
/*        feasible */

/*        gsfc used perturbed inclination - also technically feasible */
/*        probably best to readjust the 0.2 limit value and limit */
/*        discontinuity */

/*        0.2 rad = 11.45916 deg */

/*        use next line for original strn3 approach and original */
/*        inclination */

/*            IF (INCLO.GE.0.2D0) THEN */

/*        use next line for gsfc version and perturbed inclination */

	if (*inclp >= .2) {
	    ph /= sinip;
	    pgh -= cosip * ph;
	    *argpp += pgh;
	    *nodep += ph;
	    *mp += pl;
	} else {

/*           Apply periodics with Lyddane modification. */

	    sinop = sin(*nodep);
	    cosop = cos(*nodep);
	    alfdp = sinip * sinop;
	    betdp = sinip * cosop;
	    dalf = ph * cosop + pinc * cosip * sinop;
	    dbet = -ph * sinop + pinc * cosip * cosop;
	    alfdp += dalf;
	    betdp += dbet;
	    d__1 = twopi_();
	    *nodep = d_mod(nodep, &d__1);

/*           sgp4fix for afspc written intrinsic functions */
/*           NODEP used without a trigonometric function ahead */

	    if (*nodep < 0. && *opmode == 1) {
		*nodep += twopi_();
	    }
	    xls = *mp + *argpp + cosip * *nodep;
	    dls = pl + pgh - pinc * *nodep * sinip;
	    xls += dls;
	    xnoh = *nodep;
	    *nodep = atan2(alfdp, betdp);

/*           sgp4fix for afspc written intrinsic functions */
/*           NODEP used without a trigonometric function ahead */

	    if (*nodep < 0. && *opmode == 1) {
		*nodep += twopi_();
	    }
	    if ((d__1 = xnoh - *nodep, abs(d__1)) > pi_()) {
		if (*nodep < xnoh) {
		    *nodep += twopi_();
		} else {
		    *nodep -= twopi_();
		}
	    }
	    *mp += pl;
	    *argpp = xls - *mp - cosip * *nodep;
	}
    }
    chkout_("ZZDSPR", (ftnlen)6);
    return 0;
} /* zzdspr_ */

