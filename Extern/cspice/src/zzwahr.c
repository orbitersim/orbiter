/* zzwahr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b2 = 360.;

/* $Procedure      ZZWAHR ( SPICELIB private version of Newhalls' WAHR ) */
/* Subroutine */ int zzwahr_(doublereal *et, doublereal *dvnut)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer matrix[954]	/* was [9][106] */ = { 0,0,0,0,1,-171996,
	    -1742,92025,89,0,0,0,0,2,2062,2,-895,5,-2,0,2,0,1,46,0,-24,0,2,0,
	    -2,0,0,11,0,0,0,-2,0,2,0,2,-3,0,1,0,1,-1,0,-1,0,-3,0,0,0,0,-2,2,
	    -2,1,-2,0,1,0,2,0,-2,0,1,1,0,0,0,0,0,2,-2,2,-13187,-16,5736,-31,0,
	    1,0,0,0,1426,-34,54,-1,0,1,2,-2,2,-517,12,224,-6,0,-1,2,-2,2,217,
	    -5,-95,3,0,0,2,-2,1,129,1,-70,0,2,0,0,-2,0,48,0,1,0,0,0,2,-2,0,
	    -22,0,0,0,0,2,0,0,0,17,-1,0,0,0,1,0,0,1,-15,0,9,0,0,2,2,-2,2,-16,
	    1,7,0,0,-1,0,0,1,-12,0,6,0,-2,0,0,2,1,-6,0,3,0,0,-1,2,-2,1,-5,0,3,
	    0,2,0,0,-2,1,4,0,-2,0,0,1,2,-2,1,4,0,-2,0,1,0,0,-1,0,-4,0,0,0,2,1,
	    0,-2,0,1,0,0,0,0,0,-2,2,1,1,0,0,0,0,1,-2,2,0,-1,0,0,0,0,1,0,0,2,1,
	    0,0,0,-1,0,0,1,1,1,0,0,0,0,1,2,-2,0,-1,0,0,0,0,0,2,0,2,-2274,-2,
	    977,-5,1,0,0,0,0,712,1,-7,0,0,0,2,0,1,-386,-4,200,0,1,0,2,0,2,
	    -301,0,129,-1,1,0,0,-2,0,-158,0,-1,0,-1,0,2,0,2,123,0,-53,0,0,0,0,
	    2,0,63,0,-2,0,1,0,0,0,1,63,1,-33,0,-1,0,0,0,1,-58,-1,32,0,-1,0,2,
	    2,2,-59,0,26,0,1,0,2,0,1,-51,0,27,0,0,0,2,2,2,-38,0,16,0,2,0,0,0,
	    0,29,0,-1,0,1,0,2,-2,2,29,0,-12,0,2,0,2,0,2,-31,0,13,0,0,0,2,0,0,
	    26,0,-1,0,-1,0,2,0,1,21,0,-10,0,-1,0,0,2,1,16,0,-8,0,1,0,0,-2,1,
	    -13,0,7,0,-1,0,2,2,1,-10,0,5,0,1,1,0,-2,0,-7,0,0,0,0,1,2,0,2,7,0,
	    -3,0,0,-1,2,0,2,-7,0,3,0,1,0,2,2,2,-8,0,3,0,1,0,0,2,0,6,0,0,0,2,0,
	    2,-2,2,6,0,-3,0,0,0,0,2,1,-6,0,3,0,0,0,2,2,1,-7,0,3,0,1,0,2,-2,1,
	    6,0,-3,0,0,0,0,-2,1,-5,0,3,0,1,-1,0,0,0,5,0,0,0,2,0,2,0,1,-5,0,3,
	    0,0,1,0,-2,0,-4,0,0,0,1,0,-2,0,0,4,0,0,0,0,0,0,1,0,-4,0,0,0,1,1,0,
	    0,0,-3,0,0,0,1,0,2,0,0,3,0,0,0,1,-1,2,0,2,-3,0,1,0,-1,-1,2,2,2,-3,
	    0,1,0,-2,0,0,0,1,-2,0,1,0,3,0,2,0,2,-3,0,1,0,0,-1,2,2,2,-3,0,1,0,
	    1,1,2,0,2,2,0,-1,0,-1,0,2,-2,1,-2,0,1,0,2,0,0,0,1,2,0,-1,0,1,0,0,
	    0,2,-2,0,1,0,3,0,0,0,0,2,0,0,0,0,0,2,1,2,2,0,-1,0,-1,0,0,0,2,1,0,
	    -1,0,1,0,0,-4,0,-1,0,0,0,-2,0,2,2,2,1,0,-1,0,-1,0,2,4,2,-2,0,1,0,
	    2,0,0,-4,0,-1,0,0,0,1,1,2,-2,2,1,0,-1,0,1,0,2,2,1,-1,0,1,0,-2,0,2,
	    4,2,-1,0,1,0,-1,0,4,0,2,1,0,0,0,1,-1,0,-2,0,1,0,0,0,2,0,2,-2,1,1,
	    0,-1,0,2,0,2,2,2,-1,0,0,0,1,0,0,2,1,-1,0,0,0,0,0,4,-2,2,1,0,0,0,3,
	    0,2,-2,2,1,0,0,0,1,0,2,-2,0,-1,0,0,0,0,1,2,0,1,1,0,0,0,-1,-1,0,2,
	    1,1,0,0,0,0,0,-2,0,1,-1,0,0,0,0,0,2,-1,2,-1,0,0,0,0,1,0,2,0,-1,0,
	    0,0,1,0,-2,-2,0,-1,0,0,0,0,-1,2,0,1,-1,0,0,0,1,1,0,-2,1,-1,0,0,0,
	    1,0,-2,2,0,-1,0,0,0,2,0,0,2,0,1,0,0,0,0,0,2,4,2,-1,0,0,0,0,1,0,1,
	    0,1,0,0,0 };

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    double d_mod(doublereal *, doublereal *), cos(doublereal), sin(doublereal)
	    ;

    /* Local variables */
    static doublereal dddj;
    static integer i__, j;
    static doublereal t, angle[5], rasec, factr, angrt[5], argrt, d0, d1, d2, 
	    d3, f0, f1, f2, f3, l0, l1, l2, l3;
    extern doublereal twopi_(void);
    static doublereal ce, dd, dj, cl;
    extern doublereal pi_(void);
    static doublereal radian, cosang, oneday, sinang, dd2, mg0, mg1, dtwopi, 
	    mg2, mg3, lp0, lp1, lp2, lp3, arg, dpi;
    extern doublereal spd_(void);

/* $ Abstract */

/*     Calculates nutation angles delta psi and delta epsilon,  and */
/*     their rates of change, referred to the ecliptic of date, from */
/*     the Wahr series (Table 1,'Proposal to the IAU Working Group */
/*     on Nutation', John M. Wahr and Martin L. Smith 1979) */

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

/*     NUTATIONS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris Time for which nutations are sought */
/*     DVNUT      O   Nutation angles and their rates. */

/* $ Detailed_Input */

/*     ET         is the epoch for which nutation angles are being */
/*                requested expressed in TDB seconds past the epoch */
/*                of J2000. */

/* $ Detailed_Output */

/*     DVNUT      are the nutation angles and their derivatives. */
/*                Following the notation on page 112 of the */
/*                Explanatory Supplement to the Astronomical */
/*                Almanac we have */

/*                DVNUT(1) = Psi------nutation in longitude (radians) */
/*                DVNUT(2) = Epsilon--nutation in obliquity (radians) */
/*                DVNUT(3) = dPsi/dt     (radians/second) */
/*                DVNUT(4) = dEpsilon/dt (radians/second) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes the angles required for computing the */
/*     transformation from the mean of date frame for the earth */
/*     to the true of date frame of the earth. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     Explanatory Supplement to the Astronomical Almanac edited */
/*     by P. Kenneth Siedelmann. (1992) (University Science */
/*     Books, Mill Valley CA) pp. 111-116 */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB) */

/*        Corrected typos in comments. Reordered header sections. */

/* -    SPICELIB Version 1.0.0, 15-JUL-1997 (WLT) */

/*        This routine was adapted from a routine provided by */
/*        Skip Newhall.  Skip's notes indicate that he obtained this */
/*        from Jay Lieske and Mylse Standish.  The actual notes */
/*        from the original routine WAHR are given here. */

/*           Lieske 3/91.  NUTATION in the IAU J2000 system.  Univac */
/*           version obtained from Myles Standish, (subroutine WAHR) */
/*           who had obtained it from USNO.  Re-ordered terms to match */
/*           Astronomical Almanac 1984 table S23-S25 and corrected */
/*           the rate for dPsi in the 0 0 2 -2 2 term.  Eliminated */
/*           the equivalences, common block and added necessary SAVEs. */
/*           Corrected the fundamental angles (L, L', F, D, Node) to */
/*           match Almanac. */

/*        In the current routine the various angles L, L', F, D, and */
/*        Node (MG) are computed using the actual values given */
/*        in the Explanatory Supplement. */

/*        Note that there is an error in the Explanatory supplement */
/*        for the Node term.  The Explanatory Supplement (page 114) has */

/*          OMEGA = 135 degrees 2 minutes 40.280 seconds */
/*                +  etc. */

/*        The correct formulation should be: */

/*          OMEGA = 125 degrees 2 minutes 40.280 seconds */
/*                +  etc. */

/*        This is the value used in this routine.  The verification of */
/*        this error is courtesy of Myles Standish. */


/* -& */

/*     SPICELIB Functions */


/*     Parameters */

/*     NTERM is the number of SIN and COSINE terms used in the */
/*     computation of Delta Psi and Delta epsilon */


/*     The parameters below stand for */

/*        revolutions */
/*        degrees */
/*        minutes */
/*        seconds */
/*        julian century */
/*        julian century ** 2 */
/*        julian century ** 3 */

/*     These parameters are needed for converting the quantities */
/*     on page 114 of the Explanatory supplement from revolutions, */
/*     degrees, minutes and seconds / century, century**2 and century**3 */
/*     to degrees, degrees/day, degrees/(0.0001 days)**2 and */
/*     degrees/(0.0001 days)**3. */


/*     The next set of parameters is an enumeration of the various */
/*     angles needed in the computation of nutations. */


/*     Local Variables. */


/*     Below are the coefficients for the various periods of the */
/*     nutation model.  There does not appear to be any particular reason */
/*     for the ordering selected.  The n'th row corresponds to the n'th */
/*     period listed above each data statement. */

/* >> Periods: 6798.4, 3399.2, 1305.5, 1095.2, 1615.7, 3232.9, 6786.3, */
/*             943.2,  182.6,  365.3,  121.7,  365.2,  177.8,  205.9, */
/*             173.3,  182.6,  386.0,   91.3,  346.6 */


/*     Periods: 199.8, 346.6, 212.3, 119.6, 411.8, 131.7, 169.0, 329.8, */
/*              409.2, 388.3, 117.5,  13.7,  27.6,  13.6,   9.1,  31.8, */
/*               27.1,  14.8,  27.7 */


/*     Periods: 27.4, 9.6,  9.1,  7.1, 13.8, 23.9, 6.9, 13.6, 27.0, 32.0, */
/*              31.7, 9.5, 34.8, 13.2, 14.2,  5.6, 9.6, 12.8, 14.8 */


/*     Periods: 7.1, 23.9, 14.7, 29.8, 6.9, 15.4, 26.9, 29.5, 25.6, 9.1, */
/*              9.4,  9.8, 13.7,  5.5, 7.2,  8.9, 32.6, 13.8, 27.8 */

/*      Periods: 9.2,  9.3, 27.3, 10.1, 14.6,  5.8, 15.9, 22.5,  5.6, */
/*               7.3,  9.1, 29.3, 12.8,  4.7,  9.6, 12.7,  8.7, 23.8, */
/*              13.1 */

/*     Periods: 35.0, 13.6, 25.4, 14.2, 9.5, 14.2, 34.7, 32.8, 7.1, 4.8, */
/*              27.3 */
    if (first) {
	first = FALSE_;
	dpi = pi_();
	dtwopi = twopi_();
	radian = 180. / dpi;
	rasec = radian * 3600.;
	factr = rasec * 1e4;
	oneday = spd_();

/*        The following values are direct conversions to degrees from */
/*        page 114 of the Explanatory Supplement to the Astronomical */
/*        Almanac. */

/*        L0 through L3 are the coefficients for l---the mean longitude */
/*        of the Moon minus the mean longitude of the Moon's perigee. */
/*        Units for the various terms: */

/*           L0      degrees */
/*           L1      degrees/day */
/*           L2      degrees/(0.0001 days)**2 */
/*           L3      degrees/(0.0001 days)**3 */

	l0 = 134.96298138888886;
	l1 = 13.064992947243136;
	l2 = 6.5192872572139397e-4;
	l3 = 3.6484365631332527e-7;

/*        LP0 through LP3 are the coefficients for l'---the mean */
/*        longitude of the Sun minus the mean longitude of the Sun's */
/*        perigee. Units for the various terms: */

/*           LP0      degrees */
/*           LP1      degrees/day */
/*           LP2      degrees/(0.0001 days)**2 */
/*           LP3      degrees/(0.0001 days)**3 */

	lp0 = 357.52772333333331;
	lp1 = .98560028309377146;
	lp2 = -1.201414483363923e-5;
	lp3 = -6.8408185558748495e-8;

/*        F0 through F3 are the coefficients for F---the mean longitude */
/*        of the Moon minus the mean longitude of the Moon's node. Units */
/*        for the various terms: */

/*           F0      degrees */
/*           F1      degrees/day */
/*           F2      degrees/(0.0001 days)**2 */
/*           F3      degrees/(0.0001 days)**3 */

	f0 = 93.271910277777778;
	f1 = 13.229350240603848;
	f2 = -2.760338267929901e-4;
	f3 = 6.2707503428852773e-8;

/*        D0 through D3 are the coefficients for D---the mean longitude */
/*        of the Moon minus the mean longitude of the Sun. Units */
/*        for the various terms: */

/*           D0      degrees */
/*           D1      degrees/day */
/*           D2      degrees/(0.0001 days)**2 */
/*           D3      degrees/(0.0001 days)**3 */

	d0 = 297.85036305555559;
	d1 = 12.190749116495549;
	d2 = -1.4348262053484912e-4;
	d3 = 1.0831296046801845e-7;

/*        MG0 through MG3 are the coefficients for Omega---the longitude */
/*        of the mean ascending node of the lunar orbit on the ecliptic */
/*        measured from the mean equinox of date.  NOTE: The constant */
/*        term MG0 is correct.  The value */
/*               o */
/*            135 02' 40".280 */

/*        given in the Explanatory Supplement page 114 has a typo.  The */
/*        correct value is the one used here: */

/*               o */
/*            125 02' 40".280 */

/*           MG0      degrees */
/*           MG1      degrees/day */
/*           MG2      degrees/(0.0001 days)**2 */
/*           MG3      degrees/(0.0001 days)**3 */

	mg0 = 125.04452222222223;
	mg1 = -.052953764841432813;
	mg2 = 1.5522608272925558e-4;
	mg3 = 4.5605457039165659e-8;
    }

/*     Compute all of the various time components.  DJ is the delta */
/*     in the Julian date from the J2000 epoch. */

    dj = *et / oneday;
    dd = dj / 1e4;
    dddj = dd / 1e4;
    dd2 = dd * dd;
    t = dj / 365250.;

/*     Now compute all of the various angles and their rates */
/*     at the current epoch */

    angle[0] = l0 + dj * l1 + (l2 + dd * l3) * dd2;
    angle[1] = lp0 + dj * lp1 + (lp2 + dd * lp3) * dd2;
    angle[2] = f0 + dj * f1 + (f2 + dd * f3) * dd2;
    angle[3] = d0 + dj * d1 + (d2 + dd * d3) * dd2;
    angle[4] = mg0 + dj * mg1 + (mg2 + dd * mg3) * dd2;
    angrt[0] = l1 + dddj * (l2 * 2. + dd * 3. * l3);
    angrt[1] = lp1 + dddj * (lp2 * 2. + dd * 3. * lp3);
    angrt[2] = f1 + dddj * (f2 * 2. + dd * 3. * f3);
    angrt[3] = d1 + dddj * (d2 * 2. + dd * 3. * d3);
    angrt[4] = mg1 + dddj * (mg2 * 2. + dd * 3. * mg3);

/*     Wrap all of the angles and rates to range from 0 to 360, then */
/*     convert to radians. */

    for (j = 1; j <= 5; ++j) {
	angle[(i__1 = j - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("angle", i__1, 
		"zzwahr_", (ftnlen)574)] = d_mod(&angle[(i__2 = j - 1) < 5 && 
		0 <= i__2 ? i__2 : s_rnge("angle", i__2, "zzwahr_", (ftnlen)
		574)], &c_b2);
	angrt[(i__1 = j - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("angrt", i__1, 
		"zzwahr_", (ftnlen)575)] = d_mod(&angrt[(i__2 = j - 1) < 5 && 
		0 <= i__2 ? i__2 : s_rnge("angrt", i__2, "zzwahr_", (ftnlen)
		575)], &c_b2);
	angle[(i__1 = j - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("angle", i__1, 
		"zzwahr_", (ftnlen)577)] = angle[(i__2 = j - 1) < 5 && 0 <= 
		i__2 ? i__2 : s_rnge("angle", i__2, "zzwahr_", (ftnlen)577)] /
		 radian;
	angrt[(i__1 = j - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("angrt", i__1, 
		"zzwahr_", (ftnlen)578)] = angrt[(i__2 = j - 1) < 5 && 0 <= 
		i__2 ? i__2 : s_rnge("angrt", i__2, "zzwahr_", (ftnlen)578)] /
		 radian;
    }

/*     Zero out the components of the nutation array */

    for (j = 1; j <= 4; ++j) {
	dvnut[(i__1 = j - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge("dvnut", i__1, 
		"zzwahr_", (ftnlen)584)] = 0.;
    }

/*     Now we accumulate the various terms of Delta Psi and Delta */
/*     epsilon as expressed on page 115 of the Green Book */
/*     (Explanatory Supplement to the Astronomical Almanac). */

    for (i__ = 1; i__ <= 106; ++i__) {
	arg = 0.;
	argrt = 0.;
	for (j = 1; j <= 5; ++j) {
	    if (matrix[(i__1 = j + i__ * 9 - 10) < 954 && 0 <= i__1 ? i__1 : 
		    s_rnge("matrix", i__1, "zzwahr_", (ftnlen)597)] != 0) {
		arg += matrix[(i__1 = j + i__ * 9 - 10) < 954 && 0 <= i__1 ? 
			i__1 : s_rnge("matrix", i__1, "zzwahr_", (ftnlen)598)]
			 * angle[(i__2 = j - 1) < 5 && 0 <= i__2 ? i__2 : 
			s_rnge("angle", i__2, "zzwahr_", (ftnlen)598)];
		argrt += matrix[(i__1 = j + i__ * 9 - 10) < 954 && 0 <= i__1 ?
			 i__1 : s_rnge("matrix", i__1, "zzwahr_", (ftnlen)599)
			] * angrt[(i__2 = j - 1) < 5 && 0 <= i__2 ? i__2 : 
			s_rnge("angrt", i__2, "zzwahr_", (ftnlen)599)];
		arg = d_mod(&arg, &dtwopi);
	    }
	}
	cl = (doublereal) matrix[(i__1 = i__ * 9 - 4) < 954 && 0 <= i__1 ? 
		i__1 : s_rnge("matrix", i__1, "zzwahr_", (ftnlen)604)];
	if (matrix[(i__1 = i__ * 9 - 3) < 954 && 0 <= i__1 ? i__1 : s_rnge(
		"matrix", i__1, "zzwahr_", (ftnlen)606)] != 0) {
	    cl += matrix[(i__1 = i__ * 9 - 3) < 954 && 0 <= i__1 ? i__1 : 
		    s_rnge("matrix", i__1, "zzwahr_", (ftnlen)607)] * t;
	}
	ce = (doublereal) matrix[(i__1 = i__ * 9 - 2) < 954 && 0 <= i__1 ? 
		i__1 : s_rnge("matrix", i__1, "zzwahr_", (ftnlen)610)];
	if (matrix[(i__1 = i__ * 9 - 1) < 954 && 0 <= i__1 ? i__1 : s_rnge(
		"matrix", i__1, "zzwahr_", (ftnlen)612)] != 0) {
	    ce += matrix[(i__1 = i__ * 9 - 1) < 954 && 0 <= i__1 ? i__1 : 
		    s_rnge("matrix", i__1, "zzwahr_", (ftnlen)613)] * t;
	}
	cosang = cos(arg);
	sinang = sin(arg);
	dvnut[0] += cl * sinang / factr;
	dvnut[1] += ce * cosang / factr;
	dvnut[2] += cl * cosang * argrt / factr;
	dvnut[3] -= ce * sinang * argrt / factr;
    }

/*     Finally convert DVNUT(3) and DVNUT(4) to radians/second */

    dvnut[2] /= oneday;
    dvnut[3] /= oneday;
    return 0;
} /* zzwahr_ */

