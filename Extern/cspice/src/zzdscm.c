/* zzdscm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZDSCM ( SGP4 deep space common calculations ) */
/* Subroutine */ int zzdscm_(doublereal *epoch, doublereal *eccp, doublereal *
	argpp, doublereal *tc, doublereal *inclp, doublereal *nodep, 
	doublereal *np, doublereal *snodm, doublereal *cnodm, doublereal *
	sinim, doublereal *cosim, doublereal *sinomm, doublereal *cosomm, 
	doublereal *day, doublereal *e3, doublereal *ee2, doublereal *eccm, 
	doublereal *emsq, doublereal *gam, doublereal *peo, doublereal *pgho, 
	doublereal *pho, doublereal *pinco, doublereal *plo, doublereal *
	rtemsq, doublereal *se2, doublereal *se3, doublereal *sgh2, 
	doublereal *sgh3, doublereal *sgh4, doublereal *sh2, doublereal *sh3, 
	doublereal *si2, doublereal *si3, doublereal *sl2, doublereal *sl3, 
	doublereal *sl4, doublereal *s1, doublereal *s2, doublereal *s3, 
	doublereal *s4, doublereal *s5, doublereal *s6, doublereal *s7, 
	doublereal *ss1, doublereal *ss2, doublereal *ss3, doublereal *ss4, 
	doublereal *ss5, doublereal *ss6, doublereal *ss7, doublereal *sz1, 
	doublereal *sz2, doublereal *sz3, doublereal *sz11, doublereal *sz12, 
	doublereal *sz13, doublereal *sz21, doublereal *sz22, doublereal *
	sz23, doublereal *sz31, doublereal *sz32, doublereal *sz33, 
	doublereal *xgh2, doublereal *xgh3, doublereal *xgh4, doublereal *xh2,
	 doublereal *xh3, doublereal *xi2, doublereal *xi3, doublereal *xl2, 
	doublereal *xl3, doublereal *xl4, doublereal *xn, doublereal *z1, 
	doublereal *z2, doublereal *z3, doublereal *z11, doublereal *z12, 
	doublereal *z13, doublereal *z21, doublereal *z22, doublereal *z23, 
	doublereal *z31, doublereal *z32, doublereal *z33, doublereal *zmol, 
	doublereal *zmos)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal), sqrt(doublereal), d_mod(
	    doublereal *, doublereal *), atan2(doublereal, doublereal);

    /* Local variables */
    doublereal ctem, stem, xnoi;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer lsflg;
    doublereal a1, a2, a3, a4, a5, a6, a7, a8, a9, zcosg, zcosh, zcosi, zsing,
	     zsinh, zsini;
    extern doublereal twopi_(void);
    doublereal x1, x2, x3, x4, x5, x6, x7, x8, a10, cc, betasq, xnodce, zx, 
	    zy;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal c1l, zcosgl, zcoshl, zcosil, zsingl, zsinhl, zcosgs, zsinil, 
	    zcosis, zsings, zsinis;
    extern logical return_(void);
    doublereal zel, zes, c1ss;

/* $ Abstract */

/*     This subroutine provides deep space common items used by both the */
/*     secular and periodics subroutines. */

/*     This routine previously had the name DPPER, but the functions */
/*     inside weren't well organized. */

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
/* $ Brief_I/O */

/*    See Detailed_input and Detailed_Output. */

/* $ Detailed_Input */

/*    EPOCH       Epoch of TLE set as Julian day value. */

/*    EP          Eccentricity */

/*    ARGPP       Argument of perigee */

/*    TC          Minutes past EPOCH, nominally zero. */

/*    INCLP       Inclination */

/*    NODEP       Right ascension of ascending node */

/*    NP          Mean motion */

/* $ Detailed_Output */

/*    SINIM       SIN of mean inclination. */

/*    COSIM       COS of mean inclination. */

/*    SINOMM      Internal SGD4 parameter. */

/*    COSOMM      Internal SGD4 parameter. */

/*    SNODM       Internal SGD4 parameter. */

/*    CNODM       Internal SGD4 parameter. */

/*    DAY         Internal SGD4 parameter. */

/*    E3          Internal SGD4 parameter. */

/*    EE2         Internal SGD4 parameter. */

/*    EM          Eccentricity. */

/*    EMSQ        Eccentricity squared. */

/*    GAM         Internal SGD4 parameter. */

/*    PEO         Internal SGD4 parameter. */

/*    PGHO        Internal SGD4 parameter. */

/*    PHO         Internal SGD4 parameter. */

/*    PINCO       Internal SGD4 parameter. */

/*    PLO         Internal SGD4 parameter. */

/*    RTEMSQ      Internal SGD4 parameter. */

/*    SE2         Internal SGD4 parameter. */

/*    SE3         Internal SGD4 parameter. */

/*    SGH2        Internal SGD4 parameter. */

/*    SGH3        Internal SGD4 parameter. */

/*    SGH4        Internal SGD4 parameter. */

/*    SH2         Internal SGD4 parameter. */

/*    SH3         Internal SGD4 parameter. */

/*    SI2         Internal SGD4 parameter. */

/*    SI3         Internal SGD4 parameter. */

/*    SL2         Internal SGD4 parameter. */

/*    SL3         Internal SGD4 parameter. */

/*    SL4         Internal SGD4 parameter. */

/*    S1          S coefficients */

/*    S2             ... */

/*    S3             ... */

/*    S4             ... */

/*    S5             ... */

/*    S6             ... */

/*    S7             ... */

/*    SS1            ... */

/*    SS2            ... */

/*    SS3            ... */

/*    SS4            ... */

/*    SS5            ... */

/*    SS6            ... */

/*    SS7            ... */

/*    SZ1            ... */

/*    SZ2            ... */

/*    SZ3            ... */

/*    SZ11           ... */

/*    SZ12           ... */

/*    SZ13           ... */

/*    SZ21           ... */

/*    SZ22           ... */

/*    SZ23           ... */

/*    SZ31           ... */

/*    SZ32           ... */

/*    SZ33           ... */

/*    XGH2        Internal SGD4 parameter. */

/*    XGH3        Internal SGD4 parameter. */

/*    XGH4        Internal SGD4 parameter. */

/*    XH2         Internal SGD4 parameter. */

/*    XH3         Internal SGD4 parameter. */

/*    XI2         Internal SGD4 parameter. */

/*    XI3         Internal SGD4 parameter. */

/*    XL2         Internal SGD4 parameter. */

/*    XL3         Internal SGD4 parameter. */

/*    XL4         Internal SGD4 parameter. */

/*    NM          Mean motion */

/*    Z1          Z coefficients */

/*    Z2             ... */

/*    Z3             ... */

/*    Z11            ... */

/*    Z12            ... */

/*    Z13            ... */

/*    Z21            ... */

/*    Z22            ... */

/*    Z23            ... */

/*    Z31            ... */

/*    Z32            ... */

/*    Z33            ... */

/*    ZMOL        Internal SGD4 parameter. */

/*    ZMOS        Internal SGD4 parameter. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is based on the DSCOM code by David Vallado */
/*     corresponding to "Revisiting Spacetrack Report #3" [4]. */
/*     The intent is to maintain the original Vallado algorithm, */
/*     changing code only to meet NAIF format standards and to */
/*     integrate with SPICELIB. */

/*        Capitalize all variables. */

/*        ENDIF replaced with END IF. */

/*        ENDDO replaced with END DO. */

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

/*        Corrected typos in comments. */

/* -    SPICELIB Version 1.0.0, 11-NOV-2014 (EDW) */

/*        Based on routine DSCOM, 28-JUN-2005, Vallado 2006 [4]. */

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
    chkin_("ZZDSCM", (ftnlen)6);

/*     Constants */

    zes = .01675;
    zel = .0549;
    c1ss = 2.9864797e-6;
    c1l = 4.7968065e-7;
    zsinis = .39785416;
    zcosis = .91744867;
    zcosgs = .1945905;
    zsings = -.98088458;

/*     DEEP SPACE PERIODICS INITIALIZATION */

    *xn = *np;
    *eccm = *eccp;
    *snodm = sin(*nodep);
    *cnodm = cos(*nodep);
    *sinomm = sin(*argpp);
    *cosomm = cos(*argpp);
    *sinim = sin(*inclp);
    *cosim = cos(*inclp);
    *emsq = *eccm * *eccm;
    betasq = 1. - *emsq;
    *rtemsq = sqrt(betasq);

/*     INITIALIZE LUNAR SOLAR TERMS */

/*     Note, EPOCH + 18261.5D0 corresponds to JD days since */
/*     1899-12-31 12:00:00. */

    *peo = 0.;
    *pinco = 0.;
    *plo = 0.;
    *pgho = 0.;
    *pho = 0.;
    *day = *epoch + 18261.5 + *tc / 1440.;
    d__1 = 4.523602 - *day * 9.2422029e-4;
    d__2 = twopi_();
    xnodce = d_mod(&d__1, &d__2);
    stem = sin(xnodce);
    ctem = cos(xnodce);
    zcosil = .91375164 - ctem * .03568096;
    zsinil = sqrt(1. - zcosil * zcosil);
    zsinhl = stem * .089683511 / zsinil;
    zcoshl = sqrt(1. - zsinhl * zsinhl);
    *gam = *day * .001944368 + 5.8351514;
    zx = stem * .39785416 / zsinil;
    zy = zcoshl * ctem + zsinhl * .91744867 * stem;
    zx = atan2(zx, zy);
    zx = *gam + zx - xnodce;
    zcosgl = cos(zx);
    zsingl = sin(zx);

/*     DO SOLAR TERMS */

    zcosg = zcosgs;
    zsing = zsings;
    zcosi = zcosis;
    zsini = zsinis;
    zcosh = *cnodm;
    zsinh = *snodm;
    cc = c1ss;
    xnoi = 1. / *xn;

/*     Loop over the lunar and solar term flags. */

    for (lsflg = 1; lsflg <= 2; ++lsflg) {
	a1 = zcosg * zcosh + zsing * zcosi * zsinh;
	a3 = -zsing * zcosh + zcosg * zcosi * zsinh;
	a7 = -zcosg * zsinh + zsing * zcosi * zcosh;
	a8 = zsing * zsini;
	a9 = zsing * zsinh + zcosg * zcosi * zcosh;
	a10 = zcosg * zsini;
	a2 = *cosim * a7 + *sinim * a8;
	a4 = *cosim * a9 + *sinim * a10;
	a5 = -(*sinim) * a7 + *cosim * a8;
	a6 = -(*sinim) * a9 + *cosim * a10;
	x1 = a1 * *cosomm + a2 * *sinomm;
	x2 = a3 * *cosomm + a4 * *sinomm;
	x3 = -a1 * *sinomm + a2 * *cosomm;
	x4 = -a3 * *sinomm + a4 * *cosomm;
	x5 = a5 * *sinomm;
	x6 = a6 * *sinomm;
	x7 = a5 * *cosomm;
	x8 = a6 * *cosomm;
	*z31 = x1 * 12. * x1 - x3 * 3. * x3;
	*z32 = x1 * 24. * x2 - x3 * 6. * x4;
	*z33 = x2 * 12. * x2 - x4 * 3. * x4;
	*z1 = (a1 * a1 + a2 * a2) * 3. + *z31 * *emsq;
	*z2 = (a1 * a3 + a2 * a4) * 6. + *z32 * *emsq;
	*z3 = (a3 * a3 + a4 * a4) * 3. + *z33 * *emsq;
	*z11 = a1 * -6. * a5 + *emsq * (x1 * -24. * x7 - x3 * 6. * x5);
	*z12 = (a1 * a6 + a3 * a5) * -6. + *emsq * ((x2 * x7 + x1 * x8) * 
		-24. - (x3 * x6 + x4 * x5) * 6.);
	*z13 = a3 * -6. * a6 + *emsq * (x2 * -24. * x8 - x4 * 6. * x6);
	*z21 = a2 * 6. * a5 + *emsq * (x1 * 24. * x5 - x3 * 6. * x7);
	*z22 = (a4 * a5 + a2 * a6) * 6. + *emsq * ((x2 * x5 + x1 * x6) * 24. 
		- (x4 * x7 + x3 * x8) * 6.);
	*z23 = a4 * 6. * a6 + *emsq * (x2 * 24. * x6 - x4 * 6. * x8);
	*z1 = *z1 + *z1 + betasq * *z31;
	*z2 = *z2 + *z2 + betasq * *z32;
	*z3 = *z3 + *z3 + betasq * *z33;
	*s3 = cc * xnoi;
	*s2 = *s3 * -.5 / *rtemsq;
	*s4 = *s3 * *rtemsq;
	*s1 = *eccm * -15. * *s4;
	*s5 = x1 * x3 + x2 * x4;
	*s6 = x2 * x3 + x1 * x4;
	*s7 = x2 * x4 - x1 * x3;

/*        DO LUNAR TERMS */

	if (lsflg == 1) {
	    *ss1 = *s1;
	    *ss2 = *s2;
	    *ss3 = *s3;
	    *ss4 = *s4;
	    *ss5 = *s5;
	    *ss6 = *s6;
	    *ss7 = *s7;
	    *sz1 = *z1;
	    *sz2 = *z2;
	    *sz3 = *z3;
	    *sz11 = *z11;
	    *sz12 = *z12;
	    *sz13 = *z13;
	    *sz21 = *z21;
	    *sz22 = *z22;
	    *sz23 = *z23;
	    *sz31 = *z31;
	    *sz32 = *z32;
	    *sz33 = *z33;
	    zcosg = zcosgl;
	    zsing = zsingl;
	    zcosi = zcosil;
	    zsini = zsinil;
	    zcosh = zcoshl * *cnodm + zsinhl * *snodm;
	    zsinh = *snodm * zcoshl - *cnodm * zsinhl;
	    cc = c1l;
	}
    }
    d__1 = *day * .2299715 + 4.7199672 - *gam;
    d__2 = twopi_();
    *zmol = d_mod(&d__1, &d__2);
    d__1 = *day * .017201977 + 6.2565837;
    d__2 = twopi_();
    *zmos = d_mod(&d__1, &d__2);

/*     DO SOLAR TERMS */

    *se2 = *ss1 * 2. * *ss6;
    *se3 = *ss1 * 2. * *ss7;
    *si2 = *ss2 * 2. * *sz12;
    *si3 = *ss2 * 2. * (*sz13 - *sz11);
    *sl2 = *ss3 * -2. * *sz2;
    *sl3 = *ss3 * -2. * (*sz3 - *sz1);
    *sl4 = *ss3 * -2. * (-21. - *emsq * 9.) * zes;
    *sgh2 = *ss4 * 2. * *sz32;
    *sgh3 = *ss4 * 2. * (*sz33 - *sz31);
    *sgh4 = *ss4 * -18. * zes;
    *sh2 = *ss2 * -2. * *sz22;
    *sh3 = *ss2 * -2. * (*sz23 - *sz21);

/*     DO LUNAR TERMS */

    *ee2 = *s1 * 2. * *s6;
    *e3 = *s1 * 2. * *s7;
    *xi2 = *s2 * 2. * *z12;
    *xi3 = *s2 * 2. * (*z13 - *z11);
    *xl2 = *s3 * -2. * *z2;
    *xl3 = *s3 * -2. * (*z3 - *z1);
    *xl4 = *s3 * -2. * (-21. - *emsq * 9.) * zel;
    *xgh2 = *s4 * 2. * *z32;
    *xgh3 = *s4 * 2. * (*z33 - *z31);
    *xgh4 = *s4 * -18. * zel;
    *xh2 = *s2 * -2. * *z22;
    *xh3 = *s2 * -2. * (*z23 - *z21);
    chkout_("ZZDSCM", (ftnlen)6);
    return 0;
} /* zzdscm_ */

