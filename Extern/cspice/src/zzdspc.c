/* zzdspc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZDSPC ( SGP4 deep space routine ) */
/* Subroutine */ int zzdspc_(integer *irez, doublereal *d2201, doublereal *
	d2211, doublereal *d3210, doublereal *d3222, doublereal *d4410, 
	doublereal *d4422, doublereal *d5220, doublereal *d5232, doublereal *
	d5421, doublereal *d5433, doublereal *dedt, doublereal *del1, 
	doublereal *del2, doublereal *del3, doublereal *didt, doublereal *
	dmdt, doublereal *dnodt, doublereal *domdt, doublereal *argpo, 
	doublereal *argpdot, doublereal *t, doublereal *tc, doublereal *gsto, 
	doublereal *xfact, doublereal *xlamo, doublereal *no, doublereal *
	atime, doublereal *eccm, doublereal *argpm, doublereal *inclm, 
	doublereal *xli, doublereal *mm, doublereal *xni, doublereal *nodem, 
	doublereal *dndt, doublereal *xn)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_mod(doublereal *, doublereal *), sin(doublereal), cos(doublereal)
	    ;

    /* Local variables */
    doublereal delt;
    integer iret;
    doublereal xndt, xomi, fasx2, fasx4, fasx6, step2, x2omi;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal theta, xnddt;
    integer iretn;
    doublereal stepn, xldot, rptim, stepp;
    extern doublereal twopi_(void);
    doublereal g22, g32, g52, g44, g54, ft, xl;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    doublereal x2li;

/* $ Abstract */

/*     This subroutine provides deep space contributions to mean */
/*     elements for perturbing third body. These effects have been */
/*     averaged over one revolution of the sun and moon. For earth */
/*     resonance effects, the effects have been averaged over NO */
/*     revolutions of the satellite. */

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

/*     D2201      D coefficients */

/*     D2211         ... */

/*     D3210         ... */

/*     D3222         ... */

/*     D4410         ... */

/*     D4422         ... */

/*     D5220         ... */

/*     D5232         ... */

/*     D5421         ... */

/*     D5433         ... */

/*     DEDT       Internal SGD4 parameter. */

/*     DEL1       Internal SGD4 parameter. */

/*     DEL2       Internal SGD4 parameter. */

/*     DEL3       Internal SGD4 parameter. */

/*     DIDT       Internal SGD4 parameter. */

/*     DMDT       Internal SGD4 parameter. */

/*     DNODT      Internal SGD4 parameter. */

/*     DOMDT      Internal SGD4 parameter. */

/*     IREZ       Flag for resonance: 0-none, 1-one day, 2-half day */
/* . */
/*     ARGPO      Argument of perigee */

/*     ARGPDOT    Argument of perigee dot (rate) */

/*     T          Time of evaluation. */

/*     TC         Internal SGD4 parameter. */

/*     GSTO       Greenwich Sidereal Time */

/*     XFACT      Internal SGD4 parameter. */

/*     XLAMO      Internal SGD4 parameter. */

/*     NO         Mean motion. */

/*     ATIME      Internal SGD4 parameter. */

/*     EM         Eccentricity. */

/*     FT         Internal SGD4 parameter. */

/*     ARGPM      Argument of perigee */

/*     INCLM      Inclination. */

/*     XLI        Internal SGD4 parameter. */

/*     MM         Mean anomaly. */

/*     XNI        Mean motion. */

/*     NODEM      Right ascension of ascending node */

/* $ Detailed_Output */

/*     ATIME      Internal SGD4 parameter. */

/*     EM         Calculated mean eccentricity. */

/*     ARGPM      Calculated mean argument of perigee. */

/*     INCLM      Calculated mean inclination. */

/*     XLI        Internal SGD4 parameter. */

/*     MM         Calculated mean anomaly. */

/*     XNI        Calculated  mean motion. */

/*     NODEM      Calculated mean right ascension of */
/*                ascending node. */

/*     DNDT       Value XN-NO. */

/*     NM         Calculated mean motion. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is based on the DSPACE code by David Vallado */
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

/* -    SPICELIB Version 1.0.0, OCT-14-2014 (EDW) */

/*        Based on routine DPSPACE, 28-JUN-2005, Vallado 2006 [4]. */

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
    chkin_("ZZDSPC", (ftnlen)6);

/*     Constants */

    fasx2 = .13130908;
    fasx4 = 2.8843198;
    fasx6 = .37448087;
    g22 = 5.7686396;
    g32 = .95240898;
    g44 = 1.8014998;
    g52 = 1.050833;
    g54 = 4.4108898;
    rptim = .00437526908801129966;
    stepp = 720.;
    stepn = -720.;
    step2 = 259200.;

/*     Calculate deep space resonance effects. */

    *dndt = 0.;
    d__1 = *gsto + *tc * rptim;
    d__2 = twopi_();
    theta = d_mod(&d__1, &d__2);
    *eccm += *dedt * *t;
    *inclm += *didt * *t;
    *argpm += *domdt * *t;
    *nodem += *dnodt * *t;
    *mm += *dmdt * *t;

/*   sgp4fix for negative inclinations */
/*   the following if statement should be commented out */

/*        IF( INCLM .LT. 0.0D0) THEN */
/*            INCLM  = -INCLM */
/*            ARGPM  = ARGPM-PI */
/*            NODEM  = NODEM+PI */
/*        END IF */


/*     sgp4fix for propagator problems */

/*     The following integration works for negative time steps and */
/*     periods. The specific changes are unknown because the original */
/*     code was so convoluted */

/*     sgp4fix Take out atime = 0.0 and fix for faster operation */


/*     Just in case - should be set in loops if used. */

    ft = 0.;
    if (*irez != 0) {

/*     UPDATE RESONANCES : NUMERICAL (EULER-MACLAURIN) INTEGRATION */

/*     EPOCH RESTART */


/*        sgp4fix streamline check */

	if (*atime == 0. || *t * *atime <= 0. || abs(*t) < abs(*atime)) {
	    *atime = 0.;
	    *xni = *no;
	    *xli = *xlamo;
	}

/*        sgp4fix move check outside loop */

	if (*t > 0.) {
	    delt = stepp;
	} else {
	    delt = stepn;
	}

/*        ADDED FOR DO LOOP */

	iretn = 381;

/*        ADDED FOR LOOP */

	iret = 0;
	while(iretn == 381) {

/*           DOT TERMS CALCULATED */

/*           NEAR - SYNCHRONOUS RESONANCE TERMS */

	    if (*irez != 2) {
		xndt = *del1 * sin(*xli - fasx2) + *del2 * sin((*xli - fasx4) 
			* 2.) + *del3 * sin((*xli - fasx6) * 3.);
		xldot = *xni + *xfact;
		xnddt = *del1 * cos(*xli - fasx2) + *del2 * 2. * cos((*xli - 
			fasx4) * 2.) + *del3 * 3. * cos((*xli - fasx6) * 3.);
		xnddt *= xldot;
	    } else {

/*              NEAR - HALF-DAY RESONANCE TERMS */

		xomi = *argpo + *argpdot * *atime;
		x2omi = xomi + xomi;
		x2li = *xli + *xli;
		xndt = *d2201 * sin(x2omi + *xli - g22) + *d2211 * sin(*xli - 
			g22) + *d3210 * sin(xomi + *xli - g32) + *d3222 * sin(
			-xomi + *xli - g32) + *d4410 * sin(x2omi + x2li - g44)
			 + *d4422 * sin(x2li - g44) + *d5220 * sin(xomi + *
			xli - g52) + *d5232 * sin(-xomi + *xli - g52) + *
			d5421 * sin(xomi + x2li - g54) + *d5433 * sin(-xomi + 
			x2li - g54);
		xldot = *xni + *xfact;
		xnddt = *d2201 * cos(x2omi + *xli - g22) + *d2211 * cos(*xli 
			- g22) + *d3210 * cos(xomi + *xli - g32) + *d3222 * 
			cos(-xomi + *xli - g32) + *d5220 * cos(xomi + *xli - 
			g52) + *d5232 * cos(-xomi + *xli - g52) + (*d4410 * 
			cos(x2omi + x2li - g44) + *d4422 * cos(x2li - g44) + *
			d5421 * cos(xomi + x2li - g54) + *d5433 * cos(-xomi + 
			x2li - g54)) * 2.;
		xnddt *= xldot;
	    }

/*           INTEGRATOR */

/*           sgp4fix move end checks to end of routine */

	    if ((d__1 = *t - *atime, abs(d__1)) >= stepp) {
		iret = 0;
		iretn = 381;
	    } else {
		ft = *t - *atime;
		iretn = 0;
	    }
	    if (iretn == 381) {
		*xli = *xli + xldot * delt + xndt * step2;
		*xni = *xni + xndt * delt + xnddt * step2;
		*atime += delt;
	    }
	}
	*xn = *xni + xndt * ft + xnddt * ft * ft * .5;
	xl = *xli + xldot * ft + xndt * ft * ft * .5;
	if (*irez != 1) {
	    *mm = xl - *nodem * 2. + theta * 2.;
	    *dndt = *xn - *no;
	} else {
	    *mm = xl - *nodem - *argpm + theta;
	    *dndt = *xn - *no;
	}
	*xn = *no + *dndt;
    }
    chkout_("ZZDSPC", (ftnlen)6);
    return 0;
} /* zzdspc_ */

