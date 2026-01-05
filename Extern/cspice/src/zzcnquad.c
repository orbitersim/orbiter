/* zzcnquad.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b8 = 1.;

/* $Procedure  ZZCNQUAD ( Solve quadratic equation for cone intercept ) */
/* Subroutine */ int zzcnquad_(doublereal *a, doublereal *b, doublereal *c__, 
	doublereal *ub, integer *n, doublereal *r1, doublereal *r2)
{
    /* Initialized data */

    static logical first = TRUE_;
    static doublereal invub = -1.;

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);
    integer s_rnge(char *, integer, char *, integer);
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern doublereal dpmax_(void);
    integer maxix;
    doublereal coeffs[3];
    integer nx;
    doublereal maxmag;
    extern doublereal touchd_(doublereal *);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int zzbquad_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    doublereal *);
    doublereal inv1, inv2;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Solve a quadratic equation using an upper bound for the absolute */
/*     value of the roots. Only real roots are computed. This routine */
/*     addresses the case of a small, non-zero leading coefficient. */

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

/*     EQUATION */
/*     MATH */
/*     QUADRATIC */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     A, */
/*     B, */
/*     C          I   Coefficients of quadratic equation. */
/*     UB         I   Upper bound on magnitude of roots. */
/*     N          O   Number of real roots within bound. */
/*     R1         O   Root of smaller absolute value. */
/*     R2         O   Root of larger absolute value. */
/*     BIG        P   Limit on magnitude of coefficients and UB. */

/* $ Detailed_Input */

/*     A, */
/*     B, */
/*     C          are the real coefficients of a quadratic equation. */
/*                The equation is */

/*                      2 */
/*                   A X   +  B X  +  C  =  0 */


/*     UB         is an upper bound on the absolute value of real */
/*                roots to be found by this routine. Roots having */
/*                absolute value larger than UB are not returned. */

/* $ Detailed_Output */


/*     N          is the number of real roots found that satisfy the */
/*                magnitude bound constraint. */

/*                Degenerate cases: if A = B = 0, then */

/*                   If C  = 0, N is set to -1; R1 and R2 are set to */
/*                   zero. */

/*                   If C != 0, N is set to -2; R1 and R2 are set to */
/*                   zero. */

/*     R1, */
/*     R2        are roots returned in increasing order of absolute */
/*               value. If there is one real root of multiplicity 2, */
/*               both R1 and R2 are set to that root, provided that */
/*               the absolute value of the root does not exceed UB. */

/*               If the absolute value of a root exceeds UB the */
/*               corresponding output argument is set to zero. If the */
/*               roots are complex, both R1 and R2 are set to zero. */

/* $ Parameters */

/*     BIG            is a limit on the absolute value of the input */
/*                    coefficients and on the magnitude limit for the */
/*                    roots. BIG is set to */

/*                       SQRT( DPMAX() ) / 100 */

/* $ Exceptions */

/*     1)  If any of the input coefficients have absolute value larger */
/*         than the parameter BIG, the error is diagnosed by a routine */
/*         in the call tree of this routine. */

/*     2)  If UB is non-positive or larger than the parameter BIG, the */
/*         error is diagnosed by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine supports INCNSG. */

/*     This routine makes use of ZZBQUAD; it calls ZZBQUAD for */
/*     cases that can be handled accurately by that routine. */
/*     Other cases are handled in-line. */

/* $ Examples */

/*     See usage in INCNSG. */

/* $ Restrictions */

/*     1)  This is a private routine; it should not be called by */
/*         non-SPICELIB code. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 23-JAN-2017 (NJB) */

/*        Previous version 22-JUN-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     solve quadratic equation for cone intercept */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved values */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("ZZCNQUAD", (ftnlen)8);

/*     On the first pass, set the upper bound for the reciprocal */
/*     solution. */

    if (first) {
	invub = sqrt(dpmax_()) / 200.;
	first = FALSE_;
    }

/*     Handle the degenerate cases first. */

    if (*a == 0. && *b == 0.) {
	*r1 = 0.;
	*r2 = 0.;
	if (*c__ == 0.) {
	    *n = -1;
	} else {
	    *n = -2;
	}
	chkout_("ZZCNQUAD", (ftnlen)8);
	return 0;
    }

/*     Scale the input coefficients. */

/* Computing MAX */
    d__1 = abs(*a), d__2 = abs(*b), d__1 = max(d__1,d__2), d__2 = abs(*c__);
    maxmag = max(d__1,d__2);
    d__1 = *a / maxmag;
    coeffs[0] = touchd_(&d__1);
    d__1 = *b / maxmag;
    coeffs[1] = touchd_(&d__1);
    d__1 = *c__ / maxmag;
    coeffs[2] = touchd_(&d__1);

/*     Identify the coefficient of largest magnitude. */

    maxix = 1;
    for (i__ = 2; i__ <= 3; ++i__) {
	if ((d__1 = coeffs[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		"coeffs", i__1, "zzcnquad_", (ftnlen)272)], abs(d__1)) > (
		d__2 = coeffs[(i__2 = maxix - 1) < 3 && 0 <= i__2 ? i__2 : 
		s_rnge("coeffs", i__2, "zzcnquad_", (ftnlen)272)], abs(d__2)))
		 {

/*           Record the index of the maximum magnitude. */

	    maxix = i__;
	}
    }

/*     Make sure the value of maximum magnitude is +/- 1. */

    coeffs[(i__1 = maxix - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("coeffs", i__1,
	     "zzcnquad_", (ftnlen)285)] = d_sign(&c_b8, &coeffs[(i__2 = maxix 
	    - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("coeffs", i__2, "zzcnquad_",
	     (ftnlen)285)]);

/*     Find roots in a manner suited to the coefficients we have. */

    if (abs(coeffs[0]) >= 1e-8 || coeffs[0] == 0.) {

/*        This is a numerically well-behaved case. Delegate the */
/*        job to ZZBQUAD. */

	zzbquad_(coeffs, &coeffs[1], &coeffs[2], ub, n, &nx, r1, r2);
    } else if (abs(coeffs[2]) >= 1e-8) {

/*        The zero-order coefficient has magnitude >= SMALL. */

/*        The original equation */

/*              2 */
/*           a x  + b x + c = 0 */

/*        can be replaced by */

/*              2 */
/*           c y  + b y + a = 0 */

/*        where */

/*           y = 1/x */

/*        Here */

/*          |c| >= SMALL */
/*          |c| <= 1 */

/*          |a|  < SMALL */


/*        Because the quadratic coefficient is bounded away from zero, */
/*        the roots of the reciprocal equation are not in danger of */
/*        overflowing. So we can safely solve for 1/x. We might have */
/*        complex roots; these are rejected. */

/*        The roots of the transformed equation don't have a maximum */
/*        magnitude restriction imposed by UB. We set the upper bound */
/*        to a value that ZZBQUAD will allow. */

	zzbquad_(&coeffs[2], &coeffs[1], coeffs, &invub, n, &nx, &inv1, &inv2)
		;
	if (*n == 1) {

/*           We have one real root. Make sure we can invert it. */

	    if ((d__1 = inv1 * *ub, abs(d__1)) >= 1.) {


/*              |1/INV1| <= UB */


		*r1 = 1. / inv1;
	    } else {

/*              There are no real roots having magnitude within the */
/*              bound. */

		*n = 0;
	    }

/*           There is no second root. */

	    *r2 = 0.;
	} else if (*n == 2) {

/*           We have two real roots. The one of larger magnitude is */
/*           the second one. The reciprocal of this root will be */
/*           the smaller root of the original equation, as long */
/*           as the reciprocal is within bounds. */

	    if ((d__1 = inv2 * *ub, abs(d__1)) >= 1.) {


/*              |1/INV2| <= UB */


		*r1 = 1. / inv2;

/*              Proceed to the first root of the transformed equation. */

		if ((d__1 = inv1 * *ub, abs(d__1)) >= 1.) {


/*                 |1/INV1| <= UB */


		    *r2 = 1. / inv1;
		} else {

/*                 Only the second root qualifies for inversion. */

		    *n = 1;
		    *r2 = 0.;
		}
	    } else {

/*              The reciprocal of the larger root is too big; the */
/*              reciprocal of the smaller root will be even larger. */
/*              There are no real roots having magnitude within the */
/*              bound. */

		*n = 0;
		*r1 = 0.;
		*r2 = 0.;
	    }
	} else {

/*           We have no viable roots of the transformed equation, so */
/*           we have no viable roots of the original one. */

	    *n = 0;
	    *r1 = 0.;
	    *r2 = 0.;
	}
    } else {

/*        The linear coefficient B has the greatest magnitude, which */
/*        is 1. The quadratic coefficient A is "small":  0 < |A| < 1.D-8. */
/*        The zero-order coefficient is "small" as well. */

/*        It will be convenient to make B equal to 1; do this now. */

	if (*b < 0.) {
	    coeffs[0] = -coeffs[0];
	    coeffs[1] = -coeffs[1];
	    coeffs[2] = -coeffs[2];
	}

/*        In this case we use a low-order Taylor expansion about */
/*        x = 0 for the square root term of the formula for the roots: */

/*                                  inf */
/*                                  __ */
/*                           1/2    \    (k)     k */
/*           T(x) = ( 1 + x )    =  /_  f   (0) x / (k!) */

/*                                  k=0 */


/*                              2      3         4 */
/*                =  1 + x/2 - x /8 + x /16 + O(x ) */


/*        Apply this formula to that for the solution having the */
/*        positive square root term. Here let `x' be */


/*                   2 */
/*           -4ac / b */

/*        which equals */

/*           -4ac */

/*        since we've set b = 1. */


/*        Then the root is */


/*                  -1 + sqrt( 1 - 4ac ) */
/*           x  =   -------------------- */
/*            1             2a */


/*                                      2 2          3 */
/*                  -1 + ( 1 - 2ac - 16a c /8 + O((ac)) ) */
/*              =   ------------------------------------- */
/*                                   2a */

/*        Discarding the high-order terms in a, we have */


/*           x  ~=  ( -1 + 1 - 2ac ) / 2a  =  -c */
/*            1 */

/*        Similarly, we have */


/*           x  ~=  ( -1 - 1 + 2ac ) / 2a  =  ( ac - 1 )/a  = c - 1/a */
/*            2 */


/*        Based on the conditions that got us here, we know */

/*           |c| < 1 */

/*           |c - 1/a| ~= |1/a| > 1.e8 */

	*n = 0;
	*r1 = 0.;
	*r2 = 0.;
	if (abs(coeffs[2]) <= *ub) {
	    *r1 = -coeffs[2];
	    *n = 1;
	    if ((d__1 = coeffs[0] * coeffs[2] - 1., abs(d__1)) < (d__2 = 
		    coeffs[0] * *ub, abs(d__2))) {
		*r2 = coeffs[2] - 1. / coeffs[0];
		*n = 2;
	    }
	}
    }
    chkout_("ZZCNQUAD", (ftnlen)8);
    return 0;
} /* zzcnquad_ */

