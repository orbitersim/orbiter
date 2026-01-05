/* zzbquad.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  ZZBQUAD ( Solve quadratic equation with bounds ) */
/* Subroutine */ int zzbquad_(doublereal *a, doublereal *b, doublereal *c__, 
	doublereal *ub, integer *n, integer *nx, doublereal *r1, doublereal *
	r2)
{
    /* Initialized data */

    static doublereal big = 0.;
    static logical first = TRUE_;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal denom;
    extern doublereal dpmax_(void);
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal dscrim;
    extern doublereal touchd_(doublereal *);
    doublereal sqdisc;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    doublereal num1, num2;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Solve a quadratic equation using an upper bound for the absolute */
/*     value of the roots. Only real roots are computed. */

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
/*     NX         O   Number of real roots exceeding bound. */
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

/*                   If C  = 0, N is set to -1; NX is set to 0; R1 and */
/*                   R2 are set to zero. */

/*                   If C != 0, N is set to -2; NX is set to 0; R1 and */
/*                   R2 are set to zero. */


/*     NX        is the number of real roots having absolute values that */
/*               exceed the bound. */

/*               If the roots are complex, both N and NX are set to zero. */

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
/*         than the parameter BIG, the error SPICE(VALUEOUTOFRANGE) is */
/*         signaled. */

/*     2)  If UB is non-positive or larger than the parameter BIG, the */
/*         error SPICE(VALUEOUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     See ZZCNQUAD for a more robust implementation that makes use */
/*     of this one. */

/* $ Examples */

/*     See usage in ZZCNQUAD */

/* $ Restrictions */

/*     1)  This routine may suffer from loss of precision for coefficient */
/*         sets having small, non-zero leading coefficients. See ZZCNQUAD */
/*         for a more robust implementation. */

/*     2)  This is a private routine; it should not be called by */
/*         non-SPICELIB code. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 21-JUN-2016 (NJB) */

/*        Made minor updates to header comments. */

/*        24-SEP-2014 (NJB) */

/*           Original version. */

/* -& */
/* $ Index_Entries */

/*     solve quadratic equation with bounds on roots */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Use discovery check-in. */

    if (return_()) {
	return 0;
    }
    if (first) {
	big = sqrt(dpmax_()) / 100;
	first = FALSE_;
    }

/*     Set invalid counts to start out. Initialize R1 and R2. */

    *n = -3;
    *nx = -3;
    *r1 = 0.;
    *r2 = 0.;

/*     Reject all large magnitude coefficients. */

    if (abs(*a) > big || abs(*b) > big || abs(*c__) > big) {
	chkin_("ZZBQUAD", (ftnlen)7);
	setmsg_("Coefficients must have magnitude less than or equal to #, b"
		"ut were A = #; B = #; C = #.", (ftnlen)87);
	errdp_("#", &big, (ftnlen)1);
	errdp_("#", a, (ftnlen)1);
	errdp_("#", b, (ftnlen)1);
	errdp_("#", c__, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZBQUAD", (ftnlen)7);
	return 0;
    }

/*     Reject large magnitude upper bounds as well. */

    if (abs(*ub) > big) {
	chkin_("ZZBQUAD", (ftnlen)7);
	setmsg_("Upper bounds must have magnitude less than or equal to #, b"
		"ut was #.", (ftnlen)68);
	errdp_("#", &big, (ftnlen)1);
	errdp_("#", ub, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZBQUAD", (ftnlen)7);
	return 0;
    }

/*     The upper bound must be positive. */

    if (*ub <= 0.) {
	chkin_("ZZBQUAD", (ftnlen)7);
	setmsg_("Upper bound must be positive but was #.", (ftnlen)39);
	errdp_("#", ub, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZBQUAD", (ftnlen)7);
	return 0;
    }

/*     Handle the degenerate cases first. */

    if (*a == 0.) {
	if (*b == 0.) {

/*           The equation is of the form */

/*              C = 0 */

	    if (*c__ == 0.) {

/*              The equation is satisfied for all real numbers. */

		*n = -1;
		*nx = 0;
	    } else {

/*              There are no solutions. */

		*n = -2;
		*nx = 0;
	    }
	} else {

/*           The equation is first-order: */

/*              B*X + C = 0 */

/*           In this branch, B is non-zero. */

	    if (abs(*c__) <= (d__1 = *ub * *b, abs(d__1))) {
		*n = 1;
		*nx = 0;
		*r1 = -(*c__) / *b;
		*r2 = *r1;
	    } else {

/*              The magnitude of the solution is too large. */

		*n = 0;
		*nx = 1;
	    }
	}
    } else {

/*        The leading coefficient of the equation is non-zero. */

/*        We can safely compute the discriminant now, due the */
/*        check we've already performed. */

	d__1 = *b * *b - *a * 4 * *c__;
	dscrim = touchd_(&d__1);
	if (dscrim < 0.) {

/*           We have complex roots, so we're done. */

	    *n = 0;
	    *nx = 0;
	} else if (dscrim == 0.) {

/*           We have a single real root of multiplicity 2. */

/*           Compare the magnitude of the root to the upper bound. */

	    num1 = -(*b);
	    denom = *a * 2;
	    if (abs(num1) >= (d__1 = denom * *ub, abs(d__1))) {

/*              The root is too large; we won't compute it. */

		*n = 0;
		*nx = 1;
	    } else {

/*              Set both roots to the same value. In this branch, */
/*              A is non-zero. */

		*n = 1;
		*nx = 0;
		*r1 = num1 / *a / 2;
		*r2 = *r1;
	    }
	} else {

/*           We have two nominally distinct real roots. Whether */
/*           they're distinct double precision numbers depends */
/*           on the relative magnitudes of A and DSCRIM. */

	    denom = *a * 2;
	    sqdisc = sqrt(dscrim);
	    if (*b > 0.) {
		num2 = -(*b) - sqdisc;
		num1 = -(*b) + sqdisc;
	    } else {
		num2 = -(*b) + sqdisc;
		num1 = -(*b) - sqdisc;
	    }

/*           See whether the root of larger magnitude is computable. */

	    if (abs(num2) <= (d__1 = *ub * denom, abs(d__1))) {

/*              The root is computable. */
		*n = 2;
		*nx = 0;

/*              In this branch, A is non-zero. */

		*r2 = num2 / *a / 2;
		if (abs(*r2) > 0.) {

/*                 Compute R1 using R2 and C; this avoids loss */
/*                 of precision that may occur when NUM1 is computed. */

/*                 We know R1 has smaller magnitude than R2 and R2 */
/*                 is computable, and we know A is non-zero, so R1 */
/*                 can be computed without a divide-by-zero error, */
/*                 and it is computable as long as no intermediate */
/*                 results overflow. The bounds on A and R2 ensure */
/*                 that A*R2 is computable. */

		    *r1 = *c__ / (*a * *r2);
		} else {

/*                 The root of larger magnitude has magnitude 0. This */
/*                 doesn't leave many possible values for the root of */
/*                 smaller magnitude. */

		    *r1 = 0.;
		}
	    } else {

/*              The root of larger magnitude is not computable. */
/*              Check the root of smaller magnitude. */

		if (abs(num1) <= (d__1 = *ub * denom, abs(d__1))) {

/*                 The root is computable. */

		    *n = 1;
		    *nx = 1;
		    *r1 = num1 / *a / 2;
		} else {

/*                 Neither root is computable. */

		    *n = 0;
		    *nx = 2;
		}
	    }
	}
    }
    return 0;
} /* zzbquad_ */

