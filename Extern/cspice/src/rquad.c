/* rquad.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure RQUAD ( Roots of a quadratic equation ) */
/* Subroutine */ int rquad_(doublereal *a, doublereal *b, doublereal *c__, 
	doublereal *root1, doublereal *root2)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal scale;
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *);
    doublereal discrm;
    logical zeroed;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    doublereal con, lin, sqr;

/* $ Abstract */

/*     Find the roots of a quadratic equation. */

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

/*     MATH */
/*     POLYNOMIAL */
/*     ROOT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     A          I   Coefficient of quadratic term. */
/*     B          I   Coefficient of linear term. */
/*     C          I   Constant. */
/*     ROOT1      O   Root built from positive discriminant term. */
/*     ROOT2      O   Root built from negative discriminant term. */

/* $ Detailed_Input */

/*     A, */
/*     B, */
/*     C        are the coefficients of a quadratic polynomial */

/*                      2 */
/*                 A * x   +  B * x  +  C. */

/* $ Detailed_Output */

/*     ROOT1, */
/*     ROOT2    are the roots of the equation */

/*                      2 */
/*                 A * x   +  B * x  +  C = 0. */


/*              ROOT1 and ROOT2 are both arrays of length 2. The first */
/*              element of each array is the real part of a root; the */
/*              second element contains the complex part of the same */
/*              root. */

/*              When A is non-zero, ROOT1 represents the root */

/*                               _____________ */
/*                              /  2 */
/*                 - B   +    \/  B    -   4AC */
/*                 --------------------------- */
/*                               2A */


/*              and ROOT2 represents the root */

/*                               _____________ */
/*                              /  2 */
/*                 - B   -    \/  B    -   4AC */
/*                 --------------------------- . */
/*                               2A */


/*              When A is zero and B is non-zero, ROOT1 and ROOT2 both */
/*              represent the root */

/*                 - C / B. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input coefficients A and B are both zero, the error */
/*         SPICE(DEGENERATECASE) is signaled. The output arguments */
/*         are not modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     1)   Humor us and suppose we want to compute the "golden ratio." */

/*          The quantity r is defined by the equation */

/*             1/r = r/(1-r), */

/*          which is equivalent to */

/*              2 */
/*             r   +  r  -  1  =  0. */

/*          The following code fragment does the job. */


/*             C */
/*             C     Compute "golden ratio." The root we want, */
/*             C */
/*             C                ___ */
/*             C               / */
/*             C        -1 + \/  5 */
/*             C        -----------, */
/*             C             2 */
/*             C */
/*             C */
/*             C     is contained in ROOT1. */
/*             C */

/*                   CALL RQUAD ( 1.D0, 1.D0, -1.D0, ROOT1, ROOT2 ) */

/*                   PRINT *, 'The "golden ratio" is ', ROOT1(1) */


/*     2)   The equation, */

/*              2 */
/*             x   +  1  =  0 */

/*          can be solved by the code fragment */


/*             C */
/*             C     Let's do one with imaginary roots just for fun. */
/*             C */

/*                   CALL RQUAD ( 1.D0,  0.D0,  1.D0,  ROOT1,  ROOT2 ) */

/*                   PRINT *, 'ROOT1 is ', ROOT1 */
/*                   PRINT *, 'ROOT2 is ', ROOT2 */

/*          The printed results will be something like: */


/*             ROOT1 is 0.000000000000000    1.000000000000000 */
/*             ROOT2 is 0.000000000000000   -1.000000000000000 */

/* $ Restrictions */

/*     1)  No checks for overflow of the roots are performed. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 10-JUL-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     roots of a quadratic equation */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("RQUAD", (ftnlen)5);
    }

/*     The degree of the equation is zero unless at least one of the */
/*     second or first degree coefficients is non-zero. */

    if (*a == 0. && *b == 0.) {
	setmsg_("Both 1st and 2nd degree coefficients are zero.", (ftnlen)46);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("RQUAD", (ftnlen)5);
	return 0;
    }

/*     If we can scale the coefficients without zeroing any of them out, */
/*     we will do so, to help prevent overflow. */

/* Computing MAX */
    d__1 = abs(*a), d__2 = abs(*b), d__1 = max(d__1,d__2), d__2 = abs(*c__);
    scale = max(d__1,d__2);
    zeroed = *a != 0. && *a / scale == 0. || *b != 0. && *b / scale == 0. || *
	    c__ != 0. && *c__ / scale == 0.;
    if (! zeroed) {
	sqr = *a / scale;
	lin = *b / scale;
	con = *c__ / scale;
    } else {
	sqr = *a;
	lin = *b;
	con = *c__;
    }

/*     If the second-degree coefficient is non-zero, we have a bona fide */
/*     quadratic equation, as opposed to a linear equation. */

    if (sqr != 0.) {

/*        Compute the discriminant. */

/* Computing 2nd power */
	d__1 = lin;
	discrm = d__1 * d__1 - sqr * 4. * con;

/*        A non-negative discriminant indicates that the roots are */
/*        real. */

	if (discrm >= 0.) {

/*           The imaginary parts of both roots are zero. */

	    root1[1] = 0.;
	    root2[1] = 0.;

/*           We can take advantage of the fact that CON/SQR is the */
/*           product of the roots to improve the accuracy of the root */
/*           having the smaller magnitude.  We compute the larger root */
/*           first and then divide CON/SQR by it to obtain the smaller */
/*           root. */

	    if (lin < 0.) {

/*              ROOT1 will contain the root of larger magnitude. */

		root1[0] = (-lin + sqrt(discrm)) / (sqr * 2.);
		root2[0] = con / sqr / root1[0];
	    } else if (lin > 0.) {

/*              ROOT2 will contain the root of larger magnitude. */

		root2[0] = (-lin - sqrt(discrm)) / (sqr * 2.);
		root1[0] = con / sqr / root2[0];
	    } else {

/*              The roots have the same magnitude. */

		root1[0] = sqrt(discrm) / (sqr * 2.);
		root2[0] = -root1[0];
	    }

/*        The only other possibility is that the roots are complex. */

	} else {

/*           The roots are complex conjugates, so they have equal */
/*           magnitudes. */

	    root1[0] = -lin / (sqr * 2.);
	    root1[1] = sqrt(-discrm) / (sqr * 2.);
	    root2[0] = root1[0];
	    root2[1] = -root1[1];
	}

/*     If the second-degree coefficient is zero, we actually have a */
/*     linear equation. */

    } else if (lin != 0.) {
	root1[0] = -con / lin;
	root1[1] = 0.;

/*        We set the second root equal to the first, rather than */
/*        leaving it undefined. */

	moved_(root1, &c__2, root2);
    }
    chkout_("RQUAD", (ftnlen)5);
    return 0;
} /* rquad_ */

