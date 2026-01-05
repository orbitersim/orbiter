/* lgrind.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LGRIND (Lagrange polynomial interpolation with derivative) */
/* Subroutine */ int lgrind_(integer *n, doublereal *xvals, doublereal *yvals,
	 doublereal *work, doublereal *x, doublereal *p, doublereal *dp)
{
    /* System generated locals */
    integer xvals_dim1, yvals_dim1, work_dim1, work_offset, i__1, i__2, i__3, 
	    i__4, i__5, i__6, i__7;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal denom;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal c1, c2;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Evaluate a Lagrange interpolating polynomial, for a specified */
/*     set of coordinate pairs, at a specified abscissa value. Return */
/*     both the value of the polynomial and its derivative. */

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

/*     INTERPOLATION */
/*     POLYNOMIAL */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     N          I   Number of points defining the polynomial. */
/*     XVALS      I   Abscissa values. */
/*     YVALS      I   Ordinate values. */
/*     WORK      I-O  Work space array. */
/*     X          I   Point at which to interpolate the polynomial. */
/*     P          O   Polynomial value at X. */
/*     DP         O   Polynomial derivative at X. */

/* $ Detailed_Input */

/*     N        is the number of points defining the polynomial. */
/*              The arrays XVALS and YVALS contain N elements. */

/*     XVALS, */
/*     YVALS    are arrays of abscissa and ordinate values that */
/*              together define N ordered pairs. The set of points */

/*                 ( XVALS(I), YVALS(I) ) */

/*              define the Lagrange polynomial used for */
/*              interpolation. The elements of XVALS must be */
/*              distinct and in increasing order. */

/*     WORK     is an N * 2 work space array, where N is the same */
/*              dimension as that of XVALS and YVALS. It is used */
/*              by this routine as a scratch area to hold */
/*              intermediate results. */

/*     X        is the abscissa value at which the interpolating */
/*              polynomial is to be evaluated. */

/* $ Detailed_Output */

/*     P        is the value at X of the unique polynomial of */
/*              degree N-1 that fits the points in the plane */
/*              defined by XVALS and YVALS. */

/*     DP       is the derivative at X of the interpolating */
/*              polynomial described above. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If any two elements of the array XVALS are equal, the error */
/*         SPICE(DIVIDEBYZERO) is signaled. */

/*     2)  If N is less than 1, the error SPICE(INVALIDSIZE) is */
/*         signaled. */

/*     3)  This routine does not attempt to ward off or diagnose */
/*         arithmetic overflows. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given a set of N distinct abscissa values and corresponding */
/*     ordinate values, there is a unique polynomial of degree N-1, often */
/*     called the "Lagrange polynomial", that fits the graph defined by */
/*     these values. The Lagrange polynomial can be used to interpolate */
/*     the value of a function at a specified point, given a discrete */
/*     set of values of the function. */

/*     Users of this routine must choose the number of points to use */
/*     in their interpolation method. The authors of Reference [1] have */
/*     this to say on the topic: */

/*        Unless there is solid evidence that the interpolating function */
/*        is close in form to the true function F, it is a good idea to */
/*        be cautious about high-order interpolation. We */
/*        enthusiastically endorse interpolations with 3 or 4 points, we */
/*        are perhaps tolerant of 5 or 6; but we rarely go higher than */
/*        that unless there is quite rigorous monitoring of estimated */
/*        errors. */

/*     The same authors offer this warning on the use of the */
/*     interpolating function for extrapolation: */

/*        ...the dangers of extrapolation cannot be overemphasized: */
/*        An interpolating function, which is perforce an extrapolating */
/*        function, will typically go berserk when the argument X is */
/*        outside the range of tabulated values by more than the typical */
/*        spacing of tabulated points. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Fit a cubic polynomial through the points */

/*            ( -1, -2 ) */
/*            (  0, -7 ) */
/*            (  1, -8 ) */
/*            (  3, 26 ) */

/*        and evaluate this polynomial at X = 2. */

/*        The returned value of P should be 1.0, since the */
/*        unique cubic polynomial that fits these points is */

/*                       3      2 */
/*           F(X)   =   X  + 2*X  - 4*X - 7 */

/*        The returned value of DP should be 16.0, since the */
/*        derivative of f(x) is */

/*            '           2 */
/*           F (X)  =  3*X  + 4*X - 4 */


/*        Example code begins here. */


/*              PROGRAM LGRIND_EX1 */
/*              IMPLICIT NONE */

/*              DOUBLE PRECISION      P */
/*              DOUBLE PRECISION      DP */
/*              DOUBLE PRECISION      XVALS (4) */
/*              DOUBLE PRECISION      YVALS (4) */
/*              DOUBLE PRECISION      WORK  (4,2) */
/*              INTEGER               N */

/*              N         =   4 */

/*              XVALS(1)  =  -1 */
/*              XVALS(2)  =   0 */
/*              XVALS(3)  =   1 */
/*              XVALS(4)  =   3 */

/*              YVALS(1)  =  -2 */
/*              YVALS(2)  =  -7 */
/*              YVALS(3)  =  -8 */
/*              YVALS(4)  =  26 */

/*              CALL LGRIND ( N, XVALS, YVALS, WORK, 2.D0, P, DP ) */

/*              WRITE (*,*) 'P, DP = ', P, DP */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         P, DP =    1.0000000000000000        16.000000000000000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling, */
/*          "Numerical Recipes -- The Art of Scientific Computing," */
/*          chapters 3.0 and 3.1, Cambridge University Press, 1986. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Updated the header to comply with NAIF standard. Added */
/*        "IMPLICIT NONE" to code example. */

/* -    SPICELIB Version 1.0.1, 10-JAN-2014 (NJB) */

/*        Updated description of the workspace array: now the array WORK */
/*        is not described as being allowed to coincide with the input */
/*        YVALS. Such overlap would be a violation of the ANSI Fortran */
/*        77 standard. Corrected a spelling error in header */
/*        documentation. */

/* -    SPICELIB Version 1.0.0, 20-AUG-2002 (NJB) */

/* -& */
/* $ Index_Entries */

/*     interpolate function using Lagrange polynomial */
/*     Lagrange interpolation */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Check in only if an error is detected. */

    /* Parameter adjustments */
    work_dim1 = *n;
    work_offset = work_dim1 + 1;
    yvals_dim1 = *n;
    xvals_dim1 = *n;

    /* Function Body */
    if (return_()) {
	return 0;
    }

/*     No data, no interpolation. */

    if (*n < 1) {
	chkin_("LGRIND", (ftnlen)6);
	setmsg_("Array size must be positive; was #.", (ftnlen)35);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	chkout_("LGRIND", (ftnlen)6);
	return 0;
    }

/*     We're going to compute the value of our interpolating polynomial */
/*     at X by taking advantage of a recursion relation between */
/*     Lagrange polynomials of order n+1 and order n.  The method works */
/*     as follows: */

/*        Define */

/*           P               (x) */
/*            i(i+1)...(i+j) */

/*        to be the unique Lagrange polynomial that interpolates our */
/*        input function at the abscissa values */

/*           x ,  x   , ... x   . */
/*            i    i+1       i+j */


/*        Then we have the recursion relation */

/*           P              (x)  = */
/*            i(i+1)...(i+j) */

/*                                  x - x */
/*                                   i */
/*                                 -----------  *  P                (x) */
/*                                  x - x           (i+1)...(i+j) */
/*                                   i   i+j */


/*                                  x  -  x */
/*                                         i+j */
/*                               + -----------  *  P                (x) */
/*                                  x  -  x         i(i+1)...(i+j-1) */
/*                                   i     i+j */


/*        Repeated application of this relation allows us to build */
/*        successive columns, in left-to-right order, of the */
/*        triangular table */


/*           P (x) */
/*            1 */
/*                    P  (x) */
/*                     12 */
/*           P (x)             P   (x) */
/*            2                 123 */
/*                    P  (x) */
/*                     23               . */
/*                             P   (x) */
/*           .                  234            . */
/*           . */
/*           .        .                               . */
/*                    . */
/*                    .        .                           P      (x) */
/*                             .                      .     12...N */
/*                             . */
/*                                             . */

/*                                      . */


/*                             P           (x) */
/*                              (N-2)(N-1)N */
/*                    P     (x) */
/*                     (N-1)N */
/*           P (x) */
/*            N */


/*        and after N-1 steps arrive at our desired result, */


/*           P       (x). */
/*            12...N */


/*     The computation is easier to do than to describe. */


/*     We'll use the scratch array WORK to contain the current column of */
/*     our interpolation table.  To start out with, WORK(I) will contain */

/*        P (x). */
/*         I */

/*     For columns 2...N of the table, we'll also carry along the */
/*     derivative at X of each interpolating polynomial.  This will */
/*     allow us to find the derivative of the Lagrange polynomial */
/*     at X. */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	work[(i__2 = i__ + work_dim1 - work_offset) < work_dim1 << 1 && 0 <= 
		i__2 ? i__2 : s_rnge("work", i__2, "lgrind_", (ftnlen)396)] = 
		yvals[(i__3 = i__ - 1) < yvals_dim1 && 0 <= i__3 ? i__3 : 
		s_rnge("yvals", i__3, "lgrind_", (ftnlen)396)];
	work[(i__2 = i__ + (work_dim1 << 1) - work_offset) < work_dim1 << 1 &&
		 0 <= i__2 ? i__2 : s_rnge("work", i__2, "lgrind_", (ftnlen)
		397)] = 0.;
    }

/*     Compute columns 2 through N of the table.  Note that DENOM must */
/*     be non-zero, or else a divide-by-zero error will occur. */

    i__1 = *n - 1;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n - j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    denom = xvals[(i__3 = i__ - 1) < xvals_dim1 && 0 <= i__3 ? i__3 : 
		    s_rnge("xvals", i__3, "lgrind_", (ftnlen)409)] - xvals[(
		    i__4 = i__ + j - 1) < xvals_dim1 && 0 <= i__4 ? i__4 : 
		    s_rnge("xvals", i__4, "lgrind_", (ftnlen)409)];
	    if (denom == 0.) {
		chkin_("LGRIND", (ftnlen)6);
		setmsg_("XVALS(#) = XVALS(#) = #", (ftnlen)23);
		errint_("#", &i__, (ftnlen)1);
		i__3 = i__ + j;
		errint_("#", &i__3, (ftnlen)1);
		errdp_("#", &xvals[(i__3 = i__ - 1) < xvals_dim1 && 0 <= i__3 
			? i__3 : s_rnge("xvals", i__3, "lgrind_", (ftnlen)417)
			], (ftnlen)1);
		sigerr_("SPICE(DIVIDEBYZERO)", (ftnlen)19);
		chkout_("LGRIND", (ftnlen)6);
		return 0;
	    }
	    c1 = *x - xvals[(i__3 = i__ + j - 1) < xvals_dim1 && 0 <= i__3 ? 
		    i__3 : s_rnge("xvals", i__3, "lgrind_", (ftnlen)424)];
	    c2 = xvals[(i__3 = i__ - 1) < xvals_dim1 && 0 <= i__3 ? i__3 : 
		    s_rnge("xvals", i__3, "lgrind_", (ftnlen)425)] - *x;

/*           Use the chain rule to compute the derivatives.  Do this */
/*           before computing the function value, because the latter */
/*           computation will overwrite the first column of WORK. */

	    work[(i__3 = i__ + (work_dim1 << 1) - work_offset) < work_dim1 << 
		    1 && 0 <= i__3 ? i__3 : s_rnge("work", i__3, "lgrind_", (
		    ftnlen)432)] = (c1 * work[(i__4 = i__ + (work_dim1 << 1) 
		    - work_offset) < work_dim1 << 1 && 0 <= i__4 ? i__4 : 
		    s_rnge("work", i__4, "lgrind_", (ftnlen)432)] + c2 * work[
		    (i__5 = i__ + 1 + (work_dim1 << 1) - work_offset) < 
		    work_dim1 << 1 && 0 <= i__5 ? i__5 : s_rnge("work", i__5, 
		    "lgrind_", (ftnlen)432)] + (work[(i__6 = i__ + work_dim1 
		    - work_offset) < work_dim1 << 1 && 0 <= i__6 ? i__6 : 
		    s_rnge("work", i__6, "lgrind_", (ftnlen)432)] - work[(
		    i__7 = i__ + 1 + work_dim1 - work_offset) < work_dim1 << 
		    1 && 0 <= i__7 ? i__7 : s_rnge("work", i__7, "lgrind_", (
		    ftnlen)432)])) / denom;

/*           Compute the Ith entry in the Jth column. */

	    work[(i__3 = i__ + work_dim1 - work_offset) < work_dim1 << 1 && 0 
		    <= i__3 ? i__3 : s_rnge("work", i__3, "lgrind_", (ftnlen)
		    438)] = (c1 * work[(i__4 = i__ + work_dim1 - work_offset) 
		    < work_dim1 << 1 && 0 <= i__4 ? i__4 : s_rnge("work", 
		    i__4, "lgrind_", (ftnlen)438)] + c2 * work[(i__5 = i__ + 
		    1 + work_dim1 - work_offset) < work_dim1 << 1 && 0 <= 
		    i__5 ? i__5 : s_rnge("work", i__5, "lgrind_", (ftnlen)438)
		    ]) / denom;
	}
    }

/*     Our results are sitting in WORK(1,1) and WORK(1,2) at this point. */

    *p = work[(i__1 = work_dim1 + 1 - work_offset) < work_dim1 << 1 && 0 <= 
	    i__1 ? i__1 : s_rnge("work", i__1, "lgrind_", (ftnlen)447)];
    *dp = work[(i__1 = (work_dim1 << 1) + 1 - work_offset) < work_dim1 << 1 &&
	     0 <= i__1 ? i__1 : s_rnge("work", i__1, "lgrind_", (ftnlen)448)];
    return 0;
} /* lgrind_ */

