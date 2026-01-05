/* hrmint.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure HRMINT ( Hermite polynomial interpolation  ) */
/* Subroutine */ int hrmint_(integer *n, doublereal *xvals, doublereal *yvals,
	 doublereal *x, doublereal *work, doublereal *f, doublereal *df)
{
    /* System generated locals */
    integer xvals_dim1, yvals_dim1, work_dim1, work_offset, i__1, i__2, i__3, 
	    i__4, i__5, i__6, i__7;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal temp;
    integer this__, prev, next, i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal denom;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal c1, c2;
    integer xi;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    integer xij;

/* $ Abstract */

/*     Evaluate a Hermite interpolating polynomial at a specified */
/*     abscissa value. */

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
/*     YVALS      I   Ordinate and derivative values. */
/*     X          I   Point at which to interpolate the polynomial. */
/*     WORK      I-O  Work space array. */
/*     F          O   Interpolated function value at X. */
/*     DF         O   Interpolated function's derivative at X. */

/* $ Detailed_Input */

/*     N        is the number of points defining the polynomial. */
/*              The arrays XVALS and YVALS contain N and 2*N */
/*              elements respectively. */

/*     XVALS    is an array of length N containing abscissa values. */

/*     YVALS    is an array of length 2*N containing ordinate and */
/*              derivative values for each point in the domain */
/*              defined by XVALS. The elements */

/*                 YVALS( 2*I - 1 ) */
/*                 YVALS( 2*I     ) */

/*              give the value and first derivative of the output */
/*              polynomial at the abscissa value */

/*                 XVALS(I) */

/*              where I ranges from 1 to N. */

/*     WORK     is a work space array. It is used by this routine */
/*              as a scratch area to hold intermediate results. */

/*     X        is the abscissa value at which the interpolating */
/*              polynomial and its derivative are to be evaluated. */

/* $ Detailed_Output */

/*     F, */
/*     DF       are the value and derivative at X of the unique */
/*              polynomial of degree 2N-1 that fits the points and */
/*              derivatives defined by XVALS and YVALS. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If two input abscissas are equal, the error */
/*         SPICE(DIVIDEBYZERO) is signaled. */

/*     2)  If N is less than 1, the error SPICE(INVALIDSIZE) is */
/*         signaled. */

/*     3)  This routine does not attempt to ward off or diagnose */
/*         arithmetic overflows. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Users of this routine must choose the number of points to use */
/*     in their interpolation method. The authors of Reference [1] have */
/*     this to say on the topic: */

/*        Unless there is solid evidence that the interpolating function */
/*        is close in form to the true function f, it is a good idea to */
/*        be cautious about high-order interpolation. We */
/*        enthusiastically endorse interpolations with 3 or 4 points, we */
/*        are perhaps tolerant of 5 or 6; but we rarely go higher than */
/*        that unless there is quite rigorous monitoring of estimated */
/*        errors. */

/*     The same authors offer this warning on the use of the */
/*     interpolating function for extrapolation: */

/*        ...the dangers of extrapolation cannot be overemphasized: */
/*        An interpolating function, which is perforce an extrapolating */
/*        function, will typically go berserk when the argument x is */
/*        outside the range of tabulated values by more than the typical */
/*        spacing of tabulated points. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Fit a 7th degree polynomial through the points ( x, y, y' ) */

/*           ( -1,      6,       3 ) */
/*           (  0,      5,       0 ) */
/*           (  3,   2210,    5115 ) */
/*           (  5,  78180,  109395 ) */

/*        and evaluate this polynomial at x = 2. */

/*        The returned value should be 141.0, and the returned */
/*        derivative value should be 456.0, since the unique 7th degree */
/*        polynomial that fits these constraints is */

/*                     7       2 */
/*           f(x)  =  x   +  2x  + 5 */


/*        Example code begins here. */


/*              PROGRAM HRMINT_EX1 */
/*              IMPLICIT NONE */

/*              DOUBLE PRECISION      ANSWER */
/*              DOUBLE PRECISION      DERIV */
/*              DOUBLE PRECISION      XVALS (4) */
/*              DOUBLE PRECISION      YVALS (8) */
/*              DOUBLE PRECISION      WORK  (8,2) */
/*              INTEGER               N */

/*              N         =   4 */

/*              XVALS(1)  =      -1.D0 */
/*              XVALS(2)  =       0.D0 */
/*              XVALS(3)  =       3.D0 */
/*              XVALS(4)  =       5.D0 */

/*              YVALS(1)  =       6.D0 */
/*              YVALS(2)  =       3.D0 */
/*              YVALS(3)  =       5.D0 */
/*              YVALS(4)  =       0.D0 */
/*              YVALS(5)  =    2210.D0 */
/*              YVALS(6)  =    5115.D0 */
/*              YVALS(7)  =   78180.D0 */
/*              YVALS(8)  =  109395.D0 */

/*              CALL HRMINT ( N, XVALS, YVALS, 2.D0, WORK, ANSWER, */
/*             .                                           DERIV ) */

/*              WRITE (*,*) 'ANSWER = ', ANSWER */
/*              WRITE (*,*) 'DERIV  = ', DERIV */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         ANSWER =    141.00000000000000 */
/*         DERIV  =    456.00000000000000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling, */
/*          "Numerical Recipes -- The Art of Scientific Computing," */
/*          chapters 3.0 and 3.1, Cambridge University Press, 1986. */

/*     [2]  S. Conte and C. de Boor, "Elementary Numerical Analysis -- An */
/*          Algorithmic Approach," 3rd Edition, p 64, McGraw-Hill, 1980. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.2, 01-OCT-2021 (NJB) (JDR) */

/*        Edited the header to comply with NAIF standard. Fixed */
/*        a few more comment typos. Added IMPLICIT NONE to code */
/*        example. */

/* -    SPICELIB Version 1.2.1, 28-JAN-2014 (NJB) */

/*        Fixed a few comment typos. */

/* -    SPICELIB Version 1.2.0, 01-FEB-2002 (NJB) (EDW) */

/*        Bug fix: declarations of local variables XI and XIJ */
/*        were changed from DOUBLE PRECISION to INTEGER. */
/*        Note: bug had no effect on behavior of this routine. */

/* -    SPICELIB Version 1.1.0, 28-DEC-2001 (NJB) */

/*        Blanks following final newline were truncated to */
/*        suppress compilation warnings on the SGI-N32 platform. */

/* -    SPICELIB Version 1.0.0, 01-MAR-2000 (NJB) */

/* -& */
/* $ Index_Entries */

/*     interpolate function using Hermite polynomial */
/*     Hermite interpolation */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Check in only if an error is detected. */

    /* Parameter adjustments */
    work_dim1 = *n << 1;
    work_offset = work_dim1 + 1;
    yvals_dim1 = *n << 1;
    xvals_dim1 = *n;

    /* Function Body */
    if (return_()) {
	return 0;
    }

/*     No data, no interpolation. */

    if (*n < 1) {
	chkin_("HRMINT", (ftnlen)6);
	setmsg_("Array size must be positive; was #.", (ftnlen)35);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	chkout_("HRMINT", (ftnlen)6);
	return 0;
    }

/*     Copy the input array into WORK.  After this, the first column */
/*     of WORK represents the first column of our triangular */
/*     interpolation table. */

    i__1 = *n << 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	work[(i__2 = i__ + work_dim1 - work_offset) < work_dim1 << 1 && 0 <= 
		i__2 ? i__2 : s_rnge("work", i__2, "hrmint_", (ftnlen)318)] = 
		yvals[(i__3 = i__ - 1) < yvals_dim1 && 0 <= i__3 ? i__3 : 
		s_rnge("yvals", i__3, "hrmint_", (ftnlen)318)];
    }

/*     Compute the second column of the interpolation table: this */
/*     consists of the N-1 values obtained by evaluating the */
/*     first-degree interpolants at X. We'll also evaluate the */
/*     derivatives of these interpolants at X and save the results in */
/*     the second column of WORK. Because the derivative computations */
/*     depend on the function computations from the previous column in */
/*     the interpolation table, and because the function interpolation */
/*     overwrites the previous column of interpolated function values, */
/*     we must evaluate the derivatives first. */

    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c1 = xvals[(i__2 = i__) < xvals_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		"xvals", i__2, "hrmint_", (ftnlen)335)] - *x;
	c2 = *x - xvals[(i__2 = i__ - 1) < xvals_dim1 && 0 <= i__2 ? i__2 : 
		s_rnge("xvals", i__2, "hrmint_", (ftnlen)336)];
	denom = xvals[(i__2 = i__) < xvals_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		"xvals", i__2, "hrmint_", (ftnlen)337)] - xvals[(i__3 = i__ - 
		1) < xvals_dim1 && 0 <= i__3 ? i__3 : s_rnge("xvals", i__3, 
		"hrmint_", (ftnlen)337)];
	if (denom == 0.) {
	    chkin_("HRMINT", (ftnlen)6);
	    setmsg_("XVALS(#) = XVALS(#) = #", (ftnlen)23);
	    errint_("#", &i__, (ftnlen)1);
	    i__2 = i__ + 1;
	    errint_("#", &i__2, (ftnlen)1);
	    errdp_("#", &xvals[(i__2 = i__ - 1) < xvals_dim1 && 0 <= i__2 ? 
		    i__2 : s_rnge("xvals", i__2, "hrmint_", (ftnlen)346)], (
		    ftnlen)1);
	    sigerr_("SPICE(DIVIDEBYZERO)", (ftnlen)19);
	    chkout_("HRMINT", (ftnlen)6);
	    return 0;
	}

/*        The second column of WORK contains interpolated derivative */
/*        values. */

/*        The odd-indexed interpolated derivatives are simply the input */
/*        derivatives. */

	prev = (i__ << 1) - 1;
	this__ = prev + 1;
	next = this__ + 1;
	work[(i__2 = prev + (work_dim1 << 1) - work_offset) < work_dim1 << 1 
		&& 0 <= i__2 ? i__2 : s_rnge("work", i__2, "hrmint_", (ftnlen)
		364)] = work[(i__3 = this__ + work_dim1 - work_offset) < 
		work_dim1 << 1 && 0 <= i__3 ? i__3 : s_rnge("work", i__3, 
		"hrmint_", (ftnlen)364)];

/*        The even-indexed interpolated derivatives are the slopes of */
/*        the linear interpolating polynomials for adjacent input */
/*        abscissa/ordinate pairs. */

	work[(i__2 = this__ + (work_dim1 << 1) - work_offset) < work_dim1 << 
		1 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, "hrmint_", (
		ftnlen)371)] = (work[(i__3 = next + work_dim1 - work_offset) <
		 work_dim1 << 1 && 0 <= i__3 ? i__3 : s_rnge("work", i__3, 
		"hrmint_", (ftnlen)371)] - work[(i__4 = prev + work_dim1 - 
		work_offset) < work_dim1 << 1 && 0 <= i__4 ? i__4 : s_rnge(
		"work", i__4, "hrmint_", (ftnlen)371)]) / denom;

/*        The first column of WORK contains interpolated function values. */
/*        The odd-indexed entries are the linear Taylor polynomials, */
/*        for each input abscissa value, evaluated at X. */

	temp = work[(i__2 = this__ + work_dim1 - work_offset) < work_dim1 << 
		1 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, "hrmint_", (
		ftnlen)378)] * (*x - xvals[(i__3 = i__ - 1) < xvals_dim1 && 0 
		<= i__3 ? i__3 : s_rnge("xvals", i__3, "hrmint_", (ftnlen)378)
		]) + work[(i__4 = prev + work_dim1 - work_offset) < work_dim1 
		<< 1 && 0 <= i__4 ? i__4 : s_rnge("work", i__4, "hrmint_", (
		ftnlen)378)];
	work[(i__2 = this__ + work_dim1 - work_offset) < work_dim1 << 1 && 0 
		<= i__2 ? i__2 : s_rnge("work", i__2, "hrmint_", (ftnlen)381)]
		 = (c1 * work[(i__3 = prev + work_dim1 - work_offset) < 
		work_dim1 << 1 && 0 <= i__3 ? i__3 : s_rnge("work", i__3, 
		"hrmint_", (ftnlen)381)] + c2 * work[(i__4 = next + work_dim1 
		- work_offset) < work_dim1 << 1 && 0 <= i__4 ? i__4 : s_rnge(
		"work", i__4, "hrmint_", (ftnlen)381)]) / denom;
	work[(i__2 = prev + work_dim1 - work_offset) < work_dim1 << 1 && 0 <= 
		i__2 ? i__2 : s_rnge("work", i__2, "hrmint_", (ftnlen)384)] = 
		temp;
    }

/*     The last column entries were not computed by the preceding loop; */
/*     compute them now. */

    work[(i__1 = (*n << 1) - 1 + (work_dim1 << 1) - work_offset) < work_dim1 
	    << 1 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "hrmint_", (
	    ftnlen)392)] = work[(i__2 = (*n << 1) + work_dim1 - work_offset) <
	     work_dim1 << 1 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, "hrmi"
	    "nt_", (ftnlen)392)];
    work[(i__1 = (*n << 1) - 1 + work_dim1 - work_offset) < work_dim1 << 1 && 
	    0 <= i__1 ? i__1 : s_rnge("work", i__1, "hrmint_", (ftnlen)393)] =
	     work[(i__2 = (*n << 1) + work_dim1 - work_offset) < work_dim1 << 
	    1 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, "hrmint_", (ftnlen)
	    393)] * (*x - xvals[(i__3 = *n - 1) < xvals_dim1 && 0 <= i__3 ? 
	    i__3 : s_rnge("xvals", i__3, "hrmint_", (ftnlen)393)]) + work[(
	    i__4 = (*n << 1) - 1 + work_dim1 - work_offset) < work_dim1 << 1 
	    && 0 <= i__4 ? i__4 : s_rnge("work", i__4, "hrmint_", (ftnlen)393)
	    ];

/*     Compute columns 3 through 2*N of the table. */

    i__1 = (*n << 1) - 1;
    for (j = 2; j <= i__1; ++j) {
	i__2 = (*n << 1) - j;
	for (i__ = 1; i__ <= i__2; ++i__) {

/*           In the theoretical construction of the interpolation table, */
/*           there are 2*N abscissa values, since each input abscissa */
/*           value occurs with multiplicity two. In this theoretical */
/*           construction, the Jth column of the interpolation table */
/*           contains results of evaluating interpolants that span J+1 */
/*           consecutive abscissa values.  The indices XI and XIJ below */
/*           are used to pick the correct abscissa values out of the */
/*           physical XVALS array, in which the abscissa values are not */
/*           repeated. */

	    xi = (i__ + 1) / 2;
	    xij = (i__ + j + 1) / 2;
	    c1 = xvals[(i__3 = xij - 1) < xvals_dim1 && 0 <= i__3 ? i__3 : 
		    s_rnge("xvals", i__3, "hrmint_", (ftnlen)416)] - *x;
	    c2 = *x - xvals[(i__3 = xi - 1) < xvals_dim1 && 0 <= i__3 ? i__3 :
		     s_rnge("xvals", i__3, "hrmint_", (ftnlen)417)];
	    denom = xvals[(i__3 = xij - 1) < xvals_dim1 && 0 <= i__3 ? i__3 : 
		    s_rnge("xvals", i__3, "hrmint_", (ftnlen)419)] - xvals[(
		    i__4 = xi - 1) < xvals_dim1 && 0 <= i__4 ? i__4 : s_rnge(
		    "xvals", i__4, "hrmint_", (ftnlen)419)];
	    if (denom == 0.) {
		chkin_("HRMINT", (ftnlen)6);
		setmsg_("XVALS(#) = XVALS(#) = #", (ftnlen)23);
		errint_("#", &xi, (ftnlen)1);
		errint_("#", &xij, (ftnlen)1);
		errdp_("#", &xvals[(i__3 = xi - 1) < xvals_dim1 && 0 <= i__3 ?
			 i__3 : s_rnge("xvals", i__3, "hrmint_", (ftnlen)427)]
			, (ftnlen)1);
		sigerr_("SPICE(DIVIDEBYZERO)", (ftnlen)19);
		chkout_("HRMINT", (ftnlen)6);
		return 0;
	    }

/*           Compute the interpolated derivative at X for the Ith */
/*           interpolant. This is the derivative with respect to X of */
/*           the expression for the interpolated function value, which */
/*           is the second expression below. This derivative computation */
/*           is done first because it relies on the interpolated */
/*           function values from the previous column of the */
/*           interpolation table. */

/*           The derivative expression here corresponds to equation */
/*           2.35 on page 64 in reference [2]. */

	    work[(i__3 = i__ + (work_dim1 << 1) - work_offset) < work_dim1 << 
		    1 && 0 <= i__3 ? i__3 : s_rnge("work", i__3, "hrmint_", (
		    ftnlen)446)] = (c1 * work[(i__4 = i__ + (work_dim1 << 1) 
		    - work_offset) < work_dim1 << 1 && 0 <= i__4 ? i__4 : 
		    s_rnge("work", i__4, "hrmint_", (ftnlen)446)] + c2 * work[
		    (i__5 = i__ + 1 + (work_dim1 << 1) - work_offset) < 
		    work_dim1 << 1 && 0 <= i__5 ? i__5 : s_rnge("work", i__5, 
		    "hrmint_", (ftnlen)446)] + (work[(i__6 = i__ + 1 + 
		    work_dim1 - work_offset) < work_dim1 << 1 && 0 <= i__6 ? 
		    i__6 : s_rnge("work", i__6, "hrmint_", (ftnlen)446)] - 
		    work[(i__7 = i__ + work_dim1 - work_offset) < work_dim1 <<
		     1 && 0 <= i__7 ? i__7 : s_rnge("work", i__7, "hrmint_", (
		    ftnlen)446)])) / denom;

/*           Compute the interpolated function value at X for the Ith */
/*           interpolant. */

	    work[(i__3 = i__ + work_dim1 - work_offset) < work_dim1 << 1 && 0 
		    <= i__3 ? i__3 : s_rnge("work", i__3, "hrmint_", (ftnlen)
		    453)] = (c1 * work[(i__4 = i__ + work_dim1 - work_offset) 
		    < work_dim1 << 1 && 0 <= i__4 ? i__4 : s_rnge("work", 
		    i__4, "hrmint_", (ftnlen)453)] + c2 * work[(i__5 = i__ + 
		    1 + work_dim1 - work_offset) < work_dim1 << 1 && 0 <= 
		    i__5 ? i__5 : s_rnge("work", i__5, "hrmint_", (ftnlen)453)
		    ]) / denom;
	}
    }

/*     Our interpolated function value is sitting in WORK(1,1) at this */
/*     point.  The interpolated derivative is located in WORK(1,2). */

    *f = work[(i__1 = work_dim1 + 1 - work_offset) < work_dim1 << 1 && 0 <= 
	    i__1 ? i__1 : s_rnge("work", i__1, "hrmint_", (ftnlen)463)];
    *df = work[(i__1 = (work_dim1 << 1) + 1 - work_offset) < work_dim1 << 1 &&
	     0 <= i__1 ? i__1 : s_rnge("work", i__1, "hrmint_", (ftnlen)464)];
    return 0;
} /* hrmint_ */

