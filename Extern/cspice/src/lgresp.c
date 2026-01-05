/* lgresp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LGRESP ( Lagrange interpolation on equally spaced points ) */
doublereal lgresp_(integer *n, doublereal *first, doublereal *step, 
	doublereal *yvals, doublereal *work, doublereal *x)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val;

    /* Local variables */
    doublereal newx;
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal c1, c2;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Evaluate a Lagrange interpolating polynomial for a specified */
/*     set of coordinate pairs whose first components are equally */
/*     spaced, at a specified abscissa value. */

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
/*     FIRST      I   First abscissa value. */
/*     STEP       I   Step size. */
/*     YVALS      I   Ordinate values. */
/*     WORK      I-O  Work space array. */
/*     X          I   Point at which to interpolate the polynomial. */

/*     The function returns the value at X of the unique polynomial of */
/*     degree N-1 that fits the points in the plane defined by FIRST, */
/*     STEP, and YVALS. */

/* $ Detailed_Input */

/*     N        is the number of points defining the polynomial. The */
/*              array YVALS contains N elements. */

/*     FIRST, */
/*     STEP     are, respectively, a starting abscissa value and a */
/*              step size that define the set of abscissa values */
/*              at which a Lagrange interpolating polynomial is to */
/*              be defined. The set of abscissa values is */

/*                 FIRST   +   I * STEP,     I = 0, ..., N-1 */

/*              STEP must be non-zero. */

/*     YVALS    is an array of ordinate values that, together with */
/*              the abscissa values defined by FIRST and STEP, */
/*              define N ordered pairs belonging to the graph of */
/*              a function. The set of points */

/*                 (  FIRST  +  (I-1)*STEP,   YVALS(I)  ) */

/*              where I ranges from 1 to N, define the Lagrange */
/*              polynomial used for interpolation. */

/*     WORK     is a work space array of the same dimension as YVALS. */
/*              It is used by this routine as a scratch area to hold */
/*              intermediate results. */

/*     X        is the abscissa value at which the interpolating */
/*              polynomial is to be evaluated. */

/* $ Detailed_Output */

/*     The function returns the value at X of the unique polynomial of */
/*     degree N-1 that fits the points in the plane defined by FIRST, */
/*     STEP, and YVALS. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If STEP is zero, the error SPICE(INVALIDSTEPSIZE) is */
/*         signaled. The function will return the value 0.D0. */

/*     2)  If N is less than 1, the error SPICE(INVALIDSIZE) is */
/*         signaled. The function will return the value 0.D0. */

/*     3)  This routine does not attempt to ward off or diagnose */
/*         arithmetic overflows. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given a set of N distinct abscissa values and corresponding */
/*     ordinate values, there is a unique polynomial of degree N-1, */
/*     often called the "Lagrange polynomial", that fits the graph */
/*     defined by these values. The Lagrange polynomial can be used to */
/*     interpolate the value of a function at a specified point, given a */
/*     discrete set of values of the function. */

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

/*     For Lagrange interpolation on unequally spaced abscissa values, */
/*     see the SPICELIB routine LGRINT. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */

/*     1) Fit a cubic polynomial through the points */

/*            ( -1,  -2 ) */
/*            (  1,  -8 ) */
/*            (  3,  26 ) */
/*            (  5, 148 ) */

/*        and evaluate this polynomial at x = 2. */

/*        The returned value of LGRESP should be 1.D0, since the */
/*        unique cubic polynomial that fits these points is */

/*                     3      2 */
/*           F(X)  =  X  + 2*X  - 4*X - 7 */


/*        Example code begins here. */


/*              PROGRAM LGRESP_EX1 */
/*              IMPLICIT NONE */

/*              DOUBLE PRECISION      LGRESP */
/*              DOUBLE PRECISION      ANSWER */
/*              DOUBLE PRECISION      FIRST */
/*              DOUBLE PRECISION      STEP */
/*              DOUBLE PRECISION      YVALS (4) */
/*              DOUBLE PRECISION      WORK  (4) */
/*              INTEGER               N */

/*              N         =   4 */
/*              FIRST     =  -1.D0 */
/*              STEP      =   2.D0 */

/*              YVALS(1)  =  -2.D0 */
/*              YVALS(2)  =  -8.D0 */
/*              YVALS(3)  =  26.D0 */
/*              YVALS(4)  = 148.D0 */

/*              ANSWER    =   LGRESP ( N,     FIRST, STEP, */
/*             .                       YVALS, WORK,  2.D0 ) */

/*              WRITE (*,*) 'ANSWER = ', ANSWER */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         ANSWER =    1.0000000000000000 */


/*     2) Solve the same problem using a negative step. In order to */
/*        find the solution, set the elements of YVALS in reverse order. */

/*        The returned value of LGRESP would still be 1.D0. */


/*        Example code begins here. */


/*              PROGRAM LGRESP_EX2 */
/*              IMPLICIT NONE */

/*              DOUBLE PRECISION      LGRESP */
/*              DOUBLE PRECISION      ANSWER */
/*              DOUBLE PRECISION      FIRST */
/*              DOUBLE PRECISION      STEP */
/*              DOUBLE PRECISION      YVALS (4) */
/*              DOUBLE PRECISION      WORK  (4) */
/*              INTEGER               N */

/*              N         =   4 */
/*              FIRST     =   5.D0 */
/*              STEP      =  -2.D0 */

/*              YVALS(1)  = 148.D0 */
/*              YVALS(2)  =  26.D0 */
/*              YVALS(3)  =  -8.D0 */
/*              YVALS(4)  =  -2.D0 */

/*              ANSWER    =   LGRESP ( N,     FIRST, STEP, */
/*             .                       YVALS, WORK,  2.D0 ) */

/*              WRITE (*,*) 'ANSWER = ', ANSWER */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         ANSWER =    1.0000000000000000 */


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

/* -    SPICELIB Version 1.1.0, 19-FEB-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Update "N" and "WORK" detailed descriptions to remove */
/*        references to nonexistent arguments. */

/*        Updated the header to comply with NAIF standard. */
/*        Added IMPLICIT NONE to code example. Added second example */
/*        providing code to solve the problem of example #1 using a */
/*        negative step. */

/* -    SPICELIB Version 1.0.1, 10-JAN-2014 (NJB) */

/*        Updated description of the workspace array: now the array WORK */
/*        is not described as being allowed to coincide with the input */
/*        YVALS. Such overlap would be a violation of the ANSI Fortran */
/*        77 standard. Corrected several spelling errors in header */
/*        documentation. */

/* -    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB) */

/* -& */
/* $ Index_Entries */

/*     interpolate function using Lagrange polynomial */
/*     Lagrange interpolation */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Check in only if an error is detected. */

    if (return_()) {
	ret_val = 0.;
	return ret_val;
    }

/*     No data, no interpolation. */

    if (*n < 1) {
	ret_val = 0.;
	chkin_("LGRESP", (ftnlen)6);
	setmsg_("Array size must be positive; was #.", (ftnlen)35);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	chkout_("LGRESP", (ftnlen)6);
	return ret_val;
    }

/*     The step size must be non-zero. */

    if (*step == 0.) {
	ret_val = 0.;
	chkin_("LGRESP", (ftnlen)6);
	setmsg_("Step size was zero.", (ftnlen)19);
	sigerr_("SPICE(INVALIDSTEPSIZE)", (ftnlen)22);
	chkout_("LGRESP", (ftnlen)6);
	return ret_val;
    }

/*     We can simplify the interpolation problem by shifting */
/*     and scaling the abscissa values so that they start at 1 */
/*     and are separated by a unit step.  All we need to do is */
/*     shift and scale X. */

    newx = (*x - *first) / *step + 1.;

/*     We're going to compute the value of our interpolating polynomial */
/*     at X by taking advantage of a recursion relation between */
/*     Lagrange polynomials of order n+1 and order n.  The method works */
/*     as follows for the case of abscissa values that are not */
/*     necessarily uniformly spaced: */

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

/*     In the current case, we've arranged the problem so that */

/*       x    =   i. */
/*        i */

/*     We'll use the scratch array WORK to contain the current column of */
/*     our interpolation table.  To start out with, WORK(I) will contain */

/*        P (x). */
/*         I */


    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	work[i__ - 1] = yvals[i__ - 1];
    }

/*     Compute columns 2 through N of the table. */

    i__1 = *n - 1;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n - j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    c1 = (doublereal) (i__ + j) - newx;
	    c2 = newx - (doublereal) i__;
	    work[i__ - 1] = (c1 * work[i__ - 1] + c2 * work[i__]) / (
		    doublereal) j;
	}
    }

/*     Our result is sitting in WORK(1) at this point. */

    ret_val = work[0];
    return ret_val;
} /* lgresp_ */

