/* qderiv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure QDERIV ( Quadratic derivative ) */
/* Subroutine */ int qderiv_(integer *ndim, doublereal *f0, doublereal *f2, 
	doublereal *delta, doublereal *dfdt)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), vlcomg_(integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), sigerr_(char *, ftnlen), chkout_(char *, ftnlen), 
	    setmsg_(char *, ftnlen);

/* $ Abstract */

/*     Estimate the derivative of a function by finding the derivative */
/*     of a quadratic approximating function. This derivative estimate */
/*     is equivalent to that found by computing the average of forward */
/*     and backward differences. */

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
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  ------------------------------------------------- */
/*     NDIM       I   Dimension of function to be differentiated. */
/*     F0         I   Function values at left endpoint. */
/*     F2         I   Function values at right endpoint. */
/*     DELTA      I   Separation of abscissa points. */
/*     DFDT       O   Derivative vector. */

/* $ Detailed_Input */

/*     NDIM     is the dimension of the function to be */
/*              differentiated. The derivative of each */
/*              function component will be found. */

/*     F0       is an array of NDIM function values at a point on */
/*              the real line; we'll refer to this point as X0. */

/*     F2       is an array of NDIM function values at a second */
/*              point on the real line; we'll refer to this point */
/*              as X2. The points X0 and X2 must satisfy */

/*                 X2 = X0 + 2 * DELTA */


/*     DELTA    is one half of the difference between X2 and X0: */

/*                 DELTA = ( X2 - X0 ) / 2 */

/*              DELTA may be negative but must be non-zero. */

/* $ Detailed_Output */

/*     DFDT     is an N-dimensional vector representing an estimate */
/*              of the derivative of the input function at the */
/*              midpoint X1 of the interval between X0 and X2. */

/*              The Ith component of DFDT is */

/*                 ( 1 / (2*DELTA) ) * ( F2(I) - F0(I) ) */

/*              We may regard this estimate as the derivative */
/*              at X1 of a parabola fitted to the points */

/*                  ( X0, F0(I) ),  ( X2, F2(I) ) */

/*              We may also regard this derivative as the average */
/*              of the forward and backward first-order */
/*              differences of the input function defined by */
/*              F0(I), F2(I), and DELTA. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If DELTA is zero, the error SPICE(DIVIDEBYZERO) is signaled. */

/*     2)  If NDIM is less than 1, this routine will fail in a */
/*         system-dependent manner. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine estimates the derivative of a vector-valued function */
/*     using the average of forward and backward differences. */

/*     The derivative estimate computed by this routine is equivalent to */
/*     that obtained by fitting each component of the function with a */
/*     parabola at the points */

/*        (X0, f(X0)), (X1, f(X1)), (X2, f(X2)) */

/*     where */

/*         X0  =  X1 - DELTA */
/*         X2  =  X1 + DELTA */

/*     and finding the derivative of the parabolas at X1. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */


/*     1) Estimate the derivative of x**2 at x = 2. */

/*        Example code begins here. */


/*              PROGRAM QDERIV_EX1 */
/*              IMPLICIT NONE */

/*              DOUBLE PRECISION     DELTA */
/*              DOUBLE PRECISION     DFDT  (1) */
/*              DOUBLE PRECISION     F0    (1) */
/*              DOUBLE PRECISION     F2    (1) */
/*              INTEGER              N */

/*              N     = 1 */
/*              DELTA = 1.D-3 */
/*              F0(1) = ( 2.D0 - DELTA ) ** 2.D0 */
/*              F2(1) = ( 2.D0 + DELTA ) ** 2.D0 */

/*              CALL QDERIV ( N, F0, F2, DELTA, DFDT ) */

/*              WRITE ( *, '(1X,A,E25.16)'  ) '4 - DFDT(1) = ', */
/*             .                               4 - DFDT(1) */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         4 - DFDT(1) =    0.4547473508864641E-12 */


/*        Note that the difference displayed is platform-dependent, but */
/*        should be on the order of 1.E-12. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 05-AUG-2020 (JDR) */

/*        Changed input argument name "N" to "NDIM" for consistency with */
/*        other routines. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) */

/* -& */
/* $ Index_Entries */

/*     Estimate function derivative using quadratic fit */

/* -& */

/*     Use discovery check-in. */

    if (*delta == 0.) {
	chkin_("QDERIV", (ftnlen)6);
	setmsg_("Delta abscissa value is zero; a non-zero value is required.",
		 (ftnlen)59);
	sigerr_("SPICE(DIVIDEBYZERO)", (ftnlen)19);
	chkout_("QDERIV", (ftnlen)6);
	return 0;
    }


/*     Our derivative estimate is */

/*            1/2 * (   Backward_difference / DELTA */
/*                    + Forward_difference  / DELTA ) */

/*        =   ( 1/(2*DELTA) ) * ( ( F(X2) - F(X1) ) +  ( F(X1) - F(X0) ) */

/*        =   ( 1/(2*DELTA) ) * ( ( F(X2) - F(X0) ) */

/*        =    (0.5/DELTA) * F(X2)  +  (-0.5/DELTA) * F(X0) */


    d__1 = .5 / *delta;
    d__2 = -.5 / *delta;
    vlcomg_(ndim, &d__1, f2, &d__2, f0, dfdt);
    return 0;
} /* qderiv_ */

