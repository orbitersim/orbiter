/* polyds.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure POLYDS ( Compute a Polynomial and its Derivatives ) */
/* Subroutine */ int polyds_(doublereal *coeffs, integer *deg, integer *
	nderiv, doublereal *t, doublereal *p)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, k;
    doublereal scale;

/* $ Abstract */

/*     Compute the value of a polynomial and its first */
/*     NDERIV derivatives at the value T. */

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
/*     MATH */
/*     POLYNOMIAL */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     COEFFS     I   Coefficients of the polynomial to be evaluated. */
/*     DEG        I   Degree of the polynomial to be evaluated. */
/*     NDERIV     I   Number of derivatives to compute. */
/*     T          I   Point to evaluate the polynomial and derivatives */
/*     P          O   Value of polynomial and derivatives. */

/* $ Detailed_Input */

/*     COEFFS   are the coefficients of the polynomial that is */
/*              to be evaluated. The first element of this array */
/*              should be the constant term, the second element the */
/*              linear coefficient, the third term the quadratic */
/*              coefficient, and so on. The number of coefficients */
/*              supplied should be one more than DEG. */

/*                 F(X) =   COEFFS(1) + COEFFS(2)*X + COEFFS(3)*X^2 */

/*                        + COEFFS(4)*X^4 + ... + COEFFS(DEG+1)*X^DEG */

/*     DEG      is the degree of the polynomial to be evaluated. DEG */
/*              should be one less than the number of coefficients */
/*              supplied. */

/*     NDERIV   is the number of derivatives to compute. If NDERIV */
/*              is zero, only the polynomial will be evaluated. If */
/*              NDERIV = 1, then the polynomial and its first */
/*              derivative will be evaluated, and so on. If the value */
/*              of NDERIV is negative, the routine returns */
/*              immediately. */

/*     T        is the point at which the polynomial and its */
/*              derivatives should be evaluated. */

/* $ Detailed_Output */

/*     P        is an array containing the value of the polynomial and */
/*              its derivatives evaluated at T. The first element of */
/*              the array contains the value of P at T. The second */
/*              element of the array contains the value of the first */
/*              derivative of P at T and so on. The NDERIV + 1'st */
/*              element of the array contains the NDERIV'th derivative */
/*              of P evaluated at T. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If NDERIV is less than zero, the routine simply returns. */

/*     2)  If the degree of the polynomial is less than 0, the routine */
/*         returns the first NDERIV+1 elements of P set to 0. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine uses the user supplied coefficients (COEFFS) */
/*     to evaluate a polynomial (having these coefficients) and its */
/*     derivatives at the point T. The zero'th derivative of the */
/*     polynomial is regarded as the polynomial itself. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) For the polynomial */

/*           F(x) = 1 + 3*x + 0.5*x^2 + x^3 + 0.5*x^4 - x^5 + x^6 */

/*        the coefficient set */

/*           Degree  coeffs */
/*           ------  ------ */
/*           0       1 */
/*           1       3 */
/*           2       0.5 */
/*           3       1 */
/*           4       0.5 */
/*           5      -1 */
/*           6       1 */

/*        Compute the value of the polynomial and it's first */
/*        3 derivatives at the value T = 1.0. We expect: */

/*           Derivative Number     T = 1 */
/*           ------------------    ----- */
/*           F(x)         0        6 */
/*           F'(x)        1        10 */
/*           F''(x)       2        23 */
/*           F'''(x)      3        78 */


/*        Example code begins here. */


/*              PROGRAM POLYDS_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER               NDERIV */
/*              PARAMETER           ( NDERIV = 3 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      COEFFS (7) */
/*              DOUBLE PRECISION      P      ( NDERIV + 1 ) */
/*              DOUBLE PRECISION      T */

/*              INTEGER               DEG */
/*              INTEGER               I */

/*              DATA                  COEFFS / 1.D0,   3.D0, */
/*             .                               0.5D0,  1.D0, */
/*             .                               0.5D0, -1.D0, */
/*             .                               1.D0          / */

/*              T = 1.D0 */
/*              DEG = 6 */

/*              CALL POLYDS ( COEFFS, DEG, NDERIV, T, P ) */

/*              DO I= 1, NDERIV + 1 */
/*                 WRITE(*,*) 'P = ', P(I) */
/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         P =    6.0000000000000000 */
/*         P =    10.000000000000000 */
/*         P =    23.000000000000000 */
/*         P =    78.000000000000000 */


/* $ Restrictions */

/*     1)  Depending on the coefficients the user should be careful when */
/*         taking high order derivatives. As the example shows, these */
/*         can get big in a hurry. In general the coefficients of the */
/*         derivatives of a polynomial grow at a rate greater */
/*         than N! (N factorial). */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 16-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Updated the header to comply with NAIF standard. Added */
/*        full code example. Updated Exception #2 to properly describe */
/*        the routine's behavior. */

/* -    SPICELIB Version 1.1.0, 11-JUL-1995 (KRG) */

/*        Replaced the function calls to DFLOAT with standard conforming */
/*        calls to DBLE. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     compute a polynomial and its derivatives */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.0.1, 30-DEC-1988 (WLT) */

/*        The error free specification was added as well as notes */
/*        on exceptional degree or derivative requests. */

/* -& */

/*     Local variables */

    if (*nderiv < 0) {
	return 0;
    }

/*     The following loops may not look like much, but they compute */
/*     P(T), P'(T), P''(T), ... etc. */

/*     To see why, recall that if A_0 through A_N are the coefficients */
/*     of a polynomial, then P(t) can be computed from the sequence */
/*     of polynomials given by: */

/*        P_0(t) = 0 */
/*        P_1(t) = t*P_0(t)     +   A_N */
/*        P_2(t) = t*P_1(t)     +   A_[N-1] */
/*               . */
/*               . */
/*               . */
/*        P_n(t) = t*P_[n-1](t) +   A_0 */

/*     The final polynomial in this list is in fact P(t).  From this */
/*     it follows that P'(t) is given by P_n'(t).  But */

/*        P_n'(t)     = t*P_[n-1]'(t) +   P_[n-1](t) */

/*     and */

/*        P_[n-1]'(t) = t*P_[n-2]'(t) +   P_[n-2](t) */
/*                    . */
/*                    . */
/*                    . */
/*        P_2'(t)     = t*P_1'(t)     +   P_1(t) */
/*        P_1'(t)     = t*P_0'(t)     +   P_0(t) */
/*        P_0'(t)     = 0 */

/*     Rearranging the sequence we have a recursive method */
/*     for computing P'(t).  At the i'th stage we require only the i-1st */
/*     polynomials P_[i-1] and P_[i-1]' . */

/*        P_0'(t)     = 0 */
/*        P_1'(t)     = t*P_0'(t)     +   P_0(t) */
/*        P_2'(t)     = t*P_1'(t)     +   P_1(t) */
/*                    . */
/*                    . */
/*                    . */
/*        P_[n-1]'(t) = t*P_[n-2]'(t) +   P_[n-2](t) */
/*        P_n'(t)     = t*P_[n-1]'(t) +   P_[n-1](t) */


/*     Similarly, */

/*        P_0''(t)     = 0 */
/*        P_1''(t)     = t*P_0''(t)     +   2*P_0'(t) */
/*        P_2''(t)     = t*P_1''(t)     +   2*P_1'(t) */
/*                     . */
/*                     . */
/*                     . */
/*        P_[n-1]''(t) = t*P_[n-2]''(t) +   2*P_[n-2]'(t) */



/*        P_0'''(t)     = 0 */
/*        P_1'''(t)     = t*P_0'''(t)     +   3*P_0''(t) */
/*        P_2'''(t)     = t*P_1'''(t)     +   3*P_1''(t) */
/*                      . */
/*                      . */
/*                      . */
/*        P_[n-1]'''(t) = t*P_[n-2]'''(t) +   3*P_[n-2]''(t) */

/*     Thus if P(I) contains the k'th iterations of the i'th derivative */
/*     computation of P and P(I-1) contains the k'th iteration of the */
/*     i-1st derivative of P then, t*P(I) + I*P(I-1) is the value of the */
/*     k+1st iteration of the computation of the i'th derivative of */
/*     P. This can then be stored in P(I). */

/*     If in a loop we compute in-place k'th iteration of the */
/*     I'th derivative before we perform the in-place k'th iteration */
/*     of the I-1st and I-2cnd derivative, then the k-1'th values */
/*     of the I-1st and I-2cnd will not be altered and will be available */
/*     for the computation of the k'th iteration of the I-1st */
/*     derivative.  This observation gives us an economical way to */
/*     compute all of the derivatives (including the zero'th derivative) */
/*     in place.  We simply compute the iterates of the high order */
/*     derivatives first. */

/*     Initialize the polynomial value (and all of its derivatives) to be */
/*     zero. */

    i__1 = *nderiv;
    for (i__ = 0; i__ <= i__1; ++i__) {
	p[i__] = 0.;
    }

/*     Set up the loop "counters" (they count backwards) for the first */
/*     pass through the loop. */

    k = *deg;
    i__ = *nderiv;
    scale = (doublereal) (*nderiv);
    while(k >= 0) {
	while(i__ > 0) {
	    p[i__] = *t * p[i__] + scale * p[i__ - 1];
	    scale += -1;
	    --i__;
	}
	p[0] = *t * p[0] + coeffs[k];
	--k;
	i__ = *nderiv;
	scale = (doublereal) (*nderiv);
    }
    return 0;
} /* polyds_ */

