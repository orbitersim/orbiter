/* chbint.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CHBINT ( Interpolate a Chebyshev expansion ) */
/* Subroutine */ int chbint_(doublereal *cp, integer *degp, doublereal *x2s, 
	doublereal *x, doublereal *p, doublereal *dpdx)
{
    integer j;
    doublereal s, w[3], s2, dw[3];

/* $ Abstract */

/*     Return the value of a polynomial and its derivative, evaluated at */
/*     the input X, using the coefficients of the Chebyshev expansion of */
/*     the polynomial. */

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
/*     CP         I   DEGP+1 Chebyshev polynomial coefficients. */
/*     DEGP       I   Degree of polynomial. */
/*     X2S        I   Transformation parameters of polynomial. */
/*     X          I   Value for which the polynomial is to be evaluated */
/*     P          O   Value of the polynomial at X */
/*     DPDX       O   Value of the derivative of the polynomial at X */

/* $ Detailed_Input */

/*     CP       is an array of coefficients of a polynomial with */
/*              respect to the Chebyshev basis. The polynomial to be */
/*              evaluated is assumed to be of the form: */

/*                 CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ... */

/*                                      + CP(2)*T(1,S) + CP(1)*T(0,S) */

/*              where T(I,S) is the I'th Chebyshev polynomial */
/*              evaluated at a number S whose double precision */
/*              value lies between -1 and 1. The value of S is */
/*              computed from the input variables X2S(1), X2S(2) and X. */

/*     DEGP     is the degree of the Chebyshev polynomial to be */
/*              evaluated. */

/*     X2S      is an array of two parameters. These parameters are */
/*              used to transform the domain of the input variable X */
/*              into the standard domain of the Chebyshev polynomial. */
/*              X2S(1) should be a reference point in the domain of X; */
/*              X2S(2) should be the radius by which points are */
/*              allowed to deviate from the reference point and while */
/*              remaining within the domain of X. The value of */
/*              X is transformed into the value S given by */

/*                 S = ( X - X2S(1) ) / X2S(2) */

/*              Typically X2S(1) is the midpoint of the interval over */
/*              which X is allowed to vary and X2S(2) is the radius of */
/*              the interval. */

/*              The main reason for doing this is that a Chebyshev */
/*              expansion is usually fit to data over a span */
/*              from A to B where A and B are not -1 and 1 */
/*              respectively. Thus to get the "best fit" the */
/*              data was transformed to the interval [-1,1] and */
/*              coefficients generated. These coefficients are */
/*              not rescaled to the interval of the data so that */
/*              the numerical "robustness" of the Chebyshev fit will */
/*              not be lost. Consequently, when the "best fitting" */
/*              polynomial needs to be evaluated at an intermediate */
/*              point, the point of evaluation must be transformed */
/*              in the same way that the generating points were */
/*              transformed. */

/*     X        is the value for which the polynomial is to be */
/*              evaluated. */

/* $ Detailed_Output */

/*     P        is the value of the polynomial to be evaluated. It */
/*              is given by */

/*                 CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ... */

/*                                      + CP(2)*T(1,S) + CP(1)*T(0,S) */

/*              where T(I,S) is the I'th Chebyshev polynomial */
/*              evaluated  at a number S = ( X - X2S(1) )/X2S(2) */

/*     DPDX     is the value of the derivative of the polynomial at X. */
/*              It is given by */

/*                 1/X2S(2) [    CP(DEGP+1)*T'(DEGP,S) */

/*                             + CP(DEGP)*T'(DEGP-1,S) */

/*                             + ... */

/*                             + CP(2)*T'(1,S) */

/*                             + CP(1)*T'(0,S) ] */

/*              where T(I,S) and T'(I,S) are the I'th Chebyshev */
/*              polynomial and its derivative, respectively, */
/*              evaluated  at a number S = ( X - X2S(1) )/X2S(2) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  No tests are performed for exceptional values (DEGP negative, */
/*         etc.). This routine is expected to be used at a low level in */
/*         ephemeris evaluations. For that reason it has been elected as */
/*         a routine that will not participate in error handling. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes the value of a Chebyshev polynomial */
/*     expansion and the derivative of the expansion with respect to X. */
/*     The polynomial is given by */

/*        CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ... */

/*                             + CP(2)*T(1,S) + CP(1)*T(0,S) */

/*     where */

/*        S  =  ( X - X2S(1) ) / X2S(2) */

/*     and */

/*        T(I,S) is the I'th Chebyshev polynomial of the first kind */
/*        evaluated at S. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Depending upon the user's needs, there are 3 routines */
/*        available for evaluating Chebyshev polynomials. */

/*           CHBVAL   for evaluating a Chebyshev polynomial when no */
/*                    derivatives are desired. */

/*           CHBINT   for evaluating a Chebyshev polynomial and its */
/*                    first derivative. */

/*           CHBDER   for evaluating a Chebyshev polynomial and a user */
/*                    or application dependent number of derivatives. */

/*        Of these 3 the one most commonly employed by SPICE software */
/*        is CHBINT as it is used to interpolate ephemeris state */
/*        vectors; this requires the evaluation of a polynomial */
/*        and its derivative. When no derivatives are desired one */
/*        should use CHBVAL, or when more than one or an unknown */
/*        number of derivatives are desired one should use CHBDER. */

/*        The code example below illustrates how this routine might */
/*        be used to obtain points for plotting a polynomial */
/*        and its derivative. */


/*        Example code begins here. */


/*              PROGRAM CHBINT_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      CP     (7) */
/*              DOUBLE PRECISION      DPDX */
/*              DOUBLE PRECISION      X */
/*              DOUBLE PRECISION      P */
/*              DOUBLE PRECISION      X2S    (2) */

/*              INTEGER               DEGP */
/*              INTEGER               I */

/*        C */
/*        C     Set the coefficients of the polynomial and its */
/*        C     transformation parameters */
/*        C */
/*              DATA                  CP     / 1.D0,  3.D0,  0.5D0, */
/*             .                               1.D0,  0.5D0, -1.D0, */
/*             .                               1.D0               / */
/*              DATA                  X2S    / 0.5D0, 3.D0 / */

/*              DEGP   = 6 */
/*              X      = 1.D0 */

/*              CALL CHBINT ( CP, DEGP, X2S, X, P, DPDX ) */

/*              WRITE(*,'(A,F10.6)') */
/*             .        'Value of the polynomial at X=1: ', P */
/*              WRITE(*,'(A,F10.6)') '   First derivative' */
/*             .                 //  ' at X=1    : ', DPDX */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Value of the polynomial at X=1:  -0.340878 */
/*           First derivative at X=1    :   0.382716 */


/* $ Restrictions */

/*     1)  One needs to be careful that the value */

/*            (X-X2S(1)) / X2S(2) */

/*         lies between -1 and 1. Otherwise, the routine may fail */
/*         spectacularly (for example with a floating point overflow). */

/* $ Literature_References */

/*     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling, */
/*          "Numerical Recipes -- The Art of Scientific Computing," */
/*          chapter 5.4, "Recurrence Relations and Clenshaw's Recurrence */
/*          Formula," p 161, Cambridge University Press, 1986. */

/*     [2]  T. Rivlin, "The Chebyshev Polynomials," Wiley, 1974. */

/*     [3]  R. Weast and S. Selby, "CRC Handbook of Tables for */
/*          Mathematics," 4th Edition, CRC Press, 1976. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Updated the header to comply with NAIF standard. Added */
/*        full code example. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     interpolate a chebyshev expansion */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.0.1, 30-DEC-1988 (WLT) */

/*        The Error free specification was added to the routine as */
/*        well as an explanation for this designation. Examples added. */

/* -& */

/*     Local variables */


/*     Transform X to S and initialize temporary variables. */

    s = (*x - x2s[0]) / x2s[1];
    s2 = s * 2.;
    j = *degp + 1;
    w[0] = 0.;
    w[1] = 0.;
    dw[0] = 0.;
    dw[1] = 0.;

/*     Evaluate the polynomial and its derivative using recursion. */

    while(j > 1) {
	w[2] = w[1];
	w[1] = w[0];
	w[0] = cp[j - 1] + (s2 * w[1] - w[2]);
	dw[2] = dw[1];
	dw[1] = dw[0];
	dw[0] = w[1] * 2. + dw[1] * s2 - dw[2];
	--j;
    }
    *p = cp[0] + (s * w[0] - w[1]);
    *dpdx = w[0] + s * dw[0] - dw[1];

/*     Scale the derivative by 1/X2S(2) so that we have the derivative */

/*                       d P(S) */
/*                       ------ */
/*                         dX */

    *dpdx /= x2s[1];
    return 0;
} /* chbint_ */

