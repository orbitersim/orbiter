/* vnormg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VNORMG ( Vector norm, general dimension ) */
doublereal vnormg_(doublereal *v1, integer *ndim)
{
    /* System generated locals */
    integer v1_dim1, i__1, i__2, i__3;
    doublereal ret_val, d__1, d__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    double sqrt(doublereal);

    /* Local variables */
    doublereal v1max, a;
    integer i__;

/* $ Abstract */

/*     Compute the magnitude of a double precision vector of arbitrary */
/*     dimension. */

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

/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     V1         I   Vector whose magnitude is to be found. */
/*     NDIM       I   Dimension of V1. */

/*     The function returns the magnitude of V1. */

/* $ Detailed_Input */

/*     V1       is any double precision vector of arbitrary dimension. */

/*     NDIM     is the dimension of the input vector V1. */

/* $ Detailed_Output */

/*     The function returns the magnitude of V1 calculated in a */
/*     numerically stable way. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     VNORMG finds the component of V1 whose magnitude is the largest. */
/*     If the absolute magnitude of that component indicates that a */
/*     numeric overflow would occur when it is squared, or if it */
/*     indicates that an underflow would occur when squared (falsely */
/*     giving a magnitude of zero) then the following expression is */
/*     used: */

/*        VNORMG = V1MAX * MAGNITUDE OF [ (1/V1MAX)*V1 ] */

/*     Otherwise a simpler expression is used: */

/*        VNORMG = MAGNITUDE OF [ V1 ] */

/*     Beyond the logic described above, no further checking of the */
/*     validity of the input is performed. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Define a four-dimensional vector and calculate its magnitude. */

/*        Example code begins here. */


/*              PROGRAM VNORMG_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      VNORMG */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NDIM */
/*              PARAMETER           ( NDIM   = 4 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      V1 ( NDIM ) */

/*              DATA                  V1 / 12.3D0, -4.32D0, */
/*             .                           76.0D0,  1.87D0 / */

/*        C */
/*        C     Compute the magnitude of V1 */
/*        C */
/*              WRITE(*,*) 'Magnitude of v1: ', VNORMG ( V1, NDIM ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Magnitude of v1:    77.132673362201047 */


/*     2) The following table show the correlation between various input */
/*        vectors V1 and VNORMG: */

/*           NDIM    V1(NDIM)                          VNORMG */
/*           ------------------------------------------------- */
/*            1      (-7.0D20)                          7.D20 */
/*            3      (1.D0, 2.D0, 2.D0)                 3.D0 */
/*            4      (3.D0, 3.D0, 3.D0, 3.D0)           6.D0 */
/*            5      (5.D0, 12.D0, 0.D0, 0.D0, 0.D0)   13.D0 */
/*            3      (-5.D-17, 0.D0, 12.D-17)          13.D-17 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Edited the header comments to comply with NAIF standard. Added */
/*        complete code example. */

/*        Added IMPLICIT NONE statement. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     norm of n-dimensional vector */

/* -& */

/*     Local variables. */


/*  Determine the maximum component of the vector. */

    /* Parameter adjustments */
    v1_dim1 = *ndim;

    /* Function Body */
    v1max = 0.;
    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if ((d__1 = v1[(i__2 = i__ - 1) < v1_dim1 && 0 <= i__2 ? i__2 : 
		s_rnge("v1", i__2, "vnormg_", (ftnlen)214)], abs(d__1)) > 
		v1max) {
	    v1max = (d__2 = v1[(i__3 = i__ - 1) < v1_dim1 && 0 <= i__3 ? i__3 
		    : s_rnge("v1", i__3, "vnormg_", (ftnlen)214)], abs(d__2));
	}
    }

/*  If the vector is zero, return zero; otherwise normalize first. */
/*  Normalizing helps in the cases where squaring would cause overflow */
/*  or underflow.  In the cases where such is not a problem it not worth */
/*  it to optimize further. */

    if (v1max == 0.) {
	ret_val = 0.;
    } else {
	ret_val = 0.;
	i__1 = *ndim;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    a = v1[(i__2 = i__ - 1) < v1_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "v1", i__2, "vnormg_", (ftnlen)233)] / v1max;
	    ret_val += a * a;
	}
	ret_val = v1max * sqrt(ret_val);
    }

    return ret_val;
} /* vnormg_ */

