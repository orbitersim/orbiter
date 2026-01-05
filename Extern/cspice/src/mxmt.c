/* mxmt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;

/* $Procedure MXMT ( Matrix times matrix transpose, 3x3 ) */
/* Subroutine */ int mxmt_(doublereal *m1, doublereal *m2, doublereal *mout)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    doublereal prodm[9]	/* was [3][3] */;

/* $ Abstract */

/*     Multiply a 3x3 matrix and the transpose of another 3x3 matrix. */

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

/*     MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     M1         I   3x3 double precision matrix. */
/*     M2         I   3x3 double precision matrix. */
/*     MOUT       O   The product M1 times transpose of M2. */

/* $ Detailed_Input */

/*     M1       is an arbitrary 3x3 double precision matrix. */

/*     M2       is an arbitrary 3x3 double precision matrix. */
/*              Typically, M2 will be a rotation matrix since */
/*              then its transpose is its inverse (but this is */
/*              NOT a requirement). */

/* $ Detailed_Output */

/*     MOUT     is a 3x3 double precision matrix. MOUT is the product */

/*                               T */
/*                 MOUT = M1 x M2 */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The code reflects precisely the following mathematical expression */

/*        For each value of the subscripts I and J from 1 to 3: */

/*                          3 */
/*                       .----- */
/*                        \ */
/*           MOUT(I,J) =   )  M1(I,K) * M2(J,K) */
/*                        / */
/*                       '----- */
/*                         K=1 */

/*     Note that the reversal of the K and J subscripts in the right- */
/*     hand matrix M2 is what makes MOUT the product of the TRANSPOSE of */
/*     M2 and not simply of M2 itself. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given two 3x3 double precision matrices, multiply the first */
/*        matrix by the transpose of the second one. */


/*        Example code begins here. */


/*              PROGRAM MXMT_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      M1   ( 3, 3 ) */
/*              DOUBLE PRECISION      M2   ( 3, 3 ) */
/*              DOUBLE PRECISION      MOUT ( 3, 3 ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define M1. */
/*        C */
/*              DATA                  M1   /  0.0D0, -1.0D0,  0.0D0, */
/*             .                              1.0D0,  0.0D0,  0.0D0, */
/*             .                              0.0D0,  0.0D0,  1.0D0  / */

/*        C */
/*        C     Make M2 equal to M1. */
/*        C */
/*              CALL MEQU ( M1, M2 ) */

/*        C */
/*        C     Multiply M1 by the transpose of M2. */
/*        C */
/*              CALL MXMT ( M1, M2, MOUT ) */

/*              WRITE(*,'(A)') 'M1:' */
/*              DO I=1, 3 */

/*                 WRITE(*,'(3F16.7)') ( M1(I,J), J=1,3 ) */

/*              END DO */

/*              WRITE(*,*) */
/*              WRITE(*,'(A)') 'M2:' */
/*              DO I=1, 3 */

/*                 WRITE(*,'(3F16.7)') ( M2(I,J), J=1,3 ) */

/*              END DO */

/*              WRITE(*,*) */
/*              WRITE(*,'(A)') 'M1 times transpose of M2:' */
/*              DO I=1, 3 */

/*                 WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 ) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        M1: */
/*               0.0000000       1.0000000       0.0000000 */
/*              -1.0000000       0.0000000       0.0000000 */
/*               0.0000000       0.0000000       1.0000000 */

/*        M2: */
/*               0.0000000       1.0000000       0.0000000 */
/*              -1.0000000       0.0000000       0.0000000 */
/*               0.0000000       0.0000000       1.0000000 */

/*        M1 times transpose of M2: */
/*               1.0000000       0.0000000       0.0000000 */
/*               0.0000000       1.0000000       0.0000000 */
/*               0.0000000       0.0000000       1.0000000 */


/* $ Restrictions */

/*     1)  The user is responsible for checking the magnitudes of the */
/*         elements of M1 and M2 so that a floating point overflow does */
/*         not occur. (In the typical use where M1 and M2 are rotation */
/*         matrices, this not a risk at all.) */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code examples based on existing code fragments. */

/* -    SPICELIB Version 1.0.2, 22-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     matrix times matrix_transpose 3x3_case */

/* -& */

/*     Local variables */


/*  Perform the matrix multiplication */

    for (i__ = 1; i__ <= 3; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    prodm[(i__1 = i__ + j * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		    "prodm", i__1, "mxmt_", (ftnlen)256)] = m1[(i__2 = i__ - 
		    1) < 9 && 0 <= i__2 ? i__2 : s_rnge("m1", i__2, "mxmt_", (
		    ftnlen)256)] * m2[(i__3 = j - 1) < 9 && 0 <= i__3 ? i__3 :
		     s_rnge("m2", i__3, "mxmt_", (ftnlen)256)] + m1[(i__4 = 
		    i__ + 2) < 9 && 0 <= i__4 ? i__4 : s_rnge("m1", i__4, 
		    "mxmt_", (ftnlen)256)] * m2[(i__5 = j + 2) < 9 && 0 <= 
		    i__5 ? i__5 : s_rnge("m2", i__5, "mxmt_", (ftnlen)256)] + 
		    m1[(i__6 = i__ + 5) < 9 && 0 <= i__6 ? i__6 : s_rnge(
		    "m1", i__6, "mxmt_", (ftnlen)256)] * m2[(i__7 = j + 5) < 
		    9 && 0 <= i__7 ? i__7 : s_rnge("m2", i__7, "mxmt_", (
		    ftnlen)256)];
	}
    }

/*  Move the result into MOUT */

    moved_(prodm, &c__9, mout);
    return 0;
} /* mxmt_ */

