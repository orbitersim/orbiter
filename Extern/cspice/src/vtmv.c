/* vtmv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VTMV ( Vector transpose times matrix times vector, 3 dim ) */
doublereal vtmv_(doublereal *v1, doublereal *matrix, doublereal *v2)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer k, l;

/* $ Abstract */

/*     Multiply the transpose of a 3-dimensional column vector, */
/*     a 3x3 matrix, and a 3-dimensional column vector. */

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
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     V1         I   3-dimensional double precision column vector. */
/*     MATRIX     I   3x3 double precision matrix. */
/*     V2         I   3-dimensional double precision column vector. */

/*     The function returns the result of multiplying the transpose of */
/*     V1 by MATRIX by V2. */

/* $ Detailed_Input */

/*     V1       is any double precision 3-dimensional column vector. */

/*     MATRIX   is any double precision 3x3 matrix. */

/*     V2       is any double precision 3-dimensional column vector. */

/* $ Detailed_Output */

/*     The function returns the double precision value of the equation */

/*          T */
/*        V1  *  MATRIX * V2 */

/*     Notice that VTMV is actually the dot product of the vector */
/*     resulting from multiplying the transpose of V1 and MATRIX and the */
/*     vector V2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine implements the following vector/matrix/vector */
/*     multiplication: */

/*                 T */
/*        VTMV = V1  * MATRIX * V2 */

/*     V1 is a column vector which becomes a row vector when transposed. */
/*     V2 is a column vector. */

/*     No checking is performed to determine whether floating point */
/*     overflow has occurred. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute the multiplication of the transpose of a 3-dimensional */
/*        column vector, a 3x3 matrix, and a second 3-dimensional column */
/*        vector. */


/*        Example code begins here. */


/*              PROGRAM VTMV_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION      VTMV */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      MATRIX ( 3, 3 ) */
/*              DOUBLE PRECISION      V1     (    3 ) */
/*              DOUBLE PRECISION      V2     (    3 ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define V1, MATRIX and V2. */
/*        C */
/*              DATA                  V1      /  2.D0,  4.D0, 6.D0  / */
/*              DATA                  MATRIX  /  0.D0, -1.D0, 0.D0, */
/*             .                                 1.D0,  0.D0, 0.D0, */
/*             .                                 0.D0,  0.D0, 1.D0  / */
/*              DATA                  V2      /  1.D0,  1.D0, 1.D0  / */


/*              WRITE(*,'(A)') 'V1:' */
/*              DO I = 1, 3 */

/*                 WRITE(*,'(F6.1)') V1(I) */

/*              END DO */

/*              WRITE(*,*) */
/*              WRITE(*,'(A)') 'MATRIX:' */
/*              DO I = 1, 3 */

/*                 WRITE(*,'(3F6.1)') ( MATRIX(I,J), J=1,3 ) */

/*              END DO */

/*              WRITE(*,*) */
/*              WRITE(*,'(A)') 'V2:' */
/*              DO I = 1, 3 */

/*                 WRITE(*,'(F6.1)') V2(I) */

/*              END DO */

/*        C */
/*        C     Compute the transpose of V1 times MATRIX times V2. */
/*        C */
/*              WRITE(*,*) */
/*              WRITE(*,'(A,F6.1)') 'Transpose of V1 times MATRIX ' */
/*             .                 // 'times V2:', VTMV ( V1, MATRIX, V2 ) */


/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        V1: */
/*           2.0 */
/*           4.0 */
/*           6.0 */

/*        MATRIX: */
/*           0.0   1.0   0.0 */
/*          -1.0   0.0   0.0 */
/*           0.0   0.0   1.0 */

/*        V2: */
/*           1.0 */
/*           1.0 */
/*           1.0 */

/*        Transpose of V1 times MATRIX times V2:   4.0 */


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

/*        Added IMPLICIT NONE statement. */

/*        Edited the header and code to comply with NAIF standard. Added */
/*        complete code example based on existing example. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     3-dimensional vector_transpose times matrix times vector */

/* -& */

/*     Local variables */

    ret_val = 0.;
    for (k = 1; k <= 3; ++k) {
	for (l = 1; l <= 3; ++l) {
	    ret_val += v1[(i__1 = k - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "v1", i__1, "vtmv_", (ftnlen)256)] * matrix[(i__2 = k + l 
		    * 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge("matrix", i__2, 
		    "vtmv_", (ftnlen)256)] * v2[(i__3 = l - 1) < 3 && 0 <= 
		    i__3 ? i__3 : s_rnge("v2", i__3, "vtmv_", (ftnlen)256)];
	}
    }
    return ret_val;
} /* vtmv_ */

