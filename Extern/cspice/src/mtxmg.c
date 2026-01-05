/* mtxmg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure MTXMG ( Matrix transpose times matrix, general dimension ) */
/* Subroutine */ int mtxmg_(doublereal *m1, doublereal *m2, integer *nc1, 
	integer *nr1r2, integer *nc2, doublereal *mout)
{
    /* System generated locals */
    integer m1_dim1, m1_dim2, m1_offset, m2_dim1, m2_dim2, m2_offset, 
	    mout_dim1, mout_dim2, mout_offset, i__1, i__2, i__3, i__4, i__5, 
	    i__6, i__7;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, j, k;

/* $ Abstract */

/*     Multiply the transpose of a matrix with another matrix, */
/*     both of arbitrary size. (The dimensions of the matrices must be */
/*     compatible with this multiplication.) */

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
/*     M1         I   Left-hand matrix whose transpose is to be */
/*                    multiplied. */
/*     M2         I   Right-hand matrix to be multiplied. */
/*     NC1        I   Column dimension of M1 and row dimension of MOUT. */
/*     NR1R2      I   Row dimension of both M1 and M2. */
/*     NC2        I   Column dimension of both M2 and MOUT. */
/*     MOUT       O   Product matrix M1**T * M2. */

/* $ Detailed_Input */

/*     M1       is an double precision matrix of arbitrary dimension */
/*              whose transpose is the left hand multiplier of a */
/*              matrix multiplication. */

/*     M2       is an double precision matrix of arbitrary dimension */
/*              whose transpose is the left hand multiplier of a */
/*              matrix multiplication. */

/*     NC1      is the column dimension of M1 and row dimension of */
/*              MOUT. */

/*     NR1R2    is the row dimension of both M1 and M2. */

/*     NC2      is the column dimension of both M2 and MOUT. */

/* $ Detailed_Output */

/*     MOUT     is a double precision matrix containing the product */

/*                           T */
/*                 MOUT =  M1  x  M2 */

/*              where the superscript T denotes the transpose of M1. */

/*              MOUT must NOT overwrite either M1 or M2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If NR1R2 < 1, the elements of the matrix MOUT are set equal to */
/*         zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The code reflects precisely the following mathematical expression */

/*     For each value of the subscript I from 1 to NC1, and J from 1 */
/*     to NC2: */

/*     MOUT(I,J) = Summation from K=1 to NR1R2 of  ( M1(K,I) * M2(K,J) ) */

/*     Note that the reversal of the K and I subscripts in the left-hand */
/*     matrix M1 is what makes MOUT the product of the TRANSPOSE of M1 */
/*     and not simply of M1 itself. */

/*     Since this subroutine operates on matrices of arbitrary size, it */
/*     is not possible to buffer intermediate results. Thus, MOUT */
/*     should NOT overwrite either M1 or M2. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given a 2x4 and a 2x3 matrices, multiply the transpose of the */
/*        first matrix by the second one. */


/*        Example code begins here. */


/*              PROGRAM MTXMG_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      M1   ( 4, 2 ) */
/*              DOUBLE PRECISION      M2   ( 2, 3 ) */
/*              DOUBLE PRECISION      MOUT ( 4, 3 ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define M1 and M2. */
/*        C */
/*              DATA                  M1 /  1.0D0,  1.0D0, */
/*             .                            2.0D0,  1.0D0, */
/*             .                            3.0D0,  1.0D0, */
/*             .                            0.0D0,  1.0D0  / */

/*              DATA                  M2 /  1.0D0,  0.0D0, */
/*             .                            2.0D0,  0.0D0, */
/*             .                            3.0D0,  0.0D0  / */

/*        C */
/*        C     Multiply the transpose of M1 by M2. */
/*        C */
/*              CALL MTXMG ( M1, M2, 4, 2, 3, MOUT ) */

/*              WRITE(*,'(A)') 'Transpose of M1 times M2:' */
/*              DO I = 1, 4 */

/*                 WRITE(*,'(3F10.3)') ( MOUT(I,J), J=1,3) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Transpose of M1 times M2: */
/*             1.000     2.000     3.000 */
/*             2.000     4.000     6.000 */
/*             3.000     6.000     9.000 */
/*             0.000     0.000     0.000 */


/* $ Restrictions */

/*     1)  The user is responsible for checking the magnitudes of the */
/*         elements of M1 and M2 so that a floating point overflow does */
/*         not occur. */

/*     2)  MOUT must not overwrite M1 or M2 or else the intermediate */
/*         will affect the final result. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete code example based on the existing example. */

/*        Added entry #1 to $Exceptions section. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     matrix_transpose times matrix n-dimensional_case */

/* -& */

/*     Local variables */


/*  Perform the matrix multiplication */

    /* Parameter adjustments */
    m1_dim1 = *nr1r2;
    m1_dim2 = *nc1;
    m1_offset = m1_dim1 + 1;
    mout_dim1 = *nc1;
    mout_dim2 = *nc2;
    mout_offset = mout_dim1 + 1;
    m2_dim1 = *nr1r2;
    m2_dim2 = *nc2;
    m2_offset = m2_dim1 + 1;

    /* Function Body */
    i__1 = *nc1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *nc2;
	for (j = 1; j <= i__2; ++j) {
	    mout[(i__3 = i__ + j * mout_dim1 - mout_offset) < mout_dim1 * 
		    mout_dim2 && 0 <= i__3 ? i__3 : s_rnge("mout", i__3, 
		    "mtxmg_", (ftnlen)254)] = 0.;
	    i__3 = *nr1r2;
	    for (k = 1; k <= i__3; ++k) {
		mout[(i__4 = i__ + j * mout_dim1 - mout_offset) < mout_dim1 * 
			mout_dim2 && 0 <= i__4 ? i__4 : s_rnge("mout", i__4, 
			"mtxmg_", (ftnlen)256)] = mout[(i__5 = i__ + j * 
			mout_dim1 - mout_offset) < mout_dim1 * mout_dim2 && 0 
			<= i__5 ? i__5 : s_rnge("mout", i__5, "mtxmg_", (
			ftnlen)256)] + m1[(i__6 = k + i__ * m1_dim1 - 
			m1_offset) < m1_dim1 * m1_dim2 && 0 <= i__6 ? i__6 : 
			s_rnge("m1", i__6, "mtxmg_", (ftnlen)256)] * m2[(i__7 
			= k + j * m2_dim1 - m2_offset) < m2_dim1 * m2_dim2 && 
			0 <= i__7 ? i__7 : s_rnge("m2", i__7, "mtxmg_", (
			ftnlen)256)];
	    }
	}
    }

    return 0;
} /* mtxmg_ */

