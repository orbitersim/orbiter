/* mxmtg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure MXMTG  ( Matrix times matrix transpose, general dimension ) */
/* Subroutine */ int mxmtg_(doublereal *m1, doublereal *m2, integer *nr1, 
	integer *nc1c2, integer *nr2, doublereal *mout)
{
    /* System generated locals */
    integer m1_dim1, m1_dim2, m1_offset, m2_dim1, m2_dim2, m2_offset, 
	    mout_dim1, mout_dim2, mout_offset, i__1, i__2, i__3, i__4, i__5;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, j, k;
    doublereal sum;

/* $ Abstract */

/*     Multiply a matrix and the transpose of a matrix, both of */
/*     arbitrary size. */

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
/*     M1         I   Left-hand matrix to be multiplied. */
/*     M2         I   Right-hand matrix whose transpose is to be */
/*                    multiplied. */
/*     NR1        I   Row dimension of M1 and row dimension of MOUT. */
/*     NC1C2      I   Column dimension of M1 and column dimension of M2. */
/*     NR2        I   Row dimension of M2 and column dimension of MOUT. */
/*     MOUT       O   Product matrix M1 * M2**T. */

/* $ Detailed_Input */

/*     M1       is any double precision matrix of arbitrary size. */

/*     M2       is any double precision matrix of arbitrary size. */

/*              The number of columns in M2 must match the number of */
/*              columns in M1. */

/*     NR1      is the number of rows in both M1 and MOUT. */

/*     NC1C2    is the number of columns in M1 and (by necessity) */
/*              the number of columns of M2. */

/*     NR2      is the number of rows in both M2 and the number of */
/*              columns in MOUT. */

/* $ Detailed_Output */

/*     MOUT     is a double precision matrix of dimension NR1 x NR2. */

/*              MOUT is the product matrix given by */

/*                               T */
/*                 MOUT = M1 x M2 */

/*              where the superscript "T" denotes the transpose */
/*              matrix. */

/*              MOUT must not overwrite M1 or M2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The code reflects precisely the following mathematical expression */

/*     For each value of the subscript I from 1 to NR1, and J from 1 */
/*     to NR2: */

/*     MOUT(I,J) = Summation from K=1 to NC1C2 of  ( M1(I,K) * M2(J,K) ) */

/*     Notice that the order of the subscripts of M2 are reversed from */
/*     what they would be if this routine merely multiplied M1 and M2. */
/*     It is this transposition of subscripts that makes this routine */
/*     multiply M1 and the TRANPOSE of M2. */

/*     Since this subroutine operates on matrices of arbitrary size, it */
/*     is not feasible to buffer intermediate results. Thus, MOUT */
/*     should NOT overwrite either M1 or M2. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given a 2x3 and a 3x4 matrices, multiply the first matrix by */
/*        the transpose of the second one. */


/*        Example code begins here. */


/*              PROGRAM MXMTG_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      M1   ( 2, 3 ) */
/*              DOUBLE PRECISION      M2   ( 4, 3 ) */
/*              DOUBLE PRECISION      MOUT ( 2, 4 ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define M1 and M2. */
/*        C */
/*              DATA                  M1 /  1.0D0, 3.0D0, */
/*             .                            2.0D0, 2.0D0, */
/*             .                            3.0D0, 1.0D0  / */

/*              DATA                  M2 /  1.0D0, 2.0D0, 1.0D0, 2.0D0, */
/*             .                            2.0D0, 1.0D0, 2.0D0, 1.0D0, */
/*             .                            0.0D0, 2.0D0, 0.0D0, 2.0D0 / */

/*        C */
/*        C     Multiply M1 by the transpose of M2. */
/*        C */
/*              CALL MXMTG ( M1, M2, 2, 3, 4, MOUT ) */

/*              WRITE(*,'(A)') 'M1 times transpose of M2:' */
/*              DO I = 1, 2 */

/*                 WRITE(*,'(4F10.3)') ( MOUT(I,J), J=1,4) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        M1 times transpose of M2: */
/*             5.000    10.000     5.000    10.000 */
/*             7.000    10.000     7.000    10.000 */


/* $ Restrictions */

/*     1)  No error checking is performed to prevent numeric overflow or */
/*         underflow. */

/*         The user is responsible for checking the magnitudes of the */
/*         elements of M1 and M2 so that a floating point overflow does */
/*         not occur. */

/*     2)  No error checking is performed to determine if the input and */
/*         output matrices have, in fact, been correctly dimensioned. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example based on the existing example. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     matrix times matrix_transpose n-dimensional_case */

/* -& */

/*     Local variables */


/*  Perform the matrix multiplication */

    /* Parameter adjustments */
    m1_dim1 = *nr1;
    m1_dim2 = *nc1c2;
    m1_offset = m1_dim1 + 1;
    mout_dim1 = *nr1;
    mout_dim2 = *nr2;
    mout_offset = mout_dim1 + 1;
    m2_dim1 = *nr2;
    m2_dim2 = *nc1c2;
    m2_offset = m2_dim1 + 1;

    /* Function Body */
    i__1 = *nr1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *nr2;
	for (j = 1; j <= i__2; ++j) {
	    sum = 0.;
	    i__3 = *nc1c2;
	    for (k = 1; k <= i__3; ++k) {
		sum += m1[(i__4 = i__ + k * m1_dim1 - m1_offset) < m1_dim1 * 
			m1_dim2 && 0 <= i__4 ? i__4 : s_rnge("m1", i__4, 
			"mxmtg_", (ftnlen)254)] * m2[(i__5 = j + k * m2_dim1 
			- m2_offset) < m2_dim1 * m2_dim2 && 0 <= i__5 ? i__5 :
			 s_rnge("m2", i__5, "mxmtg_", (ftnlen)254)];
	    }
	    mout[(i__3 = i__ + j * mout_dim1 - mout_offset) < mout_dim1 * 
		    mout_dim2 && 0 <= i__3 ? i__3 : s_rnge("mout", i__3, 
		    "mxmtg_", (ftnlen)257)] = sum;
	}
    }
    return 0;
} /* mxmtg_ */

