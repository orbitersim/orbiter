/* mxmg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure MXMG ( Matrix times matrix, general dimension ) */
/* Subroutine */ int mxmg_(doublereal *m1, doublereal *m2, integer *nr1, 
	integer *nc1r2, integer *nc2, doublereal *mout)
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

/*     Multiply two double precision matrices of arbitrary size. */

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
/*     M1         I   NR1   x NC1R2 double precision matrix. */
/*     M2         I   NC1R2 x NC2   double precision matrix. */
/*     NR1        I   Row dimension of M1 (and also MOUT). */
/*     NC1R2      I   Column dimension of M1 and row dimension of M2. */
/*     NC2        I   Column dimension of M2 (and also MOUT). */
/*     MOUT       O   NR1 x NC2 double precision matrix. */

/* $ Detailed_Input */

/*     M1       is any double precision matrix of arbitrary size. */

/*     M2       is any double precision matrix of arbitrary size. */
/*              The number of rows in M2 must match the number of */
/*              columns in M1. */

/*     NR1      is the number of rows in both M1 and MOUT. */

/*     NC1R2    is the number of columns in M1 and (by necessity) */
/*              the number of rows of M2. */

/*     NC2      is the number of columns in both M2 and MOUT. */

/* $ Detailed_Output */

/*     MOUT     is a a double precision matrix of dimension */
/*              NR1 x NC2. MOUT is the product matrix given */
/*              by MOUT = (M1) x (M2). MOUT must not overwrite */
/*              M1 or M2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If NC1R2 < 1, the elements of the matrix MOUT are set equal to */
/*         zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The code reflects precisely the following mathematical expression */

/*     For each value of the subscript I from 1 to NC1, and J from 1 */
/*     to NC2: */

/*        MOUT(I,J) = Summation from K=1 to NC1R2 of ( M1(I,K) * M2(K,J) */

/*     Since this subroutine operates on matrices of arbitrary size, it */
/*     is not feasible to buffer intermediate results. Thus, MOUT */
/*     should NOT overwrite either M1 or M2. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given a 3x2 and a 2x3 matrices, multiply the first matrix by */
/*        the second one. */


/*        Example code begins here. */


/*              PROGRAM MXMG_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      M1   ( 3, 2 ) */
/*              DOUBLE PRECISION      M2   ( 2, 3 ) */
/*              DOUBLE PRECISION      MOUT ( 3, 3 ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define M1 and M2. */
/*        C */
/*              DATA                  M1 /  1.0D0,  2.0D0, 3.0D0, */
/*             .                            4.0D0,  5.0D0, 6.0D0  / */

/*              DATA                  M2 /  1.0D0,  2.0D0, */
/*             .                            3.0D0,  4.0D0, */
/*             .                            5.0D0,  6.0D0  / */

/*        C */
/*        C     Multiply M1 by M2. */
/*        C */
/*              CALL MXMG ( M1, M2, 3, 2, 3, MOUT ) */

/*              WRITE(*,'(A)') 'M1 times M2:' */
/*              DO I = 1, 3 */

/*                 WRITE(*,'(3F10.3)') ( MOUT(I,J), J=1,3) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        M1 times M2: */
/*             9.000    19.000    29.000 */
/*            12.000    26.000    40.000 */
/*            15.000    33.000    51.000 */


/* $ Restrictions */

/*     1)  No error checking is performed to prevent numeric overflow or */
/*         underflow. */

/*     2)  No error checking performed to determine if the input and */
/*         output matrices have, in fact, been correctly dimensioned. */

/*     3)  MOUT should not overwrite M1 or M2. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR) */

/*        Changed input argument names ROW1, COL1 and COL2 to NR1, NC1R2 */
/*        and NC2 for consistency with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example based on the existing example. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     matrix times matrix n-dimensional_case */

/* -& */

/*     Local variables */


/*  Perform the matrix multiplication */

    /* Parameter adjustments */
    m1_dim1 = *nr1;
    m1_dim2 = *nc1r2;
    m1_offset = m1_dim1 + 1;
    mout_dim1 = *nr1;
    mout_dim2 = *nc2;
    mout_offset = mout_dim1 + 1;
    m2_dim1 = *nc1r2;
    m2_dim2 = *nc2;
    m2_offset = m2_dim1 + 1;

    /* Function Body */
    i__1 = *nr1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *nc2;
	for (j = 1; j <= i__2; ++j) {
	    sum = 0.;
	    i__3 = *nc1r2;
	    for (k = 1; k <= i__3; ++k) {
		sum += m1[(i__4 = i__ + k * m1_dim1 - m1_offset) < m1_dim1 * 
			m1_dim2 && 0 <= i__4 ? i__4 : s_rnge("m1", i__4, 
			"mxmg_", (ftnlen)241)] * m2[(i__5 = k + j * m2_dim1 - 
			m2_offset) < m2_dim1 * m2_dim2 && 0 <= i__5 ? i__5 : 
			s_rnge("m2", i__5, "mxmg_", (ftnlen)241)];
	    }
	    mout[(i__3 = i__ + j * mout_dim1 - mout_offset) < mout_dim1 * 
		    mout_dim2 && 0 <= i__3 ? i__3 : s_rnge("mout", i__3, 
		    "mxmg_", (ftnlen)243)] = sum;
	}
    }

    return 0;
} /* mxmg_ */

