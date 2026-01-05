/* vtmvg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VTMVG  ( Vector transpose times matrix times vector ) */
doublereal vtmvg_(doublereal *v1, doublereal *matrix, doublereal *v2, integer 
	*nrow, integer *ncol)
{
    /* System generated locals */
    integer v1_dim1, matrix_dim1, matrix_dim2, matrix_offset, v2_dim1, i__1, 
	    i__2, i__3, i__4, i__5;
    doublereal ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer k, l;

/* $ Abstract */

/*     Multiply the transpose of a n-dimensional column vector, */
/*     a nxm matrix, and a m-dimensional column vector. */

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
/*     V1         I   N-dimensional double precision column vector. */
/*     MATRIX     I   NxM double precision matrix. */
/*     V2         I   M-dimensional double precision column vector. */
/*     NROW       I   Number of rows in MATRIX (number of rows in V1.) */
/*     NCOL       I   Number of columns in MATRIX (number of rows in V2.) */

/*     The function returns the result of (V1**T * MATRIX * V2 ). */

/* $ Detailed_Input */

/*     V1       is an n-dimensional double precision vector. */

/*     MATRIX   is an n x m double precision matrix. */

/*     V2       is an m-dimensional double precision vector. */

/*     NROW     is the number of rows in MATRIX. This is also */
/*              equivalent to the number of rows in the vector V1. */

/*     NCOL     is the number of columns in MATRIX. This is also */
/*              equivalent to the number of rows in the vector V2. */

/* $ Detailed_Output */

/*     The function returns the double precision value of the equation */
/*     (V1**T * MATRIX * V2 ). */

/*     Notice that VTMVG is actually the dot product of the vector */
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

/*                      T    |          |  |  | */
/*        VTMVG = [   V1   ] |  MATRIX  |  |V2| */
/*                           |          |  |  | */

/*     by calculating over all values of the indices K and L from 1 to */
/*     NROW and 1 to NCOL, respectively, the expression */

/*        VTMVG = Summation of ( V1(K)*MATRIX(K,L)*V2(L) ) . */

/*     V1 is a column vector which becomes a row vector when transposed. */
/*     V2 is a column vector. */

/*     No checking is performed to determine whether floating point */
/*     overflow has occurred. */

/* $ Examples */

/*     If  V1 = | 1.0D0 |  MATRIX = | 2.0D0  0.0D0 |  V2 = | 1.0D0 | */
/*              |       |           |              |       |       | */
/*              | 2.0D0 |           | 1.0D0  2.0D0 |       | 2.0D0 | */
/*              |       |           |              | */
/*              | 3.0D0 |           | 1.0D0  1.0D0 | */

/*     NROW = 3 */
/*     NCOL = 2 */

/*     then the value of the function is  21.0D0. */

/* $ Restrictions */

/*     1)  Since no error detection or recovery is implemented, the */
/*         programmer is required to insure that the inputs to this */
/*         routine are both valid and within the proper range. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added comments */
/*        to the code and moved the declaration of each local variable to */
/*        a separate line. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     n-dimensional vector_transpose times matrix times vector */

/* -& */

/*     Local variables */


/*  Perform the multiplication */

    /* Parameter adjustments */
    v1_dim1 = *nrow;
    v2_dim1 = *ncol;
    matrix_dim1 = *nrow;
    matrix_dim2 = *ncol;
    matrix_offset = matrix_dim1 + 1;

    /* Function Body */
    ret_val = 0.;
    i__1 = *nrow;
    for (k = 1; k <= i__1; ++k) {
	i__2 = *ncol;
	for (l = 1; l <= i__2; ++l) {
	    ret_val += v1[(i__3 = k - 1) < v1_dim1 && 0 <= i__3 ? i__3 : 
		    s_rnge("v1", i__3, "vtmvg_", (ftnlen)189)] * matrix[(i__4 
		    = k + l * matrix_dim1 - matrix_offset) < matrix_dim1 * 
		    matrix_dim2 && 0 <= i__4 ? i__4 : s_rnge("matrix", i__4, 
		    "vtmvg_", (ftnlen)189)] * v2[(i__5 = l - 1) < v2_dim1 && 
		    0 <= i__5 ? i__5 : s_rnge("v2", i__5, "vtmvg_", (ftnlen)
		    189)];
	}
    }
    return ret_val;
} /* vtmvg_ */

