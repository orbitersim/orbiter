/* xpsgip.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure XPSGIP ( Transpose a matrix, general dimension, in place ) */
/* Subroutine */ int xpsgip_(integer *nrow, integer *ncol, doublereal *matrix)
{
    integer dest;
    doublereal temp;
    integer k, m, n, r__, moved, start;
    doublereal source;
    integer nmoves;

/* $ Abstract */

/*     Transpose a matrix of arbitrary size and shape in place. */

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
/*     NROW       I   Number of rows of input matrix. */
/*     NCOL       I   Number of columns of input matrix. */
/*     MATRIX    I-O  Matrix to be transposed/transposed matrix. */

/* $ Detailed_Input */

/*     NROW     is the number of rows of input matrix MATRIX. */

/*     NCOL     is the number of columns of input matrix MATRIX. */

/*     MATRIX   is a matrix to be transposed. */

/* $ Detailed_Output */

/*     MATRIX   is the transposed matrix: element (i,j) of the input */
/*              matrix is element (j,i) of the output matrix. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If either NROW or NCOL is less than or equal to zero, no */
/*         action is taken. The routine simply returns. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine replaces the input matrix MATRIX with its transpose. */

/*     NOTE:  The matrix MATRIX is declared one-dimensional for */
/*            computational purposes only. The calling program may */
/*            declare it as MATRIX(NROW,NCOL) or MATRIX(NCOL,NROW). */

/*            This routine assumes that the elements of the matrix to be */
/*            transformed are stored in contiguous memory locations as */
/*            shown here. On output these elements will be rearranged */
/*            in consecutive memory locations as shown. */

/*               MATRIX on input      MATRIX on output */

/*                m_11                m_11 */
/*                m_21                m_12 */
/*                m_31                m_13 */
/*                 .                   . */
/*                 .                   . */
/*                 .                  m_1ncol */
/*                m_nrow1             m_21 */
/*                m_12                m_22 */
/*                m_22                m_23 */
/*                m_32                 . */
/*                 .                   . */
/*                 .                  m_2ncol */
/*                 .                   . */
/*                m_nrow2 */
/*                 .                   . */

/*                 .                   . */

/*                 .                   . */
/*                m_1ncol */
/*                m_2ncol             m_nrow1 */
/*                m_3ncol             m_nrow2 */
/*                 .                  m_nrow3 */
/*                 .                   . */
/*                 .                   . */
/*                m_nrowncol          m_nrowncol */


/*     For those familiar with permutations, this algorithm relies */
/*     upon the fact that the transposition of a matrix, which has */
/*     been stored as a 1-dimensional array, is simply the action of a */
/*     permutation applied to that array. Since any permutation */
/*     can be decomposed as a product of disjoint cycles, it is */
/*     possible to transpose the matrix with only one additional */
/*     storage register. However, once a cycle has been computed */
/*     it is necessary to find the next entry in the array that */
/*     has not been moved by the permutation. For this reason the */
/*     algorithm is slower than would be necessary if the numbers */
/*     of rows and columns were known in advance. */

/* $ Examples */

/*     This routine is provided for situation where it is convenient to */
/*     transpose a general two-dimensional matrix */
/*     in place rather than store the result in a */
/*     separate array. Note that the call */

/*        CALL XPOSEG ( MATRIX, NROW, NCOL, MATRIX ) */

/*     is not permitted by the ANSI Fortran 77 standard; this routine */
/*     can be called instead to achieve the same result. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 19-SEP-2006 (EDW) */

/*        Initial version date unknown. Version data entry */
/*        added this date. */

/* -& */
/* $ Index_Entries */

/*     transpose a matrix general */

/* -& */

/*     Local Variables */


/*     Take care of dumb cases first. */

    if (*nrow <= 0 || *ncol <= 0) {
	return 0;
    }

/*     Use the abbreviations N and M for NROW and NCOL. */

    n = *nrow;
    m = *ncol;

/*     Set up the upper bound for the number of objects to be moved and */
/*     initialize the counters. */

    nmoves = n * m - 2;
    moved = 0;
    start = 1;

/*     Until MOVED is equal to NMOVES, there is some matrix element that */
/*     has not been moved to its proper location in the transpose matrix. */

    while(moved < nmoves) {
	source = matrix[start];
	k = start / n;
	r__ = start - n * k;
	dest = r__ * m + k;

/*        Perform this cycle of the permutation.  We will be done when */
/*        the destination of the next element is equal to the starting */
/*        position of the first element to be moved in this cycle. */

	while(dest != start) {
	    temp = matrix[dest];
	    matrix[dest] = source;
	    source = temp;
	    ++moved;
	    k = dest / n;
	    r__ = dest - k * n;
	    dest = m * r__ + k;
	}
	matrix[dest] = source;
	dest = 0;
	++moved;

/*        Find the next element of the matrix that has not already been */
/*        moved by the transposition operation. */

	if (moved < nmoves) {
	    while(dest != start) {
		++start;
		k = start / n;
		r__ = start - k * n;
		dest = r__ * m + k;
		while(dest > start) {
		    k = dest / n;
		    r__ = dest - k * n;
		    dest = m * r__ + k;
		}
	    }
	}
    }
    return 0;
} /* xpsgip_ */

