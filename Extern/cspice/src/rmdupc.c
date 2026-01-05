/* rmdupc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure RMDUPC ( Remove duplicates from a character array ) */
/* Subroutine */ int rmdupc_(integer *nelt, char *array, ftnlen array_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int shellc_(integer *, char *, ftnlen);

/* $ Abstract */

/*     Remove duplicate elements from a character array. */

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

/*     ARRAY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NELT      I-O  Number of elements in the array. */
/*     ARRAY     I-O  Input/output array. */

/* $ Detailed_Input */

/*     NELT     on input is the number of elements in the input */
/*              array. */

/*     ARRAY    on input contains zero or more elements, from which */
/*              all duplicate elements are to be removed. */

/* $ Detailed_Output */

/*     NELT     on output is the number of elements in the output */
/*              array. */

/*     ARRAY    on output contains the distinct elements of the */
/*              input array, sorted in increasing order. (Character */
/*              arrays are sorted according to the ASCII collating */
/*              sequence). */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     Let the arrays C and I contain the following elements. */

/*           NC   = 7                NI   =   5 */
/*           C(1) = 'Miranda'        I(1) =  13 */
/*           C(2) = 'Ariel'          I(2) = -13 */
/*           C(3) = 'Umbriel'        I(3) =   0 */
/*           C(4) = 'Titania'        I(4) =   1 */
/*           C(5) = 'Miranda'        I(5) =   0 */
/*           C(6) = 'Oberon' */
/*           C(7) = 'Umbriel' */

/*     Then following the calls */

/*           CALL RMDUPC ( NC, C ) */
/*           CALL RMDUPI ( NI, I ) */

/*     C and I contain the following. */

/*           NC   = 5                NI   =   4 */
/*           C(1) = 'Ariel'          I(1) = -13 */
/*           C(2) = 'Miranda'        I(2) =   0 */
/*           C(3) = 'Oberon'         I(3) =   1 */
/*           C(4) = 'Titania'        I(4) =  13 */
/*           C(5) = 'Umbriel' */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 05-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     remove duplicates from a character array */

/* -& */

/*     Local variables */


/*     Proceed only if the array actually contains more than one element. */

    if (*nelt > 1) {

/*        Sort the array in place. */

	shellc_(nelt, array, array_len);

/*        Drop duplicate entries. Compare adjacent entries, and move */
/*        duplicates forward. (Duplicates are now adjacent, because of */
/*        sorting.) */

	j = 1;
	i__1 = *nelt;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    if (s_cmp(array + (i__ - 1) * array_len, array + (i__ - 2) * 
		    array_len, array_len, array_len) != 0) {
		++j;
		s_copy(array + (j - 1) * array_len, array + (i__ - 1) * 
			array_len, array_len, array_len);
	    }
	}
	*nelt = j;
    }
    return 0;
} /* rmdupc_ */

