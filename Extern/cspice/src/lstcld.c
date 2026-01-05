/* lstcld.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LSTCLD ( Last closest double precision array element ) */
integer lstcld_(doublereal *x, integer *n, doublereal *array)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    integer j, begin, items, middle, end;

/* $ Abstract */

/*     Find the index of the array element closest to a given number X */
/*     in an array of non-decreasing numbers. */

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
/*     SEARCH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     X          I   Search value. */
/*     N          I   Number of elements in ARRAY. */
/*     ARRAY      I   Array to be searched. */

/*     The function returns the index of the element of ARRAY */
/*     whose value is closest to X. */

/* $ Detailed_Input */

/*     X        is the value to be compared with the elements of ARRAY. */

/*     N        is the number of elements in ARRAY. */

/*     ARRAY    is an array of double precision numbers such that */

/*                         ARRAY( I ) <= ARRAY( J ) */

/*              for all I < J. */

/* $ Detailed_Output */

/*     LSTCLD   is the index of the element of the non-decreasing */
/*              sequence: {ARRAY(I) : 1 <= I <= N} that is closest */
/*              to X. In other words, ARRAY( LSTCLD( X, N, ARRAY ) ) */
/*              is the element of ARRAY whose value is closest to X. */

/*              If X falls precisely on the midpoint of consecutive array */
/*              elements, the index of the larger of the two values is */
/*              returned. */

/*              If X is closest to a value which appears more than */
/*              once in the array (since the array is ordered, these */
/*              elements would have to be consecutive), the highest index */
/*              for that value will be returned. */

/*              LSTCLD = I for some I in the range 1 to N, unless N is */
/*              less than or equal to zero, in which case LSTCLD is zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the value of N is non-positive, LSTCLD returns the value */
/*         zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     LSTCLD uses a binary search algorithm to locate the value closest */
/*     to X in the non-decreasing sequence of double precision numbers */
/*     represented by the elements of ARRAY. */

/* $ Examples */

/*     Suppose ARRAY contains the following double precision elements: */

/*     ARRAY: -1    0    1    1.5   1.5    2    3    9    9.5   100 */

/*     index:  1    2    3     4     5     6    7    8     9     10 */

/*     The following table shows the values of LSTCLD that would be */
/*     returned for various values of X, and the corresponding closest */
/*     array element values. */

/*            X      LSTCLD( X,10,ARRAY )   ARRAY( LSTCLD( X,10,ARRAY )) */
/*          -----    --------------------   --------------------------- */
/*           0.12             2                          0 */
/*          -0.12             2                          0 */
/*          -2.0              1                         -1 */
/*           2.5              7                          3 */
/*           1.3              5                        1.5 */
/*         100.0             10                        100 */
/*         100.1             10                        100 */

/* $ Restrictions */

/*     1)  If the sequence is not non-decreasing, the routine will run */
/*         to completion but the index found will not mean anything. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     R.E. Thurman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) */

/* -& */
/* $ Index_Entries */

/*     last closest d.p. array element */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 30-AUG-1990 (MJS) */

/*        The following changes were made as a result of the */
/*        NAIF CK Code and Documentation Review: */

/*        1) The name of this routine was changed from CLOSTD to */
/*           LSTCLD because it was a more descriptive name. */
/*        2) All references (comments and code) were changed to reflect */
/*           the name change. */

/* -    Beta Version 1.0.0, 15-MAY-1990 (RET) */

/* -& */

/*     Local variables */


/*     Save the size of the array and point to the beginning and ending */
/*     positions. The pointers delimit the current search interval. */

    items = *n;
    begin = 1;
    end = *n;
    if (*n <= 0) {

/*        There is nothing in the array to compare against. Zero is the */
/*        only sensible thing to return. */

	ret_val = 0;
	return ret_val;
    } else if (*x <= array[begin - 1]) {

/*        All elements of the array are at least as big as X. So the */
/*        first element is the closest to X. */

	ret_val = 1;
    } else if (array[end - 1] <= *x) {

/*        X is at least as big as all elements of the array.  So the last */
/*        element is the closest to X. */

	ret_val = end;
    } else {

/*        X lies between some pair of elements of the array. */

	while(items > 2) {
	    j = items / 2;
	    middle = begin + j;
	    if (array[middle - 1] < *x) {
		begin = middle;
	    } else {
		end = middle;
	    }
	    items = end - begin + 1;
	}

/*        Which of the two is closest? */

	if (*x - array[begin - 1] < array[end - 1] - *x) {
	    ret_val = begin;
	} else {
	    ret_val = end;
	}
    }

/*     March down the array to find the last element equal to the */
/*     closet value. */

    while(ret_val < *n && array[ret_val - 1] == array[ret_val]) {
	++ret_val;
    }
    return ret_val;
} /* lstcld_ */

