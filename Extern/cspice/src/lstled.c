/* lstled.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LSTLED ( Last double precision element less than or equal) */
integer lstled_(doublereal *x, integer *n, doublereal *array)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    integer j, begin, items, middle, end;

/* $ Abstract */

/*     Find the index of the largest array element less than or equal */
/*     to a given number X in an array of non-decreasing numbers. */

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
/*     X          I   Upper bound value to search against. */
/*     N          I   Number of elements in ARRAY. */
/*     ARRAY      I   Array of possible lower bounds. */

/*     The function returns the index of the last element of ARRAY that */
/*     is less than or equal to X. */

/* $ Detailed_Input */

/*     X        is a double precision value acting as an upper bound: the */
/*              element of ARRAY that is the greatest element less than */
/*              or equal to X is to be found. */

/*     N        is the total number of elements in ARRAY. */

/*     ARRAY    is an array of double precision numbers that forms a */
/*              non-decreasing sequence. The elements of array need not */
/*              be distinct. */

/* $ Detailed_Output */

/*     The function returns the index of the highest-indexed element in */
/*     the input array that is less than or equal to X. The routine */
/*     assumes the array elements are sorted in non-decreasing order. */

/*     Indices range from 1 to N. */

/*     If all elements of ARRAY are greater than X, the routine returns */
/*     the value 0. If N is less than or equal to zero, the routine */
/*     returns the value 0. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If N is less than or equal to zero, the function returns 0. */
/*         This case is not treated as an error. */

/*     2)  If the input array is not sorted in non-decreasing order, the */
/*         output of this routine is undefined. No error is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine uses a binary search algorithm and so requires */
/*     at most on the order of */

/*        log (N) */
/*           2 */

/*     steps to compute the value of LSTLED. */

/*     Note: If you need to find the first element of the array that is */
/*     greater than X, simply add 1 to the result returned by this */
/*     function and check to see if the result is within the array bounds */
/*     given by N. */

/* $ Examples */

/*     If ARRAY(I) = -1 + 4*I/3 (real arithmetic implied here) */

/*     N        = 10 */
/*     X        = 7.12 */

/*     then */

/*     LSTLED will be I where */
/*             (4*I/3) - 1       < or = 7.12 */
/*     but */
/*             (4*(I+1)/3) - 1   >      7.12 . */

/*     In this case our subsequence is: */
/*            1/3, 5/3, 9/3, 13/3, 17/3, 21/3, 25/3, .... 37/3 */

/*     index:  1    2    3    4     5     6     7    ....  10 */

/*     Thus LSTLED will be returned as 6 */

/*     The following table shows the values of LSTLED that would be */
/*     returned for various values of X */

/*            X       LSTLED */
/*          -----     ------- */
/*           0.12        0 */
/*           1.34        1 */
/*           5.13        4 */
/*           8.00        6 */
/*          15.10       10 */

/* $ Restrictions */

/*     1)  If the sequence of double precision numbers in the input array */
/*         ARRAY is not non-decreasing, the program will run to */
/*         completion but the index found will not mean anything. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. Improved $Detailed_Input, */
/*        $Detailed_Output, $Particulars, $Exceptions and $Restrictions */
/*        sections. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (NJB) */

/* -& */
/* $ Index_Entries */

/*     last d.p. element less_than_or_equal_to */

/* -& */

/*     Local variables */

    items = *n;
    begin = 1;
    end = *n;
    if (*n <= 0) {

/*        There's nobody home---that is there is nothing in the array */
/*        to compare against.  Zero is the only sensible thing to return. */

	ret_val = 0;
    } else if (*x < array[begin - 1]) {

/*        None of the array elements are less than or equal to X */

	ret_val = 0;
    } else if (*x >= array[end - 1]) {

/*        X is greater than or equal to all elements of the array.  Thus */
/*        the last element of the array is the last item less than or */
/*        equal to X. */

	ret_val = end;
    } else {

/*        X lies between some pair of elements of the array */

	while(items > 2) {
	    j = items / 2;
	    middle = begin + j;
	    if (array[middle - 1] <= *x) {
		begin = middle;
	    } else {
		end = middle;
	    }
	    items = end - begin + 1;
	}
	ret_val = begin;
    }
    return ret_val;
} /* lstled_ */

