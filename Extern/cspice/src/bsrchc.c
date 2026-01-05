/* bsrchc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure BSRCHC ( Binary search for a character string ) */
integer bsrchc_(char *value, integer *ndim, char *array, ftnlen value_len, 
	ftnlen array_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    logical l_lt(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer left, i__, right;

/* $ Abstract */

/*     Do a binary search for a given value within a character array, */
/*     assumed to be in nondecreasing order. Return the index of the */
/*     matching array entry, or zero if the key value is not found. */

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
/*     VALUE      I   Key value to be found in ARRAY. */
/*     NDIM       I   Dimension of ARRAY. */
/*     ARRAY      I   Character string array to search. */

/*     The function returns the index of the first matching array element */
/*     or zero if the value is not found. */

/* $ Detailed_Input */

/*     VALUE    is the key value to be found in the array. Trailing */
/*              blanks in this key are not significant: string matches */
/*              found by this routine do not require trailing blanks in */
/*              value to match those in the corresponding element of */
/*              array. */

/*     NDIM     is the number of elements in the input array. */

/*     ARRAY    is the array of character strings to be searched. */
/*              Trailing blanks in the strings in this array are not */
/*              significant. The elements in ARRAY are assumed to */
/*              sorted according to the ASCII collating sequence. */

/* $ Detailed_Output */

/*     The function returns the index of the specified value in the input */
/*     array. Indices range from 1 to NDIM. */

/*     If the input array does not contain the specified value, the */
/*     function returns zero. */

/*     If the input array contains more than one occurrence of the */
/*     specified value, the returned index may point to any of the */
/*     occurrences. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If NDIM < 1, the value of the function is zero. This is not */
/*         considered an error. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     A binary search is performed on the input array. If an element of */
/*     the array is found to match the input value, the index of that */
/*     element is returned. If no matching element is found, zero is */
/*     returned. */

/* $ Examples */

/*     Let ARRAY contain the following elements: */

/*           'BOHR' */
/*           'EINSTEIN' */
/*           'FEYNMAN' */
/*           'GALILEO' */
/*           'NEWTON' */

/*     Then */

/*           BSRCHC ( 'NEWTON',   5, ARRAY )    = 5 */
/*           BSRCHC ( 'EINSTEIN', 5, ARRAY )    = 2 */
/*           BSRCHC ( 'GALILEO',  5, ARRAY )    = 4 */
/*           BSRCHC ( 'Galileo',  5, ARRAY )    = 0 */
/*           BSRCHC ( 'BETHE',    5, ARRAY )    = 0 */

/* $ Restrictions */

/*     1)  ARRAY is assumed to be sorted in increasing order according to */
/*         the ASCII collating sequence. If this condition is not met, */
/*         the results of BSRCHC are unpredictable. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. Improved $Detailed_Input and */
/*        $Detailed_Output section. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     binary search for a character_string */

/* -& */

/*     Local variables */


/*     Set the initial bounds for the search area. */

    left = 1;
    right = *ndim;
    while(left <= right) {

/*        Check the middle element. */

	i__ = (left + right) / 2;

/*        If the middle element matches, return its location. */

	if (s_cmp(value, array + (i__ - 1) * array_len, value_len, array_len) 
		== 0) {
	    ret_val = i__;
	    return ret_val;

/*        Otherwise narrow the search area. */

	} else if (l_lt(value, array + (i__ - 1) * array_len, value_len, 
		array_len)) {
	    right = i__ - 1;
	} else {
	    left = i__ + 1;
	}
    }

/*     If the search area is empty, return zero. */

    ret_val = 0;
    return ret_val;
} /* bsrchc_ */

