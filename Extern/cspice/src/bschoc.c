/* bschoc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure BSCHOC ( Binary search with order vector, character ) */
integer bschoc_(char *value, integer *ndim, char *array, integer *order, 
	ftnlen value_len, ftnlen array_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    logical l_lt(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer left, i__, right;

/* $ Abstract */

/*     Do a binary search for a given value within an array of character */
/*     strings, accompanied by an order vector. Return the index of the */
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
/*     ORDER      I   Order vector. */

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
/*              significant. */

/*     ORDER    is an order array that can be used to access the elements */
/*              of ARRAY in order (according to the ASCII collating */
/*              sequence). The contents of ORDER are a permutation of */
/*              the sequence of integers ranging from 1 to NDIM. */

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

/*     A binary search is performed on the input array, whose order is */
/*     given by an associated order vector. If an element of the array is */
/*     found to match the input value, the index of that element is */
/*     returned. If no matching element is found, zero is returned. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Search for different character strings in an array that */
/*        is sorted following a given criteria, not necessarily */
/*        alphabetically. */

/*        Example code begins here. */


/*              PROGRAM BSCHOC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              INTEGER                 BSCHOC */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 NDIM */
/*              PARAMETER             ( NDIM   = 5  ) */

/*              INTEGER                 STRLEN */
/*              PARAMETER             ( STRLEN = 8  ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(STRLEN)      ARRAY  ( NDIM ) */
/*              CHARACTER*(STRLEN)      NAMES  ( NDIM ) */

/*              INTEGER                 I */
/*              INTEGER                 IDX */
/*              INTEGER                 ORDER  ( NDIM ) */


/*        C */
/*        C     Let ARRAY and ORDER contain the following elements: */
/*        C */
/*              DATA                    ARRAY  / 'FEYNMAN', 'BOHR', */
/*             .                     'EINSTEIN', 'NEWTON',  'GALILEO' / */

/*              DATA                    ORDER  / 2, 3, 1, 5, 4 / */

/*        C */
/*        C     Set the list of NAMES to be searched. */
/*        C */
/*              DATA                    NAMES /  'NEWTON',  'EINSTEIN', */
/*             .                      'GALILEO', 'Galileo', 'BETHE'    / */

/*        C */
/*        C     Search for the NAMES. */
/*        C */
/*              DO I = 1, NDIM */

/*                 IDX = BSCHOC ( NAMES(I), NDIM, ARRAY, ORDER ) */

/*                 IF ( IDX .EQ. 0 ) THEN */

/*                    WRITE(*,*) 'Name ', NAMES(I), */
/*             .                 ' not found in ARRAY.' */

/*                 ELSE */

/*                    WRITE(*,*) 'Name ', NAMES(I), */
/*             .                 ' found in position', IDX */

/*                 END IF */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Name NEWTON   found in position           4 */
/*         Name EINSTEIN found in position           3 */
/*         Name GALILEO  found in position           5 */
/*         Name Galileo  not found in ARRAY. */
/*         Name BETHE    not found in ARRAY. */


/*        Note that these results indicate that: */

/*            ARRAY(4) = 'NEWTON' */
/*            ARRAY(3) = 'EINSTEIN' */
/*            ARRAY(5) = 'GALILEO' */

/* $ Restrictions */

/*     1)  ORDER is assumed to give the order of the elements of ARRAY in */
/*         increasing order according to the ASCII collating sequence. If */
/*         this condition is not met, the results of BSCHOC are */
/*         unpredictable. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 17-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/*        Updated $Brief_I/O, $Detailed_Input and $Detailed_Output */
/*        sections to improve the description of the arguments and */
/*        returned values of the function. */

/* -    SPICELIB Version 1.0.0, 18-SEP-1995 (IMU) (WLT) */

/* -& */
/* $ Index_Entries */

/*     binary search for a string using an order vector */

/* -& */

/*     Local variables */


/*     Set the initial bounds for the search area. */

    left = 1;
    right = *ndim;
    while(left <= right) {

/*        Check the middle element. */

	i__ = (left + right) / 2;

/*        If the middle element matches, return its location. */

	if (s_cmp(value, array + (order[i__ - 1] - 1) * array_len, value_len, 
		array_len) == 0) {
	    ret_val = order[i__ - 1];
	    return ret_val;

/*        Otherwise narrow the search area. */

	} else if (l_lt(value, array + (order[i__ - 1] - 1) * array_len, 
		value_len, array_len)) {
	    right = i__ - 1;
	} else {
	    left = i__ + 1;
	}
    }

/*     If the search area is empty, return zero. */

    ret_val = 0;
    return ret_val;
} /* bschoc_ */

