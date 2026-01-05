/* isrchc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ISRCHC  ( Search in a character array ) */
integer isrchc_(char *value, integer *ndim, char *array, ftnlen value_len, 
	ftnlen array_len)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Search for a given value within a character string array. Return */
/*     the index of the first matching array entry, or zero if the key */
/*     value was not found. */

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

/*     The function returns the index of the first matching array */
/*     element or zero if the value is not found. */

/* $ Detailed_Input */

/*     VALUE    is the key value to be found in the array. Trailing */
/*              blanks in this key are not significant: string matches */
/*              found by this routine do not require trailing blanks in */
/*              value to match those in the corresponding element of */
/*              array. */

/*     NDIM     is the dimension of the array. */

/*     ARRAY    is the character array to be searched. Trailing */
/*              blanks in the strings in this array are not significant. */

/* $ Detailed_Output */

/*     The function returns the index of the first matching array */
/*     element in ARRAY. If VALUE is not found, ISRCHC is zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If NDIM < 1, the function value is zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The following table shows the value of ISRCHC given the contents */
/*     of ARRAY and VALUE: */

/*       ARRAY                 VALUE     ISRCHC */
/*     -----------------       -----     ------ */
/*     '1', '0', '4', '2'       '4'        3 */
/*     '1', '0', '4', '2'       '2'        4 */
/*     '1', '0', '4', '2'       '3'        0 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 03-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Extended */
/*        description of input arguments. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     search in a character array */

/* -& */

/*     Local variables */

    ret_val = 0;
    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (s_cmp(array + (i__ - 1) * array_len, value, array_len, value_len) 
		== 0) {
	    ret_val = i__;
	    return ret_val;
	}
    }
    return ret_val;
} /* isrchc_ */

