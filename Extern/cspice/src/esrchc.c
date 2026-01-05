/* esrchc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ESRCHC ( Equivalence search, character ) */
integer esrchc_(char *value, integer *ndim, char *array, ftnlen value_len, 
	ftnlen array_len)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Local variables */
    integer i__;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     Search for a given value within a character string array. */
/*     Return the index of the first equivalent array entry, or zero */
/*     if no equivalent element is found. */

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

/*     The function returns the index of the first array entry equivalent */
/*     to VALUE, or zero if none is found. */

/* $ Detailed_Input */

/*     VALUE    is an arbitrary character string. */

/*     NDIM     is the dimension of (number of elements in) an array of */
/*              character strings. */

/*     ARRAY    is the array. */

/* $ Detailed_Output */

/*     The function returns the index of the first element of the input */
/*     array equivalent to the input value, or zero if the array contains */
/*     no such elements. */

/*     Two strings are equivalent if they contain the same characters in */
/*     the same order, when blanks are ignored and uppercase and */
/*     lowercase characters are considered equal. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     ESRCHC is identical to ISRCHC, except that it looks for the first */
/*     equivalent string (as defined by EQSTR) instead of the first */
/*     identical one. */

/* $ Examples */

/*     Let ARRAY contain the following elements: */

/*        ARRAY(1) = 'This' */
/*        ARRAY(2) = 'little' */
/*        ARRAY(3) = 'piggy' */
/*        ARRAY(4) = 'went' */
/*        ARRAY(5) = 'to' */
/*        ARRAY(6) = 'market' */

/*     Then */

/*        ESRCHC ( 'PIGGY',      6, ARRAY )  =  3 */
/*        ESRCHC ( ' LiTtLe  ',  6, ARRAY )  =  2 */
/*        ESRCHC ( 'W e n t',    6, ARRAY )  =  4 */
/*        ESRCHC ( 'mall',       6, ARRAY )  =  0 */

/* $ Restrictions */

/*     1)  ESRCHC assumes that the function EQSTR does not participate */
/*         in normal SPICELIB error handling. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     search array for equivalent character_string */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Just like ISRCHC. */

    ret_val = 0;
    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (eqstr_(array + (i__ - 1) * array_len, value, array_len, value_len)
		) {
	    ret_val = i__;
	    return ret_val;
	}
    }
    return ret_val;
} /* esrchc_ */

