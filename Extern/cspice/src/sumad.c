/* sumad.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SUMAD ( Sum of a double precision array ) */
doublereal sumad_(doublereal *array, integer *n)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    integer i__;
    doublereal sum;

/* $ Abstract */

/*     Return the sum of the elements of a double precision array. */

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
/*     MATH */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ARRAY      I   Input array. */
/*     N          I   Number of elements in ARRAY. */

/*     The function returns the sum of the elements of ARRAY. */

/* $ Detailed_Input */

/*     ARRAY    is the input double precision array. */

/*     N        is the number of elements in the array. */

/* $ Detailed_Output */

/*     The function returns the sum of the elements of the input array. */
/*     That is, */

/*        SUMAD( ARRAY, N ) = ARRAY(1) + ARRAY(2) + ... + ARRAY(N) */

/*     If N is zero or negative, SUMAD is zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The value of the function is initially set to zero. The elements */
/*     of the array are then added. If the number of elements is zero or */
/*     negative, SUMAD is zero. */

/* $ Examples */

/*     Let ARRAY contain the following elements. */

/*           ARRAY(1) = 12.D0 */
/*           ARRAY(2) =  1.D0 */
/*           ARRAY(3) =  4.D0 */
/*           ARRAY(4) = 75.D0 */
/*           ARRAY(5) = 18.D0 */

/*     Then */

/*           SUMAD ( ARRAY,   -3 )       =   0.D0 */
/*           SUMAD ( ARRAY,    0 )       =   0.D0 */
/*           SUMAD ( ARRAY,    1 )       =  12.D0 */
/*           SUMAD ( ARRAY,    2 )       =  13.D0 */
/*           SUMAD ( ARRAY,    5 )       = 110.D0 */
/*           SUMAD ( ARRAY(3), 3 )       =  97.D0 */

/* $ Restrictions */

/*     1)  SUMAD does not check for overflow. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 09-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     sum of a d.p. array */

/* -& */

/*     Local variables */


/*     Begin at zero. */

    sum = 0.;

/*     Sum the elements. If N is zero or negative, nothing happens. */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum += array[i__ - 1];
    }

/*     Return the sum. */

    ret_val = sum;
    return ret_val;
} /* sumad_ */

