/* notru.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure NOTRU ( No true entries? ) */
logical notru_(logical *logcls, integer *n)
{
    /* System generated locals */
    integer i__1;
    logical ret_val;

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Determine if none the entries in an array of logicals are .TRUE. */

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

/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LOGCLS     I   An array of logicals. */
/*     N          I   Number of elements in the array LOGCLS. */

/*     The function returns .TRUE. if no entry has a value of .TRUE. */

/* $ Detailed_Input */

/*     LOGCLS   is an array of logicals. */

/*     N        is the number of elements in the array LOGCLS */

/* $ Detailed_Output */

/*     The function returns true if no entry of LOGCLS is .TRUE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If N is less than 1, the function returns a value of .TRUE. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This function examines each element of LOGCLS until */
/*     a .TRUE. value is found or until all values have been */
/*     examined. */

/* $ Examples */

/*     Suppose you needed to confirm that no entry of a character set */
/*     WORDS was one of the words in the phrase */

/*       'EVERY GOOD BOY DOES FINE' */

/*     You might execute the following block of code. */

/*           FOUND(1) = ELEMC  ( 'EVERY', WORDS ) */
/*           FOUND(2) = ELEMC  ( 'GOOD',  WORDS ) */
/*           FOUND(3) = ELEMC  ( 'BOY',   WORDS ) */
/*           FOUND(4) = ELEMC  ( 'DOES',  WORDS ) */
/*           FOUND(5) = ELEMC  ( 'FINE',  WORDS ) */

/*           OK       = NOTRU  ( FOUND,   5     ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 12-JUL-1991 (WLT) */

/* -& */
/* $ Index_Entries */

/*     test whether no logicals in an array are true */

/* -& */

/*     Just do it. */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (logcls[i__ - 1]) {
	    ret_val = FALSE_;
	    return ret_val;
	}
    }
    ret_val = TRUE_;
    return ret_val;
} /* notru_ */

