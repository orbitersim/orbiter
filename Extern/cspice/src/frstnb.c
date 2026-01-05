/* frstnb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure FRSTNB ( First non-blank character ) */
integer frstnb_(char *string, ftnlen string_len)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_len(char *, ftnlen);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Return the index of the first non-blank character in */
/*     a character string. */

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

/*     ASCII */
/*     CHARACTER */
/*     SEARCH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   Input character string. */

/*     The function returns the index of the first non-blank character in */
/*     STRING. */

/* $ Detailed_Input */

/*     STRING   is the input character string. */

/* $ Detailed_Output */

/*     The function returns the index if the first non-blank character in */
/*     the input string. If there are no non-blank characters in the */
/*     string, FRSTNB is zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     If the string is blank, return zero. Otherwise, step through */
/*     the string one character at a time until something other than */
/*     a blank is found. Return the index of that something within */
/*     the string. */

/* $ Examples */

/*     The following examples illustrate the use of FRSTNB. */

/*           FRSTNB ( 'ABCDE'         )   = 1 */
/*           FRSTNB ( 'AN EXAMPLE'    )   = 1 */
/*           FRSTNB ( '   AN EXAMPLE' )   = 4 */
/*           FRSTNB ( '             ' )   = 0 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 08-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG) */

/*        Modified the comparison to use integer values and the ICHAR() */
/*        function. This improves the performance of the subroutine. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     first non-blank character */

/* -& */

/*     Local parameters */


/*     Local variables */


/*     Just like it says in the header. */

    if (s_cmp(string, " ", string_len, (ftnlen)1) == 0) {
	ret_val = 0;
    } else {
	i__1 = i_len(string, string_len);
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (*(unsigned char *)&string[i__ - 1] != 32) {
		ret_val = i__;
		return ret_val;
	    }
	}
    }
    return ret_val;
} /* frstnb_ */

