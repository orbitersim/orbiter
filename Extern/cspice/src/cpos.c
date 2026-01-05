/* cpos.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CPOS ( Character position ) */
integer cpos_(char *str, char *chars, integer *start, ftnlen str_len, ftnlen 
	chars_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen), i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer b;
    logical found;
    integer lenstr;

/* $ Abstract */

/*     Find the first occurrence in a string of a character belonging */
/*     to a collection of characters, starting at a specified location, */
/*     searching forward. */

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

/*     SCANNING */

/* $ Keywords */

/*     CHARACTER */
/*     SEARCH */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STR        I   Any character string. */
/*     CHARS      I   A collection of characters. */
/*     START      I   Position to begin looking for one of CHARS */

/*     The function returns the index of the first character of STR */
/*     at or following index START that is in the collection CHARS. */

/* $ Detailed_Input */

/*     STR      is any character string. */

/*     CHARS    is a character string containing a collection */
/*              of characters. Spaces in CHARS are significant. */

/*     START    is the position in STR to begin looking for one of */
/*              the characters in CHARS. */

/* $ Detailed_Output */

/*     The function returns the index of the first character of STR */
/*     (at or following index START) that is one of the characters in */
/*     the string CHARS. If none of the characters is found, the */
/*     function returns zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If START is less than 1, the search begins at the first */
/*         character of the string. */

/*     2)  If START is greater than the length of the string, CPOS */
/*         returns zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     CPOS is case sensitive. */

/*     An entire family of related SPICELIB routines (POS, CPOS, NCPOS, */
/*     POSR, CPOSR, NCPOSR) is described in the Required Reading. */

/*     Those familiar with the True BASIC language should note that */
/*     these functions are equivalent to the True BASIC intrinsic */
/*     functions with the same names. */

/* $ Examples */

/*     Let STRING = 'BOB, JOHN, TED, AND MARTIN....' */
/*                   123456789012345678901234567890 */

/*     Normal (sequential) searching */
/*     ----------------------------- */

/*           CPOS( STRING, ' ,',    1  ) =  4 */

/*           CPOS( STRING, ' ,',    5  ) =  5 */

/*           CPOS( STRING, ' ,',    6  ) = 10 */

/*           CPOS( STRING, ' ,',    11 ) = 11 */

/*           CPOS( STRING, ' ,',    12 ) = 15 */

/*           CPOS( STRING, ' ,',    16 ) = 16 */

/*           CPOS( STRING, ' ,',    17 ) = 20 */

/*           CPOS( STRING, ' ,',    21 ) = 0 */


/*     START out of bounds */
/*     ------------------- */

/*           CPOS( STRING, ' ,',  -113 ) = 4 */

/*           CPOS( STRING, ' ,',    -1 ) = 4 */

/*           CPOS( STRING, ' ,',    31 ) = 0 */

/*           CPOS( STRING, ' ,',  1231 ) = 0 */


/*     Order within CHARS */
/*     ------------------ */

/*           CPOS( STRING, ',. ',   22 ) = 27 */

/*           CPOS( STRING, ' ,.',   22 ) = 27 */

/*           CPOS( STRING, ', .',   22 ) = 27 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 02-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.3, 31-JAN-2008 (BVS) */

/*        Removed non-standard end-of-declarations marker */
/*        'C%&END_DECLARATIONS' from comments. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 26-MAR-1991 (HAN) */

/*        The Required Reading file POSITION was renamed to SCANNING. */
/*        This header was updated to reflect the change. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     forward search for position of character */

/* -& */

/*     Local variables */

    lenstr = i_len(str, str_len);
    b = max(1,*start);
    found = FALSE_;
    ret_val = 0;
    while(! found) {
	if (b > lenstr) {
	    return ret_val;
	} else if (i_indx(chars, str + (b - 1), chars_len, (ftnlen)1) != 0) {
	    ret_val = b;
	    return ret_val;
	} else {
	    ++b;
	}
    }
    return ret_val;
} /* cpos_ */

