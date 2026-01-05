/* fndnwd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure FNDNWD ( Find the next word after an index ) */
/* Subroutine */ int fndnwd_(char *string, integer *start, integer *b, 
	integer *e, ftnlen string_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer size, i__, l, n, blank;
    logical thisb, lastn;

/* $ Abstract */

/*     Find the beginning and end of the first word starting at */
/*     or after a specified character. */

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

/*     PARSING */
/*     SEARCH */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   A string to examine for words. */
/*     START      I   Position in the string to start looking for words. */
/*     B          O   String position of first character of the word. */
/*     E          O   String position of last character of the word. */

/* $ Detailed_Input */

/*     STRING   is a character string that potentially consists of */
/*              words of text. */

/*     START    is the index of a letter within the string from which */
/*              to start looking for the next word. */

/* $ Detailed_Output */

/*     B        is the index of the first letter of the word substring */
/*              of STRING that begins at or after position START. If */
/*              there are no such substrings I is returned as 0. */

/*     E        is the index of the last letter of the word substring */
/*              of STRING that begins at or after position START. If */
/*              there are no such substrings J is returned as 0. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given a character string and location of a character within that */
/*     string, this routine finds the first full word of the string */
/*     that starts on or after the specified location. */

/* $ Examples */

/*     1         2         3         4         5 */
/*              12345678901234567890123456789012345678901234567890 */
/*     STRING: 'Now is the time for all good men to go home to bed' */

/*     START    I      J */
/*     -----   ---    --- */
/*     1        1      3 */
/*     2        5      6 */
/*     3        5      6 */
/*     4        5      6 */
/*     5        5      6 */
/*     6        8      10 */
/*     7        8      10 */
/*     8        8      10 */
/*     9        12     15 */

/*     48       48     50 */
/*     49       0      0 */
/*     111      0      0 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 15-OCT-1993 (WLT) */

/*        The routine was completely rewritten with a resulting */
/*        increase in execution speed of between 2000% and 6000%. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     find the next word after an index */

/* -& */
/* $ Revisions */

/* -     SPICELIB Version 2.0.0, 15-OCT-1993 (WLT) */

/*         The routine was completely rewritten with a resulting */
/*         increase in execution speed of between 2000% and 6000%. */
/*         It was tested against the old version of the routine to */
/*         ensure that the functionality was exactly duplicated. */

/* -& */

/*     Local Variables */


/*     Set up neede parameters and check obvious out-of-bound cases. */

    blank = ' ';
    size = i_len(string, string_len);
    if (*start > size) {
	*b = 0;
	*e = 0;
	return 0;
    }
    n = max(1,*start);
    l = n - 1;
    if (l <= 0) {
	lastn = FALSE_;
    } else {
	lastn = *(unsigned char *)&string[l - 1] != blank;
    }
    thisb = *(unsigned char *)&string[n - 1] == blank;

/*     Search for the beginning of a word (the last character */
/*     blank and the current non-blank). */

    while(thisb || lastn) {
	++n;
	if (n > size) {
	    *b = 0;
	    *e = 0;
	    return 0;
	}
	lastn = ! thisb;
	thisb = *(unsigned char *)&string[n - 1] == blank;
    }

/*     If we get this far, we found the beginning of the */
/*     string.  To find the end look for the next blank and */
/*     back up one. */

    *b = n;
    i__1 = size;
    for (i__ = n + 1; i__ <= i__1; ++i__) {
	if (*(unsigned char *)&string[i__ - 1] == blank) {
	    *e = i__ - 1;
	    return 0;
	}
    }

/*     If we get this far, the word ends at the end of the */
/*     string. */

    *e = size;
    return 0;
} /* fndnwd_ */

