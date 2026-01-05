/* inttxt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure INTTXT ( Convert an integer to text ) */
/* Subroutine */ int inttxt_(integer *n, char *string, ftnlen string_len)
{
    /* Initialized data */

    static char tens[9*9] = "TEN      " "TWENTY   " "THIRTY   " "FORTY    " 
	    "FIFTY    " "SIXTY    " "SEVENTY  " "EIGHTY   " "NINETY   ";
    static char number[9*19] = "ONE      " "TWO      " "THREE    " "FOUR     "
	     "FIVE     " "SIX      " "SEVEN    " "EIGHT    " "NINE     " 
	    "TEN      " "ELEVEN   " "TWELVE   " "THIRTEEN " "FOURTEEN " "FIF"
	    "TEEN  " "SIXTEEN  " "SEVENTEEN" "EIGHTEEN " "NINETEEN ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    char suff[9];
    integer x, y, space;
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    integer pad, num;

/* $ Abstract */

/*     Convert an integer to an equivalent written phrase. */
/*     For example, convert 121 to 'ONE HUNDRED TWENTY-ONE'. */

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

/*     CONVERSION */
/*     PARSING */
/*     STRING */
/*     UNITS */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     N          I   An integer (less than 10**12 in absolute value). */
/*     STRING     O   An English string representing the cardinal of N. */

/* $ Detailed_Input */

/*     N        is any integer (less than 10**12 in absolute value). */
/*              If N is less than 0, -N must be a legitimate number. */

/* $ Detailed_Output */

/*     STRING   is the English cardinal equivalent of N. STRING will */
/*              contain only upper case letters. */

/*              The longest possible output string contains 145 */
/*              characters. One such string is: */

/*                 'NEGATIVE '                                  // */
/*                 'SEVEN HUNDRED SEVENTY-SEVEN BILLION '       // */
/*                 'SEVEN HUNDRED SEVENTY-SEVEN MILLION '       // */
/*                 'SEVEN HUNDRED SEVENTY-SEVEN THOUSAND '      // */
/*                 'SEVEN HUNDRED SEVENTY-SEVEN' */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the resulting text is longer than the output string, */
/*         it will be truncated on the right, leaving only the most */
/*         significant portion of the number. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is used primarily for constructing error messages. */
/*     For example, an overflow message might look like the following: */

/*        'An excess of seventy-four parameters was detected.' */

/*     A second use might be to write dollar amounts: it's much harder */
/*     to tamper with a string like */

/*        'Two thousand four hundred seventy-one dollars' */

/*     than with the equivalent string */

/*        '$ 2471.00' */

/* $ Examples */

/*     N           STRING */
/*     ------      ------------------------------------------ */
/*     -43         NEGATIVE FORTY-THREE */
/*      1          ONE */
/*      2          TWO */
/*      3          THREE */
/*      4          FOUR */
/*      20         TWENTY */
/*      21         TWENTY-ONE */
/*      99         NINETY-NINE */
/*      82131      EIGHTY-TWO THOUSAND ONE HUNDRED THIRTY-ONE */

/* $ Restrictions */

/*     1)  This routine assumes that N will always be less than */
/*         a trillion (10**12) in absolute value. */

/*     2)  In the event that N is less than zero, this routine assumes */
/*         that -N is a legitimate integer on the host machine. */

/*     3)  This routine assumes that an integer as large as 10**9 */
/*         (one billion) is representable on the host machine. */

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

/* -    SPICELIB Version 1.0.0, 15-AUG-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     convert an integer to text */

/* -& */

/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Zero is easy. */

    if (*n == 0) {
	s_copy(string, "ZERO", string_len, (ftnlen)4);
	return 0;
    }

/*     If the number is negative, the string begins with the word */
/*     `NEGATIVE', and otherwise the number can be treated as though */
/*     it were positive. */

    if (*n < 0) {
	num = -(*n);
	s_copy(string, "NEGATIVE", string_len, (ftnlen)8);
    } else {
	num = *n;
	s_copy(string, " ", string_len, (ftnlen)1);
    }

/*     Construct the number portion, from left to right: billions, */
/*     then millions, and so on. In case of overflow, SUFFIX simply */
/*     leaves the output string unchanged, so there is no need to */
/*     check explicitly for truncation. */

    while(num > 0) {

/*        Find the right unit (billion, million, or whatever), */
/*        and the number (X) of those units. X should always */
/*        be between zero and 999, regardless of the units. */

	if (num >= 1000000000) {
	    x = num / 1000000000;
	    s_copy(suff, "BILLION", (ftnlen)9, (ftnlen)7);
	    num -= x * 1000000000;
	} else if (num >= 1000000) {
	    x = num / 1000000;
	    s_copy(suff, "MILLION", (ftnlen)9, (ftnlen)7);
	    num -= x * 1000000;
	} else if (num >= 1000) {
	    x = num / 1000;
	    s_copy(suff, "THOUSAND", (ftnlen)9, (ftnlen)8);
	    num -= x * 1000;
	} else {
	    x = num;
	    s_copy(suff, " ", (ftnlen)9, (ftnlen)1);
	    num = 0;
	}

/*        Convert X to text, ... */

	space = 1;
	while(x > 0) {
	    if (s_cmp(string, " ", string_len, (ftnlen)1) == 0) {
		pad = 0;
	    } else {
		pad = 1;
	    }
	    if (x >= 100) {
		y = x / 100;
		x -= y * 100;
		suffix_(number + ((i__1 = y - 1) < 19 && 0 <= i__1 ? i__1 : 
			s_rnge("number", i__1, "inttxt_", (ftnlen)297)) * 9, &
			pad, string, (ftnlen)9, string_len);
		suffix_("HUNDRED", &c__1, string, (ftnlen)7, string_len);
	    } else if (x >= 20) {
		y = x / 10;
		x -= y * 10;
		suffix_(tens + ((i__1 = y - 1) < 9 && 0 <= i__1 ? i__1 : 
			s_rnge("tens", i__1, "inttxt_", (ftnlen)305)) * 9, &
			pad, string, (ftnlen)9, string_len);
		if (x != 0) {
		    suffix_("-", &c__0, string, (ftnlen)1, string_len);
		    space = 0;
		}
	    } else {
		y = x;
		x = 0;
		if (s_cmp(string, " ", string_len, (ftnlen)1) == 0) {
		    space = 0;
		}
		suffix_(number + ((i__1 = y - 1) < 19 && 0 <= i__1 ? i__1 : 
			s_rnge("number", i__1, "inttxt_", (ftnlen)321)) * 9, &
			space, string, (ftnlen)9, string_len);
	    }
	}

/*        ... then add the units. Repeat as necessary. */

	suffix_(suff, &c__1, string, (ftnlen)9, string_len);
    }
    return 0;
} /* inttxt_ */

