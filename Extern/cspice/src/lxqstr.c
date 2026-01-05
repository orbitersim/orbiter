/* lxqstr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LXQSTR ( Lex quoted string ) */
/* Subroutine */ int lxqstr_(char *string, char *qchar, integer *first, 
	integer *last, integer *nchar, ftnlen string_len, ftnlen qchar_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen), i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    logical even;
    integer l, loc, pos;

/* $ Abstract */

/*     Scan (lex) a quoted string. */

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

/*     CHARACTER */
/*     PARSING */
/*     SCANNING */
/*     STRING */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   String to be scanned. */
/*     QCHAR      I   Quote delimiter character. */
/*     FIRST      I   Character position at which to start scanning. */
/*     LAST       O   Character position of end of token. */
/*     NCHAR      O   Number of characters in token. */

/* $ Detailed_Input */

/*     STRING   is a character string that may contain a `string */
/*              token' starting at the character position */
/*              indicated by the input argument FIRST (see below). */
/*              String tokens are sequences of characters that */
/*              represent literal strings. Syntactically, a string */
/*              token is a sequence of characters that begins and */
/*              ends with a designated `quote character'. Within */
/*              the token, any occurrence of the quote character */
/*              is indicated by an adjacent pair of quote */
/*              characters: for example, if the quote character is */

/*                 " */

/*              then the token representing one instance of this */
/*              character is */

/*                 """" */

/*              Here the first quote indicates the beginning of the */
/*              token, the next two quotes together indicate a */
/*              single quote character that constitutes the */
/*              `contents' of the token, and the final quote */
/*              indicates the end of the token. */

/*     QCHAR    is the quote character. This is always a single */
/*              character. The characters */

/*                 "  and ' */

/*              are common choices, but any non-blank character is */
/*              accepted. Case *is* significant in QCHAR. */


/*     FIRST    is the character position at which the routine */
/*              is to start scanning a quoted string token. Note */
/*              that the character STRING(FIRST:FIRST) must equal */
/*              QCHAR if a string token is to be found; this */
/*              routine does *not* attempt to locate the first */
/*              quoted string following the position FIRST. */

/* $ Detailed_Output */

/*     LAST     is the last character position such that the */
/*              substring STRING(FIRST:LAST) is a quoted string */
/*              token, if such a substring exists. Otherwise, the */
/*              returned value of LAST is FIRST-1. */

/*     NCHAR    is the length of the string token found by this */
/*              routine, if such a token exists. This length */
/*              includes the starting and ending bracketing quotes. */
/*              If a string token is not found, the returned value */
/*              of NCHAR is zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the input argument FIRST is less than 1 or greater than */
/*         LEN(STRING)-1, the returned value of LAST is FIRST-1, and the */
/*         returned value of NCHAR is zero. */

/*     2)  It is not an error for a quoted string token to consist of */
/*         two consecutive quote characters with no intervening */
/*         characters. Calling routines that require special treatment */
/*         of null tokens must handle this case. */

/*     3)  If the input argument QCHAR is blank, the returned value of */
/*         LAST is FIRST-1, and the returned value of NCHAR is zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Quote characters may be ANY non-blank character. For example, the */
/*     ampersand */

/*        & */

/*     is a perfectly valid quote character. If we were using the */
/*     ampersand as the quote character, then the term `doubled quote' */
/*     in the following discussion would refer to the sequence */

/*        && */

/*     not the character */

/*        " */

/*     The string tokens identified by this routine are Fortran-style */
/*     quoted strings: they start and end with quote characters. In the */
/*     interior of any such token, any quote characters are represented */
/*     by doubled quote characters. These rules imply that the number of */
/*     quote characters in a quoted string token is always even. The end */
/*     of a quoted string token is located at the first even-numbered */
/*     quote character, counting from the initial quote character, that */
/*     is  not the first member of a pair of quotes indicating an */
/*     embedded quote character. */

/*     To map the token to the string of characters it represents, use */
/*     the SPICELIB subroutine PARSQS (String parse, quoted).  PARSQS */
/*     removes the bracketing quotes from a quoted string token and */
/*     converts each doubled quote between the bracketing quotes to a */
/*     single quote. For example, the token */

/*        """" */

/*     identified by this routine would be mapped by PARSQS to a string */
/*     variable containing the single character */

/*        " */

/* $ Examples */

/*     1)  The table below illustrates the action of this routine. */


/*         STRING CONTENTS               QCHAR   FIRST   LAST   NCHAR */
/*         ========================================================== */
/*         The "SPICE" system            "       5       11     7 */
/*         The "SPICE" system            "       1       0      0 */
/*         The "SPICE" system            '       5       4      0 */
/*         The """SPICE"" system"        "       5       22     18 */
/*         The """SPICE"""" system       "       5       15     11 */
/*         The &&&SPICE system           &       5       6      2 */
/*         ' '                           '       1       3      3 */
/*         ''                            '       1       2      2 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 25-FEB-2002 (NJB) */

/*        Corrected references to other SPICELIB routines in header. */

/* -    SPICELIB Version 1.0.0, 20-OCT-1994 (NJB) */

/* -& */
/* $ Index_Entries */

/*     scan quoted string token */
/*     lex quoted string token */
/*     recognize quoted string token */

/* -& */

/*     Local variables */


/*     Error free, no check-in required. */

    l = i_len(string, string_len);

/*     Handle the cases in which we can tell right away that */
/*     no token can be found. */

    if (*first < 1 || *first > l - 1 || *(unsigned char *)qchar == ' ' || *(
	    unsigned char *)&string[*first - 1] != *(unsigned char *)qchar) {
	*last = *first - 1;
	*nchar = 0;
	return 0;
    }

/*     We started out with a quote character, if we got this far.  Now */
/*     we have to see whether a quoted string token exists.  Note that */
/*     we can safely assume FIRST+1 does not exceed L. */

    i__1 = *first;
    loc = i_indx(string + i__1, qchar, l - i__1, (ftnlen)1);
    if (loc == 0) {
	*last = *first - 1;
	*nchar = 0;
	return 0;
    }

/*     At this point, we have a candidate ending point for the token. */
/*     We must search for the actual end of the token.  The token ends */
/*     at the first even-numbered quote character that is not part of */
/*     an embedded pair of quotes. */

/*     Our method of looking for the end of the token will be to search */
/*     from left to right, keeping track of the rightmost character */
/*     position that could be the end of the string token, until we find */
/*     a definitive answer as to the status of our candidate. */
/*     The variable LAST will be used for this candidate character */
/*     position.  The variable EVEN will tell us whether we've seen an */
/*     even number of quotes.  The variable POS will point to the current */
/*     character to examine. */

    *last = *first + loc;
    even = TRUE_;
    pos = *last + 1;
    while(pos <= l) {
	if (*(unsigned char *)&string[pos - 1] == *(unsigned char *)qchar) {

/*           Each quote character we see toggles the quote parity. */

	    even = ! even;

/*           If the current parity is even, the current quote character */
/*           becomes the candidate for the final quote.  This character */
/*           can lose out only to a quote that is further to the right. */

	    if (even) {
		*last = pos;
	    }
	} else {
	    if (even) {

/*              The last even-numbered quote was followed by a non-quote */
/*              character.  We're done. */

		*nchar = *last - *first + 1;
		return 0;
	    }
	}
	++pos;

/*        At this point in the loop, */

/*           EVEN indicates whether we've seen an even number of quote */
/*           characters. */

/*           LAST is the index, relative to the start of the string, */
/*           of the last even-numbered quote we've seen.  This is the */
/*           current candidate for the closing quote. */

/*           POS is the index of the next character to examine. */

    }

/*     Since there are no further characters to examine, the value of */
/*     LAST that we already have is the largest value we can get. */

    *nchar = *last - *first + 1;
    return 0;
} /* lxqstr_ */

