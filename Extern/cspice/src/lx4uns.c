/* lx4uns.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LX4UNS (Scan for unsigned integer) */
/* Subroutine */ int lx4uns_(char *string, integer *first, integer *last, 
	integer *nchar, ftnlen string_len)
{
    /* Initialized data */

    static logical doinit = TRUE_;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_len(char *, ftnlen);

    /* Local variables */
    static integer i__, l;
    static logical digit[384];

/* $ Abstract */

/*     Scan a string from a specified starting position for the */
/*     end of an unsigned integer. */

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

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   any character string */
/*     FIRST      I   first character to scan from in STRING */
/*     LAST       O   last character that is part of an unsigned integer */
/*     NCHAR      O   number of characters in the unsigned integer. */

/* $ Detailed_Input */

/*     STRING   is any character string. */

/*     FIRST    is the location in the string to beginning scanning */
/*              for an unsigned integer. It is assumed that the */
/*              unsigned integer begins at FIRST. */

/* $ Detailed_Output */

/*     LAST     is the last character at or after FIRST such that */
/*              the substring STRING(FIRST:LAST) is an unsigned */
/*              integer. If there is no such substring, LAST */
/*              will be returned with the value FIRST-1. */

/*     NCHAR    is the number of characters in the unsigned integer */
/*              that begins at FIRST and ends at last. If there */
/*              is no such string NCHAR will be given the value 0. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If FIRST is beyond either end of the string, then LAST will be */
/*         returned with the value FIRST-1 and NCHAR will be returned */
/*         with the value 0. */

/*     2)  If STRING(FIRST:FIRST) is not part of an unsigned integer then */
/*         LAST will be returned with the value FIRST-1 and NCHAR will be */
/*         returned with the value 0. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows you to scan forward in a string to locate */
/*     an unsigned integer that begins on the input character FIRST. */

/* $ Examples */

/*     Suppose you believe that a string has the form */

/*        X%Y%Z */

/*     where X, Y, and Z are unsigned integers of some unknown */
/*     length and % stands for some non-digit character. You could */
/*     use this routine to locate the unsigned integers in the */
/*     string as shown below. We'll keep track of the beginning and */
/*     ending of the unsigned integers in the integer arrays B and E. */

/*     FIRST = 1 */
/*     I     = 0 */

/*     DO WHILE ( FIRST .LT. LEN(STRING) ) */

/*        CALL LX4UNS ( STRING, FIRST, LAST, NCHAR ) */

/*        IF ( NCHAR .GT. 0 ) THEN */

/*           I     = I    + 1 */
/*           B(I)  = FIRST */
/*           E(I)  = LAST */
/*           FIRST = LAST + 2 */

/*        ELSE */

/*           FIRST = FIRST + 1 */

/*        END IF */

/*     END DO */

/* $ Restrictions */

/*     1)  Assumes ICHAR returns values in the range [-128, 255]. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 04-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/*        Fixed return value for LAST in $Exceptions section entry #1. */

/* -    SPICELIB Version 1.1.0, 03-DEC-2001 (NJB) */

/*        Updated to work if non-printing characters are present in */
/*        the input string. Updated $Restrictions section. */

/* -    SPICELIB Version 1.0.0, 12-JUL-1994 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Scan a string for an unsigned integer. */

/* -& */

/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     First we perform some initializations that are needed on */
/*     each pass through this routine. */

    if (doinit) {
	doinit = FALSE_;
	for (i__ = -128; i__ <= 255; ++i__) {
	    digit[(i__1 = i__ + 128) < 384 && 0 <= i__1 ? i__1 : s_rnge("dig"
		    "it", i__1, "lx4uns_", (ftnlen)217)] = FALSE_;
	}
	digit[(i__1 = '0' + 128) < 384 && 0 <= i__1 ? i__1 : s_rnge("digit", 
		i__1, "lx4uns_", (ftnlen)220)] = TRUE_;
	digit[(i__1 = '1' + 128) < 384 && 0 <= i__1 ? i__1 : s_rnge("digit", 
		i__1, "lx4uns_", (ftnlen)221)] = TRUE_;
	digit[(i__1 = '2' + 128) < 384 && 0 <= i__1 ? i__1 : s_rnge("digit", 
		i__1, "lx4uns_", (ftnlen)222)] = TRUE_;
	digit[(i__1 = '3' + 128) < 384 && 0 <= i__1 ? i__1 : s_rnge("digit", 
		i__1, "lx4uns_", (ftnlen)223)] = TRUE_;
	digit[(i__1 = '4' + 128) < 384 && 0 <= i__1 ? i__1 : s_rnge("digit", 
		i__1, "lx4uns_", (ftnlen)224)] = TRUE_;
	digit[(i__1 = '5' + 128) < 384 && 0 <= i__1 ? i__1 : s_rnge("digit", 
		i__1, "lx4uns_", (ftnlen)225)] = TRUE_;
	digit[(i__1 = '6' + 128) < 384 && 0 <= i__1 ? i__1 : s_rnge("digit", 
		i__1, "lx4uns_", (ftnlen)226)] = TRUE_;
	digit[(i__1 = '7' + 128) < 384 && 0 <= i__1 ? i__1 : s_rnge("digit", 
		i__1, "lx4uns_", (ftnlen)227)] = TRUE_;
	digit[(i__1 = '8' + 128) < 384 && 0 <= i__1 ? i__1 : s_rnge("digit", 
		i__1, "lx4uns_", (ftnlen)228)] = TRUE_;
	digit[(i__1 = '9' + 128) < 384 && 0 <= i__1 ? i__1 : s_rnge("digit", 
		i__1, "lx4uns_", (ftnlen)229)] = TRUE_;
    }
    *last = *first - 1;
    l = i_len(string, string_len);

/*     If start is beyond the ends of the string, we  can quit now. */

    if (*first < 1 || *first > l) {
	*nchar = 0;
	return 0;
    }

/*     Now for the real work of the routine. Examine characters one */
/*     at a time... */

    i__1 = l;
    for (i__ = *first; i__ <= i__1; ++i__) {

/*        If this character is a digit, move the LAST pointer one */
/*        further down on the string.  Otherwise set NCHAR and return. */

	if (digit[(i__2 = *(unsigned char *)&string[i__ - 1] + 128) < 384 && 
		0 <= i__2 ? i__2 : s_rnge("digit", i__2, "lx4uns_", (ftnlen)
		255)]) {
	    ++(*last);
	} else {
	    *nchar = *last + 1 - *first;
	    return 0;
	}
    }
    *nchar = *last + 1 - *first;
    return 0;
} /* lx4uns_ */

