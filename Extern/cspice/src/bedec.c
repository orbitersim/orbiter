/* bedec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure BEDEC  ( Be a decimal number? ) */
logical bedec_(char *string, ftnlen string_len)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer c__, d__, e, l;
    extern logical beint_(char *, ftnlen), beuns_(char *, ftnlen);
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     Determine whether a string represents a decimal number. */

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

/*     ALPHANUMERIC */
/*     NUMBERS */
/*     SCANNING */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   Character string. */

/*     The function returns .TRUE. if the string represents a decimal */
/*     number. Otherwise, it returns .FALSE. */

/* $ Detailed_Input */

/*     STRING   is any string. */

/* $ Detailed_Output */

/*     If the input string contains a decimal number (as defined */
/*     in $Particulars below), the function returns .TRUE. Otherwise, */
/*     the functions returns .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     A decimal number may be constructed by concatenating */
/*     the following components in the order shown. */

/*        1) A sign ('+' or '-'), or the null string. */

/*        2) An unsigned integer (as defined by function BEUNS), */
/*           or the null string. */

/*        3) A decimal point, or the null string. */

/*        4) An unsigned integer, or the null string. */

/* $ Examples */

/*     Four classes of numbers recognized by the various BE functions. */

/*        UNS      unsigned integer */
/*        INT      integer                (includes INT) */
/*        DEC      decimal number         (includes UNS, INT) */
/*        NUM      number                 (includes UNS, INT, NUM) */

/*     The following table illustrates the differences between */
/*     the classes. (Any number of leading and trailing blanks */
/*     are acceptable.) */

/*        String                  Accepted by */
/*        ------------------      ------------------ */
/*        0                       UNS, INT, DEC, NUM */
/*        21 */
/*        21994217453648 */

/*        +0                      INT, DEC, NUM */
/*        -13 */
/*        +21946 */

/*        1.23                    DEC, NUM */
/*        12. */
/*        .17 */
/*        +4.1 */
/*        -.25 */

/*        2.3e17                  NUM */
/*        17.D-13275849 */
/*        -.194265E+0004 */

/*     Note that the functions don't take the magnitudes of the numbers */
/*     into account. They may accept numbers that cannot be represented */
/*     in Fortran variables. (For example, '2.19E999999999999' probably */
/*     exceeds the maximum floating point number on any machine, but */
/*     is perfectly acceptable to BENUM.) */

/*     The following strings are not accepted by any of the functions. */

/*        String             Reason */
/*        ---------------    ---------------------------------------- */
/*        3/4                No implied operations (rational numbers) */
/*        37+14              No explicit operations */
/*        E12                Must have mantissa */
/*        217,346.91         No commas */
/*        3.14 159 264       No embedded spaces */
/*        PI                 No special numbers */
/*        FIVE               No textual numbers */
/*        CXIV               No roman numerals */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 24-NOV-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     determine if a string is a decimal number */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     First determine whether or not a decimal point is present. */

    d__ = pos_(string, ".", &c__1, string_len, (ftnlen)1);
    c__ = d__ - 1;
    e = d__ + 1;
    if (d__ == 0) {

/*        If there is no decimal point just apply the integer test. */

	ret_val = beint_(string, string_len);
    } else {

/*        A decimal point is present, get the length of the string */
/*        and see where the decimal point is relative to the last */
/*        character. */

	l = i_len(string, string_len);
	if (l == 1) {

/*           The string is one character long and a decimal point. */
/*           Sorry, this is not a decimal number. */

	    ret_val = FALSE_;
	} else if (d__ == 1) {

/*           The decimal point occurs as the first character of the */
/*           string.  The string following it must begin with */
/*           a non-blank character and be an unsigned integer. */

	    ret_val = *(unsigned char *)&string[e - 1] != ' ' && beuns_(
		    string + (e - 1), string_len - (e - 1));
	} else if (d__ == l) {

/*           The decimal point is the last character of the string. */
/*           The character that precedes it must be non-blank and */
/*           the substring to the left must be an integer. */

	    ret_val = *(unsigned char *)&string[c__ - 1] != ' ' && beint_(
		    string, c__);
	} else if (*(unsigned char *)&string[c__ - 1] == ' ') {

/*           The decimal point occurs somewhere in the middle of the */
/*           string and the character preceding it is blank. */

	    ret_val = *(unsigned char *)&string[e - 1] != ' ' && s_cmp(string,
		     " ", c__, (ftnlen)1) == 0 && beuns_(string + (e - 1), 
		    string_len - (e - 1));
	} else if (*(unsigned char *)&string[e - 1] == ' ') {

/*           Again the decimal point occurs somewhere in the middle of */
/*           the string and the character following it is blank. */

	    ret_val = s_cmp(string + (e - 1), " ", l - (e - 1), (ftnlen)1) == 
		    0 && *(unsigned char *)&string[c__ - 1] != ' ' && beint_(
		    string, c__);
	} else if (*(unsigned char *)&string[c__ - 1] == '-' || *(unsigned 
		char *)&string[c__ - 1] == '+') {

/*           The decimal point is in the middle of the string and */
/*           is preceded by a '+' or '-'.  There should be nothing */
/*           preceding the sign and what follows the decimal point */
/*           should be an unsigned integer. (we already know that the */
/*           character following the decimal point is not a blank) */

	    if (c__ == 1) {
		ret_val = beuns_(string + (e - 1), l - (e - 1));
	    } else {
		ret_val = beuns_(string + (e - 1), l - (e - 1)) && s_cmp(
			string, " ", c__ - 1, (ftnlen)1) == 0;
	    }
	} else {

/*            Last chance, the decimal point is in the middle of the */
/*            string.  The characters to the right and left of the */
/*            point are non-blank and we know the character to the */
/*            left of the point is not a sign.  The string left must */
/*            be an integer, the string to the right must be an */
/*            unsigned integer. */

	    ret_val = beint_(string, c__) && beuns_(string + (e - 1), l - (e 
		    - 1));
	}
    }
    return ret_val;
} /* bedec_ */

