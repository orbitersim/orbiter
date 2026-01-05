/* beint.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure BEINT  ( Be an Integer? ) */
logical beint_(char *string, ftnlen string_len)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer i__, l;
    extern logical beuns_(char *, ftnlen);
    extern integer frstnb_(char *, ftnlen);
    char letter[1];

/* $ Abstract */

/*     Determine whether a string represents an integer. */

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

/*     The function returns .TRUE. if the string represents an integer. */
/*     Otherwise, it returns .FALSE. */

/* $ Detailed_Input */

/*     STRING   is any string. */

/* $ Detailed_Output */

/*     If the input string contains an integer (as defined in */
/*     $Particulars below), the function returns .TRUE. Otherwise, */
/*     the function returns .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     An integer may be either of the following: */

/*        1) An unsigned integer (as defined by function BEUNS). */

/*        2) A sign ('+' or '-') followed by an unsigned */
/*           integer. */

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

/*     determine if a string is an integer */

/* -& */

/*     Find the first non-blank character and the length of the */
/*     string. */

    l = i_len(string, string_len);
    i__ = frstnb_(string, string_len);

/*     If there isn't a non-blank character, this isn't an */
/*     integer. */

    if (i__ == 0) {
	ret_val = FALSE_;
	return ret_val;
    }

/*     Copy the first non-blank letter in the string. */

    *(unsigned char *)letter = *(unsigned char *)&string[i__ - 1];
    if (i__ < l) {

/*        The first character is not the last, so we might start with */
/*        a plus or minus.  If so the rest must be an unsigned integer. */

	if (*(unsigned char *)letter == '+' || *(unsigned char *)letter == 
		'-') {
	    ++i__;
	    if (*(unsigned char *)&string[i__ - 1] != ' ') {
		ret_val = beuns_(string + (i__ - 1), string_len - (i__ - 1));
	    } else {
		ret_val = FALSE_;
	    }
	} else {

/*           If the first character isn't plus (+) or minus (-) */
/*           the string must be an unsigned integer if its going */
/*           to be an integer. */

	    ret_val = beuns_(string + (i__ - 1), string_len - (i__ - 1));
	}
    } else {

/*        If the first (non-blank) character is the last one, then */
/*        it must be an unsigned integer, for the string to */
/*        represent an integer. */

	ret_val = beuns_(letter, (ftnlen)1);
    }
    return ret_val;
} /* beint_ */

