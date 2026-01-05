/* benum.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure BENUM  ( Be a number? ) */
logical benum_(char *string, ftnlen string_len)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    extern logical bedec_(char *, ftnlen);
    integer d__, e, f, l;
    extern logical beint_(char *, ftnlen);

/* $ Abstract */

/*     Determine whether a string represents a number. */

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

/*     The function returns .TRUE. if the string is a number. */
/*     Otherwise, it returns .FALSE. */

/* $ Detailed_Input */

/*     STRING   is any string. */

/* $ Detailed_Output */

/*     If the input string contains a number (as defined in */
/*     $Particulars below) the function returns .TRUE. Otherwise, */
/*     the function returns .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     A number may be either of the following: */

/*        1) A decimal number (as defined by function BEDEC). */

/*        2) A decimal number followed by an exponent character */
/*           ('E', 'e', 'D', or 'd') and an integer (as defined */
/*           by function BEINT). */

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

/*     determine if a string is a number */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Determine whether or not there is an exponent character in the */
/*     string. */

    l = i_len(string, string_len);
    e = cpos_(string, "EeDd", &c__1, string_len, (ftnlen)4);
    d__ = e - 1;
    f = e + 1;
    if (e == 0) {

/*        There is no exponent character, this is a number if it */
/*        is a decimal number. */

	ret_val = bedec_(string, string_len);
    } else if (e == 1 || e == l) {
	ret_val = FALSE_;
    } else if (*(unsigned char *)&string[d__ - 1] == ' ' || *(unsigned char *)
	    &string[f - 1] == ' ') {
	ret_val = FALSE_;
    } else {
	ret_val = bedec_(string, d__) && beint_(string + (f - 1), l - (f - 1))
		;
    }
    return ret_val;
} /* benum_ */

