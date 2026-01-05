/* int2hx.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure INT2HX  ( Integer to signed hexadecimal string ) */
/* Subroutine */ int int2hx_(integer *number, char *string, integer *length, 
	ftnlen string_len)
{
    /* Initialized data */

    static char digits[1*16] = "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" 
	    "B" "C" "D" "E" "F";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer begin, itemp, remndr, result;
    char tmpstr[255];

/* $ Abstract */

/*     Convert an integer to an equivalent signed hexadecimal string. */

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
/*     CONVERSION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NUMBER     I   Integer to be converted. */
/*     STRING     O   Equivalent hexadecimal string, left justified. */
/*     LENGTH     O   The length of the hexadecimal string produced. */

/* $ Detailed_Input */

/*     NUMBER   is the integer to be converted. */

/* $ Detailed_Output */

/*     STRING   is the signed hexadecimal string representing the integer */
/*              NUMBER. */

/*              The following table describes the character set used */
/*              to represent the hexadecimal digits and their */
/*              corresponding values. */

/*              Character    Value           Character    Value */
/*              ---------    -----           ---------    ----- */
/*                '0'          0                '8'          8 */
/*                '1'          1                '9'          9 */
/*                '2'          2                'A'         10 */
/*                '3'          3                'B'         11 */
/*                '4'          4                'C'         12 */
/*                '5'          5                'D'         13 */
/*                '6'          6                'E'         14 */
/*                '7'          7                'F'         15 */

/*              In order to obtain the entire signed hexadecimal number, */
/*              the output character string should be at least N */
/*              characters long, where */

/*                              # of bits per integer + 3 */
/*                    N = 1 + ---------------------------- . */
/*                                         4 */

/*              There should be 1 character position for the sign, and */
/*              one character position for each hexadecimal digit that */
/*              could be produced from any integer which can be */
/*              represented by a particular computer system. */

/*              The following table contains minimum output string */
/*              lengths necessary to obtain the complete hexadecimal */
/*              string for various integer sizes. */

/*                 Integer size in bits      Minimum output length */
/*                 --------------------      --------------------- */
/*                 8                         3 */
/*                 16                        5 */
/*                 32                        9 */
/*                 36 (really,it exists)     10 */
/*                 64                        17 */
/*                 etc. */

/*              The hexadecimal character string produced by this */
/*              routine will be left justified and consist of a */
/*              contiguous sequence of hexadecimal digits, or in the */
/*              case of a negative number, a contiguous sequence of */
/*              hexadecimal digits immediately preceded by a minus */
/*              sign, '-', e.g.: */

/*                 (1)   h h ... h */
/*                        1 2     n */

/*                 (2)   -h h ... h */
/*                         1 2     n */

/*              where h  represents an hexadecimal digit. */
/*                     i */

/*              The character string produced will be blank padded on */
/*              the right if LENGTH < LEN( STRING ). */

/*     LENGTH   is the length of the hexadecimal character string */
/*              produced by the conversion. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the output character string is not long enough to */
/*         contain the entire hexadecimal string that was produced, */
/*         the hexadecimal string will be truncated on the right. */

/*     2)  If LEN( STRING ) > LENGTH, the output character string will */
/*         be blank padded on the right. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine will convert a signed integer into an equivalent */
/*     signed hexadecimal character string. This provides a machine */
/*     independent mechanism for storing or porting integer values. */
/*     This routine is used by the routine DP2HX which converts a */
/*     double precision value into an equivalent character string. */

/*     This routine is one of a pair of routines which are used to */
/*     perform conversions between integers and equivalent signed */
/*     hexadecimal character strings: */

/*           INT2HX -- Convert an integer into a signed hexadecimal */
/*                     character string. */

/*           HX2INT -- Convert a signed hexadecimal character string */
/*                     into an integer. */

/* $ Examples */

/*     All of the values shown are for a two's complement representation */
/*     for signed integers. */

/*     The following input and output argument values illustrate the */
/*     action of INT2HX for various input values of NUMBER. */

/*         NUMBER       STRING           LENGTH */
/*         -----------  ---------------  ------ */
/*          1           '1'              1 */
/*         -1           '-1'             2 */
/*          223         'DF'             2 */
/*         -32          '-20'            3 */
/*          0           '0'              1 */

/*          2147483647  '7FFFFFFF'       8 */
/*          (Maximum 32 bit integer) */

/*         -2147483647  '-7FFFFFFF'      9 */
/*          (Minimum 32 bit integer + 1) */

/*         -2147483648  '-80000000'      9 */
/*          (Minimum 32 bit integer) */

/* $ Restrictions */

/*     1)  The maximum number of characters permitted in the output */
/*         string is specified by the local parameter STRLEN. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 22-OCT-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*     convert integer to signed hexadecimal string */

/* -& */

/*     Local Parameters */


/*     Local variables */


/*     Saved variables */


/*     Local variables */


/*     The hexadecimal digits in the integer are found by repeated */
/*     applications of the "modulus" and division operations. We fill */
/*     the string in reverse order so that the digits are in the */
/*     correct order when we have finished building the string. We then */
/*     left justify the resulting string and set the value for its */
/*     length before returning. */

/*     Make a copy of the input so that it will not be changed by this */
/*     routine. */

    itemp = *number;

/*     We need to do different things for the cases where the integer to */
/*     be converted is positive, negative, or zero. ( Actually, the */
/*     positive case and the zero case are the same, but since we can */
/*     test for integer zero exactly it will save a few arithmetic */
/*     operations if we treat it as a special case. ) The case for a */
/*     negative integer is the only one which truly might cause problems, */
/*     because ABS(minimum integer) may equal ABS(maximum integer) + 1, */
/*     on some machines. For example, on many machines with 32 bit */
/*     integers, INTMIN = -2147483648 and INTMAX = 2147483647. */

/*     Set the beginning position of the hexadecimal number to be */
/*     one past the end of the character string that will hold the */
/*     hexadecimal representation of the input number. Before each */
/*     digit of the hexadecimal number is inserted into the character */
/*     string, the beginning position is decremented, so we always know */
/*     exactly where the hexadecimal string begins. This simplifies the */
/*     calculation of the length of the hexadecimal character string at */
/*     the end of the routine. */

    begin = 256;
    if (itemp < 0) {

/*        Collect all of the digits in the string. We know we're done */
/*        when the value of ITEMP is equal to zero, thanks to the fact */
/*        that integer arithmetic operations are exact. */

	while(itemp != 0) {
	    --begin;
	    result = itemp / 16;
	    remndr = (result << 4) - itemp;
	    itemp = result;
	    *(unsigned char *)&tmpstr[begin - 1] = *(unsigned char *)&digits[(
		    i__1 = remndr) < 16 && 0 <= i__1 ? i__1 : s_rnge("digits",
		     i__1, "int2hx_", (ftnlen)309)];
	}

/*        Put the minus sign in place. */

	--begin;
	*(unsigned char *)&tmpstr[begin - 1] = '-';
    } else if (itemp > 0) {

/*        Collect all of the digits in the string. We know we're done */
/*        when the value of ITEMP is equal to zero, thanks to the fact */
/*        that integer arithmetic operations are exact. */

	while(itemp != 0) {
	    --begin;
	    result = itemp / 16;
	    remndr = itemp - (result << 4);
	    itemp = result;
	    *(unsigned char *)&tmpstr[begin - 1] = *(unsigned char *)&digits[(
		    i__1 = remndr) < 16 && 0 <= i__1 ? i__1 : s_rnge("digits",
		     i__1, "int2hx_", (ftnlen)330)];
	}
    } else {

/*        Treat zero as a special case, because it's easier. */

	--begin;
	*(unsigned char *)&tmpstr[begin - 1] = *(unsigned char *)&digits[0];
    }

/*     Set the value of the output string before returning. Let the */
/*     Fortran string assignment deal with the left justification, and */
/*     the truncation on the right if the output string STRING is not */
/*     long enough to contain all of the characters in the string */
/*     that was produced. */

    s_copy(string, tmpstr + (begin - 1), string_len, 255 - (begin - 1));

/*     Also, set the value for the length of the hexadecimal string */
/*     before returning. */

    *length = 255 - begin + 1;
    return 0;
} /* int2hx_ */

