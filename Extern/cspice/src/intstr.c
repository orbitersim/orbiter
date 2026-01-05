/* intstr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure INTSTR  ( Integer to character string ) */
/* Subroutine */ int intstr_(integer *number, char *string, ftnlen string_len)
{
    /* Initialized data */

    static char digits[1*10] = "0" "1" "2" "3" "4" "5" "6" "7" "8" "9";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, remndr, result, tmpnum;
    char tmpstr[80];

/* $ Abstract */

/*     Convert an integer to an equivalent character string. */

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
/*     STRING     O   Equivalent character string, left justified. */

/* $ Detailed_Input */

/*     NUMBER   is the integer to be converted into a character string. */

/* $ Detailed_Output */

/*     STRING   is the character string representing the integer NUMBER. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the output character string is not large enough to */
/*         contain the entire character string produced, the output */
/*         character string will be truncated on the right. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine will convert a signed integer into an equivalent */
/*     decimal character string representation. The decimal digits of */
/*     the integer NUMBER are found by repeated applications of */
/*     "modulus" and division operations. */

/* $ Examples */

/*     The following argument values illustrate the use of INTSTR. */

/*         NUMBER        STRING */
/*         ------------  --------------------- */
/*          1            '-1' */
/*         -1            '-1' */
/*          223          '223' */
/*         -32           '-32' */
/*          0            '0' */
/*          2147483647   '2147483647'   ( Maximum 32 bit integer ) */
/*         -2147483647   '-2147483647'  ( Minimum 32 bit integer + 1 ) */
/*         -2147483647   '-2147483648'  ( Minimum 32 bit integer ) */

/* $ Restrictions */

/*     1)  This routine assumes that all signed integers will fit into a */
/*         character string with LINLEN or fewer digits. See the */
/*         parameter LINLEN below for the current value. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     H.A. Neilan        (JPL) */
/*     M.J. Spencer       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.2.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.1.0, 11-MAY-1993 (HAN) (MJS) */

/*        DATA statement came before the SAVE statement. This is */
/*        a violation of the ANSI Standard. It is now the other way */
/*        around. */

/* -    SPICELIB Version 2.0.0, 14-OCT-1992 (KRG) */

/*        The routine was rewritten to fix a bug concerning the minimum */
/*        representable integer. */

/*        This routine used to negate a negative number before it began */
/*        generating its digits. This was a bad thing to do, because on */
/*        many machines the minimum representable integer and the */
/*        maximum representable integer have the following relationship: */

/*           ABS( minimum integer ) = 1 + ABS( maximum integer ). */

/*        Changing the sign of a negative number before converting it */
/*        to a character string would cause a program to crash if it */
/*        were attempting to convert the minimum representable integer */
/*        into a character string. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 07-DEC-1990 (WLT) */

/*        References to the old name INT2CH were removed and */
/*        an exception added to that section. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) */

/* -& */
/* $ Index_Entries */

/*     convert integer to character string */

/* -& */

/*     Local Parameters */


/*     Local variables */


/*    Saved values */


/*     The digits are generated in reverse order, so we fill the */
/*     character string in reverse order, from `right' to `left', */
/*     so that the digits are in the correct order when we are */
/*     done converting the integer. This is to avoid reversing the */
/*     character string before returning. The output character */
/*     string is then left justified upon exit. */

/*     Make a copy of the input so that it will not be modified. */

    tmpnum = *number;

/*     Initialize the temporary character buffer used to store the */
/*     character string as it is generated to blanks. */

    s_copy(tmpstr, " ", (ftnlen)80, (ftnlen)1);

/*     We need to do different things for the cases where the number to */
/*     be converted is positive, negative, or zero. ( Actually, the */
/*     positive case and the zero case are the same, but since we can */
/*     test for integer zero exactly it will save a few arithmetic */
/*     operations if we treat it as a special case. ) The case for a */
/*     negative number is the only one which truly might cause problems, */
/*     because ABS(minimum integer) may equal ABS(maximum integer) + 1. */
/*     For 32 bit numbers, INTMIN = -214748368 and INTMAX = 214748367. */
/*     You should be able to see the repercussions of this. */

    i__ = i_len(tmpstr, (ftnlen)80) + 1;
    if (tmpnum < 0) {

/*        Collect all of the digits in the string. */

	while(tmpnum != 0) {
	    --i__;
	    result = tmpnum / 10;
	    remndr = result * 10 - tmpnum;
	    tmpnum = result;
	    *(unsigned char *)&tmpstr[i__ - 1] = *(unsigned char *)&digits[(
		    i__1 = remndr) < 10 && 0 <= i__1 ? i__1 : s_rnge("digits",
		     i__1, "intstr_", (ftnlen)249)];
	}

/*        Put the minus sign in place. */

	--i__;
	*(unsigned char *)&tmpstr[i__ - 1] = '-';
    } else if (tmpnum > 0) {

/*        Collect all of the digits in the string. */

	while(tmpnum != 0) {
	    --i__;
	    result = tmpnum / 10;
	    remndr = tmpnum - result * 10;
	    tmpnum = result;
	    *(unsigned char *)&tmpstr[i__ - 1] = *(unsigned char *)&digits[(
		    i__1 = remndr) < 10 && 0 <= i__1 ? i__1 : s_rnge("digits",
		     i__1, "intstr_", (ftnlen)269)];
	}
    } else {

/*        Treat zero as a special case, because it's easier. */

	--i__;
	*(unsigned char *)&tmpstr[i__ - 1] = *(unsigned char *)&digits[0];
    }

/*     Set the value of the output string before returning. Let the */
/*     Fortran string equals deal with the left justification, and the */
/*     truncation on the right if the string STRING is not long enough */
/*     to contain all of the characters necessary. */

    s_copy(string, tmpstr + (i__ - 1), string_len, i_len(tmpstr, (ftnlen)80) 
	    - (i__ - 1));
    return 0;
} /* intstr_ */

