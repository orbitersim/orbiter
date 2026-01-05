/* shiftr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SHIFTR ( Shift right ) */
/* Subroutine */ int shiftr_(char *in, integer *nshift, char *fillc, char *
	out, ftnlen in_len, ftnlen fillc_len, ftnlen out_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__, n, s, nfill, inlen, nsave, outlen;

/* $ Abstract */

/*     Shift the contents of a character string to the right. */
/*     Characters moved past the end of the input string are */
/*     lost. Vacant spaces are filled with a specified character. */

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
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IN         I   Input string. */
/*     NSHIFT     I   Number of times to shift. */
/*     FILLC      I   Character to fill spaces left vacant. */
/*     OUT        O   Shifted string. */

/* $ Detailed_Input */

/*     IN       is the input character string. */

/*     NSHIFT   is the number of times the string is to be */
/*              shifted. If NSHIFT is negative, OUT will be */
/*              identical to IN. */

/*     FILLC    is the character with which spaces left vacant by */
/*              the shift are to be filled. */

/* $ Detailed_Output */

/*     OUT      is the output string. This is the input string, */
/*              shifted N times, filled with FILLC. */

/*              OUT may overwrite IN. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     As a string is shifted left or right, the leftmost or */
/*     rightmost characters of the string disappear (as if pushed */
/*     off the end of the string). This is .TRUE. regardless of */
/*     the length of the output string. */

/*     The remaining characters are shifted simultaneously, and */
/*     the spaces vacated by those characters are filled with a */
/*     replacement character. */

/* $ Examples */

/*     If FILLC = ' ' */

/*        'abcde'   shifted left twice becomes     'cde  ' */
/*        'abcde'   shifted right once becomes     ' abcd' */

/*     If FILLC = '.' */

/*        '12345 '  shifted right once becomes     '.12345' */
/*        'Apple '  shifted left ten times becomes '......' */

/*     Given the declarations */

/*        CHARACTER*3         SHORT */
/*        CHARACTER*10        LONG */

/*     The calls */

/*        CALL SHIFTR ( 'abcde ', 2, '-', SHORT ) */
/*        CALL SHIFTR ( 'abcde ', 2, '-', LONG  ) */

/*     yield the strings */

/*        SHORT = '--a' */
/*        LONG  = '--abcd    ' */

/*     while the calls */

/*        CALL SHIFTL ( 'abcde ', 2, '-', SHORT ) */
/*        CALL SHIFTL ( 'abcde ', 2, '-', LONG  ) */

/*     yield the strings */

/*        SHORT = 'cde' */
/*        LONG  = 'cde ..     ' */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     M.J. Spencer       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.1, 22-AUG-2001 (EDW) */

/*        Corrected ENDDO to END DO. */

/* -    SPICELIB Version 2.0.0, 01-SEP-1994 (MJS) */

/*        This version correctly handles negative shifts. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     shift right */

/* -& */

/*     Local variables */


/*     Get the length of the input, output strings. */

    inlen = i_len(in, in_len);
    outlen = i_len(out, out_len);

/*     If the shift is zero or negative, the string is not changed. */
/*     If longer than the input string, the entire string is shifted. */

    s = max(*nshift,0);
    n = min(inlen,s);

/*     Figure out how many characters in the input string will */
/*     be saved (will not be shifted off the end of the string, */
/*     and will fit in the output string), and how many fill */
/*     characters will be needed (no more than NSHIFT, no fewer */
/*     than zero). */

/* Computing MAX */
    i__1 = 0, i__2 = inlen - outlen;
    nsave = inlen - n - max(i__1,i__2);
    nfill = min(n,outlen);

/*     Move the saved characters to output. */

    for (i__ = nsave; i__ >= 1; --i__) {
	i__1 = i__ + s - 1;
	s_copy(out + i__1, in + (i__ - 1), i__ + s - i__1, (ftnlen)1);
    }

/*     Add as many fill characters as appropriate. */

    i__1 = nfill;
    for (i__ = 1; i__ <= i__1; ++i__) {
	*(unsigned char *)&out[i__ - 1] = *(unsigned char *)fillc;
    }

/*     Pad the output string with blanks (to cover any previous */
/*     ugliness there). */

    if (outlen > inlen) {
	i__1 = inlen;
	s_copy(out + i__1, " ", out_len - i__1, (ftnlen)1);
    }
    return 0;
} /* shiftr_ */

