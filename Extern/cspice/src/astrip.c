/* astrip.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ASTRIP ( STRIP Ascii characters from a string ) */
/* Subroutine */ int astrip_(char *instr, char *asciib, char *asciie, char *
	outstr, ftnlen instr_len, ftnlen asciib_len, ftnlen asciie_len, 
	ftnlen outstr_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer last, i__, j, k;
    extern integer lastnb_(char *, ftnlen);
    integer lwrbnd, uprbnd, outlen;

/* $ Abstract */

/*     Remove from a character string all characters which fall */
/*     between specified starting and ending characters, inclusive. */

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

/*     ASCII */
/*     CHARACTER */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INSTR      I   Input string. */
/*     ASCIIB     I   First ASCII character in range to be stripped. */
/*     ASCIIE     I   Last ASCII character in range to be stripped. */
/*     OUTSTR     O   Output (stripped) string. */

/* $ Detailed_Input */

/*     INSTR    is a character string from which all characters */
/*              between ASCIIB and ASCIIE, inclusive, are to be */
/*              removed. */

/*     ASCIIB   is the first ASCII character in the range of */
/*              characters to be removed from the input string. */
/*              ASCIIB is itself removed from the string, if */
/*              it occurs. */

/*     ASCIIE   is the last ASCII character in the range of */
/*              characters to be removed from the input string. */
/*              ASCIIE is itself removed from the string, if */
/*              it occurs. */

/* $ Detailed_Output */

/*     OUTSTR   is the input string after all the character */
/*              between ASCIIB and ASCIIE, inclusive, have */
/*              been removed. */

/*              If OUTSTR is not large enough to hold the output */
/*              string, it is truncated on the right. */

/*              OUTSTR may overwrite INSTR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     ASTRIP checks each character */
/*     in INSTR to determine if it falls between the characters ASCIIB */
/*     and ASCIIE. If so this character is removed from the string */
/*     (and the string is shortened). Remaining characters are copied */
/*     to the output string. */

/* $ Examples */

/*     The following examples illustrate the use of ASTRIP. */

/*           ASCIIB = 'b' */
/*           ASCIIE = 'k' */
/*           INSTR  = 'Now is the time for all good men to come quick.' */
/*           OUTSTR = 'Now s t tm or all oo mn to om qu.' */

/*           ASCIIB = 'a' */
/*           ASCIIE = 'z' */
/*           INSTR  = 'SELECT column TIME FROM table TEST' */
/*           OUTSTR = 'SELECT TIME FROM TEST' */

/*           ASCIIB = 'a' */
/*           ASCIIE = 'z' */
/*           INSTR  = 'this is going to be an empty string' */
/*           OUTSTR = ' ' */

/*           ASCIIB = '!' */
/*           ASCIIE = '!' */
/*           INSTR  = 'Only 32 more shopping days until Christmas!' */
/*           OUTSTR = 'Only 32 more shopping days until Christmas' */

/*     ASTRIP may also be used to strip ASCII control characters */
/*     (line feeds, tab stops, and so on), as shown in the example */
/*     below. */

/*           ASCIIB = CHAR ( 0  ) */
/*           ASCIIE = CHAR ( 31 ) */
/*           CALL ASTRIP ( STRING, ASCIIB, ASCIIE, STRING ) */

/* $ Restrictions */

/*     1)  If ASCIIB and ASCIIE are not properly ordered (that is, */
/*         if ICHAR(ASCIIB) is not less than or equal to ICHAR(ASCIIE)) */
/*         then ASTRIP will not function as described. (In fact, it will */
/*         copy the input string to the output string without change.) */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     strip ascii characters from a string */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Find the length of the output string. We don't want to */
/*     exceed it. */

    outlen = i_len(outstr, outstr_len);

/*     Find the last non-blank character of the input string. */

    last = lastnb_(instr, instr_len);

/*     Get the numeric representation of ASCIIB and ASCIIE. */

    lwrbnd = *(unsigned char *)asciib;
    uprbnd = *(unsigned char *)asciie;

/*     Step through INSTR (I) a character at a time, transferring */
/*     characters to OUTSTR (J) whenever they fall outside the range */
/*     [ASCIIB, ASCIIE]. */

/*     If the end of OUTSTR is reached, stop transferring characters */
/*     and return. */

    j = 0;
    i__1 = last;
    for (i__ = 1; i__ <= i__1; ++i__) {
	k = *(unsigned char *)&instr[i__ - 1];
	if (k < lwrbnd || k > uprbnd) {

/*           The character is kept.  Note that if the user inputs */
/*           ASCIIB and ASCIIE in the wrong order this test will */
/*           always succeed so that the output string will be */
/*           the same as the input string. */

	    ++j;
	    *(unsigned char *)&outstr[j - 1] = *(unsigned char *)&instr[i__ - 
		    1];
	    if (j == outlen) {
		return 0;
	    }
	}
    }

/*     Pad the output string with blanks. */

    if (j < outlen) {
	i__1 = j;
	s_copy(outstr + i__1, " ", outstr_len - i__1, (ftnlen)1);
    }
    return 0;
} /* astrip_ */

