/* prefix.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PREFIX (Prefix a character string) */
/* Subroutine */ int prefix_(char *pref, integer *spaces, char *string, 
	ftnlen pref_len, ftnlen string_len)
{
    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer plen, slen, shift;
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int shiftr_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);

/* $ Abstract */

/*     Add a prefix to a character string. */

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

/*     ASSIGNMENT */
/*     CHARACTER */
/*     STRING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PREF       I   Prefix. */
/*     SPACES     I   Number of spaces separating prefix and suffix. */
/*     STRING    I-O  Suffix on input, string on output. */

/* $ Detailed_Input */

/*     PREF     is the prefix to be added to the string. Trailing */
/*              blanks are ignored. (A blank prefix is interpreted */
/*              as a null prefix.) */

/*     SPACES   is the number of spaces (blanks) in the output */
/*              string separating the last non-blank character */
/*              of the prefix from the first (blank or non-blank) */
/*              character of the suffix. Typically, this will be */
/*              zero or one. If not positive, SPACES defaults to */
/*              zero. */

/*     STRING   on input is the suffix to which the prefix is to */
/*              be added. Leading blanks are significant. */

/* $ Detailed_Output */

/*     STRING   on output is the is the prefixed string. If STRING */
/*              is not large enough to contain the output string, */
/*              the output string is truncated on the right. */

/*              STRING may NOT overwrite PREF. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If SPACES is negative it is treated as zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The suffix is shifted to the right to make room for the prefix */
/*     and required spaces, which are then added to the front of the */
/*     string. (The shift operation handles any necessary truncation.) */

/* $ Examples */

/*     The following examples illustrate the use of PREFIX. */

/*           PREF         STRING (input)   SPACES    STRING (output) */
/*           ----------   --------------   ------    --------------- */
/*           'abc     '   'def    '             0    'abcdef ' */
/*           'abc     '   'def    '             1    'abc def' */
/*           'abc     '   ' def   '             0    'abc def' */
/*           'abc     '   ' def   '             1    'abc  de' */
/*           ' abc    '   'def    '             0    ' abcdef' */
/*           ' abc    '   'def    '             1    ' abc de' */
/*           ' abc    '   ' def   '            -1    ' abc de' */
/*           '        '   'def    '             0    'def    ' */
/*           '        '   'def    '             1    ' def   ' */
/*           ' abc    '   '       '             0    ' abc   ' */

/* $ Restrictions */

/*     1)  PREF and STRING must be distinct. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 18-MAR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     prefix a character_string */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.1.0, 28-FEB-1989 (WLT) */

/*         Reference to SHIFT replaced by SHIFTL. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     L is the location of the last non-blank character in the prefix. */
/*     PLEN is the length of the prefix. Remember that a blank (null) */
/*     prefix has zero length. */

    plen = lastnb_(pref, pref_len);

/*     SLEN is the allocated length of the string. */

    slen = i_len(string, string_len);

/*     We can't just do a concatenation, because the input and output */
/*     strings are of indeterminate length. (This would be a violation */
/*     of the ANSI Fortran 77 standard.) Instead, we will shift the */
/*     suffix to the right in order to make room for the prefix and */
/*     the required number of spaces. If part of the string gets */
/*     truncated, well, that's life. */

    shift = plen + max(*spaces,0);
    shiftr_(string, &shift, " ", string, string_len, (ftnlen)1, string_len);

/*     Put the non-blank part of the prefix in the vacated part of */
/*     the string. The spaces will fill themselves in. */

    if (plen > 0) {
	if (shift < slen) {
	    s_copy(string, pref, shift, pref_len);
	} else {
	    s_copy(string, pref, string_len, pref_len);
	}
    }
    return 0;
} /* prefix_ */

