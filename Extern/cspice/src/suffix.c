/* suffix.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SUFFIX (Suffix a character string) */
/* Subroutine */ int suffix_(char *suff, integer *spaces, char *string, 
	ftnlen suff_len, ftnlen string_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer slen, l;
    extern integer lastnb_(char *, ftnlen);
    integer end;

/* $ Abstract */

/*     Add a suffix to a character string. */

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
/*     SUFF       I   Suffix. */
/*     SPACES     I   Number of spaces separating prefix and suffix. */
/*     STRING    I-O  Prefix on input, string on output. */

/* $ Detailed_Input */

/*     SUFF     is the suffix to be added to the string. */
/*              Leading blanks are significant. (A blank */
/*              suffix is interpreted as a null suffix.) */

/*     SPACES   is the number of spaces (blanks) in the output */
/*              string separating the last non-blank character */
/*              of the prefix from the first (blank or non-blank) */
/*              character of the suffix. Typically, this will be */
/*              zero or one. If not positive, SPACES defaults to */
/*              zero. */

/*     STRING   on input is the prefix to which the suffix is */
/*              to be added. Leading blanks are significant. */
/*              Trailing blanks are ignored. */

/* $ Detailed_Output */

/*     STRING   on output is the suffixed string. If STRING */
/*              is not large enough to contain the output string, */
/*              the output string is truncated on the right. */

/*              STRING may NOT overwrite SUFF. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The suffix is added to the right of the last non-blank character */
/*     of the prefix. (Any necessary truncation is done automatically.) */

/* $ Examples */

/*     The following examples illustrate the use of SUFFIX. */

/*           SUFF         STRING (input)   SPACES    STRING (output) */
/*           ----------   --------------   ------    --------------- */
/*           'abc     '   'def    '             0    'defabc ' */
/*           'abc     '   'def    '             1    'def abc' */
/*           'abc     '   ' def   '             0    ' defabc' */
/*           'abc     '   ' def   '             1    ' def ab' */
/*           ' abc    '   'def    '             0    'def abc' */
/*           ' abc    '   'def    '             1    'def  ab' */
/*           ' abc    '   ' def   '            -1    ' def ab' */
/*           '        '   'def    '             0    'def    ' */
/*           '        '   'def    '             1    'def    ' */
/*           ' abc    '   '       '             0    ' abc   ' */
/*           ' abc    '   '       '             1    '  abc  ' */

/* $ Restrictions */

/*     1)  SUFF and STRING must be distinct. */

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

/*     suffix a character_string */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     SLEN is the allocated length of the string. L is the location of */
/*     the last non-blank character of the prefix. */

    slen = i_len(string, string_len);
    l = lastnb_(string, string_len);

/*     Put the suffix at the end of the string. The spaces will fill */
/*     themselves in. */

    end = l + max(*spaces,0);
    if (end < slen) {
	i__1 = end;
	s_copy(string + i__1, suff, string_len - i__1, suff_len);
    }
    return 0;
} /* suffix_ */

