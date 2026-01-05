/* quote.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure QUOTE ( Enclose in quotes ) */
/* Subroutine */ int quote_(char *in, char *left, char *right, char *out, 
	ftnlen in_len, ftnlen left_len, ftnlen right_len, ftnlen out_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    extern integer frstnb_(char *, ftnlen);
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);

/* $ Abstract */

/*     Enclose (quote) the non-blank part of a character string */
/*     between delimiting symbols. */

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
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IN         I   Input string. */
/*     LEFT       I   Left delimiter. */
/*     RIGHT      I   Right delimiter. */
/*     OUT        O   Output (quoted) string. */

/* $ Detailed_Input */

/*     IN       is the input string to be quoted. */

/*     LEFT, */
/*     RIGHT    are the left and right delimiters to be used in */
/*              quoting the input string. These may be the same */
/*              character (apostrophe, vertical bar), complementary */
/*              characters (left and right parentheses, brackets, */
/*              or braces), or two totally unrelated characters. */

/* $ Detailed_Output */

/*     OUT      is the output string. This is the non-blank part */
/*              of the input string delimited by LEFT and RIGHT. */
/*              If the output string is not large enough to contain */
/*              the quoted string, it is truncated on the right. */
/*              (The right delimiter would be lost in this case.) */

/*              If the input string is blank, the output string is */
/*              a single quoted blank. */

/*              OUT may overwrite IN. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The first character of the output string is the left delimiter, */
/*     LEFT. This is followed immediately by the non-blank part of the */
/*     input string, which is in turn followed by the right delimiter, */
/*     RIGHT. */

/*     If the input string is blank (has no non-blank characters), */
/*     a single quoted blank is returned. */

/* $ Examples */

/*     Let */
/*           IN    = '    This string has leading and trailing blanks  ' */
/*           LEFT  = '(' */
/*           RIGHT = ')' */

/*     Then */
/*           OUT   = '(This string has leading and trailing blanks)    ' */

/*     Or, let IN = '         '. Then OUT = '( )'. */

/* $ Restrictions */

/*     None. */

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

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     enclose in quotes */

/* -& */

/*     SPICELIB functions */


/*     Check for blank string first. */

    if (s_cmp(in, " ", in_len, (ftnlen)1) == 0) {
	s_copy(out, left, out_len, (ftnlen)1);
	suffix_(right, &c__1, out, (ftnlen)1, out_len);
    } else {
	i__1 = frstnb_(in, in_len) - 1;
	s_copy(out, in + i__1, out_len, lastnb_(in, in_len) - i__1);
	prefix_(left, &c__0, out, (ftnlen)1, out_len);
	suffix_(right, &c__0, out, (ftnlen)1, out_len);
    }
    return 0;
} /* quote_ */

