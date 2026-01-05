/* replch.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure REPLCH ( Replace characters in a string ) */
/* Subroutine */ int replch_(char *instr, char *old, char *new__, char *
	outstr, ftnlen instr_len, ftnlen old_len, ftnlen new_len, ftnlen 
	outstr_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Replace all occurrences of a single character with a second */
/*     character. */

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
/*     OLD        I   Character to be replaced. */
/*     NEW        I   Replacement character. */
/*     OUTSTR     O   Output string. */

/* $ Detailed_Input */

/*     INSTR    is the input character string, possibly containing */
/*              one or more occurrences of the character OLD. */

/*     OLD      is the character to be replaced wherever it occurs in */
/*              the input string. */

/*     NEW      is the character which is to replace each occurrence */
/*              of the character OLD in the output string. */

/* $ Detailed_Output */

/*     OUTSTR   is the output string. This is the input string */
/*              with every occurrence of the character OLD replaced */
/*              by the character NEW. */

/*              OUTSTR may overwrite INSTR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Copy the contents of the input string to the output string */
/*     a character at a time, replacing each occurrence of OLD with NEW. */
/*     If the output string is not long enough to contain the input */
/*     string, it is truncated on the right. */

/* $ Examples */

/*     Let */
/*           INSTR  = 'Woodsy is the Anti-Pollution Owl.' */
/*           OLD    = 'O' */
/*           NEW    = 'E' */
/*     then */
/*           OUTSTR = 'Woodsy is the Anti-Pollution Ewl.' */

/*     Note the case-sensitivity of REPLCH. The lowercase o's are */
/*     not affected. */

/*     REPLCH may similarly be used to replace control characters */
/*     (such as tab stops, line feeds, and nulls) with regular ASCII */
/*     characters (such as blanks). */

/* $ Restrictions */

/*     1)  REPLCH is sensitive to case, as shown in the examples above. */

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

/*     replace characters in a string */

/* -& */

/*   Local Variables */


/*     Move the input string to the output string. If it's too long, */
/*     this will truncate it. */

    s_copy(outstr, instr, outstr_len, instr_len);

/*     Check each character of OUTSTR and replace as necessary. */

    i__1 = i_len(outstr, outstr_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (*(unsigned char *)&outstr[i__ - 1] == *(unsigned char *)old) {
	    *(unsigned char *)&outstr[i__ - 1] = *(unsigned char *)new__;
	}
    }
    return 0;
} /* replch_ */

