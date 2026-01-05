/* rjust.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure RJUST ( Right justify a character string ) */
/* Subroutine */ int rjust_(char *input, char *output, ftnlen input_len, 
	ftnlen output_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer last, i__, first, start;
    extern integer lastnb_(char *, ftnlen), frstnb_(char *, ftnlen);
    integer loc;

/* $ Abstract */

/*     Right justify a character string. */

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
/*     STRING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INPUT      I   Input character string. */
/*     OUTPUT     O   Output character string, right justified. */

/* $ Detailed_Input */

/*     INPUT    is the input character string. */

/* $ Detailed_Output */

/*     OUTPUT   is the output character string, right justified. */
/*              If INPUT is too large to fit into OUTPUT, it is */
/*              truncated on the left. */

/*              OUTPUT may overwrite INPUT. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Any trailing blanks in the input string are removed, and */
/*     the remaining string is copied to the output string. */

/* $ Examples */

/*     The following examples should illustrate the use of RJUST. */

/*        'ABCDE          '   becomes  '          ABCDE' */
/*        'AN EXAMPLE     '            '     AN EXAMPLE' */
/*        '   AN EXAMPLE  '            '     AN EXAMPLE' */
/*        '               '            '               ' */

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

/*     right justify a character_string */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.1.0, 11-DEC-1989 (IMU) */

/*         Did not work on Sun when INPUT and OUTPUT were */
/*         the same string, and where the initial and final */
/*         locations of the non-blank part of the string */
/*         overlapped. */

/*         The solution is to move the characters one by one, */
/*         starting from the right side of the input string. */
/*         That way, nothing gets clobbered. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Blank string? It's all the same. */

    if (s_cmp(input, " ", input_len, (ftnlen)1) == 0) {
	s_copy(output, input, output_len, input_len);

/*     Get the first non-blank character. Start OUTPUT at that point. */

    } else {
	first = frstnb_(input, input_len);
	last = lastnb_(input, input_len);
	start = i_len(output, output_len) - (last - first);

/*        If the input string is too long (START < 1), move FIRST */
/*        up a little to truncate on the left. */

	if (start < 1) {
	    first += 1 - start;
	    start = 1;
	}

/*        Move the characters in reverse order, to keep from stomping */
/*        anything if the operation is being done in place. */

	loc = i_len(output, output_len);
	i__1 = first;
	for (i__ = last; i__ >= i__1; --i__) {
	    *(unsigned char *)&output[loc - 1] = *(unsigned char *)&input[i__ 
		    - 1];
	    --loc;
	}

/*        Clear the first part of OUTPUT, if necessary. */

	if (start > 1) {
	    s_copy(output, " ", start - 1, (ftnlen)1);
	}
    }
    return 0;
} /* rjust_ */

