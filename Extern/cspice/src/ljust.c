/* ljust.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LJUST ( Left justify a character string ) */
/* Subroutine */ int ljust_(char *input, char *output, ftnlen input_len, 
	ftnlen output_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer i__, j, li, lo, pos;

/* $ Abstract */

/*     Left-justify a character string. */

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
/*     OUTPUT     O   Output character string, left justified. */

/* $ Detailed_Input */

/*     INPUT    is the input character string. */

/* $ Detailed_Output */

/*     OUTPUT   is the output character string, left justified. */

/*              OUTPUT may overwrite INPUT. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Leading blanks are removed from the input character string. */
/*     If the output string is not large enough to hold the left */
/*     justified string, it is truncated on the right. */

/* $ Examples */

/*     The following examples illustrate the use of LJUST. */

/*            'ABCDE'             becomes   'ABCDE' */
/*            'AN EXAMPLE'                  'AN EXAMPLE' */
/*            '   AN EXAMPLE  '             'AN EXAMPLE' */
/*            '               '             ' ' */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 27-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 29-JUL-2013 (BVS) */

/*        Added the quick return branch for input strings that are */
/*        already left-justified. Removed the initial check for blank */
/*        input and changed logic to return an empty string after */
/*        scanning the input. Re-ordered header sections. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     left justify a character_string */

/* -& */

/*     Local variables */


/*     Is the first character of the input string non-blank? If yes, the */
/*     input string is already left-justified. There is nothing to do */
/*     but to assign the input string to the output string. */

    if (*(unsigned char *)input != ' ') {
	s_copy(output, input, output_len, input_len);
    } else {

/*        Get the first non-blank character. Start OUTPUT at that point. */

	li = i_len(input, input_len);
	lo = i_len(output, output_len);
	j = 1;

/*        Set I equal to position of first non-blank character of INPUT. */

	i__ = 0;
	pos = 1;
	while(i__ == 0 && pos <= li) {
	    if (*(unsigned char *)&input[pos - 1] != ' ') {
		i__ = pos;
	    } else {
		++pos;
	    }
	}

/*        Did we find a non-blank character? If not, the input string is */
/*        blank. Set output to blank. */

	if (i__ == 0) {
	    s_copy(output, " ", output_len, (ftnlen)1);
	} else {

/*           I is now the index of the first non-blank character of */
/*           INPUT. */

	    while(i__ <= li && j <= lo) {
		*(unsigned char *)&output[j - 1] = *(unsigned char *)&input[
			i__ - 1];
		++j;
		++i__;
	    }
	    if (j <= lo) {
		s_copy(output + (j - 1), " ", output_len - (j - 1), (ftnlen)1)
			;
	    }
	}
    }
    return 0;
} /* ljust_ */

