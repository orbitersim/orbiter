/* ljucrs.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LJUCRS ( Left-justify, Uppercase, Compress ) */
/* Subroutine */ int ljucrs_(integer *n, char *input, char *output, ftnlen 
	input_len, ftnlen output_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer lowa, lowz;
    integer i__, j, inlen;
    static integer shift;
    integer count, outlen, ich;

/* $ Abstract */

/*     Left-justify, uppercase, and space-compress a character string. */

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
/*     N          I      Maximum consecutive occurrences of space. */
/*     INPUT      I      Input string. */
/*     OUTPUT     O      Output string. */

/* $ Detailed_Input */

/*     N        is the maximum number of consecutive occurrences */
/*              of space that will be allowed to remain in the */
/*              output string. */

/*     INPUT    is the input string. */

/* $ Detailed_Output */

/*     OUTPUT   is the output string. This is the input string that */
/*              left-justified and with all occurrences of more than */
/*              N consecutive spaces removed. */

/*              If OUTPUT is not large enough to hold the */
/*              compressed string, it is truncated on the right. */

/*              OUTPUT may overwrite INPUT. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The string is left-justified and uppercased. Occurrences of more */
/*     than N consecutive spaces are removed from the input string as it */
/*     is copied to the output string. If the output string is not large */
/*     enough to hold the compressed string, it is truncated on the */
/*     right. */

/* $ Examples */

/*     Let N = 1. Then */

/*         ' Abc  DE F  ',           becomes    'ABC DE F', */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     compress uppercase left-justify a character_string */

/* -& */


/*     Local Variables */


/*     Saved variables */


/*     Initial Data */


/*     Do some set up stuff the first time through so that we do not */
/*     need to reinitialize the boundary values used for comparisons */
/*     and the shift on each call. */

    if (first) {
	first = FALSE_;
	lowa = 'a';
	lowz = 'z';
	shift = 'A' - lowa;
    }

/*     Find out how much space there is in the INPUT and OUTPUT strings */
/*     and initialize the space counter and output place holder. */

    inlen = i_len(input, input_len);
    outlen = i_len(output, output_len);
    count = 0;
    j = 0;
    i__1 = inlen;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Skip leading spaces. */

	if (j == 0 && *(unsigned char *)&input[i__ - 1] == ' ') {

/*           Another leading space. Skip it. */

	} else {

/*           Check this character to see if it is a space or not. */

	    if (*(unsigned char *)&input[i__ - 1] == ' ') {
		++count;

/*              Copy spaces until enough consecutive spaces */
/*              have been accumulated. When enough consecutive spaces */
/*              have accumulated, we no longer copy them. */

		if (count <= *n) {
		    ++j;
		    *(unsigned char *)&output[j - 1] = *(unsigned char *)&
			    input[i__ - 1];
		}
	    } else {

/*              We don't have a space here. Set the space counter to */
/*              zero. */

		count = 0;

/*              Copy this character while swapping lowercase with upper */
/*              case along the way. */

		++j;
		ich = *(unsigned char *)&input[i__ - 1];
		if (ich >= lowa && ich <= lowz) {
		    *(unsigned char *)&output[j - 1] = (char) (ich + shift);
		} else {
		    *(unsigned char *)&output[j - 1] = *(unsigned char *)&
			    input[i__ - 1];
		}
	    }
	    if (j == outlen) {
		return 0;
	    }
	}
    }

/*     Pad any left over space in the output string with blanks. Note */
/*     that if the input string was blank, J will be zero at this */
/*     point and the case below will set the whole output string to */
/*     blank. */

    if (j < outlen) {
	i__1 = j;
	s_copy(output + i__1, " ", output_len - i__1, (ftnlen)1);
    }
    return 0;
} /* ljucrs_ */

