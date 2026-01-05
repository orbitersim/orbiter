/* zzrepsub.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure      ZZREPSUB ( Replace one substring with another ) */
/* Subroutine */ int zzrepsub_(char *in, integer *left, integer *right, char *
	string, char *out, ftnlen in_len, ftnlen string_len, ftnlen out_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer next, i__, inlen;
    extern integer sumai_(integer *, integer *);
    integer remain, myleft, strlen, outlen, myrght, end, use[3];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Replace the substring (LEFT:RIGHT) with a string of any length. */

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
/*     IN         I   Input string. */
/*     LEFT, */
/*     RIGHT      I   Ends of substring to be replaced. */
/*     STRING     I   Replacement string. */
/*     OUT        O   Resulting string. */

/* $ Detailed_Input */

/*     IN         is an arbitrary character string. */

/*     LEFT, */
/*     RIGHT      are the ends of the substring to be replaced. */
/*                Legitimate substrings satisfy the following */
/*                conditions */

/*                    RIGHT > LEFT - 2 */
/*                    LEFT  > 1 */
/*                    RIGHT < LEN(STRING) + 1 */

/*                This allows users to refer to zero-length substrings */
/*                (null substrings) of IN. */

/*     STRING     is the replacement string. Essentially, the */
/*                substring (LEFT:RIGHT) is removed from the */
/*                input string, and STRING is inserted at the */
/*                point of removal. */

/* $ Detailed_Output */

/*     OUT        is the resulting string. OUT may overwrite IN. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If RIGHT is one less than LEFT, the substring to */
/*        replace will be the null substring.  In this case, */
/*        STRING will be inserted between IN(:RIGHT) and IN(LEFT:). */

/*     2) If LEFT is smaller than one, it's treated as 1. */

/*     3) If RIGHT is greater than the length of the input string, */
/*        it is treated as being the length of the string. */

/*     4) If RIGHT is less than LEFT-1, no substitution is made. */

/*     5) Whenever the output string is too small to hold the result, */
/*        the result is truncated on the right. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Ideally, replacement could be done with simple concatenation, */

/*        OUT = IN(1:LEFT-1) // STRING // IN(RIGHT+1: ) */

/*     but the Fortran 77 standard makes this illegal for strings of */
/*     unknown length. */

/*     This private routine is basically just a copy of the SPICE */
/*     routine REPSUB with all error handling removed and "reasonable" */
/*     interpretations used for exceptional cases. */

/* $ Examples */

/*     A typical use for this routine might be to replace all */
/*     occurrences of one word in a string with another word. */
/*     For example, the following code fragment replaces every */
/*     occurrence of the word 'AND' with the word 'OR' in the */
/*     character string LINE. */

/*        LEFT = WDINDX ( LINE, 'AND' ) */

/*        DO WHILE ( LEFT .NE. 0 ) */
/*           CALL REPSUB ( LINE, LEFT, LEFT+2, 'OR', LINE ) */
/*           LEFT = WDINDX ( LINE, 'AND' ) */
/*        END DO */

/*     This routine can also be used to insert substring between */
/*     two characters.  Consider the string: */

/*         IN   = 'The defendant,, was found innocent.' */

/*     to insert ' Emelda Marcos' between the first and second commas */
/*     determine the location of the pair ',,' */

/*        RIGHT = POS ( IN, ',,', 1 ) */
/*        LEFT  = RIGHT + 1 */

/*     then */

/*        CALL REPSUB ( IN, LEFT, RIGHT, ' Emelda Marcos', OUT ) */

/*     The output (OUT) will have the value: */

/*        'The defendant, Emelda Marcos, was found innocent.' */

/* $ Restrictions */

/*     The memory used by STRING and OUT must be disjoint. The memory */
/*     used by IN and OUT must be identical or disjoint. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 27-APR-1996 (WLT) */


/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Get the lengths of all the strings involved in this transaction. */

    inlen = i_len(in, in_len);
    strlen = i_len(string, string_len);
    outlen = i_len(out, out_len);
/* Computing MIN */
    i__1 = inlen + 1, i__2 = max(1,*left);
    myleft = min(i__1,i__2);
/* Computing MIN */
    i__1 = inlen, i__2 = max(0,*right);
    myrght = min(i__1,i__2);

/*     Reject bad inputs. */

    if (myleft < 1) {
	myleft = 1;
    } else if (myrght > inlen) {
	myrght = inlen;
    } else if (*right < *left - 1) {
	return 0;
    }

/*     Consider three separate sections: */

/*        1) The front of the original string. */

/*        2) The replacement string. */

/*        3) The end of the original string. */

/*     Determine how much of each section to use in the output string. */
/*     REMAIN is the number of characters that will fit in the output */
/*     string. */

    remain = outlen;
/* Computing MIN */
    i__1 = remain, i__2 = myleft - 1;
    use[0] = min(i__1,i__2);
    remain -= use[0];
    use[1] = min(remain,strlen);
    remain -= use[1];
/* Computing MIN */
    i__1 = remain, i__2 = inlen - *right;
    use[2] = min(i__1,i__2);

/*     Move the third section first. It gets moved back to front */
/*     or front to back, depending on whether the replacement string */
/*     is longer than the original substring. The main thing is to */
/*     avoid overwriting characters that have yet to be moved. */

    end = sumai_(use, &c__3);
    if (myleft + strlen > *right) {
	next = end;
	for (i__ = use[2]; i__ >= 1; --i__) {
	    i__1 = *right + i__ - 1;
	    s_copy(out + (next - 1), in + i__1, (ftnlen)1, *right + i__ - 
		    i__1);
	    --next;
	}
    } else {
	next = myleft + strlen;
	i__1 = use[2];
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i__2 = *right + i__ - 1;
	    s_copy(out + (next - 1), in + i__2, (ftnlen)1, *right + i__ - 
		    i__2);
	    ++next;
	}
    }

/*     The first two sections can be moved directly to the front of */
/*     the output string. */

    next = 1;
    i__1 = use[0];
    for (i__ = 1; i__ <= i__1; ++i__) {
	*(unsigned char *)&out[next - 1] = *(unsigned char *)&in[i__ - 1];
	++next;
    }
    i__1 = use[1];
    for (i__ = 1; i__ <= i__1; ++i__) {
	*(unsigned char *)&out[next - 1] = *(unsigned char *)&string[i__ - 1];
	++next;
    }

/*     Pad with blanks, if the output string was not filled. */

    if (end < outlen) {
	i__1 = end;
	s_copy(out + i__1, " ", out_len - i__1, (ftnlen)1);
    }
    return 0;
} /* zzrepsub_ */

