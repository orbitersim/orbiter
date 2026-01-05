/* repsub.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure REPSUB ( Replace one substring with another ) */
/* Subroutine */ int repsub_(char *in, integer *left, integer *right, char *
	string, char *out, ftnlen in_len, ftnlen string_len, ftnlen out_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer next, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer inlen;
    extern integer sumai_(integer *, integer *);
    integer remain;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    integer strlen, outlen;
    extern logical return_(void);
    integer end, use[3];

/* $ Abstract */

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

/*     IN       is an arbitrary character string. */

/*     LEFT, */
/*     RIGHT    are the ends of the substring to be replaced. */
/*              Legitimate substrings satisfy the following */
/*              conditions */

/*                  RIGHT > LEFT - 2 */
/*                  LEFT  > 1 */
/*                  RIGHT < LEN(STRING) + 1 */

/*              This allows users to refer to zero-length substrings */
/*              (null substrings) of IN. */

/*     STRING   is the replacement string. Essentially, the */
/*              substring (LEFT:RIGHT) is removed from the */
/*              input string, and STRING is inserted at the */
/*              point of removal. */

/* $ Detailed_Output */

/*     OUT      is the resulting string. OUT may overwrite IN. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If RIGHT is one less than LEFT, the substring to */
/*         replace will be the null substring. In this case, */
/*         STRING will be inserted between IN(:RIGHT) and IN(LEFT:). */

/*     2)  If LEFT is smaller than one, the error SPICE(BEFOREBEGSTR) */
/*         is signaled. */

/*     3)  If RIGHT is greater than the length of the input string, */
/*         the error SPICE(PASTENDSTR) is signaled. */

/*     4)  If RIGHT is less than LEFT-1, the error SPICE(BADSUBSTR) */
/*         is signaled. */

/*     5)  Whenever the output string is too small to hold the result, */
/*         the result is truncated on the right. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Ideally, replacement could be done with simple concatenation, */

/*        OUT = IN(1:LEFT-1) // STRING // IN(RIGHT+1: ) */

/*     but the Fortran 77 standard makes this illegal for strings of */
/*     unknown length. */

/* $ Examples */

/*     A typical use for this routine might be to replace all */
/*     occurrences of one word in a string with another word. */
/*     For example, the following code fragment replaces every */
/*     occurrence of the word 'AND' with the word 'OR' in the */
/*     character string LINE. */

/*        LEFT = WDINDX ( LINE, 'AND' ) */

/*        DO WHILE ( LEFT .NE. 0 ) */
/*           CALL   REPSUB ( LINE, LEFT, LEFT+2, 'OR', LINE ) */
/*           LEFT = WDINDX ( LINE, 'AND' ) */
/*        END DO */

/*     This routine can also be used to insert substring between */
/*     two characters. Consider the string: */

/*         IN   = 'The defendant,, was found innocent.' */

/*     to insert ' Imelda Marcos' between the first and second commas */
/*     determine the location of the pair ',,' */

/*        RIGHT = POS ( IN, ',,', 1 ) */
/*        LEFT  = RIGHT + 1 */

/*     then */

/*        CALL REPSUB ( IN, LEFT, RIGHT, ' Imelda Marcos', OUT ) */

/*     The output (OUT) will have the value: */

/*        'The defendant, Imelda Marcos, was found innocent.' */

/* $ Restrictions */

/*     1)  The memory used by STRING and OUT must be disjoint. The memory */
/*         used by IN and OUT must be identical or disjoint. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 17-JUN-1999 (WLT) */

/*        Fixed example code fragment. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 24-AUG-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     replace one substring with another substring */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("REPSUB", (ftnlen)6);
    }

/*     Get the lengths of all the strings involved in this transaction. */

    inlen = i_len(in, in_len);
    strlen = i_len(string, string_len);
    outlen = i_len(out, out_len);

/*     Reject bad inputs. */

    if (*left < 1) {
	setmsg_("REPSUB error: LEFT (#) must not be less than 1.", (ftnlen)47)
		;
	errint_("#", left, (ftnlen)1);
	sigerr_("SPICE(BEFOREBEGSTR)", (ftnlen)19);
	chkout_("REPSUB", (ftnlen)6);
	return 0;
    } else if (*right > inlen) {
	setmsg_("REPSUB error: RIGHT (#) must not exceed length of IN (#).", (
		ftnlen)57);
	errint_("#", right, (ftnlen)1);
	errint_("#", &inlen, (ftnlen)1);
	sigerr_("SPICE(PASTENDSTR)", (ftnlen)17);
	chkout_("REPSUB", (ftnlen)6);
	return 0;
    } else if (*right < *left - 1) {
	setmsg_("REPSUB error: LEFT (#) must not exceed RIGHT+1 (# + 1). ", (
		ftnlen)56);
	errint_("#", left, (ftnlen)1);
	errint_("#", right, (ftnlen)1);
	sigerr_("SPICE(BADSUBSTR)", (ftnlen)16);
	chkout_("REPSUB", (ftnlen)6);
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
    i__1 = remain, i__2 = *left - 1;
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
    if (*left + strlen > *right) {
	next = end;
	for (i__ = use[2]; i__ >= 1; --i__) {
	    i__1 = *right + i__ - 1;
	    s_copy(out + (next - 1), in + i__1, (ftnlen)1, *right + i__ - 
		    i__1);
	    --next;
	}
    } else {
	next = *left + strlen;
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
    chkout_("REPSUB", (ftnlen)6);
    return 0;
} /* repsub_ */

