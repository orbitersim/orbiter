/* remsub.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure REMSUB ( Remove a substring ) */
/* Subroutine */ int remsub_(char *in, integer *left, integer *right, char *
	out, ftnlen in_len, ftnlen out_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__, j, l, r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer inlen;
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    integer outlen;
    extern logical return_(void);

/* $ Abstract */

/*     Remove the substring (LEFT:RIGHT) from a character string. */

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
/*     LEFT       I   Position of first character to be removed. */
/*     RIGHT      I   Position of last character to be removed. */
/*     OUT        O   Output string. */

/* $ Detailed_Input */

/*     IN       is an input character string, from which a substring */
/*              is to be removed. */

/*     LEFT, */
/*     RIGHT    are the ends of the substring to be removed. */

/* $ Detailed_Output */

/*     OUT      is the output string. This is equivalent to the */
/*              string that would be created by the concatenation */

/*                    OUT = IN(1 : LEFT-1) // IN(RIGHT+1 : ) */

/*              If the string is too long to fit into OUT, it is */
/*              truncated on the right. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If LEFT > RIGHT, RIGHT < 1, LEFT < 1, RIGHT > LEN(IN), or */
/*         LEFT > LEN(IN), the error SPICE(INVALIDINDEX) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Move the characters, beginning with RIGHT, one at a time to the */
/*     positions immediately following LEFT. This has the same effect */
/*     as the concatenation */

/*           OUT = IN(1 : LEFT-1) // IN(RIGHT+1 : ) */

/*     Because this operation is not standard for strings of length (*), */
/*     this routine does not use concatenation. */

/* $ Examples */

/*     The following examples illustrate the use of REMSUB. */

/*     IN                 LEFT  RIGHT        OUT */
/*     -----------------  ----  -----        ------------------------ */
/*     'ABCDEFGHIJ'          3      5        'ABFGHIJ' */
/*     'The best rabbit'     5      8        'The  rabbit' */
/*     'The other woman'     1      4        'other woman' */
/*     'An Apple a day'      2      2        'A apple a day' */
/*     'An Apple a day'      5      2         An error is signaled. */
/*     'An Apple a day'      0      0         An error is signaled. */
/*     'An Apple a day'     -3      3         An error is signaled. */

/*     Whenever an error has been signaled, the contents of OUT are */
/*     unpredictable. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     remove a substring */

/* -& */
/* $ Revisions */

/* -     Beta Version 2.0.0, 05-JAN-1989 (HAN) */

/*         Error handling was added to detect invalid character */
/*         positions. If LEFT > RIGHT, RIGHT < 1, LEFT < 1, */
/*         RIGHT > LEN(IN), or LEFT > LEN(IN), an error is signaled. */

/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("REMSUB", (ftnlen)6);
    }

/*     If a character position is out of range, signal an error. */

    if (*left > *right || *right < 1 || *left < 1 || *right > i_len(in, 
	    in_len) || *left > i_len(in, in_len)) {
	setmsg_("Left location was *. Right location was *.", (ftnlen)42);
	errint_("*", left, (ftnlen)1);
	errint_("*", right, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("REMSUB", (ftnlen)6);
	return 0;
    } else {
	l = *left;
	r__ = *right;
    }

/*     How much of the input string will we use? And how big is the */
/*     output string? */

    inlen = lastnb_(in, in_len);
    outlen = i_len(out, out_len);

/*     Copy the first part of the input string. (One character at a */
/*     time, in case this is being done in place.) */

/* Computing MIN */
    i__2 = l - 1;
    i__1 = min(i__2,outlen);
    for (i__ = 1; i__ <= i__1; ++i__) {
	*(unsigned char *)&out[i__ - 1] = *(unsigned char *)&in[i__ - 1];
    }

/*     Now move the rest of the string over. */

    i__ = l;
    j = r__ + 1;
    while(i__ <= outlen && j <= inlen) {
	*(unsigned char *)&out[i__ - 1] = *(unsigned char *)&in[j - 1];
	++i__;
	++j;
    }

/*     Pad with blanks, if necessary. */

    if (i__ <= outlen) {
	s_copy(out + (i__ - 1), " ", out_len - (i__ - 1), (ftnlen)1);
    }
    chkout_("REMSUB", (ftnlen)6);
    return 0;
} /* remsub_ */

