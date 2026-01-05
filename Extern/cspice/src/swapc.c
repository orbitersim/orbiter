/* swapc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SWAPC ( Swap character values ) */
/* Subroutine */ int swapc_(char *a, char *b, ftnlen a_len, ftnlen b_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer alen, blen;
    char temp[1];
    integer i__, short__;

/* $ Abstract */

/*     Swap the contents of two character strings. */

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

/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     A         I-O  First string. */
/*     B         I-O  Second string. */

/* $ Detailed_Input */

/*     A, */
/*     B        are two character strings, the contents of which */
/*              are to be swapped (exchanged). */

/* $ Detailed_Output */

/*     A, */
/*     B        are the same two character strings, after their */
/*              contents have been exchanged. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is just shorthand notation for the code fragment */

/*           TEMP = A */
/*           A    = B */
/*           B    = TEMP */

/*     The characters in the string are swapped one at a time, so */
/*     no intermediate string (TEMP) is needed. This means that the */
/*     strings may be of any length. */

/* $ Examples */

/*     Let */
/*           A = 11.D0 */
/*           B = 22.D0 */

/*     Then after calling SWAPD (A,B), */

/*           A = 22.D0 */
/*           B = 11.D0 */

/* $ Restrictions */

/*     None. */

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

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     swap character values */

/* -& */

/*     Local variables */


/*     Get the lengths of the strings. */

    alen = i_len(a, a_len);
    blen = i_len(b, b_len);
    short__ = min(alen,blen);

/*     Keep going until the end of the shorter string is reached. */

    i__1 = short__;
    for (i__ = 1; i__ <= i__1; ++i__) {
	*(unsigned char *)temp = *(unsigned char *)&a[i__ - 1];
	*(unsigned char *)&a[i__ - 1] = *(unsigned char *)&b[i__ - 1];
	*(unsigned char *)&b[i__ - 1] = *(unsigned char *)temp;
    }

/*     If either string is longer than the shortest one, pad it */
/*     with blanks. */

    if (alen > short__) {
	i__1 = short__;
	s_copy(a + i__1, " ", a_len - i__1, (ftnlen)1);
    } else if (blen > short__) {
	i__1 = short__;
	s_copy(b + i__1, " ", b_len - i__1, (ftnlen)1);
    }
    return 0;
} /* swapc_ */

