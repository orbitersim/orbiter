/* lastpc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LASTPC ( Last printable character ) */
integer lastpc_(char *string, ftnlen string_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Return the index of the last printable character in a character */
/*     string. ASCII characters 33-126 are printable. (Blanks are not */
/*     considered printable.) */

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
/*     SEARCH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   Input character string. */

/*     The function returns the index of the last non-blank printable */
/*     character in STRING. */

/* $ Detailed_Input */

/*     STRING   is the input character string. */

/* $ Detailed_Output */

/*     The function returns the index of the last printable character in */
/*     the input string. */

/*     ASCII characters 33-126 are considered to be printable characters. */
/*     Blanks are not considered printable characters. If the input */
/*     string contains no printable characters, LASTPC is zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This works exactly like LASTNB, except that it skips non-printable */
/*     characters (ASCII control characters) as well as blanks. */

/* $ Examples */

/*     The program */

/*        INTEGER         FRSTNB */
/*        INTEGER         FRSTPC */
/*        INTEGER         LASTNB */
/*        INTEGER         LASTPC */

/*        CHARACTER*10    S */

/*        S( 1: 1) = ' ' */
/*        S( 2: 2) = CHAR (  2 ) */
/*        S( 3: 3) = CHAR (  3 ) */
/*        S( 4: 4) = 'A' */
/*        S( 5: 5) = 'B' */
/*        S( 6: 6) = 'C' */
/*        S( 7: 7) = CHAR (  7 ) */
/*        S( 8: 8) = CHAR (  8 ) */
/*        S( 9: 9) = CHAR (  9 ) */
/*        S(10:10) = ' ' */

/*        WRITE (*,*) 'Non-blank from ', FRSTNB(S), ' to ', LASTNB(S) */
/*        WRITE (*,*) 'Printable from ', FRSTPC(S), ' to ', LASTPC(S) */

/*        END */

/*     produces te following output: */

/*        Non-blank from 2 to 9. */
/*        Printable from 4 to 6. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 08-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     last printable character */

/* -& */

/*     Local variables */


/*     Look for the last character in the range [33,126], and return */
/*     its index. */

    for (i__ = i_len(string, string_len); i__ >= 1; --i__) {
	if (*(unsigned char *)&string[i__ - 1] >= 33 && *(unsigned char *)&
		string[i__ - 1] <= 126) {
	    ret_val = i__;
	    return ret_val;
	}
    }

/*     Still here? No printable characters. Return zero. */

    ret_val = 0;
    return ret_val;
} /* lastpc_ */

