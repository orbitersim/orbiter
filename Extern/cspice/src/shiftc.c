/* shiftc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SHIFTC ( Shift a character string ) */
/* Subroutine */ int shiftc_(char *in, char *dir, integer *nshift, char *
	fillc, char *out, ftnlen in_len, ftnlen dir_len, ftnlen fillc_len, 
	ftnlen out_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), shiftl_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), shiftr_(char *, integer *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Shift the contents of a character string to the left or right. */
/*     Characters moved past the beginning or end of the string are */
/*     lost. Vacant spaces are filled with a specified character. */

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
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IN         I   Input string. */
/*     DIR        I   Direction to shift. */
/*     NSHIFT     I   Number of times to shift. */
/*     FILLC      I   Character to fill spaces left vacant. */
/*     OUT        O   Shifted string. */

/* $ Detailed_Input */

/*     IN       is the input character string. */

/*     DIR      is the direction in which the characters in the */
/*              string are to be shifted. */

/*                    'L' or 'l'  to shift left. */
/*                    'R' or 'r'  to shift right. */

/*     NSHIFT   is the number of times the string is to be */
/*              shifted. */

/*     FILLC    is the character with which spaces left vacant by */
/*              the shift are to be filled. */

/* $ Detailed_Output */

/*     OUT      is the output string. This is the input string, */
/*              shifted N times, filled with FILLC. */

/*              OUT may overwrite IN. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  A negative shift in one direction is equal to a positive */
/*         shift in the other. */

/*     2)  If a legal direction ('L', 'l', 'R', 'r') is not supplied, */
/*         the error SPICE(ILLEGSHIFTDIR) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The first NSHIFT characters of the output string are filled */
/*     with the fill character, and the input string is appended. */

/* $ Examples */

/*     If FILLC = ' ' */

/*            'abcde'   shifted left twice becomes     'cde  ' */
/*            'abcde'   shifted right once becomes     ' abcd' */

/*     If FILLC = '.' */

/*            '12345 '  shifted right once becomes     '.12345' */
/*            'Apple '  shifted left ten times becomes '......' */

/* $ Restrictions */

/*     1)  SHIFTC is being maintained for historical reasons only. */
/*         To avoid the overhead imposed by the error handling in this */
/*         routine, use the equivalent routines SHIFTL and SHIFTR. */

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

/*     shift a character_string */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 17-OCT-1988 (IMU) */

/*        Dick Simpson reported that the statement */

/*           OUT(N+1: ) = IN */

/*        which began the right-shift section failed on his Data */
/*        General, presumably because it requires temporary buffering */
/*        of characters. The new version seems to work for all cases. */
/*        It has been tested on the VAX and on the Sun (f77 compiler). */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SHIFTC", (ftnlen)6);
    }

/*     Hand off to one of the other routines. */

    if (*(unsigned char *)dir == 'L' || *(unsigned char *)dir == 'l') {
	if (*nshift >= 0) {
	    shiftl_(in, nshift, fillc, out, in_len, (ftnlen)1, out_len);
	} else {
	    i__1 = -(*nshift);
	    shiftr_(in, &i__1, fillc, out, in_len, (ftnlen)1, out_len);
	}
    } else if (*(unsigned char *)dir == 'R' || *(unsigned char *)dir == 'r') {
	if (*nshift >= 0) {
	    shiftr_(in, nshift, fillc, out, in_len, (ftnlen)1, out_len);
	} else {
	    i__1 = -(*nshift);
	    shiftl_(in, &i__1, fillc, out, in_len, (ftnlen)1, out_len);
	}
    } else {
	setmsg_("Shift direction (#) must be L, l, R, or r.", (ftnlen)42);
	errch_("#", dir, (ftnlen)1, (ftnlen)1);
	sigerr_("SPICE(ILLEGSHIFTDIR)", (ftnlen)20);
    }
    chkout_("SHIFTC", (ftnlen)6);
    return 0;
} /* shiftc_ */

