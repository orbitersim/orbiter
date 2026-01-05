/* wnexpd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure WNEXPD ( Expand the intervals of a DP window ) */
/* Subroutine */ int wnexpd_(doublereal *left, doublereal *right, doublereal *
	window)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer card, gone, i__, j;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), scardd_(integer *, 
	    doublereal *), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Expand each of the intervals of a double precision window. */

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

/*     WINDOWS */

/* $ Keywords */

/*     WINDOWS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LEFT       I   Amount subtracted from each left endpoint. */
/*     RIGHT      I   Amount added to each right endpoint. */
/*     WINDOW    I-O  Window to be expanded. */

/* $ Detailed_Input */

/*     LEFT     is the amount to be subtracted from the left endpoint of */
/*              each interval in the input window. The amount LEFT is */
/*              signed. */

/*     RIGHT    is the amount to be added to the right endpoint of each */
/*              interval in the window. The amount RIGHT is signed. */

/*     WINDOW   on input, is a window containing zero or more */
/*              intervals. */

/* $ Detailed_Output */

/*     WINDOW   on output, is the original window with each of its */
/*              intervals expanded by LEFT units on the left and */
/*              RIGHT units on the right. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  The cardinality of the input WINDOW must be even. Left */
/*         endpoints of stored intervals must be strictly greater than */
/*         preceding right endpoints. Right endpoints must be greater */
/*         than or equal to corresponding left endpoints. Invalid window */
/*         data are not diagnosed by this routine and may lead to */
/*         unpredictable results. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine expands (lengthens) each of the intervals in */
/*     the input window. The adjustments are not necessarily symmetric. */
/*     That is, LEFT units are subtracted from the left endpoint of */
/*     each interval, and RIGHT units are added to the right endpoint */
/*     of each interval, where LEFT and RIGHT may be different. */

/*     Intervals are merged when expansion causes them to overlap. */

/* $ Examples */

/*     Let WINDOW contain the intervals */

/*           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ] */

/*     Then the following series of calls */

/*           CALL WNEXPD (  2,  1, WINDOW )              (1) */
/*           CALL WNEXPD ( -2,  2, WINDOW )              (2) */
/*           CALL WNEXPD ( -2, -1, WINDOW )              (3) */

/*     produces the following series of windows */

/*           [ -1, 4 ]  [ 5, 12 ]  [ 21, 30 ]            (1) */
/*           [  1, 6 ]  [ 7, 14 ]  [ 23, 32 ]            (2) */
/*           [  3, 5 ]  [ 9, 13 ]  [ 25, 31 ]            (3) */

/*     Note that intervals may be "expanded" by negative amounts. */
/*     In the example above, the second call shifts each interval to */
/*     the right, while the third call undoes the effect of the first */
/*     call (without restoring the merged intervals). */

/*     Note also that the third call is exactly equivalent to the */
/*     call */

/*           CALL WNCOND ( 2, 1, WINDOW ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added entry #1 in $Exceptions section. Extended LEFT and RIGHT */
/*        description in $Detailed_Input. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     expand the intervals of a d.p. window */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("WNEXPD", (ftnlen)6);
    }

/*     Get the cardinality of the window. (The size is not important; */
/*     this routine can't create any new intervals.) */

    card = cardd_(window);

/*     Expand the intervals individually. We'll take care of */
/*     overlaps later on. Negative expansion may cause some */
/*     intervals to disappear. */

    gone = 0;
    i__1 = card;
    for (i__ = 1; i__ <= i__1; i__ += 2) {
	window[i__ - gone + 5] = window[i__ + 5] - *left;
	window[i__ - gone + 6] = window[i__ + 6] + *right;
	if (window[i__ - gone + 5] > window[i__ - gone + 6]) {
	    gone += 2;
	}
    }

/*     Proceed only if at least one interval remains. (If there were */
/*     no intervals to begin with, we skip the previous loop and come */
/*     here without delay. Do not pass GO, do not collect $200.) */

    card -= gone;
    if (card == 0) {
	scardd_(&c__0, window);
	chkout_("WNEXPD", (ftnlen)6);
	return 0;
    }

/*     None of the intervals can have extended to completely contain */
/*     any of the other intervals. (They were all expanded by the */
/*     same amount. Convince yourself that this is true.) So the first */
/*     endpoint is still the first endpoint (so to speak). */

/*     Step through the window, looking for the next right endpoint */
/*     less than the following left endpoint. This marks the end of */
/*     the new first interval, and the beginning of the new second */
/*     interval. Keep this up until the last right endpoint has been */
/*     reached. This remains the last right endpoint. */

    i__ = 2;
    j = 2;
    while(j < card) {
	if (window[j + 5] < window[j + 6]) {
	    window[i__ + 5] = window[j + 5];
	    window[i__ + 6] = window[j + 6];
	    i__ += 2;
	}
	j += 2;
    }
    window[i__ + 5] = window[j + 5];
    scardd_(&i__, window);
    chkout_("WNEXPD", (ftnlen)6);
    return 0;
} /* wnexpd_ */

