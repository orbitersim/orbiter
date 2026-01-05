/* wncond.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure WNCOND ( Contract the intervals of a DP window ) */
/* Subroutine */ int wncond_(doublereal *left, doublereal *right, doublereal *
	window)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), chkout_(char *, 
	    ftnlen), wnexpd_(doublereal *, doublereal *, doublereal *);
    extern logical return_(void);

/* $ Abstract */

/*     Contract each of the intervals of a double precision window. */

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
/*     LEFT       I   Amount added to each left endpoint. */
/*     RIGHT      I   Amount subtracted from each right endpoint. */
/*     WINDOW    I-O  Window to be contracted. */

/* $ Detailed_Input */

/*     LEFT     is the amount to be added to the left endpoint of each */
/*              interval in the input window. The amount LEFT is signed. */

/*     RIGHT    is the amount to be subtracted from the right endpoint of */
/*              each interval in the window. The amount RIGHT is signed. */

/*     WINDOW   on input, is a window containing zero or more */
/*              intervals. */

/* $ Detailed_Output */

/*     WINDOW   on output, is the original window with each of its */
/*              intervals contracted by LEFT units on the left and */
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

/*     This routine contracts (shortens) each of the intervals in */
/*     the input window. The adjustments are not necessarily symmetric. */
/*     That is, LEFT units are added to the left endpoint of each */
/*     interval, and RIGHT units are subtracted from the right endpoint */
/*     of each interval, where LEFT and RIGHT may be different. */

/*     Intervals are dropped when they are contracted by amounts */
/*     greater than their measures. */

/* $ Examples */

/*     Let WINDOW contain the intervals */

/*           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ] */

/*     Then the following series of calls */

/*           CALL WNCOND (  2,  1, WINDOW )              (1) */
/*           CALL WNCOND ( -2,  2, WINDOW )              (2) */
/*           CALL WNCOND ( -2, -1, WINDOW )              (3) */

/*     produces the following series of windows */

/*           [ 9, 10 ]  [ 25, 26 ]                       (1) */
/*           [ 7,  8 ]  [ 23, 24 ]                       (2) */
/*           [ 5,  9 ]  [ 21, 25 ]                       (3) */

/*     Note that intervals may be "contracted" by negative amounts. */
/*     In the example above, the second call shifts each interval to */
/*     the left, while the third call undoes the effect of the first */
/*     call (without restoring the destroyed intervals). */

/*     Note also that the third call is exactly equivalent to the */
/*     call */

/*           CALL WNEXPD ( 2, 1, WINDOW ) */

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

/*     contract the intervals of a d.p. window */

/* -& */

/*     Spicelib functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("WNCOND", (ftnlen)6);
    }

/*     This is just negative expansion. */

    d__1 = -(*left);
    d__2 = -(*right);
    wnexpd_(&d__1, &d__2, window);
    chkout_("WNCOND", (ftnlen)6);
    return 0;
} /* wncond_ */

