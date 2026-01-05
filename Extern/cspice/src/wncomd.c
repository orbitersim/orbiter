/* wncomd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure WNCOMD ( Complement a DP window ) */
/* Subroutine */ int wncomd_(doublereal *left, doublereal *right, doublereal *
	window, doublereal *result)
{
    integer card, i__;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int scardd_(integer *, doublereal *), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen),
	     wninsd_(doublereal *, doublereal *, doublereal *);
    extern logical return_(void);

/* $ Abstract */

/*     Determine the complement of a double precision window with */
/*     respect to the interval [LEFT,RIGHT]. */

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
/*     LEFT, */
/*     RIGHT      I   Left, right endpoints of complement interval. */
/*     WINDOW     I   Input window. */
/*     RESULT     O   Complement of WINDOW with respect to [LEFT,RIGHT]. */

/* $ Detailed_Input */

/*     LEFT, */
/*     RIGHT    are the left and right endpoints of the complement */
/*              interval. */

/*     WINDOW   is the window to be complemented. */

/* $ Detailed_Output */

/*     RESULT   is the output window, containing the complement */
/*              of WINDOW with respect to the interval from LEFT */
/*              to RIGHT. If the output window is not large enough */
/*              to contain the result, as many intervals as will */
/*              fit are returned. */

/*              RESULT must be distinct from WINDOW. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If LEFT is greater than RIGHT, the error SPICE(BADENDPOINTS) */
/*         is signaled. */

/*     2)  The cardinality of the input WINDOW must be even. Left */
/*         endpoints of stored intervals must be strictly greater than */
/*         preceding right endpoints. Right endpoints must be greater */
/*         than or equal to corresponding left endpoints. Invalid window */
/*         data are not diagnosed by this routine and may lead to */
/*         unpredictable results. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Mathematically, the complement of a window contains those */
/*     points that are not contained in the window. That is, the */
/*     complement of the set of closed intervals */

/*          [ a(1), b(1) ], [ a(2), b(2) ], ..., [ a(n), b(n) ] */

/*     is the set of open intervals */

/*          ( -inf, a(1) ), ( b(1), a(2) ), ..., ( b(n), +inf ) */

/*     Because Fortran offers no satisfactory representation of */
/*     infinity, we must take the complement with respect to a */
/*     finite interval. */

/*     In addition, Fortran offers no satisfactory floating point */
/*     representation of open intervals. Therefore, the complement */
/*     of a floating point window is closure of the set theoretical */
/*     complement. In short, the floating point complement of the */
/*     window */

/*          [ a(1), b(1) ], [ a(2), b(2) ], ..., [ a(n), b(n) ] */

/*     with respect to the interval from LEFT to RIGHT is the */
/*     intersection of the windows */

/*          ( -inf, a(1) ], [ b(1), a(2) ], ..., [ b(n), +inf ) */

/*     and */

/*          [ LEFT, RIGHT ] */

/*     Note that floating point intervals of measure zero (singleton */
/*     intervals) in the original window are replaced by gaps of */
/*     measure zero, which are filled. Thus, complementing a floating */
/*     point window twice does not necessarily yield the original */
/*     window. */

/* $ Examples */

/*     Let WINDOW contain the intervals */

/*           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ] */

/*     Then the floating point complement of WINDOW with respect */
/*     to [2,20] contains the intervals */

/*           [ 3, 7 ]  [ 11, 20 ] */

/*     and the complement with respect to [ 0, 100 ] contains */

/*           [ 0, 1 ]  [ 3, 7 ]  [ 11, 23 ]  [ 27, 100 ] */

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

/*        Added entry #2 in $Exceptions section. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN) (NJB) */

/* -& */
/* $ Index_Entries */

/*     complement a d.p. window */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Set up the error processing. */

    if (return_()) {
	return 0;
    }
    chkin_("WNCOMD", (ftnlen)6);

/*     Get the cardinality of the input window. */

    card = cardd_(window);

/*     Empty out the result window before proceeding. */

    scardd_(&c__0, result);

/*     Check to see if the input interval is valid. If it is not, signal */
/*     an error and return. */

    if (*left > *right) {
	setmsg_("WNCOMD: Left endpoint may not exceed right endpoint.", (
		ftnlen)52);
	sigerr_("SPICE(BADENDPOINTS)", (ftnlen)19);
	chkout_("WNCOMD", (ftnlen)6);
	return 0;
    }

/*     There are two trivial cases: the window is empty, or it does not */
/*     intersect the input interval. In either case, the complement is */
/*     the entire interval. */

    if (card == 0 || window[6] >= *right || window[card + 5] <= *left) {
	wninsd_(left, right, result);
	chkout_("WNCOMD", (ftnlen)6);
	return 0;
    }

/*     Let WINDOW represent the set of intervals */

/*            [a1,b1], [a2,b2], ..., [aN,bN] */

/*     Then the closure of the complement of WINDOW in the reals is */

/*            (-infinity,a1], [b1,a2], [b2,a3], ..., [bN, infinity) */

/*     Thus the sequence of endpoints of WINDOW is also the sequence */
/*     of finite endpoints of its complement. Moreover, these endpoints */
/*     are simply "shifted" from their original positions in WINDOW. */
/*     This makes finding the complement of WINDOW with respect to */
/*     a given interval almost trivial. */


/*     Find the first right not less than the beginning of the input */
/*     interval. */

    i__ = 2;
    while(i__ <= card && window[i__ + 5] < *left) {
	i__ += 2;
    }

/*     If the beginning of the input interval doesn't split an interval */
/*     in the input window, the complement begins with LEFT. */

    if (i__ <= card && window[i__ + 4] > *left) {
	wninsd_(left, &window[i__ + 4], result);
    }

/*     Start schlepping endpoints [b(i),a(i+1)] from the input window */
/*     to the output window. Stop when we find one of our new right */
/*     endpoints exceeds the end of the input interval. */

    while(! failed_() && i__ < card && window[i__ + 6] < *right) {
	wninsd_(&window[i__ + 5], &window[i__ + 6], result);
	i__ += 2;
    }

/*     If the end of the input interval doesn't split an interval */
/*     in the input window, the complement ends with RIGHT. */

    if (i__ <= card && window[i__ + 5] < *right) {
	wninsd_(&window[i__ + 5], right, result);
    }
    chkout_("WNCOMD", (ftnlen)6);
    return 0;
} /* wncomd_ */

