/* wndifd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure WNDIFD ( Difference two DP windows ) */
/* Subroutine */ int wndifd_(doublereal *a, doublereal *b, doublereal *c__)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    logical keep;
    integer over;
    doublereal f;
    integer acard, bcard;
    doublereal l;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer csize;
    extern integer sized_(doublereal *);
    extern /* Subroutine */ int copyd_(doublereal *, doublereal *);
    integer needed;
    extern /* Subroutine */ int scardd_(integer *, doublereal *), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), ssized_(integer *, 
	    doublereal *), setmsg_(char *, ftnlen), errint_(char *, integer *,
	     ftnlen);
    extern logical return_(void);
    logical unrslv;
    integer apb, bpb, ape, bpe, put;

/* $ Abstract */

/*     Place the difference of two double precision windows into */
/*     a third window. */

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
/*     A, */
/*     B          I   Input windows. */
/*     C          O   Difference of A and B. */

/* $ Detailed_Input */

/*     A, */
/*     B        are SPICE windows, each of which contains zero or more */
/*              intervals. */

/* $ Detailed_Output */

/*     C        is the output SPICE window, containing the difference */
/*              of A and B --- every point contained in A, but not */
/*              contained in B. */

/*              C must be distinct from both A and B. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the difference of the two windows results in an excess of */
/*         elements, the error SPICE(WINDOWEXCESS) is signaled. */

/*     2)  The cardinality of the input windows must be even. Left */
/*         endpoints of stored intervals must be strictly greater than */
/*         preceding right endpoints. Right endpoints must be greater */
/*         than or equal to corresponding left endpoints. Invalid window */
/*         data are not diagnosed by this routine and may lead to */
/*         unpredictable results. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Mathematically, the difference of two windows contains every */
/*     point contained in the first window but not contained in the */
/*     second window. */

/*     Fortran offers no satisfactory floating point representation */
/*     of open intervals. Thus, for floating point windows we must */
/*     return the closure of the set theoretical difference: that is, */
/*     the difference plus the endpoints of the first window that are */
/*     contained in the second window. */

/* $ Examples */

/*     Let A contain the intervals */

/*           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ] */

/*     and B contain the intervals */

/*           [ 2, 4 ]  [ 8, 10 ]  [ 16, 18 ] */

/*     Then the difference of A and B contains the intervals */

/*           [ 1, 2 ]  [ 7, 8 ]  [ 10, 11 ]  [ 23, 27 ] */

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

/* -    SPICELIB Version 2.1.0, 24-AUG-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Fixed I/O type */
/*        of argument C in $Brief_I/O table. */

/*        Added entry #2 in $Exceptions section. */

/* -    SPICELIB Version 2.0.0, 16-SEP-1998 (WLT) */

/*        The previous version did not work when removing */
/*        singletons. This has been corrected. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     difference two d.p. windows */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 27-FEB-1989  (HAN) */

/*        Due to the calling sequence and functionality changes */
/*        in the routine EXCESS, the method of signaling an */
/*        excess of elements needed to be changed. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("WNDIFD", (ftnlen)6);

/*     Find the cardinality of the input windows, and the allowed size */
/*     of the output window. Also, save the size of the second window. */

    acard = cardd_(a);
    bcard = cardd_(b);
    csize = sized_(c__);
    over = 0;

/*     Empty out the output window. */

    ssized_(&csize, c__);

/*     Let's handle the pathological cases first. */

    if (bcard == 0) {
	copyd_(a, c__);
	chkout_("WNDIFD", (ftnlen)6);
	return 0;
    } else if (acard == 0) {
	chkout_("WNDIFD", (ftnlen)6);
	return 0;
    }

/*     Now get pointers to the first intervals of A and B. */

    apb = 1;
    ape = 2;
    bpb = 1;
    bpe = 2;
    put = 1;

/*     As long as the end pointer for A is less than the cardinality */
/*     of A we need to examine intervals and decide how much of */
/*     them to keep in C. */

    while(ape <= acard) {

/*        We will work with the interval [F,L] which starts out */
/*        as the next interval of A.  We modify it below as required */
/*        when subtracting out intervals of B. */

	f = a[apb + 5];
	l = a[ape + 5];

/*        Right now we have not resolved whether to keep the interval */
/*        [F,L], but until we know better we assume it is a keeper. */

	unrslv = bpe <= bcard;
	keep = TRUE_;
	while(unrslv) {
	    if (l < b[bpb + 5]) {

/*              The interval [F,L] is before the next interval of B, we */
/*              have resolved what to do with this one.   It is a */
/*              keeper. */

		unrslv = FALSE_;
	    } else if (f > b[bpe + 5]) {

/*              [F,L] is after the end of the current interval in B, */
/*              we need to look at the next interval of B */

		bpb += 2;
		bpe += 2;
		unrslv = bpe <= bcard;
	    } else {

/*              There is some overlap between the current interval */
/*              of B and the current interval of A. There are */
/*              several possibilities */

/*              1) The current interval of A is contained in the */
/*                 current interval of B (This includes singleton */
/*                 intervals in A). We just mark [F,L] so that it */
/*                 won't be kept.  We have fully resolved what to */
/*                 do with [F,L]. */

/*              2) The interval from B overlaps at the beginning */
/*                 of the interval of A */

/*                 B interval [......] */
/*                 A interval     [............] */
/*                 result of A-B     [.........] */

/*                 In this case we need to shrink the interval [F,L] */
/*                 but we have not resolved how much of the result */
/*                 to keep. */

/*              3) The interval from B falls inside the current */
/*                 interval [F,L] */

/*                 B interval        [......] */
/*                 A interval     [............] */
/*                 result of A-B  [..]      [..] */

/*                 If the interval from B is not a singleton, we store */
/*                 the first part of [F,L] in C and then set [F,L] to */
/*                 be the right interval which is still not resolved. */

/*                 If the B interval is a singleton we can ignore ignore */
/*                 it.  But we have not resolved what to do about */
/*                 [F,L], we need to look at the next interval of B. */


/*              4) The interval from B overlaps at the ending */
/*                 of the interval of A */

/*                 B interval          [......] */
/*                 A interval     [......] */
/*                 result of A-B  [....] */

/*                 We need to shrink [F,L]. In this case we know we can */
/*                 keep all of what's left because all other intervals */
/*                 of B are to the right of [F,L] */

		if (b[bpb + 5] <= f && l <= b[bpe + 5]) {

/*                 Case 1 above */

		    keep = FALSE_;
		    unrslv = FALSE_;
		} else if (b[bpb + 5] <= f) {

/*                 Case 2 above */

		    f = b[bpe + 5];
		    bpb += 2;
		    bpe += 2;
		    unrslv = bpe <= bcard;
		} else if (f <= b[bpb + 5] && l >= b[bpe + 5] && b[bpb + 5] < 
			b[bpe + 5]) {

/*                 Case 3 above (non-singleton interval of B). */

		    if (put < csize) {
			c__[put + 5] = f;
			c__[put + 6] = b[bpb + 5];
			i__1 = put + 1;
			scardd_(&i__1, c__);
			put += 2;
		    } else {
			over += 2;
		    }
		    f = b[bpe + 5];

/*                 If the interval from B contained L, we will not */
/*                 want to be keeping the singleton [F,L]. */

		    if (f == l) {
			keep = FALSE_;
			unrslv = FALSE_;
		    }
		    bpb += 2;
		    bpe += 2;
		    unrslv = unrslv && bpe <= bcard;
		} else if (f <= b[bpb + 5] && l >= b[bpe + 5] && b[bpb + 5] ==
			 b[bpe + 5]) {

/*                 Case 3 above (singleton interval of B). */

		    bpb += 2;
		    bpe += 2;
		    unrslv = bpe <= bcard;
		} else {

/*                 Case 4 above */

		    l = b[bpb + 5];
		    unrslv = FALSE_;
		}
	    }
	}

/*        If there is anything to keep in C, put it there. */

	if (keep) {

/*           Make sure there is sufficient room to do the putting. */

	    if (put < csize) {
		c__[put + 5] = f;
		c__[put + 6] = l;
		i__1 = put + 1;
		scardd_(&i__1, c__);
		put += 2;
	    } else {
		over += 2;
	    }
	}

/*        Move the pointers in A to the next interval. */

	apb += 2;
	ape += 2;
    }

/*     We've examined all of the intervals of A and B, but if we */
/*     didn't actually store all of the difference, signal an error. */

    if (over > 0) {
	needed = over + csize;
	setmsg_("The output window did not have sufficient room to contain t"
		"he result of the window difference.  It has room for # endpo"
		"ints, but # were needed to describe the difference. ", (
		ftnlen)171);
	errint_("#", &csize, (ftnlen)1);
	errint_("#", &needed, (ftnlen)1);
	sigerr_("SPICE(WINDOWEXCESS)", (ftnlen)19);
    }
    chkout_("WNDIFD", (ftnlen)6);
    return 0;
} /* wndifd_ */

