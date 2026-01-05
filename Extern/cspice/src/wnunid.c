/* wnunid.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure WNUNID ( Union two DP windows ) */
/* Subroutine */ int wnunid_(doublereal *a, doublereal *b, doublereal *c__)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    integer over, acard, bcard;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer csize;
    extern integer sized_(doublereal *);
    integer ap, bp, cp;
    extern /* Subroutine */ int scardd_(integer *, doublereal *), excess_(
	    integer *, char *, ftnlen), sigerr_(char *, ftnlen), chkout_(char 
	    *, ftnlen);
    extern logical return_(void);
    doublereal end;
    char use[1];

/* $ Abstract */

/*     Place the union of two double precision windows into a third */
/*     window. */

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
/*     C          O   Union of A and B. */

/* $ Detailed_Input */

/*     A, */
/*     B        are SPICE windows, each of which contains zero or more */
/*              intervals. */

/* $ Detailed_Output */

/*     C        is the output SPICE window, containing the union of A */
/*              and B --- every point contained in A, or in B, or in */
/*              both. */

/*              C must be distinct from both A and B. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the union of the two windows results in an excess of */
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

/*     The union of two windows contains every point contained in the */
/*     first window, or the second window, or both. */

/* $ Examples */

/*     Let A contain the intervals */

/*           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ] */

/*     and B contain the intervals */

/*           [ 2, 6 ]  [ 8, 10 ]  [ 16, 18 ] */

/*     Then the union of A and B contains the intervals */

/*           [ 1, 6 ]  [ 7, 11 ]  [ 16, 18 ]  [ 23, 27 ] */

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

/* -    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Fixed I/O type */
/*        of argument C in $Brief_I/O table. */

/*        Added entry #2 in $Exceptions section. Removed unnecessary */
/*        entries in $Revisions section. */

/* -    SPICELIB Version 1.1.0, 08-FEB-1999 (WLT) */

/*        The variable END was not initialized in the previous */
/*        edition. It is now initialized to be the minimum of */
/*        A(1) and B(1). */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     union two d.p. windows */

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
    } else {
	chkin_("WNUNID", (ftnlen)6);
    }

/*     Find the cardinality of the input windows, and the allowed size */
/*     of the output window. */

    acard = cardd_(a);
    bcard = cardd_(b);
    csize = sized_(c__);

/*     Begin with the input pointers at the first elements of the */
/*     input windows. The initial cardinality of the output window */
/*     is zero. And there is no overflow so far. */

/*     (Note that AP and BP point to the LEFT endpoints of intervals */
/*     in A and B, while CP points to the RIGHT endpoint of the latest */
/*     interval in C.) */

    ap = 1;
    bp = 1;
    cp = 0;
    end = min(a[6],b[6]);
    over = 0;

/*     When the ends of both input windows are reached, we're done. */

    while(ap < acard || bp < bcard) {

/*        If the end of one window has been reached, copy (or merge) */
/*        the next interval from the other window. */

	if (ap > acard) {
	    *(unsigned char *)use = 'B';
	} else if (bp > bcard) {
	    *(unsigned char *)use = 'A';

/*        Otherwise, let's see what we can do with the earlier of */
/*        the next intervals from A and B. */

	} else if (a[ap + 5] < b[bp + 5]) {
	    *(unsigned char *)use = 'A';
	} else if (b[bp + 5] <= a[ap + 5]) {
	    *(unsigned char *)use = 'B';
	}

/*        If there is still space in the output window, fill it */
/*        as necessary. Otherwise, stop filling the array, but continue */
/*        to count the number of elements in excess of the size of the */
/*        output window. */

/*        The general idea is this: if the next interval overlaps the */
/*        latest output interval, merge the two (extending the output */
/*        interval to the right). Otherwise, insert the next interval */
/*        intact. */

	if (*(unsigned char *)use == 'A') {
	    if (cp < csize) {
		if (a[ap + 5] <= end && cp > 0) {
/* Computing MAX */
		    d__1 = c__[cp + 5], d__2 = a[ap + 6];
		    c__[cp + 5] = max(d__1,d__2);
		} else {
		    cp += 2;
		    c__[cp + 4] = a[ap + 5];
		    c__[cp + 5] = a[ap + 6];
		}
		end = c__[cp + 5];
	    } else {
		if (a[ap + 5] <= end) {
/* Computing MAX */
		    d__1 = end, d__2 = a[ap + 6];
		    end = max(d__1,d__2);
		} else {
		    over += 2;
		    end = a[ap + 6];
		}
	    }
	    ap += 2;

/*        This is the same as the last clause, with B replacing A. */

	} else if (*(unsigned char *)use == 'B') {
	    if (cp < csize) {
		if (b[bp + 5] <= end && cp > 0) {
/* Computing MAX */
		    d__1 = c__[cp + 5], d__2 = b[bp + 6];
		    c__[cp + 5] = max(d__1,d__2);
		} else {
		    cp += 2;
		    c__[cp + 4] = b[bp + 5];
		    c__[cp + 5] = b[bp + 6];
		}
		end = c__[cp + 5];
	    } else {
		if (b[bp + 5] <= end) {
/* Computing MAX */
		    d__1 = end, d__2 = b[bp + 6];
		    end = max(d__1,d__2);
		} else {
		    over += 2;
		    end = b[bp + 6];
		}
	    }
	    bp += 2;
	}
    }

/*     Set the cardinality of the output window. */

    scardd_(&cp, c__);

/*     If there is an excess of elements, signal an error and check out */
/*     as usual. */

    if (over > 0) {
	excess_(&over, "window", (ftnlen)6);
	sigerr_("SPICE(WINDOWEXCESS)", (ftnlen)19);
    }
    chkout_("WNUNID", (ftnlen)6);
    return 0;
} /* wnunid_ */

