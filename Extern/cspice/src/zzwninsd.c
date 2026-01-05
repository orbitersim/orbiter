/* zzwninsd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure ZZWNINSD ( Insert an interval into a DP window ) */
/* Subroutine */ int zzwninsd_(doublereal *left, doublereal *right, char *
	context, doublereal *window, ftnlen context_len)
{
    /* System generated locals */
    address a__1[3];
    integer i__1[3], i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);

    /* Local variables */
    integer card, size, i__, j;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    extern integer sized_(doublereal *);
    extern /* Subroutine */ int scardd_(integer *, doublereal *);
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    char msg[1840];

/* $ Abstract */

/*      Insert an interval into a double precision window. */

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

/*      WINDOWS */

/* $ Keywords */

/*      WINDOWS */

/* $ Declarations */
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


/*     Include File:  SPICELIB Error Handling Parameters */

/*        errhnd.inc  Version 2    18-JUN-1997 (WLT) */

/*           The size of the long error message was */
/*           reduced from 25*80 to 23*80 so that it */
/*           will be accepted by the Microsoft Power Station */
/*           FORTRAN compiler which has an upper bound */
/*           of 1900 for the length of a character string. */

/*        errhnd.inc  Version 1    29-JUL-1997 (NJB) */



/*     Maximum length of the long error message: */


/*     Maximum length of the short error message: */


/*     End Include File:  SPICELIB Error Handling Parameters */

/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      LEFT, */
/*      RIGHT      I   Left, right endpoints of new interval. */
/*      CONTEXT    I   A call explanation string. */
/*      WINDOW    I,O  Input, output window. */

/* $ Detailed_Input */

/*      LEFT, */
/*      RIGHT       are the left and right endpoints of the interval */
/*                  to be inserted. */

/*      CONTEXT     a context/explanation string to append to the */
/*                  long error message if an error signals. The caller */
/*                  need not include a message. A single blank, ' ', */
/*                  represents no message. */

/*      WINDOW      on input, is a window containing zero or more */
/*                  intervals. */

/* $ Detailed_Output */

/*      WINDOW      on output, is the original window following the */
/*                  insertion of the interval from LEFT to RIGHT. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If LEFT is greater than RIGHT, the error SPICE(BADENDPOINTS) is */
/*        signaled. */

/*     2) If the insertion of the interval causes an excess of elements, */
/*        the error SPICE(WINDOWEXCESS) is signaled. */

/* $ Files */

/*      None. */

/* $ Particulars */

/*      This routine inserts the interval from LEFT to RIGHT into the */
/*      input window. If the new interval overlaps any of the intervals */
/*      in the window, the intervals are merged. Thus, the cardinality */
/*      of the input window can actually decrease as the result of an */
/*      insertion. However, because inserting an interval that is */
/*      disjoint from the other intervals in the window can increase the */
/*      cardinality of the window, the routine signals an error. */

/*      This is the only unary routine to signal an error. No */
/*      other unary routine can increase the number of intervals in */
/*      the input window. */

/*      If a non-blank CONTEXT string passes from the caller, any error */
/*      signal will return the long error message with the CONTEXT */
/*      string appended to that message. */

/* $ Examples */

/*      Let WINDOW contain the intervals */

/*            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ] */

/*      Then the following series of calls */

/*            CALL ZZWNINSD ( 5,  5, CONTEXT, WINDOW)       (1) */
/*            CALL ZZWNINSD ( 4,  8, CONTEXT, WINDOW)       (2) */
/*            CALL ZZWNINSD ( 0, 30, CONTEXT, WINDOW)       (3) */

/*      produces the following series of windows */

/*            [ 1,  3 ]  [ 5,  5 ]  [  7, 11 ]  [ 23, 27 ]   (1) */
/*            [ 1,  3 ]  [ 4, 11 ]  [ 23, 27 ]               (2) */
/*            [ 0, 30 ]                                      (3) */

/* $ Restrictions */

/*      None. */

/* $ Literature_References */

/*      None. */

/* $ Author_and_Institution */

/*      K.R. Gehringer  (JPL) */
/*      N.J. Bachman    (JPL) */
/*      H.A. Neilan     (JPL) */
/*      W.L. Taber      (JPL) */
/*      I.M. Underwood  (JPL) */

/* $ Version */

/* -     SPICELIB Version 1.1.1, 05-OCT-2021 (NJB) */

/*         Corrected typos in long error messages and in comments. */

/* -     SPICELIB Version 1.0.0, 03-MAR-2009 (EDW) */

/*         This routine is a copy of the SPICELIB WNINSD routine */
/*         changed only by the addition of the CONTEXT string. */

/* -& */
/* $ Index_Entries */

/*     insert interval into d.p. window, with context string */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Local parameters */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZWNINSD", (ftnlen)8);
    }

/*     Get the size and cardinality of the window. */

    size = sized_(window);
    card = cardd_(window);

/*     Let's try the easy cases first. No input interval? No change. */
/*     Signal that an error has occurred and set the error message. */

    if (*left > *right) {
	s_copy(msg, "Left endpoint greater-than right. Left endpoint was #1."
		" Right endpoint was #2.", (ftnlen)1840, (ftnlen)78);
/* Writing concatenation */
	i__1[0] = lastnb_(msg, (ftnlen)1840), a__1[0] = msg;
	i__1[1] = 1, a__1[1] = " ";
	i__1[2] = lastnb_(context, context_len), a__1[2] = context;
	s_cat(msg, a__1, i__1, &c__3, (ftnlen)1840);
	setmsg_(msg, (ftnlen)1840);
	errdp_("#1", left, (ftnlen)2);
	errdp_("#2", right, (ftnlen)2);
	sigerr_("SPICE(BADENDPOINTS)", (ftnlen)19);
	chkout_("ZZWNINSD", (ftnlen)8);
	return 0;
    } else if (card == 0 || *left > window[card + 5]) {

/*        Empty window? Input interval later than the end of the window? */
/*        Just insert the interval, if there's room. */

	if (size >= card + 2) {
	    i__2 = card + 2;
	    scardd_(&i__2, window);
	    window[card + 6] = *left;
	    window[card + 7] = *right;
	} else {
	    s_copy(msg, "Window has size, #1, cardinality #2. Cannot insert "
		    "an additional interval into the window.", (ftnlen)1840, (
		    ftnlen)90);
/* Writing concatenation */
	    i__1[0] = lastnb_(msg, (ftnlen)1840), a__1[0] = msg;
	    i__1[1] = 1, a__1[1] = " ";
	    i__1[2] = lastnb_(context, context_len), a__1[2] = context;
	    s_cat(msg, a__1, i__1, &c__3, (ftnlen)1840);
	    setmsg_(msg, (ftnlen)1840);
	    errint_("#1", &size, (ftnlen)2);
	    errint_("#2", &card, (ftnlen)2);
	    sigerr_("SPICE(WINDOWEXCESS)", (ftnlen)19);
	}
	chkout_("ZZWNINSD", (ftnlen)8);
	return 0;
    }

/*     Now on to the tougher cases. */

/*     Skip intervals which lie completely to the left of the input */
/*     interval. (The index I will always point to the right endpoint */
/*     of an interval). */

    i__ = 2;
    while(i__ <= card && window[i__ + 5] < *left) {
	i__ += 2;
    }

/*     There are three ways this can go. The new interval can: */

/*        1) lie entirely between the previous interval and the next. */

/*        2) overlap the next interval, but no others. */

/*        3) overlap more than one interval. */

/*     Only the first case can possibly cause an overflow, since the */
/*     other two cases require existing intervals to be merged. */


/*     Case (1). If there's room, move succeeding intervals back and */
/*     insert the new one. If there isn't room, signal an error. */

    if (*right < window[i__ + 4]) {
	if (size >= card + 2) {
	    i__2 = i__ - 1;
	    for (j = card; j >= i__2; --j) {
		window[j + 7] = window[j + 5];
	    }
	    i__2 = card + 2;
	    scardd_(&i__2, window);
	    window[i__ + 4] = *left;
	    window[i__ + 5] = *right;
	} else {
	    s_copy(msg, "Window has size, #1, cardinality #2. Cannot insert "
		    "an additional interval into the window. The new interval"
		    " lies entirely between the previous interval and thenext."
		    , (ftnlen)1840, (ftnlen)164);
/* Writing concatenation */
	    i__1[0] = lastnb_(msg, (ftnlen)1840), a__1[0] = msg;
	    i__1[1] = 1, a__1[1] = " ";
	    i__1[2] = lastnb_(context, context_len), a__1[2] = context;
	    s_cat(msg, a__1, i__1, &c__3, (ftnlen)1840);
	    setmsg_(msg, (ftnlen)1840);
	    errint_("#1", &size, (ftnlen)2);
	    errint_("#2", &card, (ftnlen)2);
	    sigerr_("SPICE(WINDOWEXCESS)", (ftnlen)19);
	    chkout_("ZZWNINSD", (ftnlen)8);
	    return 0;
	}

/*     Cases (2) and (3). */

    } else {

/*        The left and right endpoints of the new interval may or */
/*        may not replace the left and right endpoints of the existing */
/*        interval. */

/* Computing MIN */
	d__1 = *left, d__2 = window[i__ + 4];
	window[i__ + 4] = min(d__1,d__2);
/* Computing MAX */
	d__1 = *right, d__2 = window[i__ + 5];
	window[i__ + 5] = max(d__1,d__2);

/*        Skip any intervals contained in the one we modified. */
/*        (Like I, J always points to the right endpoint of an */
/*        interval.) */

	j = i__ + 2;
	while(j <= card && window[j + 5] <= window[i__ + 5]) {
	    j += 2;
	}

/*        If the modified interval extends into the next interval, */
/*        merge the two. (The modified interval grows to the right.) */

	if (j <= card && window[i__ + 5] >= window[j + 4]) {
	    window[i__ + 5] = window[j + 5];
	    j += 2;
	}

/*        Move the rest of the intervals forward to take up the */
/*        spaces left by the absorbed intervals. */

	while(j <= card) {
	    i__ += 2;
	    window[i__ + 4] = window[j + 4];
	    window[i__ + 5] = window[j + 5];
	    j += 2;
	}
	scardd_(&i__, window);
    }
    chkout_("ZZWNINSD", (ftnlen)8);
    return 0;
} /* zzwninsd_ */

