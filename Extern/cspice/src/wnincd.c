/* wnincd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure WNINCD ( Included in a double precision window ) */
logical wnincd_(doublereal *left, doublereal *right, doublereal *window)
{
    /* System generated locals */
    integer i__1;
    logical ret_val;

    /* Local variables */
    integer card, i__;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Determine whether an interval is included in a double precision */
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
/*     LEFT, */
/*     RIGHT      I   Input interval. */
/*     WINDOW     I   Input window. */

/*     The function returns .TRUE. if POINT is an element of WINDOW. */

/* $ Detailed_Input */

/*     LEFT, */
/*     RIGHT    are the endpoints of an interval, which may or may not be */
/*              contained in one of the intervals in WINDOW. */

/*     WINDOW   is a window containing zero or more intervals. */

/* $ Detailed_Output */

/*     The function returns .TRUE. if the input interval is included in */
/*     the input window --- that is, if */

/*        a(i)  <  LEFT  <  RIGHT  <  b(i) */
/*              -        -         - */

/*     for some interval [ a(i), b(i) ] in WINDOW --- and is .FALSE. */
/*     otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  The cardinality of the input WINDOW must be even. Left */
/*         endpoints of stored intervals must be strictly greater than */
/*         preceding right endpoints. Right endpoints must be greater */
/*         than or equal to corresponding left endpoints. Invalid window */
/*         data are not diagnosed by this routine and may lead to */
/*         unpredictable results. */

/*     2)  The order of the input interval's endpoints, LEFT and RIGHT, */
/*         is not checked, and that this does not affect the result. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     Let A contain the intervals */

/*           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ] */

/*     Then the following expressions are true */

/*           WNINCD ( 1.D0,  3.D0, WINDOW ) */
/*           WNINCD ( 9.D0, 10.D0, WINDOW ) */

/*     and the following expressions are false. */

/*           WNINCD (  0,  2, WINDOW ) */
/*           WNINCD ( 13, 15, WINDOW ) */
/*           WNINCD ( 29, 30, WINDOW ) */

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

/* -    SPICELIB Version 1.2.0, 14-MAR-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added entries */
/*        #1 and #2 in $Exceptions section. */

/*        Removed unnecessary $Revisions section. */

/* -    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN) */

/*        If the value of the function RETURN is .TRUE. upon execution of */
/*        this module, this function is assigned a default value of */
/*        either 0, 0.0D0, .FALSE., or blank depending on the type of */
/*        the function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     included in a d.p. window */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	ret_val = FALSE_;
	return ret_val;
    } else {
	chkin_("WNINCD", (ftnlen)6);
    }

/*     How many endpoints in the window? */

    card = cardd_(window);

/*     Check this interval agains every interval in the window. */
/*     Inefficient, but foolproof. */

    i__1 = card;
    for (i__ = 1; i__ <= i__1; i__ += 2) {
	if (*left >= window[i__ + 5] && *right <= window[i__ + 6]) {
	    ret_val = TRUE_;
	    chkout_("WNINCD", (ftnlen)6);
	    return ret_val;
	}
    }
    ret_val = FALSE_;
    chkout_("WNINCD", (ftnlen)6);
    return ret_val;
} /* wnincd_ */

