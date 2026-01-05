/* wnvald.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure WNVALD ( Validate a DP window ) */
/* Subroutine */ int wnvald_(integer *size, integer *n, doublereal *window)
{
    doublereal left;
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal right;
    extern /* Subroutine */ int scardd_(integer *, doublereal *), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), ssized_(integer *, 
	    doublereal *), setmsg_(char *, ftnlen), wninsd_(doublereal *, 
	    doublereal *, doublereal *);
    extern logical return_(void), odd_(integer *);

/* $ Abstract */

/*     Form a valid double precision window from the contents */
/*     of a window array. */

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
/*     SIZE       I   Size of window. */
/*     N          I   Original number of endpoints. */
/*     WINDOW    I-O  Input, output window. */

/* $ Detailed_Input */

/*     SIZE     is the size of the window to be validated. This is the */
/*              maximum number of endpoints that the cell used to */
/*              implement the window is capable of holding at any one */
/*              time. */

/*     N        is the original number of endpoints in the input cell. */

/*     WINDOW   on input is a (possibly uninitialized) cell array of */
/*              maximum size SIZE containing N endpoints of (possibly */
/*              unordered and non-disjoint) intervals. */

/* $ Detailed_Output */

/*     WINDOW   on output is a validated window, in which any overlapping */
/*              input intervals have been merged and the resulting set of */
/*              intervals is arranged in increasing order. */

/*              WINDOW is ready for use with any of the window routines. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the original number of endpoints N is odd, the error */
/*         SPICE(UNMATCHENDPTS) is signaled. */

/*     2)  If the original number of endpoints of the window exceeds its */
/*         size, the error SPICE(WINDOWTOOSMALL) is signaled. */

/*     3)  If the left endpoint is greater than the right endpoint, the */
/*         error SPICE(BADENDPOINTS) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine takes as input a cell array containing pairs of */
/*     endpoints and validates it to form a window. */

/*     On input, WINDOW is a cell of size SIZE containing N endpoints. */
/*     During validation, the intervals are ordered, and overlapping */
/*     intervals are merged. On output, the cardinality of WINDOW is */
/*     the number of endpoints remaining, and it is ready for use with */
/*     any of the window routines. */

/*     Because validation is done in place, there is no chance of */
/*     overflow. */

/*     Validation is primarily useful for ordering and merging */
/*     intervals read from input files or initialized in DATA */
/*     statements. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */

/*     1) Define an array containing a set of unordered and possibly */
/*        overlapping intervals, and validate the array as a SPICE */
/*        window. */


/*        Example code begins here. */


/*              PROGRAM WNVALD_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              INTEGER               CARDD */
/*              INTEGER               SIZED */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               WINSIZ */
/*              PARAMETER           ( WINSIZ = 20 ) */

/*              INTEGER               DATSIZ */
/*              PARAMETER           ( DATSIZ = 16 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      WINDOW  ( LBCELL : WINSIZ ) */
/*              DOUBLE PRECISION      WINDAT  ( DATSIZ ) */

/*              INTEGER               I */


/*              DATA                  WINDAT  /  0,  0, */
/*             .                                10, 12, */
/*             .                                 2,  7, */
/*             .                                13, 15, */
/*             .                                 1,  5, */
/*             .                                23, 29,  4*0 / */


/*        C */
/*        C     Insert the data into the SPICE cell array. */
/*        C */
/*              CALL MOVED ( WINDAT, WINSIZ, WINDOW(1) ) */

/*        C */
/*        C     Validate the input WINDOW array as a SPICE window. */
/*        C */
/*              CALL WNVALD ( WINSIZ, DATSIZ, WINDOW ) */

/*              WRITE (*,*) 'Current intervals: ', CARDD ( WINDOW ) / 2 */
/*              WRITE (*,*) 'Maximum intervals: ', SIZED ( WINDOW ) / 2 */
/*              WRITE (*,*) */
/*              WRITE (*,*) 'Intervals:' */
/*              WRITE (*,*) */

/*              DO I = 1, CARDD ( WINDOW ), 2 */
/*                 WRITE (*,*) WINDOW(I), WINDOW(I+1) */
/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Current intervals:            5 */
/*         Maximum intervals:           10 */

/*         Intervals: */

/*           0.0000000000000000        0.0000000000000000 */
/*           1.0000000000000000        7.0000000000000000 */
/*           10.000000000000000        12.000000000000000 */
/*           13.000000000000000        15.000000000000000 */
/*           23.000000000000000        29.000000000000000 */


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

/* -    SPICELIB Version 1.2.0, 16-MAR-2021 (JDR) */

/*        Changed argument name A to WINDOW for consistency with other */
/*        routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply to NAIF standard. Created complete */
/*        code example from code fragment and added example's problem */
/*        statement. */

/*        Improved description of argument WINDOW in $Detailed_Output. */

/*        Removed unnecessary $Revisions section. */

/* -    SPICELIB Version 1.1.1, 30-JUL-2002 (NJB) */

/*        Fixed bugs in example program. */

/* -    SPICELIB Version 1.1.0, 14-AUG-1995 (HAN) */

/*        Fixed a character string that continued over two lines. */
/*        The "//" characters were missing. The Alpha/OpenVMS compiler */
/*        issued a warning regarding this incorrect statement syntax. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     validate a d.p. window */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Setting up error processing. */

    if (return_()) {
	return 0;
    }
    chkin_("WNVALD", (ftnlen)6);

/*     First, some error checks. The number of endpoints must be even, */
/*     and smaller than the reported size of the window. */

    if (odd_(n)) {
	setmsg_("WNVALD: Unmatched endpoints", (ftnlen)27);
	sigerr_("SPICE(UNMATCHENDPTS)", (ftnlen)20);
	chkout_("WNVALD", (ftnlen)6);
	return 0;
    } else if (*n > *size) {
	setmsg_("WNVALD: Inconsistent value for SIZE.", (ftnlen)36);
	sigerr_("SPICE(WINDOWTOOSMALL)", (ftnlen)21);
	chkout_("WNVALD", (ftnlen)6);
	return 0;
    }

/*     Taking the easy way out, we will simply insert each new interval */
/*     as we happen upon it. We can do this safely in place. The output */
/*     window can't possibly contain more intervals than the input array. */

/*     What can go wrong is this: a left endpoint might be greater than */
/*     the corresponding left endpoint. This is a boo-boo, and should be */
/*     reported. */

    ssized_(size, window);
    scardd_(&c__0, window);
    i__ = 1;
    while(i__ < *n) {
	left = window[i__ + 5];
	right = window[i__ + 6];
	if (left > right) {
	    setmsg_("WNVALD: Left endpoint may not exceed right endpoint.", (
		    ftnlen)52);
	    sigerr_("SPICE(BADENDPOINTS)", (ftnlen)19);
	    chkout_("WNVALD", (ftnlen)6);
	    return 0;
	}
	wninsd_(&left, &right, window);
	i__ += 2;
    }
    chkout_("WNVALD", (ftnlen)6);
    return 0;
} /* wnvald_ */

