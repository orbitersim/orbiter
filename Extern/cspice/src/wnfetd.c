/* wnfetd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure WNFETD ( Fetch an interval from a DP window ) */
/* Subroutine */ int wnfetd_(doublereal *window, integer *n, doublereal *left,
	 doublereal *right)
{
    integer card;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    integer end;

/* $ Abstract */

/*     Fetch a particular interval from a double precision window. */

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
/*     WINDOW     I   Input window. */
/*     N          I   Index of interval to be fetched. */
/*     LEFT, */
/*     RIGHT      O   Left, right endpoints of the Nth interval. */

/* $ Detailed_Input */

/*     WINDOW   is a window containing zero or more intervals. */

/*     N        is the index of a particular interval within the window. */
/*              Indices range from 1 to NINT, where NINT is the number of */
/*              intervals in the window (CARDD(WINDOW)/2). */

/* $ Detailed_Output */

/*     LEFT, */
/*     RIGHT    are the left and right endpoints of the N'th interval in */
/*              the input window. If the interval is not found, LEFT and */
/*              RIGHT are not defined. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If N is less than one, the error SPICE(NOINTERVAL) is */
/*         signaled. */

/*     2)  If the interval does not exist, i.e. N > CARDD(WINDOW)/2, the */
/*         error SPICE(NOINTERVAL) is signaled. */

/*     3)  The cardinality of the input WINDOW must be even. Left */
/*         endpoints of stored intervals must be strictly greater than */
/*         preceding right endpoints. Right endpoints must be greater */
/*         than or equal to corresponding left endpoints. Invalid window */
/*         data are not diagnosed by this routine and may lead to */
/*         unpredictable results. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */

/*     1) The following code example demonstrates how to insert an */
/*        interval into an existing double precision SPICE window, and */
/*        how to loop over all its intervals to extract their left and */
/*        right points. */


/*        Example code begins here. */


/*              PROGRAM WNFETD_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              INTEGER               WNCARD */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               WNSIZE */
/*              PARAMETER           ( WNSIZE = 10 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      WINDOW      ( LBCELL:WNSIZE ) */
/*              DOUBLE PRECISION      LEFT */
/*              DOUBLE PRECISION      RIGHT */

/*              INTEGER               I */

/*        C */
/*        C     Validate the window with size WNSIZE and zero elements. */
/*        C */
/*              CALL WNVALD( WNSIZE, 0, WINDOW ) */

/*        C */
/*        C     Insert the intervals */
/*        C */
/*        C        [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ] */
/*        C */
/*        C     into WINDOW. */
/*        C */
/*              CALL WNINSD(  1.D0,  3.D0, WINDOW ) */
/*              CALL WNINSD(  7.D0, 11.D0, WINDOW ) */
/*              CALL WNINSD( 23.D0, 27.D0, WINDOW ) */

/*        C */
/*        C     Loop over the number of intervals in WINDOW, output */
/*        C     the LEFT and RIGHT endpoints for each interval. */
/*        C */
/*              DO I=1, WNCARD(WINDOW) */

/*                 CALL WNFETD( WINDOW, I, LEFT, RIGHT ) */

/*                 WRITE(*,'(A,I2,2(A,F8.3),A)') 'Interval', I, */
/*             .                  ' [', LEFT,',',  RIGHT, ']' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Interval 1 [   1.000,   3.000] */
/*        Interval 2 [   7.000,  11.000] */
/*        Interval 3 [  23.000,  27.000] */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 15-MAR-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example, problem statement and solution. Added entry #3 in */
/*        $Exceptions section. */

/*        Improved description of argument N in $Detailed_Input. */

/* -    SPICELIB Version 1.0.2, 30-JUL-2007 (EDW) */

/*        Removed erroneous description in the $Examples section */
/*        indicating "Undefined" as a return state after an error */
/*        event caused by an invalid value of N. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     fetch an interval from a d.p. window */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Set up the error processing. */

    if (return_()) {
	return 0;
    }
    chkin_("WNFETD", (ftnlen)6);


/*     How many endpoints in the window? Enough? Normally, endpoints */
/*     of the Nth interval are stored in elements 2N and 2N-1. */

    card = cardd_(window);
    end = *n << 1;
    if (*n < 1 || card < end) {
	setmsg_("WNFETD: No such interval.", (ftnlen)25);
	sigerr_("SPICE(NOINTERVAL)", (ftnlen)17);
    } else {
	*left = window[end + 4];
	*right = window[end + 5];
    }
    chkout_("WNFETD", (ftnlen)6);
    return 0;
} /* wnfetd_ */

