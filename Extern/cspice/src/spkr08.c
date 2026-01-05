/* spkr08.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure SPKR08 ( Read SPK record from segment, type 8 ) */
/* Subroutine */ int spkr08_(integer *handle, doublereal *descr, doublereal *
	et, doublereal *record)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    integer near__, last;
    doublereal step;
    integer type__, n, begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *), errdp_(char *, 
	    doublereal *, ftnlen);
    integer first;
    doublereal start;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal dc[2];
    integer ic[6], degree;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    doublereal contrl[4];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    integer grpsiz;
    extern logical return_(void), odd_(integer *);
    integer end, low;

/* $ Abstract */

/*     Read a single SPK data record from a segment of type 8 */
/*     (equally spaced discrete states, interpolated by Lagrange */
/*     polynomials). */

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

/*     SPK */
/*     TIME */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     DESCR      I   Segment descriptor. */
/*     ET         I   Target epoch. */
/*     RECORD     O   Data record. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     DESCR    are the file handle and segment descriptor for */
/*              a SPK segment of type 8. */

/*     ET       is a target epoch, for which a data record from */
/*              a specific segment is required. */

/* $ Detailed_Output */

/*     RECORD   is a set of data from the specified segment which, */
/*              when evaluated at epoch ET, will give the state */
/*              (position and velocity) of some body, relative */
/*              to some center, in some inertial reference frame. */

/*              The structure of the record is as follows: */

/*                 +----------------------+ */
/*                 | number of states (n) | */
/*                 +----------------------+ */
/*                 | start epoch          | */
/*                 +----------------------+ */
/*                 | step size            | */
/*                 +----------------------+ */
/*                 | state 1 (6 elts.)    | */
/*                 +----------------------+ */
/*                 | state 2 (6 elts.)    | */
/*                 +----------------------+ */
/*                             . */
/*                             . */
/*                             . */
/*                 +----------------------+ */
/*                 | state n (6 elts.)    | */
/*                 +----------------------+ */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     This routine follows the pattern established in the lower-numbered */
/*     SPK data type readers of not explicitly performing error */
/*     diagnoses. Exceptions are listed below nonetheless. */

/*     1)  If the input HANDLE does not designate a loaded SPK file, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If the segment specified by DESCR is not of data type 08, */
/*         the error SPICE(WRONGSPKTYPE) is signaled. */

/*     3)  If the input ET value is not within the range specified */
/*         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS) */
/*         is signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the SPK Required Reading file for a description of the */
/*     structure of a data type 8 segment. */

/* $ Examples */

/*     The data returned by the SPKRnn routine is in its rawest form, */
/*     taken directly from the segment. As such, it will be meaningless */
/*     to a user unless he/she understands the structure of the data type */
/*     completely. Given that understanding, however, the SPKRxx */
/*     routines might be used to "dump" and check segment data for a */
/*     particular epoch. */


/*     C */
/*     C     Get a segment applicable to a specified body and epoch. */
/*     C */
/*           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*     C */
/*     C     Look at parts of the descriptor. */
/*     C */
/*           CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */
/*           CENTER = ICD( 2 ) */
/*           REF    = ICD( 3 ) */
/*           TYPE   = ICD( 4 ) */

/*           IF ( TYPE .EQ. 8 ) THEN */
/*              CALL SPKR08 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     1)  Correctness of inputs must be ensured by the caller of */
/*         this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.1, 12-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */
/*        Added IMPLICIT NONE. */

/* -    SPICELIB Version 2.0.0, 06-NOV-1999 (NJB) */

/*        Data type check was relaxed to enable reading type 12 */
/*        segments. */

/* -    SPICELIB Version 1.0.1, 24-OCT-1994 (NJB) */

/*        In-line comment concerning transpose of state data was */
/*        removed. */

/* -    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB) */

/* -& */
/* $ Index_Entries */

/*     read record from type_8 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Use discovery check-in. */

    if (return_()) {
	return 0;
    }

/*     Unpack the segment descriptor, and get the start and end addresses */
/*     of the segment. */

    dafus_(descr, &c__2, &c__6, dc, ic);
    type__ = ic[3];
    begin = ic[4];
    end = ic[5];

/*     Make sure that this really is a type 8 or type 12 data segment. */

    if (type__ != 8 && type__ != 12) {
	chkin_("SPKR08", (ftnlen)6);
	setmsg_("You are attempting to locate type 8 or type 12 data in a ty"
		"pe # data segment.", (ftnlen)77);
	errint_("#", &type__, (ftnlen)1);
	sigerr_("SPICE(WRONGSPKTYPE)", (ftnlen)19);
	chkout_("SPKR08", (ftnlen)6);
	return 0;
    }

/*     Check the request time against the bounds in the segment */
/*     descriptor. */

    if (*et < dc[0] || *et > dc[1]) {
	chkin_("SPKR08", (ftnlen)6);
	setmsg_("Request time # is outside of descriptor bounds # : #.", (
		ftnlen)53);
	errdp_("#", et, (ftnlen)1);
	errdp_("#", dc, (ftnlen)1);
	errdp_("#", &dc[1], (ftnlen)1);
	sigerr_("SPICE(TIMEOUTOFBOUNDS)", (ftnlen)22);
	chkout_("SPKR08", (ftnlen)6);
	return 0;
    }

/*     The type 8 segment structure is described by this diagram from */
/*     the SPK Required Reading: */

/*        +-----------------------+ */
/*        | State 1               | */
/*        +-----------------------+ */
/*        | State 2               | */
/*        +-----------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-----------------------+ */
/*        | State N               | */
/*        +-----------------------+ */
/*        | Epoch of state 1 (ET) | */
/*        +-----------------------+ */
/*        | Step size             | */
/*        +-----------------------+ */
/*        | Polynomial degree     | */
/*        +-----------------------+ */
/*        | Number of states      | */
/*        +-----------------------+ */


/*     We'll need the last four items before we can determine which */
/*     states make up our output record. */


    i__1 = end - 3;
    dafgda_(handle, &i__1, &end, contrl);
    start = contrl[0];
    step = contrl[1];
    degree = i_dnnt(&contrl[2]);
    n = i_dnnt(&contrl[3]);
    grpsiz = degree + 1;

/*     We'll now select the set of states that define the interpolating */
/*     polynomials.  The cases of odd and even GRPSIZ are handled */
/*     separately. */

    if (odd_(&grpsiz)) {

/*        Find the index of the state whose epoch is closest to the */
/*        input epoch.  Find the first and last indices in the record */
/*        of the (GRPSIZ-1)/2 states on either side of this central */
/*        state. */

	d__1 = (*et - start) / step;
	near__ = i_dnnt(&d__1) + 1;
/* Computing MIN */
/* Computing MAX */
	i__3 = 1, i__4 = near__ - degree / 2;
	i__1 = max(i__3,i__4), i__2 = n - degree;
	first = min(i__1,i__2);
	last = first + degree;
    } else {

/*        Find the index of the last state whose epoch is less than or */
/*        equal to that of the input epoch.  Find the first and last */
/*        indices in the record of the set of GRPSIZ consecutive states */
/*        having this state as the (GRPSIZ/2)th one. */

	low = (integer) ((*et - start) / step) + 1;
/* Computing MIN */
/* Computing MAX */
	i__3 = 1, i__4 = low - degree / 2;
	i__1 = max(i__3,i__4), i__2 = n - degree;
	first = min(i__1,i__2);
	last = first + degree;
    }

/*     Put the size of the group of states, the epoch of the first */
/*     state in the record, and the step size into the output record. */

    record[0] = (doublereal) grpsiz;
    record[1] = start + (first - 1) * step;
    record[2] = step;

/*     Read the states. */

    i__1 = begin + (first - 1) * 6;
    i__2 = begin + last * 6 - 1;
    dafgda_(handle, &i__1, &i__2, &record[3]);
    return 0;
} /* spkr08_ */

