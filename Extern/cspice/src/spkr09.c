/* spkr09.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure SPKR09 ( Read SPK record from segment, type 9 ) */
/* Subroutine */ int spkr09_(integer *handle, doublereal *descr, doublereal *
	et, doublereal *record)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer high, near__, ndir, last, type__, i__, n, begin, nread;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *), errdp_(char *, 
	    doublereal *, ftnlen);
    integer first, group, start;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal dc[2];
    integer ic[6], degree, begidx, bufbas, dirbas;
    doublereal buffer[101];
    integer endidx, remain, timbas;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    doublereal contrl[2];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern integer lstltd_(doublereal *, integer *, doublereal *);
    integer wndsiz;
    extern logical return_(void), odd_(integer *);
    integer end, low;

/* $ Abstract */

/*     Read a single SPK data record from a segment of type 9 */
/*     (Unequally spaced discrete states, interpolated by Lagrange */
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
/*              a SPK segment of type 9. */

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
/*                 | epochs 1--n          | */
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

/*     2)  If the segment specified by DESCR is not of data types 9 or */
/*         13, the error SPICE(WRONGSPKTYPE) is signaled. */

/*     3)  If the input ET value is not within the range specified */
/*         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS) */
/*         is signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the SPK Required Reading file for a description of the */
/*     structure of a data type 9 segment. */

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

/*           IF ( TYPE .EQ. 9 ) THEN */
/*              CALL SPKR09 ( HANDLE, DESCR, ET, RECORD ) */
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

/*        Data type check was relaxed to enable reading type 13 */
/*        segments. */

/* -    SPICELIB Version 1.0.1, 24-OCT-1994 (NJB) */

/*        In-line comment concerning transpose of state data was */
/*        removed. */

/* -    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB) */

/* -& */
/* $ Index_Entries */

/*     read record from type_9 SPK segment */

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

/*     Make sure that this really is a type 9 or type 13 data segment. */

    if (type__ != 9 && type__ != 13) {
	chkin_("SPKR09", (ftnlen)6);
	setmsg_("You are attempting to locate type 9 or type 13 data in a ty"
		"pe # data segment.", (ftnlen)77);
	errint_("#", &type__, (ftnlen)1);
	sigerr_("SPICE(WRONGSPKTYPE)", (ftnlen)19);
	chkout_("SPKR09", (ftnlen)6);
	return 0;
    }

/*     Check the request time against the bounds in the segment */
/*     descriptor. */

    if (*et < dc[0] || *et > dc[1]) {
	chkin_("SPKR09", (ftnlen)6);
	setmsg_("Request time # is outside of descriptor bounds # : #.", (
		ftnlen)53);
	errdp_("#", et, (ftnlen)1);
	errdp_("#", dc, (ftnlen)1);
	errdp_("#", &dc[1], (ftnlen)1);
	sigerr_("SPICE(TIMEOUTOFBOUNDS)", (ftnlen)22);
	chkout_("SPKR09", (ftnlen)6);
	return 0;
    }

/*     From this point onward, we assume the segment was constructed */
/*     correctly.  In particular, we assume: */

/*        1)  The first and last epochs in the segment define a time */
/*            interval that contains the interval defined by the segment */
/*            descriptor's time bounds. */

/*        2)  The segment descriptor's time bounds are in order and are */
/*            distinct. */

/*        3)  The epochs in the segment are in strictly increasing */
/*            order. */

/*        4)  The degree of the interpolating polynomial specified by */
/*            the segment is at least 1 and is no larger than */

/*               ( L - 1 ) / 7         [integer division] */

/*            where L is the declared length of the argument RECORD. */

/*        5)  There are at least as many epochs in the segment as the */
/*            the number of points required to define an interpolating */
/*            polynomial of the specified degree. */


/*     We'll need the last two items before we can determine which */
/*     states make up our output record. */


    i__1 = end - 1;
    dafgda_(handle, &i__1, &end, contrl);
    degree = i_dnnt(contrl);
    n = i_dnnt(&contrl[1]);
    wndsiz = degree + 1;

/*     We'll now select the set of states that define the interpolating */
/*     polynomials.   We'll start out by finding the first directory */
/*     entry that is greater than or equal to the request epoch.  We'll */
/*     use the variable GROUP to indicate the set of epochs to search */
/*     within, once we've found the right directory entry. */

    ndir = (n - 1) / 100;
    dirbas = end - ndir - 2;
    if (ndir == 0) {

/*        There's no mystery about which group of epochs to search. */

	group = 1;
    } else {

/*        There's at least one directory.  Find the first directory */
/*        whose time is greater than or equal to the request time, if */
/*        there is such a directory.  We'll search linearly through the */
/*        directory entries, reading up to BUFSIZ of them at a time. */
/*        Having found the correct set of directory entries, we'll */
/*        perform a binary search within that set for the desired entry. */

	bufbas = dirbas;
	nread = min(ndir,101);
	remain = ndir - nread;
	i__1 = bufbas + 1;
	i__2 = bufbas + nread;
	dafgda_(handle, &i__1, &i__2, buffer);
	while(buffer[(i__1 = nread - 1) < 101 && 0 <= i__1 ? i__1 : s_rnge(
		"buffer", i__1, "spkr09_", (ftnlen)377)] < *et && remain > 0) 
		{
	    bufbas += nread;
	    nread = min(remain,101);
	    remain -= nread;

/*           Note:  NREAD is always > 0 here. */

	    i__1 = bufbas + 1;
	    i__2 = bufbas + nread;
	    dafgda_(handle, &i__1, &i__2, buffer);
	}

/*        At this point, BUFBAS - DIRBAS is the number of directory */
/*        entries preceding the one contained in BUFFER(1). */

	group = bufbas - dirbas + lstltd_(et, &nread, buffer) + 1;
    }

/*     GROUP now indicates the set of epochs in which to search for the */
/*     request epoch.  If GROUP is 1, the request time lies within the */
/*     inclusive time interval bounded by the first and last epochs of */
/*     the first group.  Otherwise, the request time lies in the time */
/*     interval bounded by the last element of the preceding group and */
/*     the last element of the current group. */

/*     We'll use the variable names BEGIDX and ENDIDX to refer to */
/*     the indices, relative to the set of time tags, of the first */
/*     and last time tags in the set we're going to look up. */

    if (group == 1) {
	begidx = 1;
	endidx = min(n,100);
    } else {

/*        If the group index is greater than 1, we'll include the last */
/*        time tag of the previous group in the set of time tags we look */
/*        up.  That way, the request time is bracketed by the time tag */
/*        set we look up. */

	begidx = (group - 1) * 100;
/* Computing MIN */
	i__1 = begidx + 100;
	endidx = min(i__1,n);
    }
    timbas = dirbas - n;
    i__1 = timbas + begidx;
    i__2 = timbas + endidx;
    dafgda_(handle, &i__1, &i__2, buffer);

/*     Find two adjacent epochs bounding the request epoch.  The request */
/*     time cannot be greater than all of epochs in the group, and it */
/*     cannot precede the first element of the group. */

    i__1 = endidx - begidx + 1;
    i__ = lstltd_(et, &i__1, buffer);

/*     The variables LOW and high are the indices of a pair of time */
/*     tags that bracket the request time. */

    if (i__ == 0) {
	low = 1;
    } else {
	low = begidx + i__ - 1;
    }
    high = low + 1;

/*     Now select the set of states used for interpolation. */

    if (odd_(&wndsiz)) {

/*        Find the index of the state whose epoch is closest to the */
/*        input epoch.  The index I is in the range [0, DIRSIZ], */
/*        since ENDIDX - BEGIDX never exceeds DIRSIZ, and ET is */
/*        never larger than the (ENDIDX-BEGIDX+1)th element of the */
/*        buffer. */

	if (i__ == 0) {

/*           This can happen only if the request time matches the */
/*           first time tag of the segment. */

	    near__ = low;
	} else if ((d__1 = *et - buffer[(i__1 = i__ - 1) < 101 && 0 <= i__1 ? 
		i__1 : s_rnge("buffer", i__1, "spkr09_", (ftnlen)471)], abs(
		d__1)) < (d__2 = *et - buffer[(i__2 = i__) < 101 && 0 <= i__2 
		? i__2 : s_rnge("buffer", i__2, "spkr09_", (ftnlen)471)], abs(
		d__2))) {
	    near__ = low;
	} else {
	    near__ = high;
	}

/*        The epochs whose index is NEAR is the (WNDSIZ/2 + 1)th */
/*        of the interpolating set, unless the request time is too close */
/*        to the end of the coverage interval, in which case one endpoint */
/*        of the window will coincide with an endpoint of the coverage */
/*        interval. */

/* Computing MIN */
/* Computing MAX */
	i__3 = near__ - degree / 2;
	i__1 = max(i__3,1), i__2 = n - degree;
	first = min(i__1,i__2);
	last = first + degree;
    } else {

/*        The group size is even. */

/*        The bracketing epochs we've found are the (WNDSIZ/2)th */
/*        and (WNDSIZ/2 + 1)th of the interpolating set, unless the */
/*        request time is too close to the end of the coverage interval, */
/*        in which case one endpoint of the window will coincide with */
/*        an endpoint of the coverage interval. */

/* Computing MIN */
/* Computing MAX */
	i__3 = low - degree / 2;
	i__1 = max(i__3,1), i__2 = n - degree;
	first = min(i__1,i__2);
	last = first + degree;
    }

/*     Put the size of the group of states into the output record. */

    record[0] = (doublereal) wndsiz;

/*     Read the states. */

    i__1 = begin + (first - 1) * 6;
    i__2 = begin + last * 6 - 1;
    dafgda_(handle, &i__1, &i__2, &record[1]);

/*     Finally, add the epochs to the output record. */

    start = begin + n * 6 + first - 2;
    i__1 = start + 1;
    i__2 = start + wndsiz;
    dafgda_(handle, &i__1, &i__2, &record[wndsiz * 6 + 1]);
    return 0;
} /* spkr09_ */

