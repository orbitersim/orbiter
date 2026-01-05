/* spkr18.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure SPKR18 ( Read SPK record from segment, type 18 ) */
/* Subroutine */ int spkr18_(integer *handle, doublereal *descr, doublereal *
	et, doublereal *record)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer high, ndir, last, type__, i__, n, begin, nread;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *), errdp_(char *, 
	    doublereal *, ftnlen);
    integer lsize, first, group, rsize, start;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal dc[2];
    integer ic[6];
    extern logical failed_(void);
    integer begidx, bufbas, dirbas;
    doublereal buffer[101];
    integer endidx, remain, timbas, packsz;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer maxwnd;
    doublereal contrl[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern integer lstltd_(doublereal *, integer *, doublereal *);
    integer wndsiz;
    extern logical return_(void);
    integer subtyp;
    extern logical odd_(integer *);
    integer end, low;

/* $ Abstract */

/*     Read a single SPK data record from a segment of type 18 */
/*     (MEX/Rosetta Orbit file interpolation). */

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
/* $ Abstract */

/*     Declare parameters specific to SPK type 18. */

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

/*     SPK */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-AUG-2002 (NJB) */

/* -& */

/*     SPK type 18 subtype codes: */


/*     Subtype 0:  Hermite interpolation, 12-element packets, order */
/*                 reduction at boundaries to preceding number */
/*                 equivalent to 3 mod 4. */


/*     Subtype 1:  Lagrange interpolation, 6-element packets, order */
/*                 reduction at boundaries to preceding odd number. */


/*     Packet sizes associated with the various subtypes: */


/*     End of include file spk18.inc. */

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
/*              a SPK segment of type 18. */

/*     ET       is a target epoch, for which a data record from */
/*              a specific segment is required. */

/* $ Detailed_Output */

/*     RECORD   is a set of data from the specified segment which, */
/*              when evaluated at epoch ET, will give the state */
/*              (position and velocity) of some body, relative */
/*              to some center, in some reference frame. */

/*              The structure of the record is as follows: */

/*                 +----------------------+ */
/*                 | subtype code         | */
/*                 +----------------------+ */
/*                 | number of packets (n)| */
/*                 +----------------------+ */
/*                 | packet 1             | */
/*                 +----------------------+ */
/*                 | packet 2             | */
/*                 +----------------------+ */
/*                             . */
/*                             . */
/*                             . */
/*                 +----------------------+ */
/*                 | packet n             | */
/*                 +----------------------+ */
/*                 | epochs 1--n          | */
/*                 +----------------------+ */

/*              The packet size is a function of the subtype code. */
/*              All packets in a record have the same size. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input HANDLE does not designate a loaded SPK file, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If the segment specified by DESCR is not of data type 18, */
/*         the error SPICE(WRONGSPKTYPE) is signaled. */

/*     3)  If the input ET value is not within the range specified */
/*         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS) */
/*         is signaled. */

/*     4)  If the window size is non-positive or greater than the */
/*         maximum allowed value, the error SPICE(INVALIDVALUE) is */
/*         signaled. */

/*     5)  If the window size is not compatible with the segment */
/*         subtype, the error SPICE(INVALIDVALUE) is signaled. */

/*     6)  If the segment subtype is not recognized, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/*     7)  If the input segment contains fewer than 2 packets, the */
/*         error SPICE(TOOFEWSTATES) is signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the SPK Required Reading file for a description of the */
/*     structure of a data type 18 segment. */

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

/*           IF ( TYPE .EQ. 18 ) THEN */
/*              CALL SPKR18 ( HANDLE, DESCR, ET, RECORD ) */
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

/* $ Version */

/* -    SPICELIB Version 2.0.1, 12-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 21-DEC-2012 (NJB) */

/*        An error check was added for segment packet counts */
/*        less than 2. */

/*        An in-line comment regarding deducibility of record size from */
/*        segment subtype was removed. The comment now says the actual */
/*        count of packets in the output record is inserted into the */
/*        record. */

/* -    SPICELIB Version 1.0.0, 04-SEP-2002 (NJB) */

/* -& */
/* $ Index_Entries */

/*     read record from type_18 SPK segment */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 21-DEC-2012 (NJB) */

/*        An error check was added for segment packet counts */
/*        less than 2. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Maximum polynomial degree: */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("SPKR18", (ftnlen)6);

/*     Unpack the segment descriptor, and get the start and end addresses */
/*     of the segment. */

    dafus_(descr, &c__2, &c__6, dc, ic);
    type__ = ic[3];
    begin = ic[4];
    end = ic[5];

/*     Make sure that this really is a type 18 data segment. */

    if (type__ != 18) {
	setmsg_("You are attempting to locate type * data in a type 18 data "
		"segment.", (ftnlen)67);
	errint_("*", &type__, (ftnlen)1);
	sigerr_("SPICE(WRONGSPKTYPE)", (ftnlen)19);
	chkout_("SPKR18", (ftnlen)6);
	return 0;
    }

/*     Check the request time against the bounds in the segment */
/*     descriptor. */

    if (*et < dc[0] || *et > dc[1]) {
	setmsg_("Request time # is outside of descriptor bounds # : #.", (
		ftnlen)53);
	errdp_("#", et, (ftnlen)1);
	errdp_("#", dc, (ftnlen)1);
	errdp_("#", &dc[1], (ftnlen)1);
	sigerr_("SPICE(TIMEOUTOFBOUNDS)", (ftnlen)22);
	chkout_("SPKR18", (ftnlen)6);
	return 0;
    }
/*     We'll need the last two items before we can determine which */
/*     packets make up our output record. */

    i__1 = end - 2;
    dafgda_(handle, &i__1, &end, contrl);

/*     Check the FAILED flag just in case HANDLE is not attached to */
/*     any DAF file and the error action is not set to ABORT. You need */
/*     need to do this only after the first call to DAFGDA. */

    if (failed_()) {
	chkout_("SPKR18", (ftnlen)6);
	return 0;
    }
    subtyp = i_dnnt(contrl);
    wndsiz = i_dnnt(&contrl[1]);
    n = i_dnnt(&contrl[2]);
    if (n < 2) {
	setmsg_("Packet count # is less than the minimum valid value, which "
		"is 2.", (ftnlen)64);
	errint_("#", &n, (ftnlen)1);
	sigerr_("SPICE(TOOFEWSTATES)", (ftnlen)19);
	chkout_("SPKR18", (ftnlen)6);
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

/*               MAXDEG */


/*     Set the packet size, which is a function of the subtype. */

    if (subtyp == 0) {
	packsz = 12;
    } else if (subtyp == 1) {
	packsz = 6;
    } else {
	setmsg_("Unexpected SPK type 18 subtype # found in type 18 segment.", 
		(ftnlen)58);
	errint_("#", &subtyp, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("SPKR18", (ftnlen)6);
	return 0;
    }

/*     Check the window size. */

    if (wndsiz <= 0) {
	setmsg_("Window size in type 18 segment was #; must be positive.", (
		ftnlen)55);
	errint_("#", &subtyp, (ftnlen)1);
	sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	chkout_("SPKR18", (ftnlen)6);
	return 0;
    }
    if (subtyp == 0) {
	maxwnd = 8;
	if (wndsiz > maxwnd) {
	    setmsg_("Window size in type 18 segment was #; max allowed value"
		    " is # for subtype 0 (Hermite, 12-element packets).", (
		    ftnlen)105);
	    errint_("#", &wndsiz, (ftnlen)1);
	    errint_("#", &maxwnd, (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("SPKR18", (ftnlen)6);
	    return 0;
	}
	if (odd_(&wndsiz)) {
	    setmsg_("Window size in type 18 segment was #; must be even for "
		    "subtype 0 (Hermite, 12-element packets).", (ftnlen)95);
	    errint_("#", &wndsiz, (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("SPKR18", (ftnlen)6);
	    return 0;
	}
    } else if (subtyp == 1) {
	maxwnd = 16;
	if (wndsiz > maxwnd) {
	    setmsg_("Window size in type 18 segment was #; max allowed value"
		    " is # for subtype 1 (Lagrange, 6-element packets).", (
		    ftnlen)105);
	    errint_("#", &wndsiz, (ftnlen)1);
	    errint_("#", &maxwnd, (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("SPKR18", (ftnlen)6);
	    return 0;
	}
	if (odd_(&wndsiz)) {
	    setmsg_("Window size in type 18 segment was #; must be even for "
		    "subtype 1 (Lagrange, 6-element packets).", (ftnlen)95);
	    errint_("#", &wndsiz, (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("SPKR18", (ftnlen)6);
	    return 0;
	}
    } else {
	setmsg_("This point should not be reached. Getting here may indicate"
		" that the code needs to updated to handle new subtypes.", (
		ftnlen)114);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("SPKR18", (ftnlen)6);
	return 0;
    }

/*     We'll now select the set of packets that define the interpolating */
/*     polynomials.   We'll start out by finding the first directory */
/*     entry that is greater than or equal to the request epoch.  We'll */
/*     use the variable GROUP to indicate the set of epochs to search */
/*     within, once we've found the right directory entry. */

    ndir = (n - 1) / 100;
    dirbas = end - ndir - 3;
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
	nread = min(ndir,100);
	remain = ndir - nread;
	i__1 = bufbas + 1;
	i__2 = bufbas + nread;
	dafgda_(handle, &i__1, &i__2, buffer);
	while(buffer[(i__1 = nread - 1) < 101 && 0 <= i__1 ? i__1 : s_rnge(
		"buffer", i__1, "spkr18_", (ftnlen)524)] < *et && remain > 0) 
		{
	    bufbas += nread;
	    nread = min(remain,100);
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

/*     Now select the set of packets used for interpolation.  Note */
/*     that the window size is known to be even. */

/*     Unlike SPK types 8, 9, 12, and 13, for type 18 we adjust */
/*     the window size to keep the request time within the central */
/*     interval of the window. */

/*     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd */
/*     and (WNDSIZ/2 + 1)st of the interpolating set.  If the */
/*     request time is too close to one end of the coverage interval, */
/*     we reduce the window size, after which one endpoint of the */
/*     window will coincide with an endpoint of the coverage interval. */

/*     Let LSIZE be the size of the "left half" of the window:  the */
/*     size set of window epochs to the left of the request time. */
/*     We want this size to be WNDSIZ/2, but if not enough states are */
/*     available, the set ranges from index 1 to index LOW. */

/* Computing MIN */
    i__1 = wndsiz / 2;
    lsize = min(i__1,low);

/*     RSIZE is defined analogously for the right half of the window. */

/* Computing MIN */
    i__1 = wndsiz / 2, i__2 = n - high + 1;
    rsize = min(i__1,i__2);

/*     The window size is simply the sum of LSIZE and RSIZE. */

    wndsiz = lsize + rsize;

/*     FIRST and LAST are the endpoints of the range of indices of */
/*     time tags (and packets) we'll collect in the output record. */

    first = low - lsize + 1;
    last = first + wndsiz - 1;

/*     Put the subtype and actual window size, which is the number of */
/*     packets in the record, into the output record. */

    record[0] = (doublereal) subtyp;
    record[1] = (doublereal) wndsiz;

/*     Read the packets. */

    i__1 = begin + (first - 1) * packsz;
    i__2 = begin + last * packsz - 1;
    dafgda_(handle, &i__1, &i__2, &record[2]);

/*     Finally, add the epochs to the output record. */

    start = begin + n * packsz + first - 2;
    i__1 = start + 1;
    i__2 = start + wndsiz;
    dafgda_(handle, &i__1, &i__2, &record[wndsiz * packsz + 2]);
    chkout_("SPKR18", (ftnlen)6);
    return 0;
} /* spkr18_ */

