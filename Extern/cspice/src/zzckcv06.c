/* zzckcv06.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZCKCV06 ( Private --- C-kernel segment coverage, type 06 ) */
/* Subroutine */ int zzckcv06_(integer *handle, integer *arrbeg, integer *
	arrend, integer *sclkid, doublereal *dc, doublereal *tol, char *
	timsys, doublereal *schedl, ftnlen timsys_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    integer nrec, ndir;
    extern /* Subroutine */ int sct2e_(integer *, doublereal *, doublereal *);
    integer i__;
    doublereal begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer minie;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    logical istdb;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    extern logical failed_(void);
    doublereal et;
    integer epaddr;
    doublereal buffer[4], finish;
    integer ivlbas;
    doublereal ivlbds[2], lstepc;
    integer nivdir, ptrbas;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), wninsd_(doublereal *, 
	    doublereal *, doublereal *);
    integer nintvl;
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Determine the "window" of coverage of a type 06 C-kernel segment. */

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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     CK */
/*     UTILITY */
/*     PRIVATE */

/* $ Declarations */
/* $ Abstract */

/*     Declare parameters specific to CK type 06. */

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

/*     CK */

/* $ Keywords */

/*     CK */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     B.V. Semenov      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 10-MAR-2014 (NJB) (BVS) */

/* -& */

/*     Maximum polynomial degree supported by the current */
/*     implementation of this CK type. */


/*     Integer code indicating `true': */


/*     Integer code indicating `false': */


/*     CK type 6 subtype codes: */


/*     Subtype 0:  Hermite interpolation, 8-element packets. Quaternion */
/*                 and quaternion derivatives only, no angular velocity */
/*                 vector provided. Quaternion elements are listed */
/*                 first, followed by derivatives. Angular velocity is */
/*                 derived from the quaternions and quaternion */
/*                 derivatives. */


/*     Subtype 1:  Lagrange interpolation, 4-element packets. Quaternion */
/*                 only. Angular velocity is derived by differentiating */
/*                 the interpolating polynomials. */


/*     Subtype 2:  Hermite interpolation, 14-element packets. */
/*                 Quaternion and angular angular velocity vector, as */
/*                 well as derivatives of each, are provided. The */
/*                 quaternion comes first, then quaternion derivatives, */
/*                 then angular velocity and its derivatives. */


/*     Subtype 3:  Lagrange interpolation, 7-element packets. Quaternion */
/*                 and angular velocity vector provided.  The quaternion */
/*                 comes first. */


/*     Number of subtypes: */


/*     Packet sizes associated with the various subtypes: */


/*     Maximum packet size for type 6: */


/*     Minimum packet size for type 6: */


/*     The CKPFS record size declared in ckparam.inc must be at least as */
/*     large as the maximum possible size of a CK type 6 record. */

/*     The largest possible CK type 6 record has subtype 3 (note that */
/*     records of subtype 2 have half as many epochs as those of subtype */
/*     3, for a given polynomial degree). A subtype 3 record contains */

/*        - The evaluation epoch */
/*        - The subtype and packet count */
/*        - MAXDEG+1 packets of size C06PS3 */
/*        - MAXDEG+1 time tags */


/*     End of file ck06.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a C-kernel open for read access. */
/*     ARRBEG     I   Beginning DAF address. */
/*     ARREND     I   Ending DAF address. */
/*     SCLKID     I   ID of SCLK associated with segment. */
/*     DC         I   D.p. component of CK segment descriptor. */
/*     TOL        I   Tolerance in ticks. */
/*     TIMSYS     I   Time system used to represent coverage. */
/*     SCHEDL    I/O  An initialized window/schedule of interval */

/* $ Detailed_Input */

/*     HANDLE     is the handle of some DAF that is open for reading. */

/*     ARRBEG     is the beginning address of the type 06 segment */
/*                to be examined. */

/*     ARREND     is the ending address of the type 06 segment. */

/*     SCLKID     is the ID code of the spacecraft clock associated with */
/*                the object for which the segment contains pointing. */
/*                This is the ID code used by the SCLK conversion */
/*                routines. */

/*     DC         is the double precision component of the descriptor of */
/*                the CK segment. The components are the segment start */
/*                and stop times. */

/*                Each mini-segment interval is replaced with its */
/*                intersection with the segment coverage interval */

/*                   [ DC(1), DC(2) ] */

/*                before being expanded by TOL. Mini-segment intervals */
/*                that don't intersect the segment coverage interval are */
/*                discarded, even if after expansion by TOL they would */
/*                have non-empty intersection with the segment coverage */
/*                interval. */

/*     TOL        is a tolerance value expressed in ticks of the */
/*                spacecraft clock associated with the segment. After */
/*                truncation by the segment coverage interval, and */
/*                before insertion into the coverage window, each */
/*                non-empty truncated mini-segment interval is expanded */
/*                by TOL:  the left endpoint of each interval is reduced */
/*                by TOL and the right endpoint is increased by TOL. */
/*                Any intervals that overlap as a result of the */
/*                expansion are merged. */

/*                The coverage window returned when TOL > 0 indicates */
/*                the coverage provided by the file to the CK readers */
/*                CKGPAV and CKGP when that value of TOL is passed to */
/*                them as an input. */


/*     TIMSYS     is a string indicating the time system used in the */
/*                output coverage window.TIMSYS may have the values: */

/*                   'SCLK'    Elements of SCHEDL are expressed in */
/*                             encoded SCLK ("ticks"), where the clock */
/*                             is associated with the object designated */
/*                             by IDCODE. */

/*                   'TDB'     Elements of SCHEDL are expressed as */
/*                             seconds past J2000 TDB. */

/*                TIMSYS must be consistent with the system used for */
/*                the contents of SCHEDL on input, if any. */


/*     SCHEDL     is a schedule (window) of intervals, to which the */
/*                intervals of coverage for this segment will be added. */

/* $ Detailed_Output */

/*     SCHEDL     the input schedule updated to include the intervals */
/*                of coverage for this segment. The schedule has */
/*                been adjusted to account for the provided tolerance */
/*                value. Coverage lying outside the interval */

/*                   DC(1) - TOL : DC(2) + TOL */

/*                is excluded. */

/*                The elements of SCHEDL are given in the time system */
/*                indicated by TIMSYS. */

/* $ Parameters */

/*     Several parameters associated with the type 06 C-kernel */
/*     are utilized to compute the packet size of each subtype. */
/*     See the include file 'ck06.inc' for details. */

/* $ Exceptions */

/*     1)  The error SPICE(NOTSUPPORTED) is signaled if the subtype of */
/*         the CK type 06 segment is not recognized. */

/*     2)  Routines in the call tree of this routine may signal errors */
/*         if insufficient room in SCHEDL exists or other error */
/*         conditions relating to file access arise. */

/*     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is */
/*         signaled. */

/*     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION) */
/*         is signaled. */

/*     5)  If a time conversion error occurs, the error will be */
/*         diagnosed by a routine in the call tree of this routine. */

/* $ Files */

/*     This routine reads the contents of the file associated with */
/*     HANDLE to locate coverage intervals. */

/* $ Particulars */

/*     This is a utility routine that determines the intervals */
/*     of coverage for a type 06 C-kernel segment. */

/* $ Examples */

/*     See CKCOV. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 05-FEB-2014 (NJB) (BVS) (FST) (WLT) */

/* -& */

/*     SPICELIB Functions */


/*     Number of elements in a type 6 mini-segment */
/*     interval directory: */


/*     Mini-segment epoch directory size: */


/*     Type 6 control area size: */


/*     Type 6 mini-segment control area size: */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZCKCV06", (ftnlen)8);

/*     Check tolerance value. */

    if (*tol < 0.) {
	setmsg_("Tolerance must be non-negative; actual value was #.", (
		ftnlen)51);
	errdp_("#", tol, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZCKCV06", (ftnlen)8);
	return 0;
    }

/*     Set a logical flag indicating whether the time system is SCLK. */

    istdb = eqstr_(timsys, "TDB", timsys_len, (ftnlen)3);

/*     Check time system. */

    if (! istdb) {
	if (! eqstr_(timsys, "SCLK", timsys_len, (ftnlen)4)) {
	    setmsg_("Time system spec TIMSYS was #; allowed values are SCLK "
		    "and TDB.", (ftnlen)63);
	    errch_("#", timsys, (ftnlen)1, timsys_len);
	    sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	    chkout_("ZZCKCV06", (ftnlen)8);
	    return 0;
	}
    }

/*     Fetch the mini-segment count from the segment. */

    dafgda_(handle, arrend, arrend, buffer);
    nintvl = i_dnnt(buffer);

/*     Each mini-segment contributes a coverage interval to the */
/*     total coverage of the segment. Since mini-segments can */
/*     contain gaps, we need to examine not only the mini-segment */
/*     interval bounds but the final epochs of the mini-segments. */

/*     Let IVLBAS be the base address of the mini-segment interval */
/*     bounds. Let PTRBAS be the base address of the mini-segment */
/*     pointers. */

/*     First compute PTRBAS. There are NINTVL+1 pointers. */

    ptrbas = *arrend - 2 - (nintvl + 1);

/*     Compute the number of mini-segment interval directories. */
/*     There are NINTVL + 1 interval boundaries, so the directory */
/*     count is */

/*        (  ( NINTVL + 1 ) - 1  )  /  NVDSIZ */


    nivdir = nintvl / 100;

/*     The interval bounds and their directories precede the */
/*     mini-segment pointers. */

    ivlbas = ptrbas - nivdir - (nintvl + 1);

/*     Now loop over the mini-segments and find the contribution */
/*     from each one. */

    i__1 = nintvl;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Find the interval bounds for this mini-segment. */

	i__2 = ivlbas + i__;
	i__3 = ivlbas + i__ + 1;
	dafgda_(handle, &i__2, &i__3, ivlbds);
	if (failed_()) {
	    chkout_("ZZCKCV06", (ftnlen)8);
	    return 0;
	}

/*        Now find the last epoch of this mini-segment, since */
/*        there could be a gap at the end. */

/*        Find the begin and end pointers for the current */
/*        mini-segment. Convert these from relative to */
/*        absolute DAF addresses. */

	i__2 = ptrbas + i__;
	i__3 = ptrbas + i__ + 1;
	dafgda_(handle, &i__2, &i__3, buffer);
	if (failed_()) {
	    chkout_("ZZCKCV06", (ftnlen)8);
	    return 0;
	}
	minie = *arrbeg - 1 + i_dnnt(&buffer[1]) - 1;

/*        Fetch the mini-segment's record count NREC. */

	dafgda_(handle, &minie, &minie, buffer);
	if (failed_()) {
	    chkout_("ZZCKCV06", (ftnlen)8);
	    return 0;
	}
	nrec = i_dnnt(buffer);

/*        Compute the number of epoch directories for this */
/*        mini-segment. */

	ndir = (nrec - 1) / 100;

/*        The last epoch precedes the mini-segment control */
/*        area and the epoch directories. */

	epaddr = minie - 4 - ndir;
	dafgda_(handle, &epaddr, &epaddr, &lstepc);
	if (failed_()) {
	    chkout_("ZZCKCV06", (ftnlen)8);
	    return 0;
	}
	begin = ivlbds[0];

/*        The smaller of LSTEPC and IVLBDS(2) is the */
/*        end of the mini-segment's coverage. */

	finish = min(lstepc,ivlbds[1]);

/*        Truncate the interval using the segment bounds. */

	begin = max(begin,dc[0]);
	finish = min(finish,dc[1]);

/*        Adjust the interval using the tolerance. Empty */
/*        intervals *do not get expanded*; this choice is */
/*        consistent with the type 6 reading algorithm. */

	if (begin <= finish) {
	    if (*tol > 0.) {
/* Computing MAX */
		d__1 = begin - *tol;
		begin = max(d__1,0.);
		finish += *tol;
	    }
	}

/*        Convert the time to TDB if necessary. */

	if (istdb) {
	    sct2e_(sclkid, &begin, &et);
	    begin = et;
	    sct2e_(sclkid, &finish, &et);
	    finish = et;
	    if (failed_()) {
		chkout_("ZZCKCV06", (ftnlen)8);
		return 0;
	    }
	}

/*        Insert the interval into the window. */

	if (begin <= finish) {
	    wninsd_(&begin, &finish, schedl);
	    if (failed_()) {
		chkout_("ZZCKCV06", (ftnlen)8);
		return 0;
	    }
	}
    }
    chkout_("ZZCKCV06", (ftnlen)8);
    return 0;
} /* zzckcv06_ */

