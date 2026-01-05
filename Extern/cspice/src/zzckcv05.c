/* zzckcv05.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZCKCV05 ( Private --- C-kernel segment coverage, type 05 ) */
/* Subroutine */ int zzckcv05_(integer *handle, integer *arrbeg, integer *
	arrend, integer *sclkid, doublereal *dc, doublereal *tol, char *
	timsys, doublereal *schedl, ftnlen timsys_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    logical bail;
    integer nrec;
    doublereal tick;
    integer ndir;
    extern /* Subroutine */ int sct2e_(integer *, doublereal *, doublereal *);
    doublereal begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    logical istdb;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    integer intat, invls, rsize;
    doublereal start;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal et;
    integer intbeg;
    doublereal buffer[4];
    integer tickat;
    doublereal finish;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, doublereal *, 
	    ftnlen), wninsd_(doublereal *, doublereal *, doublereal *);
    integer lsttik, lstint;
    extern logical return_(void);
    integer subtyp;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Determine the "window" of coverage of a type 05 C-kernel segment. */

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

/*     Declare parameters specific to CK type 05. */

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

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 20-AUG-2002 (NJB) */

/* -& */

/*     CK type 5 subtype codes: */


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


/*     Packet sizes associated with the various subtypes: */


/*     End of file ck05.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a C-kernel open for read access */
/*     ARRBEG     I   Beginning DAF address */
/*     ARREND     I   Ending DAF address */
/*     SCLKID     I   ID of SCLK associated with segment. */
/*     DC         I   D.p. component of CK segment descriptor. */
/*     TOL        I   Tolerance in ticks. */
/*     TIMSYS     I   Time system used to represent coverage. */
/*     SCHEDL    I/O  An initialized window/schedule of interval */

/* $ Detailed_Input */

/*     HANDLE     is the handle of some DAF that is open for reading. */

/*     ARRBEG     is the beginning address of a type 05 segment */

/*     ARREND     is the ending address of a type 05 segment. */

/*     SCLKID     is the ID code of the spacecraft clock associated with */
/*                the object for which the segment contains pointing. */
/*                This is the ID code used by the SCLK conversion */
/*                routines. */

/*     DC         is the double precision component of the descriptor of */
/*                the CK segment.  The components are the segment start */
/*                and stop times. */

/*                Each interpolation interval is replaced with its */
/*                intersection with the segment coverage interval */

/*                   [ DC(1), DC(2) ] */

/*                before being expanded by TOL. Interpolation intervals */
/*                that don't intersect the segment coverage interval are */
/*                discarded, even if after expansion by TOL they would */
/*                have non-empty intersection with the segment coverage */
/*                interval. */

/*     TOL        is a tolerance value expressed in ticks of the */
/*                spacecraft clock associated with the segment. After */
/*                truncation by the segment coverage interval, and */
/*                before insertion into the coverage window, each */
/*                non-empty truncated interpolation interval is expanded */
/*                by TOL:  the left endpoint of each interval is reduced */
/*                by TOL and the right endpoint is increased by TOL. */
/*                Any intervals that overlap as a result of the */
/*                expansion are merged. */

/*                The coverage window returned when TOL > 0 indicates */
/*                the coverage provided by the file to the CK readers */
/*                CKGPAV and CKGP when that value of TOL is passed to */
/*                them as an input. */


/*     TIMSYS     is a string indicating the time system used in the */
/*                output coverage window.  TIMSYS may have the values: */

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
/*                of coverage for this segment.  The schedule has */
/*                been adjusted to account for the provided tolerance */
/*                value.  Coverage lying outside the interval */

/*                   DC(1) - TOL : DC(2) + TOL */

/*                is excluded. */

/*                The elements of SCHEDL are given in the time system */
/*                indicated by TIMSYS. */

/* $ Parameters */

/*     Several parameters associated with the type 05 C-kernel */
/*     are utilized to compute the packet size of each subtype. */
/*     See the include file 'ck05.inc' for details. */

/* $ Files */

/*     This routine reads the contents of the file associated with */
/*     HANDLE to locate coverage intervals. */

/* $ Exceptions */

/*     1)  The error SPICE(NOTSUPPORTED) is signaled if the subtype of */
/*         the CK type 05 segment is not recognized. */

/*     2)  Routines in the call tree of this routine may signal errors */
/*         if insufficient room in SCHEDL exists or other error */
/*         conditions relating to file access arise. */

/*     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is */
/*         signaled. */

/*     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION) */
/*         is signaled. */

/*     5)  If a time conversion error occurs, the error will be */
/*         diagnosed by a routine in the call tree of this routine. */

/* $ Particulars */

/*     This is a utility routine that determines the intervals */
/*     of coverage for a type 05 C-kernel segment. */

/* $ Examples */

/*     See CKCOV. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 07-JAN-2005 (NJB) (FST) (WLT) */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZCKCV05", (ftnlen)8);
    }

/*     Check tolerance value. */

    if (*tol < 0.) {
	setmsg_("Tolerance must be non-negative; actual value was #.", (
		ftnlen)51);
	errdp_("#", tol, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZCKCV05", (ftnlen)8);
	return 0;
    }

/*     Set a logical flag indicating whether the time systm is SCLK. */

    istdb = eqstr_(timsys, "TDB", timsys_len, (ftnlen)3);

/*     Check time system. */

    if (! istdb) {
	if (! eqstr_(timsys, "SCLK", timsys_len, (ftnlen)4)) {
	    setmsg_("Time system spec TIMSYS was #; allowed values are SCLK "
		    "and TDB.", (ftnlen)63);
	    errch_("#", timsys, (ftnlen)1, timsys_len);
	    sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	    chkout_("ZZCKCV05", (ftnlen)8);
	    return 0;
	}
    }

/*     Get the meta-data associated with this segment that we */
/*     require to produce the schedule. */

/*     BUFFER(1) = Subtype Code */
/*     BUFFER(2) = Window Size */
/*     BUFFER(3) = Number of Interpolation Intervals */
/*     BUFFER(4) = Number of Packets */

    i__1 = *arrend - 3;
    dafgda_(handle, &i__1, arrend, buffer);
    subtyp = i_dnnt(buffer);
    invls = i_dnnt(&buffer[2]);
    nrec = i_dnnt(&buffer[3]);
    ndir = (nrec - 1) / 100;

/*     Compute the packet size.  This requires parameters listed */
/*     in the include file 'ck05.inc' and is based on the subtype. */

    if (subtyp == 0) {
	rsize = 8;
    } else if (subtyp == 1) {
	rsize = 4;
    } else if (subtyp == 2) {
	rsize = 14;
    } else if (subtyp == 3) {
	rsize = 7;
    } else {
	setmsg_("CK type 5 subtype <#> is not supported.", (ftnlen)39);
	errint_("#", buffer, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZCKCV05", (ftnlen)8);
	return 0;
    }

/*     Recall that the segment is layed out as: */


/*       +------------------------------+ */
/*       |                              | */
/*       |  Pointing                    | */
/*       |                              | */
/*       +------------------------------+ */
/*       |                        | */
/*       |  SCLK times            | */
/*       |                        | */
/*       +------------------------+ */
/*       |                        | */
/*       |  SCLK directory        | */
/*       |                        | */
/*       +------------------------+ */
/*       |                        | */
/*       |  Interval start times  | */
/*       |                        | */
/*       +------------------------+ */
/*       |                        | */
/*       |  Start times directory | */
/*       |                        | */
/*       +------------------------+ */
/*       |    Seconds per tick    | */
/*       +------------------------+ */
/*       |      Subtype code      | */
/*       +------------------------+ */
/*       |      Window size       | */
/*       +------------------------+ */
/*       |                        | */
/*       |  Number of intervals   | */
/*       |                        | */
/*       +------------------------+ */
/*       |                        | */
/*       |  Number of pointing    | */
/*       |      instances         | */
/*       |                        | */
/*       +------------------------+ */

    tickat = *arrbeg + rsize * nrec;
    lsttik = tickat + nrec - 1;
    intbeg = *arrbeg + rsize * nrec + nrec + ndir;
    intat = intbeg;
    lstint = intbeg + invls - 1;
    dafgda_(handle, &intat, &intat, &start);
    dafgda_(handle, &tickat, &tickat, &tick);
    while(tick < start && tickat < lsttik) {
	++tickat;
	dafgda_(handle, &tickat, &tickat, &tick);
    }

/*     If we did not find a TICK at least as big as START, we can */
/*     just return now. */

    if (tick < start) {
	chkout_("ZZCKCV05", (ftnlen)8);
	return 0;
    }
    bail = FALSE_;
    while(intat <= lstint && tickat <= lsttik && ! bail) {

/*        At this point, we have an interval that begins at START */
/*        and ends at FINISH (unless of course we never found a "good" */
/*        TICK to start with.) */

	begin = start;

/*        If the start of the interval was the start of the LAST */
/*        interval available, we can short cut the remainder of the */
/*        reads. */

	if (intat == lstint) {
	    dafgda_(handle, &lsttik, &lsttik, &finish);
	    bail = TRUE_;

/*           The routine will return at the end of this loop */
/*           iteration.  But first, we may have to update BEGIN */
/*           and FINISH, depending on the values of TOL and TIMSYS, */
/*           and we have to insert these values into SCHEDL. */
/*           We'll carry out these tasks at the end of this IF block. */
	} else {

/*           This is the expected case.  Get the start of the next */
/*           interval. */

	    ++intat;
	    dafgda_(handle, &intat, &intat, &start);

/*           Read forward from the last tick until we reach the */
/*           START of the next interval or until we run out of TICKS. */

	    while(tick < start && tickat < lsttik) {
		finish = tick;
		++tickat;
		dafgda_(handle, &tickat, &tickat, &tick);
	    }

/*           A structurally correct CK-5 segment should never allow the */
/*           next test to pass, but it's just easier to check than */
/*           police the writers of C-kernels.  The only way to get into */
/*           the block below is if TICKAT .EQ. LSTTIK */

	    if (tick < start) {
		finish = tick;
		++tickat;
	    }
	}

/*        Truncate the interval using the segment bounds. */

	begin = max(begin,dc[0]);
	finish = min(finish,dc[1]);

/*        Adjust the interval using the tolerance.  Empty */
/*        intervals *do not get expanded*; this choice is */
/*        consistent with the type 5 reading algorithm. */

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
	}

/*        Insert the interval into the window. */

	if (begin <= finish) {
	    wninsd_(&begin, &finish, schedl);
	}
    }
    chkout_("ZZCKCV05", (ftnlen)8);
    return 0;
} /* zzckcv05_ */

