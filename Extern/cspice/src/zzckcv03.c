/* zzckcv03.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZCKCV03 ( Private --- C-kernel segment coverage, type 03 ) */
/* Subroutine */ int zzckcv03_(integer *handle, integer *arrbeg, integer *
	arrend, integer *sclkid, doublereal *tol, char *timsys, doublereal *
	schedl, ftnlen timsys_len)
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
    integer intat, avsln, invls, rsize;
    doublereal start;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal et;
    integer intbeg;
    doublereal buffer[2];
    integer seglen, tickat;
    doublereal finish;
    extern /* Subroutine */ int errhan_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen);
    integer navsln;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), wninsd_(doublereal *, doublereal *, 
	    doublereal *);
    integer lsttik, lstint;
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Determine the "window" of coverage of a type 03 C-kernel segment. */

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
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a C-kernel open for read access */
/*     ARRBEG     I   Beginning DAF address */
/*     ARREND     I   Ending DAF address */
/*     SCLKID     I   ID of SCLK associated with segment. */
/*     TOL        I   Tolerance in ticks. */
/*     TIMSYS     I   Time system used to represent coverage. */
/*     SCHEDL    I/O  An initialized window/schedule of interval */

/* $ Detailed_Input */

/*     HANDLE     is the handle of some DAF that is open for reading. */

/*     ARRBEG     is the beginning address of a type 03 segment */

/*     ARREND     is the ending address of a type 03 segment. */

/*     SCLKID     is the ID code of the spacecraft clock associated with */
/*                the object for which the segment contains pointing. */
/*                This is the ID code used by the SCLK conversion */
/*                routines. */

/*     TOL        is a tolerance value expressed in ticks of the */
/*                spacecraft clock associated with the segment. Before */
/*                each interval is inserted into the coverage window, */
/*                the intervals are expanded by TOL:  the left endpoint */
/*                of each interval is reduced by TOL and the right */
/*                endpoint is increased by TOL.  Any intervals that */
/*                overlap as a result of the expansion are merged. */

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
/*                of coverage for this segment. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     This routine reads the contents of the file associated with */
/*     HANDLE to locate coverage intervals. */

/* $ Exceptions */

/*     1) The error SPICE(BADCK3SEGMENT) is signaled if the derived */
/*        segment length from ARRBEG and ARREND does not match */
/*        the possible lengths computed from the segment metadata. */

/*     2) Routines in the call tree of this routine may signal errors */
/*        if insufficient room in SCHEDL exists or other error */
/*        conditions relating to file access arise. */

/*     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is */
/*         signaled. */

/*     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION) */
/*         is signaled. */

/*     5) If a time conversion error occurs, the error will be */
/*         diagnosed by a routine in the call tree of this routine. */

/* $ Particulars */

/*     This is a utility routine that determines the intervals */
/*     of coverage for a type 03 C-kernel segment. */

/* $ Examples */

/*     See CKBRIEF's main driver. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 03-JAN-2005 (NJB) (FST) (WLT) */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZCKCV03", (ftnlen)8);
    }

/*     Check tolerance value. */

    if (*tol < 0.) {
	setmsg_("Tolerance must be non-negative; actual value was #.", (
		ftnlen)51);
	errdp_("#", tol, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZCKCV03", (ftnlen)8);
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
	    chkout_("ZZCKCV03", (ftnlen)8);
	    return 0;
	}
    }

/*     Get the number of intervals and pointing instances ( records ) */
/*     in this segment, and from that determine the number of respective */
/*     directory epochs. */

    i__1 = *arrend - 1;
    dafgda_(handle, &i__1, arrend, buffer);
    invls = i_dnnt(buffer);
    nrec = i_dnnt(&buffer[1]);
    ndir = (nrec - 1) / 100;

/*     Determine the size of the pointing packets.  This is dependent */
/*     on whether angular rate data is present in the segment or not. */
/*     We can determine this with the following computation: */

/*     Assume a record size of 4, i.e. no angular rate data. */

    navsln = nrec * 5 + ndir + invls + (invls - 1) / 100 + 2;

/*     Assume a record size of 7, i.e. angular rate data. */

    avsln = (nrec << 3) + ndir + invls + (invls - 1) / 100 + 2;

/*     Compute the actual length of the segment. */

    seglen = *arrend - *arrbeg + 1;
    if (seglen == navsln) {
	rsize = 4;
    } else if (seglen == avsln) {
	rsize = 7;
    } else {
	setmsg_("The requested segment in file # reports a length of # d.p. "
		"numbers, but the metadata in the segment indicates the lengt"
		"h must either be # (no angular rate data) or # (angular rate"
		" data). Perhaps the segment is not type 3?", (ftnlen)221);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", &seglen, (ftnlen)1);
	errint_("#", &navsln, (ftnlen)1);
	errint_("#", &avsln, (ftnlen)1);
	sigerr_("SPICE(BADCK3SEGMENT)", (ftnlen)20);
	chkout_("ZZCKCV03", (ftnlen)8);
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
	chkout_("ZZCKCV03", (ftnlen)8);
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

/*           A structurally correct CK-3 segment should never allow the */
/*           next test to pass, but it's just easier to check than */
/*           police the writers of C-kernels.  The only way to get into */
/*           the block below is if TICKAT .EQ. LSTTIK */

	    if (tick < start) {
		finish = tick;
		++tickat;
	    }
	}

/*        Adjust the interval using the tolerance. */

	if (*tol > 0.) {
/* Computing MAX */
	    d__1 = begin - *tol;
	    begin = max(d__1,0.);
	    finish += *tol;
	}

/*        Convert the time to TDB if necessary. */

	if (istdb) {
	    sct2e_(sclkid, &begin, &et);
	    begin = et;
	    sct2e_(sclkid, &finish, &et);
	    finish = et;
	}

/*        Insert the interval into the window. */

	wninsd_(&begin, &finish, schedl);
    }
    chkout_("ZZCKCV03", (ftnlen)8);
    return 0;
} /* zzckcv03_ */

