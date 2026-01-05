/* spkw21.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__71 = 71;
static integer c__21 = 21;
static integer c__1 = 1;

/* $Procedure SPKW21 ( Write SPK segment, type 21 ) */
/* Subroutine */ int spkw21_(integer *handle, integer *body, integer *center, 
	char *frame, doublereal *first, doublereal *last, char *segid, 
	integer *n, integer *dlsize, doublereal *dlines, doublereal *epochs, 
	ftnlen frame_len, ftnlen segid_len)
{
    /* System generated locals */
    integer dlines_dim1, dlines_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal descr[5];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    errdp_(char *, doublereal *, ftnlen), dafada_(doublereal *, 
	    integer *);
    integer kqmax1;
    extern /* Subroutine */ int dafbna_(integer *, doublereal *, char *, 
	    ftnlen), dafena_(void);
    extern logical failed_(void);
    integer chrcod, refcod, maxdim;
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen);
    integer kqmloc;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    doublereal prvepc;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    integer maxdsz;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), spkpds_(
	    integer *, integer *, char *, integer *, doublereal *, doublereal 
	    *, doublereal *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Write a type 21 segment to an SPK file. */

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

/*     NAIF_IDS */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     EPHEMERIS */
/*     FILES */

/* $ Declarations */
/* $ Abstract */

/*     Declare parameters specific to SPK type 21. */

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

/* -    SPICELIB Version 1.0.0, 25-DEC-2013 (NJB) */

/* -& */

/*     MAXTRM      is the maximum number of terms allowed in each */
/*                 component of the difference table contained in a type */
/*                 21 SPK difference line. MAXTRM replaces the fixed */
/*                 table parameter value of 15 used in SPK type 1 */
/*                 segments. */

/*                 Type 21 segments have variable size. Let MAXDIM be */
/*                 the dimension of each component of the difference */
/*                 table within each difference line. Then the size */
/*                 DLSIZE of the difference line is */

/*                    ( 4 * MAXDIM ) + 11 */

/*                 MAXTRM is the largest allowed value of MAXDIM. */



/*     End of include file spk21.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of an SPK file open for writing. */
/*     BODY       I   NAIF code for an ephemeris object. */
/*     CENTER     I   NAIF code for center of motion of BODY. */
/*     FRAME      I   Reference frame name. */
/*     FIRST      I   Start time of interval covered by segment. */
/*     LAST       I   End time of interval covered by segment. */
/*     SEGID      I   Segment identifier. */
/*     N          I   Number of difference lines in segment. */
/*     DLSIZE     I   Difference line size. */
/*     DLINES     I   Array of difference lines. */
/*     EPOCHS     I   Coverage end times of difference lines. */
/*     MAXTRM     P   Maximum number of terms per difference table */
/*                    component. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of an SPK file that has been */
/*              opened for writing. */

/*     BODY     is the NAIF integer code for an ephemeris object */
/*              whose state relative to another body is described */
/*              by the segment to be created. */

/*     CENTER   is the NAIF integer code for the center of motion */
/*              of the object identified by BODY. */

/*     FRAME    is the NAIF name for a reference frame relative to */
/*              which the state information for BODY is specified. */

/*     FIRST, */
/*     LAST     are, respectively, the start and stop times of */
/*              the time interval over which the segment defines */
/*              the state of BODY. */

/*     SEGID    is the segment identifier. An SPK segment */
/*              identifier may contain up to 40 characters. */

/*     N        is the number of difference lines in the input */
/*              difference line array. */

/*     DLSIZE   is the size of each difference line data structure */
/*              in the difference line array input DLINES. Let */
/*              MAXDIM be the dimension of each component of the */
/*              difference table within each difference line. Then */
/*              the size DLSIZE of the difference line is */

/*                 ( 4 * MAXDIM ) + 11 */

/*     DLINES   contains a time-ordered array of difference lines. */
/*              The Ith difference line occupies elements (1,I) */
/*              through (MAXDIM,I) of DLINES, where MAXDIM is */
/*              as described above in the description of DLSIZE. */
/*              Each difference line represents the state (x, y, */
/*              z, dx/dt, dy/dt, dz/dt, in kilometers and */
/*              kilometers per second) of BODY relative to CENTER, */
/*              specified relative to FRAME, for an interval of */
/*              time. The time interval covered by the Ith */
/*              difference line ends at the Ith element of the */
/*              array EPOCHS (described below). The interval */
/*              covered by the first difference line starts at the */
/*              segment start time. */

/*              The contents of a difference line are as shown */
/*              below: */

/*                 Dimension  Description */
/*                 ---------  ---------------------------------- */
/*                 1          Reference epoch of difference line */
/*                 MAXDIM     Stepsize function vector */
/*                 1          Reference position vector,  x */
/*                 1          Reference velocity vector,  x */
/*                 1          Reference position vector,  y */
/*                 1          Reference velocity vector,  y */
/*                 1          Reference position vector,  z */
/*                 1          Reference velocity vector,  z */
/*                 MAXDIM,3   Modified divided difference */
/*                            arrays (MDAs) */
/*                 1          Maximum integration order plus 1 */
/*                 3          Integration order array */

/*              The reference position and velocity are those of */
/*              BODY relative to CENTER at the reference epoch. */
/*              (A difference line is essentially a polynomial */
/*              expansion of acceleration about the reference */
/*              epoch.) */

/*     EPOCHS   is an array of epochs corresponding to the members */
/*              of the difference line array. The epochs are */
/*              specified as seconds past J2000 TDB. */

/*              The first difference line covers the time interval */
/*              from the segment start time to EPOCHS(1). For */
/*              I > 1, the Ith difference line covers the half-open */
/*              time interval from, but not including, EPOCHS(I-1) */
/*              through EPOCHS(I). */

/*              The elements of EPOCHS must be strictly increasing. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     MAXTRM   is the maximum number of terms allowed in */
/*              each component of the difference table */
/*              contained in the input argument RECORD. */
/*              See the INCLUDE file spk21.inc for the value */
/*              of MAXTRM. */

/* $ Exceptions */

/*     If any of the following exceptions occur, this routine will return */
/*     without creating a new segment. */

/*     1)  If FRAME is not a recognized name, the error */
/*         SPICE(INVALIDREFFRAME) is signaled. */

/*     2)  If the last non-blank character of SEGID occurs past index 40, */
/*         the error SPICE(SEGIDTOOLONG) is signaled. */

/*     3)  If SEGID contains any nonprintable characters, the error */
/*         SPICE(NONPRINTABLECHARS) is signaled. */

/*     4)  If the number of difference lines N is not at least one, */
/*         the error SPICE(INVALIDCOUNT) is signaled. */

/*     5)  If FIRST is greater than LAST, the error */
/*         SPICE(BADDESCRTIMES) is signaled. */

/*     6)  If the elements of the array EPOCHS are not in strictly */
/*         increasing order, the error SPICE(TIMESOUTOFORDER) is */
/*         signaled. */

/*     7)  If the last epoch EPOCHS(N) is less than LAST, the error */
/*         SPICE(COVERAGEGAP) is signaled. */

/*     8)  If DLSIZE is greater than the limit */

/*            ( 4 * MAXTRM ) + 11 */

/*         the error SPICE(DIFFLINETOOLARGE) is signaled. If */
/*         DLSIZE is less than 71, the error SPICE(DIFFLINETOOSMALL) */
/*         is signaled. */

/*     9)  Let KQMAX1 be the maximum integration order for a given */
/*         difference line. If any value at index KQMAX1-1 or greater */
/*         in the step size array of that difference line is zero, the */
/*         error SPICE(ZEROSTEP) is signaled. The checked portion */
/*         of the step size vector lies in the index range 2:KQMAX-1 */
/*         of the difference line containing the vector. */

/* $ Files */

/*     A new type 21 SPK segment is written to the SPK file attached */
/*     to HANDLE. */

/* $ Particulars */

/*     This routine writes an SPK type 21 data segment to the open SPK */
/*     file according to the format described in the type 21 section of */
/*     the SPK Required Reading. The SPK file must have been opened with */
/*     write access. */

/* $ Examples */

/*     Suppose that you have difference lines and are prepared to */
/*     produce a segment of type 21 in an SPK file. */

/*     The following code fragment could be used to add the new segment */
/*     to a previously opened SPK file attached to HANDLE. The file must */
/*     have been opened with write access. */

/*        C */
/*        C     Create a segment identifier. */
/*        C */
/*              SEGID = 'MY_SAMPLE_SPK_TYPE_21_SEGMENT' */

/*        C */
/*        C     Write the segment. */
/*        C */
/*              CALL SPKW21 (  HANDLE,  BODY,    CENTER,  FRAME, */
/*             .               FIRST,   LAST,    SEGID,   N, */
/*             .               DLSIZE,  DLINES,  EPOCHS         ) */

/* $ Restrictions */

/*     1)  The validity of the difference lines is not checked by */
/*         this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 03-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 21-AUG-2015 (NJB) */

/*        Relaxed step size error check to allow zero */
/*        values in elements indexed greater than KQMAX1-2. */

/* -    SPICELIB Version 1.0.0, 03-FEB-2014 (NJB) */

/* -& */
/* $ Index_Entries */

/*     write SPK type_21 ephemeris data segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     MINDSZ is the minimum MDA size; this is the size */
/*     of type 1 MDAs. */


/*     Local variables */


/*     Local variables */


/*     Standard SPICE error handling. */

    /* Parameter adjustments */
    dlines_dim1 = *dlsize;
    dlines_offset = dlines_dim1 + 1;

    /* Function Body */
    if (return_()) {
	return 0;
    }
    chkin_("SPKW21", (ftnlen)6);

/*     Make sure the difference line size is within limits. */

    maxdsz = 111;
    if (*dlsize > maxdsz) {
	setmsg_("The input difference line size is #, while the maximum supp"
		"orted by this routine is #. It is possible that this problem"
		" is due to your SPICE Toolkit being out of date.", (ftnlen)
		167);
	errint_("#", dlsize, (ftnlen)1);
	errint_("#", &maxdsz, (ftnlen)1);
	sigerr_("SPICE(DIFFLINETOOLARGE)", (ftnlen)23);
	chkout_("SPKW21", (ftnlen)6);
	return 0;
    }
    if (*dlsize < 71) {
	setmsg_("The input difference line size is #, while the minimum supp"
		"orted by this routine is #. It is possible that this problem"
		" is due to your SPICE Toolkit being out of date.", (ftnlen)
		167);
	errint_("#", dlsize, (ftnlen)1);
	errint_("#", &c__71, (ftnlen)1);
	sigerr_("SPICE(DIFFLINETOOSMALL)", (ftnlen)23);
	chkout_("SPKW21", (ftnlen)6);
	return 0;
    }

/*     Get the NAIF integer code for the reference frame. */

    namfrm_(frame, &refcod, frame_len);
    if (refcod == 0) {
	setmsg_("The reference frame # is not supported.", (ftnlen)39);
	errch_("#", frame, (ftnlen)1, frame_len);
	sigerr_("SPICE(INVALIDREFFRAME)", (ftnlen)22);
	chkout_("SPKW21", (ftnlen)6);
	return 0;
    }

/*     Check to see if the segment identifier is too long. */

    if (lastnb_(segid, segid_len) > 40) {
	setmsg_("Segment identifier contains more than 40 characters.", (
		ftnlen)52);
	sigerr_("SPICE(SEGIDTOOLONG)", (ftnlen)19);
	chkout_("SPKW21", (ftnlen)6);
	return 0;
    }

/*     Now check that all the characters in the segment identifier */
/*     can be printed. */

    i__1 = lastnb_(segid, segid_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	chrcod = *(unsigned char *)&segid[i__ - 1];
	if (chrcod < 32 || chrcod > 126) {
	    setmsg_("The segment identifier contains nonprintable characters",
		     (ftnlen)55);
	    sigerr_("SPICE(NONPRINTABLECHARS)", (ftnlen)24);
	    chkout_("SPKW21", (ftnlen)6);
	    return 0;
	}
    }

/*     The difference line count must be at least one. */

    if (*n < 1) {
	setmsg_("The difference line count was #; the count must be at least"
		" one.", (ftnlen)64);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("SPKW21", (ftnlen)6);
	return 0;
    }

/*     The segment stop time should be greater than or equal to */
/*     the begin time. */

    if (*first > *last) {
	setmsg_("The segment start time: # is greater than the segment end t"
		"ime: #", (ftnlen)65);
	errdp_("#", first, (ftnlen)1);
	errdp_("#", last, (ftnlen)1);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("SPKW21", (ftnlen)6);
	return 0;
    }

/*     Make sure the epochs form a strictly increasing sequence. */

    prvepc = epochs[0];
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (epochs[i__ - 1] <= prvepc) {
	    setmsg_("EPOCH # having index # is not greater than its predeces"
		    "sor #.", (ftnlen)61);
	    errdp_("#", &epochs[i__ - 1], (ftnlen)1);
	    errint_("#", &i__, (ftnlen)1);
	    errdp_("#", &epochs[i__ - 2], (ftnlen)1);
	    sigerr_("SPICE(TIMESOUTOFORDER)", (ftnlen)22);
	    chkout_("SPKW21", (ftnlen)6);
	    return 0;
	}
	prvepc = epochs[i__ - 1];
    }

/*     Make sure there's no gap between the last difference line */
/*     epoch and the end of the time interval defined by the segment */
/*     descriptor. */

    if (epochs[*n - 1] < *last) {
	setmsg_("Segment has coverage gap: segment end time # follows last e"
		"poch #.", (ftnlen)66);
	errdp_("#", last, (ftnlen)1);
	errdp_("#", &epochs[*n - 1], (ftnlen)1);
	sigerr_("SPICE(COVERAGEGAP)", (ftnlen)18);
	chkout_("SPKW21", (ftnlen)6);
	return 0;
    }

/*     Check the step size vectors in the difference lines. */

    maxdim = (*dlsize - 11) / 4;
    kqmloc = (maxdim << 2) + 8;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Check only the first KQMAX1-2 elements of the step size */
/*        vector. The higher-indexed elements are allowed to be zero. */

	kqmax1 = i_dnnt(&dlines[kqmloc + i__ * dlines_dim1 - dlines_offset]);
	i__2 = kqmax1 - 1;
	for (j = 2; j <= i__2; ++j) {
	    if (dlines[j + i__ * dlines_dim1 - dlines_offset] == 0.) {
		setmsg_("Step size was zero at step size vector index # with"
			"in difference line #.", (ftnlen)72);
		i__3 = j - 1;
		errint_("#", &i__3, (ftnlen)1);
		errint_("#", &i__, (ftnlen)1);
		sigerr_("SPICE(ZEROSTEP)", (ftnlen)15);
		chkout_("SPKW21", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     If we made it this far, we're ready to start writing the segment. */

/*     Create the segment descriptor. */

    spkpds_(body, center, frame, &c__21, first, last, descr, frame_len);

/*     Begin a new segment. */

    dafbna_(handle, descr, segid, segid_len);
    if (failed_()) {
	chkout_("SPKW21", (ftnlen)6);
	return 0;
    }

/*     The type 21 segment structure is shown below: */

/*        +-----------------------+ */
/*        | Difference line 1     | */
/*        +-----------------------+ */
/*        | Difference line 2     | */
/*        +-----------------------+ */
/*                   ... */
/*        +-----------------------+ */
/*        | Difference line N     | */
/*        +-----------------------+ */
/*        | Epoch 1               | */
/*        +-----------------------+ */
/*        | Epoch 2               | */
/*        +-----------------------+ */
/*                   ... */
/*        +-----------------------+ */
/*        | Epoch N               | */
/*        +-----------------------+ */
/*        | Epoch 100             | (First directory) */
/*        +-----------------------+ */
/*                   ... */
/*        +-----------------------+ */
/*        | Epoch (N/100)*100     | (Last directory) */
/*        +-----------------------+ */
/*        | Max diff table size   | */
/*        +-----------------------+ */
/*        | Number of diff lines  | */
/*        +-----------------------+ */

    i__1 = *n * *dlsize;
    dafada_(dlines, &i__1);
    dafada_(epochs, n);
    i__1 = *n / 100;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dafada_(&epochs[i__ * 100 - 1], &c__1);
    }
    d__1 = (doublereal) maxdim;
    dafada_(&d__1, &c__1);
    d__1 = (doublereal) (*n);
    dafada_(&d__1, &c__1);

/*     As long as nothing went wrong, end the segment. */

    if (! failed_()) {
	dafena_();
    }
    chkout_("SPKW21", (ftnlen)6);
    return 0;
} /* spkw21_ */

