/* spks19.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b118 = 1.;

/* $Procedure SPKS19 ( S/P Kernel, subset, type 19 ) */
/* Subroutine */ int spks19_(integer *handle, integer *baddr, integer *eaddr, 
	doublereal *begin, doublereal *end)
{
    /* Initialized data */

    static integer pktszs[3] = { 12,6,6 };

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static doublereal data[100];
    static integer npad, isel, ndir, npkt, i__, l, nread;
    static logical final;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer minib, minie;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    errdp_(char *, doublereal *, ftnlen);
    static integer shift, nsdir, noivl, start;
    static doublereal iv1beg, iv1end;
    extern /* Subroutine */ int dafada_(doublereal *, integer *), dafgda_(
	    integer *, integer *, integer *, doublereal *);
    static integer min1sz;
    extern logical failed_(void);
    extern /* Subroutine */ int dafhfn_(integer *, char *, ftnlen);
    static integer ub;
    static doublereal ivfbeg;
    static integer begidx, bufbas;
    static doublereal ivlbeg;
    static integer minbep;
    static doublereal ivfend;
    static integer bepidx, endidx, eepidx, remain, ivlbas;
    static doublereal ivlend;
    extern integer lstled_(doublereal *, integer *, doublereal *);
    static integer minndr, ptrbas, minnpk;
    extern /* Subroutine */ int chkout_(char *, ftnlen), sigerr_(char *, 
	    ftnlen);
    static doublereal contrl[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static integer curivl;
    extern integer lstltd_(doublereal *, integer *, doublereal *);
    static integer minfsz;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    static integer nintvl, wndsiz;
    extern logical return_(void);
    static integer pktsiz, subtyp;
    static char spk[255];

/* $ Abstract */

/*     Extract a subset of the data in an SPK segment of type 19 */
/*     into a new segment. */

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
/*     DAF */

/* $ Keywords */

/*     EPHEMERIS */
/*     SPK */

/* $ Declarations */
/* $ Abstract */

/*     Declare parameters specific to SPK type 19. */

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
/*     B.V. Semenov      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 11-MAY-2015 (NJB) */

/*        Updated to support subtype 2. */

/* -    SPICELIB Version 1.0.0, 07-MAR-2014 (NJB) (BVS) */

/* -& */

/*     Maximum polynomial degree supported by the current */
/*     implementation of this SPK type. */

/*     The degree is compatible with the maximum degrees */
/*     supported by types 13 and 21. */


/*     Integer code indicating `true': */


/*     Integer code indicating `false': */


/*     SPK type 19 subtype codes: */


/*     Subtype 0:  Hermite interpolation, 12-element packets. */


/*     Subtype 1:  Lagrange interpolation, 6-element packets. */


/*     Subtype 2:  Hermite interpolation, 6-element packets. */


/*     Packet sizes associated with the various subtypes: */


/*     Number of subtypes: */


/*     Maximum packet size for type 19: */


/*     Minimum packet size for type 19: */


/*     The SPKPVN record size declared in spkrec.inc must be at least as */
/*     large as the maximum possible size of an SPK type 19 record. */

/*     The largest possible SPK type 19 record has subtype 1 (note that */
/*     records of subtype 0 have half as many epochs as those of subtype */
/*     1, for a given polynomial degree). A type 1 record contains */

/*        - The subtype and packet count */
/*        - MAXDEG+1 packets of size S19PS1 */
/*        - MAXDEG+1 time tags */


/*     End of include file spk19.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of file containing source segment. */
/*     BADDR      I   Beginning address in file of source segment. */
/*     EADDR      I   Ending address in file of source segment. */
/*     BEGIN      I   Beginning (initial epoch) of subset. */
/*     END        I   End (final epoch) of subset. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     BADDR, */
/*     EADDR    are the file handle assigned to an SPK file, and the */
/*              beginning and ending addresses of a segment within */
/*              that file. Together these identify an SPK segment */
/*              from which a subset is to be extracted. */

/*              The subset is written to a second SPK file which is */
/*              open for writing, and in which a new segment has been */
/*              started. See the $Particulars section below for */
/*              details. */

/*     BEGIN, */
/*     END      are the initial and final epochs (ephemeris time) */
/*              of the subset. */

/*              The first epoch for which there will be ephemeris */
/*              data in the new segment will be the greatest time */
/*              in the source segment that is less than or equal */
/*              to BEGIN. */

/*              The last epoch for which there will be ephemeris */
/*              data in the new segment will be the smallest time */
/*              in the source segment that is greater than or equal */
/*              to END. */

/* $ Detailed_Output */

/*     None. See $Files section. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  This routine relies on the caller to ensure that the */
/*         interval [BEGIN, END] is contained in the coverage */
/*         interval of the source segment. */

/*     2)  If BEGIN > END, no data are written to the target file. */

/*     3)  If a unexpected SPK type 19 subtype is found in the input */
/*         segment, the error SPICE(INVALIDVALUE) is signaled. */

/*     4)  The input segment must have valid structure; this */
/*         routine may fail in unpredictable ways if not. */

/* $ Files */

/*     Data are extracted from the file connected to the input */
/*     handle, and written to the current DAF open for writing. */

/*     The segment descriptor and summary must already have been written */
/*     prior to calling this routine. The segment must be ended */
/*     external to this routine. */

/* $ Particulars */

/*     This routine is intended solely for use as a utility by the */
/*     routine SPKSUB. */

/*     It transfers a subset of a type 19 SPK data segment to */
/*     a properly initialized segment of a second SPK file. */

/*     The exact structure of a segment of data type 19 is described */
/*     in the section on type 19 in the SPK Required Reading. */

/* $ Examples */

/*     This routine is intended only for use as a utility by SPKSUB. */
/*     To use this routine successfully, you must: */

/*        Open the SPK file from which to extract data. */
/*        Locate the segment from which data should be extracted. */

/*        Open the SPK file to which this data should be written. */
/*        Begin a new segment (array). */
/*        Write the summary information for the array. */

/*        Call this routine to extract the appropriate data from the */
/*        SPK open for read. */

/*        End the array to which this routine writes data. */

/*     Much of this procedure is carried out by the routine SPKSUB. The */
/*     examples of that routine illustrate more fully the process */
/*     described above. */

/* $ Restrictions */

/*     1)  This routine relies on the input segment being correct; */
/*         very limited error checking is performed on the input */
/*         data. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 03-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 2.0.0, 04-APR-2017 (NJB) */

/*        Typo in comment fixed. */

/*        11-MAY-2015 (NJB) */

/*        Updated to support subtype 2. */

/* -    SPICELIB Version 1.0.0, 17-OCT-2011 (NJB) (BVS) (WLT) (IMU) (EDW) */

/* -& */
/* $ Index_Entries */

/*     subset type_19 spk_segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Mini-segment control area size: */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKS19", (ftnlen)6);

/*     Terminology */
/*     =========== */

/*       - A point P is in the "interior" of a set S if P is */
/*         an element of S and P is not contained in the boundary */
/*         of S. If S is a discrete set of distinct times, then */
/*         the interior points of S are greater than the earliest */
/*         time in S and earlier than the latest time in S. If */
/*         S is the closed interval [A, B], that is, if S is the set of */
/*         points P such that */

/*            A  <  P  <  B */
/*               -     - */

/*         then the interior of S is the set of points P such that */

/*            A  <  P  <  B */

/*       - A subset S2 of a set S1 is in the "interior" of S1 if */
/*         every point of S2 is contained in the interior of S1. */

/*       - SPK type 19 interpolation intervals are often simply */
/*         called "intervals." */

/*       - The data set corresponding to a type 19 interpolation */
/*         interval is called a "mini-segment." */

/*       - "Padding" consists of a sequence of contiguous data packets */
/*         and a corresponding sequence of epochs provided to enable */
/*         correct interpolation near interval boundaries, where the */
/*         epochs lie outside of the interval's coverage time range. */
/*         Padding data and epochs are always drawn only from the same */
/*         input mini-segment that provides data for the output */
/*         mini-segment under construction. */

/*       - A "base address" of a structure is the DAF address preceding */
/*         the first address occupied by the structure. */


/*     Algorithm */
/*     ========= */

/*     The algorithm below transfers to the output segment sufficient */
/*     data to cover the time range BEGIN : END, such that the output */
/*     segment yields interpolation behavior identical to that of the */
/*     selected portion of the input segment. */

/*     No use is made of the selection order attribute other than to */
/*     transfer it to the output segment. This simplifies the algorithm, */
/*     at the expense of making the output segment larger than necessary */
/*     by at most a small, bounded amount. Specifically, when either */
/*     BEGIN or END coincides with an interior interval boundary, a */
/*     small additional output interval is created so as to make that */
/*     boundary an interior point of the output segment's coverage */
/*     interval. This guarantees that the correct interval can be */
/*     selected when a request time coincides with the boundary of */
/*     interest. */

/*     The overall approach is: */

/*        1)  Obtain attribute information from the input segment. */

/*        2)  Create a first output mini-segment. This mini-segment is */
/*            created using data from the first input mini-segment */
/*            having an end time greater than or equal to BEGIN. */

/*            The first output mini-segment contains padding, if needed, */
/*            on both the left and right sides. On the left side, given */
/*            a nominal interpolation window width W (W must be even), */
/*            the nominal pad size NPAD is (W/2) - 1. If I is the index */
/*            of the last time tag (in the selected input mini-segment) */
/*            less than or equal to BEGIN, the pad starts at I-NPAD or */
/*            1, whichever is greater. */

/*            On the right side, if END is greater than or equal to the */
/*            last epoch of the input mini-segment, all epochs and */
/*            packets of the input mini-segment following the first ones */
/*            selected are transferred to the output mini-segment. */

/*            The first mini-segment requires padding on the right only */
/*            if END precedes the end time of the input mini-segment. In */
/*            this case the pad size is chosen so that the output */
/*            mini-segment contains W/2 epochs greater than or equal to */
/*            END, if possible. If I is the index of the first time tag */
/*            in the mini-segment greater than or equal to END, then the */
/*            pad ends at I + (W/2) - 1 or NPKT, whichever is smaller. */

/*            Note that due to the asymmetry of the search techniques */
/*            used (there are no SPICELIB right-to-left search routines */
/*            analogous to LSTLTD and LSTLED), the implementation of the */
/*            pad computation for the right side is not as similar to */
/*            that for the left side as one might expect. */

/*            The first output mini-segment and all subsequent output */
/*            mini-segments have the structure of an SPK type 18 */
/*            segment. They consist of */

/*               a) A sequence of data packets */

/*               b) A sequence of epochs */

/*               c) An epoch directory, if needed */

/*               d) A control area, consisting of */

/*                    - A subtype code */
/*                    - An interpolation window size */
/*                    - A packet count */

/*        3)  All input mini-segments whose interpolation intervals */
/*            follow that of the first used mini-segment and whose stop */
/*            times are less than or equal to END are copied whole */
/*            to the output segment. We refer to this sequence of */
/*            mini-segments as the "middle group." The middle group may */
/*            be empty. */

/*        4)  If necessary, a final output mini-segment is written. This */
/*            mini-segment will be required unless either */

/*               - The interval of the first mini-segment contains in */
/*                 its interior the interval BEG : END. */

/*               - The middle group ends at the end of the input segment. */

/*            Note that if the last interval of the middle group ends at */
/*            END, but END is less than the final input interval's stop */
/*            time, a final mini-segment is still needed to ensure */
/*            correct interval selection. If there is no middle group */
/*            and the first used interpolation interval ends at END, and */
/*            if END is less than the final input interval's stop time, */
/*            a final mini-segment is required as well. */

/*            The interpolation interval of the final output */
/*            mini-segment always starts at an input interval boundary. */
/*            This interval has padding on the left only if the */
/*            corresponding input interval has padding on the left; any */
/*            existing left side padding from the input mini-segment is */
/*            simply copied to the output mini-segment. On the right */
/*            side, padding is created if it is necessary and possible */
/*            to do so. When right side padding is used, the pad size */
/*            and placement follow the same rules used for the right */
/*            side padding of the first output mini-segment. */

/*        5)  After all output mini-segments have been written, the */
/*            following segment-level structures are written to the */
/*            output segment: */

/*               a) The output segment interpolation interval */
/*                  boundaries. This list includes the start time of */
/*                  each output interval and the stop time of the final */
/*                  output interval. */

/*               b) The output segment interpolation interval boundary */
/*                  directory, if needed. */

/*               c) The output segment's mini-segment begin and "end" */
/*                  pointers. This list consists of the segment */
/*                  base-relative first address of each mini-segment, */
/*                  plus the relative address following the final output */
/*                  mini-segment. */

/*               d) The output segment control area. This consists of: */

/*                    - The interval selection order flag. This is copied */
/*                      from the input segment. */

/*                    - The output segment interval count */




/*     See whether there's any work to do; return immediately if not. */

    if (*begin > *end) {
	chkout_("SPKS19", (ftnlen)6);
	return 0;
    }

/*     We don't check BEGIN and END against the time bounds of the */
/*     descriptor of the input file because, according to the */
/*     SPK subsetting subsystem design, the calling routine SPKSUB */
/*     has done this already. Note that the descriptor of the source */
/*     segment is not even an input to this routine. If we wanted to, */
/*     we could search the input DAF for a descriptor that mapped to */
/*     the address range BADDR : EADDR. but we're not going to do */
/*     that. */

/*     Initialize the flag indicating the existence of the "final" */
/*     output mini-segment. */

    final = FALSE_;
/* *********************************************************************** */

/*     Part 1: Obtain attributes of the input segment */

/* *********************************************************************** */

/*     Read the input segment structure control area. */

    i__1 = *eaddr - 1;
    dafgda_(handle, &i__1, eaddr, data);
    if (failed_()) {
	chkout_("SPKS19", (ftnlen)6);
	return 0;
    }

/*     Fetch the interval selection order flag and the */
/*     number of interpolation intervals. */

    isel = i_dnnt(data);
    nintvl = i_dnnt(&data[1]);

/*     Compute the number of interval boundary directories. Recall that */
/*     the final interval stop time must be accounted for, so the */
/*     directory count is */

/*        ( ( NINTVL + 1) - 1 ) / DIRSIZ */

    ndir = nintvl / 100;

/*     Find the base address IVLBAS of the interval start times. First */
/*     set PTRBAS, which is the address preceding the interval pointers. */

/*     The interval base precedes the interval bounds, the interval */
/*     directories, the interval pointers, and the control area. */

    ptrbas = *eaddr - (nintvl + 3);
    ivlbas = ptrbas - (ndir + nintvl + 1);
/* *********************************************************************** */

/*     Part 2: Create the first output mini-segment */

/* *********************************************************************** */

/*     Search for the first interval that will contribute data to the */
/*     output segment. We first find the last interval boundary that is */
/*     strictly less than the epoch BEGIN. The final interval stop time */
/*     need not be considered, since the segment covers the interval */
/*     [BEGIN : END]. Note however there is a "corner case" in which */

/*        BEGIN == END == <final interval stop time> */

/*     Since we're only examining interval start times, the last one */
/*     we may need to read is at index NINTVL. */

    nread = min(100,nintvl);
    bufbas = ivlbas;

/*     NREAD is at least 1 here. */

    i__1 = bufbas + 1;
    i__2 = bufbas + nread;
    dafgda_(handle, &i__1, &i__2, data);
    if (failed_()) {
	chkout_("SPKS19", (ftnlen)6);
	return 0;
    }
    remain = nintvl - nread;

/*     The variable NREAD is the array index of the last */
/*     item read into the buffer on the previous read */
/*     operation. On the first pass NREAD is at least 1. */

    while(remain > 0 && data[(i__1 = nread - 1) < 100 && 0 <= i__1 ? i__1 : 
	    s_rnge("data", i__1, "spks19_", (ftnlen)578)] < *begin) {
	bufbas += nread;
	nread = min(100,remain);

/*        NREAD is at least 1 here. */

	i__1 = bufbas + 1;
	i__2 = bufbas + nread;
	dafgda_(handle, &i__1, &i__2, data);
	if (failed_()) {
	    chkout_("SPKS19", (ftnlen)6);
	    return 0;
	}
	remain -= nread;
    }

/*     Let I be the index of the last interval boundary time that */
/*     precedes BEGIN. If there are no such boundary times, I will be */
/*     zero. This latter case can happen only when the first interval */
/*     start time is equal to BEGIN. */

/*     At this point BUFBAS - IVLBAS is the number of boundaries we */
/*     examined before the final call above to DAFGDA. All of those */
/*     boundary times were strictly less than BEGIN. */

    i__ = bufbas - ivlbas + lstltd_(begin, &nread, data);

/*     Let BEGIDX be the index of the last interval start time that */
/*     precedes BEGIN, unless BEGIN coincides with the first interval */
/*     start time; in this case, BEGIDX must be 1. */

    begidx = max(1,i__);

/*     In order to extract data from the mini-segment, we'll need its */
/*     address range. */

    i__1 = ptrbas + begidx;
    i__2 = ptrbas + begidx + 1;
    dafgda_(handle, &i__1, &i__2, data);
    if (failed_()) {
	chkout_("SPKS19", (ftnlen)6);
	return 0;
    }

/*     Convert the segment-base-relative mini-segment begin and end */
/*     pointers to absolute DAF addresses. */

    minib = *baddr - 1 + i_dnnt(data);
    minie = *baddr - 1 + i_dnnt(&data[1]) - 1;

/*     Read the control area of the mini-segment. */

    bufbas = minie - 3;
    i__1 = bufbas + 1;
    i__2 = bufbas + 3;
    dafgda_(handle, &i__1, &i__2, contrl);
    if (failed_()) {
	chkout_("SPKS19", (ftnlen)6);
	return 0;
    }

/*     Fetch the control area parameters for the mini-segment. */

    subtyp = i_dnnt(contrl);
    wndsiz = i_dnnt(&contrl[1]);
    npkt = i_dnnt(&contrl[2]);

/*     Set the packet size, which is a function of the subtype. */

    if (subtyp < 0 || subtyp >= 3) {
	setmsg_("Unexpected SPK type 19 subtype # found in type 19 segment w"
		"ithin mini-segment #.", (ftnlen)80);
	errint_("#", &subtyp, (ftnlen)1);
	errint_("#", &begidx, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("SPKS19", (ftnlen)6);
	return 0;
    }
    pktsiz = pktszs[(i__1 = subtyp) < 3 && 0 <= i__1 ? i__1 : s_rnge("pktszs",
	     i__1, "spks19_", (ftnlen)668)];

/*     Determine how much of the mini-segment we need to transfer. The */
/*     first step is to find the last epoch less than or equal to BEGIN */
/*     in the mini-segment's epoch list. Let MINBEP be the base address */
/*     of the epoch list (that is, the start address minus 1). */

    minbep = minib - 1 + npkt * pktsiz;

/*     Read epochs until we find one greater than or equal to BEGIN. */

/*     It's possible that only the last epoch of the input mini-segment */
/*     satisfies this criterion, but at least one epoch must satisfy it. */

    nread = min(100,npkt);
    bufbas = minbep;
    i__1 = bufbas + 1;
    i__2 = bufbas + nread;
    dafgda_(handle, &i__1, &i__2, data);
    if (failed_()) {
	chkout_("SPKS19", (ftnlen)6);
	return 0;
    }
    remain = npkt - nread;

/*     The variable NREAD is the array index of the last */
/*     item read into the buffer on the previous read */
/*     operation. */

    while(remain > 0 && data[(i__1 = nread - 1) < 100 && 0 <= i__1 ? i__1 : 
	    s_rnge("data", i__1, "spks19_", (ftnlen)703)] < *begin) {

/*        Advance the buffer base to account for the NREAD */
/*        epochs fetched on the previous DAFGDA call. */

	bufbas += nread;
	nread = min(100,remain);

/*        Since REMAIN was positive at the beginning of this */
/*        loop iteration, NREAD is positive here. */

	i__1 = bufbas + 1;
	i__2 = bufbas + nread;
	dafgda_(handle, &i__1, &i__2, data);
	if (failed_()) {
	    chkout_("SPKS19", (ftnlen)6);
	    return 0;
	}
	remain -= nread;
    }

/*     At this point BUFBAS - MINBEP is the number of epochs in the */
/*     input mini-segment we've examined before the final call above to */
/*     DAFGDA. All of those epochs were strictly less than BEGIN. */

/*     Let BEPIDX be the index of the last epoch that precedes or is */
/*     equal to BEGIN. That epoch is contained in the last buffer we */
/*     read. */

    bepidx = bufbas - minbep + lstled_(begin, &nread, data);

/*     BEPIDX is at least 1 and may be as large as NPKT. */

/*     Compute the number of pad epochs we need to maintain proper */
/*     interpolation behavior in the neighborhood of the epoch at */
/*     index BEPIDX. */

    npad = wndsiz / 2 - 1;

/*     Shift BEPIDX by the pad amount, if possible. The minimum value */
/*     of BEPIDX is 1. */

/* Computing MAX */
    i__1 = 1, i__2 = bepidx - npad;
    bepidx = max(i__1,i__2);

/*     The output mini-segment can never have fewer than two epochs. */
/*     When the window size is 2 and BEPIDX is equal to NPKT, we */
/*     must extend the window on the left. */

/* Computing MIN */
    i__1 = bepidx, i__2 = npkt - 1;
    bepidx = min(i__1,i__2);

/*     If the input interval end time is less than or equal to END, we */
/*     need to use the rest of the data from this interval. Otherwise */
/*     find out how much data from this interval we need to transfer. */

    bufbas = ivlbas + begidx;
    i__1 = bufbas + 1;
    i__2 = bufbas + 1;
    dafgda_(handle, &i__1, &i__2, &ivlend);
    if (failed_()) {
	chkout_("SPKS19", (ftnlen)6);
	return 0;
    }

/*     Let EEPIDX be the index of the last epoch we select from */
/*     the current input mini-segment. We'll set EEPIDX below. */

    if (ivlend <= *end) {

/*        The requested subset coverage is either equal to or extends */
/*        beyond the right boundary of this interval. We'll use all data */
/*        from this interval. */

	eepidx = npkt;
    } else {

/*        IVLEND is strictly greater than END. This interval covers */

/*           [BEGIN, END]. */

/*        Read epochs from this mini-segment until we find one greater */
/*        than or equal to END. We have an error if we run out of */
/*        epochs. */

/*        The input mini-segment contains ( NPKT - BEPIDX + 1 ) epochs */
/*        following and including the one at BEPIDX. */

	remain = npkt - bepidx + 1;

/*        REMAIN is at least 2 at this point, since in this case, */
/*        some epoch exceeds END, and that epoch must have index */
/*        greater than BEPIDX. */

	nread = min(100,remain);

/*        NREAD is at least 2. */

	if (nread < 2) {

/*           This code should not be reached. */

	    dafhfn_(handle, spk, (ftnlen)255);
	    setmsg_("Input file: #. Segment address range: #:#. Structural e"
		    "rror found: NREAD is #; end time of interval # is #.", (
		    ftnlen)107);
	    errch_("#", spk, (ftnlen)1, (ftnlen)255);
	    errint_("#", baddr, (ftnlen)1);
	    errint_("#", eaddr, (ftnlen)1);
	    errint_("#", &nread, (ftnlen)1);
	    errint_("#", &begidx, (ftnlen)1);
	    errdp_("#", &ivlend, (ftnlen)1);
	    sigerr_("SPICE(SPKSTRUCTUREERROR)", (ftnlen)24);
	    chkout_("SPKS19", (ftnlen)6);
	    return 0;
	}

/*        Set the buffer base address so that we start reading at */
/*        address MINBEP + BEPIDX. */

	bufbas = minbep + bepidx - 1;
	i__1 = bufbas + 1;
	i__2 = bufbas + nread;
	dafgda_(handle, &i__1, &i__2, data);
	if (failed_()) {
	    chkout_("SPKS19", (ftnlen)6);
	    return 0;
	}
	remain -= nread;

/*        NREAD is (still) at least 2. */

	while(remain > 0 && data[(i__1 = nread - 1) < 100 && 0 <= i__1 ? i__1 
		: s_rnge("data", i__1, "spks19_", (ftnlen)846)] <= *end) {
	    bufbas += nread;
	    nread = min(remain,100);

/*           NREAD is at least 1 since REMAIN was positive */
/*           at the top of the loop. */

	    i__1 = bufbas + 1;
	    i__2 = bufbas + nread;
	    dafgda_(handle, &i__1, &i__2, data);
	    if (failed_()) {
		chkout_("SPKS19", (ftnlen)6);
		return 0;
	    }
	    remain -= nread;
	}

/*        At this point BUFBAS - MINBEP is the number of epochs in the */
/*        input mini-segment we've examined before the final call above */
/*        to DAFGDA. If this set of epochs is non-empty, all of these */
/*        epochs are less than or equal to END. Note that it's possible */
/*        for END and BEGIN to be equal to the first epoch. */

/*        Let EEPIDX be the index of the first epoch that is strictly */
/*        greater than END. As asserted above, in this branch of the */
/*        code, such an epoch must exist. That epoch is contained in the */
/*        last buffer we read. */

/*        EEPIDX exceeds by 1 the index of the last epoch less than or */
/*        equal to END. */

	l = lstled_(end, &nread, data);
	eepidx = bufbas - minbep + l + 1;

/*        EEPIDX is at least 2 and is less than or equal to NPKT. */

	if (eepidx < 2 || eepidx > npkt) {

/*           This code should not be reached. */

	    dafhfn_(handle, spk, (ftnlen)255);
	    setmsg_("Input file: #. Segment address range: #:#. Structural e"
		    "rror found: last epoch is #; end time of interval # is #."
		    , (ftnlen)112);
	    errch_("#", spk, (ftnlen)1, (ftnlen)255);
	    errint_("#", baddr, (ftnlen)1);
	    errint_("#", eaddr, (ftnlen)1);
	    errdp_("#", &data[(i__1 = nread - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("data", i__1, "spks19_", (ftnlen)899)], (ftnlen)1);
	    errint_("#", &begidx, (ftnlen)1);
	    errdp_("#", &ivlend, (ftnlen)1);
	    sigerr_("SPICE(SPKSTRUCTUREERROR)", (ftnlen)24);
	    chkout_("SPKS19", (ftnlen)6);
	    return 0;
	}

/*        Compute the number of pad epochs we need to maintain proper */
/*        interpolation behavior in the neighborhood of the epoch at */
/*        index EEPIDX. */

	if (data[(i__1 = l - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("data", 
		i__1, "spks19_", (ftnlen)913)] == *end) {

/*           The epochs at indices EEPIDX-1 and EEPIDX comprise the */
/*           first two epochs of the right half of an interpolation */
/*           window of size WNDSIZ. We need two fewer pad epochs to */
/*           complete the right half of the window. */

	    npad = wndsiz / 2 - 2;
	} else {

/*           The epoch at EEPIDX is the first of the pad. */

	    npad = wndsiz / 2 - 1;
	}

/*        The maximum allowed value of EEPIDX is NPKT. */

/* Computing MIN */
	i__1 = npkt, i__2 = eepidx + npad;
	eepidx = min(i__1,i__2);
    }

/*     At this point BEPIDX and EEPIDX are both set. */

/*     Look up the input interval's start time at index BEGIDX. */
/*     We'll use this below when we compute the interval start */
/*     time of the first output mini-segment. */

    i__1 = ivlbas + begidx;
    i__2 = ivlbas + begidx;
    dafgda_(handle, &i__1, &i__2, &ivlbeg);
    if (failed_()) {
	chkout_("SPKS19", (ftnlen)6);
	return 0;
    }
/*     We're ready to start transferring data to the output segment. The */
/*     first mini-segment of the output segment will contain packets */
/*     BEPIDX through EEPIDX of the input mini-segment at index BEGIDX. */

    i__1 = eepidx;
    for (i__ = bepidx; i__ <= i__1; ++i__) {
	bufbas = minib - 1 + (i__ - 1) * pktsiz;
	i__2 = bufbas + 1;
	i__3 = bufbas + pktsiz;
	dafgda_(handle, &i__2, &i__3, data);
	dafada_(data, &pktsiz);
	if (failed_()) {
	    chkout_("SPKS19", (ftnlen)6);
	    return 0;
	}
    }

/*     Now transfer the epochs at indices BEPIDX : EEPIDX. */

/*     Inside this loop, determine the bounds of the first output */
/*     interpolation interval. Each bound is either the corresponding */
/*     bound of the input interval, or a boundary epoch (first or last) */
/*     of the output epoch list, whichever is most restrictive. */

    i__1 = eepidx;
    for (i__ = bepidx; i__ <= i__1; ++i__) {
	bufbas = minib - 1 + npkt * pktsiz + (i__ - 1);
	i__2 = bufbas + 1;
	i__3 = bufbas + 1;
	dafgda_(handle, &i__2, &i__3, data);
	dafada_(data, &c__1);
	if (failed_()) {
	    chkout_("SPKS19", (ftnlen)6);
	    return 0;
	}

/*        Let IV1BEG be the start time of the first output */
/*        interpolation interval. Determine IV1BEG on the first loop */
/*        pass. IVLBEG has already been set to the start time of the */
/*        input interval at index BEGIDX. */

	if (i__ == bepidx) {

/*           The first output interval cannot start earlier than */
/*           the interval from which its data are taken. It may */
/*           start later. */

	    iv1beg = max(ivlbeg,data[0]);
	}

/*        Determine IV1END on the final loop pass. */

	if (i__ == eepidx) {

/*           The first output interval cannot end later than */
/*           the interval from which its data are taken. It may */
/*           end earlier. */

	    iv1end = min(ivlend,data[0]);
	}
    }

/*     Create the epoch directory for the first output mini-segment. */

    minnpk = eepidx - bepidx + 1;
    minndr = (minnpk - 1) / 100;
    i__1 = minndr;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Set BUFBAS to the address that immediately precedes the */
/*        element we're about to read. We must skip over the data */
/*        packets and the first (BEPIDX-1) epochs before starting our */
/*        count. */

	bufbas = minib - 1 + npkt * pktsiz + (bepidx - 1) + i__ * 100 - 1;
	i__2 = bufbas + 1;
	i__3 = bufbas + 1;
	dafgda_(handle, &i__2, &i__3, data);
	dafada_(data, &c__1);
	if (failed_()) {
	    chkout_("SPKS19", (ftnlen)6);
	    return 0;
	}
    }

/*     Finally, write out the control area for the first mini-segment. */

    d__1 = (doublereal) subtyp;
    dafada_(&d__1, &c__1);
    d__1 = (doublereal) wndsiz;
    dafada_(&d__1, &c__1);
    d__1 = (doublereal) minnpk;
    dafada_(&d__1, &c__1);
    if (failed_()) {
	chkout_("SPKS19", (ftnlen)6);
	return 0;
    }

/*     Compute the size of the first output mini-segment; we'll need */
/*     this later to compute the second mini-segment start pointer. */
/*     The size is the sum of the sizes of the packet set, the */
/*     epochs, the epoch directories, and the control area. */

    min1sz = minnpk * (pktsiz + 1) + minndr + 3;
/* *********************************************************************** */

/*     Part 3: Transfer the middle group of mini-segments to the */
/*             output segment, if this group is non-empty */

/* *********************************************************************** */

/*     At this point, we might already be done with copying */
/*     mini-segments. If the coverage interval of the mini-segment we */
/*     just processed contains [BEGIN, END] in its interior, we're done. */
/*     If there are no more input mini-segments, we're also done. */
/*     Otherwise, we'll continue to transfer data from subsequent */
/*     input mini-segments. */

/*     At this point IVLEND is the end time of the first input */
/*     interval. Note that this time may differ from IV1END, which */
/*     is the end time of the first output interval. */

    if (ivlend > *end || begidx == nintvl) {

/*        We've transferred all the data we need. We don't need */
/*        to obtain data from other mini-segments. */

	endidx = begidx;

/*        FINAL is already set to .FALSE. */

    } else {

/*        We need more data, and there are more data to be had. */

/*        Things get a bit easier here: all mini-segments that follow */
/*        the one we just wrote, and that have end times less than or */
/*        equal to END, get copied without modification to the output */
/*        file. Note that this sequence of mini-segments could be empty. */
	curivl = begidx + 1;

/*        Initialize the start time of the final output mini-segment. */
/*        We'll update this if we produce more output mini-segments. */

	ivfbeg = ivlend;

/*        Get the end time of the interval at index CURIVL. */

	bufbas = ivlbas + curivl;
	i__1 = bufbas + 1;
	i__2 = bufbas + 1;
	dafgda_(handle, &i__1, &i__2, &ivlend);
	if (failed_()) {
	    chkout_("SPKS19", (ftnlen)6);
	    return 0;
	}

/*        CURIVL is the index of the interval we're about to process, */
/*        and if CURIVL is in range, IVLEND is its end time. */

	while(ivlend <= *end && curivl <= nintvl) {

/*           Entering this loop means the "middle" component of the */
/*           output segment is non-empty. */

/*           Get the begin and end pointers for the current mini-segment. */

	    bufbas = ptrbas + curivl - 1;
	    i__1 = bufbas + 1;
	    i__2 = bufbas + 2;
	    dafgda_(handle, &i__1, &i__2, data);
	    if (failed_()) {
		chkout_("SPKS19", (ftnlen)6);
		return 0;
	    }
	    minib = *baddr - 1 + i_dnnt(data);
	    minie = *baddr - 1 + i_dnnt(&data[1]) - 1;

/*           Transfer all data from DAF address MINIB through DAF */
/*           address MINIE to the target SPK segment. */

	    remain = minie - minib + 1;
	    bufbas = minib - 1;
	    nread = min(100,remain);
	    while(remain > 0) {
		i__1 = bufbas + 1;
		i__2 = bufbas + nread;
		dafgda_(handle, &i__1, &i__2, data);
		dafada_(data, &nread);
		if (failed_()) {
		    chkout_("SPKS19", (ftnlen)6);
		    return 0;
		}
		remain -= nread;
		bufbas += nread;
		nread = min(100,remain);
	    }

/*           We've copied the mini-segment at index CURIVL. */

/*           Save the end time of this mini-segment in case */
/*           this one turns out NOT to be the last; in that */
/*           case this is the final interval's start time. */

	    ivfbeg = ivlend;

/*           Get the end time of the next interval, if there */
/*           is one. */

	    ++curivl;
	    if (curivl <= nintvl) {
		bufbas = ivlbas + curivl;
		i__1 = bufbas + 1;
		i__2 = bufbas + 1;
		dafgda_(handle, &i__1, &i__2, &ivlend);
		if (failed_()) {
		    chkout_("SPKS19", (ftnlen)6);
		    return 0;
		}
	    }
	}

/*        We've transferred the middle group, if it exists, to the */
/*        output segment. */

/*        If the last mini-segment we transferred isn't the last of the */
/*        input segment, we're going to copy at least a portion of the */
/*        next mini-segment to the output file. */

/*        At this point CURIVL is the index of the next interval to */
/*        process, if any. If CURIVL is valid, IVLEND is the interval's */
/*        end time. */

/* *********************************************************************** */

/*     Part 4: Create the final output mini-segment, if necessary */

/* *********************************************************************** */
	if (curivl > nintvl) {

/*           The coverage of the middle group extends to the end of */
/*           the coverage of the input segment. There's no more data to */
/*           transfer. */

/*           FINAL is already set to .FALSE. */

	    endidx = nintvl;
	} else {

/*           We're going to create one last output mini-segment. */

	    final = TRUE_;

/*           The input segment contains at least one more interpolation */
/*           interval, and the end time of this interval is greater than */
/*           END. Note that if this interval's end time were equal to */
/*           END, the interval would have been processed in the loop */
/*           above. */

	    endidx = curivl;

/*           In order to extract data from the mini-segment, we'll need */
/*           its address range. */

	    i__1 = ptrbas + endidx;
	    i__2 = ptrbas + endidx + 1;
	    dafgda_(handle, &i__1, &i__2, data);
	    if (failed_()) {
		chkout_("SPKS19", (ftnlen)6);
		return 0;
	    }
	    minib = *baddr - 1 + i_dnnt(data);
	    minie = *baddr - 1 + i_dnnt(&data[1]) - 1;

/*           Read the control area of the mini-segment. */

	    bufbas = minie - 3;
	    i__1 = bufbas + 1;
	    i__2 = bufbas + 3;
	    dafgda_(handle, &i__1, &i__2, contrl);
	    if (failed_()) {
		chkout_("SPKS19", (ftnlen)6);
		return 0;
	    }

/*           Fetch the control area parameters for the mini-segment. */

	    subtyp = i_dnnt(contrl);
	    wndsiz = i_dnnt(&contrl[1]);
	    npkt = i_dnnt(&contrl[2]);

/*           Set the packet size, which is a function of the subtype. */

	    if (subtyp < 0 || subtyp >= 3) {
		setmsg_("Unexpected SPK type 19 subtype # found in type 19 s"
			"egment within mini-segment #.", (ftnlen)80);
		errint_("#", &subtyp, (ftnlen)1);
		errint_("#", &curivl, (ftnlen)1);
		sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
		chkout_("SPKS19", (ftnlen)6);
		return 0;
	    }
	    pktsiz = pktszs[(i__1 = subtyp) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "pktszs", i__1, "spks19_", (ftnlen)1303)];

/*           Determine how much of the mini-segment we need to transfer. */
/*           The first step is to find the last epoch less than or equal */
/*           to END in the mini-segment's epoch list. Let MINBEP be the */
/*           base address of the epoch list (that is, the start address */
/*           minus 1). */

	    minbep = minib - 1 + npkt * pktsiz;

/*           Read epochs until we find one strictly greater than END. */
/*           The previous interval was the last one with an end time */
/*           less than or equal to END, so the epoch we seek should */
/*           exist. We have an error condition if it doesn't. */

	    nread = min(100,npkt);
	    bufbas = minbep;
	    i__1 = bufbas + 1;
	    i__2 = bufbas + nread;
	    dafgda_(handle, &i__1, &i__2, data);
	    if (failed_()) {
		chkout_("SPKS19", (ftnlen)6);
		return 0;
	    }
	    remain = npkt - nread;

/*           The variable NREAD is the array index of the last item read */
/*           into the buffer on the previous read operation. */

	    while(remain > 0 && data[(i__1 = nread - 1) < 100 && 0 <= i__1 ? 
		    i__1 : s_rnge("data", i__1, "spks19_", (ftnlen)1337)] <= *
		    end) {
		bufbas += nread;
		nread = min(100,remain);
		i__1 = bufbas + 1;
		i__2 = bufbas + nread;
		dafgda_(handle, &i__1, &i__2, data);
		if (failed_()) {
		    chkout_("SPKS19", (ftnlen)6);
		    return 0;
		}
		remain -= nread;
	    }

/*           At this point BUFBAS - MINBEP is the number of epochs in */
/*           the input mini-segment we've examined before the final call */
/*           above to DAFGDA. If this set of epochs is non-empty, all of */
/*           these epochs are less than or equal to END. Note that it's */
/*           possible for END and BEGIN to be equal to the first epoch. */

/*           Let EEPIDX be the index of the first epoch that is strictly */
/*           greater than END. As asserted above, in this branch of the */
/*           code, such an epoch must exist. That epoch is contained in */
/*           the last buffer we read. */

/*           EEPIDX exceeds by 1 the index of the last epoch less than */
/*           or equal to END. */

	    l = lstled_(end, &nread, data);
	    eepidx = bufbas - minbep + l + 1;

/*           EEPIDX is at least 2 and is less than or equal to NPKT. */

	    if (eepidx < 2) {

/*              This code should not be reached, since getting here */
/*              implies the first epoch of the interval is greater than */
/*              END. */

		dafhfn_(handle, spk, (ftnlen)255);
		setmsg_("Input file: #. Segment address range: #:#. Structur"
			"al error found: no epochs in final input interval ex"
			"ceed END. Interval index is #; END is #.", (ftnlen)
			143);
		errch_("#", spk, (ftnlen)1, (ftnlen)255);
		errint_("#", baddr, (ftnlen)1);
		errint_("#", eaddr, (ftnlen)1);
		errint_("#", &endidx, (ftnlen)1);
		errdp_("#", end, (ftnlen)1);
		sigerr_("SPICE(SPKSTRUCTUREERROR)", (ftnlen)24);
		chkout_("SPKS19", (ftnlen)6);
		return 0;
	    }

/*           Compute the number of pad epochs we need to maintain proper */
/*           interpolation behavior in the neighborhood of the epoch at */
/*           index EEPIDX. */

	    if (data[(i__1 = l - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("data",
		     i__1, "spks19_", (ftnlen)1404)] == *end) {

/*              The epochs at indices EEPIDX-1 and EEPIDX comprise */
/*              the first two epochs of the right half of an */
/*              interpolation window of size WNDSIZ. We need two */
/*              fewer pad epochs to complete the right half of the */
/*              window. */

		npad = wndsiz / 2 - 2;
	    } else {

/*              The epoch at EEPIDX is the first of the pad. */

		npad = wndsiz / 2 - 1;
	    }

/*           Update the final epoch index to include the pad. The index */
/*           cannot exceed the mini-segment's packet count. */

/* Computing MIN */
	    i__1 = npkt, i__2 = eepidx + npad;
	    eepidx = min(i__1,i__2);

/*           EEPIDX must always exceed BEPIDX; no interpolation */
/*           interval may have zero length. */

/*           When BEGIN is equal to END, and both are equal to the */
/*           first epoch, and the window size is 2, NPAD will be */
/*           -1, and EEPIDX will be 1. We don't want to allow */
/*           EEPIDX to be less than 2. */

	    eepidx = max(eepidx,2);

/*           EEPIDX should always be in range at this point. */

	    if (eepidx < 2 || eepidx > npkt) {

/*              This code should not be reached, since getting here */
/*              implies the first epoch of the interval is greater than */
/*              END. */

		dafhfn_(handle, spk, (ftnlen)255);
		setmsg_("Input file: #. Segment address range: #:#. BEPIDX ="
			" #; EEPIDX = #; NPKT = #.Interval index is #; END is"
			" #.", (ftnlen)106);
		errch_("#", spk, (ftnlen)1, (ftnlen)255);
		errint_("#", baddr, (ftnlen)1);
		errint_("#", eaddr, (ftnlen)1);
		errint_("#", &bepidx, (ftnlen)1);
		errint_("#", &eepidx, (ftnlen)1);
		errint_("#", &npkt, (ftnlen)1);
		errint_("#", &endidx, (ftnlen)1);
		errdp_("#", end, (ftnlen)1);
		sigerr_("SPICE(SPKSTRUCTUREERROR)", (ftnlen)24);
		chkout_("SPKS19", (ftnlen)6);
		return 0;
	    }

/*           Write the packets of the last mini-segment. */

	    i__1 = eepidx;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		bufbas = minib - 1 + (i__ - 1) * pktsiz;
		i__2 = bufbas + 1;
		i__3 = bufbas + pktsiz;
		dafgda_(handle, &i__2, &i__3, data);
		dafada_(data, &pktsiz);
		if (failed_()) {
		    chkout_("SPKS19", (ftnlen)6);
		    return 0;
		}
	    }

/*           Write the epochs of the last mini-segment. Save the */
/*           final epoch; we'll need it later. */

	    i__1 = eepidx;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		bufbas = minib - 1 + npkt * pktsiz + (i__ - 1);
		i__2 = bufbas + 1;
		i__3 = bufbas + 1;
		dafgda_(handle, &i__2, &i__3, data);
		dafada_(data, &c__1);
		if (failed_()) {
		    chkout_("SPKS19", (ftnlen)6);
		    return 0;
		}
		if (i__ == eepidx) {

/*                 The current interval is the last of the output */
/*                 segment. The interval end must be greater than or */
/*                 equal to END. It's safe to simply choose the final */
/*                 epoch as the interval end. */

		    ivfend = data[0];
		}
	    }

/*           Create epoch directories for the last mini-segment. */

	    minnpk = eepidx;
	    minndr = (minnpk - 1) / 100;
	    i__1 = minndr;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		bufbas = minib - 1 + npkt * pktsiz + i__ * 100 - 1;
		i__2 = bufbas + 1;
		i__3 = bufbas + 1;
		dafgda_(handle, &i__2, &i__3, data);
		dafada_(data, &c__1);
		if (failed_()) {
		    chkout_("SPKS19", (ftnlen)6);
		    return 0;
		}
	    }

/*           Finally, write out the control area for the last */
/*           mini-segment. */

	    d__1 = (doublereal) subtyp;
	    dafada_(&d__1, &c__1);
	    d__1 = (doublereal) wndsiz;
	    dafada_(&d__1, &c__1);
	    d__1 = (doublereal) minnpk;
	    dafada_(&d__1, &c__1);
	    if (failed_()) {
		chkout_("SPKS19", (ftnlen)6);
		return 0;
	    }

/*           Compute the size in DAF addresses of the last mini-segment. */
/*           This is the sum of the sizes of the packet space, the */
/*           epochs, the directories, and the control area. */

	    minfsz = minnpk * (pktsiz + 1) + minndr + 3;
	}

/*        We're done with the final mini-segment. */

    }

/*     We've transferred all of the data we need from mini-segments at */
/*     indices BEGIDX : ENDIDX. */
/* *********************************************************************** */

/*     Part 5: Create segment-level data structures in the output segment */

/* *********************************************************************** */

/*     Write out the interval bounds for the new segment. */

/*     Let NOIVL be the number of intervals in the output subset */
/*     segment. */

    noivl = endidx - begidx + 1;

/*     The first interval start time is IV1BEG. */

    dafada_(&iv1beg, &c__1);
    if (failed_()) {
	chkout_("SPKS19", (ftnlen)6);
	return 0;
    }

/*     Write the remaining interval boundaries. */

    if (noivl == 1) {

/*        The final interval boundary is the stop time of */
/*        the first interval. */

	dafada_(&iv1end, &c__1);
	if (failed_()) {
	    chkout_("SPKS19", (ftnlen)6);
	    return 0;
	}
    } else {

/*        There are multiple output mini-segments. There is either */
/*        a non-empty middle group, a final mini-segment, or both. */

/*        Set the upper bound of the interval boundary transfer loop. */

	if (final) {

/*           We'll transfer all interval start times up to, */
/*           but not including, the final one. */

	    ub = noivl - 1;
	} else {

/*           There's no mini-segment following the middle group. */

/*           Transfer all start times of the middle group, plus */
/*           the end time of the last interval of the middle */
/*           group. */

	    ub = noivl + 1;
	}

/*        Transfer interval boundaries from the middle group. */

	i__1 = ub;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    bufbas = ivlbas + (begidx - 1) + (i__ - 1);
	    i__2 = bufbas + 1;
	    i__3 = bufbas + 1;
	    dafgda_(handle, &i__2, &i__3, data);
	    dafada_(data, &c__1);
	    if (failed_()) {
		chkout_("SPKS19", (ftnlen)6);
		return 0;
	    }
	}

/*        If the "final" mini-segment exists, we haven't */
/*        transferred its interval boundaries. Do it now. */

	if (final) {

/*           The start and end times of the last output interpolation */
/*           interval are stored in IVFBEG and IVFEND. */

/*           Note that IVFBEG was initialized after the first output */
/*           mini-segment was written, and it was updated if necessary */
/*           in the block of code that transferred the middle group. */

	    dafada_(&ivfbeg, &c__1);
	    dafada_(&ivfend, &c__1);
	    if (failed_()) {
		chkout_("SPKS19", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     The interval boundaries have been written. */

/*     Create an interval boundary directory for the new segment. Every */
/*     boundary whose index relative to BEGIDX-1 is multiple of DIRSIZ */
/*     becomes a directory entry, unless that entry has no successors. */
/*     This implies that the interval bounds to be read belong to the */
/*     range */

/*        BEGIDX + 1  :  ENDIDX - 1 */

/*     This implies that we can read all of the directory entries */
/*     from the input segment; we won't use as directory entries */
/*     the initial or final interval bounds of the output segment. */

/*     Since the number of epoch boundaries is NOIVL + 1, the directory */
/*     count is */

/*        ( ( NOIVL + 1 ) - 1 ) / DIRSIZ */


    nsdir = noivl / 100;
    i__1 = nsdir;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Look up the interval boundary at offset I*DIRSIZ from */
/*        the boundary index BEGIDX-1. */

	bufbas = ivlbas + (begidx - 1) + i__ * 100 - 1;
	i__2 = bufbas + 1;
	i__3 = bufbas + 1;
	dafgda_(handle, &i__2, &i__3, data);

/*        Write this directory entry to the output segment. */

	dafada_(data, &c__1);
	if (failed_()) {
	    chkout_("SPKS19", (ftnlen)6);
	    return 0;
	}
    }

/*     Write out mini-segment pointers for the new segment. */

/*     The first output mini-segment ranges from relative */
/*     addresses 1 : MIN1SZ. */

    dafada_(&c_b118, &c__1);
    if (noivl == 1) {

/*        The next pointer indicates the first address after the */
/*        mini-segment, whether or not there is another mini-segment. */

/*        Note that MIN1SZ was initialized after the first output */
/*        mini-segment was written. */

	d__1 = (doublereal) (min1sz + 1);
	dafada_(&d__1, &c__1);
    } else {

/*        There are multiple output mini-segments. There is either */
/*        a non-empty middle group, a final mini-segment, or both. */

/*        We can obtain from the input segment the sizes of the */
/*        mini-segments that were copied whole. */

	start = min1sz + 1;

/*        Set the upper bound of the mini-segment pointer transfer loop. */

	if (final) {

/*           We'll transfer all mini-segment start pointers up to and */
/*           including the start pointer of the final output */

	    ub = noivl;
	} else {

/*           The middle group is non-empty, and there's no mini-segment */
/*           following the middle group. */

/*           Write all start pointers of the middle group, plus the end */
/*           pointer of the last mini-segment of the middle group. The */
/*           end pointer is the successor of the last DAF address */
/*           occupied by the mini-segment. */

	    ub = noivl + 1;
	}

/*        Write mini-segment pointers from the middle group. */

/*        All of the middle group pointers of the output segment will be */
/*        shifted relative to the corresponding pointers of the input */
/*        segment. The shift reflects the sum of the sizes of the input */
/*        mini-segments preceding the first one from which data were */
/*        transferred, as well as the amount by which the first output */
/*        mini-segment "shrank" relative to the mini-segment from which */
/*        it was created. The shift equals the difference between the */
/*        final address of the first output mini-segment and the final */
/*        address of the input mini-segment at index BEGIDX. */

	i__1 = ub;
	for (i__ = 2; i__ <= i__1; ++i__) {

/*           Look up the Ith start pointer. */

	    bufbas = ptrbas + (begidx - 1) + (i__ - 1);
	    i__2 = bufbas + 1;
	    i__3 = bufbas + 1;
	    dafgda_(handle, &i__2, &i__3, data);

/*           On the first pass, compute the pointer shift amount. */

	    if (i__ == 2) {
		shift = min1sz + 1 - i_dnnt(data);
	    }
	    start = i_dnnt(data) + shift;
	    d__1 = (doublereal) start;
	    dafada_(&d__1, &c__1);
	    if (failed_()) {
		chkout_("SPKS19", (ftnlen)6);
		return 0;
	    }
	}

/*        If the "final" mini-segment exists, we haven't */
/*        transferred its end pointer. Do it now. */

	if (final) {

/*           MINFSZ is the size of the final output mini-segment. */

/*           The end pointer of the last output mini-segment is */
/*           START+MINFSZ. The end pointer is the successor of the last */
/*           DAF address of the mini-segment. */

/*           Write the pointer. */

	    d__1 = (doublereal) (start + minfsz);
	    dafada_(&d__1, &c__1);
	}
    }

/*     Write the interval count and selection flag to the */
/*     new segment. */

    d__1 = (doublereal) isel;
    dafada_(&d__1, &c__1);
    d__1 = (doublereal) noivl;
    dafada_(&d__1, &c__1);
    chkout_("SPKS19", (ftnlen)6);
    return 0;
} /* spks19_ */

