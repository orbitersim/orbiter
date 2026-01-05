/* spkw19.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__198 = 198;
static integer c__27 = 27;
static integer c__2 = 2;
static integer c__6 = 6;
static integer c__1 = 1;

/* $Procedure SPKW19 ( Write SPK segment, type 19 ) */
/* Subroutine */ int spkw19_(integer *handle, integer *body, integer *center, 
	char *frame, doublereal *first, doublereal *last, char *segid, 
	integer *nintvl, integer *npkts, integer *subtps, integer *degres, 
	doublereal *packts, doublereal *epochs, doublereal *ivlbds, logical *
	sellst, ftnlen frame_len, ftnlen segid_len)
{
    /* Initialized data */

    static integer pktszs[3] = { 12,6,6 };

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer isel, ndir, i__, j, k;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafps_(integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    doublereal descr[5];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    integer bepix, eepix;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), dafada_(
	    doublereal *, integer *);
    doublereal dc[2];
    extern /* Subroutine */ int dafbna_(integer *, doublereal *, char *, 
	    ftnlen);
    integer ic[6];
    extern /* Subroutine */ int dafena_(void);
    extern logical failed_(void);
    integer segbeg, chrcod, refcod, segend, pktbeg;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    integer pktend;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    integer minisz;
    extern logical return_(void);
    integer pktdsz, winsiz, pktsiz, subtyp;
    extern logical odd_(integer *);

/* $ Abstract */

/*     Write a type 19 segment to an SPK file. */

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

/*     DAF */
/*     NAIF_IDS */
/*     SPC */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     EPHEMERIS */
/*     FILES */

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

/* $ Abstract */

/*     Declare SPK data record size.  This record is declared in */
/*     SPKPVN and is passed to SPK reader (SPKRxx) and evaluator */
/*     (SPKExx) routines. */

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

/*     1) If new SPK types are added, it may be necessary to */
/*        increase the size of this record.  The header of SPKPVN */
/*        should be updated as well to show the record size */
/*        requirement for each data type. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 05-OCT-2012 (NJB) */

/*        Updated to support increase of maximum degree to 27 for types */
/*        2, 3, 8, 9, 12, 13, 18, and 19. See SPKPVN for a list */
/*        of record size requirements as a function of data type. */

/* -    SPICELIB Version 1.0.0, 16-AUG-2002 (NJB) */

/* -& */

/*     End include file spkrec.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of an SPK file open for writing. */
/*     BODY       I   NAIF ID code for an ephemeris object. */
/*     CENTER     I   NAIF ID code for center of motion of BODY. */
/*     FRAME      I   Reference frame name. */
/*     FIRST      I   Start time of interval covered by segment. */
/*     LAST       I   End time of interval covered by segment. */
/*     SEGID      I   Segment identifier. */
/*     NINTVL     I   Number of mini-segments and interpolation */
/*                    intervals. */
/*     NPKTS      I   Array of packet counts of mini-segments. */
/*     SUBTPS     I   Array of segment subtypes of mini-segments. */
/*     DEGRES     I   Array of polynomial degrees of mini-segments. */
/*     PACKTS     I   Array of data packets of mini-segments. */
/*     EPOCHS     I   Array of epochs of mini-segments. */
/*     IVLBDS     I   Interpolation interval bounds. */
/*     SELLST     I   Interval selection flag. */
/*     MAXDEG     P   Maximum allowed degree of interpolating polynomial. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of an SPK file that has been opened */
/*              for writing. */

/*     BODY     is the NAIF integer code for an ephemeris object */
/*              whose state relative to another body is described */
/*              by the segment to be created. */

/*     CENTER   is the NAIF integer code for the center of motion */
/*              of the object identified by BODY. */

/*     FRAME    is the NAIF name for a reference frame */
/*              relative to which the state information for BODY */
/*              is specified. */

/*     FIRST, */
/*     LAST     are, respectively, the bounds of the time interval */
/*              over which the segment defines the state of BODY. */

/*              FIRST must be greater than or equal to the first */
/*              interpolation interval start time; LAST must be */
/*              less than or equal to the last interpolation */
/*              interval stop time. See the description of IVLBDS */
/*              below. */

/*     SEGID    is the segment identifier. An SPK segment */
/*              identifier may contain up to 40 characters. */

/*     NINTVL   is the number of interpolation intervals */
/*              associated with the input data. The interpolation */
/*              intervals are associated with data sets referred */
/*              to as "mini-segments." */

/*              The input data comprising each mini-segment are: */

/*                 - a packet count */
/*                 - a type 19 subtype */
/*                 - an interpolating polynomial degree */
/*                 - a sequence of type 19 data packets */
/*                 - a sequence of packet epochs */

/*              These inputs are described below. */

/*     NPKTS    is an array of packet counts. The Ith element of */
/*              NPKTS is the packet count of the Ith interpolation */
/*              interval/mini-segment. */

/*              NPKTS has dimension NINTVL. */

/*     SUBTPS   is an array of type 19 subtypes. The Ith element */
/*              of SUBTPS is the subtype of the packets associated */
/*              with the Ith interpolation interval/mini-segment. */

/*              SUBTPS has dimension NINTVL. */

/*     DEGRES   is an array of interpolating polynomial degrees. */
/*              The Ith element of DEGRES is the polynomial degree */
/*              of the packets associated with the Ith */
/*              interpolation interval/mini-segment. */

/*              For subtype 0, interpolation degrees must be */
/*              equivalent to 3 mod 4, that is, they must be in */
/*              the set */

/*                 { 3, 7, 11, ..., MAXDEG } */

/*              For subtype 1, interpolation degrees must be odd */
/*              and must be in the range 1:MAXDEG. */

/*              DEGRES has dimension NINTVL. */

/*     PACKTS   is an array containing data packets for all input */
/*              mini-segments. The packets for a given */
/*              mini-segment are stored contiguously in increasing */
/*              time order. The order of the sets of packets for */
/*              different mini-segments is the same as the order */
/*              of their corresponding interpolation intervals. */

/*              Each packet represents geometric states of BODY */
/*              relative to CENTER, specified relative to FRAME. */
/*              The packet structure depends on the segment */
/*              subtype as follows: */

/*                 Type 0 (indicated by code S19TP0): */

/*                     x,  y,  z,  dx/dt,  dy/dt,  dz/dt, */
/*                     vx, vy, vz, dvx/dt, dvy/dt, dvz/dt */

/*                 where x, y, z represent Cartesian position */
/*                 components and  vx, vy, vz represent Cartesian */
/*                 velocity components. Note well: vx, vy, and */
/*                 vz *are not necessarily equal* to the time */
/*                 derivatives of x, y, and z. This packet */
/*                 structure mimics that of the Rosetta/MEX orbit */
/*                 file. */

/*                 Type 1 (indicated by code S19TP1): */

/*                     x,  y,  z,  dx/dt,  dy/dt,  dz/dt */

/*                 where x, y, z represent Cartesian position */
/*                 components and  vx, vy, vz represent Cartesian */
/*                 velocity components. */


/*                 Type 2 (indicated by code S19TP2): */

/*                     Data are identical to type 1; only the */
/*                     interpolation algorithm is different. */

/*              Position units are kilometers, velocity units */
/*              are kilometers per second, and acceleration units */
/*              are kilometers per second per second. */

/*     EPOCHS   is an array containing epochs for all input */
/*              mini-segments. Each epoch is expressed as seconds */
/*              past J2000 TDB. The epochs have a one-to-one */
/*              relationship with the packets in the input packet */
/*              array. */

/*              The epochs for a given mini-segment are stored */
/*              contiguously in increasing order. The order of the */
/*              sets of epochs for different mini-segments is the */
/*              same as the order of their corresponding */
/*              interpolation intervals. */

/*              For each mini-segment, "padding" is allowed: the */
/*              sequence of epochs for that mini-segment may start */
/*              before the corresponding interpolation interval */
/*              start time and end after the corresponding */
/*              interpolation interval stop time. Padding is used */
/*              to control behavior of interpolating polynomials */
/*              near interpolation interval boundaries. */

/*              Due to possible use of padding, the elements of */
/*              EPOCHS, taken as a whole, may not be in increasing */
/*              order. */

/*     IVLBDS   is an array of interpolation interval boundary */
/*              times. This array is an ordered list of the */
/*              interpolation interval start times, to which the */
/*              the end time for the last interval is appended. */

/*              The Ith interpolation interval is the time */
/*              coverage interval of the Ith mini-segment (see the */
/*              description of NPKTS above). */

/*              For each mini-segment, the corresponding */
/*              interpolation interval's start time is greater */
/*              than or equal to the mini-segment's first epoch, */
/*              and the interval's stop time is less than or equal */
/*              to the mini-segment's last epoch. */

/*              For each interpolation interval other than the */
/*              last, the interval's coverage stop time coincides */
/*              with the coverage start time of the next interval. */
/*              There are no coverage gaps, and coverage overlap */
/*              for adjacent intervals consists of a single epoch. */

/*              IVLBDS has dimension NINTVL+1. */

/*     SELLST   is a logical flag indicating to the SPK type 19 */
/*              segment reader SPKR19 how to select the */
/*              interpolation interval when a request time */
/*              coincides with a time boundary shared by two */
/*              interpolation intervals. When SELLST ("select */
/*              last") is .TRUE., the later interval is selected; */
/*              otherwise the earlier interval is selected. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     MAXDEG   is the maximum allowed degree of the interpolating */
/*              polynomial. */

/*              See the INCLUDE file spk19.inc for the value of */
/*              MAXDEG. */

/* $ Exceptions */

/*     If any of the following exceptions occur, this routine will */
/*     return without creating a new segment. */

/*     1)  If FIRST is greater than LAST, the error */
/*         SPICE(BADDESCRTIMES) is signaled. */

/*     2)  If FRAME is not a recognized name, the error */
/*         SPICE(INVALIDREFFRAME) is signaled. */

/*     3)  If the last non-blank character of SEGID occurs past index */
/*         40, the error SPICE(SEGIDTOOLONG) is signaled. */

/*     4)  If SEGID contains any nonprintable characters, the error */
/*         SPICE(NONPRINTABLECHARS) is signaled. */

/*     5)  If NINTVL is not at least 1, the error SPICE(INVALIDCOUNT) */
/*         is signaled. */

/*     6)  If the elements of the array IVLBDS are not in strictly */
/*         increasing order, the error SPICE(BOUNDSOUTOFORDER) is */
/*         signaled. */

/*     7)  If the first interval start time IVLBDS(1) is greater than */
/*         FIRST, or if the last interval end time IVLBDS(N+1) is less */
/*         than LAST, the error SPICE(COVERAGEGAP) is signaled. */

/*     8)  If any packet count in the array NPKTS is not at least 2, the */
/*         error SPICE(TOOFEWPACKETS) is signaled. */

/*     9)  If any subtype code in the array SUBTPS is not recognized, */
/*         the error SPICE(INVALIDSUBTYPE) is signaled. */

/*     10) If any interpolation degree in the array DEGRES */
/*         is not at least 1 or is greater than MAXDEG, the */
/*         error SPICE(INVALIDDEGREE) is signaled. */

/*     11) If the window size implied by any element of the array DEGRES */
/*         is odd, the error SPICE(BADWINDOWSIZE) is signaled. */

/*     12) If the elements of the array EPOCHS corresponding to a given */
/*         mini-segment are not in strictly increasing order, the error */
/*         SPICE(TIMESOUTOFORDER) is signaled. */

/*     13) If the first epoch of a mini-segment exceeds the start */
/*         time of the associated interpolation interval, or if the */
/*         last epoch of the mini-segment precedes the end time of the */
/*         interpolation interval, the error SPICE(BOUNDSDISAGREE) */
/*         is signaled. */

/*     14) If an error occurs while writing the output segment, the error */
/*         is signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     A new type 19 SPK segment is written to the SPK file attached */
/*     to HANDLE. */

/* $ Particulars */

/*     This routine writes an SPK type 19 data segment to the open SPK */
/*     file according to the format described in the type 19 section of */
/*     the SPK Required Reading. The SPK file must have been opened with */
/*     write access. */

/* $ Examples */

/*     Suppose that you have states and are prepared to produce */
/*     a segment of type 19 in an SPK file. */

/*     The following code fragment could be used to add the new segment */
/*     to a previously opened SPK file attached to HANDLE. The file must */
/*     have been opened with write access. */

/*        C */
/*        C     Create a segment identifier. */
/*        C */
/*                  SEGID = 'MY_SAMPLE_SPK_TYPE_19_SEGMENT' */

/*        C */
/*        C     Write the segment. */
/*        C */
/*              CALL SPKW19 ( HANDLE,  BODY,    CENTER,  FRAME, */
/*             .              FIRST,   LAST,    SEGID,   NINTVL, */
/*             .              NPKTS,   SUBTPS,  DEGRES,  PACKTS, */
/*             .              EPOCHS,  IVLBDS,  SELLST           ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 03-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 21-DEC-2015 (NJB) */

/*        Updated to support subtype 2. */

/* -    SPICELIB Version 1.0.0, 05-FEB-2014 (NJB) (BVS) */

/* -& */
/* $ Index_Entries */

/*     write SPK type_19 ephemeris data segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved values */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKW19", (ftnlen)6);

/*     Start with a parameter compatibility check. */

    if (FALSE_) {
	setmsg_("SPK type 19 record size may be as large as #, but SPKPVN re"
		"cord size is #.", (ftnlen)74);
	errint_("#", &c__198, (ftnlen)1);
	errint_("#", &c__198, (ftnlen)1);
	sigerr_("SPICE(BUG0)", (ftnlen)11);
	chkout_("SPKW19", (ftnlen)6);
	return 0;
    }

/*     Make sure the segment descriptor bounds are */
/*     correctly ordered. */

    if (*last < *first) {
	setmsg_("Segment start time is #; stop time is #; bounds must be in "
		"nondecreasing order.", (ftnlen)79);
	errdp_("#", first, (ftnlen)1);
	errdp_("#", last, (ftnlen)1);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("SPKW19", (ftnlen)6);
	return 0;
    }

/*     Get the NAIF integer code for the reference frame. */

    namfrm_(frame, &refcod, frame_len);
    if (refcod == 0) {
	setmsg_("The reference frame # is not supported.", (ftnlen)39);
	errch_("#", frame, (ftnlen)1, frame_len);
	sigerr_("SPICE(INVALIDREFFRAME)", (ftnlen)22);
	chkout_("SPKW19", (ftnlen)6);
	return 0;
    }

/*     Check to see if the segment identifier is too long. */

    if (lastnb_(segid, segid_len) > 40) {
	setmsg_("Segment identifier contains more than 40 characters.", (
		ftnlen)52);
	sigerr_("SPICE(SEGIDTOOLONG)", (ftnlen)19);
	chkout_("SPKW19", (ftnlen)6);
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
	    chkout_("SPKW19", (ftnlen)6);
	    return 0;
	}
    }

/*     The mini-segment/interval count must be positive. */

    if (*nintvl < 1) {
	setmsg_("Mini-segment/interval count was #; this count must be posit"
		"ive.", (ftnlen)63);
	errint_("#", nintvl, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("SPKW19", (ftnlen)6);
	return 0;
    }

/*     Make sure the interval bounds form a strictly */
/*     increasing sequence. */

/*     Note that there are NINTVL+1 bounds. */

    i__1 = *nintvl;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (ivlbds[i__ - 1] >= ivlbds[i__]) {
	    setmsg_("Interval bounds at indices # and # are # and # respecti"
		    "vely. The difference is #. The bounds are required to be"
		    " strictly increasing.", (ftnlen)132);
	    errint_("#", &i__, (ftnlen)1);
	    i__2 = i__ + 1;
	    errint_("#", &i__2, (ftnlen)1);
	    errdp_("#", &ivlbds[i__ - 1], (ftnlen)1);
	    errdp_("#", &ivlbds[i__], (ftnlen)1);
	    d__1 = ivlbds[i__] - ivlbds[i__ - 1];
	    errdp_("#", &d__1, (ftnlen)1);
	    sigerr_("SPICE(BOUNDSOUTOFORDER)", (ftnlen)23);
	    chkout_("SPKW19", (ftnlen)6);
	    return 0;
	}
    }

/*     Make sure the time span of the descriptor doesn't extend */
/*     beyond the span of the interval bounds. */

    if (*first < ivlbds[0] || *last > ivlbds[*nintvl]) {
	setmsg_("First interval start time is #; segment start time is #; se"
		"gment stop time is #; last interval stop time is #. This seq"
		"uence of times is required to be non-decreasing: segment cov"
		"erage must be contained within the union of the interpolatio"
		"n intervals.", (ftnlen)251);
	errdp_("#", ivlbds, (ftnlen)1);
	errdp_("#", first, (ftnlen)1);
	errdp_("#", last, (ftnlen)1);
	errdp_("#", &ivlbds[*nintvl], (ftnlen)1);
	sigerr_("SPICE(COVERAGEGAP)", (ftnlen)18);
	chkout_("SPKW19", (ftnlen)6);
	return 0;
    }

/*     Check the input data before writing to the file. */

/*     This order of operations entails some redundant */
/*     calculations, but it allows for rapid error */
/*     detection. */

/*     Initialize the mini-segment packet array indices, */
/*     and those of the mini-segment epoch array as well. */

    pktbeg = 0;
    pktend = 0;
    bepix = 0;
    eepix = 0;
    i__1 = *nintvl;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        First, just make sure the packet count for the current */
/*        mini-segment is at least two. This check reduces our chances */
/*        of a subscript range violation. */

/*        Check the number of packets. */

	if (npkts[i__ - 1] < 2) {
	    setmsg_("At least 2 packets are required for SPK type 19. Number"
		    " of packets supplied was # in mini-segment at index #.", (
		    ftnlen)109);
	    errint_("#", &npkts[i__ - 1], (ftnlen)1);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(TOOFEWPACKETS)", (ftnlen)20);
	    chkout_("SPKW19", (ftnlen)6);
	    return 0;
	}

/*        Set the packet size, which is a function of the subtype. Also */
/*        set the window size. First check the subtype, which will be */
/*        used as an array index. */

	subtyp = subtps[i__ - 1];
	if (subtyp < 0 || subtyp > 2) {
	    setmsg_("Unexpected SPK type 19 subtype # found in mini-segment "
		    "#.", (ftnlen)57);
	    errint_("#", &subtyp, (ftnlen)1);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(INVALIDSUBTYPE)", (ftnlen)21);
	    chkout_("SPKW19", (ftnlen)6);
	    return 0;
	}
	pktsiz = pktszs[(i__2 = subtyp) < 3 && 0 <= i__2 ? i__2 : s_rnge(
		"pktszs", i__2, "spkw19_", (ftnlen)688)];
	if (subtyp == 0) {
	    winsiz = (degres[i__ - 1] + 1) / 2;
	} else if (subtyp == 1) {
	    winsiz = degres[i__ - 1] + 1;
	} else if (subtyp == 2) {
	    winsiz = (degres[i__ - 1] + 1) / 2;
	} else {
	    setmsg_("Subtype = #; not expected.", (ftnlen)26);
	    errint_("#", &subtyp, (ftnlen)1);
	    sigerr_("SPICE(BUG1)", (ftnlen)11);
	}

/*        Make sure that the degree of the interpolating polynomials is */
/*        in range. */

	if (degres[i__ - 1] < 1 || degres[i__ - 1] > 27) {
	    setmsg_("The interpolating polynomials of mini-segment # have de"
		    "gree #; the valid degree range is [1, #]", (ftnlen)95);
	    errint_("#", &i__, (ftnlen)1);
	    errint_("#", &degres[i__ - 1], (ftnlen)1);
	    errint_("#", &c__27, (ftnlen)1);
	    sigerr_("SPICE(INVALIDDEGREE)", (ftnlen)20);
	    chkout_("SPKW19", (ftnlen)6);
	    return 0;
	}

/*        Make sure that the window size is even. */

	if (odd_(&winsiz)) {
	    setmsg_("The interpolating polynomials of mini-segment # have wi"
		    "ndow size # and degree # for SPK type 19. The mini-segme"
		    "nt subtype is #. The degree must be equivalent to 3 mod "
		    "4 for subtype 0 (Hermite interpolation) and be odd for s"
		    "ubtype 1 (Lagrange interpolation).", (ftnlen)257);
	    errint_("#", &i__, (ftnlen)1);
	    errint_("#", &winsiz, (ftnlen)1);
	    errint_("#", &degres[i__ - 1], (ftnlen)1);
	    errint_("#", &subtps[i__ - 1], (ftnlen)1);
	    sigerr_("SPICE(BADWINDOWSIZE)", (ftnlen)20);
	    chkout_("SPKW19", (ftnlen)6);
	    return 0;
	}

/*        Make sure the epochs of the Ith mini-segment form a */
/*        strictly increasing sequence. */

/*        To start out, determine the indices of the epoch sequence */
/*        of the Ith mini-segment. We'll call the begin and end */
/*        epoch indices BEPIX and EEPIX respectively. */

	bepix = eepix + 1;
	eepix = bepix - 1 + npkts[i__ - 1];
	i__2 = npkts[i__ - 1] - 1;
	for (j = 1; j <= i__2; ++j) {
	    k = bepix + j - 1;
	    if (epochs[k - 1] >= epochs[k]) {
		setmsg_("In mini-segment #, epoch # having index # in array "
			"EPOCHS and index # in the mini-segment is greater th"
			"an or equal to its successor #.", (ftnlen)134);
		errint_("#", &i__, (ftnlen)1);
		errdp_("#", &epochs[k - 1], (ftnlen)1);
		errint_("#", &k, (ftnlen)1);
		errint_("#", &j, (ftnlen)1);
		errdp_("#", &epochs[k], (ftnlen)1);
		sigerr_("SPICE(TIMESOUTOFORDER)", (ftnlen)22);
		chkout_("SPKW19", (ftnlen)6);
		return 0;
	    }
	}

/*        Make sure that the span of the input epochs of the Ith */
/*        mini-segment includes the Ith interpolation interval. */

	if (epochs[bepix - 1] > ivlbds[i__ - 1]) {
	    setmsg_("Interpolation interval # start time # precedes mini-seg"
		    "ment's first epoch #.", (ftnlen)76);
	    errint_("#", &i__, (ftnlen)1);
	    errdp_("#", &ivlbds[i__ - 1], (ftnlen)1);
	    errdp_("#", &epochs[bepix - 1], (ftnlen)1);
	    sigerr_("SPICE(BOUNDSDISAGREE)", (ftnlen)21);
	    chkout_("SPKW19", (ftnlen)6);
	    return 0;
	} else if (epochs[eepix - 1] < ivlbds[i__]) {
	    setmsg_("Interpolation interval # end time # exceeds mini-segmen"
		    "t's last epoch #.", (ftnlen)72);
	    errint_("#", &i__, (ftnlen)1);
	    errdp_("#", &ivlbds[i__], (ftnlen)1);
	    errdp_("#", &epochs[eepix - 1], (ftnlen)1);
	    sigerr_("SPICE(BOUNDSDISAGREE)", (ftnlen)21);
	    chkout_("SPKW19", (ftnlen)6);
	    return 0;
	}
    }

/*     If we made it this far, we're ready to start writing the segment. */

/*     The type 19 segment structure is eloquently described by this */
/*     diagram from the SPK Required Reading: */

/*        +--------------------------------+ */
/*        | Interval 1 mini-segment        | */
/*        +--------------------------------+ */
/*              . */
/*              . */
/*              . */
/*        +--------------------------------+ */
/*        | Interval N mini-segment        | */
/*        +--------------------------------+ */
/*        | Interval 1 start time          | */
/*        +--------------------------------+ */
/*              . */
/*              . */
/*              . */
/*        +--------------------------------+ */
/*        | Interval N start time          | */
/*        +--------------------------------+ */
/*        | Interval N stop time           | */
/*        +--------------------------------+ */
/*        | Interval start 100             | (First interval directory) */
/*        +--------------------------------+ */
/*              . */
/*              . */
/*              . */
/*        +--------------------------------+ */
/*        | Interval start (N/100)*100     | (Last interval directory) */
/*        +--------------------------------+ */
/*        | Interval 1 start pointer       | */
/*        +--------------------------------+ */
/*              . */
/*              . */
/*              . */
/*        +--------------------------------+ */
/*        | Interval N start pointer       | */
/*        +--------------------------------+ */
/*        | Interval N stop pointer + 1    | */
/*        +--------------------------------+ */
/*        | Boundary choice flag           | */
/*        +--------------------------------+ */
/*        | Number of intervals            | */
/*        +--------------------------------+ */


/*     SPK type 19 mini-segments have the following structure: */

/*        +-----------------------+ */
/*        | Packet 1              | */
/*        +-----------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-----------------------+ */
/*        | Packet M              | */
/*        +-----------------------+ */
/*        | Epoch 1               | */
/*        +-----------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-----------------------+ */
/*        | Epoch M               | */
/*        +-----------------------+ */
/*        | Epoch 100             | (First time tag directory) */
/*        +-----------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-----------------------+ */
/*        | Epoch ((M-1)/100)*100 | (Last time tag directory) */
/*        +-----------------------+ */
/*        | Subtype code          | */
/*        +-----------------------+ */
/*        | Window size           | */
/*        +-----------------------+ */
/*        | Number of packets     | */
/*        +-----------------------+ */


/*     Create the segment descriptor. We don't use SPKPDS because */
/*     that routine doesn't allow creation of a singleton segment. */

    ic[0] = *body;
    ic[1] = *center;
    ic[2] = refcod;
    ic[3] = 19;
    dc[0] = *first;
    dc[1] = *last;
    dafps_(&c__2, &c__6, dc, ic, descr);

/*     Begin a new segment. */

    dafbna_(handle, descr, segid, segid_len);
    if (failed_()) {
	chkout_("SPKW19", (ftnlen)6);
	return 0;
    }

/*     Re-initialize the mini-segment packet array indices, */
/*     and those of the mini-segment epoch array as well. */

    pktbeg = 0;
    pktend = 0;
    bepix = 0;
    eepix = 0;

/*     Write data for each mini-segment to the file. */

    i__1 = *nintvl;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Set the packet size, which is a function of the subtype. */

	subtyp = subtps[i__ - 1];
	pktsiz = pktszs[(i__2 = subtyp) < 3 && 0 <= i__2 ? i__2 : s_rnge(
		"pktszs", i__2, "spkw19_", (ftnlen)941)];
	if (subtyp == 0) {
	    winsiz = (degres[i__ - 1] + 1) / 2;
	} else if (subtyp == 1) {
	    winsiz = degres[i__ - 1] + 1;
	} else if (subtyp == 2) {
	    winsiz = (degres[i__ - 1] + 1) / 2;
	} else {
	    setmsg_("Subtype = #; not expected.", (ftnlen)26);
	    errint_("#", &subtyp, (ftnlen)1);
	    sigerr_("SPICE(BUG2)", (ftnlen)11);
	}

/*        Now that we have the packet size, we can compute */
/*        mini-segment packet index range. We'll let PKTDSZ */
/*        be the total count of packet data entries for this */
/*        mini-segment. */

	pktdsz = npkts[i__ - 1] * pktsiz;
	pktbeg = pktend + 1;
	pktend = pktbeg - 1 + pktdsz;

/*        At this point, we're read to start writing the */
/*        current mini-segment to the file. Start with the */
/*        packet data. */

	dafada_(&packts[pktbeg - 1], &pktdsz);

/*        Write the epochs for this mini-segment. */

	bepix = eepix + 1;
	eepix = bepix - 1 + npkts[i__ - 1];
	dafada_(&epochs[bepix - 1], &npkts[i__ - 1]);

/*        Compute the number of epoch directories for the */
/*        current mini-segment. */

	ndir = (npkts[i__ - 1] - 1) / 100;

/*        Write the epoch directories to the segment. */

	i__2 = ndir;
	for (j = 1; j <= i__2; ++j) {
	    k = bepix - 1 + j * 100;
	    dafada_(&epochs[k - 1], &c__1);
	}

/*        Write the mini-segment's subtype, window size, and packet */
/*        count to the segment. */

	d__1 = (doublereal) subtps[i__ - 1];
	dafada_(&d__1, &c__1);
	d__1 = (doublereal) winsiz;
	dafada_(&d__1, &c__1);
	d__1 = (doublereal) npkts[i__ - 1];
	dafada_(&d__1, &c__1);
	if (failed_()) {
	    chkout_("SPKW19", (ftnlen)6);
	    return 0;
	}
    }

/*     We've finished writing the mini-segments. */

/*     Next write the interpolation interval bounds. */

    i__1 = *nintvl + 1;
    dafada_(ivlbds, &i__1);

/*     Create and write directories for the interval */
/*     bounds. */

/*     The directory count is the interval bound count */
/*     (N+1), minus 1, divided by the directory size. */

    ndir = *nintvl / 100;
    i__1 = ndir;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dafada_(&ivlbds[i__ * 100 - 1], &c__1);
    }

/*     Now we compute and write the start/stop pointers */
/*     for each mini-segment. */

/*     The pointers are relative to the DAF address */
/*     preceding the segment. For example, a pointer */
/*     to the first DAF address in the segment has */
/*     value 1. */

    segend = 0;
    i__1 = *nintvl;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Set the packet size, which is a function of the subtype. */

	pktsiz = pktszs[(i__2 = subtps[i__ - 1]) < 3 && 0 <= i__2 ? i__2 : 
		s_rnge("pktszs", i__2, "spkw19_", (ftnlen)1054)];

/*        In order to compute the end pointer of the current */
/*        mini-segment, we must compute the size, in terms */
/*        of DAF addresses, of this mini-segment. The formula */
/*        for the size is */

/*            size =     n_packets * packet_size */
/*                    +  n_epochs */
/*                    +  n_epoch_directories */
/*                    +  3 */

/*                 =     n_packets * ( packet_size + 1 ) */
/*                    +  ( n_packets - 1 ) / DIRSIZ */
/*                    +  3 */

	minisz = npkts[i__ - 1] * (pktsiz + 1) + (npkts[i__ - 1] - 1) / 100 + 
		3;
	segbeg = segend + 1;
	segend = segbeg + minisz - 1;

/*        Write the mini-segment begin pointer. */

/*        After the loop terminates, the final end pointer, incremented */
/*        by 1, will be written. */

	d__1 = (doublereal) segbeg;
	dafada_(&d__1, &c__1);
    }

/*     Write the last mini-segment end pointer, incremented by one. */
/*     SEGEND was computed on the last iteration of the above loop. */

    d__1 = (doublereal) (segend + 1);
    dafada_(&d__1, &c__1);

/*     Write out the interval selection flag. The input */
/*     boolean value is represented by a numeric constant. */

    if (*sellst) {
	isel = 1;
    } else {
	isel = -1;
    }
    d__1 = (doublereal) isel;
    dafada_(&d__1, &c__1);

/*     Write the mini-segment/interpolation interval count. */

    d__1 = (doublereal) (*nintvl);
    dafada_(&d__1, &c__1);

/*     End the segment. */

    dafena_();
    chkout_("SPKW19", (ftnlen)6);
    return 0;
} /* spkw19_ */

