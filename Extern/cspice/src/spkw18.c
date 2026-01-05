/* spkw18.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__15 = 15;
static integer c__2 = 2;
static integer c__6 = 6;
static integer c__1 = 1;

/* $Procedure SPKW18 ( Write SPK segment, type 18 ) */
/* Subroutine */ int spkw18_(integer *handle, integer *subtyp, integer *body, 
	integer *center, char *frame, doublereal *first, doublereal *last, 
	char *segid, integer *degree, integer *n, doublereal *packts, 
	doublereal *epochs, ftnlen frame_len, ftnlen segid_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafps_(integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    doublereal descr[5];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    errdp_(char *, doublereal *, ftnlen), dafada_(doublereal *, 
	    integer *);
    doublereal dc[2];
    extern /* Subroutine */ int dafbna_(integer *, doublereal *, char *, 
	    ftnlen);
    integer ic[6];
    extern /* Subroutine */ int dafena_(void);
    extern logical failed_(void);
    integer chrcod, refcod;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    integer packsz;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    doublereal maxtim;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    integer winsiz;
    extern logical odd_(integer *);

/* $ Abstract */

/*     Write a type 18 segment to an SPK file. */

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
/*     SPC */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     EPHEMERIS */
/*     FILES */

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
/*     HANDLE     I   Handle of an SPK file open for writing. */
/*     SUBTYP     I   SPK type 18 subtype code. */
/*     BODY       I   NAIF code for an ephemeris object. */
/*     CENTER     I   NAIF code for center of motion of BODY. */
/*     FRAME      I   Reference frame name. */
/*     FIRST      I   Start time of interval covered by segment. */
/*     LAST       I   End time of interval covered by segment. */
/*     SEGID      I   Segment identifier. */
/*     DEGREE     I   Degree of interpolating polynomials. */
/*     N          I   Number of packets. */
/*     PACKTS     I   Array of packets. */
/*     EPOCHS     I   Array of epochs corresponding to packets. */
/*     MAXDEG     P   Maximum allowed degree of interpolating polynomial. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of an SPK file that has been opened */
/*              for writing. */

/*     SUBTYP   is an integer code indicating the subtype of the */
/*              segment to be created. */

/*     BODY     is the NAIF integer code for an ephemeris object whose */
/*              state relative to another body is described by the */
/*              segment to be created. */

/*     CENTER   is the NAIF integer code for the center of motion of the */
/*              object identified by BODY. */

/*     FRAME    is the NAIF name for a reference frame relative to which */
/*              the state information for BODY is specified. */

/*     FIRST, */
/*     LAST     are, respectively, the start and stop times of the time */
/*              interval over which the segment defines the state of */
/*              BODY. */

/*     SEGID    is the segment identifier. An SPK segment identifier may */
/*              contain up to 40 characters. */

/*     DEGREE   is the nominal degree of the polynomials used to */
/*              interpolate the states contained in the input packets. */
/*              All components of the state vectors are interpolated by */
/*              polynomials of the specified degree, except near the */
/*              segment boundaries, or if the total number of states in */
/*              the segment is too few to allow interpolation using the */
/*              specified degree. */

/*              If the actual interpolation degree is reduced, the */
/*              highest degree feasible degree valid for the */
/*              interpolation type is used. */

/*     N        is the number of packets in the input packet array. */

/*     PACKTS   is a time-ordered array of data packets representing */
/*              geometric states of BODY relative to CENTER, specified */
/*              relative to FRAME. The packet structure depends on the */
/*              segment subtype as follows: */

/*                 Type 0 (indicated by code S18TP0): */

/*                    x,  y,  z,  dx/dt,  dy/dt,  dz/dt, */
/*                    vx, vy, vz, dvx/dt, dvy/dt, dvz/dt */

/*                 where x, y, z represent Cartesian position components */
/*                 and  vx, vy, vz represent Cartesian velocity */
/*                 components. Note well: vx, vy, and vz *are not */
/*                 necessarily equal* to the time derivatives of x, y, */
/*                 and z. This packet structure mimics that of the */
/*                 Rosetta/MEX orbit file from which the data are taken. */

/*                 Type 1 (indicated by code S18TP1): */

/*                    x,  y,  z,  dx/dt,  dy/dt,  dz/dt */

/*                 where x, y, z represent Cartesian position components */
/*                 and  vx, vy, vz represent Cartesian velocity */
/*                 components. */

/*              Position units are kilometers, velocity units are */
/*              kilometers per second, and acceleration units are */
/*              kilometers per second per second. */

/*     EPOCHS   is an array of epochs corresponding to the members of the */
/*              packets array. The epochs are specified as seconds past */
/*              J2000, TDB. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     MAXDEG   is the maximum allowed degree of the interpolating */
/*              polynomial. If the value of MAXDEG is increased, the */
/*              SPICELIB routine SPKPVN must be changed accordingly. In */
/*              particular, the size of the record passed to SPKRnn and */
/*              SPKEnn must be increased, and comments describing the */
/*              record size must be changed. */

/* $ Exceptions */

/*     If any of the following exceptions occur, this routine will */
/*     return without creating a new segment. */

/*     1)  If FRAME is not a recognized name, the error */
/*         SPICE(INVALIDREFFRAME) is signaled. */

/*     2)  If the last non-blank character of SEGID occurs past index 40, */
/*         the error SPICE(SEGIDTOOLONG) is signaled. */

/*     3)  If SEGID contains any nonprintable characters, the error */
/*         SPICE(NONPRINTABLECHARS) is signaled. */

/*     4)  If DEGREE is not at least 1 or is greater than MAXDEG, the */
/*         error SPICE(INVALIDDEGREE) is signaled. */

/*     5)  If the window size implied by DEGREE is odd, the error */
/*         SPICE(INVALIDDEGREE) is signaled. */

/*     6)  If the number of packets N is not at least 2, the error */
/*         SPICE(TOOFEWSTATES) is signaled. */

/*     7)  If FIRST is greater than or equal to LAST, the error */
/*         SPICE(BADDESCRTIMES) is signaled. */

/*     8)  If the elements of the array EPOCHS are not in strictly */
/*         increasing order, the error SPICE(TIMESOUTOFORDER) is */
/*         signaled. */

/*     9)  If the first epoch EPOCHS(1) is greater than FIRST, the error */
/*         SPICE(BADDESCRTIMES) is signaled. */

/*     10) If the last epoch EPOCHS(N) is less than LAST, the error */
/*         SPICE(BADDESCRTIMES) is signaled. */

/*     11) If the subtype code is not recognized, the error */
/*         SPICE(INVALIDVALUE) is signaled. */

/* $ Files */

/*     A new type 18 SPK segment is written to the SPK file attached */
/*     to HANDLE. */

/* $ Particulars */

/*     This routine writes an SPK type 18 data segment to the open SPK */
/*     file according to the format described in the type 18 section of */
/*     the SPK Required Reading. The SPK file must have been opened with */
/*     write access. */

/* $ Examples */

/*     Suppose that you have states and are prepared to produce */
/*     a segment of type 18 in an SPK file. */

/*     The following code fragment could be used to add the new segment */
/*     to a previously opened SPK file attached to HANDLE. The file must */
/*     have been opened with write access. */

/*        C */
/*        C     Create a segment identifier. */
/*        C */
/*                  SEGID = 'MY_SAMPLE_SPK_TYPE_18_SEGMENT' */

/*        C */
/*        C     Write the segment. */
/*        C */
/*              CALL SPKW18 (  HANDLE,  BODY,    CENTER,  FRAME, */
/*             .               FIRST,   LAST,    SEGID,   DEGREE, */
/*             .               N,       STATES,  EPOCHS          ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 09-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 21-DEC-2012 (NJB) */

/*        Increased the minimum packet count from 1 to 2. */

/* -    SPICELIB Version 1.0.1, 29-APR-2003 (NJB) */

/*        Description of error condition arising from invalid window */
/*        size was corrected. */

/* -    SPICELIB Version 1.0.0, 13-MAY-2002 (NJB) */

/* -& */
/* $ Index_Entries */

/*     write SPK type_18 ephemeris data segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKW18", (ftnlen)6);
    }

/*     Set the packet size, which is a function of the subtype. */

    if (*subtyp == 0) {
	packsz = 12;
    } else if (*subtyp == 1) {
	packsz = 6;
    } else {
	setmsg_("Unexpected SPK type 18 subtype requested: #", (ftnlen)43);
	errint_("#", subtyp, (ftnlen)1);
	sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	chkout_("SPKW18", (ftnlen)6);
	return 0;
    }

/*     Set the window size corresponding to the input degree.  This */
/*     size will be used in various places below. */

    if (*subtyp == 0) {
	winsiz = (*degree + 1) / 2;
    } else if (*subtyp == 1) {
	winsiz = *degree + 1;
    } else {
	setmsg_("This point should not be reached. Getting here may indicate"
		" that the code needs to updated to handle new subtypes.", (
		ftnlen)114);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("SPKW18", (ftnlen)6);
	return 0;
    }

/*     Get the NAIF integer code for the reference frame. */

    namfrm_(frame, &refcod, frame_len);
    if (refcod == 0) {
	setmsg_("The reference frame # is not supported.", (ftnlen)39);
	errch_("#", frame, (ftnlen)1, frame_len);
	sigerr_("SPICE(INVALIDREFFRAME)", (ftnlen)22);
	chkout_("SPKW18", (ftnlen)6);
	return 0;
    }

/*     Check to see if the segment identifier is too long. */

    if (lastnb_(segid, segid_len) > 40) {
	setmsg_("Segment identifier contains more than 40 characters.", (
		ftnlen)52);
	sigerr_("SPICE(SEGIDTOOLONG)", (ftnlen)19);
	chkout_("SPKW18", (ftnlen)6);
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
	    chkout_("SPKW18", (ftnlen)6);
	    return 0;
	}
    }

/*     Make sure that the degree of the interpolating polynomials is */
/*     in range. */

    if (*degree < 1 || *degree > 15) {
	setmsg_("The interpolating polynomials have degree #; the valid degr"
		"ee range is [1, #]", (ftnlen)77);
	errint_("#", degree, (ftnlen)1);
	errint_("#", &c__15, (ftnlen)1);
	sigerr_("SPICE(INVALIDDEGREE)", (ftnlen)20);
	chkout_("SPKW18", (ftnlen)6);
	return 0;
    }

/*     Make sure that the window size is even.  If not, the input */
/*     DEGREE is incompatible with the subtype. */

    if (odd_(&winsiz)) {
	setmsg_("The interpolating polynomials have degree #; for SPK type 1"
		"8, the degree must be equivalent to 3 mod 4 for Hermite inte"
		"rpolation and odd for for Lagrange interpolation.", (ftnlen)
		168);
	errint_("#", degree, (ftnlen)1);
	sigerr_("SPICE(INVALIDDEGREE)", (ftnlen)20);
	chkout_("SPKW18", (ftnlen)6);
	return 0;
    }

/*     Make sure that the number of packets is sufficient to define a */
/*     polynomial whose degree is DEGREE. */

    if (*n < 2) {
	setmsg_("At least 2 packets are required for SPK type 18.  Number of"
		" packets supplied:  #", (ftnlen)80);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(TOOFEWSTATES)", (ftnlen)19);
	chkout_("SPKW18", (ftnlen)6);
	return 0;
    }

/*     The segment stop time should be greater than or equal to */
/*     the begin time. */

    if (*first > *last) {
	setmsg_("The segment start time: # is greater then the segment end t"
		"ime: #", (ftnlen)65);
	errdp_("#", first, (ftnlen)1);
	errdp_("#", last, (ftnlen)1);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("SPKW18", (ftnlen)6);
	return 0;
    }

/*     Make sure the epochs form a strictly increasing sequence. */

    maxtim = epochs[0];
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (epochs[i__ - 1] <= maxtim) {
	    setmsg_("EPOCH # having index # is not greater than its predeces"
		    "sor #.", (ftnlen)61);
	    errdp_("#", &epochs[i__ - 1], (ftnlen)1);
	    errint_("#", &i__, (ftnlen)1);
	    errdp_("#", &epochs[i__ - 2], (ftnlen)1);
	    sigerr_("SPICE(TIMESOUTOFORDER)", (ftnlen)22);
	    chkout_("SPKW18", (ftnlen)6);
	    return 0;
	} else {
	    maxtim = epochs[i__ - 1];
	}
    }

/*     Make sure that the span of the input epochs includes the interval */
/*     defined by the segment descriptor. */

    if (epochs[0] > *first) {
	setmsg_("Segment start time # precedes first epoch #.", (ftnlen)44);
	errdp_("#", first, (ftnlen)1);
	errdp_("#", epochs, (ftnlen)1);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("SPKW18", (ftnlen)6);
	return 0;
    } else if (epochs[*n - 1] < *last) {
	setmsg_("Segment end time # follows last epoch #.", (ftnlen)40);
	errdp_("#", last, (ftnlen)1);
	errdp_("#", &epochs[*n - 1], (ftnlen)1);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("SPKW18", (ftnlen)6);
	return 0;
    }

/*     If we made it this far, we're ready to start writing the segment. */


/*     Create the segment descriptor.  We don't use SPKPDS because */
/*     that routine doesn't allow creation of a singleton segment. */

    ic[0] = *body;
    ic[1] = *center;
    namfrm_(frame, &ic[2], frame_len);
    if (failed_()) {
	chkout_("SPKW18", (ftnlen)6);
	return 0;
    }
    ic[3] = 18;
    dc[0] = *first;
    dc[1] = *last;
    dafps_(&c__2, &c__6, dc, ic, descr);

/*     Begin a new segment. */

    dafbna_(handle, descr, segid, segid_len);
    if (failed_()) {
	chkout_("SPKW18", (ftnlen)6);
	return 0;
    }

/*     The type 18 segment structure is eloquently described by this */
/*     diagram from the SPK Required Reading: */

/*        +-----------------------+ */
/*        | Packet 1              | */
/*        +-----------------------+ */
/*        | Packet 2              | */
/*        +-----------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-----------------------+ */
/*        | Packet N              | */
/*        +-----------------------+ */
/*        | Epoch 1               | */
/*        +-----------------------+ */
/*        | Epoch 2               | */
/*        +-----------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-----------------------+ */
/*        | Epoch N               | */
/*        +-----------------------+ */
/*        | Epoch 100             | (First directory) */
/*        +-----------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-----------------------+ */
/*        | Epoch ((N-1)/100)*100 | (Last directory) */
/*        +-----------------------+ */
/*        | Subtype code          | */
/*        +-----------------------+ */
/*        | Window size           | */
/*        +-----------------------+ */
/*        | Number of packets     | */
/*        +-----------------------+ */


    i__1 = *n * packsz;
    dafada_(packts, &i__1);
    dafada_(epochs, n);
    i__1 = (*n - 1) / 100;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dafada_(&epochs[i__ * 100 - 1], &c__1);
    }
    d__1 = (doublereal) (*subtyp);
    dafada_(&d__1, &c__1);
    d__1 = (doublereal) winsiz;
    dafada_(&d__1, &c__1);
    d__1 = (doublereal) (*n);
    dafada_(&d__1, &c__1);

/*     As long as nothing went wrong, end the segment. */

    if (! failed_()) {
	dafena_();
    }
    chkout_("SPKW18", (ftnlen)6);
    return 0;
} /* spkw18_ */

