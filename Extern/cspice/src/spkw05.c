/* spkw05.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__1 = 1;

/* $Procedure SPKW05 ( Write SPK segment, type 5 ) */
/* Subroutine */ int spkw05_(integer *handle, integer *body, integer *center, 
	char *frame, doublereal *first, doublereal *last, char *segid, 
	doublereal *gm, integer *n, doublereal *states, doublereal *epochs, 
	ftnlen frame_len, ftnlen segid_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafps_(integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    doublereal descr[5];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    integer value;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), dafada_(
	    doublereal *, integer *), dafbna_(integer *, doublereal *, char *,
	     ftnlen), dafena_(void);
    extern logical failed_(void);
    integer refcod;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    doublereal dcd[2];
    integer icd[6];

/* $ Abstract */

/*     Write an SPK segment of type 5 given a time-ordered set of */
/*     discrete states and epochs, and the gravitational parameter */
/*     of a central body. */

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
/*     SPC */
/*     NAIF_IDS */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of an SPK file open for writing. */
/*     BODY       I   Body code for ephemeris object. */
/*     CENTER     I   Body code for the center of motion of the body. */
/*     FRAME      I   The reference frame of the states. */
/*     FIRST      I   First valid time for which states can be computed. */
/*     LAST       I   Last valid time for which states can be computed. */
/*     SEGID      I   Segment identifier. */
/*     GM         I   Gravitational parameter of central body. */
/*     N          I   Number of states and epochs. */
/*     STATES     I   States. */
/*     EPOCHS     I   Epochs. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of an SPK file */
/*              opened for writing. */

/*     BODY     is the NAIF ID for the body whose states are */
/*              to be recorded in an SPK file. */

/*     CENTER   is the NAIF ID for the center of motion associated */
/*              with BODY. */

/*     FRAME    is the reference frame that states are referenced to, */
/*              for example 'J2000'. */

/*     FIRST, */
/*     LAST     are the bounds on the ephemeris times, expressed as */
/*              seconds past J2000, for which the states can be used */
/*              to interpolate a state for BODY. */

/*     SEGID    is the segment identifier. An SPK segment identifier */
/*              may contain up to 40 characters. */

/*     GM       is the gravitational parameter of the central body */
/*              ( in units of kilometers **3 / seconds **2 ). */

/*     N        is the number of states and epochs to be stored */
/*              in the segment. */

/*     STATES   contains a time-ordered array of geometric states */
/*              ( x, y, z, dx/dt, dy/dt, dz/dt, in kilometers and */
/*              kilometers per second ) of the target body with */
/*              respect to the central body specified in the segment */
/*              descriptor. */

/*     EPOCHS   contains the epochs (ephemeris seconds past J2000) */
/*              corresponding to the states in STATES. Epochs must */
/*              form a strictly increasing sequence. */

/* $ Detailed_Output */

/*     None. A type 5 segment is written to the file attached to HANDLE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If GM is not positive, the error SPICE(NONPOSITIVEMASS) */
/*         is signaled. */

/*     2)  If the input epochs do not form an increasing sequence, the */
/*         error SPICE(UNORDEREDTIMES) is signaled. */

/*     3)  If the number of states and epochs is not positive, the */
/*         error SPICE(NUMSTATESNOTPOS) is signaled. */

/*     4)  If FIRST is greater than LAST, the error */
/*         SPICE(BADDESCRTIMES) is signaled. */

/*     5)  If SEGID is more than 40 characters long, the error */
/*         SPICE(SEGIDTOOLONG) is signaled. */

/*     6)  If SEGID contains any nonprintable characters, the error */
/*         SPICE(NONPRINTABLECHARS) is signaled. */

/*     7)  If a file I/O problem occurs, an error is signaled by a */
/*         routine in the call tree of this routine. */

/* $ Files */

/*     A new type 05 SPK segment is written to the SPK file attached */
/*     to HANDLE. */

/* $ Particulars */

/*     This routine writes an SPK type 05 data segment to the open SPK */
/*     file according to the format described in the type 05 section of */
/*     the SPK Required Reading. The SPK file must have been opened with */
/*     write access. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) This example demonstrates how to create an SPK type 5 kernel */
/*        containing only one segment, given a time-ordered set of */
/*        discrete states and epochs, and the gravitational parameter */
/*        of a central body. */


/*        Example code begins here. */


/*              PROGRAM SPKW05_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 40 ) */

/*        C */
/*        C     Define the segment identifier parameters. */
/*        C */
/*              CHARACTER*(*)         SPK5 */
/*              PARAMETER           ( SPK5  = 'spkw05_ex1.bsp' ) */

/*              CHARACTER*(*)         REF */
/*              PARAMETER           ( REF    = 'J2000'          ) */

/*              DOUBLE PRECISION      GMSUN */
/*              PARAMETER           ( GMSUN  = 132712440023.310D0 ) */

/*              INTEGER               BODY */
/*              PARAMETER           ( BODY   = 3  ) */

/*              INTEGER               CENTER */
/*              PARAMETER           ( CENTER = 10 ) */

/*              INTEGER               NSTATS */
/*              PARAMETER           ( NSTATS = 9  ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(NAMLEN)    IFNAME */
/*              CHARACTER*(NAMLEN)    SEGID */

/*              DOUBLE PRECISION      EPOCHS (    NSTATS ) */
/*              DOUBLE PRECISION      FIRST */
/*              DOUBLE PRECISION      LAST */
/*              DOUBLE PRECISION      STATES ( 6, NSTATS ) */

/*              INTEGER               HANDLE */
/*              INTEGER               NCOMCH */

/*        C */
/*        C     Define the states and epochs. */
/*        C */
/*              DATA                  STATES / */
/*             .       101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0, */
/*             .       102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0, */
/*             .       103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0, */
/*             .       104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0, */
/*             .       105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0, */
/*             .       106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0, */
/*             .       107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0, */
/*             .       108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0, */
/*             .       109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0 / */

/*              DATA                  EPOCHS / 100.D0, 200.D0, 300.D0, */
/*             .                               400.D0, 500.D0, 600.D0, */
/*             .                               700.D0, 800.D0, 900.D0 / */

/*        C */
/*        C     Set the start and end times of interval covered by */
/*        C     segment. */
/*        C */
/*              FIRST  = EPOCHS(1) */
/*              LAST   = EPOCHS(NSTATS) */

/*        C */
/*        C     NCOMCH is the number of characters to reserve for the */
/*        C     kernel's comment area. This example doesn't write */
/*        C     comments, so set to zero. */
/*        C */
/*              NCOMCH = 0 */

/*        C */
/*        C     Internal file name and segment ID. */
/*        C */
/*              IFNAME = 'Type 5 SPK internal file name.' */
/*              SEGID  = 'SPK type 5 test segment' */

/*        C */
/*        C     Open a new SPK file. */
/*        C */
/*              CALL SPKOPN( SPK5, IFNAME, NCOMCH, HANDLE ) */

/*        C */
/*        C     Write the segment. */
/*        C */
/*              CALL SPKW05 ( HANDLE, BODY,   CENTER, REF, */
/*             .              FIRST,  LAST,   SEGID,  GMSUN, */
/*             .              NSTATS, STATES, EPOCHS        ) */

/*        C */
/*        C     Close the SPK file. */
/*        C */
/*              CALL SPKCLS ( HANDLE ) */

/*              END */


/*        When this program is executed, no output is presented on */
/*        screen. After run completion, a new SPK type 5 exists in */
/*        the output directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     J.M. Lynch         (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        example code from existing fragment. Removed unnecessary */
/*        $Revisions section. */

/* -    SPICELIB Version 1.1.0, 30-OCT-2006 (BVS) */

/*        Removed restriction that the input reference frame should be */
/*        inertial by changing the routine that determines the frame ID */
/*        from the name from IRFNUM to NAMFRM. */

/* -    SPICELIB Version 1.0.2, 27-JAN-2003 (EDW) */

/*        Added error check to catch non-positive gravitational */
/*        parameter GM. */

/* -    SPICELIB Version 1.0.1, 05-OCT-1993 (KRG) */

/*        Removed all references to a specific method of opening the SPK */
/*        file in the $Brief_I/O, $Detailed_Input, $Particulars and */
/*        $Examples sections of the header. It is assumed that a person */
/*        using this routine has some knowledge of the DAF system and the */
/*        methods for obtaining file handles. */

/* -    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     write SPK type_5 ephemeris data segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     SIDLEN is the maximum number of characters allowed in an */
/*     SPK segment identifier. */

/*     NS is the size of a packed SPK segment descriptor. */

/*     ND is the number of double precision components in an SPK */
/*     segment descriptor. */

/*     NI is the number of integer components in an SPK segment */
/*     descriptor. */

/*     DTYPE is the data type. */

/*     FPRINT is the integer value of the first printable ASCII */
/*     character. */

/*     LPRINT is the integer value of the last printable ASCII character. */



/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKW05", (ftnlen)6);
    }
    if (*gm <= 0.) {
	setmsg_("GM = #; Non-positive gravitational parameter", (ftnlen)44);
	errdp_("#", gm, (ftnlen)1);
	sigerr_("SPICE(NONPOSITIVEMASS)", (ftnlen)22);
	chkout_("SPKW05", (ftnlen)6);
	return 0;
    }

/*     Get the NAIF integer code for the reference frame. */

    namfrm_(frame, &refcod, frame_len);
    if (refcod == 0) {
	setmsg_("The reference frame # is not supported.", (ftnlen)39);
	errch_("#", frame, (ftnlen)1, frame_len);
	sigerr_("SPICE(INVALIDREFFRAME)", (ftnlen)22);
	chkout_("SPKW05", (ftnlen)6);
	return 0;
    }

/*     Make sure that the number of states and epochs is positive. */

    if (*n <= 0) {
	setmsg_("The number of states and epochs is not positive. N = #", (
		ftnlen)54);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(NUMSTATESNOTPOS)", (ftnlen)22);
	chkout_("SPKW05", (ftnlen)6);
	return 0;
    }

/*     Check the input epochs to make sure that they form a */
/*     strictly increasing sequence. */

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (epochs[i__ - 1] <= epochs[i__ - 2]) {
	    setmsg_("Epoch # is out of order. ", (ftnlen)25);
	    errdp_("#", &epochs[i__ - 1], (ftnlen)1);
	    sigerr_("SPICE(UNORDEREDTIMES)", (ftnlen)21);
	    chkout_("SPKW05", (ftnlen)6);
	    return 0;
	}
    }

/*     The segment stop time should be greater then the begin time. */

    if (*first > *last) {
	setmsg_("The segment start time: # is greater then the segment end t"
		"ime: #", (ftnlen)65);
	errdp_("#", first, (ftnlen)1);
	errdp_("#", last, (ftnlen)1);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("SPKW05", (ftnlen)6);
	return 0;
    }

/*     Now check that all the characters in the segid can be printed. */

    i__1 = lastnb_(segid, segid_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	value = *(unsigned char *)&segid[i__ - 1];
	if (value < 32 || value > 126) {
	    setmsg_("The segment identifier contains nonprintable characters",
		     (ftnlen)55);
	    sigerr_("SPICE(NONPRINTABLECHARS)", (ftnlen)24);
	    chkout_("SPKW05", (ftnlen)6);
	    return 0;
	}
    }

/*     Also check to see if the segment identifier is too long. */

    if (lastnb_(segid, segid_len) > 40) {
	setmsg_("Segment identifier contains more than 40 characters.", (
		ftnlen)52);
	sigerr_("SPICE(SEGIDTOOLONG)", (ftnlen)19);
	chkout_("SPKW05", (ftnlen)6);
	return 0;
    }

/*     Store the start and end times to be associated */
/*     with this segment. */

    dcd[0] = *first;
    dcd[1] = *last;

/*     Create the integer portion of the descriptor. */

    icd[0] = *body;
    icd[1] = *center;
    icd[2] = refcod;
    icd[3] = 5;

/*     Pack the segment descriptor. */

    dafps_(&c__2, &c__6, dcd, icd, descr);

/*     Begin a new segment. */

    dafbna_(handle, descr, segid, segid_len);
    if (failed_()) {
	chkout_("SPKW05", (ftnlen)6);
	return 0;
    }

/*     This could hardly be simpler. Stuff the states into the segment, */
/*     followed by the epochs. */

    i__1 = *n * 6;
    dafada_(states, &i__1);
    dafada_(epochs, n);

/*     If there are at least 100 state/epoch pairs, write a directory */
/*     containing every 100'th epoch. */

    i__ = 100;
    while(i__ <= *n) {
	dafada_(&epochs[i__ - 1], &c__1);
	i__ += 100;
    }

/*     Store the GM of the central body, and the number of states. */

    dafada_(gm, &c__1);
    d__1 = (doublereal) (*n);
    dafada_(&d__1, &c__1);

/*     If anything went wrong, don't end the segment. */

    if (! failed_()) {
	dafena_();
    }
    chkout_("SPKW05", (ftnlen)6);
    return 0;
} /* spkw05_ */

