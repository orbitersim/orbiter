/* spkw08.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__27 = 27;
static integer c__2 = 2;
static integer c__6 = 6;
static integer c__1 = 1;

/* $Procedure SPKW08 ( Write SPK segment, type 8 ) */
/* Subroutine */ int spkw08_(integer *handle, integer *body, integer *center, 
	char *frame, doublereal *first, doublereal *last, char *segid, 
	integer *degree, integer *n, doublereal *states, doublereal *begtim, 
	doublereal *step, ftnlen frame_len, ftnlen segid_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen), chkin_(
	    char *, ftnlen), dafps_(integer *, integer *, doublereal *, 
	    integer *, doublereal *);
    doublereal descr[5];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    doublereal ltime;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    char etstr[40];
    extern /* Subroutine */ int dafada_(doublereal *, integer *), dafbna_(
	    integer *, doublereal *, char *, ftnlen), dafena_(void);
    extern logical failed_(void);
    integer chrcod, refcod;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    doublereal dcd[2];
    integer icd[6];
    doublereal tol;

/* $ Abstract */

/*     Write a type 8 segment to an SPK file. */

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

/*     Declare parameters specific to SPK type 8. */

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

/* -    SPICELIB Version 1.0.0, 11-JAN-2014 (NJB) */

/* -& */

/*     MAXDEG         is the maximum allowed degree of type 8 */
/*                    interpolating polynomials. If the value of MAXDEG */
/*                    is increased, the SPICELIB routine SPKPVN must be */
/*                    changed accordingly. In particular, the size of */
/*                    the record passed to SPKRnn and SPKEnn must be */
/*                    increased, and comments describing the record size */
/*                    must be changed. */

/*                    The record size requirement is */

/*                       MAXREC = ( 3 * ( MAXDEG + 1 ) )  +  3 */



/*     TOLSCL         is a tolerance scale factor (also called a */
/*                    "relative tolerance") used for time coverage */
/*                    bound checking. TOLSCL is unitless. TOLSCL */
/*                    produces a tolerance value via the formula */

/*                       TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) ) */

/*                    where FIRST and LAST are the coverage time bounds */
/*                    of a type 2 segment, expressed as seconds past */
/*                    J2000 TDB. */

/*                    The resulting parameter TOL is used as a tolerance */
/*                    for comparing the input segment descriptor time */
/*                    bounds to the first and last epoch covered by the */
/*                    sequence of time intervals defined by the inputs */
/*                    to SPKW08: */

/*                       EPOCH1 */
/*                       STEP */
/*                       N */

/*     Tolerance scale for coverage gap at the endpoints */
/*     of the segment coverage interval: */


/*     End of include file spk08.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MAXDEG     P   Maximum degree of interpolating polynomials. */
/*     TOLSCL     P   Scale factor used to compute time bound tolerance. */
/*     HANDLE     I   Handle of an SPK file open for writing. */
/*     BODY       I   NAIF code for an ephemeris object. */
/*     CENTER     I   NAIF code for center of motion of BODY. */
/*     FRAME      I   Reference frame name. */
/*     FIRST      I   Start time of interval covered by segment. */
/*     LAST       I   End time of interval covered by segment. */
/*     SEGID      I   Segment identifier. */
/*     DEGREE     I   Degree of interpolating polynomials. */
/*     N          I   Number of states. */
/*     STATES     I   Array of states. */
/*     BEGTIM     I   Epoch of first state in STATES array. */
/*     STEP       I   Time step separating epochs of states. */
/*     MAXDEG     P   Maximum allowed degree of interpolating polynomial. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of an SPK file that has been */
/*              opened for writing. */

/*     BODY     is the NAIF integer code for an ephemeris object */
/*              whose state relative to another body is described */
/*              by the segment to be created. */

/*     CENTER   is the NAIF integer code for the center of motion */
/*              of the object identified by BODY. */

/*     FRAME    is the NAIF name for a reference frame */
/*              relative to which the state information for BODY */
/*              is specified. */

/*     FIRST, */
/*     LAST     are, respectively, the start and stop times of */
/*              the time interval over which the segment defines */
/*              the state of BODY. */

/*     SEGID    is the segment identifier. An SPK segment */
/*              identifier may contain up to 40 characters. */

/*     DEGREE   is the degree of the Lagrange polynomials used to */
/*              interpolate the states. All components of the */
/*              state vectors are interpolated by polynomials of */
/*              fixed degree. */

/*     N        is the number of states in the input state vector */
/*              array. */

/*     STATES   contains a time-ordered array of geometric states */
/*              ( x, y, z, dx/dt, dy/dt, dz/dt, in kilometers and */
/*              kilometers per second ) of BODY relative to CENTER, */
/*              specified relative to FRAME. */

/*     BEGTIM   is the epoch corresponding to the first state in */
/*              the state array. Because extra states are needed */
/*              at the beginning and end of the segment in order */
/*              for the interpolation method to work, BEGTIM will */
/*              normally precede FIRST. */

/*     STEP     is the time step separating the epochs of adjacent */
/*              states in the input state array. STEP is specified */
/*              in seconds. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     See the include file spk08.inc for declarations of the */
/*     parameters described below. */

/*     MAXDEG   is the maximum degree of Lagrange polynomials that */
/*              can be used to interpolate states from the segment */
/*              written by this routine. */

/*     TOLSCL   is a tolerance scale factor (also called a */
/*              "relative tolerance") used for time coverage */
/*              bound checking. TOLSCL is unitless. TOLSCL */
/*              produces a tolerance value via the formula */

/*                 TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) ) */

/*              where FIRST and LAST are the coverage time bounds */
/*              of a type 8 segment, expressed as seconds past */
/*              J2000 TDB. */

/*              The resulting parameter TOL is used as a tolerance */
/*              for comparing the input segment descriptor time */
/*              bounds to the first and last epoch covered by the */
/*              sequence of time intervals defined by the inputs */
/*              to SPKW08: */

/*                 BEGTIM */
/*                 STEP */
/*                 N */

/* $ Exceptions */

/*     If any of the following exceptions occur, this routine will return */
/*     without creating a new segment. */

/*     1)  If FRAME is not a recognized name, the error */
/*         SPICE(INVALIDREFFRAME) is signaled. */

/*     2)  If the last non-blank character of SEGID occurs past index 40, */
/*         the error SPICE(SEGIDTOOLONG) is signaled. */

/*     3)  If SEGID contains any nonprintable characters, the error */
/*         SPICE(NONPRINTABLECHARS) is signaled. */

/*     4)  If DEGREE is not at least 1 or is greater than MAXDEG, the */
/*         error SPICE(INVALIDDEGREE) is signaled. */

/*     5)  If the number of states N is not at least DEGREE+1, the error */
/*         SPICE(TOOFEWSTATES) is signaled. */

/*     6)  If FIRST is greater than LAST, the error */
/*         SPICE(BADDESCRTIMES) is signaled. */

/*     7)  If STEP is non-positive, the error SPICE(INVALIDSTEPSIZE) is */
/*         signaled. */

/*     8)  If the start time of the first record exceeds the descriptor */
/*         begin time by more than a computed tolerance, or if the end */
/*         time of the last record precedes the descriptor end time by */
/*         more than a computed tolerance, the error SPICE(COVERAGEGAP) */
/*         is signaled. See the $Parameters section above for a */
/*         description of the tolerance. */

/* $ Files */

/*     A new type 8 SPK segment is written to the SPK file attached */
/*     to HANDLE. */

/* $ Particulars */

/*     This routine writes an SPK type 08 data segment to the open SPK */
/*     file according to the format described in the type 08 section of */
/*     the SPK Required Reading. The SPK file must have been opened with */
/*     write access. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) This example demonstrates how to create an SPK type 8 kernel */
/*        containing only one segment, given a time-ordered set of */
/*        discrete states and epochs. */


/*        Example code begins here. */


/*              PROGRAM SPKW08_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 40 ) */

/*        C */
/*        C     Define the segment identifier parameters. */
/*        C */
/*              CHARACTER*(*)         SPK8 */
/*              PARAMETER           ( SPK8   = 'spkw08_ex1.bsp' ) */

/*              CHARACTER*(*)         REF */
/*              PARAMETER           ( REF    = 'J2000'          ) */

/*              INTEGER               BODY */
/*              PARAMETER           ( BODY   = 3  ) */

/*              INTEGER               CENTER */
/*              PARAMETER           ( CENTER = 10 ) */

/*              INTEGER               DEGREE */
/*              PARAMETER           ( DEGREE = 3  ) */

/*              INTEGER               NSTATS */
/*              PARAMETER           ( NSTATS = 9  ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(NAMLEN)    IFNAME */
/*              CHARACTER*(NAMLEN)    SEGID */

/*              DOUBLE PRECISION      BEGTIM */
/*              DOUBLE PRECISION      FIRST */
/*              DOUBLE PRECISION      LAST */
/*              DOUBLE PRECISION      STATES ( 6, NSTATS ) */
/*              DOUBLE PRECISION      STEP */

/*              INTEGER               HANDLE */
/*              INTEGER               NCOMCH */

/*        C */
/*        C     Set the array of discrete states to write to the SPK */
/*        C     segment. */
/*        C */
/*              DATA                  STATES / */
/*             .        101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0, */
/*             .        102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0, */
/*             .        103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0, */
/*             .        104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0, */
/*             .        105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0, */
/*             .        106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0, */
/*             .        107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0, */
/*             .        108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0, */
/*             .        109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0  / */


/*        C */
/*        C     Set the start and end times of interval covered by */
/*        C     segment, and the time step separating epochs of states. */
/*        C */
/*              FIRST = 100.D0 */
/*              LAST  = 900.D0 */
/*              STEP  = 100.D0 */

/*        C */
/*        C     NCOMCH is the number of characters to reserve for the */
/*        C     kernel's comment area. This example doesn't write */
/*        C     comments, so set to zero. */
/*        C */
/*              NCOMCH = 0 */

/*        C */
/*        C     Internal file name and segment ID. */
/*        C */
/*              IFNAME = 'Type 8 SPK internal file name.' */
/*              SEGID  = 'SPK type 8 test segment' */

/*        C */
/*        C     Open a new SPK file. */
/*        C */
/*              CALL SPKOPN( SPK8, IFNAME, NCOMCH, HANDLE ) */

/*        C */
/*        C     Set the epoch of first state in STATES array to be */
/*        C     the start time of the interval covered by the segment. */
/*        C */
/*              BEGTIM = FIRST */

/*        C */
/*        C     Create a type 8 segment. */
/*        C */
/*              CALL SPKW08 (  HANDLE,  BODY,    CENTER,  REF, */
/*             .               FIRST,   LAST,    SEGID,   DEGREE, */
/*             .               NSTATS,  STATES,  BEGTIM,  STEP     ) */

/*        C */
/*        C     Close the SPK file. */
/*        C */
/*              CALL SPKCLS ( HANDLE ) */

/*              END */


/*        When this program is executed, no output is presented on */
/*        screen. After run completion, a new SPK type 8 exists in */
/*        the output directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     J.M. Lynch         (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 03-JUN-2021 (JDR) */

/*        Changed the input argument name EPOCH1 to BEGTIM for */
/*        consistency with other routines. */

/*        Edited the header to comply with NAIF standard. Added */
/*        complete example code from existing fragment. */

/*        Removed unnecessary $Revisions sections. */

/* -    SPICELIB Version 3.0.0, 11-JAN-2013 (NJB) */

/*        Relaxed test on relationship between the time bounds of the */
/*        input record set (determined by BEGTIM, STEP, and N) and the */
/*        descriptor bounds FIRST and LAST. Now the descriptor bounds */
/*        may extend beyond the time bounds of the record set by a ratio */
/*        computed using the parameter TOLSCL (see $Parameters above for */
/*        details). MAXDEG was increased to 27. */

/* -    SPICELIB Version 2.0.0, 19-SEP-1995 (WLT) */

/*        The routine was upgraded to support non-inertial reference */
/*        frames. */

/* -    SPICELIB Version 1.0.1, 05-OCT-1993 (KRG) */

/*        Removed all references to a specific method of opening the SPK */
/*        file in the $Brief_I/O, $Detailed_Input, $Particulars and */
/*        $Examples sections of the header. It is assumed that a person */
/*        using this routine has some knowledge of the DAF system and the */
/*        methods for obtaining file handles. */

/* -    SPICELIB Version 1.0.0, 08-AUG-1993 (NJB) (JML) (WLT) */

/* -& */
/* $ Index_Entries */

/*     write SPK type_8 ephemeris data segment */

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
    }
    chkin_("SPKW08", (ftnlen)6);

/*     Get the NAIF integer code for the reference frame. */

    namfrm_(frame, &refcod, frame_len);
    if (refcod == 0) {
	setmsg_("The reference frame # is not supported.", (ftnlen)39);
	errch_("#", frame, (ftnlen)1, frame_len);
	sigerr_("SPICE(INVALIDREFFRAME)", (ftnlen)22);
	chkout_("SPKW08", (ftnlen)6);
	return 0;
    }

/*     Check to see if the segment identifier is too long. */

    if (lastnb_(segid, segid_len) > 40) {
	setmsg_("Segment identifier contains more than 40 characters.", (
		ftnlen)52);
	sigerr_("SPICE(SEGIDTOOLONG)", (ftnlen)19);
	chkout_("SPKW08", (ftnlen)6);
	return 0;
    }

/*     Now check that all the characters in the segment identifier */
/*     can be printed. */

    i__1 = lastnb_(segid, segid_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	chrcod = *(unsigned char *)&segid[i__ - 1];
	if (chrcod < 32 || chrcod > 126) {
	    setmsg_("The segment identifier contains nonprintable characters"
		    ": ICHAR(SEGID(#:#))  = #", (ftnlen)79);
	    errint_("#", &i__, (ftnlen)1);
	    errint_("#", &i__, (ftnlen)1);
	    errint_("#", &chrcod, (ftnlen)1);
	    sigerr_("SPICE(NONPRINTABLECHARS)", (ftnlen)24);
	    chkout_("SPKW08", (ftnlen)6);
	    return 0;
	}
    }

/*     Make sure that the degree of the interpolating polynomials is */
/*     in range. */

    if (*degree < 1 || *degree > 27) {
	setmsg_("The interpolating polynomials have degree #; the valid degr"
		"ee range is [1, #].", (ftnlen)78);
	errint_("#", degree, (ftnlen)1);
	errint_("#", &c__27, (ftnlen)1);
	sigerr_("SPICE(INVALIDDEGREE)", (ftnlen)20);
	chkout_("SPKW08", (ftnlen)6);
	return 0;
    }

/*     Make sure that the number of states is sufficient to define a */
/*     polynomial whose degree is DEGREE. */

    if (*n <= *degree) {
	setmsg_("At least # states are required to define a polynomial of de"
		"gree #.  Number of states supplied:  #.", (ftnlen)98);
	i__1 = *degree + 1;
	errint_("#", &i__1, (ftnlen)1);
	errint_("#", degree, (ftnlen)1);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(TOOFEWSTATES)", (ftnlen)19);
	chkout_("SPKW08", (ftnlen)6);
	return 0;
    }

/*     The segment stop time should be greater than the begin time. */

    if (*first >= *last) {
	setmsg_("The segment start time: # is greater than or equal to the s"
		"egment end time: #", (ftnlen)77);
	errdp_("#", first, (ftnlen)1);
	errdp_("#", last, (ftnlen)1);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("SPKW08", (ftnlen)6);
	return 0;
    }

/*     The step size must be positive. */

    if (*step <= 0.) {
	setmsg_("The step size must be > 0 but was #. ", (ftnlen)37);
	errdp_("#", step, (ftnlen)1);
	sigerr_("SPICE(INVALIDSTEPSIZE)", (ftnlen)22);
	chkout_("SPKW08", (ftnlen)6);
	return 0;
    }

/*     Compute the tolerance to use for descriptor time bound checks. */

/* Computing MAX */
    d__1 = abs(*first), d__2 = abs(*last);
    tol = max(d__1,d__2) * 1e-13;
    if (*first < *begtim - tol) {
	setmsg_("The segment descriptor start time # is too much less than t"
		"he beginning time of the  segment data # (in seconds past J2"
		"000: #). The difference is # seconds; the  tolerance is # se"
		"conds.", (ftnlen)185);
	etcal_(first, etstr, (ftnlen)40);
	errch_("#", etstr, (ftnlen)1, (ftnlen)40);
	etcal_(begtim, etstr, (ftnlen)40);
	errch_("#", etstr, (ftnlen)1, (ftnlen)40);
	errdp_("#", first, (ftnlen)1);
	d__1 = *begtim - *first;
	errdp_("#", &d__1, (ftnlen)1);
	errdp_("#", &tol, (ftnlen)1);
	sigerr_("SPICE(COVERAGEGAP)", (ftnlen)18);
	chkout_("SPKW08", (ftnlen)6);
	return 0;
    }

/*     The end time of the final record must be greater than or */
/*     equal to the end time of the segment. */

    ltime = *begtim + (*n - 1) * *step;
    if (*last > ltime + tol) {
	setmsg_("The segment descriptor end time # is too much greater than "
		"the end time of the segment data # (in seconds past J2000: #"
		"). The difference is # seconds; the tolerance is # seconds.", 
		(ftnlen)178);
	etcal_(last, etstr, (ftnlen)40);
	errch_("#", etstr, (ftnlen)1, (ftnlen)40);
	etcal_(&ltime, etstr, (ftnlen)40);
	errch_("#", etstr, (ftnlen)1, (ftnlen)40);
	errdp_("#", last, (ftnlen)1);
	d__1 = *last - ltime;
	errdp_("#", &d__1, (ftnlen)1);
	errdp_("#", &tol, (ftnlen)1);
	sigerr_("SPICE(COVERAGEGAP)", (ftnlen)18);
	chkout_("SPKW08", (ftnlen)6);
	return 0;
    }

/*     If we made it this far, we're ready to start writing the segment. */

/*     Store the start and end times to be associated */
/*     with this segment. */

    dcd[0] = *first;
    dcd[1] = *last;

/*     Create the integer portion of the descriptor. */

    icd[0] = *body;
    icd[1] = *center;
    icd[2] = refcod;
    icd[3] = 8;

/*     Pack the segment descriptor. */

    dafps_(&c__2, &c__6, dcd, icd, descr);

/*     Begin a new segment. */

    dafbna_(handle, descr, segid, segid_len);
    if (failed_()) {
	chkout_("SPKW08", (ftnlen)6);
	return 0;
    }

/*     The type 8 segment structure is eloquently described by this */
/*     diagram from the SPK Required Reading: */

/*        +-----------------------+ */
/*        | State 1               | */
/*        +-----------------------+ */
/*        | State 2               | */
/*        +-----------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-----------------------+ */
/*        | State N               | */
/*        +-----------------------+ */
/*        | Epoch of state 1 (ET) | */
/*        +-----------------------+ */
/*        | Step size             | */
/*        +-----------------------+ */
/*        | Polynomial degree     | */
/*        +-----------------------+ */
/*        | Number of states      | */
/*        +-----------------------+ */


    i__1 = *n * 6;
    dafada_(states, &i__1);
    dafada_(begtim, &c__1);
    dafada_(step, &c__1);
    d__1 = (doublereal) (*degree);
    dafada_(&d__1, &c__1);
    d__1 = (doublereal) (*n);
    dafada_(&d__1, &c__1);

/*     As long as nothing went wrong, end the segment. */

    if (! failed_()) {
	dafena_();
    }
    chkout_("SPKW08", (ftnlen)6);
    return 0;
} /* spkw08_ */

