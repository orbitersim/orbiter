/* spkw20.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__50 = 50;
static integer c__40 = 40;
static integer c__2 = 2;
static integer c__6 = 6;
static integer c__1 = 1;

/* $Procedure SPKW20 ( SPK, write segment, type 20 ) */
/* Subroutine */ int spkw20_(integer *handle, integer *body, integer *center, 
	char *frame, doublereal *first, doublereal *last, char *segid, 
	doublereal *intlen, integer *n, integer *polydg, doublereal *cdata, 
	doublereal *dscale, doublereal *tscale, doublereal *initjd, 
	doublereal *initfr, ftnlen frame_len, ftnlen segid_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Local variables */
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen), chkin_(
	    char *, ftnlen), dafps_(integer *, integer *, doublereal *, 
	    integer *, doublereal *);
    doublereal btime, descr[5];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    doublereal ltime;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    char etstr[40];
    extern /* Subroutine */ int dafada_(doublereal *, integer *), dafbna_(
	    integer *, doublereal *, char *, ftnlen), dafena_(void);
    extern logical failed_(void);
    extern /* Subroutine */ int chckid_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    integer refcod, ninrec;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen);
    doublereal numrec;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    char netstr[40];
    doublereal dcd[2];
    extern doublereal j2000_(void);
    integer icd[6];
    extern doublereal spd_(void);
    doublereal tol;

/* $ Abstract */

/*     Write a type 20 segment to an SPK file. */

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
/*     TIME */
/*     SPK */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Abstract */

/*     Declare parameters specific to SPK type 20. */

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

/* -    SPICELIB Version 1.0.0, 30-DEC-2013 (NJB) */

/* -& */
/*     MAXDEG         is the maximum allowed degree of the input */
/*                    Chebyshev expansions. If the value of MAXDEG is */
/*                    increased, the SPICELIB routine SPKPVN must be */
/*                    changed accordingly. In particular, the size of */
/*                    the record passed to SPKRnn and SPKEnn must be */
/*                    increased, and comments describing the record size */
/*                    must be changed. */

/*                    The record size requirement is */

/*                       MAXREC = 3 * ( MAXDEG + 3 ) */



/*     TOLSCL         is a tolerance scale factor (also called a */
/*                    "relative tolerance") used for time coverage */
/*                    bound checking. TOLSCL is unitless. TOLSCL */
/*                    produces a tolerance value via the formula */

/*                       TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) ) */

/*                    where FIRST and LAST are the coverage time bounds */
/*                    of a type 20 segment, expressed as seconds past */
/*                    J2000 TDB. */

/*                    The resulting parameter TOL is used as a tolerance */
/*                    for comparing the input segment descriptor time */
/*                    bounds to the first and last epoch covered by the */
/*                    sequence of time intervals defined by the inputs */
/*                    to SPKW20: */

/*                       INITJD */
/*                       INITFR */
/*                       INTLEN */
/*                       N */

/*     Tolerance scale for coverage gap at the endpoints */
/*     of the segment coverage interval: */


/*     End of include file spk20.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of SPK file open for writing. */
/*     BODY       I   NAIF code for ephemeris object. */
/*     CENTER     I   NAIF code for the center of motion of the body. */
/*     FRAME      I   Reference frame name. */
/*     FIRST      I   Start time of interval covered by segment. */
/*     LAST       I   End time of interval covered by segment. */
/*     SEGID      I   Segment identifier. */
/*     INTLEN     I   Length of time covered by logical record (days). */
/*     N          I   Number of logical records in segment. */
/*     POLYDG     I   Chebyshev polynomial degree. */
/*     CDATA      I   Array of Chebyshev coefficients and positions. */
/*     DSCALE     I   Distance scale of data. */
/*     TSCALE     I   Time scale of data. */
/*     INITJD     I   Integer part of begin time (TDB Julian date) of */
/*                    first record. */
/*     INITFR     I   Fractional part of begin time of first record. */

/* $ Detailed_Input */

/*     HANDLE   is the DAF handle of an SPK file to which a type 20 */
/*              segment is to be added. The SPK file must be open */
/*              for writing. */

/*     BODY     is the NAIF integer code for an ephemeris object */
/*              whose state relative to another body is described */
/*              by the segment to be created. */

/*     CENTER   is the NAIF integer code for the center of motion */
/*              of the object identified by BODY. */

/*     FRAME    is the NAIF name for a reference frame relative to */
/*              which the state information for BODY is specified. */

/*     FIRST, */
/*     LAST     are the start and stop times of the time interval */
/*              over which the segment defines the state of the */
/*              object identified by BODY. */

/*     SEGID    is a segment identifier. An SPK segment identifier */
/*              may contain up to 40 characters. */

/*     INTLEN   is the length of time, in TDB Julian days, covered */
/*              by each set of Chebyshev polynomial coefficients */
/*              (each logical record). */

/*     N        is the number of logical records to be stored in */
/*              the segment. There is one logical record for each */
/*              time period. Each logical record contains three */
/*              sets of Chebyshev coefficients---one for each */
/*              coordinate---and three position vector components. */

/*     POLYDG   is the degree of each set of Chebyshev */
/*              polynomials, i.e. the number of Chebyshev */
/*              coefficients per coordinate minus one. POLYDG must */
/*              be less than or equal to the parameter MAXDEG. */

/*     CDATA    is an array containing all the sets of Chebyshev */
/*              polynomial coefficients and position components to */
/*              be placed in the new segment of the SPK file. */
/*              There are three sets of coefficients and position */
/*              components for each time interval covered by the */
/*              segment. */

/*              The coefficients and position components are */
/*              stored in CDATA in order as follows: */

/*                 the (POLYDG + 1) coefficients for the first */
/*                 coordinate of the first logical record, */
/*                 followed by the X component of position at the */
/*                 first interval midpoint. The first coefficient */
/*                 is that of the constant term of the expansion. */

/*                 the coefficients for the second coordinate, */
/*                 followed by the Y component of position at the */
/*                 first interval midpoint. */

/*                 the coefficients for the third coordinate, */
/*                 followed by the Z component of position at the */
/*                 first interval midpoint. */

/*                 the coefficients for the first coordinate for */
/*                 the second logical record, followed by the X */
/*                 component of position at the second interval */
/*                 midpoint. */

/*                 and so on. */

/*              The logical data records are stored contiguously: */

/*                 +----------+ */
/*                 | Record 1 | */
/*                 +----------+ */
/*                 | Record 2 | */
/*                 +----------+ */
/*                     ... */
/*                 +----------+ */
/*                 | Record N | */
/*                 +----------+ */

/*              The contents of an individual record are: */

/*                 +--------------------------------------+ */
/*                 | Coeff set for X velocity component   | */
/*                 +--------------------------------------+ */
/*                 | X position component                 | */
/*                 +--------------------------------------+ */
/*                 | Coeff set for Y velocity component   | */
/*                 +--------------------------------------+ */
/*                 | Y position component                 | */
/*                 +--------------------------------------+ */
/*                 | Coeff set for Z velocity component   | */
/*                 +--------------------------------------+ */
/*                 | Z position component                 | */
/*                 +--------------------------------------+ */

/*                   Each coefficient set has the structure: */

/*                 +--------------------------------------+ */
/*                 | Coefficient of T_0                   | */
/*                 +--------------------------------------+ */
/*                 | Coefficient of T_1                   | */
/*                 +--------------------------------------+ */
/*                                   ... */
/*                 +--------------------------------------+ */
/*                 | Coefficient of T_POLYDG              | */
/*                 +--------------------------------------+ */

/*              Where T_n represents the Chebyshev polynomial */
/*              of the first kind of degree n. */

/*     DSCALE, */
/*     TSCALE   are, respectively, the distance scale of the input */
/*              position and velocity data in km, and the time */
/*              scale of the input velocity data in TDB seconds. */

/*              For example, if the input distance data have units */
/*              of astronomical units (AU), DSCALE should be set */
/*              to the number of km in one AU. If the input */
/*              velocity data have time units of Julian days, then */
/*              TSCALE should be set to the number of seconds per */
/*              Julian day (86400). */

/*     INITJD   is the integer part of the Julian ephemeris date */
/*              of initial epoch of the first record. INITJD may */
/*              be less than, equal to, or greater than the */
/*              initial epoch. */

/*     INITFR   is the fractional part of the Julian ephemeris date */
/*              of initial epoch of the first record. INITFR has */
/*              units of Julian days. INITFR has magnitude */
/*              strictly less than 1 day. The sum */

/*                 INITJD + INITFR */

/*              equals the Julian ephemeris date of the initial */
/*              epoch of the first record. */

/* $ Detailed_Output */

/*     None. This routine writes data to an SPK file. */

/* $ Parameters */

/*     The parameters described in this section are declared in the */
/*     Fortran INCLUDE file spk20.inc */

/*     MAXDEG   is the maximum allowed degree of the input */
/*              Chebyshev expansions. */

/*     TOLSCL   is a tolerance scale factor (also called a */
/*              "relative tolerance") used for time coverage */
/*              bound checking. TOLSCL is unitless. TOLSCL */
/*              produces a tolerance value via the formula */

/*                 TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) ) */

/*              where FIRST and LAST are the coverage time bounds */
/*              of a type 20 segment, expressed as seconds past */
/*              J2000 TDB. */

/*              The resulting parameter TOL is used as a tolerance */
/*              for comparing the input segment descriptor time */
/*              bounds to the first and last epoch covered by the */
/*              sequence of time intervals defined by the inputs */

/*                 INITJD */
/*                 INITFR */
/*                 INTLEN */
/*                 N */

/*              See the $Exceptions section below for a description */
/*              of the error check using this tolerance. */

/* $ Exceptions */

/*     1)  If the number of sets of coefficients is not positive, */
/*         the error SPICE(INVALIDCOUNT) is signaled. */

/*     2)  If the interval length is not positive, the error */
/*         SPICE(INTLENNOTPOS) is signaled. */

/*     3)  If the name of the reference frame is not recognized, */
/*         the error SPICE(INVALIDREFFRAME) is signaled. */

/*     4)  If segment stop time is not greater than or equal to */
/*         the begin time, the error SPICE(BADDESCRTIMES) is signaled. */

/*     5)  If the start time of the first record exceeds the descriptor */
/*         begin time by more than a computed tolerance, or if the end */
/*         time of the last record precedes the descriptor end time by */
/*         more than a computed tolerance, the error SPICE(COVERAGEGAP) */
/*         is signaled. See the $Parameters section above for a */
/*         description of the tolerance. */

/*     6)  If the input degree POLYDG is less than 0 or greater than */
/*         MAXDEG, the error SPICE(INVALIDDEGREE) is signaled. */

/*     7)  If the last non-blank character of SEGID occurs past index 40, */
/*         or if SEGID contains any nonprintable characters, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     8)  If either the distance or time scale is non-positive, the */
/*         error SPICE(NONPOSITIVESCALE) is signaled. */

/* $ Files */

/*     A new type 20 SPK segment is written to the SPK file attached */
/*     to HANDLE. */

/* $ Particulars */

/*     This routine writes an SPK type 20 data segment to the designated */
/*     SPK file, according to the format described in the SPK Required */
/*     Reading. */

/*     Each segment can contain data for only one target, central body, */
/*     and reference frame. The Chebyshev polynomial degree and length */
/*     of time covered by each logical record are also fixed. However, */
/*     an arbitrary number of logical records of Chebyshev polynomial */
/*     coefficients can be written in each segment. Minimizing the */
/*     number of segments in an SPK file will help optimize how the */
/*     SPICE system accesses the file. */

/* $ Examples */

/*     Suppose that you have in an array CDATA sets of Chebyshev */
/*     polynomial coefficients and position vectors representing the */
/*     state of the moon (NAIF ID = 301), relative to the Earth-moon */
/*     barycenter (NAIF ID = 3), in the J2000 reference frame, and you */
/*     want to put these into a type 20 segment in an existing SPK file. */
/*     The following code could be used to add one new type 20 segment. */
/*     To add multiple segments, put the call to SPKW20 in a loop. */

/*     C */
/*     C      First open the SPK file and get a handle for it. */
/*     C */
/*            CALL DAFOPW ( SPKNAM, HANDLE ) */

/*     C */
/*     C      Create a segment identifier. */
/*     C */
/*            SEGID = 'MY_SAMPLE_SPK_TYPE_20_SEGMENT' */

/*     C */
/*     C      Note that the interval length INTLEN has units */
/*     C      of Julian days. The start time of the first record */
/*     C      is expressed using two inputs: integer and fractional */
/*     C      portions of the Julian ephemeris date of the start */
/*     C      time. */
/*     C */
/*     C      Write the segment. */
/*     C */
/*            CALL SPKW20 ( HANDLE, 301,    3,      'J2000', */
/*          .               FIRST,  LAST,   SEGID,  INTLEN, */
/*          .               N,      POLYDG, CDATA,  DSCALE, */
/*          .               TSCALE, INITJD, INITFR           ) */

/*     C */
/*     C      Close the file. */
/*     C */
/*            CALL DAFCLS ( HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.S. Zukor         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 05-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Added missing description of INITFR to $Brief_I/O section. */

/* -    SPICELIB Version 1.0.0, 17-JAN-2017 (NJB) (KSZ) */

/* -& */
/* $ Index_Entries */

/*     write SPK type_20 data segment */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */


/*     DTYPE is the SPK data type. */


/*     ND is the number of double precision components in an SPK */
/*     segment descriptor. SPK uses ND = 2. */


/*     NI is the number of integer components in an SPK segment */
/*     descriptor. SPK uses NI = 6. */


/*     NS is the size of a packed SPK segment descriptor. */


/*     SIDLEN is the maximum number of characters allowed in an */
/*     SPK segment identifier. */


/*     Local variables */



/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKW20", (ftnlen)6);

/*     The number of sets of coefficients must be positive. */

    if (*n <= 0) {
	setmsg_("The number of sets of coordinate coefficients is not positi"
		"ve. N = # ", (ftnlen)69);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("SPKW20", (ftnlen)6);
	return 0;
    }

/*     Make sure that the degree of the interpolating polynomials is */
/*     in range. */

    if (*polydg < 0 || *polydg > 50) {
	setmsg_("The interpolating polynomials have degree #; the valid degr"
		"ee range is [0, #].", (ftnlen)78);
	errint_("#", polydg, (ftnlen)1);
	errint_("#", &c__50, (ftnlen)1);
	sigerr_("SPICE(INVALIDDEGREE)", (ftnlen)20);
	chkout_("SPKW20", (ftnlen)6);
	return 0;
    }

/*     The interval length must be positive. */

    if (*intlen <= 0.) {
	setmsg_("The interval length is not positive.N = #", (ftnlen)41);
	errdp_("#", intlen, (ftnlen)1);
	sigerr_("SPICE(INTLENNOTPOS)", (ftnlen)19);
	chkout_("SPKW20", (ftnlen)6);
	return 0;
    }

/*     Get the NAIF integer code for the reference frame. */

    namfrm_(frame, &refcod, frame_len);
    if (refcod == 0) {
	setmsg_("The reference frame # is not supported.", (ftnlen)39);
	errch_("#", frame, (ftnlen)1, frame_len);
	sigerr_("SPICE(INVALIDREFFRAME)", (ftnlen)22);
	chkout_("SPKW20", (ftnlen)6);
	return 0;
    }

/*     The segment stop time must be greater than or equal to the begin */
/*     time. */

    if (*first > *last) {
	setmsg_("The segment start time: # (# TDB) is greater than the segme"
		"nt end time: (# TDB).", (ftnlen)80);
	etcal_(first, etstr, (ftnlen)40);
	errch_("#", etstr, (ftnlen)1, (ftnlen)40);
	errdp_("#", first, (ftnlen)1);
	etcal_(last, netstr, (ftnlen)40);
	errch_("#", netstr, (ftnlen)1, (ftnlen)40);
	errdp_("#", last, (ftnlen)1);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("SPKW20", (ftnlen)6);
	return 0;
    }

/*     The distance and time scales must be positive. */

    if (*dscale <= 0.) {
	setmsg_("The distance scale is not positive.DSCALE = #", (ftnlen)45);
	errdp_("#", dscale, (ftnlen)1);
	sigerr_("SPICE(NONPOSITIVESCALE)", (ftnlen)23);
	chkout_("SPKW20", (ftnlen)6);
	return 0;
    }
    if (*tscale <= 0.) {
	setmsg_("The time scale is not positive.TSCALE = #", (ftnlen)41);
	errdp_("#", tscale, (ftnlen)1);
	sigerr_("SPICE(NONPOSITIVESCALE)", (ftnlen)23);
	chkout_("SPKW20", (ftnlen)6);
	return 0;
    }

/*     The begin time of the first record must be less than or equal */
/*     to the begin time of the segment. Convert the two-part input */
/*     epoch to seconds past J2000 for the purpose of this check. */

    btime = spd_() * (*initjd - j2000_() + *initfr);
    ltime = btime + *n * *intlen * spd_();

/*     Compute the tolerance to use for descriptor time bound checks. */

/* Computing MAX */
    d__1 = abs(btime), d__2 = abs(ltime);
    tol = max(d__1,d__2) * 1e-13;
    if (*first < btime - tol) {
	setmsg_("The segment descriptor start time # is too much less than t"
		"he beginning time of the segment data # (in seconds past J20"
		"00: #). The difference is # seconds; the tolerance is # seco"
		"nds.", (ftnlen)183);
	etcal_(first, etstr, (ftnlen)40);
	errch_("#", etstr, (ftnlen)1, (ftnlen)40);
	etcal_(&btime, etstr, (ftnlen)40);
	errch_("#", etstr, (ftnlen)1, (ftnlen)40);
	errdp_("#", first, (ftnlen)1);
	d__1 = btime - *first;
	errdp_("#", &d__1, (ftnlen)1);
	errdp_("#", &tol, (ftnlen)1);
	sigerr_("SPICE(COVERAGEGAP)", (ftnlen)18);
	chkout_("SPKW20", (ftnlen)6);
	return 0;
    }

/*     The end time of the final record must be greater than or */
/*     equal to the end time of the segment. */

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
	chkout_("SPKW20", (ftnlen)6);
	return 0;
    }

/*     Now check the validity of the segment identifier. */

    chckid_("SPK segment identifier", &c__40, segid, (ftnlen)22, segid_len);
    if (failed_()) {
	chkout_("SPKW20", (ftnlen)6);
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
    icd[3] = 20;

/*     Pack the segment descriptor. */

    dafps_(&c__2, &c__6, dcd, icd, descr);

/*     Begin a new segment of SPK type 20 form: */

/*        Record 1 */
/*        Record 2 */
/*        ... */
/*        Record N */
/*        DSCALE     ( distance scale in km ) */
/*        TSCALE     ( time scale in seconds ) */
/*        INITJD     ( integer part of initial epoch of first record, */
/*                     expressed as a TDB Julian date ) */
/*        INITFR     ( fractional part of initial epoch, in units of */
/*                     TDB Julian days ) */
/*        INTLEN     ( length of interval covered by each record, in */
/*                     units of TDB Julian days ) */
/*        RSIZE      ( number of data elements in each record ) */
/*        N          ( number of records in segment ) */

/*     Each record will have the form: */

/*        X coefficients */
/*        X position component at interval midpoint */
/*        Y coefficients */
/*        Y position component at interval midpoint */
/*        Z coefficients */
/*        Z position component at interval midpoint */


    dafbna_(handle, descr, segid, segid_len);

/*     Calculate the number of entries in a record. */

    ninrec = (*polydg + 2) * 3;

/*     Fill segment with N records of data. */

    i__1 = *n * ninrec;
    dafada_(cdata, &i__1);

/*     Store the distance and time scales. */

    dafada_(dscale, &c__1);
    dafada_(tscale, &c__1);

/*     Store the integer and fractional parts of the initial epoch of */
/*     the first record. */

    dafada_(initjd, &c__1);
    dafada_(initfr, &c__1);

/*     Store the length of interval covered by each record. */

    dafada_(intlen, &c__1);

/*     Store the size of each record (total number of array elements). */
/*     Note that this size is smaller by 2 than the size of a type 2 */
/*     record of the same degree, since the record coverage midpoint */
/*     and radius are not stored. */

    d__1 = (doublereal) ninrec;
    dafada_(&d__1, &c__1);

/*     Store the number of records contained in the segment. */

    numrec = (doublereal) (*n);
    dafada_(&numrec, &c__1);

/*     End this segment. */

    dafena_();
    chkout_("SPKW20", (ftnlen)6);
    return 0;
} /* spkw20_ */

