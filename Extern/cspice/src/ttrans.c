/* ttrans.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2000 = 2000;
static integer c__1 = 1;
static integer c__4 = 4;
static integer c__100 = 100;
static integer c__400 = 400;
static integer c__1991 = 1991;
static integer c__6 = 6;
static integer c__21 = 21;
static integer c__280 = 280;
static integer c__12 = 12;
static integer c__7 = 7;
static doublereal c_b188 = 3600.;
static doublereal c_b189 = 60.;

/* $Procedure TTRANS ( Time transformation ) */
/* Subroutine */ int ttrans_(char *from, char *to, doublereal *tvec, ftnlen 
	from_len, ftnlen to_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer extra[12] = { 0,0,1,1,1,1,1,1,1,1,1,1 };
    static integer dpjan0[12] = { 0,31,59,90,120,151,181,212,243,273,304,334 }
	    ;
    static integer dpbegl[12] = { 0,31,60,91,121,152,182,213,244,274,305,335 }
	    ;
    static logical nodata = TRUE_;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static doublereal jd1101;
    static integer dn2000;
    static doublereal dp2000, frac;
    static integer nref, week;
    static doublereal secs;
    static integer year;
    static doublereal mins;
    static char vars__[32*1];
    static integer qint;
    static char rest[32], myto[32];
    extern /* Subroutine */ int zzcvpool_(char *, integer *, logical *, 
	    ftnlen), zzctruin_(integer *);
    static integer i__;
    static doublereal halfd;
    extern logical elemc_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char recog[8*21];
    static integer fmday;
    static doublereal daydp;
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen);
    static logical needy[21];
    static integer dyear;
    static doublereal tempd;
    static logical found;
    static integer tempi;
    static logical forml[21];
    static integer wkday;
    static doublereal tsecs;
    static integer dofyr, pfrom, month, dpsun;
    static doublereal hours, dt;
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static doublereal taitab[280];
    static integer daytab[280];
    extern /* Subroutine */ int rmaind_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal jdsecs, daylen;
    static integer parsed[21];
    extern /* Subroutine */ int orderc_(char *, integer *, integer *, ftnlen);
    static doublereal formal, secspd;
    static integer ordvec[21];
    static logical update;
    static integer doffst, offset;
    extern integer lstled_(doublereal *, integer *, doublereal *);
    extern /* Subroutine */ int reordc_(integer *, integer *, char *, ftnlen),
	     reordi_(integer *, integer *, integer *);
    static doublereal exsecs, lastdt;
    extern integer lstlei_(integer *, integer *, integer *);
    static integer daynum, fyrday;
    static char unifrm[8*27];
    extern /* Subroutine */ int ssizec_(integer *, char *, ftnlen);
    static integer refptr, dayptr;
    extern doublereal unitim_(doublereal *, char *, char *, ftnlen, ftnlen);
    static integer sunday, taiptr;
    extern /* Subroutine */ int insrtc_(char *, char *, ftnlen, ftnlen);
    static char myfrom[32];
    extern /* Subroutine */ int reordl_(integer *, integer *, logical *);
    extern integer lstlti_(integer *, integer *, integer *);
    extern logical return_(void);
    extern /* Subroutine */ int gdpool_(char *, integer *, integer *, integer 
	    *, doublereal *, logical *, ftnlen), setmsg_(char *, ftnlen);
    static integer usrctr[2];
    extern /* Subroutine */ int swpool_(char *, integer *, char *, ftnlen, 
	    ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen), 
	    nextwd_(char *, char *, char *, ftnlen, ftnlen, ftnlen), rmaini_(
	    integer *, integer *, integer *, integer *);
    static integer yr1, yr4;
    extern doublereal j2000_(void);
    extern logical odd_(integer *);
    static doublereal tai;
    static integer day, rem;
    extern doublereal spd_(void);
    static integer pto, yr100, yr400;

/* $ Abstract */

/*     Transform a time vector from one representation and system */
/*     to another. */

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

/*     TIME */

/* $ Keywords */

/*     PARSING */
/*     TIME */

/* $ Declarations */
/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MXCOMP     P    maximum number of components allowed for TVEC. */
/*     TO         I    description of a time vector. */
/*     FROM       I    description of a time vector. */
/*     TVEC      I-O   time vector representing an epoch. */

/* $ Detailed_Input */

/*     TVEC     is called a time vector. It is an array of double */
/*              precision numbers that represent some epoch. To */
/*              determine its meaning you must examine the string */
/*              FROM. Note that the number of significant entries */
/*              in TVEC is implied by FROM. */

/*     FROM, */
/*     TO       are two strings used to describe the type of time vector */
/*              TVEC. FROM is the type of the input vector TVEC and */
/*              TO is the type of the output TVEC */

/*              The interpretation of TVEC  is as follows: */

/*                 TYPE      Interpretation of TVEC */
/*                 ------    ------------------------------------------- */
/*                 YMD(F)  - year, month, day,   hour, minutes, seconds */
/*                 YD(F)   - year,  day-of-year, hour, minutes, seconds */
/*                 YD.D(F) - year, number of days past beginning of year */
/*                 DAYSEC  - calendar days past 1 jan 1 AD, */
/*                           seconds past beg day */
/*                 DP2000  - calendar days past 1 jan 2000, */
/*                           seconds past beg day */
/*                 JDUTC   - julian date UTC. */
/*                 FORMAL  - seconds in the formal calendar since J2000. */
/*                 YWD(F)  - year, week, day, hour, minutes, seconds */
/*                 YMWD(F) - year, month, week, day, hour, minutes, */
/*                           seconds */
/*                 TAI     - atomic seconds past Atomic J2000. */
/*                 TDT     - Terrestrial Dynamical Time */
/*                 TDB     - Barycentric Dynamical Time */
/*                 JED     - Julian Ephemeris Date (based on TDB) */
/*                 ET      - Ephemeris time (same as TDB) */
/*                 JDTDB   - Julian Date based on TDB (same as JED) */
/*                 JDTDT   - Julian Date based on TDT */

/*              The number of components of TVEC implied by TYPE is */
/*              as follows: */

/*                 YMD     - 6 */
/*                 YD      - 5 */
/*                 JDUTC   - 1 */
/*                 FORMAL  - 1 */
/*                 YD.D    - 2 */
/*                 DAYSEC  - 2 */
/*                 DP2000  - 2 */
/*                 YWD     - 6 */
/*                 YMWD    - 7 */
/*                 TAI     - 1 */
/*                 TDT     - 1 */
/*                 TDB     - 1 */
/*                 JED     - 1 */
/*                 ET      - 1 */
/*                 JDTDB   - 1 */
/*                 JDTDT   - 1 */


/*              For all types, only the last component of the */
/*              time vector may be non-integer. If other components */
/*              have fractional parts only their truncated integer */
/*              components will be recognized. */

/*              YMD and YD */

/*                 These types are assumed to be different */
/*                 representations on UTC time markers. Thus */
/*                 the hour, minutes and seconds portions all */
/*                 represent time elapsed */
/*                 since the beginning of a day. As such the */
/*                 seconds portion of HMS may range up to (but */
/*                 not include) 61 on days when positive leap */
/*                 seconds occur and may range up to (but not */
/*                 include) 59 on days during which negative */
/*                 leapseconds occur. */

/*              YD.D type. */

/*                 Y is the calendar year used in civil time keeping */
/*                 D is the day of the calendar year --- for any time */
/*                   during the first of January, the integer portion */
/*                   of the day will be 1. */

/*                   The fractional portion is the fractional part of */
/*                   the specific day. Thus the amount of time */
/*                   specified by the fractional portion of the day */
/*                   depends upon whether or not the day has a leap */
/*                   second.  ".D" can be computed from the formula */

/*                         number of seconds past beginning of day */
/*                   .D = --------------------------------------- */
/*                            number of UTC seconds in the day. */

/*              FORMAL type. */

/*                 The FORMAL type for TVEC gives the number of */
/*                 seconds past the epoch J2000 (noon Jan 1 2000) */
/*                 on the formal calendar (no leap seconds --- */
/*                 all days contain 86400 seconds)  The formal clock */
/*                 is simply held still for one second during */
/*                 positive leap seconds. Times during leap seconds */
/*                 cannot be represented in this system. */

/*                 This system is converted internally to a */
/*                 calendar days past epoch and seconds */
/*                 past beginning of day form. For this reason, */
/*                 times that occur during a positive leap second */
/*                 can never be represented. Moreover, if a negative */
/*                 leapsecond occurs, times that occur during the */
/*                 ``missing'' leapsecond will simply be placed */
/*                 at the beginning of the next day. Thus two */
/*                 different FORMAL times can represent the */
/*                 same time around a negative leap second. */

/*                 FORMAL time is equivalent to somewhat parochial */
/*                 ``UTC seconds past J2000'' that is produced */
/*                 by the SPICE routine TPARSE. */

/*              JDUTC type. */

/*                 This system is similar to the FORMAL system */
/*                 described above. All days are assumed to have */
/*                 86400 seconds. All numbers of the form */

/*                    integer + 0.5 */

/*                 fall at the beginning of calendar UTC days. */

/*                 There is no way to represent times during a */
/*                 positive leapsecond. Times during missing */
/*                 negative leap seconds are represented in two ways. */

/*              DAYSEC type. */

/*                 This time vector has the form of calendar */
/*                 days since January 1, of the year 1 A.D. */
/*                 and number of seconds past the beginning of the */
/*                 calendar day. */
/*                 (January 2 of the year 1 A.D. is 1 calendar */
/*                 day past January 1, 1 A.D.) */

/*              DP2000 type. */

/*                 This time vector has the same form as DAYSEC */
/*                 time vectors. The only difference is that */
/*                 the reference epoch is JAN 1, 2000. */

/*              YWD and YMWD types. */

/*                 These time vectors are used to specify a time */
/*                 that are most conveniently expressed by phrases */
/*                 such as "the third Monday of every month" or */
/*                 "Beginning with the second Wednesday of the new */
/*                 year and every 4th Wednesday thereafter." */

/*                 The hours, minutes and seconds components of */
/*                 these time vectors are the */
/*                 same as for the Year-Month-Day and Year-Day UTC */
/*                 time vectors. */

/*                 The Y component refers to the calendar year, and */
/*                 in the YMWD vector, the M component refers to */
/*                 the calendar month. */

/*                 The W component refers to the week of the */
/*                 Year (YWD) or Month (YMWD).  The first week */
/*                 begins on the first day of the year or the first */
/*                 day of the month. The D component is the day of the */
/*                 week with 1 corresponding to Sunday, 2 to Monday, */
/*                 and so on with 7 corresponding to Saturday. */

/*                 Thus the YMWD time vector */

/*                    1991 */
/*                      11 */
/*                       3 */
/*                       5 */
/*                      12 */
/*                       0 */
/*                       0 */

/*                 refers to 12:00:00 on the third Thursday of */
/*                 November of 1991. */

/*                 The YWD time vector */

/*                    1997 */
/*                      11 */
/*                       4 */
/*                      13 */
/*                       5 */
/*                      11 */

/*                 refers to 12:05:11 on the eleventh Wednesday */
/*                 of 1997. */

/*              Formal Calendar Time Vectors */
/*              ============================ */
/*              The types YMDF, YDF, YD.D(F), YWDF, YMWDF are similar */
/*              to the corresponding base types: YMD, YD, YD.D, YWD */
/*              and YMWD. However, these types represent formal */
/*              time vectors. Each day contains exactly 86400 seconds. */
/*              The difference between formal and non-formal systems */
/*              can only be seen during a positive leapsecond or */
/*              during the second following a negative leapsecond. */

/*              Epochs during a positive leapsecond on input are */
/*              placed in the first second of the next day. Epochs */
/*              during a positive leapsecond on output are held */
/*              at 00:00:00 of the next day. */

/*              Epochs during the first second following a negative */
/*              leapsecond are counted as belonging to the previous */
/*              day if both the input and output types are formal */
/*              types. */


/*              Calendars */
/*              ===================== */
/*              In all time vectors for which a year is specified, */
/*              the year is assumed to belong to the Gregorian */
/*              Calendar---every 4th year is a leapyear except */
/*              for centuries (such as 1900) that are not divisible */
/*              by 400. This calendar is formally extended */
/*              indefinitely backward and forward in time. */

/*              Note that the Gregorian Calendar did not */
/*              formally exist prior to October 15, 1582. Prior to */
/*              that time the Julian Calendar was used (in the */
/*              Julian Calendar every 4th year is a leapyear, including */
/*              all centuries). */

/*              If you have epochs relative to the Julian calendar, */
/*              the SPICE routine JUL2GR is available for converting */
/*              to the formal Gregorian Calendar. */


/*              Epochs Prior to 1972 */
/*              ===================== */
/*              UTC as it exists today, was adopted in 1972. For */
/*              epochs prior to 1972, it is assumed that the difference */
/*              between TAI and UTC is a constant value. */

/*              Years prior to 1 A.D. */
/*              ===================== */
/*              A year belonging to the B.C. era,  may be */
/*              represented by subtracting the year from 1. */
/*              Thus to specify 27 B.C (Gregorian) set the */
/*              year component of the time vector to -26. */


/*              Notes: */
/*              ====== */
/*              The FORMAL and JDUTC types should not be used */
/*              for times near a leap second. However, for times */
/*              removed from leap seconds they pose no problems. */

/*              The DAYSEC and DP2000 are useful for representing */
/*              times that are given in atomic seconds past some */
/*              reference epoch other than J2000. */

/* $ Detailed_Output */

/*     TVEC     is the time vector corresponding to the input */
/*              time vector but with components consistent with */
/*              the type specified by input variable TO. */

/* $ Parameters */

/*     MXCOMP   is the maximum number of components that can appear in */
/*              TVEC. */

/* $ Exceptions */

/*     1)  If the type of either FROM or TO is not recognized, the */
/*         error SPICE(UNKNONWNTIMESYSTEM) is signaled. */

/*     2)  If a leapseconds kernel has not been loaded prior a call */
/*         to TTRANS, the error  SPICE(NOLEAPSECONDS) is signaled. */

/*     3)  If epochs associated with leapseconds in the leapseconds */
/*         kernel are not in increasing order, the error */
/*         SPICE(BADLEAPSECONDS) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is the fundamental translator between various */
/*     representations of time in the SPICE system. However, it */
/*     is intended to be a mid-level routine that few user's should */
/*     have need of calling. */

/*     In addition to translating between time systems, this routine */
/*     can be used to normalize the components of a time string */
/*     so that they are in the normal range for a particular */
/*     representation. This allows you to easily do arithmetic */
/*     with epochs. */

/* $ Examples */

/*     Suppose you need to convert a time expressed as seconds */
/*     past J2000 (TDB) to Pacific Daylight time. The following */
/*     example shows how you might use TTRANS to accomplish this */
/*     task. */

/*      TVEC(1) = ET */

/*      CALL TTRANS ( 'TDB', 'YMD', TVEC ) */

/*      The seconds component of PDT is the same as the seconds */
/*      component of UTC. We save and add the UTC-PDT offset */
/*      to the hours and minutes component of the time vector. */

/*      SECNDS  = TVEC(6) */
/*      TVEC(6) = 0.0D0 */

/*      TVEC(4) = TVEC(4) - 7.0D0 */
/*      TVEC(5) = TVEC(5) + 0.0D0 */

/*      CALL TTRANS ( 'YMDF', 'YMDF', TVEC ) */

/*      Now reset the seconds component to the original value */
/*      and pass the time vector to some formatting routine. */

/*      TVEC(6) = SECNDS */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.6.0, 05-SEP-2021 (EDW) (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Removed INT casts in HMSSEC calls. The casts prevent */
/*        correct calculation of TDB time for non integer hour */
/*        and minute values in time strings from STR2ET. */

/*        Removed reference to FURNSH from the "LSK variable */
/*        not present" long error message. */

/* -    SPICELIB Version 1.5.0, 09-SEP-2013 (BVS) */

/*        Updated to keep track of the POOL counter and call ZZCVPOOL. */

/* -    SPICELIB Version 1.4.0, 05-MAR-2009 (NJB) */

/*        Bug fix: this routine now keeps track of whether its */
/*        kernel pool look-up succeeded. If not, a kernel pool */
/*        lookup is attempted on the next call to this routine. */

/* -    SPICELIB Version 1.3.0, 15-NOV-2006 (NJB) */

/*        A reference to RTPOOL was replaced by a reference */
/*        to GDPOOL. */

/* -    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in RMAIND and RMAINI calls. Changed reference to LDPOOL to */
/*        reference to FURNSH in an error message. */

/* -    SPICELIB Version 1.1.0, 09-JUN-1999 (WLT) */

/*        The routine was modified so that uniform time system */
/*        transformations (see UNITIM) are handled without */
/*        performing intermediate computations. This gives a slight */
/*        improvement in the accuracy of some computations. */

/*        In addition, two unused variables were removed. */

/* -    SPICELIB Version 1.0.0, 17-SEP-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Convert from one time vector to another */
/*     Convert between various parsed time representations */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in RMAIND and RMAINI calls. Changed reference to LDPOOL to */
/*        reference to FURNSH in an error message. */

/* -& */

/*     SPICELIB functions */


/*     Local (in-line) functions */


/*     Local parameters */


/*     Parameters */

/*     We declare the variables that contain the number of days in */
/*     400 years, 100 years, 4 years and 1 year. */


/*     The following integers give the number of days during the */
/*     associated month of a non-leap year. */


/*     The integers that follow give the number of days in a normal */
/*     year that precede the first of the month. */


/*     The integers that follow give the number of days in a leap */
/*     year that precede the first of the month. */


/*     MAXLP is the maximum number of leap seconds that can be */
/*     stored internally.   The value of 140 should be sufficient */
/*     to store leap seconds through the year 2100. */


/*     MAXVAR is the number of kernel pool variables required by this */
/*     routine. */



/*     The following gives us an "enumeration" for all of the */
/*     various types of time vectors that are recognized. */

/*     DAYSEC */
/*     DAYP2 */
/*     ET */
/*     FRML */
/*     JDTDB */
/*     JDTDT */
/*     JDUTC */
/*     JED */
/*     TAI */
/*     TDB */
/*     TDT */
/*     YD */
/*     YDD */
/*     YDDF */
/*     YDF */
/*     YMD */
/*     YMDF */
/*     YMWD */
/*     YMWDF */
/*     YWD */
/*     YWDF */


/*     The following parameters just make the code seem a bit */
/*     more natural. */


/*     Local variables */


/*     The array EXTRA contains the number of many additional days that */
/*     appear before the first of a month during a leap year (as opposed */
/*     to a non-leap year). */


/*     DPJAN0(I) gives the number of days that occur before the I'th */
/*     month of a normal year. */


/*     Saved variables */


/*     Initial values */


/*     Definitions of statement functions. */

/*     The number of days elapsed since Jan 1, of year 1 A.D. to */
/*     Jan 1 of YEAR is given by: */


/*     Return 1 if YEAR is divisible by N, otherwise return 0. */


/*     The number of leap days in a year is given by: */


/*     To compute the day of the year we */

/*        look up the number of days to the beginning of the month, */

/*        add on the number leap days that occurred prior to that */
/*        time */

/*        add on the number of days into the month */


/*     The number of days since 1 Jan 1 A.D. is given by: */


/*     The number of seconds represented by HOURS hours MINS minutes */
/*     and SECS seconds. */

    if (return_()) {
	return 0;
    } else {
	chkin_("TTRANS", (ftnlen)6);
    }

/*     The first time any of the entry points are called we */
/*     must set up the "watcher" for the kernel pool variables */
/*     that will be needed by this routine. */

    if (first) {
	first = FALSE_;
	secspd = spd_();
	halfd = spd_() / 2.;
/* Computing MAX */
	i__3 = 0, i__4 = abs(c__2000) / c__4 * c__4 + 1 - abs(c__2000);
/* Computing MAX */
	i__5 = 0, i__6 = abs(c__2000) / c__100 * c__100 + 1 - abs(c__2000);
/* Computing MAX */
	i__7 = 0, i__8 = abs(c__2000) / c__400 * c__400 + 1 - abs(c__2000);
	dn2000 = (c__2000 - 1) * 365 + (c__2000 - 1) / 4 - (c__2000 - 1) / 
		100 + (c__2000 - 1) / 400 + (dpjan0[(i__1 = c__1 - 1) < 12 && 
		0 <= i__1 ? i__1 : s_rnge("dpjan0", i__1, "ttrans_", (ftnlen)
		963)] + extra[(i__2 = c__1 - 1) < 12 && 0 <= i__2 ? i__2 : 
		s_rnge("extra", i__2, "ttrans_", (ftnlen)963)] * (max(i__3,
		i__4) - max(i__5,i__6) + max(i__7,i__8)) + c__1) - 1;
/* Computing MAX */
	i__3 = 0, i__4 = abs(c__1991) / c__4 * c__4 + 1 - abs(c__1991);
/* Computing MAX */
	i__5 = 0, i__6 = abs(c__1991) / c__100 * c__100 + 1 - abs(c__1991);
/* Computing MAX */
	i__7 = 0, i__8 = abs(c__1991) / c__400 * c__400 + 1 - abs(c__1991);
	sunday = (c__1991 - 1) * 365 + (c__1991 - 1) / 4 - (c__1991 - 1) / 
		100 + (c__1991 - 1) / 400 + (dpjan0[(i__1 = c__1 - 1) < 12 && 
		0 <= i__1 ? i__1 : s_rnge("dpjan0", i__1, "ttrans_", (ftnlen)
		964)] + extra[(i__2 = c__1 - 1) < 12 && 0 <= i__2 ? i__2 : 
		s_rnge("extra", i__2, "ttrans_", (ftnlen)964)] * (max(i__3,
		i__4) - max(i__5,i__6) + max(i__7,i__8)) + c__6) - 1;
	jd1101 = j2000_() - (doublereal) dn2000 - .5;

/*        Initialize the list of Uniform time systems. */

	ssizec_(&c__21, unifrm, (ftnlen)8);

/*        Set up the set of recognized time vectors. */

/*        The following 4 parallel arrays are here */
/*        to assist in the task of classifying the */
/*        FROM and TO time representations. The arrays */
/*        contain: */

/*        RECOG   the strings that are recognized as legitimate */
/*                time representations */

/*        PARSED  a unique integer that can be used to stand */
/*                for each recognized format.  This is used */
/*                in the various IF THEN blocks to decide */
/*                how a time vector should be processed instead */
/*                of the name because integer compares are */
/*                much faster than string comparisons. */

/*        FORML   is a logical that indicates whether or not the */
/*                corresponding time system is a formal system */
/*                or UTC based system.  FORML(I) = YES implies */
/*                the time system is formal.  FORML(I) means it */
/*                isn't. */

/*        NEEDY   is a logical that indicates whether or not */
/*                there is a YEAR in the time system.  It should */
/*                be read "NEED Y" for "need year"  not "needy" */
/*                as when you are destitute.  NEEDY(I) = YES means */
/*                the time system has a year.  NEEDY(I) = NO means */
/*                it doesn't */

	s_copy(recog, "DAYSEC ", (ftnlen)8, (ftnlen)7);
	parsed[0] = 1;
	forml[0] = FALSE_;
	needy[0] = FALSE_;
	s_copy(recog + 8, "DP2000 ", (ftnlen)8, (ftnlen)7);
	parsed[1] = 2;
	forml[1] = FALSE_;
	needy[1] = FALSE_;
	s_copy(recog + 16, "ET ", (ftnlen)8, (ftnlen)3);
	parsed[2] = 3;
	forml[2] = FALSE_;
	needy[2] = FALSE_;
	insrtc_("ET", unifrm, (ftnlen)2, (ftnlen)8);
	s_copy(recog + 24, "FORMAL ", (ftnlen)8, (ftnlen)7);
	parsed[3] = 4;
	forml[3] = TRUE_;
	needy[3] = FALSE_;
	s_copy(recog + 32, "JDTDB ", (ftnlen)8, (ftnlen)6);
	parsed[4] = 5;
	forml[4] = FALSE_;
	needy[4] = FALSE_;
	insrtc_("JDTDB", unifrm, (ftnlen)5, (ftnlen)8);
	s_copy(recog + 40, "JDTDT ", (ftnlen)8, (ftnlen)6);
	parsed[5] = 6;
	forml[5] = FALSE_;
	needy[5] = FALSE_;
	insrtc_("JDTDT", unifrm, (ftnlen)5, (ftnlen)8);
	s_copy(recog + 48, "JDUTC ", (ftnlen)8, (ftnlen)6);
	parsed[6] = 7;
	forml[6] = TRUE_;
	needy[6] = FALSE_;
	s_copy(recog + 56, "JED ", (ftnlen)8, (ftnlen)4);
	parsed[7] = 8;
	forml[7] = FALSE_;
	needy[7] = FALSE_;
	insrtc_("JED", unifrm, (ftnlen)3, (ftnlen)8);
	s_copy(recog + 64, "TAI ", (ftnlen)8, (ftnlen)4);
	parsed[8] = 9;
	forml[8] = FALSE_;
	needy[8] = FALSE_;
	insrtc_("TAI", unifrm, (ftnlen)3, (ftnlen)8);
	s_copy(recog + 72, "TDB ", (ftnlen)8, (ftnlen)4);
	parsed[9] = 10;
	forml[9] = FALSE_;
	needy[9] = FALSE_;
	insrtc_("TDB", unifrm, (ftnlen)3, (ftnlen)8);
	s_copy(recog + 80, "TDT ", (ftnlen)8, (ftnlen)4);
	parsed[10] = 11;
	forml[10] = FALSE_;
	needy[10] = FALSE_;
	insrtc_("TDT", unifrm, (ftnlen)3, (ftnlen)8);
	s_copy(recog + 88, "YD ", (ftnlen)8, (ftnlen)3);
	parsed[11] = 12;
	forml[11] = FALSE_;
	needy[11] = TRUE_;
	s_copy(recog + 96, "YD.D ", (ftnlen)8, (ftnlen)5);
	parsed[12] = 13;
	forml[12] = FALSE_;
	needy[12] = TRUE_;
	s_copy(recog + 104, "YD.DF ", (ftnlen)8, (ftnlen)6);
	parsed[13] = 14;
	forml[13] = TRUE_;
	needy[13] = TRUE_;
	s_copy(recog + 112, "YDF ", (ftnlen)8, (ftnlen)4);
	parsed[14] = 15;
	forml[14] = TRUE_;
	needy[14] = TRUE_;
	s_copy(recog + 120, "YMD ", (ftnlen)8, (ftnlen)4);
	parsed[15] = 16;
	forml[15] = FALSE_;
	needy[15] = TRUE_;
	s_copy(recog + 128, "YMDF ", (ftnlen)8, (ftnlen)5);
	parsed[16] = 17;
	forml[16] = TRUE_;
	needy[16] = TRUE_;
	s_copy(recog + 136, "YMWD ", (ftnlen)8, (ftnlen)5);
	parsed[17] = 18;
	forml[17] = FALSE_;
	needy[17] = TRUE_;
	s_copy(recog + 144, "YMWDF ", (ftnlen)8, (ftnlen)6);
	parsed[18] = 19;
	forml[18] = TRUE_;
	needy[18] = TRUE_;
	s_copy(recog + 152, "YWD ", (ftnlen)8, (ftnlen)4);
	parsed[19] = 20;
	forml[19] = FALSE_;
	needy[19] = TRUE_;
	s_copy(recog + 160, "YWDF ", (ftnlen)8, (ftnlen)5);
	parsed[20] = 21;
	forml[20] = TRUE_;
	needy[20] = TRUE_;
	orderc_(recog, &c__21, ordvec, (ftnlen)8);
	reordc_(ordvec, &c__21, recog, (ftnlen)8);
	reordi_(ordvec, &c__21, parsed);
	reordl_(ordvec, &c__21, forml);
	reordl_(ordvec, &c__21, needy);

/*        Initialize the local POOL counter to user value. */

	zzctruin_(usrctr);

/*        Set up the kernel pool watchers */

	s_copy(vars__, "DELTET/DELTA_AT", (ftnlen)32, (ftnlen)15);
	swpool_("TTRANS", &c__1, vars__, (ftnlen)6, (ftnlen)32);
    }

/*     Check to see if any of the kernel items required by this */
/*     routine have been updated since the last call to this */
/*     entry point. */

    zzcvpool_("TTRANS", usrctr, &update, (ftnlen)6);
    if (update || nodata) {

/*        We load the TAI-UTC offsets and formal leapsecond epochs */
/*        into the TAITAB.  (We will modify this array in a minute). */

	gdpool_("DELTET/DELTA_AT", &c__1, &c__280, &nref, taitab, &found, (
		ftnlen)15);

/*        Make sure all of the requested data was there. */

	if (! found) {
	    nodata = TRUE_;
	    setmsg_("The variable that points to the leapseconds (DELTET/DEL"
		    "TA_AT) could not be located in the kernel pool.  It is l"
		    "ikely that the leapseconds kernel has not been loaded.", (
		    ftnlen)165);
	    sigerr_("SPICE(NOLEAPSECONDS)", (ftnlen)20);
	    chkout_("TTRANS", (ftnlen)6);
	    return 0;
	}

/*        Transform the TAITAB in place to give the TAI time tag */
/*        at the beginning of the UTC day in which a leap */
/*        second occurred and the TAI time tag at the beginning */
/*        of the next day.  Pictorially, the table is transformed */

/*               +----------------------+         +-------------------+ */
/*               | DELTA_1 (TAI to UTC) |         | TAI at start of   | */
/*               |                      |         | day before TAI-UTC| */
/*               |                      |         | change occurred   | */
/*               +----------------------+         +-------------------+ */
/*        from:  | First Formal time    |     to: | TAI time at start | */
/*               | associated with      |         | of next day UTC.  | */
/*               | DELTA_1              |         | after DELTA_1 jump| */
/*               +----------------------+         +-------------------+ */
/*               | DELTA_2 (TAI to UTC) |         | TAI at start of   | */
/*               |                      |         | day before TAI-UTC| */
/*               |                      |         | jump occurred     | */
/*               +----------------------+         +-------------------+ */
/*               | First Formal time    |         | TAI time at start | */
/*               | associated with      |         | of next day UTC.  | */
/*               | DELTA_2              |         | after DELTA_2 jump| */
/*               +----------------------+         +-------------------+ */
/*                        .                                . */
/*                        .                                . */
/*                        .                                . */


/*        At the same time, load the table DAYTAB. It contains the */
/*        the day number past 1 Jan 1 AD for the beginning of the */
/*        days loaded in TAITAB. */

	lastdt = taitab[0] - 1.;
	i__1 = nref;
	for (i__ = 1; i__ <= i__1; i__ += 2) {
	    offset = i__;
	    refptr = i__ + 1;
	    dt = taitab[(i__2 = offset - 1) < 280 && 0 <= i__2 ? i__2 : 
		    s_rnge("taitab", i__2, "ttrans_", (ftnlen)1216)];
	    formal = taitab[(i__2 = refptr - 1) < 280 && 0 <= i__2 ? i__2 : 
		    s_rnge("taitab", i__2, "ttrans_", (ftnlen)1217)];
	    taitab[(i__2 = offset - 1) < 280 && 0 <= i__2 ? i__2 : s_rnge(
		    "taitab", i__2, "ttrans_", (ftnlen)1218)] = formal - 
		    secspd + lastdt;
	    taitab[(i__2 = refptr - 1) < 280 && 0 <= i__2 ? i__2 : s_rnge(
		    "taitab", i__2, "ttrans_", (ftnlen)1219)] = formal + dt;
	    daynum = (integer) ((formal + halfd) / secspd) + dn2000;
	    daytab[(i__2 = offset - 1) < 280 && 0 <= i__2 ? i__2 : s_rnge(
		    "daytab", i__2, "ttrans_", (ftnlen)1224)] = daynum - 1;
	    daytab[(i__2 = refptr - 1) < 280 && 0 <= i__2 ? i__2 : s_rnge(
		    "daytab", i__2, "ttrans_", (ftnlen)1225)] = daynum;
	    lastdt = dt;
	}

/*        Since we don't have to do it very often, make sure the */
/*        times in the TAI table are in increasing order. */

	i__1 = nref;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    nodata = TRUE_;
	    if (taitab[(i__2 = i__ - 2) < 280 && 0 <= i__2 ? i__2 : s_rnge(
		    "taitab", i__2, "ttrans_", (ftnlen)1239)] >= taitab[(i__3 
		    = i__ - 1) < 280 && 0 <= i__3 ? i__3 : s_rnge("taitab", 
		    i__3, "ttrans_", (ftnlen)1239)]) {
		setmsg_("Either the leapsecond epochs taken from the kernel "
			"pool are not properly ordered or the UTC - TAI offse"
			"ts are completely out of range. ", (ftnlen)135);
		sigerr_("SPICE(BADLEAPSECONDS)", (ftnlen)21);
		chkout_("TTRANS", (ftnlen)6);
		return 0;
	    }
	}

/*        At this point, we've completed all checks on kernel data. */

	nodata = FALSE_;
    }

/*     Make local normalized copies of FROM and TO. */

    nextwd_(from, myfrom, rest, from_len, (ftnlen)32, (ftnlen)32);
    nextwd_(to, myto, rest, to_len, (ftnlen)32, (ftnlen)32);
    ucase_(myfrom, myfrom, (ftnlen)32, (ftnlen)32);
    ucase_(myto, myto, (ftnlen)32, (ftnlen)32);

/*     Make sure that the FROM and TO are recognized time types. */

    pto = bsrchc_(myto, &c__21, recog, (ftnlen)32, (ftnlen)8);
    pfrom = bsrchc_(myfrom, &c__21, recog, (ftnlen)32, (ftnlen)8);

/*     Eventually, we need to handle SCLKs.  When that happens */
/*     we will do it here and in a similarly marked spot at */
/*     the end of this routine.  First see if we know how to */
/*     handle the FROM system. */

/*     IF ( PFROM .EQ. 0 ) THEN */

/*        CALL ISSCLK ( FROM,ERROR, FOUND ) */

/*        IF ( .NOT. FOUND ) THEN */
/*           IF ( ERROR .NE. ' ' ) THEN */
/*              CALL SETMSG ( ERROR ) */
/*              CALL SIGERR ( 'SPICE(TIMESYSTEMPROBLEM)' ) */
/*              CALL CHKOUT ( 'TTRANS' ) */
/*              RETURN */
/*           END IF */
/*        ELSE */
/*           CALL SCLKTV ( FROM, TVEC ) */
/*           PFROM = TDB */
/*        END IF */

/*     END IF */

/*     Now check to see if we know how to handle the  TO system. */

/*     IF ( PTO .EQ. 0 ) THEN */

/*        CALL ISSCLK ( TO, ERROR, FOUND ) */

/*        IF ( .NOT. FOUND ) THEN */

/*           IF ( ERROR .NE. ' ' ) THEN */
/*              CALL SETMSG ( ERROR ) */
/*              CALL SIGERR ( 'SPICE(TIMESYSTEMPROBLEM)' ) */
/*              CALL CHKOUT ( 'TTRANS' ) */
/*           END IF */

/*        ELSE */

/*           MKSCLK = .TRUE. */
/*           PTO    =  TDB */

/*        END IF */

/*     END IF */


/*     For now we are NOT going to deal with SCLK so if something */
/*     isn't recognized, we can just signal an error and quit. */

    if (pfrom == 0) {
	setmsg_("The FROM time representation '#' is not recognized. ", (
		ftnlen)52);
	errch_("#", from, (ftnlen)1, from_len);
	sigerr_("SPICE(UNKNONWNTIMESYSTEM)", (ftnlen)25);
	chkout_("TTRANS", (ftnlen)6);
	return 0;
    } else if (pto == 0) {
	setmsg_("The TO time representation '#' is not recognized. ", (ftnlen)
		50);
	errch_("#", from, (ftnlen)1, from_len);
	sigerr_("SPICE(UNKNONWNTIMESYSTEM)", (ftnlen)25);
	chkout_("TTRANS", (ftnlen)6);
	return 0;
    }

/*     OK.  We have made our last attempt at diagnosing a user error. */
/*     From this point on we assume that the user input exactly what */
/*     was intended. */

/*     We convert the time vector to days past 1 jan 01 and seconds */
/*     past the beginning of the day.  None of the cases below */
/*     are particularly tricky.  There's just a lot of cases. */

    if (pfrom == 16 || pfrom == 17) {
	year = (integer) tvec[0];
	month = (integer) tvec[1];
	day = (integer) tvec[2];
	i__1 = month - 1;
	rmaini_(&i__1, &c__12, &dyear, &month);
	year += dyear;
	++month;
	doffst = 0;
	if (year <= 0) {
	    rmaini_(&year, &c__400, &yr400, &tempi);
	    year = tempi;
	    if (year == 0) {
		year += 400;
		--yr400;
	    }
	    doffst = yr400 * 146097;
	}
/* Computing MAX */
	i__3 = 0, i__4 = abs(year) / c__4 * c__4 + 1 - abs(year);
/* Computing MAX */
	i__5 = 0, i__6 = abs(year) / c__100 * c__100 + 1 - abs(year);
/* Computing MAX */
	i__7 = 0, i__8 = abs(year) / c__400 * c__400 + 1 - abs(year);
	daynum = (year - 1) * 365 + (year - 1) / 4 - (year - 1) / 100 + (year 
		- 1) / 400 + (dpjan0[(i__1 = month - 1) < 12 && 0 <= i__1 ? 
		i__1 : s_rnge("dpjan0", i__1, "ttrans_", (ftnlen)1378)] + 
		extra[(i__2 = month - 1) < 12 && 0 <= i__2 ? i__2 : s_rnge(
		"extra", i__2, "ttrans_", (ftnlen)1378)] * (max(i__3,i__4) - 
		max(i__5,i__6) + max(i__7,i__8)) + day) - 1 + doffst;

/*        Calculate seconds from midnight, 00:00:00. */

	secs = tvec[3] * 3600. + tvec[4] * 60. + tvec[5];
    } else if (pfrom == 12 || pfrom == 15) {
	year = (integer) tvec[0];
	day = (integer) tvec[1];
	month = 1;
	doffst = 0;
	if (year <= 0) {
	    rmaini_(&year, &c__400, &yr400, &tempi);
	    year = tempi;
	    if (year == 0) {
		year += 400;
		--yr400;
	    }
	    doffst = yr400 * 146097;
	}
/* Computing MAX */
	i__3 = 0, i__4 = abs(year) / c__4 * c__4 + 1 - abs(year);
/* Computing MAX */
	i__5 = 0, i__6 = abs(year) / c__100 * c__100 + 1 - abs(year);
/* Computing MAX */
	i__7 = 0, i__8 = abs(year) / c__400 * c__400 + 1 - abs(year);
	daynum = (year - 1) * 365 + (year - 1) / 4 - (year - 1) / 100 + (year 
		- 1) / 400 + (dpjan0[(i__1 = month - 1) < 12 && 0 <= i__1 ? 
		i__1 : s_rnge("dpjan0", i__1, "ttrans_", (ftnlen)1408)] + 
		extra[(i__2 = month - 1) < 12 && 0 <= i__2 ? i__2 : s_rnge(
		"extra", i__2, "ttrans_", (ftnlen)1408)] * (max(i__3,i__4) - 
		max(i__5,i__6) + max(i__7,i__8)) + day) - 1 + doffst;

/*        Calculate seconds from midnight, 00:00:00. */

	secs = tvec[2] * 3600. + tvec[3] * 60. + tvec[4];
    } else if (pfrom == 13 || pfrom == 14) {
	year = (integer) tvec[0];
	day = (integer) tvec[1];
	month = 1;
	doffst = 0;
	if (year <= 0) {
	    rmaini_(&year, &c__400, &yr400, &tempi);
	    year = tempi;
	    if (year == 0) {
		year += 400;
		--yr400;
	    }
	    doffst = yr400 * 146097;
	}
	frac = tvec[1] - (doublereal) day;
/* Computing MAX */
	i__3 = 0, i__4 = abs(year) / c__4 * c__4 + 1 - abs(year);
/* Computing MAX */
	i__5 = 0, i__6 = abs(year) / c__100 * c__100 + 1 - abs(year);
/* Computing MAX */
	i__7 = 0, i__8 = abs(year) / c__400 * c__400 + 1 - abs(year);
	daynum = (year - 1) * 365 + (year - 1) / 4 - (year - 1) / 100 + (year 
		- 1) / 400 + (dpjan0[(i__1 = month - 1) < 12 && 0 <= i__1 ? 
		i__1 : s_rnge("dpjan0", i__1, "ttrans_", (ftnlen)1439)] + 
		extra[(i__2 = month - 1) < 12 && 0 <= i__2 ? i__2 : s_rnge(
		"extra", i__2, "ttrans_", (ftnlen)1439)] * (max(i__3,i__4) - 
		max(i__5,i__6) + max(i__7,i__8)) + day) - 1 + doffst;

/*        Normally the length of a day is 86400 seconds, but this day */
/*        might be a leapsecond day.  We will set DAYLEN to SECSPD and */
/*        change it if it turns out this is a day with a leapsecond. */

	if (pfrom == 14) {
	    secs = frac * secspd;
	} else {
	    daylen = secspd;
	    dayptr = lstlei_(&daynum, &nref, daytab);
	    if (odd_(&dayptr)) {
		daylen = taitab[(i__1 = dayptr) < 280 && 0 <= i__1 ? i__1 : 
			s_rnge("taitab", i__1, "ttrans_", (ftnlen)1454)] - 
			taitab[(i__2 = dayptr - 1) < 280 && 0 <= i__2 ? i__2 :
			 s_rnge("taitab", i__2, "ttrans_", (ftnlen)1454)];
	    }
	    secs = frac * daylen;
	}
    } else if (pfrom == 4) {

/*        First lets get the number of days since 1-Jan-2000 00:00:00 */

	d__1 = tvec[0] + halfd;
	rmaind_(&d__1, &secspd, &dp2000, &secs);
	daynum = (integer) dp2000 + dn2000;
    } else if (pfrom == 7) {

/*        JD1101 is the julian date UTC of Jan 1, 1 AD. */

	jdsecs = (tvec[0] - jd1101) * secspd;
	rmaind_(&jdsecs, &secspd, &daydp, &secs);
	daynum = (integer) daydp;
    } else if (pfrom == 1) {
	daynum = (integer) tvec[0];
	secs = tvec[1];
    } else if (pfrom == 2) {
	daynum = (integer) tvec[0] + dn2000;
	secs = tvec[1];
    } else if (pfrom == 20 || pfrom == 21) {
	year = (integer) tvec[0];
	week = (integer) tvec[1] - 1;
	wkday = (integer) tvec[2];
	month = 1;

/*        Compute the days past 1 jan 1 of the beginning of this */
/*        year and month. */

	doffst = 0;
	if (year <= 0) {
	    rmaini_(&year, &c__400, &yr400, &tempi);
	    year = tempi;
	    if (year == 0) {
		year += 400;
		--yr400;
	    }
	    doffst = yr400 * 146097;
	}
/* Computing MAX */
	i__3 = 0, i__4 = abs(year) / c__4 * c__4 + 1 - abs(year);
/* Computing MAX */
	i__5 = 0, i__6 = abs(year) / c__100 * c__100 + 1 - abs(year);
/* Computing MAX */
	i__7 = 0, i__8 = abs(year) / c__400 * c__400 + 1 - abs(year);
	daynum = (year - 1) * 365 + (year - 1) / 4 - (year - 1) / 100 + (year 
		- 1) / 400 + (dpjan0[(i__1 = month - 1) < 12 && 0 <= i__1 ? 
		i__1 : s_rnge("dpjan0", i__1, "ttrans_", (ftnlen)1527)] + 
		extra[(i__2 = month - 1) < 12 && 0 <= i__2 ? i__2 : s_rnge(
		"extra", i__2, "ttrans_", (ftnlen)1527)] * (max(i__3,i__4) - 
		max(i__5,i__6) + max(i__7,i__8)) + c__1) - 1 + doffst;
	i__1 = daynum - sunday;
	rmaini_(&i__1, &c__7, &qint, &dpsun);
	fyrday = dpsun + 1;
	i__1 = wkday - fyrday;
	rmaini_(&i__1, &c__7, &qint, &offset);
	daynum = daynum + week * 7 + offset;

/*        Calculate seconds from midnight, 00:00:00. */

	secs = tvec[3] * 3600. + tvec[4] * 60. + tvec[5];
    } else if (pfrom == 18 || pfrom == 19) {
	year = (integer) tvec[0];
	month = (integer) tvec[1];
	week = (integer) tvec[2] - 1;
	day = (integer) tvec[3];
	doffst = 0;
	if (year <= 0) {
	    rmaini_(&year, &c__400, &yr400, &tempi);
	    year = tempi;
	    if (year == 0) {
		year += 400;
		--yr400;
	    }
	    doffst = yr400 * 146097;
	}
/* Computing MAX */
	i__3 = 0, i__4 = abs(year) / c__4 * c__4 + 1 - abs(year);
/* Computing MAX */
	i__5 = 0, i__6 = abs(year) / c__100 * c__100 + 1 - abs(year);
/* Computing MAX */
	i__7 = 0, i__8 = abs(year) / c__400 * c__400 + 1 - abs(year);
	daynum = (year - 1) * 365 + (year - 1) / 4 - (year - 1) / 100 + (year 
		- 1) / 400 + (dpjan0[(i__1 = month - 1) < 12 && 0 <= i__1 ? 
		i__1 : s_rnge("dpjan0", i__1, "ttrans_", (ftnlen)1566)] + 
		extra[(i__2 = month - 1) < 12 && 0 <= i__2 ? i__2 : s_rnge(
		"extra", i__2, "ttrans_", (ftnlen)1566)] * (max(i__3,i__4) - 
		max(i__5,i__6) + max(i__7,i__8)) + c__1) - 1 + doffst;
	i__1 = daynum - sunday;
	rmaini_(&i__1, &c__7, &qint, &dpsun);
	fmday = dpsun + 1;
	i__1 = day - fmday;
	rmaini_(&i__1, &c__7, &qint, &offset);
	daynum = daynum + week * 7 + offset;

/*        Calculate seconds from midnight, 00:00:00. */

	secs = tvec[4] * 3600. + tvec[5] * 60. + tvec[6];

/*     If we get to this point the type must be one of the continuous */
/*     time types: 'TAI', 'TDT', 'TDB', 'JED', 'ET', 'JDTDT', 'JDTDB'. */

    } else {

/*        If the output time is one of the continuous time systems */
/*        we can take a short cut and just perform the computation */
/*        directly. */

	if (elemc_(myto, unifrm, (ftnlen)32, (ftnlen)8)) {
	    tvec[0] = unitim_(tvec, myfrom, myto, (ftnlen)32, (ftnlen)32);
	    chkout_("TTRANS", (ftnlen)6);
	    return 0;
	}

/*        The output time system isn't one of the uniform time systems. */
/*        Convert what we have to TAI and then to the DAYNUM, SECOND */
/*        representation. */

	tai = unitim_(tvec, myfrom, "TAI", (ftnlen)32, (ftnlen)3);
	taiptr = lstled_(&tai, &nref, taitab);

/*        If the TAIPTR value is odd, then the TAI time falls during */
/*        a day with a leap second.  We can just look up the day */
/*        number and compute the number of seconds into that */
/*        day directly ... */

	if (odd_(&taiptr)) {
	    daynum = daytab[(i__1 = taiptr - 1) < 280 && 0 <= i__1 ? i__1 : 
		    s_rnge("daytab", i__1, "ttrans_", (ftnlen)1618)];
	    secs = tai - taitab[(i__1 = taiptr - 1) < 280 && 0 <= i__1 ? i__1 
		    : s_rnge("taitab", i__1, "ttrans_", (ftnlen)1619)];

/*        ...Otherwise, all days since the reference TAI time have */
/*        the same number of seconds (SECSPD).  (This statement applies */
/*        to days that precede the first reference TAI time too.) */
/*        Thus we can simply compute the number of days and seconds */
/*        that have elapsed since the reference TAI time. */

	} else {

/*           If TAI is before the first time in the table, we can */
/*           compute the number of days and seconds before the first */
/*           entry in the TAI table. */

	    taiptr = max(taiptr,1);
	    d__1 = tai - taitab[(i__1 = taiptr - 1) < 280 && 0 <= i__1 ? i__1 
		    : s_rnge("taitab", i__1, "ttrans_", (ftnlen)1638)];
	    rmaind_(&d__1, &secspd, &daydp, &secs);
	    daynum = (integer) daydp + daytab[(i__1 = taiptr - 1) < 280 && 0 
		    <= i__1 ? i__1 : s_rnge("daytab", i__1, "ttrans_", (
		    ftnlen)1641)];
	}
    }
    if (forml[(i__1 = pfrom - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("forml", 
	    i__1, "ttrans_", (ftnlen)1648)]) {
	rmaind_(&secs, &secspd, &daydp, &tsecs);
	daynum += (integer) daydp;
	secs = tsecs;
    }
/*     ================================================================== */

/*     Force the seconds into the range 0 to 86401 or 86400 */
/*     depending upon whether or not the output system is a formal */
/*     time system or not. */

    if (forml[(i__1 = pto - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("forml", 
	    i__1, "ttrans_", (ftnlen)1663)] && forml[(i__2 = pfrom - 1) < 21 
	    && 0 <= i__2 ? i__2 : s_rnge("forml", i__2, "ttrans_", (ftnlen)
	    1663)]) {

/*        We don't have to do anything here. */

    } else {
	if (secs > secspd - 1. || secs < 0.) {

/*           First convert to TAI... */

/* Computing MAX */
	    i__1 = 1, i__2 = lstlei_(&daynum, &nref, daytab);
	    dayptr = max(i__1,i__2);
	    secs += (doublereal) (daynum - daytab[(i__1 = dayptr - 1) < 280 &&
		     0 <= i__1 ? i__1 : s_rnge("daytab", i__1, "ttrans_", (
		    ftnlen)1676)]) * secspd;
	    tai = taitab[(i__1 = dayptr - 1) < 280 && 0 <= i__1 ? i__1 : 
		    s_rnge("taitab", i__1, "ttrans_", (ftnlen)1678)] + secs;

/*           ...then back to DAYNUM and SECS */

	    taiptr = lstled_(&tai, &nref, taitab);
	    if (odd_(&taiptr)) {
		daynum = daytab[(i__1 = taiptr - 1) < 280 && 0 <= i__1 ? i__1 
			: s_rnge("daytab", i__1, "ttrans_", (ftnlen)1687)];
		secs = tai - taitab[(i__1 = taiptr - 1) < 280 && 0 <= i__1 ? 
			i__1 : s_rnge("taitab", i__1, "ttrans_", (ftnlen)1688)
			];
	    } else {
		taiptr = max(1,taiptr);
		daynum = daytab[(i__1 = taiptr - 1) < 280 && 0 <= i__1 ? i__1 
			: s_rnge("daytab", i__1, "ttrans_", (ftnlen)1694)];
		d__1 = tai - taitab[(i__1 = taiptr - 1) < 280 && 0 <= i__1 ? 
			i__1 : s_rnge("taitab", i__1, "ttrans_", (ftnlen)1696)
			];
		rmaind_(&d__1, &secspd, &daydp, &secs);
		daynum += (integer) daydp;
	    }
	}
    }

/*     One last thing.  If we are going to a formal time vector, */
/*     we want to ignore positive leapseconds. (Negative ones */
/*     were handled above, the clock jumped ahead one second */
/*     when the second hand got to 59.) */

/*     The idea is that we want the clock */
/*     to stand still during the leapsecond.  Yeah this is bogus, */
/*     but people with analog clocks don't have any other choice. */

/*     We are in a positive leapsecond only if SECS is greater than */
/*     the number of seconds in a normal day.  In that case we */
/*     increment the day number by one and set SECS to zero. */

    if (forml[(i__1 = pto - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("forml", 
	    i__1, "ttrans_", (ftnlen)1719)] && secs > secspd) {
	++daynum;
	secs = 0.;
    }

/*     OK. Now we have DAYNUM and SECS,  convert this form to the */
/*     one requested. */

/*     If there is a 'Y' in the form we are to convert to, then we */
/*     will need some form of year, etc.  Do the work now and sort it */
/*     it all out at the appropriate time later on. */

    if (needy[(i__1 = pto - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("needy", 
	    i__1, "ttrans_", (ftnlen)1731)]) {
	yr400 = daynum / 146097;
	rem = daynum - yr400 * 146097;

/*        We want to be able to deal with years prior to  1 Jan 1 */
/*        So we make sure the remainder is positive. */

	if (rem < 0) {
	    --yr400;
	    rem += 146097;
	}
/* Computing MIN */
	i__1 = 3, i__2 = rem / 36524;
	yr100 = min(i__1,i__2);
	rem -= yr100 * 36524;
/* Computing MIN */
	i__1 = 24, i__2 = rem / 1461;
	yr4 = min(i__1,i__2);
	rem -= yr4 * 1461;
/* Computing MIN */
	i__1 = 3, i__2 = rem / 365;
	yr1 = min(i__1,i__2);
	rem -= yr1 * 365;
	dofyr = rem + 1;
	year = yr400 * 400 + yr100 * 100 + (yr4 << 2) + yr1 + 1;
/* Computing MAX */
	i__1 = 0, i__2 = abs(year) / c__4 * c__4 + 1 - abs(year);
/* Computing MAX */
	i__3 = 0, i__4 = abs(year) / c__100 * c__100 + 1 - abs(year);
/* Computing MAX */
	i__5 = 0, i__6 = abs(year) / c__400 * c__400 + 1 - abs(year);
	if (max(i__1,i__2) - max(i__3,i__4) + max(i__5,i__6) == 0) {
	    month = lstlti_(&dofyr, &c__12, dpjan0);
	    day = dofyr - dpjan0[(i__1 = month - 1) < 12 && 0 <= i__1 ? i__1 :
		     s_rnge("dpjan0", i__1, "ttrans_", (ftnlen)1759)];
	} else {
	    month = lstlti_(&dofyr, &c__12, dpbegl);
	    day = dofyr - dpbegl[(i__1 = month - 1) < 12 && 0 <= i__1 ? i__1 :
		     s_rnge("dpbegl", i__1, "ttrans_", (ftnlen)1762)];
	}

/*        We only want to convert that portion of seconds less than */
/*        86399 to hours, minutes and seconds.  Take anything extra */
/*        and put it in EXSECS. */

/* Computing MAX */
	d__1 = 0., d__2 = secs - secspd + 1;
	exsecs = max(d__1,d__2);
	tsecs = secs - exsecs;
	rmaind_(&tsecs, &c_b188, &hours, &tempd);
	rmaind_(&tempd, &c_b189, &mins, &tsecs);
	tsecs += exsecs;
    }
/* ===================================================================== */

/*     Finally, we convert to the requested output. */

    if (pto == 16 || pto == 17) {
	tvec[0] = (doublereal) year;
	tvec[1] = (doublereal) month;
	tvec[2] = (doublereal) day;
	tvec[3] = hours;
	tvec[4] = mins;
	tvec[5] = tsecs;
    } else if (pto == 12 || pto == 15) {
	tvec[0] = (doublereal) year;
	tvec[1] = (doublereal) dofyr;
	tvec[2] = hours;
	tvec[3] = mins;
	tvec[4] = tsecs;
    } else if (pto == 13 || pto == 14) {
	tvec[0] = (doublereal) year;
	if (pto == 13) {
	    dayptr = lstlei_(&daynum, &nref, daytab);
	    daylen = secspd;
	    if (odd_(&dayptr)) {
		daylen = taitab[(i__1 = dayptr) < 280 && 0 <= i__1 ? i__1 : 
			s_rnge("taitab", i__1, "ttrans_", (ftnlen)1811)] - 
			taitab[(i__2 = dayptr - 1) < 280 && 0 <= i__2 ? i__2 :
			 s_rnge("taitab", i__2, "ttrans_", (ftnlen)1811)];
	    }
	    tvec[1] = (doublereal) dofyr + secs / daylen;
	} else {
	    tvec[1] = (doublereal) dofyr + secs / secspd;
	}
    } else if (pto == 4) {
	tvec[0] = (doublereal) (daynum - dn2000) * secspd - halfd + secs;
    } else if (pto == 7) {
	tvec[0] = jd1101 + (doublereal) daynum + secs / secspd;
    } else if (pto == 1) {
	tvec[0] = (doublereal) daynum;
	tvec[1] = secs;
    } else if (pto == 2) {
	tvec[0] = (doublereal) (daynum - dn2000);
	tvec[1] = secs;
    } else if (pto == 20 || pto == 21) {

/*        First compute the day of the week, and the week number */

	i__1 = daynum - sunday;
	rmaini_(&i__1, &c__7, &qint, &day);
	week = (dofyr - 1) / 7 + 1;

/*        Now just put everything where it belongs. */

	tvec[0] = (doublereal) year;
	tvec[1] = (doublereal) week;
	tvec[2] = (doublereal) day + 1.;
	tvec[3] = hours;
	tvec[4] = mins;
	tvec[5] = tsecs;
    } else if (pto == 18 || pto == 19) {

/*        First compute how many weeks into the month DAYNUM is, */
/*        and compute the day of week number. */

	tvec[0] = (doublereal) year;
	doffst = 0;
	if (year <= 0) {
	    rmaini_(&year, &c__400, &yr400, &tempi);
	    year = tempi;
	    if (year == 0) {
		year += 400;
		--yr400;
	    }
	    doffst = yr400 * 146097;
	}
/* Computing MAX */
	i__3 = 0, i__4 = abs(year) / c__4 * c__4 + 1 - abs(year);
/* Computing MAX */
	i__5 = 0, i__6 = abs(year) / c__100 * c__100 + 1 - abs(year);
/* Computing MAX */
	i__7 = 0, i__8 = abs(year) / c__400 * c__400 + 1 - abs(year);
	week = (daynum - ((year - 1) * 365 + (year - 1) / 4 - (year - 1) / 
		100 + (year - 1) / 400 + (dpjan0[(i__1 = month - 1) < 12 && 0 
		<= i__1 ? i__1 : s_rnge("dpjan0", i__1, "ttrans_", (ftnlen)
		1880)] + extra[(i__2 = month - 1) < 12 && 0 <= i__2 ? i__2 : 
		s_rnge("extra", i__2, "ttrans_", (ftnlen)1880)] * (max(i__3,
		i__4) - max(i__5,i__6) + max(i__7,i__8)) + c__1) - 1) - 
		doffst) / 7 + 1;
	i__1 = daynum - sunday;
	rmaini_(&i__1, &c__7, &qint, &day);

/*        Now just move the remaining stuff into TVEC. */

	tvec[1] = (doublereal) month;
	tvec[2] = (doublereal) week;
	tvec[3] = (doublereal) day + 1.;
	tvec[4] = hours;
	tvec[5] = mins;
	tvec[6] = tsecs;

/*     If we get to this point the type must be one of the continuous */
/*     time types: 'TAI', 'TDT', 'TDB', 'JED', 'ET', 'JDTDT', 'JDTDB'. */

/*     First convert to TAI and then to the appropriate output type. */

    } else {
/* Computing MAX */
	i__1 = 1, i__2 = lstlei_(&daynum, &nref, daytab);
	dayptr = max(i__1,i__2);
	secs += (doublereal) (daynum - daytab[(i__1 = dayptr - 1) < 280 && 0 
		<= i__1 ? i__1 : s_rnge("daytab", i__1, "ttrans_", (ftnlen)
		1902)]) * secspd;
	tai = taitab[(i__1 = dayptr - 1) < 280 && 0 <= i__1 ? i__1 : s_rnge(
		"taitab", i__1, "ttrans_", (ftnlen)1904)] + secs;
	tvec[0] = unitim_(&tai, "TAI", myto, (ftnlen)3, (ftnlen)32);
    }

/*     Here's where we will handle conversion to SCLK when */
/*     we get around to implementing that portion of TTRANS */


/*     IF ( MKSCLK ) THEN */
/*        CALL TVSCLK ( TO, TVEC ) */
/*     END IF */

/*     END IF */

    chkout_("TTRANS", (ftnlen)6);
    return 0;
} /* ttrans_ */

