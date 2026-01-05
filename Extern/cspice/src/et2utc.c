/* et2utc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static doublereal c_b22 = 10.;
static integer c__1 = 1;

/* $Procedure ET2UTC ( Ephemeris Time to UTC ) */
/* Subroutine */ int et2utc_(doublereal *et, char *format, integer *prec, 
	char *utcstr, ftnlen format_len, ftnlen utcstr_len)
{
    /* Initialized data */

    static char mthnam[3*12] = "JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" 
	    "AUG" "SEP" "OCT" "NOV" "DEC";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6;
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double d_int(doublereal *), pow_di(doublereal *, integer *), d_nint(
	    doublereal *);
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer), 
	    i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer bday, eday, year;
    static doublereal tvec[8];
    static integer hour, i__;
    static doublereal scale;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char fract[80];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen);
    static integer month;
    extern logical failed_(void);
    static doublereal frcsec;
    static integer second;
    static doublereal whlsec;
    static integer bmonth, emonth;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), prefix_(char *, integer *, char *, ftnlen, ftnlen);
    static char endstr[80];
    static integer myprec, minute;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), dpstrf_(doublereal *,
	     integer *, char *, char *, ftnlen, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen);
    extern doublereal unitim_(doublereal *, char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), ttrans_(
	    char *, char *, doublereal *, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);
    static integer bsc, esc, bhr, bmn;
    static doublereal tai;
    static integer day, ehr, emn;
    static char fmt[4], str[80];

/* $ Abstract */

/*     Convert an input time from ephemeris seconds past J2000 */
/*     to Calendar, Day-of-Year, or Julian Date format, UTC. */

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

/*     TIME */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Epoch, given in ephemeris seconds past J2000. */
/*     FORMAT     I   Format of output epoch. */
/*     PREC       I   Digits of precision in fractional seconds or days. */
/*     UTCSTR     O   Output time string, UTC. */

/* $ Detailed_Input */

/*     ET       is the input epoch, ephemeris seconds past J2000. */

/*     FORMAT   is the format of the output time string. It may be */
/*              any of the following: */

/*                 'C'      Calendar format, UTC. */

/*                 'D'      Day-of-Year format, UTC. */

/*                 'J'      Julian Date format, UTC. */

/*                 'ISOC'   ISO Calendar format, UTC. */

/*                 'ISOD'   ISO Day-of-Year format, UTC. */

/*     PREC     is the number of digits of precision to which */
/*              fractional seconds (for Calendar and Day-of-Year */
/*              formats) or days (for Julian Date format) are to */
/*              be computed. If PREC is zero or smaller, no decimal */
/*              point is appended to the output string. If PREC is */
/*              greater than 14, it is treated as 14. */

/* $ Detailed_Output */

/*     UTCSTR   is the output time string equivalent to the input */
/*              epoch, in the specified format. Some examples are */
/*              shown below. */

/*                 'C'      '1986 APR 12 16:31:09.814' */
/*                 'D'      '1986-102 // 16:31:12.814' */
/*                 'J'      'JD 2446533.18834276' */
/*                 'ISOC'   '1987-04-12T16:31:12.814' */
/*                 'ISOD'   '1987-102T16:31:12.814' */

/*              If an error occurs, UTCSTR is not changed. */

/*              Fractional seconds, or for Julian dates, fractional */
/*              days, are rounded to the precision level specified */
/*              by the input argument PREC. */

/*              UTCSTR should be declared to be at least */
/*              20 + PREC characters in length to ensure */
/*              sufficient room to hold calendar strings */
/*              for modern epochs. For epochs prior to */
/*              1000 A.D. at least 24 + PREC characters in */
/*              length are required to hold the output */
/*              calendar string. */

/*              For epochs prior to 1000 A.D. Jan 1 calendar */
/*              and day of year formats are returned with the */
/*              era (A.D. or B.C.) attached to the year. For */
/*              example */

/*                 '877 A.D. MAR 17 13:29:11.829' */
/*                 '471 B.C. Jan 01 12:00:00.000' */
/*                 '471 B.C. 001 // 12:00:00.000' */

/*              ISO formats do not support the inclusion of an era. */
/*              For years prior to 1 A.D. an error will be signaled */
/*              if ISO format has been requested. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the format for the output string is not recognized, the */
/*         error SPICE(INVALIDTIMEFORMAT) is signaled. */

/*     2)  If PREC is less than or equal to zero, it is treated as */
/*         zero. If PREC is greater than 14, it is treated as 14. */

/*     3)  If one of the ISO formats is specified (ISOC or ISOD) but the */
/*         year corresponding to ET is prior to 1 A.D. on the Gregorian */
/*         Calendar, the error SPICE(YEAROUTOFRANGE) is signaled. */

/*     4)  Epochs prior to 15 Oct, 1582 on the Gregorian calendar (the */
/*         calendar commonly used in western societies) are returned in */
/*         the "extended" Gregorian Calendar. To convert epochs to the */
/*         Julian calendar see the SPICELIB routine GR2JUL. */

/*     5)  This routine does not attempt to account for variations */
/*         in the length of the second that were in effect prior */
/*         to Jan 1, 1972. For days prior to that date, we assume */
/*         there are exactly 86400 ephemeris seconds. Consequently */
/*         the UTC Gregorian calendar strings produced for epochs */
/*         prior to Jan 1, 1972 differ from the corresponding */
/*         TDB calendar strings by approximately 41.18 seconds. */
/*         (TDB Gregorian calendar strings are produced by the */
/*         routine ETCAL). */

/*     6)  If a leapseconds kernel has not been loaded prior to calling */
/*         this routine, an error is signaled by a routine in the */
/*         call tree of this routine. */

/* $ Files */

/*     A leapseconds kernel must be loaded via FURNSH prior to calling */
/*     this routine. The kernel need be loaded only once during a program */
/*     run. */

/* $ Particulars */

/*     This routine handles the task of converting a double precision */
/*     representation of an epoch to a character string suitable for */
/*     human consumption. The more general routine TIMOUT may also be */
/*     used to convert ET to time strings. */

/* $ Examples */

/*     Let the value of ET be -527644192.5403653 ephemeris seconds */
/*     past J2000. Assuming that the nominal values in the kernel pool */
/*     have not been altered, the following calls */

/*        CALL ET2UTC ( ET, 'C', 0, UTCSTR ) */
/*        CALL ET2UTC ( ET, 'C', 3, UTCSTR ) */
/*        CALL ET2UTC ( ET, 'D', 5, UTCSTR ) */
/*        CALL ET2UTC ( ET, 'J', 7, UTCSTR ) */

/*     produce the following output strings */

/*        '1983 APR 13 12:09:14' */
/*        '1983 APR 13 12:09:14.274' */
/*        '1983-103 // 12:09:14.27400' */
/*        'JD 2445438.0064152' */

/*     respectively. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  J. Jespersen and J. Fitz-Randolph, "From Sundials to Atomic */
/*          Clocks, Understanding Time and Frequency," Dover */
/*          Publications, Inc. New York, 1982. */

/* $ Author_and_Institution */

/*     C.H. Acton         (JPL) */
/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     J.M. Lynch         (JPL) */
/*     W.M. Owen          (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.5, 24-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Extended $Files */
/*        section. */

/* -    SPICELIB Version 3.0.4, 06-APR-2009 (NJB) */

/*        Header was updated to state that fractional */
/*        seconds or days are rounded in the output */
/*        string. */

/* -    SPICELIB Version 3.0.3, 28-JAN-2008 (BVS) */

/*        Fixed typo in the ISOC example string in $Detailed_Output. */

/* -    SPICELIB Version 3.0.2, 29-JUL-2003 (NJB) (CHA) */

/*        Various header changes were made to improve clarity and */
/*        more fully explain the routine's functionality. */

/* -    SPICELIB Version 3.0.1, 14-SEP-2000 (EDW) */

/*        Added FAILED check after TTRANS call during the calendar "C" */
/*        format processing to catch failure signal from TTRANS. */
/*        Lack of this check caused CSPICE based programs to core dump */
/*        if ET2UTC was called without a leapseconds kernel while */
/*        error action was set to RETURN. */

/* -    SPICELIB Version 3.0.0, 13-MAR-1996 (WLT) */

/*        The construction of the numerical components of the */
/*        output string are now handled by the SPICELIB routine */
/*        TTRANS. */

/*        In addition the routine now supports the ISO formats and */
/*        the era associated with an epoch (B.C. or A.D.) in non */
/*        ISO formats. */

/* -    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG) */

/*        Removed some potential compile warnings that could be caused */
/*        by truncation of double precision values to integers through */
/*        a direct assignment. The direct assignment has been replaced */
/*        with a call to the intrinsic function IDINT. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 21-MAR-1991 (NJB) (JML) */

/*        Two bugs involving rounding errors were corrected. One of */
/*        the bugs caused conversion errors of magnitude as large as */
/*        1 second. See $Revisions for details. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (IMU) */

/* -& */
/* $ Index_Entries */

/*     ephemeris time to utc */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.0.0, 13-MAR-1996 (WLT) */

/*        The construction of the numerical components of the */
/*        output string are now handled by the SPICELIB routine */
/*        TTRANS. */

/*        In addition the routine now supports the era associated */
/*        with an epoch (B.C. or A.D.) */

/* -    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG) */

/*        Removed some potential compile warnings that could be */
/*        caused by truncation of double precision values to */
/*        integers through a direct assignment. The direct */
/*        assignment has been replaced with a call to the */
/*        intrinsic function IDINT. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was */
/*        added following the header. */

/* -    SPICELIB Version 2.0.0, 21-MAR-1991 (NJB) (JML) */

/*        1) In the previous version of this routine, the algorithm */
/*        that was used permitted inconsistent conversion of the */
/*        integer and fractional parts of the UTC value */
/*        corresponding to the input ET value. */

/*           In the case where rounding the double precision UTC */
/*           time corresponding to the input ET value to PREC */
/*           decimal places resulted in a carry (to the integer */
/*           portion of the UTC value), the integer portion of the */
/*           UTC value was treated correctly, but the fraction was */
/*           not always rounded correctly. The specific case where */
/*           the problem occurred was when the input ET value */
/*           mapped to a UTC time having a fractional part that */
/*           rounded up to 1.0 when rounded PREC decimal places, */
/*           but that did not round up to 1.0 when rounded to the */
/*           nearest PREC+1 decimal places. The set of such */
/*           fractions can be represented as */

/*             { 1 - EPSILON :      EPSILON  <  5 * ( 10 ** */
/*             -(PREC+1) ) - and */

/*                                  EPSILON  >  5 * ( 10 ** */
/*                                  -(PREC+2) ) */

/*                                                                     } */

/*           For example, if the input ET mapped to the UTC time */

/*              2 JAN 1991 00:34:12.99994, */

/*           then a call to this routine with PREC set to 3 would */
/*           result in the output */

/*              2 JAN 1991 00:34:13.999 */

/*           instead of the correct value */

/*              2 JAN 1991 00:34:13.000 */

/*           On the other hand, if the input ET mapped to the UTC */
/*           time */

/*              2 JAN 1991 00:34:12.99996, */

/*           then a call to this routine with PREC set to 3 would */
/*           result in the correct output. */

/*           This error was apparently difficult to generate: it */
/*           has never been reported by any SPICELIB users, and was */
/*           eventually discovered by NAIF staff. */

/*        2) The second bug is somewhat less severe, as far as the */
/*        magnitude of the error is concerned. However, it's */
/*        easier to generate this error. Namely, in some cases, */
/*        the fractional part of the input ET value is rounded to */
/*        PREC SIGNIFICANT DIGITS, rather than to PREC decimal */
/*        places. The effect of this is that the fraction is */
/*        occasionally truncated rather than rounded. For example, */
/*        the ET value equivalent to the UTC string */

/*              1991 JAN 2 00:34:12.0009 */

/*           would be converted to */

/*              1991 JAN 2 00:34:12.000 */

/*           instead of the correct value */

/*              1991 JAN 2 00:34:12.001 */

/*           when the input argument PREC was set equal to 3. */

/*        The modifications made to solve these problems are as */
/*        follows: */

/*           1)  The input ET value, after conversion to `UTC */
/*               seconds past 2000', is broken up into the sum of a */
/*               whole number of seconds and a non-negative, */
/*               fractional number of seconds. The fact that the */
/*               fractional part is non-negative simplifies the */
/*               conversion of the fraction. */

/*           2)  The fraction is rounded to PREC decimal places--- */
/*               that is, to the nearest integer multiple of */
/*               10**(-PREC). If the rounding results in a carry, */
/*               the whole number portion of the time value is */
/*               incremented by 1 second. After this step, the */
/*               whole number of seconds correctly accounts for */
/*               any necessary rounding of the fraction. */

/*           3)  The whole number portion of the time value is */
/*           passed through the inverse Muller-Wimberly algorithm */
/*           to obtain years, months, days, hours, minutes, and */
/*           whole seconds. A small fraction is added to the whole */
/*           number to prevent round-off error from occurring when */
/*           divisions are performed. */

/*           4)  The fraction is converted to a string using the */
/*               SPICELIB routine DPSTRF. To ensure that DPSTRF */
/*               produces an output string containing PREC decimal */
/*               places, an integer is added to the fraction value */
/*               before supplying it to DPSTRF. This integer */
/*               `anchors' the first significant digit of the input */
/*               value in the units place. */

/* -& */

/*     SPICELIB functions */


/*     Local Functions (Statement Functions) */


/*     Local variables */


/*     Save everything between calls */


/*     Initial values */


/*     The function NDIGIT gives the number of digits required to */
/*     display a non-negative integer that is less than 10000 */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ET2UTC", (ftnlen)6);

/*     Convert FORMAT to uppercase for ease of comparison. Make sure it's */
/*     one of the recognized formats. */

    ucase_(format, fmt, format_len, (ftnlen)4);
    if (s_cmp(fmt, "J", (ftnlen)4, (ftnlen)1) != 0 && s_cmp(fmt, "C", (ftnlen)
	    4, (ftnlen)1) != 0 && s_cmp(fmt, "D", (ftnlen)4, (ftnlen)1) != 0 
	    && s_cmp(fmt, "ISOD", (ftnlen)4, (ftnlen)4) != 0 && s_cmp(fmt, 
	    "ISOC", (ftnlen)4, (ftnlen)4) != 0) {
	setmsg_("ET2UTC: Format specification for output time string is not "
		"recognized. Valid specifications are: 'C', 'D', 'J', 'ISOC',"
		" or 'ISOD'. The supplied format was '#'. ", (ftnlen)160);
	errch_("#", format, (ftnlen)1, format_len);
	sigerr_("SPICE(INVALIDTIMEFORMAT)", (ftnlen)24);
	chkout_("ET2UTC", (ftnlen)6);
	return 0;
    }

/*     Force PREC into an acceptable range */

/* Computing MAX */
    i__1 = 0, i__2 = min(14,*prec);
    myprec = max(i__1,i__2);

/*     If the output is Julian Date, we're ready to go. Remember that */
/*     the day part of Julian Date already has seven digits built in. */

    if (s_cmp(fmt, "J", (ftnlen)4, (ftnlen)1) == 0) {
	tvec[0] = *et;
	ttrans_("TDB", "JDUTC", tvec, (ftnlen)3, (ftnlen)5);
	if (failed_()) {
	    chkout_("ET2UTC", (ftnlen)6);
	    return 0;
	}
	i__1 = myprec + 7;
	dpstrf_(tvec, &i__1, "F", str, (ftnlen)1, (ftnlen)80);
	prefix_("JD", &c__0, str, (ftnlen)2, (ftnlen)80);
	s_copy(utcstr, str, utcstr_len, (ftnlen)80);
	chkout_("ET2UTC", (ftnlen)6);
	return 0;
    }

/*     If we've dropped past the IF-THEN block above, we need */
/*     to construct a calendar format string. First thing to */
/*     do is convert from ET to TAI. */

    tai = unitim_(et, "TDB", "TAI", (ftnlen)3, (ftnlen)3);

/*     We're going to break up TAI into an integer and a */
/*     fractional part.  The integer will be the greatest */
/*     integer less than or equal to TAI, and the fraction */
/*     will be the difference between TAI and the integer */
/*     part.  The fraction will always be in the interval */

/*        [0, 1) */

/*     After making this decomposition, we'll adjust the integer */
/*     and fraction to take rounding into account.  The result */
/*     of the adjustment is that the fraction will be an integer */
/*     number of time units of length 10**(-MYPREC) seconds, where */
/*     the integer is in the range [0, (10**MYPREC)-1].  If the */
/*     fraction rounds up to 1, the fraction will be set to zero, */
/*     and the whole number portion of TAI will be incremented. */

/*     Since the integers involved may be too large to represent */
/*     using the INTEGER data type, we'll represent them with */
/*     double precision numbers.  We'll use the intrinsic ANINT */
/*     function to keep round-off from creeping into these d.p. */
/*     numbers representing integers. */

/*     Find the greatest integer less than or equal to TAI. */
/*     Recall that INT truncates toward the origin.  If TAI */
/*     is negative and is not already an integer, the result we */
/*     want is one less than AINT(TAI). */

    whlsec = d_int(&tai);
    if (tai < 0. && tai != whlsec) {
	whlsec += -1.;
    }

/*     The fractional part of TAI must be rounded to the */
/*     nearest multiple of 10**(-MYPREC).  Fractions that are */
/*     equidistant from two multiples of 10**(-MYPREC) are */
/*     rounded up. */

/*     To accomplish the rounding, we scale the fraction by */
/*     10**MYPREC. */


    d__1 = pow_di(&c_b22, &myprec);
    scale = d_nint(&d__1);
    d__1 = scale * (tai - whlsec);
    frcsec = d_nint(&d__1);

/*     If a carry occurred, the fraction becomes zero, and */
/*     we must increment WHLSEC. */

    if (frcsec == scale) {
	whlsec += 1.;
	frcsec = 0.;
    }
    frcsec /= scale;

/*     Now, we let TTRANS handle the transformation to */
/*     the desired components for output. */

/*     FRCSEC will be converted to a string containing MYPREC digits. */
/*     This will be done later on when the output string is */
/*     assembled. */

    tvec[0] = whlsec;
    if (s_cmp(fmt, "C", (ftnlen)4, (ftnlen)1) == 0 || s_cmp(fmt, "ISOC", (
	    ftnlen)4, (ftnlen)4) == 0) {
	ttrans_("TAI", "YMD", tvec, (ftnlen)3, (ftnlen)3);
	if (failed_()) {
	    chkout_("ET2UTC", (ftnlen)6);
	    return 0;
	}
	year = i_dnnt(tvec);
	month = i_dnnt(&tvec[1]);
	day = i_dnnt(&tvec[2]);
	hour = i_dnnt(&tvec[3]);
	minute = i_dnnt(&tvec[4]);
	second = i_dnnt(&tvec[5]);

/*        The beginning of the string is going to be the year. */
/*        Depending upon the size of the year, it may or */
/*        may not have an era label.  However the end of the */
/*        string has a fixed size.  We set up that portion of the */
/*        string now.  First fill in the month... */

	if (s_cmp(fmt, "C", (ftnlen)4, (ftnlen)1) == 0) {
	    s_copy(endstr, " MMM 00 00:00:00", (ftnlen)80, (ftnlen)16);
	    s_copy(endstr + 1, mthnam + ((i__1 = month - 1) < 12 && 0 <= i__1 
		    ? i__1 : s_rnge("mthnam", i__1, "et2utc_", (ftnlen)658)) *
		     3, (ftnlen)3, (ftnlen)3);

/*           ... and then fill in the day portion of the string. */

	    eday = 7;
/* Computing MIN */
	    i__1 = 1, i__2 = day / 1000;
/* Computing MIN */
	    i__3 = 1, i__4 = day / 100;
/* Computing MIN */
	    i__5 = 1, i__6 = day / 10;
	    bday = eday - (min(i__1,i__2) + min(i__3,i__4) + min(i__5,i__6) + 
		    1) + 1;
	    intstr_(&day, endstr + (bday - 1), eday - (bday - 1));
	    ehr = 10;
	    emn = 13;
	    esc = 16;
	} else {
	    s_copy(endstr, "-0M-00T00:00:00", (ftnlen)80, (ftnlen)15);
	    eday = 6;
/* Computing MIN */
	    i__1 = 1, i__2 = day / 1000;
/* Computing MIN */
	    i__3 = 1, i__4 = day / 100;
/* Computing MIN */
	    i__5 = 1, i__6 = day / 10;
	    bday = eday - (min(i__1,i__2) + min(i__3,i__4) + min(i__5,i__6) + 
		    1) + 1;
	    emonth = 3;
/* Computing MIN */
	    i__1 = 1, i__2 = month / 1000;
/* Computing MIN */
	    i__3 = 1, i__4 = month / 100;
/* Computing MIN */
	    i__5 = 1, i__6 = month / 10;
	    bmonth = emonth - (min(i__1,i__2) + min(i__3,i__4) + min(i__5,
		    i__6) + 1) + 1;
	    intstr_(&month, endstr + (bmonth - 1), emonth - (bmonth - 1));
	    intstr_(&day, endstr + (bday - 1), eday - (bday - 1));
	    ehr = 9;
	    emn = 12;
	    esc = 15;
	}
    } else {

/*        We must have day of year format.  Convert TAI to that */
/*        format. */

	ttrans_("TAI", "YD", tvec, (ftnlen)3, (ftnlen)2);
	if (failed_()) {
	    chkout_("ET2UTC", (ftnlen)6);
	    return 0;
	}
	year = i_dnnt(tvec);
	month = 1;
	day = i_dnnt(&tvec[1]);
	hour = i_dnnt(&tvec[2]);
	minute = i_dnnt(&tvec[3]);
	second = i_dnnt(&tvec[4]);

/*        As in the previous case, the end of the output string will */
/*        have a fixed size.  We fill in the day portion of the string */
/*        now.  Note that we have set things up so that the hour, */
/*        minutes and seconds appear in the same location in both */
/*        day of year and calendar format of strings. */

	if (s_cmp(fmt, "D", (ftnlen)4, (ftnlen)1) == 0) {
	    s_copy(endstr, "-000 // 00:00:00", (ftnlen)80, (ftnlen)16);
	    eday = 4;
/* Computing MIN */
	    i__1 = 1, i__2 = day / 1000;
/* Computing MIN */
	    i__3 = 1, i__4 = day / 100;
/* Computing MIN */
	    i__5 = 1, i__6 = day / 10;
	    bday = eday - (min(i__1,i__2) + min(i__3,i__4) + min(i__5,i__6) + 
		    1) + 1;
	    intstr_(&day, endstr + (bday - 1), eday - (bday - 1));
	    ehr = 10;
	    emn = 13;
	    esc = 16;
	} else {
	    s_copy(endstr, "-000T00:00:00", (ftnlen)80, (ftnlen)13);
	    eday = 4;
/* Computing MIN */
	    i__1 = 1, i__2 = day / 1000;
/* Computing MIN */
	    i__3 = 1, i__4 = day / 100;
/* Computing MIN */
	    i__5 = 1, i__6 = day / 10;
	    bday = eday - (min(i__1,i__2) + min(i__3,i__4) + min(i__5,i__6) + 
		    1) + 1;
	    intstr_(&day, endstr + (bday - 1), eday - (bday - 1));
	    ehr = 7;
	    emn = 10;
	    esc = 13;
	}
    }

/*     Fill out the hours, minutes and integer portion of */
/*     seconds in the output string. */

/* Computing MIN */
    i__1 = 1, i__2 = hour / 1000;
/* Computing MIN */
    i__3 = 1, i__4 = hour / 100;
/* Computing MIN */
    i__5 = 1, i__6 = hour / 10;
    bhr = ehr - (min(i__1,i__2) + min(i__3,i__4) + min(i__5,i__6) + 1) + 1;
/* Computing MIN */
    i__1 = 1, i__2 = minute / 1000;
/* Computing MIN */
    i__3 = 1, i__4 = minute / 100;
/* Computing MIN */
    i__5 = 1, i__6 = minute / 10;
    bmn = emn - (min(i__1,i__2) + min(i__3,i__4) + min(i__5,i__6) + 1) + 1;
/* Computing MIN */
    i__1 = 1, i__2 = second / 1000;
/* Computing MIN */
    i__3 = 1, i__4 = second / 100;
/* Computing MIN */
    i__5 = 1, i__6 = second / 10;
    bsc = esc - (min(i__1,i__2) + min(i__3,i__4) + min(i__5,i__6) + 1) + 1;
    intstr_(&hour, endstr + (bhr - 1), ehr - (bhr - 1));
    intstr_(&minute, endstr + (bmn - 1), emn - (bmn - 1));
    intstr_(&second, endstr + (bsc - 1), esc - (bsc - 1));

/*     Append the fractional part of the seconds component. */

    if (myprec > 0) {

/*        DPSTRF gives MYPREC significant digits in the output, */
/*        not necessarily MYPREC digits to the right of the */
/*        decimal point.  We will add a one-digit integer to */
/*        FRCSEC to `anchor' the first significant digit of */
/*        FRCSEC in a known place.  That way, we can get DPSTRF */
/*        to give us a known number of digits after the decimal */
/*        point. */

/*        The integer part of FRCSEC will not affect the output */
/*        string. */

	frcsec += 1.;
	i__1 = myprec + 1;
	dpstrf_(&frcsec, &i__1, "F", fract, (ftnlen)1, (ftnlen)80);
	i__ = i_indx(fract, ".", (ftnlen)80, (ftnlen)1);
	i__1 = esc;
	s_copy(endstr + i__1, fract + (i__ - 1), 80 - i__1, i__ + myprec - (
		i__ - 1));
    }

/*     The end of the time string is now complete.  We need to */
/*     construct the year portion of the string. We are going to */
/*     append an era if the year is before 1000 A.D. Note that */
/*     we make sure the first character in the ending string */
/*     is a blank (' ') if the era is to be attached.  Otherwise */
/*     we'd get confusing day of year formats like */
/*     999 A.D.-019 // 12:13:18. */

    if (year >= 1000) {
	intstr_(&year, str, (ftnlen)80);
    } else if (year > 0) {
	intstr_(&year, str, (ftnlen)80);
	if (s_cmp(fmt, "C", (ftnlen)4, (ftnlen)1) == 0 || s_cmp(fmt, "D", (
		ftnlen)4, (ftnlen)1) == 0) {
	    suffix_("A.D.", &c__1, str, (ftnlen)4, (ftnlen)80);
	    *(unsigned char *)endstr = ' ';
	}
    } else if (year <= 0) {
	if (s_cmp(fmt, "C", (ftnlen)4, (ftnlen)1) == 0 || s_cmp(fmt, "D", (
		ftnlen)4, (ftnlen)1) == 0) {
	    year = -year + 1;
	    intstr_(&year, str, (ftnlen)80);
	    suffix_("B.C.", &c__1, str, (ftnlen)4, (ftnlen)80);
	    *(unsigned char *)endstr = ' ';
	} else {
	    year = -year + 1;
	    setmsg_("The year of the ET epoch supplied is # B.C.  Years in t"
		    "his era are not supported in ISO format. ", (ftnlen)96);
	    errint_("#", &year, (ftnlen)1);
	    sigerr_("SPICE(YEAROUTOFRANGE)", (ftnlen)21);
	    chkout_("ET2UTC", (ftnlen)6);
	    return 0;
	}
    }

/*     Finally append the ENDSTR to STR to get the fully formatted */
/*     string. */

    suffix_(endstr, &c__0, str, (ftnlen)80, (ftnlen)80);
    s_copy(utcstr, str, utcstr_len, (ftnlen)80);
    chkout_("ET2UTC", (ftnlen)6);
    return 0;
} /* et2utc_ */

