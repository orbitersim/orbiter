/* etcal.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2000 = 2000;
static integer c__1 = 1;
static integer c__12 = 12;
static integer c__6 = 6;

/* $Procedure ETCAL ( Convert ET to Calendar format ) */
/* Subroutine */ int etcal_(doublereal *et, char *calstr, ftnlen calstr_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer extra[12] = { 0,0,1,1,1,1,1,1,1,1,1,1 };
    static integer dpjan0[12] = { 0,31,59,90,120,151,181,212,243,273,304,334 }
	    ;
    static integer dpbegl[12] = { 0,31,60,91,121,152,182,213,244,274,305,335 }
	    ;
    static char months[3*12] = "JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" 
	    "AUG" "SEP" "OCT" "NOV" "DEC";

    /* System generated locals */
    address a__1[12];
    integer i__1, i__2, i__3[12];
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    double d_int(doublereal *);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);

    /* Local variables */
    static integer dn2000;
    static doublereal dp2000, frac;
    static char date[180];
    static doublereal remd, secs;
    static integer year, mins;
    static char dstr[16], hstr[16], mstr[16], sstr[16], ystr[16];
    static doublereal halfd, q;
    static integer tsecs, dofyr, month, hours;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    static doublereal mynum;
    static integer bh, bm, iq;
    static doublereal secspd;
    static char messge[16];
    static integer offset;
    static doublereal dmnint;
    static logical adjust;
    static integer daynum;
    extern integer intmin_(void), intmax_(void);
    extern /* Subroutine */ int dpstrf_(doublereal *, integer *, char *, char 
	    *, ftnlen, ftnlen);
    static doublereal dmxint, mydnom;
    extern /* Subroutine */ int cmprss_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern integer lstlti_(integer *, integer *, integer *);
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);
    static integer yr1, yr4;
    static char era[16];
    static integer day, rem;
    extern doublereal spd_(void);
    static integer yr100, yr400;

/* $ Abstract */

/*     Convert from an ephemeris epoch measured in seconds past */
/*     the epoch of J2000 to a calendar string format using a */
/*     formal calendar free of leapseconds. */

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
/*     ET         I   Ephemeris time measured in seconds past J2000. */
/*     CALSTR     O   A standard calendar representation of ET. */

/* $ Detailed_Input */

/*     ET       is an epoch measured in ephemeris seconds */
/*              past the epoch of J2000. */

/* $ Detailed_Output */

/*     CALSTR   is a calendar string representing the input ephemeris */
/*              epoch. This string is based upon extending the */
/*              Gregorian Calendar backward and forward indefinitely */
/*              keeping the same rules for determining leap years. */
/*              Moreover, there is no accounting for leapseconds. */

/*              To be sure that all of the date can be stored in */
/*              CALSTR, it should be declared to have length at */
/*              least 48 characters. */

/*              The string will have the following format */

/*                 year (era) mon day hr:mn:sc.sss */

/*              Where: */

/*                 year --- is the year */
/*                 era  --- is the chronological era associated with */
/*                          the date. For years after 999 A.D. */
/*                          the era is omitted. For years */
/*                          between 1 A.D. and 999 A.D. (inclusive) */
/*                          era is the string 'A.D.' For epochs */
/*                          before 1 A.D. Jan 1 00:00:00, era is */
/*                          given as 'B.C.' and the year is converted */
/*                          to years before the "Christian Era". */
/*                          The last B.C. epoch is */

/*                            1 B.C. DEC 31 23:59:59.999 */

/*                          The first A.D. epoch (which occurs .001 */
/*                          seconds after the last B.C. epoch) is: */

/*                             1 A.D. JAN 1 00:00:00.000 */

/*                          Note: there is no year 0 A.D. or 0 B.C. */
/*                 mon  --- is a 3-letter abbreviation for the month */
/*                          in all capital letters. */
/*                 day  --- is the day of the month */
/*                 hr   --- is the hour of the day (between 0 and 23) */
/*                          leading zeros are added to hr if the */
/*                          numeric value is less than 10. */
/*                 mn   --- is the minute of the hour (0 to 59) */
/*                          leading zeros are added to mn if the */
/*                          numeric value is less than 10. */
/*                 sc.sss   is the second of the minute to 3 decimal */
/*                          places ( 0 to 59.999). Leading zeros */
/*                          are added if the numeric value is less */
/*                          than 10. Seconds are truncated, not */
/*                          rounded. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the input ET is so large that the corresponding */
/*         number of days since 1 A.D. Jan 1, 00:00:00 is */
/*         within 1 of overflowing or underflowing an integer, */
/*         ET will not be converted to the correct string */
/*         representation rather, the string returned will */
/*         state that the epoch was before or after the day */
/*         that is INTMIN +1 or INTMAX - 1 days after */
/*         1 A.D. Jan 1, 00:00:00. */

/*     2)  If the output string is not sufficiently long to hold */
/*         the full date, it will be truncated on the right. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is an error free routine for converting ephemeris epochs */
/*     represented as seconds past the J2000 epoch to formal */
/*     calendar strings based upon the Gregorian Calendar. This formal */
/*     time is often useful when one needs a human recognizable */
/*     form of an ephemeris epoch. There is no accounting for leap */
/*     seconds in the output times produced. */

/*     Note: The calendar epochs produced are not the same as the */
/*           UTC calendar epochs that correspond to ET. The strings */
/*           produced by this routine may vary from the corresponding */
/*           UTC epochs by more than 1 minute. */

/*     This routine can be used in creating error messages or */
/*     in routines and programs in which one prefers to report */
/*     times without employing leapseconds to produce exact UTC */
/*     epochs. */

/* $ Examples */

/*     Suppose you wish to  report that no data is */
/*     available at a particular ephemeris epoch ET. The following */
/*     code shows how you might accomplish this task: */

/*           CALL DPSTRF ( ET,  6, 'F', ETSTR  ) */
/*           CALL ETCAL  ( ET,          CALSTR ) */

/*           E1 = RTRIM   (             CALSTR ) */
/*           E2 = RTRIM   (             ETSTR  ) */

/*           WRITE (*,*) 'There is no data available for the body ' */
/*           WRITE (*,*) 'at requested time: ' */
/*           WRITE (*,*) '   ', CALSTR(1:E1), ' (', ETSTR(1:E2), ')' */

/* $ Restrictions */

/*     1)  One must keep in mind when using this routine that */
/*         ancient times are not based upon the Gregorian */
/*         calendar. For example the 0 point of the Julian */
/*         Date system is 4713 B.C. Jan 1, 12:00:00 on the Julian */
/*         Calendar. If one formalized the Gregorian calendar */
/*         and extended it indefinitely, the zero point of the Julian */
/*         date system corresponds to 4714 B.C. NOV 24 12:00:00 on */
/*         the Gregorian calendar. There are several reasons for this. */
/*         Leap years in the Julian calendar occur every */
/*         4 years (including *all* centuries). Moreover,  the */
/*         Gregorian calendar "effectively" begins on 15 Oct, 1582 A.D. */
/*         which is 5 Oct, 1582 A.D. in the Julian Calendar. */

/*         Therefore you must be careful in your interpretation */
/*         of ancient dates produced by this routine. */

/* $ Literature_References */

/*     [1]  J. Jespersen and J. Fitz-Randolph, "From Sundials to Atomic */
/*          Clocks, Understanding Time and Frequency," Dover */
/*          Publications, Inc. New York, 1982. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.3.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. Changed output argument name */
/*        STRING to CALSTR for consistency with other routines. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. Added TIME required reading. */

/* -    SPICELIB Version 2.2.0, 05-MAR-1998 (WLT) */

/*        The documentation concerning the appearance of the output */
/*        time string was corrected so that it does not suggest */
/*        a comma is inserted after the day of the month. The */
/*        comma was removed from the output string in Version 2.0.0 */
/*        (see the note below) but the documentation was not upgraded */
/*        accordingly. */

/* -    SPICELIB Version 2.1.0, 20-MAY-1996 (WLT) */

/*        Two arrays that were initialized but never used were */
/*        removed. */

/* -    SPICELIB Version 2.0.0, 16-AUG-1995 (KRG) */

/*        If the day number was less than 10, the spacing was off for */
/*        the rest of the time by one space, that for the "tens" digit. */
/*        This has been fixed by using a leading zero when the number of */
/*        days is < 10. */

/*        Also, the comma that appeared between the month/day/year */
/*        and the hour:minute:seconds tokens has been removed. This was */
/*        done in order to make the calendar date format of ETCAL */
/*        consistent with the calendar date format of ET2UTC. */


/* -    SPICELIB Version 1.0.0, 14-DEC-1993 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Convert ephemeris time to a formal calendar date */

/* -& */

/*     SPICELIB Functions. */


/*     We declare the variables that contain the number of days in */
/*     400 years, 100 years, 4 years and 1 year. */


/*     The following integers give the number of days during the */
/*     associated month of a non-leap year. */


/*     The integers that follow give the number of days in a normal */
/*     year that precede the first of the month. */


/*     The integers that follow give the number of days in a leap */
/*     year that precede the first of the month. */


/*     The variables below hold the components of the output string */
/*     before they are put together. */


/*     We will construct our string using the local variable DATE */
/*     and transfer the results to the output CALSTR when we are */
/*     done. */


/*     MONTHS contains 3-letter abbreviations for the months of the year */


/*     The array EXTRA contains the number of additional days that */
/*     appear before the first of a month during a leap year (as opposed */
/*     to a non-leap year). */


/*     DPJAN0(I) gives the number of days that occur before the I'th */
/*     month of a normal year. */


/*     Definitions of statement functions. */


/*     The number of days elapsed since Jan 1, of year 1 A.D. to */
/*     Jan 1 of YEAR is given by: */


/*     The number of leap days in a year is given by: */


/*     To compute the day of the year we */

/*        look up the number of days to the beginning of the month, */

/*        add on the number leap days that occurred prior to that */
/*        time */

/*        add on the number of days into the month */


/*     The number of days since 1 Jan 1 A.D. is given by: */

    if (first) {
	first = FALSE_;
	halfd = spd_() / 2.;
	secspd = spd_();
	dn2000 = (c__2000 - 1) * 365 + (c__2000 - 1) / 4 - (c__2000 - 1) / 
		100 + (c__2000 - 1) / 400 + (dpjan0[(i__1 = c__1 - 1) < 12 && 
		0 <= i__1 ? i__1 : s_rnge("dpjan0", i__1, "etcal_", (ftnlen)
		555)] + extra[(i__2 = c__1 - 1) < 12 && 0 <= i__2 ? i__2 : 
		s_rnge("extra", i__2, "etcal_", (ftnlen)555)] * ((c__2000 / 4 
		<< 2) / c__2000 - c__2000 / 100 * 100 / c__2000 + c__2000 / 
		400 * 400 / c__2000) + c__1) - 1;
	dmxint = (doublereal) intmax_();
	dmnint = (doublereal) intmin_();
    }

/*     Now we "in-line" compute the following call. */

/*        call rmaind ( et + halfd, secspd, dp2000, secs ) */

/*     because we can't make a call to rmaind. */

/*     The reader may wonder why we use et + halfd.  The value */
/*     et is seconds past the ephemeris epoch of J2000 which */
/*     is at 2000 Jan 1, 12:00:00.  We want to compute days past */
/*     2000 Jan 1, 00:00:00.  The seconds past THAT epoch is et + halfd. */
/*     We add on 0.0005 seconds so that the string produced will be */
/*     rounded to the nearest millisecond. */

    mydnom = secspd;
    mynum = *et + halfd;
    d__1 = mynum / mydnom;
    q = d_int(&d__1);
    remd = mynum - q * mydnom;
    if (remd < 0.) {
	q += -1.;
	remd += mydnom;
    }
    secs = remd;
    dp2000 = q;

/*     Do something about the problem when ET is vastly */
/*     out of range.  (Day number outside MAX and MIN integer). */

    if (dp2000 + dn2000 < dmnint + 1) {
	dp2000 = dmnint - dn2000 + 1;
	s_copy(messge, "Epoch before ", (ftnlen)16, (ftnlen)13);
	secs = 0.;
    } else if (dp2000 + dn2000 > dmxint - 1) {
	dp2000 = dmxint - dn2000 - 1;
	s_copy(messge, "Epoch after ", (ftnlen)16, (ftnlen)12);
	secs = 0.;
    } else {
	s_copy(messge, " ", (ftnlen)16, (ftnlen)1);
    }

/*     Compute the number of days since 1 .A.D. Jan 1, 00:00:00. */
/*     From the tests in the previous IF-ELSE IF-ELSE block this */
/*     addition is guaranteed not to overflow. */

    daynum = (integer) (dp2000 + (doublereal) dn2000);

/*     If the number of days is negative, we need to do a little */
/*     work so that we can represent the date in the B.C. era. */
/*     We add enough multiples of 400 years so that the year will */
/*     be positive and then we subtract off the appropriate multiple */
/*     of 400 years later. */

    if (daynum < 0) {

/*        Since we can't make the call below and remain */
/*        error free, we compute it ourselves. */

/*        call rmaini ( daynum, dp400y, offset, daynum ) */

	iq = daynum / 146097;
	rem = daynum - iq * 146097;
	if (rem < 0) {
	    --iq;
	    rem += 146097;
	}
	offset = iq;
	daynum = rem;
	adjust = TRUE_;
    } else {
	adjust = FALSE_;
    }

/*     Next we compute the year.  Divide out multiples of 400, 100 */
/*     4 and 1 year.  Finally combine these to get the correct */
/*     value for year.  (Note this is all integer arithmetic.) */

/*     Recall that DP1Y   =    365 */
/*                 DP4Y   =  4*DPY    + 1 */
/*                 DP100Y = 25*DP4Y   - 1 */
/*                 DP400Y =  4*DP100Y + 1 */

    yr400 = daynum / 146097;
    rem = daynum - yr400 * 146097;
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

/*     Get the month, and day of month (depending upon whether */
/*     we have a leap year or not). */

    if ((year / 4 << 2) / year - year / 100 * 100 / year + year / 400 * 400 / 
	    year == 0) {
	month = lstlti_(&dofyr, &c__12, dpjan0);
	day = dofyr - dpjan0[(i__1 = month - 1) < 12 && 0 <= i__1 ? i__1 : 
		s_rnge("dpjan0", i__1, "etcal_", (ftnlen)682)];
    } else {
	month = lstlti_(&dofyr, &c__12, dpbegl);
	day = dofyr - dpbegl[(i__1 = month - 1) < 12 && 0 <= i__1 ? i__1 : 
		s_rnge("dpbegl", i__1, "etcal_", (ftnlen)685)];
    }

/*     If we had to adjust the year to make it positive, we now */
/*     need to correct it and then convert it to a B.C. year. */

    if (adjust) {
	year += offset * 400;
	year = -year + 1;
	s_copy(era, " B.C. ", (ftnlen)16, (ftnlen)6);
    } else {

/*        If the year is less than 1000, we can't just write it */
/*        out.  We need to add the era.  If we don't do this */
/*        the dates look very confusing. */

	if (year < 1000) {
	    s_copy(era, " A.D. ", (ftnlen)16, (ftnlen)6);
	} else {
	    s_copy(era, " ", (ftnlen)16, (ftnlen)1);
	}
    }

/*     Convert Seconds to Hours, Minute and Seconds. */
/*     We work with thousandths of a second in integer arithmetic */
/*     so that all of the truncation work with seconds will already */
/*     be done.  (Note that we already know that SECS is greater than */
/*     or equal to zero so we'll have no problems with HOURS, MINS */
/*     or SECS becoming negative.) */

    tsecs = (integer) (secs * 1e3);
    frac = secs - (doublereal) tsecs;
    hours = tsecs / 3600000;
    tsecs -= hours * 3600000;
    mins = tsecs / 60000;
    tsecs -= mins * 60000;
    secs = (doublereal) tsecs / 1e3;

/*     We round seconds if we can do so without getting seconds to be */
/*     bigger than 60. */

    if (secs + 5e-4 < 60.) {
	secs += 5e-4;
    }

/*     Finally, get the components of our date string. */

    intstr_(&year, ystr, (ftnlen)16);
    if (day >= 10) {
	intstr_(&day, dstr, (ftnlen)16);
    } else {
	s_copy(dstr, "0", (ftnlen)16, (ftnlen)1);
	intstr_(&day, dstr + 1, (ftnlen)15);
    }

/*     We want to zero pad the hours minutes and seconds. */

    if (hours < 10) {
	bh = 2;
    } else {
	bh = 1;
    }
    if (mins < 10) {
	bm = 2;
    } else {
	bm = 1;
    }
    s_copy(mstr, "00", (ftnlen)16, (ftnlen)2);
    s_copy(hstr, "00", (ftnlen)16, (ftnlen)2);
    s_copy(sstr, " ", (ftnlen)16, (ftnlen)1);

/*     Now construct the string components for hours, minutes and */
/*     seconds. */

    secs = (integer) (secs * 1e3) / 1e3;
    intstr_(&hours, hstr + (bh - 1), 16 - (bh - 1));
    intstr_(&mins, mstr + (bm - 1), 16 - (bm - 1));
    dpstrf_(&secs, &c__6, "F", sstr, (ftnlen)1, (ftnlen)16);

/*     The form of the output for SSTR has a leading blank followed by */
/*     the first significant digit.  If a decimal point is in the */
/*     third slot, then SSTR is of the form ' x.xxxxx'  and we need */
/*     to insert a leading zero. */

    if (*(unsigned char *)&sstr[2] == '.') {
	*(unsigned char *)sstr = '0';
    }

/*     We don't want any leading spaces in SSTR, (HSTR and MSTR don't */
/*     have leading spaces by construction. */

    ljust_(sstr, sstr, (ftnlen)16, (ftnlen)16);

/*     Now form the date string, squeeze out extra spaces and */
/*     left justify the whole thing. */

/* Writing concatenation */
    i__3[0] = 16, a__1[0] = messge;
    i__3[1] = 16, a__1[1] = ystr;
    i__3[2] = 16, a__1[2] = era;
    i__3[3] = 3, a__1[3] = months + ((i__1 = month - 1) < 12 && 0 <= i__1 ? 
	    i__1 : s_rnge("months", i__1, "etcal_", (ftnlen)794)) * 3;
    i__3[4] = 1, a__1[4] = " ";
    i__3[5] = 3, a__1[5] = dstr;
    i__3[6] = 1, a__1[6] = " ";
    i__3[7] = 2, a__1[7] = hstr;
    i__3[8] = 1, a__1[8] = ":";
    i__3[9] = 2, a__1[9] = mstr;
    i__3[10] = 1, a__1[10] = ":";
    i__3[11] = 6, a__1[11] = sstr;
    s_cat(date, a__1, i__3, &c__12, (ftnlen)180);
    cmprss_(" ", &c__1, date, date, (ftnlen)1, (ftnlen)180, (ftnlen)180);
    ljust_(date, date, (ftnlen)180, (ftnlen)180);
    s_copy(calstr, date, calstr_len, (ftnlen)180);
    return 0;
} /* etcal_ */

