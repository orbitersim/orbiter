/* jul2gr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1582 = 1582;
static integer c__10 = 10;
static integer c__15 = 15;
static integer c__4 = 4;
static integer c__100 = 100;
static integer c__400 = 400;
static integer c__5 = 5;
static integer c__12 = 12;
static integer c_b27 = 146097;
static integer c__1461 = 1461;

/* $Procedure JUL2GR (Julian to Gregorian Calendar) */
/* Subroutine */ int jul2gr_0_(int n__, integer *year, integer *month, 
	integer *day, integer *doy)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer extra[12] = { 0,0,1,1,1,1,1,1,1,1,1,1 };
    static integer dpjan0[12] = { 0,31,59,90,120,151,181,212,243,273,304,334 }
	    ;
    static integer dpbegl[12] = { 0,31,60,91,121,152,182,213,244,274,305,335 }
	    ;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer dayg, dayj, m, rdayg, rdayj, dofyr, yroff, m4, tmpyr, dy, 
	    yr;
    extern /* Subroutine */ int rmaini_(integer *, integer *, integer *, 
	    integer *);
    static integer offset, offstg, offstj, m100, tmpday, m400;
    extern integer lstlti_(integer *, integer *, integer *);
    static integer mon;

/* $ Abstract */

/*     Convert Year Month and Day on the Julian Calendar */
/*     to the Gregorian Calendar */

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

/*     None. */

/* $ Keywords */

/*     TIME */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     YEAR      I-O  Year  of Julian Calendar/Gregorian Calendar */
/*     MONTH     I-O  Month of Julian Calendar/Gregorian Calendar */
/*     DAY       I-O  Day of Month in Julian Calendar/Gregorian Calendar */
/*     DOY        O   Day of Year in Gregorian Calendar */

/* $ Detailed_Input */

/*     YEAR     is an integer representing the year of an epoch, E, in */
/*              the Julian proleptic calendar. Note that the year 0 */
/*              and negative values are required to represent */
/*              years in the pre-Christian era (B.C.) A year, Y B.C., */
/*              should be represented as -(Y-1).  For example the year */
/*              435 B.C. should be input as -434. */

/*     MONTH    is an integer representing the month of some epoch, E, */
/*              in the Julian proleptic calendar. Months */
/*              outside the usual range from 1 to 12 are converted */
/*              to the standard range using modular arithmetic and */
/*              the input year is adjusted appropriately. */


/*     DAY      is the day of the month of some epoch, E, in the Julian */
/*              proleptic calendar. */

/*              Note to input an epoch as the day of a year, set MONTH */
/*              to 1 and DAY to the day of the year. */

/* $ Detailed_Output */

/*     YEAR     is an integer representing the year of the epoch, E, */
/*              above in the Gregorian calendar. Note that the year */
/*              0 (zero) and negative values are used to represent */
/*              years in the pre-Christian era (B.C.) A year, Y B.C., */
/*              is be represented as -(Y-1).  For example the year */
/*              435 B.C. will be returned as -434. */

/*     MONTH    is an integer representing the month of the epoch, E, */
/*              above in the Gregorian Calendar calendar. */

/*     DAY      is the day of the month of the epoch, E, above in the */
/*              Gregorian Calendar */

/*     DOY      is the day of the year of the epoch, E, above in the */
/*              Gregorian Calendar. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a mid-level utility routine to assist in the assignment */
/*     and presentation of ancient epochs. */

/*     The SPICE software deals primarily with epochs represented on */
/*     in the Gregorian Calendar. However, the Gregorian calendar */
/*     was not adopted until October 15, 1582. As a result, epochs */
/*     prior to that time are usually represented in the Julian */
/*     proleptic calendar. */

/*     Formally, both calendars can be extended indefinitely forward */
/*     and backward in time due the algorithmic nature of the */
/*     determination of calendar representation. */

/*     When converting "parsed" calendar epochs in the SPICE system, */
/*     you need to first convert to the Gregorian Calendar. From that */
/*     point the SPICE toolkit can easily convert the epoch to Julian */
/*     date or seconds past the J2000 epoch. */

/*     This routine allows you to take a numeric representation of */
/*     an epoch represented in the Julian proleptic calendar and */
/*     convert that to an epoch in the Gregorian calendar. */

/*     To convert from Gregorian Calendar to Julian proleptic */
/*     calendar, use the entry point GR2JUL. */

/* $ Examples */

/*     Suppose you need to find the epoch (in seconds past the */
/*     J2000) of some ancient epoch that occurred at */
/*     3:00 on March 4 of the year 121 B.C. And that this epoch */
/*     is based on the Julian proleptic calendar. We first need */
/*     to convert the Julian Calendar date to the Gregorian Calendar. */

/*     Here's the declarations we'll need */

/*        INTEGER               YEAR */
/*        INTEGER               MONTH */
/*        INTEGER               DAY */
/*        INTEGER               DOY */

/*        DOUBLE PRECISION      TVEC ( 6 ) */
/*        DOUBLE PRECISION      TDB */

/*     You first need to convert the calendar date of this epoch */
/*     integers. (We don't need to worry about the hours for a moment). */

/*        YEAR  = -120 */
/*        MONTH =  3 */
/*        DAY   =  4 */

/*     Convert this Year, Month and Day to the Gregorian Calendar. */

/*        CALL JUL2GR ( YEAR, MONTH, DAY, DOY ) */

/*     Now construct a time vector for use in the routine TTRANS. */
/*     Note now we use the hour component of the epoch (the fourth */
/*     component of the time vector TVEC). */

/*        TVEC(1) = DBLE( YEAR ) */
/*        TVEC(2) = DBLE( MONTH ) */
/*        TVEC(3) = DBLE( DAY ) */
/*        TVEC(4) = 3.0D0 */
/*        TVEC(5) = 0.0D0 */
/*        TVEC(6) = 0.0D0 */

/*     Now the routine TTRANS can convert the time vector from */
/*     the input YMD format to barycentric dynamical time. */

/*        CALL TTRANS ( 'YDM', 'TDB', TVEC ) */

/*        TDB = TVEC(1) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 02-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.2.0, 26-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in RMAINI calls. */

/* -    SPICELIB Version 1.1.1, 23-SEP-1999 (WLT) */

/*        Removed the unused variable DPMON. */

/* -    SPICELIB Version 1.1.0, 23-FEB-1998 (WLT) */

/*        The routine was upgraded so that it will handle without */
/*        error months that are outside the range from 1 to 12. */

/* -    SPICELIB Version 1.0.0, 13-MAR-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Convert from Julian proleptic to Gregorian Calendar */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 23-FEB-1998 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in RMAINI calls. */

/* -& */

/*     Spicelib Functions */


/*     Local (in-line) Functions */


/*     Local parameters */

/*     We declare the variables that contain the number of days in */
/*     400 years (Gregorian), 100 years (Gregorian), 4 years and 1 year. */


/*     The following integers give the number of days during the */
/*     associated month of a non-leap year. */


/*     The integers that follow give the number of days in a normal */
/*     year that precede the first of the month. */


/*     The integers that follow give the number of days in a leap */
/*     year that precede the first of the month. */


/*     Local variables */


/*     The array EXTRA contains the number of additional days that */
/*     appear before the first of a month during a leap year (as opposed */
/*     to a non-leap year). */


/*     DPJAN0(I) gives the number of days that occur before the I'th */
/*     month of a normal year. */


/*     Saved variables */


/*     Initial values */

    switch(n__) {
	case 1: goto L_gr2jul;
	}


/*     Definitions of statement functions. */

/*     The number of days elapsed since Gregorian Jan 1, of year 1 A.D. */
/*     to Jan 1 of YR is given by: */


/*     The number of days elapsed since Julian Jan 1, of year 1 A.D. */
/*     to Jan 1 of YR is given by: */


/*     Return 1 if YR is divisible by M, otherwise return 0. */


/*     The number of leap days in a Gregorian year is given by: */


/*     The number of leap days in a Julian year is given by: */


/*     To compute the day of the year we */

/*        look up the number of days to the beginning of the month, */

/*        add on the number leap days that occurred prior to that */
/*        time */

/*        add on the number of days into the month */


/*     The number of days since 1 Jan 1 A.D. (Gregorian) is given by: */


/*     The number of days since 1 Jan 1 A.D. (Julianis given by: */


/*     If this is the first pass through this entry point (or the */
/*     companion entry point) we need to set up some reference points. */

/*     RDAYG   is the number of days past 1 A.D. Jan 1 of the Gregorian */
/*             calendar of the date Oct 15, 1582 */

/*     RDAYJ   is the number of days past 1 A.D. Jan 1 of the Julian */
/*             calendar of the date Oct 5, 1582. */

/*     OFFSTJ and OFFSTG are just the offset from one count of days */
/*     to the other. */

    if (first) {
	first = FALSE_;
/* Computing MAX */
	i__3 = 0, i__4 = abs(c__1582) / c__4 * c__4 + 1 - abs(c__1582);
/* Computing MAX */
	i__5 = 0, i__6 = abs(c__1582) / c__100 * c__100 + 1 - abs(c__1582);
/* Computing MAX */
	i__7 = 0, i__8 = abs(c__1582) / c__400 * c__400 + 1 - abs(c__1582);
	rdayg = (c__1582 - 1) * 365 + (c__1582 - 1) / 4 - (c__1582 - 1) / 100 
		+ (c__1582 - 1) / 400 + (dpjan0[(i__1 = c__10 - 1) < 12 && 0 
		<= i__1 ? i__1 : s_rnge("dpjan0", i__1, "jul2gr_", (ftnlen)
		544)] + extra[(i__2 = c__10 - 1) < 12 && 0 <= i__2 ? i__2 : 
		s_rnge("extra", i__2, "jul2gr_", (ftnlen)544)] * (max(i__3,
		i__4) - max(i__5,i__6) + max(i__7,i__8)) + c__15) - 1;
/* Computing MAX */
	i__3 = 0, i__4 = abs(c__1582) / c__4 * c__4 + 1 - abs(c__1582);
	rdayj = (c__1582 - 1) * 365 + (c__1582 - 1) / 4 + (dpjan0[(i__1 = 
		c__10 - 1) < 12 && 0 <= i__1 ? i__1 : s_rnge("dpjan0", i__1, 
		"jul2gr_", (ftnlen)545)] + extra[(i__2 = c__10 - 1) < 12 && 0 
		<= i__2 ? i__2 : s_rnge("extra", i__2, "jul2gr_", (ftnlen)545)
		] * max(i__3,i__4) + c__5) - 1;
	offstj = rdayj - rdayg;
	offstg = rdayg - rdayj;
    }

/*     Make local copies of the year, month and day.  Then get the */
/*     YEARs into a positive range. */

    i__1 = *month - 1;
    rmaini_(&i__1, &c__12, &yroff, &mon);
    yr = *year + yroff;
    ++mon;
    dy = *day;
    if (yr <= 0) {
	rmaini_(&yr, &c__4, &m4, &tmpyr);
	yr = tmpyr;
	if (yr == 0) {
	    yr += 4;
	    --m4;
	}
	offset = m4 * 1461;
    } else {
	offset = 0;
    }

/*     First get the day number (Julian) for the input */
/*     year month and day. */

/* Computing MAX */
    i__3 = 0, i__4 = abs(yr) / c__4 * c__4 + 1 - abs(yr);
    dayj = (yr - 1) * 365 + (yr - 1) / 4 + (dpjan0[(i__1 = mon - 1) < 12 && 0 
	    <= i__1 ? i__1 : s_rnge("dpjan0", i__1, "jul2gr_", (ftnlen)583)] 
	    + extra[(i__2 = mon - 1) < 12 && 0 <= i__2 ? i__2 : s_rnge("extra"
	    , i__2, "jul2gr_", (ftnlen)583)] * max(i__3,i__4) + dy) - 1 + 
	    offset;

/*     This day is DAYJ - RDAYJ days after 1582 Oct 5 on the */
/*     julian calendar.  But this is the same as the number */
/*     of days past 1582 Oct 15 on the Gregorian Calendar */
/*     So the Gregorian day number is DAYJ - RDAYJ + RDAYG */
/*     which is the same as DAYJ + OFFSTG. */

    dayg = dayj + offstg;

/*     Now that we have the Gregorian day number it's a fairly */
/*     straight forward task to get the year, month and day */
/*     on the Gregorian calendar. */

    rmaini_(&dayg, &c_b27, &m400, &tmpday);
    dayg = tmpday;
/* Computing MIN */
    i__1 = 3, i__2 = dayg / 36524;
    m100 = min(i__1,i__2);
    dayg -= m100 * 36524;
/* Computing MIN */
    i__1 = 24, i__2 = dayg / 1461;
    m4 = min(i__1,i__2);
    dayg -= m4 * 1461;
/* Computing MIN */
    i__1 = 3, i__2 = dayg / 365;
    m = min(i__1,i__2);
    dayg -= m * 365;
    dofyr = dayg + 1;
    yr = m400 * 400 + m100 * 100 + (m4 << 2) + m + 1;

/*     Now look up the month number and compute the day of the month. */
/*     How we do this depends on whether or not this is a leap year. */

/* Computing MAX */
    i__1 = 0, i__2 = abs(yr) / c__4 * c__4 + 1 - abs(yr);
/* Computing MAX */
    i__3 = 0, i__4 = abs(yr) / c__100 * c__100 + 1 - abs(yr);
/* Computing MAX */
    i__5 = 0, i__6 = abs(yr) / c__400 * c__400 + 1 - abs(yr);
    if (max(i__1,i__2) - max(i__3,i__4) + max(i__5,i__6) == 0) {
	mon = lstlti_(&dofyr, &c__12, dpjan0);
	dy = dofyr - dpjan0[(i__1 = mon - 1) < 12 && 0 <= i__1 ? i__1 : 
		s_rnge("dpjan0", i__1, "jul2gr_", (ftnlen)625)];
    } else {
	mon = lstlti_(&dofyr, &c__12, dpbegl);
	dy = dofyr - dpbegl[(i__1 = mon - 1) < 12 && 0 <= i__1 ? i__1 : 
		s_rnge("dpbegl", i__1, "jul2gr_", (ftnlen)628)];
    }
    *year = yr;
    *month = mon;
    *day = dy;
    *doy = dofyr;
    return 0;
/* $Procedure GR2JUL (Gregorian to Julian Calendar) */

L_gr2jul:
/* $ Abstract */

/*     Convert Year Month and Day on the  Gregorian Calendar */
/*     to the Julian Calendar */

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

/*     None. */

/* $ Keywords */

/*     TIME */

/* $ Declarations */

/*     INTEGER               YEAR */
/*     INTEGER               MONTH */
/*     INTEGER               DAY */
/*     INTEGER               DOY */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     YEAR      I-O  Year  of Gregorian Calendar/Julian Calendar */
/*     MONTH     I-O  Month of Gregorian Calendar/Julian Calendar */
/*     DAY       I-O  Day of Month in Gregorian Calendar/Julian Calendar */
/*     DOY        O   Day of Year in Julian Calendar */

/* $ Detailed_Input */

/*     YEAR     is an integer representing the year of an epoch, E, in */
/*              the Gregorian calendar. Note that the year 0 (zero) */
/*              and negative values are required to represent */
/*              years in the pre-Christian era (B.C.) A year, Y B.C. */
/*              should be represented as -(Y-1).  For example the year */
/*              435 B.C. should be input as -434. */

/*     MONTH    is an integer representing the month of some epoch, E, */
/*              in the Gregorian calendar. Months */
/*              outside the usual range from 1 to 12 are converted */
/*              to the standard range using modular arithmetic and */
/*              the input year is adjusted appropriately. */

/*     DAY      is the day of the month of some epoch, E, in the */
/*              Gregorian calendar. */

/*              Note to input an epoch as the day of a year, set MONTH */
/*              to 1 and DAY to the day of the year. */

/* $ Detailed_Output */

/*     YEAR     is an integer representing the year of the epoch, E, */
/*              above in the Julian calendar. Note that the year 0 */
/*              (zero) and negative values are used to represent */
/*              years in the pre-Christian era (B.C.) A year, Y B.C., */
/*              is be represented as -(Y-1).  For example the year */
/*              435 B.C. will be returned as -434. */

/*     MONTH    is an integer representing the month of the epoch, E, */
/*              above in the Julian Calendar calendar. */

/*     DAY      is the day of the month of the epoch, E, above in the */
/*              Julian Calendar */

/*     DOY      is the day of the year of the epoch, E, above in the */
/*              Julian Calendar. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a mid-level utility routine to assist in the assignment */
/*     and presentation of Ancient epochs. */

/*     The SPICE software deals primarily with epochs represented on */
/*     in the Gregorian Calendar. However, the Gregorian calendar */
/*     was not adopted until October 15, 1582. As a result, epochs */
/*     prior to that time are usually represented in the Julian */
/*     proleptic calendar. */

/*     Formally, both calendars can be extended indefinitely forward */
/*     and backward in time due the algorithmic nature of the */
/*     determination of calendar representation. */

/*     This routine allows you to take a numeric representation of */
/*     an epoch represented in the Gregorian calendar and */
/*     convert that to an epoch in the Julian calendar. */

/*     To convert from Julian Calendar to Gregorian */
/*     calendar, use the entry point JUL2GR. */

/* $ Examples */

/*     Suppose you need to print an epoch (given in seconds past the */
/*     J2000 epoch) of some ancient epoch that occurred during */
/*     pre-Christian era, and that you want to represent this epoch */
/*     using the Julian proleptic calendar. */

/*     Here's the declarations we'll need */

/*        INTEGER               YEAR */
/*        INTEGER               MONTH */
/*        INTEGER               DAY */
/*        INTEGER               DOY */

/*        DOUBLE PRECISION      TVEC ( 6 ) */
/*        DOUBLE PRECISION      TDB */

/*     You first need to convert TDB (the epoch in Seconds past J2000) */
/*     to a calendar representation. */

/*        TVEC(1) = TDB. */

/*        CALL TTRANS ( 'TDB', 'YMD', TVEC ) */

/*     The output time vector will be relative to the Gregorian */
/*     Calendar. Collect the year, month and day from the time */
/*     vector. */

/*        YEAR    = INT ( TVEC(1) ) */
/*        MONTH   = INT ( TVEC(2) ) */
/*        DAY     = INT ( TVEC(3) ) */

/*     The hours, minutes and seconds appear in components 4 through 6 */
/*     of the time vector. We can ignore them in the conversion */
/*     of the calendar from Gregorian to Julian. */

/*        CALL GR2JUL ( YEAR, MONTH, DAY, DOY ) */

/*     Now create a string from the YEAR, MONTH, DAY and TVEC(4) */
/*     through TVEC(6). */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 02-OCT-2021 (JDR) */

/*        Reordered header sections. Edited the header to comply with */
/*        NAIF standard. */

/* -    SPICELIB Version 1.1.0, 23-FEB-1998 (WLT) */

/*        The routine was upgraded so that it will handle without */
/*        error months that are outside the range from 1 to 12. */

/* -    SPICELIB Version 1.0.0, 13-MAR-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Convert from Gregorian to Julian Calendar */

/* -& */

/*     If this is the first pass through this entry point (or the */
/*     companion entry point) we need to set up some reference points. */

/*     RDAYG   is the number of days past 1 A.D. Jan 1 of the Gregorian */
/*             calendar of the date Oct 15, 1582 */

/*     RDAYJ   is the number of days past 1 A.D. Jan 1 of the Julian */
/*             calendar of the date Oct 5, 1582. */

/*     OFFSTJ and OFFSTG are just the offset from one count of days */
/*     to the other. */

    if (first) {
	first = FALSE_;
/* Computing MAX */
	i__3 = 0, i__4 = abs(c__1582) / c__4 * c__4 + 1 - abs(c__1582);
/* Computing MAX */
	i__5 = 0, i__6 = abs(c__1582) / c__100 * c__100 + 1 - abs(c__1582);
/* Computing MAX */
	i__7 = 0, i__8 = abs(c__1582) / c__400 * c__400 + 1 - abs(c__1582);
	rdayg = (c__1582 - 1) * 365 + (c__1582 - 1) / 4 - (c__1582 - 1) / 100 
		+ (c__1582 - 1) / 400 + (dpjan0[(i__1 = c__10 - 1) < 12 && 0 
		<= i__1 ? i__1 : s_rnge("dpjan0", i__1, "jul2gr_", (ftnlen)
		865)] + extra[(i__2 = c__10 - 1) < 12 && 0 <= i__2 ? i__2 : 
		s_rnge("extra", i__2, "jul2gr_", (ftnlen)865)] * (max(i__3,
		i__4) - max(i__5,i__6) + max(i__7,i__8)) + c__15) - 1;
/* Computing MAX */
	i__3 = 0, i__4 = abs(c__1582) / c__4 * c__4 + 1 - abs(c__1582);
	rdayj = (c__1582 - 1) * 365 + (c__1582 - 1) / 4 + (dpjan0[(i__1 = 
		c__10 - 1) < 12 && 0 <= i__1 ? i__1 : s_rnge("dpjan0", i__1, 
		"jul2gr_", (ftnlen)866)] + extra[(i__2 = c__10 - 1) < 12 && 0 
		<= i__2 ? i__2 : s_rnge("extra", i__2, "jul2gr_", (ftnlen)866)
		] * max(i__3,i__4) + c__5) - 1;
	offstj = rdayj - rdayg;
	offstg = rdayg - rdayj;
    }

/*     Make Local Copies of YEAR, MONTH and DAY and get YEAR into */
/*     a positive range. */

    i__1 = *month - 1;
    rmaini_(&i__1, &c__12, &yroff, &mon);
    yr = *year + yroff;
    ++mon;
    dy = *day;
    if (yr <= 0) {
	rmaini_(&yr, &c__400, &m400, &tmpyr);
	yr = tmpyr;
	if (yr == 0) {
	    yr += 400;
	    --m400;
	}
	offset = m400 * 146097;
    } else {
	offset = 0;
    }

/*     First get the day number (Gregorian) for the input */
/*     year month and day. */

/* Computing MAX */
    i__3 = 0, i__4 = abs(yr) / c__4 * c__4 + 1 - abs(yr);
/* Computing MAX */
    i__5 = 0, i__6 = abs(yr) / c__100 * c__100 + 1 - abs(yr);
/* Computing MAX */
    i__7 = 0, i__8 = abs(yr) / c__400 * c__400 + 1 - abs(yr);
    dayg = (yr - 1) * 365 + (yr - 1) / 4 - (yr - 1) / 100 + (yr - 1) / 400 + (
	    dpjan0[(i__1 = mon - 1) < 12 && 0 <= i__1 ? i__1 : s_rnge("dpjan0"
	    , i__1, "jul2gr_", (ftnlen)903)] + extra[(i__2 = mon - 1) < 12 && 
	    0 <= i__2 ? i__2 : s_rnge("extra", i__2, "jul2gr_", (ftnlen)903)] 
	    * (max(i__3,i__4) - max(i__5,i__6) + max(i__7,i__8)) + dy) - 1 + 
	    offset;

/*     This day is DAYG - RDAYG days after 1582 Oct 15 on the */
/*     Gregorian calendar.  But this is the same as the number */
/*     of days past 1582 Oct 5 on the Julian Calendar */
/*     So the Julian day number is DAYG - RDAYG + RDAYJ */
/*     which is the same as DAYG + OFFSTJ. */

    dayj = dayg + offstj;

/*     Now that we have the Julian day number it's a fairly */
/*     straight forward task to get the year, month and day */
/*     on the Julian calendar. */

    rmaini_(&dayj, &c__1461, &m4, &tmpday);
    dayj = tmpday;
/* Computing MIN */
    i__1 = 3, i__2 = dayj / 365;
    m = min(i__1,i__2);
    dayj -= m * 365;
    dofyr = dayj + 1;
    yr = (m4 << 2) + m + 1;

/*     Now look up the month number and compute the day of the month. */
/*     How we do this depends on whether or not this is a leap year. */

/* Computing MAX */
    i__1 = 0, i__2 = abs(yr) / c__4 * c__4 + 1 - abs(yr);
    if (max(i__1,i__2) == 0) {
	mon = lstlti_(&dofyr, &c__12, dpjan0);
	dy = dofyr - dpjan0[(i__1 = mon - 1) < 12 && 0 <= i__1 ? i__1 : 
		s_rnge("dpjan0", i__1, "jul2gr_", (ftnlen)937)];
    } else {
	mon = lstlti_(&dofyr, &c__12, dpbegl);
	dy = dofyr - dpbegl[(i__1 = mon - 1) < 12 && 0 <= i__1 ? i__1 : 
		s_rnge("dpbegl", i__1, "jul2gr_", (ftnlen)940)];
    }
    *year = yr;
    *month = mon;
    *day = dy;
    *doy = dofyr;
    return 0;
} /* jul2gr_ */

/* Subroutine */ int jul2gr_(integer *year, integer *month, integer *day, 
	integer *doy)
{
    return jul2gr_0_(0, year, month, day, doy);
    }

/* Subroutine */ int gr2jul_(integer *year, integer *month, integer *day, 
	integer *doy)
{
    return jul2gr_0_(1, year, month, day, doy);
    }

