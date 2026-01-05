/* tparse.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__400 = 400;

/* $Procedure TPARSE ( Parse a UTC time string ) */
/* Subroutine */ int tparse_(char *string, doublereal *sp2000, char *errmsg, 
	ftnlen string_len, ftnlen errmsg_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_dnnt(doublereal *);

    /* Local variables */
    integer year;
    doublereal tvec[10];
    logical mods;
    integer temp;
    char type__[5];
    integer q;
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    integer ntvec, month;
    logical ok;
    extern /* Subroutine */ int tcheck_(doublereal *, char *, logical *, char 
	    *, logical *, char *, ftnlen, ftnlen, ftnlen), rmaini_(integer *, 
	    integer *, integer *, integer *);
    logical succes, yabbrv;
    char modify[8*5];
    logical adjust;
    char pictur[80];
    extern /* Subroutine */ int tpartv_(char *, doublereal *, integer *, char 
	    *, char *, logical *, logical *, logical *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen), texpyr_(integer *);
    extern doublereal j2000_(void);
    integer day;
    extern doublereal spd_(void);

/* $ Abstract */

/*     Parse a time string and return seconds past the J2000 epoch */
/*     on a formal calendar. */

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
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   Input time string, UTC. */
/*     SP2000     O   Equivalent UTC seconds past J2000. */
/*     ERRMSG     O   Descriptive error message. */

/* $ Detailed_Input */

/*     STRING   is an input time string, containing a Calendar or Julian */
/*              Date. It may be in several different formats and can make */
/*              use of abbreviations. Several example strings and the */
/*              times that they translate to are listed below in the */
/*              $Examples section. */

/* $ Detailed_Output */

/*     SP2000   is the equivalent of UTC, expressed in UTC seconds past */
/*              J2000. If an error occurs, or if the input time string is */
/*              ambiguous, SP2000 is not changed. */

/*     ERRMSG   is a descriptive error message, which is blank when no */
/*              error occurs. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The input string is examined and the various components of a date */
/*     are identified: julian date, year, month, day of year, day of */
/*     month, hour, minutes, seconds. These items are assumed to be */
/*     components on a calendar that contains no leapseconds (i.e. every */
/*     day is assumed to have exactly 86400 seconds). */

/*     TPARSE recognizes a wide range of standard time formats. The */
/*     $Examples section contains a list of several common strings that */
/*     are recognized and their interpretation. TPARSE relies on the */
/*     lower level SPICELIB routine TPARTV to interpret the input string. */

/*     Here is a brief summary of some of the basic rules used in the */
/*     interpretation of strings. */

/*     1)  Unless the substring 'JD' or 'jd' is present, the string is */
/*         assumed to be a calendar format (day-month-year or year and */
/*         day of year). If the substring JD or jd is present, the */
/*         string is assumed to represent a Julian date. */

/*     2)  If the Julian date specifier is not present, any integer */
/*         greater than 999 is regarded as being a year specification. */

/*     3)  A dash '-' can represent a minus sign only if it precedes */
/*         the first digit in the string and the string contains */
/*         the Julian date specifier (JD). (No negative years, */
/*         months, days, etc. are allowed). */

/*     4)  Numeric components of a time string must be separated */
/*         by a character that is not a digit or decimal point. */
/*         Only one decimal component is allowed. For example */
/*         1994219.12819 is sometimes interpreted as the */
/*         219th day of 1994 + 0.12819 days. TPARSE does not */
/*         support such strings. */

/*     5)  No exponential components are allowed. For example you */
/*         can't specify the Julian date of J2000 as 2.451545E6. */
/*         You also can't input 1993 Jun 23 23:00:01.202E-4 and have */
/*         to explicitly list all zeros that follow the decimal */
/*         point: i.e. 1993 Jun 23 23:00:00.0001202. */

/*     6)  The single colon (:) when used to separate numeric */
/*         components of a string is interpreted as separating */
/*         Hours, Minutes, and Seconds of time. */

/*     7)  If a double slash (//) or double colon (::) follows */
/*         a pair of integers, those integers are assumed  to */
/*         represent the year and day of year. */

/*     8)  A quote followed by an integer less than 100 is regarded */
/*         as an abbreviated year. For example: '93 would be regarded */
/*         as the 93rd year of the reference century. See the SPICELIB */
/*         routine TEXPYR for further discussion of abbreviated years. */

/*     9)  An integer followed by 'B.C.' or 'A.D.' is regarded as */
/*         a year in the era associated with that abbreviation. */

/*     10) All dates are regarded as belonging to the extended */
/*         Gregorian Calendar (the Gregorian calendar is the calendar */
/*         currently used by western society). See the SPICELIB routine */
/*         JUL2GR for converting from Julian Calendar to the Gregorian */
/*         Calendar. */

/*     11) If the ISO date-time separator (T) is present in the string */
/*         ISO allowed token patterns are examined for a match */
/*         with the current token list. If no match is found the */
/*         search is abandoned and appropriate diagnostic messages */
/*         are generated. Historically the interpretation of ISO */
/*         formatted time strings deviates from the ISO standard in */
/*         allowing two digit years and expanding years in the 0 to 99 */
/*         range the same way as is done for non ISO formatted strings. */
/*         Due to this interpretation it is impossible to specify */
/*         times in years in the 0 A.D. to 99 A.D. range using ISO */
/*         formatted strings on the input. */

/*     12) If two delimiters are found in succession in the time */
/*         string, the time string is diagnosed as an erroneous string. */
/*         (Delimiters are comma, white space, dash, slash, period, or */
/*         day of year mark. The day of year mark is a pair of forward */
/*         slashes or a pair of colons.) */

/*         Note the delimiters do not have to be the same. The pair */
/*         of characters ",-" counts as two successive delimiters. */

/*     13) White space and commas serve only to delimit tokens in the */
/*         input string. They do not affect the meaning of any */
/*         of the tokens. */

/*     14) If an integer is greater than 1000 (and the 'JD' label */
/*         is not present, the integer is regarded as a year. */

/*     15) When the size of the integer components does not clearly */
/*         specify a year the following patterns are assumed */

/*         Calendar Format */

/*            Year Month Day */
/*            Month Day Year */
/*            Year Day Month */

/*            where Month is the name of a month, not its numeric */
/*            value. */

/*            When integer components are separated by slashes (/) */
/*            as in 3/4/5. Month, Day, Year is assumed (2005 March 4) */

/*         Day of Year Format. */

/*            If a day of year marker is present (// or ::) the */
/*            pattern */

/*              I-I// or I-I:: (where I stands for an integer) */

/*            is interpreted as Year Day-of-Year. However, I-I/ is */
/*            regarded as ambiguous. */

/*     To understand the complete list of strings that can be understood */
/*     by TPARSE you need to examine the SPICELIB routine TPARTV and read */
/*     the appendix to the TIME required reading entitled "Parsing Time */
/*     Strings." */

/*     TPARSE does not support the specification of time system */
/*     such as TDT or TDB; AM/PM specifications of time; or time */
/*     zones (such as PDT, UTC+7:20, etc.). */

/*     If some part of the time string is not recognized or if */
/*     the meaning of the components are not clear, an error string */
/*     is constructed that explains the problem with the string. */

/*     Since the routine works by breaking the input string into */
/*     a sequence of tokens whose meanings are determined by position */
/*     and magnitude, you can supply strings such as 1993 FEB 35 and */
/*     have this correctly interpreted as March 7, 1993. However, */
/*     this default action can be modified so that only "proper" */
/*     calendar dates and times are recognized. To do this call */
/*     the routine TPARCH as shown below: */

/*        CALL TPARCH ( 'YES' ) */

/*     This will cause the routine to treat dates and times with */
/*     components outside the normal range as errors. */

/*     To return to the default behavior */

/*        CALL TPARCH ( 'NO' ) */

/*     This routine returns information about parse errors in the output */
/*     string ERRMSG. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Parse a series of time strings representing calendar dates and */
/*        output number of seconds past J2000 epoch that corresponds to */
/*        each of them. Some of the input strings have an invalid format */
/*        which is reflected in their output. */


/*        Example code begins here. */


/*              PROGRAM TPARSE_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local constants */
/*        C */
/*              INTEGER               ERRMLN */
/*              PARAMETER           ( ERRMLN = 36 ) */

/*              INTEGER               DATELN */
/*              PARAMETER           ( DATELN = 22 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(DATELN)    DATE   (7) */
/*              CHARACTER*(ERRMLN)    ERRMSG */

/*              DOUBLE PRECISION      SP2000 */

/*              INTEGER               I */

/*        C */
/*        C     Assign an array of calendar dates. Not all of them are */
/*        C     valid. */
/*        C */
/*              DATA                  DATE   / '1986-01-18T12:19:52.18', */
/*             .                               '17JUN1982 18:28:28', */
/*             .                               '182-''92/ 12:28:29.182', */
/*             .                               '''67-271/ 12:28:30.291', */
/*             .                               '-467-14-25 26:00:75', */
/*             .                               '1993 FEB 35', */
/*             .                               '1993 MAR 7' / */

/*        C */
/*        C     Loop over the DATE array, call TPARSE for each element. */
/*        C */
/*              WRITE(*,'(A)') '    Input string        ' */
/*             .            // 'UTC sec past J2000' */
/*              WRITE(*,'(A)') '----------------------  ' */
/*             .            // '------------------' */

/*              DO I= 1, 7 */

/*                 CALL TPARSE ( DATE(I), SP2000, ERRMSG ) */

/*                 IF ( ERRMSG .EQ. ' ' ) THEN */

/*                    WRITE(*,'(A22,2X,F18.6)') DATE(I), SP2000 */

/*                 ELSE */

/*                    WRITE(*,'(A22,2X,A)') DATE(I), ERRMSG */

/*                 END IF */

/*              END DO */

/*              END */


/*        When this program was executed on a PC/Linux/gfortran/64-bit */
/*        platform, the output was: */


/*            Input string        UTC sec past J2000 */
/*        ----------------------  ------------------ */
/*        1986-01-18T12:19:52.18   -440293207.820000 */
/*        17JUN1982 18:28:28       -553541492.000000 */
/*        182-'92/ 12:28:29.182    -236820690.818000 */
/*        '67-271/ 12:28:30.291    2137710510.291000 */
/*        -467-14-25 26:00:75     An unexpected delimiter ('-') was en */
/*        1993 FEB 35              -215265600.000000 */
/*        1993 MAR 7               -215265600.000000 */


/*        Note that the "1993 FEB 35" string in converted to UTC seconds */
/*        past J2000, interpreted as "1993 MAR 7". Also note that the */
/*        error message resulting from parsing "-467-14-25 26:00:75" */
/*        is truncated to the provided ERRMLN length. */


/*     2) Below is a sampling of some of the time formats that are */
/*        acceptable as inputs to TPARSE. A complete discussion of */
/*        permissible formats is given in the reference document */
/*        time.req. */

/*        ISO (T) Formats. */

/*        String                        Year Mon  DOY DOM  HR Min Sec */
/*        ----------------------------  ---- ---  --- ---  -- --- ----- */
/*        1996-12-18T12:28:28           1996 Dec   na  18  12  28 28 */
/*        1986-01-18T12                 1986 Jan   na  18  12  00 00 */
/*        1986-01-18T12:19              1986 Jan   na  18  12  19 00 */
/*        1986-01-18T12:19:52.18        1986 Jan   na  18  12  19 52.18 */
/*        1986-01-18T12:19:52.18Z       1986 Jan   na  18  12  19 52.18 */
/*        1995-08T18:28:12              1995  na  008  na  18  28 12 */
/*        1995-08T18:28:12Z             1995  na  008  na  18  28 12 */
/*        1995-18T                      1995  na  018  na  00  00 00 */
/*        0000-01-01T                   1 BC Jan   na  01  00  00 00 */


/*        Calendar Formats. */

/*        String                        Year   Mon DOM  HR Min  Sec */
/*        ----------------------------  ----   --- ---  -- ---  ------ */
/*        Tue Aug  6 11:10:57  1996     1996   Aug  06  11  10  57 */
/*        1 DEC 1997 12:28:29.192       1997   Dec  01  12  28  29.192 */
/*        2/3/1996 17:18:12.002         1996   Feb  03  17  18  12.002 */
/*        Mar 2 12:18:17.287 1993       1993   Mar  02  12  18  17.287 */
/*        1992 11:18:28  3 Jul          1992   Jul  03  11  18  28 */
/*        June 12, 1989 01:21           1989   Jun  12  01  21  00 */
/*        1978/3/12 23:28:59.29         1978   Mar  12  23  28  59.29 */
/*        17JUN1982 18:28:28            1982   Jun  17  18  28  28 */
/*        13:28:28.128 1992 27 Jun      1992   Jun  27  13  28  28.128 */
/*        1972 27 jun 12:29             1972   Jun  27  12  29  00 */
/*        '93 Jan 23 12:29:47.289       1993*  Jan  23  12  29  47.289 */
/*        27 Jan 3, 19:12:28.182        2027*  Jan  03  19  12  28.182 */
/*        23 A.D. APR 4, 18:28:29.29    0023** Apr  04  18  28  29.29 */
/*        18 B.C. Jun 3, 12:29:28.291   -017** Jun  03  12  29  28.291 */
/*        29 Jun  30 12:29:29.298       2029+  Jun  30  12  29  29.298 */
/*        29 Jun '30 12:29:29.298       2030*  Jun  29  12  29  29.298 */


/*        Day of Year Formats. */

/*        String                        Year  DOY HR Min Sec */
/*        ----------------------------  ----  --- -- --- ------ */
/*        1997-162::12:18:28.827        1997  162 12  18 28.827 */
/*        162-1996/12:28:28.287         1996  162 12  28 28.287 */
/*        1993-321/12:28:28.287         1993  231 12  28 28.287 */
/*        1992 183// 12:18:19           1992  183 12  18 19 */
/*        17:28:01.287 1992-272//       1992  272 17  28 01.287 */
/*        17:28:01.282 272-1994//       1994  272 17  28 01.282 */
/*        '92-271/ 12:28:30.291         1992* 271 12  28 30.291 */
/*        92-182/ 18:28:28.281          1992* 182 18  28 28.281 */
/*        182-92/ 12:29:29.192          0182+ 092 12  29 29.192 */
/*        182-'92/ 12:28:29.182         1992  182 12  28 29.182 */


/*        Julian Date Strings. */

/*        jd 28272.291                  Julian Date   28272.291 */
/*        2451515.2981 (JD)             Julian Date 2451515.2981 */
/*        2451515.2981 JD               Julian Date 2451515.2981 */

/*                                     Abbreviations Used in Tables */

/*                                        na    --- Not Applicable */
/*                                        Mon   --- Month */
/*                                        DOY   --- Day of Year */
/*                                        DOM   --- Day of Month */
/*                                        Wkday --- Weekday */
/*                                        Hr    --- Hour */
/*                                        Min   --- Minutes */
/*                                        Sec   --- Sec */

/*        *  The default interpretation of a year that has been */
/*           abbreviated to two digits with or without a leading quote */
/*           as in 'xy or xy (such as '92 or 92) is to treat the year as */
/*           19xy if xy > 68 and to treat it as 20xy otherwise. Thus '70 */
/*           is interpreted as 1970 and '67 is treated as 2067. However, */
/*           you may change the "split point" and centuries through use */
/*           of the SPICE routine TSETYR. See that routine for a */
/*           discussion of how you may reset the split point. */

/*        ** All epochs are regarded as belonging to the Gregorian */
/*           calendar. We formally extend the Gregorian calendar backward */
/*           and forward in time for all epochs. If you have epochs */
/*           belonging to the Julian Calendar, consult the SPICELIB */
/*           routines TPARTV and JUL2GR for a discussion concerning */
/*           conversions to the Gregorian calendar and ET. The routines */
/*           TIMDEF and STR2ET, used together, also support conversions */
/*           from Julian Calendar epochs to ET. */

/*        +  When a day of year format or calendar format string is */
/*           input and neither of the integer components of the date is */
/*           greater than 1000, the first integer is regarded as being */
/*           the year. */

/*        Any integer greater than 1000 is regarded as a year */
/*        specification. Thus 1001-1821//12:28:28 is interpreted as */
/*        specifying two years and will be rejected as ambiguous. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     M. Costa Sitja     (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     W.M. Owen          (JPL) */
/*     B.V. Semenov       (JPL) */
/*     M.J. Spencer       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.1.0, 23-DEC-2021 (JDR) (MCS) */

/*        Changed the output argument name ERROR to ERRMSG for */
/*        consistency with other routines. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary entries in $Revisions section. */

/*        Added complete example code. */

/*        Updated $Examples to refer to STR2ET and TIMDEF as a */
/*        mechanism to convert from Julian Calendar to ephemeris time. */

/*        Added TIME to $Required_Reading list. */

/* -    SPICELIB Version 5.0.1, 18-MAY-2010 (BVS) */

/*        Removed "C$" marker from text in the header. */

/* -    SPICELIB Version 5.0.0, 30-DEC-1997 (WLT) */

/*        The routine was modified to compensate for the inability */
/*        of the Muller-Wimberly formula to handle negative years */
/*        (that is years prior to 1 AD). */

/*        Comments concerning the default century used for two */
/*        digit years were upgraded. */

/* -    SPICELIB Version 4.0.0, 08-APR-1996 (WLT) */

/*        All of the token recognition and parsing was moved */
/*        into the routine TPARTV. The entry point TPARCH */
/*        was moved to the routine TCHECK. */

/*        This routine now merely assembles the */
/*        parsed components to produce SP2000. */

/*        The number of strings now recognized has been greatly */
/*        increased. However, the interpretation given to */
/*        strings such as 31 Jan 32 has been changed. */

/* -    SPICELIB Version 3.0.0, 30-JUL-1993 (WLT) */

/*        The entry point TPARCH was added so that users may */
/*        restrict the set of input calendar strings to those */
/*        that are in proper form. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 18-NOV-1991 (MJS) */

/*        TPARSE no longer accepts a blank time string. */

/* -    SPICELIB Version 1.0.1, 26-MAR-1991 (JML) */

/*        In the $Detailed_Input section of the header, the */
/*        description of how default values are assigned to */
/*        tokens in STRING was clarified. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (IMU) */

/* -& */
/* $ Index_Entries */

/*     parse a utc time string */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 18-NOV-1991 (MJS) */

/*        TPARSE no longer accepts a blank time string. Prior to */
/*        this fix, TPARSE interpreted a blank time string to be */
/*        -1577880000.000 UTC seconds (1 JAN 1950 00:00:00). */

/* -    SPICELIB Version 1.0.1, 26-MAR-1991 (JML) */

/*        In the $Detailed_Input section of the header, the */
/*        description of how default values are assigned to */
/*        tokens in STRING was clarified. */

/*        NAIFers are accustomed to specifying day of year */
/*        formats of UTC strings in the following form: */

/*           1986-247 // 12:00:00 */

/*        This revision to the header states explicitly that */
/*        the // is a blank token which results in the default */
/*        value being assigned to the month token. The previous */
/*        version of the header implied that tokens could be left */
/*        out or "missing" from the string, and that default values */
/*        would automatically be assigned. This works only for */
/*        tokens missing from the right end of the string. For */
/*        default values to be assigned to tokens missing from the */
/*        middle of a UTC string, consecutive delimiters such as */
/*        // or :: must be included. */

/* -& */

/*     SPICELIB functions */


/*     Parameters */


/*     Local variables */


/*     All the work of taking apart the string is handled */
/*     by TPARTV. */

    s_copy(errmsg, " ", errmsg_len, (ftnlen)1);
    succes = TRUE_;
    tpartv_(string, tvec, &ntvec, type__, modify, &mods, &yabbrv, &succes, 
	    pictur, errmsg, string_len, (ftnlen)5, (ftnlen)8, (ftnlen)80, 
	    errmsg_len);
    if (! succes) {
	return 0;
    }

/*     We are not going to support all of the various */
/*     time string modifiers that can be parsed. */

    if (mods) {
	if (s_cmp(modify + 32, " ", (ftnlen)8, (ftnlen)1) != 0) {
	    s_copy(errmsg, "TPARSE does not support the specification of a t"
		    "ime system in a string.  The time system # was specified"
		    ". ", errmsg_len, (ftnlen)106);
	    repmc_(errmsg, "#", modify + 32, errmsg, errmsg_len, (ftnlen)1, (
		    ftnlen)8, errmsg_len);
	    return 0;
	} else if (s_cmp(modify + 16, " ", (ftnlen)8, (ftnlen)1) != 0) {
	    s_copy(errmsg, "TPARSE does not support the specification of a t"
		    "ime zone in a time string.  The time zone '#' was specif"
		    "ied. ", errmsg_len, (ftnlen)109);
	    repmc_(errmsg, "#", modify + 16, errmsg, errmsg_len, (ftnlen)1, (
		    ftnlen)8, errmsg_len);
	    return 0;
	} else if (s_cmp(modify + 24, " ", (ftnlen)8, (ftnlen)1) != 0) {
	    s_copy(errmsg, "TPARSE does not support the AM/PM conventions fo"
		    "r time strings. ", errmsg_len, (ftnlen)64);
	    return 0;
	}
    }
    if (s_cmp(type__, "JD", (ftnlen)5, (ftnlen)2) == 0) {

/*        Nothing to do but convert TVEC(1). */

	*sp2000 = (tvec[0] - j2000_()) * spd_();
    } else if (s_cmp(type__, "YMD", (ftnlen)5, (ftnlen)3) == 0 || s_cmp(
	    type__, "YD", (ftnlen)5, (ftnlen)2) == 0) {
	tcheck_(tvec, type__, &mods, modify, &ok, errmsg, (ftnlen)5, (ftnlen)
		8, errmsg_len);
	if (! ok) {
	    return 0;
	}

/*        If we have day of year format, we move it into the */
/*        month-day of month format. */

	if (s_cmp(type__, "YD", (ftnlen)5, (ftnlen)2) == 0) {
	    tvec[5] = tvec[4];
	    tvec[4] = tvec[3];
	    tvec[3] = tvec[2];
	    tvec[2] = tvec[1];
	    tvec[1] = 1.;
	}

/*        Get the year month and day as integers. */

	year = i_dnnt(tvec);
	month = i_dnnt(&tvec[1]);
	day = i_dnnt(&tvec[2]);

/*        Fix up the year as needed. */

	if (s_cmp(modify, "B.C.", (ftnlen)8, (ftnlen)4) == 0) {
	    year = 1 - year;
	} else if (s_cmp(modify, "A.D.", (ftnlen)8, (ftnlen)4) == 0) {

/*           Do nothing. */

	} else if (year < 100) {
	    texpyr_(&year);
	}

/*        Apply the Muller-Wimberly formula and then tack on */
/*        the seconds. */

	if (year < 1) {

/*           The Muller-Wimberly formula doesn't work for years */
/*           less than 0.  So we boost the year by an appropriate */
/*           multiple of 400 and then subtract the appropriate */
/*           number of days later. */

	    adjust = TRUE_;
	    temp = year;
	    rmaini_(&temp, &c__400, &q, &year);
	    year += 400;
	    --q;
	} else {
	    adjust = FALSE_;
	}
	day = year * 367 - (year + (month + 9) / 12) * 7 / 4 - ((year + (
		month - 9) / 7) / 100 + 1) * 3 / 4 + month * 275 / 9 + day - 
		730516;
	if (adjust) {

/*           Adjust DAY by the appropriate multiple of 400 years. */

	    day += q * 146097;
	}
	*sp2000 = ((doublereal) day - .5) * spd_() + tvec[3] * 3600. + tvec[4]
		 * 60. + tvec[5];
    } else {

/*        We've already covered all the bases we are planning to */
/*        cover in this routine.  Any other case is regarded as an */
/*        error. */

	s_copy(errmsg, "The only type of time strings that are handled by TP"
		"ARSE are 'JD', 'YMD' and 'YD' (year day-of-year).  You've en"
		"tered a string of the type #. ", errmsg_len, (ftnlen)142);
	repmc_(errmsg, "#", type__, errmsg, errmsg_len, (ftnlen)1, (ftnlen)5, 
		errmsg_len);
    }
    return 0;
} /* tparse_ */

