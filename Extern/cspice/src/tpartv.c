/* tpartv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__300 = 300;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__8 = 8;

/* $Procedure TPARTV ( Time string ---parse to a time vector) */
/* Subroutine */ int tpartv_(char *string, doublereal *tvec, integer *ntvec, 
	char *type__, char *modify, logical *mods, logical *yabbrv, logical *
	succes, char *pictur, char *error, ftnlen string_len, ftnlen type_len,
	 ftnlen modify_len, ftnlen pictur_len, ftnlen error_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char zones[3*8] = "EST" "EDT" "CST" "CDT" "MST" "MDT" "PST" "PDT";
    static char offset[6*8] = "UTC-5 " "UTC-4 " "UTC-6 " "UTC-5 " "UTC-7 " 
	    "UTC-6 " "UTC-8 " "UTC-7 ";

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_indx(char *, char *, ftnlen, ftnlen), s_cmp(char *, char *, 
	    ftnlen, ftnlen), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern logical zztokns_(char *, char *, ftnlen, ftnlen);
    static integer begs[5], ends[5], from, b, e;
    extern /* Subroutine */ int zzinssub_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static integer i__, r__;
    static char delim[1*3];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    repmc_(char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen);
    static integer mapto, b1, b2, e1, e2;
    static char known[12*300];
    extern integer rtrim_(char *, ftnlen);
    extern logical zzist_(char *, ftnlen);
    static integer to;
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static char meanng[12*300];
    static logical havera;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static logical havapm;
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    extern integer intmax_(void);
    static logical havwdy;
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static logical havzon;
    extern logical zzcmbt_(char *, char *, logical *, ftnlen, ftnlen);
    static integer nknown;
    static logical resolv, havsys;
    extern logical zzgrep_(char *, ftnlen);
    static logical l2r, r2l;
    extern logical zznote_(char *, integer *, integer *, ftnlen), zzvalt_(
	    char *, integer *, integer *, char *, ftnlen, ftnlen), zzremt_(
	    char *, ftnlen), zzrept_(char *, char *, logical *, ftnlen, 
	    ftnlen), zzsubt_(char *, char *, logical *, ftnlen, ftnlen), 
	    zzispt_(char *, integer *, integer *, ftnlen);
    static char rep[12];
    static integer use;
    extern logical zzunpck_(char *, logical *, doublereal *, integer *, char *
	    , char *, char *, ftnlen, ftnlen, ftnlen, ftnlen), zztpats_(
	    integer *, integer *, char *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     Parse the components of a time string and return a vector of the */
/*     components of that string. Also return an array of any modifiers */
/*     present in the input string; these may alter the interpretation */
/*     of the components. */

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
/*     STRING     I   A string to be parsed as a time */
/*     TVEC       O   A vector giving the components of the time. */
/*     NTVEC      O   The number of components supplied for TVEC */
/*     TYPE       O   The type of the "time vector" TVEC */
/*     MODIFY     O   A list of modifiers present in STRING. */
/*     MODS       O   A logical indicating the presence of a modifier */
/*     YABBRV     O   A logical indicating that a year was abbreviated */
/*     SUCCES     O   A logical indicating whether STRING was parsed. */
/*     PICTUR     O   A time format picture associated with STRING */
/*     ERROR      O   A diagnostic message if STRING couldn't be parsed */

/*     The function returns */

/* $ Detailed_Input */

/*     STRING   is a character string that represents some */
/*              julian or calendar epoch. */

/* $ Detailed_Output */

/*     TVEC     is a vector of double precision numbers that represent */
/*              the input string. The number and meaning of the */
/*              components of TVEC depend upon the input string. This */
/*              meaning can be determined from the output variable */
/*              TYPE. */

/*                 TYPE     NTVEC     TVEC Components */
/*                 ----------------------------------------------------- */
/*                 YMD      3 to 6    TVEC(1) is the calendar year */
/*                                    TVEC(2) is the numeric value of the */
/*                                            month (1-12) */
/*                                    TVEC(3) is the day of the month */
/*                                    TVEC(4) is the hour of the day */
/*                                    TVEC(5) is the minute of the hour */
/*                                    TVEC(6) is the second of the minute */

/*                 YD       2 to 5    TVEC(1) is the calendar year */
/*                                    TVEC(2) is the day of the year */
/*                                    TVEC(3) is the hour of the day */
/*                                    TVEC(4) is the minute of the hour */
/*                                    TVEC(5) is the second of the minute */

/*                 JD       1         TVEC(1) is the julian date */

/*              Note that the values of TVEC are not forced into the */
/*              normal ranges used in daily conversation.  TPARTV */
/*              simply reports what's found in the string and does */
/*              not pass judgement on the "correctness" of these */
/*              components. */

/*     NTVEC    is the actual number of components that were present */
/*              in the string. For example a user might have */
/*              supplied only year, month and day of an epoch. */
/*              In such a case NTVEC will be set to 3. The components */
/*              actually supplied will be 1 through NTVEC. Values */
/*              not supplied are set to zero. */

/*     TYPE     is the type of time string supplied. This is a function */
/*              of whether the string contains year, month and day, */
/*              day of year, or julian date. */

/*     MODIFY   is an array of character strings that indicate */
/*              whether a modifier to the calendar string was supplied. */
/*              If a particular modifier was not supplied, the */
/*              value of that component of MODIFY will be set to */
/*              a blank. Modifiers are used to change the meaning */
/*              of time strings. */

/*              For example 12:12:29 Jan 1, 1996  means 12 hours past */
/*              midnight on Jan 1, 1996 in the UTC time system. But */
/*              if we modify the string to be: */

/*                 12:12:29 A.M. Jan 1, Tuesday PDT 1996 B.C. */

/*              the string takes on an entirely different meaning. */

/*              Five different modifiers are recognized by TPARTV: */
/*              the era associated with the epoch, day of week of */
/*              the epoch, time zone of an epoch,  AM/PM used in */
/*              daily time usage, and the system (UTC, TDB, TT, or TDT). */

/*              Again whether or not modifiers are compatible with the */
/*              time and date components or with each other is not */
/*              determined by TPARTV. TPARTV simply reports what is */
/*              present in the string, leaving the task of deciding */
/*              the meaning of the string to the calling routine. */

/*              The components of MODIFY, their meaning and possible */
/*              values are given below. */

/*                                        Possible */
/*                 Component   Meaning    Non-blank Modifier Values */
/*                 ---------   ---------  ------------------------- */
/*                 1           ERA        'A.D.', 'B.C.' */
/*                 2           Weekday    'SUN', 'MON', ... etc. */
/*                 3           Time Zone  'UTC+i:i', 'UTC-i:i' */
/*                 4           AM/PM      'A.M.', 'P.M.' */
/*                 5           System     'UTC', 'TDB', 'TT', 'TDT' */

/*              TPARTV recognizes the standard abbreviations of */
/*              all continental U.S. time zones. */

/*                 PDT --- Pacific  Daylight Time  (UTC-07:00) */
/*                 PST --- Pacific  Standard Time  (UTC-08:00) */
/*                 MDT --- Mountain Daylight Time  (UTC-06:00) */
/*                 MST --- Mountain Standard Time  (UTC-07:00) */
/*                 CDT --- Central  Daylight Time  (UTC-05:00) */
/*                 CST --- Central  Standard Time  (UTC-06:00) */
/*                 EDT --- Eastern  Daylight Time  (UTC-04:00) */
/*                 EST --- Eastern  Standard Time  (UTC-05:00) */

/*              In addition it recognizes offsets from UTC expressed */
/*              as UTC+/-HR:MN. Note that through out SPICELIB */
/*              the minutes component of the UTC offset are always */
/*              regarded as positive offsets from the hour offset. */

/*              All Time zones are returned in MODIFY as UTC offsets */
/*              as indicated in the table above. */

/*     MODS     is .TRUE. if some non-blank modifier was supplied. */

/*     YABBRV   is .TRUE. if a year was supplied in the abbreviated */
/*              form 'YR  where YR is a two digit integer. */

/*     SUCCES   is .TRUE. if the string was successfully parsed. */
/*              Otherwise it is set to .FALSE. and a diagnostic */
/*              is supplied in the argument ERROR. */

/*     PICTUR   is a string that gives a format picture that can */
/*              be used by the routine TIMOUT to construct a time */
/*              string of the same form as the input time string. */

/*              If some component of the input string could not be */
/*              identified, PICTUR is returned as a blank. However, */
/*              if all components of the input string could be */
/*              identified and the string is simply ambiguous, PICTUR */
/*              will contain a format picture that corresponds to */
/*              the ambiguous input. Consequently, you must check */
/*              the value of PICTUR to determine if TPARTV has */
/*              been able to construct a format picture. */

/*     ERROR    is blank if the string was successfully parsed. */
/*              Otherwise a human readable diagnostic is returned */
/*              in ERROR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  All problems detected by this routine are reported via the */
/*         variables SUCCES and ERROR. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine parses in input string that represents some */
/*     epoch in some time system. In addition it constructs a */
/*     format picture that describes the position and meaning */
/*     of the various components of the string. */

/*     This routine is intended to be used in close conjunction with */
/*     the routines TTRANS and TIMOUT. */

/*     The string is parsed by first determining its recognizable */
/*     substrings (integers, punctuation marks, names of months, */
/*     names of weekdays, time systems, time zones, etc.) These */
/*     recognizable substrings are called the tokens of the input */
/*     string. The meaning of some tokens are immediately determined. */
/*     For example named months, weekdays, time systems have clear */
/*     meanings. However, the meanings of numeric components must */
/*     be deciphered from their magnitudes and location in */
/*     the string relative to the immediately recognized components */
/*     of the input string. */

/*     To determine the meaning of the numeric tokens in the input */
/*     string, a set of "productions rules" and transformations are */
/*     applied to the full set of tokens in the string. These */
/*     transformations are repeated until the meaning of every token */
/*     has been determined or until further transformations yield */
/*     no new clues into the meaning of the numeric tokens. */

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
/*         219th day of 1994 + 0.12819 days. TPARTV does not */
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

/*     The table below gives a list of abbreviations used to */
/*     classify tokens. */

/*                /   ---  slash punctuation mark */
/*                H   ---  hour */
/*                M   ---  Minute */
/*                S   ---  Second */
/*                Y   ---  year */
/*                d   ---  day of year marker */
/*                i   ---  unsigned integer */
/*                m   ---  month */
/*                n   ---  unsigned decimal number */
/*                y   ---  day of year */
/*                -   ---  dash punctuation mark */
/*                D   ---  day of month */
/*                :   ---  colon punctuation mark */

/*      Given these abbreviations the following (rather lengthy) */
/*      table gives the set of built in token patterns that */
/*      are recognized and the associated interpretation of that */
/*      pattern. */

/*         Pattern         Meaning         Pattern         Meaning */
/*         ------------------------        ------------------------- */
/*         Y-i-it......... YmD             i/i/ii:i:n..... mDYHMS */
/*         Y-i-iti........ YmDH            i/i/ii:n....... mDYHM */
/*         Y-i-iti:i...... YmDHM           i/i/ii:n....... mDYHM */
/*         Y-i-iti:i:i.... YmDHMS          i:i:ii-i-Y..... HMSmDY */
/*         Y-i-iti:i:n.... YmDHMS          i:i:ii/i/Y..... HMSmDY */
/*         Y-i-iti:n...... YmDHM           i:i:ii/i/i..... HMSmDY */
/*         Y-i-itn........ YmDH            i:i:iimY....... HMSDmY */
/*         Y-i/........... Yy              i:i:imiY....... HMSmDY */
/*         Y-i/i:i........ YyHM            i:i:ni-i-Y..... HMSmDY */
/*         Y-i/i:i:i...... YyHMS           i:i:ni/i/Y..... HMSmDY */
/*         Y-i/i:i:n...... YyHMS           i:i:ni/i/i..... HMSmDY */
/*         Y-i/i:n........ YyHM            i:i:nimY....... HMSDmY */
/*         Y-id........... Yy              i:i:nmiY....... HMSmDY */
/*         Y-idi:i........ YyHM            i:ii-i-Y....... HMmDY */
/*         Y-idi:i:i...... YyHMS           i:ii/i/Y....... HMmDY */
/*         Y-idi:i:n...... YyHMS           i:ii/i/i....... HMmDY */
/*         Y-idi:n........ YyHM            i:iimY......... HMDmY */
/*         Y-it........... Yy              i:imiY......... HMmDY */
/*         Y-iti.......... YyH             i:ni-i-Y....... HMmDY */
/*         Y-iti:i........ YyHM            i:ni/i/Y....... HMmDY */
/*         Y-iti:i:i...... YyHMS           i:ni/i/i....... HMmDY */
/*         Y-iti:i:n...... YyHMS           i:nimY......... HMDmY */
/*         Y-iti:n........ YyHM            i:nmiY......... HMmDY */
/*         Y-itn.......... YyH             iYd............ yY */
/*         Yid............ Yy              iYdi:i......... yYHM */
/*         Yidi:i......... YyHM            iYdi:i:i....... yYHMS */
/*         Yidi:i:i....... YyHMS           iYdi:i:n....... yYHMS */
/*         Yidi:i:n....... YyHMS           iYdi:n......... yYHM */
/*         Yidi:n......... YyHM            iiY............ mDY */
/*         Yii............ YmD             iiYi........... mDYH */
/*         Yiii........... YmDH            iiYi:i......... mDYHM */
/*         Yiii:i......... YmDHM           iiYi:i:i....... mDYHMS */
/*         Yiii:i:i....... YmDHMS          iiYi:i:n....... mDYHMS */
/*         Yiii:i:n....... YmDHMS          iiYi:n......... mDYHM */
/*         Yiii:n......... YmDHM           iiYn........... mDYH */
/*         Yiiii.......... YmDHM           iid............ Yy */
/*         Yiiiii......... YmDHMS          iidi:i......... YyHM */
/*         Yiiiin......... YmDHMS          iidi:i:i....... YyHMS */
/*         Yiiin.......... YmDHM           iidi:i:n....... YyHMS */
/*         Yiin........... YmDH            iidi:n......... YyHM */
/*         Yim............ YDm             iim............ YDm */
/*         Yimi........... YDmH            iimi........... YDmH */
/*         Yimi:i......... YDmHM           iimi:i......... YDmHM */
/*         Yimi:i:i....... YDmHMS          iimi:i:i....... YDmHMS */
/*         Yimi:i:n....... YDmHMS          iimi:i:n....... YDmHMS */
/*         Yimi:n......... YDmHM           iimi:n......... YDmHM */
/*         Yimn........... YDmH            iimii.......... YDmHM */
/*         Yin............ YmD             iimiii......... YDmHMS */
/*         Ymi............ YmD             iimiin......... YDmHMS */
/*         Ymii........... YmDH            iimin.......... YDmHM */
/*         Ymii:i......... YmDHM           iimn........... YDmH */
/*         Ymii:i:i....... YmDHMS          imY............ DmY */
/*         Ymii:i:n....... YmDHMS          imYi........... DmYH */
/*         Ymii:n......... YmDHM           imYi:i......... DmYHM */
/*         Ymin........... YmDH            imYi:i:i....... DmYHMS */
/*         Ymn............ YmD             imYi:i:n....... DmYHMS */
/*         Ynm............ YDm             imYi:n......... DmYHM */
/*         i-Y/........... yY              imYn........... DmYH */
/*         i-Y/i:i........ yYHM            imi............ YmD */
/*         i-Y/i:i:i...... yYHMS           imi:i:iY....... DmHMSY */
/*         i-Y/i:i:n...... yYHMS           imi:i:nY....... DmHMSY */
/*         i-Y/i:n........ yYHM            imi:iY......... DmHMY */
/*         i-Yd........... yY              imi:nY......... DmHMY */
/*         i-Ydi:i........ yYHM            imii........... YmDH */
/*         i-Ydi:i:i...... yYHMS           imii:i......... YmDHM */
/*         i-Ydi:i:n...... yYHMS           imii:i:i....... YmDHMS */
/*         i-Ydi:n........ yYHM            imii:i:n....... YmDHMS */
/*         i-i-Y.......... mDY             imii:n......... YmDHM */
/*         i-i-Yi:i....... mDYHM           imiii.......... YmDHM */
/*         i-i-Yi:i:i..... mDYHMS          imiiii......... YmDHMS */
/*         i-i-Yi:i:n..... mDYHMS          imiiin......... YmDHMS */
/*         i-i-Yi:n....... mDYHM           imiin.......... YmDHM */
/*         i-i-it......... YmD             imin........... YmDH */
/*         i-i-iti........ YmDH            imn............ YmD */
/*         i-i-iti:i...... YmDHM           inY............ mDY */
/*         i-i-iti:i:i.... YmDHMS          inm............ YDm */
/*         i-i-iti:i:n.... YmDHMS          miY............ mDY */
/*         i-i-iti:n...... YmDHM           miYi........... mDYH */
/*         i-i-itn........ YmDH            miYi:i......... mDYHM */
/*         i-i/i:i........ YyHM            miYi:i:i....... mDYHMS */
/*         i-i/i:i:i...... YyHMS           miYi:i:n....... mDYHMS */
/*         i-i/i:i:n...... YyHMS           miYi:n......... mDYHM */
/*         i-i/i:n........ YyHM            miYn........... mDYH */
/*         i-idi:i........ YyHM            mii............ mDY */
/*         i-idi:i:i...... YyHMS           mii:i:iY....... mDHMSY */
/*         i-idi:i:n...... YyHMS           mii:i:nY....... mDHMSY */
/*         i-idi:n........ YyHM            mii:iY......... mDHMY */
/*         i-it........... Yy              mii:nY......... mDHMY */
/*         i-iti.......... YyH             miii........... mDYH */
/*         i-iti:i........ YyHM            miii:i......... mDYHM */
/*         i-iti:i:i...... YyHMS           miii:i:i....... mDYHMS */
/*         i-iti:i:n...... YyHMS           miii:i:n....... mDYHMS */
/*         i-iti:n........ YyHM            miii:n......... mDYHM */
/*         i-itn.......... YyH             miiii.......... mDYHM */
/*         i/i/Y.......... mDY             miiiii......... mDYHMS */
/*         i/i/Y/i:n...... mDYHM           miiiin......... mDYHMS */
/*         i/i/Yi:i....... mDYHM           miiin.......... mDYHM */
/*         i/i/Yi:i:i..... mDYHMS          miin........... mDYH */
/*         i/i/Yi:i:n..... mDYHMS          mnY............ mDY */
/*         i/i/i.......... mDY             mni............ mDY */
/*         i/i/ii:i....... mDYHM           nmY............ DmY */
/*         i/i/ii:i:i..... mDYHMS */

/* $ Examples */

/*     Suppose you need to convert various time strings to ephemeris */
/*     seconds past J2000. The following pair of calls shows */
/*     how you would use this routine together with the routines */
/*     TCHECK and TTRANS to perform this task. */


/*         CALL TPARTV ( STRING, */
/*        .              TVEC,   NTVEC, TYPE, */
/*        .              MODIFY, MODS,  YABBRV, SUCCES, */
/*        .              PICTUR, ERROR ) */


/*         IF ( .NOT. SUCCES ) THEN */

/*            Use the SPICE error handling facility to post an */
/*            error message and signal an error. */

/*            CALL SETMSG ( ERROR ) */
/*            CALL SIGERR ( 'MYCHECK(BADTIME)' ) */
/*            CALL CHKOUT ( 'MYROUTINE' ) */
/*            RETURN */
/*         END IF */

/*         Check the components of TVEC to make sure everything */
/*         makes sense. */

/*         CALL TCHECK( TVEC, TYPE, MODS, MODIFY, OK, ERROR ) */

/*         IF ( .NOT. OK ) THEN */

/*            Use the SPICE error handling facility to post an */
/*            error message and signal an error. */

/*            CALL SETMSG ( ERROR ) */
/*            CALL SIGERR ( 'MYCHECK(BADTIME)' ) */
/*            CALL CHKOUT ( 'MYROUTINE' ) */
/*            RETURN */
/*         END IF */

/*         CALL TTRANS ( TYPE, 'ET', TVEC ) */

/*         ET = TVEC(1) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.2.0, 23-DEC-2021 (EDW) (BVS) (JDR) */

/*        ISO format logic recognizes/evaluates ISO time strings */
/*        with or without trailing 'Z'. */

/*        Reordered header sections. Edited the header to comply with */
/*        NAIF standard. */

/*        Updated $Exceptions entry #1 wording. */

/* -    SPICELIB Version 3.1.0, 15-AUG-2002 (WLT) */

/*        Replaced the call to INSSUB with ZZINSSUB so that this */
/*        routine can legitimately be called error free. */

/* -    SPICELIB Version 3.0.0, 10-MAY-1999 (WLT) */

/*        The routine was modified so that weekday followed by a comma */
/*        is recognized as a legitimate pattern when parsing. */

/* -    SPICELIB Version 2.0.0, 16-APR-1997 (WLT) */

/*        The routine was modified so that last-chance removal of */
/*        delimiters ',', '-', and '/' are removed one at a time */
/*        (instead of all at once as in version 1.0.0) and the */
/*        resulting representation checked against */
/*        the built-in list. */

/*        In addition the set of built-in patterns was increased */
/*        from 185 to 203. See ZZTPATS for more details. */

/* -    SPICELIB Version 1.0.0, 10-AUG-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Parse a time string into a vector of components */

/* -& */

/*     SPICELIB Functions */


/*     Private Functions */


/*     Parameters */


/*     ERA */
/*     WDAY */
/*     ZONE */
/*     AMPM */
/*     SYSTEM */


/*     Local Variables. */

/*     The number of known time patterns NKNOWN comes from the include */
/*     file timepars.inc */


/*     Time Zone Variables */


/*     Standard SPICE error handling. */


/*     So far there are no modifiers to the time string. */

    *mods = FALSE_;
    *yabbrv = FALSE_;
    for (i__ = 1; i__ <= 5; ++i__) {
	s_copy(modify + (i__ - 1) * modify_len, " ", modify_len, (ftnlen)1);
    }

/*     On the first call to this routine we load the built in */
/*     representation patterns. */

    if (first) {
	if (zztpats_(&c__300, &nknown, known, meanng, (ftnlen)12, (ftnlen)12))
		 {
	    first = FALSE_;
	} else {
	    s_copy(pictur, " ", pictur_len, (ftnlen)1);
	    *succes = FALSE_;
	    s_copy(error, "There is an incompatibility between ZZTPATS and t"
		    "he room allocated for KNOWN in TPARTV.", error_len, (
		    ftnlen)87);
	    return 0;
	}
    }

/*     First step is to tokenize the string.  The new representation */
/*     is maintained in ZZTIME.  We'll get it later if we need it. */

    resolv = zztokns_(string, error, string_len, error_len);
    if (! resolv) {
	*succes = FALSE_;
	*ntvec = 0;
	s_copy(type__, " ", type_len, (ftnlen)1);
	s_copy(pictur, " ", pictur_len, (ftnlen)1);
	return 0;
    }

/*     The result of tokenizing the string will be a representation */
/*     that contains the following letters. */

/*           '        The quote character */
/*           [        The left parenthesis */
/*           ]        The right parenthesis */
/*           ,        The comma */
/*           -        The dash */
/*           .        The decimal point */
/*           /        The slash---used to separate date components. */
/*           :        The colon (used to separate time components) */
/*           N  ---   stands for one of the symbols A.M. or P.M. */
/*           O        stands for the symbol UTC+ */
/*           Z  ---   stands for a time zone such as PDT, PSD, CDT, etc. */
/*           b        stands for a block of white space */
/*           d        stands for the day of year marker (// or ::) */
/*           e  ---   stands for the era (B.C. or A.D.) */
/*           j        stands for julian date */
/*           m        stands for a month */
/*           o        stands for the symbol UTC- */
/*           s  ---   stands for a time system (UTC, TDB, TT, TDT) */
/*           t        stands the ISO date-T-time separator. */
/*           w  ---   stands for the day of the week. */
/*           i        stands for a sequence of digits */

/*     We will gradually remove many of these and replace the i, i. */
/*     and i.i with the following items */

/*           n       stands for a decimal number */
/*           Y       stands for a year */
/*           D       stands for the day in a month */
/*           y       stands for the day of the year */
/*           H       stands for hours */
/*           M       stands for minutes */
/*           S       stands for seconds */


/*     We will use the following logical functions to modify */
/*     the tokenized representation: */

/*        ZZTOKNS --- breaks the string down into a list of recognized */
/*                    tokens and stores an internal model for this */
/*                    list.  The begins and ends of the substrings */
/*                    associated with the tokenization are maintained */
/*                    inside the routine ZZTIME (which ZZTOKNS is an */
/*                    entry point to).  If some substring cannot be */
/*                    recognized, ZZTOKNS returns the value FALSE */
/*                    together with a diagnostic indicating what */
/*                    was wrong with the input string. */

/*        ZZCMBT  --- combines one or more tokens into a single token. */
/*                    this is performed only once and is done either */
/*                    scanning left to right or right to left. */
/*                    It returns TRUE if a combination is performed. */

/*        ZZREMT  --- removes all instances of a token from the tokenized */
/*                    representation.  It returns TRUE is an item */
/*                    is removed. */

/*        ZZSUBT  --- substitutes the first occurrence of a */
/*                    subpattern (scanning left to right or right to */
/*                    left) with another pattern of the same length. */
/*                    This is where we attach new meaning to the */
/*                    tokenized pattern.  It returns TRUE if a */
/*                    substitution is performed. */

/*        ZZREPT  --- is a combination of the ZZSUBT and ZZREMT */
/*                    This performs ZZSUBT on the string, but then */
/*                    remove all occurrences of the special character */
/*                    * from the tokenized list. It returns TRUE */
/*                    is a substitution is performed. */

/*        ZZNOTE  --- returns the begin and end of the first occurrence */
/*                    of some token, and then removes the token */
/*                    from the tokenized representation.  We use this */
/*                    primarily to extract modifiers from the tokenized */
/*                    string.  These should occur only once and once */
/*                    removed allow us to more easily attach meaning */
/*                    to the remaining tokens. The value of ZZNOTE */
/*                    is true if the requested item could be found, */
/*                    otherwise it is false and the begin and end */
/*                    of the requested substring are set to 0. */

/*        ZZIST   --- returns TRUE if the specified token is present */
/*                    in the tokenized substring. */

/*        ZZISPT  --- returns true is a pair of consecutive tokens */
/*                    from a list are located in the representation */
/*                    of the tokenized string.  This is used to */
/*                    locate consecutive pairs of delimiters in the */
/*                    input string. It returns TRUE if a pair of */
/*                    consecutive items is located.  Otherwise */
/*                    it returns FALSE. */

/*        ZZVALT  --- allows you to substitute a new token for any */
/*                    integer (i) that lies within a specified range */
/*                    of values.  This is primarily used to recognize */
/*                    years in the input string. */

/*        ZZGREP  --- is used to get the current representation of the */
/*                    tokenized string (with all processing resulting */
/*                    from use of the manipulation routines taken into */
/*                    account). */

/*        ZZTPATS --- is used to set up the large list of canned patterns */
/*                    that are recognized as legitimate tokenizations. */
/*                    Almost all legitimate time strings when tokenized */
/*                    will match one of these patterns. */

/*        ZZUNPCK --- uses STRING together with the current */
/*                    representation of it's tokens to return a */
/*                    time vector.  If a problem is encountered with */
/*                    the current tokens, it returns a diagnostic */
/*                    message that indicates why the string */
/*                    could not be parsed.  Note ZZUNPCK should be */
/*                    called only after all string modifiers have */
/*                    been retrieved via a call to ZZNOTE (or by */
/*                    manually removing them). */

/*     Next Step is to combine some tokens so that we won't run */
/*     into problems later on.  We may introduce some new components */
/*     in the process. */

    l2r = TRUE_;
    r2l = ! l2r;
    if (zzcmbt_("Oi", "z", &l2r, (ftnlen)2, (ftnlen)1)) {
	resolv = zzcmbt_("z:i", "Z", &l2r, (ftnlen)3, (ftnlen)1);
	resolv = zzsubt_("z", "Z", &l2r, (ftnlen)1, (ftnlen)1);
    }
    if (zzcmbt_("oi", "z", &l2r, (ftnlen)2, (ftnlen)1)) {
	resolv = zzcmbt_("z:i", "Z", &l2r, (ftnlen)3, (ftnlen)1);
	resolv = zzsubt_("z", "Z", &l2r, (ftnlen)1, (ftnlen)1);
    }

/*     Next we resolve any months, or weekdays that are followed */
/*     by periods. */

    resolv = zzrept_("m.", "m*", &l2r, (ftnlen)2, (ftnlen)2);
    resolv = zzrept_("w.", "w*", &l2r, (ftnlen)2, (ftnlen)2);
    resolv = zzrept_("w,", "w*", &l2r, (ftnlen)2, (ftnlen)2);

/*     Now convert the right most integer-decimal-point pair to the */
/*     number representation. */

    if (zzcmbt_("i.i", "n", &r2l, (ftnlen)3, (ftnlen)1)) {

/*        We aren't going to do anything here.  We are simply */
/*        using the IF-THEN...ELSE IF ... ENDIF  to make sure */
/*        we only replace one decimal place. */

    } else if (zzcmbt_("i.", "n", &r2l, (ftnlen)2, (ftnlen)1)) {

/*        Same as the previous comment. */

    }

/*     Remove any white space from the tokenization. */

    resolv = zzremt_("b", (ftnlen)1);

/*     User Custom Formats (this still needs a modicum of work). */
/*     ---------------------------------------------------------------- */
/*     ================================================================ */


/*     RESOLV = ZZGREP ( REP ) */
/*     USE    = ISRCHC ( REP, NCUSTM, CUSTOM ) */

/*     IF ( USE .GT. 0 ) THEN */
/*        RESOLV = ZZREPT ( CUSTM(USE), CMEANS(USE), L2R ) */
/*     ELSE */
/*        RESOLV =  .FALSE. */
/*     END IF */

/*     IF ( RESOLV ) THEN */

/*        SUCCES = ZZUNPCK ( STRING, YABBRV, ... */
/*                           TVEC,   NTVEC, TYPE, PICTUR, ERROR ) */
/*        ERROR  = ' ' */

/*        RETURN */
/*     END IF */



/*     Julian Date */
/*     ---------------------------------------------------------------- */
/*     ================================================================ */

    if (zzist_("j", (ftnlen)1)) {

/*        This is some form of Julian Date. Handle this case */
/*        right here and return. */

	resolv = zzrept_("[s]", "*s*", &l2r, (ftnlen)3, (ftnlen)3);
	*mods = *mods || zznote_("s", &b, &e, (ftnlen)1);
	if (*mods) {
	    ucase_(string + (b - 1), modify + (modify_len << 2), e - (b - 1), 
		    modify_len);
	}
	resolv = zzrept_("[j]", "*j*", &l2r, (ftnlen)3, (ftnlen)3);
	resolv = zzremt_("j", (ftnlen)1);
	if (! zzist_("n", (ftnlen)1)) {
	    resolv = zzsubt_("i", "n", &l2r, (ftnlen)1, (ftnlen)1);
	}
	resolv = zzcmbt_("-n", "n", &l2r, (ftnlen)2, (ftnlen)1);
	resolv = zzsubt_("n", "J", &l2r, (ftnlen)1, (ftnlen)1);

/*        We let ZZUNPK handle the parsing or diagnosis of any problems. */

	*succes = zzunpck_(string, yabbrv, tvec, ntvec, type__, pictur, error,
		 string_len, type_len, pictur_len, error_len);
	if (i_indx(pictur, "JULIAND.", pictur_len, (ftnlen)8) > 0) {
	    suffix_("::RND", &c__1, pictur, (ftnlen)5, pictur_len);
	}
	if (s_cmp(modify + (modify_len << 2), " ", modify_len, (ftnlen)1) != 
		0) {
	    suffix_("::", &c__1, pictur, (ftnlen)2, pictur_len);
	    suffix_(modify + (modify_len << 2), &c__0, pictur, modify_len, 
		    pictur_len);
	}
	return 0;
    }

/*     Calendar Date Formats. */
/*     ---------------------------------------------------------------- */
/*     ================================================================ */

/*     Replace any integers greater than 1000 by Y. */

    b = 1000;
    e = intmax_();
    resolv = zzvalt_(string, &b, &e, "Y", string_len, (ftnlen)1);

/*     If the ISO time delimiter 't' is present we don't perform */
/*     any further simplifications. */

    if (zzist_("t", (ftnlen)1)) {
	resolv = zzgrep_(rep, (ftnlen)12);
	use = bsrchc_(rep, &nknown, known, (ftnlen)12, (ftnlen)12);
	if (use != 0) {
	    resolv = zzrept_(known + ((i__1 = use - 1) < 300 && 0 <= i__1 ? 
		    i__1 : s_rnge("known", i__1, "tpartv_", (ftnlen)1036)) * 
		    12, meanng + ((i__2 = use - 1) < 300 && 0 <= i__2 ? i__2 :
		     s_rnge("meanng", i__2, "tpartv_", (ftnlen)1036)) * 12, &
		    l2r, (ftnlen)12, (ftnlen)12);
	    *succes = zzunpck_(string, yabbrv, tvec, ntvec, type__, pictur, 
		    error, string_len, type_len, pictur_len, error_len);

/*           If you tag an ISO as A.D., other logic in the Time */
/*           subsystem will not intepret the two digit year value as a */
/*           two digit abbreviation. */

/*            MODIFY(ERA) = 'A.D.' */

	    if (i_indx(pictur, ".#", pictur_len, (ftnlen)2) != 0) {
		suffix_("::RND", &c__1, pictur, (ftnlen)5, pictur_len);
	    }
	    if (s_cmp(modify + (modify_len << 1), " ", modify_len, (ftnlen)1) 
		    != 0) {
		suffix_("::", &c__1, pictur, (ftnlen)2, pictur_len);
		suffix_(modify + (modify_len << 1), &c__0, pictur, modify_len,
			 pictur_len);
	    }
	    if (s_cmp(modify + (modify_len << 2), " ", modify_len, (ftnlen)1) 
		    != 0) {
		suffix_("::", &c__1, pictur, (ftnlen)2, pictur_len);
		suffix_(modify + (modify_len << 2), &c__0, pictur, modify_len,
			 pictur_len);
	    }
	} else {
	    *succes = FALSE_;
	    *ntvec = 0;
	    *mods = FALSE_;
	    s_copy(type__, " ", type_len, (ftnlen)1);
	    s_copy(pictur, " ", pictur_len, (ftnlen)1);
	    s_copy(error, "The input string uses the ISO  \"T\" date/time de"
		    "limiter but does not match any of the accepted ISO forma"
		    "ts. ", error_len, (ftnlen)107);
	}
	return 0;
    }

/*     If we reach this point, either we didn't have any custom */
/*     formats supplied or we didn't match any of them. */
/*     Resolve any abbreviated years.  We've already set integers */
/*     that are 1000 or greater to 'Y'  Only 1 or 2 digit integers */
/*     can be year abbreviations.  We replace the 3 digit integers */
/*     with I temporarily; locate any abbreviated years; reset all */
/*     the 3-digit back to 'i'.  (Note 3-digit means value between */
/*     100 and 999.  003 is not regarded as a 3 digit number). */

    b = 100;
    e = 1000;
    resolv = zzvalt_(string, &b, &e, "I", string_len, (ftnlen)1);
    *yabbrv = zzrept_("'i", "*Y", &l2r, (ftnlen)2, (ftnlen)2);
    while(zzsubt_("I", "i", &l2r, (ftnlen)1, (ftnlen)1)) {
	++b;
    }

/*     Resolve the system, and other text components. */

    resolv = zzrept_("[e]", "*e*", &l2r, (ftnlen)3, (ftnlen)3);
    resolv = zzrept_("[w]", "*w*", &l2r, (ftnlen)3, (ftnlen)3);
    resolv = zzrept_("[N]", "*N*", &l2r, (ftnlen)3, (ftnlen)3);
    resolv = zzrept_("[Z]", "*Z*", &l2r, (ftnlen)3, (ftnlen)3);
    resolv = zzrept_("[s]", "*s*", &l2r, (ftnlen)3, (ftnlen)3);
    resolv = zzsubt_("ie", "Ye", &l2r, (ftnlen)2, (ftnlen)2);

/*     Note the positions of ERA, WEEKDAY, TIME-ZONE, AMPM marker */
/*     and time SYSTEM. */

    havera = zznote_("e", begs, ends, (ftnlen)1);
    havwdy = zznote_("w", &begs[1], &ends[1], (ftnlen)1);
    havzon = zznote_("Z", &begs[2], &ends[2], (ftnlen)1);
    havapm = zznote_("N", &begs[3], &ends[3], (ftnlen)1);
    havsys = zznote_("s", &begs[4], &ends[4], (ftnlen)1);
    *mods = havera || havwdy || havzon || havapm || havsys;
    if (*mods) {
	for (i__ = 1; i__ <= 5; ++i__) {
	    if (begs[(i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("begs",
		     i__1, "tpartv_", (ftnlen)1127)] != 0) {
		i__1 = begs[(i__2 = i__ - 1) < 5 && 0 <= i__2 ? i__2 : s_rnge(
			"begs", i__2, "tpartv_", (ftnlen)1128)] - 1;
		ucase_(string + i__1, modify + (i__ - 1) * modify_len, ends[(
			i__3 = i__ - 1) < 5 && 0 <= i__3 ? i__3 : s_rnge(
			"ends", i__3, "tpartv_", (ftnlen)1128)] - i__1, 
			modify_len);
	    }
	}
	if (havera) {
	    if (*(unsigned char *)&modify[0] == 'A') {
		s_copy(modify, "A.D.", modify_len, (ftnlen)4);
	    } else {
		s_copy(modify, "B.C.", modify_len, (ftnlen)4);
	    }
	}
	if (havapm) {
	    if (*(unsigned char *)&modify[modify_len * 3] == 'A') {
		s_copy(modify + modify_len * 3, "A.M.", modify_len, (ftnlen)4)
			;
	    } else {
		s_copy(modify + modify_len * 3, "P.M.", modify_len, (ftnlen)4)
			;
	    }
	}
	s_copy(modify + (modify_len + 3), " ", modify_len - 3, (ftnlen)1);
	if (havzon) {
	    mapto = isrchc_(modify + (modify_len << 1), &c__8, zones, 
		    modify_len, (ftnlen)3);
	    if (mapto != 0) {
		s_copy(modify + (modify_len << 1), offset + ((i__1 = mapto - 
			1) < 8 && 0 <= i__1 ? i__1 : s_rnge("offset", i__1, 
			"tpartv_", (ftnlen)1155)) * 6, modify_len, (ftnlen)6);
	    }
	}
    }

/*     Try our built in formats without any further substitution. */

    resolv = zzgrep_(rep, (ftnlen)12);
    use = bsrchc_(rep, &nknown, known, (ftnlen)12, (ftnlen)12);
    if (use > 0) {
	resolv = zzrept_(known + ((i__1 = use - 1) < 300 && 0 <= i__1 ? i__1 :
		 s_rnge("known", i__1, "tpartv_", (ftnlen)1170)) * 12, meanng 
		+ ((i__2 = use - 1) < 300 && 0 <= i__2 ? i__2 : s_rnge("mean"
		"ng", i__2, "tpartv_", (ftnlen)1170)) * 12, &l2r, (ftnlen)12, (
		ftnlen)12);
	*succes = zzunpck_(string, yabbrv, tvec, ntvec, type__, pictur, error,
		 string_len, type_len, pictur_len, error_len);
	if (i_indx(pictur, ".#", pictur_len, (ftnlen)2) != 0) {
	    suffix_("::RND", &c__1, pictur, (ftnlen)5, pictur_len);
	}
	if (s_cmp(modify + (modify_len << 1), " ", modify_len, (ftnlen)1) != 
		0) {
	    suffix_("::", &c__1, pictur, (ftnlen)2, pictur_len);
	    suffix_(modify + (modify_len << 1), &c__0, pictur, modify_len, 
		    pictur_len);
	}
	if (s_cmp(modify + (modify_len << 2), " ", modify_len, (ftnlen)1) != 
		0) {
	    suffix_("::", &c__1, pictur, (ftnlen)2, pictur_len);
	    suffix_(modify + (modify_len << 2), &c__0, pictur, modify_len, 
		    pictur_len);
	}
	return 0;
    }

/*     Make sure we don't have a pair of successive delimiters */
/*     or a delimiter at either end of the input string. */

    if (zzispt_(",/-:d.", &from, &to, (ftnlen)6)) {
	*succes = FALSE_;
	*ntvec = 0;
	s_copy(type__, " ", type_len, (ftnlen)1);
	s_copy(error, string, error_len, string_len);
	i__1 = to + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &from, error, error_len, (ftnlen)1, error_len);
	prefix_("There are two successive delimiters <#> in the input string"
		".  This is an ambiguous input. ' ", &c__0, error, (ftnlen)92, 
		error_len);
	repmc_(error, "#", string + (from - 1), error, error_len, (ftnlen)1, 
		to - (from - 1), error_len);
	suffix_("'", &c__0, error, (ftnlen)1, error_len);
	s_copy(pictur, " ", pictur_len, (ftnlen)1);
	return 0;
    }

/*     A delimiter hanging at either end of the string shall be */
/*     regarded as an error. */

    resolv = zzgrep_(rep, (ftnlen)12);
    r__ = rtrim_(rep, (ftnlen)12);
    if (i_indx(",/-:.", rep, (ftnlen)5, (ftnlen)1) > 0) {
	resolv = zzsubt_(rep, "Q", &l2r, (ftnlen)1, (ftnlen)1);
	resolv = FALSE_;
    } else if (i_indx(",/-:.", rep + (r__ - 1), (ftnlen)5, (ftnlen)1) > 0) {
	resolv = zzsubt_(rep + (r__ - 1), "Q", &l2r, (ftnlen)1, (ftnlen)1);
	resolv = FALSE_;
    }
    if (! resolv) {
	resolv = zznote_("Q", &from, &to, (ftnlen)1);
	s_copy(error, string, error_len, string_len);
	i__1 = to + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &from, error, error_len, (ftnlen)1, error_len);
	prefix_("An unexpected delimiter ('#') was encountered in the input "
		"string. ' ", &c__0, error, (ftnlen)69, error_len);
	suffix_("'", &c__0, error, (ftnlen)1, error_len);
	repmc_(error, "#", string + (from - 1), error, error_len, (ftnlen)1, 
		to - (from - 1), error_len);
	s_copy(pictur, " ", pictur_len, (ftnlen)1);
	*succes = FALSE_;
	return 0;
    }

/*     We probably made it unscathed through the check above. */
/*     Remove delimiters ',', '/', and '-' and retry the built-in */
/*     patterns. */

    *(unsigned char *)&delim[0] = ',';
    *(unsigned char *)&delim[1] = '-';
    *(unsigned char *)&delim[2] = '/';
    for (i__ = 1; i__ <= 3; ++i__) {
	resolv = zzremt_(delim + ((i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("delim", i__1, "tpartv_", (ftnlen)1261)), (ftnlen)1);
	resolv = zzgrep_(rep, (ftnlen)12);
	use = bsrchc_(rep, &nknown, known, (ftnlen)12, (ftnlen)12);
	if (use > 0) {
	    resolv = zzrept_(known + ((i__1 = use - 1) < 300 && 0 <= i__1 ? 
		    i__1 : s_rnge("known", i__1, "tpartv_", (ftnlen)1268)) * 
		    12, meanng + ((i__2 = use - 1) < 300 && 0 <= i__2 ? i__2 :
		     s_rnge("meanng", i__2, "tpartv_", (ftnlen)1268)) * 12, &
		    l2r, (ftnlen)12, (ftnlen)12);
	    *succes = zzunpck_(string, yabbrv, tvec, ntvec, type__, pictur, 
		    error, string_len, type_len, pictur_len, error_len);
	    if (i_indx(pictur, ".#", pictur_len, (ftnlen)2) != 0) {
		suffix_("::RND", &c__1, pictur, (ftnlen)5, pictur_len);
	    }
	    if (s_cmp(modify + (modify_len << 1), " ", modify_len, (ftnlen)1) 
		    != 0) {
		suffix_("::", &c__1, pictur, (ftnlen)2, pictur_len);
		suffix_(modify + (modify_len << 1), &c__0, pictur, modify_len,
			 pictur_len);
	    }
	    if (s_cmp(modify + (modify_len << 2), " ", modify_len, (ftnlen)1) 
		    != 0) {
		suffix_("::", &c__1, pictur, (ftnlen)2, pictur_len);
		suffix_(modify + (modify_len << 2), &c__0, pictur, modify_len,
			 pictur_len);
	    }
	    return 0;
	}
    }

/*     If we make it to this point, we must have a pretty funky */
/*     time string.  There are some obvious incompatibilities. We */
/*     check them now */

    if (zznote_("e", &b, &e, (ftnlen)1)) {
    } else if (zznote_("s", &b, &e, (ftnlen)1)) {
    } else if (zznote_("Z", &b, &e, (ftnlen)1)) {
    } else if (zznote_("w", &b, &e, (ftnlen)1)) {
    } else if (zznote_("N", &b, &e, (ftnlen)1)) {
    }

/*     If B is non-zero the item in question is a duplicate */
/*     modifier. */

    if (b > 0) {
	*succes = FALSE_;
	*ntvec = 0;
	s_copy(type__, " ", type_len, (ftnlen)1);
	s_copy(error, string, error_len, string_len);
	i__1 = e + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &b, error, error_len, (ftnlen)1, error_len);
	prefix_("The substring \"#\" is a duplicate modifier of the input st"
		"ring: ' ", &c__0, error, (ftnlen)65, error_len);
	suffix_("'", &c__0, error, (ftnlen)1, error_len);
	repmc_(error, "#", string + (b - 1), error, error_len, (ftnlen)1, e - 
		(b - 1), error_len);
	s_copy(pictur, " ", pictur_len, (ftnlen)1);
	return 0;
    }

/*     Look for unresolved markers */

    if (zznote_("[", &b, &e, (ftnlen)1)) {
    } else if (zznote_("]", &b, &e, (ftnlen)1)) {
    } else if (zznote_("O", &b, &e, (ftnlen)1)) {
    } else if (zznote_("o", &b, &e, (ftnlen)1)) {
    } else if (zznote_("z", &b, &e, (ftnlen)1)) {
    }
    if (b > 0) {
	*succes = FALSE_;
	*ntvec = 0;
	s_copy(type__, " ", type_len, (ftnlen)1);
	s_copy(error, string, error_len, string_len);
	i__1 = e + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &b, error, error_len, (ftnlen)1, error_len);
	prefix_("The substring \"#\" could not be resolved in the input stri"
		"ng: ' ", &c__0, error, (ftnlen)63, error_len);
	suffix_("'", &c__0, error, (ftnlen)1, error_len);
	repmc_(error, "#", string + (b - 1), error, error_len, (ftnlen)1, e - 
		(b - 1), error_len);
	s_copy(pictur, " ", pictur_len, (ftnlen)1);
	return 0;
    }
    if (zzist_("m", (ftnlen)1) && zzist_("d", (ftnlen)1)) {
	*succes = FALSE_;
	*ntvec = 0;
	s_copy(type__, " ", type_len, (ftnlen)1);
	s_copy(error, string, error_len, string_len);
	resolv = zznote_("m", &b1, &e1, (ftnlen)1);
	resolv = zznote_("d", &b2, &e2, (ftnlen)1);
	b = max(b1,b2);
	e = max(e1,e2);
	i__1 = e + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &b, error, error_len, (ftnlen)1, error_len);
	b = min(b1,b2);
	e = min(e1,e2);
	i__1 = e + 1;
	zzinssub_(error, ">", &i__1, error, error_len, (ftnlen)1, error_len);
	zzinssub_(error, "<", &b, error, error_len, (ftnlen)1, error_len);
	prefix_("Both a month \"#\" and day of year delimiter \"#\" appear i"
		"n the input string: ' ", &c__0, error, (ftnlen)77, error_len);
	suffix_("'", &c__0, error, (ftnlen)1, error_len);
	repmc_(error, "#", string + (b1 - 1), error, error_len, (ftnlen)1, e1 
		- (b1 - 1), error_len);
	repmc_(error, "#", string + (b2 - 1), error, error_len, (ftnlen)1, e2 
		- (b2 - 1), error_len);
	s_copy(pictur, " ", pictur_len, (ftnlen)1);
	return 0;
    }

/*     Make the remaining obvious substitutions for hours, */
/*     minutes, and seconds */

    if (zzrept_("i:i:i:n", "D*H*M*S", &r2l, (ftnlen)7, (ftnlen)7)) {
    } else if (zzrept_("i:i:i:i", "D*H*M*S", &r2l, (ftnlen)7, (ftnlen)7)) {
    } else if (zzrept_("i:i:n", "H*M*S", &r2l, (ftnlen)5, (ftnlen)5)) {
    } else if (zzrept_("i:i:i", "H*M*S", &r2l, (ftnlen)5, (ftnlen)5)) {
    } else if (zzrept_("i:n", "H*M", &r2l, (ftnlen)3, (ftnlen)3)) {
    } else if (zzrept_("i:i", "H*M", &r2l, (ftnlen)3, (ftnlen)3)) {
    }
    resolv = zzremt_(":", (ftnlen)1);

/*     Handle the obvious substitutions of an integer next to */
/*     a Month. */

    if (zzsubt_("<miiH", "mDY", &l2r, (ftnlen)5, (ftnlen)3)) {
    } else if (zzsubt_("<mi", "mD", &l2r, (ftnlen)3, (ftnlen)2)) {
    } else if (zzsubt_("Siim>", "SYDm", &l2r, (ftnlen)5, (ftnlen)4)) {
    } else if (zzsubt_("im>", "Dm", &l2r, (ftnlen)3, (ftnlen)2)) {
    } else if (zzsubt_("miY>", "mDY", &l2r, (ftnlen)4, (ftnlen)3)) {
    } else if (zzsubt_("Ymi", "YmD", &l2r, (ftnlen)3, (ftnlen)3)) {
    } else if (zzsubt_("Smi", "SmD", &l2r, (ftnlen)3, (ftnlen)3)) {
    } else if (zzsubt_("Mmi", "MmD", &l2r, (ftnlen)3, (ftnlen)3)) {
    } else if (zzsubt_("imY", "DmY", &l2r, (ftnlen)3, (ftnlen)3)) {
    } else if (zzsubt_("imH", "DmH", &l2r, (ftnlen)3, (ftnlen)3)) {
    } else if (zzrept_("Yid", "Yy*", &l2r, (ftnlen)3, (ftnlen)3)) {
    } else if (zzrept_("iYd", "yY*", &l2r, (ftnlen)3, (ftnlen)3)) {
    } else if (zzrept_("Ydi", "Y*y", &l2r, (ftnlen)3, (ftnlen)3)) {
    }

/*     That's it we let ZZUNPCK handle the problem of diagnosing */
/*     or decoding the current representation. */

    *succes = zzunpck_(string, yabbrv, tvec, ntvec, type__, pictur, error, 
	    string_len, type_len, pictur_len, error_len);
    if (s_cmp(pictur, " ", pictur_len, (ftnlen)1) != 0) {
	if (i_indx(pictur, ".#", pictur_len, (ftnlen)2) != 0) {
	    suffix_("::RND", &c__1, pictur, (ftnlen)5, pictur_len);
	}
	if (s_cmp(modify + (modify_len << 1), " ", modify_len, (ftnlen)1) != 
		0) {
	    suffix_("::", &c__1, pictur, (ftnlen)2, pictur_len);
	    suffix_(modify + (modify_len << 1), &c__0, pictur, modify_len, 
		    pictur_len);
	}
	if (s_cmp(modify + (modify_len << 2), " ", modify_len, (ftnlen)1) != 
		0) {
	    suffix_("::", &c__1, pictur, (ftnlen)2, pictur_len);
	    suffix_(modify + (modify_len << 2), &c__0, pictur, modify_len, 
		    pictur_len);
	}
    }
    return 0;
} /* tpartv_ */

