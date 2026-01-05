/* timout.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;
static integer c__0 = 0;
static integer c__1 = 1;
static integer c__14 = 14;
static doublereal c_b279 = 0.;
static doublereal c_b280 = 1.;
static doublereal c_b343 = 100.;

/* $Procedure TIMOUT ( Time Output ) */
/* Subroutine */ int timout_(doublereal *et, char *pictur, char *output, 
	ftnlen pictur_len, ftnlen output_len)
{
    /* Initialized data */

    static char months[9*12] = "January  " "February " "March    " "April    "
	     "May      " "June     " "July     " "August   " "September" 
	    "October  " "November " "December ";
    static integer mlen[12] = { 7,8,5,5,3,4,4,6,9,7,8,8 };
    static char wkdays[9*7] = "Sunday   " "Monday   " "Tuesday  " "Wednesday" 
	    "Thursday " "Friday   " "Saturday ";
    static integer wklen[7] = { 6,6,7,9,8,6,8 };
    static logical first = TRUE_;
    static doublereal power[15] = { 1.,10.,100.,1e3,1e4,1e5,1e6,1e7,1e8,1e9,
	    1e10,1e11,1e12,1e13,1e14 };

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;
    doublereal d__1, d__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    double d_int(doublereal *);
    integer i_dnnt(doublereal *);

    /* Local variables */
    static doublereal frac, hoff;
    extern /* Subroutine */ int scan_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    ftnlen, ftnlen);
    static logical have[52];
    static doublereal moff;
    static integer jday, gday;
    static doublereal incr;
    static integer last, dump[11];
    static doublereal myet;
    static integer part, type__;
    static doublereal tvec[8];
    static integer jdoy, gdoy, indx;
    static char tsys[16];
    static integer b, e, i__, j;
    extern /* Subroutine */ int lcase_(char *, char *, ftnlen, ftnlen);
    static doublereal x, delta;
    static logical doera;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen);
    static integer appnd, ident[100], class__[44], gyear;
    static doublereal tempd;
    static integer jyear;
    static doublereal value;
    static char marks[8*43];
    extern /* Subroutine */ int dpfmt_(doublereal *, char *, char *, ftnlen, 
	    ftnlen);
    static integer width, ndump;
    static doublereal ntvec[8];
    extern integer rtrim_(char *, ftnlen);
    static integer start;
    static doublereal ptvec[8];
    static char mymon[9];
    static integer mylen;
    static char intyp[16], mywkd[9];
    static integer pntrs[100];
    static char ywfmt[8];
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    static char mystr[256];
    static integer wktyp;
    static logical go2jul;
    extern /* Subroutine */ int gr2jul_(integer *, integer *, integer *, 
	    integer *), jul2gr_(integer *, integer *, integer *, integer *);
    static integer id[52];
    static logical ok;
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static logical making;
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *);
    extern /* Subroutine */ int timdef_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);
    extern integer brckti_(integer *, integer *, integer *), isrchi_(integer *
	    , integer *, integer *);
    static integer length[52];
    static doublereal factor;
    static integer mrklen[43];
    static doublereal intmed;
    static integer nmarks;
    static char orignl[32*52];
    static integer caltyp, timfmt;
    static doublereal values[52];
    static integer compnt[16]	/* was [8][2] */;
    static char ymdfmt[8];
    extern doublereal unitim_(doublereal *, char *, char *, ftnlen, ftnlen);
    static char string[256], bastyp[16];
    static logical dozone;
    static integer stopat, trncat, ntokns;
    static doublereal timpad;
    extern logical return_(void);
    static char substr[256];
    static integer jmonth, gmonth, timtyp, montyp;
    static logical unknwn, pumpup;
    static integer numtyp;
    static logical vanish;
    extern /* Subroutine */ int scanpr_(integer *, char *, integer *, integer 
	    *, ftnlen), prefix_(char *, integer *, char *, ftnlen, ftnlen), 
	    scanrj_(integer *, integer *, integer *, integer *, integer *, 
	    integer *), ttrans_(char *, char *, doublereal *, ftnlen, ftnlen),
	     rmaind_(doublereal *, doublereal *, doublereal *, doublereal *), 
	    chkout_(char *, ftnlen);
    extern doublereal j2000_(void);
    static integer beg[100];
    static char cal[16];
    static doublereal pad[52];
    static integer end[100];
    extern doublereal j1950_(void), spd_(void);
    static char fmt[32], zon[32];
    extern /* Subroutine */ int zzutcpm_(char *, integer *, doublereal *, 
	    doublereal *, integer *, logical *, ftnlen);

/* $ Abstract */

/*     Convert an input epoch represented in TDB seconds past the TDB */
/*     epoch of J2000 to a character string formatted to the */
/*     specifications of a user's format picture. */

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
/*     ET         I   Epoch in seconds past the ephemeris epoch J2000. */
/*     PICTUR     I   A format specification for the output string. */
/*     OUTPUT     O   A string representation of the input epoch. */

/* $ Detailed_Input */

/*     ET       is a double precision representation of time in seconds */
/*              past the ephemeris epoch J2000. */

/*     PICTUR   is a string that specifies how the output should be */
/*              presented. The string is made up of various markers */
/*              that stand for various components associated with */
/*              a time. */

/*              There are five types of markers that may appear in a */
/*              format picture. These are String Markers, Numeric */
/*              Markers, Meta markers, Modifier Markers and Literal */
/*              Markers. */

/*              The PICTUR string is examined and the various markers */
/*              are identified. The output time string is constructed */
/*              by replacing each of the identified markers with */
/*              an appropriate time component. */

/*              The various markers and their meanings are discussed */
/*              in the $Particulars section below. */

/*              Note that leading and trailing blanks in PICTUR are */
/*              ignored. */

/* $ Detailed_Output */

/*     OUTPUT   is a time string equivalent to the input epoch ET, */
/*              matching the format specified by PICTUR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  A leapseconds kernel must be loaded via the routine FURNSH */
/*         before calling this routine. If a leapsecond kernel has not */
/*         been loaded, an error is signaled by a routine in the call */
/*         tree of this routine. */

/*     2)  If PICTUR contains the numeric marker 'YYYY' and the */
/*         magnitude of year is too large to be displayed as a four-digit */
/*         integer, TIMOUT will replace it by '****'. */

/*     3)  If the requested precision is higher than 12 decimal places, */
/*         TIMOUT will truncate the decimal part down to 12, and OUTPUT */
/*         will have all the remaining digits in the decimal part set to */
/*         zero. */

/*     4)  Double colon (::), when is not part of one of the supported */
/*         markers, has no effect and will be presented as is on the */
/*         output string. */

/* $ Files */

/*     A leapseconds kernel must be "loaded" via the routine FURNSH */
/*     prior to calling TIMOUT. */

/* $ Particulars */

/*     A format picture is simply a string of letters that lets */
/*     TIMOUT know where various components of a time representation */
/*     should be placed during creation of the time string. */
/*     Here's an example of such a picture: */

/*        MON DD,YYYY  HR:MN:SC.#### (TDB) ::TDB */

/*     Here is a sample of times that would be created by using this */
/*     format. */

/*        JAN 12,1992  12:28:18.2772 (TDB) */
/*        FEB 13,1994  23:18:25.2882 (TDB) */
/*        AUG 21,1995  00:02:00.1881 (TDB) */

/*     As you can see from the samples above, the format picture */
/*     specifies that every time string created should begin with a */
/*     three-letter abbreviation for the month, followed by a space and */
/*     the day of the month. The day of month is followed immediately by */
/*     a comma and the year. The year component is followed by two */
/*     spaces. The next outputs are hours represented as a two digit */
/*     integer, a colon, minutes represented as a two digit integer, */
/*     another colon, and seconds truncated to 4 decimal places and */
/*     having a two digit integer part (rounding can be commanded; see */
/*     the discussion of truncation and rounding below). This is */
/*     followed by a space and the string '(TDB)'. The special marker */
/*     '::TDB' in the time picture is an ``invisible'' marker. It is */
/*     used to specify the time system that should be used in creating */
/*     the time string (in this case Barycentric Dynamical Time). */

/*     TIMOUT does not recognize all of the parts of the time format */
/*     picture in the example above. The list of recognized parts and */
/*     unrecognized parts is shown in the table below. */

/*       Recognized       Unrecognized */
/*       ----------       ------------ */
/*       'MON'            ' ' */
/*       'DD'             ',' */
/*       'YYYY'           '  ' */
/*       'HR'             ':' */
/*       'MN'             '(TDB)' */
/*       'SC' */
/*       '.####' */
/*       '::TDB' */

/*     The unrecognized parts are called literal markers. They are */
/*     copied exactly as they appear in PICTUR into the output string. */
/*     The recognized parts of the picture are replaced by a */
/*     component of time or, as in the case of '::TDB' are used */
/*     as instructions about the overall properties of the time */
/*     string. */

/*     The full list of recognized markers, their classification */
/*     and meaning are given below. */

/*     MARKER       CLASS     MEANING */
/*     -----------  --------  ----------------------------------------- */
/*     '.##...'     modifier  represents a numeric component that */
/*                            immediately precedes this in a decimal */
/*                            format. Number of decimal places */
/*                            equals the number of '#' characters */
/*     '::GCAL'     meta      dates are reported in Gregorian calendar */
/*     '::JCAL'     meta      dates are reported in Julian calendar */
/*     '::MCAL'     meta      dates after 15 October, 1582 are reported */
/*                            in Gregorian calendar; before that */
/*                            dates are reported in Julian calendar */

/*     '::RND'      meta      round output to places specified by */
/*                            least significant component */

/*     '::TDB'      meta      all components should be TDB */

/*     '::TDT'      meta      all components should be TT (TDT) */

/*     '::TT'       meta      all components should be TT (TDT) */

/*     '::TRNC'     meta      truncate all output components (default) */
/*     '::UTC'      meta      all components should be UTC (default) */
/*     '::UTC+h:m'  meta      all components in UTC offset by +h (hours) */
/*                            and +m (minutes) so as to allow time zones. */
/*     '::UTC-h:m'  meta      all components in UTC offset by -h (hours) */
/*                            and -m (minutes) so as to allow time zones. */
/*     'AMPM'       string    String (either 'A.M.' or 'P.M.') */
/*                            indicating whether hours are before */
/*                            or after noon. */
/*     'ampm'       string    String (either 'a.m.' or 'p.m.') */
/*                            indicating whether hours are before */
/*                            or after noon. */
/*     'AP'         numeric   AM/PM equivalents of the hour component */
/*                            of a time. */
/*     'DD'         numeric   Day of month */
/*     'DOY'        numeric   Day of year */
/*     'ERA'        string    String (either 'B.C.' or 'A.D.') giving */
/*                            era associated with an epoch. */
/*     '?ERA?'      string    String: either ' B.C. ' or ' A.D. ' if the */
/*                            year is before 1000 A.D. otherwise a */
/*                            blank: ' '. */
/*     'era'        string    String (either 'b.c.' or 'a.d.') giving */
/*                            era associated with an epoch. */
/*     '?era?'      string    String: either ' b.c. ' or ' a.d. ' if the */
/*                            year is before 1000 A.D. otherwise a */
/*                            blank: ' '. */
/*     'HR'         numeric   hour component of time */
/*     'JULIAND'    numeric   Julian date component of time */
/*     'MM'         numeric   numeric representation of month component */
/*     'MN'         numeric   minute component of time */
/*     'MON'        string    upper case three letter abbreviation for */
/*                            month */
/*     'Mon'        string    capitalized three letter abbreviation for */
/*                            month */
/*     'mon'        string    lower case three letter abbreviation for */
/*                            month */
/*     'MONTH'      string    upper case full name of month */
/*     'Month'      string    capitalized full name of month */
/*     'month'      string    lower case full name of month */
/*     'SC'         numeric   seconds component of time */
/*     'SP1950'     numeric   seconds past 1950 component of time */
/*     'SP2000'     numeric   seconds past 2000 component of time */
/*     'YR'         numeric   last two digits of year component of time */
/*     'YYYY'       numeric   year component of time */
/*     'WEEKDAY'    string    upper case day of week */
/*     'Weekday'    string    capitalized day of week */
/*     'weekday'    string    lower case day of week */
/*     'WKD'        string    upper case three letter abbreviation for */
/*                            day of week. */
/*     'Wkd'        string    capitalized three letter abbreviation for */
/*                            day of week. */
/*     'wkd'        string    lower case three letter abbreviation for */
/*                            day of week. */

/*     String Markers */

/*        String markers are portions of the format picture that will */
/*        be replaced with a character string that represents the */
/*        corresponding component of a time. */

/*     Numeric Markers */

/*        Numeric markers are portions of the format picture that will */
/*        be replaced with a decimal string that represents the */
/*        corresponding component of a time. */

/*     Meta Markers */

/*        Meta markers (listed under the class ``meta'' in the */
/*        table above) are used to indicate "global" properties of */
/*        your time string. You may specify time scale and how */
/*        rounding should be performed on the components of time */
/*        in your output string. Meta markers may be placed anywhere */
/*        in your format picture. They do not contribute to placement */
/*        of characters in output time strings. Also there are no */
/*        restrictions on how many meta markers you may place in */
/*        the format picture. However, if you supply conflicting */
/*        `meta' markers (for example '::TDT' and '::TDB') in your */
/*        picture the first marker listed (in left to right order) */
/*        overrules the conflicting marker that appears later in */
/*        the picture. */

/*     Default Meta Markers */

/*        If you do not specify a time system, calendar, or time */
/*        zone through the use of a Meta Marker, TIMOUT uses the */
/*        values returned by the SPICE routine TIMDEF. The default */
/*        time system, calendar returned by TIMDEF are UTC and */
/*        the Gregorian calendar. The default time zone returned */
/*        by TIMDEF is a blank indicating that no time zone offset */
/*        should be used. */

/*        See the header for the routine TIMDEF for a more complete */
/*        discussion of setting and retrieving default values. */

/*     Modifier Markers */

/*        The numeric markers listed in the table above stand */
/*        for integers unless they are modified through use of a */
/*        modifier marker. The strings */

/*           .# */
/*           .## */
/*           .### */
/*           .#### */

/*        are used to this end. When a numeric marker is followed */
/*        immediately by one of these modifiers, the corresponding time */
/*        component will be written with the number of decimal places */
/*        indicated by the number of successive occurrences of the */
/*        character '#'. Any numeric token may be modified. */

/*     Rounding vs. Truncation */

/*        The meta markers ::TRNC and ::RND allow you to control */
/*        how the output time picture is rounded. If you specify */
/*        ::TRNC all components of time are simply truncated to */
/*        the precision specified by the marker and any modifier. */
/*        If you specify ::RND the output time is rounded to the */
/*        least significant component of the format picture. The */
/*        default action is truncation. */

/*        Whether an output time string should be rounded or */
/*        truncated depends upon what you plan to do with the */
/*        string. For example suppose you simply want to get the */
/*        calendar date associated with a time and not the time of */
/*        day. Then you probably do not want to round your output. */
/*        Rounding 1992 Dec 31, 13:12:00 to the nearest day */
/*        produces 1993 Jan 1. Thus in this case rounding is probably */
/*        not appropriate. */

/*        However, if you are producing output for plotting using */
/*        Julian Date, seconds past 1950 or seconds past 2000, you will */
/*        probably want your output rounded so as to produce a smoother */
/*        plot. */

/*     Time Systems */

/*        TIMOUT can produce output strings for epochs relative to */
/*        any of the systems UTC, TT or TDT, or TDB. If you do not */
/*        explicitly specify a time system, TIMOUT will produce strings */
/*        relative to the time system returned by the SPICE routine */
/*        TIMDEF. Unless you call TIMDEF and change it, the default time */
/*        system is UTC. However, by using one of the Meta Markers */
/*        ::UTC, ::TT, ::TDT, or ::TDB you may specify that TIMOUT */
/*        produce time strings relative to the UTC, TT or TDT, or TDB */
/*        system respectively. */

/*     Time Zones */

/*        The meta markers ::UTC+h:m  and ::UTC-h:m  allow you to */
/*        offset UTC times so that you may represent times in a time */
/*        zone other than GMT. For example you can output times in */
/*        Pacific Standard time by placing the meta-marker ::UTC-8 in */
/*        your format picture. */

/*        For instance, if you use the picture */

/*           YYYY Mon DD, HR:MN:SC ::UTC */

/*        you will get output strings such as: */

/*           1995 Jan 03, 12:00:00 */

/*        If you use the picture */


/*           YYYY Mon DD, HR:MN:SC ::UTC-8 */

/*        you will get output strings such as: */

/*           1995 Jan 03, 04:00:00 */

/*        Finally, if you use the picture */

/*           YYYY Mon DD, HR:MN:SC ::UTC-8:15 */

/*        you will get output string */

/*           1995 Jan 03, 03:45:00 */

/*        Note that the minutes are always added or subtracted based on */
/*        the sign present in the time zone specifier. In the case of */
/*        ::UTC+h:m, minutes are added. In the case ::UTC-h:m, minutes */
/*        are subtracted. */

/*        The unsigned part of the hours component can be no more than */
/*        12. The unsigned part of the minutes component can be no */
/*        more than 59. */

/*     Calendars */

/*        The calendar currently used by western countries is the */
/*        Gregorian calendar. This calendar begins on Oct 15, 1582. */
/*        Prior to Gregorian calendar the Julian calendar was used. The */
/*        last Julian calendar date prior to the beginning of the */
/*        Gregorian calendar is Oct 5, 1582. */

/*        The primary difference between the Julian and Gregorian */
/*        calendars is in the determination of leap years. Nevertheless, */
/*        both can be formally extended backward and forward in time */
/*        indefinitely. */

/*        By default TIMOUT uses the default calendar returned by */
/*        TIMDEF. Under most circumstances this will be the Gregorian */
/*        calendar (::GCAL). However you may specify that TIMOUT use a */
/*        specific calendar through use of one of the calendar Meta */
/*        Markers. You may specify that TIMOUT use the Julian calendar */
/*        (::JCAL), the Gregorian calendar (::GCAL)  or a mixture of */
/*        both (::MCAL). */

/*        If you specify ::MCAL, epochs that occur after the beginning */
/*        of the Gregorian calendar will be represented using the */
/*        Gregorian calendar, and epochs prior to the beginning of the */
/*        Gregorian calendar will be represented using the Julian */
/*        calendar. */

/*     Getting Software to Construct Pictures for You. */

/*        Although it is not difficult to construct time format */
/*        pictures, you do need to be aware of the various markers that */
/*        may appear in a format picture. */

/*        There is an alternative means for getting a format picture. */
/*        The routine TPICTR constructs format pictures from a sample */
/*        time string. For example, suppose you would like your time */
/*        strings to look like the basic pattern of the string below. */

/*           'Fri Jul 26 12:22:09 PDT 1996' */

/*        You can call TPICTR with this string, and it will create the */
/*        appropriate PICTUR for use with TIMOUT. */

/*           CALL TPICTR ( 'Fri Jul 26 12:22:09 PDT 1996', */
/*          .              PICTUR, OK, ERRMSG             ) */

/*        The result will be: */

/*           'Wkd Mon DD HR:MN:SC (PDT) ::UTC-7' */

/*        Note: not every date that you can read is interpretable by */
/*        TPICTR. For example, you might be able to understand that */
/*        19960212121116 is Feb 2 1996, 12:11:16. However, TPICTR */
/*        cannot recognize this string. Thus it is important to check */
/*        the logical output OK to make sure that TPICTR was able to */
/*        understand the time picture you provided. */

/*        Even thought TPICTR can not recognize every time pattern that */
/*        has been used by various people, it does recognize nearly all */
/*        patterns that you use when you want to communicate outside */
/*        your particular circle of colleagues. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */

/*     1) Given a sample with the format of the UNIX date string */
/*        local to California, create a SPICE time picture for use */
/*        in TIMOUT. */

/*        Using that SPICE time picture, convert a series of ephemeris */
/*        times to that picture format. */

/*        Use the LSK kernel below to load the leap seconds and time */
/*        constants required for the conversions. */

/*           naif0012.tls */


/*        Example code begins here. */


/*              PROGRAM TIMOUT_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               ERRLEN */
/*              PARAMETER           ( ERRLEN  = 400 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN  = 64  ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(ERRLEN)    ERR */
/*              CHARACTER*(TIMLEN)    PICTUR */
/*              CHARACTER*(TIMLEN)    SAMPLE */
/*              CHARACTER*(TIMLEN)    TIMSTR */
/*              CHARACTER*(TIMLEN)    UTCSTR */

/*              DOUBLE PRECISION      ET */

/*              LOGICAL               OK */

/*        C */
/*        C     Load LSK file. */
/*        C */
/*              CALL FURNSH ( 'naif0012.tls' ) */

/*        C */
/*        C     Create the required time picture. */
/*        C */
/*              SAMPLE = 'Thu Oct 01 11:11:11 PDT 1111' */

/*              CALL TPICTR ( SAMPLE, PICTUR, OK, ERR ) */

/*              IF ( .NOT. OK ) THEN */

/*                 WRITE(*,*) 'Invalid time picture.' */
/*                 WRITE(*,*) ERR */

/*              ELSE */

/*        C */
/*        C        Convert the input UTC time to ephemeris time. */
/*        C */
/*                 UTCSTR = '26 Nov 2018  23:23:00 UTC' */
/*                 CALL STR2ET ( UTCSTR, ET ) */

/*        C */
/*        C         Now convert ET to the desired output format. */
/*        C */
/*                  CALL TIMOUT ( ET, PICTUR, TIMSTR ) */
/*                  WRITE (*,*) 'Sample format: ', SAMPLE */
/*                  WRITE (*,*) 'Time picture : ', PICTUR */
/*                  WRITE (*,*) */
/*                  WRITE (*,*) 'Input UTC    : ', UTCSTR */
/*                  WRITE (*,*) 'Output       : ', TIMSTR */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Sample format: Thu Oct 01 11:11:11 PDT 1111 */
/*         Time picture : Wkd Mon DD HR:MN:SC PDT YYYY ::UTC-7 */

/*         Input UTC    : 26 Nov 2018  23:23:00 UTC */
/*         Output       : Mon Nov 26 16:23:00 PDT 2018 */


/*     2) Convert a UTC time to a string that contains both the */
/*        calendar representations of the date as well as the Julian */
/*        date; for example a string of the form: */

/*           "Thu Aug 01 09:47:16 PDT 1996 (2450297.1994 JDUTC)" */

/*        Use the LSK kernel below to load the leap seconds and time */
/*        constants required for the conversions. */

/*           naif0012.tls */


/*        Example code begins here. */


/*              PROGRAM TIMOUT_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN  = 80 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(TIMLEN)    PICTUR */
/*              CHARACTER*(TIMLEN)    TIMSTR */
/*              CHARACTER*(TIMLEN)    UTCSTR */

/*              DOUBLE PRECISION      ET */

/*        C */
/*        C     Load LSK file. */
/*        C */
/*              CALL FURNSH ( 'naif0012.tls' ) */

/*        C */
/*        C     Convert the input UTC time to ephemeris time. */
/*        C */
/*              UTCSTR = '26 Nov 2018  16:23:00 UTC' */
/*              CALL STR2ET ( UTCSTR, ET ) */

/*        C */
/*        C     Create the required time picture. This could be done */
/*        C     using TPICTR. */
/*        C */
/*              PICTUR = 'Wkd Mon DD HR:MN ::UTC-7 YYYY ' */
/*             .      // '(JULIAND.#### JDUTC)' */

/*        C */
/*        C      Now convert ET to the desired output format. */
/*        C */
/*               CALL TIMOUT ( ET, PICTUR, TIMSTR ) */
/*               WRITE (*,*) 'Input UTC: ', UTCSTR */
/*               WRITE (*,*) 'Output   : ', TIMSTR */

/*               END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Input UTC: 26 Nov 2018  16:23:00 UTC */
/*         Output   : Mon Nov 26 09:23  2018 (2458449.1826 JDUTC) */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     M.J. Spencer       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.4.0, 07-AUG-2021 (EDW) (JDR) (NJB) */

/*        Corrected typo preventing correct calculation of decimal */
/*        values for HR.###... and MN.###... markers with ::UTC+N:M */
/*        and ::UTC-N:M meta tags. */

/*        Added "::TT" as a time system meta marker equivalent-to/ */
/*        alias-for "::TDT". No change to functionality. */

/*        Corrected OUTPUT argument name in $Brief_I/O section (it was */
/*        STRING) and improved its description in $Detailed_Output. */
/*        Fixed call to TPICTR in $Particulars. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary entries in $Revisions section. Converted the */
/*        existing code fragments into complete examples and added */
/*        references to required LSKs. */

/*        Updated $Exceptions section, rewording the existing entry */
/*        and adding three additional cases. */

/* -    SPICELIB Version 3.3.1, 31-JAN-2017 (NJB) */

/*        Updated header comments to draw attention to the fact that */
/*        rounding can be commanded. */

/* -    SPICELIB Version 3.3.0, 23-OCT-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in RMAIND call. Replaced header references to LDPOOL with */
/*        references to FURNSH. */

/* -    SPICELIB Version 3.2.0, 09-APR-2002 (WLT) */

/*        Added code to bracket the fractional part of a time component */
/*        so that it cannot become negative due to inability to invert */
/*        arithmetic operations with double precision arithmetic. */

/* -    SPICELIB Version 3.1.0, 21-JUN-2001 (WLT) */

/*        Added the format picture components ?ERA? and ?era? which */
/*        vanish for years after 999 A.D. */

/* -    SPICELIB Version 3.0.2, 10-APR-2000 (WLT) */

/*        Declared SCAN to be external. */

/* -    SPICELIB Version 3.0.1, 22-JUN-1998 (WLT) */

/*        A number of typographical and grammatical errors */
/*        were corrected in the header. */

/* -    SPICELIB Version 3.0.0, 30-DEC-1997 (WLT) */

/*        The previous version of this routine did not output */
/*        fractional components for epochs prior to 1 A.D. */

/*        In addition, the default time system, calendar and time zone */
/*        are obtained from TIMDEF. */

/* -    SPICELIB Version 2.0.0, 01-APR-1997 (WLT) */

/*        In the event that the format picture requested 'YR' as */
/*        the first component of a time string, the previous edition */
/*        of this routine used the year value corresponding to the */
/*        last call to this routine (or whatever happened to be in */
/*        memory on the first call). This error has been corrected. */

/* -    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT) (MJS) (NJB) */

/* -& */
/* $ Index_Entries */

/*     Convert and format d.p. seconds past J2000 as a string */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.0.0, 30-DEC-1997 (WLT) */

/*        The previous version of this routine did not output */
/*        fractional components for epochs prior to 1 A.D. */

/*        This error was due to overuse of the original year */
/*        component returned from TTRANS. The original year */
/*        component is now saved for use in computing the fractional */
/*        component. The modified year (used in printing B.C. epochs) */
/*        is stored in a separate variable. */

/* -    SPICELIB Version 2.0.0, 01-APR-1997 (WLT) */

/*        In the event that the format picture requested 'YR' as */
/*        the first component of a time string, the previous edition */
/*        of this routine used the year value corresponding to the */
/*        last call to this routine (or whatever happened to be in */
/*        memory on the first call). This error has been corrected. */


/*        The error was fixed by recoding the following IF THEN statement */

/*              IF (       TYPE .EQ. YEAR */
/*    .               .OR. TYPE .EQ. MONTH */
/*    .               .OR. TYPE .EQ. MON */
/*    .               .OR. TYPE .EQ. DAY */
/*    .               .OR. TYPE .EQ. DOY */
/*    .               .OR. TYPE .EQ. NOON */
/*    .               .OR. TYPE .EQ. HOUR */
/*    .               .OR. TYPE .EQ. ERA */
/*    .               .OR. TYPE .EQ. AMPM */
/*    .               .OR. TYPE .EQ. MINUTE */
/*    .               .OR. TYPE .EQ. SEC   ) THEN */

/*        as */

/*              IF (       TYPE .EQ. YEAR */
/*    .               .OR. TYPE .EQ. YR */
/*    .               .OR. TYPE .EQ. MONTH */
/*    .               .OR. TYPE .EQ. MON */
/*    .               .OR. TYPE .EQ. DAY */
/*    .               .OR. TYPE .EQ. DOY */
/*    .               .OR. TYPE .EQ. NOON */
/*    .               .OR. TYPE .EQ. HOUR */
/*    .               .OR. TYPE .EQ. ERA */
/*    .               .OR. TYPE .EQ. AMPM */
/*    .               .OR. TYPE .EQ. MINUTE */
/*    .               .OR. TYPE .EQ. SEC   ) THEN */


/* -    Beta Version 2.1.0, 17-MAR-1994 (MJS) (NJB) */

/*        Integer argument to BRCKTD changed from 0 to 0.0D0. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     The parameters below act essentially as an enumeration */
/*     of the various kinds of components we will be looking at in the */
/*     input time string. */


/*     The following parameters serve as an enumeration of the various */
/*     time formats that are recognized. */


/*     The parameters below are used to declare the space needed for */
/*     scanning the input format string. */


/*     The length of the local string that we will use for copying */
/*     the format string. */


/*     Local variables */


/*     The next set of variables holds the marks and auxiliary */
/*     arrays used for scanning the format string. */


/*     The variables below are used to hold, base formats, values of */
/*     time vector components, adjustments to use when rounding, */
/*     the lengths of the format pictures and whether or not various */
/*     components have already been computed. */


/*     The array below contains the indexes of the various values */
/*     associated with the three different times of time vectors */
/*     that we will be using YMD, YD, CONTIN. */


/*     We will be making a local copy of the input format string */
/*     and the input time. */


/*     The integers below are used to mark substring boundaries. */


/*     Times come in three flavors: TT or TDT, TDB, UTC.  The one used */
/*     on this particular invocation of TIMOUT is stored in TIMTYP. */
/*     The routine TTRANS needs to have input and output time vector */
/*     types.  The one used based upon the input PICTUR is stored */
/*     in BASTYP. */


/*     Loop counters and delimiters */


/*     Utility double precision numbers */


/*     The array power is used to assist in the truncation of double */
/*     precision values. */


/*     calendar variables. */


/*     Character string representations for months and week days. */


/*     Save everything. */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("TIMOUT", (ftnlen)6);

/*     Chapter 1. Initializations. */
/*     ================================================================= */

/*     On the first pass, we need to set up the recognized tokens */
/*     that will be used for scanning, the classes of these tokens */
/*     and the array of ID's for time systems. */

    if (first) {
	first = FALSE_;
	s_copy(marks, "YYYY", (ftnlen)8, (ftnlen)4);
	s_copy(marks + 8, "YR", (ftnlen)8, (ftnlen)2);
	s_copy(marks + 16, "MON", (ftnlen)8, (ftnlen)3);
	s_copy(marks + 24, "Mon", (ftnlen)8, (ftnlen)3);
	s_copy(marks + 32, "mon", (ftnlen)8, (ftnlen)3);
	s_copy(marks + 40, "MONTH", (ftnlen)8, (ftnlen)5);
	s_copy(marks + 48, "Month", (ftnlen)8, (ftnlen)5);
	s_copy(marks + 56, "month", (ftnlen)8, (ftnlen)5);
	s_copy(marks + 64, "MM", (ftnlen)8, (ftnlen)2);
	s_copy(marks + 72, "DOY", (ftnlen)8, (ftnlen)3);
	s_copy(marks + 80, "WKD", (ftnlen)8, (ftnlen)3);
	s_copy(marks + 88, "Wkd", (ftnlen)8, (ftnlen)3);
	s_copy(marks + 96, "wkd", (ftnlen)8, (ftnlen)3);
	s_copy(marks + 104, "WEEKDAY", (ftnlen)8, (ftnlen)7);
	s_copy(marks + 112, "Weekday", (ftnlen)8, (ftnlen)7);
	s_copy(marks + 120, "weekday", (ftnlen)8, (ftnlen)7);
	s_copy(marks + 128, "DD", (ftnlen)8, (ftnlen)2);
	s_copy(marks + 136, "MN", (ftnlen)8, (ftnlen)2);
	s_copy(marks + 144, "HR", (ftnlen)8, (ftnlen)2);
	s_copy(marks + 152, "SC", (ftnlen)8, (ftnlen)2);
	s_copy(marks + 160, ".#", (ftnlen)8, (ftnlen)2);
	s_copy(marks + 168, "#", (ftnlen)8, (ftnlen)1);
	s_copy(marks + 176, "JULIAND", (ftnlen)8, (ftnlen)7);
	s_copy(marks + 184, "::UTC", (ftnlen)8, (ftnlen)5);
	s_copy(marks + 192, "::TDB", (ftnlen)8, (ftnlen)5);
	s_copy(marks + 200, "::TDT", (ftnlen)8, (ftnlen)5);
	s_copy(marks + 208, "::TT", (ftnlen)8, (ftnlen)4);
	s_copy(marks + 216, "SP2000", (ftnlen)8, (ftnlen)6);
	s_copy(marks + 224, "SP1950", (ftnlen)8, (ftnlen)6);
	s_copy(marks + 232, "::RND", (ftnlen)8, (ftnlen)5);
	s_copy(marks + 240, "::TRNC", (ftnlen)8, (ftnlen)6);
	s_copy(marks + 248, "ERA", (ftnlen)8, (ftnlen)3);
	s_copy(marks + 256, "era", (ftnlen)8, (ftnlen)3);
	s_copy(marks + 264, "AMPM", (ftnlen)8, (ftnlen)4);
	s_copy(marks + 272, "ampm", (ftnlen)8, (ftnlen)4);
	s_copy(marks + 280, "::UTC+", (ftnlen)8, (ftnlen)6);
	s_copy(marks + 288, "::UTC-", (ftnlen)8, (ftnlen)6);
	s_copy(marks + 296, "::JCAL", (ftnlen)8, (ftnlen)6);
	s_copy(marks + 304, "::GCAL", (ftnlen)8, (ftnlen)6);
	s_copy(marks + 312, "::MCAL", (ftnlen)8, (ftnlen)6);
	s_copy(marks + 320, "AP", (ftnlen)8, (ftnlen)2);
	s_copy(marks + 328, "?ERA?", (ftnlen)8, (ftnlen)5);
	s_copy(marks + 336, "?era?", (ftnlen)8, (ftnlen)5);
	nmarks = 43;
	scanpr_(&nmarks, marks, mrklen, pntrs, (ftnlen)8);

/*        Now that we've prepared our recognized substrings and */
/*        auxiliary arrays for scanning, collect the id's of the */
/*        various marks and classify the various marks. */
/*        substrings. */

	id[1] = 0;
	id[2] = bsrchc_("YYYY", &nmarks, marks, (ftnlen)4, (ftnlen)8);
	id[3] = bsrchc_("YR", &nmarks, marks, (ftnlen)2, (ftnlen)8);
	id[4] = bsrchc_("MON", &nmarks, marks, (ftnlen)3, (ftnlen)8);
	id[5] = bsrchc_("Mon", &nmarks, marks, (ftnlen)3, (ftnlen)8);
	id[6] = bsrchc_("mon", &nmarks, marks, (ftnlen)3, (ftnlen)8);
	id[7] = bsrchc_("MONTH", &nmarks, marks, (ftnlen)5, (ftnlen)8);
	id[8] = bsrchc_("Month", &nmarks, marks, (ftnlen)5, (ftnlen)8);
	id[9] = bsrchc_("month", &nmarks, marks, (ftnlen)5, (ftnlen)8);
	id[10] = bsrchc_("MM", &nmarks, marks, (ftnlen)2, (ftnlen)8);
	id[11] = bsrchc_("DOY", &nmarks, marks, (ftnlen)3, (ftnlen)8);
	id[12] = bsrchc_("WKD", &nmarks, marks, (ftnlen)3, (ftnlen)8);
	id[13] = bsrchc_("Wkd", &nmarks, marks, (ftnlen)3, (ftnlen)8);
	id[14] = bsrchc_("wkd", &nmarks, marks, (ftnlen)3, (ftnlen)8);
	id[15] = bsrchc_("WEEKDAY", &nmarks, marks, (ftnlen)7, (ftnlen)8);
	id[16] = bsrchc_("Weekday", &nmarks, marks, (ftnlen)7, (ftnlen)8);
	id[17] = bsrchc_("weekday", &nmarks, marks, (ftnlen)7, (ftnlen)8);
	id[18] = bsrchc_("DD", &nmarks, marks, (ftnlen)2, (ftnlen)8);
	id[19] = bsrchc_("MN", &nmarks, marks, (ftnlen)2, (ftnlen)8);
	id[20] = bsrchc_("HR", &nmarks, marks, (ftnlen)2, (ftnlen)8);
	id[21] = bsrchc_("SC", &nmarks, marks, (ftnlen)2, (ftnlen)8);
	id[22] = bsrchc_(".#", &nmarks, marks, (ftnlen)2, (ftnlen)8);
	id[23] = bsrchc_("#", &nmarks, marks, (ftnlen)1, (ftnlen)8);
	id[24] = bsrchc_("JULIAND", &nmarks, marks, (ftnlen)7, (ftnlen)8);
	id[25] = bsrchc_("::UTC", &nmarks, marks, (ftnlen)5, (ftnlen)8);
	id[26] = bsrchc_("::TDB", &nmarks, marks, (ftnlen)5, (ftnlen)8);
	id[27] = bsrchc_("::TDT", &nmarks, marks, (ftnlen)5, (ftnlen)8);
	id[28] = bsrchc_("::TT", &nmarks, marks, (ftnlen)4, (ftnlen)8);
	id[29] = bsrchc_("SP2000", &nmarks, marks, (ftnlen)6, (ftnlen)8);
	id[30] = bsrchc_("SP1950", &nmarks, marks, (ftnlen)6, (ftnlen)8);
	id[31] = bsrchc_("::RND", &nmarks, marks, (ftnlen)5, (ftnlen)8);
	id[32] = bsrchc_("::TRNC", &nmarks, marks, (ftnlen)6, (ftnlen)8);
	id[33] = bsrchc_("ERA", &nmarks, marks, (ftnlen)3, (ftnlen)8);
	id[34] = bsrchc_("era", &nmarks, marks, (ftnlen)3, (ftnlen)8);
	id[35] = bsrchc_("?ERA?", &nmarks, marks, (ftnlen)5, (ftnlen)8);
	id[36] = bsrchc_("?era?", &nmarks, marks, (ftnlen)5, (ftnlen)8);
	id[37] = bsrchc_("AMPM", &nmarks, marks, (ftnlen)4, (ftnlen)8);
	id[38] = bsrchc_("ampm", &nmarks, marks, (ftnlen)4, (ftnlen)8);
	id[39] = bsrchc_("::UTC+", &nmarks, marks, (ftnlen)6, (ftnlen)8);
	id[40] = bsrchc_("::UTC-", &nmarks, marks, (ftnlen)6, (ftnlen)8);
	id[41] = bsrchc_("::JCAL", &nmarks, marks, (ftnlen)6, (ftnlen)8);
	id[42] = bsrchc_("::GCAL", &nmarks, marks, (ftnlen)6, (ftnlen)8);
	id[43] = bsrchc_("::MCAL", &nmarks, marks, (ftnlen)6, (ftnlen)8);
	id[46] = bsrchc_("AP", &nmarks, marks, (ftnlen)2, (ftnlen)8);
	class__[(i__1 = id[1]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1307)] = 2;
	class__[(i__1 = id[2]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1308)] = 3;
	class__[(i__1 = id[3]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1309)] = 4;
	class__[(i__1 = id[4]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1310)] = 48;
	class__[(i__1 = id[5]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1311)] = 48;
	class__[(i__1 = id[6]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1312)] = 48;
	class__[(i__1 = id[7]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1313)] = 48;
	class__[(i__1 = id[8]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1314)] = 48;
	class__[(i__1 = id[9]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1315)] = 48;
	class__[(i__1 = id[10]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1316)] = 11;
	class__[(i__1 = id[11]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1317)] = 12;
	class__[(i__1 = id[12]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1318)] = 49;
	class__[(i__1 = id[13]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1319)] = 49;
	class__[(i__1 = id[14]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1320)] = 49;
	class__[(i__1 = id[15]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1321)] = 49;
	class__[(i__1 = id[16]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1322)] = 49;
	class__[(i__1 = id[17]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1323)] = 49;
	class__[(i__1 = id[18]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1324)] = 19;
	class__[(i__1 = id[19]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1325)] = 20;
	class__[(i__1 = id[20]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1326)] = 21;
	class__[(i__1 = id[21]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1327)] = 22;
	class__[(i__1 = id[22]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1328)] = 23;
	class__[(i__1 = id[23]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1329)] = 24;
	class__[(i__1 = id[24]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1330)] = 25;
	class__[(i__1 = id[25]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1331)] = 45;
	class__[(i__1 = id[26]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1332)] = 45;
	class__[(i__1 = id[27]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1333)] = 45;
	class__[(i__1 = id[28]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1334)] = 45;
	class__[(i__1 = id[29]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1335)] = 30;
	class__[(i__1 = id[30]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1336)] = 31;
	class__[(i__1 = id[31]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1337)] = 32;
	class__[(i__1 = id[32]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1338)] = 33;
	class__[(i__1 = id[33]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1339)] = 50;
	class__[(i__1 = id[34]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1340)] = 50;
	class__[(i__1 = id[35]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1341)] = 50;
	class__[(i__1 = id[36]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1342)] = 50;
	class__[(i__1 = id[37]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1343)] = 51;
	class__[(i__1 = id[38]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1344)] = 51;
	class__[(i__1 = id[39]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1345)] = 45;
	class__[(i__1 = id[40]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1346)] = 45;
	class__[(i__1 = id[41]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1347)] = 46;
	class__[(i__1 = id[42]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1348)] = 46;
	class__[(i__1 = id[43]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1349)] = 46;
	class__[(i__1 = id[46]) < 44 && 0 <= i__1 ? i__1 : s_rnge("class", 
		i__1, "timout_", (ftnlen)1350)] = 47;
	for (i__ = 1; i__ <= 52; ++i__) {
	    pad[(i__1 = i__ - 1) < 52 && 0 <= i__1 ? i__1 : s_rnge("pad", 
		    i__1, "timout_", (ftnlen)1353)] = 0.;
	}
	pad[21] = .5;
	pad[19] = pad[21] * 60.;
	pad[20] = pad[19] * 60.;
	pad[18] = pad[20] * 24.;
	pad[10] = pad[18] * 30.;
	pad[11] = pad[18];
	pad[47] = pad[10];
	pad[2] = pad[18] * 365.;
	pad[3] = pad[18] * 365.;
	pad[24] = pad[18];
	pad[29] = pad[21];
	pad[30] = pad[21];
	pad[46] = pad[20];

/*        After we've made the initial scan for tokens and determined */
/*        the time system requested, we will want to get rid of the */
/*        time system tokens. */

	dump[0] = id[25];
	dump[1] = id[27];
	dump[2] = id[28];
	dump[3] = id[26];
	dump[4] = id[31];
	dump[5] = id[32];
	dump[6] = id[40];
	dump[7] = id[39];
	dump[8] = id[41];
	dump[9] = id[42];
	dump[10] = id[43];
	ndump = 11;

/*        Set up the default formats for the various time components */

	s_copy(orignl + 64, "YYYY", (ftnlen)32, (ftnlen)4);
	length[2] = 4;
	s_copy(orignl + 96, "0Y", (ftnlen)32, (ftnlen)2);
	length[3] = 2;
	s_copy(orignl + 352, "0DD", (ftnlen)32, (ftnlen)3);
	length[11] = 3;
	s_copy(orignl + 576, "0D", (ftnlen)32, (ftnlen)2);
	length[18] = 2;
	s_copy(orignl + 320, "0M", (ftnlen)32, (ftnlen)2);
	length[10] = 2;
	s_copy(orignl + 640, "0H", (ftnlen)32, (ftnlen)2);
	length[20] = 2;
	s_copy(orignl + 1472, "0H", (ftnlen)32, (ftnlen)2);
	length[46] = 2;
	s_copy(orignl + 608, "0M", (ftnlen)32, (ftnlen)2);
	length[19] = 2;
	s_copy(orignl + 672, "0S", (ftnlen)32, (ftnlen)2);
	length[21] = 2;
	s_copy(orignl + 768, "XXXXXXX", (ftnlen)32, (ftnlen)7);
	length[24] = 7;
	s_copy(orignl + 928, "XXXXXXXXXXX", (ftnlen)32, (ftnlen)11);
	length[29] = 11;
	s_copy(orignl + 960, "XXXXXXXXXXX", (ftnlen)32, (ftnlen)11);
	length[30] = 11;

/*        Finally set up the component pointers... */

	compnt[0] = 52;
	compnt[1] = 11;
	compnt[2] = 19;
	compnt[3] = 21;
	compnt[4] = 20;
	compnt[5] = 22;
	compnt[8] = 1;
    }

/*     Chapter 2.  Parsing the input picture. */
/*     ============================================================== */

/*     First let's copy the input picture into local storage */
/*     (left justified) and get just past the end of the */
/*     significant portion (this way the loop that constructs the */
/*     output string will terminate with no unfinished business */
/*     left to resolve). */

    s_copy(mystr, " ", (ftnlen)256, (ftnlen)1);
    ljust_(pictur, mystr, pictur_len, (ftnlen)255);
    e = rtrim_(mystr, (ftnlen)256) + 1;
    start = 1;

/*     Scan the input string. */

    scan_(mystr, marks, mrklen, pntrs, &c__100, &start, &ntokns, ident, beg, 
	    end, e, (ftnlen)8);

/*     Locate the time system that will be used.  This must */
/*     be one of the following: UTC, TDB, TT, TDT */

    unknwn = TRUE_;
    go2jul = FALSE_;
    dozone = FALSE_;
    i__ = 1;
    hoff = 0.;
    moff = 0.;

/*     Get the default time type from TIMDEF */

    timdef_("GET", "SYSTEM", tsys, (ftnlen)3, (ftnlen)6, (ftnlen)16);
    if (s_cmp(tsys, "UTC", (ftnlen)16, (ftnlen)3) == 0) {
	timtyp = id[25];
    } else if (s_cmp(tsys, "TDB", (ftnlen)16, (ftnlen)3) == 0) {
	timtyp = id[26];
    } else if (s_cmp(tsys, "TDT", (ftnlen)16, (ftnlen)3) == 0 || s_cmp(tsys, 
	    "TT", (ftnlen)16, (ftnlen)2) == 0) {
	timtyp = id[27];
    } else {
	timtyp = id[39];
	timdef_("GET", "ZONE", zon, (ftnlen)3, (ftnlen)4, (ftnlen)32);
	prefix_("::", &c__0, zon, (ftnlen)2, (ftnlen)32);
	zzutcpm_(zon, &c__1, &hoff, &moff, &last, &ok, (ftnlen)32);
	dozone = ok;

/*        The routine TIMDEF uses ZZUTCPM to determine whether */
/*        or not a time zone is legitimate before it stores it */
/*        to be "GOTTEN."  As a result the value of OK should */
/*        always be .TRUE.  However, just in case TIMDEF should */
/*        someday use something other that ZZUTCPM for checking */
/*        we put in the unneeded check below. */

	if (! ok) {
	    timtyp = id[25];
	}
    }
    while(unknwn && i__ <= ntokns) {
	if (class__[(i__2 = ident[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 :
		 s_rnge("ident", i__1, "timout_", (ftnlen)1513)]) < 44 && 0 <=
		 i__2 ? i__2 : s_rnge("class", i__2, "timout_", (ftnlen)1513)]
		 == 45) {
	    timtyp = ident[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("ident", i__1, "timout_", (ftnlen)1514)];
	    unknwn = FALSE_;
	    dozone = FALSE_;
	    if (ident[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "ident", i__1, "timout_", (ftnlen)1518)] == id[39] || 
		    ident[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
		    "ident", i__2, "timout_", (ftnlen)1518)] == id[40]) {

/*              We've got a time zone specification. Parse it and */
/*              store the offsets from UTC. */

		zzutcpm_(mystr, &beg[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? 
			i__1 : s_rnge("beg", i__1, "timout_", (ftnlen)1525)], 
			&hoff, &moff, &last, &ok, (ftnlen)256);
		if (ok) {
		    dozone = TRUE_;
		    timtyp = id[39];

/*                 If we ran all the way up to the end of the next */
/*                 token, we simply reset the identity of the next */
/*                 token to be a zone type and increment  I. */

/*                 This way we never see the next token in this loop */
/*                 and it gets removed later when time systems and */
/*                 other meta markers from  our copy of the time */
/*                 format string. */

		    if (last == end[(i__1 = i__) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("end", i__1, "timout_", (ftnlen)1542)]) {
			ident[(i__1 = i__) < 100 && 0 <= i__1 ? i__1 : s_rnge(
				"ident", i__1, "timout_", (ftnlen)1543)] = 
				ident[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? 
				i__2 : s_rnge("ident", i__2, "timout_", (
				ftnlen)1543)];
			++i__;
		    } else {
			end[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : 
				s_rnge("end", i__1, "timout_", (ftnlen)1546)] 
				= last;
			beg[(i__1 = i__) < 100 && 0 <= i__1 ? i__1 : s_rnge(
				"beg", i__1, "timout_", (ftnlen)1547)] = last 
				+ 1;
		    }
		}
	    }
	}
	++i__;
    }

/*     Determine whether we should use the Julian or gregorian (default) */
/*     calendar */

    unknwn = TRUE_;
    i__ = 1;

/*     Get the default calendar from TIMDEF. */

    timdef_("GET", "CALENDAR", cal, (ftnlen)3, (ftnlen)8, (ftnlen)16);
    if (s_cmp(cal, "GREGORIAN", (ftnlen)16, (ftnlen)9) == 0) {
	caltyp = id[42];
    } else if (s_cmp(cal, "JULIAN", (ftnlen)16, (ftnlen)6) == 0) {
	caltyp = id[41];
    } else {
	caltyp = id[43];
    }
    while(unknwn && i__ <= ntokns) {
	if (class__[(i__2 = ident[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 :
		 s_rnge("ident", i__1, "timout_", (ftnlen)1583)]) < 44 && 0 <=
		 i__2 ? i__2 : s_rnge("class", i__2, "timout_", (ftnlen)1583)]
		 == 46) {
	    caltyp = ident[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("ident", i__1, "timout_", (ftnlen)1584)];
	    unknwn = FALSE_;
	}
	++i__;
    }

/*     Next determine whether or not we shall be performing rounding */
/*     on output. */

    pumpup = isrchi_(&id[31], &ntokns, ident) != 0;

/*     Determine if we have an Era specification */

    doera = isrchi_(&id[34], &ntokns, ident) != 0 || isrchi_(&id[33], &ntokns,
	     ident) != 0 || isrchi_(&id[35], &ntokns, ident) != 0 || isrchi_(&
	    id[36], &ntokns, ident) != 0;

/*     Until we've examined the year, we assume that the era is not */
/*     supposed to vanish. */

    vanish = FALSE_;

/*     Next remove all of the time system dudes from the list of */
/*     tokens. */

    scanrj_(dump, &ndump, &ntokns, ident, beg, end);

/*     If the user wants to round the output, we need to pump up ET */
/*     by the smallest significant part of the input picture.  But */
/*     in either case we are going to pad the input time.  For now */
/*     we pad it by zero. */

    timpad = 0.;
    if (pumpup) {

/*        We need to determine the amount to pad ET by.  So we need */
/*        to look at the string and find the least significant component */
/*        that has been requested.  Keep in mind that the last token */
/*        is of type NONAME (its a blank) by construction. */

	i__ = 1;
	while(i__ <= ntokns) {
	    type__ = class__[(i__2 = ident[(i__1 = i__ - 1) < 100 && 0 <= 
		    i__1 ? i__1 : s_rnge("ident", i__1, "timout_", (ftnlen)
		    1638)]) < 44 && 0 <= i__2 ? i__2 : s_rnge("class", i__2, 
		    "timout_", (ftnlen)1638)];
	    if (type__ == 2 || type__ == 23 || type__ == 24 || type__ == 51 ||
		     type__ == 50 || type__ == 48 || type__ == 49) {

/*              Don't do anything, just go on to the next token. */

		++i__;
	    } else {

/*              Look up the amount we should pad our time by. */

		factor = 1.;
		incr = pad[(i__1 = type__ - 1) < 52 && 0 <= i__1 ? i__1 : 
			s_rnge("pad", i__1, "timout_", (ftnlen)1659)];

/*              Examine the next token.  If it's not a decimal point */
/*              and marker, we have the least significant part of */
/*              this component. */

		++i__;
		type__ = class__[(i__2 = ident[(i__1 = i__ - 1) < 100 && 0 <= 
			i__1 ? i__1 : s_rnge("ident", i__1, "timout_", (
			ftnlen)1667)]) < 44 && 0 <= i__2 ? i__2 : s_rnge(
			"class", i__2, "timout_", (ftnlen)1667)];
		if (type__ == 23) {
		    factor *= .1;
		    ++i__;

/*                 Now just look for the end of the string of place */
/*                 holders */

		    while(ident[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("ident", i__1, "timout_", (ftnlen)1678)] ==
			     id[23]) {
			factor *= .1;
			++i__;
		    }
		}

/*              Now compute the time pad for this component of the */
/*              time string. */

		incr *= factor;
		if (timpad != 0.) {
		    timpad = min(timpad,incr);
		} else {
		    timpad = incr;
		}
	    }
	}
    }

/*     Right now we don't have any components of the time format */
/*     and we don't need any of them so far. */

    for (part = 1; part <= 52; ++part) {
	have[(i__1 = part - 1) < 52 && 0 <= i__1 ? i__1 : s_rnge("have", i__1,
		 "timout_", (ftnlen)1708)] = FALSE_;
    }

/*     Set up the input time format and the output time format that will */
/*     be used later. */

/*     The input time format is used to convert the basic ET we have now */
/*     to one of the various time formats that are supported by the */
/*     routine TTRANS.  If we are going to construct a string in one of */
/*     the dynamical time systems we will call the input time a formal */
/*     time in seconds past a formal calendar epoch of J2000.  If on the */
/*     other hand we are going to construct a UTC based string, we will */
/*     convert our ET to an earth based epoch (TT, TDT) and use this as */
/*     our base input system. */


    myet = *et;
    if (timtyp == id[26]) {

/*        Since we are likely to need SP2000, SP1950 or JD, we */
/*        compute them now. */

	myet += timpad;
	values[29] = myet;
	values[24] = unitim_(&myet, "TDB", "JDTDB", (ftnlen)3, (ftnlen)5);
	values[30] = values[29] + spd_() * (j2000_() - j1950_());
	s_copy(bastyp, "FORMAL", (ftnlen)16, (ftnlen)6);
	s_copy(ymdfmt, "YMDF", (ftnlen)8, (ftnlen)4);
	s_copy(ywfmt, "YMWDF", (ftnlen)8, (ftnlen)5);
	have[29] = TRUE_;
	have[30] = TRUE_;
	have[24] = TRUE_;
    } else if (timtyp == id[27] || timtyp == id[28]) {
	myet = unitim_(&myet, "TDB", "TDT", (ftnlen)3, (ftnlen)3) + timpad;
	values[29] = myet;
	values[24] = unitim_(&myet, "TDT", "JDTDT", (ftnlen)3, (ftnlen)5);
	values[30] = values[29] + spd_() * (j2000_() - j1950_());
	s_copy(bastyp, "FORMAL", (ftnlen)16, (ftnlen)6);
	s_copy(ymdfmt, "YMDF", (ftnlen)8, (ftnlen)4);
	s_copy(ywfmt, "YMWDF", (ftnlen)8, (ftnlen)5);
	have[29] = TRUE_;
	have[30] = TRUE_;
	have[24] = TRUE_;
    } else {

/*        In this case we convert to an earth based frame for our */
/*        working epoch.  This rounds properly when it's time to get */
/*        fractional components. */

	myet = unitim_(&myet, "TDB", "TDT", (ftnlen)3, (ftnlen)3) + timpad;
	s_copy(bastyp, "TDT", (ftnlen)16, (ftnlen)3);
	s_copy(ymdfmt, "YMD", (ftnlen)8, (ftnlen)3);
	s_copy(ywfmt, "YMWD", (ftnlen)8, (ftnlen)4);
    }

/*     Chapter 3.  Building the Output String */
/*     ================================================================== */


/*     Now we are ready to go, we need to fetch the tokens */
/*     and construct the output string.  We will */
/*     put the next portion of the output at APPND */

    appnd = 1;
    making = FALSE_;
    i__1 = ntokns;
    for (i__ = 1; i__ <= i__1; ++i__) {
	type__ = class__[(i__3 = ident[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? 
		i__2 : s_rnge("ident", i__2, "timout_", (ftnlen)1790)]) < 44 
		&& 0 <= i__3 ? i__3 : s_rnge("class", i__3, "timout_", (
		ftnlen)1790)];
	tvec[0] = myet;

/*        If the next marker is not one we use as a place holder */
/*        in the fractional part of decimal formats AND we */
/*        are in the process of building a format, then the format */
/*        building is done.  We can construct the component and */
/*        append it to the string we are building. */

	if (type__ != 24 && making) {

/*           We also need to be sure this isn't a decimal point */
/*           marker before we add on to the output string. */

	    if (type__ != 23 || have[22]) {

/*              We are going to truncate the number to the number of */
/*              places requested NOT round. */

		i__3 = width - length[(i__2 = numtyp - 1) < 52 && 0 <= i__2 ? 
			i__2 : s_rnge("length", i__2, "timout_", (ftnlen)1812)
			] - 1;
		trncat = brckti_(&i__3, &c__0, &c__14);
		frac = value - d_int(&value);
		if (frac < 0.) {
		    value += -1.;
		    frac += 1.;
		}
		d__1 = frac * power[(i__2 = trncat) < 15 && 0 <= i__2 ? i__2 :
			 s_rnge("power", i__2, "timout_", (ftnlen)1820)];
		intmed = (d_int(&d__1) - .125) / power[(i__3 = trncat) < 15 &&
			 0 <= i__3 ? i__3 : s_rnge("power", i__3, "timout_", (
			ftnlen)1820)];
		frac = brcktd_(&intmed, &c_b279, &c_b280);
		value = d_int(&value) + frac;
		dpfmt_(&value, fmt, substr, (ftnlen)32, (ftnlen)256);
		s_copy(string + (appnd - 1), substr, 256 - (appnd - 1), (
			ftnlen)256);
		appnd += width;
		have[22] = FALSE_;
		making = FALSE_;
	    }
	}

/*        If the token isn't recognized we can just */
/*        append it to the string we are constructing and */
/*        adjust the point at which the next substring is */
/*        to be appended. */

	if (type__ == 2) {
	    i__2 = beg[(i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge(
		    "beg", i__3, "timout_", (ftnlen)1847)] - 1;
	    s_copy(string + (appnd - 1), mystr + i__2, 256 - (appnd - 1), end[
		    (i__4 = i__ - 1) < 100 && 0 <= i__4 ? i__4 : s_rnge("end",
		     i__4, "timout_", (ftnlen)1847)] - i__2);
	    appnd = appnd - beg[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
		    s_rnge("beg", i__2, "timout_", (ftnlen)1848)] + end[(i__3 
		    = i__ - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge("end", i__3,
		     "timout_", (ftnlen)1848)] + 1;

/*        If the token is a place holder, we either just append it */
/*        or tack it on to a format string we are creating.. */

	} else if (type__ == 24) {
	    if (making) {
		b = width + 1;
		e = b - beg[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
			s_rnge("beg", i__2, "timout_", (ftnlen)1859)] + end[(
			i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge(
			"end", i__3, "timout_", (ftnlen)1859)];
		i__2 = beg[(i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : 
			s_rnge("beg", i__3, "timout_", (ftnlen)1860)] - 1;
		s_copy(fmt + (b - 1), mystr + i__2, e - (b - 1), end[(i__4 = 
			i__ - 1) < 100 && 0 <= i__4 ? i__4 : s_rnge("end", 
			i__4, "timout_", (ftnlen)1860)] - i__2);
		width = width - beg[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? 
			i__2 : s_rnge("beg", i__2, "timout_", (ftnlen)1861)] 
			+ end[(i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : 
			s_rnge("end", i__3, "timout_", (ftnlen)1861)] + 1;
	    } else {
		i__2 = beg[(i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : 
			s_rnge("beg", i__3, "timout_", (ftnlen)1863)] - 1;
		s_copy(string + (appnd - 1), mystr + i__2, (ftnlen)1, end[(
			i__4 = i__ - 1) < 100 && 0 <= i__4 ? i__4 : s_rnge(
			"end", i__4, "timout_", (ftnlen)1863)] - i__2);
		appnd = appnd - beg[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? 
			i__2 : s_rnge("beg", i__2, "timout_", (ftnlen)1864)] 
			+ end[(i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : 
			s_rnge("end", i__3, "timout_", (ftnlen)1864)] + 1;
	    }

/*        If the token is the decimal point plus place holder */
/*        AND we are making a format, we append it to the current */
/*        format and determine the fractional part of the current */
/*        quantity. */

	} else if (type__ == 23) {
	    if (! making) {
		b = appnd;
		e = appnd - beg[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
			s_rnge("beg", i__2, "timout_", (ftnlen)1878)] + end[(
			i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge(
			"end", i__3, "timout_", (ftnlen)1878)];
		i__2 = beg[(i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : 
			s_rnge("beg", i__3, "timout_", (ftnlen)1879)] - 1;
		s_copy(string + (b - 1), mystr + i__2, e - (b - 1), end[(i__4 
			= i__ - 1) < 100 && 0 <= i__4 ? i__4 : s_rnge("end", 
			i__4, "timout_", (ftnlen)1879)] - i__2);
		appnd = e + 1;
		have[22] = FALSE_;
	    } else if (timfmt == 2) {
		b = width + 1;
		e = b - beg[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
			s_rnge("beg", i__2, "timout_", (ftnlen)1886)] + end[(
			i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge(
			"end", i__3, "timout_", (ftnlen)1886)];
		i__2 = beg[(i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : 
			s_rnge("beg", i__3, "timout_", (ftnlen)1887)] - 1;
		s_copy(fmt + (b - 1), mystr + i__2, e - (b - 1), end[(i__4 = 
			i__ - 1) < 100 && 0 <= i__4 ? i__4 : s_rnge("end", 
			i__4, "timout_", (ftnlen)1887)] - i__2);
		width = e;
		have[22] = TRUE_;
	    } else {
		b = width + 1;
		e = b - beg[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
			s_rnge("beg", i__2, "timout_", (ftnlen)1894)] + end[(
			i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge(
			"end", i__3, "timout_", (ftnlen)1894)];
		i__2 = beg[(i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : 
			s_rnge("beg", i__3, "timout_", (ftnlen)1895)] - 1;
		s_copy(fmt + (b - 1), mystr + i__2, e - (b - 1), end[(i__4 = 
			i__ - 1) < 100 && 0 <= i__4 ? i__4 : s_rnge("end", 
			i__4, "timout_", (ftnlen)1895)] - i__2);
		width = e;
		have[22] = TRUE_;

/*              Since we obviously are going to be needing */
/*              the fractional part of this component we fetch it */
/*              now and add it to whatever the integer part of the */
/*              current value is.  Here's how we do this. */
/*              If we truncated the input time to this component */
/*              we'd have a value on an "integer" portion of the */
/*              time scale. */
/*              . */
/*              .               current */
/*              .               time */
/*              .               truncated    .---MYET */
/*                                   |       | */
/*                                   v       v */
/*              time scale: ---------+-------X-----------+----- */
/*                                                       ^ */
/*                                                       | */
/*              .                               truncated time */
/*              .                               plus 1 in this */
/*              .                               component */
/*              . */
/*              Add one to the truncated component to get the */
/*              next integer component.  Finally we convert these */
/*              two constructed stings to seconds so that we can */
/*              get the "fractional part" of the current component. */
/*              Fortunately, when we computed the integer value */
/*              for this component we constructed the time */
/*              vectors we need, so we don't have to go to a lot */
/*              of trouble now. */

		ttrans_(intyp, bastyp, ptvec, (ftnlen)16, (ftnlen)16);
		ttrans_(intyp, bastyp, ntvec, (ftnlen)16, (ftnlen)16);
/* Computing MAX */
		d__1 = 1., d__2 = ntvec[0] - ptvec[0];
		delta = max(d__1,d__2);
		d__1 = (myet - ptvec[0]) / delta;
		frac = brcktd_(&c_b279, &c_b280, &d__1);
		value += frac;
	    }
	} else {

/*        If we get to this point we have an honest time */
/*        string component to fetch.  We might already have */
/*        this guy.  If so we can just collect him from the */
/*        values buffer (although this collection is performed */
/*        after the next long IF-THEN block that gets the value */
/*        if we don't already have it). */

	    making = TRUE_;
	    have[22] = FALSE_;
	    s_copy(fmt, orignl + (((i__2 = type__ - 1) < 52 && 0 <= i__2 ? 
		    i__2 : s_rnge("orignl", i__2, "timout_", (ftnlen)1952)) <<
		     5), (ftnlen)32, (ftnlen)32);
	    width = length[(i__2 = type__ - 1) < 52 && 0 <= i__2 ? i__2 : 
		    s_rnge("length", i__2, "timout_", (ftnlen)1953)];
	    numtyp = type__;
	    if (! have[(i__2 = type__ - 1) < 52 && 0 <= i__2 ? i__2 : s_rnge(
		    "have", i__2, "timout_", (ftnlen)1956)]) {
		tvec[0] = myet;

/*              Most components are handled in the next block. */

		if (type__ == 3 || type__ == 4 || type__ == 11 || type__ == 
			48 || type__ == 19 || type__ == 12 || type__ == 51 || 
			type__ == 21 || type__ == 50 || type__ == 47 || 
			type__ == 20 || type__ == 22) {
		    ttrans_(bastyp, ymdfmt, tvec, (ftnlen)16, (ftnlen)8);

/*                 The seconds component is finished.  Regardless */
/*                 of any zone or calendar modifications, we just */
/*                 don't have to deal with this component any more. */

		    values[21] = tvec[5];

/*                 If we need to deal with time zones, this is */
/*                 the time to do it. */

		    if (timtyp == id[39]) {
			tvec[3] += hoff;
			tvec[4] += moff;
			tvec[5] = 0.;
			ttrans_("YMDF", "YMDF", tvec, (ftnlen)4, (ftnlen)4);
		    }

/*                 One way or the other the hours and minutes components */
/*                 are finished.  Record their values. */

		    values[20] = tvec[3];
		    values[19] = tvec[4];
		    if (values[20] == 0.) {
			values[46] = 12.;
		    } else if (values[20] > 12.) {
			values[46] = values[20] - 12.;
		    } else {
			values[46] = values[20];
		    }

/*                 Finally, if we need to change the calendar to */
/*                 Julian this is the place to handle it. */

		    jyear = i_dnnt(tvec);
		    jmonth = i_dnnt(&tvec[1]);
		    jday = i_dnnt(&tvec[2]);
		    gr2jul_(&jyear, &jmonth, &jday, &jdoy);
		    gyear = jyear;
		    gmonth = jmonth;
		    gday = jday;
		    jul2gr_(&gyear, &gmonth, &gday, &gdoy);
		    if (caltyp == id[42]) {
			values[2] = (doublereal) gyear;
			values[10] = (doublereal) gmonth;
			values[18] = (doublereal) gday;
			values[11] = (doublereal) gdoy;
			go2jul = FALSE_;
		    } else if (caltyp == id[41]) {
			values[2] = (doublereal) jyear;
			values[10] = (doublereal) jmonth;
			values[18] = (doublereal) jday;
			values[11] = (doublereal) jdoy;
			go2jul = TRUE_;
		    } else if (caltyp == id[43]) {
			if (gyear < 1582) {
			    go2jul = TRUE_;
			} else if (gyear > 1582) {
			    go2jul = FALSE_;
			} else if (gmonth < 10) {
			    go2jul = TRUE_;
			} else if (gmonth > 10) {
			    go2jul = FALSE_;
			} else if (gday >= 15) {
			    go2jul = FALSE_;
			} else {
			    go2jul = TRUE_;
			}
			if (go2jul) {
			    values[2] = (doublereal) jyear;
			    values[10] = (doublereal) jmonth;
			    values[18] = (doublereal) jday;
			    values[11] = (doublereal) jdoy;
			} else {
			    values[2] = (doublereal) gyear;
			    values[10] = (doublereal) gmonth;
			    values[18] = (doublereal) gday;
			    values[11] = (doublereal) gdoy;
			}
		    }

/*                 Determine the era associated with the epoch.  Also */
/*                 if the year component is negative, we handle  that */
/*                 now. */

/*                 We store the actual value of the year so that */
/*                 it can be used when determining rounding of */
/*                 other components. */

		    values[51] = values[2];
		    if (doera) {
			if (values[2] < 1.) {
			    values[2] = 1. - values[2];
			    values[49] = 1.;
			} else {
			    values[49] = 2.;
			}
			vanish = values[2] >= 1e3;
		    }

/*                 Fetch the last two digits of the year. */

		    rmaind_(&values[2], &c_b343, &x, &tempd);
		    values[3] = tempd;
		    have[2] = TRUE_;
		    have[3] = TRUE_;
		    have[11] = TRUE_;
		    have[10] = TRUE_;
		    have[47] = TRUE_;
		    have[18] = TRUE_;
		    have[20] = TRUE_;
		    have[19] = TRUE_;
		    have[21] = TRUE_;
		    have[46] = TRUE_;
		    have[49] = TRUE_;
		} else if (type__ == 49) {
		    tvec[0] = myet;
		    ttrans_(bastyp, ywfmt, tvec, (ftnlen)16, (ftnlen)8);

/*                 Weekday. If we need to deal with time zones, this is */
/*                 the time to do it. */

		    if (timtyp == id[39]) {
			tvec[4] += hoff;
			tvec[5] += moff;
			tvec[6] = 0.;
			ttrans_("YMWDF", "YMWDF", tvec, (ftnlen)5, (ftnlen)5);
		    }
		    values[48] = tvec[3];
		    have[48] = TRUE_;
		} else if (type__ == 31 || type__ == 30) {

/*                 The only way to get here is if the output time */
/*                 type is UTC or a time zone (otherwise we'd */
/*                 already HAVE SP2000 and SP1950). */

		    tvec[0] = myet;
		    ttrans_(bastyp, "FORMAL", tvec, (ftnlen)16, (ftnlen)6);
		    values[29] = tvec[0];
		    values[30] = values[29] + spd_() * (j2000_() - j1950_());
		    have[29] = TRUE_;
		    have[30] = TRUE_;
		} else if (type__ == 25) {

/*                 The same tale can be told here as in the last */
/*                 case.  We can only get here if this is UTC */
/*                 output. */

		    tvec[0] = myet;
		    ttrans_(bastyp, "JDUTC", tvec, (ftnlen)16, (ftnlen)5);
		    values[24] = tvec[0];
		    have[24] = TRUE_;
		}
	    }

/*           O.K. whatever thing we are about to construct, we now */
/*           have it's numeric value.  It's time to construct its */
/*           string  value. */


/*           We need to treat character months, weekdays, eras, a.m.'s */
/*           and p.m.'s specially. */

	    if (type__ == 48) {
		indx = i_dnnt(&values[10]);
		s_copy(mymon, months + ((i__2 = indx - 1) < 12 && 0 <= i__2 ? 
			i__2 : s_rnge("months", i__2, "timout_", (ftnlen)2195)
			) * 9, (ftnlen)9, (ftnlen)9);
		montyp = ident[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
			s_rnge("ident", i__2, "timout_", (ftnlen)2196)];

/*              There is no ELSE case in the block below because all of */
/*              the possible MONTYP values are checked explicitly. */

		if (montyp == id[4]) {
		    ucase_(mymon, mymon, (ftnlen)9, (ftnlen)9);
		    s_copy(mymon + 3, " ", (ftnlen)6, (ftnlen)1);
		    mylen = 3;
		} else if (montyp == id[5]) {
		    s_copy(mymon + 3, " ", (ftnlen)6, (ftnlen)1);
		    mylen = 3;
		} else if (montyp == id[6]) {
		    lcase_(mymon, mymon, (ftnlen)9, (ftnlen)9);
		    s_copy(mymon + 3, " ", (ftnlen)6, (ftnlen)1);
		    mylen = 3;
		} else if (montyp == id[8]) {
		    mylen = mlen[(i__2 = indx - 1) < 12 && 0 <= i__2 ? i__2 : 
			    s_rnge("mlen", i__2, "timout_", (ftnlen)2214)];
		} else if (montyp == id[7]) {
		    ucase_(mymon, mymon, (ftnlen)9, (ftnlen)9);
		    mylen = mlen[(i__2 = indx - 1) < 12 && 0 <= i__2 ? i__2 : 
			    s_rnge("mlen", i__2, "timout_", (ftnlen)2217)];
		} else if (montyp == id[9]) {
		    lcase_(mymon, mymon, (ftnlen)9, (ftnlen)9);
		    mylen = mlen[(i__2 = indx - 1) < 12 && 0 <= i__2 ? i__2 : 
			    s_rnge("mlen", i__2, "timout_", (ftnlen)2220)];
		}
		s_copy(string + (appnd - 1), mymon, 256 - (appnd - 1), (
			ftnlen)9);
		appnd += mylen;
		making = FALSE_;
	    } else if (type__ == 49) {
		indx = i_dnnt(&values[48]);
		s_copy(mywkd, wkdays + ((i__2 = indx - 1) < 7 && 0 <= i__2 ? 
			i__2 : s_rnge("wkdays", i__2, "timout_", (ftnlen)2230)
			) * 9, (ftnlen)9, (ftnlen)9);
		wktyp = ident[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
			s_rnge("ident", i__2, "timout_", (ftnlen)2231)];

/*              There is no ELSE case in the block below because all of */
/*              the possible WKTYP values are checked explicitly. */

		if (wktyp == id[12]) {
		    ucase_(mywkd, mywkd, (ftnlen)9, (ftnlen)9);
		    s_copy(mywkd + 3, " ", (ftnlen)6, (ftnlen)1);
		    mylen = 3;
		} else if (wktyp == id[13]) {
		    s_copy(mywkd + 3, " ", (ftnlen)6, (ftnlen)1);
		    mylen = 3;
		} else if (wktyp == id[14]) {
		    lcase_(mywkd, mywkd, (ftnlen)9, (ftnlen)9);
		    s_copy(mywkd + 3, " ", (ftnlen)6, (ftnlen)1);
		    mylen = 3;
		} else if (wktyp == id[16]) {
		    mylen = wklen[(i__2 = indx - 1) < 7 && 0 <= i__2 ? i__2 : 
			    s_rnge("wklen", i__2, "timout_", (ftnlen)2249)];
		} else if (wktyp == id[15]) {
		    ucase_(mywkd, mywkd, (ftnlen)9, (ftnlen)9);
		    mylen = wklen[(i__2 = indx - 1) < 7 && 0 <= i__2 ? i__2 : 
			    s_rnge("wklen", i__2, "timout_", (ftnlen)2252)];
		} else if (wktyp == id[17]) {
		    lcase_(mywkd, mywkd, (ftnlen)9, (ftnlen)9);
		    mylen = wklen[(i__2 = indx - 1) < 7 && 0 <= i__2 ? i__2 : 
			    s_rnge("wklen", i__2, "timout_", (ftnlen)2255)];
		}
		s_copy(string + (appnd - 1), mywkd, 256 - (appnd - 1), (
			ftnlen)9);
		appnd += mylen;
		making = FALSE_;
	    } else if (type__ == 50) {
		if (values[49] == 2. && (ident[(i__2 = i__ - 1) < 100 && 0 <= 
			i__2 ? i__2 : s_rnge("ident", i__2, "timout_", (
			ftnlen)2265)] == id[33] || ident[(i__3 = i__ - 1) < 
			100 && 0 <= i__3 ? i__3 : s_rnge("ident", i__3, "tim"
			"out_", (ftnlen)2265)] == id[35])) {
		    s_copy(string + (appnd - 1), " A.D.", 256 - (appnd - 1), (
			    ftnlen)5);
		} else if (values[49] == 2. && (ident[(i__2 = i__ - 1) < 100 
			&& 0 <= i__2 ? i__2 : s_rnge("ident", i__2, "timout_",
			 (ftnlen)2271)] == id[34] || ident[(i__3 = i__ - 1) < 
			100 && 0 <= i__3 ? i__3 : s_rnge("ident", i__3, "tim"
			"out_", (ftnlen)2271)] == id[36])) {
		    s_copy(string + (appnd - 1), " a.d.", 256 - (appnd - 1), (
			    ftnlen)5);
		} else if (values[49] == 1. && (ident[(i__2 = i__ - 1) < 100 
			&& 0 <= i__2 ? i__2 : s_rnge("ident", i__2, "timout_",
			 (ftnlen)2276)] == id[33] || ident[(i__3 = i__ - 1) < 
			100 && 0 <= i__3 ? i__3 : s_rnge("ident", i__3, "tim"
			"out_", (ftnlen)2276)] == id[35])) {
		    s_copy(string + (appnd - 1), " B.C.", 256 - (appnd - 1), (
			    ftnlen)5);
		} else {
		    s_copy(string + (appnd - 1), " b.c.", 256 - (appnd - 1), (
			    ftnlen)5);
		}

/*              If we have the vanishing kind of era, and we've */
/*              determined that it needs to vanish, then blank out the */
/*              portion of the string we just filled in. and don't */
/*              increment the place holder. */

		if (ident[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"ident", i__2, "timout_", (ftnlen)2293)] == id[35] || 
			ident[(i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : 
			s_rnge("ident", i__3, "timout_", (ftnlen)2293)] == id[
			36]) {
		    if (vanish) {
			s_copy(string + (appnd - 1), " ", 256 - (appnd - 1), (
				ftnlen)1);
			++appnd;
		    } else {
			appnd += 6;
		    }
		} else {
		    ljust_(string + (appnd - 1), string + (appnd - 1), 256 - (
			    appnd - 1), 256 - (appnd - 1));
		    appnd += 4;
		}
		making = FALSE_;
	    } else if (type__ == 51) {
		if (ident[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
			"ident", i__2, "timout_", (ftnlen)2313)] == id[37] && 
			values[20] >= 12.) {
		    s_copy(string + (appnd - 1), "P.M.", 256 - (appnd - 1), (
			    ftnlen)4);
		} else if (ident[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
			s_rnge("ident", i__2, "timout_", (ftnlen)2318)] == id[
			37] && values[20] < 12.) {
		    s_copy(string + (appnd - 1), "A.M.", 256 - (appnd - 1), (
			    ftnlen)4);
		} else if (ident[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
			s_rnge("ident", i__2, "timout_", (ftnlen)2323)] == id[
			38] && values[20] >= 12.) {
		    s_copy(string + (appnd - 1), "p.m.", 256 - (appnd - 1), (
			    ftnlen)4);
		} else {
		    s_copy(string + (appnd - 1), "a.m.", 256 - (appnd - 1), (
			    ftnlen)4);
		}
		appnd += 4;
		making = FALSE_;
	    } else {
		value = values[(i__2 = type__ - 1) < 52 && 0 <= i__2 ? i__2 : 
			s_rnge("values", i__2, "timout_", (ftnlen)2339)];
	    }

/*           If we are now creating a format string, we should */
/*           construct the previous time representation and */
/*           the next for this component (just in case we need it */
/*           later). */

	    if (making) {

/*              We store the value of our current type in the */
/*              CURRNT slot of the values array.  This value */
/*              is used by the single numeric types, JD, SP2000, */
/*              and SP1950. */

		values[0] = values[(i__2 = type__ - 1) < 52 && 0 <= i__2 ? 
			i__2 : s_rnge("values", i__2, "timout_", (ftnlen)2357)
			];

/*              Here's how this works:  We will copy all of */
/*              the components of the time representation up to */
/*              the current one.  This is the truncated representation */
/*              of our epoch.  We then copy these same components into */
/*              another time vector, but add an increment to the */
/*              component corresponding to the one we are dealing with */
/*              now.  We use an increment of 0 for those components that */
/*              already contain their fractional part. We use an */
/*              increment of 1 for the components that typically have */
/*              integer representations. */


/*              Zero out the previous and next time vectors so we won't */
/*              have to do it when we are filling in the truncated */
/*              portions. */

		for (j = 1; j <= 7; ++j) {
		    ptvec[(i__2 = j - 1) < 8 && 0 <= i__2 ? i__2 : s_rnge(
			    "ptvec", i__2, "timout_", (ftnlen)2377)] = 0.;
		    ntvec[(i__2 = j - 1) < 8 && 0 <= i__2 ? i__2 : s_rnge(
			    "ntvec", i__2, "timout_", (ftnlen)2378)] = 0.;
		}
		if (type__ == 3 || type__ == 4) {
		    stopat = 1;
		    timfmt = 1;
		    s_copy(intyp, ymdfmt, (ftnlen)16, (ftnlen)8);
		    incr = 1.;
		} else if (type__ == 11) {
		    stopat = 2;
		    timfmt = 1;
		    s_copy(intyp, ymdfmt, (ftnlen)16, (ftnlen)8);
		    incr = 1.;
		} else if (type__ == 19 || type__ == 12) {
		    stopat = 3;
		    timfmt = 1;
		    s_copy(intyp, ymdfmt, (ftnlen)16, (ftnlen)8);
		    incr = 1.;
		} else if (type__ == 21 || type__ == 47) {

/*                 Note that in this case (and the next 2) that if we */
/*                 an HOUR component, we had to get it either from */
/*                 a Day of Year format or from a Year Month Day */
/*                 format. Thus we have all of the more significant */
/*                 components for this format. */

		    stopat = 4;
		    timfmt = 1;
		    s_copy(intyp, ymdfmt, (ftnlen)16, (ftnlen)8);
		    incr = 1.;
		} else if (type__ == 20) {
		    stopat = 5;
		    timfmt = 1;
		    s_copy(intyp, ymdfmt, (ftnlen)16, (ftnlen)8);
		    incr = 1.;
		} else if (type__ == 22) {
		    stopat = 6;
		    timfmt = 1;
		    s_copy(intyp, ymdfmt, (ftnlen)16, (ftnlen)8);
		    incr = 0.;
		} else if (type__ == 25) {
		    stopat = 1;
		    timfmt = 2;
		    incr = 0.;
		    if (timtyp == id[27] || timtyp == id[28]) {
			s_copy(intyp, "JDTDT", (ftnlen)16, (ftnlen)5);
		    } else if (timtyp == id[26]) {
			s_copy(intyp, "JDTDB", (ftnlen)16, (ftnlen)5);
		    } else if (timtyp == id[25] || timtyp == id[39]) {
			s_copy(intyp, "JDUTC", (ftnlen)16, (ftnlen)5);
		    }
		} else {

/*                 The only types left are the continuous (numeric) */
/*                 types. */

		    stopat = 1;
		    timfmt = 2;
		    incr = 0.;
		    s_copy(intyp, bastyp, (ftnlen)16, (ftnlen)16);
		}

/*              Ok.  We are now ready to construct the previous */
/*              and next time vectors. */

		i__2 = stopat;
		for (j = 1; j <= i__2; ++j) {
		    ptvec[(i__3 = j - 1) < 8 && 0 <= i__3 ? i__3 : s_rnge(
			    "ptvec", i__3, "timout_", (ftnlen)2469)] = values[
			    (i__5 = compnt[(i__4 = j + (timfmt << 3) - 9) < 
			    16 && 0 <= i__4 ? i__4 : s_rnge("compnt", i__4, 
			    "timout_", (ftnlen)2469)] - 1) < 52 && 0 <= i__5 ?
			     i__5 : s_rnge("values", i__5, "timout_", (ftnlen)
			    2469)];
		    ntvec[(i__3 = j - 1) < 8 && 0 <= i__3 ? i__3 : s_rnge(
			    "ntvec", i__3, "timout_", (ftnlen)2470)] = ptvec[(
			    i__4 = j - 1) < 8 && 0 <= i__4 ? i__4 : s_rnge(
			    "ptvec", i__4, "timout_", (ftnlen)2470)];
		}
		ntvec[(i__2 = stopat - 1) < 8 && 0 <= i__2 ? i__2 : s_rnge(
			"ntvec", i__2, "timout_", (ftnlen)2473)] = ntvec[(
			i__3 = stopat - 1) < 8 && 0 <= i__3 ? i__3 : s_rnge(
			"ntvec", i__3, "timout_", (ftnlen)2473)] + incr;

/*              If the type is a year or month, then we need to set */
/*              the month to 1, so that we will be working with */
/*              beginnings of years not beginning of last months of */
/*              the previous year. */

		if (type__ == 3 || type__ == 4) {
		    ptvec[1] = 1.;
		    ntvec[1] = 1.;
		    ptvec[2] = 1.;
		    ntvec[2] = 1.;
		} else if (type__ == 11) {
		    ptvec[2] = 1.;
		    ntvec[2] = 1.;
		}
		if (go2jul && timfmt != 2) {

/*                 Convert both PTVEC and NTVEC to the gregorian */
/*                 calendar */

		    jyear = i_dnnt(ptvec);
		    jmonth = i_dnnt(&ptvec[1]);
		    jday = i_dnnt(&ptvec[2]);
		    jul2gr_(&jyear, &jmonth, &jday, &jdoy);
		    ptvec[0] = (doublereal) jyear;
		    ptvec[1] = (doublereal) jmonth;
		    ptvec[2] = (doublereal) jday;
		    jyear = i_dnnt(ntvec);
		    jmonth = i_dnnt(&ntvec[1]);
		    jday = i_dnnt(&ntvec[2]);
		    jul2gr_(&jyear, &jmonth, &jday, &jdoy);
		    ntvec[0] = (doublereal) jyear;
		    ntvec[1] = (doublereal) jmonth;
		    ntvec[2] = (doublereal) jday;
		}

/*              Handle the +/- time zone shifts. */

		if (dozone && timfmt != 2) {
		    ptvec[3] -= hoff;
		    ntvec[3] -= hoff;
		    ptvec[4] -= moff;
		    ntvec[4] -= moff;
		    ptvec[5] = 0.;
		    ntvec[5] = 0.;
		    ttrans_("YMDF", "YMDF", ptvec, (ftnlen)4, (ftnlen)4);
		    ttrans_("YMDF", "YMDF", ntvec, (ftnlen)4, (ftnlen)4);
		    if (type__ == 22) {
			ptvec[5] = values[21];
			ntvec[5] = values[21];
		    }
		}
	    }
	}
    }

/*     All that's left to do is to copy the constructed string */
/*     to the output string. */

    s_copy(output, string, output_len, (ftnlen)256);
    chkout_("TIMOUT", (ftnlen)6);
    return 0;
} /* timout_ */

