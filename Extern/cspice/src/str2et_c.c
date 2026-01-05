/*

-Procedure str2et_c ( String to ET )

-Abstract

   Convert a string representing an epoch to a double precision
   value representing the number of TDB seconds past the J2000
   epoch corresponding to the input epoch.

-Disclaimer

   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
   PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
   TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
   WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
   PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
   SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
   SOFTWARE AND RELATED MATERIALS, HOWEVER USED.

   IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
   BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
   LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
   INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
   REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
   REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.

   RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
   THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
   CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
   ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.

-Required_Reading

   TIME

-Keywords

   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void str2et_c ( ConstSpiceChar * timstr,
                   SpiceDouble    * et   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   timstr     I   A string representing an epoch.
   et         O   The equivalent value in seconds past J2000, TDB.

-Detailed_Input

   timstr      is a string representing an epoch. Virtually all
               common calendar representations are allowed. You may
               specify a time string belonging to any of the
               systems TDB, TDT, UTC. Moreover, you may specify a
               time string relative to a specific UTC based time
               zone.

               The rules used in the parsing of `timstr' are spelled out
               in great detail in the reference document time.req. The
               basics are given in the -Particulars section below.

-Detailed_Output

   et          is the double precision number of TDB seconds past the
               J2000 epoch that corresponds to the input `timstr'.

-Parameters

   None.

-Exceptions

   1)  If the `timstr' input string cannot be recognized as a
       legitimate time string, the error SPICE(UNPARSEDTIME) is
       signaled by a routine in the call tree of this routine.

   2)  If more than one time system is specified as part of the input
       time string, the error SPICE(TIMECONFLICT) is signaled by a
       routine in the call tree of this routine.

   3)  If any component of the input time string is outside the
       normal range of usage, the error SPICE(BADTIMESTRING) is
       signaled by a routine in the call tree of this routine. For
       example, the day January 35 is outside the normal range of
       days in January. The checks applied are spelled out in the
       routine TCHECK.

   4)  If a time zone is specified with hours or minutes components
       that are outside of the normal range, the error
       SPICE(TIMEZONEERROR) is signaled by a routine in the call tree
       of this routine.

   5)  If the `timstr' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `timstr' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This routine computes the ephemeris epoch corresponding to an
   input string. The ephemeris epoch is represented as seconds
   past the J2000 epoch in the time system known as Barycentric
   Dynamical Time (TDB). This time system is also referred to as
   Ephemeris Time (ET) throughout the SPICE Toolkit.

   The variety of ways people have developed for representing
   times is enormous. It is unlikely that any single routine
   can accommodate the wide variety of custom time formats that
   have arisen in various computing contexts. However, we
   believe that this routine will correctly interpret most time
   formats used throughout the planetary science community.
   For example this routine supports ISO time formats and UNIX
   `date` output formats. One obvious omission from the strings
   recognized by this routine are strings of the form

        93234.1829  or 1993234.1829

   Some readers may recognize this as the epoch that is 0.1829
   days past the beginning of the 234'th day of 1993. However,
   many other readers may regard this interpretation as a bit
   obscure.

   Below we outline some of the rules used in the interpretation
   of strings. A more complete discussion of the interpretation
   of strings is given in the reference document time.req.


   Default Behavior
   ----------------

   Consider the string

      1988 June 13, 3:29:48

   There is nothing in this string to indicate what time system
   the date and time belong to. Moreover, there is nothing to
   indicate whether the time is based on a 24-hour clock or
   twelve hour clock.

   In the absence of such indicators, the default interpretation
   of this string is to regard the time of day to be a time on
   a 24-hour clock in the UTC time system. The date is a date
   on the Gregorian Calendar (this is the calendar used in nearly
   all western societies).

   Labels
   ------

   If you add more information to the string, str2et_c can make a
   more informed interpretation of the time string. For example:

      1988 June 13, 3:29:48 P.M.

   is still regarded as a UTC epoch. However, with the addition
   of the "P.M." label it is now interpreted as the same epoch
   as the unlabeled epoch 1988 June 13, 15:29:48. Similarly

      1988 June 13, 12:29:48 A.M.

   is interpreted as

      1988 June 13, 00:29:48

   For the record: 12:00 A.M. corresponds to Midnight (00:00 on the
   24 hour clock.  12:00 P.M. corresponds to Noon. (12:00) on the
   24 hour clock.

   You may add still further indicators to the string. For example

      1988 June 13, 3:29:48 P.M. PST

   is interpreted as an epoch in the Pacific Standard Time system.
   This is equivalent to

      1988 June 13, 07:29:48 UTC

   The following U.S. time zones are recognized.

      EST   --- Eastern Standard Time  ( UTC-5:00 )
      CST   --- Central Standard Time  ( UTC-6:00 )
      MST   --- Mountain Standard Time ( UTC-7:00 )
      PST   --- Pacific Standard Time  ( UTC-8:00 )

      EDT   --- Eastern Daylight Time  ( UTC-4:00 )
      CDT   --- Central Daylight Time  ( UTC-5:00 )
      MDT   --- Mountain Daylight Time ( UTC-6:00 )
      PDT   --- Pacific Daylight Time  ( UTC-7:00 )

   In addition any other time zone may be specified by representing
   its offset from UTC. This notation starts with the letters "UTC"
   followed by a "+" for time zones east of Greenwich and "-" for
   time zones west of Greenwich. This is followed by the number of
   hours to add or subtract from UTC. This is optionally followed
   by a colon ":" and the number of minutes to add or subtract to
   get the local time zone. Thus to specify the time zone of
   Calcutta (which is 5 and 1/2 hours ahead of UTC) you would
   specify the time zone to be UTC+5:30. To specify the time zone
   of Newfoundland (which is 3 and 1/2 hours behind UTC) use the
   offset notation UTC-3:30.

   For the Record:  Leapseconds occur at the same time in all
   time zones. In other words, the seconds component of a time
   string is the same for any time zone as is the seconds
   component of UTC. Thus the following are all legitimate
   ways to represent an epoch of some event that occurred
   in the leapsecond

      1995 December 31  23:59:60.5  (UTC)


      1996 January   1, 05:29:60.5  (UTC+5:30 --- Calcutta Time)
      1995 December 31, 20:29:60.5  (UTC-3:30 --- Newfoundland)
      1995 December 31  18:59:60.5  (EST)
      1995 December 31  17:59:60.5  (CST)
      1995 December 31  16:59:60.5  (MST)
      1995 December 31  15:59:60.5  (PST)


   In addition to specifying time zones, you may specify that the
   string be interpreted as a formal calendar representation in
   either the Barycentric Dynamical Time system (TDB) or the
   Terrestrial Dynamical Time system (TDT).  In These systems there
   are no leapseconds. Times in TDB are written as

      1988 June 13, 12:29:48 TDB

   TDT times are written as:

      1988 June 13, 12:29:48 TDT

   Finally, you may explicitly state that the time system is UTC

      1988 June 13, 12:29:48 UTC.


   Abbreviating Years
   ------------------

   Although it can lead to confusion, many people are in the
   habit of abbreviating years when they write them in dates.
   For example

      99 Jan 13,  12:28:24

   Upon seeing such a string, most of us would regard this
   as being 1999 January 13, 12:28:24 and not January 13 of
   the year 99. This routine interprets years that are less
   than 100 as belonging either to the 1900's or 2000's. Years
   greater than 68 ( 69 - 99 ) are regarded as being an
   abbreviation with the "19" suppressed (1969 - 1999). Years
   smaller than 69 ( 00 - 68 ) are regarded as being an
   abbreviation with the "20" suppressed (2000 - 2068).

   Note that in general it is usually a good idea to write
   out the year. Or if you'd like to save some typing
   abbreviate 1999 as '99.

   If you need to specify an epoch whose year
   is less than 1000, we recommend that you specify the era
   along with the year. For example if you want to specify
   the year 13 A.D. write it as

      13 A.D. Jan 12

   When specifying the era it should immediately follow the year.
   Both the A.D. and B.C. eras are supported.


   Changing Default Behavior
   -------------------------

   As discussed above, if a string is unlabeled, it is regarded
   as representing a string in the UTC time system on the
   Gregorian calendar. In addition abbreviated years are
   regarded as abbreviations of the years from 1969 to 2068.

   You may modify these defaults through the routines timdef_c
   and tsetyr_c.

   You may:

      Set the calendar to be Gregorian, Julian or a mixture of
      two via the timdef_c;

      Set the time system to be UTC, TDB, TDT or any time zone
      via the routine timdef_c;

      Set the range of year abbreviations to be any 100 year
      interval via the routine tsetyr_c.

   See the SPICELIB routine TEXPYR and timdef_c for details on changing
   defaults.

   These alterations affect only the interpretation of unlabeled
   strings. If an input string is labeled the specification
   in the label is used.


   If any component of a date or time is out of range, str2et_c
   regards the string as erroneous. Below is a list of
   erroneous strings and why they are regarded as such.

      1997 Jan 32 12:29:29     --- there are only 31 days in January

      '98 Jan 12 13:29:29 A.M. --- Hours must be between 1 and 12
                                   inclusive when A.M. or P.M. is
                                   specified.

      1997 Feb 29, 12:29:20.0  --- February has only 29 days in
                                   1997. This would be ok if the
                                   year was 1996.


      1992 Mar 12 12:62:20     --- Minutes must be between 0 and 59
                                   inclusive.

      1993 Mar 18 15:29:60.5   --- Seconds is out of range for this
                                   date. It would not be out of
                                   range for Dec 31 23:59:60.5 or
                                   Jun 30 23:59:60.5 because these
                                   can be leapseconds (UTC).

   Specifics On Interpretation of the Input String
   -----------------------------------------------

   The process of examining the string to determine its meaning is
   called "parsing" the string. The string is parsed by first
   determining its recognizable substrings (integers, punctuation
   marks, names of months, names of weekdays, time systems, time
   zones, etc.) These recognizable substrings are called the tokens
   of the input string. The meaning of some tokens are immediately
   determined. For example named months, weekdays, time systems have
   clear meanings. However, the meanings of numeric components must
   be deciphered from their magnitudes and location in the string
   relative to the immediately recognized components of the input
   string.

   To determine the meaning of the numeric tokens in the input
   string, a set of "production rules" and transformations are
   applied to the full set of tokens in the string. These
   transformations are repeated until the meaning of every token
   has been determined, or until further transformations yield
   no new clues into the meaning of the numeric tokens.

   1)  Unless the substring "JD" or "jd" is present, the string is
       assumed to be a calendar format (day-month-year or year and
       day of year). If the substring JD or jd is present, the
       string is assumed to represent a Julian date.

   2)  If the Julian date specifier is not present, any integer
       greater than 999 is regarded as being a year specification.

   3)  A dash "-" can represent a minus sign only if it precedes
       the first digit in the string and the string contains
       the Julian date specifier (JD). (No negative years,
       months, days, etc. are allowed).

   4)  Numeric components of a time string must be separated
       by a character that is not a digit or decimal point.
       Only one decimal component is allowed. For example
       1994219.12819 is sometimes interpreted as the
       219th day of 1994 + 0.12819 days. str2et_c does not
       support such strings.

   5)  No exponential components are allowed. For example you
       can't specify the Julian date of J2000 as 2.451545E6.
       You also can't input 1993 Jun 23 23:00:01.202E-4 and have
       to explicitly list all zeros that follow the decimal
       point: i.e. 1993 Jun 23 23:00:00.0001202.

   6)  The single colon (:) when used to separate numeric
       components of a string is interpreted as separating
       Hours, Minutes, and Seconds of time.

   7)  If a double slash (//) or double colon (::) follows
       a pair of integers, those integers are assumed  to
       represent the year and day of year.

   8)  A quote followed by an integer less than 100 is regarded
       as an abbreviated year. For example: '93 would be regarded
       as the 93rd year of the reference century. See the SPICELIB
       routine TEXPYR for further discussion of abbreviated years.

   9)  An integer followed by "B.C." or "A.D." is regarded as
       a year in the era associated with that abbreviation.

   10  All dates are regarded as belonging to the extended
       Gregorian Calendar (the Gregorian calendar is the calendar
       currently used by western society). See the routine timdef_c
       to modify this behavior.

   11) If the ISO date-time separator (T) is present in the string
       ISO allowed token patterns are examined for a match
       with the current token list. If no match is found the
       search is abandoned and appropriate diagnostic messages
       are generated. Historically the interpretation of ISO 
       formatted time strings deviates from the ISO standard in 
       allowing two digit years and expanding years in the 0 to 99 
       range the same way as is done for non ISO formatted strings. 
       Due to this interpretation it is impossible to specify 
       times in years in the 0 A.D. to 99 A.D. range using ISO 
       formatted strings on the input.

   12) If two delimiters are found in succession in the time
       string, the time string is diagnosed as an erroneous string.
       (Delimiters are comma, white space, dash, slash, period, or
       day of year mark. The day of year mark is a pair of forward
       slashes or a pair of colons.)

       Note the delimiters do not have to be the same. The pair
       of characters ",-" counts as two successive delimiters.

   13) White space and commas serve only to delimit tokens in the
       input string. They do not affect the meaning of any
       of the tokens.

   14) If an integer is greater than 1000 (and the "JD" label
       is not present, the integer is regarded as a year.

   15) When the size of the integer components does not clearly
       specify a year the following patterns are assumed

       Calendar Format

          Year Month Day
          Month Day Year
          Year Day Month

          where Month is the name of a month, not its numeric
          value.

          When integer components are separated by slashes (/)
          as in 3/4/5. Month, Day, Year is assumed (2005 March 4)

       Day of Year Format.

          If a day of year marker is present (// or ::) the
          pattern

            I-I// or I-I:: (where I stands for an integer)

          is interpreted as Year Day-of-Year. However, I-I/ is
          regarded as ambiguous.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose you would like to determine whether your favorite
      time representation is supported by str2et_c. The small
      program below gives you a simple way to experiment with
      str2et_c. (Note that erroneous inputs will be flagged by
      signaling an error.)

      Example code begins here.


      /.
         Program str2et_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"


      int main()
      {

         /.
         Local constants
         ./
         #define FILSIZ          128
         #define TIMSIZ           64

         /.
         Local variables
         ./
         SpiceChar               caldr  [ TIMSIZ ];
         SpiceChar               dayofy [ TIMSIZ ];
         SpiceChar               lsk    [ FILSIZ ];
         SpiceChar               timstr [ TIMSIZ ];

         SpiceDouble             et;


         /.
         First get the name of the leapseconds kernel, and load it.
         ./
         prompt_c ( "Leapseconds kernel: ", FILSIZ, lsk );
         furnsh_c ( lsk );

         /.
         Get the time string.
         ./
         prompt_c ( "Time string: ", TIMSIZ, timstr );

         /.
         Convert the string to ET and then back to UTC calendar
         and day-of-year formats.
         ./
         str2et_c ( timstr, &et );
         et2utc_c ( et, "C", 0, TIMSIZ, caldr  );
         et2utc_c ( et, "D", 0, TIMSIZ, dayofy );

         /.
         Print the results
         ./
         printf ( "\n" );
         printf ( "TBD seconds from J2000 epoch: %f\n", et     );
         printf ( "Calendar    Format:           %s\n", caldr  );
         printf ( "Day of year Format:           %s\n", dayofy );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the LCK file named naif0012.tls and the time
      string "2017-07-14T19:46:00", the output was:


      Leapseconds kernel: naif0012.tls
      Time string: 2017-07-14T19:46:00

      TBD seconds from J2000 epoch: 553333629.183727
      Calendar    Format:           2017 JUL 14 19:46:00
      Day of year Format:           2017-195 // 19:46:00


   2) Below is a sampling of some of the time formats that are
      acceptable as inputs to str2et_c. A complete discussion of
      permissible formats is given in the reference document
      time.req.

      ISO (T) Formats.

      String                        Year Mon  DOY DOM  HR Min Sec
      ----------------------------  ---- ---  --- ---  -- --- ------
      1996-12-18T12:28:28           1996 Dec   na  18  12  28 28
      1986-01-18T12                 1986 Jan   na  18  12  00 00
      1986-01-18T12:19              1986 Jan   na  18  12  19 00
      1986-01-18T12:19:52.18        1986 Jan   na  18  12  19 52.18
      1986-01-18T12:19:52.18Z       1986 Jan   na  18  12  19 52.18
      1995-08T18:28:12              1995  na  008  na  18  28 12
      1995-08T18:28:12Z             1995  na  008  na  18  28 12
      1995-18T                      1995  na  018  na  00  00 00
      0000-01-01T                   1 BC Jan   na  01  00  00 00


      Calendar Formats.

      String                        Year   Mon DOM  HR Min  Sec
      ----------------------------  ----   --- ---  -- ---  ------
      Tue Aug  6 11:10:57  1996     1996   Aug  06  11  10  57
      1 DEC 1997 12:28:29.192       1997   Dec  01  12  28  29.192
      2/3/1996 17:18:12.002         1996   Feb  03  17  18  12.002
      Mar 2 12:18:17.287 1993       1993   Mar  02  12  18  17.287
      1992 11:18:28  3 Jul          1992   Jul  03  11  18  28
      June 12, 1989 01:21           1989   Jun  12  01  21  00
      1978/3/12 23:28:59.29         1978   Mar  12  23  28  59.29
      17JUN1982 18:28:28            1982   Jun  17  18  28  28
      13:28:28.128 1992 27 Jun      1992   Jun  27  13  28  28.128
      1972 27 jun 12:29             1972   Jun  27  12  29  00
      '93 Jan 23 12:29:47.289       1993*  Jan  23  12  29  47.289
      27 Jan 3, 19:12:28.182        2027*  Jan  03  19  12  28.182
      23 A.D. APR 4, 18:28:29.29    0023** Apr  04  18  28  29.29
      18 B.C. Jun 3, 12:29:28.291   -017** Jun  03  12  29  28.291
      29 Jun  30 12:29:29.298       2029+  Jun  30  12  29  29.298
      29 Jun '30 12:29:29.298       2030*  Jun  29  12  29  29.298


      Day of Year Formats.

      String                        Year  DOY HR Min Sec
      ----------------------------  ----  --- -- --- ------
      1997-162::12:18:28.827        1997  162 12  18 28.827
      162-1996/12:28:28.287         1996  162 12  28 28.287
      1993-321/12:28:28.287         1993  231 12  28 28.287
      1992 183// 12:18:19           1992  183 12  18 19
      17:28:01.287 1992-272//       1992  272 17  28 01.287
      17:28:01.282 272-1994//       1994  272 17  28 01.282
      '92-271/ 12:28:30.291         1992* 271 12  28 30.291
      92-182/ 18:28:28.281          1992* 182 18  28 28.281
      182-92/ 12:29:29.192          0182+ 092 12  29 29.192
      182-'92/ 12:28:29.182         1992  182 12  28 29.182


      Julian Date Strings

      jd 28272.291                  Julian Date   28272.291
      2451515.2981 (JD)             Julian Date 2451515.2981
      2451515.2981 JD               Julian Date 2451515.2981

                                    Abbreviations Used in Tables

                                      na    --- Not Applicable
                                      Mon   --- Month
                                      DOY   --- Day of Year
                                      DOM   --- Day of Month
                                      Wkday --- Weekday
                                      Hr    --- Hour
                                      Min   --- Minutes
                                      Sec   --- Seconds

      *  The default interpretation of a year that has been
         abbreviated to two digits with or without a leading quote
         as in 'xy or xy (such as '92 or 92) is to treat the year as
         19xy if xy > 68 and to treat it as 20xy otherwise. Thus '70
         is interpreted as 1970 and '67 is treated as 2067. However,
         you may change the "split point" and centuries through use
         of the CSPICE routine tsetyr_c. See that routine for a
         discussion of how you may reset the split point.

      ** All epochs are regarded as belonging to the Gregorian
         calendar. We formally extend the Gregorian calendar backward
         and forward in time for all epochs. If you have epochs
         belonging to the Julian Calendar, consult the SPICELIB
         routines TPARTV and JUL2GR for a discussion concerning
         conversions to the Gregorian calendar and ET. The routines
         timdef_c and str2et_c, used together, also support conversions
         from Julian Calendar epochs to ET.

      +  When a day of year format or calendar format string is
         input and neither of the integer components of the date is
         greater than 1000, the first integer is regarded as being
         the year.

      Any integer greater than 1000 is regarded as a year
      specification. Thus 1001-1821//12:28:28 is interpreted as
      specifying two years and will be rejected as ambiguous.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   M. Costa Sitja      (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.0, 23-DEC-2021 (JDR) (EDW) (MCS)

       Changed input argument name "str" to "timstr" for consistency
       with other routines.

       Header edits to expand description of ISO format.

       Edited the header comments to comply with NAIF standard.
       Updated code example.

       Comments concerning the default century for abbreviated years
       were updated to reflect changes to texpyr_.

       Replaced references to tpartv_ by time.req.

   -CSPICE Version 1.1.5, 02-NOV-2009 (CHA)

       A few minor grammar fixes in the header.

   -CSPICE Version 1.1.4, 16-JAN-2008 (EDW)

       Corrected typos in header titles:

       Detailed Input to -Detailed_Input.
       Detailed Output to -Detailed_Output.

   -CSPICE Version 1.1.3, 12-NOV-2006 (EDW)

       Added -Parameters section header.

   -CSPICE Version 1.1.2, 29-JUL-2003 (CHA) (NJB)

       Various minor header corrections were made.

   -CSPICE Version 1.1.1, 10-FEB-2002 (NJB)

       Corrected typo in header.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       Re-implemented routine without dynamically allocated, temporary
       strings. -Exceptions section of header was updated.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW) (WLT)

-Index_Entries

   Convert a string to TDB seconds past the J2000 epoch

-&
*/

{ /* Begin str2et_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "str2et_c" );


   /*
   Check the input string timstr to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "str2et_c", timstr );


   /*
   Call the f2c'd Fortran routine.
   */
   str2et_ ( ( char       * ) timstr,
             ( doublereal * ) et,
             ( ftnlen       ) strlen(timstr)  );


   chkout_c ( "str2et_c" );


}/* End str2et_c */
