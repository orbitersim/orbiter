/*

-Procedure timout_c ( Time Output )

-Abstract

   Convert an input epoch represented in TDB seconds past the TDB
   epoch of J2000 to a character string formatted to the
   specifications of a user's format picture.

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


   void timout_c ( SpiceDouble       et,
                   ConstSpiceChar  * pictur,
                   SpiceInt          outlen,
                   SpiceChar       * output )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   et         I   Epoch in seconds past the ephemeris epoch J2000.
   pictur     I   A format specification for the output string.
   outlen     I   Maximum length of output string `output'.
   output     O   A string representation of the input epoch.

-Detailed_Input

   et          is a double precision representation of time in seconds
               past the ephemeris epoch J2000.

   pictur      is a string that specifies how the output should be
               presented. The string is made up of various markers
               that stand for various components associated with
               a time.

               There are five types of markers that may appear in a
               format picture. These are String Markers, Numeric
               Markers, Meta markers, Modifier Markers and Literal
               Markers.

               The `pictur' string is examined and the various markers
               are identified. The output time string is constructed
               by replacing each of the identified markers with
               an appropriate time component.

               The various markers and their meanings are discussed
               in the -Particulars section below.

               Note that leading and trailing blanks in `pictur' are
               ignored.

   outlen      is the maximum allowed length of the output string
               `output'. This length must be large enough to hold the
               output string plus the null-terminator character. If the
               output string is expected to have N characters, `outlen'
               should be at least N+1.

-Detailed_Output

   output      is a time string equivalent to the input epoch `et',
               matching the format specified by `pictur'.

-Parameters

   None.

-Exceptions

   1)  A leapseconds kernel must be loaded via the routine furnsh_c
       before calling this routine. If a leapsecond kernel has not
       been loaded, an error is signaled by a routine in the call
       tree of this routine.

   2)  If `pictur' contains the numeric marker "YYYY" and the magnitude
       of year is too large to be displayed as a four-digit integer,
       timout_c will replace it by "****".

   3)  If the requested precision is higher than 12 decimal places,
       timout_c will truncate the decimal part down to 12, and `output'
       will have all the remaining digits in the decimal part set to
       zero.

   4)  Double colon (::), when is not part of one of the supported
       markers, has no effect and will be presented as is on the
       output string.

   5)  If the `pictur' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `pictur' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   7)  If the `output' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   8)  If the `output' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   A leapseconds kernel must be "loaded" via the routine furnsh_c
   prior to calling timout_c.

-Particulars

   A format picture is simply a string of letters that lets
   timout_c know where various components of a time representation
   should be placed during creation of the time string.
   Here's an example of such a picture:

      MON DD,YYYY  HR:MN:SC.#### (TDB) ::TDB

   Here is a sample of times that would be created by using this
   format.

      JAN 12,1992  12:28:18.2772 (TDB)
      FEB 13,1994  23:18:25.2882 (TDB)
      AUG 21,1995  00:02:00.1881 (TDB)

   As you can see from the samples above, the format picture
   specifies that every time string created should begin with a
   three-letter abbreviation for the month, followed by a space and
   the day of the month. The day of month is followed immediately by
   a comma and the year. The year component is followed by two
   spaces. The next outputs are hours represented as a two digit
   integer, a colon, minutes represented as a two digit integer,
   another colon, and seconds truncated to 4 decimal places and
   having a two digit integer part (rounding can be commanded; see
   the discussion of truncation and rounding below). This is
   followed by a space and the string "(TDB)". The special marker
   "::TDB" in the time picture is a ``invisible'' marker. It is
   used to specify the time system that should be used in creating
   the time string (in this case Barycentric Dynamical Time).

   timout_c does not recognize all of the parts of the time format
   picture in the example above. The list of recognized parts and
   unrecognized parts is shown in the table below.

     Recognized       Unrecognized
     ----------       ------------
     "MON"            " "
     "DD"             ","
     "YYYY"           "  "
     "HR"             ":"
     "MN"             "(TDB)"
     "SC"
     ".####"
     "::TDB"

   The unrecognized parts are called literal markers. They are
   copied exactly as they appear in `pictur' into the output string.
   The recognized parts of the picture are replaced by a
   component of time or, as in the case of "::TDB" are used
   as instructions about the overall properties of the time
   string.

   The full list of recognized markers, their classification
   and meaning are given below.

   MARKER       CLASS     MEANING
   -----------  --------  -----------------------------------------
   ".##..."     modifier  represents a numeric component that
                          immediately precedes this in a decimal
                          format. Number of decimal places
                          equals the number of '#' characters
   "::GCAL"     meta      dates are reported in Gregorian calendar
   "::JCAL"     meta      dates are reported in Julian calendar
   "::MCAL"     meta      dates after 15 October, 1582 are reported
                          in Gregorian calendar; before that
                          dates are reported in Julian calendar

   "::RND"      meta      round output to places specified by
                          least significant component

   "::TDB"      meta      all components should be TDB

   "::TDT"      meta      all components should be TT (TDT)

   "::TT"       meta      all components should be TT (TDT)

   "::TRNC"     meta      truncate all output components (default)
   "::UTC"      meta      all components should be UTC (default)
   "::UTC+h:m"  meta      all components in UTC offset by +h (hours)
                          and +m (minutes) so as to allow time zones.
   "::UTC-h:m"  meta      all components in UTC offset by -h (hours)
                          and -m (minutes) so as to allow time zones.
   "AMPM"       string    String (either "A.M." or "P.M.")
                          indicating whether hours are before
                          or after noon.
   "ampm"       string    String (either "a.m." or "p.m.")
                          indicating whether hours are before
                          or after noon.
   "AP"         numeric   AM/PM equivalents of the hour component
                          of a time.
   "DD"         numeric   Day of month
   "DOY"        numeric   Day of year
   "ERA"        string    String (either "B.C." or "A.D.") giving
                          era associated with an epoch.
   "?ERA?"      string    String: either " B.C. " or " A.D. " if the
                          year is before 1000 A.D. otherwise a
                          blank: " ".
   "era"        string    String (either "b.c." or "a.d.") giving
                          era associated with an epoch.
   "?era?"      string    String: either " b.c. " or " a.d. " if the
                          year is before 1000 A.D. otherwise a
                          blank: " ".
   "HR"         numeric   hour component of time
   "JULIAND"    numeric   Julian date component of time
   "MM"         numeric   numeric representation of month component
   "MN"         numeric   minute component of time
   "MON"        string    upper case three letter abbreviation for
                          month
   "Mon"        string    capitalized three letter abbreviation for
                          month
   "mon"        string    lower case three letter abbreviation for
                          month
   "MONTH"      string    upper case full name of month
   "Month"      string    capitalized full name of month
   "month"      string    lower case full name of month
   "SC"         numeric   seconds component of time
   "SP1950"     numeric   seconds past 1950 component of time
   "SP2000"     numeric   seconds past 2000 component of time
   "YR"         numeric   last two digits of year component of time
   "YYYY"       numeric   year component of time
   "WEEKDAY"    string    upper case day of week
   "Weekday"    string    capitalized day of week
   "weekday"    string    lower case day of week
   "WKD"        string    upper case three letter abbreviation for
                          day of week.
   "Wkd"        string    capitalized three letter abbreviation for
                          day of week.
   "wkd"        string    lower case three letter abbreviation for
                          day of week.

   String Markers

      String markers are portions of the format picture that will
      be replaced with a character string that represents the
      corresponding component of a time.

   Numeric Markers

      Numeric markers are portions of the format picture that will
      be replaced with a decimal string that represents the
      corresponding component of a time.

   Meta Markers

      Meta markers (listed under the class ``meta'' in the
      table above) are used to indicate "global" properties of
      your time string. You may specify time scale and how
      rounding should be performed on the components of time
      in your output string. Meta markers may be placed anywhere
      in your format picture. They do not contribute to placement
      of characters in output time strings. Also there are no
      restrictions on how many meta markers you may place in
      the format picture. However, if you supply conflicting
      `meta' markers (for example "::TDT" and "::TDB") in your
      picture the first marker listed (in left to right order)
      overrules the conflicting marker that appears later in
      the picture.

   Default Meta Markers

      If you do not specify a time system, calendar, or time
      zone through the use of a Meta Marker, timout_c uses the
      values returned by the SPICE routine timdef_c. The default
      time system, calendar returned by timdef_c are UTC and
      the Gregorian calendar. The default time zone returned
      by timdef_c is a blank indicating that no time zone offset
      should be used.

      See the header for the routine timdef_c for a more complete
      discussion of setting and retrieving default values.

   Modifier Markers

      The numeric markers listed in the table above stand
      for integers unless they are modified through use of a
      modifier marker. The strings

         .#
         .##
         .###
         .####

      are used to this end. When a numeric marker is followed
      immediately by one of these modifiers, the corresponding time
      component will be written with the number of decimal places
      indicated by the number of successive occurrences of the
      character '#'. Any numeric token may be modified.

   Rounding vs. Truncation

      The meta markers ::TRNC and ::RND allow you to control
      how the output time picture is rounded. If you specify
      ::TRNC all components of time are simply truncated to
      the precision specified by the marker and any modifier.
      If you specify ::RND the output time is rounded to the
      least significant component of the format picture. The
      default action is truncation.

      Whether an output time string should be rounded or
      truncated depends upon what you plan to do with the
      string. For example suppose you simply want to get the
      calendar date associated with a time and not the time of
      day. Then you probably do not want to round your output.
      Rounding 1992 Dec 31, 13:12:00 to the nearest day
      produces 1993 Jan 1. Thus in this case rounding is probably
      not appropriate.

      However, if you are producing output for plotting using
      Julian Date, seconds past 1950 or seconds past 2000, you will
      probably want your output rounded so as to produce a smoother
      plot.

   Time Systems

      timout_c can produce output strings for epochs relative to
      any of the systems UTC, TT or TDT, or TDB. If you do not
      explicitly specify a time system, timout_c will produce strings
      relative to the time system returned by the SPICE routine
      timdef_c. Unless you call timdef_c and change it, the default time
      system is UTC. However, by using one of the Meta Markers
      ::UTC, ::TT, ::TDT, or ::TDB you may specify that timout_c
      produce time strings relative to the UTC, TT or TDT, or TDB
      system respectively.

   Time Zones

      The meta markers ::UTC+h:m  and ::UTC-h:m  allow you to
      offset UTC times so that you may represent times in a time
      zone other than GMT. For example you can output times in
      Pacific Standard time by placing the meta-marker ::UTC-8 in
      your format picture.

      For instance, if you use the picture

         YYYY Mon DD, HR:MN:SC ::UTC

      you will get output strings such as:

         1995 Jan 03, 12:00:00

      If you use the picture


         YYYY Mon DD, HR:MN:SC ::UTC-8

      you will get output strings such as:

         1995 Jan 03, 04:00:00

      Finally, if you use the picture

         YYYY Mon DD, HR:MN:SC ::UTC-8:15

      you will get output string

         1995 Jan 03, 03:45:00

      Note that the minutes are always added or subtracted based on
      the sign present in the time zone specifier. In the case of
      ::UTC+h:m, minutes are added. In the case ::UTC-h:m, minutes
      are subtracted.

      The unsigned part of the hours component can be no more than
      12. The unsigned part of the minutes component can be no
      more than 59.

   Calendars

      The calendar currently used by western countries is the
      Gregorian calendar. This calendar begins on Oct 15, 1582.
      Prior to Gregorian calendar the Julian calendar was used. The
      last Julian calendar date prior to the beginning of the
      Gregorian calendar is Oct 5, 1582.

      The primary difference between the Julian and Gregorian
      calendars is in the determination of leap years. Nevertheless,
      both can be formally extended backward and forward in time
      indefinitely.

      By default timout_c uses the default calendar returned by
      timdef_c. Under most circumstances this will be the Gregorian
      calendar (::GCAL). However you may specify that timout_c use a
      specific calendar through use of one of the calendar Meta
      Markers. You may specify that timout_c use the Julian calendar
      (::JCAL), the Gregorian calendar (::GCAL)  or a mixture of
      both (::MCAL).

      If you specify ::MCAL, epochs that occur after the beginning
      of the Gregorian calendar will be represented using the
      Gregorian calendar, and epochs prior to the beginning of the
      Gregorian calendar will be represented using the Julian
      calendar.

   Getting Software to Construct Pictures for You.

      Although it is not difficult to construct time format
      pictures, you do need to be aware of the various markers that
      may appear in a format picture.

      There is an alternative means for getting a format picture.
      The routine tpictr_c constructs format pictures from a sample
      time string. For example, suppose you would like your time
      strings to look like the basic pattern of the string below.

         "Fri Jul 26 12:22:09 PDT 1996"

      You can call tpictr_c with this string, and it will create the
      appropriate `pictur' for use with timout_c.

         tpictr_c ( "Fri Jul 26 12:22:09 PDT 1996", LENPIC, LENERR,
                    pictur, &ok, errmsg );

      The result will be:

         "Wkd Mon DD HR:MN:SC (PDT) ::UTC-7"

      Note: not every date that you can read is interpretable by
      tpictr_c. For example, you might be able to understand that
      19960212121116 is Feb 2 1996, 12:11:16. However, tpictr_c
      cannot recognize this string. Thus it is important to check
      the logical output OK to make sure that tpictr_c was able to
      understand the time picture you provided.

      Even thought tpictr_c can not recognize every time pattern that
      has been used by various people, it does recognize nearly all
      patterns that you use when you want to communicate outside
      your particular circle of colleagues.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Given a sample with the format of the UNIX date string
      local to California, create a SPICE time picture for use
      in timout_c.

      Using that SPICE time picture, convert a series of ephemeris
      times to that picture format.

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program timout_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define ERRLEN       400
         #define TIMLEN       65

         /.
         Local variables
         ./
         SpiceBoolean         ok;

         SpiceChar            err    [ERRLEN];
         SpiceChar            pictur [TIMLEN];
         SpiceChar          * sample;
         SpiceChar            timstr [TIMLEN];
         SpiceChar          * utcstr;

         SpiceDouble          et;


         /.
         Load LSK file.
         ./
         furnsh_c ( "naif0012.tls" );

         /.
         Create the required time picture.
         ./
         sample = "Thu Oct 01 11:11:11 PDT 1111";

         tpictr_c ( sample, ERRLEN, TIMLEN, pictur, &ok, err );

         if ( ! ok )
         {

            printf( "Invalid time picture.\n" );
            printf( "%s\n", err );

         }
         else
         {

            /.
            Convert the input UTC time to ephemeris time.
            ./
            utcstr = "24 Mar 2018  16:23:00 UTC";
            str2et_c ( utcstr, &et );

            /.
            Now convert `et' to the desired output format.
            ./
            timout_c ( et, pictur, TIMLEN, timstr );

            printf( "Sample format:  %s\n", sample );
            printf( "Time picture :  %s\n", pictur );
            printf( "\n" );
            printf( "Input UTC    :  %s\n", utcstr );
            printf( "Output       :  %s\n", timstr );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Sample format:  Thu Oct 01 11:11:11 PDT 1111
      Time picture :  Wkd Mon DD HR:MN:SC PDT YYYY ::UTC-7

      Input UTC    :  24 Mar 2018  16:23:00 UTC
      Output       :  Sat Mar 24 09:23:00 PDT 2018


   2) Convert a UTC time to a string that contains both the
      calendar representations of the date as well as the Julian
      date; for example a string of the form:

         "Thu Aug 01 09:47:16 PDT 1996 (2450297.1994 JDUTC)"

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program timout_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define TIMLEN       80

         /.
         Local variables
         ./
         SpiceChar          * pictur;
         SpiceChar            timstr [TIMLEN];
         SpiceChar          * utcstr;

         SpiceDouble          et;

         /.
         Load LSK file.
         ./
         furnsh_c ( "naif0012.tls" );

         /.
         Convert the input UTC time to ephemeris time.
         ./
         utcstr = "24 Mar 2018  16:23:00 UTC";
         str2et_c ( utcstr, &et );

         /.
         Create the required time picture. This could be done
         using tpictr_c.
         ./
         pictur = "Wkd Mon DD HR:MN ::UTC-7 YYYY (JULIAND.#### JDUTC)";

         /.
         Now convert `et' to the desired output format.
         ./
         timout_c ( et, pictur, TIMLEN, timstr );
         printf( "Input UTC:  %s\n", utcstr );
         printf( "Output   :  %s\n", timstr );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input UTC:  24 Mar 2018  16:23:00 UTC
      Output   :  Sat Mar 24 09:23  2018 (2458202.1826 JDUTC)


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.0, 10-AUG-2021 (EDW) (JDR) (NJB)

       Changed the input argument name "lenout" to "outlen" for
       consistency with other routines.

       Corrected typo preventing correct calculation of decimal
       values for HR.###... and MN.###... markers with ::UTC+N:M
       and ::UTC-N:M meta tags.

       Added "::TT" as a time system meta marker equivalent-to/
       alias-for "::TDT". No change to functionality.

       Edited the header to comply with NAIF standard. Converted the
       existing code fragments into complete examples and added
       references to required LSKs.

       Updated -Exceptions section, rewording the existing entry
       and adding three additional cases. Fixed call to tpictr_c in
       -Particulars.

       Corrected header example string: changed token HH to HR.

       Updated "output" argument description in -Detailed_Output.

   -CSPICE Version 1.1.1, 14-AUG-2006 (EDW)

       Edited incorrect description of output. Replace mention of
       ldpool_c with furnsh_c.

   -CSPICE Version 1.1.0, 09-FEB-1998 (NJB)

       Re-implemented routine without dynamically allocated, temporary
       strings. Updated the -Exceptions header section.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   Convert and format d.p. seconds past J2000 as a string

-&
*/

{ /* Begin timout_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "timout_c");


   /*
   Check the input string pictur to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "timout_c", pictur );

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "timout_c", output, outlen );

   /*
   Call our beloved f2c'd routine
   */
   timout_( ( doublereal * ) &et ,
            ( char       * ) pictur,
            ( char       * ) output,
            ( ftnlen       ) strlen(pictur),
            ( ftnlen       ) outlen-1        );

   /*
   The string returned, output, is a Fortranish type string.
   Convert the string to C type.
   */
   F2C_ConvertStr ( outlen, output );


   chkout_c ( "timout_c");

} /* End timout_c */
