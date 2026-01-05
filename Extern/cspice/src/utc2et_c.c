/*

-Procedure utc2et_c ( UTC to Ephemeris Time )

-Abstract

   Convert an input time from Calendar or Julian Date format, UTC,
   to ephemeris seconds past J2000.

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

   KERNEL
   TIME

-Keywords

   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void utc2et_c ( ConstSpiceChar  * utcstr,
                   SpiceDouble     * et      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   utcstr     I   Input time string, UTC.
   et         O   Output epoch, ephemeris seconds past J2000.

-Detailed_Input

   utcstr      is an input time string, containing a Calendar or Julian
               Date, UTC. Any input string acceptable to the SPICELIB
               routine TPARTV are acceptable to utc2et_c. The length of
               `utcstr' should not exceed 80 characters.

-Detailed_Output

   et          is the equivalent of `utcstr', expressed in ephemeris
               seconds past J2000. If an error occurs, or if the
               input string is ambiguous, `et' is not changed.

-Parameters

   None.

-Exceptions

   1)  If the input time string is ambiguous, the error
       SPICE(INVALIDTIMESTRING) is signaled by a routine in the call
       tree of this routine.

   2)  This routine does not attempt to account for variations
       in the length of the second that were in effect prior
       to Jan 1, 1972. For days prior to that date, we assume
       there are exactly 86400 ephemeris seconds.

   3)  If the `utcstr' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `utcstr' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   Note: NAIF recommends the use of str2et_c instead of utc2et_c.

   This routine handles that task of converting strings
   representing epochs in the UTC system to ephemeris seconds
   (TDB) past the epoch of the J2000 frame.

   Although this routine is intended primarily for the
   conversion of epochs during the "space age" it may also
   be used to convert much earlier epochs. However, before
   using this routine to convert epochs prior to 1972
   you must be sure that the assumptions made by in the
   implementation are consistent with the accuracy of
   the input calendar string.

   As noted in the -Exceptions section, this routine does not attempt
   to account for variations in the length of the second that were
   used prior to Jan 1, 1972. Instead each "UTC" day prior to
   Jan 1, 1972 is assumed to have exactly 86400 TDT seconds.

   Ancient Epochs
   --------------

   The calendar used today, the Gregorian calendar, has its
   initial epoch on 15 October, 1582. Prior to that epoch the
   Julian calendar was used for the recording of epochs.
   October 15, 1582 (Gregorian) corresponds to
   October 05, 1582 (Julian). From the point of view of the
   implementation of this routine, all epochs belong to the
   Gregorian calendar extended indefinitely backward in time.
   If you need to obtain ephemeris seconds past the J2000 epoch
   from Julian Calendar strings, we suggest that
   you make use of the CSPICE routine str2et_c.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Below is a sampling of some of the time formats that
      are acceptable as inputs to utc2et_c. A complete discussion
      of permissible formats is given in the SPICELIB routine
      TPARTV as well as the User's reference file time.req
      located in the "doc" directory of the toolkit.


      ISO (T) Formats (with or without trailing "Z").

      String                        Year Mon  DOY DOM  HR Min Sec
      ----------------------------  ---- ---  --- ---  -- --- ------
      1996-12-18T12:28:28           1996 Dec   na  18  12  28 28
      1986-01-18T12                 1986 Jan   na  18  12  00 00
      1986-01-18T12:19              1986 Jan   na  18  12  19 00
      1986-01-18T12:19:52.18Z       1986 Jan   na  18  12  19 52.18
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


      Day of Year Formats

      String                        Year  DOY HR Min Sec
      ----------------------------  ----  --- -- --- ------
      1997-162::12:18:28.827        1997  162 12  18 28.827
      162-1996/12:28:28.287         1996  162 12  28 28.287
      1993-321/12:28:28.287         1993  231 12  28 28.287
      1992 183// 12 18 19           1992  183 12  18 19
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
         abbreviated with a leading quote as in 'xy (such as '92) is
         to treat the year as 19xy if xy > 68 and to treat it is
         20xy otherwise. Thus '69 is interpreted as 1969 and '68 is
         treated as 2068. However, you may change the "split point"
         and centuries through use of the CSPICE routine tsetyr_c
         which is a routine in the SPICELIB module TEXPYR. See
         that routine for a discussion of how you may reset the
         split point.

      ** All epochs are regarded as belonging to the Gregorian
         calendar. We formally extend the Gregorian calendar
         backward and forward in time for all epochs. If you have
         epochs belonging to the Julian Calendar, consult the
         SPICELIB routines TPARTV and JUL2GR for a discussion
         concerning conversions to the Gregorian calendar and ET.

      +  When a day of year format or calendar format string is
         input and neither of the integer components of the date is
         greater than 1000, the first integer is regarded as being
         the year.

   2) Convert a calendar date in UTC string format to Julian
      Ephemeris date.

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program utc2et_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants.
         ./
         #define        UTCSTR        "Dec 19 2003  16:48:00"

         /.
         Local variables.
         ./
         SpiceDouble             et;
         SpiceDouble             jed;

         /.
         Load the LSK file.
         ./
         furnsh_c ( "naif0012.tls" );

         /.
         Convert input UTC string to Ephemeris Time.
         ./
         utc2et_c ( UTCSTR, &et );
         printf ( "UTC time             : %s\n", UTCSTR  );
         printf ( "Ephemeris time       : %21.9f\n", et  );

         /.
         Convert the Ephemeris Time to Julian ephemeris date, i.e.
         Julian date relative to TDB time scale.
         ./
         jed = unitim_c ( et, "ET", "JED" );
         printf ( "Julian Ephemeris Date: %21.9f\n", jed );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      UTC time             : Dec 19 2003  16:48:00
      Ephemeris time       :   125124544.183560610
      Julian Ephemeris Date:     2452993.200742865


-Restrictions

   1)  The conversion between ET and UTC depends on the values in the
       input kernel pool. The kernel pool should be loaded prior to
       calling this routine.

   2)  Before using this routine for epochs prior to Jan 1, 1972
       be sure to check the -Particulars section to make sure
       that the assumptions made in this routine are consistent
       with the accuracy you require for your application.

-Literature_References

   [1]  J. Jespersen and J. Fitz-Randolph, "From Sundials to Atomic
        Clocks, Understanding Time and Frequency," Dover
        Publications, Inc. New York, 1982.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.1, 13-AUG-2021 (JDR) (EDW)

       Header edits to expand description of ISO format.

       Edited the header to comply with NAIF standard. Added
       complete code example. Fixed minor typos in first example.

       Added note to -Particulars section.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB) (EDW)

       Re-implemented routine without dynamically allocated, temporary
       strings. Corrected typo in chkout_c module name.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW) (WMO) (WLT) (IMU)

-Index_Entries

   UTC to ephemeris time

-&
*/

{ /* Begin utc2et_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "utc2et_c" );


   /*
   Check the input string utcstr to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "utc2et_c", utcstr );


   /*
   Call the f2c'd routine.
   */
   utc2et_( ( char       * ) utcstr,
            ( doublereal * ) et,
            ( ftnlen       ) strlen(utcstr) );


   chkout_c ( "utc2et_c" );


} /* End utc2et_c */
