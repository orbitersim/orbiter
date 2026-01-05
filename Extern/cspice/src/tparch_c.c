/*

-Procedure tparch_c ( Parse check---check format of strings )

-Abstract

   Restrict the set of strings that are recognized by SPICE time
   parsing routines to those that have standard values for all time
   components.

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

   PARSING
   TIME

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void tparch_c ( ConstSpiceChar    * type )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   type       I   String: Use "YES" to restrict time inputs.

-Detailed_Input

   type        is a character string that is used to adjust the set of
               strings that will be regarded as valid time strings by
               SPICE time parsing routines.

               The default behavior of SPICE time software is to allow
               an extended range of values for the various components
               (tokens) of a time string. For example, using its default
               behavior, tparse_c would regard 1993 JAN 367 as a valid
               time string and return the UTC seconds past the J2000
               epoch value that corresponds to Jan 2, 1994.

               While this is a "reasonable" interpretation of such a
               string, there may be occasions when such a string should
               be regarded as an error.

               By calling tparch_c with a value of "YES", the action of
               the time software will be modified. Strings that have
               components that are out of the range of values used in
               most English discourse will be regarded as errors. Thus
               the numeric values of MONTH, DAY, HOUR, MINUTE, and
               SECOND must satisfy the following conditions to be
               regarded as legitimate calendar time strings.

                  ITEM     Valid Range
                  ------   -----------------------------------------
                  MONTH    1 to 13
                  DAY      1 to 365 (366 for leap years) when
                           DAY is interpreted as the day of year
                           i.e. the month token is empty.
                           1 to 31  if month is January
                           1 to 28  (29 in leap years) if month is
                                    February
                           1 to 31  if month is March
                           1 to 30  if month is April
                           1 to 31  if month is May
                           1 to 31  if month is June
                           1 to 30  if month is July
                           1 to 31  if month is August
                           1 to 30  if month is September
                           1 to 31  if month is October
                           1 to 30  if month is November
                           1 to 31  if month is December
                  HOUR     0 to 23
                  MINUTE   0 to 59
                  SECOND   0 up to but not including 60 on days that
                           can not have a leapsecond.
                           0 up to but not including 61 for times
                           that are the last second of June or
                           December. In other words,
                                JUN 30, 23:59:60.xxxxxx...x
                           and  DEC 31, 23:59:60.xxxxxx...x

               To reset the action of time software to the default
               action, set `type' to a value that is not equivalent to
               "YES" when case and spaces are ignored.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If the `type' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   2)  If the `type' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This routine is used to alter the collections of strings
   that SPICE software regards as legitimate calendar strings. The
   default behavior of SPICE software is to accept strings such
   as FEB 34, 1993 and to interpret these in a "natural way"
   (FEB 34, 1993 is regarded as MARCH 6, 1993.) This behavior
   is sometimes useful for "private" programs that you write.
   However, such a string may be a typo (a finger accidentally hit
   two keys for the day instead of one). Given that this string
   does not appear in common usage, you may want to consider
   that it is more likely the result of erroneous input. You
   can alter the behavior of SPICE software so that it will
   treat such a string as an error. To do this call this entry
   point with `type' having the value "YES".

      tparch_c ( "YES" );

   Until the behavior is reset by calling tparch_c with a value
   other than "YES" (such as "NO"), SPICE software will treat all
   out-of-bound components of time strings as errors.

   If you are happy with the SPICE default interpretation of
   strings, you do not need to make any calls to tparch_c.

   All time parsing routines --including the top-level APIs tparse_c
   and utc2et_c-- respect the setting assigned by tparch_c, except the
   CSPICE routine str2et_c.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) When accepting times as input interactively, you usually
      read a string typed at a keyboard and then pass that string
      to the SPICE time system to convert it to an ephemeris time.
      The default behavior of SPICE software is to accept strings
      such as FEB 34, 1993 and to interpret these in a "natural way"
      (FEB 34, 1993 is regarded as MARCH 6, 1993.) The following
      example code demonstrates how to modify this behavior.


      Example code begins here.


      /.
         Program tparch_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define TIMSTR       "FEB 34, 1993"
         #define ERRMLN       1001

         /.
         Local variables.
         ./
         SpiceChar            errmsg [ERRMLN];

         SpiceDouble          sp2000;

         /.
         First, demonstrate the default behavior of SPICE.
         Let's get the number of UTC seconds past J2000 epoch.
         ./
         tparse_c ( TIMSTR, ERRMLN, &sp2000, errmsg );

         if ( !strncmp( errmsg, "", 1 ) )
         {
            printf( "UTC (s):  %17.6f\n", sp2000 );
         }
         else
         {
            printf( "Error  : %s\n", errmsg );
         }

         /.
         Now, turn error checking on and parse the time string
         again.
         ./
         tparch_c ( "YES" );
         tparse_c ( TIMSTR, ERRMLN, &sp2000, errmsg );

         if ( !strncmp( errmsg, "", 1 ) )
         {
            printf( "UTC (s):  %17.6f\n", sp2000 );
         }
         else
         {
            printf( "Error  : %s\n", errmsg );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      UTC (s):  -215352000.000000
      Error  : The day of the month specified for the month of February was ***


      Warning: incomplete output. 1 line extended past the right
      margin of the header and has been truncated. This line is
      marked by "***" at the end of the line.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 01-NOV-2021 (JDR)

-Index_Entries

   Restrict time strings to proper form

-&
*/

{ /* Begin tparch_c */

   /*
   Discovery error tracing. No check-in required.
   */

   /*
   Check the input string `type' to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_DISCOVER, "tparch_c", type   );

   /*
   Call the f2c'd Fortran routine.
   */
   tparch_ (  ( char       * )  type,
              ( ftnlen       )  strlen(type)  );

} /* End tparch_c */
