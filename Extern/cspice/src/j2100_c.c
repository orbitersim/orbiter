/*

-Procedure j2100_c ( Julian Date of 2100 JAN 1.5 )

-Abstract

   Return the Julian Date of 2100 JAN 01 12:00:00 (2100 JAN 1.5).

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

   None.

-Keywords

   CONSTANTS

*/

   #include "SpiceUsr.h"

   SpiceDouble j2100_c ( void )

/*

-Brief_I/O

   The function returns the Julian Date of 2100 JAN 01 12:00:00
   (2100 JAN 1.5).

-Detailed_Input

   None.

-Detailed_Output

   The function returns 2488070.0, the Julian Date corresponding
   to 2100 JAN 01 12:00:00 (2100 JAN 1.5).

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The function always returns the constant value shown above.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Display the double precision value for the J2100 date.


      Example code begins here.


      /.
         Program j2100_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Display the J2100 date in 16.8 floating point format
         ./
         printf ( "J2100 date: %16.8f\n", j2100_c() );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      J2100 date: 2488070.00000000


   2) Convert an input time in UTC format to TDB seconds past the
      following reference epochs:

         - Besselian date 1900 and 1950; and

         - Julian date 1900, 1950, 2000 and 2100.

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program j2100_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants.
         ./
         #define        UTCSTR        "1991-NOV-26"

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
         str2et_c ( UTCSTR, &et );
         printf ( "Input ephemeris time  : %20.3f\n\n", et );

         /.
         Convert the Ephemeris Time to Julian ephemeris date, i.e.
         Julian date relative to TDB time scale.
         ./
         jed = unitim_c ( et, "ET", "JED" );

         /.
         Convert Julian Date to TDB seconds past the reference epochs
         and output the results.
         ./
         printf ( "TDB seconds past B1900: %20.3f\n",
                  ( jed - b1900_c() ) * spd_c()     );
         printf ( "TDB seconds past B1950: %20.3f\n",
                  ( jed - b1950_c() ) * spd_c()     );
         printf ( "TDB seconds past J1900: %20.3f\n",
                  ( jed - j1900_c() ) * spd_c()     );
         printf ( "TDB seconds past J1950: %20.3f\n",
                  ( jed - j1950_c() ) * spd_c()     );
         printf ( "TDB seconds past J2000: %20.3f\n",
                  ( jed - j2000_c() ) * spd_c()     );
         printf ( "TDB seconds past J2100: %20.3f\n",
                  ( jed - j2100_c() ) * spd_c()     );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input ephemeris time  :       -255614341.817

      TDB seconds past B1900:       2900118570.055
      TDB seconds past B1950:       1322272271.321
      TDB seconds past J1900:       2900145658.183
      TDB seconds past J1950:       1322265658.183
      TDB seconds past J2000:       -255614341.817
      TDB seconds past J2100:      -3411374341.817


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 06-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code examples.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (WLT) (IMU)

-Index_Entries

   julian date of 2100 jan 1.5

-&
*/

{ /* Begin j2100_c */


   return  2488070.0;


} /* End j2100_c */
