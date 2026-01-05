/*

-Procedure jyear_c ( Seconds per julian year )

-Abstract

   Return the number of seconds in a julian year.

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

   SpiceDouble jyear_c ( void )

/*

-Brief_I/O

   The function returns the number of seconds per julian year.

-Detailed_Input

   None.

-Detailed_Output

   The function returns the number of seconds per julian year.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The julian year is often used as a fundamental unit of time when
   dealing with ephemeris data. For this reason its value in terms of
   ephemeris seconds is recorded in this function.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Display the number of seconds in a Julian year.

      Example code begins here.


      /.
         Program jyear_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Display the number of seconds in a Julian Year, in 16.3 floating
         point format
         ./
         printf ( "Seconds per Julian year: %16.3f\n", jyear_c() );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Seconds per Julian year:     31557600.000


   2) Suppose you wish to compute the number of julian centuries
      that have elapsed since the ephemeris epoch J1950 (beginning
      of the julian year 1950) at a particular UTC epoch.

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program jyear_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants.
         ./
         #define        UTCSTR        "2044-JUL-31"

         /.
         Local variables.
         ./
         SpiceDouble             et;
         SpiceDouble             centur;

         /.
         Load the LSK file.
         ./
         furnsh_c ( "naif0012.tls" );

         /.
         Convert input UTC string to Ephemeris Time.
         ./
         str2et_c ( UTCSTR, &et );
         printf ( "Input ephemeris time      : %16.3f\n", et );


         centur = ( et - unitim_c ( j1950_c(), "JED", "ET" ) );
         centur = centur / ( 100.0 * jyear_c() );

         printf ( "Centuries past J1950 epoch: %16.10f\n", centur );

         return ( 0 );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input ephemeris time      :   1406808069.183
      Centuries past J1950 epoch:     0.9457905763


-Restrictions

   None.

-Literature_References

   [1]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the
        Astronomical Almanac," Page 8, University Science Books,
        1992.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 06-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code examples.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (WLT)

-Index_Entries

   Number of seconds per julian year

-&
*/

{ /* Begin jyear_c */

   return 31557600.0;

} /* End jyear_c */
