/*

-Procedure clight_c ( C, Speed of light in a vacuum )

-Abstract

   Return the speed of light in a vacuum (IAU official
   value, in km/sec).

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

   SpiceDouble clight_c ( void )

/*

-Brief_I/O

   The function returns the speed of light in vacuum (km/sec).

-Detailed_Input

   None.

-Detailed_Output

   The function returns the IAU official value for the speed of light
   in vacuum: 299792.458 km/sec.

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

   1) Display the IAU official double precision value
      for the constant speed of light in the vacuum, in km/sec.

      Example code begins here.


      /.
         Program clight_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Display the value for the constant speed of light in the
         vacuum.
         ./
         printf ( "Speed of light: %12.4f\n", clight_c( ) );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Speed of light:  299792.4580


   2) The following example uses clight_c to determine the one-way
      light-time (tau) to Mars whose geometric position relative to
      the Earth is available.

      Use the SPK kernel below to load the required ephemerides
      for Mars and the Earth.

         de405.bsp


      Example code begins here.


      /.
         Program clight_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants
         ./
         #define  ET             0.0

         /.
         Local variables
         ./
         SpiceDouble             lt;
         SpiceDouble             pos    [ 3 ];
         SpiceDouble             tau;

         /.
         Load the SPK file.
         ./
         furnsh_c ( "de405.bsp" );

         /.
         Obtain the geometric position of Mars relative to the Earth at
         the given ET, in J2000 frame.
         ./
         spkpos_c ( "MARS", ET, "J2000", "NONE", "EARTH", pos, &lt );

         /.
         Compute the one-way light time using clight_c
         ./
         tau = vnorm_c ( pos ) / clight_c ( );

         /.
         Display the one-way light time values obtained from spkpos_c
         and using clight_c.
         ./
         printf ( "One-way light time (spkpos_c): %16.9f\n", lt );
         printf ( "One-way light time (clight_c): %16.9f\n", tau  );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      One-way light time (spkpos_c):    922.961207764
      One-way light time (clight_c):    922.961207764


      Note that the following SPK readers

         spkezr_c
         spkez_c
         spkpos_c
         spkezp_c

      return the one-way light time as an output.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.3, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code example.

   -CSPICE Version 1.0.2, 07-FEB-2008 (EDW) (NJB)

       Corrected typos in header titles:

       Detailed Input to -Detailed_Input
       Detailed Output to -Detailed_Output

       Updated example to show pointer output argument
       `tau' and list other high-level SPK routines that
       return light time. Call to bodmat_c was removed
       from example.

   -CSPICE Version 1.0.1, 11-NOV-2006 (EDW)

       Added -Parameters section header.

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW)

-Index_Entries

   measured velocity of light in a vacuum

-&
*/

{ /* Begin clight_c */

  return 299792.458;

} /* End clight_c */
