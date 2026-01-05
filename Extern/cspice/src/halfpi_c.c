/*

-Procedure halfpi_c ( Half the value of pi )

-Abstract

   Return half the value of pi (the ratio of the circumference of
   a circle to its diameter).

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

   #include <math.h>
   #include "SpiceUsr.h"

   SpiceDouble halfpi_c ( void )

/*

-Brief_I/O

   The function returns half the value of pi.

-Detailed_Input

   None.

-Detailed_Output

   The function returns half the value of pi (the ratio of
   a circle's circumference to its diameter), determined by
   the ACOS function. That is,

         halfpi_c = acos ( -1.0 ) * 0.50

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The first time the function is referenced, the value is computed
   as shown above. The value is saved, and returned directly upon
   subsequent reference.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following code example returns the double precision value of
      the constant pi/2.0 and prints it out.

      Example code begins here.


      /.
         Program halfpi_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main ()
      {
         /.
         Print the double precision value of pi/2.0
         ./
         printf( "Half pi: %25.22f\n", halfpi_c( ) );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Half pi:  1.5707963267948965579990


   2) Compute the transformation from inertial to body fixed
      coordinates, given the directions of the north pole and prime
      meridian of the body.

      When using the following values for Pluto, extracted from the
      PCK kernel pck00010.tpc:

         Right ascension (deg): 132.993
         Declination     (deg):  -6.163
         Prime meridian  (deg): 302.695

      at ephemeris epoch 2000 Jan 1 12:00:00 TDB, the result should
      match that obtained using the following call:

         pxform_c ( "J2000", "IAU_PLUTO", 0.0, tipm );

      Use the PCK kernel below to load the triaxial ellipsoidal shape
      model and orientation data for Pluto.

         pck00010.tpc


      Example code begins here.


      /.
         Program halfpi_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main ( )
      {
         /.
         Compute the transformation from inertial to body
         fixed coordinates, given the directions of the north
         pole and prime meridian of the body.

         Local variables.
         ./
         SpiceDouble   ra;
         SpiceDouble   dec;
         SpiceDouble   w;
         SpiceDouble   tipm [3][3];

         /.
         Load the PCK.
         ./
         furnsh_c ( "pck00010.tpc" );

         /.
         Assign the values for Pluto, in radians.
         ./
         ra  = 132.993 * rpd_c( );
         dec =  -6.163 * rpd_c( );
         w   = 302.695 * rpd_c( );

         /.
         The transformation is defined by the compound
         rotation

           [W] [pi/2 - Dec] [RA + pi/2]
              3            1           3
         ./
         rotate_c (        ra + halfpi_c(), 3, tipm );
         rotmat_c ( tipm, halfpi_c() - dec, 1, tipm );
         rotmat_c ( tipm, w,                3, tipm );

         /.
         Print the results
         ./
         printf( "Rotation matrix, from pole direction "
                 "and prime\nmeridian:\n"                  );
         printf( "   %12.6f %12.6f %12.6f\n",
                        tipm[0][0], tipm[1][0], tipm[2][0] );
         printf( "   %12.6f %12.6f %12.6f\n",
                        tipm[0][1], tipm[1][1], tipm[2][1] );
         printf( "   %12.6f %12.6f %12.6f\n",
                        tipm[0][2], tipm[1][2], tipm[2][2] );

         /.
         Use pxform_c to obtain the same transformation.
         ./
         pxform_c ( "J2000", "IAU_PLUTO", 0.0, tipm );
         printf( "\nRotation matrix, from pxform_c:\n"     );
         printf( "   %12.6f %12.6f %12.6f\n",
                        tipm[0][0], tipm[1][0], tipm[2][0] );
         printf( "   %12.6f %12.6f %12.6f\n",
                        tipm[0][1], tipm[1][1], tipm[2][1] );
         printf( "   %12.6f %12.6f %12.6f\n",
                        tipm[0][2], tipm[1][2], tipm[2][2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Rotation matrix, from pole direction and prime
      meridian:
            -0.333489    -0.655091    -0.677968
            -0.434428    -0.531449     0.727210
            -0.836694     0.537045    -0.107357

      Rotation matrix, from pxform_c:
            -0.333489    -0.655091    -0.677968
            -0.434428    -0.531449     0.727210
            -0.836694     0.537045    -0.107357


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Created simple example
       for retrieving pi/2.0 value, and converted code fragment into full
       example code.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries

   half the value of pi

-&
*/

{ /* Begin halfpi_c */

   /*
   Local Variables
   */

   static SpiceDouble  value = 0.;


   if ( value == 0.)
      {
      value = 0.5 * acos( -1. );
      }


   return value;


} /* End halfpi_c */
