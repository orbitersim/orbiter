/*

-Procedure vsep_c  ( Angular separation of vectors, 3 dimensions )

-Abstract

   Find the separation angle in radians between two double
   precision, 3-dimensional vectors. This angle is defined as zero
   if either vector is zero.

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

   ANGLE
   VECTOR

*/

   #include <math.h>
   #include "SpiceUsr.h"
   #undef    vsep_c


   SpiceDouble vsep_c ( ConstSpiceDouble     v1     [3],
                        ConstSpiceDouble     v2     [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   First vector.
   v2         I   Second vector.

   The function returns the angle between `v1' and `v2' expressed in
   radians.

-Detailed_Input

   v1,
   v2          are two double precision 3-dimensional vectors. Either
               `v1' or `v2', or both, may be the zero vector.

               An implicit assumption exists that `v1' and `v2' are
               specified in the same reference frame. If this is not
               the case, the numerical result of this routine has no
               meaning.

-Detailed_Output

   The function returns the angle between `v1' and `v2' expressed in
   radians.

   vsep_c is strictly non-negative. If either `v1' or `v2' is the zero
   vector, then vsep_c is defined to be 0 radians.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   In the plane, it is a simple matter to calculate the angle
   between two vectors once the two vectors have been made to be
   unit length. Then, since the two vectors form the two equal
   sides of an isosceles triangle, the length of the third side
   is given by the expression

      length = 2.0 * sin ( vsep_c /2.0 )

   The length is given by the magnitude of the difference of the
   two unit vectors

      length = norm ( u1 - u2 )

   Once the length is found, the value of vsep_c may be calculated
   by inverting the first expression given above as

      vsep_c  = 2.0 * arcsin ( length/2.0 )

   This expression becomes increasingly unstable when vsep_c gets
   larger than pi/2 radians or 90 degrees. In this situation (which
   is easily detected by determining the sign of the dot product of
   `v1' and `v2') the supplementary angle is calculated first and
   then vsep_c is given by

         vsep_c  = pi - supplementary_angle

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define two sets of 3-dimensional vectors and compute the
      angular separation between each vector in first set and the
      corresponding vector in the second set.


      Example code begins here.


      /.
         Program vsep_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define SETSIZ       3

         /.
         Local variables.
         ./
         SpiceInt             i;

         /.
         Define the two vector sets.
         ./
         SpiceDouble          v1     [SETSIZ][3] = {
                                   {1.0,  0.0,  0.0},
                                   {1.0,  0.0,  0.0},
                                   {3.0,  0.0,  0.0} };

         SpiceDouble          v2     [SETSIZ][3] = {
                                   { 1.0,  0.0,  0.0},
                                   { 0.0,  1.0,  0.0},
                                   {-5.0,  0.0,  0.0} };

         /.
         Calculate the angular separation between each pair
         of vectors.
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            printf( "First vector            :  %5.1f %5.1f %5.1f\n",
                                       v1[i][0], v1[i][1], v1[i][2] );
            printf( "Second vector           :  %5.1f %5.1f %5.1f\n",
                                       v2[i][0], v2[i][1], v2[i][2] );
            printf( "Angular separation (rad):  %14.10f\n",
                                  vsep_c ( v1[i], v2[i] ) );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      First vector            :    1.0   0.0   0.0
      Second vector           :    1.0   0.0   0.0
      Angular separation (rad):    0.0000000000

      First vector            :    1.0   0.0   0.0
      Second vector           :    0.0   1.0   0.0
      Angular separation (rad):    1.5707963268

      First vector            :    3.0   0.0   0.0
      Second vector           :   -5.0   0.0   0.0
      Angular separation (rad):    3.1415926536


-Restrictions

   1)  The user is required to insure that the input vectors will not
       cause floating point overflow upon calculation of the vector
       dot product since no error detection or correction code is
       implemented. In practice, this is not a significant
       restriction.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   W.M. Owen           (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.2, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.1.1, 17-APR-2006 (EDW)

       Typo correction to the value of PI/2 in the -Examples
       section, 1.571 instead of 1.71.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input vectors const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (KRG) (WMO) (WLT)

-Index_Entries

   angular separation of 3-dimensional vectors

-&
*/

{ /* Begin vsep_c */


   /*
   Local variables

   The following declarations represent, respectively:

   Magnitudes of v1, v2
   Either of the difference vectors: v1-v2 or v1-(-v2)
   Unit vectors parallel to v1 and v2
   */

   SpiceDouble     dmag1;
   SpiceDouble     dmag2;
   SpiceDouble     vtemp[3];
   SpiceDouble     u1[3];
   SpiceDouble     u2[3];
   SpiceDouble     vsep;


   /*
   Calculate the magnitudes of v1 and v2; if either is 0, vsep = 0
   */

   unorm_c ( v1, u1, &dmag1 );

   if ( dmag1 == 0.0 )
      {
      vsep = 0.0;
      return vsep;
      }

      unorm_c ( v2, u2, &dmag2 );

   if ( dmag2 == 0.0 )
      {
      vsep = 0.0;
      return vsep;
      }

   if ( vdot_c(u1,u2) > 0. )
      {
      vtemp[0] = u1[0] - u2[0];
      vtemp[1] = u1[1] - u2[1];
      vtemp[2] = u1[2] - u2[2];

      vsep = 2.00 * asin (0.50 * vnorm_c(vtemp));
      }

   else if ( vdot_c(u1,u2) < 0. )
      {
      vtemp[0] = u1[0] + u2[0];
      vtemp[1] = u1[1] + u2[1];
      vtemp[2] = u1[2] + u2[2];

      vsep = pi_c() - 2.00 * asin (0.50 * vnorm_c(vtemp));
      }

   else
      {
      vsep = halfpi_c();
      }


   return vsep;

} /* End vsep_c */
