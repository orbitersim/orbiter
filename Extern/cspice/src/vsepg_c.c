/*

-Procedure vsepg_c ( Angular separation of vectors, general dimension )

-Abstract

   Find the separation angle in radians between two double precision
   vectors of arbitrary dimension. This angle is defined as zero if
   either vector is zero.

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
   #undef   vsepg_c

   SpiceDouble vsepg_c ( ConstSpiceDouble * v1,
                         ConstSpiceDouble * v2,
                         SpiceInt           ndim )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   First vector.
   v2         I   Second vector.
   ndim       I   The number of elements in `v1' and `v2'.

   The function returns the angle between `v1' and `v2' expressed in
   radians.

-Detailed_Input

   v1,
   v2          are two double precision vectors of arbitrary dimension.
               Either `v1' or `v2', or both, may be the zero vector.

               An implicit assumption exists that `v1' and `v2' are
               specified in the same reference space. If this is not
               the case, the numerical result of this routine has no
               meaning.

   ndim        is the dimension of both `v1' and `v2'.

-Detailed_Output

   The function returns the angle between `v1' and `v2' expressed in
   radians.

   vsepg_c is strictly non-negative. For input vectors of four or more
   dimensions, the angle is defined as the generalization of the
   definition for three dimensions. If either `v1' or `v2' is the zero
   vector, then vsepg_c is defined to be 0 radians.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   In four or more dimensions this angle does not have a physically
   realizable interpretation. However, the angle is defined as
   the generalization of the following definition which is valid in
   three or two dimensions:

      In the plane, it is a simple matter to calculate the angle
      between two vectors once the two vectors have been made to be
      unit length. Then, since the two vectors form the two equal
      sides of an isosceles triangle, the length of the third side
      is given by the expression

         length = 2.0 * sin ( vsepg_c/2.0 )

      The length is given by the magnitude of the difference of the
      two unit vectors

         length = norm ( u1 - u2 )

      Once the length is found, the value of vsepg_c may be calculated
      by inverting the first expression given above as

         vsepg_c = 2.0 * arcsin ( length/2.0 )

      This expression becomes increasingly unstable when vsepg_c gets
      larger than pi/2 radians or 90 degrees. In this situation
      (which is easily detected by determining the sign of the dot
      product of `v1' and `v2') the supplementary angle is calculated
      first and then vsepg_c is given by

         vsepg_c = pi - supplementary_angle

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define two sets of n-dimensional vectors and compute the
      angular separation between each vector in first set and the
      corresponding vector in the second set.


      Example code begins here.


      /.
         Program vsepg_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NDIM         4
         #define SETSIZ       3

         /.
         Local variables.
         ./
         SpiceInt             i;

         /.
         Define the two vector sets.
         ./
         SpiceDouble          v1     [SETSIZ][NDIM] = {
                                   {1.0,  0.0,  0.0,  0.0},
                                   {1.0,  0.0,  0.0,  0.0},
                                   {3.0,  0.0,  0.0,  0.0} };

         SpiceDouble          v2     [SETSIZ][NDIM] = {
                                   { 1.0,  0.0,  0.0,  0.0},
                                   { 0.0,  1.0,  0.0,  0.0},
                                   {-5.0,  0.0,  0.0,  0.0} };

         /.
         Calculate the angular separation between each pair
         of vectors.
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            printf( "First vector            :  %5.1f %5.1f %5.1f %5.1f\n",
                                     v1[i][0], v1[i][1], v1[i][2], v1[i][3] );
            printf( "Second vector           :  %5.1f %5.1f %5.1f %5.1f\n",
                                     v2[i][0], v2[i][1], v2[i][2], v2[i][3] );
            printf( "Angular separation (rad):  %14.10f\n",
                             vsepg_c ( v1[i], v2[i], NDIM ) );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      First vector            :    1.0   0.0   0.0   0.0
      Second vector           :    1.0   0.0   0.0   0.0
      Angular separation (rad):    0.0000000000

      First vector            :    1.0   0.0   0.0   0.0
      Second vector           :    0.0   1.0   0.0   0.0
      Angular separation (rad):    1.5707963268

      First vector            :    3.0   0.0   0.0   0.0
      Second vector           :   -5.0   0.0   0.0   0.0
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

   C.A. Curzon         (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   H.A. Neilan         (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 31-JUL-2020 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.0.0, 29-JUN-1999 (EDW) (WLT) (HAN) (KRG) (CAC)

-Index_Entries

   angular separation of n-dimensional vectors

-&
*/

{ /* Begin vsepg_c */


   /*
   Local variables
   */
   SpiceDouble       mag1;
   SpiceDouble       mag2;
   SpiceDouble       mag_dif;
   SpiceDouble       r1;
   SpiceDouble       r2;
   SpiceInt          i;

   mag1 = vnormg_c( v1, ndim);
   mag2 = vnormg_c( v2, ndim);


   /*
   If either v1 or v2 have magnitude zero, the separation is 0.
   */
   if ( ( mag1 == 0.) || ( mag2 == 0.) )
      {
      return 0;
      }

   if ( vdotg_c( v1, v2, ndim ) < 0. )
      {
      r1      = 1./mag1;
      r2      = 1./mag2;
      mag_dif = 0.;

      for ( i = 0; i < ndim; i++ )
         {
         mag_dif += pow( ( v1[i]*r1 - v2[i]*r2 ), 2);
         }

      mag_dif = sqrt(mag_dif);

      return ( 2. * asin (0.5 * mag_dif) );

      }
   else if ( vdotg_c (v1, v2, ndim) > 0. )
      {
      r1      = 1./mag1;
      r2      = 1./mag2;
      mag_dif = 0.;

      for ( i = 0; i < ndim; i++ )
         {
         mag_dif += pow( ( v1[i]*r1 + v2[i]*r2 ), 2);
         }

      mag_dif = sqrt(mag_dif);

      return ( pi_c() - 2. * asin (0.5 * mag_dif) );
      }

   return ( halfpi_c());



} /* End vsepg_c */
