/*

-Procedure vlcomg_c ( Vector linear combination, general dimension )

-Abstract

   Compute a vector linear combination of two double precision
   vectors of arbitrary dimension.

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

   VECTOR

*/

   #include "SpiceUsr.h"
   #undef   vlcomg_c

   void vlcomg_c ( SpiceInt            n,
                   SpiceDouble         a,
                   ConstSpiceDouble *  v1,
                   SpiceDouble         b,
                   ConstSpiceDouble *  v2,
                   SpiceDouble      *  sum )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   n          I   Dimension of vector space.
   a          I   Coefficient of `v1'.
   v1         I   Vector in n-space.
   b          I   Coefficient of `v2'.
   v2         I   Vector in n-space.
   sum        O   Linear vector combination a*v1 + b*v2.

-Detailed_Input

   n           is the dimension of `v1', `v2' and `sum'.

   a           is the double precision scalar variable that multiplies
               `v1'.

   v1          is an arbitrary, double precision n-dimensional vector.

   b           is the double precision scalar variable that multiplies
               `v2'.

   v2          is an arbitrary, double precision n-dimensional vector.

-Detailed_Output

   sum         is the double precision n-dimensional vector which
               contains the linear combination

                  a * v1 + b * v2

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The code reflects precisely the following mathematical expression

      For each value of the index `i', from 0 to n-1:

         sum[i] = a * v1[i] + b * v2[i]

   No error checking is performed to guard against numeric overflow.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Perform the projection of a 4-dimensional vector into a
      2-dimensional plane in 4-space.


      Example code begins here.


      /.
         Program vlcomg_ex1
      ./
      #include <math.h>
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NDIM         4

         /.
         Local variables.
         ./
         SpiceDouble          puv    [NDIM];
         SpiceDouble          v      [NDIM];

         /.
         Let `x' be an arbitrary NDIM-vector
         ./
         SpiceDouble          x      [NDIM] = { 4.0, 35.0, -5.0, 7.0 };

         /.
         Let `u' and `v' be orthonormal NDIM-vectors spanning the
         plane of interest.
         ./
         SpiceDouble          u      [NDIM] = { 0.0,  0.0,  1.0, 0.0 };

         v[0] =  sqrt(3.0)/3.0;
         v[1] = -sqrt(3.0)/3.0;
         v[2] =  0.0;
         v[3] =  sqrt(3.0)/3.0;

         /.
         Compute the projection of `x' onto this 2-dimensional
         plane in NDIM-space.
         ./
         vlcomg_c ( NDIM, vdotg_c ( x, u, NDIM ), u,
                          vdotg_c ( x, v, NDIM ), v, puv );

         /.
         Display the results.
         ./
         printf( "Input vector             :  %5.1f %5.1f %5.1f %5.1f\n",
                                                   x[0], x[1], x[2], x[3] );
         printf( "Projection into 2-d plane:  %5.1f %5.1f %5.1f %5.1f\n",
                                           puv[0], puv[1], puv[2], puv[3] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input vector             :    4.0  35.0  -5.0   7.0
      Projection into 2-d plane:   -8.0   8.0  -5.0  -8.0


-Restrictions

   1)  No error checking is performed to guard against numeric
       overflow or underflow. The user is responsible for insuring
       that the input values are reasonable.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.0.0, 30-JUN-1999 (WLT)

-Index_Entries

   linear combination of two n-dimensional vectors

-&
*/

{ /* Begin vlcomg_c */

   /*
   Local variables
   */
   SpiceInt       i;


   /* A simple loop to do the work. */
   for ( i = 0; i < n; i++ )
      {
      sum[i] = a*v1[i] + b*v2[i];
      }


} /* End vlcomg_c */
