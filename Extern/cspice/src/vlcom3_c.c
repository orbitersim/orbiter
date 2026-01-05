/*

-Procedure vlcom3_c ( Vector linear combination, 3 dimensions )

-Abstract

   Compute the vector linear combination of three double precision
   3-dimensional vectors.

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
   #undef    vlcom3_c


   void vlcom3_c ( SpiceDouble        a,
                   ConstSpiceDouble   v1 [3],
                   SpiceDouble        b,
                   ConstSpiceDouble   v2 [3],
                   SpiceDouble        c,
                   ConstSpiceDouble   v3 [3],
                   SpiceDouble        sum[3]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   a          I   Coefficient of `v1'.
   v1         I   Vector in 3-space.
   b          I   Coefficient of `v2'.
   v2         I   Vector in 3-space.
   c          I   Coefficient of `v3'.
   v3         I   Vector in 3-space.
   sum        O   Linear vector combination a*v1 + b*v2 + c*v3.

-Detailed_Input

   a           is the double precision scalar variable that multiplies
               `v1'.

   v1          is an arbitrary, double precision 3-dimensional vector.

   b           is the double precision scalar variable that multiplies
               `v2'.

   v2          is an arbitrary, double precision 3-dimensional vector.

   c           is the double precision scalar variable that multiplies
               `v3'.

   v3          is a double precision 3-dimensional vector.

-Detailed_Output

   sum         is the double precision 3-dimensional vector which
               contains the linear combination

                  a * v1 + b * v2 + c * v3

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The code reflects precisely the following mathematical expression

      For each value of the index `i', from 0 to 2:

   sum[i] = a * v1[i] + b * v2[i] + c * v3[i]

   No error checking is performed to guard against numeric overflow.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose you have an instrument with an elliptical field
      of view described by its angular extent along the semi-minor
      and semi-major axes.

      The following code example demonstrates how to create
      16 vectors aiming at visualizing the field-of-view in
      three dimensional space.


      Example code begins here.


      /.
         Program vlcom3_ex1
      ./
      #include <math.h>
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.

         Define the two angular extends, along the semi-major
         (u) and semi-minor (v) axes of the elliptical field
         of view, in radians.
         ./
         #define MAXANG       0.07
         #define MINANG       0.035

         /.
         Local variables.
         ./
         SpiceDouble          a;
         SpiceDouble          b;
         SpiceDouble          step;
         SpiceDouble          theta;
         SpiceDouble          vector [3];
         SpiceInt             i;

         /.
         Let `u' and `v' be orthonormal 3-vectors spanning the
         focal plane of the instrument, and `z' its
         boresight.
         ./
         SpiceDouble          u      [3] = { 1.0,  0.0,  0.0 };
         SpiceDouble          v      [3] = { 0.0,  1.0,  0.0 };
         SpiceDouble          z      [3] = { 0.0,  0.0,  1.0 };

         /.
         Find the length of the ellipse's axes. Note that
         we are dealing with unitary vectors.
         ./
         a = tan ( MAXANG );
         b = tan ( MINANG );

         /.
         Compute the vectors of interest and display them
         ./
         theta = 0.0;
         step  = twopi_c() / 16;

         for ( i = 0; i < 16; i++ )
         {

            vlcom3_c ( 1.0, z, a * cos(theta), u, b * sin(theta), v, vector );

            printf( "%2d: %9.6f %9.6f %9.6f\n",
                    (int)i, vector[0], vector[1], vector[2] );

            theta = theta + step;

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       0:  0.070115  0.000000  1.000000
       1:  0.064777  0.013399  1.000000
       2:  0.049578  0.024759  1.000000
       3:  0.026832  0.032349  1.000000
       4:  0.000000  0.035014  1.000000
       5: -0.026832  0.032349  1.000000
       6: -0.049578  0.024759  1.000000
       7: -0.064777  0.013399  1.000000
       8: -0.070115  0.000000  1.000000
       9: -0.064777 -0.013399  1.000000
      10: -0.049578 -0.024759  1.000000
      11: -0.026832 -0.032349  1.000000
      12: -0.000000 -0.035014  1.000000
      13:  0.026832 -0.032349  1.000000
      14:  0.049578 -0.024759  1.000000
      15:  0.064777 -0.013399  1.000000


-Restrictions

   1)  No error checking is performed to guard against numeric
       overflow or underflow. The user is responsible for insuring
       that the input values are reasonable.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Added -Restrictions #1.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input vectors const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (WLT)

-Index_Entries

   linear combination of three 3-dimensional vectors

-&
*/

{ /* Begin vlcom3_c */


   /* This really doesn't require a degree in rocket science */

   sum[0] = a*v1[0] + b*v2[0] + c*v3[0];
   sum[1] = a*v1[1] + b*v2[1] + c*v3[1];
   sum[2] = a*v1[2] + b*v2[2] + c*v3[2];


} /* End vlcom3_c */
