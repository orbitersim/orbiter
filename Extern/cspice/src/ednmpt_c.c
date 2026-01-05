/*

-Procedure ednmpt_c ( Ellipsoid normal vector to surface point )

-Abstract

   Return the unique point on an ellipsoid's surface where the
   outward normal direction is a given vector.

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

   ELLIPSOID
   GEOMETRY
   NORMAL

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void ednmpt_c ( SpiceDouble         a,
                   SpiceDouble         b,
                   SpiceDouble         c,
                   ConstSpiceDouble    normal [3],
                   SpiceDouble         point  [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   a          I   Length of the ellipsoid semi-axis along the X-axis.
   b          I   Length of the ellipsoid semi-axis along the Y-axis.
   c          I   Length of the ellipsoid semi-axis along the Z-axis.
   normal     I   Outward normal direction.
   point      O   Point where outward normal is parallel to `normal'.

-Detailed_Input

   a           is the length of the semi-axis of the ellipsoid
               that is parallel to the X-axis of the body-fixed
               coordinate system.

   b           is the length of the semi-axis of the ellipsoid
               that is parallel to the Y-axis of the body-fixed
               coordinate system.

   c           is the length of the semi-axis of the ellipsoid
               that is parallel to the Z-axis of the body-fixed
               coordinate system.

   normal      is a non-zero vector. The unique point on the
               ellipsoid at which `normal' is an outward normal vector
               is sought.

-Detailed_Output

   point       is the unique point on the ellipsoid at which `normal'
               is an outward normal vector.

               `point' is a 3-vector giving the body-fixed coordinates
               of a point on the ellipsoid. In body-fixed coordinates,
               the semi-axes of the ellipsoid are aligned with the X,
               Y, and Z-axes of the coordinate system.

-Parameters

   None.

-Exceptions

   1)  If any of the semi-axis lengths is non-positive, the error
       SPICE(BADAXISLENGTH) is signaled by a routine in the call tree of
       this routine.

   2)  If any of the semi-axis lengths underflows to zero when divided by
       the largest semi-axis length, the error SPICE(AXISUNDERFLOW) is
       signaled by a routine in the call tree of this routine.

   3)  If `normal' is the zero vector, the error SPICE(ZEROVECTOR)
       is signaled by a routine in the call tree of this routine.

   4)  If the input pass the above checks but lead to a divide-by-zero error
       or to a computing an invalid argument of a fractional exponential
       expression, the error SPICE(DEGENERATECASE) is signaled by a routine
       in the call tree of this routine.

-Files

   None.

-Particulars

   This routine can be used to determine the distance between an
   ellipsoid and a non-intersecting plane. This distance computation
   supports computation of terminator points on an ellipsoid.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Choose a triaxial ellipsoid with three unequal semi-axis
      lengths. Pick several vectors; find the points on the
      ellipsoid where the respective outward normals are parallel to
      those vectors.

      Check the results: at each point, a computed outward normal
      vector should have very small angular separation from the
      input vector. Also, the point should be on the surface of the
      ellipsoid. The ellipsoid can be thought of as a level surface
      of the function

                           2        2         2
         f(x, y, z) = (x/a)  + (y/b)  +  (z/c)

      where `a', `b', `c' are the semi-axis lengths of the ellipsoid.
      Specifically, the ellipsoid is the set

         { (x, y, z) : f(x, y, z)  =  1 }

      We can evaluate F at a point to determine whether that point
      is close to the ellipsoid's surface.


      Example code begins here.


      /.
         Program ednmpt_ex1
      ./
      #include <math.h>
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          a;
         SpiceDouble          b;
         SpiceDouble          c;
         SpiceDouble          normal [3];
         SpiceDouble          point  [3];
         SpiceDouble          xnorml [3];

         /.
         Initialize the ellipsoid semi-axes.
         ./
         a = 10.0;
         b =  5.0;
         c =  2.0;

         /.
         Pick several vectors; find the points
         on the ellipsoid where the respective
         outward normals are parallel to those
         vectors; check the results.
         ./
         vpack_c ( 0.0, 0.0, 3.0, xnorml );
         ednmpt_c ( a, b, c, xnorml, point );
         surfnm_c ( a, b, c, point, normal );

         printf( " \n" );
         printf( "Semi-axis lengths:    %13.8f %13.8f %13.8f\n", a, b, c );
         printf( "Input vector:         %13.8f %13.8f %13.8f\n",
                                 xnorml[0], xnorml[1], xnorml[2] );
         printf( "Point:                %13.8f %13.8f %13.8f\n",
                                    point[0], point[1], point[2] );
         printf( "Outward normal:       %13.8f %13.8f %13.8f\n",
                                 normal[0], normal[1], normal[2] );
         printf( "Angular error (rad):  %13.8f\n",
                         vsep_c ( normal, xnorml ) );
         printf( "Off-surface error:    %13.8f\n",
                   pow( (point[0]/a), 2 ) + pow( (point[1]/b), 2 )
                 + pow( (point[2]/c), 2 ) - 1 );
         printf( " \n" );

         vpack_c ( 15.0, -7.0, 3.0, xnorml );
         ednmpt_c ( a, b, c, xnorml, point );
         surfnm_c ( a, b, c, point, normal );

         printf( "Semi-axis lengths:    %13.8f %13.8f %13.8f\n", a, b, c );
         printf( "Input vector:         %13.8f %13.8f %13.8f\n",
                                 xnorml[0], xnorml[1], xnorml[2] );
         printf( "Point:                %13.8f %13.8f %13.8f\n",
                                    point[0], point[1], point[2] );
         printf( "Outward normal:       %13.8f %13.8f %13.8f\n",
                                 normal[0], normal[1], normal[2] );
         printf( "Angular error (rad):  %13.8f\n",
                         vsep_c ( normal, xnorml ) );
         printf( "Off-surface error:    %13.8f\n",
                   pow( (point[0]/a), 2 ) + pow( (point[1]/b), 2 )
                 + pow( (point[2]/c), 2 ) - 1 );
         printf( " \n" );

         vpack_c ( 15.0, -7.0, 3.0, xnorml );
         ednmpt_c ( a, b, c, xnorml, point );
         surfnm_c ( a, b, c, point, normal );

         printf( "Semi-axis lengths:    %13.8f %13.8f %13.8f\n", a, b, c );
         printf( "Input vector:         %13.8f %13.8f %13.8f\n",
                                 xnorml[0], xnorml[1], xnorml[2] );
         printf( "Point:                %13.8f %13.8f %13.8f\n",
                                    point[0], point[1], point[2] );
         printf( "Outward normal:       %13.8f %13.8f %13.8f\n",
                                 normal[0], normal[1], normal[2] );
         printf( "Angular error (rad):  %13.8f\n",
                         vsep_c ( normal, xnorml ) );
         printf( "Off-surface error:    %13.8f\n",
                   pow( (point[0]/a), 2 ) + pow( (point[1]/b), 2 )
                 + pow( (point[2]/c), 2 ) - 1 );
         printf( " \n" );

         vpack_c ( a/2, b/2, c/2, xnorml );
         ednmpt_c ( a, b, c, xnorml, point );
         surfnm_c ( a, b, c, point, normal );

         printf( "Semi-axis lengths:    %13.8f %13.8f %13.8f\n", a, b, c );
         printf( "Input vector:         %13.8f %13.8f %13.8f\n",
                                 xnorml[0], xnorml[1], xnorml[2] );
         printf( "Point:                %13.8f %13.8f %13.8f\n",
                                    point[0], point[1], point[2] );
         printf( "Outward normal:       %13.8f %13.8f %13.8f\n",
                                 normal[0], normal[1], normal[2] );
         printf( "Angular error (rad):  %13.8f\n",
                         vsep_c ( normal, xnorml ) );
         printf( "Off-surface error:    %13.8f\n",
                   pow( (point[0]/a), 2 ) + pow( (point[1]/b), 2 )
                 + pow( (point[2]/c), 2 ) - 1 );
         printf( " \n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Semi-axis lengths:      10.00000000    5.00000000    2.00000000
      Input vector:            0.00000000    0.00000000    3.00000000
      Point:                   0.00000000    0.00000000    2.00000000
      Outward normal:          0.00000000    0.00000000    1.00000000
      Angular error (rad):     0.00000000
      Off-surface error:       0.00000000

      Semi-axis lengths:      10.00000000    5.00000000    2.00000000
      Input vector:           15.00000000   -7.00000000    3.00000000
      Point:                   9.73103203   -1.13528707    0.07784826
      Outward normal:          0.89165745   -0.41610681    0.17833149
      Angular error (rad):     0.00000000
      Off-surface error:       0.00000000

      Semi-axis lengths:      10.00000000    5.00000000    2.00000000
      Input vector:           15.00000000   -7.00000000    3.00000000
      Point:                   9.73103203   -1.13528707    0.07784826
      Outward normal:          0.89165745   -0.41610681    0.17833149
      Angular error (rad):     0.00000000
      Off-surface error:       0.00000000

      Semi-axis lengths:      10.00000000    5.00000000    2.00000000
      Input vector:            5.00000000    2.50000000    1.00000000
      Point:                   9.69412864    1.21176608    0.07755303
      Outward normal:          0.88045091    0.44022545    0.17609018
      Angular error (rad):     0.00000000
      Off-surface error:       0.00000000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 08-FEB-2021 (JDR)

-Index_Entries

   point on an ellipsoid having given surface normal

-&
*/

{ /* Begin ednmpt_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "ednmpt_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   ednmpt_ (  ( doublereal * ) &a,
              ( doublereal * ) &b,
              ( doublereal * ) &c,
              ( doublereal * )  normal,
              ( doublereal * )  point  );

   chkout_c ( "ednmpt_c" );

} /* End ednmpt_c */
