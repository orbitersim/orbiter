/*

-Procedure raxisa_c ( Rotation axis of a matrix )

-Abstract

   Compute the axis of the rotation given by an input matrix
   and the angle of the rotation about that axis.

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

   ROTATION

-Keywords

   ANGLE
   MATRIX
   ROTATION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef raxisa_c


   void raxisa_c ( ConstSpiceDouble     matrix[3][3],
                   SpiceDouble          axis  [3],
                   SpiceDouble        * angle       )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   matrix     I   3x3 rotation matrix in double precision.
   axis       O   Axis of the rotation.
   angle      O   Angle through which the rotation is performed.

-Detailed_Input

   matrix      is a 3x3 rotation matrix in double precision.

-Detailed_Output

   axis        is a unit vector pointing along the axis of the rotation.
               In other words, `axis' is a unit eigenvector of the input
               matrix, corresponding to the eigenvalue 1. If the input
               matrix is the identity matrix, `axis' will be the vector
               (0, 0, 1). If the input rotation is a rotation by pi
               radians, both `axis' and -axis may be regarded as the
               axis of the rotation.

   angle       is the angle between `v' and matrix*v for any non-zero
               vector `v' orthogonal to `axis'.  `angle' is given in
               radians. The angle returned will be in the range from 0
               to pi radians.

-Parameters

   None.

-Exceptions

   1)  If the input matrix is not a rotation matrix (where a fairly
       loose tolerance is used to check this), an error is signaled
       by a routine in the call tree of this routine.

   2)  If the input matrix is the identity matrix, this routine
       returns an angle of 0.0, and an axis of ( 0.0, 0.0, 1.0 ).

-Files

   None.

-Particulars

   Every rotation matrix has an axis `a' such any vector `v'
   parallel to that axis satisfies the equation

      v = matrix * v

   This routine returns a unit vector `axis' parallel to the axis of
   the input rotation matrix. Moreover for any vector `w' orthogonal
   to the axis of the rotation, the two vectors

       axis,
       w x (matrix*w)

      (where "x" denotes the cross product operation)

   will be positive scalar multiples of one another (at least
   to within the ability to make such computations with double
   precision arithmetic, and under the assumption that `matrix'
   does not represent a rotation by zero or pi radians).

   The angle returned will be the angle between `w' and matrix*w
   for any vector orthogonal to `axis'.

   If the input matrix is a rotation by 0 or pi radians some
   choice must be made for the axis returned. In the case of
   a rotation by 0 radians, `axis' is along the positive z-axis.
   In the case of a rotation by 180 degrees, two choices are
   possible. The choice made this routine is unspecified.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given an axis and an angle of rotation about that axis,
      determine the rotation matrix. Using this matrix as input,
      compute the axis and angle of rotation, and verify that
      the later are equivalent by subtracting the original matrix
      and the one resulting from using the computed axis and angle
      of rotation on the axisar_c call.


      Example code begins here.


      /.
         Program raxisa_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          angle;
         SpiceDouble          angout;
         SpiceDouble          axout  [3];
         SpiceDouble          r      [3][3];
         SpiceDouble          rout   [3][3];

         SpiceInt             i;

         /.
         Define an axis and an angle for rotation.
         ./
         SpiceDouble          axis   [3] = { 1.0, 2.0, 3.0 };

         angle = 0.1 * twopi_c ( );

         /.
         Determine the rotation matrix.
         ./
         axisar_c ( axis, angle, r );

         /.
         Now calculate the rotation axis and angle based on the
         matrix as input.
         ./
         raxisa_c ( r, axout, &angout );

         printf( "Axis : %11.8f %11.8f %11.8f\n",
                                     axout[0], axout[1], axout[2] );
         printf( "Angle: %11.8f\n", angout );
         printf( " \n" );

         /.
         Now input the `axout' and `angout' to axisar_c to
         compare against the original rotation matrix `r'.
         ./
         printf( "Difference between input and output matrices:\n" );

         axisar_c ( axout, angout, rout );

         for ( i = 0; i < 3; i++ )
         {

            printf( "%20.16f %19.16f %19.16f\n",
                    rout[i][0] - r[i][0],
                    rout[i][1] - r[i][1],
                    rout[i][2] - r[i][2] );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Axis :  0.26726124  0.53452248  0.80178373
      Angle:  0.62831853

      Difference between input and output matrices:
       -0.0000000000000001  0.0000000000000000  0.0000000000000000
        0.0000000000000001 -0.0000000000000001  0.0000000000000000
        0.0000000000000000  0.0000000000000001  0.0000000000000000


      Note, the zero matrix is accurate to round-off error. A
      numerical demonstration of equality.


   2) This routine can be used to numerically approximate the
      instantaneous angular velocity vector of a rotating object.

      Suppose that R(t) is the rotation matrix whose columns
      represent the inertial pointing vectors of the body-fixed axes
      of an object at time t.

      Then the angular velocity vector points along the vector given
      by:

                              T
          limit  axis( R(t+h)R )
          h-->0

      And the magnitude of the angular velocity at time t is given
      by:

                             T
         d angle ( R(t+h)R(t) )
         ----------------------   at   h = 0
                   dh

      This code example computes the instantaneous angular velocity
      vector of the Earth at 2000 Jan 01 12:00:00 TDB.

      Use the PCK kernel below to load the required triaxial
      ellipsoidal shape model and orientation data for the Earth.

         pck00010.tpc


      Example code begins here.


      /.
         Program raxisa_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          angle;
         SpiceDouble          angvel [3];
         SpiceDouble          axis   [3];
         SpiceDouble          infrot [3][3];
         SpiceDouble          h;
         SpiceDouble          rt     [3][3];
         SpiceDouble          rth    [3][3];
         SpiceDouble          t;

         /.
         Load a PCK file containing a triaxial
         ellipsoidal shape model and orientation
         data for the Earth.
         ./
         furnsh_c ( "pck00010.tpc" );

         /.
         Load time into the double precision variable `t'
         and the delta time (1 ms) into the double precision
         variable `h'
         ./
         t = 0.0;
         h = 1e-3;

         /.
         Get the rotation matrices from IAU_EARTH to J2000
         at `t' and t+h.
         ./
         pxform_c ( "IAU_EARTH", "J2000", t,   rt  );
         pxform_c ( "IAU_EARTH", "J2000", t+h, rth );

         /.
         Compute the infinitesimal rotation R(t+h)R(t)**T
         ./
         mxmt_c ( rth, rt, infrot );

         /.
         Compute the `axis' and `angle' of the infinitesimal rotation
         ./
         raxisa_c ( infrot, axis, &angle );

         /.
         Scale `axis' to get the angular velocity vector
         ./
         vscl_c ( angle/h, axis, angvel );

         /.
         Output the results.
         ./
         printf( "Instantaneous angular velocity vector:\n" );
         printf( "%14.10f %14.10f %14.10f\n",
                 angvel[0], angvel[1], angvel[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Instantaneous angular velocity vector:
        0.0000000000   0.0000000000   0.0000729212


-Restrictions

   1)  If the input matrix is not a rotation matrix but is close enough
       to pass the tests this routine performs on it, no error will be
       signaled, but the results may have poor accuracy.

   2)  The input matrix is taken to be an object that acts on (rotates)
       vectors---it is not regarded as a coordinate transformation. To
       find the axis and angle of a coordinate transformation, input
       the transpose of that matrix to this routine.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   F.S. Turner         (JPL)

-Version

   -CSPICE Version 1.0.2, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Added complete code examples.

   -CSPICE Version 1.0.1, 05-JAN-2005 (NJB) (WLT) (FST)

       Various header updates were made to reflect changes
       made to the underlying SPICELIB Fortran code.
       Miscellaneous header corrections were made as well.

   -CSPICE Version 1.0.0, 31-MAY-1999 (WLT) (NJB)

-Index_Entries

   rotation axis of a matrix

-&
*/

{ /* Begin raxisa_c */

   /*
   Local variables
   */
   SpiceDouble             tmpmat[3][3];


   /*
   Error free: no error tracing.
   */

   /*
   Transpose the input matrix to put it in column-major order.
   */

   xpose_c ( matrix, tmpmat );

   raxisa_ (  ( doublereal * ) tmpmat,
              ( doublereal * ) axis,
              ( doublereal * ) angle  );

} /* End raxisa_c */
