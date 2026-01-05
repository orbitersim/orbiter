/*

-Procedure axisar_c ( Axis and angle to rotation )

-Abstract

   Construct a rotation matrix that rotates vectors by a specified
   angle about a specified axis.

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

   MATRIX
   ROTATION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef axisar_c


   void axisar_c ( ConstSpiceDouble  axis   [3],
                   SpiceDouble       angle,
                   SpiceDouble       r      [3][3]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   axis       I   Rotation axis.
   angle      I   Rotation angle, in radians.
   r          O   Rotation matrix corresponding to `axis' and `angle'.

-Detailed_Input

   axis,
   angle       are, respectively, a rotation axis and a rotation
               angle.  `axis' and `angle' determine a coordinate
               transformation whose effect on any vector `v' is to
               rotate `v' by `angle' radians about the vector `axis'.

-Detailed_Output

   r           is a rotation matrix representing the coordinate
               transformation determined by `axis' and `angle': for
               each vector `v', r*v is the vector resulting from
               rotating `v' by `angle' radians about `axis'.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If `axis' is the zero vector, the rotation generated is the
       identity. This is consistent with the specification of vrotv_c.

-Files

   None.

-Particulars

   axisar_c can be thought of as a partial inverse of raxisa_c. axisar_c
   really is a `left inverse': the code fragment

      raxisa_c ( r,    axis,  &angle );
      axisar_c ( axis, angle,  r     );

   preserves `r', except for round-off error, as long as `r' is a
   rotation matrix.

   On the other hand, the code fragment

      axisar_c ( axis, angle,  r     );
      raxisa_c ( r,    axis,  &angle );

   preserves `axis' and `angle', except for round-off error, only if
   `angle' is in the range (0, pi). So axisar_c is a right inverse
   of raxisa_c only over a limited domain.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute a matrix that rotates vectors by pi/2 radians about
      the Z-axis, and compute the rotation axis and angle based on
      that matrix.


      Example code begins here.


      /.
         Program axisar_ex1
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
         SpiceDouble          axis   [3];
         SpiceDouble          axout  [3];
         SpiceDouble          rotmat [3][3];

         SpiceInt             i;

         /.
         Define an axis and an angle for rotation.
         ./
         axis[0] = 0.0;
         axis[1] = 0.0;
         axis[2] = 1.0;
         angle   = halfpi_c();

         /.
         Determine the rotation matrix.
         ./
         axisar_c ( axis, angle, rotmat );

         /.
         Now calculate the rotation axis and angle based on
         `rotmat' as input.
         ./
         raxisa_c ( rotmat, axout, &angout );

         /.
         Display the results.
         ./
         printf( "Rotation matrix:\n" );
         printf( "\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf( "%10.5f %9.5f %9.5f\n",
                    rotmat[i][0], rotmat[i][1], rotmat[i][2] );
         }
         printf( "\n" );
         printf( "Rotation axis       : %9.5f %9.5f %9.5f\n",
                                 axout[0], axout[1], axout[2] );
         printf( "Rotation angle (deg): %9.5f\n", angout * dpr_c() );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Rotation matrix:

         0.00000  -1.00000   0.00000
         1.00000   0.00000   0.00000
         0.00000   0.00000   1.00000

      Rotation axis       :   0.00000   0.00000   1.00000
      Rotation angle (deg):  90.00000


   2) Linear interpolation between two rotation matrices.

      Let r(t) be a time-varying rotation matrix; `r' could be
      a C-matrix describing the orientation of a spacecraft
      structure. Given two points in time `t1' and `t2' at which
      r(t) is known, and given a third time `t3', where

         t1  <  t3  <  t2,

      we can estimate r[t3 - 1] by linear interpolation. In other
      words, we approximate the motion of `r' by pretending that
      `r' rotates about a fixed axis at a uniform angular rate
      during the time interval [t1, t2]. More specifically, we
      assume that each column vector of `r' rotates in this
      fashion. This procedure will not work if `r' rotates through
      an angle of pi radians or more during the time interval
      [t1, t2]; an aliasing effect would occur in that case.


      Example code begins here.


      /.
         Program axisar_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          angle;
         SpiceDouble          axis   [3];
         SpiceDouble          delta  [3][3];
         SpiceDouble          frac;
         SpiceDouble          q      [3][3];
         SpiceDouble          r1     [3][3];
         SpiceDouble          r2     [3][3];
         SpiceDouble          r3     [3][3];
         SpiceDouble          t1;
         SpiceDouble          t2;
         SpiceDouble          t3;

         SpiceInt             i;

         /.
         Lets assume that r(t) is the matrix that rotates
         vectors by pi/2 radians about the Z-axis every
         minute.

         Let

            r1 = r[t1 - 1], for t1 =  0", and
            r2 = r[t2 - 1], for t1 = 60".

         Define both matrices and times.
         ./
         axis[0] = 0.0;
         axis[1] = 0.0;
         axis[2] = 1.0;

         t1 =  0.0;
         t2 = 60.0;
         t3 = 30.0;

         ident_c ( r1 );
         axisar_c ( axis, halfpi_c(), r2 );

         mxmt_c ( r2, r1, q );
         raxisa_c ( q, axis, &angle );

         /.
         Find the fraction of the total rotation angle that `r'
         rotates through in the time interval [t1, t3].
         ./
         frac = ( t3 - t1 )  /  ( t2 - t1 );

         /.
         Finally, find the rotation `delta' that r(t) undergoes
         during the time interval [t1, t3], and apply that
         rotation to `r1', yielding r[t3 - 1], which we'll call `r3'.
         ./
         axisar_c ( axis, frac * angle, delta );
         mxm_c ( delta, r1, r3 );

         /.
         Display the results.
         ./
         printf( "Time (s)            : %9.5f\n", t3 );
         printf( "Rotation axis       : %9.5f %9.5f %9.5f\n",
                                    axis[0], axis[1], axis[2] );
         printf( "Rotation angle (deg): %9.5f\n", frac * angle * dpr_c() );
         printf( "Rotation matrix     :\n" );
         printf( "\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf( "%10.5f %9.5f %9.5f\n", r3[i][0], r3[i][1], r3[i][2] );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Time (s)            :  30.00000
      Rotation axis       :   0.00000   0.00000   1.00000
      Rotation angle (deg):  45.00000
      Rotation matrix     :

         0.70711  -0.70711   0.00000
         0.70711   0.70711   0.00000
         0.00000   0.00000   1.00000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 06-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code examples based on existing code fragments.

   -CSPICE Version 1.0.0, 18-JUN-1999 (NJB)

-Index_Entries

   axis and angle to rotation

-&
*/

{ /* Begin axisar_c */



   /*
   Error free:  no error tracing required.
   */

   axisar_ (  ( doublereal * ) axis,
              ( doublereal * ) &angle,
              ( doublereal * ) r       );

   /*
   Transpose the output matrix to put it in row-major order.
   */

   xpose_c ( r, r );


} /* End axisar_c */
