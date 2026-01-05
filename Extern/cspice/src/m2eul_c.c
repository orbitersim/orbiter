/*

-Procedure m2eul_c ( Matrix to Euler angles )

-Abstract

   Factor a rotation matrix as a product of three rotations about
   specified coordinate axes.

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
   TRANSFORMATION

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef    m2eul_c


   void  m2eul_c ( ConstSpiceDouble    r[3][3],
                   SpiceInt            axis3,
                   SpiceInt            axis2,
                   SpiceInt            axis1,
                   SpiceDouble       * angle3,
                   SpiceDouble       * angle2,
                   SpiceDouble       * angle1  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   r          I   A rotation matrix to be factored.
   axis3,
   axis2,
   axis1      I   Numbers of third, second, and first rotation axes.
   angle3,
   angle2,
   angle1     O   Third, second, and first Euler angles, in radians.

-Detailed_Input

   r           is a 3x3 rotation matrix that is to be factored as
               a product of three rotations about a specified
               coordinate axes. The angles of these rotations are
               called "Euler angles."

   axis3,
   axis2,
   axis1       are the indices of the rotation axes of the
               "factor" rotations, whose product is `r'. `r' is
               factored as

                  r = [ angle3 ]      [ angle2 ]      [ angle1 ]
                                axis3           axis2           axis1

               The axis numbers must belong to the set {1, 2, 3}.
               The second axis number MUST differ from the first
               and third axis numbers.

               See the -Particulars section below for details
               concerning this notation.

-Detailed_Output

   angle3,
   angle2,
   angle1      are the Euler angles corresponding to the matrix
               `r' and the axes specified by `axis3', `axis2', and
               `axis1'. These angles satisfy the equality

                  r = [ angle3 ]      [ angle2 ]      [ angle1 ]
                                axis3           axis2           axis1


               See the -Particulars section below for details
               concerning this notation.

               The range of `angle3' and `angle1' is (-pi, pi].

               The range of `angle2' depends on the exact set of
               axes used for the factorization. For
               factorizations in which the first and third axes
               are the same,

                  r = [R]  [S]  [T] ,
                         a    b    a

               the range of `angle2' is [0, pi].

               For factorizations in which the first and third
               axes are different,

                  r = [R]  [S]  [T] ,
                         a    b    c

               the range of `angle2' is [-pi/2, pi/2].

               For rotations such that `angle3' and `angle1' are not
               uniquely determined, `angle3' will always be set to
               zero; `angle1' is then uniquely determined.

-Parameters

   None.

-Exceptions

   1)  If any of `axis3', `axis2', or `axis1' do not have values in

          { 1, 2, 3 }

       the error SPICE(BADAXISNUMBERS) is signaled by a routine in
       the call tree of this routine.

   2)  If `axis2' is equal to `axis3' or `axis1', the error
       SPICE(BADAXISNUMBERS) is signaled by a routine in the call
       tree of this routine. An arbitrary rotation matrix cannot be
       expressed using a sequence of Euler angles unless the second
       rotation axis differs from the other two.

   3)  If the input matrix `r' is not a rotation matrix, the error
       SPICE(NOTAROTATION) is signaled by a routine in the call tree
       of this routine.

   4)  If `angle3' and `angle1' are not uniquely determined, `angle3'
       is set to zero, and `angle1' is determined.

-Files

   None.

-Particulars

   A word about notation: the symbol

      [ x ]
           i

   indicates a coordinate system rotation of `x' radians about the
   ith coordinate axis. To be specific, the symbol

      [ x ]
           1

   indicates a coordinate system rotation of `x' radians about the
   first, or x-, axis; the corresponding matrix is

      .-                    -.
      |  1      0       0    |
      |                      |
      |  0    cos(x)  sin(x) |
      |                      |
      |  0   -sin(x)  cos(x) |
      `-                    -'

   Remember, this is a COORDINATE SYSTEM rotation by x radians; this
   matrix, when applied to a vector, rotates the vector by -x
   radians, not `x' radians. Applying the matrix to a vector yields
   the vector's representation relative to the rotated coordinate
   system.

   The analogous rotation about the second, or y-, axis is
   represented by

      [ x ]
           2

   which symbolizes the matrix

      .-                    -.
      | cos(x)   0   -sin(x) |
      |                      |
      |  0       1      0    |
      |                      |
      | sin(x)   0    cos(x) |
      `-                    -'

   and the analogous rotation about the third, or z-, axis is
   represented by

      [ x ]
           3

   which symbolizes the matrix

      .-                    -.
      |  cos(x)  sin(x)   0  |
      |                      |
      | -sin(x)  cos(x)   0  |
      |                      |
      |  0        0       1  |
      `-                    -'


   The input matrix is assumed to be the product of three
   rotation matrices, each one of the form

      .-                    -.
      |  1      0       0    |
      |                      |
      |  0    cos(r)  sin(r) |     (rotation of `r' radians about the
      |                      |      x-axis),
      |  0   -sin(r)  cos(r) |
      `-                    -'


      .-                    -.
      | cos(s)   0   -sin(s) |
      |                      |
      |  0       1      0    |     (rotation of `s' radians about the
      |                      |      y-axis),
      | sin(s)   0    cos(s) |
      `-                    -'

   or

      .-                    -.
      |  cos(t)  sin(t)   0  |
      |                      |
      | -sin(t)  cos(t)   0  |     (rotation of `t' radians about the
      |                      |      z-axis),
      |  0        0       1  |
      `-                    -'

   where the second rotation axis is not equal to the first or
   third. Any rotation matrix can be factored as a sequence of
   three such rotations, provided that this last criterion is met.

   This routine is related to the CSPICE routine eul2m_c, which
   produces a rotation matrix, given a sequence of Euler angles.
   This routine is a `right inverse' of eul2m_c: the sequence of
   calls

      m2eul_c ( r,  axis3,   axis2,   axis1,
                    angle3,  angle2,  angle1     );

      eul2m_c (     angle3,  angle2,  angle1,
                    axis3,   axis2,   axis1,   r );

   preserves `r', except for round-off error.


   On the other hand, the sequence of calls

      eul2m_c ( angle3,  angle2,  angle1,
                axis3,   axis2,   axis1,   r );

      m2eul_c ( r,  axis3,   axis2,   axis1,
                    angle3,  angle2,  angle1 );


   preserve `angle3', `angle2', and `angle1' only if these angles start
   out in the ranges that m2eul_c's outputs are restricted to.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Conversion of instrument pointing from a matrix representation
      to Euler angles:

      Suppose we want to find camera pointing in alpha, delta, and
      kappa, given the inertial-to-camera coordinate transformation


      .-                                                         -.
      |  0.491273796781358  0.508726203218642  0.706999085398824  |
      |                                                           |
      | -0.508726203218642 -0.491273796781358  0.706999085398824  |
      |                                                           |
      |  0.706999085398824 -0.706999085398824  0.017452406437284  |
      `-                                                         -'


      Find angles alpha, delta, kappa such that

         TICAM  =  [ kappa ]  [ pi/2 - delta ]  [ pi/2 + alpha ] .
                            3                 1                 3

      Example code begins here.


      /.
         Program m2eul_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          alpha;
         SpiceDouble          ang1;
         SpiceDouble          ang2;
         SpiceDouble          delta;
         SpiceDouble          kappa;

         SpiceDouble          ticam  [3][3] = {
              { 0.491273796781358,  0.508726203218642,  0.706999085398824},
              {-0.508726203218642, -0.491273796781358,  0.706999085398824},
              { 0.706999085398824, -0.706999085398824,  0.017452406437284} };

         m2eul_c ( ticam, 3, 1, 3, &kappa, &ang2, &ang1 );

         delta = halfpi_c ( ) - ang2;
         alpha = ang1     - halfpi_c ( );

         if ( kappa < 0.0 )
         {
            kappa = kappa + twopi_c ( );
         }

         if ( alpha < 0.0 )
         {
            alpha = alpha + twopi_c ( );
         }

         printf( "Alpha (deg):  %23.14f\n", dpr_c ( ) * alpha );
         printf( "Delta (deg):  %23.14f\n", dpr_c ( ) * delta );
         printf( "Kappa (deg):  %23.14f\n", dpr_c ( ) * kappa );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Alpha (deg):       315.00000000000000
      Delta (deg):         1.00000000000003
      Kappa (deg):        45.00000000000000


   2) Conversion of instrument pointing angles from a non-J2000,
      not necessarily inertial frame to J2000-relative RA, Dec,
      and Twist.

      Suppose that we have orientation for the CASSINI Narrow Angle
      Camera (NAC) frame expressed as

         [ gamma ]  [ beta ]  [ alpha ]
                  1         2          3

      with respect to the CASSINI spacecraft frame.

      We want to express that orientation with respect to the J2000
      frame as the sequence of rotations

         [ twist ]  [ dec ]  [ ra ] .
                  3        1       3

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: m2eul_ex2.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                      Contents
            ---------                      --------
            naif0010.tls                   Leapseconds
            cas00145.tsc                   Cassini SCLK
            cas_v40.tf                     Cassini frames
            08052_08057ra.bc               Orientation for Cassini

         \begindata

            KERNELS_TO_LOAD = ( 'naif0010.tls'
                                'cas00145.tsc'
                                'cas_v40.tf'
                                '08052_08057ra.bc')

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program m2eul_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define META         "m2eul_ex2.tm"
         #define ALPHA         89.9148
         #define BETA          -0.03300
         #define GAMMA        -90.009796

         /.
         Local variables
         ./
         SpiceDouble          ra;
         SpiceDouble          ang1;
         SpiceDouble          ang2;
         SpiceDouble          dec;
         SpiceDouble          et;
         SpiceDouble          inst2j [3][3];
         SpiceDouble          inst2s [3][3];
         SpiceDouble          s2j    [3][3];
         SpiceDouble          twist;

         /.
         Load the kernels.
         ./
         furnsh_c ( META );

         /.
         First, we use function eul2m_c to form the
         transformation from instrument coordinates to
         CASSINI spacecraft frame.
         ./
         eul2m_c ( GAMMA * rpd_c ( ), BETA * rpd_c ( ), ALPHA * rpd_c ( ),
                   1,                 2,                3,         inst2s );

         /.
         Now we compute the transformation from CASSINI
         spacecraft frame to J2000, at a given time.
         ./
         str2et_c ( "2008 Feb 25", &et );
         pxform_c ( "CASSINI_SC_COORD", "J2000", et, s2j );

         /.
         Next, we form the transformation from instrument
         coordinates to J2000 frame.
         ./
         mxm_c ( s2j, inst2s, inst2j );

         /.
         Finally, we express `inst2j' using the desired Euler
         angles.
         ./
         m2eul_c ( inst2j, 3, 1, 3, &twist, &ang1, &ang2 );

         ra   =  ang2 - halfpi_c ( );
         dec  =  halfpi_c ( ) - ang1;

         /.
         If we wish to make sure that `ra', `dec', and `twist' are in
         the ranges [0, 2pi), [-pi/2, pi/2], and [0, 2pi)
         respectively, we may add the code
         ./
         if ( ra < 0.0 )
         {
            ra = ra + twopi_c ( );
         }

         if ( twist < 0.0 )
         {
            twist = twist + twopi_c ( );
         }

         /.
         Now `ra', `dec', and `twist' express the instrument pointing
         as RA, Dec, and Twist, relative to the J2000 system.
         ./
         printf( "RA    (deg):  %23.14f\n", dpr_c ( ) * ra );
         printf( "Dec   (deg):  %23.14f\n", dpr_c ( ) * dec );
         printf( "Twist (deg):  %23.14f\n", dpr_c ( ) * twist );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      RA    (deg):        83.77802387778848
      Dec   (deg):       -14.92925498590898
      Twist (deg):       294.55732942050986


      Note:  more than one definition of RA, Dec, and Twist is
      extant. Before using this example in an application, check
      that the definition given here is consistent with that used
      in your application.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.3.2, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code examples from existing fragments.

   -CSPICE Version 1.3.1, 13-OCT-2004 (NJB)

       Fixed header typo.

   -CSPICE Version 1.3.0, 21-OCT-1998 (NJB)

       Made input matrix const.

   -CSPICE Version 1.2.0, 13-FEB-1998 (EDW)

       Minor corrections to header.

   -CSPICE Version 1.2.0, 08-FEB-1998 (NJB)

       Removed local variables used for temporary capture of outputs.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

       Based on SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)

-Index_Entries

   matrix to euler angles

-&
*/

{ /* Begin m2eul_c */

   /*
   Local variables
   */
   SpiceDouble             loc_r[3][3];


   /*
   Participate in error tracing.
   */
   chkin_c ( "m2eul_c" );


   /*
   Transpose the input matrix to put it in column-major order.
   */
   xpose_c ( r, loc_r );


   /*
   Call the f2c'd version of m2eul:
   */
   m2eul_ ( (doublereal *) loc_r,
            (integer    *) &axis3,
            (integer    *) &axis2,
            (integer    *) &axis1,
            (doublereal *) angle3,
            (doublereal *) angle2,
            (doublereal *) angle1  );


   chkout_c ( "m2eul_c" );

} /* End m2eul_c */
