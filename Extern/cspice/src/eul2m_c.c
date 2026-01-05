/*

-Procedure eul2m_c ( Euler angles to matrix )

-Abstract

   Construct a rotation matrix from a set of Euler angles.

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
   TRANSFORMATION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void eul2m_c ( SpiceDouble  angle3,
                  SpiceDouble  angle2,
                  SpiceDouble  angle1,
                  SpiceInt     axis3,
                  SpiceInt     axis2,
                  SpiceInt     axis1,
                  SpiceDouble  r [3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   angle3,
   angle2,
   angle1     I   Rotation angles about third, second, and first
                  rotation axes (radians).
   axis3,
   axis2,
   axis1      I   Axis numbers of third, second, and first rotation
                  axes.
   r          O   Product of the 3 rotations.

-Detailed_Input

   angle3,
   angle2,
   angle1,
   axis3,
   axis2,
   axis1       are, respectively, a set of three angles and three
               coordinate axis numbers; each pair angleX and axisX
               specifies a coordinate transformation consisting of a
               rotation by ANGLEx radians about the coordinate axis
               indexed by AXISx. These coordinate transformations are
               typically symbolized by

                  [ angleX ]     .
                            axisX

               See the -Particulars section below for details concerning
               this notation.

               Note that these coordinate transformations rotate vectors
               by

                  -angleX

               radians about the axis indexed by axisX.

               The values of axisX may be 1, 2, or 3, indicating the X,
               Y, and Z axes respectively.

-Detailed_Output

   r           is a rotation matrix representing the composition of the
               rotations defined by the input angle-axis pairs.
               Together, the three pairs specify a composite
               transformation that is the result of performing the
               rotations about the axes indexed by `axis1', `axis2', and
               `axis3', in that order. So,

                  r = [ angle3 ]      [ angle2 ]      [ angle1 ]
                                axis3           axis2           axis1

               See the -Particulars section below for details concerning
               this notation.

               The resulting matrix `r' may be thought of as a coordinate
               transformation; applying it to a vector yields the
               vector's coordinates in the rotated system.

               Viewing `r' as a coordinate transformation matrix, the
               basis that `r' transforms vectors to is created by rotating
               the original coordinate axes first by `angle1' radians
               about the coordinate axis indexed by `axis1', next by
               `angle2' radians about the coordinate axis indexed by
               `axis2', and finally by `angle3' radians about coordinate
               axis indexed by `axis3'. At the second and third steps of
               this process, the coordinate axes about which rotations
               are performed belong to the bases resulting from the
               previous rotations.

-Parameters

   None.

-Exceptions

   1)  If any of `axis3', `axis2', or `axis1' do not have values in

          { 1, 2, 3 }

       the error SPICE(BADAXISNUMBERS) is signaled by a routine in
       the call tree of this routine.

-Files

   None.

-Particulars

   A word about notation: the symbol

      [ x ]
           i

   indicates a rotation of x radians about the ith coordinate axis.
   To be specific, the symbol

      [ x ]
           1

   indicates a coordinate system rotation of x radians about the
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
   radians, not x radians. Applying the matrix to a vector yields
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

   From time to time, (depending on one's line of work, perhaps) one
   may happen upon a pair of coordinate systems related by a
   sequence of rotations. For example, the coordinate system
   defined by an instrument such as a camera is sometime specified
   by RA, DEC, and twist; if alpha, delta, and phi are the rotation
   angles, then the series of rotations

      [ phi ]     [ pi/2  -  delta ]     [ alpha ]
             3                      2             3

   produces a transformation from inertial to camera coordinates.

   This routine is related to the CSPICE routine m2eul_c, which
   produces a sequence of Euler angles, given a rotation matrix.
   This routine is a "left inverse" of m2eul_c: the sequence of
   calls

      m2eul_c ( r,  axis3,   axis2,   axis1,
                   &angle3, &angle2, &angle1     );

      eul2m_c (     angle3,  angle2,  angle1,
                    axis3,   axis2,   axis1,   r );

   preserves `r', except for round-off error.


   On the other hand, the sequence of calls

      eul2m_c (     angle3,  angle2,  angle1,
                    axis3,   axis2,   axis1,   r );

      m2eul_c ( r,  axis3,   axis2,   axis1,
                   &angle3, &angle2, &angle1     );

   preserve `angle3', `angle2', and `angle1' only if these angles start
   out in the ranges that m2eul_c's outputs are restricted to.

-Examples

   1)  Create a coordinate transformation matrix by rotating
       the original coordinate axes first by 30 degrees about
       the z axis, next by 60 degrees about the y axis resulting
       from the first rotation, and finally by -50 degrees about
       the z axis resulting from the first two rotations.

                /.

                Create the coordinate transformation matrix

                              o          o          o
                   R  =  [ -50  ]   [  60  ]   [  30  ]
                                 3          2          3

                All angles in radians, please. The CSPICE
                function rpd_c (radians per degree) gives the
                conversion factor.

                The z axis is `axis 3'; the y axis is `axis 2'.
                ./

                angle1 = rpd_c() *  30.;
                angle2 = rpd_c() *  60.;
                angle3 = rpd_c() * -50.;

                axis1  = 3;
                axis2  = 2;
                axis3  = 3;

                eul2m_c (  angle3, angle2, angle1,
                           axis3,  axis2,  axis1,   r  );


   2)  A trivial example using actual numbers.

       The call

          eul2m_c (  0.,     0.,     halfpi_c(),
                     1,        1,            3,      r  );

       sets r equal to the matrix

          +-                  -+
          |  0      1       0  |
          |                    |
          | -1      0       0  |.
          |                    |
          |  0      0       1  |
          +-                  -+


   3)  Finding the rotation matrix specified by a set of `clock,
       cone, and twist' angles, as defined on the Voyager 2 project:

          Voyager 2 narrow angle camera pointing, relative to the
          Sun-Canopus coordinate system, was frequently specified
          by a set of Euler angles called `clock, cone, and twist'.
          These defined a 3-2-3 coordinate transformation matrix
          TSCTV as the product

             [ twist ]  [ cone ]   [ clock ] .
                      3         2           3

          Given the angles clock, cone, and twist (in units of
          radians), we can compute tsctv with the call

             eul2m_c (  twist,  cone,  clock,
                        3,      2,     3,      tsctv  );


   4)  Finding the rotation matrix specified by a set of `right
       ascension, declination, and twist' angles, as defined on the
       Galileo project:

          Galileo scan platform pointing, relative to an inertial
          reference frame, (EME50 variety) is frequently specified
          by a set of Euler angles called `right ascension (RA),
          declination (Dec), and twist'. These define a 3-2-3
          coordinate transformation matrix TISP as the product

             [ Twist ]  [ pi/2 - Dec ]   [ RA ] .
                      3               2        3

          Given the angles ra, dec, and twist (in units of radians),
          we can compute tisp with the code fragment

             eul2m_c (  twist,   halfpi_c()-dec,   ra,
                        3,       2,                3,   tisp  );

-Restrictions

   1)  Beware: more than one definition of "RA, DEC and twist"
       exists.

-Literature_References

   [1]  W. Owen, "Galileo Attitude and Camera Models," JPL
        Interoffice Memorandum 314-323, Nov. 11, 1983. NAIF document
        number 204.0.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.3, 02-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.2, 26-DEC-2006 (NJB)

       Fixed header typo.

   -CSPICE Version 1.0.1, 13-OCT-2004 (NJB)

       Fixed header typo.

   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB)

       Based on SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)

-Index_Entries

   euler angles to matrix

-&
*/

{ /* Begin eul2m_c */


   /*
   Local variables
   */
   SpiceDouble             loc_r[3][3];

   /*
   Participate in error handling
   */

   chkin_c ( "eul2m_c");


   /*
   Call the f2c'd version of eul2m:
   */
   eul2m_ ( (doublereal *) &angle3,
            (doublereal *) &angle2,
            (doublereal *) &angle1,
            (integer    *) &axis3,
            (integer    *) &axis2,
            (integer    *) &axis1,
            (doublereal *) loc_r        );

   /*
   Transpose the output matrix to put it in row-major order.
   */
   xpose_c ( loc_r, r );


   chkout_c ( "eul2m_c");

} /* End eul2m_c */
