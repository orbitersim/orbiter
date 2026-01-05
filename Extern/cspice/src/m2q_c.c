/*

-Procedure m2q_c ( Matrix to quaternion )

-Abstract

   Find a unit quaternion corresponding to a specified rotation
   matrix.

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

   MATH
   MATRIX
   ROTATION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    m2q_c


   void m2q_c (  ConstSpiceDouble  r[3][3],
                 SpiceDouble       q[4]     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   r          I   A rotation matrix.
   q          O   A unit quaternion representing `r'.

-Detailed_Input

   r           is a rotation matrix.

-Detailed_Output

   q           is a unit-length SPICE-style quaternion representing
               `r'. See the discussion of quaternion styles in
               -Particulars below.

               `q' is a 4-dimensional vector. If `r' rotates vectors in
               the counterclockwise sense by an angle of `theta' radians
               about a unit vector `a', where

                  0 < theta < pi
                    -       -

               then letting h = theta/2,

                  q = ( cos(h), sin(h)a ,  sin(h)a ,  sin(h)a ).
                                       1          2          3

               The restriction that `theta' must be in the range [0, pi]
               determines the output quaternion `q' uniquely
               except when theta = pi; in this special case, both of
               the quaternions

                  q = ( 0,  a ,  a ,  a  )
                             1    2    3
               and

                  q = ( 0, -a , -a , -a  )
                             1    2    3

               are possible outputs.

-Parameters

   None.

-Exceptions

   1)  If `r' is not a rotation matrix, the error SPICE(NOTAROTATION)
       is signaled by a routine in the call tree of this routine.

-Files

   None.

-Particulars

   A unit quaternion is a 4-dimensional vector for which the sum of
   the squares of the components is 1. Unit quaternions can be used
   to represent rotations in the following way: given a rotation
   angle `theta', where

      0 < theta < pi
        -       -

   and a unit vector `a', we can represent the transformation that
   rotates vectors in the counterclockwise sense by theta radians about
   `a' using the quaternion `q', where

      q = ( cos(theta/2), sin(theta/2)a , sin(theta/2)a , sin(theta/2)a )
                                       1               2               3

   As mentioned in Detailed Output, our restriction on the range of
   `theta' determines `q' uniquely, except when theta = pi.

   The CSPICE routine q2m_c is an one-sided inverse of this routine:
   given any rotation matrix `r', the calls

      m2q_c ( r, q );
      q2m_c ( q, r );

   leave `r' unchanged, except for round-off error. However, the
   calls

      q2m_c ( q, r );
      m2q_c ( r, q );

   might preserve `q' or convert `q' to -q.


   Quaternion Styles
   -----------------

   There are different "styles" of quaternions used in
   science and engineering applications. Quaternion styles
   are characterized by

   -  The order of quaternion elements

   -  The quaternion multiplication formula

   -  The convention for associating quaternions
      with rotation matrices

   Two of the commonly used styles are

      - "SPICE"

         > Invented by Sir William Rowan Hamilton
         > Frequently used in mathematics and physics textbooks

      - "Engineering"

         > Widely used in aerospace engineering applications


   CSPICE function interfaces ALWAYS use SPICE quaternions.
   Quaternions of any other style must be converted to SPICE
   quaternions before they are passed to CSPICE functions.


   Relationship between SPICE and Engineering Quaternions
   ------------------------------------------------------

   Let M be a rotation matrix such that for any vector V,

      M*V

   is the result of rotating V by theta radians in the
   counterclockwise direction about unit rotation axis vector A.
   Then the SPICE quaternions representing M are

      (+/-) (  cos(theta/2),
               sin(theta/2) A(1),
               sin(theta/2) A(2),
               sin(theta/2) A(3)  )

   while the engineering quaternions representing M are

      (+/-) ( -sin(theta/2) A(1),
              -sin(theta/2) A(2),
              -sin(theta/2) A(3),
               cos(theta/2)       )

   For both styles of quaternions, if a quaternion q represents
   a rotation matrix M, then -q represents M as well.

   Given an engineering quaternion

      QENG   = ( q0,  q1,  q2,  q3 )

   the equivalent SPICE quaternion is

      QSPICE = ( q3, -q0, -q1, -q2 )


   Associating SPICE Quaternions with Rotation Matrices
   ----------------------------------------------------

   Let FROM and TO be two right-handed reference frames, for
   example, an inertial frame and a spacecraft-fixed frame. Let the
   symbols

      V    ,   V
       FROM     TO

   denote, respectively, an arbitrary vector expressed relative to
   the FROM and TO frames. Let M denote the transformation matrix
   that transforms vectors from frame FROM to frame TO; then

      V   =  M * V
       TO         FROM

   where the expression on the right hand side represents left
   multiplication of the vector by the matrix.

   Then if the unit-length SPICE quaternion q represents M, where

      q = (q0, q1, q2, q3)

   the elements of M are derived from the elements of q as follows:

        +-                                                         -+
        |           2    2                                          |
        | 1 - 2*( q2 + q3 )   2*(q1*q2 - q0*q3)   2*(q1*q3 + q0*q2) |
        |                                                           |
        |                                                           |
        |                               2    2                      |
    M = | 2*(q1*q2 + q0*q3)   1 - 2*( q1 + q3 )   2*(q2*q3 - q0*q1) |
        |                                                           |
        |                                                           |
        |                                                   2    2  |
        | 2*(q1*q3 - q0*q2)   2*(q2*q3 + q0*q1)   1 - 2*( q1 + q2 ) |
        |                                                           |
        +-                                                         -+

   Note that substituting the elements of -q for those of q in the
   right hand side leaves each element of M unchanged; this shows
   that if a quaternion q represents a matrix M, then so does the
   quaternion -q.

   To map the rotation matrix M to a unit quaternion, we start by
   decomposing the rotation matrix as a sum of symmetric
   and skew-symmetric parts:

                                      2
      M = [ I  +  (1-cos(theta)) OMEGA  ] + [ sin(theta) OMEGA ]

                   symmetric                   skew-symmetric


   OMEGA is a skew-symmetric matrix of the form

                 +-             -+
                 |  0   -n3   n2 |
                 |               |
       OMEGA  =  |  n3   0   -n1 |
                 |               |
                 | -n2   n1   0  |
                 +-             -+

   The vector N of matrix entries (n1, n2, n3) is the rotation axis
   of M and theta is M's rotation angle. Note that N and theta
   are not unique.

   Let

      C = cos(theta/2)
      S = sin(theta/2)

   Then the unit quaternions Q corresponding to M are

      Q = +/- ( C, S*n1, S*n2, S*n3 )

   The mappings between quaternions and the corresponding rotations
   are carried out by the CSPICE routines

      q2m_c {quaternion to matrix}
      m2q_c {matrix to quaternion}

   m2q_c always returns a quaternion with scalar part greater than
   or equal to zero.


   SPICE Quaternion Multiplication Formula
   ---------------------------------------

   Given a SPICE quaternion

      Q = ( q0, q1, q2, q3 )

   corresponding to rotation axis A and angle theta as above, we can
   represent Q using "scalar + vector" notation as follows:

      s =   q0           = cos(theta/2)

      v = ( q1, q2, q3 ) = sin(theta/2) * A

      Q = s + v

   Let Q1 and Q2 be SPICE quaternions with respective scalar
   and vector parts s1, s2 and v1, v2:

      Q1 = s1 + v1
      Q2 = s2 + v2

   We represent the dot product of v1 and v2 by

      <v1, v2>

   and the cross product of v1 and v2 by

      v1 x v2

   Then the SPICE quaternion product is

      Q1*Q2 = s1*s2 - <v1,v2>  + s1*v2 + s2*v1 + (v1 x v2)

   If Q1 and Q2 represent the rotation matrices M1 and M2
   respectively, then the quaternion product

      Q1*Q2

   represents the matrix product

      M1*M2

-Examples

   1)  A case amenable to checking by hand calculation:

          To convert the rotation matrix

                   +-              -+
                   |  0     1    0  |
                   |                |
             r  =  | -1     0    0  |
                   |                |
                   |  0     0    1  |
                   +-              -+

          also represented as

             [ pi/2 ]
                     3

          to a quaternion, we can use the code fragment

             rotate_c (  halfpi_c(),  3,  r );
             m2q_c    (  r,               q );

          m2q_c will return `q' as

             ( sqrt(2)/2, 0, 0, -sqrt(2)/2 )

          Why?  Well, `r' is a reference frame transformation that
          rotates vectors by -pi/2 radians about the axis vector

              a = ( 0, 0, 1 )

          Equivalently, `r' rotates vectors by pi/2 radians in
          the counterclockwise sense about the axis vector

             -a = ( 0, 0, -1 )

          so our definition of `q',

             h = theta/2

             q = ( cos(h), sin(h)a , sin(h)a , sin(h)a  )
                                  1         2         3

          implies that in this case,

             q =  ( cos(pi/4),  0,  0,  -sin(pi/4) )

               =  ( sqrt(2)/2,  0,  0,  -sqrt(2)/2 )


   2)  Finding a quaternion that represents a rotation specified by
       a set of Euler angles:

          Suppose our original rotation `r' is the product

             [ tau ]  [ pi/2 - delta ]  [ alpha ] .
                    3                 2          3

          The code fragment

             eul2m_c  ( tau,   halfpi_c() - delta,   alpha,
                        3,     2,                    3,      r );

             m2q_c    ( r, q );

          yields a quaternion `q' that represents `r'.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.2, 24-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Moved ROTATIONS required
       reading from -Literature_References to -Required_Reading section.

   -CSPICE Version 1.1.1, 27-FEB-2008 (NJB)

       Updated header; added information about SPICE
       quaternion conventions. Made minor edits throughout
       header.

   -CSPICE Version 1.1.0, 21-OCT-1998 (NJB)

        Made input matrix const.

   -CSPICE Version 1.0.1, 13-FEB-1998 (EDW)

        Minor corrections to header.

   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB) (WLT)

        Based on SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)

-Index_Entries

   matrix to quaternion

-&
*/

{ /* Begin m2q_c */

   /*
   Local variables
   */
   SpiceDouble             loc_r[3][3];


   /*
   Participate in error tracing.
   */
   chkin_c ( "m2q_c" );


   /*
   Transpose the input matrix to put it in column-major order.
   */
   xpose_c ( r, loc_r );


   /*
   Call the f2c'd version of m2q:
   */
   m2q_ ( (doublereal *) loc_r,
          (doublereal *) q      );


   chkout_c ( "m2q_c" );


} /* End m2q_c */
