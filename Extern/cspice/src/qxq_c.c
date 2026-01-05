/*

-Procedure qxq_c ( Quaternion times quaternion )

-Abstract

   Multiply two quaternions.

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
   POINTING
   ROTATION

*/

   #include "SpiceUsr.h"
   #undef   qxq_c


   void qxq_c ( ConstSpiceDouble    q1   [4],
                ConstSpiceDouble    q2   [4],
                SpiceDouble         qout [4]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   q1         I   First SPICE quaternion factor.
   q2         I   Second SPICE quaternion factor.
   qout       O   Product of `q1' and `q2'.

-Detailed_Input

   q1          is a 4-vector representing a SPICE-style quaternion.
               See the discussion of "Quaternion Styles" in the
               -Particulars section below.

               Note that multiple styles of quaternions are in use.
               This routine will not work properly if the input
               quaternions do not conform to the SPICE convention.

   q2          is a second SPICE-style quaternion.

-Detailed_Output

   qout        is 4-vector representing the quaternion product

                  q1 * q2

               Representing q(i) as the sums of scalar (real)
               part s(i) and vector (imaginary) part v(i)
               respectively,

                  q1 = s1 + v1
                  q2 = s2 + v2

               qout has scalar part s3 defined by

                  s3 = s1 * s2 - <v1, v2>

               and vector part v3 defined by

                  v3 = s1 * v2  +  s2 * v1  +  v1 x v2

               where the notation < , > denotes the inner
               product operator and x indicates the cross
               product operator.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

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

   Let `m' be a rotation matrix such that for any vector `v',

      m*v

   is the result of rotating `v' by theta radians in the
   counterclockwise direction about unit rotation axis vector `a'.
   Then the SPICE quaternions representing `m' are

      (+/-) (  cos(theta/2),
               sin(theta/2) * a(0),
               sin(theta/2) * a(1),
               sin(theta/2) * a(2)  )

   while the engineering quaternions representing `m' are

      (+/-) ( -sin(theta/2) * a(0),
              -sin(theta/2) * a(1),
              -sin(theta/2) * a(2),
               cos(theta/2)         )

   For both styles of quaternions, if a quaternion `q' represents
   a rotation matrix `m', then -q represents `m' as well.

   Given an engineering quaternion

      qeng   = ( q0,  q1,  q2,  q3 )

   the equivalent SPICE quaternion is

      qspice = ( q3, -q0, -q1, -q2 )


   Associating SPICE Quaternions with Rotation Matrices
   ----------------------------------------------------

   Let `from' and `to' be two right-handed reference frames, for
   example, an inertial frame and a spacecraft-fixed frame. Let the
   symbols

      v    ,   v
       from     to

   denote, respectively, an arbitrary vector expressed relative to
   the `from' and `to' frames. Let `m' denote the transformation matrix
   that transforms vectors from frame `from' to frame `to'; then

      v   =  m * v
       to         from

   where the expression on the right hand side represents left
   multiplication of the vector by the matrix.

   Then if the unit-length SPICE quaternion `q' represents `m', where

      q = (q0, q1, q2, q3)

   the elements of `m' are derived from the elements of `q' as follows:

        .-                                                           -.
        |            2    2                                           |
        |  1 - 2*( q2 + q3 )   2*(q1*q2 - q0*q3)   2*(q1*q3 + q0*q2)  |
        |                                                             |
        |                                                             |
        |                                2    2                       |
    m = |  2*(q1*q2 + q0*q3)   1 - 2*( q1 + q3 )   2*(q2*q3 - q0*q1)  |
        |                                                             |
        |                                                             |
        |                                                    2    2   |
        |  2*(q1*q3 - q0*q2)   2*(q2*q3 + q0*q1)   1 - 2*( q1 + q2 )  |
        |                                                             |
        `-                                                           -'

   Note that substituting the elements of -q for those of `q' in the
   right hand side leaves each element of `m' unchanged; this shows
   that if a quaternion `q' represents a matrix `m', then so does the
   quaternion -q.

   To map the rotation matrix `m' to a unit quaternion, we start by
   decomposing the rotation matrix as a sum of symmetric
   and skew-symmetric parts:

                                        2
      m = [ I  +  (1-cos(theta)) * omega  ] + [ sin(theta) * omega ]

                       symmetric                 skew-symmetric


   `omega' is a skew-symmetric matrix of the form

                 .-               -.
                 |   0   -n2   n1  |
                 |                 |
       omega  =  |   n2   0   -n0  |
                 |                 |
                 |  -n1   n0   0   |
                 `-               -'

   The vector `n' of matrix entries (n0, n1, n2) is the rotation axis
   of `m' and `theta' is m's rotation angle. Note that `n' and `theta'
   are not unique.

   Let

      cth = cos(theta/2)
      sth = sin(theta/2)

   Then the unit quaternions `q' corresponding to `m' are

      q = +/- ( cth, sth*n0, sth*n1, sth*n2 )

   The mappings between quaternions and the corresponding rotations
   are carried out by the CSPICE routines

      q2m_c {quaternion to matrix}
      m2q_c {matrix to quaternion}

   m2q_c always returns a quaternion with scalar part greater than
   or equal to zero.


   SPICE Quaternion Multiplication Formula
   ---------------------------------------

   Given a SPICE quaternion

      q = ( q0, q1, q2, q3 )

   corresponding to rotation axis `a' and angle `theta' as above, we can
   represent `q' using "scalar + vector" notation as follows:

      s =   q0           = cos(theta/2)

      v = ( q1, q2, q3 ) = sin(theta/2) * a

      q = s + v

   Let `quat1' and `quat2' be SPICE quaternions with respective scalar
   and vector parts `s1', `s2' and `v1', `v2':

      quat1 = s1 + v1
      quat2 = s2 + v2

   We represent the dot product of `v1' and `v2' by

      <v1, v2>

   and the cross product of `v1' and `v2' by

      v1 x v2

   Then the SPICE quaternion product is

      quat1*quat2 = s1*s2 - <v1,v2>  + s1*v2 + s2*v1 + (v1 x v2)

   If `quat1' and `quat2' represent the rotation matrices `m1' and `m2'
   respectively, then the quaternion product

      quat1*quat1

   represents the matrix product

      m1*m2

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given the "basis" quaternions:

         qid:  ( 1.0, 0.0, 0.0, 0.0 )
         qi :  ( 0.0, 1.0, 0.0, 0.0 )
         qj :  ( 0.0, 0.0, 1.0, 0.0 )
         qk :  ( 0.0, 0.0, 0.0, 1.0 )

      the following quaternion products give these results:

          Product       Expected result
         -----------   ----------------------
          qi  * qj     ( 0.0, 0.0, 0.0, 1.0 )
          qj  * qk     ( 0.0, 1.0, 0.0, 0.0 )
          qk  * qi     ( 0.0, 0.0, 1.0, 0.0 )
          qi  * qi     (-1.0, 0.0, 0.0, 0.0 )
          qj  * qj     (-1.0, 0.0, 0.0, 0.0 )
          qk  * qk     (-1.0, 0.0, 0.0, 0.0 )
          qid * qi     ( 0.0, 1.0, 0.0, 0.0 )
          qi  * qid    ( 0.0, 1.0, 0.0, 0.0 )
          qid * qj     ( 0.0, 0.0, 1.0, 0.0 )

      The following code example uses QXQ to produce these results.


      Example code begins here.


      /.
         Program qxq_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          qout   [4];

         /.
         Let `qid', `qi', `qj', `qk' be the "basis"
         quaternions.
         ./
         SpiceDouble          qid    [4] = { 1.0,  0.0,  0.0,  0.0 };
         SpiceDouble          qi     [4] = { 0.0,  1.0,  0.0,  0.0 };
         SpiceDouble          qj     [4] = { 0.0,  0.0,  1.0,  0.0 };
         SpiceDouble          qk     [4] = { 0.0,  0.0,  0.0,  1.0 };

         /.
         Compute:

            qi x qj = qk
            qj x qk = qi
            qk x qi = qj
         ./
         qxq_c ( qi, qj, qout );
         printf( "qi x qj  = %7.1f %7.1f %7.1f %7.1f\n",
                             qout[0], qout[1], qout[2], qout[3] );
         printf( "     qk  = %7.1f %7.1f %7.1f %7.1f\n",
                             qk[0],   qk[1],   qk[2],   qk[3]   );
         printf( " \n" );

         qxq_c ( qj, qk, qout );
         printf( "qj x qk  = %7.1f %7.1f %7.1f %7.1f\n",
                             qout[0], qout[1], qout[2], qout[3] );
         printf( "     qi  = %7.1f %7.1f %7.1f %7.1f\n",
                             qi[0],   qi[1],   qi[2],   qi[3]   );
         printf( " \n" );

         qxq_c ( qk, qi, qout );
         printf( "qk x qi  = %7.1f %7.1f %7.1f %7.1f\n",
                             qout[0], qout[1], qout[2], qout[3] );
         printf( "     qj  = %7.1f %7.1f %7.1f %7.1f\n",
                             qj[0],   qj[1],   qj[2],   qj[3]   );
         printf( " \n" );

         /.
         Compute:

            qi x qi  ==  -qid
            qj x qj  ==  -qid
            qk x qk  ==  -qid
         ./
         qxq_c ( qi, qi, qout );
         printf( "qi x qi  = %7.1f %7.1f %7.1f %7.1f\n",
                             qout[0], qout[1], qout[2], qout[3] );
         printf( "     qid = %7.1f %7.1f %7.1f %7.1f\n",
                             qid[0],   qid[1],  qid[2],  qid[3] );
         printf( " \n" );

         qxq_c ( qj, qj, qout );
         printf( "qj x qj  = %7.1f %7.1f %7.1f %7.1f\n",
                             qout[0], qout[1], qout[2], qout[3] );
         printf( "     qid = %7.1f %7.1f %7.1f %7.1f\n",
                             qid[0],   qid[1],  qid[2],  qid[3] );
         printf( " \n" );

         qxq_c ( qk, qk, qout );
         printf( "qk x qk  = %7.1f %7.1f %7.1f %7.1f\n",
                             qout[0], qout[1], qout[2], qout[3] );
         printf( "     qid = %7.1f %7.1f %7.1f %7.1f\n",
                             qid[0],   qid[1],  qid[2],  qid[3] );
         printf( " \n" );

         /.
         Compute:

            qid x qi  = qi
            qi  x qid = qi
            qid x qj  = qj
         ./
         qxq_c ( qid, qi, qout );
         printf( "qid x qi = %7.1f %7.1f %7.1f %7.1f\n",
                             qout[0], qout[1], qout[2], qout[3] );
         printf( "      qi = %7.1f %7.1f %7.1f %7.1f\n",
                             qi[0],   qi[1],   qi[2],   qi[3]   );
         printf( " \n" );

         qxq_c ( qi, qid, qout );
         printf( "qi x qid = %7.1f %7.1f %7.1f %7.1f\n",
                             qout[0], qout[1], qout[2], qout[3] );
         printf( "      qi = %7.1f %7.1f %7.1f %7.1f\n",
                             qi[0],   qi[1],   qi[2],   qi[3]   );
         printf( " \n" );

         qxq_c ( qid, qj, qout );
         printf( "qid x qj = %7.1f %7.1f %7.1f %7.1f\n",
                             qout[0], qout[1], qout[2], qout[3] );
         printf( "      qj = %7.1f %7.1f %7.1f %7.1f\n",
                             qj[0],   qj[1],   qj[2],   qj[3]   );
         printf( " \n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      qi x qj  =     0.0     0.0     0.0     1.0
           qk  =     0.0     0.0     0.0     1.0

      qj x qk  =     0.0     1.0     0.0     0.0
           qi  =     0.0     1.0     0.0     0.0

      qk x qi  =     0.0     0.0     1.0     0.0
           qj  =     0.0     0.0     1.0     0.0

      qi x qi  =    -1.0     0.0     0.0     0.0
           qid =     1.0     0.0     0.0     0.0

      qj x qj  =    -1.0     0.0     0.0     0.0
           qid =     1.0     0.0     0.0     0.0

      qk x qk  =    -1.0     0.0     0.0     0.0
           qid =     1.0     0.0     0.0     0.0

      qid x qi =     0.0     1.0     0.0     0.0
            qi =     0.0     1.0     0.0     0.0

      qi x qid =     0.0     1.0     0.0     0.0
            qi =     0.0     1.0     0.0     0.0

      qid x qj =     0.0     0.0     1.0     0.0
            qj =     0.0     0.0     1.0     0.0


   2) Compute the composition of two rotation matrices by
      converting them to quaternions and computing their
      product, and by directly multiplying the matrices.

      Example code begins here.


      /.
         Program qxq_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          cmout  [3][3];
         SpiceDouble          q1     [4];
         SpiceDouble          q2     [4];
         SpiceDouble          qout   [4];

         SpiceDouble          cmat1  [3][3] = { {1.0,  0.0,  0.0},
                                                {0.0, -1.0,  0.0},
                                                {0.0,  0.0, -1.0} };

         SpiceDouble          cmat2  [3][3] = { {0.0,  1.0,  0.0},
                                                {1.0,  0.0,  0.0},
                                                {0.0,  0.0, -1.0} };

         /.
         Convert the C-matrices to quaternions.
         ./
         m2q_c ( cmat1, q1 );
         m2q_c ( cmat2, q2 );

         /.
         Find the product.
         ./
         qxq_c ( q1, q2, qout );

         /.
         Convert the result to a C-matrix.
         ./
         q2m_c ( qout, cmout );

         printf( "Using quaternion product:\n" );
         printf( "%9.4f %9.4f %9.4f\n",
                 cmout[0][0], cmout[0][1], cmout[0][2] );
         printf( "%9.4f %9.4f %9.4f\n",
                 cmout[1][0], cmout[1][1], cmout[1][2] );
         printf( "%9.4f %9.4f %9.4f\n",
                 cmout[2][0], cmout[2][1], cmout[2][2] );

         /.
         Multiply `cmat1' and `cmat2' directly.
         ./
         mxm_c ( cmat1, cmat2, cmout );

         printf( "Using matrix product:\n" );
         printf( "%9.4f %9.4f %9.4f\n",
                 cmout[0][0], cmout[0][1], cmout[0][2] );
         printf( "%9.4f %9.4f %9.4f\n",
                 cmout[1][0], cmout[1][1], cmout[1][2] );
         printf( "%9.4f %9.4f %9.4f\n",
                 cmout[2][0], cmout[2][1], cmout[2][2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Using quaternion product:
         0.0000    1.0000    0.0000
        -1.0000    0.0000    0.0000
         0.0000    0.0000    1.0000
      Using matrix product:
         0.0000    1.0000    0.0000
        -1.0000    0.0000    0.0000
         0.0000    0.0000    1.0000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.2, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Created complete code examples from existing example and code
       fragments.

   -CSPICE Version 1.0.1, 27-FEB-2008 (NJB)

       Updated header; added information about SPICE
       quaternion conventions.

   -CSPICE Version 1.0.0, 27-OCT-2005 (NJB)

-Index_Entries

   quaternion times quaternion
   multiply quaternion by quaternion

-&
*/

{ /* Begin qxq_c */

   /*
   Local variables
   */
   SpiceDouble             cross[3];


   /*
   This routine is error free.
   */

   /*
   Assign the scalar portion of the product `vout'.
   */
   qout[0]  =  q1[0]*q2[0] - vdot_c( q1+1, q2+1 );

   /*
   Compute the cross product term of the vector component of
   vout.
   */
   vcrss_c ( q1+1, q2+1, cross );

   /*
   Assign the vector portion of the product `vout'.
   */
   vlcom3_c ( q1[0],   q2+1,
              q2[0],   q1+1,
              1.0,     cross,   qout+1 );


} /* End qxq_c */
