/*

-Procedure xf2rav_c ( Transform to rotation and angular velocity)

-Abstract

   Determine the rotation matrix and angular velocity of the
   rotation from a state transformation matrix.

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

   FRAMES

*/

   #include "SpiceUsr.h"
   #undef xf2rav_c


   void xf2rav_c ( ConstSpiceDouble   xform [6][6],
                   SpiceDouble        rot   [3][3],
                   SpiceDouble        av    [3]     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   xform      I   is a state transformation matrix.
   rot        O   is the rotation associated with `xform'.
   av         O   is the angular velocity associated with `xform'.

-Detailed_Input

   xform       is a state transformation matrix from one frame
               FRAME1 to some other frame FRAME2.

-Detailed_Output

   rot         is a rotation that gives the transformation from
               some frame FRAME1 to another frame FRAME2.

   av          is the angular velocity of the transformation.
               In other words, if `p' is the position of a fixed
               point in FRAME2, then from the point of view of
               FRAME1, `p' rotates (in a right handed sense) about
               an axis parallel to `av'. Moreover the rate of rotation
               in radians per unit time is given by the length of
               `av'.

               More formally, the velocity `v' of `p' in FRAME1 is
               given by
                                  T
                   v  = av x ( rot  * p )

               The components of `av' are given relative to FRAME1.

-Parameters

   None.

-Exceptions

   Error free.

   1)  No checks are performed on `xform' to ensure that it is indeed
       a state transformation matrix.

-Files

   None.

-Particulars

   This routine is essentially a macro routine for converting
   state transformation matrices into the equivalent representation
   in terms of a rotation and angular velocity.

   This routine is an inverse of the routine rav2xf_c.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose that you wanted to determine the angular velocity
      of the Earth body-fixed reference frame with respect to
      J2000 at a particular epoch ET. The following code example
      illustrates a procedure for computing the angular velocity.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: xf2rav_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                     Contents
            ---------                     --------
            earth_720101_070426.bpc       Earth historical
                                          binary PCK
            naif0012.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'earth_720101_070426.bpc',
                                'naif0012.tls'            )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program xf2rav_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define META         "xf2rav_ex1.tm"
         #define UTCSTR       "2005-OCT-10 16:00:00"

         /.
         Local variables.
         ./
         SpiceDouble          av     [3];
         SpiceDouble          et;
         SpiceDouble          ftmtrx [6][6];
         SpiceDouble          rot    [3][3];

         SpiceInt             i;

         /.
         Load SPICE kernels.
         ./
         furnsh_c ( META );

         /.
         Convert the input time to seconds past J2000 TDB.
         ./
         str2et_c ( UTCSTR, &et );

         /.
         Get the transformation matrix from J2000 frame to
         ITRF93.
         ./
         sxform_c ( "J2000", "ITRF93", et, ftmtrx );

         /.
         Now get the angular velocity by calling xf2rav_c
         ./
         xf2rav_c ( ftmtrx, rot, av );

         /.
         Display the results.
         ./
         printf( "Rotation matrix:\n" );
         for ( i = 0; i < 3; i++ )
         {

            printf( "%16.11f %15.11f %15.11f\n",
                    rot[i][0], rot[i][1], rot[i][2] );

         }

         printf( "\n" );
         printf( "Angular velocity:\n" );
         printf( "%16.11f %15.11f %15.11f\n", av[0], av[1], av[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Rotation matrix:
        -0.18603277688  -0.98254352801   0.00014659080
         0.98254338275  -0.18603282936  -0.00053610915
         0.00055402128   0.00004429795   0.99999984555

      Angular velocity:
         0.00000004025   0.00000000324   0.00007292114


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

   -CSPICE Version 1.0.2, 19-MAY-2020 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

       Added ROTATION to the required readings.

   -CSPICE Version 1.0.1, 12-APR-2007 (EDW)

       Edit to abstract.

   -CSPICE Version 1.0.0, 18-JUN-1999 (WLT) (NJB)

-Index_Entries

   State transformation to rotation and angular velocity

-&
*/

{ /* Begin xf2rav_c */

   /*
   Local variables
   */

   SpiceDouble             drdt  [3][3];
   SpiceDouble             omega [3][3];

   SpiceInt                i;
   SpiceInt                j;


   /*
   Error free:  no tracing required.


   A state transformation matrix xform has the following form


       [      |     ]
       |  r   |  0  |
       |      |     |
       | -----+-----|
       |  dr  |     |
       |  --  |  r  |
       [  dt  |     ]


   where r is a rotation and dr/dt is the time derivative of that
   rotation.  From this we can immediately read the rotation and
   its derivative.
   */

   for ( i = 0;  i < 3;  i++ )
      {
      for ( j = 0;  j < 3;  j++ )
         {
         rot [i][j]   =  xform[i  ][j];
         drdt[i][j]   =  xform[i+3][j];
         }
      }


   /*
   Recall that rot is a transformation that converts positions
   in some frame frame1 to positions in a second frame frame2.

   The angular velocity matrix omega (the cross product matrix
   corresponding to av) has the following property.

   If p is the position of an object that is stationary with
   respect to frame2 then the velocity v of that object in frame1
   is given by:
                        t
       v  =  omega * rot  *  p

   But v is also given by

                  t
             d rot
       v =   -----  * p
               dt

   So that
                                t
                  t        d rot
       omega * rot    =   -------
                             dt

   Hence
                           t
                     d rot
       omega    =   -------  *  rot
                       dt

   */

   mtxm_c ( drdt, rot, omega );


   /*

   Recall that omega has the form

       _                     _
      |                       |
      |   0    -av[2]  av[1]  |
      |                       |
      |  av[2]    0   -av[0]  |
      |                       |
      | -av[1]   av[0]   0    |
      |_                     _|

   */


   av[0] = omega[2][1];
   av[1] = omega[0][2];
   av[2] = omega[1][0];


} /* End xf2rav_c */
