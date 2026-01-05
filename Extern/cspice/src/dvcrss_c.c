/*

-Procedure dvcrss_c ( Derivative of Vector cross product )

-Abstract

   Compute the cross product of two 3-dimensional vectors
   and the derivative of this cross product.

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

   DERIVATIVE
   VECTOR

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #undef   dvcrss_c

   void dvcrss_c ( ConstSpiceDouble s1  [6],
                   ConstSpiceDouble s2  [6],
                   SpiceDouble      sout[6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   s1         I   Left hand state for cross product and derivative.
   s2         I   Right hand state for cross product and derivative.
   sout       O   State associated with cross product of positions.

-Detailed_Input

   s1          is any state vector. Typically, this might represent the
               apparent state of a planet or the Sun, which defines the
               orientation of axes of some coordinate system.

   s2          is any state vector.

-Detailed_Output

   sout        is the state associated with the cross product of the
               position components of `s1' and `s2'. In other words, if
               `s1' = (p1,v1) and `s2' = (p2,v2) then `sout' is
               ( p1xp2, d/dt( p1xp2 ) ).

-Parameters

   None.

-Exceptions

   Error free.

   1)  If `s1' and `s2' are large in magnitude (taken together,
       their magnitude surpasses the limit allowed by the
       computer) then it may be possible to generate a
       floating point overflow from an intermediate
       computation even though the actual cross product and
       derivative may be well within the range of double
       precision numbers.

       dvcrss_c does NOT check the magnitude of `s1' or `s2' to
       insure that overflow will not occur.

-Files

   None.

-Particulars

   dvcrss_c calculates the three-dimensional cross product of two
   vectors and the derivative of that cross product according to
   the definition. The components of this state are stored
   in a local buffer vector until the calculation is complete.
   Thus sout may overwrite 's1' or 's2'  without interfering with
   intermediate computations.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute the cross product of two 3-dimensional vectors
      and the derivative of this cross product.


      Example code begins here.


      /.
         Program dvcrss_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          sout   [6];

         SpiceInt             i;

         /.
         Set `s1' and `s2' vectors.
         ./
         SpiceDouble          s1     [2][6] = {
                                            {0.0, 1.0, 0.0, 1.0, 0.0, 0.0},
                                            {5.0, 5.0, 5.0, 1.0, 0.0, 0.0}  };
         SpiceDouble          s2     [2][6] = {
                                         { 1.0,  0.0,  0.0, 1.0, 0.0, 0.0},
                                         {-1.0, -1.0, -1.0, 2.0, 0.0, 0.0}  };

         /.
         For each vector `s1' and `s2', compute their cross product
         and its derivative.
         ./
         for ( i = 0; i < 2; i++ )
         {

            dvcrss_c ( s1[i], s2[i], sout );

            printf( "S1  : %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f\n",
                                     s1[i][0], s1[i][1], s1[i][2],
                                     s1[i][3], s1[i][4], s1[i][5] );
            printf( "S2  : %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f\n",
                                     s2[i][0], s2[i][1], s2[i][2],
                                     s2[i][3], s2[i][4], s2[i][5] );
            printf( "SOUT: %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f\n",
                                        sout[0], sout[1], sout[2],
                                        sout[3], sout[4], sout[5] );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      S1  :    0.0    1.0    0.0    1.0    0.0    0.0
      S2  :    1.0    0.0    0.0    1.0    0.0    0.0
      SOUT:    0.0    0.0   -1.0    0.0    0.0   -1.0

      S1  :    5.0    5.0    5.0    1.0    0.0    0.0
      S2  :   -1.0   -1.0   -1.0    2.0    0.0    0.0
      SOUT:    0.0    0.0    0.0    0.0   11.0  -11.0


   2) One can construct non-inertial coordinate frames from apparent
      positions of objects or defined directions. However, if one
      wants to convert states in this non-inertial frame to states
      in an inertial reference frame, the derivatives of the axes of
      the non-inertial frame are required.

      Define a reference frame with the apparent direction of the
      Sun as seen from Earth as the primary axis X. Use the Earth
      pole vector to define with the primary axis the XY plane of
      the frame, with the primary axis Y pointing in the direction
      of the pole.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: dvcrss_ex2.tm

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
            de421.bsp                     Planetary ephemeris
            pck00008.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00008.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program dvcrss_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          state  [6];
         SpiceDouble          tmpsta [6];
         SpiceDouble          trans  [6][6];
         SpiceDouble          x_new  [6];
         SpiceDouble          y_new  [6];
         SpiceDouble          z_new  [6];
         SpiceDouble          zinert [6];

         /.
         Define the earth body-fixed pole vector (z). The pole
         has no velocity in the Earth fixed frame IAU_EARTH.
         ./
         SpiceDouble          z      [6] = { 0.0, 0.0, 1.0, 0.0, 0.0, 0.0 };

         /.
         Load SPK, PCK, and LSK kernels, use a meta kernel for
         convenience.
         ./
         furnsh_c ( "dvcrss_ex2.tm" );

         /.
         Calculate the state transformation between IAU_EARTH and
         J2000 at an arbitrary epoch.
         ./
         str2et_c ( "Jan 1, 2009", &et );
         sxform_c ( "IAU_EARTH", "J2000", et, trans );

         /.
         Transform the earth pole vector from the IAU_EARTH frame
         to J2000.
         ./
         mxvg_c ( trans, z, 6, 6, zinert );

         /.
         Calculate the apparent state of the Sun from Earth at
         the epoch `et' in the J2000 frame.
         ./
         spkezr_c ( "Sun", et, "J2000", "lt+s", "Earth", state, &lt );

         /.
         Define the X axis of the new frame to aligned with
         the computed state. Calculate the state's unit vector
         and its derivative to get the X axis and its
         derivative.
         ./
         dvhat_c ( state, x_new );

         /.
         Define the Z axis of the new frame as the cross product
         between the computed state and the Earth pole.
         Calculate the Z direction in the new reference frame,
         then calculate the this direction's unit vector and its
         derivative to get the Z axis and its derivative.
         ./
         dvcrss_c ( state, zinert, tmpsta );
         dvhat_c ( tmpsta, z_new );

         /.
         As for `z_new', calculate the Y direction in the new
         reference frame, then calculate this direction's unit
         vector and its derivative to get the Y axis and its
         derivative.
         ./
         ducrss_c ( z_new, state, tmpsta );
         dvhat_c ( tmpsta, y_new );

         /.
         Display the results.
         ./
         printf( "New X-axis:\n" );
         printf( "   position: %15.12f %15.12f %15.12f\n",
                              x_new[0], x_new[1], x_new[2] );
         printf( "   velocity: %15.12f %15.12f %15.12f\n",
                              x_new[3], x_new[4], x_new[5] );
         printf( "New Y-axis:\n" );
         printf( "   position: %15.12f %15.12f %15.12f\n",
                              y_new[0], y_new[1], y_new[2] );
         printf( "   velocity: %15.12f %15.12f %15.12f\n",
                              y_new[3], y_new[4], y_new[5] );
         printf( "New Z-axis:\n" );
         printf( "   position: %15.12f %15.12f %15.12f\n",
                              z_new[0], z_new[1], z_new[2] );
         printf( "   velocity: %15.12f %15.12f %15.12f\n",
                              z_new[3], z_new[4], z_new[5] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      New X-axis:
         position:  0.183446637633 -0.901919663328 -0.391009273602
         velocity:  0.000000202450  0.000000034660  0.000000015033
      New Y-axis:
         position:  0.078846540163 -0.382978080242  0.920386339077
         velocity:  0.000000082384  0.000000032309  0.000000006387
      New Z-axis:
         position: -0.979862518033 -0.199671507623  0.000857203851
         velocity:  0.000000044531 -0.000000218531 -0.000000000036


      Note that these vectors define the transformation between the
      new frame and J2000 at the given `et':

             .-            -.
             |       :      |
             |   R   :  0   |
         M = | ......:......|
             |       :      |
             | dRdt  :  R   |
             |       :      |
             `-            -'

      with

         r    = { {x_new[0], y_new[0], z_new[0]},
                  {x_new[1], y_new[1], z_new[1]},
                  {x_new[2], y_new[2], z_new[2]} }

         dRdt = { {x_new[3], y_new[3], z_new[3]},
                  {x_new[4], y_new[4], z_new[4]},
                  {x_new[5], y_new[5], z_new[5]} }

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 06-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code examples.

   -CSPICE Version 1.0.0, 23-NOV-2009 (EDW)

-Index_Entries

   Compute the derivative of a cross product

-&
*/

{ /* Begin dvcrss_c */

   /*
   Local variables
   */

   SpiceDouble vtemp [3];
   SpiceDouble dvtmp1[6];
   SpiceDouble dvtmp2[6];

   /*
   Calculate the cross product of `s1' and `s2', store it in `vtemp'.
   */
   vcrss_c (s1, s2, vtemp );

   /*
   Calculate the two components of the derivative of s1 x s2.
   */
   vcrss_c ( &(s1[3]), s2,       dvtmp1 );
   vcrss_c ( s1,       &(s2[3]), dvtmp2 );

   /*
   Put all of the pieces into `sout'.
   */
   vequ_c ( vtemp, sout );
   vadd_c ( dvtmp1, dvtmp2, &(sout[3]));

} /* End dvcrss_c */
