/*

-Procedure ducrss_c ( Unit Normalized Cross Product and Derivative )

-Abstract

   Compute the unit vector parallel to the cross product of
   two 3-dimensional vectors and the derivative of this unit vector.

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
   #undef    ducrss_c

   void ducrss_c ( ConstSpiceDouble    s1     [6],
                   ConstSpiceDouble    s2     [6],
                   SpiceDouble         sout   [6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   s1         I   Left hand state for cross product and derivative.
   s2         I   Right hand state for cross product and derivative.
   sout       O   Unit vector and derivative of the cross product.

-Detailed_Input

   s1          is any state vector. Typically, this might represent the
               apparent state of a planet or the Sun, which defines the
               orientation of axes of some coordinate system.

   s2          is any state vector.

-Detailed_Output

   sout        is the unit vector parallel to the cross product of the
               position components of `s1' and `s2' and the derivative of
               the unit vector.

               If the cross product of the position components is
               the zero vector, then the position component of the
               output will be the zero vector. The velocity component
               of the output will simply be the derivative of the
               cross product of the position components of `s1' and `s2'.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If the position components of `s1' and `s2' cross together to
       give a zero vector, the position component of the output
       will be the zero vector. The velocity component of the
       output will simply be the derivative of the cross product
       of the position vectors.

   2)  If `s1' and `s2' are large in magnitude (taken together,
       their magnitude surpasses the limit allowed by the
       computer) then it may be possible to generate a
       floating point overflow from an intermediate
       computation even though the actual cross product and
       derivative may be well within the range of double
       precision numbers.

-Files

   None.

-Particulars

   ducrss_c calculates the unit vector parallel to the cross product
   of two vectors and the derivative of that unit vector.
   The results of the computation may overwrite either of the
   input vectors.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) One can construct non-inertial coordinate frames from apparent
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

         File name: ducrss_ex1.tm

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
         Program ducrss_ex1
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
         furnsh_c ( "ducrss_ex1.tm" );

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
         Define the z axis of the new frame as the cross product
         between the apparent direction of the Sun and the Earth
         pole. `z_new' cross `x_new' defines the Y axis of the
         derived frame.
         ./
         dvhat_c ( state, x_new );
         ducrss_c ( state, zinert, z_new );
         ducrss_c ( z_new, state, y_new );

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

   1)  No checking of `s1' or `s2' is done to prevent floating point
       overflow. The user is required to determine that the magnitude
       of each component of the states is within an appropriate range
       so as not to cause floating point overflow. In almost every
       case there will be no problem and no checking actually needs
       to be done.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 02-JUL-2021 (JDR)

       Rebuilt the wrapper to directly call the f2c'd version of the API,
       which scales the inputs to reduce chance of numeric overflow.

       Edited the header to comply with NAIF standard. Added complete
       code example.

   -CSPICE Version 1.0.0, 23-NOV-2009 (EDW) (NJB)

-Index_Entries

   Compute a unit cross product and its derivative

-&
*/

{ /* Begin ducrss_c */

   /*
   Error free:  no error tracing required.
   */

   /*
   Call the f2c'd Fortran routine.
   */
   ducrss_ (  ( doublereal * )  s1,
              ( doublereal * )  s2,
              ( doublereal * )  sout   );

} /* End ducrss_c */
