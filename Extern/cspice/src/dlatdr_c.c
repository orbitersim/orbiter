/*

-Procedure dlatdr_c ( Derivative of latitudinal w.r.t. rectangular )

-Abstract

   Compute the Jacobian matrix of the transformation from
   rectangular to latitudinal coordinates.

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

   COORDINATES
   DERIVATIVES
   MATRIX

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"


   void dlatdr_c ( SpiceDouble   x,
                   SpiceDouble   y,
                   SpiceDouble   z,
                   SpiceDouble   jacobi[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   x          I   X-coordinate of point.
   y          I   Y-coordinate of point.
   z          I   Z-coordinate of point.
   jacobi     O   Matrix of partial derivatives.

-Detailed_Input

   x,
   y,
   z           are the rectangular coordinates of the point at
               which the Jacobian of the map from rectangular
               to latitudinal coordinates is desired.

-Detailed_Output

   jacobi      is the matrix of partial derivatives of the conversion
               between rectangular and latitudinal coordinates. It
               has the form

                  .-                             -.
                  |  dr/dx     dr/dy     dr/dz    |
                  |  dlon/dx   dlon/dy   dlon/dz  |
                  |  dlat/dx   dlat/dy   dlat/dz  |
                  `-                             -'

               evaluated at the input values of x, y, and z.

-Parameters

   None.

-Exceptions

   1)  If the input point is on the Z-axis (X and Y = 0), the Jacobian
       is undefined, the error SPICE(POINTONZAXIS) is signaled by a
       routine in the call tree of this routine.

-Files

   None.

-Particulars

   When performing vector calculations with velocities it is
   usually most convenient to work in rectangular coordinates.
   However, once the vector manipulations have been performed
   it is often desirable to convert the rectangular representations
   into latitudinal coordinates to gain insights about phenomena
   in this coordinate frame.

   To transform rectangular velocities to derivatives of coordinates
   in a latitudinal system, one uses the Jacobian of the
   transformation between the two systems.

   Given a state in rectangular coordinates

      ( x, y, z, dx, dy, dz )

   the corresponding latitudinal coordinate derivatives are given by
   the matrix equation:

                       t          |                     t
      (dr, dlon, dlat)   = jacobi |        * (dx, dy, dz)
                                  |(x,y,z)

   This routine computes the matrix

            |
      jacobi|
            |(x, y, z)

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the latitudinal state of the Earth as seen from
      Mars in the IAU_MARS reference frame at January 1, 2005 TDB.
      Map this state back to rectangular coordinates as a check.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: dlatdr_ex1.tm

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
            pck00010.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00010.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program dlatdr_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          drectn [3];
         SpiceDouble          et;
         SpiceDouble          jacobi [3][3];
         SpiceDouble          lat;
         SpiceDouble          lon;
         SpiceDouble          lt;
         SpiceDouble          latvel [3];
         SpiceDouble          rectan [3];
         SpiceDouble          r;
         SpiceDouble          state  [6];

         /.
         Load SPK, PCK and LSK kernels, use a meta kernel for
         convenience.
         ./
         furnsh_c ( "dlatdr_ex1.tm" );

         /.
         Look up the apparent state of earth as seen from Mars
         at January 1, 2005 TDB, relative to the IAU_MARS reference
         frame.
         ./
         str2et_c ( "January 1, 2005 TDB", &et );

         spkezr_c ( "Earth", et, "IAU_MARS", "LT+S", "Mars", state, &lt );

         /.
         Convert position to latitudinal coordinates.
         ./
         reclat_c ( state, &r, &lon, &lat );

         /.
         Convert velocity to latitudinal coordinates.
         ./

         dlatdr_c ( state[0], state[1], state[2], jacobi );

         mxv_c ( jacobi, state+3, latvel );

         /.
         As a check, convert the latitudinal state back to
         rectangular coordinates.
         ./
         latrec_c ( r, lon, lat, rectan );

         drdlat_c ( r, lon, lat, jacobi );

         mxv_c ( jacobi, latvel, drectn );

         printf( " \n" );
         printf( "Rectangular coordinates:\n" );
         printf( " \n" );
         printf( " X (km)                 =  %17.8e\n", state[0] );
         printf( " Y (km)                 =  %17.8e\n", state[1] );
         printf( " Z (km)                 =  %17.8e\n", state[2] );
         printf( " \n" );
         printf( "Rectangular velocity:\n" );
         printf( " \n" );
         printf( " dX/dt (km/s)           =  %17.8e\n", state[3] );
         printf( " dY/dt (km/s)           =  %17.8e\n", state[4] );
         printf( " dZ/dt (km/s)           =  %17.8e\n", state[5] );
         printf( " \n" );
         printf( "Latitudinal coordinates:\n" );
         printf( " \n" );
         printf( " Radius    (km)         =  %17.8e\n", r );
         printf( " Longitude (deg)        =  %17.8e\n", lon/rpd_c() );
         printf( " Latitude  (deg)        =  %17.8e\n", lat/rpd_c() );
         printf( " \n" );
         printf( "Latitudinal velocity:\n" );
         printf( " \n" );
         printf( " d Radius/dt    (km/s)  =  %17.8e\n", latvel[0] );
         printf( " d Longitude/dt (deg/s) =  %17.8e\n", latvel[1]/rpd_c() );
         printf( " d Latitude/dt  (deg/s) =  %17.8e\n", latvel[2]/rpd_c() );
         printf( " \n" );
         printf( "Rectangular coordinates from inverse mapping:\n" );
         printf( " \n" );
         printf( " X (km)                 =  %17.8e\n", rectan[0] );
         printf( " Y (km)                 =  %17.8e\n", rectan[1] );
         printf( " Z (km)                 =  %17.8e\n", rectan[2] );
         printf( " \n" );
         printf( "Rectangular velocity from inverse mapping:\n" );
         printf( " \n" );
         printf( " dX/dt (km/s)           =  %17.8e\n", drectn[0] );
         printf( " dY/dt (km/s)           =  %17.8e\n", drectn[1] );
         printf( " dZ/dt (km/s)           =  %17.8e\n", drectn[2] );
         printf( " \n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Rectangular coordinates:

       X (km)                 =    -7.60961826e+07
       Y (km)                 =     3.24363805e+08
       Z (km)                 =     4.74704840e+07

      Rectangular velocity:

       dX/dt (km/s)           =     2.29520749e+04
       dY/dt (km/s)           =     5.37601112e+03
       dZ/dt (km/s)           =    -2.08811490e+01

      Latitudinal coordinates:

       Radius    (km)         =     3.36535219e+08
       Longitude (deg)        =     1.03202903e+02
       Latitude  (deg)        =     8.10898662e+00

      Latitudinal velocity:

       d Radius/dt    (km/s)  =    -1.12116011e+01
       d Longitude/dt (deg/s) =    -4.05392876e-03
       d Latitude/dt  (deg/s) =    -3.31899303e-06

      Rectangular coordinates from inverse mapping:

       X (km)                 =    -7.60961826e+07
       Y (km)                 =     3.24363805e+08
       Z (km)                 =     4.74704840e+07

      Rectangular velocity from inverse mapping:

       dX/dt (km/s)           =     2.29520749e+04
       dY/dt (km/s)           =     5.37601112e+03
       dZ/dt (km/s)           =    -2.08811490e+01


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Added complete code example.

   -CSPICE Version 1.0.0, 13-JUL-2001 (WLT) (NJB)

-Index_Entries

   Jacobian of rectangular w.r.t. latitudinal coordinates

-&
*/

{ /* Begin dlatdr_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "dlatdr_c" );


   dlatdr_ ( (doublereal *) &x,
             (doublereal *) &y,
             (doublereal *) &z,
             (doublereal *) jacobi  );

   /*
   Transpose the Jacobian to create a C-style matrix.
   */
   xpose_c ( jacobi, jacobi );


   chkout_c ( "dlatdr_c" );

} /* End dlatdr_c */
