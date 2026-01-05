/*

-Procedure drdgeo_c ( Derivative of rectangular w.r.t. geodetic )

-Abstract

   Compute the Jacobian matrix of the transformation from geodetic
   to rectangular coordinates.

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

   void drdgeo_c ( SpiceDouble    lon,
                   SpiceDouble    lat,
                   SpiceDouble    alt,
                   SpiceDouble    re,
                   SpiceDouble    f,
                   SpiceDouble    jacobi[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   lon        I   Geodetic longitude of point (radians).
   lat        I   Geodetic latitude of point (radians).
   alt        I   Altitude of point above the reference spheroid.
   re         I   Equatorial radius of the reference spheroid.
   f          I   Flattening coefficient.
   jacobi     O   Matrix of partial derivatives.

-Detailed_Input

   lon         is the geodetic longitude of point (radians).

   lat         is the geodetic latitude  of point (radians).

   alt         is the altitude of point above the reference spheroid.

   re          is the equatorial radius of the reference spheroid.

   f           is the flattening coefficient = (re-rp) / re,  where `rp' is
               the polar radius of the spheroid. (More importantly
               rp = re*(1-f).)

-Detailed_Output

   jacobi      is the matrix of partial derivatives of the conversion
               between geodetic and rectangular coordinates. It
               has the form

                  .-                             -.
                  |  dx/dlon   dx/dlat  dx/dalt   |
                  |  dy/dlon   dy/dlat  dy/dalt   |
                  |  dz/dlon   dz/dlat  dz/dalt   |
                  `-                             -'

               evaluated at the input values of `lon', `lat' and `alt'.

               The formulae for computing `x', `y', and `z' from
               geodetic coordinates are given below.

                  x = [alt +        re/g(lat,f)]*cos(lon)*cos(lat)


                  y = [alt +        re/g(lat,f)]*sin(lon)*cos(lat)

                                     2
                  z = [alt + re*(1-f) /g(lat,f)]*         sin(lat)

               where

                  re    is the polar radius of the reference spheroid.

                  f     is the flattening factor (the polar radius is
                        obtained by multiplying the equatorial radius by 1-f).

                  g( lat, f ) is given by

                         2             2     2
                     sqrt ( cos (lat) + (1-f) * sin (lat) )

-Parameters

   None.

-Exceptions

   1)  If the flattening coefficient is greater than or equal to one,
       the error SPICE(VALUEOUTOFRANGE) is signaled by a routine in
       the call tree of this routine.

   2)  If the equatorial radius is non-positive, the error
       SPICE(BADRADIUS) is signaled by a routine in the call tree of
       this routine.

-Files

   None.

-Particulars

   It is often convenient to describe the motion of an object in
   the geodetic coordinate system. However, when performing
   vector computations its hard to beat rectangular coordinates.

   To transform states given with respect to geodetic coordinates
   to states with respect to rectangular coordinates, one makes use
   of the Jacobian of the transformation between the two systems.

   Given a state in geodetic coordinates

        ( lon, lat, alt, dlon, dlat, dalt )

   the velocity in rectangular coordinates is given by the matrix
   equation:

                  t          |                                 t
      (dx, dy, dz)   = jacobi|             * (dlon, dlat, dalt)
                             |(lon,lat,alt)


   This routine computes the matrix

            |
      jacobi|
            |(lon,lat,alt)

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the geodetic state of the earth as seen from
      Mars in the IAU_MARS reference frame at January 1, 2005 TDB.
      Map this state back to rectangular coordinates as a check.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: drdgeo_ex1.tm

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
         Program drdgeo_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          alt;
         SpiceDouble          drectn [3];
         SpiceDouble          et;
         SpiceDouble          f;
         SpiceDouble          jacobi [3][3];
         SpiceDouble          lat;
         SpiceDouble          lon;
         SpiceDouble          lt;
         SpiceDouble          geovel [3];
         SpiceDouble          radii  [3];
         SpiceDouble          re;
         SpiceDouble          rectan [3];
         SpiceDouble          rp;
         SpiceDouble          state  [6];

         SpiceInt             n;

         /.
         Load SPK, PCK, and LSK kernels, use a meta kernel for
         convenience.
         ./
         furnsh_c ( "drdgeo_ex1.tm" );

         /.
         Look up the radii for Mars.  Although we
         omit it here, we could first call badkpv_c
         to make sure the variable BODY499_RADII
         has three elements and numeric data type.
         If the variable is not present in the kernel
         pool, bodvrd_c will signal an error.
         ./
         bodvrd_c ( "MARS", "RADII", 3, &n, radii );

         /.
         Compute flattening coefficient.
         ./
         re  =  radii[0];
         rp  =  radii[2];
         f   =  ( re - rp ) / re;

         /.
         Look up the apparent state of earth as seen from Mars at
         January 1, 2005 TDB, relative to the IAU_MARS reference
         frame.
         ./
         str2et_c ( "January 1, 2005 TDB", &et );

         spkezr_c ( "Earth", et, "IAU_MARS", "LT+S", "Mars", state, &lt );

         /.
         Convert position to geodetic coordinates.
         ./
         recgeo_c ( state, re, f, &lon, &lat, &alt );

         /.
         Convert velocity to geodetic coordinates.
         ./

         dgeodr_c ( state[0], state[1], state[2], re, f, jacobi );

         mxv_c ( jacobi, state+3, geovel );

         /.
         As a check, convert the geodetic state back to
         rectangular coordinates.
         ./
         georec_c ( lon, lat, alt, re, f, rectan );

         drdgeo_c ( lon, lat, alt, re, f, jacobi );

         mxv_c ( jacobi, geovel, drectn );

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
         printf( "Ellipsoid shape parameters: \n" );
         printf( " \n" );
         printf( " Equatorial radius (km) =  %17.8e\n", re );
         printf( " Polar radius      (km) =  %17.8e\n", rp );
         printf( " Flattening coefficient =  %17.8e\n", f );
         printf( " \n" );
         printf( "Geodetic coordinates:\n" );
         printf( " \n" );
         printf( " Longitude (deg)        =  %17.8e\n", lon / rpd_c() );
         printf( " Latitude  (deg)        =  %17.8e\n", lat / rpd_c() );
         printf( " Altitude  (km)         =  %17.8e\n", alt );
         printf( " \n" );
         printf( "Geodetic velocity:\n" );
         printf( " \n" );
         printf( " d Longitude/dt (deg/s) =  %17.8e\n", geovel[0]/rpd_c() );
         printf( " d Latitude/dt  (deg/s) =  %17.8e\n", geovel[1]/rpd_c() );
         printf( " d Altitude/dt  (km/s)  =  %17.8e\n", geovel[2] );
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

      Ellipsoid shape parameters:

       Equatorial radius (km) =     3.39619000e+03
       Polar radius      (km) =     3.37620000e+03
       Flattening coefficient =     5.88600756e-03

      Geodetic coordinates:

       Longitude (deg)        =     1.03202903e+02
       Latitude  (deg)        =     8.10898757e+00
       Altitude  (km)         =     3.36531823e+08

      Geodetic velocity:

       d Longitude/dt (deg/s) =    -4.05392876e-03
       d Latitude/dt  (deg/s) =    -3.31899337e-06
       d Altitude/dt  (km/s)  =    -1.12116015e+01

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

       Edited the -Examples section to comply with NAIF standard.
       Added complete code example.

   -CSPICE Version 1.0.0, 20-JUL-2001 (WLT) (NJB)

-Index_Entries

   Jacobian of rectangular w.r.t. geodetic coordinates

-&
*/

{ /* Begin drdgeo_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "drdgeo_c" );


   drdgeo_ ( (doublereal *) &lon,
             (doublereal *) &lat,
             (doublereal *) &alt,
             (doublereal *) &re,
             (doublereal *) &f,
             (doublereal *) jacobi  );

   /*
   Transpose the Jacobian to create a C-style matrix.
   */
   xpose_c ( jacobi, jacobi );


   chkout_c ( "drdgeo_c" );

} /* End drdgeo_c */
