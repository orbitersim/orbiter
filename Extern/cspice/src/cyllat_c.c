/*

-Procedure cyllat_c ( Cylindrical to latitudinal )

-Abstract

   Convert from cylindrical to latitudinal coordinates.

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

   CONVERSION
   COORDINATES

*/

   #include <math.h>
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"

   void cyllat_c ( SpiceDouble    r,
                   SpiceDouble    clon,
                   SpiceDouble    z,
                   SpiceDouble *  radius,
                   SpiceDouble *  lon,
                   SpiceDouble *  lat )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   r          I   Distance of point from z axis.
   clon       I   Cylindrical angle of point from XZ plane (radians).
   z          I   Height of point above XY plane.
   radius     O   Distance of point from origin.
   lon        O   Longitude of point (radians).
   lat        O   Latitude of point (radians).

-Detailed_Input

   r           is the distance of the input point from Z axis.

   clon        is the cylindrical angle of the point from XZ plane
               (radians).

   z           is the height of the point above XY plane.

-Detailed_Output

   radius      is the distance of the input point from origin.

   lon         is the longitude (i.e. angle from the XZ plane) of the
               input point (radians). `lon' is set equal to `clon'.

   lat         is the latitude (i.e. angle above the XY plane) of the
               input point (radians). The range of `lat' is [-pi, pi].

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This routine converts coordinates given in cylindrical
   coordinates to coordinates in latitudinal coordinates.

   Latitudinal coordinates are defined by a distance from a central
   reference point, an angle from a reference meridian, and an angle
   above the equator of a sphere centered at the central reference
   point.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute the cylindrical coordinates of the position of the
      Moon as seen from the Earth, and convert them to latitudinal
      and rectangular coordinates.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: cyllat_ex1.tm

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
            naif0012.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'naif0012.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program cyllat_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          clon;
         SpiceDouble          et;
         SpiceDouble          lat;
         SpiceDouble          lon;
         SpiceDouble          lt;
         SpiceDouble          pos    [3];
         SpiceDouble          radius;
         SpiceDouble          rectan [3];
         SpiceDouble          r;
         SpiceDouble          z;

         /.
         Load SPK and LSK kernels, use a meta kernel for
         convenience.
         ./
         furnsh_c ( "cyllat_ex1.tm" );

         /.
         Look up the geometric state of the Moon as seen from
         the Earth at 2017 Mar 20, relative to the J2000
         reference frame.
         ./
         str2et_c ( "2017 Mar 20", &et );

         spkpos_c ( "Moon", et, "J2000", "NONE", "Earth", pos, &lt );

         /.
         Convert the position vector `pos' to cylindrical
         coordinates.
         ./
         reccyl_c ( pos, &r, &clon, &z );

         /.
         Convert the cylindrical coordinates to latitudinal.
         ./
         cyllat_c ( r, clon, z, &radius, &lon, &lat );

         /.
         Convert the latitudinal coordinates to rectangular.
         ./
         latrec_c ( radius, lon, lat, rectan );

         printf( " \n" );
         printf( "Original rectangular coordinates:\n" );
         printf( " \n" );
         printf( " X          (km):  %19.8f\n", pos[0] );
         printf( " Y          (km):  %19.8f\n", pos[1] );
         printf( " Z          (km):  %19.8f\n", pos[2] );
         printf( " \n" );
         printf( "Cylindrical coordinates:\n" );
         printf( " \n" );
         printf( " Radius     (km):  %19.8f\n", r );
         printf( " Longitude (deg):  %19.8f\n", clon*dpr_c ( ) );
         printf( " Z          (km):  %19.8f\n", z );
         printf( " \n" );
         printf( "Latitudinal coordinates:\n" );
         printf( " \n" );
         printf( " Radius     (km):  %19.8f\n", radius );
         printf( " Longitude (deg):  %19.8f\n", lon*dpr_c ( ) );
         printf( " Latitude  (deg):  %19.8f\n", lat*dpr_c ( ) );
         printf( " \n" );
         printf( "Rectangular coordinates from latrec_c:\n" );
         printf( " \n" );
         printf( " X          (km):  %19.8f\n", rectan[0] );
         printf( " Y          (km):  %19.8f\n", rectan[1] );
         printf( " Z          (km):  %19.8f\n", rectan[2] );
         printf( " \n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Original rectangular coordinates:

       X          (km):      -55658.44323296
       Y          (km):     -379226.32931475
       Z          (km):     -126505.93063865

      Cylindrical coordinates:

       Radius     (km):      383289.01777726
       Longitude (deg):         261.65040211
       Z          (km):     -126505.93063865

      Latitudinal coordinates:

       Radius     (km):      403626.33912495
       Longitude (deg):         261.65040211
       Latitude  (deg):         -18.26566077

      Rectangular coordinates from latrec_c:

       X          (km):      -55658.44323296
       Y          (km):     -379226.32931475
       Z          (km):     -126505.93063865


   2) Create a table showing a variety of cylindrical coordinates
      and the corresponding latitudinal coordinates.

      Corresponding latitudinal and cylindrical coordinates are
      listed to three decimal places. All input and output angles
      are in degrees.


      Example code begins here.


      /.
         Program cyllat_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NREC         11

         /.
         Local variables.
         ./
         SpiceDouble          lat;
         SpiceDouble          lon;
         SpiceDouble          radius;
         SpiceDouble          rclon;

         SpiceInt             i;

         /.
         Define the input cylindrical coordinates. Angles
         in degrees.
         ./

         SpiceDouble          r      [NREC] = { 0.0, 1.0, 1.0,
                                                0.0, 1.0, 1.0,
                                                0.0, 1.0, 1.0,
                                                0.0, 0.0      };

         SpiceDouble          clon   [NREC] = {   0.0,   0.0,  90.0,
                                                  0.0, 180.0, -90.0,
                                                  0.0,  45.0, 180.0,
                                                180.0,  33.0        };

         SpiceDouble          z      [NREC] = {  0.0,  0.0,  0.0,
                                                 1.0,  1.0,  0.0,
                                                -1.0,  0.0, -1.0,
                                                 1.0,  0.0          };

         /.
         Print the banner.
         ./
         printf( "    r       clon      z      radius    lon      lat \n" );
         printf( " -------  -------  -------  -------  -------  ------- \n" );

         /.
         Do the conversion. Output angles in degrees.
         ./
         for ( i = 0; i < NREC; i++ )
         {

            rclon = clon[i] * rpd_c ( );

            cyllat_c ( r[i], rclon, z[i], &radius, &lon, &lat );

            printf( "%8.3f %8.3f %8.3f ", r[i], clon[i], z[i] );
            printf( "%8.3f %8.3f %8.3f\n", radius, lon * dpr_c ( ),
                                                   lat * dpr_c ( ) );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


          r       clon      z      radius    lon      lat
       -------  -------  -------  -------  -------  -------
         0.000    0.000    0.000    0.000    0.000    0.000
         1.000    0.000    0.000    1.000    0.000    0.000
         1.000   90.000    0.000    1.000   90.000    0.000
         0.000    0.000    1.000    1.000    0.000   90.000
         1.000  180.000    1.000    1.414  180.000   45.000
         1.000  -90.000    0.000    1.000  -90.000    0.000
         0.000    0.000   -1.000    1.000    0.000  -90.000
         1.000   45.000    0.000    1.000   45.000    0.000
         1.000  180.000   -1.000    1.414  180.000  -45.000
         0.000  180.000    1.000    1.000  180.000   90.000
         0.000   33.000    0.000    0.000   33.000    0.000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.0, 02-JUL-2021 (JDR)

       Changed the input argument name "lonc" to "clon" for consistency
       with other routines.

       Edited the header to comply with NAIF standard.
       Added complete code examples.

   -CSPICE Version 1.1.1, 26-JUL-2016 (BVS)

       Minor headers edits.

   -CSPICE Version 1.1.0, 23-JUL-2001 (NJB)

       Removed tab characters from source file.

   -CSPICE Version 1.0.1, 08-FEB-1998 (EDW)

       Corrected and clarified header entries. Removed return call.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW) (WLT)

-Index_Entries

   cylindrical to latitudinal

-&
*/

{ /* Begin cyllat_c */

   /*
   Local variables
   */

   SpiceDouble    lattud;
   SpiceDouble    rho;
   SpiceDouble    x;
   SpiceDouble    y;
   SpiceDouble    big;


   /* Computing biggest absolute value */

   big = MaxAbs( r, z);

   if (big > 0.)
      {
      x = r / big;
      y = z / big;
      rho = big * sqrt(x * x + y * y);
      }
   else
      {
      rho = 0.;
      }

   if (rho == 0.)
      {
      lattud = 0.;
      }
   else
      {
      lattud = atan2( z, r );
      }


   /*  Move results to output variables */

   *lon    = clon;
   *radius = rho;
   *lat    = lattud;


} /* End cyllat_c */
