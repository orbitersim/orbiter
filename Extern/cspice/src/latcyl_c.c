/*

-Procedure latcyl_c ( Latitudinal to cylindrical coordinates )

-Abstract

   Convert from latitudinal coordinates to cylindrical coordinates.

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


   void latcyl_c ( SpiceDouble    radius,
                   SpiceDouble    lon,
                   SpiceDouble    lat,
                   SpiceDouble *  r,
                   SpiceDouble *  clon,
                   SpiceDouble *  z )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   radius     I   Distance of a point from the origin.
   lon        I   Angle of the point from the XZ plane in radians.
   lat        I   Angle of the point from the XY plane in radians.
   r          O   Distance of the point from the z axis.
   clon       O   Angle of the point from the XZ plane in radians.
   z          O   Height of the point above the XY plane.

-Detailed_Input

   radius      is the distance of a point from the origin.

   lon         is the angle of the point from the XZ plane in radians.

   lat         is the angle of the point from the XY plane in radians.

-Detailed_Output

   r           is the distance of the point from the z axis.

   clon        is the angle of the point from the XZ plane in radians.
               `clon' is set equal to `lon'.

   z           is the height of the point above the XY plane.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This routine returns the cylindrical coordinates of a point
   whose position is input in latitudinal coordinates.

   Latitudinal coordinates are defined by a distance from a central
   reference point, an angle from a reference meridian, and an angle
   above the equator of a sphere centered at the central reference
   point.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute the latitudinal coordinates of the position of the Moon
      as seen from the Earth, and convert them to cylindrical and
      rectangular coordinates.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: latcyl_ex1.tm

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
         Program latcyl_ex1
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
         furnsh_c ( "latcyl_ex1.tm" );

         /.
         Look up the geometric state of the Moon as seen from
         the Earth at 2017 Mar 20, relative to the J2000
         reference frame.
         ./
         str2et_c ( "2017 Mar 20", &et );

         spkpos_c ( "Moon", et, "J2000", "NONE", "Earth", pos, &lt );

         /.
         Convert the position vector `pos' to latitudinal
         coordinates.
         ./
         reclat_c ( pos, &radius, &lon, &lat );

         /.
         Convert the latitudinal coordinates to cylindrical.
         ./
         latcyl_c ( radius, lon, lat, &r, &clon, &z );

         /.
         Convert the cylindrical coordinates to rectangular.
         ./
         cylrec_c ( r, clon, z, rectan );

         printf( " \n" );
         printf( "Original rectangular coordinates:\n" );
         printf( " \n" );
         printf( " X          (km):  %19.8f\n", pos[0] );
         printf( " Y          (km):  %19.8f\n", pos[1] );
         printf( " Z          (km):  %19.8f\n", pos[2] );
         printf( " \n" );
         printf( "Latitudinal coordinates:\n" );
         printf( " \n" );
         printf( " Radius     (km):  %19.8f\n", radius );
         printf( " Longitude (deg):  %19.8f\n", lon*dpr_c ( ) );
         printf( " Latitude  (deg):  %19.8f\n", lat*dpr_c ( ) );
         printf( " \n" );
         printf( "Cylindrical coordinates:\n" );
         printf( " \n" );
         printf( " Radius     (km):  %19.8f\n", r );
         printf( " Longitude (deg):  %19.8f\n", clon*dpr_c ( ) );
         printf( " Z          (km):  %19.8f\n", z );
         printf( " \n" );
         printf( "Rectangular coordinates from cylrec_c:\n" );
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

      Latitudinal coordinates:

       Radius     (km):      403626.33912495
       Longitude (deg):         -98.34959789
       Latitude  (deg):         -18.26566077

      Cylindrical coordinates:

       Radius     (km):      383289.01777726
       Longitude (deg):         -98.34959789
       Z          (km):     -126505.93063865

      Rectangular coordinates from cylrec_c:

       X          (km):      -55658.44323296
       Y          (km):     -379226.32931475
       Z          (km):     -126505.93063865


   2) Create a table showing a variety of latitudinal coordinates
      and the corresponding cylindrical coordinates.

      Corresponding latitudinal and cylindrical coordinates are
      listed to three decimal places. Input and output angles are
      in degrees.


      Example code begins here.


      /.
         Program latcyl_ex2
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
         SpiceDouble          clon;
         SpiceDouble          r;
         SpiceDouble          rlat;
         SpiceDouble          rlon;
         SpiceDouble          z;

         SpiceInt             i;

         /.
         Define the input latitudinal coordinates. Angles in degrees.
         ./

         SpiceDouble          radius [NREC] = { 0.0,  1.0,     1.0,
                                                1.0,  1.4142,  1.0,
                                                1.0,  1.0,     1.4142,
                                                1.0,  0.0             };

         SpiceDouble          lon    [NREC] = {   0.0,    0.0,   90.0,
                                                  0.0,  180.0,  -90.0,
                                                  0.0,   45.0,  180.0,
                                                180.0,    33.0        };

         SpiceDouble          lat    [NREC] = {  90.0,   0.0,    0.0,
                                                 90.0,  45.0,    0.0,
                                                -90.0,   0.0,  -45.0,
                                                 90.0,   0.0         };

         /.
         Print the banner.
         ./
         printf( "  radius    lon      lat       r       clon      z   \n" );
         printf( " -------  -------  -------  -------  -------  -------\n" );

         /.
         Do the conversion. Output angles in degrees.
         ./
         for ( i = 0; i < NREC; i++ )
         {

            rlon = lon[i] * rpd_c ( );
            rlat = lat[i] * rpd_c ( );

            latcyl_c ( radius[i], rlon, rlat, &r, &clon, &z );

            printf( "%8.3f %8.3f %8.3f ", radius[i], lon[i], lat[i] );
            printf( "%8.3f %8.3f %8.3f\n", r, clon * dpr_c ( ), z );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


        radius    lon      lat       r       clon      z
       -------  -------  -------  -------  -------  -------
         0.000    0.000   90.000    0.000    0.000    0.000
         1.000    0.000    0.000    1.000    0.000    0.000
         1.000   90.000    0.000    1.000   90.000    0.000
         1.000    0.000   90.000    0.000    0.000    1.000
         1.414  180.000   45.000    1.000  180.000    1.000
         1.000  -90.000    0.000    1.000  -90.000    0.000
         1.000    0.000  -90.000    0.000    0.000   -1.000
         1.000   45.000    0.000    1.000   45.000    0.000
         1.414  180.000  -45.000    1.000  180.000   -1.000
         1.000  180.000   90.000    0.000  180.000    1.000
         0.000   33.000    0.000    0.000   33.000    0.000


   3) Other than the obvious conversion between coordinate systems
      this routine could be used to obtain the axial projection
      from a sphere to a cylinder about the z-axis that contains
      the equator of the sphere.

      Such a projection is valuable because it preserves the
      areas between regions on the sphere and their projections to
      the cylinder.


      Example code begins here.


      /.
         Program latcyl_ex3
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          clon;
         SpiceDouble          lat;
         SpiceDouble          lon;
         SpiceDouble          radius;
         SpiceDouble          r;
         SpiceDouble          z;

         /.
         Define the point whose projection is to be
         computed.
         ./
         radius =  100.0;
         lon    =   45.0  * rpd_c ( );
         lat    =  -12.5 * rpd_c ( );

         /.
         Convert the latitudinal coordinates to cylindrical.
         ./
         latcyl_c ( radius, lon, lat, &r, &clon, &z );

         printf( "Coordinates of the projected point on cylinder:\n" );
         printf( " \n" );
         printf( " Radius     (km):  %22.11f\n", r );
         printf( " Longitude (deg):  %22.11f\n", clon*dpr_c ( ) );
         printf( " Z          (km):  %22.11f\n", z );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Coordinates of the projected point on cylinder:

       Radius     (km):          97.62960071199
       Longitude (deg):          45.00000000000
       Z          (km):         -21.64396139381


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 04-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Added complete code examples.

       Changed the input argument name "lonc" to "clon" for consistency
       with other routines.

   -CSPICE Version 1.0.1, 26-JUL-2016 (BVS)

       Minor headers edits.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (WLT)

-Index_Entries

   latitudinal to cylindrical coordinates

-&
*/

{ /* Begin latcyl_c */

   /*
   Local variables
   */

   SpiceDouble    rh;
   SpiceDouble    zz;


   /* Convert to cylindrical, storing in temporary variables */

   rh = radius * cos( lat );
   zz = radius * sin( lat );


   /* Move the results to output variables. */

   *clon = lon;
   *r    = rh;
   *z    = zz;


} /* End latcyl_c */
