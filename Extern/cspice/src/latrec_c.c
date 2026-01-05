/*

-Procedure latrec_c ( Latitudinal to rectangular coordinates )

-Abstract

   Convert from latitudinal coordinates to rectangular coordinates.

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

   void latrec_c ( SpiceDouble    radius,
                   SpiceDouble    lon,
                   SpiceDouble    lat,
                   SpiceDouble    rectan[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   radius     I   Distance of a point from the origin.
   lon        I   Longitude of point in radians.
   lat        I   Latitude of point in radians.
   rectan     O   Rectangular coordinates of the point.

-Detailed_Input

   radius      is the distance of a point from the origin.

   lon         is the longitude of the input point. This is the angle
               between the prime meridian and the meridian containing
               `rectan'. The direction of increasing longitude is from
               the +X axis towards the +Y axis.

               `lon' is measured in radians. On input, the range
               of longitude is unrestricted.

   lat         is the latitude of the input point. This is the angle
               from the XY plane of the ray from the origin through the
               point.

               `lat' is measured in radians. On input, the range of
               latitude is unrestricted.

-Detailed_Output

   rectan      are the rectangular coordinates of the input point.
               `rectan' is a 3 vector.

               The units associated with `rectan' are those
               associated with the input `radius'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This routine returns the rectangular coordinates of a point
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

   1) Compute the latitudinal coordinates of the position of the
      Moon as seen from the Earth, and convert them to rectangular
      coordinates.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: latrec_ex1.tm

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
         Program latrec_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          et;
         SpiceDouble          lat;
         SpiceDouble          lon;
         SpiceDouble          lt;
         SpiceDouble          pos    [3];
         SpiceDouble          radius;
         SpiceDouble          rectan [3];

         /.
         Load SPK and LSK kernels, use a meta kernel for
         convenience.
         ./
         furnsh_c ( "latrec_ex1.tm" );

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
         Convert the latitudinal to rectangular coordinates.
         ./

         latrec_c ( radius, lon, lat, rectan );

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

      Latitudinal coordinates:

       Radius     (km):      403626.33912495
       Longitude (deg):         -98.34959789
       Latitude  (deg):         -18.26566077

      Rectangular coordinates from latrec_c:

       X          (km):      -55658.44323296
       Y          (km):     -379226.32931475
       Z          (km):     -126505.93063865


   2) Create a table showing a variety of latitudinal coordinates
      and the corresponding rectangular coordinates.

      Corresponding latitudinal and rectangular coordinates are
      listed to three decimal places. Input angles are in degrees.


      Example code begins here.


      /.
         Program latrec_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants.
         ./
         #define NPTS            11

         /.
         Local variables.
         ./
         SpiceDouble             rectan[3];
         SpiceDouble             rlon;
         SpiceDouble             rlat;

         SpiceInt                i;


         /.
         Define eleven sets of latitude coordinates, `lon'
         and `lat' expressed in degrees.
         ./
         SpiceDouble             radius[NPTS] = { 0.0,    1.0,    1.0,
                                                  1.0,    1.0,    1.0,
                                                  1.0,    1.4142, 1.4142,
                                                  1.4142, 1.732          };

         SpiceDouble             lon   [NPTS] = {  0.0,  0.0,   90.0,
                                                   0.0, 180.0, -90.0,
                                                   0.0,  45.0,   0.0,
                                                  90.0,  45.0        };

         SpiceDouble             lat   [NPTS] = {   0.0,  0.0,     0.0,
                                                   90.0,  0.0,     0.0,
                                                  -90.0,  0.0,    45.0,
                                                   45.0,  35.2643      };

         /.
         Print a header for the data output.
         ./
         printf( "  radius    lon      lat    rect[0]  rect[1]  rect[2]\n" );
         printf( " -------  -------  -------  -------  -------  -------\n" );

         for ( i = 0; i < NPTS; i++ )
         {
            /.
            Convert `lon' and `lat' from degrees to radians.
            ./
            rlon = lon[i] * rpd_c();
            rlat = lat[i] * rpd_c();

            /.
            Convert the coordinates from latitudinal to rectangular.
            ./
            latrec_c ( radius[i], rlon, rlat, rectan );


            /.
            Output the row of the coordinate table.
            ./
            printf ( "%8.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n",
                                    radius[i], lon[i],    lat[i],
                                    rectan[0], rectan[1], rectan[2] );
         }

         return( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


        radius    lon      lat    rect[0]  rect[1]  rect[2]
       -------  -------  -------  -------  -------  -------
         0.000    0.000    0.000    0.000    0.000    0.000
         1.000    0.000    0.000    1.000    0.000    0.000
         1.000   90.000    0.000    0.000    1.000    0.000
         1.000    0.000   90.000    0.000    0.000    1.000
         1.000  180.000    0.000   -1.000    0.000    0.000
         1.000  -90.000    0.000    0.000   -1.000    0.000
         1.000    0.000  -90.000    0.000    0.000   -1.000
         1.414   45.000    0.000    1.000    1.000    0.000
         1.414    0.000   45.000    1.000    0.000    1.000
         1.414   90.000   45.000    0.000    1.000    1.000
         1.732   45.000   35.264    1.000    1.000    1.000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 04-JUL-2021 (JDR)

       Changed input argument names "longitude" and "latitude to "lon"
       and "lat" for consistency with other routines.

       Edited the header to comply with NAIF standard. Added
       complete code examples based on existing example.

   -CSPICE Version 1.0.1, 29-JUL-2003 (NJB) (CHA)

       Various header corrections were made.

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW)

-Index_Entries

   latitudinal to rectangular coordinates

-&
*/

{ /* Begin latrec_c */

   /* Function Body */

   rectan[0] = radius * cos( lon ) * cos( lat );
   rectan[1] = radius * sin( lon ) * cos( lat );
   rectan[2] = radius * sin( lat );

} /* End latrec_c */
