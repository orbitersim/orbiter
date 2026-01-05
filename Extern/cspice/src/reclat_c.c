/*

-Procedure reclat_c ( Rectangular to latitudinal coordinates )

-Abstract

   Convert from rectangular coordinates to latitudinal coordinates.

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
   #include "SpiceZim.h"
   #undef    reclat_c


   void reclat_c ( ConstSpiceDouble    rectan[3],
                   SpiceDouble       * radius,
                   SpiceDouble       * lon,
                   SpiceDouble       * lat      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   rectan     I   Rectangular coordinates of a point.
   radius     O   Distance of the point from the origin.
   lon        O   Longitude of the point in radians.
   lat        O   Latitude of the point in radians.

-Detailed_Input

   rectan      are the rectangular coordinates of the input point.
               `rectan' is a 3-vector.

-Detailed_Output

   radius      is the distance of the point from the origin.

               The units associated with `radius' are those
               associated with the input `rectan'.

   lon         is the longitude of the input point. This is angle
               between the prime meridian and the meridian
               containing `rectan'. The direction of increasing
               longitude is from the +X axis towards the +Y axis.

               `lon' is output in radians. The range of `lon'
               is [-pi, pi].


   lat         is the latitude of the input point. This is the angle
               from the XY plane of the ray from the origin through the
               point.

               `lat' is output in radians. The range of `lat'
               is [-pi/2, pi/2].

-Parameters

   None.

-Exceptions

   Error free.

   1)  If the X and Y components of `rectan' are both zero, the
       longitude is set to zero.

   2)  If `rectan' is the zero vector, longitude and latitude are
       both set to zero.

-Files

   None.

-Particulars

   This routine returns the latitudinal coordinates of a point
   whose position is input in rectangular coordinates.

   Latitudinal coordinates are defined by a distance from a central
   reference point, an angle from a reference meridian, and an angle
   above the equator of a sphere centered at the central reference
   point.

-Examples

   The numerical results shown for thes examples may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Compute the latitudinal coordinates of the position of the
      Moon as seen from the Earth, and convert them to rectangular
      coordinates.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: reclat_ex1.tm

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
         Program reclat_ex1
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
         furnsh_c ( "reclat_ex1.tm" );

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


   2) Create a table showing a variety of rectangular coordinates
      and the corresponding latitudinal coordinates.

      Corresponding rectangular and latitudinal coordinates are
      listed to three decimal places. Output angles are in degrees.


      Example code begins here.


      /.
         Program reclat_ex2
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

         SpiceInt             i;

         /.
         Define the input rectangular coordinates.
         ./
         SpiceDouble          rectan [NREC][3] = {
                                   { 0.0,         0.0,         0.0},
                                   { 1.0,         0.0,         0.0},
                                   { 0.0,         1.0,         0.0},
                                   { 0.0,         0.0,         1.0},
                                   {-1.0,         0.0,         0.0},
                                   { 0.0,        -1.0,         0.0},
                                   { 0.0,         0.0,        -1.0},
                                   { 1.0,         1.0,         0.0},
                                   { 1.0,         0.0,         1.0},
                                   { 0.0,         1.0,         1.0},
                                   { 1.0,         1.0,         1.0} };

         /.
         Print the banner.
         ./
         printf( " rect[0]  rect[1]  rect[2]   radius    lon      lat\n"   );
         printf( " -------  -------  -------  -------  -------  -------\n" );

         /.
         Do the conversion. Output angles are in degrees.
         ./
         for ( i = 0; i < NREC; i++ )
         {

            reclat_c ( rectan[i], &radius, &lon, &lat );

            printf( "%8.3f %8.3f %8.3f ", rectan[i][0], rectan[i][1],
                                          rectan[i][2]               );
            printf( "%8.3f %8.3f %8.3f\n", radius, lon * dpr_c ( ),
                                                   lat * dpr_c ( ) );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       rect[0]  rect[1]  rect[2]   radius    lon      lat
       -------  -------  -------  -------  -------  -------
         0.000    0.000    0.000    0.000    0.000    0.000
         1.000    0.000    0.000    1.000    0.000    0.000
         0.000    1.000    0.000    1.000   90.000    0.000
         0.000    0.000    1.000    1.000    0.000   90.000
        -1.000    0.000    0.000    1.000  180.000    0.000
         0.000   -1.000    0.000    1.000  -90.000    0.000
         0.000    0.000   -1.000    1.000    0.000  -90.000
         1.000    1.000    0.000    1.414   45.000    0.000
         1.000    0.000    1.000    1.414    0.000   45.000
         0.000    1.000    1.000    1.414   90.000   45.000
         1.000    1.000    1.000    1.732   45.000   35.264


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.3.0, 10-AUG-2021 (JDR)

       Changed input argument names "longitude" and "latitude to "lon"
       and "lat" for consistency with other routines.

       Edited the header to comply with NAIF standard. Added
       complete code example based on existing example.

       Added -Particulars section.

   -CSPICE Version 1.2.1, 30-JUL-2003 (NJB)

       Various header changes were made to improve clarity. Some
       minor header corrections were made.

   -CSPICE Version 1.2.0, 28-AUG-2001 (NJB)

       Removed tab characters from source file. Now includes
       interface macro header SpiceZim.h.

   -CSPICE Version 1.1.0, 21-OCT-1998 (NJB)

       Made input vector const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries

   rectangular to latitudinal coordinates

-&
*/

{ /* Begin reclat_c */

   /*
   Local variables and definitions.
   */

   SpiceDouble   vmax;
   SpiceDouble   x1;
   SpiceDouble   y1;
   SpiceDouble   z1;


   /* Function Body */

   vmax = MaxAbs(  rectan[0], MaxAbs( rectan[1], rectan[2] )   );

   if ( vmax > 0.)
      {
      x1      = rectan[0] / vmax;
      y1      = rectan[1] / vmax;
      z1      = rectan[2] / vmax;
      *radius = vmax * sqrt( x1*x1 + y1*y1 + z1*z1 );
      *lat    = atan2(z1, sqrt( x1*x1 + y1*y1 ) );


      if ( x1 == 0. && y1 == 0.)
         {
         *lon = 0.;
         }

      else
        {
        *lon = atan2(y1, x1);
        }

      }

   else
      {

      /*
      The vector is the zero vector.
      */

      *radius = 0.;
      *lon    = 0.;
      *lat    = 0.;
      }


} /* End reclat_c */
