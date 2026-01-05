/*

-Procedure recsph_c ( Rectangular to spherical coordinates )

-Abstract

   Convert from rectangular coordinates to spherical coordinates.

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
   #undef    recsph_c


   void recsph_c ( ConstSpiceDouble     rectan[3],
                   SpiceDouble        * r,
                   SpiceDouble        * colat,
                   SpiceDouble        * slon      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   rectan     I   Rectangular coordinates of a point.
   r          O   Distance of the point from the origin.
   colat      O   Angle of the point from the Z-axis in radians
   slon       O   Longitude of the point in radians.

-Detailed_Input

   rectan      are the rectangular coordinates of a point.

-Detailed_Output

   r           is the distance of the point from the origin.

   colat       is the angle between the point and the positive z-axis in
               radians. The range of `colat' is [0, pi].

   slon        is the longitude of the point in radians. This is the
               angle between the positive X-axis and the orthogonal
               projection of the point onto the XY plane. `slon'
               increases in the counterclockwise sense about the
               positive Z-axis. The range of `slon' is [-pi, pi].

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This routine returns the spherical coordinates of a point
   whose position is input in rectangular coordinates.

   spherical coordinates are defined by a distance from a central
   reference point, an angle from a reference meridian, and an angle
   from the z-axis.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute the spherical coordinates of the position of the Moon
      as seen from the Earth, and convert them to rectangular
      coordinates.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: recsph_ex1.tm

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
         Program recsph_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          colat;
         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          pos    [3];
         SpiceDouble          radius;
         SpiceDouble          rectan [3];
         SpiceDouble          slon;

         /.
         Load SPK and LSK kernels, use a meta kernel for
         convenience.
         ./
         furnsh_c ( "recsph_ex1.tm" );

         /.
         Look up the geometric state of the Moon as seen from
         the Earth at 2017 Mar 20, relative to the J2000
         reference frame.
         ./
         str2et_c ( "2017 Mar 20", &et );

         spkpos_c ( "Moon", et, "J2000", "NONE", "Earth", pos, &lt );

         /.
         Convert the position vector `pos' to spherical
         coordinates.
         ./
         recsph_c ( pos, &radius, &colat, &slon );

         /.
         Convert the spherical coordinates to rectangular.
         ./
         sphrec_c ( radius, colat, slon, rectan );

         printf( " \n" );
         printf( "Original rectangular coordinates:\n" );
         printf( " \n" );
         printf( " X           (km):  %19.8f\n", pos[0] );
         printf( " Y           (km):  %19.8f\n", pos[1] );
         printf( " Z           (km):  %19.8f\n", pos[2] );
         printf( " \n" );
         printf( "Spherical coordinates:\n" );
         printf( " \n" );
         printf( " Radius      (km):  %19.8f\n", radius );
         printf( " Colatitude (deg):  %19.8f\n", colat*dpr_c ( ) );
         printf( " Longitude  (deg):  %19.8f\n", slon*dpr_c ( ) );
         printf( " \n" );
         printf( "Rectangular coordinates from sphrec_c:\n" );
         printf( " \n" );
         printf( " X           (km):  %19.8f\n", rectan[0] );
         printf( " Y           (km):  %19.8f\n", rectan[1] );
         printf( " Z           (km):  %19.8f\n", rectan[2] );
         printf( " \n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Original rectangular coordinates:

       X           (km):      -55658.44323296
       Y           (km):     -379226.32931475
       Z           (km):     -126505.93063865

      Spherical coordinates:

       Radius      (km):      403626.33912495
       Colatitude (deg):         108.26566077
       Longitude  (deg):         -98.34959789

      Rectangular coordinates from sphrec_c:

       X           (km):      -55658.44323296
       Y           (km):     -379226.32931475
       Z           (km):     -126505.93063865


   2) Create a table showing a variety of rectangular coordinates
      and the corresponding spherical coordinates.

      Corresponding rectangular and spherical coordinates are
      listed to three decimal places. Output angles are in degrees.


      Example code begins here.


      /.
         Program recsph_ex2
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
         SpiceDouble          colat;
         SpiceDouble          radius;
         SpiceDouble          slon;

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
         printf( " rect[0]  rect[1]  rect[2]   radius   colat     slon\n" );
         printf( " -------  -------  -------  -------  -------  -------\n" );

         /.
         Do the conversion. Output angles in degrees.
         ./
         for ( i = 0; i < NREC; i++ )
         {

            recsph_c ( rectan[i], &radius, &colat, &slon );

            printf( "%8.3f %8.3f %8.3f ", rectan[i][0], rectan[i][1],
                                          rectan[i][2]               );
            printf( "%8.3f %8.3f %8.3f\n", radius, colat * dpr_c(),
                                                    slon * dpr_c() );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       rect[0]  rect[1]  rect[2]   radius   colat     slon
       -------  -------  -------  -------  -------  -------
         0.000    0.000    0.000    0.000    0.000    0.000
         1.000    0.000    0.000    1.000   90.000    0.000
         0.000    1.000    0.000    1.000   90.000   90.000
         0.000    0.000    1.000    1.000    0.000    0.000
        -1.000    0.000    0.000    1.000   90.000  180.000
         0.000   -1.000    0.000    1.000   90.000  -90.000
         0.000    0.000   -1.000    1.000  180.000    0.000
         1.000    1.000    0.000    1.414   90.000   45.000
         1.000    0.000    1.000    1.414   45.000    0.000
         0.000    1.000    1.000    1.414   45.000   90.000
         1.000    1.000    1.000    1.732   54.736   45.000


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

   -CSPICE Version 1.2.0, 10-AUG-2021 (JDR)

       Changed the output argument name "lon" to "slon" for consistency
       with other routines.

       Edited the header to comply with NAIF standard.
       Added complete code examples.

   -CSPICE Version 1.1.2, 26-JUL-2016 (BVS)

       Minor headers edits.

   -CSPICE Version 1.1.1, 07-JAN-2002 (NJB) (EDW)

       Fixed description of slon in -Brief_I/O and Detailed_I/O
       header sections.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input coordinate array const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (WLT)

-Index_Entries

   rectangular to spherical coordinates

-&
*/

{ /* Begin recsph_c */

   /*
   Local constants
   */

   SpiceDouble    x;
   SpiceDouble    y;
   SpiceDouble    z;
   SpiceDouble    big;


   /* Computing maximum magnitude of the elements of rectan */

   big = MaxAbs( rectan[0], MaxAbs( rectan[1], rectan[2] ) );

   if (big > 0.)
      {

      x = rectan[0] / big;
      y = rectan[1] / big;
      z = rectan[2] / big;

      *r     = big * sqrt(x * x + y * y + z * z );
      *colat = atan2( sqrt(x * x + y * y), z );

      x = rectan[0];
      y = rectan[1];

      if (x == 0. && y == 0.)
         {
         *slon = 0.;
         }
      else
         {
         *slon = atan2(y, x);
         }
      }

   else
      {
      *r     = 0.;
      *colat = 0.;
      *slon   = 0.;
      }


} /* End recsph_c */
