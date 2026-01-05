/*

-Procedure sphcyl_c ( Spherical to cylindrical coordinates )

-Abstract

   Convert from spherical coordinates to cylindrical coordinates.

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


   void sphcyl_c ( SpiceDouble     radius,
                   SpiceDouble     colat,
                   SpiceDouble     slon,
                   SpiceDouble   * r,
                   SpiceDouble   * clon,
                   SpiceDouble   * z )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  -------------------------------------------------
   radius     I   Distance of point from origin.
   colat      I   Polar angle (co-latitude in radians) of point.
   slon       I   Azimuthal angle (longitude) of point (radians).
   r          O   Distance of point from Z axis.
   clon       O   Angle (radians) of point from XZ plane.
   z          O   Height of point above XY plane.

-Detailed_Input

   radius      is the distance of the point from origin.

   colat       is the polar angle (co-latitude in radians) of the point.

   slon        is the azimuthal angle (longitude) of the point (radians).

-Detailed_Output

   r           is the distance of the point of interest from Z axis.

   clon        is the cylindrical angle (radians) of the point from the
               XZ plane. `clon' is set equal to `slon'.

   z           is the height of the point above XY plane.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This returns the cylindrical coordinates of a point whose
   position is input through spherical coordinates.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute the spherical coordinates of the position of the Moon
      as seen from the Earth, and convert them to cylindrical and
      rectangular coordinates.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: sphcyl_ex1.tm

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
         Program sphcyl_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          clon;
         SpiceDouble          colat;
         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          pos    [3];
         SpiceDouble          r;
         SpiceDouble          radius;
         SpiceDouble          rectan [3];
         SpiceDouble          slon;
         SpiceDouble          z;

         /.
         Load SPK and LSK kernels, use a meta kernel for
         convenience.
         ./
         furnsh_c ( "sphcyl_ex1.tm" );

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
         Convert the spherical coordinates to cylindrical.
         ./
         sphcyl_c ( radius, colat, slon, &r, &clon, &z );

         /.
         Convert the cylindrical coordinates to rectangular.
         ./
         cylrec_c ( r, clon, z, rectan );

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
         printf( "Cylindrical coordinates:\n" );
         printf( " \n" );
         printf( " Radius      (km):  %19.8f\n", r );
         printf( " Longitude  (deg):  %19.8f\n", clon*dpr_c ( ) );
         printf( " Z           (km):  %19.8f\n", z );
         printf( " \n" );
         printf( "Rectangular coordinates from cylrec_c:\n" );
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

      Cylindrical coordinates:

       Radius      (km):      383289.01777726
       Longitude  (deg):         -98.34959789
       Z           (km):     -126505.93063865

      Rectangular coordinates from cylrec_c:

       X           (km):      -55658.44323296
       Y           (km):     -379226.32931475
       Z           (km):     -126505.93063865


   2) Create a table showing a variety of spherical coordinates
      and the corresponding cylindrical coordinates.

      Corresponding spherical and cylindrical coordinates are
      listed to three decimal places. Input and output angles are
      in degrees.


      Example code begins here.


      /.
         Program sphcyl_ex2
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
         SpiceDouble          rcolat;
         SpiceDouble          rslon;
         SpiceDouble          z;

         SpiceInt             i;

         /.
         Define the input spherical coordinates. Angles in degrees.
         ./
         SpiceDouble          radius [NREC] = {  0.0,  1.0,     1.0,
                                                 1.0,  1.4142,  1.0,
                                                 1.0,  1.0,     1.4142,
                                                 1.0,  0.0             };

         SpiceDouble          colat  [NREC] = {  0.0,  90.0,  90.0,
                                                 0.0,  45.0,  90.0,
                                               180.0,  90.0, 135.0,
                                                 0.0,  90.0        };

         SpiceDouble          slon   [NREC] = {  0.0,   0.0,  90.0,
                                                 0.0, 180.0, -90.0,
                                                 0.0,  45.0, 180.0,
                                               180.0,  33.0        };

         /.
         Print the banner.
         ./
         printf( "  radius   colat     slon      r       clon      z\n"    );
         printf( " -------  -------  -------  -------  -------  -------\n" );

         /.
         Do the conversion. Output angles in degrees.
         ./
         for ( i = 0; i < NREC; i++ )
         {

            rcolat = colat[i] * rpd_c ( );
            rslon  = slon[i]  * rpd_c ( );

            sphcyl_c ( radius[i], rcolat, rslon, &r, &clon, &z );

            printf( "%8.3f %8.3f %8.3f ", radius[i], colat[i], slon[i] );
            printf( "%8.3f %8.3f %8.3f\n", r, clon * dpr_c ( ), z );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


        radius   colat     slon      r       clon      z
       -------  -------  -------  -------  -------  -------
         0.000    0.000    0.000    0.000    0.000    0.000
         1.000   90.000    0.000    1.000    0.000    0.000
         1.000   90.000   90.000    1.000   90.000    0.000
         1.000    0.000    0.000    0.000    0.000    1.000
         1.414   45.000  180.000    1.000  180.000    1.000
         1.000   90.000  -90.000    1.000  -90.000    0.000
         1.000  180.000    0.000    0.000    0.000   -1.000
         1.000   90.000   45.000    1.000   45.000    0.000
         1.414  135.000  180.000    1.000  180.000   -1.000
         1.000    0.000  180.000    0.000  180.000    1.000
         0.000   90.000   33.000    0.000   33.000    0.000


   3) Other than the obvious conversion between coordinate systems
      this routine could be used to obtain the axial projection
      from a sphere to a cylinder about the z-axis that contains
      the equator of the sphere.

      Such a projection is valuable because it preserves the
      areas between regions on the sphere and their projections to
      the cylinder.


      Example code begins here.


      /.
         Program sphcyl_ex3
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          clon;
         SpiceDouble          colat;
         SpiceDouble          radius;
         SpiceDouble          r;
         SpiceDouble          slon;
         SpiceDouble          z;

         /.
         Define the point whose projection is to be
         computed.
         ./
         radius =   100.0;
         slon   =    45.0 * rpd_c();
         colat  =   102.5 * rpd_c();

         /.
         Convert the spherical coordinates to cylindrical.
         ./
         sphcyl_c ( radius, colat, slon, &r, &clon, &z );

         printf( "Coordinates of the projected point on cylinder:\n" );
         printf( " \n" );
         printf( " Radius     (km):  %22.11f\n", r );
         printf( " Longitude (deg):  %22.11f\n", clon*dpr_c() );
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
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 05-JUL-2021 (JDR)

       Changed the output argument name "lon" to "clon" for
       consistency with other routines.

       Edited the header to comply with NAIF standard.
       Added complete code examples.

   -CSPICE Version 1.0.1, 26-JUL-2016 (BVS)

       Minor headers edits.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries

   spherical to cylindrical coordinates

-&
*/

{ /* Begin sphcyl_c */

   /*
   Local variables
   */

   SpiceDouble    rr;
   SpiceDouble    zz;

   /*
   Convert to cylindrical coordinates, storing the results in
   temporary variables.
   */

   rr = radius * sin( colat );
   zz = radius * cos( colat );


   /* Move the results to the output variables. */

   *clon = slon;
   *r    = rr;
   *z    = zz;


} /* End sphcyl_c */
