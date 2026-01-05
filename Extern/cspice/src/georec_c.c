/*

-Procedure georec_c ( Geodetic to rectangular coordinates )

-Abstract

   Convert geodetic coordinates to rectangular coordinates.

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

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void georec_c ( SpiceDouble lon,
                   SpiceDouble lat,
                   SpiceDouble alt,
                   SpiceDouble re,
                   SpiceDouble f,
                   SpiceDouble rectan[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   lon        I   Geodetic longitude of point (radians).
   lat        I   Geodetic latitude  of point (radians).
   alt        I   Altitude of point above the reference spheroid.
   re         I   Equatorial radius of the reference spheroid.
   f          I   Flattening coefficient.
   rectan     O   Rectangular coordinates of point.

-Detailed_Input

   lon         is the geodetic longitude of the input point. This is
               the angle between the prime meridian and the meridian
               containing `rectan'. The direction of increasing
               longitude is from the +X axis towards the +Y axis.

               Longitude is measured in radians. On input, the
               range of longitude is unrestricted.

   lat         is the geodetic latitude of the input point. For a
               point P on the reference spheroid, this is the angle
               between the XY plane and the outward normal vector at
               P. For a point P not on the reference spheroid, the
               geodetic latitude is that of the closest point to P on
               the spheroid.

               Latitude is measured in radians. On input, the
               range of latitude is unrestricted.

   alt         is the altitude of point above the reference spheroid.
               `alt' must be in the same units as `re'.

   re          is the equatorial radius of a reference spheroid. This
               spheroid is a volume of revolution: its horizontal
               cross sections are circular. The shape of the
               spheroid is defined by an equatorial radius `re' and
               a polar radius `rp'. `re' must be in the same units
               as `alt'.

   f           is the flattening coefficient = (re-rp) / re,  where
               `rp' is the polar radius of the spheroid.

-Detailed_Output

   rectan      are the rectangular coordinates of the input point.

               The units associated with `rectan' are those associated
               with the inputs `alt' and `re'.

-Parameters

   None.

-Exceptions

   1)  If the flattening coefficient is greater than or equal to one,
       the error SPICE(VALUEOUTOFRANGE) is signaled by a routine in
       the call tree of this routine.

   2)  If the equatorial radius is less than or equal to zero, the
       error SPICE(VALUEOUTOFRANGE) is signaled by a routine in the
       call tree of this routine.

-Files

   None.

-Particulars

   Given the geodetic coordinates of a point, and the constants
   describing the reference spheroid,  this routine returns the
   bodyfixed rectangular coordinates of the point. The bodyfixed
   rectangular frame is that having the X-axis pass through the
   0 degree latitude 0 degree longitude point. The Y-axis passes
   through the 0 degree latitude 90 degree longitude. The Z-axis
   passes through the 90 degree latitude point. For some bodies
   this coordinate system may not be a right-handed coordinate
   system.

-Examples

   This routine can be used to convert body fixed geodetic
   coordinates (such as the used for United States Geological
   Survey topographic maps) to bodyfixed rectangular coordinates
   such as the Satellite Tracking and Data Network of 1973.

   1) Find the rectangular coordinates of the point having Earth
      geodetic coordinates:

         lon (deg) =  118.0
         lat (deg) =   32.0
         alt (km)  =    0.0

      Use the PCK kernel below to load the required triaxial
      ellipsoidal shape model and orientation data for the Earth.

         pck00010.tpc


      Example code begins here.


      /.
         Program georec_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          alt;
         SpiceDouble          f;
         SpiceDouble          lat;
         SpiceDouble          lon;
         SpiceDouble          radii  [3];
         SpiceDouble          re;
         SpiceDouble          rectan [3];
         SpiceDouble          rp;

         SpiceInt             n;

         /.
         Load a PCK file containing a triaxial
         ellipsoidal shape model and orientation
         data for the Earth.
         ./
         furnsh_c ( "pck00010.tpc" );

         /.
         Retrieve the triaxial radii of the Earth
         ./
         bodvrd_c ( "EARTH", "RADII", 3, &n, radii );

         /.
         Compute flattening coefficient.
         ./
         re  =  radii[0];
         rp  =  radii[2];
         f   =  ( re - rp ) / re;

         /.
         Set a geodetic position.
         ./
         lon = 118.0 * rpd_c ( );
         lat =  30.0 * rpd_c ( );
         alt =   0.0;

         /.
         Do the conversion.
         ./
         georec_c ( lon, lat, alt, radii[0], f, rectan );

         printf( "Geodetic coordinates in deg and km (lon, lat, alt)\n" );
         printf( " %13.6f %13.6f %13.6f\n",
                 lon * dpr_c ( ), lat * dpr_c ( ), alt );
         printf( "Rectangular coordinates in km (x, y, z)\n" );
         printf( " %13.6f %13.6f %13.6f\n", rectan[0], rectan[1], rectan[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Geodetic coordinates in deg and km (lon, lat, alt)
          118.000000     30.000000      0.000000
      Rectangular coordinates in km (x, y, z)
        -2595.359123   4881.160589   3170.373523


   2) Create a table showing a variety of rectangular coordinates
      and the corresponding Earth geodetic coordinates. The
      values are computed using the equatorial radius of the Clark
      66 spheroid and the Clark 66 flattening factor:

         radius: 6378.2064
         flattening factor: 1./294.9787

      Note: the values shown above may not be current or suitable
            for your application.


      Corresponding rectangular and geodetic coordinates are
      listed to three decimal places. Input angles are in degrees.


      Example code begins here.


      /.
         Program georec_ex2
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
         SpiceDouble          clarkr;
         SpiceDouble          clarkf;
         SpiceDouble          rectan [3];
         SpiceDouble          rlat;
         SpiceDouble          rlon;

         SpiceInt             i;

         /.
         Define the input geodetic coordinates. Angles in
         degrees.
         ./
         SpiceDouble          lon    [NREC] = { 0.0,    0.0,  90.0,
                                                0.0,  180.0, -90.0,
                                                0.0,   45.0,   0.0,
                                               90.0,    45.0       };

         SpiceDouble          lat    [NREC] = {  90.0,    0.0,    0.0,
                                                 90.0,    0.0,    0.0,
                                                -90.0,    0.0,   88.707,
                                                 88.707, 88.1713        };

         SpiceDouble          alt    [NREC] = { -6356.5838,     0.0,
                                        0.0,        0.0,        0.0,
                                        0.0,        0.0,        0.0,
                                    -6355.5725, -6355.5725, -6355.5612 };

         /.
         Using the equatorial radius of the Clark66 spheroid
         (clarkr = 6378.2064 km) and the Clark 66 flattening
         factor (clarkf = 1.0 / 294.9787 ) convert from
         body fixed rectangular coordinates.
         ./
         clarkr = 6378.2064;
         clarkf = 1.0 / 294.9787;

         /.
         Print the banner.
         ./
         printf( "   lon      lat       alt     rectan[0]  rectan[1] "
                 " rectan[2]\n" );
         printf( " -------  -------  ---------  ---------  --------- "
                 " ---------\n" );

         /.
         Do the conversion.
         ./
         for ( i = 0; i < NREC; i++ )
         {

            rlon = lon[i] * rpd_c ( );
            rlat = lat[i] * rpd_c ( );

            georec_c ( rlon, rlat, alt[i], clarkr, clarkf, rectan );

            printf( "%8.3f %8.3f %10.3f", lon[i], lat[i], alt[i] );
            printf( "%11.3f %10.3f %10.3f\n",
                    rectan[0], rectan[1], rectan[2] );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


         lon      lat       alt     rectan[0]  rectan[1]  rectan[2]
       -------  -------  ---------  ---------  ---------  ---------
         0.000   90.000  -6356.584      0.000      0.000      0.000
         0.000    0.000      0.000   6378.206      0.000      0.000
        90.000    0.000      0.000      0.000   6378.206      0.000
         0.000   90.000      0.000      0.000      0.000   6356.584
       180.000    0.000      0.000  -6378.206      0.000      0.000
       -90.000    0.000      0.000      0.000  -6378.206      0.000
         0.000  -90.000      0.000      0.000      0.000  -6356.584
        45.000    0.000      0.000   4510.073   4510.073      0.000
         0.000   88.707  -6355.573      1.000      0.000      1.000
        90.000   88.707  -6355.573      0.000      1.000      1.000
        45.000   88.171  -6355.561      1.000      1.000      1.000


-Restrictions

   None.

-Literature_References

   [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
        Astrodynamics," Dover Publications Inc., 1971.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   H.A. Neilan         (JPL)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.4, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Added two complete code
       examples.

   -CSPICE Version 1.0.3, 26-JUL-2016 (BVS)

       Minor headers edits.

   -CSPICE Version 1.0.2, 30-JUL-2003 (NJB)

       Various header corrections were made.

   -CSPICE Version 1.0.1, 11-JAN-2003 (EDW)

       Removed a spurious non-printing character.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (CHA) (HAN) (WLT)

-Index_Entries

   geodetic to rectangular coordinates

-&
*/

{ /* Begin georec_c */

   /*
   Participate in error handling
   */

   chkin_c ( "georec_c");


   /*
   Call the f2c'd routine.
   */

   georec_( ( doublereal * ) &lon,
            ( doublereal * ) &lat,
            ( doublereal * ) &alt,
            ( doublereal * ) &re,
            ( doublereal * ) &f,
            ( doublereal * ) rectan );


   chkout_c ( "georec_c");


} /* End georec_c */
