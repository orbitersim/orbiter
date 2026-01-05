/*

-Procedure recgeo_c ( Rectangular to geodetic )

-Abstract

   Convert from rectangular coordinates to geodetic coordinates.

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
   #include "SpiceZim.h"
   #undef    recgeo_c


   void recgeo_c ( ConstSpiceDouble     rectan[3],
                   SpiceDouble          re,
                   SpiceDouble          f,
                   SpiceDouble        * lon,
                   SpiceDouble        * lat,
                   SpiceDouble        * alt        )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   rectan     I   Rectangular coordinates of a point.
   re         I   Equatorial radius of the reference spheroid.
   f          I   Flattening coefficient.
   lon        O   Geodetic longitude of the point (radians).
   lat        O   Geodetic latitude  of the point (radians).
   alt        O   Altitude of the point above reference spheroid.

-Detailed_Input

   rectan      are the rectangular coordinates of a point. `rectan'
               must be in the same units as `re'.

   re          is the equatorial radius of a reference spheroid.
               This spheroid is a volume of revolution: its
               horizontal cross sections are circular. The shape of
               the spheroid is defined by an equatorial radius `re' and
               a polar radius `rp'. `re' must be in the same units as
               `rectan'.

   f           is the flattening coefficient = (re-rp) / re,  where
               `rp' is the polar radius of the spheroid.

-Detailed_Output

   lon         is the geodetic longitude of the input point. This
               is the angle between the prime meridian and the
               meridian containing `rectan'. The direction of
               increasing longitude is from the +X axis towards the
               +Y axis.

               `lon' is output in radians. The range of `lon' is
               [-pi, pi].

   lat         is the geodetic latitude of the input point. For a
               point P on the reference spheroid, this is the angle
               between the XY plane and the outward normal vector at
               P. For a point P not on the reference spheroid, the
               geodetic latitude is that of the closest point to P on
               the spheroid.

               `lat' is output in radians. The range of `lat' is
               [-pi/2, pi/2].

   alt         is the altitude of point above the reference spheroid.

               The units associated with `alt' are those associated
               with the inputs `rectan' and `re'.

-Parameters

   None.

-Exceptions

   1)  If the equatorial radius is non-positive, the error
       SPICE(VALUEOUTOFRANGE) is signaled by a routine in the call
       tree of this routine.

   2)  If the flattening coefficient is greater than or equal to one,
       the error SPICE(VALUEOUTOFRANGE) is signaled by a routine in
       the call tree of this routine.

   3)  For points inside the reference ellipsoid, the nearest
       point on the ellipsoid to `rectan' may not be unique, so
       latitude may not be well-defined.

-Files

   None.

-Particulars

   Given the body-fixed rectangular coordinates of a point, and the
   constants describing the reference spheroid,  this routine
   returns the geodetic coordinates of the point. The body-fixed
   rectangular frame is that having the X-axis pass through the
   0 degree latitude 0 degree longitude point. The Y-axis passes
   through the 0 degree latitude 90 degree longitude. The Z-axis
   passes through the 90 degree latitude point. For some bodies
   this coordinate system may not be a right-handed coordinate
   system.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the geodetic coordinates of the point having Earth
      rectangular coordinates:

         X (km) =  -2541.748162
         Y (km) =   4780.333036
         Z (km) =   3360.428190

      Use the PCK kernel below to load the required triaxial
      ellipsoidal shape model and orientation data for the Earth.

         pck00010.tpc


      Example code begins here.


      /.
         Program recgeo_ex1
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
         Set a body-fixed position.
         ./
         rectan[0] =  -2541.748162;
         rectan[1] =   4780.333036;
         rectan[2] =   3360.428190;

         /.
         Do the conversion.
         ./
         recgeo_c ( rectan, radii[0], f, &lon, &lat, &alt );

         printf( "Rectangular coordinates in km (x, y, z)\n" );
         printf( " %13.6f %13.6f %13.6f\n", rectan[0], rectan[1], rectan[2] );
         printf( "Geodetic coordinates in deg and km (lon, lat, alt)\n" );
         printf( " %13.6f %13.6f %13.6f\n",
                 lon * dpr_c ( ), lat * dpr_c ( ), alt );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Rectangular coordinates in km (x, y, z)
        -2541.748162   4780.333036   3360.428190
      Geodetic coordinates in deg and km (lon, lat, alt)
          118.000000     31.999957      0.001916


   2) Create a table showing a variety of rectangular coordinates
      and the corresponding Earth geodetic coordinates. The
      values are computed using the equatorial radius of the Clark
      66 spheroid and the Clark 66 flattening factor:

         radius: 6378.2064
         flattening factor: 1./294.9787

      Note: the values shown above may not be current or suitable
            for your application.


      Corresponding rectangular and geodetic coordinates are
      listed to three decimal places. Output angles are in degrees.

      Example code begins here.


      /.
         Program recgeo_ex2
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
         SpiceDouble          alt;
         SpiceDouble          clarkr;
         SpiceDouble          clarkf;
         SpiceDouble          lat;
         SpiceDouble          lon;

         SpiceInt             i;

         /.
         Define the input rectangular coordinates.
         ./
         SpiceDouble          rectan [NREC][3] = {
                                   {    0.0,        0.0,        0.0   },
                                   { 6378.2064,     0.0,        0.0   },
                                   {    0.0,     6378.2064,     0.0   },
                                   {    0.0,        0.0,     6378.2064},
                                   {-6378.2064,     0.0,        0.0   },
                                   {    0.0,    -6378.2064,     0.0   },
                                   {    0.0,        0.0,    -6378.2064},
                                   { 6378.2064,  6378.2064,     0.0   },
                                   { 6378.2064,     0.0,     6378.2064},
                                   {    0.0,     6378.2064,  6378.2064},
                                   { 6378.2064,  6378.2064,  6378.2064} };

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
         printf( " rectan[0]  rectan[1]  rectan[2]    lon      lat   "
                 "    alt\n" );
         printf( " ---------  ---------  ---------  -------  ------- "
                 " ---------\n" );

         /.
         Do the conversion. Output angles in degrees.
         ./
         for ( i = 0; i < NREC; i++ )
         {

            recgeo_c ( rectan[i], clarkr, clarkf, &lon, &lat, &alt );

            printf( "%10.3f %10.3f %10.3f",
                    rectan[i][0], rectan[i][1], rectan[i][2] );
            printf( "%9.3f %8.3f %10.3f\n",
                    lon * dpr_c ( ), lat * dpr_c ( ), alt );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       rectan[0]  rectan[1]  rectan[2]    lon      lat       alt
       ---------  ---------  ---------  -------  -------  ---------
           0.000      0.000      0.000    0.000   90.000  -6356.584
        6378.206      0.000      0.000    0.000    0.000      0.000
           0.000   6378.206      0.000   90.000    0.000      0.000
           0.000      0.000   6378.206    0.000   90.000     21.623
       -6378.206      0.000      0.000  180.000    0.000      0.000
           0.000  -6378.206      0.000  -90.000    0.000      0.000
           0.000      0.000  -6378.206    0.000  -90.000     21.623
        6378.206   6378.206      0.000   45.000    0.000   2641.940
        6378.206      0.000   6378.206    0.000   45.137   2652.768
           0.000   6378.206   6378.206   90.000   45.137   2652.768
        6378.206   6378.206   6378.206   45.000   35.370   4676.389


-Restrictions

   None.

-Literature_References

   [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
        Astrodynamics," Dover Publications Inc., 1971.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.4, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Added two complete code
       examples.

   -CSPICE Version 1.2.3, 26-JUL-2016 (BVS)

       Minor headers edits.

   -CSPICE Version 1.2.2, 02-JUL-2007 (NJB)

       In -Examples section of header, heading and description of
       right-hand table was updated to use correct names of columns.
       Term "bodyfixed" is now hyphenated.

   -CSPICE Version 1.2.1, 30-JUL-2003 (NJB) (CHA)

       Various header changes were made to improve clarity. Some
       minor header corrections were made.

   -CSPICE Version 1.2.0, 28-AUG-2001 (NJB)

       Removed tab characters from source file. Include interface
       macro definition file SpiceZim.h.

   -CSPICE Version 1.1.0, 21-OCT-1998 (NJB)

       Made input vector const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries

   rectangular to geodetic

-&
*/

{ /* Begin recgeo_c */

   /*
   Participate in error handling
   */

   chkin_c ( "recgeo_c");


   /*
   Call the f2c'd routine.
   */

   recgeo_( ( doublereal * ) rectan,
            ( doublereal * ) &re,
            ( doublereal * ) &f,
            ( doublereal * ) lon,
            ( doublereal * ) lat,
            ( doublereal * ) alt);


   chkout_c ( "recgeo_c");


} /* End recgeo_c */
