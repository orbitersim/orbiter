/*

-Procedure pgrrec_c ( Planetographic to rectangular )

-Abstract

   Convert planetographic coordinates to rectangular coordinates.

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

   KERNEL
   NAIF_IDS
   PCK

-Keywords

   CONVERSION
   COORDINATES
   GEOMETRY
   MATH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void pgrrec_c ( ConstSpiceChar  * body,
                   SpiceDouble       lon,
                   SpiceDouble       lat,
                   SpiceDouble       alt,
                   SpiceDouble       re,
                   SpiceDouble       f,
                   SpiceDouble       rectan[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   body       I   Body with which coordinate system is associated.
   lon        I   Planetographic longitude of a point (radians).
   lat        I   Planetographic latitude of a point (radians).
   alt        I   Altitude of a point above reference spheroid.
   re         I   Equatorial radius of the reference spheroid.
   f          I   Flattening coefficient.
   rectan     O   Rectangular coordinates of the point.

-Detailed_Input

   body        is the name of the body with which the planetographic
               coordinate system is associated.

               `body' is used by this routine to look up from the
               kernel pool the prime meridian rate coefficient giving
               the body's spin sense. See the -Files and -Particulars
               header sections below for details.

   lon         is the planetographic longitude of the input point.
               This is the angle between the prime meridian and the
               meridian containing the input point. For bodies
               having prograde (aka direct) rotation, the direction
               of increasing longitude is positive west: from the +X
               axis of the rectangular coordinate system toward the
               -Y axis. For bodies having retrograde rotation, the
               direction of increasing longitude is positive east:
               from the +X axis toward the +Y axis.

               The earth, moon, and sun are exceptions:
               planetographic longitude is measured positive east for
               these bodies.

               The default interpretation of longitude by this
               and the other planetographic coordinate conversion
               routines can be overridden; see the discussion in
               -Particulars below for details.

               `lon' is measured in radians. On input, the range
               of longitude is unrestricted.

   lat         is the planetographic latitude of the input point.
               For a point `p' on the reference spheroid, this is the
               angle between the XY plane and the outward normal
               vector at `p'. For a point `p' not on the reference
               spheroid, the planetographic latitude is that of the
               closest point to `p' on the spheroid.

               `lat' is measured in radians. On input, the
               range of latitude is unrestricted.

   alt         is the altitude of point above the reference spheroid.
               Units of `alt' must match those of `re'.

   re          is the equatorial radius of a reference spheroid.
               This spheroid is a volume of revolution: its
               horizontal cross sections are circular. The shape of
               the spheroid is defined by an equatorial radius `re' and
               a polar radius `rp'. Units of `re' must match those of
               `alt'.

   f           is the flattening coefficient of the body, a
               dimensionless value defined as:

                  (re - rp) / re

               where `rp' is the polar radius of the spheroid, and the
               units of `rp' match those of `re'.

-Detailed_Output

   rectan      are the rectangular coordinates of the input point.
               See the discussion below in the -Particulars header
               section for details.

               The units associated with `rectan' are those associated
               with the inputs `alt' and `re'.

-Parameters

   None.

-Exceptions

   1)  If the body name `body' cannot be mapped to a NAIF ID code, and
       if `body' is not a string representation of an integer, the
       error SPICE(IDCODENOTFOUND) is signaled by a routine in the
       call tree of this routine.

   2)  If the kernel variable

          BODY<ID code>_PGR_POSITIVE_LON

       is present in the kernel pool but has a value other
       than one of

           'EAST'
           'WEST'

       the error SPICE(INVALIDOPTION) is signaled by a routine in the
       call tree of this routine. Case and blanks are ignored when
       these values are interpreted.

   3)  If polynomial coefficients for the prime meridian of `body' are
       not available in the kernel pool, and if the kernel variable
       BODY<ID code>_PGR_POSITIVE_LON is not present in the kernel
       pool, the error SPICE(MISSINGDATA) is signaled by a routine in
       the call tree of this routine.

   4)  If the equatorial radius is non-positive, the error
       SPICE(VALUEOUTOFRANGE) is signaled by a routine in the call
       tree of this routine.

   5)  If the flattening coefficient is greater than or equal to one,
       the error SPICE(VALUEOUTOFRANGE) is signaled by a routine in
       the call tree of this routine.

   6)  If the `body' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   7)  If the `body' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   This routine expects a kernel variable giving body's prime
   meridian angle as a function of time to be available in the
   kernel pool. Normally this item is provided by loading a PCK
   file. The required kernel variable is named

      BODY<body ID>_PM

   where <body ID> represents a string containing the NAIF integer
   ID code for `body'. For example, if `body' is "JUPITER", then
   the name of the kernel variable containing the prime meridian
   angle coefficients is

      BODY599_PM

   See the PCK Required Reading for details concerning the prime
   meridian kernel variable.

   The optional kernel variable

      BODY<body ID>_PGR_POSITIVE_LON

   also is normally defined via loading a text kernel. When this
   variable is present in the kernel pool, the prime meridian
   coefficients for `body' are not required by this routine. See the
   -Particulars section below for details.

-Particulars

   Given the planetographic coordinates of a point, this routine
   returns the body-fixed rectangular coordinates of the point. The
   body-fixed rectangular frame is that having the X-axis pass
   through the 0 degree latitude 0 degree longitude direction, the
   Z-axis pass through the 90 degree latitude direction, and the
   Y-axis equal to the cross product of the unit Z-axis and X-axis
   vectors.

   The planetographic definition of latitude is identical to the
   planetodetic (also called "geodetic" in SPICE documentation)
   definition. In the planetographic coordinate system, latitude is
   defined using a reference spheroid. The spheroid is
   characterized by an equatorial radius and a polar radius. For a
   point P on the spheroid, latitude is defined as the angle between
   the X-Y plane and the outward surface normal at P. For a point P
   off the spheroid, latitude is defined as the latitude of the
   nearest point to P on the spheroid. Note if P is an interior
   point, for example, if P is at the center of the spheroid, there
   may not be a unique nearest point to P.

   In the planetographic coordinate system, longitude is defined
   using the spin sense of the body. Longitude is positive to the
   west if the spin is prograde and positive to the east if the spin
   is retrograde. The spin sense is given by the sign of the first
   degree term of the time-dependent polynomial for the body's prime
   meridian Euler angle "W":  the spin is retrograde if this term is
   negative and prograde otherwise. For the sun, planets, most
   natural satellites, and selected asteroids, the polynomial
   expression for W may be found in a SPICE PCK kernel.

   The earth, moon, and sun are exceptions: planetographic longitude
   is measured positive east for these bodies.

   If you wish to override the default sense of positive longitude
   for a particular body, you can do so by defining the kernel
   variable

      BODY<body ID>_PGR_POSITIVE_LON

   where <body ID> represents the NAIF ID code of the body. This
   variable may be assigned either of the values

      'WEST'
      'EAST'

   For example, you can have this routine treat the longitude
   of the earth as increasing to the west using the kernel
   variable assignment

      BODY399_PGR_POSITIVE_LON = 'WEST'

   Normally such assignments are made by placing them in a text
   kernel and loading that kernel via furnsh_c.

   The definition of this kernel variable controls the behavior of
   the CSPICE planetographic routines

      pgrrec_c
      recpgr_c
      dpgrdr_c
      drdpgr_c

   It does not affect the other CSPICE coordinate conversion
   routines.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the rectangular coordinates of the point having Mars
      planetographic coordinates:

         longitude = 90 degrees west
         latitude  = 45 degrees north
         altitude  = 300 km

      Use the PCK kernel below to load the required triaxial
      ellipsoidal shape model and orientation data for Mars.

         pck00008.tpc


      Example code begins here.


      /.
         Program pgrrec_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local variables
         ./
         SpiceDouble             alt;
         SpiceDouble             f;
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             radii  [3];
         SpiceDouble             re;
         SpiceDouble             rectan [3];
         SpiceDouble             rp;

         SpiceInt                n;


         /.
         Load a PCK file containing a triaxial
         ellipsoidal shape model and orientation
         data for Mars.
         ./
         furnsh_c ( "pck00008.tpc" );

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
         Do the conversion.  Note that we must provide
         longitude and latitude in radians.
         ./
         lon =  90.0  * rpd_c();
         lat =  45.0  * rpd_c();
         alt =   3.e2;

         pgrrec_c ( "mars", lon, lat, alt, re, f, rectan );


         printf ( "\n"
                  "Planetographic coordinates:\n"
                  "\n"
                  "  Longitude (deg)        = %18.9e\n"
                  "  Latitude  (deg)        = %18.9e\n"
                  "  Altitude  (km)         = %18.9e\n"
                  "\n"
                  "Ellipsoid shape parameters:\n"
                  "\n"
                  "  Equatorial radius (km) = %18.9e\n"
                  "  Polar radius      (km) = %18.9e\n"
                  "  Flattening coefficient = %18.9e\n"
                  "\n"
                  "Rectangular coordinates:\n"
                  "\n"
                  "  X (km)                 = %18.9e\n"
                  "  Y (km)                 = %18.9e\n"
                  "  Z (km)                 = %18.9e\n"
                  "\n",
                  lon / rpd_c(),
                  lat / rpd_c(),
                  alt,
                  re,
                  rp,
                  f,
                  rectan[0],
                  rectan[1],
                  rectan[2]              );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Planetographic coordinates:

        Longitude (deg)        =    9.000000000e+01
        Latitude  (deg)        =    4.500000000e+01
        Altitude  (km)         =    3.000000000e+02

      Ellipsoid shape parameters:

        Equatorial radius (km) =    3.396190000e+03
        Polar radius      (km) =    3.376200000e+03
        Flattening coefficient =    5.886007556e-03

      Rectangular coordinates:

        X (km)                 =    1.604703022e-13
        Y (km)                 =   -2.620678915e+03
        Z (km)                 =    2.592408909e+03


   2) Below is a table showing a variety of rectangular coordinates
      and the corresponding Mars planetographic coordinates. The
      values are computed using the reference spheroid having radii

         Equatorial radius:    3396.190
         Polar radius:         3376.200

      Note:  the values shown above may not be current or suitable
             for your application.


      Corresponding rectangular and planetographic coordinates are
      listed to three decimal places.


      rectan[0]  rectan[1]  rectan[2]       lon       lat        alt
      --------------------------------------------------------------
       3396.190      0.000      0.000     0.000     0.000      0.000
      -3396.190      0.000      0.000   180.000     0.000      0.000
      -3406.190      0.000      0.000   180.000     0.000     10.000
      -3386.190      0.000      0.000   180.000     0.000    -10.000
          0.000  -3396.190      0.000    90.000     0.000      0.000
          0.000   3396.190      0.000   270.000     0.000      0.000
          0.000      0.000   3376.200     0.000    90.000      0.000
          0.000      0.000  -3376.200     0.000   -90.000      0.000
          0.000      0.000      0.000     0.000    90.000  -3376.200


   3) Below we show the analogous relationships for the earth,
      using the reference ellipsoid radii

         Equatorial radius:    6378.140
         Polar radius:         6356.750

      Note the change in longitudes for points on the +/- Y axis
      for the earth vs the Mars values.

      rectan[0]  rectan[1]  rectan[2]       lon       lat        alt
      --------------------------------------------------------------
       6378.140      0.000      0.000     0.000     0.000      0.000
      -6378.140      0.000      0.000   180.000     0.000      0.000
      -6388.140      0.000      0.000   180.000     0.000     10.000
      -6368.140      0.000      0.000   180.000     0.000    -10.000
          0.000  -6378.140      0.000   270.000     0.000      0.000
          0.000   6378.140      0.000    90.000     0.000      0.000
          0.000      0.000   6356.750     0.000    90.000      0.000
          0.000      0.000  -6356.750     0.000   -90.000      0.000
          0.000      0.000      0.000     0.000    90.000  -6356.750

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   H.A. Neilan         (JPL)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 26-DEC-2004 (CHA) (NJB) (HAN) (BVS) (WLT)

-Index_Entries

   convert planetographic to rectangular coordinates

-&
*/

{ /* Begin pgrrec_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "pgrrec_c" );


   /*
   Check the input string body to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "pgrrec_c", body );


   /*
   Call the f2c'd Fortran routine.
   */
   pgrrec_ ( ( char       * ) body,
             ( doublereal * ) &lon,
             ( doublereal * ) &lat,
             ( doublereal * ) &alt,
             ( doublereal * ) &re,
             ( doublereal * ) &f,
             ( doublereal * ) rectan,
             ( ftnlen       ) strlen(body)  );



   chkout_c ( "pgrrec_c" );

} /* End pgrrec_c */
