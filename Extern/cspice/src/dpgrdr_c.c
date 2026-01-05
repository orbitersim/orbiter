/*

-Procedure dpgrdr_c ( Derivative of planetographic w.r.t. rectangular )

-Abstract

   Compute the Jacobian matrix of the transformation from
   rectangular to planetographic coordinates.

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
   #include "SpiceZmc.h"


   void dpgrdr_c ( ConstSpiceChar  * body,
                   SpiceDouble       x,
                   SpiceDouble       y,
                   SpiceDouble       z,
                   SpiceDouble       re,
                   SpiceDouble       f,
                   SpiceDouble       jacobi[3][3]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   body       I   Body with which coordinate system is associated.
   x          I   X-coordinate of point.
   y          I   Y-coordinate of point.
   z          I   Z-coordinate of point.
   re         I   Equatorial radius of the reference spheroid.
   f          I   Flattening coefficient.
   jacobi     O   Matrix of partial derivatives.

-Detailed_Input

   body        is the name of the body with which the planetographic
               coordinate system is associated.

               `body' is used by this routine to look up from the
               kernel pool the prime meridian rate coefficient giving
               the body's spin sense. See the -Files and -Particulars
               header sections below for details.

   x,
   y,
   z           are the rectangular coordinates of the point at
               which the Jacobian of the map from rectangular
               to planetographic coordinates is desired.

   re          is the equatorial radius of the reference spheroid. This
               spheroid is a volume of revolution: its horizontal
               cross sections are circular. The shape of the
               spheroid is defined by an equatorial radius `re' and
               a polar radius `rp'.

   f           is the flattening coefficient =

                  (re-rp) / re

               where `rp' is the polar radius of the spheroid. The
               units of `rp' match those of `re'. (More importantly
               rp = re*(1-f) )

-Detailed_Output

   jacobi      is the matrix of partial derivatives of the conversion
               from rectangular to planetographic coordinates. It
               has the form

                  .-                             -.
                  |  dlon/dx   dlon/dy   dlon/dz  |
                  |  dlat/dx   dlat/dy   dlat/dz  |
                  |  dalt/dx   dalt/dy   dalt/dz  |
                  `-                             -'

               evaluated at the input values of `x', `y', and `z'.

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

   6)  If the input point is on the Z-axis (X = 0 and Y = 0), the
       Jacobian matrix is undefined, an error is signaled by a
       routine in the call tree of this routine.

   7)  If the `body' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   8)  If the `body' input string has zero length, the error
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

   When performing vector calculations with velocities it is usually
   most convenient to work in rectangular coordinates. However, once
   the vector manipulations have been performed, it is often
   desirable to convert the rectangular representations into
   planetographic coordinates to gain insights about phenomena in
   this coordinate frame.

   To transform rectangular velocities to derivatives of coordinates
   in a planetographic system, one uses the Jacobian of the
   transformation between the two systems.

   Given a state in rectangular coordinates

      ( x, y, z, dx, dy, dz )

   the velocity in planetographic coordinates is given by the matrix
   equation:
                        t          |                     t
      (dlon, dlat, dalt)   = jacobi|       * (dx, dy, dz)
                                   |(x,y,z)

   This routine computes the matrix

            |
      jacobi|
            |(x, y, z)


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

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Find the planetographic state of the earth as seen from
      Mars in the J2000 reference frame at January 1, 2005 TDB.
      Map this state back to rectangular coordinates as a check.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: dpgrdr_ex1.tm

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
            pck00008.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00008.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program dpgrdr_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local variables
         ./
         SpiceDouble             alt;
         SpiceDouble             drectn [3];
         SpiceDouble             et;
         SpiceDouble             f;
         SpiceDouble             jacobi [3][3];
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             lt;
         SpiceDouble             pgrvel [3];
         SpiceDouble             radii  [3];
         SpiceDouble             re;
         SpiceDouble             rectan [3];
         SpiceDouble             rp;
         SpiceDouble             state  [6];

         SpiceInt                n;


         /.
         Load SPK, PCK, and LSK kernels, use a meta kernel for
         convenience.
         ./
         furnsh_c ( "dpgrdr_ex1.tm" );

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
         Look up the geometric state of earth as seen from Mars at
         January 1, 2005 TDB, relative to the J2000 reference
         frame.
         ./
         str2et_c ( "January 1, 2005 TDB", &et);

         spkezr_c ( "Earth", et,    "J2000", "LT+S",
                    "Mars",  state, &lt            );

         /.
         Convert position to planetographic coordinates.
         ./
         recpgr_c ( "mars", state, re, f, &lon, &lat, &alt );

         /.
         Convert velocity to planetographic coordinates.
         ./

         dpgrdr_c ( "MARS",  state[0],  state[1],  state[2],
                    re,      f,         jacobi               );

         mxv_c ( jacobi, state+3, pgrvel );


         /.
         As a check, convert the planetographic state back to
         rectangular coordinates.
         ./
         pgrrec_c ( "mars", lon, lat, alt, re, f, rectan );
         drdpgr_c ( "mars", lon, lat, alt, re, f, jacobi );

         mxv_c ( jacobi, pgrvel, drectn );

         printf ( "\n"
                  "Rectangular coordinates:\n"
                  "\n"
                  "  X (km)                 = %18.9e\n"
                  "  Y (km)                 = %18.9e\n"
                  "  Z (km)                 = %18.9e\n"
                  "\n"
                  "Rectangular velocity:\n"
                  "\n"
                  "  dX/dt (km/s)           = %18.9e\n"
                  "  dY/dt (km/s)           = %18.9e\n"
                  "  dZ/dt (km/s)           = %18.9e\n"
                  "\n",
                  state [0],
                  state [1],
                  state [2],
                  state [3],
                  state [4],
                  state [5]                );

         printf ( "Ellipsoid shape parameters:\n"
                  "\n"
                  "  Equatorial radius (km) = %18.9e\n"
                  "  Polar radius      (km) = %18.9e\n"
                  "  Flattening coefficient = %18.9e\n"
                  "\n",
                  re,
                  rp,
                  f                );

         printf ( "Planetographic coordinates:\n"
                  "\n"
                  "  Longitude (deg)        = %18.9e\n"
                  "  Latitude  (deg)        = %18.9e\n"
                  "  Altitude  (km)         = %18.9e\n"
                  "\n"
                  "Planetographic velocity:\n"
                  "\n"
                  "  d Longitude/dt (deg/s) = %18.9e\n"
                  "  d Latitude/dt  (deg/s) = %18.9e\n"
                  "  d Altitude/dt  (km/s)  = %18.9e\n"
                  "\n",
                  lon / rpd_c(),
                  lat / rpd_c(),
                  alt,
                  pgrvel[0]/rpd_c(),
                  pgrvel[1]/rpd_c(),
                  pgrvel[2]                );

         printf ( "Rectangular coordinates from inverse mapping:\n"
                  "\n"
                  "  X (km)                 = %18.9e\n"
                  "  Y (km)                 = %18.9e\n"
                  "  Z (km)                 = %18.9e\n"
                  "\n"
                  "Rectangular velocity from inverse mapping:\n"
                  "\n"
                  "  dX/dt (km/s)           = %18.9e\n"
                  "  dY/dt (km/s)           = %18.9e\n"
                  "  dZ/dt (km/s)           = %18.9e\n"
                  "\n",
                  rectan [0],
                  rectan [1],
                  rectan [2],
                  drectn [0],
                  drectn [1],
                  drectn [2]                );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Rectangular coordinates:

        X (km)                 =    1.460397337e+08
        Y (km)                 =    2.785466054e+08
        Z (km)                 =    1.197503176e+08

      Rectangular velocity:

        dX/dt (km/s)           =   -4.704327200e+01
        dY/dt (km/s)           =    9.073261550e+00
        dZ/dt (km/s)           =    4.757916901e+00

      Ellipsoid shape parameters:

        Equatorial radius (km) =    3.396190000e+03
        Polar radius      (km) =    3.376200000e+03
        Flattening coefficient =    5.886007556e-03

      Planetographic coordinates:

        Longitude (deg)        =    2.976676594e+02
        Latitude  (deg)        =    2.084450444e+01
        Altitude  (km)         =    3.365318255e+08

      Planetographic velocity:

        d Longitude/dt (deg/s) =   -8.357706663e-06
        d Latitude/dt  (deg/s) =    1.593556685e-06
        d Altitude/dt  (km/s)  =   -1.121160078e+01

      Rectangular coordinates from inverse mapping:

        X (km)                 =    1.460397337e+08
        Y (km)                 =    2.785466054e+08
        Z (km)                 =    1.197503176e+08

      Rectangular velocity from inverse mapping:

        dX/dt (km/s)           =   -4.704327200e+01
        dY/dt (km/s)           =    9.073261550e+00
        dZ/dt (km/s)           =    4.757916901e+00


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header comments to comply with NAIF standard.
       Modified code example to use meta-kernel to load kernels.

       Updated example code to split printf statement in three in order to
       comply with ANSI-C maximum string literal of length.

   -CSPICE Version 1.0.0, 26-DEC-2004 (NJB) (WLT)

-Index_Entries

   Jacobian of planetographic  w.r.t. rectangular coordinates

-&
*/

{ /* Begin dpgrdr_c */


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "dpgrdr_c" );

   /*
   Check the input string body to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "dpgrdr_c", body );

   /*
   Call the f2c'd Fortran routine.
   */
   dpgrdr_ ( ( char       * ) body,
             ( doublereal * ) &x,
             ( doublereal * ) &y,
             ( doublereal * ) &z,
             ( doublereal * ) &re,
             ( doublereal * ) &f,
             ( doublereal * ) jacobi,
             ( ftnlen       ) strlen(body)  );

   /*
   Convert Jacobian matrix to row-major order.
   */
   xpose_c ( jacobi, jacobi );


   chkout_c ( "dpgrdr_c" );

} /* End dpgrdr_c */
