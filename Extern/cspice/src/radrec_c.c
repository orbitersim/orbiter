/*

-Procedure radrec_c ( Range, RA and DEC to rectangular coordinates )

-Abstract

   Convert from range, right ascension, and declination to
   rectangular coordinates.

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

   void radrec_c ( SpiceDouble range,
                   SpiceDouble ra,
                   SpiceDouble dec,
                   SpiceDouble rectan[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   range      I   Distance of a point from the origin.
   ra         I   Right ascension of point in radians.
   dec        I   Declination of point in radians.
   rectan     O   Rectangular coordinates of the point.

-Detailed_Input

   range       is the distance of the point from the origin. Input
               should be in terms of the same units in which the
               output is desired.

   ra          is the right ascension of the point. This is the angular
               distance measured toward the east from the prime
               meridian to the meridian containing the input point.
               The direction of increasing right ascension is from
               the +X axis towards the +Y axis.

               The range (i.e., the set of allowed values) of
               `ra' is unrestricted. Units are radians.

   dec         is the declination of the point. This is the angle from
               the XY plane of the ray from the origin through the
               point.

               The range (i.e., the set of allowed values) of
               `dec' is unrestricted. Units are radians.

-Detailed_Output

   rectan      is the array containing the rectangular coordinates of
               the point.

               The units associated with `rectan' are those
               associated with the input `range'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This routine converts the right ascension, declination, and range
   of a point into the associated rectangular coordinates.

   The input is defined by a distance from a central reference point,
   an angle from a reference meridian, and an angle above the equator
   of a sphere centered at the central reference point.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Convert to the J2000 frame the right ascension and declination
      of an object initially expressed with respect to the B1950
      reference frame.


      Example code begins here.


      /.
         Program radrec_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          decb;
         SpiceDouble          decj;
         SpiceDouble          mtrans [3][3];
         SpiceDouble          r;
         SpiceDouble          rab;
         SpiceDouble          raj;
         SpiceDouble          v1950  [3];
         SpiceDouble          v2000  [3];

         /.
         Set the initial right ascension and declination
         coordinates of the object, given with respect
         to the B1950 reference frame.
         ./
         rab  = 135.88680896;
         decb =  17.50151037;

         /.
         Convert `rab' and `decb' to a 3-vector expressed in
         the B1950 frame.
         ./
         radrec_c ( 1.0, rab * rpd_c(), decb * rpd_c(), v1950 );

         /.
         We use the CSPICE routine pxform_c to obtain the
         transformation  matrix for converting vectors between
         the B1950 and J2000 reference frames.  Since
         both frames are inertial, the input time value we
         supply to pxform_c is arbitrary.  We choose zero
         seconds past the J2000 epoch.
         ./
         pxform_c ( "B1950", "J2000", 0.0, mtrans );

         /.
         Transform the vector to the J2000 frame.
         ./
         mxv_c ( mtrans, v1950, v2000 );

         /.
         Find the right ascension and declination of the
         J2000-relative vector.
         ./
         recrad_c ( v2000, &r, &raj, &decj );

         /.
         Output the results.
         ./
         printf( "Right ascension (B1950 frame): %f\n", rab );
         printf( "Declination (B1950 frame)    : %f\n", decb );

         printf( "Right ascension (J2000 frame): %f\n", raj * dpr_c() );
         printf( "Declination (J2000 frame)    : %f\n", decj * dpr_c() );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Right ascension (B1950 frame): 135.886809
      Declination (B1950 frame)    : 17.501510
      Right ascension (J2000 frame): 136.587682
      Declination (J2000 frame)    : 17.300443


   2) Define a set of 15 right ascension-declination data pairs for
      the Earth's pole at different ephemeris epochs, convert them
      to rectangular coordinates and compute the angular separation
      between these coordinates and the IAU_EARTH pole given by a
      PCK kernel.

      Use the PCK kernel below to load the required triaxial
      ellipsoidal shape model and orientation data for the Earth.

         pck00010.tpc


      Example code begins here.


      /.
         Program radrec_ex2
      ./
      #include <math.h>
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NCOORD       15

         /.
         Local variables
         ./
         SpiceDouble          pole   [3];
         SpiceDouble          mtrans [3][3];
         SpiceDouble          v2000  [3];
         SpiceInt             i;

         /.
         Define a set of 15 right ascension-declination
         coordinate pairs (in degrees) for the Earth's pole
         and the array of corresponding ephemeris times in
         J2000 TDB seconds.
         ./
         SpiceDouble          ra     [NCOORD] = { 180.003739, 180.003205,
                                                  180.002671, 180.002137,
                                                  180.001602, 180.001068,
                                                  180.000534, 360.000000,
                                                  359.999466, 359.998932,
                                                  359.998397, 359.997863,
                                                  359.997329, 359.996795,
                                                  359.996261 };

         SpiceDouble          dec    [NCOORD] = { 89.996751, 89.997215,
                                                  89.997679, 89.998143,
                                                  89.998608, 89.999072,
                                                  89.999536, 90.000000,
                                                  89.999536, 89.999072,
                                                  89.998607, 89.998143,
                                                  89.997679, 89.997215,
                                                  89.996751 };

         SpiceDouble          et     [NCOORD] = { -18408539.52023917,
                                                  -15778739.49107254,
                                                  -13148939.46190590,
                                                  -10519139.43273926,
                                                   -7889339.40357262,
                                                   -5259539.37440598,
                                                   -2629739.34523934,
                                                         60.68392730,
                                                    2629860.71309394,
                                                    5259660.74226063,
                                                    7889460.77142727,
                                                   10519260.80059391,
                                                   13149060.82976055,
                                                   15778860.85892719,
                                                   18408660.88809383 };

         SpiceDouble          z      [3] = { 0.0, 0.0, 1.0 };

         /.
         Load a PCK kernel.
         ./
         furnsh_c ( "pck00010.tpc" );

         /.
         Print the banner out.
         ./
         printf( "       et           Angular difference\n" );
         printf( "------------------  ------------------\n" );

         for ( i = 0; i < NCOORD; i++ )
         {

            /.
            Convert the right ascension and declination
            coordinates (in degrees) to rectangular.
            ./
            radrec_c ( 1.0, ra[i] * rpd_c(), dec[i] * rpd_c(), v2000 );

            /.
            Retrieve the transformation matrix from the J2000
            frame to the IAU_EARTH frame.
            ./
            pxform_c ( "J2000", "IAU_EARTH", et[i], mtrans );

            /.
            Rotate the `v2000' vector into IAU_EARTH. This vector
            should equal (round-off) the Z direction unit vector.
            ./
            mxv_c ( mtrans, v2000, pole );

            /.
            Output the ephemeris time and the angular separation
            between the rotated vector and the Z direction unit
            vector.
            ./
            printf( "%18.8f  %17.16f\n",
                    et[i], vsep_c ( pole, z ) * dpr_c() );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


             et           Angular difference
      ------------------  ------------------
      -18408539.52023917  0.0000001559918278
      -15778739.49107254  0.0000000106799881
      -13148939.46190590  0.0000001773517911
      -10519139.43273926  0.0000003440236194
       -7889339.40357262  0.0000004893045693
       -5259539.37440598  0.0000003226327536
       -2629739.34523934  0.0000001559609507
             60.68392730  0.0000000107108706
        2629860.71309394  0.0000001773826862
        5259660.74226063  0.0000003440544891
        7889460.77142727  0.0000004892736740
       10519260.80059391  0.0000003226018712
       13149060.82976055  0.0000001559300556
       15778860.85892719  0.0000000107417474
       18408660.88809383  0.0000001774135760


-Restrictions

   None.

-Literature_References

   [1]  L. Taff, "Celestial Mechanics, A Computational Guide for the
        Practitioner," Wiley, 1985

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.3, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing code fragment and a second
       example.

       Added -Particulars section.

   -CSPICE Version 1.0.2, 28-JUL-2003 (NJB)

       Various header corrections were made.

   -CSPICE Version 1.0.1, 13-APR-2000 (NJB)

       Made some minor updates and corrections in the code example.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries

   range ra and dec to rectangular coordinates
   right_ascension and declination to rectangular

-&
*/

{ /* Begin radrec_c */

   /*
   There isn't much to say or do...
   */

   latrec_c ( range, ra, dec, rectan );


} /* End radrec_c */
