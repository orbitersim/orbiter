/*

-Procedure recrad_c ( Rectangular coordinates to RA and DEC )

-Abstract

   Convert rectangular coordinates to range, right ascension, and
   declination.

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
   #undef    recrad_c


   void recrad_c ( ConstSpiceDouble    rectan[3],
                   SpiceDouble       * range,
                   SpiceDouble       * ra,
                   SpiceDouble       * dec      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   rectan     I   Rectangular coordinates of a point.
   range      O   Distance of the point from the origin.
   ra         O   Right ascension in radians.
   dec        O   Declination in radians.

-Detailed_Input

   rectan      are the rectangular coordinates of a point.

-Detailed_Output

   range       is the distance of the point `rectan' from the origin.

               The units associated with `range' are those associated
               with the input `rectan'.

   ra          is the right ascension of `rectan'. This is the angular
               distance measured toward the east from the prime meridian
               to the meridian containing the input point. The direction
               of increasing right ascension is from the +X axis towards
               the +Y axis.

               `ra' is output in radians. The range of `ra' is [0, 2*pi].


   dec         is the declination of `rectan'. This is the angle from
               the XY plane of the ray from the origin through the
               point.

               `dec' is output in radians. The range of `dec' is
               [-pi/2, pi/2].

-Parameters

   None.

-Exceptions

   Error free.

   1)  If the X and Y components of `rectan' are both zero, the
       right ascension is set to zero.

   2)  If `rectan' is the zero vector, right ascension and declination
       are both set to zero.

-Files

   None.

-Particulars

   None.

-Examples

   The following code fragment converts right ascension and
   declination from the B1950 reference frame to the J2000 frame.

      #include "SpiceUsr.h"

      SpiceDouble      ra;
      SpiceDouble      dec;
      SpiceDouble      r;
      SpiceDouble      mtrans  [ 3 ][ 3 ];
      SpiceDouble      v1950   [ 3 ];
      SpiceDouble      v2000   [ 3 ];

      /.
      Convert RA and DEC to a 3-vector expressed in the B1950 frame.
      ./
      radrec_c ( 1.0, ra, dec, v1950 );

      /.
      We use the CSPICE routine pxform_c to obtain the transformation
      matrix for converting vectors between the B1950 and J2000
      reference frames. Since both frames are inertial, the input time
      value we supply to pxform_c is arbitrary. We choose zero seconds
      past the J2000 epoch as the input value.
      ./
      pxform_c ( "B1950", "J2000", 0.0, mtrans );

      /.
      Transform the vector to the J2000 frame.
      ./
      mxv_c    ( mtrans, v1950, v2000 );

      /.
      Find the RA and DEC of the J2000-relative vector.
      ./
      recrad_c ( v2000, &r, &ra, &dec );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.3, 04-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.1.2, 30-JUL-2003 (NJB)

       Various header corrections were made.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

      Made input vector const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries

   rectangular coordinates to ra and dec
   rectangular to right_ascension and declination

-&
*/

{ /* Begin recrad_c */

   /*
   Call reclat_c to perform the conversion to angular terms.
   */

   reclat_c ( rectan, range, ra, dec );


   /*
   Right ascension is always in the domain [0, 2Pi].  Rectan_c returns
   ra in the domain [ -Pi, Pi ].  If ra is negative, add 2 Pi to map the
   value to the correct domain
   */

   if ( *ra < 0. )
      {
      *ra = *ra + twopi_c();
      }



} /* End recrad_c */
