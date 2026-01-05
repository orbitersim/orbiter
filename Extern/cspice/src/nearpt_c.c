/*

-Procedure nearpt_c ( Nearest point on an ellipsoid )

-Abstract

   Locate the point on the surface of an ellipsoid that is nearest
   to a specified position. Also return the altitude of the position
   above the ellipsoid.

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

   ELLIPSOID
   GEOMETRY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef    nearpt_c


   void nearpt_c ( ConstSpiceDouble    positn[3],
                   SpiceDouble         a,
                   SpiceDouble         b,
                   SpiceDouble         c,
                   SpiceDouble         npoint[3],
                   SpiceDouble       * alt        )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   positn     I   Position of a point in bodyfixed frame.
   a          I   Length of semi-axis parallel to x-axis.
   b          I   Length of semi-axis parallel to y-axis.
   c          I   Length on semi-axis parallel to z-axis.
   npoint     O   Point on the ellipsoid closest to positn.
   alt        O   Altitude of positn above the ellipsoid.

-Detailed_Input

   positn      3-vector giving the position of a point with respect to
               the center of an ellipsoid. The vector is expressed in a
               body-fixed reference frame. The semi-axes of the
               ellipsoid are aligned with the x, y, and z-axes of the
               body-fixed frame.

   a           is the length of the semi-axis of the ellipsoid that is
               parallel to the x-axis of the bodyfixed coordinate
               system.

   b           is the length of the semi-axis of the ellipsoid that is
               parallel to the y-axis of the bodyfixed coordinate
               system.

   c           is the length of the semi-axis of the ellipsoid that is
               parallel to the z-axis of the bodyfixed coordinate
               system.

-Detailed_Output

   npoint      is the nearest point on the ellipsoid to `positn'.
               `npoint' is a 3-vector expressed in the body-fixed
               reference frame.

   alt         is the altitude of `positn' above the ellipsoid. If
               `positn' is inside the ellipsoid, `alt' will be negative
               and have magnitude equal to the distance between `npoint'
               and `positn'.

-Parameters

   None.

-Exceptions

   1)  If any of the axis lengths `a', `b' or `c' are non-positive, the
       error SPICE(BADAXISLENGTH) is signaled by a routine in the
       call tree of this routine.

   2)  If the ratio of the longest to the shortest ellipsoid axis
       is large enough so that arithmetic expressions involving its
       squared value may overflow, the error SPICE(BADAXISLENGTH)
       is signaled by a routine in the call tree of this routine.

   3)  If any of the expressions

          a * abs( positn[0] ) / m^2
          b * abs( positn[1] ) / m^2
          c * abs( positn[2] ) / m^2

       where `m' is the minimum of { a, b, c }, is large enough so
       that arithmetic expressions involving these sub-expressions
       may overflow, the error SPICE(INPUTSTOOLARGE) is signaled by a
       routine in the call tree of this routine.

   4)  If the axes of the ellipsoid have radically different
       magnitudes, for example if the ratios of the axis lengths vary
       by 10 orders of magnitude, the results may have poor
       precision. No error checks are done to identify this problem.

   5)  If the axes of the ellipsoid and the input point `positn' have
       radically different magnitudes, for example if the ratio of
       the magnitude of `positn' to the length of the shortest axis is
       1.e25, the results may have poor precision. No error checks
       are done to identify this problem.

-Files

   None.

-Particulars

   Many applications of this routine are more easily performed
   using the higher-level CSPICE routine subpnt_c. This routine
   is the mathematical workhorse on which subpnt_c relies.

   This routine solves for the location, N, on the surface of an
   ellipsoid nearest to an arbitrary location, P, relative to that
   ellipsoid.

-Examples

   Example 1.

   The code fragment below illustrates how you can use CSPICE to
   compute the sub-earth point on the moon.

      /.
      Load the ephemeris, leapseconds and physical constants files
      first. We assume the names of these files are stored in the
      character variables SPK, LSK and PCK.
      ./
      furnsh_c ( SPK );
      furnsh_c ( LSK );
      furnsh_c ( PCK );

      /.
      Get the apparent position of the Moon as seen from Earth.
      Look up this position vector in the moon body-fixed frame
      IAU_MOON. The orientation of the IAU_MOON frame will be
      computed at epoch et-lt.
      ./
      spkpos_c ( "moon", et, "IAU_MOON", "lt+s", "earth, trgpos, &lt );

      /.
      Negate the moon's apparent position to obtain the
      position of the earth in the moon's body-fixed frame.
      ./
      vminus_c ( trgpos, evec );

      /.
      Get the lengths of the principal axes of the moon. Transfer the
      elements of the array radii to the variables a, b, c to enhance
      readability.
      ./
      bodvcd_c (  399,    "RADII",  3,   &dim,  radii );
      vupack_c (  radii,  &a,       &b,  &c    );

      /.
      Finally get the point `subpnt' on the surface of the
      moon closest to the earth --- the sub-earth point.
      ./
      nearpt_c ( evec, a, b, c, subpnt, &alt );


   Example 2.

      One can use this routine to define a generalization of GEODETIC
      coordinates called GAUSSIAN coordinates of a triaxial body. (The
      name is derived from the famous Gauss-map of classical
      differential geometry).  The coordinates are longitude, latitude,
      and altitude.

      We let the x-axis of the body fixed coordinate system point along
      the longest axis of the triaxial body. The y-axis points along
      the middle axis and the z-axis points along the shortest axis.

      Given a point P, there is a point on the ellipsoid that is
      closest to P, call it Q. The latitude and longitude of P is
      determined by constructing the outward pointing unit normal to
      the ellipsoid at Q. The latitude of P is the latitude that the
      normal points towards in the bodyfixed frame. The longitude of P
      is the longitude the normal points to in the bodyfixed frame. The
      altitude is the signed distance from P to Q, positive if P is
      outside the ellipsoid, negative if P is inside. (the mapping of
      the point Q to the unit normal at Q is the Gauss-map of Q).

      To obtain the Gaussian coordinates of a point whose position in
      bodyfixed rectangular coordinates is given by a vector P, the
      code fragment below will suffice.

         nearpt_c ( p,    a,  b,     c,   q,  &alt  );
         surfnm_c (       a,  b,     c    q,  nrml  );
         reclat_c ( nrml, &r, &long, &lat           );

      The Gaussian coordinates are long, lat, alt.

-Restrictions

   1)  See the -Exceptions header section above.

-Literature_References

   None.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.0.0, 01-NOV-2021 (NJB) (JDR) (EDW)

       Edit to logic to reduce unneeded operations when
       error or projection vectors equal zero. Addition
       of details concerning the "ellipsoid near point"
       problem and solution.

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.3.2, 17-NOV-2005 (NJB) (EDW)

       The -Exceptions and -Restrictions header sections were updated.
       A reference to bodvar_c in the header was changed to a
       reference to bodvcd_c.

   -CSPICE Version 1.3.1, 28-JUL-2003 (NJB) (CHA)

       Various header corrections were made.

   -CSPICE Version 1.3.0, 21-OCT-1998 (NJB)

       Made input vector const.

   -CSPICE Version 1.2.0, 15-FEB-1998 (EDW)

       Minor corrections to header.

   -CSPICE Version 1.2.0, 08-FEB-1998 (NJB)

       Removed local variables used for temporary capture of outputs.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

       Based on  SPICELIB Version 1.1.0, 27-NOV-1990 (WLT)

-Index_Entries

   distance from point to ellipsoid
   nearest point on an ellipsoid

-&
*/

{ /* Begin nearpt_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "nearpt_c" );


   /*
   Call the f2c'd nearpt.
   */
   nearpt_( (doublereal *)  positn,
            (doublereal *)  &a,
            (doublereal *)  &b,
            (doublereal *)  &c,
            (doublereal *)  npoint,
            (doublereal *)  alt     );


   chkout_c ( "nearpt_c" );


} /* End nearpt_c */
