/*

-Procedure npedln_c ( Nearest point on ellipsoid to line )

-Abstract

   Find nearest point on a triaxial ellipsoid to a specified line,
   and the distance from the ellipsoid to the line.

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

   ELLIPSES

-Keywords

   ELLIPSOID
   GEOMETRY
   MATH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    npedln_c


   void npedln_c ( SpiceDouble         a,
                   SpiceDouble         b,
                   SpiceDouble         c,
                   ConstSpiceDouble    linept[3],
                   ConstSpiceDouble    linedr[3],
                   SpiceDouble         pnear[3],
                   SpiceDouble       * dist      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   a          I   Length of ellipsoid's semi-axis in the x direction
   b          I   Length of ellipsoid's semi-axis in the y direction
   c          I   Length of ellipsoid's semi-axis in the z direction
   linept     I   Point on line
   linedr     I   Direction vector of line
   pnear      O   Nearest point on ellipsoid to line
   dist       O   Distance of ellipsoid from line

-Detailed_Input

   a,
   b,
   c           are the lengths of the semi-axes of a triaxial
               ellipsoid which is centered at the origin and
               oriented so that its axes lie on the x-, y- and
               z- coordinate axes.  a, b, and c are the lengths of
               the semi-axes that point in the x, y, and z
               directions respectively.

   linept
   linedr      are, respectively, a point and a direction vector
               that define a line. The line is the set of vectors

                  linept   +   t * linedr

               where t is any real number.

-Detailed_Output

   pnear       is the point on the ellipsoid that is closest to
               the line, if the line doesn't intersect the
               ellipsoid.

               If the line intersects the ellipsoid, pnear will
               be a point of intersection. If linept is outside
               of the ellipsoid, pnear will be the closest point
               of intersection. If linept is inside the
               ellipsoid, pnear will not necessarily be the
               closest point of intersection.


   dist        is the distance of the line from the ellipsoid.
               This is the minimum distance between any point on
               the line and any point on the ellipsoid.

               If the line intersects the ellipsoid, dist is zero.

-Parameters

   None.

-Exceptions

   If this routine detects an error, the output arguments `pnear' and
   `dist' are not modified.

   1)  If the length of any semi-axis of the ellipsoid is
       non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled.

   2)  If the line's direction vector is the zero vector, the error
       SPICE(ZEROVECTOR) is signaled.

   3)  If the length of any semi-axis of the ellipsoid is zero after
       the semi-axis lengths are scaled by the reciprocal of the
       magnitude of the longest semi-axis and then squared, the error
       SPICE(DEGENERATECASE) is signaled.

   4)  If the input ellipsoid is extremely flat or needle-shaped
       and has its shortest axis close to perpendicular to the input
       line, numerical problems could cause this routine's algorithm
       to fail, in which case, the error SPICE(DEGENERATECASE) is
       signaled.

-Files

   None.

-Particulars

   For any ellipsoid and line, if the line does not intersect the
   ellipsoid, there is a unique point on the ellipsoid that is
   closest to the line. Therefore, the distance dist between
   ellipsoid and line is well-defined. The unique line segment of
   length dist that connects the line and ellipsoid is normal to
   both of these objects at its endpoints.

   If the line intersects the ellipsoid, the distance between the
   line and ellipsoid is zero.

-Examples

   1)   We can find the distance between an instrument optic axis ray
        and the surface of a body modelled as a tri-axial ellipsoid
        using this routine. If the instrument position and pointing
        unit vector in body-fixed coordinates are

           linept = ( 1.0e6,  2.0e6,  3.0e6 )

        and

           linedr = ( -4.472091234e-1
                      -8.944182469e-1,
                      -4.472091234e-3  )

        and the body semi-axes lengths are

           a = 7.0e5
           b = 7.0e5
           c = 6.0e5,

        then the call to npedln_c

           npedln_c ( a, b, c, linept, linedr, pnear, &dist );

        yields a value for pnear, the nearest point on the body to
        the optic axis ray, of

           (  -.16333110792340931E+04,
              -.32666222157820771E+04,
               .59999183350006724E+06  )

        and a value for dist, the distance to the ray, of

           .23899679338299707E+06

        (These results were obtained on a PC-Linux system under gcc.)

        In some cases, it may not be clear that the closest point
        on the line containing an instrument boresight ray is on
        the boresight ray itself; the point may lie on the ray
        having the same vertex as the boresight ray and pointing in
        the opposite direction. To rule out this possibility, we
        can make the following test:

           /.
           Find the difference vector between the closest point
           on the ellipsoid to the line containing the boresight
           ray and the boresight ray's vertex. Find the
           angular separation between this difference vector
           and the boresight ray. If the angular separation
           does not exceed pi/2, we have the nominal geometry.
           Otherwise, we have an error.
           ./

           vsub_c ( pnear,  linept,  diff );

           sep = vsep_c ( diff, linedr );

           if (  sep <= halfpi_c()  )
           {
              [ perform normal processing ]
           }
           else
           {
              [ handle error case ]
           }

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.1, 04-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.1.0, 01-JUN-2010 (NJB)

       Added touchd_ calls to tests for squared, scaled axis length
       underflow. This forces rounding to zero in certain cases where
       it otherwise might not occur due to use of extended registers.

   -CSPICE Version 1.0.1, 06-DEC-2002 (NJB)

       Outputs shown in header example have been corrected to
       be consistent with those produced by this routine.

   -CSPICE Version 1.0.0, 03-SEP-1999 (NJB)

-Index_Entries

   distance between line and ellipsoid
   distance between line of sight and body
   nearest point on ellipsoid to line

-&
*/

{ /* Begin npedln_c */



   /*
   Local variables
   */

   SpiceBoolean            found  [2];
   SpiceBoolean            ifound;
   SpiceBoolean            xfound;

   SpiceDouble             oppdir [3];
   SpiceDouble             mag;
   SpiceDouble             normal [3];
   SpiceDouble             prjpt  [3];
   SpiceDouble             prjnpt [3];
   SpiceDouble             pt     [2][3];
   SpiceDouble             scale;
   SpiceDouble             scla;
   SpiceDouble             scla2;
   SpiceDouble             sclb;
   SpiceDouble             sclb2;
   SpiceDouble             sclc;
   SpiceDouble             sclc2;
   SpiceDouble             sclpt  [3];
   SpiceDouble             udir   [3];

   SpiceEllipse            cand;
   SpiceEllipse            prjel;

   SpiceInt                i;

   SpicePlane              candpl;
   SpicePlane              prjpl;


   /*
   Static variables
   */


   /*
   Participate in error tracing.
   */

   chkin_c ( "npedln_c" );



   /*
   The algorithm used in this routine has two parts.  The first
   part handles the case where the input line and ellipsoid
   intersect.  Our procedure is simple in that case; we just
   call surfpt_c twice, passing it first one ray determined by the
   input line, then a ray pointing in the opposite direction.
   The second part of the algorithm handles the case where surfpt_c
   doesn't find an intersection.

   Finding the nearest point on the ellipsoid to the line, when the
   two do not intersect, is a matter of following four steps:

   1)  Find the points on the ellipsoid where the surface normal
       is normal to the line's direction.  This set of points is
       an ellipse centered at the origin.  The point we seek MUST
       lie on this `candidate' ellipse.

   2)  Project the candidate ellipse onto a plane that is normal
       to the line's direction.  This projection preserves
       distance from the line; the nearest point to the line on
       this new ellipse is the projection of the nearest point to
       the line on the candidate ellipse, and these two points are
       exactly the same distance from the line.  If computed using
       infinite-precision arithmetic, this projection would be
       guaranteed to be non-degenerate as long as the input
       ellipsoid were non-degenerate.  This can be verified by
       taking the inner product of the scaled normal to the candidate
       ellipse plane and the line's unitized direction vector
       (these vectors are called normal and udir in the code below);
       the inner product is strictly greater than 1 if the ellipsoid
       is non-degenerate.

   3)  The nearest point on the line to the projected ellipse will
       be contained in the plane onto which the projection is done;
       we find this point and then find the nearest point to it on
       the projected ellipse.  The distance between these two points
       is the distance between the line and the ellipsoid.

   4)  Finally, we find the point on the candidate ellipse that was
       projected to the nearest point to the line on the projected
       ellipse that was found in step 3.  This is the nearest point
       on the ellipsoid to the line.



                    Glossary of Geometric Variables


          a,
          b,
          c           Input ellipsoid's semi-axis lengths.

          point       Point of intersection of line and ellipsoid
                      if the intersection is non-empty.

          candpl      Plane containing candidate ellipse.

          normal      Normal vector to the candidate plane candpl.

          cand        Candidate ellipse.

          linept,
          linedr,     Point and direction vector on input line.

          udir        Unitized line direction vector.

          prjpl       Projection plane; the candidate ellipse is
                      projected onto this plane to yield prjel.

          prjel       Projection of the candidate ellipse cand onto
                      the projection plane prjel.

          prjpt       Projection of line point.

          prjnpt      Nearest point on projected ellipse to
                      projection of line point.

          pnear       Nearest point on ellipsoid to line.

   */



   /*
   We need a valid normal vector.
   */

   unorm_c ( linedr, udir, &mag );

   if ( mag == 0. )
   {
      setmsg_c( "Line direction vector is the zero vector. " );
      sigerr_c( "SPICE(ZEROVECTOR)"                          );
      chkout_c( "npedln_c"                                   );
      return;
   }


   if (         ( a <= 0. )
          ||    ( b <= 0. )
          ||    ( c <= 0. )   )
   {
      setmsg_c  ( "Semi-axis lengths: a = #,  b = #,  c = #."  );
      errdp_c   ( "#", a                                       );
      errdp_c   ( "#", b                                       );
      errdp_c   ( "#", c                                       );
      sigerr_c  ( "SPICE(INVALIDAXISLENGTH)"                   );
      chkout_c  ( "npedln_c"                                   );
      return;
   }


   /*
   Scale the semi-axes lengths for better numerical behavior.
   If squaring any one of the scaled lengths causes it to
   underflow to zero, we cannot continue the computation. Otherwise,
   scale the viewing point too.
   */

   scale  =  maxd_c ( 3, a, b, c );

   scla   =  a / scale;
   sclb   =  b / scale;
   sclc   =  c / scale;

   scla2  =  scla*scla;
   sclb2  =  sclb*sclb;
   sclc2  =  sclc*sclc;

   if (       ( (SpiceDouble)touchd_(&scla2)   ==   0. )
         ||   ( (SpiceDouble)touchd_(&sclb2)   ==   0. )
         ||   ( (SpiceDouble)touchd_(&sclc2)   ==   0. )   )
   {
      setmsg_c ( "Semi-axis too small:  a = #, b = #, c = #. " );
      errdp_c  ( "#", a                                        );
      errdp_c  ( "#", b                                        );
      errdp_c  ( "#", c                                        );
      sigerr_c ( "SPICE(DEGENERATECASE)"                       );
      chkout_c ( "npedln_c"                                    );
      return;
   }


   /*
   Scale linept.
   */
   sclpt[0]  =  linept[0] / scale;
   sclpt[1]  =  linept[1] / scale;
   sclpt[2]  =  linept[2] / scale;

   /*
   Hand off the intersection case to surfpt_c.  surfpt_c determines
   whether rays intersect a body, so we treat the line as a pair
   of rays.
   */

   vminus_c ( udir, oppdir );

   surfpt_c ( sclpt, udir,   scla, sclb, sclc, pt[0], &(found[0]) );
   surfpt_c ( sclpt, oppdir, scla, sclb, sclc, pt[1], &(found[1]) );

   for ( i = 0;  i < 2;  i++ )
   {
      if ( found[i] )
      {
         *dist  =  0.0;

         vequ_c   ( pt[i],  pnear         );
         vscl_c   ( scale,  pnear,  pnear );
         chkout_c ( "npedln_c"            );
         return;
      }
   }


   /*
   Getting here means the line doesn't intersect the ellipsoid.

   Find the candidate ellipse CAND.  NORMAL is a normal vector to
   the plane containing the candidate ellipse.   Mathematically the
   ellipse must exist, since it's the intersection of an ellipsoid
   centered at the origin and a plane containing the origin.  Only
   numerical problems can prevent the intersection from being found.
   */

   normal[0]  =  udir[0] / scla2;
   normal[1]  =  udir[1] / sclb2;
   normal[2]  =  udir[2] / sclc2;

   nvc2pl_c ( normal, 0., &candpl );

   inedpl_c ( scla, sclb, sclc, &candpl, &cand, &xfound );

   if ( !xfound )
   {
      setmsg_c ( "Candidate ellipse could not be found."  );
      sigerr_c ( "SPICE(DEGENERATECASE)"                  );
      chkout_c ( "npedln_c"                               );
      return;
   }

   /*
   Project the candidate ellipse onto a plane orthogonal to the
   line.  We'll call the plane prjpl and the projected ellipse prjel.
   */
   nvc2pl_c ( udir,   0.,     &prjpl );
   pjelpl_c ( &cand,  &prjpl, &prjel );


   /*
   Find the point on the line lying in the projection plane, and
   then find the near point PRJNPT on the projected ellipse.  Here
   PRJPT is the point on the line lying in the projection plane.
   The distance between PRJPT and PRJNPT is DIST.
   */

   vprjp_c  ( sclpt,   &prjpl,  prjpt         );
   npelpt_c ( prjpt,   &prjel,  prjnpt,  dist );


   /*
   Find the near point pnear on the ellipsoid by taking the inverse
   orthogonal projection of prjnpt; this is the point on the
   candidate ellipse that projects to prjnpt.  Note that the
   output dist was computed in step 3 and needs only to be re-scaled.

   The inverse projection of pnear ought to exist, but may not
   be calculable due to numerical problems (this can only happen
   when the input ellipsoid is extremely flat or needle-shaped).
   */

   vprjpi_c ( prjnpt, &prjpl, &candpl, pnear, &ifound );

   if ( !ifound )
   {
      setmsg_c ( "Inverse projection could not be found."  );
      sigerr_c ( "SPICE(DEGENERATECASE)"                   );
      chkout_c ( "npedln_c"                                );
      return;
   }

   /*
   Undo the scaling.
   */

   vscl_c ( scale,  pnear,  pnear );

   *dist *= scale;


   chkout_c ( "npedln_c" );

} /* End npedln_c */
