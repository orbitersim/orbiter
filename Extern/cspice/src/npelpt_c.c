/*

-Procedure npelpt_c  ( Nearest point on ellipse to point )

-Abstract

   Find the nearest point on an ellipse to a specified point, both
   in three-dimensional space, and find the distance between the
   ellipse and the point.

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

   CONIC
   ELLIPSE
   GEOMETRY
   MATH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    npelpt_c


   void npelpt_c ( ConstSpiceDouble      point  [3],
                   ConstSpiceEllipse   * ellips,
                   SpiceDouble           pnear  [3],
                   SpiceDouble         * dist       )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   point      I   Point whose distance to an ellipse is to be found.
   ellips     I   A SPICE ellipse.
   pnear      O   Nearest point on ellipse to input point.
   dist       O   Distance of input point to ellipse.

-Detailed_Input

   point       is a point in 3-dimensional space.

   ellips      is a SPICE ellipse that represents an ellipse
               in three-dimensional space.

-Detailed_Output

   pnear       is the nearest point on `ellips' to `point'.

   dist        is the distance between `point' and `pnear'. This is
               the distance between `point' and the ellipse.

-Parameters

   None.

-Exceptions

   1)  If the input ellipse `ellips' has one or both semi-axis lengths
       equal to zero, the error SPICE(DEGENERATECASE) is signaled.

   2)  If the geometric ellipse represented by `ellips' does not
       have a unique point nearest to the input point, any point
       at which the minimum distance is attained may be returned
       in `pnear'.

   3)  If a ratio of non-zero ellipse radii violates the constraints
       imposed by nearpt_c, an error is signaled by a routine in the
       call tree of this routine.

   4)  The routine does not check for overflow when scaling or
       translating the input point.

-Files

   None.

-Particulars

   Given an ellipse and a point in 3-dimensional space, if the
   orthogonal projection of the point onto the plane of the ellipse
   is on or outside of the ellipse, then there is a unique point on
   the ellipse closest to the original point. This routine finds
   that nearest point on the ellipse. If the projection falls inside
   the ellipse, there may be multiple points on the ellipse that are
   at the minimum distance from the original point. In this case,
   one such closest point will be returned.

   This routine returns a distance, rather than an altitude, in
   contrast to the CSPICE routine nearpt_c. Because our ellipse is
   situated in 3-space and not 2-space, the input point is not
   "inside" or "outside" the ellipse, so the notion of altitude does
   not apply to the problem solved by this routine. In the case of
   nearpt_c, the input point is on, inside, or outside the ellipsoid,
   so it makes sense to speak of its altitude.

-Examples

   1)  For planetary rings that can be modeled as flat disks with
       elliptical outer boundaries, the distance of a point in
       space from a ring's outer boundary can be computed using this
       routine. Suppose `center', `smajor', and `sminor' are the center,
       semi-major axis, and semi-minor axis of the ring's boundary.
       Suppose also that `scpos' is the position of a spacecraft.
       `scpos', `center', `smajor', and `sminor' must all be expressed in
       the same coordinate system. We can find the distance from
       the spacecraft to the ring using the code fragment

          #include "SpiceUsr.h"
               .
               .
               .
          /.
          Make a SPICE ellipse representing the ring,
          then use npelpt_c to find the distance between
          the spacecraft position and RING.
          ./
          cgv2el_c ( center, smajor, sminor,  ring );
          npelpt_c ( scpos,  ring,   pnear,  &dist );



   2)  The problem of finding the distance of a line from a tri-axial
       ellipsoid can be reduced to the problem of finding the
       distance between the same line and an ellipse; this problem in
       turn can be reduced to the problem of finding the distance
       between an ellipse and a point. The routine npedln_c carries
       out this process and uses npelpt_c to find the ellipse-to-point
       distance.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 24-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 02-SEP-1999 (NJB)

-Index_Entries

   nearest point on ellipse to point

-&
*/

{ /* Begin npelpt_c */


   /*
   Local variables
   */

   SpiceDouble             center [3];
   SpiceDouble             majlen;
   SpiceDouble             minlen;
   SpiceDouble             rotate [3][3];
   SpiceDouble             scale;
   SpiceDouble             smajor [3];
   SpiceDouble             sminor [3];
   SpiceDouble             tmppnt [3];
   SpiceDouble             prjpnt [3];


   /*
   Participate in error tracing.
   */
   chkin_c ( "npelpt_c" );


   /*
   Here's an overview of our solution:

      Let ELPL be the plane containing the ELLIPS, and let PRJ be
      the orthogonal projection of the POINT onto ELPL.  Let X be
      any point in the plane ELPL.  According to the Pythagorean
      Theorem,

                         2                       2                  2
         || POINT - X ||    =   || POINT - PRJ ||   +  || PRJ - X ||.

      Then if we can find a point X on ELLIPS that minimizes the
      rightmost term, that point X is the closest point on the
      ellipse to POINT.

      So, we find the projection PRJ, and then solve the problem of
      finding the closest point on ELLIPS to PRJ.  To solve this
      problem, we find a triaxial ellipsoid whose intersection with
      the plane ELPL is precisely ELLIPS, and two of whose axes lie
      in the plane ELPL.  The closest point on ELLIPS to PRJ is also
      the closest point on the ellipsoid to ELLIPS.  But we have the
      SPICELIB routine NEARPT on hand to find the closest point on an
      ellipsoid to a specified point, so we've reduced our problem to
      a solved problem.

      There is a subtle point to worry about here:  if PRJ is outside
      of ELLIPS (PRJ is in the same plane as ELLIPS, so `outside'
      does make sense here), then the closest point on ELLIPS to PRJ
      coincides with the closest point on the ellipsoid to PRJ,
      regardless of how we choose the z-semi-axis length of the
      ellipsoid.  But the correspondence may be lost if PRJ is inside
      the ellipse, if we don't choose the z-semi-axis length
      correctly.

      Though it takes some thought to verify this (and we won't prove
      it here), making the z-semi-axis of the ellipsoid longer than
      the other two semi-axes is sufficient to maintain the
      coincidence of the closest point on the ellipsoid to PRJPNT and
      the closest point on the ellipse to PRJPNT.
   */


   /*
   Find the ellipse's center and semi-axes.
   */
   el2cgv_c ( ellips, center, smajor, sminor );


   /*
   Find the lengths of the semi-axes, and scale the vectors to try
   to prevent arithmetic unpleasantness.  Degenerate ellipses are
   turned away at the door.
   */

   minlen = vnorm_c (sminor);
   majlen = vnorm_c (smajor);

   if (   MinVal ( majlen, minlen )  ==  0.0  )
   {
      setmsg_c ( "Ellipse semi-axis lengths: # #." );
      errdp_c  ( "#", majlen                       );
      errdp_c  ( "#", minlen                       );
      sigerr_c ( "SPICE(DEGENERATECASE)"           );
      chkout_c ( "npelpt_c"                        );
      return;
   }


   scale = 1.0 / majlen;

   vscl_c ( scale, smajor, smajor );
   vscl_c ( scale, sminor, sminor );


   /*
   Translate ellipse and point so that the ellipse is centered at
   the origin.  Scale the point's coordinates to maintain the
   correct relative position to the scaled ellipse.
   */
   vsub_c ( point, center, tmppnt );
   vscl_c ( scale, tmppnt, tmppnt );


   /*
   We want to reduce the problem to a two-dimensional one.  We'll
   work in a coordinate system whose x- and y- axes are aligned with
   the semi-major and semi-minor axes of the input ellipse.  The
   z-axis is picked to give us a right-handed system.  We find the
   matrix that transforms coordinates to our new system using twovec_c.
   */
   twovec_c ( smajor, 1, sminor, 2, rotate );


   /*
   Apply the coordinate transformation to our scaled input point.
   */
   mxv_c ( rotate, tmppnt, tmppnt );


   /*
   We must find the distance between the orthogonal projection of
   tmppnt onto the x-y plane and the ellipse.  The projection is
   just

      ( TMPPNT[0], TMPPNT[1], 0 );

   we'll call this projection prjpnt.
   */

   vpack_c ( tmppnt[0],  tmppnt[1],  0.0,  prjpnt );


   /*
   Now we're ready to find the distance between and a triaxial
   ellipsoid whose intersection with the x-y plane is the ellipse
   and whose third semi-axis lies on the z-axis.

   Because we've scaled the ellipse's axes so as to give the longer
   axis length 1, a length of 2.0 suffices for the ellipsoid's
   z-semi-axis.

   Find the nearest point to prjpnt on the ellipsoid, pnear.
   */
   nearpt_c ( prjpnt, 1.0, minlen/majlen, 2.0, pnear, dist );


   /*
   Scale the near point coordinates back to the original scale.
   */
   vscl_c ( majlen, pnear, pnear );


   /*
   Apply the required inverse rotation and translation to pnear.
   Compute the point-to-ellipse distance.
   */
   mtxv_c ( rotate, pnear,  pnear );
   vadd_c ( pnear,  center, pnear );

   *dist = vdist_c ( pnear, point );


   chkout_c ( "npelpt_c" );

} /* End npelpt_c */
