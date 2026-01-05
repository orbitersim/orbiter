/*

-Procedure inelpl_c ( Intersection of ellipse and plane )

-Abstract

   Find the intersection of an ellipse and a plane.

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
   PLANES

-Keywords

   ELLIPSE
   GEOMETRY
   MATH

*/
   #include <math.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZim.h"
   #undef   inelpl_c


   void inelpl_c ( ConstSpiceEllipse  * ellips,
                   ConstSpicePlane    * plane,
                   SpiceInt           * nxpts,
                   SpiceDouble          xpt1[3],
                   SpiceDouble          xpt2[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   ellips     I   A SPICE ellipse.
   plane      I   A SPICE plane.
   nxpts      O   Number of intersection points of plane and ellipse.
   xpt1,
   xpt2       O   Intersection points.

-Detailed_Input

   ellips      is a SPICE ellipse. The ellipse is allowed to
               be degenerate: one or both semi-axes may have
               zero length.

   plane       is a SPICE plane. The intersection of plane
               and ellipse is sought.

-Detailed_Output

   nxpts       is the number of points of intersection of the
               geometric plane and ellipse represented by `plane' and
               `ellips'. `nxpts' may take the values 0, 1, 2 or -1.
               The value -1 indicates that the ellipse consists of
               more than one point lies in the plane, so the number
               of intersection points is infinite.

               When the ellipse consists of a single point and
               lies in the plane, `nxpts' is set to 1.

   xpt1,
   xpt2        are the points of intersection of the input plane
               and ellipse. If there is only one intersection
               point, both xpt1 and xpt2 contain that point. If
               the number of intersection points is zero or
               infinite, the contents of xpt1 and xpt2 are
               undefined.

-Parameters

   None.

-Exceptions

   1)  If the input plane is invalid, the error SPICE(INVALIDPLANE)
       is signaled. The input plane must be a SPICE plane: the normal
       vector must be non-zero and the constant must be non-negative.

   2)  If the input ellipse has non-orthogonal axes, the error
       SPICE(INVALIDELLIPSE) is signaled.

   3)  The input ellipse is allowed to be a line segment or a point;
       these cases are not considered to be errors. If the ellipse
       consists of a single point and lies in the plane, the number
       of intersection points is set to 1 (rather than -1) and
       the output arguments `xpt1' and `xpt2' are assigned the value
       of the ellipse's center.

-Files

   None.

-Particulars

   This routine computes the intersection set of a non-degenerate
   plane with a possibly degenerate ellipse. The ellipse is allowed
   to consist of a line segment or a point.

   A plane may intersect an ellipse in 0, 1, 2, or infinitely many
   points. For there to be an infinite set of intersection points,
   the ellipse must lie in the plane and consist of more than one

-Examples

   1)  If we want to find the angle of some ray above the limb of an
       ellipsoid, where the angle is measured in a plane containing
       the ray and a "down" vector, we can follow the procedure
       given below. We assume the ray does not intersect the
       ellipsoid. The result we seek is called angle, imaginatively
       enough.

       We assume that all vectors are given in body-fixed
       coordinates.

          #include "SpiceUsr.h"
              .
              .
              .
     /.
     Find the limb of the ellipsoid as seen from the
     point observ. Here a, b, and c are the lengths of
     the semi-axes of the ellipsoid. The limb is
     returned as a SpiceEllipse.
     ./

     edlimb_c ( a, b, c, observ, &limb );

     /.
     The ray direction vector is raydir, so the ray is the
     set of points

        observ  +  t * raydir

     where t is any non-negative real number.

     The `down' vector is just -observ. The vectors
     observ and raydir are spanning vectors for the plane
     we're interested in. We can use psv2pl_c to represent
     this plane by a SPICE plane.
     ./
     psv2pl_c ( observ, observ, raydir, &plane );

     /.
     Find the intersection of the plane defined by observ
     and raydir with the limb.
     ./
     inelpl_c ( limb, plane, nxpts, xpt1, xpt2 );

     /.
     We always expect two intersection points, if the vector
     down is valid.
     ./
     if ( nxpts < 2 )
     {
        [ do something about the error ]
     }

     /.
     Form the vectors from observ to the intersection
     points. Find the angular separation between the
     boresight ray and each vector from observ to the
     intersection points.
     ./
     vsub_c   ( xpt1, observ, vec1 );
     vsub_c   ( xpt2, observ, vec2 );

     sep1 = vsep_c ( vec1, raydir );
     sep2 = vsep_c ( vec2, raydir );

     /.
     The angular separation we're after is the minimum of
     the two separations we've computed.
     ./
     angle = mind_c ( 2, sep1, sep2 );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 2.1.1, 24-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Removed non-standard header section "Revisions", moving its
       contents to the relevant entry in -Version section.

   -CSPICE Version 2.1.0, 07-OCT-2011 (NJB)

       Relaxed ellipse semi-axes orthogonality test limit
       SEPLIM from 1.D-12 TO 1.D-9 radians. The angular
       separation of the axes of the input ellipse must not
       differ from pi/2 radians by more than this limit.

   -CSPICE Version 2.0.0, 14-JAN-2008 (NJB)

       Bug fix: the routine's specification and behavior have been
       updated so the routine now returns a meaningful result for the
       case of an ellipse consisting of a single point. In this case,
       if an intersection is found, the number of intersection points
       is set to 1 and both intersection arguments are set equal to
       the ellipse's center.

       Bug fix: in the degenerate case where the input ellipse is a
       line segment of positive length, and this segment intersects
       the plane, the number of intersection points is set to 1
       rather than 2.

       Invalid input planes and ellipses are now diagnosed.
       Error handling code has been added to trap errors that had
       been erroneously passed off to lower level routines for
       diagnosis.

   -CSPICE Version 1.0.0, 28-AUG-2001 (NJB)

-Index_Entries

   intersection of ellipse and plane

-&
*/

{ /* Begin inelpl_c */


   /*
   Local constants
   */
   #define SEPLIM          ( 1.0e-9 )

   /*
   Local variables
   */
   SpiceDouble             alpha;
   SpiceDouble             angle1;
   SpiceDouble             angle2;
   SpiceDouble             beta;
   SpiceDouble             center [3];
   SpiceDouble             constant;
   SpiceDouble             inpcon;
   SpiceDouble             normal [3];
   SpiceDouble             point  [3];
   SpiceDouble             sep;
   SpiceDouble             smajor [3];
   SpiceDouble             sminor [3];
   SpiceDouble             v      [2];

   SpicePlane              trans;



   /*
   Participate in error tracing.
   */
   chkin_c ( "inelpl_c" );


   /*
   Check the input plane.
   */
   pl2nvc_c ( plane, normal, &inpcon );

   if ( vzero_c(normal) )
   {
      setmsg_c ( "Input SPICE plane has zero normal vector." );
      sigerr_c ( "SPICE(INVALIDPLANE)"                       );
      chkout_c ( "inelpl_c"                                  );
      return;
   }
   else if ( inpcon < 0.0 )
   {
      setmsg_c ( "Input SPICE plane has non-positive "
                 "constant #. Properly constructed "
                 "SPICE planes always have non-negative "
                 "constants."                                );
      errdp_c  ( "#",  inpcon                                );
      sigerr_c ( "SPICE(INVALIDPLANE)"                       );
      chkout_c ( "inelpl_c"                                  );
      return;
   }

   /*
   Get the components of the input ellipse; check for
   invalid semi-axes. The semi-axes may have zero length
   but they must always be orthogonal. We require this
   check only if both semi-axes have non-zero length.
   */
   el2cgv_c ( ellips, center, smajor, sminor );

   if ( !vzero_c(sminor) )
   {
      sep = vsep_c( smajor, sminor );

      if (  fabs( sep-halfpi_c() )  >  SEPLIM  )
      {
         setmsg_c ( "Input SPICE ellipse has non-orthogonal "
                    "semi-axes: (#,#,#) and (#,#,#). Angular "
                    "separation of these vectors is # radians. "
                    "Properly constructed SPICE ellipses "
                    "always have orthogonal semi-axes."         );
         errdp_c  ( "#",  smajor[0]                             );
         errdp_c  ( "#",  smajor[1]                             );
         errdp_c  ( "#",  smajor[2]                             );
         errdp_c  ( "#",  sminor[0]                             );
         errdp_c  ( "#",  sminor[1]                             );
         errdp_c  ( "#",  sminor[2]                             );
         errdp_c  ( "#",  sep                                   );
         sigerr_c ( "SPICE(INVALIDELLIPSE)"                     );
         chkout_c ( "inelpl_c"                                  );
         return;
      }
   }

   /*
   If the input ellipse is a single point, decide now
   whether the ellipse lies in the plane.
   */

   if ( vzero_c(smajor) )
   {
      /*
      The ellipse is a single point. If the ellipse's center
      lies in the plane, the whole ellipse is the one
      intersection point. Check the inner product of the
      center and the plane's normal vector.
      */

      if (  vdot_c(center, normal)  ==  inpcon  )
      {
         /*
         The center does in fact lie in the plane.
         */

         *nxpts = 1;

         vequ_c ( center, xpt1 );
         vequ_c ( center, xpt2 );
      }
      else
      {
         /*
         There's no intersection: the intersection arguments
         are left undefined in this case.
         */

         *nxpts = 0;
      }

      /*
      Return now; this simplifies the logic to follow.
      */
      chkout_c ( "inelpl_c" );
      return;
   }

   /*
   At this point the ellipse may still be degenerate: it can be a
   line segment. We'll need to compute the intersection point or
   points if we have a positive, finite intersection set.

   The first thing we want to do is translate the plane and the
   ellipse so as to center the ellipse at the origin.  To translate
   the plane, just get a point and normal vector, and translate
   the point.  Find the plane constant of the translated plane.
   */
   pl2nvp_c ( plane,   normal,  point     );
   vsub_c   ( point,   center,  point     );
   nvp2pl_c ( normal,  point,   &trans    );
   pl2nvc_c ( &trans,  normal,  &constant );

   /*
   Ok, we can get to work.  The locus of the ellipse is

      cos(theta) smajor  +  sin(theta) sminor,

   and any point x of the ellipse that intersects the input plane
   satisfies

      < x, normal >  =  constant.

   Substituting our expression for points on the ellipse into the
   second equation, we arrive at

         cos(theta) < smajor, normal >
      +  sin(theta) < sminor, normal >   =  constant.        (1)

   This equation merits a little analysis.  First, if `normal'
   is orthogonal to `smajor' and `sminor, the plane and ellipse must
   be parallel. Also, the left side of the equation is zero in
   this case. If `constant' is non-zero, there are no solutions:
   the ellipse and plane are parallel but do not intersect. If
   `constant' is zero, the ellipse lies in the plane: all values of
   theta are solutions. Let's get this case out of the way
   right now, shall we?
   */
   v[0] = vdot_c ( smajor, normal );
   v[1] = vdot_c ( sminor, normal );

   /*
   Test whether the plane and ellipse are parallel.
   */
   if (  vzerog_c( v, 2 )  )
   {
      /*
      The ellipse lies in the plane if and only if constant is zero.
      In any case, we don't modify xpt1 or xpt2.
      */
      if ( constant == 0.0 )
      {
         *nxpts = -1;
      }
      else
      {
         *nxpts = 0;
      }

      chkout_c ( "inelpl_c" );
      return;
   }


   /*
   Now if `normal' is not orthogonal to both `smajor' and `sminor',
   the vector

      v = (  < smajor, normal >,  < sminor, normal >  )

   is non-zero.  We can re-write (1) as

      < u, v >  =  constant,

   where

      u = ( cos(theta), sin(theta) ).

   If alpha is the angle between u and v, we have

      < u, v >  =  || u ||  *  || v ||  *  cos(alpha),

   so

      || v ||  *  cos(alpha)  =  constant.                (2)

   `constant' is positive, since pl2nvc_c returns the distance
   between its input plane and the origin as the output
   plane constant.

   Equation (2) has solutions if and only if

      || v ||  >    constant.                             (3)
               -


   Let's return right now if there are no solutions.
   */
   if ( vnormg_c ( v, 2 )  < constant )
   {
      *nxpts = 0;

      chkout_c ( "inelpl_c" );
      return;
   }


   /*
   Since (3) above is satisfied, the plane and ellipse intersect.
   We can find alpha by the formula

      alpha  =  +  arccos (  constant  /  || v ||  )

   Since `alpha' is the angular separation between `u' and `v', we
   can find `u' once we have the angular position of `v'; let's
   call that `beta'.  The angular position of `u'(which we called
   `theta' earlier) will be

      theta   =   beta  +  alpha.
                        -

   The values of `theta' are the angles we seek.
   */
   alpha   =  acos  (  constant  /  vnormg_c ( v, 2 )  );

   beta    =  atan2 ( v[1], v[0] );

   angle1  =  beta - alpha;
   angle2  =  beta + alpha;

   /*
   Determine the number of intersection points. We have a special
   case if the semi-minor axis has length zero: in that case `beta' is
   zero or Pi, and although `angle1' and `angle2' may differ, the
   cosines of these angles are identical. Since in this case
   the solutions corresponding to `angle1' and `angle2' have the
   form

      center + cos(angle1)*smajor
      center + cos(angle2)*smajor

   the solutions are identical.
   */

   if ( vzero_c(sminor) )
   {
      *nxpts = 1;
   }
   else
   {
      if ( angle1 == angle2 )
      {
         /*
         This case occurs when `alpha' is zero.
         */

         *nxpts = 1;
      }
      else
      {
         *nxpts = 2;
      }
   }

   /*
   Compute the intersection points.
   */
   vlcom3_c ( 1.0,          center,
              cos(angle1),  smajor,
              sin(angle1),  sminor,   xpt1 );

   vlcom3_c ( 1.0,          center,
              cos(angle2),  smajor,
              sin(angle2),  sminor,   xpt2 );

   chkout_c ( "inelpl_c" );

} /* End inelpl_c */
