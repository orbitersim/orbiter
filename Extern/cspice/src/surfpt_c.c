/*

-Procedure surfpt_c ( Surface point on an ellipsoid )

-Abstract

   Determine the intersection of a line-of-sight vector with the
   surface of an ellipsoid.

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
   INTERSECTION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef    surfpt_c


   void surfpt_c ( ConstSpiceDouble   positn[3],
                   ConstSpiceDouble   u[3],
                   SpiceDouble        a,
                   SpiceDouble        b,
                   SpiceDouble        c,
                   SpiceDouble        point[3],
                   SpiceBoolean     * found     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   positn     I   Position of the observer in body-fixed frame.
   u          I   Vector from the observer in some direction.
   a          I   Length of the ellipsoid semi-axis along the X-axis.
   b          I   Length of the ellipsoid semi-axis along the Y-axis.
   c          I   Length of the ellipsoid semi-axis along the Z-axis.
   point      O   Point on the ellipsoid pointed to by `u'.
   found      O   Flag indicating if `u' points at the ellipsoid.

-Detailed_Input

   positn      3-vector giving the position of an observer with
               respect to the center of an ellipsoid. The vector is
               expressed in a body-fixed reference frame. The
               semi-axes of the ellipsoid are aligned with the x, y,
               and z-axes of the body-fixed frame.

   u           Pointing vector emanating from the observer.

   a           Length of the semi-axis of the ellipsoid that is
               parallel to the X-axis of the body-fixed reference
               frame.

   b           Length of the semi-axis of the ellipsoid that is
               parallel to the Y-axis of the body-fixed reference
               frame.

   c           Length of the semi-axis of the ellipsoid that is
               parallel to the Z-axis of the body-fixed reference
               frame.

-Detailed_Output

   point       If the ray with direction vector `u' emanating from
               `positn' intersects the ellipsoid, `point' will be
               returned with the body-fixed coordinates of the point
               where the ray first meets the ellipsoid. Otherwise,
               `point' will be returned as (0, 0, 0).

   found       A logical flag indicating whether or not the ray from
               `positn' with direction `u' actually intersects the
               ellipsoid. If the ray does intersect the ellipsoid,
               `found' will be returned as SPICETRUE. If the ray misses the
               ellipsoid, `found' will be returned as SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If the input vector is the zero vector, the error
       SPICE(ZEROVECTOR) is signaled by a routine in the call tree of
       this routine.

   2)  If any of the body's axes is zero, the error
       SPICE(BADAXISLENGTH) is signaled by a routine in the call tree
       of this routine.

-Files

   None.

-Particulars

   This routine assumes that an ellipsoid having semi-axes of length a,
   b and c is given. Moreover, it is assumed that these axes are
   parallel to the x-, y-, and z-axes of a coordinate system whose
   origin is the geometric center of the ellipsoid---this is called the
   body-fixed coordinate frame.

-Examples

   A typical use of surfpt_c would be to obtain the planetocentric
   coordinates of the point at which the optic axis of a
   spacecraft-mounted instrument intersects the surface of a target
   body, given the following items.

      1) The epoch (et) of observation, and the inertial
         pointing (vpnt) of the instrument at this epoch.

      2) The apparent position (vtarg) of the center of the
         target body as seen from the spacecraft at the epoch
         of observation, and the one-way light time (tau)
         from the target to the spacecraft.

   In order to find the point of intersection, the following items are
   also needed.

      3) The transformation (tibf) from inertial
         to body-fixed coordinates at epoch et-tau.

      4) The radii (r) of the tri-axial ellipsoid
         used to model the target body.

   These may be obtained from the kernel pool via calls to pxform_c and
   bodvrd_c or bodvcd_c respectively.

   The position of the observer is just the negative of the
   spacecraft-target vector, vtarg, computed using the vminus_c module.
   (Note that this is NOT the same as the apparent position of the
   spacecraft as seen from the target!) Both vectors must be in
   body-fixed coordinates. The point of intersection is found as
   follows.

      vminus_c ( vtarg, vpos );
      mxv_c    ( tibf,  vpos,  vpos );
      mxv_c    ( tibf,  vpnt,  vpnt );

      surfpt_c ( vpos, vpnt, r[0], r[1], r[2], vsurf, &found );

   Note that vsurf may or may not be a point of intersection, depending
   on whether found is SPICETRUE or SPICEFALSE. Note also that vsurf is
   a vector from the center to the surface of the target, in body-fixed
   coordinates, which may be converted directly to planetocentric
   latitude, longitude, and radius:

     reclat_c ( vsurf, &radius, &lon, &lat );

   To get the inertial vector from the spacecraft to the surface point,
   you must subtract vpos from vsurf, and rotate the resulting vector
   back to inertial coordinates:

      vsub_c ( vsurf, vpos,  vsurf );
      mtxv_c ( tibf,  vsurf, vsurf );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   C.H. Acton          (JPL)
   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.4.4, 05-AUG-2021 (NJB) (JDR)

       Edited the header to comply with NAIF standard.

       Changed variable name `long' to `lon' in header code fragment.

   -CSPICE Version 1.4.3, 24-OCT-2005 (NJB)

       Header update: reference to bodvar_c was replaced with
       references to bodvrd_c and bodvcd_c.

   -CSPICE Version 1.4.2, 27-JUL-2003 (NJB) (CHA)

       Various header corrections were made.

   -CSPICE Version 1.4.1, 28-NOV-2002 (NJB)

       Made miscellaneous updates to header comments.

   -CSPICE Version 1.4.0, 27-AUG-1999 (NJB)

       Now uses local "found" flag of type logical.

   -CSPICE Version 1.3.0, 22-OCT-1998 (NJB)

       Made input vectors const.

   -CSPICE Version 1.2.0, 08-FEB-1998 (NJB)

       Removed local variables used for temporary capture of outputs.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB) (WLT)

       Based on SPICELIB Version 1.1.0, 07-AUG-1996 (WLT)

-Index_Entries

   line of sight intercept with body
   point of intersection between ray and ellipsoid
   surface point of intersection of ray and ellipsoid

-&
*/

{ /* Begin surfpt_c */

   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "surfpt_c");


   /*
   Call the f2c'd surfpt.
   */
   surfpt_( (doublereal *)  positn,
            (doublereal *)  u,
            (doublereal *)  &a,
            (doublereal *)  &b,
            (doublereal *)  &c,
            (doublereal *)  point,
            (logical    *)  &fnd  );

   *found = fnd;

   chkout_c ( "surfpt_c");

} /* End surfpt_c */
