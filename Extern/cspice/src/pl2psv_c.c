/*

-Procedure pl2psv_c ( Plane to point and spanning vectors )

-Abstract

   Return a point and two orthogonal spanning vectors that generate
   a specified plane.

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

   PLANES

-Keywords

   GEOMETRY
   MATH
   PLANE

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #undef    pl2psv_c


   void pl2psv_c ( ConstSpicePlane  * plane,
                   SpiceDouble        point[3],
                   SpiceDouble        span1[3],
                   SpiceDouble        span2[3]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   plane      I   A SPICE plane.
   point,
   span1,
   span2      O   A point in the input plane and two vectors
                  spanning the input plane.

-Detailed_Input

   plane       is a SPICE plane.

-Detailed_Output

   point,
   span1,
   span2       are, respectively, a point and two orthogonal spanning
               vectors that generate the geometric plane represented by
               `plane'. The geometric plane is the set of vectors

                  point   +   s * span1   +   t * span2

               where `s' and `t' are real numbers. `point' is the closest
               point in the plane to the origin; this point is always a
               multiple of the plane's normal vector. `span1' and `span2'
               are an orthonormal pair of vectors. `point', `span1', and
               `span2' are mutually orthogonal.

-Parameters

   None.

-Exceptions

   Error free.

   1)  The input plane MUST have been created by one of the CSPICE
       routines

          nvc2pl_c ( Normal vector and constant to plane )
          nvp2pl_c ( Normal vector and point to plane    )
          psv2pl_c ( Point and spanning vectors to plane )

       Otherwise, the results of this routine are unpredictable.

-Files

   None.

-Particulars

   CSPICE geometry routines that deal with planes use the `plane'
   data type to represent input and output planes. This data type
   makes the routine interfaces simpler and more uniform.

   The CSPICE routines that produce SPICE planes from data that
   define a plane are:

      nvc2pl_c ( Normal vector and constant to plane )
      nvp2pl_c ( Normal vector and point to plane    )
      psv2pl_c ( Point and spanning vectors to plane )

   The CSPICE routines that convert SPICE planes to data that
   define a plane are:

      pl2nvc_c ( Plane to normal vector and constant )
      pl2nvp_c ( Plane to normal vector and point    )
      pl2psv_c ( Plane to point and spanning vectors )

-Examples

   1) Project a vector `v' orthogonally onto a plane defined by
      `point', `span1', and `span2'. `proj' is the projection we want; it
      is the closest vector in the plane to `v'.

         psv2pl_c ( point,  span1, span2, &plane );
         vprjp_c  ( &v,    &plane, &proj         );


   2) Find the intersection of a plane and the unit sphere. This
      is a geometry problem that arises in computing the
      intersection of a plane and a triaxial ellipsoid. The
      CSPICE routine inedpl_c computes this intersection, but this
      example does illustrate how to use this routine.


         /.
         The geometric plane of interest will be represented
         by the SPICE plane plane in this example.

         The intersection circle will be represented by the
         vectors center, v1, and v2; the circle is the set
         of points

            center  +  cos(theta) v1  +  sin(theta) v2,

         where theta is in the interval (-pi, pi].

         The logical variable found indicates whether the
         intersection is non-empty.

         The center of the intersection circle will be the
         closest point in the plane to the origin. This
         point is returned by pl2psv_c. The distance of the
         center from the origin is the norm of center.
         ./

         pl2psv_c  ( &plane, center, span1, span2 );

         dist = vnorm_c ( center )


         /.
         The radius of the intersection circle will be

                 ____________
             _  /          2
              \/  1 - dist

         since the radius of the circle, the distance of the
         plane from the origin, and the radius of the sphere
         (1) are the lengths of the sides of a right triangle.

         ./

         found = ( dist <= 1.0 );

         if ( found )
         {
            radius = sqrt ( 1.0 - pow(dist,2) );

            vscl_c ( radius, span1, v1 );
            vscl_c ( radius, span2, v2 ) ;
         }



   3) Apply a linear transformation represented by the matrix `m' to
      a plane represented by the normal vector `n' and the constant C.
      Find a normal vector and constant for the transformed plane.

         /.
         Make a SPICE plane from n and c, and then find a
         point in the plane and spanning vectors for the
         plane.  n need not be a unit vector.
         ./
         nvc2pl_c ( n,       c,     &plane         );
         pl2psv_c ( &plane,  point,  span1,  span2 );


         /.
         Apply the linear transformation to the point and
         spanning vectors. All we need to do is multiply
         these vectors by m, since for any linear
         transformation T,

               T ( point  +  t1 * span1     +  t2 * span2 )

            =  T (point)  +  t1 * T(span1)  +  t2 * T(span2),

         which means that T(point), T(span1), and T(span2)
         are a point and spanning vectors for the transformed
         plane.
         ./

         mxv_c ( m, point, tpoint );
         mxv_c ( m, span1, tspan1 );
         mxv_c ( m, span2, tspan2 );

         /.
         Make a new SPICE plane tplane from the
         transformed point and spanning vectors, and find a
         unit normal and constant for this new plane.
         ./

         psv2pl_c ( tpoint,   tspan1,  tspan2,   &tplane );
         pl2nvc_c ( &tplane,  tn,      &tc               );

-Restrictions

   None.

-Literature_References

   [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
        7th Edition, Addison Wesley, 1988.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 24-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added example #1.

   -CSPICE Version 1.0.0, 05-MAR-1999 (NJB)

-Index_Entries

   plane to point and spanning vectors

-&
*/

{ /* Begin pl2psv_c */


   /*
   Local variables
   */
   SpiceDouble             normal[3];


   /*
   This is an error-free function; no check-in is required.
   */

   /*
   Find the closest point in the plane to the origin.
   */
   vscl_c ( plane->constant, plane->normal, point );


   /*
   Next, find an orthogonal pair of vectors that are also orthogonal to
   the plane's normal vector.  The CSPICE routine frame_c does this for
   us.  normal, span1, and span2 form a right-handed orthonormal system
   upon output from frame_c.
   */

   MOVED ( plane->normal, 3, normal );

   frame_c ( normal, span1, span2 );


} /* End pl2psv_c */
