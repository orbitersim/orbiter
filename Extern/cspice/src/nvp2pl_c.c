/*

-Procedure nvp2pl_c ( Normal vector and point to plane )

-Abstract

   Make a SPICE plane from a normal vector and a point.

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
   #undef    nvp2pl_c


   void nvp2pl_c ( ConstSpiceDouble    normal[3],
                   ConstSpiceDouble    point [3],
                   SpicePlane        * plane     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   normal,
   point      I   A normal vector and a point defining a plane.
   plane      O   A SPICE plane structure representing the plane.

-Detailed_Input

   normal,
   point
                  are, respectively, a normal vector and point that
                  define a plane in three-dimensional space. normal
                  need not be a unit vector. Let the symbol < a, b >
                  indicate the inner product of vectors a and b;
                  then the geometric plane is the set of vectors x
                  in three-dimensional space that satisfy

                     < x - point, normal >  =  0.

-Detailed_Output

   plane       is a SPICE plane structure that represents the
               geometric plane defined by point and normal.

-Parameters

   None.

-Exceptions

   1)  If the input vector `normal' is the zero vector, the error
       SPICE(ZEROVECTOR) is signaled.

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

   Any of these last three routines may be used to convert this
   routine's output, `plane', to another representation of a
   geometric plane.

-Examples

   1)  Project a vector v orthogonally onto a plane defined by point
       and normal. proj is the projection we want; it is the
       closest vector in the plane to v.

          nvp2pl_c ( normal,  point,   &plane );
          vprjp_c  ( v,       &plane,  proj   );


   2)  Given a point in a plane and a normal vector, find the
       distance of the plane from the origin. We make a
       `plane' from the point and normal, then convert the
       plane to a unit normal and constant. The output constant
       is (according to the specification of pl2nvc_c) the distance of
       the plane from the origin.

          nvp2pl_c ( normal,  point,  &plane   );
          pl2nvc_c ( &plane,  normal, constant );

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

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 05-MAR-1999 (NJB)

-Index_Entries

   normal vector and point to plane

-&
*/

{ /* Begin nvp2pl_c */


   /*
   This routine checks in only if an error is discovered.
   */

   if ( return_c() )
   {
      return;
   }


   /*
   The normal vector must be non-zero.
   */

   if ( vzero_c (normal) )
   {
      chkin_c  ( "nvp2pl_c"                         );
      setmsg_c ( "Plane's normal must be non-zero." );
      sigerr_c ( "SPICE(ZEROVECTOR)"                );
      chkout_c ( "nvp2pl_c"                         );
      return;
   }


   vhat_c ( normal, plane->normal );

   plane->constant  =  vdot_c ( point, plane->normal );


   /*
   The constant should be the distance of the plane from the
   origin.  If the constant is negative, negate both it and the
   normal vector.
   */

   if ( plane->constant  <  0. )
   {
      plane->constant  =   - (plane->constant);

      vminus_c ( plane->normal, plane->normal );
   }


} /* End nvp2pl_c */
