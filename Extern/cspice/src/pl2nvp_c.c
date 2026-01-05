/*

-Procedure pl2nvp_c ( Plane to normal vector and point )

-Abstract

   Return a unit normal vector and point that define a specified
   plane.

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
   #undef    pl2nvp_c


   void pl2nvp_c ( ConstSpicePlane   * plane,
                   SpiceDouble         normal[3],
                   SpiceDouble         point [3]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   plane      I   A SPICE plane.
   normal,
   point      O   A unit normal vector and point that define `plane'.

-Detailed_Input

   plane       is a SPICE plane.

-Detailed_Output

   normal,
   point       are, respectively, a unit normal vector and point
               that define the geometric plane represented by
               `plane'. Let the symbol < a, b > indicate the inner
               product of vectors `a' and `b'; then the geometric
               plane is the set of vectors `x' in three-dimensional
               space that satisfy

                  < x - point, normal >  =  0.

               `point' is always the closest point in the input
               plane to the origin. `point' is always a
               non-negative scalar multiple of `normal'.

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

   1)  Given a plane normal and constant, find a point in
       the plane. `point' is the point we seek.

          nvc2pl_c (  normal, const, &plane  );
          pl2nvp_c ( &plane, normal,  point  );

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

   plane to normal vector and point

-&
*/

{ /* Begin pl2nvp_c */


   /*
   Return the stored normal vector.
   */
   MOVED ( plane->normal, 3, normal );


   /*
   Find the closest point in the plane to the origin.
   */
   vscl_c ( plane->constant, plane->normal, point );


} /* End pl2nvp_c */
