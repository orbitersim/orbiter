/*

-Procedure pl2nvc_c ( Plane to normal vector and constant )

-Abstract

   Return a unit normal vector and constant that define a specified
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
   #undef    pl2nvc_c


   void pl2nvc_c ( ConstSpicePlane   * plane,
                   SpiceDouble         normal[3],
                   SpiceDouble       * konst     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   plane      I   A SPICE plane.
   normal,
   konst      O   A normal vector and constant defining the
                  geometric plane represented by `plane'.

-Detailed_Input

   plane       is a SPICE plane.

-Detailed_Output

   normal,
   konst       are, respectively, a unit normal vector and
               constant that define the geometric plane
               represented by `plane'. Let the symbol < a, b >
               indicate the inner product of vectors `a' and `b';
               then the geometric plane is the set of vectors `x'
               in three-dimensional space that satisfy

                  < x,  normal >  =  konst.

               `normal' is a unit vector. `konst' is the distance of
               the plane from the origin;

                  konst * normal

               is the closest point in the plane to the origin.

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

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Determine the distance of a plane from the origin, and
      confirm the result by calculating the dot product (inner
      product) of a vector from the origin to the plane and a
      vector in that plane.

      The dot product between these two vectors should be zero,
      to double precision round-off, so orthogonal to that
      precision.


      Example code begins here.


      /.
         Program pl2nvc_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {


         SpiceDouble          konst;
         SpicePlane           plane;
         SpiceDouble          plnvec [3];
         SpiceDouble          vec    [3];

         /.
         Define the plane with a vector normal to the plan
         and a point in the plane.
         ./
         SpiceDouble          normal [3] = { -1.0,  5.0,   -3.5 };
         SpiceDouble          point  [3] = { 9.0, -0.65, -12.0 };

         /.
         Create the SPICE plane from the normal and point.
         ./
         nvp2pl_c ( normal, point, &plane );

         /.
         Calculate the normal vector and constant defining
         the plane. The constant value is the distance from
         the origin to the plane.
         ./
         pl2nvc_c ( &plane, normal, &konst );
         printf( "Distance to the plane: %11.7f\n", konst );

         /.
         Confirm the results. Calculate a vector
         from the origin to the plane.
         ./
         vscl_c ( konst, normal, vec );
         printf( "Vector from origin   : %11.7f %11.7f %11.7f\n",
                                           vec[0], vec[1], vec[2] );
         printf( " \n" );

         /.
         Now calculate a vector in the plane from the
         location in the plane defined by `vec'.
         ./
         vsub_c ( vec, point, plnvec );

         /.
         These vectors should be orthogonal.
         ./
         printf( "dot product          : %11.7f\n", vdot_c ( plnvec, vec ) );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Distance to the plane:   4.8102899
      Vector from origin   :  -0.7777778   3.8888889  -2.7222222

      dot product          :  -0.0000000


   2) Apply a linear transformation represented by a matrix to
      a plane represented by a normal vector and a constant.

      Find a normal vector and constant for the transformed plane.


      Example code begins here.


      /.
         Program pl2nvc_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpicePlane           plane;
         SpiceDouble          m      [3][3];
         SpiceDouble          point  [3];
         SpiceDouble          span1  [3];
         SpiceDouble          span2  [3];
         SpiceDouble          tkonst;
         SpiceDouble          tnorml [3];
         SpicePlane           tplane;
         SpiceDouble          tpoint [3];
         SpiceDouble          tspan1 [3];
         SpiceDouble          tspan2 [3];

         /.
         Set the normal vector and the constant defining the
         initial plane.
         ./
         SpiceDouble          normal [3] = {
                                         -0.1616904, 0.8084521, -0.5659165 };
         SpiceDouble          konst      = 4.8102899;

         /.
         Define a transformation matrix to the right-handed
         reference frame having the +i unit vector as primary
         axis, aligned to the original frame's +X axis, and
         the -j unit vector as second axis, aligned to the +Y
         axis.
         ./
         SpiceDouble          axdef  [3] = { 1.0,  0.0,  0.0 };
         SpiceDouble          plndef [3] = { 0.0, -1.0,  0.0 };

         twovec_c ( axdef, 1, plndef, 2, m );

         /.
         Make a SPICE plane from `normal' and `konst', and then
         find a point in the plane and spanning vectors for the
         plane.  `normal' need not be a unit vector.
         ./
         nvc2pl_c ( normal, konst, &plane );
         pl2psv_c ( &plane, point, span1, span2 );

         /.
         Apply the linear transformation to the point and
         spanning vectors.  All we need to do is multiply
         these vectors by `m', since for any linear
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
         Make a new SPICE plane `tplane' from the
         transformed point and spanning vectors, and find a
         unit normal and constant for this new plane.
         ./
         psv2pl_c (  tpoint, tspan1, tspan2, &tplane );
         pl2nvc_c ( &tplane, tnorml, &tkonst );

         /.
         Print the results.
         ./
         printf( "Unit normal vector: %11.7f %11.7f %11.7f\n",
                               tnorml[0], tnorml[1], tnorml[2] );
         printf( "Constant          : %11.7f\n", tkonst );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Unit normal vector:  -0.1616904  -0.8084521   0.5659165
      Constant          :   4.8102897


-Restrictions

   None.

-Literature_References

   [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
        7th Edition, Addison Wesley, 1988.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 24-AUG-2021 (JDR)

       Changed the output argument name "constant" to "konst" for
       consistency with other routines.

       Edited the header to comply with NAIF standard. Added complete code
       examples.

   -CSPICE Version 1.0.1, 06-FEB-2003 (EDW)

       Trivial correction to header docs.

   -CSPICE Version 1.0.0, 05-MAR-1999 (NJB)

-Index_Entries

   plane to normal vector and constant

-&
*/

{ /* Begin pl2nvc_c */


   /*
   Unpack the plane.
   */

   MOVED ( plane->normal, 3, normal );

   *konst = plane->constant;


} /* End pl2nvc_c */
