/*

-Procedure nvc2pl_c ( Normal vector and constant to plane )

-Abstract

   Make a SPICE plane from a normal vector and a constant.

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
   #undef    nvc2pl_c


   void nvc2pl_c ( ConstSpiceDouble     normal[3],
                   SpiceDouble          konst,
                   SpicePlane        *  plane     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   normal,
   konst      I   A normal vector and constant defining a plane.
   plane      O   A SPICE plane structure representing the plane.

-Detailed_Input

   normal,
   konst       are, respectively, a normal vector and constant
               defining a plane. normal need not be a unit vector.
               Let the symbol < a, b > indicate the inner product of
               vectors a and b; then the geometric plane is the set
               of vectors x in three-dimensional space that satisfy

                  < x,  normal >  =  konst.

-Detailed_Output

   plane       is a SPICE plane structure that represents the
               geometric plane defined by `normal' and `konst'.

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

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Construct a SPICE plane from a normal vector and a constant.


      Example code begins here.


      /.
         Program nvc2pl_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local variables.
         ./
         SpiceDouble          konst;
         SpicePlane           plane;
         SpiceDouble          okonst;
         SpiceDouble          onorml [3];

         /.
         Set the normal vector and the constant defining the
         plane.
         ./
         SpiceDouble          normal [3] = { 1.0, 1.0, 1.0 };

         konst = 23.0;

         printf( "Inputs:\n" );
         printf( "   Normal vector: %11.7f %11.7f %11.7f\n",
                            normal[0], normal[1], normal[2] );
         printf( "   Constant     : %11.7f\n", konst );
         printf( " \n" );

         /.
         Make a SPICE plane from `normal' and `konst'.
         `normal' need not be a unit vector.
         ./
         nvc2pl_c ( normal, konst, &plane );

         /.
         Print the results.
         ./
         pl2nvc_c ( &plane, onorml, &okonst );
         printf( "Generated plane:\n" );
         printf( "   Normal vector: %11.7f %11.7f %11.7f\n",
                            onorml[0], onorml[1], onorml[2] );
         printf( "   Constant     : %11.7f\n", okonst );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Inputs:
         Normal vector:   1.0000000   1.0000000   1.0000000
         Constant     :  23.0000000

      Generated plane:
         Normal vector:   0.5773503   0.5773503   0.5773503
         Constant     :  13.2790562


   2) Apply a linear transformation represented by a matrix to
      a plane represented by a normal vector and a constant.

      Find a normal vector and constant for the transformed plane.


      Example code begins here.


      /.
         Program nvc2pl_ex2
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

   1)  No checking is done to prevent arithmetic overflow.

-Literature_References

   [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
        7th Edition, Addison Wesley, 1988.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.0, 24-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete code
       examples.

       Changed the input argument name "constant" to "konst" for
       consistency with other routines.

   -CSPICE Version 1.0.1, 02-NOV-2009 (NJB)

       Corrected header typo.

   -CSPICE Version 1.0.0, 01-MAR-1999 (NJB)

-Index_Entries

   normal vector and constant to plane

-&
*/

{ /* Begin nvc2pl_c */


   /*
   Local variables
   */
   SpiceDouble             mag;



   /*
   This routine checks in only if an error is discovered.
   */

   if ( return_c () )
   {
      return;
   }

   unorm_c ( normal, plane->normal, &mag );


   /*
   The normal vector must be non-zero.
   */
   if ( mag == 0. )
   {
      chkin_c  ( "nvc2pl_c"                          );
      setmsg_c ( "plane's normal must be non-zero."  );
      sigerr_c ( "SPICE(ZEROVECTOR)"                 );
      chkout_c ( "nvc2pl_c"                          );
      return;
   }


   /*
   To find the plane constant corresponding to the unitized normal
   vector, we observe that

      < x, normal > = konst,

   so

      < x, normal / || normal || >   =   konst / || normal ||

   */


   plane->constant = konst / mag;


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


} /* End nvc2pl_c */
