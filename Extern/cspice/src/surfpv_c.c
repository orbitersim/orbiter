/*

-Procedure surfpv_c ( Surface point and velocity )

-Abstract

   Find the state (position and velocity) of the surface intercept
   defined by a specified ray, ray velocity, and ellipsoid.

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
   #include "SpiceZst.h"
   #undef   surfpv_c


   void surfpv_c ( ConstSpiceDouble      stvrtx[6],
                   ConstSpiceDouble      stdir [6],
                   SpiceDouble           a,
                   SpiceDouble           b,
                   SpiceDouble           c,
                   SpiceDouble           stx   [6],
                   SpiceBoolean        * found      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   stvrtx     I   State of ray's vertex.
   stdir      I   State of ray's direction vector.
   a          I   Length of ellipsoid semi-axis along the X-axis.
   b          I   Length of ellipsoid semi-axis along the Y-axis.
   c          I   Length of ellipsoid semi-axis along the Z-axis.
   stx        O   State of surface intercept.
   found      O   Flag indicating whether intercept state was found.

-Detailed_Input

   stvrtx      is the state of a ray's vertex. The first three
               components of `stvrtx' are the vertex's x, y, and z
               position components; the vertex's x, y, and z
               velocity components follow.

               The reference frame relative to which `stvrtx' is
               specified has axes aligned with with those of a
               triaxial ellipsoid. See the description below of
               the arguments `a', `b', and `c'.

               The vertex may be inside or outside of this
               ellipsoid, but not on it, since the surface
               intercept is a discontinuous function at
               vertices on the ellipsoid's surface.

               No assumption is made about the units of length
               and time, but these units must be consistent with
               those of the other inputs.


   stdir       is the state of the input ray's direction vector.
               The first three components of `stdir' are a non-zero
               vector giving the x, y, and z components of the
               ray's direction; the direction vector's x, y, and
               z velocity components follow.

               `stdir' is specified relative to the same reference
               frame as is `stvrtx'.


   a,
   b,
   c           are, respectively, the lengths of a triaxial
               ellipsoid's semi-axes lying along the x, y, and
               z axes of the reference frame relative to which
               `stvrtx' and `stdir' are specified.

-Detailed_Output

   stx         is the state of the intercept of the input ray on
               the surface of the input ellipsoid. The first
               three components of `stx' are the intercept's x, y,
               and z position components; the intercept's x, y,
               and z velocity components follow.

               `stx' is specified relative to the same reference
               frame as are `stvrtx' and `stdir'.

               `stx' is defined if and only if both the intercept
               and its velocity are computable, as indicated by
               the output argument `found'.

               The position units of `stx' are the same as those of
               `stvrtx', `stdir', and `a', `b', and `c'. The time units are
               the same as those of `stvrtx' and `stdir'.


   found       is a logical flag indicating whether `stx' is
               defined. `found' is SPICETRUE if and only if both the
               intercept and its velocity are computable. Note
               that in some cases the intercept may computable
               while the velocity is not; this can happen for
               near-tangency cases.

-Parameters

   None.

-Exceptions

   1)  If the input ray's direction vector is the zero vector, an
       error is signaled by a routine in the call tree of this
       routine.

   2)  If any of the ellipsoid's axis lengths is nonpositive, an
       error is signaled by a routine in the call tree of this
       routine.

   3)  If the vertex of the ray is on the ellipsoid, the error
       SPICE(INVALIDVERTEX) is signaled by a routine in the call tree
       of this routine.

-Files

   None.

-Particulars

   The position and velocity of the ray's vertex as well as the
   ray's direction vector and velocity vary with time. The
   inputs to surfpv_c may be considered the values of these
   vector functions at a particular time, say t0. Thus

      State of vertex:           stvrtx = ( V(t0), V'(t0) )

      State of direction vector: stdir  = ( D(t0), D'(t0) )

   To determine the intercept point, W(t0), we simply compute the
   intersection of the ray originating at V(t0) in the direction of
   D(t0) with the ellipsoid

        2      2      2
       x      y      z
      --- +  --- +  ---  =  1
        2      2      2
       A      B      C

   W(t) is the path of the intercept point along the surface of
   the ellipsoid. To determine the velocity of the intercept point,
   we need to take the time derivative of W(t), and evaluate it at
   t0. Unfortunately W(t) is a complicated expression, and its
   derivative is even more complicated.

   However, we know that the derivative of W(t) at t0, W'(t0), is
   tangent to W(t) at t0. Thus W'(t0) lies in the plane that is tangent
   to the ellipsoid at t0. Let X(t) be the curve in the tangent plane
   that represents the intersection of the ray emanating from V(t0)
   with direction D(t0) with that tangent plane.

      X'(t0) = W'(t0)

   The expression for X'(t) is much simpler than that of W'(t);
   surfpv_c evaluates X'(t) at t0.


   Derivation of X(t) and X'(t)
   ----------------------------------------------------------------

   W(t0) is the intercept point. Let N be a surface normal at I(t0).
   Then the tangent plane at W(t0) is the set of points X(t) such
   that

      < X(t) - I(t0), N > = 0

   X(t) can be expressed as the vector sum of the vertex
   and some scalar multiple of the direction vector,

      X(t) = V(t) + s(t) * D(t)

   where s(t) is a scalar function of time. The derivative of
   X(t) is given by

      X'(t) = V'(t)  +  s(t) * D'(t)  +  s'(t) * D(t)

   We have V(t0), V'(t0), D(t0), D'(t0), W(t0), and N, but to
   evaluate X'(t0), we need s(t0) and s'(t0). We derive an
   expression for s(t) as follows.

   Because X(t) is in the tangent plane, it must satisfy

      < X(t) - W(t0), N > = 0.

   Substituting the expression for X(t) into the equation above
   gives

      < V(t) + s(t) * D(t) - W(t0), N > = 0.

   Thus

      < V(t) - W(t0), N >  +  s(t) * < D(t), N > = 0,

   and
                   < V(t) - W(t0), N >
      s(t)  =  -  ---------------------
                       < D(t), N >

   The derivative of s(t) is given by

      s'(t) =

          < D(t),N > * < V'(t),N >  -  < V(t)-W(t0),N > * < D'(t),N >
      -  -------------------------------------------------------------
                                           2
                                < D(t), N >

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Illustrate the role of the ray vertex velocity and
      ray direction vector velocity via several simple cases. Also
      show the results of a near-tangency computation.


      Example code begins here.


      /.
         Program surfpv_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         SpiceBoolean            found;

         SpiceDouble             a;
         SpiceDouble             b;
         SpiceDouble             c;
         SpiceDouble             stvrtx [6];
         SpiceDouble             stdir  [6];
         SpiceDouble             stx    [6];

         SpiceInt                i;


         a = 1.0;
         b = 2.0;
         c = 3.0;

         printf ( "\nEllipsoid radii: \n"
                  "     a = %f\n"
                  "     b = %f\n"
                  "     c = %f\n",
                  a,
                  b,
                  c                      );


         for ( i = 0;  i < 3;  i++ )
         {
            if ( i == 0 )
            {
               printf ( "\n%s\n\n",
                        "Case 1: Vertex varies, direction is constant"  );

               stvrtx[0] =  2.0;
               stvrtx[1] =  0.0;
               stvrtx[2] =  0.0;
               stvrtx[3] =  0.0;
               stvrtx[4] =  0.0;
               stvrtx[5] =  3.0;

               stdir[0]  = -1.0;
               stdir[1]  =  0.0;
               stdir[2]  =  0.0;
               stdir[3]  =  0.0;
               stdir[4]  =  0.0;
               stdir[5]  =  0.0;
            }
            else if ( i == 1 )
            {
               printf ( "\n%s\n\n",
                        "Case 2: Vertex and direction both vary"  );

               stvrtx[0] =  2.0;
               stvrtx[1] =  0.0;
               stvrtx[2] =  0.0;
               stvrtx[3] =  0.0;
               stvrtx[4] =  0.0;
               stvrtx[5] =  3.0;

               stdir[0]  = -1.0;
               stdir[1]  =  0.0;
               stdir[2]  =  0.0;
               stdir[3]  =  0.0;
               stdir[4]  =  0.0;
               stdir[5]  =  4.0;
            }
            else
            {
               printf ( "\n%s\n\n",
                        "Case 3: Vertex and direction both vary; "
                        "near-tangent case"                       );

               stvrtx[2] =  c - 1.e-15;
               stvrtx[5] =  1.e299;
               stdir[5]  =  1.e299;
            }

            printf ( "Vertex:\n"
                     " %20.12e %20.12e %20.12e\n",
                     stvrtx[0],
                     stvrtx[1],
                     stvrtx[2]                    );

            printf ( "Vertex velocity:\n"
                     " %20.12e %20.12e %20.12e\n",
                     stvrtx[3],
                     stvrtx[4],
                     stvrtx[5]                    );

            printf ( "Direction:\n"
                     " %20.12e %20.12e %20.12e\n",
                     stdir[0],
                     stdir[1],
                     stdir[2]                     );

            printf ( "Direction velocity:\n"
                     " %20.12e %20.12e %20.12e\n",
                     stdir[3],
                     stdir[4],
                     stdir[5]                     );

            surfpv_c ( stvrtx, stdir, a, b, c, stx, &found );

            if ( !found)
            {
               printf ( " No intercept state found.\n" );
            }
            else
            {
               printf ( "Intercept:\n"
                        " %20.12e %20.12e %20.12e\n",
                        stx[0],
                        stx[1],
                        stx[2]                        );

               printf ( "Intercept velocity:\n"
                        " %20.12e %20.12e %20.12e\n\n",
                        stx[3],
                        stx[4],
                        stx[5]                        );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Ellipsoid radii:
           a = 1.000000
           b = 2.000000
           c = 3.000000

      Case 1: Vertex varies, direction is constant

      Vertex:
         2.000000000000e+00   0.000000000000e+00   0.000000000000e+00
      Vertex velocity:
         0.000000000000e+00   0.000000000000e+00   3.000000000000e+00
      Direction:
        -1.000000000000e+00   0.000000000000e+00   0.000000000000e+00
      Direction velocity:
         0.000000000000e+00   0.000000000000e+00   0.000000000000e+00
      Intercept:
         1.000000000000e+00   0.000000000000e+00   0.000000000000e+00
      Intercept velocity:
         0.000000000000e+00   0.000000000000e+00   3.000000000000e+00


      Case 2: Vertex and direction both vary

      Vertex:
         2.000000000000e+00   0.000000000000e+00   0.000000000000e+00
      Vertex velocity:
         0.000000000000e+00   0.000000000000e+00   3.000000000000e+00
      Direction:
        -1.000000000000e+00   0.000000000000e+00   0.000000000000e+00
      Direction velocity:
         0.000000000000e+00   0.000000000000e+00   4.000000000000e+00
      Intercept:
         1.000000000000e+00   0.000000000000e+00   0.000000000000e+00
      Intercept velocity:
         0.000000000000e+00   0.000000000000e+00   7.000000000000e+00


      Case 3: Vertex and direction both vary; near-tangent case

      Vertex:
         2.000000000000e+00   0.000000000000e+00   3.000000000000e+00
      Vertex velocity:
         0.000000000000e+00   0.000000000000e+00  1.000000000000e+299
      Direction:
        -1.000000000000e+00   0.000000000000e+00   0.000000000000e+00
      Direction velocity:
         0.000000000000e+00   0.000000000000e+00  1.000000000000e+299
      Intercept:
         2.580956827952e-08   0.000000000000e+00   3.000000000000e+00
      Intercept velocity:
       -3.874532036208e+306   0.000000000000e+00  2.999999974190e+299


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   J.E. McLean         (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.2, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Reformatted example's output to comply with maximum line length
       for header comments.

       Updated -Exceptions section.

   -CSPICE Version 1.0.1, 22-JAN-2009 (NJB) (JEM) (WLT)

       Corrected header typo.

   -CSPICE Version 1.0.0, 05-JAN-2009 (NJB) (JEM) (WLT)

-Index_Entries

   ellipsoid surface point and velocity

-&
*/

{ /* Begin surfpv_c */


   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "surfpv_c" );


   surfpv_ (  (doublereal *)  stvrtx,
              (doublereal *)  stdir,
              (doublereal *)  &a,
              (doublereal *)  &b,
              (doublereal *)  &c,
              (doublereal *)  stx,
              (logical    *)  &fnd   );


   *found = (SpiceBoolean) fnd;


   chkout_c ( "surfpv_c" );

} /* End surfpv_c */
