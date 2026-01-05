/*

-Procedure pjelpl_c ( Project ellipse onto plane )

-Abstract

   Project an ellipse onto a plane, orthogonally.

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

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef    pjelpl_c


   void pjelpl_c ( ConstSpiceEllipse  * elin,
                   ConstSpicePlane    * plane,
                   SpiceEllipse       * elout  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   elin       I   A SPICE ellipse to be projected.
   plane      I   A plane onto which elin is to be projected.
   elout      O   A SPICE ellipse resulting from the projection.

-Detailed_Input

   elin,
   plane       are, respectively, a SPICE ellipse and a
               SPICE plane. The geometric ellipse represented
               by elin is to be orthogonally projected onto the
               geometric plane represented by plane.

-Detailed_Output

   elout       is a SPICE ellipse that represents the geometric
               ellipse resulting from orthogonally projecting the
               ellipse represented by inel onto the plane
               represented by plane.

-Parameters

   None.

-Exceptions

   1)  If the input plane is invalid, an error is signaled by a
       routine in the call tree of this routine.

   2)  The input ellipse may be degenerate--its semi-axes may be
       linearly dependent. Such ellipses are allowed as inputs.

   3)  The ellipse resulting from orthogonally projecting the input
       ellipse onto a plane may be degenerate, even if the input
       ellipse is not.

-Files

   None.

-Particulars

   Projecting an ellipse orthogonally onto a plane can be thought of
   finding the points on the plane that are `under' or `over' the
   ellipse, with the `up' direction considered to be perpendicular
   to the plane. More mathematically, the orthogonal projection is
   the set of points Y in the plane such that for some point X in
   the ellipse, the vector Y - X is perpendicular to the plane.
   The orthogonal projection of an ellipse onto a plane yields
   another ellipse.

-Examples

   1)  With  center  = { 1.,  1.,  1. },
             vect1   = { 2.,  0.,  0. },
             vect2   = { 0.,  1.,  1. },
             normal  = { 0.,  0.,  1. }

       the code fragment

             nvc2pl_c ( normal,  0.,      plane           );
             cgv2el_c ( center,  vect1,   vect2,   elin   );
             pjelpl_c ( elin,    plane,   elout           );
             el2cgv_c ( elout,   prjctr,  prjmaj,  prjmin );

       returns

             prjctr  = { 1.,  1.,  0. },
             prjmaj  = { 2.,  0.,  0. },
             prjmin  = { 0.,  1.,  0. }


   2)  With  vect1   = { 2.,  0.,  0. },
             vect2   = { 1.,  1.,  1. },
             center  = { 0.,  0.,  0. },
             normal  = { 0.,  0.,  1. },

       the code fragment

             nvc2pl_c ( normal,  0.,      plane           );
             cgv2el_c ( center,  vect1,   vect2,   elin   );
             pjelpl_c ( elin,    plane,   elout           );
             el2cgv_c ( elout,   prjctr,  prjmaj,  prjmin );

       returns

             prjctr  = { 0.,  0.,  0. };

             prjmaj  = { -2.227032728823213,
                         -5.257311121191336e-1,
                          0.                  };

             prjmin  = {  2.008114158862273e-1,
                         -8.506508083520399e-1,
                          0.                  };



   3)    An example of actual use: Suppose we wish to compute the
         distance from an ellipsoid to a line. Let the line be
         defined by a point P and a direction vector DIRECT; the
         line is the set of points

            P   +   t * DIRECT,

         where t is any real number. Let the ellipsoid have semi-
         axis lengths A, B, and C.

         We can reduce the problem to that of finding the distance
         between the line and an ellipse on the ellipsoid surface by
         considering the fact that the surface normal at the nearest
         point to the line will be orthogonal to DIRECT; the set of
         surface points where this condition holds lies in a plane,
         and hence is an ellipse on the surface. The problem can be
         further simplified by projecting the ellipse orthogonally
         onto the plane defined by

            < X, DIRECT >  =  0.

         The problem is then a two dimensional one: find the
         distance of the projected ellipse from the intersection of
         the line and this plane (which is necessarily one point).
         A `paraphrase' of the relevant code is:

            #include "SpiceUsr.h"
                 .
                 .
                 .
            /.
            Step 1. Find the candidate ellipse cand.
                      normal is a normal vector to the plane
                      containing the candidate ellipse. The
                      ellipse must exist, since it's the
                      intersection of an ellipsoid centered at
                      the origin and a plane containing the
                      origin. For this reason, we don't check
                      inedpl_c's "found flag" found below.
            ./

            normal[0]  =  direct[0] / (a*a);
            normal[1]  =  direct[1] / (b*b);
            normal[2]  =  direct[2] / (c*c);

            nvc2pl_c ( normal, 0., &candpl );

            inedpl_c ( a, b, c, &candpl, cand, &found );


            /.
            Step 2. Project the candidate ellipse onto a
                      plane orthogonal to the line. We'll
                      call the plane prjpl and the
                      projected ellipse prjel.
            ./
            nvc2pl_c ( direct,  0.,     &prjpl );
            pjelpl_c ( &cand,   &prjpl, &prjel );


            /.
            Step 3. Find the point on the line lying in the
                      projection plane, and then find the
                      near point pjnear on the projected
                      ellipse. Here prjpt is the point on the
                      input line that lies in the projection
                      plane. The distance between prjpt and
                      pjnear is dist.
            ./

            vprjp_c  ( linept,  &prjpl,  prjpt          );
            npelpt_c ( &prjel,   prjpt,  pjnear,  &dist );


            /.
            Step 4. Find the near point pnear on the
                     ellipsoid by taking the inverse
                     orthogonal projection of PJNEAR; this is
                     the point on the candidate ellipse that
                     projects to pjnear. Note that the output
                     dist was computed in step 3.

                     The inverse projection of pjnear is
                     guaranteed to exist, so we don't have to
                     check found.
            ./
            vprjpi_c ( pjnear, &prjpl, &candpl, pnear, &found );


            /.
            The value of dist returned is the distance we're looking
            for.

            The procedure described here is carried out in the routine
            npedln_c.
            ./

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

   project ellipse onto plane

-&
*/

{ /* Begin pjelpl_c */



   /*
   Local variables
   */
   SpiceDouble             center[3];
   SpiceDouble             cnst;
   SpiceDouble             normal[3];
   SpiceDouble             prjctr[3];
   SpiceDouble             prjvc1[3];
   SpiceDouble             prjvc2[3];
   SpiceDouble             smajor[3];
   SpiceDouble             sminor[3];



   /*
   Participate in error tracing.
   */
   chkin_c ( "pjelpl_c" );


   /*
   Find generating vectors of the input ellipse.
   */
   el2cgv_c ( elin, center, smajor, sminor );


   /*
   Find a normal vector for the input plane.
   */
   pl2nvc_c ( plane, normal, &cnst );


   /*
   Find the components of the semi-axes that are orthogonal to the
   input plane's normal vector.  The components are generating
   vectors for the projected plane.
   */
   vperp_c ( smajor,  normal,  prjvc1 );
   vperp_c ( sminor,  normal,  prjvc2 );


   /*
   Find the projection of the ellipse's center onto the input plane.
   This is the center of the projected ellipse.

   In case the last assertion is non-obvious, note that the
   projection we're carrying out is the composition of a linear
   mapping (projection to a plane containing the origin and parallel
   to PLANE) and a translation mapping (adding the closest point to
   the origin in PLANE to every point), and both linear mappings and
   translations carry the center of an ellipse to the center of the
   ellipse's image.  Let's state this using mathematical symbols.
   Let L be a linear mapping and let T be a translation mapping,
   say

      T(x) = x + A.

   Then

         T  (  L ( center + cos(theta)smajor + sin(theta)sminor )  )

      =  A  +  L ( center + cos(theta)smajor + sin(theta)sminor )

      =  A  +  L (center)
            +  cos(theta) L(smajor)
            +  sin(theta) L(sminor)

   From the form of this last expression, we see that we have an
   ellipse centered at

         A  +  L (center)

      =  T  (  L (center)  )

   This last term is the image of the center of the original ellipse,
   as we wished to demonstrate.

   Now in the case of orthogonal projection onto a plane PL, L can be
   taken as the orthogonal projection onto a parallel plane PL'
   containing the origin.  Then L is a linear mapping.  Let M be
   the multiple of the normal vector of PL such that M is contained
   in PL (M is the closest point in PL to the origin).  Then the
   orthogonal projection mapping onto PL, which we will name PRJ,
   can be defined by

     PRJ (x)  =  L (x)  +  M.

   So PRJ is the composition of a translation and a linear mapping,
   as claimed.
   */

   vprjp_c ( center, plane, prjctr );


   /*
   Put together the projected ellipse.
   */
   cgv2el_c ( prjctr, prjvc1, prjvc2, elout );


   chkout_c ( "pjelpl_c" );

} /* End pjelpl_c */
