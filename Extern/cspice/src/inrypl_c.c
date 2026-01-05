/*

-Procedure inrypl_c ( Intersection of ray and plane )

-Abstract

   Find the intersection of a ray and a plane.

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

*/
   #include <math.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    inrypl_c


   void inrypl_c ( ConstSpiceDouble     vertex [3],
                   ConstSpiceDouble     dir    [3],
                   ConstSpicePlane    * plane,
                   SpiceInt           * nxpts,
                   SpiceDouble          xpt    [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   vertex,
   dir        I   Vertex and direction vector of ray.
   plane      I   A SPICE plane.
   nxpts      O   Number of intersection points of ray and plane.
   xpt        O   Intersection point, if nxpts = 1.

-Detailed_Input

   vertex,
   dir         are a point and direction vector that define a
               ray in three-dimensional space.

   plane       is a SPICE plane.

-Detailed_Output

   nxpts       is the number of points of intersection of the
               input ray and plane. Values and meanings of
               nxpts are:

                  0     No intersection.

                  1     One point of intersection. Note that
                        this case may occur when the ray's
                        vertex is in the plane.

                 -1     An infinite number of points of
                        intersection; the ray lies in the plane.


   xpt         is the point of intersection of the input ray
               and plane, when there is exactly one point of
               intersection.

               If the ray lies in the plane, xpt is set equal to
               vertex.

               If there is no intersection, xpt is the zero vector.

-Parameters

   None.

-Exceptions

   1)  If the ray's direction vector is the zero vector, the error
       SPICE(ZEROVECTOR) is signaled. `nxpts' and `xpt' are not modified.

   2)  If the ray's vertex is further than dpmax_c / 3 from the origin,
       the error SPICE(VECTORTOOBIG) is signaled. `nxpts' and `xpt' are not
       modified.

   3)  If the input plane is further than dpmax_c / 3 from the origin, the
       error SPICE(VECTORTOOBIG) is signaled. `nxpts' and `xpt' are not
       modified.

   4)  The input plane should be created by one of the CSPICE
       routines

          nvc2pl_c
          nvp2pl_c
          psv2pl_c

       Invalid input planes will cause unpredictable results.

   5)  In the interest of good numerical behavior, in the case
       where the ray's vertex is not in the plane, this routine
       considers that an intersection of the ray and plane occurs
       only if the distance between the ray's vertex and the
       intersection point is less than dpmax_c / 3.

       If `vertex' is not in the plane and this condition is not
       met, then `nxpts' is set to 0 and `xpt' is set to the zero
       vector.

-Files

   None.

-Particulars

   The intersection of a ray and plane in three-dimensional space
   can be a the empty set, a single point, or the ray itself.

-Examples

   1)  Find the camera projection of the center of an extended
       body. For simplicity, we assume:

          -- The camera has no distortion;  the image of a point
             is determined by the intersection of the focal plane
             and the line determined by the point and the camera's
             focal point.

          -- The camera's pointing matrix (C-matrix) is available
             in a C-kernel.


             /.
             Load Leapseconds and SCLK kernels to support time
             conversion.
             ./

             furnsh_c ( "leap.ker" );
             furnsh_c ( "sclk.ker" );

             /.
             Load an SPK file containing ephemeris data for
             observer (a spacecraft, whose NAIF integer code
             is sc) and target at the UTC epoch of observation.
             ./
             furnsh_c ( "spk.bsp" );

             /.
             Load a C-kernel containing camera pointing for
             the UTC epoch of observation.
             ./
             furnsh_c ( "ck.bc" ) ;


             /.
             Find the ephemeris time (barycentric dynamical time)
             and encoded spacecraft clock times corresponding to
             the UTC epoch of observation.
             ./
             utc2et_c ( utc, &et          );
             sce2c_c  ( sc,  et,  &sclkdp );

             /.
             Encode the pointing lookup tolerance.
             ./
             sctiks_c ( sc, tolch, &toldp );


             /.
             Find the observer-target vector at the observation
             epoch. In this example, we'll use a light-time and stellar
             aberration corrected state vector.
             ./

             spkez_c ( target, et, "J2000", "LT+S", sc, state, &lt );

             /.
             Look up camera pointing.
             ./
             ckgp_c ( camera, sclkdp, toldp, "J2000", cmat, &clkout,
                      &found                                        );

             if ( !found )
             {
                /.
                No pointing was available.
                ./

                [Handle this case...]

                return;
             }

             /.
             Negate the spacecraft-to-target body vector and
             convert it to camera coordinates.
             ./
             vminus_c ( state, dir       );
             mxv_c    ( cmat,  dir,  dir );


             /.
             If FL is the camera's focal length, the effective
             focal point is

                FL * ( 0, 0, 1 )
             ./

             vscl_c ( FL, zvec, focus );


             /.
             The camera's focal plane contains the origin in
             camera coordinates, and the z-vector is orthogonal
             to the plane. Make a SPICE plane representing
             the focal plane.
             ./
             nvc2pl_c ( zvec, 0., &fplane );

             /.
             The image of the target body's center in the focal
             plane is defined by the intersection with the focal
             plane of the ray whose vertex is the focal point and
             whose direction is dir.
             ./

             inrypl_c ( focus, dir, fplane, &nxpts, image );

             if ( nxpts == 1 )
             {
                /.
                The body center does project to the focal plane.
                Check whether the image is actually in the
                camera's field of view...
                ./

                [Handle this case...]
             }
             else
             {
                /.
                The body center does not map to the focal plane.
                ./

                [Handle this case...]
             }


   2)  Find the Saturn ring plane intercept of a spacecraft-mounted
       instrument's boresight vector. We want the find the point
       in the ring plane that will be observed by an instrument
       with a give boresight direction at a specified time. We
       must account for light time and stellar aberration in order
       to find this point. The intercept point will be expressed
       in Saturn body-fixed coordinates.

          -- The ring plane is equatorial.

          -- Light travels in a straight line.

          -- The light time correction for the ring plane intercept
             can be obtained by performing three light-time
             correction iterations. If this assumption does not
             lead to a sufficiently accurate result, additional
             iterations can be performed.

          -- A Newtonian approximation of stellar aberration
             suffices.

          -- The boresight vector is given in J2000 coordinates.

          -- The observation epoch is et ephemeris seconds past
             J2000.

          -- The boresight vector, spacecraft and planetary
             ephemerides, and ring plane orientation are all known
             with sufficient accuracy for the application.

          -- All necessary kernels are loaded by the caller of
             this example routine.


       (A similar technique could be used to obtain low-accuracy
       predictions of radio occultations. In that case, the
       instrument boresight ray's direction vector would be replaced
       by the vector from the observer to the light-time corrected
       radio source position.)

       We omit display of the portion of the code that loads SPICE
       kernels.

          #include "SpiceUsr.h"
          #include "SpiceZfc.h"


          void ring_xpt ( ConstSpiceChar    * sc,
                          SpiceDouble         et,
                          ConstSpiceDouble    borvec[3],
                          SpiceDouble       * sbfxpt,
                          SpiceBoolean      * found    )
          {

             /.
             Local constants
             ./
             #define SATURN          699

             /.
             Local variables
             ./
             SpiceBoolean            fnd;

             SpiceDouble             borv2  [3];
             SpiceDouble             corvec [3];
             SpiceDouble             lt;
             SpiceDouble             satssb [6];
             SpiceDouble             scpos  [3];
             SpiceDouble             scssb  [6];
             SpiceDouble             state  [6];
             SpiceDouble             stcorr [3];
             SpiceDouble             tau;
             SpiceDouble             tipm   [3][3];
             SpiceDouble             xpt    [3];
             SpiceDouble             zvec   [3];

             SpiceInt                i;
             SpiceInt                nxpts;
             SpiceInt                scid;

             SpicePlane              plane;


             /.
             First step: account for stellar aberration. Since the
             instrument pointing is given, we need to find the intercept
             point such that, when the stellar aberration correction is
             applied to the vector from the spacecraft to that point,
             the resulting vector is parallel to borvec. An easy solution
             is to apply the inverse of the normal stellar aberration
             correction to borvec, and then solve the intercept problem
             with this corrected boresight vector.

             Find the position of the observer relative
             to the solar system barycenter at et.
             ./
             bodn2c_c ( sc, &scid, &fnd );

             if ( !fnd )
             {
                setmsg_c ( "ID code for body # was not found." );
                errch_c  ( "#",  sc                            );
                sigerr_c ( "SPICE(NOTRANSLATION"               );
                return;
             }

             spkssb_c ( scid, et, "j2000", scssb );


             /.
             We now wish to find the vector corvec that, when corrected for
             stellar aberration, yields borvec. A good first approximation is
             obtained by applying the stellar aberration correction for
             transmission to borvec. Note that the routine called is not
             a wrapper, so there is no letter 'c' at the end of its name.
             The prototype for this routine is declared in SpiceZfc.h.
             ./
             stlabx_ ( (doublereal *) borvec, scssb+3, corvec );

             /.
             The inverse of the stellar aberration correction
             applicable to corvec should be a very good estimate of
             the correction we need to apply to borvec. Apply
             this correction to borvec to obtain an improved estimate
             of corvec.
             ./
             stelab_c  ( corvec, scssb+3,  borv2  );
             vsub_c    ( borv2,  corvec,   stcorr );
             vsub_c    ( borvec, stcorr,   corvec );

             /.
             Because the ring plane intercept may be quite far from
             Saturn's center, we cannot assume light time from the intercept
             to the observer is well approximated by light time from
             Saturn's center to the observer. We compute the light time
             explicitly using an iterative approach.

             We can however use the light time from Saturn's center to
             the observer to obtain a first estimate of the actual light
             time.
             ./
             spkezr_c ( "SATURN", et, "J2000", "LT", sc, state, &lt );

             tau = lt;

             /.
             Find the ring plane intercept and calculate the
             light time from it to the spacecraft.
             Perform three iterations.
             ./
             i       =  0;
             *found  =  SPICETRUE;

             while (  ( i < 3 ) && ( *found )  )
             {
                /.
                Find the position of Saturn relative
                to the solar system barycenter at et-tau.
                ./
                spkssb_c ( SATURN, et-tau, "J2000", satssb );

                /.
                Find the Saturn-to-observer vector defined by these
                two position vectors.
                ./
                vsub_c ( scssb, satssb, scpos );

                /.
                Look up Saturn's pole at et-tau; this is the third
                row of the matrix that transforms J2000
                coordinates to Saturn body-fixed coordinates.
                ./
                pxform_c ( "J2000", "IAU_SATURN", et-tau, tipm );

                vequ_c ( tipm[2], zvec );

                /.
                Make a SPICE plane representing the ring plane.
                We're treating Saturn's center as the origin, so
                the plane constant is 0.
                ./
                nvc2pl_c ( zvec, 0.0, &plane );

                /.
                Find the intersection of the ring plane and the
                ray having vertex scpos and direction vector
                corvec.
                ./
                inrypl_c ( scpos, corvec, &plane, &nxpts, xpt );

                /.
                If the number of intersection points is 1,
                find the next light time estimate.
                ./
                if ( nxpts == 1 )
                {
                   /.
                   Find the light time (zero-order) from the
                   intercept point to the spacecraft.
                   ./
                   tau  =  vdist_c ( scpos, xpt )  /  clight_c();
                   i++;
                }
                else
                {
                   *found = SPICEFALSE;
                }
             }
             /.
             At this point, if found is SPICETRUE, we iterated
             three times, and xpt is our estimate of the
             position of the ring plane intercept point
             relative to Saturn in the J2000 frame. This is the
             point observed by an instrument pointed in direction
             borvec at et at mounted on the spacecraft sc.

             If found is SPICEFALSE, the boresight ray does not
             intersect the ring plane.

             As a final step, transform xpt to Saturn body-fixed
             coordinates.
             ./
             if ( *found )
             {
                mxv_c ( tipm, xpt, sbfxpt );
             }

          }

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)

-Version

   -CSPICE Version 1.0.3, 24-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.2, 01-FEB-2017 (BVS)

       Typo fix: pnv2pl_c -> nvp2pl_c.

   -CSPICE Version 1.0.1, 12-DEC-2002 (NJB)

       Header fix: ring plane intercept algorithm was corrected.
       Now light time is computed accurately, and stellar aberration
       is accounted for. Example was turned into a complete
       subroutine.

   -CSPICE Version 1.0.0, 26-JUN-1999 (NJB)

-Index_Entries

   intersection of ray and plane

-&
*/

{ /* Begin inrypl_c */

   /*
   Local constants
   */
   #define MARGIN  3.0


   /*
   Local macros
   */
   #define CLEAR_VEC( v )    (v)[0] = 0.; (v)[1] = 0.; (v)[2] = 0.;

   /*
   Local variables
   */
   SpiceDouble             constant;
   SpiceDouble             prjdif;
   SpiceDouble             prjdir;
   SpiceDouble             prjvn;
   SpiceDouble             mscale;
   SpiceDouble             normal  [3];
   SpiceDouble             scale;
   SpiceDouble             sclcon;
   SpiceDouble             sclvtx  [3];
   SpiceDouble             toobig;
   SpiceDouble             udir    [3];



   /*
   Participate in error tracing.
   */

   if ( return_c() )
   {
      return;
   }

   chkin_c ( "inrypl_c" );



   /*
   We'll give the name toobig to the bound dpmax_c() / MARGIN.
   If we let vtxprj be the orthogonal projection of vertex onto
   plane, and let diff be the vector vtxprj - vertex, then
   we'll ensure that

      ||  diff  ||    <     2 * toobig

   Check the distance of the ray's vertex from the origin.
   */

   toobig = dpmax_c() / MARGIN;

   if (  vnorm_c (vertex)  >=  toobig  )
   {
      setmsg_c ( "Ray's vertex is too far from the origin." );
      sigerr_c ( "SPICE(VECTORTOOBIG)"                      );
      chkout_c ( "inrypl_c"                                 );
      return;
   }


   /*
   Check the distance of the plane from the origin.  (The returned
   plane constant IS this distance.)
   */
   pl2nvc_c ( plane, normal, &constant );

   if (  constant >=  toobig  )
   {
      setmsg_c ( "Plane is too far from the origin." );
      sigerr_c ( "SPICE(VECTORTOOBIG)"               );
      chkout_c ( "inrypl_c"                          );
      return;
   }


   /*
   Check the ray's direction vector.
   */
   vhat_c ( dir, udir );

   if ( vzero_c (udir) )
   {
      setmsg_c ( "Ray's direction vector is the zero vector."  );
      sigerr_c ( "SPICE(ZEROVECTOR)"                           );
      chkout_c ( "inrypl_c"                                    );
      return;
   }


   /*
   That takes care of the error cases.  Now scale the input vertex
   and plane to improve numerical behavior.
   */
   mscale = MaxAbs ( constant, vnorm_c(vertex) );

   if ( mscale != 0. )
   {
      vscl_c ( 1.0 / mscale, vertex, sclvtx );
      sclcon  =  constant / mscale;
   }
   else
   {
      vequ_c ( vertex, sclvtx );
      sclcon   =  constant;
   }


   if ( mscale > 1.0 )
   {
      toobig = toobig / mscale;
   }


   /*
   Find the projection (coefficient) of the ray's vertex along the
   plane's normal direction.
   */

   prjvn = vdot_c ( sclvtx, normal );

   /*
   If this projection is the plane constant, the ray's vertex lies in
   the plane.  We have one intersection or an infinite number of
   intersections.  It all depends on whether the ray actually lies
   in the plane.

   The absolute value of prjdif is the distance of the ray's vertex
   from the plane.
   */

   prjdif  =  sclcon - prjvn;

   if ( prjdif == 0. )
   {
      /*
      xpt is the original, unscaled vertex.
      */

      vequ_c ( vertex, xpt );

      if (  vdot_c ( normal, udir ) == 0.  )
      {
         /*
         The ray's in the plane.
         */

         *nxpts  =  -1;
      }
      else
      {
         *nxpts  =   1;
      }

      chkout_c ( "inrypl_c" );
      return;
   }



   /*
   Ok, the ray's vertex is not in the plane.  The ray may still be
   parallel to or may point away from the plane.  If the ray does
   point towards the plane, mathematicians would say that the
   ray does intersect the plane, but the computer may disagree.

   For this routine to find an intersection, both of the following
   conditions must be met:

      -- The ray must point toward the plane; this happens when
         prjdif has the same sign as < udir, normal >.

      -- The vector difference (xpt - sclvtx) must not overflow.

    Qualitatively, the case of interest looks something like the
    picture below:


                    * sclvtx
                    |\
                    | \   <-- udir
                    |  \
   length of this   |   \|
    segment is      |   -*
                    |
   | prjdif |   --> | ___________________________
                    |/                          /
                    |       *                  /   <-- plane
                   /|        xpt              /
                  / ^                        /
                 /  | normal                /
                /   | .                    /
               /    |/|                   /
              / .---| /                  /
             /  |   |/                  /
            /   `---*                  /
           /          Projection of sclvtx onto the plane
          /                          /
         /                          /
        ----------------------------


   */


   /*
   Find the projection of the direction vector along the plane's
   normal vector.
   */

   prjdir  =  vdot_c ( udir, normal );


   /*
   We're done if the ray doesn't point toward the plane.  prjdif
   has already been found to be non-zero at this point; prjdir is
   zero if the ray and plane are parallel.  The CSPICE routine
   smsgnd_ will return a value of SPICEFALSE if prjdir is zero.
   */

   if ( ! smsgnd_ (&prjdir, &prjdif)  )
   {
      /*
      The ray is parallel to or points away from the plane.
      */
      *nxpts  = 0;

      CLEAR_VEC ( xpt );

      chkout_c ( "inrypl_c" );
      return;
   }


   /*
   The difference xpt - sclvtx is the hypotenuse of a right triangle
   formed by sclvtx, xpt, and the orthogonal projection of sclvtx
   onto the plane.  We'll obtain the hypotenuse by scaling udir.
   We must make sure that this hypotenuse does not overflow.  The
   scale factor has magnitude

       | prjdif |
     --------------
       | prjdir |

   and UDIR is a unit vector, so as long as

       | prjdif |   <   | prjdir |  *  toobig

   the hypotenuse is no longer than toobig.  The product can be
   computed safely since prjdir has magnitude 1 or less.
   */


   if (  fabs(prjdif)  >=  fabs(prjdir) * toobig  )
   {
      /*
      If the hypotenuse is too long, we say that no intersection exists.
      */
      *nxpts = 0;
      CLEAR_VEC ( xpt );

      chkout_c ( "inrypl_c" );
      return;
   }


   /*
   We conclude that it's safe to compute xpt.  Scale udir and add
   the result to sclvtx.  The addition is safe because both addends
   have magnitude no larger than toobig.  The vector thus obtained
   is the intersection point.
   */

   *nxpts   =   1;
   scale    =   fabs (prjdif)  /   fabs (prjdir);

   vlcom_c ( 1.0, sclvtx, scale, udir, xpt );

   /*
   Re-scale xpt.  This is safe, since toobig has already been
   scaled to allow for any growth of xpt at this step.
   */

   vscl_c ( mscale, xpt, xpt );


   chkout_c ( "inrypl_c" );

} /* End inrypl_c */
