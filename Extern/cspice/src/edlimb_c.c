/*

-Procedure edlimb_c   ( Ellipsoid Limb )

-Abstract

   Find the limb of a triaxial ellipsoid, viewed from a specified
   point.

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

-Keywords

   ELLIPSE
   ELLIPSOID
   GEOMETRY
   MATH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    edlimb_c


   void edlimb_c ( SpiceDouble           a,
                   SpiceDouble           b,
                   SpiceDouble           c,
                   ConstSpiceDouble      viewpt[3],
                   SpiceEllipse        * limb      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   a          I   Length of ellipsoid semi-axis lying on the x-axis.
   b          I   Length of ellipsoid semi-axis lying on the y-axis.
   c          I   Length of ellipsoid semi-axis lying on the z-axis.
   viewpt     I   Location of viewing point.
   limb       O   Limb of ellipsoid as seen from viewing point.

-Detailed_Input

   a,
   b,
   c           are the lengths of the semi-axes of a triaxial
               ellipsoid. The ellipsoid is centered at the
               origin and oriented so that its axes lie on the
               x, y and z axes.  a, b, and c are the lengths of
               the semi-axes that point in the x, y, and z
               directions respectively.

   viewpt      is a point from which the ellipsoid is viewed.
               viewpt must be outside of the ellipsoid.

-Detailed_Output

   limb        is a SPICE ellipse that represents the limb of
               the ellipsoid.

-Parameters

   None.

-Exceptions

   1)  If the length of any semi-axis of the ellipsoid is non-positive, the
       error SPICE(INVALIDAXISLENGTH) is signaled by a routine in the call
       tree of this routine. `limb' is not modified.

   2)  If the length of any semi-axis of the ellipsoid is zero after the
       semi-axis lengths are scaled by the reciprocal of the magnitude of
       the longest semi-axis and then squared, the error
       SPICE(DEGENERATECASE) is signaled by a routine in the call tree of
       this routine. `limb' is not modified.

   3)  If the viewing point `viewpt' is inside the ellipse, the error
       SPICE(INVALIDPOINT) is signaled by a routine in the call tree of this
       routine. `limb' is not modified.

   4)  If the geometry defined by the input ellipsoid and viewing point is
       so extreme that the limb cannot be found, the error
       SPICE(DEGENERATECASE) is signaled by a routine in the call tree of
       this routine.

   5)  If the shape of the ellipsoid and the viewing geometry are
       such that the limb is an excessively flat ellipsoid, the
       limb may be a degenerate ellipse. You must determine whether
       this possibility poses a problem for your application.

-Files

   None.

-Particulars

   The limb of a body, as seen from a viewing point, is the boundary
   of the portion of the body's surface that is visible from that
   viewing point. In this definition, we consider a surface point
   to be `visible' if it can be connected to the viewing point by a
   line segment that doesn't pass through the body. This is a purely
   geometrical definition that ignores the matter of which portions
   of the surface are illuminated, or whether the view is obscured by
   any additional objects.

   If a body is modeled as a triaxial ellipsoid, the limb is always
   an ellipse. The limb is determined by its center, a semi-major
   axis vector, and a semi-minor axis vector.

   We note that the problem of finding the limb of a triaxial
   ellipsoid is mathematically identical to that of finding its
   terminator, if one makes the simplifying assumption that the
   terminator is the limb of the body as seen from the vertex of the
   umbra. So, this routine can be used to solve this simplified
   version of the problem of finding the terminator.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given an ellipsoid and a viewpoint exterior to it, calculate
      the limb ellipse as seen from that viewpoint.


      Example code begins here.


      /.
         Program edlimb_ex1
      ./
      #include <math.h>
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define UBEL         9

         /.
         Local variables.
         ./
         SpiceDouble          a;
         SpiceDouble          b;
         SpiceDouble          c;
         SpiceDouble          ecentr [3];
         SpiceEllipse         limb;
         SpiceDouble          smajor [3];
         SpiceDouble          sminor [3];

         /.
         Define a viewpoint exterior to the ellipsoid.
         ./
         SpiceDouble          viewpt [3] = { 2.0,  0.0,  0.0 };

         /.
         Define an ellipsoid.
         ./
         a = sqrt( 2.0 );
         b = 2.0 * sqrt( 2.0 );
         c = sqrt( 2.0 );

         /.
         Calculate the limb ellipse as seen by from the
         viewpoint.
         ./
         edlimb_c ( a, b, c, viewpt, &limb );

         /.
         Output the structure components.
         ./
         el2cgv_c ( &limb, ecentr, smajor, sminor );

         printf( "Limb ellipse as seen from viewpoint:\n" );
         printf( "   Semi-minor axis: %10.6f %10.6f %10.6f\n",
                              sminor[0], sminor[1], sminor[2] );
         printf( "   Semi-major axis: %10.6f %10.6f %10.6f\n",
                              smajor[0], smajor[1], smajor[2] );
         printf( "   Center         : %10.6f %10.6f %10.6f\n",
                              ecentr[0], ecentr[1], ecentr[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Limb ellipse as seen from viewpoint:
         Semi-minor axis:   0.000000   0.000000  -1.000000
         Semi-major axis:   0.000000   2.000000  -0.000000
         Center         :   1.000000   0.000000   0.000000


   2) We'd like to find the apparent limb of Jupiter, corrected for
      light time and stellar aberration, as seen from JUNO
      spacecraft's position at a given UTC time.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: edlimb_ex2.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                           Contents
            ---------                           --------
            juno_rec_160522_160729_160909.bsp   JUNO s/c ephemeris
            pck00010.tpc                        Planet orientation
                                                and radii
            naif0012.tls                        Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'juno_rec_160522_160729_160909.bsp',
                                'pck00010.tpc',
                                'naif0012.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program edlimb_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define UTCSTR       "2016 Jul 14 19:45:00"

         /.
         Local variables.
         ./
         SpiceDouble          center [3];
         SpiceDouble          et;
         SpiceDouble          jpos   [8];
         SpiceEllipse         limb;
         SpiceDouble          lt;
         SpiceDouble          rad    [3];
         SpiceDouble          smajor [3];
         SpiceDouble          sminor [3];
         SpiceDouble          scpjfc [3];
         SpiceDouble          scpos  [3];
         SpiceDouble          tipm   [3][3];

         SpiceInt             n;

         /.
         Load the required kernels.
         ./
         furnsh_c ( "edlimb_ex2.tm" );

         /.
         Find the viewing point in Jupiter-fixed coordinates. To
         do this, find the apparent position of Jupiter as seen
         from the spacecraft in Jupiter-fixed coordinates and
         negate this vector. In this case we'll use light time
         and stellar aberration corrections to arrive at the
         apparent limb. `jpos' is the Jupiter's position as seen
         from the spacecraft.  `scpos' is the spacecraft's position
         relative to Jupiter.
         ./
         str2et_c ( UTCSTR, &et );
         spkpos_c ( "JUPITER", et, "J2000", "LT+S", "JUNO", jpos, &lt );

         vminus_c ( jpos, scpos );

         /.
         Get Jupiter's semi-axis lengths...
         ./
         bodvrd_c ( "JUPITER", "RADII", 3, &n, rad );

         /.
         ...and the transformation from J2OOO to Jupiter
         equator and prime meridian coordinates. Note that we
         use the orientation of Jupiter at the time of
         emission of the light that arrived at the
         spacecraft at time `et'.
         ./
         pxform_c ( "J2000", "IAU_JUPITER", et-lt, tipm );

         /.
         Transform the spacecraft's position into Jupiter-
         fixed coordinates.
         ./
         mxv_c ( tipm, scpos, scpjfc );

         /.
         Find the apparent limb.  `limb' is a SPICE ellipse
         representing the limb.
         ./
         edlimb_c ( rad[0], rad[1], rad[2], scpjfc, &limb );

         /.
         `center', `smajor', and `sminor' are the limb's center,
         semi-major axis of the limb, and a semi-minor axis
         of the limb.  We obtain these from `limb' using the
         CSPICE routine el2cgv_c ( Ellipse to center and
         generating vectors ).
         ./
         el2cgv_c ( &limb, center, smajor, sminor );

         /.
         Output the structure components.
         ./
         printf( "Apparent limb of Jupiter as seen from JUNO:\n" );
         printf( "   UTC time       :  %s\n", UTCSTR );
         printf( "   Semi-minor axis: %13.6f %13.6f %13.6f\n",
                              sminor[0], sminor[1], sminor[2] );
         printf( "   Semi-major axis: %13.6f %13.6f %13.6f\n",
                              smajor[0], smajor[1], smajor[2] );
         printf( "   Center         : %13.6f %13.6f %13.6f\n",
                              center[0], center[1], center[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Apparent limb of Jupiter as seen from JUNO:
         UTC time       :  2016 Jul 14 19:45:00
         Semi-minor axis:  12425.547643  -5135.572410  65656.053303
         Semi-major axis:  27305.667297  66066.222576     -0.000000
         Center         :    791.732472   -327.228993   -153.408849


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.1, 24-AUG-2021 (NJB) (JDR)

       Edited the header to comply with NAIF standard.
       Added complete code examples.

       Corrected two typos in header comments.

   -CSPICE Version 1.1.0, 24-JUN-2014 (NJB)

       Edit to correct chkout_c call passing the wrong routine name.

   -CSPICE Version 1.0.1, 24-OCT-2005 (NJB)

       Header update: reference to bodvar_c was replaced with
       reference to bodvcd_c.

   -CSPICE Version 1.0.0, 13-JUN-1999 (NJB)

-Index_Entries

   ellipsoid limb

-&
*/

{ /* Begin edlimb_c */


   /*
   Local variables
   */

   SpiceBoolean            found;

   SpiceDouble             level;
   SpiceDouble             normal  [3];
   SpiceDouble             scale;
   SpiceDouble             scla;
   SpiceDouble             scla2;
   SpiceDouble             sclb;
   SpiceDouble             sclb2;
   SpiceDouble             sclc;
   SpiceDouble             sclc2;
   SpiceDouble             v       [3];

   SpicePlane              lplane;



   /*
   Participate in error tracing.
   */

   chkin_c ( "edlimb_c" );

   if (         ( a <= 0. )
          ||    ( b <= 0. )
          ||    ( c <= 0. )   )
   {
      setmsg_c  ( "Semi-axis lengths: a = #,  b = #,  c = #."  );
      errdp_c   ( "#", a                                       );
      errdp_c   ( "#", b                                       );
      errdp_c   ( "#", c                                       );
      sigerr_c  ( "SPICE(DEGENERATECASE)"                      );
      chkout_c  ( "edlimb_c"                                   );
      return;
   }


   /*
   Scale the semi-axes lengths for better numerical behavior.
   If squaring any one of the scaled lengths causes it to
   underflow to zero, we cannot continue the computation. Otherwise,
   scale the viewing point too.
   */

   scale  =  MaxAbs ( a, b     );
   scale  =  MaxAbs ( c, scale );

   scla   =  a / scale;
   sclb   =  b / scale;
   sclc   =  c / scale;

   scla2  =  scla*scla;
   sclb2  =  sclb*sclb;
   sclc2  =  sclc*sclc;

   if (       ( scla2   ==   0. )
         ||   ( sclb2   ==   0. )
         ||   ( sclc2   ==   0. )   )
   {
      setmsg_c ( "Semi-axis too small:  a = #, b = #, c = #. " );
      errdp_c  ( "#", a                                        );
      errdp_c  ( "#", b                                        );
      errdp_c  ( "#", c                                        );
      sigerr_c ( "SPICE(DEGENERATECASE)"                       );
      chkout_c ( "edlimb_c"                                    );
      return;
   }

   vscl_c ( 1. / scale,  viewpt,  v );


   /*
   The viewing point must be outside of the ellipsoid.  level is the
   constant of the level surface that v lies on.  The ellipsoid
   itself is the level surface corresponding to level = 1.
   */

   level   =     ( v[0]*v[0] / scla2 )
              +  ( v[1]*v[1] / sclb2 )
              +  ( v[2]*v[2] / sclc2 );

   if ( level < 1. )
   {
      setmsg_c ( "Viewing point is inside the ellipsoid." );
      sigerr_c ( "SPICE(DEGENERATECASE)"                  );
      chkout_c ( "edlimb_c"                               );
      return;
   }


   /*
   Find a normal vector for the limb plane.

   To compute this vector, we use the fact that the surface normal at
   each limb point is orthogonal to the line segment connecting the
   viewing point and the limb point.   Let the notation

      < a, b >

   indicate the dot product of the vectors a and b.  If we call the
   viewing point v and the limb point x, then



                          x[0]         x[1]         x[2]
      0  =   < v - x,  ( -------- ,   -------- ,   --------  )  >
                              2           2             2
                          scla        sclb          sclc


                          x[0]         x[1]         x[2]
         =   <   v,    ( -------- ,   -------- ,   --------  )  >
                              2           2             2
                          scla        sclb          sclc


                          x[0]         x[1]         x[2]
          - <   x,    ( -------- ,   -------- ,   --------  )  >
                              2           2             2
                          scla        sclb          sclc

                              2           2             2
                          x[0]        x[1]          x[2]
         =             --------  +   --------  +  --------
                             2            2             2
                         scla         sclb          sclc


         =   1


   This last equation is just the equation of the scaled ellipsoid.
   We can combine the last two equalities and interchange the
   positions of x and v to obtain


                    v[0]         v[1]         v[2]
      <   x,    ( -------- ,   -------- ,   --------  )  >   =   1
                        2           2             2
                    scla        sclb          sclc


   This is the equation of the limb plane.
   */

   /*
   Put together a SPICE plane, lplane, that represents the limb
   plane.
   */
   normal[0]  =  v[0] / scla2;
   normal[1]  =  v[1] / sclb2;
   normal[2]  =  v[2] / sclc2;

   nvc2pl_c ( normal, 1.0, &lplane );


   /*
   Find the limb by intersecting the limb plane with the ellipsoid.
   */
   inedpl_c ( scla,  sclb,  sclc,  &lplane,  limb,  &found );


   /*
   found should be true unless we've encountered numerical problems.
   */

   if ( !found )
   {
      setmsg_c ( "Ellipsoid shape and viewing geometry are too "
                 "extreme; the limb was not found. "             );
      sigerr_c ( "SPICE(DEGENERATECASE)"                         );
      chkout_c ( "edlimb_c"                                      );
      return;
   }


   /*
   Undo the scaling before returning the limb.
   */

   vscl_c ( scale,  limb->center,     limb->center    );
   vscl_c ( scale,  limb->semiMajor,  limb->semiMajor );
   vscl_c ( scale,  limb->semiMinor,  limb->semiMinor );


   chkout_c ( "edlimb_c" );

} /* End edlimb_c */
