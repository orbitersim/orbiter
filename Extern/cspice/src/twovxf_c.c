/*

-Procedure twovxf_c ( Two states defining a frame transformation )

-Abstract

   Find the state transformation from a base frame to the
   right-handed frame defined by two state vectors: one state
   vector defining a specified axis and a second state vector
   defining a specified coordinate plane.

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

   AXES
   FRAMES
   MATRIX
   TRANSFORMATION

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void twovxf_c ( ConstSpiceDouble    axdef  [6],
                   SpiceInt            indexa,
                   ConstSpiceDouble    plndef [6],
                   SpiceInt            indexp,
                   SpiceDouble         xform  [6][6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   axdef      I   State defining a principal axis.
   indexa     I   Principal axis number of `axdef' (x=1, y=2, z=3).
   plndef     I   State defining (with `axdef') a principal plane.
   indexp     I   Second axis number (with `indexa') of principal
                  plane.
   xform      O   Output state transformation matrix.

-Detailed_Input

   axdef       is a "generalized" state vector defining one of the
               principal axes of a reference frame. This vector
               consists of three components of a vector-valued
               function of one independent variable t followed by
               the derivatives of the components with respect to that
               variable:

                  ( a, b, c, da/dt, db/dt, dc/dt )

               This routine treats the input states as unitless, but
               in most applications the input states represent
               quantities that have associated units. The first three
               components must have the same units, and the units of
               the last three components must be compatible with
               those of the first three: if the first three
               components of `axdef'

                  ( a, b, c )

               have units U and t has units T, then the units of
               `axdef' normally would be

                  ( U, U, U, U/T, U/T, U/T )

               Note that the direction and angular velocity defined
               by `axdef' are actually independent of U, so scaling
               `axdef' doesn't affect the output of this routine.

               `axdef' could represent position and velocity; it could
               also represent velocity and acceleration. `axdef' could
               for example represent the velocity and acceleration of
               a time-dependent position vector ( x(t), y(t), z(t) ),
               in which case `axdef' would be defined by

                  a     = dx/dt
                  b     = dy/dt
                  c     = dz/dt

                           2      2
                  da/dt = d x / dt

                           2      2
                  db/dt = d y / dt

                           2      2
                  dc/dt = d z / dt

               Below, we'll call the normalized (unit length) version
               of

                  ( a, b, c )

               the "direction" of `axdef'.

               We call the frame relative to which `axdef' is specified
               the "base frame." The input state `plndef' must be
               specified relative to the same base frame.

   indexa      is the index of the reference frame axis that is
               parallel to the direction of `axdef'.

                  indexa   Axis
                  ------   ----
                     1       x
                     2       y
                     3       z

   plndef      is a state vector defining (with `axdef') a principal
               plane of the reference frame. This vector consists
               of three components followed by their derivatives with
               respect to the independent variable `t' associated with
               `axdef', so `plndef' is

                  ( e, f, g, de/dt, df/dt, dg/dt )

               Below, we'll call the unitized version of

                  ( e, f, g )

               the "direction" of `plndef'.

               The second axis of the principal plane containing the
               direction vectors of `axdef' and `plndef' is perpendicular
               to the first axis and has positive dot product with
               the direction vector of `plndef'.

               The first three components of `plndef' must have the
               same units, and the units of the last three components
               must be compatible with those of the first three: if
               the first three components of `plndef'

                  ( e, f, g )

               have units U2 and `t' has units T, then the units of
               `plndef' normally would be

                  ( U2, U2, U2, U2/T, U2/T, U2/T )

               Note that ***for meaningful results, the angular
               velocities defined by `axdef' and `plndef' must both have
               units of 1/T.***

               As with `axdef', scaling `plndef' doesn't affect the
               output of this routine.

               `axdef' and `plndef' must be specified relative to a
               common reference frame, which we call the "base
               frame."

   indexp      is the index of  second axis of the principal frame
               determined by `axdef' and `plndef'. The association of
               integer values and axes is the same as for `indexa'.

-Detailed_Output

   xform       is the 6x6 matrix that transforms states from the
               frame relative to which `axdef' and `plndef' are specified
               (the "base frame") to the frame whose axes and
               derivative are determined by `axdef', `plndef', `indexa' and
               `indexp'.

               The matrix `xform' has the structure shown below:

                  .-              -.
                  |        :       |
                  |    r   :   0   |
                  |        :       |
                  | .......:.......|
                  |        :       |
                  |  dr/dt :   r   |
                  |        :       |
                  `-              -'

               where `r' is a rotation matrix that is a function of
               the independent variable associated with `axdef' and
               `plndef', and where dr/dt is the derivative of `r'
               with respect to that independent variable.

-Parameters

   None.

-Exceptions

   1)  If `indexa' or `indexp' is not in the set {1,2,3}, the error
       SPICE(BADINDEX) is signaled by a routine in the call tree of
       this routine.

   2)  If `indexa' and `indexp' are the same, the error
       SPICE(UNDEFINEDFRAME) is signaled by a routine in the call
       tree of this routine.

   3)  If the cross product of the vectors `axdef' and `plndef' is zero,
       the error SPICE(DEPENDENTVECTORS) is signaled by a routine in
       the call tree of this routine.

-Files

   None.

-Particulars

   Given two linearly independent state vectors `axdef' and `plndef',
   define vectors `dir1' and `dir2' by

      dir1 = ( axdef[0],   axdef[1],   axdef[2]  )
      dir2 = ( plndef[0],  plndef[1],  plndef[2] )

   Then there is a unique right-handed reference frame `f' having:

      `dir1' lying along the `indexa' axis.

      `dir2' lying in the indexa-indexp coordinate plane, such that
      the dot product of `dir2' with the positive `indexp' axis is
      positive.

   This routine determines the 6x6 matrix that transforms states
   from the base frame used to represent the input vectors to the
   the frame `f' determined by `axdef' and `plndef'. Thus a state vector

      s       = ( x, y, z, dx/dt, dy/dt, dz/dt )
       base

   in the input reference frame will be transformed to

      s       = xform * s
       f                 base

   in the frame `f' determined by `axdef' and `plndef'.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) The time-dependent Sun-Canopus reference frame associated with
      a spacecraft uses the spacecraft-sun state to define the Z axis
      and the Canopus direction to define the X-Z plane.

      Find the geometric position of the Earth as seen from the Mars
      Reconnaissance Orbiter spacecraft (MRO) at a specified time,
      relative to the Sun-Canopus reference frame associated with
      MRO.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: twovxf_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                        Contents
            ---------                        --------
            naif0012.tls                     Leapseconds
            de430.bsp                        Planetary ephemeris
            mro_psp4_ssd_mro95a.bsp          MRO ephemeris

         \begindata

            KERNELS_TO_LOAD = ( 'naif0012.tls',
                                'de430.bsp',
                                'mro_psp4_ssd_mro95a.bsp' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program twovxf_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define META         "twovxf_ex1.tm"

         /.
         Define the Right Ascension and Declination, and the
         proper motion in both coordinates, of Canopus, relative
         to the J2000 frame at J2000 epoch, in degrees and
         arcsecond/yr respectively. Note that the values used here
         may not be suitable for real applications.
         ./
         #define RAJ2K        90.3991968556
         #define DECJ2K       -52.6956610556
         #define PMRA         19.93e-3
         #define PMDEC        23.24e-3

         /.
         Local variables
         ./
         SpiceDouble          dec;
         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          pcano  [3];
         SpiceDouble          ra;
         SpiceDouble          rpmra;
         SpiceDouble          rpmdec;
         SpiceDouble          state  [6];
         SpiceDouble          stcano [6];
         SpiceDouble          sterth [6];
         SpiceDouble          stsun  [6];
         SpiceDouble          xfisc  [6][6];

         /.
         Load kernel files via the meta-kernel.
         ./
         furnsh_c ( META );

         /.
         Convert the TDB input time string to seconds past
         J2000, TDB.
         ./
         str2et_c ( "2007 SEP 30 00:00:00 TDB", &et );

         /.
         Define an approximate "state vector" for Canopus using
         the J2000-relative, unit direction vector toward Canopus
         at a specified time `et' (time is needed to compute proper
         motion) as position and the zero vector as velocity.
         ./
         convrt_c ( PMRA,  "ARCSECONDS", "RADIANS", &rpmra  );
         convrt_c ( PMDEC, "ARCSECONDS", "RADIANS", &rpmdec );

         ra  = RAJ2K  * rpd_c() + rpmra  * et/jyear_c();
         dec = DECJ2K * rpd_c() + rpmdec * et/jyear_c();

         radrec_c ( 1.0, ra, dec, pcano );

         /.
         Compute MRO geometric velocity w.r.t. the Solar System
         Barycenter, and use it to correct the Canopus direction
         for stellar aberration.
         ./
         spkezr_c ( "MRO", et, "J2000", "NONE", "SSB", state, &lt );

         stelab_c ( pcano, state+3, stcano );

         vpack_c ( 0.0, 0.0, 0.0, stcano+3 );

         /.
         Let `stsun' be the J2000-relative apparent state of the Sun
         relative to the spacecraft at `et'.
         ./
         spkezr_c ( "SUN", et, "J2000", "CN+S", "MRO", stsun, &lt );

         /.
         The matrix `xfisc' transforms states from J2000 frame
         to the Sun-Canopus reference frame at `et'.
         ./
         twovxf_c ( stsun, 3, stcano, 1, xfisc );

         /.
         Compute the apparent state of the Earth as seen from MRO
         in the J2000 frame at `et' and transform that vector into
         the Sun-Canopus reference frame.
         ./
         spkezr_c ( "EARTH", et, "J2000", "CN+S", "MRO", state, &lt );

         mxvg_c ( xfisc, state, 6, 6, sterth );

         /.
         Display the results.
         ./
         printf( "Earth as seen from MRO in Sun-Canopus frame (km and "
                 "km/s):\n" );
         printf( "   position: %15.3f %15.3f %15.3f\n",
                        sterth[0], sterth[1], sterth[2] );
         printf( "   velocity: %15.3f %15.3f %15.3f\n",
                        sterth[3], sterth[4], sterth[5] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Earth as seen from MRO in Sun-Canopus frame (km and km/s):
         position:   -16659764.322    97343706.915   106745539.738
         velocity:           2.691         -10.345          -7.877


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 05-AUG-2021 (JDR)

-Index_Entries

   define a state transformation matrix from two states

-&
*/

{ /* Begin twovxf_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "twovxf_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   twovxf_ (  ( doublereal * )  axdef,
              ( integer    * ) &indexa,
              ( doublereal * )  plndef,
              ( integer    * ) &indexp,
              ( doublereal * )  xform  );

   /*
   Transpose the output matrix to put it in row-major order.
   */
   xpose6_c ( xform, xform );

   chkout_c ( "twovxf_c" );

} /* End twovxf_c */
