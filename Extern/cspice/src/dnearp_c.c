/*

-Procedure dnearp_c ( Derivative of near point )

-Abstract

   Compute the state (position and velocity) of an ellipsoid surface
   point nearest to the position component of a specified state.

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

   DERIVATIVE
   ELLIPSOID
   GEOMETRY

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void dnearp_c ( ConstSpiceDouble    state  [6],
                   SpiceDouble         a,
                   SpiceDouble         b,
                   SpiceDouble         c,
                   SpiceDouble         dnear  [6],
                   SpiceDouble         dalt   [2],
                   SpiceBoolean      * found      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   state      I   State of an object in body-fixed coordinates.
   a          I   Length of semi-axis parallel to X-axis.
   b          I   Length of semi-axis parallel to Y-axis.
   c          I   Length on semi-axis parallel to Z-axis.
   dnear      O   State of the nearest point on the ellipsoid.
   dalt       O   Altitude and derivative of altitude.
   found      O   Flag that indicates whether `dnear' is degenerate.

-Detailed_Input

   state       is a 6-vector giving the position and velocity of some
               object in the body-fixed coordinates of the ellipsoid.

               In body-fixed coordinates, the semi-axes of the ellipsoid
               are aligned with the x, y, and z-axes of the coordinate
               system.

   a           is the length of the semi-axis of the ellipsoid that is
               parallel to the X-axis of the body-fixed coordinate
               system.

   b           is the length of the semi-axis of the ellipsoid that is
               parallel to the Y-axis of the body-fixed coordinate
               system.

   c           is the length of the semi-axis of the ellipsoid that is
               parallel to the Z-axis of the body-fixed coordinate
               system.

-Detailed_Output

   dnear       is the 6-vector giving the position and velocity in
               body-fixed coordinates of the point on the ellipsoid,
               closest to the object whose position and velocity are
               represented by `state'.

               While the position component of `dnear' is always
               meaningful, the velocity component of `dnear' will be
               meaningless if `found' if SPICEFALSE (See the discussion of
               the meaning of `found' below.)


   dalt        is an array of two double precision numbers. The first
               gives the altitude of `state' with respect to the
               ellipsoid. The second gives the rate of change of the
               altitude.

               Note that the rate of change of altitude is meaningful if
               and only if `found' is SPICETRUE (See the discussion of the
               meaning of `found' below.)

   found       is a logical flag indicating whether or not the velocity
               portion of `dnear' is meaningful. If the velocity portion
               of `dnear' is meaningful `found' will be returned with a
               value of SPICETRUE. Under very rare circumstance the velocity
               of the near point is undefined. Under these circumstances
               `found' will be returned with the value SPICEFALSE.

               `found' can be SPICEFALSE only for states whose position
               components are inside the ellipsoid and then only at
               points on a special surface contained inside the
               ellipsoid called the focal set of the ellipsoid.

               `a' point in the interior is on this special surface only
               if there are two or more points on the ellipsoid that are
               closest to it. The origin is such a point and the only
               such point if the ellipsoid is a sphere. For
               non-spheroidal ellipsoids the focal set contains small
               portions of the planes of symmetry of the ellipsoid.

-Parameters

   None.

-Exceptions

   1)  If the axes are non-positive, an error is signaled by a
       routine in the call tree of this routine.

   2)  If an object is passing through the interior of an ellipsoid
       there are points at which there is more than 1 point on the
       ellipsoid that is closest to the object. At these points the
       velocity of the near point is undefined. (See the description
       of the output variable `found').

-Files

   None.

-Particulars

   If an object is moving relative to some triaxial body along a
   trajectory c(t) then there is a companion trajectory n(t) that
   gives the point on the ellipsoid that is closest to c(t) as a
   function of `t'. The instantaneous position and velocity of c(t),
   `state', are sufficient to compute the instantaneous position and
   velocity of n(t), `dnear'.

   This routine computes `dnear' from `state'. In addition it returns the
   altitude and rate of change of altitude.

   Note that this routine can compute `dnear' for `state' outside, on,
   or inside the ellipsoid. However, the velocity of `dnear' and
   derivative of altitude do not exist for a "small" set of `state'
   in the interior of the ellipsoid. See the discussion of `found'
   above for a description of this set of points.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Suppose you wish to compute the velocity of the ground track
      of a satellite as it passes over a location on Mars and that
      the moment of passage has been previously determined. (We
      assume that the spacecraft is close enough to the surface that
      light time corrections do not matter.)

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: dnearp_ex1.tm

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
            pck00010.tpc                     Planet orientation and
                                             radii
            naif0012.tls                     Leapseconds
            de430.bsp                        Planetary ephemeris
            mar097.bsp                       Mars satellite ephemeris
            mro_psp4_ssd_mro95a.bsp          MRO ephemeris

         \begindata

            KERNELS_TO_LOAD = ( 'pck00010.tpc',
                                'naif0012.tls',
                                'de430.bsp',
                                'mar097.bsp',
                                'mro_psp4_ssd_mro95a.bsp' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program dnearp_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define BODYNM       "MARS"
         #define META         "dnearp_ex1.tm"

         /.
         Local variables
         ./
         SpiceDouble          a;
         SpiceDouble          b;
         SpiceDouble          c;
         SpiceDouble          dalt   [2];
         SpiceDouble          dnear  [6];
         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          radii  [3];
         SpiceDouble          state  [6];
         SpiceDouble          gtvel  [3];

         SpiceInt             dim;

         SpiceBoolean         found;

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
         First get the axes of the body.
         ./
         bodvrd_c ( BODYNM, "RADII", 3, &dim, radii );
         vupack_c ( radii, &a, &b, &c );

         /.
         Get the geometric state of the spacecraft with
         respect to BODYNM in the body-fixed reference frame
         at `et' and compute the state of the sub-spacecraft point.
         ./
         spkezr_c ( "MRO", et, "IAU_MARS", "NONE", BODYNM, state, &lt );
         dnearp_c ( state, a, b, c, dnear, dalt, &found );

         if ( found )
         {

            /.
            `dnear' contains the state of the subspacecraft point.
            ./
            vequ_c ( dnear+3, gtvel );

            printf( "Ground-track velocity (km/s): %9.6f %9.6f %9.6f\n",
                                            gtvel[0], gtvel[1], gtvel[2] );
            printf( "Ground-track speed    (km/s): %9.6f\n",
                                           vnorm_c ( gtvel ) );
         }
         else
         {
            printf( "DNEAR is degenerate.\n" );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Ground-track velocity (km/s):  0.505252  1.986553 -2.475506
      Ground-track speed    (km/s):  3.214001


   2) Suppose you wish to compute the one-way doppler shift of a
      radar mounted on board a spacecraft as it passes over some
      region. Moreover, assume that for your purposes it is
      sufficient to neglect effects of atmosphere, topography and
      antenna pattern for the sake of this computation.

      Use the meta-kernel from Example 1 above.


      Example code begins here.


      /.
         Program dnearp_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define BODYNM       "MARS"
         #define META         "dnearp_ex1.tm"

         /.
         Define the central frequency of the radar,
         in megahertz.
         ./
         #define RCFRQ        20.0

         /.
         Local variables
         ./
         SpiceDouble          a;
         SpiceDouble          b;
         SpiceDouble          c;
         SpiceDouble          dalt   [2];
         SpiceDouble          dnear  [6];
         SpiceDouble          et;
         SpiceDouble          lt;
         SpiceDouble          radii  [3];
         SpiceDouble          shift;
         SpiceDouble          state  [6];

         SpiceInt             dim;

         SpiceBoolean         found;

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
         First get the axes of the body.
         ./
         bodvrd_c ( BODYNM, "RADII", 3, &dim, radii );
         vupack_c ( radii, &a, &b, &c );

         /.
         Get the geometric state of the spacecraft with
         respect to BODYNM in the body-fixed reference frame
         at `et' and compute the state of the sub-spacecraft point.
         ./
         spkezr_c ( "MRO", et, "IAU_MARS", "NONE", BODYNM, state, &lt );
         dnearp_c ( state, a, b, c, dnear, dalt, &found );

         if ( found )
         {

            /.
            The change in frequency is given by multiplying `shift'
            times the carrier frequency
            ./
            shift = ( dalt[1] / clight_c() );
            printf( "Central frequency (MHz): %19.16f\n", RCFRQ );
            printf( "Doppler shift     (MHz): %19.16f\n", RCFRQ * shift );
         }
         else
         {
            printf( "DNEAR is degenerate.\n" );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Central frequency (MHz): 20.0000000000000000
      Doppler shift     (MHz): -0.0000005500991159


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 01-NOV-2021 (JDR)

-Index_Entries

   Velocity of the nearest point on an ellipsoid
   Rate of change of the altitude over an ellipsoid
   Derivative of altitude over an ellipsoid
   Velocity of a ground track

-&
*/

{ /* Begin dnearp_c */

   /*
   Local variables.
   */
   logical            logCfound;

   /*
   Participate in error tracing.
   */
   chkin_c ( "dnearp_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   dnearp_ (  ( doublereal * )  state,
              ( doublereal * ) &a,
              ( doublereal * ) &b,
              ( doublereal * ) &c,
              ( doublereal * )  dnear,
              ( doublereal * )  dalt,
              ( logical    * ) &logCfound  );

   /*
   Set the output SpiceBoolean `found' flag.
   */
   *found = (SpiceBoolean)logCfound;

   chkout_c ( "dnearp_c" );

} /* End dnearp_c */
