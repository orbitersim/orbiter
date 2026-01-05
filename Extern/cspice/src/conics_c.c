/*

-Procedure conics_c ( Determine state from conic elements )

-Abstract

   Determine the state (position, velocity) of an orbiting body
   from a set of elliptic, hyperbolic, or parabolic orbital
   elements.

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

   CONIC
   EPHEMERIS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZim.h"
   #undef    conics_c


   void conics_c ( ConstSpiceDouble  elts[8],
                   SpiceDouble       et,
                   SpiceDouble       state[6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   elts       I   Conic elements.
   et         I   Input time.
   state      O   State of orbiting body at et.

-Detailed_Input

   elts        are conic osculating elements describing the orbit of a
               body around a primary. The elements are, in order:

                  RP      Perifocal distance.
                  ECC     Eccentricity.
                  INC     Inclination.
                  LNODE   Longitude of the ascending node.
                  ARGP    Argument of periapse.
                  M0      Mean anomaly at epoch.
                  T0      Epoch.
                  MU      Gravitational parameter.

               Units are km, rad, rad/sec, km**3/sec**2.

               The epoch T0 is given in ephemeris seconds past J2000.
               T0 is the instant at which the state of the body is
               specified by the elements.

               The same elements are used to describe all three types
               (elliptic, hyperbolic, and parabolic) of conic orbit.

   et          is the time at which the state of the orbiting body
               is to be determined, in ephemeris seconds J2000.

-Detailed_Output

   state       is the state (position and velocity) of the body at
               time `et'. Components are x, y, z, dx/dt, dy/dt, dz/dt.

-Parameters

   None.

-Exceptions

   1)  If the eccentricity supplied is less than 0, the error
       SPICE(BADECCENTRICITY) is signaled by a routine in the call tree of
       this routine.

   2)  If a non-positive periapse distance is supplied, the error
       SPICE(BADPERIAPSEVALUE) is signaled by a routine in the call tree of
       this routine.

   3)  If a non-positive value for the attracting mass is supplied, the
       error SPICE(BADGM) is signaled by a routine in the call tree of this
       routine.

   4)  If `elts' is such that the resulting orbit at periapsis has
       either its position or velocity equal to zero, or the square
       of the resulting specific angular momentum's magnitude is
       zero, an error is signaled by a routine in the call tree of
       this routine. This is an indication of invalid `elts' elements.

   5)  If `et' is such that the offset in time from periapsis, at which
       the state is to be determined, is so large that there is a
       danger of floating point overflow during computation, an error
       is signaled by a routine in the call tree of this routine.

-Files

   None.

-Particulars

   None.

-Examples

   Let `vinit' contain the initial state of a spacecraft relative to the
   center of a planet at epoch `et', and let `gm' be the gravitation
   parameter of the planet. The call

      oscelt_c ( vinit, et, gm, elts );

   produces a set of osculating elements describing the nominal
   orbit that the spacecraft would follow in the absence of all
   other bodies in the solar system and non-gravitational forces
   on the spacecraft.

   Now let `state' contain the state of the same spacecraft at some
   other, later epoch. The difference between this state and the
   state predicted by the nominal orbit at the same epoch can be
   computed as follows.

      conics_c ( elts,    later, nominal );
      vsubg_c  ( nominal, state, 6, diff );

      printf( "Perturbation in x, dx/dt = %f %f", diff[0], diff[3] );
      printf( "                y, dy/dt = %f %f", diff[1], diff[4] );
      printf( "                z, dz/dt = %f %f", diff[2], diff[5] );

-Restrictions

   None.

-Literature_References

   [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
        Astrodynamics," Dover Publications Inc., 1971.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.2, 02-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added entry
       #4 and updated entry #5 in -Exceptions section.

   -CSPICE Version 1.1.1, 29-JUL-2003 (NJB)

       Various header corrections were made.

   -CSPICE Version 1.1.0, 24-JUL-2001 (NJB)

       Changed protoype: input elts is now type (ConstSpiceDouble *).
       Implemented interface macro for casting input array to const.

   -CSPICE Version 1.0.1, 08-FEB-1998 (EDW)

       Corrected and clarified header entries.

   -CSPICE Version 1.0.0, 10-NOV-1997 (EDW)

-Index_Entries

   state from conic elements

-&
*/

{ /* Begin conics_c */

   /*
   Participate in error tracing.
   */

   chkin_c ( "conics_c");

   conics_ ( ( doublereal * ) elts,
             ( doublereal * ) &et,
             ( doublereal * ) state );

   chkout_c ( "conics_c");


} /* End conics_c */
