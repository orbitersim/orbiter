/*

-Procedure prop2b_c ( Propagate a two-body solution )

-Abstract

   Compute the state of a massless body at time t_0 + dt by applying
   the two-body force model to a given central mass and a given body
   state at time t_0.

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
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZim.h"
   #undef    prop2b_c

   void prop2b_c ( SpiceDouble         gm,
                   ConstSpiceDouble    pvinit[6],
                   SpiceDouble         dt,
                   SpiceDouble         pvprop[6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   gm         I   Gravity of the central mass.
   pvinit     I   Initial state from which to propagate a state.
   dt         I   Time offset from initial state to propagate to.
   pvprop     O   The propagated state.

-Detailed_Input

   gm          is the gravitational constant G times the mass M of the
               central body.

   pvinit      is the state at some specified time relative to the
               central mass. The mass of the object is assumed to
               be negligible when compared to the central mass.

   dt          is an offset in time from the time of the initial
               state to which the two-body state should be
               propagated. (The units of time and distance must be
               the same in `gm', `pvinit', and `dt').

-Detailed_Output

   pvprop      is the two-body propagation of the initial state
               `dt' units of time past the epoch of the initial state.

-Parameters

   None.

-Exceptions

   1)  If `gm' is not positive, the error SPICE(NONPOSITIVEMASS) is
       signaled by a routine in the call tree of this routine.

   2)  If the position of the initial state is the zero vector, the error
       SPICE(ZEROPOSITION) is signaled by a routine in the call tree of this
       routine.

   3)  If the velocity of the initial state is the zero vector, the error
       SPICE(ZEROVELOCITY) is signaled by a routine in the call tree of this
       routine.

   4)  If the cross product of the position and velocity of `pvinit'
       has squared length of zero, the error SPICE(NONCONICMOTION)
       is signaled by a routine in the call tree of this routine.

   5)  If `dt' is so large that there is a danger of floating point overflow
       during computation, the error SPICE(DTOUTOFRANGE) is signaled by a
       routine in the call tree of this routine and a message is generated
       describing the problem. The value of `dt' must be "reasonable". In
       other words, `dt' should be less than 10**20 seconds for realistic
       solar system orbits specified in the MKS system. (The actual bounds
       on `dt' are much greater but require substantial computation.) The
       "reasonableness" of `dt' is checked at run-time.

-Files

   None.

-Particulars

   This routine uses a universal variables formulation for the
   two-body motion of an object in orbit about a central mass. It
   propagates an initial state to an epoch offset from the
   epoch of the initial state by time `dt'.

   This routine does not suffer from the finite precision
   problems of the machine that are inherent to classical
   formulations based on the solutions to Kepler's equation:

         n( t - T ) = E - e * sin(E)         elliptic case
         n( t - T ) = e * sinh(F) - F        hyperbolic case

   The derivation used to determine the propagated state is a
   slight variation of the derivation in Danby's book
   "Fundamentals of Celestial Mechanics" [1].

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Use the two-body force model to propagate the state of a
      massless body orbiting the Earth at 100,000,000 km after half
      a period.

      In circular two-body motion, the orbital speed is

         s     = sqrt(mu/r)

      where mu is the central mass. After tau/2 = pi*r/s seconds
      (half period), the state should equal the negative of the
      original state.

      Example code begins here.


      /.
         Program prop2b_ex1
      ./
      #include <stdio.h>
      #include <math.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local variables.
         ./
         SpiceDouble            mu;
         SpiceDouble            pvinit [ 6 ];
         SpiceDouble            r;
         SpiceDouble            speed;
         SpiceDouble            state  [ 6 ];
         SpiceDouble            t;

         /.
         Initial values.
         ./
         mu    =  3.9860043543609598E+05;
         r     =  1.0e+08;
         speed =  sqrt( mu / r );
         t     =  pi_c()*r/speed;

         pvinit[0] =  0.0;
         pvinit[1] =  r/sqrt(2.0);
         pvinit[2] =  r/sqrt(2.0);
         pvinit[3] =  0.0;
         pvinit[4] = -speed/sqrt(2.0);
         pvinit[5] =  speed/sqrt(2.0);

         /.
         Calculate the state of the body at 0.5 period
         after the epoch.
         ./
         prop2b_c ( mu, pvinit, t, state );

         /.
         The `state' vector should equal -pvinit
         ./
         printf( "State at t0:\n" );
         printf( "   R   (km): %16.5f %16.5f %16.5f\n",
                        pvinit[0], pvinit[1], pvinit[2] );
         printf( "   V (km/s): %16.5f %16.5f %16.5f\n",
                        pvinit[3], pvinit[4], pvinit[5] );

         printf( "\n" );
         printf( "State at tau/2:\n" );
         printf( "   R   (km): %16.5f %16.5f %16.5f\n",
                        state[0], state[1], state[2]    );
         printf( "   V (km/s): %16.5f %16.5f %16.5f\n",
                        state[3], state[4], state[5]    );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      State at t0:
         R   (km):          0.00000   70710678.11865   70710678.11865
         V (km/s):          0.00000         -0.04464          0.04464

      State at tau/2:
         R   (km):         -0.00000  -70710678.11865  -70710678.11865
         V (km/s):          0.00000          0.04464         -0.04464


   2) When the eccentricity of an orbit is near 1, and the epoch
      of classical elements is near the epoch of periapse, classical
      formulations that propagate a state from elements tend to
      lack robustness due to the finite precision of floating point
      machines. In those situations it is better to use a universal
      variables formulation to propagate the state.

      By using this routine, you need not go from a state to
      elements and back to a state. Instead, you can get the state
      from an initial state.

      If `pvinit' is your initial state and you want the state 3600
      seconds later, the following call will suffice.

         Look up gm somewhere

         dt = 3600.0;

         prop2b_c ( gm, pvinit, dt, pvprop );

      After the call, `pvprop' will contain the state of the
      object 3600 seconds after the time it had state `pvinit'.

-Restrictions

   1)  Users should be sure that `gm', `pvinit' and `dt' are all in the
       same system of units ( for example MKS ).

-Literature_References

   [1]  J. Danby, "Fundamentals of Celestial Mechanics," 2nd Edition,
        pp 168-180, Willman-Bell, 1988.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.1, 01-NOV-2021 (JDR)

       Edited -Examples section to comply with NAIF standard. Added
       complete code example.

   -CSPICE Version 1.1.0, 24-JUL-2001 (NJB)

       Changed prototype: input pvinit is now type
       (ConstSpiceDouble [6]). Implemented interface macro for
       casting input pvinit to const.

   -CSPICE Version 1.0.1, 20-MAR-1998 (EDW)

       Minor correction to header.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (WLT)

-Index_Entries

   Propagate state vector using two-body force model

-&
*/

{ /* Begin prop2b_c */


   /*
   Participate in error handling.
   */

   chkin_c ( "prop2b_c");


   prop2b_ ( ( doublereal * ) &gm,
             ( doublereal * ) pvinit,
             ( doublereal * ) &dt,
             ( doublereal * ) pvprop );


   chkout_c ( "prop2b_c");


} /* End prop2b_c */
