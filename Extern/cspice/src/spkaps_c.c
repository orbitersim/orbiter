/*

-Procedure spkaps_c ( SPK, apparent state )

-Abstract

   Return the state (position and velocity) of a target body
   relative to an observer specified by its state and
   acceleration relative to the solar system barycenter. The
   returned state may be optionally corrected for light time
   and stellar aberration. All input and output vectors are
   expressed relative to an inertial reference frame.

   This routine supersedes spkapp_c.

   SPICE users normally should call the high-level API routines
   spkezr_c or spkez_c rather than this routine.

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

   SPK

-Keywords

   EPHEMERIS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"
   #undef spkaps_c


   void spkaps_c ( SpiceInt           targ,
                   SpiceDouble        et,
                   ConstSpiceChar   * ref,
                   ConstSpiceChar   * abcorr,
                   ConstSpiceDouble   stobs [6],
                   ConstSpiceDouble   accobs[3],
                   SpiceDouble        starg [6],
                   SpiceDouble      * lt,
                   SpiceDouble      * dlt      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   targ       I   Target body.
   et         I   Observer epoch.
   ref        I   Inertial reference frame of output state.
   abcorr     I   Aberration correction flag.
   stobs      I   State of the observer relative to the SSB.
   accobs     I   Acceleration of the observer relative to the SSB.
   starg      O   State of target.
   lt         O   One way light time between observer and target.
   dlt        O   Derivative of light time with respect to time.

-Detailed_Input

   targ        is the NAIF ID code for a target body. The target
               and observer define a state vector whose position
               component points from the observer to the target.

   et          is the ephemeris time, expressed as seconds past
               J2000 TDB, at which the state of the target body
               relative to the observer is to be computed.  `et'
               refers to time at the observer's location.

   ref         is the inertial reference frame with respect to which
               the input state `stobs', the input acceleration `accobs',
               and the output state `starg' are expressed. `ref' must be
               recognized by the SPICE Toolkit. The acceptable
               frames are listed in the Frames Required Reading, as
               well as in the CSPICE routine chgirf_.

               Case and blanks are not significant in the string
               `ref'.

   abcorr      indicates the aberration corrections to be applied to
               the state of the target body to account for one-way
               light time and stellar aberration. See the discussion in
               the -Particulars section of spkezr_c for recommendations
               on how to choose aberration corrections.

               `abcorr' may be any of the following:

                  "NONE"     Apply no correction. Return the
                             geometric state of the target body
                             relative to the observer.

               The following values of `abcorr' apply to the
               "reception" case in which photons depart from the
               target's location at the light-time corrected epoch
               et-lt and *arrive* at the observer's location at `et':

                  "LT"       Correct for one-way light time (also
                             called "planetary aberration") using a
                             Newtonian formulation. This correction
                             yields the state of the target at the
                             moment it emitted photons arriving at
                             the observer at `et'.

                             The light time correction uses an
                             iterative solution of the light time
                             equation (see -Particulars for details).
                             The solution invoked by the "LT" option
                             uses one iteration.

                  "LT+S"     Correct for one-way light time and
                             stellar aberration using a Newtonian
                             formulation. This option modifies the
                             state obtained with the "LT" option to
                             account for the observer's velocity
                             relative to the solar system
                             barycenter. The result is the apparent
                             state of the target---the position and
                             velocity of the target as seen by the
                             observer.

                  "CN"       Converged Newtonian light time
                             correction. In solving the light time
                             equation, the "CN" correction iterates
                             until the solution converges (three
                             iterations on all supported platforms).
                             Whether the "CN+S" solution is
                             substantially more accurate than the
                             "LT" solution depends on the geometry
                             of the participating objects and on the
                             accuracy of the input data. In all
                             cases this routine will execute more
                             slowly when a converged solution is
                             computed. See the -Particulars section of
                             spkezr_c for a discussion of precision of
                             light time corrections.

                  "CN+S"     Converged Newtonian light time
                             correction and stellar aberration
                             correction.

               The following values of `abcorr' apply to the
               "transmission" case in which photons *depart* from
               the observer's location at `et' and arrive at the
               target's location at the light-time corrected epoch
               et+lt:

                  "XLT"      "Transmission" case: correct for
                             one-way light time using a Newtonian
                             formulation. This correction yields the
                             state of the target at the moment it
                             receives photons emitted from the
                             observer's location at `et'.

                  "XLT+S"    "Transmission" case: correct for
                             one-way light time and stellar
                             aberration using a Newtonian
                             formulation  This option modifies the
                             state obtained with the "XLT" option to
                             account for the observer's velocity
                             relative to the solar system
                             barycenter. The position component of
                             the computed target state indicates the
                             direction that photons emitted from the
                             observer's location must be "aimed" to
                             hit the target.

                  "XCN"      "Transmission" case: converged
                             Newtonian light time correction.

                  "XCN+S"    "Transmission" case: converged
                             Newtonian light time correction and
                             stellar aberration correction.


               Neither special nor general relativistic effects are
               accounted for in the aberration corrections applied
               by this routine.

               Case and blanks are not significant in the string
               `abcorr'.

   stobs       is the geometric state of the observer relative
               to the solar system barycenter at `et'. The
               target and observer define a state vector whose
               position component points from the observer to the
               target. `stobs' is expressed relative to the reference
               frame designated by `ref'.

   accobs      is the geometric acceleration of the observer
               relative to the solar system barycenter at `et'. This
               is the derivative with respect to time of the
               velocity portion of `stobs'. `accobs' is expressed
               relative to the reference frame designated by `ref'.

               `accobs' is used for computing stellar aberration
               corrected velocity. If stellar aberration corrections
               are not specified by `abcorr', `accobs' is ignored; the
               caller need not provide a valid input value in this
               case.

-Detailed_Output

   starg       is a Cartesian state vector representing the position
               and velocity of the target body relative to the
               specified observer. `starg' is corrected for the
               specified aberration, and is expressed with respect
               to the specified inertial reference frame. The first
               three components of `starg' represent the x-, y- and
               z-components of the target's position; last three
               components form the corresponding velocity vector.

               The position component of `starg' points from the
               observer's location at `et' to the aberration-corrected
               location of the target. Note that the sense of the
               position vector is independent of the direction of
               radiation travel implied by the aberration
               correction.

               Units are always km and km/sec.

   lt          is the one-way light time between the observer and
               target in seconds. If the target state is corrected
               for light time, then `lt' is the one-way light time
               between the observer and the light time-corrected
               target location.

   dlt         is the derivative with respect to barycentric
               dynamical time of the one way light time between
               target and observer:

                  dlt = d(lt)/d(et)

               `dlt' can also be described as the rate of change of
               one way light time. `dlt' is unitless, since `lt' and
               `et' both have units of TDB seconds.

               If the observer and target are at the same position,
               then `dlt' is set to zero.

-Parameters

   None.

-Exceptions

   1)  If the value of `abcorr' is not recognized, an error is signaled
       by a routine in the call tree of this routine.

   2)  If `abcorr' calls for stellar aberration but not light
       time corrections, the error SPICE(NOTSUPPORTED) is
       signaled by a routine in the call tree of this routine.

   3)  If `abcorr' calls for relativistic light time corrections, the
       error SPICE(NOTSUPPORTED) is signaled by a routine in the call
       tree of this routine.

   4)  If the reference frame requested is not a recognized
       inertial reference frame, the error SPICE(BADFRAME)
       is signaled by a routine in the call tree of this routine.

   5)  If the state of the target relative to the solar system
       barycenter cannot be computed, an error is signaled by a
       routine in the call tree of this routine.

   6)  If the observer and target are at the same position,
       then `dlt' is set to zero. This situation could arise,
       for example, when the observer is Mars and the target
       is the Mars barycenter.

   7)  If any of the `ref' or `abcorr' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   8)  If any of the `ref' or `abcorr' input strings has zero length,
       the error SPICE(EMPTYSTRING) is signaled.

-Files

   This routine computes states using SPK files that have been
   loaded into the SPICE system, normally via the kernel loading
   interface routine furnsh_c. Application programs typically load
   kernels once before this routine is called, for example during
   program initialization; kernels need not be loaded repeatedly.
   See the routine furnsh_c and the SPK and KERNEL Required Reading
   for further information on loading (and unloading) kernels.

   If any of the ephemeris data used to compute `starg' are expressed
   relative to a non-inertial frame in the SPK files providing those
   data, additional kernels may be needed to enable the reference
   frame transformations required to compute the state. Normally
   these additional kernels are PCK files or frame kernels. Any such
   kernels must already be loaded at the time this routine is
   called.

-Particulars

   This routine supports higher-level SPK API routines that can
   perform both light time and stellar aberration corrections.

   User applications normally will not need to call this routine
   directly. However, this routine can improve run-time efficiency
   in situations where many targets are observed from the same
   location at the same time. In such cases, the state and
   acceleration of the observer relative to the solar system
   barycenter need be computed only once per look-up epoch.

   When apparent positions, rather than apparent states, are
   required, consider using the high-level position-only API
   routines

      spkpos_c
      spkezp_c

   or the low-level, position-only analog of this routine

      spkapo_c

   In general, the position-only routines are more efficient.

   See the header of the routine spkezr_c for a detailed discussion
   of aberration corrections.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Look up a sequence of states of the Moon as seen from the
      Earth. Use light time and stellar aberration corrections.
      Compute the first state for the epoch 2000 JAN 1 12:00:00 TDB;
      compute subsequent states at intervals of 1 hour. For each
      epoch, display the states, the one way light time between
      target and observer, and the rate of change of the one way
      light time.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: spkaps_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                     Contents
            ---------                     --------
            de418.bsp                     Planetary ephemeris
            pck00008.tpc                  Planet orientation and
                                          radii
            naif0008.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de418.bsp',
                                'pck00008.tpc',
                                'naif0008.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program spkaps_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"
      #include "SpiceZfc.h"

      int main()
      {
         /.
         Local constants

         The meta-kernel name shown here refers to a file whose contents
         are those shown above. This file and the kernels it references
         must exist in your current working directory.
         ./
         #define META                   "spkaps_ex1.tm"

         /.
         Use a time step of 1 hour; look up 5 states.
         ./
         #define STEP                   3600.0
         #define MAXITR                 5

         /.
         Local variables
         ./
         SpiceDouble             acc     [3];
         SpiceDouble             dlt;
         SpiceDouble             et;
         SpiceDouble             et0;
         SpiceDouble             lt;
         SpiceDouble             state   [6];
         SpiceDouble             state0  [6];
         SpiceDouble             state2  [6];
         SpiceDouble             stobs   [6];
         SpiceDouble             tdelta;

         SpiceInt                dim;
         SpiceInt                i;

         /.
         Load the SPK and LSK kernels via the meta-kernel.
         ./
         furnsh_c ( META );

         /.
         Convert the start time to seconds past J2000 TDB.
         ./
         str2et_c ( "2000 JAN 1 12:00:00 TDB", &et0 );

         /.
         Step through a series of epochs, looking up a
         state vector at each one.
         ./
         for ( i = 0;  i < MAXITR;  i++ )
         {
            et = et0 + i*STEP;

            /.
            Look up a state vector at epoch ET using the
            following inputs:

               Target:                 Moon (NAIF ID code 301)
               Reference frame:        J2000
               Aberration correction:  Light time and stellar
                                       aberration ('LT+S')
               Observer:               Earth (NAIF ID code 399)

            Before we can execute this computation, we'll need the
            geometric state and acceleration of the observer relative to
            the solar system barycenter at ET, expressed relative to the
            J2000 reference frame. First find the state:
            ./
            spkssb_c ( 399, et, "j2000", stobs );

            /.
            Next compute the acceleration. We numerically differentiate
            the velocity using a quadratic approximation.
            ./
            tdelta = 1.0;

            spkssb_c ( 399, et-tdelta, "j2000", state0 );
            spkssb_c ( 399, et+tdelta, "j2000", state2 );

            dim = 3;
            qderiv_c ( dim, state0+3, state2+3, tdelta, acc );

            /.
            Now compute the desired state vector:
            ./
            spkaps_c ( 301,   et,  "j2000", "lt+s",
                       stobs, acc, state,   &lt,    &dlt );

            printf( "et = %20.6f\n",                        et       );
            printf( "J2000 x-position (km):   %20.8f\n",    state[0] );
            printf( "J2000 y-position (km):   %20.8f\n",    state[1] );
            printf( "J2000 z-position (km):   %20.8f\n",    state[2] );
            printf( "J2000 x-velocity (km/s): %20.12f\n",   state[3] );
            printf( "J2000 y-velocity (km/s): %20.12f\n",   state[4] );
            printf( "J2000 z-velocity (km/s): %20.12f\n",   state[5] );
            printf( "One-way light time (s):  %20.12f\n",   lt       );
            printf( "Light time rate:         %20.08e\n\n", dlt      );
         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      et =             0.000000
      J2000 x-position (km):       -291584.61369498
      J2000 y-position (km):       -266693.40583163
      J2000 z-position (km):        -76095.65320924
      J2000 x-velocity (km/s):       0.643439157435
      J2000 y-velocity (km/s):      -0.666065873657
      J2000 z-velocity (km/s):      -0.301310063429
      One-way light time (s):        1.342310610325
      Light time rate:               1.07316909e-07

      et =          3600.000000
      J2000 x-position (km):       -289256.45942322
      J2000 y-position (km):       -269080.60545908
      J2000 z-position (km):        -77177.35277130
      J2000 x-velocity (km/s):       0.649970320169
      J2000 y-velocity (km/s):      -0.660148253293
      J2000 z-velocity (km/s):      -0.299630417907
      One-way light time (s):        1.342693954864
      Light time rate:               1.05652599e-07

      et =          7200.000000
      J2000 x-position (km):       -286904.89654240
      J2000 y-position (km):       -271446.41676468
      J2000 z-position (km):        -78252.96553362
      J2000 x-velocity (km/s):       0.656443883155
      J2000 y-velocity (km/s):      -0.654183552046
      J2000 z-velocity (km/s):      -0.297928532945
      One-way light time (s):        1.343071311734
      Light time rate:               1.03990457e-07

      et =         10800.000000
      J2000 x-position (km):       -284530.13302757
      J2000 y-position (km):       -273790.67111559
      J2000 z-position (km):        -79322.41170392
      J2000 x-velocity (km/s):       0.662859504730
      J2000 y-velocity (km/s):      -0.648172246851
      J2000 z-velocity (km/s):      -0.296204558469
      One-way light time (s):        1.343442689069
      Light time rate:               1.02330665e-07

      et =         14400.000000
      J2000 x-position (km):       -282132.37807792
      J2000 y-position (km):       -276113.20159697
      J2000 z-position (km):        -80385.61203056
      J2000 x-velocity (km/s):       0.669216846492
      J2000 y-velocity (km/s):      -0.642114815280
      J2000 z-velocity (km/s):      -0.294458644904
      One-way light time (s):        1.343808095656
      Light time rate:               1.00673404e-07


-Restrictions

   1)  This routine should not be used to compute geometric states.
       Instead, use spkezr_c, spkez_c, or spkgeo_c. spkgeo_c, which is called
       by spkezr_c and spkez_c, introduces less round-off error when the
       observer and target have a common center that is closer to
       both objects than is the solar system barycenter.

   2)  The kernel files to be used by spkaps_c must be loaded
       (normally by the CSPICE kernel loader furnsh_c) before
       this routine is called.

   3)  Unlike most other SPK state computation routines, this
       routine requires that the output state be relative to an
       inertial reference frame.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.0, 01-NOV-2021 (JDR)

       Fixed size of "accobs" argument: it was 6 when it should be 3.

       Edited the header and meta-kernel in -Examples section to comply with
       NAIF standard. Updated code example comments.

       Updated code example to use qderiv_c instead of f2c'ed routine.

   -CSPICE Version 1.0.1, 07-JUL-2014 (NJB)

       Descriptions of aberration correction choices that include
       stellar aberration were missing. These have been added.
       Erroneous claim that stellar aberration specifiers (instances
       of "+S") in `abcorr' are ignored was deleted.

       Discussion of light time corrections was updated. Assertions
       that converged light time corrections are unlikely to be
       useful were removed.

   -CSPICE Version 1.0.0, 11-JAN-2008 (NJB)

-Index_Entries

   low-level aberration-corrected state computation
   low-level light time and stellar aberration correction

-&
*/

{ /* Begin spkaps_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "spkaps_c" );


   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkaps_c", ref    );
   CHKFSTR ( CHK_STANDARD, "spkaps_c", abcorr );


   spkaps_ ( (integer    *) &targ,
             (doublereal *) &et,
             (char       *) ref,
             (char       *) abcorr,
             (doublereal *) stobs,
             (doublereal *) accobs,
             (doublereal *) starg,
             (doublereal *) lt,
             (doublereal *) dlt,
             (ftnlen      ) strlen(ref),
             (ftnlen      ) strlen(abcorr) );



   chkout_c ( "spkaps_c" );

} /* End spkaps_c */
