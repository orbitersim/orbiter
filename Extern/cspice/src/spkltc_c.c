/*

-Procedure spkltc_c ( S/P Kernel, light time corrected state )

-Abstract

   Return the state (position and velocity) of a target body
   relative to an observer, optionally corrected for light time,
   expressed relative to an inertial reference frame.

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

   FRAMES
   SPK

-Keywords

   EPHEMERIS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"
   #undef spkltc_c


   void spkltc_c ( SpiceInt           targ,
                   SpiceDouble        et,
                   ConstSpiceChar   * ref,
                   ConstSpiceChar   * abcorr,
                   ConstSpiceDouble   stobs[6],
                   SpiceDouble        starg[6],
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
   starg      O   State of target.
   lt         O   One way light time between observer and target.
   dlt        O   Derivative of light time with respect to time.

-Detailed_Input

   targ        is the NAIF ID code for a target body. The target
               and observer define a state vector whose position
               component points from the observer to the target.

   et          is the ephemeris time, expressed as seconds past
               J2000 TDB, at which the state of the target body
               relative to the observer is to be computed. `et'
               refers to time at the observer's location.

   ref         is the inertial reference frame with respect to which
               the input state `stobs' and the output state `starg' are
               expressed. `ref' must be recognized by the CSPICE
               Toolkit. The acceptable frames are listed in the Frames
               Required Reading, as well as in the CSPICE routine
               chgirf_.

               Case and blanks are not significant in the string
               `ref'.


   abcorr      indicates the aberration corrections to be applied to
               the state of the target body to account for one-way
               light time. See the discussion in the -Particulars
               section for recommendations on how to choose
               aberration corrections.

               If `abcorr' includes the stellar aberration correction
               symbol "+S", this flag is simply ignored. Aside from
               the possible presence of this symbol, `abcorr' may be
               any of the following:

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

                             The light time correction involves
                             iterative solution of the light time
                             equation (see -Particulars for details).
                             The solution invoked by the "LT" option
                             uses one iteration.

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

                  "XCN"      "Transmission" case: converged
                             Newtonian light time correction.


               Neither special nor general relativistic effects are
               accounted for in the aberration corrections applied
               by this routine.

               Case and blanks are not significant in the string
               `abcorr'.


   stobs       is the geometric (uncorrected) state of the observer
               relative to the solar system barycenter at epoch `et'.
               `stobs' is a 6-vector: the first three components of
               `stobs' represent a Cartesian position vector; the last
               three components represent the corresponding velocity
               vector. `stobs' is expressed relative to the inertial
               reference frame designated by `ref'.

               Units are always km and km/sec.

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

   1)  For the convenience of the caller, the input aberration
       correction flag can call for stellar aberration correction via
       inclusion of the "+S" suffix. This portion of the aberration
       correction flag is ignored if present.

   2)  If the value of `abcorr' is not recognized, an error
       is signaled by a routine in the call tree of this
       routine.

   3)  If the reference frame requested is not a recognized
       inertial reference frame, the error SPICE(BADFRAME)
       is signaled by a routine in the call tree of this routine.

   4)  If the state of the target relative to the solar system
       barycenter cannot be computed, an error is signaled by a
       routine in the call tree of this routine.

   5)  If the observer and target are at the same position,
       then `dlt' is set to zero. This situation could arise,
       for example, when the observer is Mars and the target
       is the Mars barycenter.

   6)  If a division by zero error would occur in the computation of
       `dlt', the error SPICE(DIVIDEBYZERO) is signaled by a routine in
       the call tree of this routine.

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
   these additional kernels are PCK files or frame kernels. Any
   such kernels must already be loaded at the time this routine is
   called.

-Particulars

   This routine supports higher-level SPK API routines that can
   perform both light time and stellar aberration corrections.
   User applications normally will not need to call this routine
   directly.

   See the header of the routine spkezr_c for a detailed discussion
   of aberration corrections.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Look up a sequence of states of the Moon as seen from the
      Earth. Use light time corrections. Compute the first state for
      the epoch 2000 JAN 1 12:00:00 TDB; compute subsequent states at
      intervals of 1 hour. For each epoch, display the states, the
      one way light time between target and observer, and the rate of
      change of the one way light time.


      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: spkltc_ex1.tm

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
         Program spkltc_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants

         The meta-kernel name shown here refers to a file whose contents
         are those shown above. This file and the kernels it references
         must exist in your current working directory.
         ./
         #define META                   "spkltc_ex1.tm"

         /.
         Use a time step of 1 hour; look up 5 states.
         ./
         #define STEP                   3600.0
         #define MAXITR                 5

         /.
         Local variables
         ./
         SpiceDouble             dlt;
         SpiceDouble             et;
         SpiceDouble             et0;
         SpiceDouble             lt;
         SpiceDouble             state  [6];
         SpiceDouble             stobs  [6];
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
               Aberration correction:  Light time ('LT')
               Observer:               Earth (NAIF ID code 399)

            Before we can execute this computation, we'll need
            the geometric state of the observer relative to the
            solar system barycenter at ET, expressed relative
            to the J2000 reference frame:
            ./
            spkssb_c ( 399, et, "j2000", stobs );

            spkltc_c ( 301,   et,    "j2000", "lt",
                       stobs, state, &lt,   &dlt   );

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
      J2000 x-position (km):       -291569.26541283
      J2000 y-position (km):       -266709.18647826
      J2000 z-position (km):        -76099.15511876
      J2000 x-velocity (km/s):       0.643530613222
      J2000 y-velocity (km/s):      -0.666081817008
      J2000 z-velocity (km/s):      -0.301322831796
      One-way light time (s):        1.342310610325
      Light time rate:               1.07316909e-07

      et =          3600.000000
      J2000 x-position (km):       -289240.78128184
      J2000 y-position (km):       -269096.44087958
      J2000 z-position (km):        -77180.89972576
      J2000 x-velocity (km/s):       0.650062115201
      J2000 y-velocity (km/s):      -0.660162739217
      J2000 z-velocity (km/s):      -0.299642673906
      One-way light time (s):        1.342693954864
      Light time rate:               1.05652599e-07

      et =          7200.000000
      J2000 x-position (km):       -286888.88736709
      J2000 y-position (km):       -271462.30170548
      J2000 z-position (km):        -78256.55568214
      J2000 x-velocity (km/s):       0.656535991543
      J2000 y-velocity (km/s):      -0.654196576804
      J2000 z-velocity (km/s):      -0.297940273074
      One-way light time (s):        1.343071311734
      Light time rate:               1.03990457e-07

      et =         10800.000000
      J2000 x-position (km):       -284513.79173691
      J2000 y-position (km):       -273806.60031034
      J2000 z-position (km):        -79326.04318327
      J2000 x-velocity (km/s):       0.662951900546
      J2000 y-velocity (km/s):      -0.648183807097
      J2000 z-velocity (km/s):      -0.296215779371
      One-way light time (s):        1.343442689069
      Light time rate:               1.02330665e-07

      et =         14400.000000
      J2000 x-position (km):       -282115.70368389
      J2000 y-position (km):       -276129.16976799
      J2000 z-position (km):        -80389.28296571
      J2000 x-velocity (km/s):       0.669309503775
      J2000 y-velocity (km/s):      -0.642124908057
      J2000 z-velocity (km/s):      -0.294469343362
      One-way light time (s):        1.343808095656
      Light time rate:               1.00673404e-07


-Restrictions

   1)  The routine spkgeo_c should be used instead of this routine
       to compute geometric states. spkgeo_c introduces less
       round-off error when the observer and target have common
       center that is closer to both objects than is the solar
       system barycenter.

   2)  The kernel files to be used by spkltc_c must be loaded
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

   -CSPICE Version 1.0.2, 10-AUG-2021 (JDR)

       Edited the header and example's meta-kernel to comply with NAIF
       standard. Updated code example comments.

       Added frames.req to the list of -Required_Reading. Updated
       -Exceptions, -Restrictions and -Literature_References sections.

   -CSPICE Version 1.0.1, 07-JUL-2014 (NJB)

       Discussion of light time corrections was updated. Assertions
       that converged light time corrections are unlikely to be
       useful were removed.

   -CSPICE Version 1.0.0, 11-JAN-2008 (NJB)

-Index_Entries

   low-level light time correction
   light-time corrected state from SPK file
   get light-time corrected state

-&
*/

{ /* Begin spkltc_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "spkltc_c" );


   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkltc_c", ref    );
   CHKFSTR ( CHK_STANDARD, "spkltc_c", abcorr );


   spkltc_ ( (integer    *) &targ,
             (doublereal *) &et,
             (char       *) ref,
             (char       *) abcorr,
             (doublereal *) stobs,
             (doublereal *) starg,
             (doublereal *) lt,
             (doublereal *) dlt,
             (ftnlen      ) strlen(ref),
             (ftnlen      ) strlen(abcorr) );

   chkout_c ( "spkltc_c" );

} /* End spkltc_c */
