/*

-Procedure spkcpo_c ( SPK, constant position observer state )

-Abstract

   Return the state of a specified target relative to an "observer,"
   where the observer has constant position in a specified reference
   frame. The observer's position is provided by the calling program
   rather than by loaded SPK files.

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
   PCK
   SPK
   TIME

-Keywords

   EPHEMERIS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef   spkcpo_c

   void spkcpo_c ( ConstSpiceChar       * target,
                   SpiceDouble            et,
                   ConstSpiceChar       * outref,
                   ConstSpiceChar       * refloc,
                   ConstSpiceChar       * abcorr,
                   ConstSpiceDouble       obspos [3],
                   ConstSpiceChar       * obsctr,
                   ConstSpiceChar       * obsref,
                   SpiceDouble            state  [6],
                   SpiceDouble          * lt         )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   target     I   Name of target ephemeris object.
   et         I   Observation epoch.
   outref     I   Reference frame of output state.
   refloc     I   Output reference frame evaluation locus.
   abcorr     I   Aberration correction.
   obspos     I   Observer position relative to center of motion.
   obsctr     I   Center of motion of observer.
   obsref     I   Frame of observer position.
   state      O   State of target with respect to observer.
   lt         O   One way light time between target and
                  observer.

-Detailed_Input

   target      is the name of a target body. Optionally, you may
               supply the ID code of the object as an integer
               string. For example, both "EARTH" and "399" are
               legitimate strings to supply to indicate the target
               is Earth.

               Case and leading and trailing blanks are not
               significant in the string `target'.


   et          is the ephemeris time at which the state of the
               target relative to the observer is to be computed. `et'
               is expressed as seconds past J2000 TDB. `et' refers to
               time at the observer's location.


   outref      is the name of the reference frame with respect to
               which the output state is expressed.

               When `outref' is time-dependent (non-inertial), its
               orientation relative to the J2000 frame is evaluated
               in the manner commanded by the input argument `refloc'
               (see description below).

               Case and leading and trailing blanks are not
               significant in the string `outref'.


   refloc      is a string indicating the output reference frame
               evaluation locus: this is the location associated
               with the epoch at which this routine is to evaluate
               the orientation, relative to the J2000 frame, of the
               output frame `outref'. The values and meanings of
               `refloc' are:

                  "OBSERVER"  Evaluate `outref' at the observer's
                              epoch `et'.

                              Normally the locus "OBSERVER" should
                              be selected when `outref' is centered
                              at the observer.


                  "TARGET"    Evaluate `outref' at the target epoch;
                              letting `lt' be the one-way light time
                              between the target and observer, the
                              target epoch is

                                 et-lt  if reception aberration
                                        corrections are used

                                 et+lt  if transmission aberration
                                        corrections are used

                                 et     if no aberration corrections
                                        are used

                              Normally the locus "TARGET" should
                              be selected when `outref' is centered
                              at the target object.


                  "CENTER"    Evaluate the frame `outref' at the epoch
                              associated its center. This epoch,
                              which we'll call `etctr', is determined
                              as follows:

                                 Let `ltctr' be the one-way light time
                                 between the observer and the center
                                 of `outref'. Then `etctr' is

                                    et-ltctr  if reception
                                              aberration corrections
                                              are used

                                    et+ltctr  if transmission
                                              aberration corrections
                                              are used

                                    et        if no aberration
                                              corrections are used


                              The locus "CENTER" should be selected
                              when the user intends to obtain
                              results compatible with those produced
                              by spkezr_c.

               When `outref' is inertial, all choices of `refloc'
               yield the same results.

               Case and leading and trailing blanks are not
               significant in the string `refloc'.


   abcorr      indicates the aberration corrections to be applied to
               the observer-target state to account for one-way
               light time and stellar aberration.

               `abcorr' may be any of the following:

                  "NONE"     Apply no correction. Return the
                             geometric state of the target
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
                             equation. The solution invoked by the
                             "LT" option uses one iteration.

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
                             until the solution converges.

                  "CN+S"     Converged Newtonian light time
                             and stellar aberration corrections.


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
                             formulation. This option modifies the
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
                             Newtonian light time and stellar
                             aberration corrections.


               Neither special nor general relativistic effects are
               accounted for in the aberration corrections applied
               by this routine.

               Case and leading and trailing blanks are not
               significant in the string `abcorr'.


   obspos      is the fixed (constant) geometric position of an
               observer relative to its center of motion `obsctr',
               expressed in the reference frame `obsref'.

               Units are always km.


   obsctr      is the name of the center of motion of `obspos'. The
               ephemeris of `obsctr' is provided by loaded SPK files.

               Optionally, you may supply the integer ID code for
               the object as an integer string. For example both
               "MOON" and "301" are legitimate strings that indicate
               the moon is the center of motion.

               Case and leading and trailing blanks are not
               significant in the string `obsctr'.


   obsref      is the name of the reference frame relative to which
               the input position `obspos' is expressed. The observer
               has constant position relative to its center of
               motion in this reference frame.

               Case and leading and trailing blanks are not
               significant in the string `obsref'.

-Detailed_Output

   state       is a Cartesian state vector representing the position
               and velocity of the target relative to the specified
               observer. `state' is corrected for the specified
               aberrations and is expressed with respect to the
               reference frame specified by `outref'. The first three
               components of `state' represent the x-, y- and
               z-components of the target's position; the last three
               components form the corresponding velocity vector.

               The position component of `state' points from the
               observer's location at `et' to the aberration-corrected
               location of the target. Note that the sense of the
               position vector is independent of the direction of
               radiation travel implied by the aberration
               correction.

               The velocity component of `state' is the derivative
               with respect to time of the position component of
               `state'.

               Units are always km and km/sec.

               When `state' is expressed in a time-dependent
               (non-inertial) output frame, the orientation of that
               frame relative to the J2000 frame is evaluated in the
               manner indicated by the input argument `refloc' (see
               description above).


   lt          is the one-way light time between the observer and
               target in seconds. If the target state is corrected
               for aberrations, then `lt' is the one-way light time
               between the observer and the light time corrected
               target location.

-Parameters

   None.

-Exceptions

   1)  If either the name of the center of motion or the target
       cannot be translated to its NAIF ID code, an error is signaled
       by a routine in the call tree of this routine.

   2)  If the reference frame `outref' is unrecognized, an error is
       signaled by a routine in the call tree of this routine.

   3)  If the reference frame `obsref' is unrecognized, an error is
       signaled by a routine in the call tree of this routine.

   4)  If the frame evaluation locus `refloc' is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   5)  If the loaded kernels provide insufficient data to compute
       the requested state vector, an error is signaled
       by a routine in the call tree of this routine.

   6)  If an error occurs while reading an SPK or other kernel file,
       the error  is signaled by a routine in the call tree of
       this routine.

   7)  If the aberration correction `abcorr' is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   8)  If any of the `target', `outref', `refloc', `abcorr',
       `obsctr', `obsref' or `obspos' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   9)  If any of the `target', `outref', `refloc', `abcorr', `obsctr'
       or `obsref' input strings has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   Appropriate kernels must be loaded by the calling program before
   this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for the observer center and target
      must be loaded. If aberration corrections are used, the
      states of the observer center and target relative to the
      solar system barycenter must be calculable from the
      available ephemeris data. Typically ephemeris data are made
      available by loading one or more SPK files using furnsh_c.

   The following data may be required:

   -  PCK data: if the target frame is a PCK frame, rotation data
      for the target frame must be loaded. These may be provided
      in a text or binary PCK file.

   -  Frame data: if a frame definition not built into SPICE is
      required, for example to convert the observer-target state
      to the output frame, that definition must be available in
      the kernel pool. Typically frame definitions are supplied
      by loading a frame kernel using furnsh_c.

   -  Additional kernels: if any frame used in this routine's
      state computation is a CK frame, then at least one CK and
      corresponding SCLK kernel is required. If dynamic frames
      are used, additional SPK, PCK, CK, or SCLK kernels may be
      required.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   This routine computes observer-target states for observers whose
   trajectories are not provided by SPK files.

   Observers supported by this routine must have constant position
   with respect to a specified center of motion, expressed in a
   caller-specified reference frame. The state of the center of
   motion relative to the target must be computable using
   loaded SPK data.

   For applications in which the observer has constant, non-zero
   velocity relative to its center of motion, the CSPICE routine

      spkcvo_c     { SPK, constant velocity observer state }

   can be used.

   This routine is suitable for computing states of target ephemeris
   objects, as seen from landmarks on the surface of an extended
   object, in cases where no SPK data are available for those
   landmarks.

   This routine's treatment of the output reference frame differs
   from that of the principal SPK API routines

      spkezr_c
      spkez_c
      spkpos_c
      spkezp_c

   which require both observer and target ephemerides to be provided
   by loaded SPK files:

      The SPK API routines listed above evaluate the orientation of
      the output reference frame (with respect to the J2000 frame)
      at an epoch corrected for one-way light time between the
      observer and the center of the output frame. When the center
      of the output frame is not the target (for example, when the
      target is on the surface of Mars and the output frame is
      centered at Mars' center), the epoch of evaluation may not
      closely match the light-time corrected epoch associated with
      the target itself. A similar problem may occur when the
      observer is a surface point on an extended body and the
      output frame is centered at the body center: the listed
      routines will correct the orientation of the output frame for
      one-way light time between the frame center and the observer.

      This routine allows the caller to dictate how the orientation
      of the output reference frame is to be evaluated. The caller
      passes to this routine an input string called the output
      frame's evaluation "locus." This string specifies the location
      associated with the output frame's evaluation epoch. The three
      possible values of the locus are

         "TARGET"
         "OBSERVER"
         "CENTER"

      The choice of locus has an effect when aberration corrections
      are used and the output frame is non-inertial.

      When the locus is "TARGET" and light time corrections are
      used, the orientation of the output frame is evaluated at the
      epoch obtained by correcting the observation epoch `et' for
      one-way light time `lt'. The evaluation epoch will be either
      et-lt or et+lt for reception or transmission corrections
      respectively.

      For remote sensing applications where the target is a surface
      point on an extended object, and the orientation of that
      object should be evaluated at the emission time, the locus
      "TARGET" should be used.

      When the output frame's orientation should be evaluated at
      the observation epoch `et', which is the case when the
      output frame is centered at the observer, the locus
      "OBSERVER" should be used.

      The locus option "CENTER" is provided for compatibility
      with existing SPK state computation APIs such as spkezr_c.

      Note that the output frame evaluation locus does not affect
      the computation of light time between the target and
      observer.


   The SPK routines that compute observer-target states for
   combinations of objects having ephemerides provided by the SPK
   system and objects having constant position or constant velocity
   are

      spkcpo_c {SPK, Constant position observer}
      spkcpt_c {SPK, Constant position target}
      spkcvo_c {SPK, Constant velocity observer}
      spkcvt_c {SPK, Constant velocity target}

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Compute apparent solar azimuth and elevation as seen from a
      specified surface point on the earth.

      Task Description
      ================

      In this example we'll use the location of the DSN station
      DSS-14 as our surface point.

      We'll perform the solar azimuth and elevation computation two
      ways:

         - Using a station frame kernel to provide the
           specification of a topocentric reference frame
           centered at DSS-14.

         - Computing inline the transformation from the earth-fixed,
           earth-centered frame ITRF93 to a topocentric frame
           centered at DSS-14.

      Note that results of the two computations will differ
      slightly. There are three sources of the differences:

         1) The station position is time-dependent due to tectonic
            plate motion, and epochs of the station positions used
            to specify the axes of the topocentric frame are
            different in the two cases. This gives rise to different
            orientations of the frame's axes relative to the frame
            ITRF93.

         2) The two computations use different earth radii; this
            results in computation of different geodetic latitudes
            of the station. This difference also affects the
            topocentric frame orientation relative to ITRF93.

         3) The station movement between ET and the epoch at which
            the DSS-14_TOPO frame is specified contributes a very
            small offset---on the order of 10 cm---to the station-sun
            position vector, expressed in the ITRF93 frame.


      Kernels
      =======

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: spkcpo_ex1.tm

         This is the meta-kernel file for the header code example for
         the subroutine spkcpo_c. These kernel files can be found on
         the NAIF website.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                        Contents
            ---------                        --------
            de421.bsp                        Planetary ephemeris
            pck00010.tpc                     Planet orientation and
                                             radii
            naif0010.tls                     Leapseconds
            earth_720101_070426.bpc          Earth historical
                                             binary PCK
            earthstns_itrf93_050714.bsp      DSN station SPK
            earth_topo_050714.tf             DSN station FK
            mgs_moc_v20.ti                   MGS MOC instrument
                                             parameters
            mgs_sclkscet_00061.tsc           MGS SCLK coefficients
            mgs_sc_ext12.bc                  MGS s/c bus attitude
            mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris

         \begindata

         KERNELS_TO_LOAD = ( 'de421.bsp',
                             'pck00010.tpc',
                             'naif0010.tls',
                             'earth_720101_070426.bpc',
                             'earthstns_itrf93_050714.bsp',
                             'earth_topo_050714.tf',
                             'mgs_moc_v20.ti',
                             'mgs_sclkscet_00061.tsc',
                             'mgs_sc_ext12.bc',
                             'mgs_ext12_ipng_mgs95j.bsp'  )

         \begintext

         End of meta-kernel.


      Example code begins here.


      /.
         Program spkcpo_ex1

         This program uses spkcpo_c to compute solar azimuth
         and elevation at a given surface point on the earth.
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define  META     "spkcpo_ex1.tm"
         #define  TIMFMT   "YYYY MON DD HR:MN:SC.###### UTC"
         #define  TIMLEN   41

         /.
         Local variables
         ./
         SpiceChar             * abcorr;
         SpiceChar               emitim  [ TIMLEN ];
         SpiceChar             * refloc;
         SpiceChar             * obsctr;
         SpiceChar             * obsref;
         SpiceChar             * obstim;
         SpiceChar             * outref;
         SpiceChar             * target;

         SpiceDouble             az;
         SpiceDouble             el;
         SpiceDouble             et;
         SpiceDouble             f;
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             lt0;
         SpiceDouble             lt1;
         SpiceDouble             normal [ 3 ] ;
         SpiceDouble             obsalt;
         SpiceDouble             obslat;
         SpiceDouble             obslon;
         SpiceDouble             obspos [ 3 ];
         SpiceDouble             r;
         SpiceDouble             radii  [ 3 ];
         SpiceDouble             re;
         SpiceDouble             rp;
         SpiceDouble             state0 [ 6 ];
         SpiceDouble             state1 [ 6 ];
         SpiceDouble             topvec [ 3 ];
         SpiceDouble             xform  [ 3 ][ 3 ];

         SpiceDouble             z [ 3 ]  = { 0.0, 0.0, 1.0 };

         SpiceInt                n;


         /.
         Load SPICE kernels.
         ./
         furnsh_c ( META );

         /.
         Convert the observation time to seconds past J2000 TDB.
         ./
         obstim = "2003 OCT 13 06:00:00.000000 UTC";

         str2et_c ( obstim, &et );

         /.
         Set the target, observer center, and observer frame.
         ./
         target = "SUN";
         obsctr = "EARTH";
         obsref = "ITRF93";

         /.
         Set the position of DSS-14 relative to the earth's
         center at the J2000 epoch, expressed in the
         ITRF93 reference frame. Values come from the
         earth station SPK specified in the meta-kernel.

         The actual station velocity is non-zero due
         to tectonic plate motion; we ignore the motion
         in this example. See the routine spkcvo_c for an
         example in which the plate motion is accounted for.
         ./
         obspos[0] =  -2353.6213656676991;
         obspos[1] =  -4641.3414911499403;
         obspos[2] =   3677.0523293197439;

         /.
         Find the apparent state of the sun relative
         to the station in the DSS-14_TOPO reference frame.
         Evaluate the output frame's orientation, that is the
         orientation of the DSS-14_TOPO frame relative to the
         J2000 frame, at the observation epoch. This
         correction is obtained by setting `refloc' to
         "OBSERVER".
         ./

         outref = "DSS-14_TOPO";
         abcorr = "CN+S";

         refloc = "OBSERVER";

         /.
         Compute the observer-target state.
         ./
         spkcpo_c ( target, et,     outref, refloc,
                    abcorr, obspos, obsctr,
                    obsref, state0, &lt0            );

         /.
         Compute planetocentric coordinates of the
         observer-target position in the local
         topocentric reference frame DSS-14_TOPO.
         ./
         reclat_c ( state0, &r, &lon, &lat );

         /.
         Compute solar azimuth. The latitude we've
         already computed is the elevation. Express
         both angles in degrees.
         ./
         el =   lat * dpr_c();
         az = - lon * dpr_c();

         if ( az < 0.0 )
         {
            az +=  360.0;
         }

         /.
         Display the computed state, light time. and angles.
         ./
         timout_c ( et-lt0, TIMFMT, TIMLEN, emitim );

         printf ( "\n"
                  " Frame evaluation locus:     %s\n"
                  "\n"
                  " Target:                     %s\n"
                  " Observation time:           %s\n"
                  " Observer center:            %s\n"
                  " Observer frame:             %s\n"
                  " Emission time:              %s\n"
                  " Output reference frame:     %s\n"
                  " Aberration correction:      %s\n"
                  "\n"
                  " Observer-target position (km):\n"
                  " %20.8f %20.8f %20.8f\n"
                  " Observer-target velocity (km/s):\n"
                  " %20.8f %20.8f %20.8f\n"
                  " Light time (s):        %20.8f\n",

                  refloc,    target,    obstim,    obsctr,
                  obsref,    emitim,    outref,    abcorr,
                  state0[0], state0[1], state0[2],
                  state0[3], state0[4], state0[5], lt0   );

         printf ( "\n"
                  " Solar azimuth (deg):   %20.8f\n"
                  " Solar elevation (deg): %20.8f\n",
                  az, el                             );


         /.
         For an arbitrary surface point, we might not
         have a frame kernel available. In this case
         we can look up the state in the observer frame
         using spkcpo_c and then convert the state to
         the local topocentric frame. We'll first
         create the transformation matrix for converting
         vectors in the observer frame to the topocentric
         frame.

         First step: find the geodetic (planetodetic)
         coordinates of the observer. We need the
         equatorial radius and flattening coefficient
         of the reference ellipsoid.
         ./
         bodvrd_c ( "EARTH", "RADII", 3, &n, radii );

         re = radii[0];
         rp = radii[2];

         f  = ( re - rp ) / re;

         recgeo_c ( obspos, re, f, &obslon, &obslat, &obsalt );

         /.
         Find the outward surface normal on the reference
         ellipsoid at the observer's longitude and latitude.
         ./
         latrec_c ( 1.0, obslon, obslat, normal );

         /.
         The topocentric frame has its +Z axis aligned
         with NORMAL and its +X axis pointed north.
         The north direction is aligned with the component
         of the ITRF93 +Z axis orthogonal to the topocentric
         +Z axis.
         ./
         twovec_c ( normal, 3, z, 1, xform );

         outref = "ITRF93";
         abcorr = "CN+S";

         refloc = "OBSERVER";

         /.
         Compute the observer-target state.
         ./
         spkcpo_c ( target, et,     outref, refloc,
                    abcorr, obspos, obsctr,
                    obsref, state1, &lt1            );
         /.
         Convert the position to the topocentric frame.
         ./
         mxv_c ( xform, state1, topvec );

         /.
         Compute azimuth and elevation.
         ./
         reclat_c ( topvec, &r, &lon, &lat );

         el =   lat * dpr_c();
         az = - lon * dpr_c();

         if ( az < 0.0 )
         {
            az +=  360.0;
         }

         printf ( "\n\n\n"
                  " AZ/EL computed without frame kernel:\n\n"
                  " Distance between last two "
                  "positions (km): %20.8f\n",
                  vdist_c( state0, topvec )   );

         printf ( "\n"
                  " Solar azimuth (deg):   %20.8f\n"
                  " Solar elevation (deg): %20.8f\n"
                  "\n",
                  az, el                             );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       Frame evaluation locus:     OBSERVER

       Target:                     SUN
       Observation time:           2003 OCT 13 06:00:00.000000 UTC
       Observer center:            EARTH
       Observer frame:             ITRF93
       Emission time:              2003 OCT 13 05:51:42.068322 UTC
       Output reference frame:     DSS-14_TOPO
       Aberration correction:      CN+S

       Observer-target position (km):
          62512272.82074845    58967494.42513601  -122059095.46751881
       Observer-target velocity (km/s):
              2475.97326517       -9870.26706232       -3499.90809969
       Light time (s):                497.93167797

       Solar azimuth (deg):           316.67141599
       Solar elevation (deg):         -54.85253168



       AZ/EL computed without frame kernel:

       Distance between last two positions (km):           3.07056970

       Solar azimuth (deg):           316.67141786
       Solar elevation (deg):         -54.85253216


   2) Demonstrate applications of the output frame evaluation locus.

      The following program is not necessarily realistic: for
      brevity, it combines several unrelated computations.

      Task Description
      ================

      Find the state of the Mars Global Surveyor spacecraft, as seen
      from a given surface point on earth, corrected for light time
      and stellar aberration, expressed in the earth fixed reference
      frame ITRF93. The surface point is the position of the DSN
      station DSS-14.

      Contrast the states computed by setting the output frame
      evaluation locus to "OBSERVER" and to "CENTER". Show that the
      latter choice produces results very close to those that
      can be obtained using spkezr_c.

      Also compute the central meridian longitude on Mars of DSS-14.
      This computation performs aberration corrections for the center
      of Mars.

      Note that in general, the routine subpnt_c should be used for
      sub-observer point computations when high-accuracy aberration
      corrections are desired.

      The observation epoch is 2003 OCT 13 06:00:00 UTC.


      Kernels
      =======

      Use the meta-kernel of example 1 above.


      Example code begins here.


      /.
         Program spkcpo_ex2

         This program demonstrates the use of spkcpo_c.
         Computations are performed using all three possible
         values of the output frame evaluation locus `refloc':

            "OBSERVER"
            "CENTER"
            "TARGET"

         Several unrelated computations are performed in this
         routine. In particular, of the central meridian
         longitude on Mars is included simply to demonstrate
         use of the TARGET" option.
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define  META     "spkcpo_ex1.tm"
         #define  TIMFMT   "YYYY MON DD HR:MN:SC.###### UTC"
         #define  TIMLEN   41

         /.
         Local variables
         ./
         SpiceChar             * abcorr;
         SpiceChar               emitim  [ TIMLEN ];
         SpiceChar             * refloc;
         SpiceChar             * obsctr;
         SpiceChar             * obsref;
         SpiceChar             * obsrvr;
         SpiceChar             * obstim;
         SpiceChar             * outref;
         SpiceChar             * target;

         SpiceDouble             et;
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             lt0;
         SpiceDouble             lt1;
         SpiceDouble             lt2;
         SpiceDouble             lt3;
         SpiceDouble             obspos [ 3 ];
         SpiceDouble             obsvec [ 3 ];
         SpiceDouble             r;
         SpiceDouble             state0 [ 6 ];
         SpiceDouble             state1 [ 6 ];
         SpiceDouble             state2 [ 6 ];
         SpiceDouble             state3 [ 6 ];

         /.
         Load SPICE kernels.
         ./
         furnsh_c ( META );

         /.
         Convert the observation time to seconds past J2000 TDB.
         ./
         obstim = "2003 OCT 13 06:00:00.000000 UTC";

         str2et_c ( obstim, &et );

         /.
         Set the target, observer center, and observer frame.
         ./
         target = "MGS";
         obsctr = "EARTH";
         obsref = "ITRF93";

         /.
         Set the position of DSS-14 relative to the earth's
         center at the J2000 epoch, expressed in the
         ITRF93 reference frame. Values come from the
         earth station SPK specified in the meta-kernel.

         The actual station velocity is non-zero due
         to tectonic plate motion; we ignore the motion
         in this example. See the routine SPKCVO for an
         example in which the plate motion is accounted for.
         ./
         obspos[0] =  -2353.6213656676991;
         obspos[1] =  -4641.3414911499403;
         obspos[2] =   3677.0523293197439;

         /.
         Find the apparent state of the spacecraft relative
         to the station in the ITRF93 reference frame.
         Evaluate the earth's orientation, that is the
         orientation of the ITRF93 frame relative to the
         J2000 frame, at the observation epoch. This
         correction is obtained by setting `refloc' to
         "OBSERVER".
         ./

         outref = "ITRF93";
         abcorr = "CN+S";

         refloc = "OBSERVER";

         /.
         Compute the observer-target state.
         ./
         spkcpo_c ( target, et,     outref, refloc,
                    abcorr, obspos, obsctr,
                    obsref, state0, &lt0            );

         /.
         Display the computed state and light time.
         ./
         timout_c ( et-lt0, TIMFMT, TIMLEN, emitim );

         printf ( "\n"
                  " Frame evaluation locus:     %s\n"
                  "\n"
                  " Target:                     %s\n"
                  " Observation time:           %s\n"
                  " Observer center:            %s\n"
                  " Observer frame:             %s\n"
                  " Emission time:              %s\n"
                  " Output reference frame:     %s\n"
                  " Aberration correction:      %s\n"
                  "\n"
                  " Observer-target position (km):\n"
                  " %20.8f %20.8f %20.8f\n"
                  " Observer-target velocity (km/s):\n"
                  " %20.8f %20.8f %20.8f\n"
                  " Light time (s):   %20.8f\n",

                  refloc,    target,    obstim,    obsctr,
                  obsref,    emitim,    outref,
                  abcorr,    state0[0], state0[1], state0[2],
                  state0[3], state0[4], state0[5], lt0       );

         /.
         Repeat the computation, this time evaluating the
         earth's orientation at the epoch obtained by
         subtracting from the observation time the one way
         light time from the earth's center.

         This is equivalent to looking up the observer-target
         state using spkezr_c.
         ./
         refloc = "CENTER";

         spkcpo_c ( target, et,     outref, refloc,
                    abcorr, obspos, obsctr,
                    obsref, state1, &lt1            );

         /.
         Display the computed state and light time.
         ./
         timout_c ( et-lt1, TIMFMT, TIMLEN, emitim );

         printf ( "\n\n"
                  " Frame evaluation locus:     %s\n"
                  "\n"
                  " Target:                     %s\n"
                  " Observation time:           %s\n"
                  " Observer center:            %s\n"
                  " Observer frame:             %s\n"
                  " Emission time:              %s\n"
                  " Output reference frame:     %s\n"
                  " Aberration correction:      %s\n"
                  "\n"
                  " Observer-target position (km):\n"
                  " %20.8f %20.8f %20.8f\n"
                  " Observer-target velocity (km/s):\n"
                  " %20.8f %20.8f %20.8f\n"
                  " Light time (s):   %20.8f\n",

                  refloc,    target,    obstim,    obsctr,
                  obsref,    emitim,    outref,
                  abcorr,    state1[0], state1[1], state1[2],
                  state1[3], state1[4], state1[5], lt1      );

         printf ( "\n"
                  " Distance between above positions (km):"
                  "    %20.8f\n"
                  " Velocity difference magnitude  (km/s):"
                  "    %20.8f\n",
                  vdist_c( state0,   state1   ),
                  vdist_c( state0+3, state1+3 )                     );

         /.
         Check: compare the state computed directly above
         to one produced by spkezr_c:
         ./
         obsrvr = "DSS-14";

         spkezr_c ( target,  et,      outref,  abcorr,
                    obsrvr,  state2,  &lt2            );

         printf ( "\n\n"
                  " State computed using spkezr_c:\n"
                  "\n"
                  " Target:                 %s\n"
                  " Observation time:       %s\n"
                  " Output reference frame: %s\n"
                  " Aberration correction:  %s\n"
                  " Observer:               %s\n"
                  "\n"
                  " Observer-target position (km):\n"
                  " %20.8f %20.8f %20.8f\n"
                  " Observer-target velocity (km/s):\n"
                  " %20.8f %20.8f %20.8f\n"
                  " Light time (s): %20.8f\n",

                  target,    obstim,    outref,
                  abcorr,    obsrvr,
                  state2[0], state2[1], state2[2],
                  state2[3], state2[4], state2[5], lt2   );

         printf ( "\n"
                  " Distance between last two "
                  "positions (km): %20.8f\n"
                  " Velocity difference magnitude    "
                  " (km/s): %20.8f\n",
                  vdist_c( state1,   state2   ),
                  vdist_c( state1+3, state2+3 )          );

         /.
         Finally, compute an observer-target state in
         a frame centered at the target.
         This state can be used to compute the sub-observer
         longitude. The reference frame is the Mars-fixed
         frame IAU_MARS.
         ./

         target = "MARS";
         outref = "IAU_MARS";

         refloc = "TARGET";

         spkcpo_c ( target, et,     outref, refloc,
                    abcorr, obspos, obsctr,
                    obsref, state3, &lt3            );

         /.
         Central meridian longitude is the longitude of the
         observer relative to the target center, so we must
         negate the position portion of the state we just
         computed.
         ./
         vminus_c ( state3, obsvec );

         reclat_c ( obsvec, &r, &lon, &lat );

         printf ( "\n\n"
                  " Frame evaluation locus:     %s\n"
                  "\n"
                  " Target:                     %s\n"
                  " Observation time:           %s\n"
                  " Observer center:            %s\n"
                  " Observer frame:             %s\n"
                  " Emission time:              %s\n"
                  " Output reference frame:     %s\n"
                  " Aberration correction:      %s\n"
                  "\n"
                  " Observer-target position (km):\n"
                  " %20.8f %20.8f %20.8f\n"
                  " Observer-target velocity (km/s):\n"
                  " %20.8f %20.8f %20.8f\n"
                  " Light time (s):   %20.8f\n",

                  refloc,    target,    obstim,    obsctr,
                  obsref,    emitim,    outref,
                  abcorr,    state3[0], state3[1], state3[2],
                  state3[3], state3[4], state3[5], lt3   );

         printf ( "\n"
                  " Central meridian\n"
                  " longitude (deg):  %20.8f\n\n\n",
                  lon * dpr_c()                       );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       Frame evaluation locus:     OBSERVER

       Target:                     MGS
       Observation time:           2003 OCT 13 06:00:00.000000 UTC
       Observer center:            EARTH
       Observer frame:             ITRF93
       Emission time:              2003 OCT 13 05:55:44.201144 UTC
       Output reference frame:     ITRF93
       Aberration correction:      CN+S

       Observer-target position (km):
         -53720675.37947631   -51381249.05335969   -18838416.34718024
       Observer-target velocity (km/s):
             -3751.69274754        3911.73417167          -2.17503628
       Light time (s):           255.79885530


       Frame evaluation locus:     CENTER

       Target:                     MGS
       Observation time:           2003 OCT 13 06:00:00.000000 UTC
       Observer center:            EARTH
       Observer frame:             ITRF93
       Emission time:              2003 OCT 13 05:55:44.201144 UTC
       Output reference frame:     ITRF93
       Aberration correction:      CN+S

       Observer-target position (km):
         -53720595.74385086   -51381332.31464963   -18838416.34738571
       Observer-target velocity (km/s):
             -3751.69880992        3911.72835653          -2.17503628
       Light time (s):           255.79885530

       Distance between above positions (km):            115.21404099
       Velocity difference magnitude  (km/s):              0.00840050


       State computed using spkezr_c:

       Target:                 MGS
       Observation time:       2003 OCT 13 06:00:00.000000 UTC
       Output reference frame: ITRF93
       Aberration correction:  CN+S
       Observer:               DSS-14

       Observer-target position (km):
         -53720595.74378239   -51381332.31467460   -18838416.34737090
       Observer-target velocity (km/s):
             -3751.69880992        3911.72835653          -2.17503628
       Light time (s):         255.79885530

       Distance between last two positions (km):           0.00007437
       Velocity difference magnitude     (km/s):           0.00000000


       Frame evaluation locus:     TARGET

       Target:                     MARS
       Observation time:           2003 OCT 13 06:00:00.000000 UTC
       Observer center:            EARTH
       Observer frame:             ITRF93
       Emission time:              2003 OCT 13 05:55:44.201144 UTC
       Output reference frame:     IAU_MARS
       Aberration correction:      CN+S

       Observer-target position (km):
         -71445232.12770155     2312773.74175659    27766441.52048387
       Observer-target velocity (km/s):
               155.65895286        5061.78618477           5.09447030
       Light time (s):           255.79702283

       Central meridian
       longitude (deg):           -1.85409037


-Restrictions

   1)  This routine may not be suitable for work with stars or other
       objects having large distances from the observer, due to loss
       of precision in position vectors.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   S.C. Krening        (JPL)
   B.V. Semenov        (JPL)

-Version

   -CSPICE Version 1.0.2, 05-AUG-2021 (JDR)

       Edited the header to comply to NAIF standard.

       Modified code examples output format for the solutions to fit
       within the -Examples section without modifications. Removed
       unnecessary code from examples.

   -CSPICE Version 1.0.1, 08-SEP-2015 (NJB)

       The -Exceptions section of the header was updated
       to mention exceptions involving null pointers and
       empty input strings.

   -CSPICE Version 1.0.0, 27-MAR-2012 (NJB) (SCK) (BVS)

-Index_Entries

   state relative to constant_position_observer
   state relative to constant_position surface_point
   state relative to surface_point on extended_object
   state relative to landmark on extended_object

-&
*/

{ /* Begin spkcpo_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "spkcpo_c" );

   /*
   Check the input state pointer.
   */
   CHKPTR ( CHK_STANDARD, "spkcpo_c", obspos );

   /*
   Check the input strings.
   */
   CHKFSTR ( CHK_STANDARD, "spkcpo_c", target );
   CHKFSTR ( CHK_STANDARD, "spkcpo_c", outref );
   CHKFSTR ( CHK_STANDARD, "spkcpo_c", refloc );
   CHKFSTR ( CHK_STANDARD, "spkcpo_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "spkcpo_c", obsctr );
   CHKFSTR ( CHK_STANDARD, "spkcpo_c", obsref );

   /*
   Check the output pointers.
   */
   CHKPTR ( CHK_STANDARD, "spkcpo_c", state );
   CHKPTR ( CHK_STANDARD, "spkcpo_c", lt    );

   /*
   Let the f2c'd routine do the work.
   */
   spkcpo_ ( ( char       * ) target,
             ( doublereal * ) &et,
             ( char       * ) outref,
             ( char       * ) refloc,
             ( char       * ) abcorr,
             ( doublereal * ) obspos,
             ( char       * ) obsctr,
             ( char       * ) obsref,
             ( doublereal * ) state,
             ( doublereal * ) lt,
             ( ftnlen       ) strlen(target),
             ( ftnlen       ) strlen(outref),
             ( ftnlen       ) strlen(refloc),
             ( ftnlen       ) strlen(abcorr),
             ( ftnlen       ) strlen(obsctr),
             ( ftnlen       ) strlen(obsref)  );

   chkout_c ( "spkcpo_c" );

} /* End spkcpo_c */
