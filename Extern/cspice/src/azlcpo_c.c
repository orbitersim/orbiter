/*

-Procedure azlcpo_c ( AZ/EL, constant position observer state )

-Abstract

   Return the azimuth/elevation coordinates of a specified target
   relative to an "observer," where the observer has constant
   position in a specified reference frame. The observer's position
   is provided by the calling program rather than by loaded SPK
   files.

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

   COORDINATES
   EPHEMERIS

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void azlcpo_c ( ConstSpiceChar    * method,
                   ConstSpiceChar    * target,
                   SpiceDouble         et,
                   ConstSpiceChar    * abcorr,
                   SpiceBoolean        azccw,
                   SpiceBoolean        elplsz,
                   ConstSpiceDouble    obspos [3],
                   ConstSpiceChar    * obsctr,
                   ConstSpiceChar    * obsref,
                   SpiceDouble         azlsta [6],
                   SpiceDouble       * lt         )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   method     I   Method to obtain the surface normal vector.
   target     I   Name of target ephemeris object.
   et         I   Observation epoch.
   abcorr     I   Aberration correction.
   azccw      I   Flag indicating how azimuth is measured.
   elplsz     I   Flag indicating how elevation is measured.
   obspos     I   Observer position relative to center of motion.
   obsctr     I   Center of motion of observer.
   obsref     I   Body-fixed, body-centered frame of observer's
                  center.
   azlsta     O   State of target with respect to observer,
                  in azimuth/elevation coordinates.
   lt         O   One way light time between target and
                  observer.

-Detailed_Input

   method      is a short string providing parameters defining the
               computation method to be used to obtain the surface
               normal vector that defines the local zenith. Parameters
               include, but are not limited to, the shape model used to
               represent the body's surface of observer's center of
               motion.

               The only choice currently supported is

                  "ELLIPSOID"        The intercept computation uses
                                     a triaxial ellipsoid to model
                                     the body's surface of the
                                     observer's center of motion.
                                     The ellipsoid's radii must be
                                     available in the kernel pool.

               Neither case nor white space are significant in
               `method'. For example, the string " eLLipsoid " is
               valid.

               In a later Toolkit release, this argument will be
               used to invoke a wider range of surface
               representations. For example, it will be possible to
               represent the target body's surface using a digital
               shape model.

   target      is the name of a target body. Optionally, you may supply
               the ID code of the object as an integer string. For
               example, both "EARTH" and "399" are legitimate strings
               to supply to indicate the target is Earth.

               Case and leading and trailing blanks are not significant
               in the string `target'.

   et          is the ephemeris time at which the state of the
               target relative to the observer is to be computed. `et'
               is expressed as seconds past J2000 TDB. `et' refers to
               time at the observer's location.

   abcorr      is a short string that indicates the aberration
               corrections to be applied to the observer-target state
               to account for one-way light time and stellar
               aberration.

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
                             Newtonian light time and stellar
                             aberration corrections.


               Neither special nor general relativistic effects are
               accounted for in the aberration corrections applied
               by this routine.

               Case and leading and trailing blanks are not
               significant in the string `abcorr'.

   azccw       is a flag indicating how the azimuth is measured.

               If `azccw' is SPICETRUE, the azimuth increases in the
               counterclockwise direction; otherwise it increases
               in the clockwise direction.

   elplsz      is a flag indicating how the elevation is measured.

               If `elplsz' is SPICETRUE, the elevation increases from
               the XY plane toward +Z; otherwise toward -Z.

   obspos      is the fixed (constant) geometric position of an
               observer relative to its center of motion `obsctr',
               expressed in the reference frame `obsref'.

               `obspos' does not need to be located on the surface of
               the object centered at `obsctr'.

               Units are always km.

   obsctr      is the name of the center of motion of `obspos'. The
               ephemeris of `obsctr' is provided by loaded SPK files.

               Optionally, you may supply the integer ID code for the
               object as an integer string. For example both "MOON" and
               "301" are legitimate strings that indicate the moon is
               the center of motion.

               Case and leading and trailing blanks are not significant
               in the string `obsctr'.

   obsref      is the name of the body-fixed, body-centered reference
               frame associated with the observer's center of motion,
               relative to which the input position `obspos' is
               expressed. The observer has constant position relative
               to its center of motion in this reference frame.

               Case and leading and trailing blanks are not significant
               in the string `obsref'.

-Detailed_Output

   azlsta      is a state vector representing the position and
               velocity of the target relative to the specified
               observer, corrected for the specified aberrations
               and expressed in azimuth/elevation coordinates. The
               first three components of `azlsta' represent the range,
               azimuth and elevation of the target's position; the
               last three components form the corresponding velocity
               vector:

                  azlsta = ( r, az, el, dr/dt, daz/dt, del/dt )

               The position component of `azlsta' points from the
               observer's location at `et' to the aberration-corrected
               location of the target. Note that the sense of the
               position vector is independent of the direction of
               radiation travel implied by the aberration correction.

               The velocity component of `azlsta' is the derivative with
               respect to time of the position component of `azlsta'.

               Azimuth, elevation and its derivatives are measured with
               respect to the axes of the local topocentric reference
               frame. See the -Particulars section for the definition
               of this reference frame.

               The azimuth is the angle between the projection onto the
               local topocentric principal (X-Y) plane of the vector
               from the observer's position to the target and the
               principal axis of the reference frame. The azimuth is
               zero on the +X axis.

               The elevation is the angle between the vector from the
               observer's position to the target and the local
               topocentric principal plane. The elevation is zero on
               the plane.

               Units are km for `r', radians for `az' and `el', km/sec for
               dr/dt, and radians/sec for daz/dt and del/dt. The range
               of `az' is [0, 2*pi] and the range of `el' is [-pi/2, pi/2].

               The way azimuth and elevation are measured depend
               respectively on the value of the logical flags `azccw' and
               `elplsz'. See the description of these input arguments for
               details.

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

   2)  If the reference frame `obsref' is not recognized, the error
       SPICE(UNKNOWNFRAME) is signaled by a routine in the call tree
       of this routine. A frame name may fail to be recognized
       because a required frame specification kernel has not been
       loaded; another cause is a misspelling of the frame name.

   3)  If the reference frame `obsref' is not centered at the
       observer's center of motion `obsctr', the error
       SPICE(INVALIDFRAME) is signaled by a routine in the call tree
       of this routine.

   4)  If the radii of the center of motion body are not available
       from the kernel pool, an error is signaled by a routine in
       the call tree of this routine.

   5)  If the size of the `obsctr' body radii kernel variable is not
       three, an error is signaled by a routine in the call tree of
       this routine.

   6)  If any of the three `obsctr' body radii is less-than or equal to
       zero, an error is signaled by a routine in the call tree of
       this routine.

   7)  If the ratio of the longest to the shortest
       radii is large enough so that arithmetic expressions
       involving its squared value may overflow, an error is
       signaled by a routine in the call tree of this routine.

   8)  If the radii of the center of motion body and the axes of
       `obspos' have radically different magnitudes so that arithmetic
       overflow may occur during the computation of the nearest
       point of the observer on the center of motion's reference
       ellipsoid, an error is signaled by a routine in the call tree
       of this routine. Note that even if there is no overflow, if
       the ratios of the radii lengths, or the ratio of the
       magnitude of `obspos' and the shortest radius vary by many
       orders of magnitude, the results may have poor precision.

   9)  If the computation `method' is not recognized, the error
       SPICE(INVALIDMETHOD) is signaled by a routine in the call tree
       of this routine.

   10) If the loaded kernels provide insufficient data to compute
       the requested state vector, an error is signaled by a routine
       in the call tree of this routine.

   11) If an error occurs while reading an SPK or other kernel file,
       the error  is signaled by a routine in the call tree of this
       routine.

   12) If the aberration correction `abcorr' is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   13) If `target' is on the Z-axis ( x = 0 and y = 0 ) of the local
       topocentric frame centered at `obspos', an error is signaled by
       a routine in the call tree of this routine. See item 2 in the
       -Restrictions section for further details.

   14) If any of the `method', `target', `abcorr', `obsctr' or
       `obsref' input string pointers is null, the error
       SPICE(NULLPOINTER) is signaled.

   15) If any of the `method', `target', `abcorr', `obsctr' or
       `obsref' input strings has zero length, the error
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

   -  Shape and orientation data: if the computation method is
      specified as "Ellipsoid," triaxial radii for the center body
      must be loaded into the kernel pool. Typically this is done by
      loading a text PCK file via furnsh_c. Additionally, rotation
      data for the body-fixed, body-centered frame associated with
      the observer's center of motion must be loaded. These may be
      provided in a text or binary PCK file. In some cases these
      data may be provided by a CK file.

   The following data may be required:

   -  Frame data: if a frame definition not built into SPICE is
      required, for example to convert the observer-target state
      to the body-fixed body-centered frame, that definition
      must be available in the kernel pool. Typically frame
      definitions are supplied by loading a frame kernel using
      furnsh_c.

   -  Additional kernels: if a CK frame is used in this routine's
      state computation, then at least one CK and corresponding SCLK
      kernel is required. If dynamic frames are used, additional
      SPK, PCK, CK, or SCLK kernels may be required.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   This routine computes azimuth/elevation coordinates of a target
   as seen from an observer whose trajectory is not provided by SPK
   files.

   Observers supported by this routine must have constant position
   with respect to a specified center of motion, expressed in a
   caller-specified reference frame. The state of the center of
   motion relative to the target must be computable using
   loaded SPK data.

   This routine is suitable for computing the azimuth/elevation
   coordinates and its derivatives of target ephemeris
   objects, as seen from landmarks on the surface of an extended
   object, in cases where no SPK data are available for those
   landmarks.

   The azimuth/elevation coordinates are defined with respect to
   the observer's local topocentric reference frame. This frame is
   generally defined as follows:

   -  the +Z axis is aligned with the surface normal outward
      vector at the observer's location;

   -  the +X axis is aligned with the component of the +Z axis
      of the body-fixed reference frame orthogonal to the
      outward normal vector, i.e. the +X axis points towards
      the body's North pole;

   -  the +Y axis completes the right-handed system.

   For observers located on the +Z axis of the body-fixed frame
   designated by `obsref', the following definition of the local
   topocentric reference frame is used by this routine:

   -  the +Z axis is aligned with the surface normal outward
      vector at the observer's location;

   -  the +X axis aligned with the +X axis of the body-fixed
      reference frame;

   -  the +Y axis completes the right-handed system.

   In both cases, the origin of the local topocentric frame is
   the observer's location.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the azimuth/elevation state of Venus as seen from the
      DSS-14 station at a given epoch first using the position of
      the station given as a vector in the ITRF93 frame and then
      using the data provided in the kernel pool for the DSS-14
      station.


      Task description
      ================

      In this example, we will obtain the apparent state of Venus as
      seen from DSS-14 station in the DSS-14 topocentric reference
      frame. For this computation, we'll use the DSS-14 station's
      location given as a vector in the ITRF93 frame.

      Then we will compute same apparent state using spkpos_c to
      obtain a Cartesian state vector, after which we will transform
      the vector coordinates to azimuth, elevation and range and
      their derivatives using recazl_c and dazldr_c.

      In order to introduce the usage of the logical flags `azccw'
      and `elplsz', we will request the azimuth to be measured
      clockwise and the elevation positive towards the +Z
      axis of the DSS-14_TOPO reference frame.

      Results from the two computations will not agree exactly
      because of time-dependent differences in the orientation,
      relative to the ITRF93 frame, of the topocentric frame centered
      at DSS-14. This orientation varies with time due to movement of
      the station, which is affected by tectonic plate motion. The
      computation using azlcpo_c evaluates the orientation of this
      frame using the station location at the observation epoch,
      while the spkpos_c computation uses the orientation provided by
      the station frame kernel. The latter is fixed and is derived
      from the station location at an epoch specified in the
      documentation of that kernel.


      Kernels
      =======

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: azlcpo_ex1.tm

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
            de430.bsp                        Planetary ephemeris
            naif0011.tls                     Leapseconds
            pck00010.tpc                     Planetary constants
            earth_720101_070426.bpc          Earth historical
                                                binary PCK
            earthstns_itrf93_050714.bsp      DSN station SPK
            earth_topo_050714.tf             DSN station FK

         \begindata

         KERNELS_TO_LOAD = ( 'de430.bsp',
                             'naif0011.tls',
                             'pck00010.tpc',
                             'earth_720101_070426.bpc',
                             'earthstns_itrf93_050714.bsp',
                             'earth_topo_050714.tf'         )

         \begintext

         End of meta-kernel.


      Example code begins here.


      /.
         Program azlcpo_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define META         "azlcpo_ex1.tm"

         /.
         Local variables
         ./
         SpiceChar          * abcorr;
         SpiceChar          * obs;
         SpiceChar          * obsctr;
         SpiceChar          * obsref;
         SpiceChar          * obstim;
         SpiceChar          * method;
         SpiceChar          * ref;
         SpiceChar          * target;

         SpiceDouble          az;
         SpiceDouble          azlsta [6];
         SpiceDouble          azlvel [3];
         SpiceDouble          el;
         SpiceDouble          et;
         SpiceDouble          jacobi [3][3];
         SpiceDouble          lt;
         SpiceDouble          state  [6];
         SpiceDouble          obspos [3];
         SpiceDouble          r;

         SpiceBoolean         azccw;
         SpiceBoolean         elplsz;

         /.
         Load SPICE kernels.
         ./
         furnsh_c ( META );

         /.
         Convert the observation time to seconds past J2000 TDB.
         ./
         obstim = "2003 Jan 01 00:00:00 TDB";

         str2et_c ( obstim, &et );

         /.
         Set the method, target, center of motion of the observer,
         frame of observer position, and aberration corrections.
         ./
         method = "ELLIPSOID";
         target = "VENUS";
         obsctr = "EARTH";
         obsref = "ITRF93";
         abcorr = "CN+S";

         /.
         Set the position of DSS-14 relative to the earth's
         center at the observation epoch, expressed in the
         ITRF93 reference frame. Values come from the
         earth station SPK specified in the meta-kernel.

         The actual station velocity is non-zero due
         to tectonic plate motion; we ignore the motion
         in this example.
         ./
         obspos[0] =  -2353.621419700;
         obspos[1] =  -4641.341471700;
         obspos[2] =   3677.052317800;

         /.
         We want the azimuth/elevation coordinates to be measured
         with the azimuth increasing clockwise and the
         elevation positive towards +Z axis of the local
         topocentric reference frame
         ./
         azccw  = SPICEFALSE;
         elplsz = SPICETRUE;

         azlcpo_c ( method, target, et,     abcorr, azccw, elplsz,
                    obspos, obsctr, obsref, azlsta, &lt );

         /.
         In order to check the results obtained using azlcpo_c
         we are going to compute the same azimuth/elevation state
         using the position of DSS-14 and its local topocentric
         reference frame "DSS-14_TOPO" from the kernel pool.
         ./
         obs    = "DSS-14";
         ref    = "DSS-14_TOPO";

         /.
         Compute the observer-target state.
         ./
         spkezr_c ( target, et, ref, abcorr, obs, state, &lt );

         /.
         Convert the position to azimuth/elevation coordinates.
         ./
         recazl_c ( state, azccw, elplsz, &r, &az, &el );

         /.
         Convert velocity to azimuth/elevation coordinates.
         ./
         dazldr_c ( state[0], state[1], state[2], azccw, elplsz, jacobi );

         mxv_c ( jacobi, state+3, azlvel );

         printf( "\n" );
         printf( "AZ/EL coordinates (from azlcpo_c):\n" );
         printf( "\n" );
         printf( "   Range     (km)         =  %19.8f\n", azlsta[0] );
         printf( "   Azimuth   (deg)        =  %19.8f\n",
                                      azlsta[1] * dpr_c() );
         printf( "   Elevation (deg)        =  %19.8f\n",
                                      azlsta[2] * dpr_c() );
         printf( "\n" );
         printf( "AZ/EL coordinates (using kernels):\n" );
         printf( "\n" );
         printf( "   Range     (km)         =  %19.8f\n", r );
         printf( "   Azimuth   (deg)        =  %19.8f\n", az * dpr_c() );
         printf( "   Elevation (deg)        =  %19.8f\n", el * dpr_c() );
         printf( "\n" );
         printf( "AZ/EL velocity (from azlcpo_c):\n" );
         printf( "\n" );
         printf( "   d Range/dt    (km/s)   =  %19.8f\n", azlsta[3] );
         printf( "   d Azimuth/dt  (deg/s)  =  %19.8f\n",
                                      azlsta[4] * dpr_c() );
         printf( "   d Elevation/dt (deg/s) =  %19.8f\n",
                                      azlsta[5] * dpr_c() );
         printf( "\n" );
         printf( "AZ/EL velocity (using kernels):\n" );
         printf( "\n" );
         printf( "   d Range/dt     (km/s)  =  %19.8f\n", azlvel[0] );
         printf( "   d Azimuth/dt   (deg/s) =  %19.8f\n",
                                      azlvel[1] * dpr_c() );
         printf( "   d Elevation/dt (deg/s) =  %19.8f\n",
                                      azlvel[2] * dpr_c() );
         printf( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      AZ/EL coordinates (from azlcpo_c):

         Range     (km)         =    89344802.82679011
         Azimuth   (deg)        =         269.04481881
         Elevation (deg)        =         -25.63088321

      AZ/EL coordinates (using kernels):

         Range     (km)         =    89344802.82679011
         Azimuth   (deg)        =         269.04481846
         Elevation (deg)        =         -25.63088278

      AZ/EL velocity (from azlcpo_c):

         d Range/dt    (km/s)   =          13.41734176
         d Azimuth/dt  (deg/s)  =           0.00238599
         d Elevation/dt (deg/s) =          -0.00339644

      AZ/EL velocity (using kernels):

         d Range/dt     (km/s)  =          13.41734176
         d Azimuth/dt   (deg/s) =           0.00238599
         d Elevation/dt (deg/s) =          -0.00339644


      Note the discrepancy in the AZ/EL coordinates found by the two
      computation methods. Please refer to the task description for
      an explanation.

-Restrictions

   1)  This routine may not be suitable for work with stars or other
       objects having large distances from the observer, due to loss
       of precision in position vectors.

   2)  The Jacobian matrix of the transformation from rectangular to
       azimuth/elevation coordinates has a singularity for points
       located on the Z-axis ( x = 0 and y = 0 ) of the local
       topocentric frame centered at `obspos'; therefore the
       derivative of the azimuth/elevation coordinates cannot be
       computed for those points.

       A user who wishes to compute the azimuth/elevation
       coordinates, without their derivatives, of `target' as seen
       from `obspos' at the input time `et', for those cases when `target'
       is located along the local topocentric Z-axis, could do so by
       executing the following calls:

          spkcpo_c ( target, et,     obsref, "OBSERVER", abcorr,
                     obspos, obsctr, obsref, state,      &lt );

          range = vnorm_c ( state );

       By definition, the azimuth is zero and the elevation is
       either pi/2 if `elplsz' is SPICETRUE, or -pi/2 otherwise.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 01-NOV-2021 (JDR)

-Index_Entries

   AZ/EL_coordinates relative to constant_position_observer
   AZ/EL_coordinates w.r.t. constant_position surface_point
   AZ/EL_coordinates relative to surface_point extended_object
   AZ/EL_coordinates relative to landmark on extended_object

-&
*/

{ /* Begin azlcpo_c */

   /*
   Local variables.
   */
   logical            logCazccw;
   logical            logCelplsz;

   /*
   Participate in error tracing.
   */
   chkin_c ( "azlcpo_c" );

   /*
   Check the input string arguments:

      method
      target
      abcorr
      obsctr
      obsref

   Make sure each pointer is non-null and each string contains
   at least one data character: that is, one character
   preceding the null terminator.
   */
   CHKFSTR ( CHK_STANDARD, "azlcpo_c", method );
   CHKFSTR ( CHK_STANDARD, "azlcpo_c", target );
   CHKFSTR ( CHK_STANDARD, "azlcpo_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "azlcpo_c", obsctr );
   CHKFSTR ( CHK_STANDARD, "azlcpo_c", obsref );

   /*
   Get a type logical copy of the `azccw' flag.
   */
   logCazccw = azccw;

   /*
   Get a type logical copy of the `elplsz' flag.
   */
   logCelplsz = elplsz;

   /*
   Call the f2c'd Fortran routine.
   */
   azlcpo_ (  ( char       * )  method,
              ( char       * )  target,
              ( doublereal * ) &et,
              ( char       * )  abcorr,
              ( logical    * ) &logCazccw,
              ( logical    * ) &logCelplsz,
              ( doublereal * )  obspos,
              ( char       * )  obsctr,
              ( char       * )  obsref,
              ( doublereal * )  azlsta,
              ( doublereal * )  lt,
              ( ftnlen       )  strlen(method),
              ( ftnlen       )  strlen(target),
              ( ftnlen       )  strlen(abcorr),
              ( ftnlen       )  strlen(obsctr),
              ( ftnlen       )  strlen(obsref)  );

   chkout_c ( "azlcpo_c" );

} /* End azlcpo_c */
