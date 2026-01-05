/*

-Procedure srfxpt_c ( Surface intercept point )

-Abstract

   Deprecated: This routine has been superseded by the CSPICE
   routine sincpt_c. This routine is supported for purposes of
   backward compatibility only.

   Given an observer and a direction vector defining a ray, compute
   the surface intercept point of the ray on a target body at a
   specified epoch, optionally corrected for light time and stellar
   aberration.

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
   NAIF_IDS
   PCK
   SPK
   TIME

-Keywords

   GEOMETRY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef   srfxpt_c


   void srfxpt_c ( ConstSpiceChar      * method,
                   ConstSpiceChar      * target,
                   SpiceDouble           et,
                   ConstSpiceChar      * abcorr,
                   ConstSpiceChar      * obsrvr,
                   ConstSpiceChar      * dref,
                   ConstSpiceDouble      dvec   [3],
                   SpiceDouble           spoint [3],
                   SpiceDouble         * dist,
                   SpiceDouble         * trgepc,
                   SpiceDouble           obspos [3],
                   SpiceBoolean        * found      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   method     I   Computation method.
   target     I   Name of target body.
   et         I   Epoch in ephemeris seconds past J2000 TDB.
   abcorr     I   Aberration correction.
   obsrvr     I   Name of observing body.
   dref       I   Reference frame of input direction vector.
   dvec       I   Ray's direction vector.
   spoint     O   Surface intercept point on the target body.
   dist       O   Distance from the observer to the intercept point.
   trgepc     O   Intercept epoch.
   obspos     O   Observer position relative to target center.
   found      O   Flag indicating whether intercept was found.

-Detailed_Input

   method      is a short string providing parameters defining
               the computation method to be used. Parameters
               include, but are not limited to, the shape model
               used to represent the surface of the target body.

               The only choice currently supported is

                  "Ellipsoid"        The intercept computation uses
                                     a triaxial ellipsoid to model
                                     the surface of the target body.
                                     The ellipsoid's radii must be
                                     available in the kernel pool.

               Neither case nor white space are significant in
               `method'. For example, the string " eLLipsoid " is
               valid.

               In a later Toolkit release, this argument will be
               used to invoke a wider range of surface
               representations. For example, it will be possible to
               represent the target body's surface using a digital
               model.

   target      is the name of the target body.  `target' is
               case-insensitive, and leading and trailing blanks in
               `target' are not significant. Optionally, you may supply
               a string containing the integer ID code for the object.
               For example both "MOON" and "301" are legitimate strings
               that indicate the moon is the target body.

               When the target body's surface is represented by a
               tri-axial ellipsoid, this routine assumes that a kernel
               variable representing the ellipsoid's radii is present
               in the kernel pool. Normally the kernel variable would
               be defined by loading a PCK file.

   et          is the epoch of participation of the observer, expressed
               as ephemeris seconds past J2000 TDB: `et' is the epoch
               at which the observer's state is computed.

               When aberration corrections are not used, `et' is also
               the epoch at which the state and orientation of the
               target body are computed.

               When aberration corrections are used, `et' is the epoch
               at which the observer's state relative to the solar
               system barycenter is computed; in this case the position
               and orientation of the target body are computed at et-lt
               or et+lt, where `lt' is the one-way light time between
               the intercept point and the observer, and the sign
               applied to lt depends on the selected correction. See
               the description of `abcorr' below for details.

   abcorr      indicates the aberration correction to be applied
               when computing the observer-target state and the
               orientation of the target body.  `abcorr' may be any of
               the following.

                  "NONE"     Apply no correction. Return the
                             geometric surface intercept point on the
                             target body.

               Let `lt' represent the one-way light time between the
               observer and the surface intercept point (note: NOT
               between the observer and the target body's center).
               The following values of `abcorr' apply to the
               "reception" case in which photons depart from the
               intercept point's location at the light-time
               corrected epoch et-lt and *arrive* at the observer's
               location at `et':

                  "LT"       Correct for one-way light time (also
                             called "planetary aberration") using a
                             Newtonian formulation. This correction
                             yields the location of the surface
                             intercept point at the moment it
                             emitted photons arriving at the
                             observer at `et'.

                             The light time correction uses an
                             iterative solution of the light time
                             equation. The solution invoked by the
                             "LT" option uses one iteration.

                             Both the target state as seen by the
                             observer, and rotation of the target
                             body, are corrected for light time.

                  "LT+S"     Correct for one-way light time and
                             stellar aberration using a Newtonian
                             formulation. This option modifies the
                             state obtained with the "LT" option to
                             account for the observer's velocity
                             relative to the solar system
                             barycenter. The result is the apparent
                             surface intercept point as seen by the
                             observer.

                  "CN"       Converged Newtonian light time
                             correction. In solving the light time
                             equation, the "CN" correction iterates
                             until the solution converges. Both the
                             state and rotation of the target body
                             are corrected for light time.

                  "CN+S"     Converged Newtonian light time
                             and stellar aberration corrections.

               The following values of `abcorr' apply to the
               "transmission" case in which photons *depart* from
               the observer's location at `et' and arrive at the
               intercept point at the light-time corrected epoch
               et+lt:

                  "XLT"      "Transmission" case: correct for
                             one-way light time using a Newtonian
                             formulation. This correction yields the
                             intercept location at the moment it
                             receives photons emitted from the
                             observer's location at `et'.

                             The light time correction uses an
                             iterative solution of the light time
                             equation. The solution invoked by the
                             "LT" option uses one iteration.

                             Both the target state as seen by the
                             observer, and rotation of the target
                             body, are corrected for light time.

                  "XLT+S"    "Transmission" case: correct for
                             one-way light time and stellar
                             aberration using a Newtonian
                             formulation  This option modifies the
                             intercept obtained with the "XLT"
                             option to account for the observer's
                             velocity relative to the solar system
                             barycenter.

                  "XCN"      Converged Newtonian light time
                             correction. This is the same as "XLT"
                             correction but with further iterations
                             to a converged Newtonian light time
                             solution.

                  "XCN+S"    "Transmission" case: converged
                             Newtonian light time and stellar
                             aberration corrections.

   obsrvr      is the name of the observing body. This is typically
               a spacecraft, the earth, or a surface point on the
               earth.  `obsrvr' is case-insensitive, and leading and
               trailing blanks in `obsrvr' are not significant.
               Optionally, you may supply a string containing the
               integer ID code for the object. For example both
               "EARTH" and "399" are legitimate strings that indicate
               the earth is the observer.

   dref        is the name of the reference frame relative to which the
               input direction vector is expressed. This may be any
               frame supported by the SPICE system, including built-in
               frames (documented in the Frames Required Reading) and
               frames defined by a loaded frame kernel (FK).

               When `dref' designates a non-inertial frame, the
               orientation of the frame is evaluated at an epoch
               dependent on the frame's center and, if the center is
               not the observer, on the selected aberration
               correction. See the description of the direction
               vector `dvec' for details.

   dvec        is a pointing vector emanating from the observer. The
               intercept with the target body's surface of the ray
               defined by the observer and `dvec' is sought.

               `dvec' is specified relative to the reference frame
               designated by `dref'.

               Non-inertial reference frames are treated as follows:
               if the center of the frame is at the observer's
               location, the frame is evaluated at `et'. If the
               frame's center is located elsewhere, then letting
               `ltcent' be the one-way light time between the observer
               and the central body associated with the frame, the
               orientation of the frame is evaluated at et-ltcent,
               et+ltcent, or `et' depending on whether the requested
               aberration correction is, respectively, for received
               radiation, transmitted radiation, or is omitted.
               `ltcent' is computed using the method indicated by
               `abcorr'.

-Detailed_Output

   spoint      is the surface intercept point on the target body of
               the ray defined by the observer and the direction
               vector. If the ray intersects the target body in
               multiple points, the selected intersection point is
               the one closest to the observer. The output
               argument `found' (see below) indicates whether an
               intercept was found.

               `spoint' is expressed in Cartesian coordinates,
               relative to the body-fixed frame associated with the
               target body. The body-fixed target frame is
               evaluated at the intercept epoch `trgepc' (see
               description below).

               When light time correction is used, the duration of
               light travel between `spoint' to the observer is
               considered to be the one way light time. When both
               light time and stellar aberration corrections are
               used, `spoint' is selected such that, when `spoint' is
               corrected for light time and the vector from the
               observer to the light-time corrected location of
               `spoint' is corrected for stellar aberration, the
               resulting vector is parallel to the ray defined by
               the observer's location and `dvec'.

               The components of `spoint' are given in units of km.

   dist        is the distance between the observer and the surface
               intercept on the target body.  `dist' is given in units
               of km.

   trgepc      is the "intercept epoch." This is the epoch at which
               the ray defined by `obsrvr' and `dvec' intercepts the
               target surface at `spoint'.  `trgepc' is defined as
               follows: letting `lt' be the one-way light time between
               the observer and the intercept point, `trgepc' is the
               epoch et-lt, et+lt, or `et' depending on whether the
               requested aberration correction is, respectively, for
               received radiation, transmitted radiation, or
               omitted. `lt' is computed using the method indicated by
               `abcorr'.

               `trgepc' is expressed as seconds past J2000 TDB.

   obspos      is the vector from the center of the target body at
               epoch `trgepc' to the observer at epoch `et'.  `obspos' is
               expressed in the target body-fixed reference frame
               evaluated at `trgepc'. (This is the frame relative to
               which `spoint' is given.)

               `obspos' is returned to simplify various related
               computations that would otherwise be cumbersome. For
               example, the vector `xvec' from the observer to `spoint'
               can be calculated via the call

                  vsub_c ( spoint, obspos, xvec );

               The components of `obspos' are given in units of km.

   found       is a logical flag indicating whether or not the ray
               intersects the target. If an intersection exists
               `found' will be returned as SPICETRUE. If the ray misses
               the target, `found' will be returned as SPICEFALSE.

-Parameters

   None.

-Exceptions

   If any of the listed errors occur, the output arguments are
   left unchanged.

   1)  If the input argument `method' is not recognized, an error
       is signaled by a routine in the call tree of this
       routine.

   2)  If `target' cannot be mapped to an ID code, the error
       SPICE(IDCODENOTFOUND) is signaled by a routine in the call
       tree of this routine.

   3)  If `obsrvr' cannot be mapped to an ID code, an error is signaled
       by a routine in the call tree of this routine.

   4)  If the input argument `abcorr' is invalid, an error
       is signaled by a routine in the call tree of this
       routine.

   5)  If a body-fixed reference frame associated with the target
       cannot be found, the error SPICE(NOFRAME) is signaled by a
       routine in the call tree of this routine.

   6)  If `obsrvr' and `target' map to the same NAIF integer ID codes, an
       error is signaled by a routine in the call tree of this
       routine.

   7)  If frame definition data enabling the evaluation of the state
       of the target relative to the observer in target body-fixed
       coordinates have not been loaded prior to calling srfxpt_c, an
       error is signaled by a routine in the call tree of this
       routine.

   8)  If the specified aberration correction is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   9)  If insufficient ephemeris data have been loaded prior to
       calling srfxpt_c, an error is signaled by a
       routine in the call tree of this routine. Note that when
       light time correction is used, sufficient ephemeris data
       must be available to propagate the states of both observer
       and target to the solar system barycenter.

   10) If the computation method has been specified as "Ellipsoid"
       and triaxial radii of the target body have not been loaded
       into the kernel pool prior to calling srfxpt_c, an error is
       signaled by a routine in the call tree of this routine.

   11) If PCK data needed to define the target body-fixed frame have
       not been loaded prior to calling srfxpt_c, an error is signaled
       by a routine in the call tree of this routine.

   12) If the reference frame designated by `dref' is not recognized
       by the SPICE frame subsystem, an error is signaled
       by a routine in the call tree of this routine.

   13) If the direction vector `dvec' is the zero vector, an error
       is signaled  by a routine in the call tree of this routine.

   14) If radii for `target' are not found in the kernel pool, an error
       is signaled by a routine in the call tree of this routine.

   15) If the size of the `target' body radii kernel variable is not
       three, an error is signaled by a routine in the call tree of
       this routine.

   16) If any of the three `target' body radii is less-than or equal to
       zero, an error is signaled by a routine in the call tree of
       this routine.

   17) If any of the `method', `target', `abcorr', `obsrvr' or `dref'
       input string pointers is null, the error SPICE(NULLPOINTER) is
       signaled.

   18) If any of the `method', `target', `abcorr', `obsrvr' or `dref'
       input strings has zero length, the error SPICE(EMPTYSTRING) is
       signaled.

-Files

   Appropriate SPK, PCK, and frame kernels must be loaded by the
   calling program before this routine is called.  CK, SCLK, and
   IK kernels may be required as well.

   The following data are required:

   -  SPK data: ephemeris data for target and observer must be
      loaded. If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data.
      Typically ephemeris data are made available by loading one
      or more SPK files via furnsh_c.

   -  PCK data: if the computation method is specified as
      "Ellipsoid," triaxial radii for the target body must be
      loaded into the kernel pool. Typically this is done by
      loading a text PCK file via furnsh_c.

   -  Further PCK data: rotation data for the target body must
      be loaded. These may be provided in a text or binary PCK
      file.

   -  Frame data: if a frame definition is required to convert
      the observer and target states to the body-fixed frame of
      the target, that definition must be available in the kernel
      pool. Similarly, the frame definition required to map
      between the frame designated by `dref' and the target
      body-fixed frame must be available. Typically the
      definitions of frames not already built-in to SPICE are
      supplied by loading a frame kernel.

   The following data may be required:

   -  CK data: if the frame to which `dref' refers is fixed to
      a spacecraft instrument or structure, at least one CK file will
      be needed to permit transformation of vectors between that
      frame and both J2000 and the target body-fixed frame.

   -  SCLK data: if a CK file is needed, an associated SCLK kernel
      is required to enable conversion between encoded SCLK
      (used to time-tag CK data) and barycentric dynamical time
      (TDB).

   -  IK data: one or more I-kernels may be required to
      enable transformation of vectors from an instrument-fixed
      frame to a spacecraft-fixed frame whose attitude is given
      by a C-kernel.


   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   Given a ray defined by a direction vector and the location of an
   observer, srfxpt_c computes the surface intercept point of the ray
   on a specified target body. srfxpt_c also determines the distance
   between the observer and the surface intercept point.

   When aberration corrections are used, this routine finds the
   value of `spoint' such that, if `spoint' is regarded as an ephemeris
   object, after the selected aberration corrections are applied to
   the vector from the observer to `spoint', the resulting vector is
   parallel to the direction vector `dvec'.

   This routine computes light time corrections using light time
   between the observer and the surface intercept point, as opposed
   to the center of the target. Similarly, stellar aberration
   corrections done by this routine are based on the direction of
   the vector from the observer to the light-time corrected
   intercept point, not to the target center. This technique avoids
   errors due to the differential between aberration corrections
   across the target body. Therefore it's valid to use aberration
   corrections with this routine even when the observer is very
   close to the intercept point, in particular when the
   observer-intercept point distance is much less than the
   observer-target center distance. It's also valid to use stellar
   aberration corrections even when the intercept point is near or
   on the limb (as may occur in occultation computations using a
   point target).

   When comparing surface intercept point computations with results
   from sources other than SPICE, it's essential to make sure the
   same geometric definitions are used.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following program computes surface intercept points on
      Mars for the boresight and FOV boundary vectors of the MGS MOC
      narrow angle camera. The intercepts are computed for a single
      observation epoch. Light time and stellar aberration
      corrections are used. For simplicity, camera distortion is
      ignored.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: srfxpt_ex1.tm

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
            de405s.bsp                       Planetary ephemeris
            mars_iau2000_v0.tpc              Planet orientation and
                                             radii
            naif0011.tls                     Leapseconds
            mgs_moc_v20.ti                   MGS MOC instrument
                                             parameters
            mgs_sclkscet_00061.tsc           MGS SCLK coefficients
            mgs_sc_ext12.bc                  MGS s/c bus attitude
            mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris


         \begindata

            KERNELS_TO_LOAD = ( 'de405s.bsp',
                                'mars_iau2000_v0.tpc',
                                'naif0011.tls',
                                'mgs_moc_v20.ti',
                                'mgs_sclkscet_00061.tsc',
                                'mgs_sc_ext12.bc',
                                'mgs_ext12_ipng_mgs95j.bsp' )
         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program srfxpt_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"
      #include "SpiceZmc.h"

      int main()
      {

         /.
         Local parameters
         ./

         #define ABCLEN              20
         #define LNSIZE              81
         #define METLEN              41
         #define NAMLEN              33
         #define TIMLEN              51
         #define SHPLEN              81
         #define NCORNR               4


         /.
         Local variables
         ./
         SpiceBoolean            found;

         SpiceChar             * abcorr  = "LT+S";
         SpiceChar             * camera  = "MGS_MOC_NA";
         SpiceChar               dref    [NAMLEN];
         SpiceChar             * method  = "Ellipsoid";
         SpiceChar             * obsrvr  = "MGS";
         SpiceChar               shape   [ SHPLEN ];
         SpiceChar             * target  = "Mars";
         SpiceChar               title   [ LNSIZE ];
         SpiceChar             * utc     = "2003 OCT 13 06:00:00 UTC";

         SpiceDouble             bounds  [NCORNR][3];
         SpiceDouble             bsight  [3];
         SpiceDouble             dist;
         SpiceDouble             dvec    [3];
         SpiceDouble             et;
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             obspos [3];
         SpiceDouble             radius;
         SpiceDouble             spoint [3];
         SpiceDouble             trgepc;

         SpiceInt                camid;
         SpiceInt                i;
         SpiceInt                n;


         /.
         Load kernel files:

            - Leapseconds kernel
            - MGS SCLK kernel
            - Text PCK file
            - Planetary SPK file
            - MGS I-kernel
            - MGS spacecraft bus C-kernel
            - MGS SPK file
         ./
         furnsh_c ( "srfxpt_ex1.tm" );

         /.
         Convert the UTC request time to ET (seconds past
         J2000, TDB).
         ./
         str2et_c ( utc, &et );

         /.
         Get the MGS MOC Narrow angle camera (MGS_MOC_NA)
         ID code.  Then look up the field of view (FOV)
         parameters.
         ./
         bodn2c_c ( camera, &camid, &found );

         if ( !found )
         {
             setmsg_c ( "Could not find ID code for "
                        "instrument #."               );
             errch_c  ( "#", camera                   );
             sigerr_c ( "SPICE(NOTRANSLATION)"        );
         }

         getfov_c ( camid, NCORNR, SHPLEN, NAMLEN,
                    shape, dref,   bsight, &n,     bounds );


         printf ( "\n"
                  "Surface Intercept Locations for Camera\n"
                  "FOV Boundary and Boresight Vectors\n"
                  "\n"
                  "   Instrument:             %s\n"
                  "   Epoch:                  %s\n"
                  "   Aberration correction:  %s\n"
                  "\n",
                  camera, utc, abcorr                             );

         /.
         Now compute and display the surface intercepts for the
         boresight and all of the FOV boundary vectors.
         ./

         for ( i = 0;  i <= NCORNR;  i++ )
         {
            if ( i < NCORNR )
            {
               sprintf ( title, "Corner vector %d", (int)i );

               vequ_c ( bounds[i], dvec );
            }
            else
            {
               strcpy ( title,  "Boresight vector" );

               vequ_c ( bsight, dvec );
            }

            /.
            Compute the surface intercept point using
            the specified aberration corrections.

            srfxpt_c will signal an error if required kernel
            data are unavailable.  See example (2) below for
            a suggestion on detecting absence of C-kernel
            data prior to calling srfxpt_c.
            ./
            srfxpt_c ( method,
                       target,  et,    abcorr,
                       obsrvr,  dref,  dvec,
                       spoint,  &dist, &trgepc,  obspos, &found );

            if ( found )
            {
               /.
               Convert rectangular coordinates to planetocentric
               latitude and longitude.  Convert radians to degrees.
               ./
               reclat_c ( spoint, &radius, &lon, &lat );

               lon *= dpr_c ();
               lat *= dpr_c ();

               /.
               Display the results.
               ./

               printf ( "\n"
                        "%s\n", title );

               sprintf ( title, "  Vector in %s frame = ", dref );

               printf ( "\n"
                        "%s\n", title );

               if ( i < NCORNR )
               {
                   printf ( "   %18.10e %18.10e %18.10e\n",
                            bounds[i][0], bounds[i][1], bounds[i][2] );
               }
               else
               {
                  printf ( "   %18.10e %18.10e %18.10e\n",
                           bsight[0],    bsight[1],    bsight[2]    );
               }

               printf ( "\n"
                        "  Intercept:\n"
                        "\n"
                        "     Radius                   (km)  = %18.10e\n"
                        "     Planetocentric Latitude  (deg) = %18.10e\n"
                        "     Planetocentric Longitude (deg) = %18.10e\n"
                        "     Range                    (km)  = %18.10e\n"
                        "\n",
                        radius,  lat,  lon,  dist                          );
            }
            else
            {
                printf ( "\n"
                         "Intercept not found.\n"
                         "\n"                     );
            }

         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Surface Intercept Locations for Camera
      FOV Boundary and Boresight Vectors

         Instrument:             MGS_MOC_NA
         Epoch:                  2003 OCT 13 06:00:00 UTC
         Aberration correction:  LT+S


      Corner vector 0

        Vector in MGS_MOC_NA frame =
           1.8571383810e-06  -3.8015622659e-03   9.9999277403e-01

        Intercept:

           Radius                   (km)  =   3.3849411359e+03
           Planetocentric Latitude  (deg) =  -4.8477481852e+01
           Planetocentric Longitude (deg) =  -1.2347407883e+02
           Range                    (km)  =   3.8898310725e+02


      Corner vector 1

        Vector in MGS_MOC_NA frame =
           1.8571383810e-06   3.8015622659e-03   9.9999277403e-01

        Intercept:

           Radius                   (km)  =   3.3849396988e+03
           Planetocentric Latitude  (deg) =  -4.8481636267e+01
           Planetocentric Longitude (deg) =  -1.2339882275e+02
           Range                    (km)  =   3.8897512490e+02


      Corner vector 2

        Vector in MGS_MOC_NA frame =
          -1.8571383810e-06   3.8015622659e-03   9.9999277403e-01

        Intercept:

           Radius                   (km)  =   3.3849396899e+03
           Planetocentric Latitude  (deg) =  -4.8481661837e+01
           Planetocentric Longitude (deg) =  -1.2339882596e+02
           Range                    (km)  =   3.8897466598e+02


      Corner vector 3

        Vector in MGS_MOC_NA frame =
          -1.8571383810e-06  -3.8015622659e-03   9.9999277403e-01

        Intercept:

           Radius                   (km)  =   3.3849411271e+03
           Planetocentric Latitude  (deg) =  -4.8477507428e+01
           Planetocentric Longitude (deg) =  -1.2347408199e+02
           Range                    (km)  =   3.8898264817e+02


      Boresight vector

        Vector in MGS_MOC_NA frame =
           0.0000000000e+00   0.0000000000e+00   1.0000000000e+00

        Intercept:

           Radius                   (km)  =   3.3849404102e+03
           Planetocentric Latitude  (deg) =  -4.8479579751e+01
           Planetocentric Longitude (deg) =  -1.2343645375e+02
           Range                    (km)  =   3.8897573918e+02


   2) srfxpt_c will signal an error if required kernel data are
      unavailable: for example, in the program of Example 1, if the
      C-kernel containing data for the MGS bus had a gap at epoch `et',
      srfxpt_c would be unable to transform the direction vector `dvec'
      from the reference frame fixed to the camera to the reference
      frame fixed to the target body.

      We could modify the code of Example 1 as shown below to test for
      the availability of C-kernel data. We would add the declarations
      shown, and we'd call the C-kernel reader ckgp_c to find whether the
      desired pointing was available. Depending on the value of the
      `found' flag returned by ckgp_c, we'd go on to compute the surface
      intercept point or respond to the error condition.


                            .
                            .
                            .
         /.
         Local parameters
         ./
         #define BUSID            ( -94000 )
         #define MGS              ( -94 )
                            .
                            .
                            .

         /.
         Local variables
         ./
         SpiceDouble             clkout;
         SpiceDouble             cmat    [3][3];
         SpiceDouble             sclkdp;

                            .
                            .
                            .
         /.
         Look up the transformation from the J2000 frame to the
         MGS spacecraft frame. To do this, we'll need to represent
         our observation epoch in terms of MGS encoded SCLK.
         ./
         sce2c_c ( MGS, et, &sclkdp );

         /.
         Look up the spacecraft attitude from the C-kernel.
         ./
         ckgp_c ( BUSID, sclkdp,  0.,    "J2000",
                  cmat,  &clkout, &found         );

         if ( found )
         {

            [Proceed to compute intercept point]
         }
         else
         {

            [Handle case where pointing is unavailable
             for the epoch of interest]
         }
                            .
                            .
                            .

-Restrictions

   1)  A cautionary note: if aberration corrections are used, and if
       `dref' is the target body-fixed frame, the epoch at which that
       frame is evaluated is offset from `et' by the light time between
       the observer and the *center* of the target body. This light
       time normally will differ from the light time between the
       observer and intercept point. Consequently the orientation of
       the target body-fixed frame at `trgepc' will not match that of
       the target body-fixed frame at the epoch associated with `dref'.
       As a result, various derived quantities may not be as
       expected: for example, `obspos' would not be the inverse of the
       aberration-corrected position of the target as seen by the
       observer.

       In many applications the errors arising from this frame
       discrepancy may be insignificant; however a safe approach is
       to always use as `dref' a frame other than the target body-fixed
       frame.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.5, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Updated example #1 to use a meta-kernel to load the required
       kernels.

   -CSPICE Version 1.0.4, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.3, 19-MAY-2010 (BVS)

       Index line now states that this routine is deprecated.

   -CSPICE Version 1.0.2, 07-FEB-2008 (NJB)

       -Abstract now states that this routine is deprecated.

       Header typo was corrected; reference to vminus_c was replaced
       with reference to vsub_c.

   -CSPICE Version 1.0.1, 22-JUL-2004 (NJB)

       Made trivial change to description of `obsrvr' in
       Detailed Input header section.

   -CSPICE Version 1.0.0, 27-FEB-2004 (NJB)

-Index_Entries

   DEPRECATED surface intercept point

-&
*/

{ /* Begin srfxpt_c */

   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "srfxpt_c" );

   /*
   Check the input string arguments:

      method
      target
      abcorr
      obsrvr
      dref

   Make sure each pointer is non-null and each string contains
   at least one data character:  that is, one character
   preceding the null terminator.
   */
   CHKFSTR ( CHK_STANDARD, "srfxpt_c", method );
   CHKFSTR ( CHK_STANDARD, "srfxpt_c", target );
   CHKFSTR ( CHK_STANDARD, "srfxpt_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "srfxpt_c", obsrvr );
   CHKFSTR ( CHK_STANDARD, "srfxpt_c", dref   );

   /*
   Call the f2c'd SPICELIB function.
   */
   srfxpt_ ( (char       *) method,
             (char       *) target,
             (doublereal *) &et,
             (char       *) abcorr,
             (char       *) obsrvr,
             (char       *) dref,
             (doublereal *) dvec,
             (doublereal *) spoint,
             (doublereal *) dist,
             (doublereal *) trgepc,
             (doublereal *) obspos,
             (logical    *) &fnd,
             (ftnlen      ) strlen(method),
             (ftnlen      ) strlen(target),
             (ftnlen      ) strlen(abcorr),
             (ftnlen      ) strlen(obsrvr),
             (ftnlen      ) strlen(dref)    );

   /*
   Move the found flag into a variable of type SpiceBoolean.
   The SpiceBoolean type may have a different size than
   the logical type.
   */

   *found = fnd;

   chkout_c ( "srfxpt_c" );

} /* End srfxpt_c */
