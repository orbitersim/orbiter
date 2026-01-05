/*

-Procedure sincpt_c ( Surface intercept )

-Abstract

   Compute, for a given observer and a ray emanating from the
   observer, the surface intercept of the ray on a target body at
   a specified epoch, optionally corrected for light time and
   stellar aberration.

   The surface of the target body may be represented by a triaxial
   ellipsoid or by topographic data provided by DSK files.

   This routine supersedes srfxpt_c.

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

   CK
   DSK
   FRAMES
   NAIF_IDS
   PCK
   SCLK
   SPK
   TIME

-Keywords

   GEOMETRY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef   sincpt_c


   void sincpt_c ( ConstSpiceChar      * method,
                   ConstSpiceChar      * target,
                   SpiceDouble           et,
                   ConstSpiceChar      * fixref,
                   ConstSpiceChar      * abcorr,
                   ConstSpiceChar      * obsrvr,
                   ConstSpiceChar      * dref,
                   ConstSpiceDouble      dvec   [3],
                   SpiceDouble           spoint [3],
                   SpiceDouble         * trgepc,
                   SpiceDouble           srfvec [3],
                   SpiceBoolean        * found       )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   method     I   Computation method.
   target     I   Name of target body.
   et         I   Epoch in TDB seconds past J2000 TDB.
   fixref     I   Body-fixed, body-centered target body frame.
   abcorr     I   Aberration correction flag.
   obsrvr     I   Name of observing body.
   dref       I   Reference frame of ray's direction vector.
   dvec       I   Ray's direction vector.
   spoint     O   Surface intercept point on the target body.
   trgepc     O   Intercept epoch.
   srfvec     O   Vector from observer to intercept point.
   found      O   Flag indicating whether intercept was found.

-Detailed_Input

   method      is a short string providing parameters defining
               the computation method to be used. In the syntax
               descriptions below, items delimited by brackets
               are optional.

               `method' may be assigned the following values:

                  "ELLIPSOID"

                     The intercept computation uses a triaxial
                     ellipsoid to model the surface of the target
                     body. The ellipsoid's radii must be available
                     in the kernel pool.


                  "DSK/UNPRIORITIZED[/SURFACES = <surface list>]"

                     The intercept computation uses topographic data
                     to model the surface of the target body. These
                     data must be provided by loaded DSK files.

                     The surface list specification is optional. The
                     syntax of the list is

                        <surface 1> [, <surface 2>...]

                     If present, it indicates that data only for the
                     listed surfaces are to be used; however, data
                     need not be available for all surfaces in the
                     list. If absent, loaded DSK data for any surface
                     associated with the target body are used.

                     The surface list may contain surface names or
                     surface ID codes. Names containing blanks must
                     be delimited by escaped double quotes, for example

                        "SURFACES = \"Mars MEGDR 128 PIXEL/DEG\""

                     If multiple surfaces are specified, their names
                     or IDs must be separated by commas.

                     See the -Particulars section below for details
                     concerning use of DSK data.


               Neither case nor white space are significant in
               `method', except within double-quoted strings. For
               example, the string " eLLipsoid " is valid.

               Within double-quoted strings, blank characters are
               significant, but multiple consecutive blanks are
               considered equivalent to a single blank. Case is
               not significant. So

                  "Mars MEGDR 128 PIXEL/DEG"

               is equivalent to

                  " mars megdr  128  pixel/deg "

               but not to

                  "MARS MEGDR128PIXEL/DEG"

   target      is the name of the target body. `target' is
               case-insensitive, and leading and trailing blanks in
               `target' are not significant. Optionally, you may
               supply a string containing the integer ID code
               for the object. For example both "MOON" and "301"
               are legitimate strings that indicate the Moon is the
               target body.

               When the target body's surface is represented by a
               tri-axial ellipsoid, this routine assumes that a
               kernel variable representing the ellipsoid's radii is
               present in the kernel pool. Normally the kernel
               variable would be defined by loading a PCK file.

   et          is the epoch of participation of the observer,
               expressed as ephemeris seconds past J2000 TDB: `et' is
               the epoch at which the observer's state is computed.

               When aberration corrections are not used, `et' is also
               the epoch at which the state and orientation of the
               target body are computed.

               When aberration corrections are used, the position
               and orientation of the target body are computed at
               et-lt or et+lt, where `lt' is the one-way light time
               between the intercept point and the observer, and the
               sign applied to `lt' depends on the selected
               correction. See the description of `abcorr' below for
               details.

   fixref      is the name of a body-fixed reference frame centered
               on the target body. `fixref' may be any such frame
               supported by the SPICE system, including built-in
               frames (documented in the Frames Required Reading)
               and frames defined by a loaded frame kernel (FK). The
               string `fixref' is case-insensitive, and leading and
               trailing blanks in `fixref' are not significant.

               The output intercept point `spoint' and the observer-to-
               intercept vector `srfvec' will be expressed relative to
               this reference frame.

   abcorr      indicates the aberration corrections to be applied
               when computing the target's position and orientation.

               For remote sensing applications, where the apparent
               surface intercept point seen by the observer is
               desired, normally the correction

                  "CN+S"

               should be used. This and the other supported options
               are described below. `abcorr' may be any of the
               following:

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

                             Both the target position as seen by the
                             observer, and rotation of the target
                             body, are corrected for light time.

                  "LT+S"     Correct for one-way light time and
                             stellar aberration using a Newtonian
                             formulation. This option modifies the
                             surface intercept obtained with the
                             "LT" option to account for the
                             observer's velocity relative to the
                             solar system barycenter. These
                             computations yield the apparent surface
                             intercept point.

                  "CN"       Converged Newtonian light time
                             correction. In solving the light time
                             equation, the "CN" correction iterates
                             until the solution converges. Both the
                             position and rotation of the target
                             body are corrected for light time.

                  "CN+S"     Converged Newtonian light time and
                             stellar aberration corrections. This
                             option produces a solution that is at
                             least as accurate at that obtainable
                             with the "LT+S" option. Whether the
                             "CN+S" solution is substantially more
                             accurate depends on the geometry of the
                             participating objects and on the
                             accuracy of the input data. In all
                             cases this routine will execute more
                             slowly when a converged solution is
                             computed.

                             For reception-case applications
                             involving intercepts near the target
                             body limb, this option should be used.

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
                             "XLT" option uses one iteration.

                             Both the target position as seen by the
                             observer, and rotation of the target
                             body, are corrected for light time.

                  "XLT+S"    "Transmission" case: correct for
                             one-way light time and stellar
                             aberration using a Newtonian
                             formulation. This option modifies the
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
                             aberration corrections. This option
                             produces a solution that is at least as
                             accurate at that obtainable with the
                             "XLT+S" option. Whether the "XCN+S"
                             solution is substantially more accurate
                             depends on the geometry of the
                             participating objects and on the
                             accuracy of the input data. In all
                             cases this routine will execute more
                             slowly when a converged solution is
                             computed.

                             For transmission-case applications
                             involving intercepts near the target
                             body limb, this option should be used.

               Case and embedded blanks are not significant in
               `abcorr'. For example, the string

                 "Cn + s"

               is valid.

   obsrvr      is the name of the observing body. This is typically
               a spacecraft, the earth, or a surface point on the
               earth or on another extended object.

               The observer must be outside the target body.

               `obsrvr' is case-insensitive, and leading and
               trailing blanks in `obsrvr' are not significant.
               Optionally, you may supply a string containing the
               integer ID code for the object. For example both
               "MOON" and "301" are legitimate strings that indicate
               the Moon is the observer.

   dref        is the name of the reference frame relative to which
               the ray's direction vector is expressed. This may be
               any frame supported by the SPICE system, including
               built-in frames (documented in the Frames Required
               Reading) and frames defined by a loaded frame kernel
               (FK). The string `dref' is case-insensitive, and
               leading and trailing blanks in `dref' are not
               significant.

               When `dref' designates a non-inertial frame, the
               orientation of the frame is evaluated at an epoch
               dependent on the frame's center and, if the center is
               not the observer, on the selected aberration
               correction. See the description of the direction
               vector `dvec' for details.

   dvec        is a ray direction vector emanating from the observer.
               The intercept with the target body's surface of the ray
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
               the one closest to the observer. The output argument
               `found' (see below) indicates whether an intercept was
               found.

               `spoint' is expressed in Cartesian coordinates,
               relative to the target body-fixed frame designated by
               `fixref'. The body-fixed target frame is evaluated at
               the intercept epoch `trgepc' (see description below).

               When light time correction is used, the duration of
               light travel between `spoint' to the observer is
               considered to be the one way light time. When both light
               time and stellar aberration corrections are used,
               `spoint' is computed such that, when the vector from the
               observer to `spoint' is corrected for light time and
               stellar aberration, the resulting vector lies on the ray
               defined by the observer's location and `dvec'.

               The components of `spoint' are given in units of km.

   trgepc      is the "intercept epoch." `trgepc' is defined as
               follows: letting `lt' be the one-way light time between
               the observer and the intercept point, `trgepc' is the
               epoch et-lt, et+lt, or `et' depending on whether the
               requested aberration correction is, respectively, for
               received radiation, transmitted radiation, or
               omitted. `lt' is computed using the method indicated by
               `abcorr'.

               `trgepc' is expressed as TDB seconds past J2000 TDB.

   srfvec      is the vector from the observer's position at `et' to
               the aberration-corrected (or optionally, geometric)
               position of `spoint', where the aberration corrections
               are specified by `abcorr'. `srfvec' is expressed in the
               target body-fixed reference frame designated by
               `fixref', evaluated at `trgepc'.

               The components of `srfvec' are given in units of km.

               One can use the CSPICE function vnorm_c to obtain the
               distance between the observer and `spoint':

                  dist = vnorm_c ( srfvec );

               The observer's position `obspos', relative to the
               target body's center, where the center's position is
               corrected for aberration effects as indicated by
               `abcorr', can be computed via the call:

                  vsub_c ( spoint, srfvec, obspos );

               To transform the vector `srfvec' from a reference frame
               `fixref' at time `trgepc' to a time-dependent reference
               frame `ref' at time `et', the routine pxfrm2_c should be
               called. Let `xform' be the 3x3 matrix representing the
               rotation from the reference frame `fixref' at time
               `trgepc' to the reference frame `ref' at time `et'. Then
               `srfvec' can be transformed to the result `refvec' as
               follows:

                   pxfrm2_c ( fixref, ref,    trgepc, et, xform );
                   mxv_c    ( xform,  srfvec, refvec );

               The second example in the -Examples header section
               below presents a complete program that demonstrates
               this procedure.

   found       is a logical flag indicating whether or not the ray
               intersects the target. If an intersection exists `found'
               will be returned as SPICETRUE. If the ray misses the
               target, `found' will be returned as SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If the specified aberration correction is unrecognized, an
       error is signaled by a routine in the call tree of this
       routine.

   2)  If either the target or observer input strings cannot be
       converted to an integer ID code, the error
       SPICE(IDCODENOTFOUND) is signaled by a routine in the call
       tree of this routine.

   3)  If `obsrvr' and `target' map to the same NAIF integer ID code, the
       error SPICE(BODIESNOTDISTINCT) is signaled by a routine in the
       call tree of this routine.

   4)  If the input target body-fixed frame `fixref' is not recognized,
       the error SPICE(NOFRAME) is signaled by a routine in the call
       tree of this routine. A frame name may fail to be recognized
       because a required frame specification kernel has not been
       loaded; another cause is a misspelling of the frame name.

   5)  If the input frame `fixref' is not centered at the target body,
       the error SPICE(INVALIDFRAME) is signaled by a routine in the
       call tree of this routine.

   6)  If the input argument `method' cannot be parsed, an error
       is signaled by either this routine or a routine in the
       call tree of this routine.

   7)  If the target and observer have distinct identities but are at
       the same location (for example, the target is Mars and the
       observer is the Mars barycenter), the error
       SPICE(NOSEPARATION) is signaled by a routine in the call tree
       of this routine.

   8)  If insufficient ephemeris data have been loaded prior to
       calling sincpt_c, an error is signaled by a
       routine in the call tree of this routine. Note that when
       light time correction is used, sufficient ephemeris data must
       be available to propagate the states of both observer and
       target to the solar system barycenter.

   9)  If the computation method specifies an ellipsoidal target
       shape and triaxial radii of the target body have not been
       loaded into the kernel pool prior to calling sincpt_c, an error
       is signaled by a routine in the call tree of this routine.

   10) The target must be an extended body: if any of the radii of
       the target body are non-positive, an error is signaled by a
       routine in the call tree of this routine.

   11) If PCK data specifying the target body-fixed frame orientation
       have not been loaded prior to calling sincpt_c, an error is
       signaled by a routine in the call tree of this routine.

   12) If the reference frame designated by `dref' is not recognized
       by the SPICE frame subsystem, the error SPICE(NOFRAME)
       is signaled by a routine in the call tree of this routine.

   13) If the direction vector `dvec' is the zero vector, the error
       SPICE(ZEROVECTOR) is signaled by a routine in the call tree of
       this routine.

   14) If `method' specifies that the target surface is represented by
       DSK data, and no DSK files are loaded for the specified
       target, an error is signaled by a routine in the call tree
       of this routine.

   15) If `method' specifies that the target surface is represented
       by DSK data, and DSK data are not available for a portion of
       the target body's surface, an intercept might not be found.
       This routine does not revert to using an ellipsoidal surface
       in this case.

   16) If any of the `method', `target', `fixref', `abcorr', `obsrvr'
       or `dref' input string pointers is null, the error
       SPICE(NULLPOINTER) is signaled.

   17) If any of the `method', `target', `fixref', `abcorr', `obsrvr'
       or `dref' input strings has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   Appropriate kernels must be loaded by the calling program before
   this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for target and observer must be
      loaded. If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data. Ephemeris
      data are made available by loading one or more SPK files via
      furnsh_c.

   -  PCK data: if the computation method is specified as
      "Ellipsoid," triaxial radii for the target body must be
      loaded into the kernel pool. Typically this is done by
      loading a text PCK file via furnsh_c.

   -  Target body orientation data: these may be provided in a text or
      binary PCK file. In some cases, target body orientation may
      be provided by one more more CK files. In either case, data
      are made available by loading the files via furnsh_c.

   The following data may be required:

   -  DSK data: if `method' indicates that DSK data are to be used,
      DSK files containing topographic data for the target body
      must be loaded. If a surface list is specified, data for
      at least one of the listed surfaces must be loaded.

   -  Surface name-ID associations: if surface names are specified
      in `method', the association of these names with their
      corresponding surface ID codes must be established by
      assignments of the kernel variables

         NAIF_SURFACE_NAME
         NAIF_SURFACE_CODE
         NAIF_SURFACE_BODY

      Normally these associations are made by loading a text
      kernel containing the necessary assignments. An example
      of such assignments is

         NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG'
         NAIF_SURFACE_CODE += 1
         NAIF_SURFACE_BODY += 499

   -  Frame data: if a frame definition is required to convert
      the observer and target states to the body-fixed frame of
      the target, that definition must be available in the kernel
      pool. Similarly, the frame definition required to map
      between the frame designated by `dref' and the target
      body-fixed frame must be available. Typically the
      definitions of frames not already built-in to SPICE are
      supplied by loading a frame kernel.

   -  CK data: if the frame to which `dref' refers is fixed to a
      spacecraft instrument or structure, at least one CK file
      will be needed to permit transformation of vectors between
      that frame and both the J2000 and the target body-fixed
      frames.

   -  SCLK data: if a CK file is needed, an associated SCLK
      kernel is required to enable conversion between encoded SCLK
      (used to time-tag CK data) and barycentric dynamical time
      (TDB).

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   Given a ray defined by a direction vector and the location of an
   observer, sincpt_c computes the surface intercept point of the ray
   on a specified target body. sincpt_c also determines the vector
   from the observer to the surface intercept point. If the ray
   intersects the target in multiple locations, the intercept
   closest to the observer is selected.

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


   Using DSK data
   ==============

      DSK loading and unloading
      -------------------------

      DSK files providing data used by this routine are loaded by
      calling furnsh_c and can be unloaded by calling unload_c or
      kclear_c. See the documentation of furnsh_c for limits on numbers
      of loaded DSK files.

      For run-time efficiency, it's desirable to avoid frequent
      loading and unloading of DSK files. When there is a reason to
      use multiple versions of data for a given target body---for
      example, if topographic data at varying resolutions are to be
      used---the surface list can be used to select DSK data to be
      used for a given computation. It is not necessary to unload
      the data that are not to be used. This recommendation presumes
      that DSKs containing different versions of surface data for a
      given body have different surface ID codes.


      DSK data priority
      -----------------

      A DSK coverage overlap occurs when two segments in loaded DSK
      files cover part or all of the same domain---for example, a
      given longitude-latitude rectangle---and when the time
      intervals of the segments overlap as well.

      When DSK data selection is prioritized, in case of a coverage
      overlap, if the two competing segments are in different DSK
      files, the segment in the DSK file loaded last takes
      precedence. If the two segments are in the same file, the
      segment located closer to the end of the file takes
      precedence.

      When DSK data selection is unprioritized, data from competing
      segments are combined. For example, if two competing segments
      both represent a surface as a set of triangular plates, the
      union of those sets of plates is considered to represent the
      surface.

      Currently only unprioritized data selection is supported.
      Because prioritized data selection may be the default behavior
      in a later version of the routine, the UNPRIORITIZED keyword is
      required in the `method' argument.


      Syntax of the `method' input argument
      -----------------------------------

      The keywords and surface list in the `method' argument
      are called "clauses." The clauses may appear in any
      order, for example

         "DSK/<surface list>/UNPRIORITIZED"
         "DSK/UNPRIORITIZED/<surface list>"
         "UNPRIORITIZED/<surface list>/DSK"

      The simplest form of the `method' argument specifying use of
      DSK data is one that lacks a surface list, for example:

         "DSK/UNPRIORITIZED"

      For applications in which all loaded DSK data for the target
      body are for a single surface, and there are no competing
      segments, the above string suffices. This is expected to be
      the usual case.

      When, for the specified target body, there are loaded DSK
      files providing data for multiple surfaces for that body, the
      surfaces to be used by this routine for a given call must be
      specified in a surface list, unless data from all of the
      surfaces are to be used together.

      The surface list consists of the string

         "SURFACES = "

      followed by a comma-separated list of one or more surface
      identifiers. The identifiers may be names or integer codes in
      string format. For example, suppose we have the surface
      names and corresponding ID codes shown below:

         Surface Name                              ID code
         ------------                              -------
         "Mars MEGDR 128 PIXEL/DEG"                1
         "Mars MEGDR 64 PIXEL/DEG"                 2
         "Mars_MRO_HIRISE"                         3

      If data for all of the above surfaces are loaded, then
      data for surface 1 can be specified by either

         "SURFACES = 1"

      or

         "SURFACES = \"Mars MEGDR 128 PIXEL/DEG\""

      Escaped double quotes are used to delimit the surface name because
      it contains blank characters.

      To use data for surfaces 2 and 3 together, any
      of the following surface lists could be used:

         "SURFACES = 2, 3"

         "SURFACES = \"Mars MEGDR  64 PIXEL/DEG\", 3"

         "SURFACES = 2, Mars_MRO_HIRISE"

         "SURFACES = \"Mars MEGDR 64 PIXEL/DEG\", Mars_MRO_HIRISE"

      An example of a `method' argument that could be constructed
      using one of the surface lists above is

         "DSK/UNPRIORITIZED/SURFACES = \"Mars MEGDR 64 PIXEL/DEG\", 3"


      Round-off errors and mitigating algorithms
      ------------------------------------------

      When topographic data are used to represent the surface of a
      target body, round-off errors can produce some results that
      may seem surprising.

      Note that, since the surface in question might have mountains,
      valleys, and cliffs, the points of intersection found for
      nearly identical sets of inputs may be quite far apart from
      each other: for example, a ray that hits a mountain side in a
      nearly tangent fashion may, on a different host computer, be
      found to miss the mountain and hit a valley floor much farther
      from the observer, or even miss the target altogether.

      Round-off errors can affect segment selection: for example, a
      ray that is expected to intersect the target body's surface
      near the boundary between two segments might hit either
      segment, or neither of them; the result may be
      platform-dependent.

      A similar situation exists when a surface is modeled by a set
      of triangular plates, and the ray is expected to intersect the
      surface near a plate boundary.

      To avoid having the routine fail to find an intersection when
      one clearly should exist, this routine uses two "greedy"
      algorithms:

         1) If the ray passes sufficiently close to any of the
            boundary surfaces of a segment (for example, surfaces of
            maximum and minimum longitude or latitude), that segment
            is tested for an intersection of the ray with the
            surface represented by the segment's data.

            This choice prevents all of the segments from being
            missed when at least one should be hit, but it could, on
            rare occasions, cause an intersection to be found in a
            segment other than the one that would be found if higher
            precision arithmetic were used.

         2) For type 2 segments, which represent surfaces as
            sets of triangular plates, each plate is expanded very
            slightly before a ray-plate intersection test is
            performed. The default plate expansion factor is

               1 + 1.e-10

            In other words, the sides of the plate are lengthened by
            1/10 of a micron per km. The expansion keeps the centroid
            of the plate fixed.

            Plate expansion prevents all plates from being missed
            in cases where clearly at least one should be hit.

            As with the greedy segment selection algorithm, plate
            expansion can occasionally cause an intercept to be
            found on a different plate than would be found if higher
            precision arithmetic were used. It also can occasionally
            cause an intersection to be found when the ray misses
            the target by a very small distance.


      Aberration corrections
      ----------------------

      For irregularly shaped target bodies, the distance between the
      observer and the nearest surface intercept need not be a
      continuous function of time; hence the one-way light time
      between the intercept and the observer may be discontinuous as
      well. In such cases, the computed light time, which is found
      using an iterative algorithm, may converge slowly or not at all.
      In all cases, the light time computation will terminate, but
      the result may be less accurate than expected.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following program computes surface intercept points on Mars
      for the boresight and FOV boundary vectors of the MGS MOC
      narrow angle camera. The intercepts are computed for a single
      observation epoch. Converged Newtonian light time and stellar
      aberration corrections are used. For simplicity, camera
      distortion is ignored.

      Intercepts are computed using both triaxial ellipsoid and
      topographic surface models.

      The topographic model is based on data from the MGS MOLA DEM
      megr90n000cb, which has a resolution of 4 pixels/degree. A
      triangular plate model was produced by computing a 720 x 1440
      grid of interpolated heights from this DEM, then tessellating
      the height grid. The plate model is stored in a type 2 segment
      in the referenced DSK file.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: sincpt_ex1.tm

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
            mar097.bsp                       Mars satellite ephemeris
            pck00010.tpc                     Planet orientation and
                                             radii
            naif0011.tls                     Leapseconds
            mgs_moc_v20.ti                   MGS MOC instrument
                                             parameters
            mgs_sclkscet_00061.tsc           MGS SCLK coefficients
            mgs_sc_ext12.bc                  MGS s/c bus attitude
            mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris
            megr90n000cb_plate.bds           Plate model based on
                                             MEGDR DEM, resolution
                                             4 pixels/degree.

         \begindata

            KERNELS_TO_LOAD = ( 'de430.bsp',
                                'mar097.bsp',
                                'pck00010.tpc',
                                'naif0011.tls',
                                'mgs_moc_v20.ti',
                                'mgs_sclkscet_00061.tsc',
                                'mgs_sc_ext12.bc',
                                'mgs_ext12_ipng_mgs95j.bsp',
                                'megr90n000cb_plate.bds'      )
         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program sincpt_ex1
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
         #define META                "sincpt_ex1.tm"
         #define ABCLEN              20
         #define LNSIZE              81
         #define NAMLEN              33
         #define TIMLEN              51
         #define SHPLEN              81
         #define NCORNR               4
         #define NMETH                2

         /.
         Local variables
         ./
         SpiceBoolean            found;

         SpiceChar             * abcorr          = "CN+S";
         SpiceChar             * camera          = "MGS_MOC_NA";
         SpiceChar               dref    [NAMLEN];
         SpiceChar             * fixref          = "IAU_MARS";
         SpiceChar             * methds  [NMETH] =
                               {
                                  "Ellipsoid",
                                  "DSK/UNPRIORITIZED"
                               };

         SpiceChar             * obsrvr  = "MGS";
         SpiceChar               shape   [SHPLEN];

         SpiceChar             * srftyp  [NMETH] =
                               {
                                 "Ellipsoid",
                                 "MGS/MOLA topography, 4 pixel/deg"
                               };

         SpiceChar             * target  = "Mars";
         SpiceChar               title   [LNSIZE];

         SpiceChar             * utc     = "2003 OCT 13 06:00:00 UTC";

         SpiceDouble             bounds  [NCORNR][3];
         SpiceDouble             bsight  [3];
         SpiceDouble             dist;
         SpiceDouble             dvec    [3];
         SpiceDouble             et;
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             radius;
         SpiceDouble             spoint [3];
         SpiceDouble             srfvec [3];
         SpiceDouble             trgepc;

         SpiceInt                camid;
         SpiceInt                i;
         SpiceInt                k;
         SpiceInt                n;


         /.
         Load kernel files:
         ./
         furnsh_c ( META );

         /.
         Convert the UTC request time to ET (seconds past
         J2000, TDB).
         ./
         str2et_c ( utc, &et );

         /.
         Get the MGS MOC Narrow angle camera (MGS_MOC_NA)
         ID code. Then look up the field of view (FOV)
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

         /.
         getfov_c will return the name of the camera-fixed frame
         in the string `dref', the camera boresight vector in
         the array `bsight', and the FOV corner vectors in the
         array `bounds'.
         ./
         getfov_c ( camid, NCORNR, SHPLEN, NAMLEN,
                    shape, dref,   bsight, &n,     bounds );

         printf ( "\n"
                  "Surface Intercept Locations for Camera\n"
                  "FOV Boundary and Boresight Vectors\n"
                  "\n"
                  "   Instrument:             %s\n"
                  "   Epoch:                  %s\n"
                  "   Aberration correction:  %s\n",
                  camera, utc, abcorr                             );

         /.
         Now compute and display the surface intercepts for the
         boresight and all of the FOV boundary vectors.
         ./

         for ( i = 0;  i <= NCORNR;  i++ )
         {
            if ( i < NCORNR )
            {
               sprintf ( title, "Corner vector %d", (int)(i+1) );

               vequ_c ( bounds[i], dvec );
            }
            else
            {
               strcpy ( title,  "Boresight vector" );

               vequ_c ( bsight, dvec );
            }

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
                     "  Intercept:\n" );

            /.
            Compute the surface intercept point using
            the specified aberration corrections. Loop
            over the set of computation methods.
            ./
            for ( k = 0;  k < NMETH;  k++ )
            {

               sincpt_c ( methds[k],
                          target,  et,     fixref, abcorr,
                          obsrvr,  dref,   dvec,   spoint,
                          &trgepc, srfvec, &found         );

               if ( found )
               {
                  /.
                  Compute range from observer to apparent intercept.
                  ./
                  dist = vnorm_c( srfvec );

                  /.
                  Convert rectangular coordinates to planetocentric
                  latitude and longitude. Convert radians to degrees.
                  ./
                  reclat_c ( spoint, &radius, &lon, &lat );

                  lon *= dpr_c ();
                  lat *= dpr_c ();

                  /.
                  Display the results.
                  ./
                  printf ( "\n"
                           "    Surface representation: %s\n"
                           "\n"
                           "     Radius                   (km)  = %18.10f\n"
                           "     Planetocentric Latitude  (deg) = %18.10f\n"
                           "     Planetocentric Longitude (deg) = %18.10f\n"
                           "     Range                    (km)  = %18.10f\n",
                           srftyp[k], radius,  lat,  lon,  dist            );
               }
               else
               {
                   printf ( "\n"
                            "Intercept not found.\n"
                            "\n"                     );
               }
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
         Aberration correction:  CN+S

      Corner vector 1

        Vector in MGS_MOC_NA frame =
           1.8571383810e-06  -3.8015622659e-03   9.9999277403e-01

        Intercept:

          Surface representation: Ellipsoid

           Radius                   (km)  =    3384.9411357607
           Planetocentric Latitude  (deg) =     -48.4774823672
           Planetocentric Longitude (deg) =    -123.4740748197
           Range                    (km)  =     388.9830822570

          Surface representation: MGS/MOLA topography, 4 pixel/deg

           Radius                   (km)  =    3387.6408267726
           Planetocentric Latitude  (deg) =     -48.4922595600
           Planetocentric Longitude (deg) =    -123.4754119350
           Range                    (km)  =     386.1451004041

      Corner vector 2

        Vector in MGS_MOC_NA frame =
           1.8571383810e-06   3.8015622659e-03   9.9999277403e-01

        Intercept:

          Surface representation: Ellipsoid

           Radius                   (km)  =    3384.9396985743
           Planetocentric Latitude  (deg) =     -48.4816367789
           Planetocentric Longitude (deg) =    -123.3988187487
           Range                    (km)  =     388.9751000527

          Surface representation: MGS/MOLA topography, 4 pixel/deg

           Radius                   (km)  =    3387.6403704508
           Planetocentric Latitude  (deg) =     -48.4963866889
           Planetocentric Longitude (deg) =    -123.4007435481
           Range                    (km)  =     386.1361644332

      Corner vector 3

        Vector in MGS_MOC_NA frame =
          -1.8571383810e-06   3.8015622659e-03   9.9999277403e-01

        Intercept:

          Surface representation: Ellipsoid

           Radius                   (km)  =    3384.9396897287
           Planetocentric Latitude  (deg) =     -48.4816623489
           Planetocentric Longitude (deg) =    -123.3988219550
           Range                    (km)  =     388.9746411355

          Surface representation: MGS/MOLA topography, 4 pixel/deg

           Radius                   (km)  =    3387.6403603146
           Planetocentric Latitude  (deg) =     -48.4964120424
           Planetocentric Longitude (deg) =    -123.4007467292
           Range                    (km)  =     386.1357106985

      Corner vector 4

        Vector in MGS_MOC_NA frame =
          -1.8571383810e-06  -3.8015622659e-03   9.9999277403e-01

        Intercept:

          Surface representation: Ellipsoid

           Radius                   (km)  =    3384.9411269138
           Planetocentric Latitude  (deg) =     -48.4775079405
           Planetocentric Longitude (deg) =    -123.4740779752
           Range                    (km)  =     388.9826233195

          Surface representation: MGS/MOLA topography, 4 pixel/deg

           Radius                   (km)  =    3387.6408166345
           Planetocentric Latitude  (deg) =     -48.4922849169
           Planetocentric Longitude (deg) =    -123.4754150656
           Range                    (km)  =     386.1446466486

      Boresight vector

        Vector in MGS_MOC_NA frame =
           0.0000000000e+00   0.0000000000e+00   1.0000000000e+00

        Intercept:

          Surface representation: Ellipsoid

      [...]


      Warning: incomplete output. Only 100 out of 112 lines have been
      provided.


   2) Use sincpt_c to perform a consistency check on a sub-observer
      point computation.

      Use subpnt_c to find the sub-spacecraft point on Mars for the
      Mars Reconnaissance Orbiter spacecraft (MRO) at a specified time,
      using both the 'Ellipsoid/Near point' computation method and an
      ellipsoidal target shape, and the "DSK/Unprioritized/Nadir"
      method and a DSK-based shape model.

      Use both LT+S and CN+S aberration corrections to illustrate
      the differences.

      Convert the spacecraft to sub-observer point vector obtained from
      subpnt_c into the MRO_HIRISE_LOOK_DIRECTION reference frame at
      the observation time. Perform a consistency check with this
      vector: compare the Mars surface intercept of the ray emanating
      from the spacecraft and pointed along this vector with the
      sub-observer point.

      Perform the sub-observer point and surface intercept computations
      using both triaxial ellipsoid and topographic surface models.

      For this example, the topographic model is based on the MGS MOLA
      DEM megr90n000eb, which has a resolution of 16 pixels/degree.
      Eight DSKs, each covering longitude and latitude ranges of 90
      degrees, were made from this data set. For the region covered by
      a given DSK, a grid of approximately 1500 x 1500 interpolated
      heights was produced, and this grid was tessellated using
      approximately 4.5 million triangular plates, giving a total plate
      count of about 36 million for the entire DSK set.

      All DSKs in the set use the surface ID code 499001, so there is
      no need to specify the surface ID in the `method' strings passed
      to sincpt_c and subpnt_c.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: sincpt_ex2.tm

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
            mar097.bsp                       Mars satellite ephemeris
            pck00010.tpc                     Planet orientation and
                                             radii
            naif0011.tls                     Leapseconds
            mro_psp4_ssd_mro95a.bsp          MRO ephemeris
            mro_v11.tf                       MRO frame specifications
            mro_sclkscet_00022_65536.tsc     MRO SCLK coefficients
                                             parameters
            mro_sc_psp_070925_071001.bc      MRO attitude
            megr90n000eb_*_plate.bds         Plate model DSKs based
                                             on MEGDR DEM, resolution
                                             16 pixels/degree.

         \begindata

            KERNELS_TO_LOAD = (

               'de430.bsp',
               'mar097.bsp',
               'pck00010.tpc',
               'naif0011.tls',
               'mro_psp4_ssd_mro95a.bsp',
               'mro_v11.tf',
               'mro_sclkscet_00022_65536.tsc',
               'mro_sc_psp_070925_071001.bc',
               'megr90n000eb_LL000E00N_UR090E90N_plate.bds'
               'megr90n000eb_LL000E90S_UR090E00S_plate.bds'
               'megr90n000eb_LL090E00N_UR180E90N_plate.bds'
               'megr90n000eb_LL090E90S_UR180E00S_plate.bds'
               'megr90n000eb_LL180E00N_UR270E90N_plate.bds'
               'megr90n000eb_LL180E90S_UR270E00S_plate.bds'
               'megr90n000eb_LL270E00N_UR360E90N_plate.bds'
               'megr90n000eb_LL270E90S_UR360E00S_plate.bds'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program sincpt_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define META            "sincpt_ex2.tm"
         #define NCORR           2
         #define NMETH           2

         /.
         Local variables
         ./
         SpiceBoolean            found;

         static SpiceChar      * abcorr[NCORR] =
                                 {
                                    "LT+S", "CN+S"
                                 };

         static SpiceChar      * fixref = "IAU_MARS";

         static SpiceChar      * sinmth[NMETH] =
                                 {
                                    "Ellipsoid",
                                    "DSK/Unprioritized"
                                 };

         static SpiceChar      * submth[NMETH] =
                                 {
                                    "Ellipsoid/Near point",
                                    "DSK/Unprioritized/Nadir"
                                 };

         static SpiceChar      * hiref;

         SpiceDouble             alt;
         SpiceDouble             et;
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             mrovec [3];
         SpiceDouble             radius;
         SpiceDouble             spoint [3];
         SpiceDouble             srfvec [3];
         SpiceDouble             trgepc;
         SpiceDouble             xepoch;
         SpiceDouble             xform  [3][3];
         SpiceDouble             xpoint [3];
         SpiceDouble             xvec   [3];

         SpiceInt                i;
         SpiceInt                j;

         /.
         Load kernel files via the meta-kernel.
         ./
         furnsh_c ( META );

         /.
         Convert the TDB request time string to seconds past
         J2000, TDB.
         ./
         str2et_c ( "2007 SEP 30 00:00:00 TDB", &et );

         /.
         Compute the sub-spacecraft point using each method.
         Compute the results using both LT+S and CN+S aberration
         corrections.
         ./
         for ( i = 0;  i < NMETH;  i++ )
         {

            printf ( "\nSub-observer point computation "
                     "method = %s\n",  submth[i] );

            for ( j = 0;  j < NCORR;  j++ )
            {
               subpnt_c ( submth[i],
                          "mars", et,     fixref,  abcorr[j],
                          "mro",  spoint, &trgepc, srfvec    );

               /.
               Compute the observer's altitude above `spoint'.
               ./
               alt = vnorm_c ( srfvec );

               /.
               Express `srfvec' in the MRO_HIRISE_LOOK_DIRECTION
               reference frame at epoch `et'. Since `srfvec' is expressed
               relative to the IAU_MARS frame at `trgepc', we must
               call pxfrm2_c to compute the position transformation matrix
               from IAU_MARS at `trgepc' to the MRO_HIRISE_LOOK_DIRECTION
               frame at time `et'.

               To make code formatting a little easier, we'll store
               the long MRO reference frame name in a variable:
               ./
               hiref = "MRO_HIRISE_LOOK_DIRECTION";

               pxfrm2_c ( "iau_mars", hiref,  trgepc, et, xform );
               mxv_c    (  xform,     srfvec, mrovec            );

               /.
               Convert sub-observer point rectangular coordinates to
               planetocentric latitude and longitude. Convert radians to
               degrees.
               ./
               reclat_c ( spoint, &radius, &lon, &lat );

               lon *= dpr_c();
               lat *= dpr_c();

               /.
               Write the results.
               ./
               printf ( "\n"
                        "   Aberration correction = %s\n\n"
                        "      MRO-to-sub-observer vector in\n"
                        "      MRO HIRISE look direction frame\n"
                        "         X-component             (km) = %21.9f\n"
                        "         Y-component             (km) = %21.9f\n"
                        "         Z-component             (km) = %21.9f\n"
                        "      Sub-observer point radius  (km) = %21.9f\n"
                        "      Planetocentric latitude   (deg) = %21.9f\n"
                        "      Planetocentric longitude  (deg) = %21.9f\n"
                        "      Observer altitude          (km) = %21.9f\n",
                        abcorr[j],
                        mrovec[0],
                        mrovec[1],
                        mrovec[2],
                        radius,
                        lat,
                        lon,
                        alt         );

               /.
               Consistency check: find the surface intercept on
               Mars of the ray emanating from the spacecraft and having
               direction vector `mrovec' in the MRO HIRISE look direction
               reference frame at `et'. Call the intercept point
               `xpoint'. `xpoint' should coincide with `spoint', up to a
               small round-off error.
               ./
               sincpt_c ( sinmth[i], "mars",  et,    "iau_mars",
                          abcorr[j], "mro",   hiref, mrovec,
                          xpoint,    &xepoch, xvec,  &found     );

               if ( !found )
               {
                  printf ( "Bug: no intercept\n" );
               }
               else
               {
                  /.
                  Report the distance between `xpoint' and `spoint'.
                  ./
                  printf ( "      Intercept comparison error (km) = "
                           "%21.9f\n\n",
                           vdist_c( xpoint, spoint )                  );
               }
            }
         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Sub-observer point computation method = Ellipsoid/Near point

         Aberration correction = LT+S

            MRO-to-sub-observer vector in
            MRO HIRISE look direction frame
               X-component             (km) =           0.286933229
               Y-component             (km) =          -0.260425939
               Z-component             (km) =         253.816326385
            Sub-observer point radius  (km) =        3388.299078378
            Planetocentric latitude   (deg) =         -38.799836378
            Planetocentric longitude  (deg) =        -114.995297227
            Observer altitude          (km) =         253.816622175
            Intercept comparison error (km) =           0.000002144


         Aberration correction = CN+S

            MRO-to-sub-observer vector in
            MRO HIRISE look direction frame
               X-component             (km) =           0.286933107
               Y-component             (km) =          -0.260426683
               Z-component             (km) =         253.816315915
            Sub-observer point radius  (km) =        3388.299078376
            Planetocentric latitude   (deg) =         -38.799836382
            Planetocentric longitude  (deg) =        -114.995297449
            Observer altitude          (km) =         253.816611705
            Intercept comparison error (km) =           0.000000001


      Sub-observer point computation method = DSK/Unprioritized/Nadir

         Aberration correction = LT+S

            MRO-to-sub-observer vector in
            MRO HIRISE look direction frame
               X-component             (km) =           0.282372596
               Y-component             (km) =          -0.256289313
               Z-component             (km) =         249.784871247
            Sub-observer point radius  (km) =        3392.330239436
            Planetocentric latitude   (deg) =         -38.800230156
            Planetocentric longitude  (deg) =        -114.995297338
            Observer altitude          (km) =         249.785162334
            Intercept comparison error (km) =           0.000002412


         Aberration correction = CN+S

            MRO-to-sub-observer vector in
            MRO HIRISE look direction frame
               X-component             (km) =           0.282372464
               Y-component             (km) =          -0.256290075
               Z-component             (km) =         249.784860121
            Sub-observer point radius  (km) =        3392.330239564
            Planetocentric latitude   (deg) =         -38.800230162
            Planetocentric longitude  (deg) =        -114.995297569
            Observer altitude          (km) =         249.785151209
            Intercept comparison error (km) =           0.000000001


-Restrictions

   1)  A cautionary note: if aberration corrections are used, and
       if `dref' is the target body-fixed frame, the epoch at which
       that frame is evaluated is offset from `et' by the light time
       between the observer and the *center* of the target body.
       This light time normally will differ from the light time
       between the observer and intercept point. Consequently the
       orientation of the target body-fixed frame at `trgepc' will
       not match that of the target body-fixed frame at the epoch
       associated with `dref'. As a result, various derived quantities
       may not be as expected: for example, `srfvec' would not be
       parallel to `dvec'.

       In many applications the errors arising from this frame
       discrepancy may be insignificant; however a safe approach is
       to always use as `dref' a frame other than the target
       body-fixed frame.

   2)  This routine must not be used for cases where the observer
       is inside the target body. This routine does not attempt to
       detect this condition.

       If the observer is a point on a target surface described
       by DSK data, care must be taken to ensure the observer is
       sufficiently far outside the target. The routine should
       not be used for surfaces for which "outside" cannot be
       defined.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   S.C. Krening        (JPL)
   B.V. Semenov        (JPL)

-Version

   -CSPICE Version 2.0.1, 01-NOV-2021 (JDR) (NJB)

       Edited the header to comply with NAIF standard.

       Updated -Detailed_Input and -Restrictions sections to state that
       the observer must be outside the target body.

   -CSPICE Version 2.0.0, 05-APR-2017 (NJB) (SCK) (BVS)

       Updated to support use of DSKs.

   -CSPICE Version 1.0.2, 02-APR-2011 (NJB) (SCK)

       References to the new pxfrm2_c routine were added, which
       changed the Detailed Output section and the second example.

       Miscellaneous, minor header comment corrections were made.

   -CSPICE Version 1.0.1, 06-FEB-2009 (NJB)

       Typos in the Detailed Input section's description of `dref'
       were corrected. Incorrect frame name fixfrm was changed to
       fixref in documentation.

       In the header examples, meta-kernel names were updated to use
       the suffix

          ".tm"

   -CSPICE Version 1.0.0, 02-MAR-2008 (NJB)

-Index_Entries

   find surface intercept point
   find intersection of ray and target body surface
   find intercept of ray on target body surface

-&
*/

{ /* Begin sincpt_c */


   /*
   Local variables
   */
   logical                 fnd;

   /*
   Participate in error tracing.
   */
   chkin_c ( "sincpt_c" );

   /*
   Check the input string arguments:

      method
      target
      fixref
      abcorr
      obsrvr
      dref

   Make sure each pointer is non-null and each string contains
   at least one data character: that is, one character
   preceding the null terminator.
   */
   CHKFSTR ( CHK_STANDARD, "sincpt_c", method );
   CHKFSTR ( CHK_STANDARD, "sincpt_c", target );
   CHKFSTR ( CHK_STANDARD, "sincpt_c", fixref );
   CHKFSTR ( CHK_STANDARD, "sincpt_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "sincpt_c", obsrvr );
   CHKFSTR ( CHK_STANDARD, "sincpt_c", dref   );

   /*
   Call the f2c'd SPICELIB function.
   */
   sincpt_ ( (char       *) method,
             (char       *) target,
             (doublereal *) &et,
             (char       *) fixref,
             (char       *) abcorr,
             (char       *) obsrvr,
             (char       *) dref,
             (doublereal *) dvec,
             (doublereal *) spoint,
             (doublereal *) trgepc,
             (doublereal *) srfvec,
             (logical    *) &fnd,
             (ftnlen      ) strlen(method),
             (ftnlen      ) strlen(target),
             (ftnlen      ) strlen(fixref),
             (ftnlen      ) strlen(abcorr),
             (ftnlen      ) strlen(obsrvr),
             (ftnlen      ) strlen(dref)    );

   /*
   Move the found flag into a variable of type SpiceBoolean.
   The SpiceBoolean type may have a different size than
   the logical type.
   */

   *found = fnd;

   chkout_c ( "sincpt_c" );

} /* End sincpt_c */
