/*

-Procedure subslr_c ( Sub-solar point )

-Abstract

   Compute the rectangular coordinates of the sub-solar point on
   a target body at a specified epoch, optionally corrected for
   light time and stellar aberration.

   The surface of the target body may be represented by a triaxial
   ellipsoid or by topographic data provided by DSK files.

   This routine supersedes subsol_c.

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

   DSK
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
   #undef subslr_c


   void subslr_c ( ConstSpiceChar       * method,
                   ConstSpiceChar       * target,
                   SpiceDouble            et,
                   ConstSpiceChar       * fixref,
                   ConstSpiceChar       * abcorr,
                   ConstSpiceChar       * obsrvr,
                   SpiceDouble            spoint [3],
                   SpiceDouble          * trgepc,
                   SpiceDouble            srfvec [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   method     I   Computation method.
   target     I   Name of target body.
   et         I   Epoch in ephemeris seconds past J2000 TDB.
   fixref     I   Body-fixed, body-centered target body frame.
   abcorr     I   Aberration correction.
   obsrvr     I   Name of observing body.
   spoint     O   Sub-solar point on the target body.
   trgepc     O   Sub-solar point epoch.
   srfvec     O   Vector from observer to sub-solar point.

-Detailed_Input

   method      is a short string providing parameters defining
               the computation method to be used. In the syntax
               descriptions below, items delimited by brackets
               are optional.

               `method' may be assigned the following values:

                  "NEAR POINT/ELLIPSOID"

                     The sub-solar point computation uses a triaxial
                     ellipsoid to model the surface of the target body.
                     The sub-solar point is defined as the nearest
                     point on the target relative to the sun.

                     The word "NADIR" may be substituted for the phrase
                     "NEAR POINT" in the string above.

                     For backwards compatibility, the older syntax

                        "Near point: ellipsoid"

                     is accepted as well.


                  "INTERCEPT/ELLIPSOID"

                     The sub-solar point computation uses a triaxial
                     ellipsoid to model the surface of the target body.
                     The sub-solar point is defined as the target
                     surface intercept of the line containing the sun
                     and the target's center.

                     For backwards compatibility, the older syntax

                        "Intercept: ellipsoid"

                     is accepted as well.


                  "NADIR/DSK/UNPRIORITIZED[/SURFACES = <surface list>]"

                     The sub-solar point computation uses DSK data to
                     model the surface of the target body. The
                     sub-solar point is defined as the intercept, on
                     the surface represented by the DSK data, of the
                     line containing the sun and the nearest point on
                     the target's reference ellipsoid. If multiple such
                     intercepts exist, the one closest to the sun is
                     selected.

                     Note that this definition of the sub-solar point
                     is not equivalent to the "nearest point on the
                     surface to the sun." The phrase "NEAR POINT" may
                     NOT be substituted for "NADIR" in the string
                     above.

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


                  "INTERCEPT/DSK/UNPRIORITIZED[/SURFACES = <surface list>]"

                     The sub-solar point computation uses DSK data to
                     model the surface of the target body. The
                     sub-solar point is defined as the target surface
                     intercept of the line containing the sun and the
                     target's center.

                     If multiple such intercepts exist, the one closest
                     to the sun is selected.

                     The surface list specification is optional. The
                     syntax of the list is identical to that for the
                     NADIR option described above.


                  Neither case nor white space are significant in
                  `method', except within double-quoted strings. For
                  example, the string " eLLipsoid/nearpoint " is valid.

                  Within double-quoted strings, blank characters are
                  significant, but multiple consecutive blanks are
                  considered equivalent to a single blank. Case is
                  not significant. So

                     "Mars MEGDR 128 PIXEL/DEG"

                  is equivalent to

                     " mars megdr  128  pixel/deg "

                  but not to

                     "MARS MEGDR128PIXEL/DEG"


   target      is the name of the target body. The target body is
               an ephemeris object (its trajectory is given by
               SPK data), and is an extended object.

               The string `target' is case-insensitive, and leading
               and trailing blanks in `target' are not significant.
               Optionally, you may supply a string containing the
               integer ID code for the object. For example both
               "MOON" and "301" are legitimate strings that indicate
               the Moon is the target body.

               When the target body's surface is represented by a
               tri-axial ellipsoid, this routine assumes that a
               kernel variable representing the ellipsoid's radii is
               present in the kernel pool. Normally the kernel
               variable would be defined by loading a PCK file.


   et          is the epoch of participation of the observer,
               expressed as ephemeris seconds past J2000 TDB: `et' is
               the epoch at which the observer's state is computed.

               When aberration corrections are not used, `et' is also
               the epoch at which the position and orientation of
               the target body and the position of the Sun are
               computed.

               When aberration corrections are used, `et' is the epoch
               at which the observer's state relative to the solar
               system barycenter is computed; in this case the
               position and orientation of the target body are
               computed at et-lt, where `lt' is the one-way light time
               between the sub-solar point and the observer. See the
               description of `abcorr' below for details.


   fixref      is the name of a body-fixed reference frame centered
               on the target body. `fixref' may be any such frame
               supported by the SPICE system, including built-in
               frames (documented in the Frames Required Reading)
               and frames defined by a loaded frame kernel (FK). The
               string `fixref' is case-insensitive, and leading and
               trailing blanks in `fixref' are not significant.

               The output sub-solar point `spoint' and the
               observer-to-sub-solar point vector `srfvec' will be
               expressed relative to this reference frame.


   abcorr      indicates the aberration correction to be applied
               when computing the target position and orientation
               and the position of the Sun.

               For remote sensing applications, where the apparent
               sub-solar point seen by the observer is desired,
               normally either of the corrections

                  "LT+S"
                  "CN+S"

               should be used. These and the other supported options
               are described below. `abcorr' may be any of the
               following:

                  "NONE"     Apply no correction. Return the
                             geometric sub-solar point on the target
                             body.

               Let `lt' represent the one-way light time between the
               observer and the sub-solar point (note: NOT between
               the observer and the target body's center). The
               following values of `abcorr' apply to the "reception"
               case in which photons depart from the sub-solar
               point's location at the light-time corrected epoch
               et-lt and *arrive* at the observer's location at `et':

                  "LT"       Correct for one-way light time (also
                             called "planetary aberration") using a
                             Newtonian formulation. This correction
                             yields the location of sub-solar
                             point at the moment it emitted photons
                             arriving at the observer at `et'.

                             The light time correction uses an
                             iterative solution of the light time
                             equation. The solution invoked by the
                             "LT" option uses one iteration.

                             The target position and orientation as
                             seen by the observer are corrected for
                             light time. The position of the Sun
                             relative to the target is corrected for
                             one-way light time between the Sun and
                             target.

                  "LT+S"     Correct for one-way light time and
                             stellar aberration using a Newtonian
                             formulation. This option modifies the
                             sub-solar point obtained with the "LT"
                             option to account for the observer's
                             velocity relative to the solar system
                             barycenter. These corrections yield
                             the apparent sub-solar point.

                  "CN"       Converged Newtonian light time
                             correction. In solving the light time
                             equation, the "CN" correction iterates
                             until the solution converges. Both the
                             position and rotation of the target
                             body, and the position of the Sun, are
                             corrected for light time.

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

               Neither case nor white space are significant in
               `abcorr'. For example, the string

                 "Lt + s"

               is valid.


   obsrvr      is the name of the observing body. The observing body
               is an ephemeris object: it typically is a spacecraft,
               the earth, or a surface point on the earth. `obsrvr' is
               case-insensitive, and leading and trailing blanks in
               `obsrvr' are not significant. Optionally, you may
               supply a string containing the integer ID code for
               the object. For example both "MOON" and "301" are
               legitimate strings that indicate the Moon is the
               observer.

               The observer may coincide with the target.

-Detailed_Output

   spoint      is the sub-solar point on the target body.

               For target shapes modeled by ellipsoids, the
               sub-solar point is defined either as the point on the
               target body that is closest to the sun, or the target
               surface intercept of the line from the sun to the
               target's center.

               For target shapes modeled by topographic data
               provided by DSK files, the sub-solar point is defined
               as the target surface intercept of the line from the
               sun to either the nearest point on the reference
               ellipsoid, or to the target's center. If multiple
               such intercepts exist, the one closest to the sun is
               selected.

               The input argument `method' selects the target shape
               model and sub-solar point definition to be used.

               `spoint' is expressed in Cartesian coordinates,
               relative to the body-fixed target frame designated by
               `fixref'. The body-fixed target frame is evaluated at
               the sub-solar point epoch `trgepc' (see description
               below).

               When aberration corrections are used, `spoint' is
               computed using target body position and orientation
               that have been adjusted for the corrections
               applicable to `spoint' itself rather than to the target
               body's center. In particular, if the stellar
               aberration correction applicable to `spoint' is
               represented by a shift vector S, then the light-time
               corrected position of the target is shifted by S
               before the sub-solar point is computed.

               The components of `spoint' have units of km.


   trgepc      is the "sub-solar point epoch." `trgepc' is defined as
               follows: letting `lt' be the one-way light time between
               the observer and the sub-solar point, `trgepc' is
               either the epoch et-lt or `et' depending on whether the
               requested aberration correction is, respectively, for
               received radiation or omitted. `lt' is computed using
               the method indicated by `abcorr'.

               `trgepc' is expressed as seconds past J2000 TDB.


   srfvec      is the vector from the observer's position at `et' to
               the aberration-corrected (or optionally, geometric)
               position of `spoint', where the aberration corrections
               are specified by `abcorr'. `srfvec' is expressed in the
               target body-fixed reference frame designated by
               `fixref', evaluated at `trgepc'.

               The components of `srfvec' are given in units of km.

               One can use the CSPICE function vnorm_c to obtain the
               distance between the observer and `spoint':

                  dist = vnorm_c( srfvec );

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

-Parameters

   None.

-Exceptions

   1)  If the specified aberration correction is unrecognized, an
       error is signaled by a routine in the call tree of this
       routine.

   2)  If transmission aberration corrections are specified, the
       error SPICE(NOTSUPPORTED) is signaled by a routine in the call
       tree of this routine.

   3)  If either the target or observer input strings cannot be
       converted to an integer ID code, the error
       SPICE(IDCODENOTFOUND) is signaled by a routine in the call
       tree of this routine.

   4)  If the input target body-fixed frame `fixref' is not recognized,
       the error SPICE(NOFRAME) is signaled by a routine in the call
       tree of this routine. A frame name may fail to be recognized
       because a required frame specification kernel has not been
       loaded; another cause is a misspelling of the frame name.

   5)  If the input frame `fixref' is not centered at the target body,
       the error SPICE(INVALIDFRAME) is signaled by a routine in the
       call tree of this routine.

   6)  If the input argument `method' is not recognized, the error
       SPICE(INVALIDMETHOD) is signaled by this routine, or, the
       error is signaled by a routine in the call tree of this
       routine.

   7)  If the sub-solar point type is not specified or is not
       recognized, the error SPICE(INVALIDSUBTYPE) is signaled by a
       routine in the call tree of this routine.

   8)  If insufficient ephemeris data have been loaded prior to
       calling subslr_c, an error is signaled by a
       routine in the call tree of this routine. Note that when
       light time correction is used, sufficient ephemeris data must
       be available to propagate the states of observer, target, and
       the Sun to the solar system barycenter.

   9)  If the computation method specifies an ellipsoidal target
       shape and triaxial radii of the target body have not been
       loaded into the kernel pool prior to calling subslr_c, an error
       is signaled by a routine in the call tree of this routine.

   10) The target must be an extended body, and must have a shape
       for which a sub-solar point can be defined.

       If the target body's shape is modeled by DSK data, the shape
       must be such that the specified sub-solar point definition is
       applicable. For example, if the target shape is a torus, both
       the NADIR and INTERCEPT definitions might be inapplicable,
       depending on the relative locations of the sun and target.

   11) If PCK data specifying the target body-fixed frame orientation
       have not been loaded prior to calling subslr_c, an error is
       signaled by a routine in the call tree of this routine.

   12) If `method' specifies that the target surface is represented by
       DSK data, and no DSK files are loaded for the specified
       target, an error is signaled by a routine in the call tree
       of this routine.

   13) If `method' specifies that the target surface is represented by
       DSK data, and the ray from the observer to the sub-observer
       point doesn't intersect the target body's surface, the error
       SPICE(SUBPOINTNOTFOUND) is signaled by a routine in the call
       tree of this routine.

   14) If the surface intercept on the target body's reference
       ellipsoid of the observer to target center vector cannot not
       be computed, the error SPICE(DEGENERATECASE) is signaled by a
       routine in the call tree of this routine. Note that this is a
       very rare case.

   15) If the target body is the sun, the error SPICE(INVALIDTARGET)
       is signaled by a routine in the call tree of this routine.

   16) If radii for `target' are not found in the kernel pool, an error
       is signaled by a routine in the call tree of this routine.

   17) If the size of the `target' body radii kernel variable is not
       three, an error is signaled by a routine in the call tree of
       this routine.

   18) If any of the three `target' body radii is less-than or equal to
       zero, an error is signaled by a routine in the call tree of
       this routine.

   19) If any of the `method', `target', `fixref', `abcorr' or
       `obsrvr' input string pointers is null, the error
       SPICE(NULLPOINTER) is signaled.

   20) If any of the `method', `target', `fixref', `abcorr' or
       `obsrvr' input strings has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   Appropriate kernels must be loaded by the calling program before
   this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for target, observer, and Sun must
      be loaded. If aberration corrections are used, the states of
      target, observer, and the Sun relative to the solar system
      barycenter must be calculable from the available ephemeris
      data. Typically ephemeris data are made available by loading
      one or more SPK files via furnsh_c.

   -  Target body orientation data: these may be provided in a text or
      binary PCK file. In some cases, target body orientation may
      be provided by one more more CK files. In either case, data
      are made available by loading the files via furnsh_c.

   -  Shape data for the target body:

        PCK data:

           If the target body shape is modeled as an ellipsoid,
           triaxial radii for the target body must be loaded into
           the kernel pool. Typically this is done by loading a
           text PCK file via furnsh_c.

           Triaxial radii are also needed if the target shape is
           modeled by DSK data, but the DSK NADIR method is
           selected.

        DSK data:

           If the target shape is modeled by DSK data, DSK files
           containing topographic data for the target body must be
           loaded. If a surface list is specified, data for at
           least one of the listed surfaces must be loaded.

   The following data may be required:

   -  Frame data: if a frame definition is required to convert the
      observer and target states to the body-fixed frame of the
      target, that definition must be available in the kernel
      pool. Typically the definition is supplied by loading a
      frame kernel via furnsh_c.

   -  Surface name-ID associations: if surface names are specified
      in `method', the association of these names with their
      corresponding surface ID codes must be established by
      assignments of the kernel variables

         NAIF_SURFACE_NAME
         NAIF_SURFACE_CODE
         NAIF_SURFACE_BODY

      Normally these associations are made by loading a text
      kernel containing the necessary assignments. An example
      of such an assignment is

         NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG'
         NAIF_SURFACE_CODE += 1
         NAIF_SURFACE_BODY += 499

   -  SCLK data: if the target body's orientation is provided by
      CK files, an associated SCLK kernel must be loaded.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   There are two different popular ways to define the sub-solar
   point: "nearest point on target to the Sun" or "target surface
   intercept of the line containing the Sun and target." These
   coincide when the target is spherical and generally are distinct
   otherwise.

   This routine computes light time corrections using light time
   between the observer and the sub-solar point, as opposed to the
   center of the target. Similarly, stellar aberration corrections
   done by this routine are based on the direction of the vector
   from the observer to the light-time corrected sub-solar point,
   not to the target center. This technique avoids errors due to the
   differential between aberration corrections across the target
   body. Therefore it's valid to use aberration corrections with
   this routine even when the observer is very close to the
   sub-solar point, in particular when the observer to sub-solar
   point distance is much less than the observer to target center
   distance.

   When comparing sub-solar point computations with results from
   sources other than SPICE, it's essential to make sure the same
   geometric definitions are used.


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
      both represent a surface as sets of triangular plates, the
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

         "NADIR/DSK/UNPRIORITIZED/<surface list>"
         "DSK/NADIR/<surface list>/UNPRIORITIZED"
         "UNPRIORITIZED/<surface list>/DSK/NADIR"

      The simplest form of the `method' argument specifying use of
      DSK data is one that lacks a surface list, for example:

         "NADIR/DSK/UNPRIORITIZED"
         "INTERCEPT/DSK/UNPRIORITIZED"

      For applications in which all loaded DSK data for the target
      body are for a single surface, and there are no competing
      segments, the above strings suffice. This is expected to be
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

      Escaped double quotes are used to delimit the surface name
      because it contains blank characters.

      To use data for surfaces 2 and 3 together, any
      of the following surface lists could be used:

         "SURFACES = 2, 3"

         "SURFACES = \"Mars MEGDR  64 PIXEL/DEG\", 3"

         "SURFACES = 2, Mars_MRO_HIRISE"

         "SURFACES = \"Mars MEGDR 64 PIXEL/DEG\", Mars_MRO_HIRISE"

      An example of a `method' argument that could be constructed
      using one of the surface lists above is

      "NADIR/DSK/UNPRIORITIZED/SURFACES= \"Mars MEGDR 64 PIXEL/DEG\",3"


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

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Find the sub-solar point on Mars as seen from the Earth for a
      specified time.

      Compute the sub-solar point using both triaxial ellipsoid
      and topographic surface models. Topography data are provided by
      a DSK file. For the ellipsoid model, use both the "intercept"
      and "near point" sub-observer point definitions; for the DSK
      case, use both the "intercept" and "nadir" definitions.

      Display the locations of both the sun and the sub-solar
      point relative to the center of Mars, in the IAU_MARS
      body-fixed reference frame, using both planetocentric and
      planetographic coordinates.

      The topographic model is based on data from the MGS MOLA DEM
      megr90n000cb, which has a resolution of 4 pixels/degree. A
      triangular plate model was produced by computing a 720 x 1440
      grid of interpolated heights from this DEM, then tessellating
      the height grid. The plate model is stored in a type 2 segment
      in the referenced DSK file.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: subslr_ex1.tm

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
            megr90n000cb_plate.bds           Plate model based on
                                             MEGDR DEM, resolution
                                             4 pixels/degree.

         \begindata

            KERNELS_TO_LOAD = ( 'de430.bsp',
                                'mar097.bsp',
                                'pck00010.tpc',
                                'naif0011.tls',
                                'megr90n000cb_plate.bds' )
         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program subslr_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local parameters
         ./
         #define META            "subslr_ex1.tm"
         #define NMETH           4

         /.
         Local variables
         ./
         static SpiceChar      * method[NMETH] =
                                 {
                                    "Intercept/ellipsoid",
                                    "Near point/ ellipsoid",
                                    "Intercept/DSK/Unprioritized",
                                    "Nadir/DSK/Unprioritized"
                                 };

         SpiceDouble             et;
         SpiceDouble             f;
         SpiceDouble             radii  [3];
         SpiceDouble             re;
         SpiceDouble             rp;
         SpiceDouble             spclat;
         SpiceDouble             spclon;
         SpiceDouble             spcrad;
         SpiceDouble             spgalt;
         SpiceDouble             spglat;
         SpiceDouble             spglon;
         SpiceDouble             spoint [3];
         SpiceDouble             srfvec [3];
         SpiceDouble             sunlt;
         SpiceDouble             sunpos [3];
         SpiceDouble             sunst  [6];
         SpiceDouble             supcln;
         SpiceDouble             supclt;
         SpiceDouble             supcrd;
         SpiceDouble             supgal;
         SpiceDouble             supgln;
         SpiceDouble             supglt;
         SpiceDouble             trgepc;

         SpiceInt                i;
         SpiceInt                n;

         /.
         Load kernel files via the meta-kernel.
         ./
         furnsh_c ( META );

         /.
         Convert the UTC request time string to seconds past
         J2000, TDB.
         ./
         str2et_c ( "2008 aug 11 00:00:00", &et );

         /.
         Look up the target body's radii. We'll use these to
         convert Cartesian to planetographic coordinates. Use
         the radii to compute the flattening coefficient of
         the reference ellipsoid.
         ./
         bodvrd_c ( "MARS", "RADII", 3, &n, radii );

         /.
         Let `re' and `rp' be, respectively, the equatorial and
         polar radii of the target.
         ./
         re = radii[0];
         rp = radii[2];

         f  = ( re - rp ) / re;

         /.
         Compute sub-observer point using light time and stellar
         aberration corrections. Use both ellipsoid and DSK
         shape models, and use all of the "near point,"
         "intercept," and "nadir" sub-observer point definitions.
         ./
         for ( i = 0;  i < NMETH;  i++ )
         {
            subslr_c ( method[i],
                       "mars",  et,     "iau_mars", "cn+s",
                       "earth", spoint, &trgepc,    srfvec );

            /.
            Convert the sub-observer point's rectangular coordinates
            to planetographic longitude, latitude and altitude.
            Convert radians to degrees.
            ./
            recpgr_c ( "mars",  spoint,  re,     f,
                       &spglon, &spglat, &spgalt   );

            spglon *= dpr_c();
            spglat *= dpr_c();

            /.
            Convert the sub-solar point's rectangular coordinates to
            planetocentric radius, longitude, and latitude. Convert
            radians to degrees.
            ./
            reclat_c ( spoint, &spcrad, &spclon, &spclat );

            spclon *= dpr_c();
            spclat *= dpr_c();

            /.
            Compute the Sun's apparent position relative to the
            sub-solar point at `trgepc'. Add the position of the
            sub-solar point relative to the target's center to
            obtain the position of the sun relative to the target's
            center. Express the latter position in planetographic
            coordinates.
            ./
            spkcpo_c ( "sun",   trgepc,  "iau_mars", "OBSERVER",
                       "lt+s",  spoint,  "mars",     "iau_mars",
                       sunst,   &sunlt                          );

            vadd_c ( sunst, spoint, sunpos );

            recpgr_c ( "mars",  sunpos,  re,     f,
                       &supgln, &supglt, &supgal    );

            supgln *= dpr_c ();
            supglt *= dpr_c ();

            /.
            Convert the Sun's rectangular coordinates to
            planetocentric radius, longitude, and latitude.
            Convert radians to degrees.
            ./
            reclat_c ( sunpos, &supcrd, &supcln, &supclt );

            supcln *= dpr_c();
            supclt *= dpr_c();

            /.
            Write the results.
            ./
            printf ( "\n"
                     " Computation method = %s\n\n"
                     "  Sub-solar point altitude            (km) = %21.9f\n"
                     "  Sub-solar planetographic longitude (deg) = %21.9f\n"
                     "  Sun's planetographic longitude     (deg) = %21.9f\n"
                     "  Sub-solar planetographic latitude  (deg) = %21.9f\n"
                     "  Sun's planetographic latitude      (deg) = %21.9f\n"
                     "  Sub-solar planetocentric longitude (deg) = %21.9f\n"
                     "  Sun's planetocentric longitude     (deg) = %21.9f\n"
                     "  Sub-solar planetocentric latitude  (deg) = %21.9f\n"
                     "  Sun's planetocentric latitude      (deg) = %21.9f\n"
                     "\n",
                     method[i],
                     spgalt,
                     spglon,
                     supgln,
                     spglat,
                     supglt,
                     spclon,
                     supcln,
                     spclat,
                     supclt      );
         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       Computation method = Intercept/ellipsoid

        Sub-solar point altitude            (km) =           0.000000000
        Sub-solar planetographic longitude (deg) =         175.810675508
        Sun's planetographic longitude     (deg) =         175.810675508
        Sub-solar planetographic latitude  (deg) =          23.668550281
        Sun's planetographic latitude      (deg) =          23.420823362
        Sub-solar planetocentric longitude (deg) =        -175.810675508
        Sun's planetocentric longitude     (deg) =        -175.810675508
        Sub-solar planetocentric latitude  (deg) =          23.420819936
        Sun's planetocentric latitude      (deg) =          23.420819936


       Computation method = Near point/ ellipsoid

        Sub-solar point altitude            (km) =           0.000000000
        Sub-solar planetographic longitude (deg) =         175.810675408
        Sun's planetographic longitude     (deg) =         175.810675408
        Sub-solar planetographic latitude  (deg) =          23.420823362
        Sun's planetographic latitude      (deg) =          23.420823362
        Sub-solar planetocentric longitude (deg) =        -175.810675408
        Sun's planetocentric longitude     (deg) =        -175.810675408
        Sub-solar planetocentric latitude  (deg) =          23.175085578
        Sun's planetocentric latitude      (deg) =          23.420819936


       Computation method = Intercept/DSK/Unprioritized

        Sub-solar point altitude            (km) =          -4.052254284
        Sub-solar planetographic longitude (deg) =         175.810675512
        Sun's planetographic longitude     (deg) =         175.810675512
        Sub-solar planetographic latitude  (deg) =          23.668848891
        Sun's planetographic latitude      (deg) =          23.420823362
        Sub-solar planetocentric longitude (deg) =        -175.810675512
        Sun's planetocentric longitude     (deg) =        -175.810675512
        Sub-solar planetocentric latitude  (deg) =          23.420819936
        Sun's planetocentric latitude      (deg) =          23.420819936


       Computation method = Nadir/DSK/Unprioritized

        Sub-solar point altitude            (km) =          -4.022302438
        Sub-solar planetographic longitude (deg) =         175.810675412
        Sun's planetographic longitude     (deg) =         175.810675412
        Sub-solar planetographic latitude  (deg) =          23.420823362
        Sun's planetographic latitude      (deg) =          23.420823362
        Sub-solar planetocentric longitude (deg) =        -175.810675412
        Sun's planetocentric longitude     (deg) =        -175.810675412
        Sub-solar planetocentric latitude  (deg) =          23.174793924
        Sun's planetocentric latitude      (deg) =          23.420819936


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   S.C. Krening        (JPL)
   B.V. Semenov        (JPL)

-Version

   -CSPICE Version 2.0.1, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Fixed
       comments on example.

   -CSPICE Version 2.0.0, 05-APR-2017 (NJB) (SCK) (BVS)

       Fixed some header comment typos.

       Updated to support use of DSKs.

   -CSPICE Version 1.1.0, 08-MAY-2012 (NJB) (SCK)

       Some changes to computation of the surface point-to-sun vector
       were made in the underlying SPICELIB routine SUBSLR. These
       changes have a small effect on the outputs of this routine.

       -Exceptions removed: the observer and target are now
       permitted to coincide.

       The header example program was updated to reflect the new
       method of computing the apparent sun location, and the set
       of kernels referenced by the example meta-kernel were updated.
       The display of the program's output was updated accordingly.

       References to the new pxfrm2_c routine were added to the
       Detailed Output section.

   -CSPICE Version 1.0.1, 06-FEB-2009 (NJB)

       Incorrect frame name fixfrm was changed to fixref in
       documentation.

       In the header examples, meta-kernel names were updated to use
       the suffix

          ".tm"

   -CSPICE Version 1.0.0, 02-MAR-2008 (NJB)

-Index_Entries

   find sub-solar point on target body
   find nearest point to sun on target body

-&
*/

{ /* Begin subslr_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "subslr_c" );

   /*
   Check the input strings: method, target, fixref, abcorr, and obsrvr.
   Make sure none of the pointers are null and that each string
   contains at least one non-null character.
   */
   CHKFSTR ( CHK_STANDARD, "subslr_c", method );
   CHKFSTR ( CHK_STANDARD, "subslr_c", target );
   CHKFSTR ( CHK_STANDARD, "subslr_c", fixref );
   CHKFSTR ( CHK_STANDARD, "subslr_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "subslr_c", obsrvr );

   /*
   Call the f2c'd routine.
   */
   subslr_ ( ( char         * ) method,
             ( char         * ) target,
             ( doublereal   * ) &et,
             ( char         * ) fixref,
             ( char         * ) abcorr,
             ( char         * ) obsrvr,
             ( doublereal   * ) spoint,
             ( doublereal   * ) trgepc,
             ( doublereal   * ) srfvec,
             ( ftnlen         ) strlen(method),
             ( ftnlen         ) strlen(target),
             ( ftnlen         ) strlen(fixref),
             ( ftnlen         ) strlen(abcorr),
             ( ftnlen         ) strlen(obsrvr)  );

   chkout_c ( "subslr_c" );

} /* End subslr_c */
