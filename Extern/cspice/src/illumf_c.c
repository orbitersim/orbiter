/*

-Procedure illumf_c ( Illumination angles, general source, return flags )

-Abstract

   Compute the illumination angles---phase, incidence, and
   emission---at a specified point on a target body. Return logical
   flags indicating whether the surface point is visible from
   the observer's position and whether the surface point is
   illuminated.

   The target body's surface is represented using topographic data
   provided by DSK files, or by a reference ellipsoid.

   The illumination source is a specified ephemeris object.

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

   ANGLES
   GEOMETRY
   ILLUMINATION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef illumf_c


   void illumf_c ( ConstSpiceChar        * method,
                   ConstSpiceChar        * target,
                   ConstSpiceChar        * ilusrc,
                   SpiceDouble             et,
                   ConstSpiceChar        * fixref,
                   ConstSpiceChar        * abcorr,
                   ConstSpiceChar        * obsrvr,
                   ConstSpiceDouble        spoint [3],
                   SpiceDouble           * trgepc,
                   SpiceDouble             srfvec [3],
                   SpiceDouble           * phase,
                   SpiceDouble           * incdnc,
                   SpiceDouble           * emissn,
                   SpiceBoolean          * visibl,
                   SpiceBoolean          * lit       )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   method     I   Computation method.
   target     I   Name of target body.
   ilusrc     I   Name of illumination source.
   et         I   Epoch in TDB seconds past J2000 TDB.
   fixref     I   Body-fixed, body-centered target body frame.
   abcorr     I   Aberration correction flag.
   obsrvr     I   Name of observing body.
   spoint     I   Body-fixed coordinates of a target surface point.
   trgepc     O   Target surface point epoch.
   srfvec     O   Vector from observer to target surface point.
   phase      O   Phase angle at the surface point.
   incdnc     O   Source incidence angle at the surface point.
   emissn     O   Emission angle at the surface point.
   visibl     O   Visibility flag (SPICETRUE == visible).
   lit        O   Illumination flag (SPICETRUE == illuminated).

-Detailed_Input

   method      is a short string providing parameters defining
               the computation method to be used. In the syntax
               descriptions below, items delimited by brackets
               are optional.

               `method' may be assigned the following values:

                  "ELLIPSOID"

                     The illumination angle computation uses a
                     triaxial ellipsoid to model the surface of the
                     target body. The ellipsoid's radii must be
                     available in the kernel pool.


                  "DSK/UNPRIORITIZED[/SURFACES = <surface list>]"

                     The illumination angle computation uses
                     topographic data to model the surface of the
                     target body. These data must be provided by
                     loaded DSK files.

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


               Neither case nor white space are significant in `method',
               except within double-quoted strings representing
               surfaces. For example, the string " eLLipsoid " is valid.

               Within double-quoted strings representing surfaces, blank
               characters are significant, but multiple consecutive
               blanks are considered equivalent to a single blank. Case
               is not significant. So

                  \"Mars MEGDR 128 PIXEL/DEG\"

               is equivalent to

                  \" mars megdr  128  pixel/deg \"

               but not to

                  \"MARS MEGDR128PIXEL/DEG\"

   target      is the name of the target body. `target' is
               case-insensitive, and leading and trailing blanks in
               `target' are not significant. Optionally, you may
               supply a string containing the integer ID code for
               the object. For example both "MOON" and "301" are
               legitimate strings that indicate the Moon is the
               target body.

   ilusrc      is the name of the illumination source. This source
               may be any ephemeris object. Case, blanks, and
               numeric values are treated in the same way as for the
               input `target'.

   et          is the epoch, expressed as seconds past J2000 TDB,
               for which the apparent illumination angles at the
               specified surface point on the target body, as seen
               from the observing body, are to be computed.

   fixref      is the name of a body-fixed reference frame centered
               on the target body. `fixref' may be any such frame
               supported by the SPICE system, including built-in
               frames (documented in the Frames Required Reading)
               and frames defined by a loaded frame kernel (FK). The
               string `fixref' is case-insensitive, and leading and
               trailing blanks in `fixref' are not significant.

               The input surface point `spoint' and the output vector
               `srfvec' are expressed relative to this reference
               frame.

   abcorr      is the aberration correction to be used in computing
               the position and orientation of the target body and
               the location of the illumination source.

               For remote sensing applications, where the apparent
               illumination angles seen by the observer are desired,
               normally either of the corrections

                  "LT+S"
                  "CN+S"

               should be used. These and the other supported options
               are described below. `abcorr' may be any of the
               following:

                  "NONE"     No aberration correction.

               Let `lt' represent the one-way light time between the
               observer and the input surface point `spoint' (note: NOT
               between the observer and the target body's center). The
               following values of `abcorr' apply to the "reception" case
               in which photons depart from `spoint' at the light-time
               corrected epoch et-lt and *arrive* at the observer's
               location at `et':

                  "LT"       Correct both the position of `spoint' as
                             seen by the observer, and the position
                             of the illumination source as seen by
                             the target, for light time. Correct the
                             orientation of the target for light
                             time.

                  "LT+S"     Correct both the position of `spoint' as
                             seen by the observer, and the position
                             of the illumination source as seen by
                             the target, for light time and stellar
                             aberration. Correct the orientation of
                             the target for light time.

                  "CN"       Converged Newtonian light time
                             correction. In solving the light time
                             equations for `spoint' and the
                             illumination source, the "CN"
                             correction iterates until the solution
                             converges.

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


               The following values of `abcorr' apply to the
               "transmission" case in which photons *arrive* at
               `spoint' at the light-time corrected epoch et+lt and
               *depart* from the observer's location at `et':

                  "XLT"      "Transmission" case: correct for
                             one-way light time using a Newtonian
                             formulation. This correction yields the
                             illumination angles at the moment that
                             `spoint' receives photons emitted from the
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
                             formulation  This option modifies the
                             angles obtained with the "XLT" option
                             to account for the observer's and
                             target's velocities relative to the
                             solar system barycenter (the latter
                             velocity is used in computing the
                             direction to the apparent illumination
                             source).

                  "XCN"      Converged Newtonian light time
                             correction. This is the same as XLT
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

               Neither case nor white space are significant in
               `abcorr'. For example, the string

                 "Lt + s"

               is valid.

   obsrvr      is the name of the observing body. The observing body is
               an ephemeris object: it typically is a spacecraft, an
               extended body, or a surface point for which ephemeris data
               are available. `obsrvr' is case-insensitive, and leading
               and trailing blanks in `obsrvr' are not significant.
               Optionally, you may supply a string containing the
               integer ID code for the object. For example both
               "MOON" and "301" are legitimate strings that indicate
               the Moon is the observer.

               `obsrvr' may be not be identical to `target'.

   spoint      is a surface point on the target body, expressed in
               Cartesian coordinates, relative to the body-fixed
               target frame designated by `fixref'.

               `spoint' need not be visible from the observer's
               location at the epoch `et'.

               The components of `spoint' have units of km.

-Detailed_Output

   trgepc      is the "target surface point epoch." `trgepc' is defined as
               follows: letting `lt' be the one-way light time between the
               observer and the input surface point `spoint', `trgepc' is
               either the epoch et-lt, et+lt or `et' depending on whether
               the requested aberration correction is, respectively, for
               received radiation, transmitted radiation or omitted. `lt'
               is computed using the method indicated by `abcorr'.

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

                  dist = vnorm_c ( srfvec );

               The observer's position `obspos', relative to the
               target body's center, where the center's position is
               corrected for aberration effects as indicated by
               `abcorr', can be computed via the call:

                  vsub_c ( spoint, srfvec, obspos );

               To transform the vector `srfvec' from a reference frame
               `fixref' at time `trgepc' to a time-dependent reference frame
               `ref' at `et', the routine pxfrm2_c should be called. For
               example, let `xform' be 3x3 matrix representing the
               rotation from the body-fixed reference frame `fixref' at
               time `trgepc' to the time-dependent frame `ref' at time `et'.
               Then `srfvec' can be transformed to the result `refvec' as
               follows:

                  pxfrm2_c ( fixref, ref,    trgepc, et, xform );
                  mxv_c    ( xform,  srfvec, refvec            );


   The following outputs depend on the existence of a well-defined
   outward normal vector to the surface at `spoint'. See restriction 1.


   phase       is the phase angle at `spoint', as seen from `obsrvr' at time
               `et'. This is the angle between the negative of the vector
               `srfvec' and the spoint-illumination source vector at
               `trgepc'. Units are radians. The range of `phase' is [0, pi].
               See -Particulars below for a detailed discussion of the
               definition.

   incdnc      is the illumination source incidence angle at `spoint', as
               seen from `obsrvr' at time `et'. This is the angle between
               the surface normal vector at `spoint' and the spoint-source
               vector at `trgepc'. Units are radians. The range of `incdnc'
               is [0, pi]. See -Particulars below for a detailed
               discussion of the definition.

   emissn      is the emission angle at `spoint', as seen from `obsrvr' at
               time `et'. This is the angle between the surface normal
               vector at `spoint' and the negative of the vector `srfvec'.
               Units are radians. The range of `emissn' is [0, pi]. See
               -Particulars below for a detailed discussion of the
               definition.

   visibl      is a logical flag indicating whether the surface
               point is visible to the observer. `visibl' takes into
               account whether the target surface occults `spoint',
               regardless of the emission angle at `spoint'. `visibl' is
               returned with the value SPICETRUE if `spoint' is visible;
               otherwise it is SPICEFALSE.

   lit         is a logical flag indicating whether the surface
               point is illuminated; the point is considered to be
               illuminated if the vector from the point to the
               center of the illumination source doesn't intersect
               the target surface. `lit' takes into account whether
               the target surface casts a shadow on `spoint',
               regardless of the incidence angle at `spoint'. `lit' is
               returned with the value SPICETRUE if `spoint' is
               illuminated; otherwise it is SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If the specified aberration correction is relativistic or
       calls for stellar aberration but not light time correction,
       the error SPICE(NOTSUPPORTED) is signaled by a routine in the
       call tree of this routine.

   2)  If the specified aberration correction is any other
       unrecognized value, an error is signaled
       by a routine in the call tree of this routine.

   3)  If any of the target, observer, or illumination source input
       strings cannot be converted to an integer ID code, the error
       SPICE(IDCODENOTFOUND) is signaled by a routine in the call
       tree of this routine.

   4)  If `obsrvr' and `target' map to the same NAIF integer ID code, the
       error SPICE(BODIESNOTDISTINCT) is signaled by a routine in the
       call tree of this routine.

   5)  If the input target body-fixed frame `fixref' is not recognized,
       the error SPICE(NOFRAME) is signaled by a routine in the call
       tree of this routine. A frame name may fail to be recognized
       because a required frame specification kernel has not been
       loaded; another cause is a misspelling of the frame name.

   6)  If the input frame `fixref' is not centered at the target body,
       the error SPICE(INVALIDFRAME) is signaled by a routine in the
       call tree of this routine.

   7)  If the input argument `method' cannot be parsed, an error
       is signaled by either this routine or a routine in the
       call tree of this routine.

   8)  If insufficient ephemeris data have been loaded prior to
       calling illumf_c, an error is signaled by a
       routine in the call tree of this routine. Note that when
       light time correction is used, sufficient ephemeris data must
       be available to propagate the states of observer, target, and
       the illumination source to the solar system barycenter.

   9)  If the computation method specifies an ellipsoidal target
       shape and triaxial radii of the target body have not been
       loaded into the kernel pool prior to calling illumf_c, an error
       is signaled by a routine in the call tree of this routine.

   10) If PCK data specifying the target body-fixed frame orientation
       have not been loaded prior to calling illumf_c, an error is
       signaled by a routine in the call tree of this routine.

   11) If `method' specifies that the target surface is represented by
       DSK data, and no DSK files are loaded for the specified
       target, an error is signaled by a routine in the call tree
       of this routine.

   12) If `method' specifies that the target surface is represented by
       DSK data, and data representing the portion of the surface on
       which `spoint' is located are not available, an error is
       signaled by a routine in the call tree of this routine.

   13) If `method' specifies that the target surface is represented
       by DSK data, `spoint' must lie on the target surface, not above
       or below it. A small tolerance is used to allow for round-off
       error in the calculation determining whether `spoint' is on the
       surface.

       If, in the DSK case, `spoint' is too far from the surface, an
       error is signaled by a routine in the call tree of this
       routine.

       If the surface is represented by a triaxial ellipsoid, `spoint'
       is not required to be close to the ellipsoid; however, the
       results computed by this routine will be unreliable if `spoint'
       is too far from the ellipsoid.

   14) If radii for `target' are not found in the kernel pool, an error
       is signaled by a routine in the call tree of this routine.

   15) If the size of the `target' body radii kernel variable is not
       three, an error is signaled by a routine in the call tree of
       this routine.

   16) If any of the three `target' body radii is less-than or equal to
       zero, an error is signaled by a routine in the call tree of
       this routine.

   17) If any of the `method', `target', `ilusrc', `fixref', `abcorr'
       or `obsrvr' input string pointers is null, the error
       SPICE(NULLPOINTER) is signaled.

   18) If any of the `method', `target', `ilusrc', `fixref', `abcorr'
       or `obsrvr' input strings has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   Appropriate kernels must be loaded by the calling program before
   this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for target, observer, and the
      illumination source must be loaded. If aberration
      corrections are used, the states of target, observer, and
      the illumination source relative to the solar system
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
           modeled by DSK data, and the DSK NADIR method is
           selected.

        DSK data:

           If the target shape is modeled by DSK data, DSK files
           containing topographic data for the target body must be
           loaded. If a surface list is specified, data for at
           least one of the listed surfaces must be loaded. DSK
           files are loaded via furnsh_c.

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
      of such assignments is

         NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG'
         NAIF_SURFACE_CODE += 1
         NAIF_SURFACE_BODY += 499

   -  SCLK data: if the target body's orientation is provided by
      CK files, an associated SCLK kernel must be loaded.


   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   CSPICE contains four routines that compute illumination angles:

      illumf_c (this routine)

      illumg_c (same as this routine, except that illumination
                and visibility flags are not returned)

      ilumin_c (same as illumg_c, except that the sun is fixed
                as the illumination source)

      illum_c  (deprecated)


   illumf_c is the most capable of the set.


   Illumination angles
   ===================

   The term "illumination angles" refers to the following set of
   angles:


      phase angle              Angle between the vectors from the
                               surface point to the observer and
                               from the surface point to the
                               illumination source.

      incidence angle          Angle between the surface normal at
                               the specified surface point and the
                               vector from the surface point to the
                               illumination source.

      emission angle           Angle between the surface normal at
                               the specified surface point and the
                               vector from the surface point to the
                               observer.

   The diagram below illustrates the geometric relationships
   defining these angles. The labels for the incidence, emission,
   and phase angles are "inc.", "e.", and "phase".


                                                    *
                                            illumination source

                  surface normal vector
                            ._                 _.
                            |\                 /|  illumination
                              \    phase      /    source vector
                               \   .    .    /
                               .            .
                                 \   ___   /
                            .     \/     \/
                                  _\ inc./
                           .    /   \   /
                           .   |  e. \ /
       *             <--------------- *  surface point on
    viewing            vector            target body
    location           to viewing
    (observer)         location


   Note that if the target-observer vector, the target normal vector
   at the surface point, and the target-illumination source vector
   are coplanar, then phase is the sum of the incidence and emission
   angles. This rarely occurs; usually

      phase angle  <  incidence angle + emission angle

   All of the above angles can be computed using light time
   corrections, light time and stellar aberration corrections, or no
   aberration corrections. In order to describe apparent geometry as
   observed by a remote sensing instrument, both light time and
   stellar aberration corrections should be used.

   The way aberration corrections are applied by this routine
   is described below.

      Light time corrections
      ======================

         Observer-target surface point vector
         ------------------------------------

         Let `et' be the epoch at which an observation or remote
         sensing measurement is made, and let et-lt ("lt" stands
         for "light time") be the epoch at which the photons
         received at `et' were emitted from the surface point `spoint'.
         Note that the light time between the surface point and
         observer will generally differ from the light time between
         the target body's center and the observer.


         Target body's orientation
         -------------------------

         Using the definitions of `et' and `lt' above, the target body's
         orientation at et-lt is used. The surface normal is
         dependent on the target body's orientation, so the body's
         orientation model must be evaluated for the correct epoch.


         Target body -- illumination source vector
         -----------------------------------------

         The surface features on the target body near `spoint' will
         appear in a measurement made at `et' as they were at et-lt.
         In particular, lighting on the target body is dependent on
         the apparent location of the illumination source as seen
         from the target body at et-lt. So, a second light time
         correction is used to compute the position of the
         illumination source relative to the surface point.


      Stellar aberration corrections
      ==============================

      Stellar aberration corrections are applied only if
      light time corrections are applied as well.

         Observer-target surface point body vector
         -----------------------------------------

         When stellar aberration correction is performed, the
         direction vector `srfvec' is adjusted so as to point to the
         apparent position of `spoint': considering `spoint' to be an
         ephemeris object, `srfvec' points from the observer's
         position at `et' to the light time and stellar aberration
         corrected position of `spoint'.

         Target body-illumination source vector
         --------------------------------------

         The target body-illumination source vector is the apparent
         position of the illumination source, corrected for light
         time and stellar aberration, as seen from the target body
         at time et-lt.


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
      -------------------------------------

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

         "SURFACES ="

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

         "SURFACES = "\"Mars MEGDR 128 PIXEL/DEG\""

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

         "DSK/UNPRIORITIZED/SURFACES = \"Mars MEGDR 64 PIXEL/DEG\", 3"


      Aberration corrections using DSK data
      -------------------------------------

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
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the phase, solar incidence, and emission angles at the
      sub-solar and sub-spacecraft points on Mars as seen from the Mars
      Global Surveyor spacecraft at a specified UTC time.

      Use both an ellipsoidal Mars shape model and topographic data
      provided by a DSK file. For both surface points, use the "near
      point" and "nadir" definitions for ellipsoidal and DSK shape
      models, respectively.

      Use converged Newtonian light time and stellar aberration
      corrections.

      The topographic model is based on data from the MGS MOLA DEM
      megr90n000cb, which has a resolution of 4 pixels/degree. A
      triangular plate model was produced by computing a 720 x 1440
      grid of interpolated heights from this DEM, then tessellating
      the height grid. The plate model is stored in a type 2 segment
      in the referenced DSK file.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: illumf_ex1.tm

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
            mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris
            megr90n000cb_plate.bds           Plate model based on
                                             MEGDR DEM, resolution
                                             4 pixels/degree.

         \begindata

            KERNELS_TO_LOAD = ( 'de430.bsp',
                                'mar097.bsp',
                                'pck00010.tpc',
                                'naif0011.tls',
                                'mgs_ext12_ipng_mgs95j.bsp',
                                'megr90n000cb_plate.bds'      )
         \begintext


      Example code begins here.


      /.
         Program illumf_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local constants
         ./
         #define                 META   "illumf_ex1.tm"
         #define                 MTHLEN 81
         #define                 NMETH  2

         /.
         Local variables
         ./
         SpiceBoolean            ssclit;
         SpiceBoolean            sscvis;
         SpiceBoolean            ssllit;
         SpiceBoolean            sslvis;

         SpiceChar             * abcorr;
         SpiceChar             * fixref;
         SpiceChar               ilumth [NMETH][MTHLEN] =
            {"Ellipsoid", "DSK/Unprioritized" };

         SpiceChar             * ilusrc;
         SpiceChar             * target;
         SpiceChar             * obsrvr;
         SpiceChar               submth [NMETH][MTHLEN] =
            {"Near Point/Ellipsoid", "DSK/Nadir/Unprioritized" };

         SpiceChar             * utc;

         SpiceDouble             et;
         SpiceDouble             srfvec [3];
         SpiceDouble             sscemi;
         SpiceDouble             sscphs;
         SpiceDouble             sscpt  [3];
         SpiceDouble             sscsol;
         SpiceDouble             sslemi;
         SpiceDouble             sslphs;
         SpiceDouble             sslsol;
         SpiceDouble             ssolpt [3];
         SpiceDouble             trgepc;

         SpiceInt                i;


         /.
         Load kernel files.
         ./
         furnsh_c ( META );

         /.
         Convert the UTC request time string to seconds past
         J2000 TDB.
         ./
         utc = "2003 OCT 13 06:00:00 UTC";

         str2et_c ( utc, &et );

         printf ( "\n"
                  "UTC epoch is %s\n", utc );


         /.
         Assign observer, target, and illumination source
         names. The acronym MGS indicates Mars Global
         Surveyor. See NAIF_IDS for a list of names
         recognized by SPICE.

         Also set the target body-fixed frame and
         the aberration correction flag.
         ./

         target = "Mars";
         obsrvr = "MGS";
         ilusrc = "Sun";
         fixref = "IAU_MARS";
         abcorr = "CN+S";

         for ( i = 0;  i < NMETH;  i++ )
         {
            /.
            Find the sub-solar point on Mars as
            seen from the MGS spacecraft at `et'. Use the
            "near point" style of sub-point definition
            when the shape model is an ellipsoid, and use
            the "nadir" style when the shape model is
            provided by DSK data. This makes it easy to
            verify the solar incidence angle when
            the target is modeled as an  ellipsoid.
            ./
            subslr_c ( submth[i], target,  et,
                       fixref,    abcorr,  obsrvr,
                       ssolpt,    &trgepc, srfvec  );

            /.
            Now find the sub-spacecraft point.
            ./
            subpnt_c ( submth[i], target,  et,
                       fixref,    abcorr,  obsrvr,
                       sscpt,     &trgepc, srfvec  );

            /.
            Find the phase, solar incidence, and emission
            angles at the sub-solar point on Mars as
            seen from MGS at time `et'.
            ./
            illumf_c ( ilumth[i], target,  ilusrc,
                       et,        fixref,  abcorr,
                       obsrvr,    ssolpt,  &trgepc,
                       srfvec,    &sslphs, &sslsol,
                       &sslemi,   &sslvis, &ssllit   );

            /.
            Do the same for the sub-spacecraft point.
            ./
            illumf_c ( ilumth[i], target,  ilusrc,
                       et,        fixref,  abcorr,
                       obsrvr,    sscpt,   &trgepc,
                       srfvec,    &sscphs, &sscsol,
                       &sscemi,   &sscvis, &ssclit   );

            /.
            Convert the angles to degrees and write them out.
            ./
            sslphs *= dpr_c();
            sslsol *= dpr_c();
            sslemi *= dpr_c();

            sscphs *= dpr_c();
            sscsol *= dpr_c();
            sscemi *= dpr_c();

            printf ( "\n"
                     "   illumf_c method: %s\n"
                     "   subpnt_c method: %s\n"
                     "   subslr_c method: %s\n"
                     "\n"
                     "      Illumination angles at the "
                     "sub-solar point:\n"
                     "\n"
                     "      Phase angle            (deg): %15.9f\n"
                     "      Solar incidence angle  (deg): %15.9f\n"
                     "      Emission angle         (deg): %15.9f\n"
                     "      Visible, Lit flags "
                     "(0 == false, 1 == true):  %d, %d\n",
                     ilumth[i],
                     submth[i],
                     submth[i],
                     sslphs,
                     sslsol,
                     sslemi,
                     (int)sslvis,
                     (int)ssllit                                    );

            if ( i == 0 )
            {
               printf ( "        The solar incidence angle "
                        "should be 0.\n"
                        "        The emission and phase "
                        "angles should be equal.\n"          );
            }

            printf ( "\n"
                     "      Illumination angles at the "
                     "sub-s/c point:\n"
                     "\n"
                     "      Phase angle            (deg): %15.9f\n"
                     "      Solar incidence angle  (deg): %15.9f\n"
                     "      Emission angle         (deg): %15.9f\n"
                     "      Visible, Lit flags "
                     "(0 == false, 1 == true):  %d, %d\n",
                     sscphs,
                     sscsol,
                     sscemi,
                     (int)sscvis,
                     (int)ssclit                                    );

            if ( i == 0 )
            {
               printf ( "        The emission angle "
                        "should be 0.\n"
                        "        The solar incidence "
                        "and phase angles should be equal.\n"  );
            }
         }
         printf ( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      UTC epoch is 2003 OCT 13 06:00:00 UTC

         illumf_c method: Ellipsoid
         subpnt_c method: Near Point/Ellipsoid
         subslr_c method: Near Point/Ellipsoid

            Illumination angles at the sub-solar point:

            Phase angle            (deg):   138.370270685
            Solar incidence angle  (deg):     0.000000000
            Emission angle         (deg):   138.370270685
            Visible, Lit flags (0 == false, 1 == true):  0, 1
              The solar incidence angle should be 0.
              The emission and phase angles should be equal.

            Illumination angles at the sub-s/c point:

            Phase angle            (deg):   101.439331040
            Solar incidence angle  (deg):   101.439331041
            Emission angle         (deg):     0.000000002
            Visible, Lit flags (0 == false, 1 == true):  1, 0
              The emission angle should be 0.
              The solar incidence and phase angles should be equal.

         illumf_c method: DSK/Unprioritized
         subpnt_c method: DSK/Nadir/Unprioritized
         subslr_c method: DSK/Nadir/Unprioritized

            Illumination angles at the sub-solar point:

            Phase angle            (deg):   138.387071678
            Solar incidence angle  (deg):     0.967122745
            Emission angle         (deg):   137.621480599
            Visible, Lit flags (0 == false, 1 == true):  0, 1

            Illumination angles at the sub-s/c point:

            Phase angle            (deg):   101.439331359
            Solar incidence angle  (deg):   101.555993667
            Emission angle         (deg):     0.117861156
            Visible, Lit flags (0 == false, 1 == true):  1, 0


-Restrictions

   1)  Results from this routine are not meaningful if the input
       point lies on a ridge or vertex of a surface represented by
       DSK data, or if for any other reason the direction of the
       outward normal vector at the point is undefined.

   2)  The illumination state indicated by the output argument `lit'
       is computed treating the illumination source as a single
       point. Surface points that are illuminated by part of the
       source are classified as "lit" or not depending on whether the
       center of the source is visible from those points.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)

-Version

   -CSPICE Version 1.0.1, 20-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       -Restrictions section.

   -CSPICE Version 1.0.0, 04-APR-2017 (NJB) (BVS)

      16-AUG-2016 (NJB) (BVS)

-Index_Entries

   illumination angles general source with flags
   lighting angles general source with flags
   phase angle general source with flags
   incidence angle general source with flags
   emission angle general source with flags

-&
*/

{ /* Begin illumf_c */


   /*
   Local variables
   */
   logical                 loclit;
   logical                 locvis;

   /*
   Participate in error tracing.
   */
   chkin_c ( "illumf_c" );


   /*
   Check the input strings: target, fixref, abcorr, and obsrvr. Make
   sure none of the pointers are null and that each string contains at
   least one non-null character.
   */
   CHKFSTR ( CHK_STANDARD, "illumf_c", method );
   CHKFSTR ( CHK_STANDARD, "illumf_c", target );
   CHKFSTR ( CHK_STANDARD, "illumf_c", ilusrc );
   CHKFSTR ( CHK_STANDARD, "illumf_c", fixref );
   CHKFSTR ( CHK_STANDARD, "illumf_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "illumf_c", obsrvr );

   /*
   Call the f2c'd routine.
   */
   illumf_ ( ( char         * ) method,
             ( char         * ) target,
             ( char         * ) ilusrc,
             ( doublereal   * ) &et,
             ( char         * ) fixref,
             ( char         * ) abcorr,
             ( char         * ) obsrvr,
             ( doublereal   * ) spoint,
             ( doublereal   * ) trgepc,
             ( doublereal   * ) srfvec,
             ( doublereal   * ) phase,
             ( doublereal   * ) incdnc,
             ( doublereal   * ) emissn,
             ( logical      * ) &locvis,
             ( logical      * ) &loclit,
             ( ftnlen         ) strlen(method),
             ( ftnlen         ) strlen(target),
             ( ftnlen         ) strlen(ilusrc),
             ( ftnlen         ) strlen(fixref),
             ( ftnlen         ) strlen(abcorr),
             ( ftnlen         ) strlen(obsrvr)  );

   /*
   Set output SpiceBoolean arguments.
   */

   *visibl = (SpiceBoolean)locvis;
   *lit    = (SpiceBoolean)loclit;

   chkout_c ( "illumf_c" );

} /* End illumf_c */
