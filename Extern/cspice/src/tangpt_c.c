/*

-Procedure tangpt_c ( Ray-ellipsoid tangent point )

-Abstract

   Compute, for a given observer, ray emanating from the observer,
   and target, the "tangent point": the point on the ray nearest
   to the target's surface. Also compute the point on the target's
   surface nearest to the tangent point.

   The locations of both points are optionally corrected for light
   time and stellar aberration.

   The surface shape is modeled as a triaxial ellipsoid.

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

   ABCORR
   CK
   FRAMES
   NAIF_IDS
   PCK
   SCLK
   SPK
   TIME

-Keywords

   ELLIPSOID
   GEOMETRY
   RAY

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void tangpt_c ( ConstSpiceChar    * method,
                   ConstSpiceChar    * target,
                   SpiceDouble         et,
                   ConstSpiceChar    * fixref,
                   ConstSpiceChar    * abcorr,
                   ConstSpiceChar    * corloc,
                   ConstSpiceChar    * obsrvr,
                   ConstSpiceChar    * dref,
                   ConstSpiceDouble    dvec   [3],
                   SpiceDouble         tanpt  [3],
                   SpiceDouble       * alt,
                   SpiceDouble       * range,
                   SpiceDouble         srfpt  [3],
                   SpiceDouble       * trgepc,
                   SpiceDouble         srfvec [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   method     I   Computation method.
   target     I   Name of target body.
   et         I   Epoch in ephemeris seconds past J2000 TDB.
   fixref     I   Body-fixed, body-centered target body frame.
   abcorr     I   Aberration correction.
   corloc     I   Aberration correction locus: "TANGENT POINT" or
                  "SURFACE POINT".
   obsrvr     I   Name of observing body.
   dref       I   Reference frame of ray direction vector.
   dvec       I   Ray direction vector.
   tanpt      O   "Tangent point": point on ray nearest to surface.
   alt        O   Altitude of tangent point above surface.
   range      O   Distance of tangent point from observer.
   srfpt      O   Point on surface nearest to tangent point.
   trgepc     O   Epoch associated with correction locus.
   srfvec     O   Vector from observer to surface point `srfpt'.

-Detailed_Input

   method      is a short string providing parameters defining
               the computation method to be used.

               `method' is currently restricted to the value

                  "ELLIPSOID"

               This value indicates that the target shape is
               modeled as a triaxial ellipsoid.

               `method' is case-insensitive, and leading and trailing
               blanks in `method' are not significant.

   target      is the name of the target body. `target' is
               case-insensitive, and leading and trailing blanks in
               `target' are not significant. Optionally, you may
               supply a string containing the integer ID code
               for the object. For example both "MOON" and "301"
               are legitimate strings that indicate the Moon is the
               target body.

               If the target is identified by name rather than ID code,
               the target name must be recognized by SPICE. Radii
               defining a triaxial ellipsoid target shape model must be
               available in the kernel pool. See the -Files section
               below.

   et          is the epoch associated with the observer, expressed as
               ephemeris seconds past J2000 TDB. `et' is the epoch at
               which radiation is received by the observer, when an
               observation is made, or in the case of transmission from
               the observer, at which radiation is emitted.

               `et' is the epoch at which the state of the observer
               relative to the solar system barycenter is computed.

               When aberration corrections are not used, `et' is also
               the epoch at which the state and orientation of the
               target body are computed.

               When aberration corrections are used, the position
               and orientation of the target body are computed at
               et-lt or et+lt, where `lt' is the one-way light time
               between the aberration correction locus and the
               observer. The sign applied to `lt' depends on the
               selected correction. See the descriptions of `abcorr'
               and `corloc' below for details.

   fixref      is the name of a body-fixed reference frame centered on
               the target body. `fixref' may be any such frame supported
               by the SPICE system, including built-in frames
               (documented in frames.req) and frames defined by a
               loaded frame kernel (FK). The string `fixref' is
               case-insensitive, and leading and trailing blanks in
               `fixref' are not significant.

               The output points `tanpt' and `srfpt', and the
               observer-to-surface point vector `srfvec' will be
               expressed relative to this reference frame.

   abcorr      indicates the aberration corrections to be applied
               when computing the target's position and orientation.

               See the description of the aberration correction
               locus `corloc' for further details on how aberration
               corrections are applied.

               For remote sensing applications, where the apparent
               tangent or surface point seen by the observer is
               desired, normally one of the corrections

                  "CN+S" or "NONE"

               should be selected. For applications involving
               transmission from the observer, normally "XCN+S" or
               "NONE" should be selected.

               Light-time-only corrections can be useful for
               testing but generally don't accurately model geometry
               applicable to remote sensing observations or signal
               transmission.

               The supported options are described below.

               `abcorr' may be any of the following:

                  "NONE"     Compute outputs without applying
                             aberration corrections.

                             "NONE" may be suitable when the
                             magnitudes of the aberration
                             corrections are negligible.

               Let `lt' represent the one-way light time between the
               observer and the aberration correction locus specified
               by `corloc'. The following values of `abcorr' apply to the
               "reception" case in which radiation departs from the
               aberration correction locus at the light-time corrected
               epoch et-lt and arrives at the observer's location at
               `et':

                  "LT"       Correct for one-way light time between
                             the aberration correction locus and
                             the observer, using a Newtonian
                             formulation. This correction yields the
                             position of the aberration correction
                             locus at the moment it emitted radiation
                             arriving at the observer at `et'.

                             The light time correction uses an
                             iterative solution of the light time
                             equation. The solution invoked by the
                             "LT" option uses several iterations
                             but does not guarantee convergence.

                             Both the target position as seen by the
                             observer, and rotation of the target
                             body, are corrected for light time.

                  "LT+S"     Correct for one-way light time and
                             stellar aberration using a Newtonian
                             formulation. This option modifies the
                             aberration correction locus solution
                             obtained with the "LT" option to
                             account for the observer's velocity
                             relative to the solar system
                             barycenter. These corrections yield the
                             apparent aberration correction locus.

                  "CN"       Converged Newtonian light time
                             correction. In solving the light time
                             equation, the "CN" correction iterates
                             until either the solution converges or
                             a large iteration limit is reached.
                             Both the position and orientation of
                             the target body are corrected for light
                             time.

                  "CN+S"     Converged Newtonian light time and stellar
                             aberration corrections. This option
                             produces a solution that is at least as
                             accurate at that obtainable with the
                             "LT+S" option. Whether the "CN+S" solution
                             is substantially more accurate depends on
                             the geometry of the participating objects
                             and on the accuracy of the input data. In
                             some cases this routine will execute more
                             slowly when a converged solution is
                             computed.

                             For reception-case applications where
                             aberration corrections are applied, this
                             option should be used, unless the
                             magnitudes of the corrections are
                             negligible.

               The following values of `abcorr' apply to the
               "transmission" case in which radiation *departs* from
               the observer's location at `et' and arrives at the
               aberration correction locus at the light-time
               corrected epoch et+lt:

                  "XLT"      "Transmission" case: correct for
                             one-way light time between the
                             aberration correction locus and the
                             observer, using a Newtonian
                             formulation. This correction yields the
                             position of the aberration correction
                             locus at the moment it receives radiation
                             emitted from the observer's location at
                             `et'.

                             The light time correction uses an
                             iterative solution of the light time
                             equation. The solution invoked by the
                             "XLT" option uses several iterations
                             but does not guarantee convergence.

                             Both the target position as seen by the
                             observer, and rotation of the target
                             body, are corrected for light time.

                  "XLT+S"    "Transmission" case: correct for
                             one-way light time and stellar
                             aberration using a Newtonian
                             formulation. This option modifies the
                             aberration correction locus solution
                             obtained with the "XLT" option to
                             account for the observer's velocity
                             relative to the solar system
                             barycenter.

                             Stellar aberration is computed for
                             transmitted, rather than received,
                             radiation.

                             These corrections yield the analog for
                             the transmission case of the apparent
                             aberration correction locus.

                  "XCN"      "Transmission" case: converged Newtonian
                             light time correction. In solving the
                             light time equation, the "XCN" correction
                             iterates until either the solution
                             converges or a large iteration limit is
                             reached. Both the position and rotation of
                             the target body are corrected for light
                             time.

                  "XCN+S"    "Transmission" case: converged Newtonian
                             light time and stellar aberration
                             corrections. This option produces a
                             solution that is at least as accurate at
                             that obtainable with the "XLT+S" option.
                             Whether the "XCN+S" solution is
                             substantially more accurate depends on the
                             geometry of the participating objects and
                             on the accuracy of the input data. In some
                             cases this routine will execute more
                             slowly when a converged solution is
                             computed.

                             For transmission-case applications where
                             aberration corrections are applied, this
                             option should be used, unless the
                             magnitudes of the corrections are
                             negligible.

               Case and embedded blanks are not significant in
               `abcorr'. For example, the string

                  "Cn + s"

               is valid.

   corloc      specifies the aberration correction "locus," which is
               the fixed point in the frame designated by `fixref' for
               which light time and stellar aberration corrections are
               computed.

               Differential aberration effects across the surface of
               the target body are not considered by this routine. When
               aberration corrections are used, the effective positions
               of the observer and target, and the orientation of the
               target, are computed according to the corrections
               determined for the aberration correction locus.

               The light time used to determine the position and
               orientation of the target body is that between the
               aberration correction locus and the observer.

               The stellar aberration correction applied to the
               position of the target is that computed for the
               aberration correction locus.

               The descriptions below apply only when aberration
               corrections are used.

               The values and meanings of `corloc' are:

                  "TANGENT POINT"    Compute corrections at the
                                     "tangent point," which is the
                                     point on the ray, defined by `dref'
                                     and `dvec', nearest to the target's
                                     surface.

                  "SURFACE POINT"    Compute corrections at the
                                     point on the target's surface
                                     nearest to the tangent point.

               Case and leading and trailing blanks are not significant
               in `corloc'.

   obsrvr      is the name of the observing body. This is typically
               a spacecraft or a surface point on an extended
               ephemeris object. `obsrvr' is case-insensitive, and
               leading and trailing blanks in `obsrvr' are not
               significant. Optionally, you may supply a string
               containing the integer ID code for the object. For
               example both "MOON" and "301" are legitimate strings
               that indicate the Moon is the observer.

               If the observer is identified by name rather than ID
               code, the observer name must be recognized by SPICE. See
               the -Files section below.

   dref        is the name of the reference frame relative to which
               the ray direction vector is expressed. This may be
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
               The tangent point on the ray and the point on the target
               body's surface nearest to the tangent point are sought.

               `dvec' is specified relative to the reference frame
               designated by `dref'.

               Non-inertial reference frames are treated as follows:
               if the center of the frame is at the observer's
               location, the frame's orientation is evaluated at `et'.
               If the frame's center is located elsewhere, then
               letting `ltcent' be the one-way light time between the
               observer and the central body associated with the
               frame, the orientation of the frame is evaluated at
               et-ltcent, et+ltcent, or `et' depending on whether the
               requested aberration correction is, respectively, for
               received radiation, transmitted radiation, or is
               omitted. `ltcent' is computed using the method
               indicated by `abcorr'.

-Detailed_Output

   tanpt       is the "tangent point": the point on the ray defined by
               `dref' and `dvec' nearest to the target body's surface.

               `tanpt' is a vector originating at the target body's
               center, expressed in the reference frame designated
               by `fixref', the orientation of which is evaluated at
               `trgepc' (see description below). Units are km.

               If the ray intersects the surface, `tanpt' is the
               nearest point of intersection to the observer.

               If the ray points away from the surface---that is, if
               the angle between the ray and the outward normal at the
               target surface point nearest to the observer, computed
               using the specified aberration corrections, is less than
               or equal to 90 degrees---then `tanpt' is set to the
               position of the observer relative to the target center.

               `tanpt' is computed using the aberration corrections
               specified by `abcorr' and `corloc'.

               When the aberration correction locus is set to
               "TANGENT POINT", and the position of `tanpt' is
               corrected for aberration as specified by `abcorr', the
               resulting point will lie on the input ray.

   alt         is the altitude of the tangent point above the
               target body's surface. This is the distance between
               `tanpt' and `srfpt'. Units are km.

               If the ray intersects the surface, `alt' is set to the
               exact double precision value 0.0. `alt' may be used as
               an indicator of whether a ray-surface intersection
               exists.

   range       is the distance between the observer and the tangent
               point. Units are km.

               If the ray points away from the surface (see the
               description of `tanpt' above), `range' is set to the
               exact double precision value 0.0. `range' may be used
               as an indicator of whether this geometric condition
               exists.

   srfpt       is the point on the target body's surface nearest to the
               tangent point.

               `srfpt' is a vector originating at the target body's
               center, expressed in the reference frame designated
               by `fixref', the orientation of which is evaluated at
               `trgepc' (see description below). Units are km.

               `srfpt' is computed using the aberration corrections
               specified by `abcorr' and `corloc'.

               When the aberration correction locus is set to
               "SURFACE POINT", and the position of `srfpt' is
               corrected for aberration as specified by `abcorr', the
               resulting point will lie on the ray emanating from
               the observer and pointing in the direction of `srfvec'.

               If the ray intersects the surface, `srfpt' is the point of
               intersection nearest to the observer.

               If the ray points away from the surface (see the
               description of `tanpt' above), `srfpt' is set to the target
               surface point nearest to the observer.

   trgepc      is the epoch associated with the aberration correction
               locus. `trgepc' is defined as follows: letting `lt' be the
               one-way light time between the observer and the
               aberration correction locus, `trgepc' is the epoch et-lt,
               et+lt, or `et' depending on whether the requested
               aberration correction is, respectively, for received
               radiation, transmitted radiation, or omitted. `lt' is
               computed using the method indicated by `abcorr'.

               `trgepc' is expressed as seconds past J2000 TDB.

               The name `trgepc', which stands for "target epoch,"
               is used for compatibility with other SPICE high-level
               geometry routines. Note that the epoch it designates
               is not associated with the target body's center.

   srfvec      is the vector from the observer's position at `et' to
               the surface point `srfpt', where the position of `srfpt'
               is corrected for aberrations as specified by `abcorr'
               and `corloc'. `srfvec' is expressed in the target
               body-fixed reference frame designated by `fixref',
               evaluated at `trgepc'. Units are km.

               One can use the CSPICE function vnorm_c to obtain the
               distance between the observer and `srfpt':

                  dist = vnorm_c ( srfvec );

               The observer's position `obspos', relative to the
               target body's center, where the center's position is
               corrected for aberration effects as indicated by
               `abcorr' and `corloc', can be computed via the call:

                  vsub_c ( srfpt, srfvec, obspos );

               To transform the vector `srfvec' from the reference frame
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

   2)  If `method' is not equivalent to "ELLIPSOID", when case and
       blanks are ignored in the comparison, the error
       SPICE(NOTSUPPORTED) is signaled by a routine in the call tree
       of this routine.

   3)  If `corloc' is not equivalent to either "TANGENT POINT" or
       "SURFACE POINT", when case and blanks are ignored, the error
       SPICE(NOTSUPPORTED) is signaled by a routine in the call tree
       of this routine.

   4)  If the direction vector `dvec' is the zero vector, the error
       SPICE(ZEROVECTOR) is signaled by a routine in the call tree of
       this routine.

   5)  If either the target or observer input strings cannot be
       converted to an integer ID code, the error
       SPICE(IDCODENOTFOUND) is signaled by a routine in the call
       tree of this routine.

   6)  If `obsrvr' and `target' map to the same NAIF integer ID code, the
       error SPICE(BODIESNOTDISTINCT) is signaled by a routine in the
       call tree of this routine.

   7)  If triaxial radii of the target body have not been loaded
       into the kernel pool prior to a call to this routine, an
       error is signaled by a routine in the call tree of this
       routine.

   8)  If the number of radii associated with the target body is not
       three, the error SPICE(INVALIDCOUNT) is signaled by a routine
       in the call tree of this routine.

   9)  If the input target body-fixed frame `fixref' is not recognized,
       the error SPICE(NOFRAME) is signaled by a routine in the call
       tree of this routine. A frame name may fail to be recognized
       because a required frame specification kernel has not been
       loaded; another cause is a misspelling of the frame name.

   10) If the input frame `fixref' is not centered at the target body,
       the error SPICE(INVALIDFRAME) is signaled by a routine in the
       call tree of this routine.

   11) If the reference frame designated by `dref' is not recognized
       by the SPICE frame subsystem, the error SPICE(NOFRAME) is
       signaled by a routine in the call tree of this routine.

   12) If insufficient ephemeris data have been loaded prior to
       calling tangpt_c, an error is signaled by a routine in the call
       tree of this routine. Note that when light time correction is
       used, sufficient ephemeris data must be available to
       propagate the states of both observer and target to the solar
       system barycenter. If light time correction is used and
       the ray's frame `dref' is non-inertial, sufficient ephemeris
       data must be available to compute the state of that frame's
       center relative to the solar system barycenter.

   13) If the target and observer have distinct identities but are at
       the same location (for example, the target is Mars and the
       observer is the Mars barycenter), the error
       SPICE(NOSEPARATION) is signaled by a routine in the call tree
       of this routine.

   14) The target must be an extended body: if any of the radii of
       the target body are non-positive, an error is signaled by a
       routine in the call tree of this routine.

   15) If the observer does not coincide with the target, but the
       observer is located inside the ellipsoid modeling the
       target body's shape, the error SPICE(INVALIDGEOMETRY) is
       signaled by a routine in the call tree of this routine.

   16) If the transformation between the ray frame `dref' and the
       J2000 frame cannot be computed, an error is signaled by a
       routine in the call tree of this routine.

   17) If the transformation between the J2000 frame and the
       target body-fixed, body-centered frame `fixref' cannot be
       computed, an error is signaled by a routine in the call tree
       of this routine.

   18) If the nearest point to the target on the line containing
       the input ray cannot be computed, an error is signaled by a
       routine in the call tree of this routine. This type of error
       may result from degenerate geometry; for example, if after
       scaling the reference ellipsoid axes to make the longest
       semi-axis a unit vector, another scaled axis is so short that
       its squared length underflows to zero, no result can be
       computed.

   19) It is not an error for the ray to intersect the target body
       or to point away from it so that the nearest point
       to the ellipsoid on the line containing the ray lies behind
       the ray's vertex.

   20) If any of the `method', `target', `fixref', `abcorr',
       `corloc', `obsrvr' or `dref' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   21) If any of the `method', `target', `fixref', `abcorr',
       `corloc', `obsrvr' or `dref' input strings has zero length,
       the error SPICE(EMPTYSTRING) is signaled.

-Files

   Appropriate kernels must be loaded by the calling program before
   this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for target and observer must be
      loaded. If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data.
      Typically ephemeris data are made available by loading one or
      more SPK files via furnsh_c.

   -  PCK data: triaxial radii for the target body must be loaded
      into the kernel pool. Typically this is done by loading a text
      PCK file via furnsh_c.

   -  Target orientation data: rotation data for the target body
      must be loaded. These may be provided in a text or binary PCK
      file, or by a CK file.

   The following data may be required:

   -  SPK data: if aberration corrections are used, and if the ray
      frame `dref' is non-inertial, ephemeris data for that frame's
      center must be loaded. The state of that object relative to
      the solar system barycenter must be calculable from the
      available ephemeris data.

   -  Frame specifications: if a frame definition is required to
      convert the observer and target states to the body-fixed frame
      of the target, that definition must be available in the kernel
      pool. Similarly, the frame definition required to map between
      the frame designated by `dref' and the target body-fixed frame
      must be available. Typically the definitions of frames not
      already built-in to SPICE are supplied by loading a frame
      kernel.

   -  Ray frame orientation data: if the frame to which `dref' refers
      is non-inertial, PCK or CK data for the frame's orientation
      are required. If the frame is fixed to a spacecraft instrument
      or structure, at least one CK file will be needed to permit
      transformation of vectors between that frame and both the
      J2000 and the target body-fixed frames.

   -  Ray direction data: if the ray direction is defined by a
      vector expressed in a spacecraft reference frame, an IK may be
      required to provide the coordinates of the ray's direction in
      that frame.

   -  SCLK data: if a CK file is needed, an associated SCLK kernel
      is required to enable conversion between encoded SCLK (used to
      time-tag CK data) and barycentric dynamical time (TDB).

   -  Leapseconds data: if SCLK data are needed, a leapseconds
      kernel usually is needed as well.

   -  Body name-ID mappings: if the target or observer name is
      not built into the SPICE software, the mapping between the
      name and the corresponding ID code must be present in the
      kernel pool. Such mappings are usually introduced by loading
      a frame kernel or other text kernel containing them.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   Given an observer, the direction vector of a ray emanating from
   the observer, and an extended target body represented by a
   triaxial ellipsoid, tangpt_c computes the "tangent point": a point
   nearest to the target body's surface nearest to the ray. The
   corresponding surface point nearest to the tangent point is
   computed as well.

   For remote sensing observations, for maximum accuracy, reception
   light time and stellar aberration corrections should be used.
   These corrections model observer-target-ray geometry as it is
   observed.

   For signal transmission applications, for maximum accuracy,
   transmission light time and stellar aberration corrections should
   be used. These corrections model the observer-target-ray geometry
   that applies to the transmitted signal. For example, these
   corrections are needed to calculate the minimum altitude of the
   signal's path over the target body.

   In some cases, the magnitudes of light time and stellar
   aberration corrections are negligible. When these corrections
   can be ignored, significantly faster execution can be achieved
   by setting the input `abcorr' to "NONE".

   This routine ignores differential aberration effects over the
   target body's surface: it computes corrections only at a
   user-specified point, which is called the "aberration correction
   locus." The user may select either the tangent point or
   corresponding surface point as the locus. In many cases, the
   differences between corrections for these points are very small.

   The -Examples header section below presents geometric cases for
   which aberration correction magnitudes are significant, and cases
   for which they are not.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following program computes tangent and surface points for
      the MAVEN IUVS instrument. The observer is the MAVEN
      spacecraft; the target body is Mars. The ray direction is
      that of the boresight of the MAVEN IUVS instrument.

      The aberration corrections used in this example are often
      suitable for remote sensing observations: converged Newtonian
      light time and stellar aberration "reception" corrections. In
      some cases it is reasonable to omit aberration corrections;
      see the second and third example programs below for
      demonstrations of the effects of different aberration
      correction choices.

      In this example, the aberration correction locus is the
      tangent point, meaning that converged light time and stellar
      aberration corrections are computed for that point. The epoch
      `trgepc' is used to compute the light time-corrected target
      position and orientation, and the stellar aberration
      correction applicable to the tangent point is applied to the
      observer-target position vector, in order to model apparent
      observation geometry.

      Three geometric cases are covered by this example:

         - The "normal" case, in which the ray defined by the
           MAVEN IUVS boresight passes over Mars at low altitude.

           In the example code, there are two computations that fall
           into this category.

         - The "intercept" case, in which the ray intersects Mars.

         - The "look away" case, in which the elevation of the ray's
           direction vector, measured from the local level plane at
           the sub-spacecraft point, is greater than or equal to 0.
           The aberration corrections used to compute the
           sub-observer point for this case are those applicable to
           the aberration correction locus.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: tangpt_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         All kernels referenced by this meta-kernel are available
         from the MAVEN SPICE PDS archive.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

         File name                          Contents
         ---------                          --------
         mar097s.bsp                        Mars satellite ephemeris
         maven_iuvs_v11.ti                  MAVEN IUVS instrument
                                            information
         maven_orb_rec_201001_210101_v1.bsp MAVEN s/c ephemeris
         mvn_v09.tf                         MAVEN frame
                                            specifications
         mvn_app_rel_201005_201011_v01.bc   MAVEN Articulated
                                            Payload Platform
                                            attitude
         mvn_iuvs_rem_201001_201231_v01.bc  MAVEN IUVS instrument
                                            internal mirror
                                            attitude
         mvn_sc_rel_201005_201011_v01.bc    MAVEN s/c attitude
         mvn_sclkscet_00086.tsc             MAVEN SCLK coefficients
         naif0012.tls                       Leapseconds
         pck00010.tpc                       Planet and satellite
                                            orientation and radii

         \begindata

            KERNELS_TO_LOAD = (
               'mar097s.bsp',
               'maven_iuvs_v11.ti',
               'maven_orb_rec_201001_210101_v1.bsp',
               'maven_v09.tf',
               'mvn_app_rel_201005_201011_v01.bc',
               'mvn_iuvs_rem_201001_201231_v01.bc',
               'mvn_sc_rel_201005_201011_v01.bc',
               'mvn_sclkscet_00086.tsc',
               'naif0012.tls',
               'pck00010.tpc' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program tangpt_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Parameters
         ./
         #define META         "tangpt_ex1.tm"
         #define TIMFMT       "YYYY-MM-DD HR:MN:SC.###### UTC ::RND"
         #define FRNMLN       33
         #define LNSIZE       73
         #define NCASE        3
         #define NTIMES       4
         #define ROOM         12
         #define SHPLEN       26
         #define TIMLEN       36

         /.
         Local variables
         ./
         SpiceChar          * abcorr;
         SpiceChar            casenm [LNSIZE];
         SpiceChar            fixref [FRNMLN];
         SpiceChar          * locus;
         SpiceChar          * obsrvr;
         SpiceChar            rayfrm [FRNMLN];
         SpiceChar            shape  [SHPLEN];
         SpiceChar          * target;
         SpiceChar            timstr [TIMLEN];
         SpiceDouble          alt;
         SpiceDouble          bounds [ROOM][3];
         SpiceDouble          et;
         SpiceDouble          range;
         SpiceDouble          raydir [3];
         SpiceDouble          srfpt  [3];
         SpiceDouble          srfvec [3];
         SpiceDouble          tanpt  [3];
         SpiceDouble          trgepc;

         SpiceInt             i;
         SpiceInt             nvec;

         /.
         Initial values
         ./
         SpiceChar            cases  [NCASE][LNSIZE] = {
                                                  "Ray slightly above limb",
                                                  "Intercept",
                                                  "Look-away" };

         SpiceChar          * insnam = "MAVEN_IUVS";

         SpiceChar            utctim [NTIMES][TIMLEN] = {
                                           "2020-10-11 16:01:43.000000 UTC",
                                           "2020-10-11 16:17:43.000000 UTC",
                                           "2020-10-11 16:49:07.000000 UTC",
                                           "2020-10-11 17:12:08.000000 UTC" };

         /.
         Load kernels.
         ./
         furnsh_c ( META );

         printf( "\n" );
         printf( "Instrument: %s\n", insnam );
         printf( "\n" );

         /.
         Get the instrument reference frame name and
         the instrument boresight direction in the
         instrument frame.
         ./
         getfvn_c ( insnam, ROOM,  SHPLEN,
                    FRNMLN, shape, rayfrm,
                    raydir, &nvec, bounds );

         /.
         Initialize inputs to tangpt_c, except for time.
         ./
         target = "MARS";
         obsrvr = "MAVEN";
         strncpy( fixref, "IAU_MARS", 9 );
         abcorr = "CN+S";
         locus  = "TANGENT POINT";

         /.
         Compute the apparent tangent point for each time.
         ./
         printf( "Aberration correction:       %s\n", abcorr );
         printf( "Aberration correction locus: %s\n", locus );

         for ( i = 0; i < NTIMES; i++ )
         {
            str2et_c ( utctim[i], &et );

            tangpt_c ( "ELLIPSOID", target, et,     fixref,  abcorr,
                       locus,       obsrvr, rayfrm, raydir,  tanpt,
                       &alt,        &range, srfpt,  &trgepc, srfvec );

            /.
            Set the label for the geometric case.
            ./
            if ( alt == 0 )
            {
               strncpy( casenm, cases[1], LNSIZE );
            }
            else if ( range == 0.0 )
            {
               strncpy( casenm, cases[2], LNSIZE );
            }
            else
            {
               strncpy( casenm, cases[0], LNSIZE );
            }

            /.
            Convert the target epoch to a string for output.
            ./
            timout_c ( trgepc, TIMFMT, TIMLEN, timstr );

            printf( "\n" );

            printf( "  Observation Time = %s\n", utctim[i] );
            printf( "  Target Time      = %s\n", timstr );

            printf( "    ALT    (km) =  %14.7f\n", alt );
            printf( "    RANGE  (km) =  %14.7f\n", range );
            printf( "    TANPT  (km) =  %14.7f %14.7f %14.7f\n",
                                    tanpt[0], tanpt[1], tanpt[2] );
            printf( "    SRFPT  (km) =  %14.7f %14.7f %14.7f\n",
                                    srfpt[0], srfpt[1], srfpt[2] );
            printf( "    SRFVEC (km) =  %14.7f %14.7f %14.7f\n",
                                 srfvec[0], srfvec[1], srfvec[2] );

            printf( "    Geometric case = %s\n", casenm );
         }

         printf( "\n" );

         return ( 0 );
      }


      When this program was executed on a PC/Linux/gcc/64-bit
      platform, the output was:


      Instrument: MAVEN_IUVS

      Aberration correction:       CN+S
      Aberration correction locus: TANGENT POINT

        Observation Time = 2020-10-11 16:01:43.000000 UTC
        Target Time      = 2020-10-11 16:01:42.983021 UTC
          ALT    (km) =      99.4262977
          RANGE  (km) =    5090.1928435
          TANPT  (km) =   -2273.0408575   1072.4423944  -2415.6104827
          SRFPT  (km) =   -2208.5678350   1042.0234063  -2346.3031728
          SRFVEC (km) =   -2138.0677257   3050.4078643   3470.3929222
          Geometric case = Ray slightly above limb

        Observation Time = 2020-10-11 16:17:43.000000 UTC
        Target Time      = 2020-10-11 16:17:42.993820 UTC
          ALT    (km) =       0.0000000
          RANGE  (km) =    1852.8381880
          TANPT  (km) =     752.0909507  -1781.3912506  -2775.5390159
          SRFPT  (km) =     752.0909507  -1781.3912506  -2775.5390159
          SRFVEC (km) =    -700.9743439   1162.4766255   1261.0679662
          Geometric case = Intercept

        Observation Time = 2020-10-11 16:49:07.000000 UTC
        Target Time      = 2020-10-11 16:49:06.998907 UTC
          ALT    (km) =     218.2661426
          RANGE  (km) =     327.7912133
          TANPT  (km) =    2479.8672359  -1772.2350525   1931.8678816
          SRFPT  (km) =    2330.3561559  -1665.3870838   1814.0966731
          SRFVEC (km) =      77.3692694    325.9571470   -207.0099587
          Geometric case = Ray slightly above limb

        Observation Time = 2020-10-11 17:12:08.000000 UTC
        Target Time      = 2020-10-11 17:12:08.000000 UTC
          ALT    (km) =     969.2772042
          RANGE  (km) =       0.0000000
          TANPT  (km) =     -58.1087763   2034.6474343   3844.2010767
          SRFPT  (km) =     -45.2530638   1584.5115999   2985.8825113
          SRFVEC (km) =      12.8557125   -450.1358344   -858.3185654
          Geometric case = Look-away


   2) The following program computes tangent and surface points for
      the MRO MCS A1 instrument, for a single epoch. The observer is
      the MRO spacecraft; the target body is Mars. The ray direction
      is that of the boresight of the MRO MCS A1 instrument.

      The aberration corrections used in this example are converged
      Newtonian light time and stellar aberration corrections,
      converged Newtonian light time alone, and "NONE."

      For remote sensing observations made by a spacecraft in low
      orbit about Mars, both the combination of light time and
      stellar aberration corrections and omission of aberration
      corrections may be valid. See the output of this program and
      of the third example program below for examples of how results
      differ due to the choice of aberration corrections.

      Use of light time corrections alone is presented to
      illustrate, by way of contrast, the effect of this choice.
      This choice can be useful for testing but is unlikely to be
      correct for modeling actual observation geometry.

      Separate computations are performed using both the tangent
      point and the corresponding surface point---the nearest point
      on the target surface to the tangent point---as the aberration
      correction locus.

      Three geometric cases are covered by this example:

         - The "normal" case, in which the ray defined by the
           MRO MCS A1 boresight passes over Mars at low altitude.

           In the example code, there are two computations that fall
           into this category.

         - The "intercept" case, in which the ray intersects Mars.

         - The "look away" case, in which the elevation of the ray's
           direction vector, measured from the local level plane at
           the sub-spacecraft point, is greater than or equal to 0.
           The target position and orientation used for this
           computation are the same as those used to compute the
           aberration correction locus.


      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: tangpt_ex2.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         All kernels referenced by this meta-kernel are available
         from the MRO SPICE PDS archive.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                       Contents
            ---------                       --------
            mar097.bsp                      Mars satellite ephemeris
            mro_mcs_psp_201001_201031.bc    MRO MCS attitude
            mro_mcs_v10.ti                  MRO MCS instrument
                                            information
            mro_psp57_ssd_mro95a.bsp        MRO s/c ephemeris
            mro_sc_psp_201027_201102.bc     MRO s/c bus attitude
            mro_sclkscet_00095_65536.tsc    MRO SCLK coefficients
            mro_v16.tf                      MRO frame specifications
            naif0012.tls                    Leapseconds
            pck00008.tpc                    Planet and satellite
                                            orientation and radii

         \begindata

            KERNELS_TO_LOAD = (
               'mar097.bsp',
               'mro_mcs_psp_201001_201031.bc',
               'mro_mcs_v10.ti',
               'mro_psp57_ssd_mro95a.bsp',
               'mro_sc_psp_201027_201102.bc',
               'mro_sclkscet_00095_65536.tsc',
               'mro_v16.tf',
               'naif0012.tls',
               'pck00008.tpc' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program tangpt_ex2
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Parameters
         ./
         #define META         "tangpt_ex2.tm"
         #define TIMFMT       "YYYY-MM-DD HR:MN:SC.###### UTC ::RND"
         #define CORLEN       11
         #define FRNMLN       33
         #define LOCLEN       26
         #define NCASE        5
         #define ROOM         4
         #define SHPLEN       26
         #define TIMLEN       36

         /.
         Local variables
         ./
         SpiceChar            abcorr [CORLEN];
         SpiceChar            fixref [FRNMLN];
         SpiceChar            locus  [LOCLEN];
         SpiceChar          * obsrvr;
         SpiceChar            rayfrm [FRNMLN];
         SpiceChar            shape  [SHPLEN];
         SpiceChar          * target;
         SpiceChar            timstr [TIMLEN];
         SpiceDouble          alt;
         SpiceDouble          bounds [ROOM][3];
         SpiceDouble          diff   [3];
         SpiceDouble          et;
         SpiceDouble          range;
         SpiceDouble          raydir [3];
         SpiceDouble          srfpt  [3];
         SpiceDouble          srfvec [3];
         SpiceDouble          svalt;
         SpiceDouble          svepoc;
         SpiceDouble          svrang;
         SpiceDouble          svsrfp [3];
         SpiceDouble          svsrfv [3];
         SpiceDouble          svtanp [3];
         SpiceDouble          tanpt  [3];
         SpiceDouble          trgepc;

         SpiceInt             i;
         SpiceInt             nvec;

         /.
         Initial values
         ./
         SpiceChar            corrs  [NCASE][CORLEN] = { "CN+S", "CN+S",
                                                         "CN",   "CN",
                                                         "NONE" };

         SpiceChar          * insnam = "MRO_MCS_A1";

         SpiceChar            loci   [NCASE][LOCLEN] = { "TANGENT POINT",
                                                         "SURFACE POINT",
                                                         "TANGENT POINT",
                                                         "SURFACE POINT",
                                                         "TANGENT POINT" };

         SpiceChar          * utctim = "2020-10-31 00:01:23.111492 UTC";

         /.
         Load kernels.
         ./
         furnsh_c ( META );

         printf( "\n" );
         printf( "Instrument: %s\n", insnam );

         /.
         Get the instrument reference frame name and
         the instrument boresight direction in the
         instrument frame.
         ./
         getfvn_c ( insnam, ROOM,  SHPLEN,
                    FRNMLN, shape, rayfrm,
                    raydir, &nvec, bounds );

         /.
         Initialize inputs to tangpt_c that are common to all
         cases.
         ./
         target = "MARS";
         obsrvr = "MRO";
         strncpy( fixref, "IAU_MARS", 9 );

         /.
         Compute the apparent tangent point for each case.
         ./
         for ( i = 0; i < NCASE; i++ )
         {
            printf( "\n" );

            strncpy( abcorr, corrs[i], CORLEN );
            strncpy( locus, loci[i], LOCLEN );

            printf( "Aberration correction:       %s\n",
                                                  abcorr );

            printf( "Aberration correction locus: %s\n",
                                                   locus );

            printf( "\n" );

            str2et_c ( utctim, &et );

            tangpt_c ( "ELLIPSOID", target, et,     fixref,  abcorr,
                       locus,       obsrvr, rayfrm, raydir,  tanpt,
                       &alt,        &range, srfpt,  &trgepc, srfvec );

            /.
            Convert the target epoch to a string for output.
            ./
            timout_c ( trgepc, TIMFMT, TIMLEN, timstr );

            printf( "  Observation Time = %s\n",               utctim );

            printf( "  Target Time      = %s\n",                 timstr );

            printf( "    ALT    (km) =  %14.7f\n", alt );
            printf( "    RANGE  (km) =  %14.7f\n", range );
            printf( "    TANPT  (km) =  %14.7f %14.7f %14.7f\n",
                                    tanpt[0], tanpt[1], tanpt[2] );
            printf( "    SRFPT  (km) =  %14.7f %14.7f %14.7f\n",
                                    srfpt[0], srfpt[1], srfpt[2] );
            printf( "    SRFVEC (km) =  %14.7f %14.7f %14.7f\n",
                                 srfvec[0], srfvec[1], srfvec[2] );

            if ( i == 0 )
            {

               /.
               Save results for comparison.
               ./
               svalt  = alt;
               svepoc = trgepc;
               svrang = range;
               vequ_c ( tanpt, svtanp );
               vequ_c ( srfpt, svsrfp );
               vequ_c ( srfvec, svsrfv );
            }
            else
            {

               /.
               Compare results to CN+S, tangent point
               locus case.
               ./
               printf( "\n" );

               printf( "  Differences from case 1 outputs:\n" );

               printf( "    Target time delta (ms) =  %9.4f\n",
                                     1.e3 * ( trgepc - svepoc ) );

               printf( "    ALT    delta (m) =  %9.4f\n",
                                   1.e3 * ( alt - svalt ) );

               printf( "    RANGE  delta (m) =  %9.4f\n",
                               1.e3 * ( range - svrang  ) );

               vsub_c ( tanpt, svtanp, diff );
               vscl_c ( 1.e3, diff, diff );
               printf( "    TANPT  delta (m) =  %9.4f %9.4f %9.4f\n",
                                            diff[0], diff[1], diff[2] );

               vsub_c ( srfpt, svsrfp, diff );
               vscl_c ( 1.e3, diff, diff );
               printf( "    SRFPT  delta (m) =  %9.4f %9.4f %9.4f\n",
                                            diff[0], diff[1], diff[2] );

               vsub_c ( srfvec, svsrfv, diff );
               vscl_c ( 1.e3, diff, diff );
               printf( "    SRFVEC delta (m) =  %9.4f %9.4f %9.4f\n",
                                            diff[0], diff[1], diff[2] );
            }

            printf( "\n" );
         }

         return ( 0 );
      }


      When this program was executed on a PC/Linux/gcc/64-bit
      platform, the output was:


      Instrument: MRO_MCS_A1

      Aberration correction:       CN+S
      Aberration correction locus: TANGENT POINT

        Observation Time = 2020-10-31 00:01:23.111492 UTC
        Target Time      = 2020-10-31 00:01:23.106946 UTC
          ALT    (km) =      39.1034486
          RANGE  (km) =    1362.8659249
          TANPT  (km) =   -2530.9040220  -1630.9806346   1644.3612074
          SRFPT  (km) =   -2502.1342299  -1612.4406294   1625.4496512
          SRFVEC (km) =    -589.3842679   -234.0892764  -1206.9635473


      Aberration correction:       CN+S
      Aberration correction locus: SURFACE POINT

        Observation Time = 2020-10-31 00:01:23.111492 UTC
        Target Time      = 2020-10-31 00:01:23.106944 UTC
          ALT    (km) =      39.1014434
          RANGE  (km) =    1362.8679108
          TANPT  (km) =   -2530.9025464  -1630.9796845   1644.3602376
          SRFPT  (km) =   -2502.1342295  -1612.4406300   1625.4496511
          SRFVEC (km) =    -589.3866439   -234.0905954  -1206.9643086

        Differences from case 1 outputs:
          Target time delta (ms) =    -0.0019
          ALT    delta (m) =    -2.0052
          RANGE  delta (m) =     1.9859
          TANPT  delta (m) =     1.4757    0.9501   -0.9698
          SRFPT  delta (m) =     0.0004   -0.0006   -0.0000
          SRFVEC delta (m) =    -2.3760   -1.3189   -0.7614


      Aberration correction:       CN
      Aberration correction locus: TANGENT POINT

        Observation Time = 2020-10-31 00:01:23.111492 UTC
        Target Time      = 2020-10-31 00:01:23.106946 UTC
          ALT    (km) =      39.1714711
          RANGE  (km) =    1362.8658567
          TANPT  (km) =   -2530.9135880  -1631.0820975   1644.3878335
          SRFPT  (km) =   -2502.0942100  -1612.5090527   1625.4434517
          SRFVEC (km) =    -589.3346511   -234.0562242  -1206.9963133

        Differences from case 1 outputs:
          Target time delta (ms) =     0.0000
          ALT    delta (m) =    68.0225
          RANGE  delta (m) =    -0.0683
          TANPT  delta (m) =    -9.5660 -101.4629   26.6261
          SRFPT  delta (m) =    40.0199  -68.4233   -6.1994
          SRFVEC delta (m) =    49.6168   33.0522  -32.7661


      Aberration correction:       CN
      Aberration correction locus: SURFACE POINT

        Observation Time = 2020-10-31 00:01:23.111492 UTC
        Target Time      = 2020-10-31 00:01:23.106944 UTC
          ALT    (km) =      39.1714973
          RANGE  (km) =    1362.8658326
          TANPT  (km) =   -2530.9135903  -1631.0821391   1644.3878436
          SRFPT  (km) =   -2502.0941931  -1612.5090815   1625.4434492
          SRFVEC (km) =    -589.3346210   -234.0562071  -1206.9963050

        Differences from case 1 outputs:
          Target time delta (ms) =    -0.0019
          ALT    delta (m) =    68.0487
          RANGE  delta (m) =    -0.0924
          TANPT  delta (m) =    -9.5682 -101.5045   26.6362
          SRFPT  delta (m) =    40.0368  -68.4521   -6.2020
          SRFVEC delta (m) =    49.6469   33.0694  -32.7577


      Aberration correction:       NONE
      Aberration correction locus: TANGENT POINT

        Observation Time = 2020-10-31 00:01:23.111492 UTC
        Target Time      = 2020-10-31 00:01:23.111492 UTC
          ALT    (km) =      39.1090103
          RANGE  (km) =    1362.9233525
          TANPT  (km) =   -2530.9082604  -1630.9831041   1644.3638384
          SRFPT  (km) =   -2502.1343747  -1612.4404639   1625.4495931
          SRFVEC (km) =    -589.4063032   -234.0970874  -1207.0162978

        Differences from case 1 outputs:
          Target time delta (ms) =     4.5460
          ALT    delta (m) =     5.5616
          RANGE  delta (m) =    57.4276
          TANPT  delta (m) =    -4.2384   -2.4695    2.6310
          SRFPT  delta (m) =    -0.1448    0.1655   -0.0581
          SRFVEC delta (m) =   -22.0352   -7.8109  -52.7505


   3) The following program computes tangent and surface points for
      a ray pointing from the Goldstone DSN station DSS-14 to the
      location of the MRO spacecraft, for a single epoch. The target
      body is Mars.

      The aberration corrections used in this example are

         CN+S
         XCN+S
         CN
         NONE

      Results using CN+S corrections are computed for both locus
      choices: TANGENT POINT and SURFACE POINT.

      For each case other than the one using CN+S corrections for
      the TANGENT POINT locus, differences between results for the
      former and latter case are shown.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: tangpt_ex3.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         All kernels referenced by this meta-kernel are available
         from the NAIF SPICE server in the generic kernels area
         or from the MRO SPICE PDS archive.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                       Contents
            ---------                       --------
            mar097.bsp                      Mars satellite ephemeris
            mro_psp57_ssd_mro95a.bsp        MRO s/c ephemeris
            earthstns_itrf93_201023.bsp     DSN station locations
            naif0012.tls                    Leapseconds
            pck00010.tpc                    Planet and satellite
                                            orientation and radii
            earth_latest_high_prec.bpc      High accuracy Earth
                                            attitude

         \begindata

            KERNELS_TO_LOAD = (
               'mar097.bsp'
               'mro_psp57_ssd_mro95a.bsp'
               'earthstns_itrf93_201023.bsp'
               'naif0012.tls'
               'pck00010.tpc'
               'earth_latest_high_prec.bpc' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program tangpt_ex3
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Parameters
         ./
         #define META         "tangpt_ex3.tm"
         #define TIMFMT       "YYYY-MM-DD HR:MN:SC.###### UTC ::RND"
         #define CORLEN       11
         #define LOCLEN       26
         #define NCASE        5
         #define TIMLEN       36

         /.
         Local variables
         ./
         SpiceChar            abcorr [CORLEN];
         SpiceChar          * fixref;
         SpiceChar            locus  [LOCLEN];
         SpiceChar          * obsrvr;
         SpiceChar          * rayfrm;
         SpiceChar          * sc;
         SpiceChar          * target;
         SpiceChar            timstr [TIMLEN];
         SpiceDouble          alt;
         SpiceDouble          diff   [3];
         SpiceDouble          et;
         SpiceDouble          range;
         SpiceDouble          raydir [3];
         SpiceDouble          raylt;
         SpiceDouble          srfpt  [3];
         SpiceDouble          srfvec [3];
         SpiceDouble          svalt;
         SpiceDouble          svepoc;
         SpiceDouble          svrang;
         SpiceDouble          svsrfp [3];
         SpiceDouble          svsrfv [3];
         SpiceDouble          svtanp [3];
         SpiceDouble          tanpt  [3];
         SpiceDouble          trgepc;

         SpiceInt             i;

         /.
         Initial values
         ./
         SpiceChar            corrs  [NCASE][CORLEN] = { "CN+S", "XCN+S",
                                                         "CN",   "NONE",
                                                         "CN+S" };

         SpiceChar            loci   [NCASE][LOCLEN] = { "TANGENT POINT",
                                                         "TANGENT POINT",
                                                         "TANGENT POINT",
                                                         "TANGENT POINT",
                                                         "SURFACE POINT" };

         SpiceChar          * utctim = "2020-12-30 00:00:00 UTC";

         /.
         Load kernels.
         ./
         furnsh_c ( META );

         /.
         Set name of spacecraft used to define ray direction.
         ./
         sc = "MRO";

         /.
         Initialize inputs to tangpt_c that are common to all
         cases.
         ./
         target = "MARS";
         obsrvr = "DSS-14";
         fixref = "IAU_MARS";
         rayfrm = "J2000";

         /.
         Convert observation time to TDB seconds past J2000.
         ./
         str2et_c ( utctim, &et );

         /.
         Generate ray direction vector. Use apparent position
         of the MRO spacecraft.
         ./
         spkpos_c ( sc, et, rayfrm, "CN+S", obsrvr, raydir, &raylt );

         printf( "\n" );
         printf( "Observer:   %s\n", obsrvr );
         printf( "Target:     %s\n", target );
         printf( "Spacecraft: %s\n", sc );

         /.
         Compute the apparent tangent point for each case.
         ./
         for ( i = 0; i < NCASE; i++ )
         {
            printf( "\n" );

            strncpy( abcorr, corrs[i], CORLEN );
            strncpy( locus, loci[i], LOCLEN );

            printf( "Aberration correction:       %s\n",
                                                  abcorr );

            printf( "Aberration correction locus: %s\n",
                                                   locus );

            printf( "\n" );

            /.
            Compute tangent point.
            ./
            tangpt_c ( "ELLIPSOID", target, et,     fixref,  abcorr,
                       locus,       obsrvr, rayfrm, raydir,  tanpt,
                       &alt,        &range, srfpt,  &trgepc, srfvec );

            /.
            Convert the target epoch to a string for output.
            ./
            timout_c ( trgepc, TIMFMT, TIMLEN, timstr );

            printf( "  Observation Time = %s\n",               utctim );

            printf( "  Target Time      = %s\n",                 timstr );

            printf( "    ALT    (km) =  %13.3f\n", alt );
            printf( "    RANGE  (km) =  %13.3f\n", range );
            printf( "    TANPT  (km) =  %13.3f %13.3f %13.3f\n",
                                    tanpt[0], tanpt[1], tanpt[2] );
            printf( "    SRFPT  (km) =  %13.3f %13.3f %13.3f\n",
                                    srfpt[0], srfpt[1], srfpt[2] );
            printf( "    SRFVEC (km) =  %13.3f %13.3f %13.3f\n",
                                 srfvec[0], srfvec[1], srfvec[2] );

            if ( i == 0 )
            {

               /.
               Save results for comparison.
               ./
               svalt  = alt;
               svepoc = trgepc;
               svrang = range;
               vequ_c ( tanpt, svtanp );
               vequ_c ( srfpt, svsrfp );
               vequ_c ( srfvec, svsrfv );
            }
            else
            {

               /.
               Compare results to CN+S, tangent point
               locus case.
               ./
               printf( "\n" );

               printf( "  Differences from case 1 outputs:\n" );

               printf( "    Target time delta (s) =  %13.6f\n",
                                                trgepc - svepoc );

               printf( "    ALT    delta (km) =  %12.3f\n",
                                                             alt - svalt );

               printf( "    RANGE  delta (km) =  %12.3f\n",
                                                          range - svrang );

               vsub_c ( tanpt, svtanp, diff );
               printf( "    TANPT  delta (km) =  %12.3f %12.3f %9.3f\n",
                                               diff[0], diff[1], diff[2] );

               vsub_c ( srfpt, svsrfp, diff );
               printf( "    SRFPT  delta (km) =  %12.3f %12.3f %9.3f\n",
                                               diff[0], diff[1], diff[2] );

               vsub_c ( srfvec, svsrfv, diff );
               printf( "    SRFVEC delta (km) =  %12.3f %12.3f %9.3f\n",
                                               diff[0], diff[1], diff[2] );
            }

            printf( "\n" );
         }

         return ( 0 );
      }


      When this program was executed on a PC/Linux/gcc/64-bit
      platform, the output was:


      Observer:   DSS-14
      Target:     MARS
      Spacecraft: MRO

      Aberration correction:       CN+S
      Aberration correction locus: TANGENT POINT

        Observation Time = 2020-12-30 00:00:00 UTC
        Target Time      = 2020-12-29 23:52:40.613204 UTC
          ALT    (km) =        140.295
          RANGE  (km) =  131724847.608
          TANPT  (km) =       1351.574      1182.155     -3029.495
          SRFPT  (km) =       1298.181      1135.455     -2908.454
          SRFVEC (km) =  121233989.354  -5994858.328  51164606.676


      Aberration correction:       XCN+S
      Aberration correction locus: TANGENT POINT

        Observation Time = 2020-12-30 00:00:00 UTC
        Target Time      = 2020-12-30 00:07:19.347692 UTC
          ALT    (km) =       4921.539
          RANGE  (km) =  131713124.520
          TANPT  (km) =       -413.404     -8220.856     -1193.471
          SRFPT  (km) =       -168.808     -3356.879      -483.938
          SRFVEC (km) =  120615301.766 -13523495.083  51160641.665

        Differences from case 1 outputs:
          Target time delta (s) =     878.734488
          ALT    delta (km) =      4781.244
          RANGE  delta (km) =    -11723.089
          TANPT  delta (km) =     -1764.978    -9403.011  1836.024
          SRFPT  delta (km) =     -1466.989    -4492.334  2424.517
          SRFVEC delta (km) =   -618687.588 -7528636.755 -3965.012


      Aberration correction:       CN
      Aberration correction locus: TANGENT POINT

        Observation Time = 2020-12-30 00:00:00 UTC
        Target Time      = 2020-12-29 23:52:40.613219 UTC
          ALT    (km) =       3409.162
          RANGE  (km) =  131724843.177
          TANPT  (km) =       1933.641      5183.696     -3951.091
          SRFPT  (km) =        965.945      2589.501     -1962.095
          SRFVEC (km) =  121233070.966  -5997405.748  51166472.910

        Differences from case 1 outputs:
          Target time delta (s) =       0.000015
          ALT    delta (km) =      3268.868
          RANGE  delta (km) =        -4.431
          TANPT  delta (km) =       582.067     4001.541  -921.596
          SRFPT  delta (km) =      -332.236     1454.046   946.360
          SRFVEC delta (km) =      -918.388    -2547.420  1866.234


      Aberration correction:       NONE
      Aberration correction locus: TANGENT POINT

        Observation Time = 2020-12-30 00:00:00 UTC
        Target Time      = 2020-12-30 00:00:00.000000 UTC
          ALT    (km) =        781.382
          RANGE  (km) =  131718986.013
          TANPT  (km) =        615.190     -3545.867     -2111.285
          SRFPT  (km) =        500.266     -2883.463     -1713.075
          SRFVEC (km) =  120983074.323  -9765994.151  51162607.074

        Differences from case 1 outputs:
          Target time delta (s) =     439.386796
          ALT    delta (km) =       641.087
          RANGE  delta (km) =     -5861.595
          TANPT  delta (km) =      -736.384    -4728.022   918.210
          SRFPT  delta (km) =      -797.915    -4018.919  1195.379
          SRFVEC delta (km) =   -250915.031 -3771135.823 -1999.603


      Aberration correction:       CN+S
      Aberration correction locus: SURFACE POINT

        Observation Time = 2020-12-30 00:00:00 UTC
        Target Time      = 2020-12-29 23:52:40.613204 UTC
          ALT    (km) =        140.308
          RANGE  (km) =  131724847.611
          TANPT  (km) =       1351.579      1182.159     -3029.507
          SRFPT  (km) =       1298.181      1135.455     -2908.454
          SRFVEC (km) =  121233989.351  -5994858.333  51164606.689

        Differences from case 1 outputs:
          Target time delta (s) =       0.000000
          ALT    delta (km) =         0.013
          RANGE  delta (km) =         0.003
          TANPT  delta (km) =         0.005        0.004    -0.012
          SRFPT  delta (km) =        -0.000        0.000     0.000
          SRFVEC delta (km) =        -0.003       -0.005     0.013


-Restrictions

   1)  This routine is applicable only to computations for which
       radiation paths can be modeled as straight lines.

   2)  This routine does not account for differential aberration
       corrections across the target body surface: when aberration
       corrections are used, the entire target ellipsoid's position
       and orientation are modified by the corrections that apply at
       the aberration correction locus.

   3)  A cautionary note: if aberration corrections are used, and if
       `dref' is the target body-fixed frame, the epoch at which that
       frame is evaluated is offset from `et' by the light time
       between the observer and the *center* of the target body.
       This light time normally will differ from the light time
       between the observer and the tangent or surface point.
       Consequently the orientation of the target body-fixed frame
       at `trgepc' will not match that of the target body-fixed frame
       at the epoch associated with `dref'. As a result, various
       derived quantities may not be as expected: for example,
       `srfvec' would not be parallel to `dvec'.

       In many applications the errors arising from this frame
       discrepancy may be insignificant; however a safe approach is
       to always use as `dref' a frame other than the target
       body-fixed frame.

-Literature_References

   None.

-Author_and_Institution

   M. Costa Sitja      (JPL)

-Version

   -CSPICE Version 1.0.0, 28-OCT-2021 (MCS)

-Index_Entries

   find ray-ellipsoid tangent point
   find nearest point to ray on ellipsoid

-&
*/

{ /* Begin tangpt_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "tangpt_c" );

   /*
   Check the input string arguments:

      method
      target
      fixref
      abcorr
      corloc
      obsrvr
      dref

   Make sure each pointer is non-null and each string contains
   at least one data character: that is, one character
   preceding the null terminator.
   */
   CHKFSTR ( CHK_STANDARD, "tangpt_c", method );
   CHKFSTR ( CHK_STANDARD, "tangpt_c", target );
   CHKFSTR ( CHK_STANDARD, "tangpt_c", fixref );
   CHKFSTR ( CHK_STANDARD, "tangpt_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "tangpt_c", corloc );
   CHKFSTR ( CHK_STANDARD, "tangpt_c", obsrvr );
   CHKFSTR ( CHK_STANDARD, "tangpt_c", dref   );

   /*
   Call the f2c'd Fortran routine.
   */
   tangpt_ (  ( char       * )  method,
              ( char       * )  target,
              ( doublereal * ) &et,
              ( char       * )  fixref,
              ( char       * )  abcorr,
              ( char       * )  corloc,
              ( char       * )  obsrvr,
              ( char       * )  dref,
              ( doublereal * )  dvec,
              ( doublereal * )  tanpt,
              ( doublereal * )  alt,
              ( doublereal * )  range,
              ( doublereal * )  srfpt,
              ( doublereal * )  trgepc,
              ( doublereal * )  srfvec,
              ( ftnlen       )  strlen(method),
              ( ftnlen       )  strlen(target),
              ( ftnlen       )  strlen(fixref),
              ( ftnlen       )  strlen(abcorr),
              ( ftnlen       )  strlen(corloc),
              ( ftnlen       )  strlen(obsrvr),
              ( ftnlen       )  strlen(dref)   );

   chkout_c ( "tangpt_c" );

} /* End tangpt_c */
