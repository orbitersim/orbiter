/*

-Procedure limbpt_c ( Limb points on an extended object )

-Abstract

   Find limb points on a target body. The limb is the set of points
   of tangency on the target of rays emanating from the observer.
   The caller specifies half-planes bounded by the observer-target
   center vector in which to search for limb points.

   The surface of the target body may be represented either by a
   triaxial ellipsoid or by topographic data.

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
   SPK
   TIME

-Keywords

   GEOMETRY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef limbpt_c


   void limbpt_c ( ConstSpiceChar    * method,
                   ConstSpiceChar    * target,
                   SpiceDouble         et,
                   ConstSpiceChar    * fixref,
                   ConstSpiceChar    * abcorr,
                   ConstSpiceChar    * corloc,
                   ConstSpiceChar    * obsrvr,
                   ConstSpiceDouble    refvec[3],
                   SpiceDouble         rolstp,
                   SpiceInt            ncuts,
                   SpiceDouble         schstp,
                   SpiceDouble         soltol,
                   SpiceInt            maxn,
                   SpiceInt            npts  [],
                   SpiceDouble         points[][3],
                   SpiceDouble         epochs[],
                   SpiceDouble         tangts[][3]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   method     I   Computation method.
   target     I   Name of target body.
   et         I   Epoch in ephemeris seconds past J2000 TDB.
   fixref     I   Body-fixed, body-centered target body frame.
   abcorr     I   Aberration correction.
   corloc     I   Aberration correction locus.
   obsrvr     I   Name of observing body.
   refvec     I   Reference vector for cutting half-planes.
   rolstp     I   Roll angular step for cutting half-planes.
   ncuts      I   Number of cutting half-planes.
   schstp     I   Angular step size for searching.
   soltol     I   Solution convergence tolerance.
   maxn       I   Maximum number of entries in output arrays.
   npts       O   Counts of limb points corresponding to cuts.
   points     O   Limb points.
   epochs     O   Times associated with limb points.
   tangts     O   Tangent vectors emanating from the observer.

-Detailed_Input

   method      is a short string providing parameters defining
               the computation method to be used. In the syntax
               descriptions below, items delimited by brackets
               are optional.

               `method' may be assigned the following values:

                 "TANGENT/DSK/UNPRIORITIZED[/SURFACES = <surface list>]"

                     The limb point computation uses topographic data
                     provided by DSK files (abbreviated as "DSK data"
                     below) to model the surface of the target body. A
                     limb point is defined as the point of tangency, on
                     the surface represented by the DSK data, of a ray
                     emanating from the observer.

                     Limb points are generated within a specified set
                     of "cutting" half-planes that have as an edge the
                     line containing the observer-target vector.
                     Multiple limb points may be found within a given
                     half-plane, if the target body shape allows for
                     this.

                     The surface list specification is optional. The
                     syntax of the list is

                        <surface 1> [, <surface 2>...]

                     If present, it indicates that data only for the
                     listed surfaces are to be used; however, data need
                     not be available for all surfaces in the list. If
                     the list is absent, loaded DSK data for any
                     surface associated with the target body are used.

                     The surface list may contain surface names or
                     surface ID codes. Names containing blanks must
                     be delimited by double quotes, for example

                        SURFACES = \"Mars MEGDR 128 PIXEL/DEG\"

                     If multiple surfaces are specified, their names
                     or IDs must be separated by commas.

                     See the -Particulars section below for details
                     concerning use of DSK data.

                     This is the highest-accuracy method supported by
                     this routine. It generally executes much more
                     slowly than the "GUIDED" method described below.


                 "GUIDED/DSK/UNPRIORITIZED[/SURFACES = <surface list>]"

                     This method uses DSK data as described above, but
                     limb points generated by this method are "guided"
                     so as to lie in the limb plane of the target
                     body's reference ellipsoid, on the target body's
                     surface. This method produces a unique limb point
                     for each cutting half-plane. If multiple limb
                     point candidates lie in a given cutting
                     half-plane, the outermost one is chosen.

                     This method may be used only with the "CENTER"
                     aberration correction locus (see the description
                     of `corloc' below).

                     Limb points generated by this method are
                     approximations; they are generally not true
                     ray-surface tangent points. However, these
                     approximations can be generated much more quickly
                     than tangent points.


                 "TANGENT/ELLIPSOID"
                 "GUIDED/ELLIPSOID"

                     Both of these methods generate limb points on the
                     target body's reference ellipsoid. The "TANGENT"
                     option may be used with any aberration correction
                     locus, while the "GUIDED" option may be used only
                     with the "CENTER" locus (see the description of
                     `corloc' below).

                     When the locus is set to "CENTER", these methods
                     produce the same results.


                  Neither case nor white space are significant in
                  `method', except within double-quoted strings. For
                  example, the string " eLLipsoid/tAnGenT " is valid.

                  Within double-quoted strings, blank characters are
                  significant, but multiple consecutive blanks are
                  considered equivalent to a single blank. Case is
                  not significant. So

                     \"Mars MEGDR 128 PIXEL/DEG\"

                  is equivalent to

                     \" mars megdr  128  pixel/deg \"

                  but not to

                     \"MARS MEGDR128PIXEL/DEG\"


   target      is the name of the target body. The target body is
               an extended ephemeris object.

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
               expressed as TDB seconds past J2000 TDB: `et' is
               the epoch at which the observer's state is computed.

               When aberration corrections are not used, `et' is also
               the epoch at which the position and orientation of
               the target body are computed.

               When aberration corrections are used, the position
               and orientation of the target body are computed at
               et-lt, where `lt' is the one-way light time between the
               aberration correction locus and the observer. The
               locus is specified by the input argument `corloc'.
               See the descriptions of `abcorr' and `corloc' below for
               details.


   fixref      is the name of a body-fixed reference frame centered
               on the target body. `fixref' may be any such frame
               supported by the SPICE system, including built-in
               frames (documented in the Frames Required Reading)
               and frames defined by a loaded frame kernel (FK). The
               string `fixref' is case-insensitive, and leading and
               trailing blanks in `fixref' are not significant.

               The output limb points in the array `points' and the
               output observer-target tangent vectors in the array
               `tangts' are expressed relative to this reference frame.


   abcorr      indicates the aberration corrections to be applied
               when computing the target's position and orientation.
               Corrections are applied at the location specified by
               the aberration correction locus argument `corloc',
               which is described below.

               For remote sensing applications, where apparent limb
               points seen by the observer are desired, normally
               either of the corrections

                  "LT+S"
                  "CN+S"

               should be used. The correction "NONE" may be suitable
               for cases in which the target is very small and the
               observer is close to, and has small velocity relative
               to, the target (e.g. comet Churyumov-Gerasimenko and
               the Rosetta Orbiter).

               These and the other supported options are described
               below. `abcorr' may be any of the following:

                  "NONE"     Apply no correction. Return the
                             geometric limb points on the target
                             body.

               Let `lt' represent the one-way light time between the
               observer and the aberration correction locus. The
               following values of `abcorr' apply to the "reception"
               case in which photons depart from the locus at the
               light-time corrected epoch et-lt and *arrive* at the
               observer's location at `et':


                  "LT"       Correct for one-way light time (also
                             called "planetary aberration") using a
                             Newtonian formulation. This correction
                             yields the locus at the moment it
                             emitted photons arriving at the
                             observer at `et'.

                             The light time correction uses an
                             iterative solution of the light time
                             equation. The solution invoked by the
                             "LT" option uses two iterations.

                             Both the target position as seen by the
                             observer, and rotation of the target
                             body, are corrected for light time.

                  "LT+S"     Correct for one-way light time and
                             stellar aberration using a Newtonian
                             formulation. This option modifies the
                             locus obtained with the "LT" option to
                             account for the observer's velocity
                             relative to the solar system
                             barycenter. These corrections yield
                             points on the apparent limb.

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

               The following values of `abcorr' apply to the
               "transmission" case in which photons depart from the
               observer's location at `et' and arrive at the aberration
               correction locus at the light-time corrected epoch
               et+lt:

                  "XLT"      Correct for one-way light time (also
                             called "planetary aberration") using a
                             Newtonian formulation. This correction
                             yields the locus at the moment it
                             receives photons departing from the
                             observer at `et'.

                             The light time correction uses an
                             iterative solution of the light time
                             equation. The solution invoked by the
                             "LT" option uses two iterations.

                             Both the target position as seen by the
                             observer, and rotation of the target
                             body, are corrected for light time.

                  "XLT+S"    Correct for one-way transmission light
                             time and stellar aberration using a
                             Newtonian formulation. This option
                             modifies the locus obtained with the "XLT"
                             option to account for the observer's
                             velocity relative to the solar system
                             barycenter. These corrections yield points
                             on the apparent limb.

                  "XCN"      Converged transmission Newtonian light
                             time correction. In solving the light time
                             equation, the "XCN" correction iterates
                             until the solution converges. Both the
                             position and rotation of the target body
                             are corrected for light time.

                  "XCN+S"    Converged transmission Newtonian light
                             time and stellar aberration corrections.
                             This option produces a solution that is at
                             least as accurate at that obtainable with
                             the `XLT+S' option. Whether the "XCN+S"
                             solution is substantially more accurate
                             depends on the geometry of the
                             participating objects and on the accuracy
                             of the input data. In all cases this
                             routine will execute more slowly when a
                             converged solution is computed.


   corloc      is a string specifying the aberration correction
               locus: the point or set of points for which
               aberration corrections are performed. `corloc' may be
               assigned the values:

                  "CENTER"

                      Light time and stellar aberration corrections
                      are applied to the vector from the observer to
                      the center of the target body. The one way
                      light time from the target center to the
                      observer is used to determine the epoch at
                      which the target body orientation is computed.

                      This choice is appropriate for small target
                      objects for which the light time from the
                      surface to the observer varies little across
                      the entire target. It may also be appropriate
                      for large, nearly ellipsoidal targets when the
                      observer is very far from the target.

                      Computation speed for this option is faster
                      than for the "ELLIPSOID LIMB" option.

                  "ELLIPSOID LIMB"

                      Light time and stellar aberration corrections
                      are applied to individual limb points on the
                      reference ellipsoid. For a limb point on the
                      surface described by topographic data, lying
                      in a specified cutting half-plane, the unique
                      reference ellipsoid limb point in the same
                      half-plane is used as the locus of the
                      aberration corrections.

                      This choice is appropriate for large target
                      objects for which the light time from the limb
                      to the observer is significantly different
                      from the light time from the target center to
                      the observer.

                      Because aberration corrections are repeated for
                      individual limb points, computational speed for
                      this option is relatively slow.


   obsrvr      is the name of the observing body. The observing body
               is an ephemeris object: it typically is a spacecraft,
               the earth, or a surface point on the earth. `obsrvr' is
               case-insensitive, and leading and trailing blanks in
               `obsrvr' are not significant. Optionally, you may
               supply a string containing the integer ID code for
               the object. For example both "MOON" and "301" are
               legitimate strings that indicate the Moon is the
               observer.


   refvec,
   rolstp,
   ncuts       are, respectively, a reference vector, a roll step
               angle, and a count of cutting half-planes.

               `refvec' defines the first of a sequence of cutting
               half-planes in which limb points are to be found.
               Each cutting half-plane has as its edge the line
               containing the observer-target vector; the first
               half-plane contains `refvec'.

               `refvec' is expressed in the body-fixed reference frame
               designated by `fixref'.

               `rolstp' is an angular step by which to roll the
               cutting half-planes about the observer-target vector.
               The first half-plane is aligned with `refvec'; the ith
               half-plane is rotated from `refvec' about the
               observer-target vector in the counter-clockwise
               direction by (i-1)*rolstp. Units are radians.
               `rolstp' should be set to

                  2*pi/ncuts

               to generate an approximately uniform distribution of
               limb points along the limb.

               `ncuts' is the number of cutting half-planes used to
               find limb points; the angular positions of
               consecutive half-planes increase in the positive
               sense (counterclockwise) about the target-observer
               vector and are distributed roughly equally about that
               vector: each half-plane has angular separation of
               approximately

                  `rolstp' radians

               from each of its neighbors. When the aberration
               correction locus is set to "CENTER", the angular
               separation is the value above, up to round-off. When
               the locus is "ELLIPSOID LIMB", the separations are
               less uniform due to differences in the aberration
               corrections used for the respective limb points.


   schstp,
   soltol      are used only for DSK-based surfaces. These inputs
               are, respectively, the search angular step size and
               solution convergence tolerance used to find tangent
               rays and associated limb points within each cutting
               half plane. These values are used when the `method'
               argument includes the "TANGENT" option. In this case,
               limb points are found by a two-step search process:

                  1) Bracketing: starting with the direction
                     opposite the observer-target vector, rays
                     emanating from the observer are generated
                     within the half-plane at successively greater
                     angular separations from the initial direction,
                     where the increment of angular separation is
                     `schstp'. The rays are tested for intersection
                     with the target surface. When a transition
                     between non-intersection to intersection is
                     found, the angular separation of a tangent ray
                     has been bracketed.

                  2) Root finding: each time a tangent ray is
                     bracketed, a search is done to find the angular
                     separation from the starting direction at which
                     a tangent ray exists. The search terminates
                     when successive rays are separated by no more
                     than `soltol'. When the search converges, the
                     last ray-surface intersection point found in
                     the convergence process is considered to be a
                     limb point.


                `schstp' and `soltol' have units of radians.

                Target bodies with simple surfaces---for example,
                convex shapes---will have a single limb point within
                each cutting half-plane. For such surfaces, `schstp'
                can be set large enough so that only one bracketing
                step is taken. A value greater than pi, for example
                4.0, is recommended.

                Target bodies with complex surfaces can have
                multiple limb points within a given cutting
                half-plane. To find all limb points, `schstp' must be
                set to a value smaller than the angular separation
                of any two limb points in any cutting half-plane,
                where the vertex of the angle is the observer.
                `schstp' must not be too small, or the search will be
                excessively slow.

                For both kinds of surfaces, `soltol' must be chosen so
                that the results will have the desired precision.
                Note that the choice of `soltol' required to meet a
                specified bound on limb point height errors depends
                on the observer-target distance.


   maxn        is the maximum number of limb points that can be
               stored in the output array `points'.

-Detailed_Output

   npts        is an array of counts of limb points within the
               specified set of cutting half-planes. The Ith
               element of `npts' is the limb point count in the Ith
               half-plane. `npts' should be declared with length
               at least `ncuts'.

               For most target bodies, there will be one limb point
               per half-plane. For complex target shapes, the limb
               point count in a given half-plane can be greater
               than one (see example 3 below), and it can be zero.


   points      is an array containing the limb points found by this
               routine. Sets of limb points associated with
               half-planes are ordered by the indices of the
               half-planes in which they're found. The limb points
               in a given half-plane are ordered by decreasing
               angular separation from the observer-target
               direction; the outermost limb point in a given
               half-plane is the first of that set.

               The limb points for the half-plane containing `refvec'
               occupy array elements

                  points[ 0         ][0]         through
                  points[ npts[0]-1 ][2]

               Limb points for the second half plane occupy
               elements

                  points[ npts[0]           ][0] through
                  points[ npts[0]+npts[1]-1 ][2]

               and so on.

               `points' should be declared with dimensions

                  [maxn][3]

               Limb points are expressed in the reference frame
               designated by `fixref'. For each limb point, the
               orientation of the frame is evaluated at the epoch
               corresponding to the limb point; the epoch is
               provided in the output array `epochs' (described
               below).

               Units of the limb points are km.


   epochs      is an array of epochs associated with the limb
               points, accounting for light time if aberration
               corrections are used. `epochs' contains one element
               for each limb point. `epochs' should be declared
               with length

                  maxn

               The element

                  epochs[i]

               is associated with the limb point

                  points[i][j], j = 0 to 2

               If `corloc' is set to "CENTER", all values of `epochs'
               will be the epoch associated with the target body
               center. That is, if aberration corrections are used,
               and if `lt' is the one-way light time from the target
               center to the observer, the elements of `epochs' will
               all be set to

                  et - lt

               If `corloc' is set to "ELLIPSOID LIMB", all values of
               `epochs' for the limb points in a given half plane
               will be those for the reference ellipsoid limb point
               in that half plane. That is, if aberration
               corrections are used, and if lt[i] is the one-way
               light time to the observer from the reference
               ellipsoid limb point in the ith half plane, the
               elements of `epochs' for that half plane will all be
               set to

                  et - lt[i]


   tangts      is an array of tangent vectors connecting the
               observer to the limb points. The tangent vectors are
               expressed in the frame designated by `fixref'. For the
               Ith vector, the orientation of the frame is
               evaluated at the Ith epoch provided in the output
               array `epochs' (described above).

               `tangts' should be declared with dimensions

                  [maxn][3]

               The elements

                  tangts[i][j], j = 0 to 2

               are associated with the limb point

                  points[i][j], j = 0 to 2

               Units of the tangent vectors are km.

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

   6)  If the input argument `method' is not recognized, the error
       SPICE(INVALIDMETHOD) is signaled by either this routine or a
       routine in the call tree of this routine.

   7)  If `method' contains an invalid limb type, the error
       SPICE(INVALIDLIMBTYPE) is signaled by a routine in the call
       tree of this routine.

   8)  If the target and observer have distinct identities but are
       at the same location, the error SPICE(NOSEPARATION) is
       signaled by a routine in the call tree of this routine.

   9)  If insufficient ephemeris data have been loaded prior to
       calling limbpt_c, an error is signaled by a routine in
       the call tree of this routine. When light time correction is
       used, sufficient ephemeris data must be available to
       propagate the states of both observer and target to the solar
       system barycenter.

   10) If the computation method requires an ellipsoidal target shape
       and triaxial radii of the target body have not been loaded
       into the kernel pool prior to calling limbpt_c, an error is
       signaled by a routine in the call tree of this routine.

       When the target shape is modeled by topographic data, radii
       of the reference triaxial ellipsoid are still required if
       the aberration correction locus is ELLIPSOID LIMB or if
       the limb point generation method is GUIDED.

   11) If the radii are available in the kernel pool but the count
       of radii values is not three, the error SPICE(BADRADIUSCOUNT)
       is signaled by a routine in the call tree of this routine.

   12) If the target body's shape is modeled as an ellipsoid, and if
       any of the radii of the target body are non-positive, an error
       is signaled by a routine in the call tree of this routine. The
       target must be an extended body.

   13) If PCK data specifying the target body-fixed frame orientation
       have not been loaded prior to calling limbpt_c, an error is
       signaled by a routine in the call tree of this routine.

   14) If `method' specifies that the target surface is represented by
       DSK data, and no DSK files are loaded for the specified
       target, an error is signaled by a routine in the call tree
       of this routine.

   15) If the array bound `maxn' is less than 1, the error
       SPICE(INVALIDSIZE) is signaled by a routine in the call tree
       of this routine.

   16) If the number of cutting half-planes specified by `ncuts' is
       negative or greater than `maxn', the error SPICE(INVALIDCOUNT)
       is signaled by a routine in the call tree of this routine.

   17) If the aberration correction locus is not recognized, the
       error SPICE(INVALIDLOCUS) is signaled by a routine in the call
       tree of this routine.

   18) If the aberration correction locus is "ELLIPSOID LIMB" but
       limb type is not "TANGENT", the error SPICE(BADLIMBLOCUSMIX)
       is signaled by a routine in the call tree of this routine.

   19) If the reference vector `refvec' is the zero vector, the error
       SPICE(ZEROVECTOR) is signaled by a routine in the call tree of
       this routine.

   20) If the reference vector `refvec' and the observer target vector
       are linearly dependent, the error SPICE(DEGENERATECASE) is
       signaled by a routine in the call tree of this routine.

   21) If the limb computation uses the target ellipsoid limb plane,
       and the limb plane normal and reference vector `refvec' are
       linearly dependent, the error SPICE(DEGENERATECASE) is
       signaled by a routine in the call tree of this routine.

   22) If the limb points cannot all be stored in the output `points'
       array, the error SPICE(OUTOFROOM) is signaled by a routine in
       the call tree of this routine.

   23) If the surface is represented by DSK data, and if the search
       step is non-positive, the error SPICE(INVALIDSEARCHSTEP) is
       signaled by a routine in the call tree of this routine.

   24) If the surface is represented by DSK data, and if the search
       tolerance is non-positive, the error SPICE(INVALIDTOLERANCE)
       is signaled by a routine in the call tree of this routine.

   25) If the roll step is non-positive and `ncuts' is greater than 1,
       the error SPICE(INVALIDROLLSTEP) is signaled by a routine in
       the call tree of this routine.

   26) If any of the `method', `target', `fixref', `abcorr', `corloc'
       or `obsrvr' input string pointers is null, the error
       SPICE(NULLPOINTER) is signaled.

   27) If any of the `method', `target', `fixref', `abcorr', `corloc'
       or `obsrvr' input strings has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   Appropriate kernels must be loaded by the calling program before
   this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for target and observer must be
      loaded. If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data.
      Typically ephemeris data are made available by loading one
      or more SPK files via furnsh_c.

   -  Target body orientation data: these may be provided in a text
      or binary PCK file. In some cases, target body orientation
      may be provided by one more more CK files. In either case,
      data are made available by loading the files via furnsh_c.

   -  Shape data for the target body:

        PCK data:

           If the target body shape is modeled as an ellipsoid,
           triaxial radii for the target body must be loaded into
           the kernel pool. Typically this is done by loading a
           text PCK file via furnsh_c.

           Triaxial radii are also needed if the target shape is
           modeled by DSK data but one or both of the "GUIDED" limb
           definition method or the "ELLIPSOID LIMB" aberration
           correction locus are selected.

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
      of such a set of assignments is

         NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG'
         NAIF_SURFACE_CODE += 1
         NAIF_SURFACE_BODY += 499

   -  SCLK data: if the target body's orientation is provided by
      CK files, an associated SCLK kernel must be loaded.


   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

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
      -------------------------------------

      The keywords and surface list in the `method' argument
      are called "clauses." The clauses may appear in any
      order, for example

         TANGENT/DSK/UNPRIORITIZED/<surface list>
         DSK/TANGENT/<surface list>/UNPRIORITIZED
         UNPRIORITIZED/<surface list>/DSK/TANGENT

      The simplest form of the `method' argument specifying use of
      DSK data is one that lacks a surface list, for example:

         "TANGENT/DSK/UNPRIORITIZED"
         "GUIDED/DSK/UNPRIORITIZED"

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

         SURFACES =

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

      Double quotes are used to delimit the surface name because
      it contains blank characters.

      To use data for surfaces 2 and 3 together, any
      of the following surface lists could be used:

         "SURFACES = 2, 3"

         "SURFACES = \"Mars MEGDR  64 PIXEL/DEG\", 3"

         "SURFACES = 2, Mars_MRO_HIRISE"

         "SURFACES = \"Mars MEGDR 64 PIXEL/DEG\", Mars_MRO_HIRISE"

      An example of a `method' argument that could be constructed
      using one of the surface lists above is

      "NADIR/DSK/UNPRIORITIZED/SURFACES= \"Mars MEGDR 64 PIXEL/DEG\",3"

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Find apparent limb points on Phobos as seen from Mars.

      Due to Phobos' irregular shape, the TANGENT limb point
      definition will used. It suffices to compute light time and
      stellar aberration corrections for the center of Phobos, so
      the "CENTER" aberration correction locus will be used. Use
      converged Newtonian light time and stellar aberration
      corrections in order to model the apparent position and
      orientation of Phobos.

      For comparison, compute limb points using both ellipsoid
      and topographic shape models.

      Use the target body-fixed +Z axis as the reference direction
      for generating cutting half-planes. This choice enables the
      user to see whether the first limb point is near the target's
      north pole.

      For each option, use just three cutting half-planes, in order
      to keep the volume of output manageable. In most applications,
      the number of cuts and the number of resulting limb points
      would be much greater.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: limbpt_ex1.tm

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
            phobos512.bds                    DSK based on
                                             Gaskell ICQ Q=512
                                             Phobos plate model
         \begindata

            KERNELS_TO_LOAD = ( 'de430.bsp',
                                'mar097.bsp',
                                'pck00010.tpc',
                                'naif0011.tls',
                                'phobos512.bds' )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program limbpt_ex1

         Find limb points on Phobos as seen from Mars.

         Compute limb points using the tangent definition.
         Perform aberration corrections for the target center.
         Use both ellipsoid and DSK shape models.
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define META            "limbpt_ex1.tm"
         #define MTHLEN          51
         #define NMETH            2
         #define MAXN         10000

         /.
         Local variables
         ./
         SpiceChar             * abcorr;
         SpiceChar             * corloc;
         SpiceChar             * fixref;
         SpiceChar             * obsrvr;

         SpiceChar               method [NMETH][MTHLEN] =

                           { "TANGENT/ELLIPSOID",
                             "TANGENT/DSK/UNPRIORITIZED" };

         SpiceChar             * target;

         SpiceDouble             delrol;
         SpiceDouble             et;
         SpiceDouble             points  [MAXN][3];
         SpiceDouble             roll;
         SpiceDouble             schstp;
         SpiceDouble             soltol;
         SpiceDouble             tangts  [MAXN][3];
         SpiceDouble             trgeps  [MAXN];
         SpiceDouble             z       [3] = { 0.0, 0.0, 1.0 };

         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                k;
         SpiceInt                ncuts;
         SpiceInt                npts    [MAXN];
         SpiceInt                start;

         /.
         Load kernel files via the meta-kernel.
         ./
         furnsh_c ( META );

         /.
         Set target, observer, and target body-fixed,
         body-centered reference frame.
         ./
         obsrvr = "MARS";
         target = "PHOBOS";
         fixref = "IAU_PHOBOS";

         /.
         Set aberration correction and correction locus.
         ./
         abcorr = "CN+S";
         corloc = "CENTER";

         /.
         Convert the UTC request time string seconds past
         J2000, TDB.
         ./
         str2et_c ( "2008 AUG 11 00:00:00", &et );

         /.
         Compute a set of limb points using light time and
         stellar aberration corrections. Use both ellipsoid
         and DSK shape models. Use a step size of 100
         microradians to ensure we don't miss the limb.
         Set the convergence tolerance to 100 nanoradians,
         which will limit the height error to about 1 meter.
         Compute 3 limb points for each computation method.
         ./
         schstp = 1.0e-4;
         soltol = 1.0e-7;
         ncuts  = 3;

         printf ( "\n"
                  "Observer:       %s\n"
                  "Target:         %s\n"
                  "Frame:          %s\n"
                  "\n"
                  "Number of cuts: %d\n",
                  obsrvr,
                  target,
                  fixref,
                  (int)ncuts            );

         delrol = twopi_c() / ncuts;


         for ( i = 0;  i < NMETH;  i++ )
         {
            limbpt_c ( method[i], target, et,     fixref,
                       abcorr,    corloc, obsrvr, z,
                       delrol,    ncuts,  schstp, soltol,
                       MAXN,      npts,   points, trgeps,
                       tangts                            );
            /.
            Write the results.
            ./
            printf ( "\n\n"
                     "Computation method = %s\n"
                     "Locus              = %s\n",
                     method[i],
                     corloc                     );

            start = 0;

            for ( j = 0;  j < ncuts;  j++ )
            {
               roll = j * delrol;

               printf ( "\n"
                        "  Roll angle (deg) = %21.9f\n"
                        "     Target epoch  = %21.9f\n"
                        "     Number of limb points at this "
                        "roll angle: %d\n",
                        roll * dpr_c(),
                        trgeps[j],
                        npts[j]                            );

               printf ( "      Limb points\n" );

               for ( k = 0;  k < npts[j];  k++ )
               {
                  printf ( " %20.9f %20.9f %20.9f\n",
                           points[k+start][0],
                           points[k+start][1],
                           points[k+start][2]        );
               }

               start += npts[j];
            }
         }
         printf ( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Observer:       MARS
      Target:         PHOBOS
      Frame:          IAU_PHOBOS

      Number of cuts: 3


      Computation method = TANGENT/ELLIPSOID
      Locus              = CENTER

        Roll angle (deg) =           0.000000000
           Target epoch  =   271684865.152078211
           Number of limb points at this roll angle: 1
            Limb points
                0.016445326         -0.000306114          9.099992715

        Roll angle (deg) =         120.000000000
           Target epoch  =   271684865.152078211
           Number of limb points at this roll angle: 1
            Limb points
               -0.204288375         -9.235230829         -5.333237706

        Roll angle (deg) =         240.000000000
           Target epoch  =   271684865.152078211
           Number of limb points at this roll angle: 1
            Limb points
                0.242785221          9.234520095         -5.333231253


      Computation method = TANGENT/DSK/UNPRIORITIZED
      Locus              = CENTER

        Roll angle (deg) =           0.000000000
           Target epoch  =   271684865.152078211
           Number of limb points at this roll angle: 1
            Limb points
               -0.398901673          0.007425178          9.973720555

        Roll angle (deg) =         120.000000000
           Target epoch  =   271684865.152078211
           Number of limb points at this roll angle: 1
            Limb points
               -0.959300281         -8.537573427         -4.938700447

        Roll angle (deg) =         240.000000000
           Target epoch  =   271684865.152078211
           Number of limb points at this roll angle: 1
            Limb points
               -1.380536729          9.714334047         -5.592916790


   2) Find apparent limb points on Mars as seen from the earth.
      Compare results using different computation options.

      Use both the "TANGENT" and "GUIDED" limb point definitions. For
      the tangent limb points, use the "ELLIPSOID LIMB" aberration
      correction locus; for the guided limb points, use the "CENTER"
      locus. For the "GUIDED" limb points, also compute the distance
      of each point from the corresponding point computed using the
      "TANGENT" definition.

      For comparison, compute limb points using both ellipsoid and
      topographic shape models.

      Check the limb points by computing the apparent emission
      angles at each limb point.

      For the ellipsoid shape model, we expect emission angles very
      close to 90 degrees, since each illumination angle calculation
      is done using aberration corrections for the limb point at
      which the angles are measured.

      Use the target body-fixed +Z axis as the reference direction
      for generating cutting half-planes. This choice enables the
      user to see whether the first limb point is near the target's
      north pole.

      For each option, use just three cutting half-planes, in order
      to keep the volume of output manageable. In most applications,
      the number of cuts and the number of resulting limb points
      would be much greater.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: limbpt_ex2.tm

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
            megr90n000cb_plate.bds           DSK plate model based on
                                             MGS MOLAR MEGDR DEM,
                                             resolution 4
                                             pixels/degree.

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
         Program limbpt_ex1

         Find limb points on Mars as seen from the earth.

         Compute limb points using both the tangent and
         "guided" definitions.

         For the tangent limb points, perform aberration
         corrections for the reference ellipsoid limb.

         Check limb points by computing emission angles
         at each point.

         Use both ellipsoid and DSK shape models.
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define META            "limbpt_ex2.tm"
         #define CORLEN          21
         #define MTHLEN          51
         #define NMETH            3
         #define MAXN         10000

         /.
         Local variables
         ./
         SpiceChar             * abcorr;

         ConstSpiceChar        * corloc [NMETH] =

                           { "ELLIPSOID LIMB",
                             "ELLIPSOID LIMB",
                             "CENTER"          };

         SpiceChar             * fixref;

         ConstSpiceChar        * ilumth [NMETH] =

                           { "ELLIPSOID",
                             "DSK/UNPRIORITIZED",
                             "DSK/UNPRIORITIZED" };

         ConstSpiceChar        * method [NMETH] =

                           { "TANGENT/ELLIPSOID",
                             "TANGENT/DSK/UNPRIORITIZED",
                             "GUIDED/DSK/UNPRIORITIZED"  };

         SpiceChar             * obsrvr;
         SpiceChar             * target;

         SpiceDouble             alt;
         SpiceDouble             delrol;
         SpiceDouble             dist;
         SpiceDouble             emissn;
         SpiceDouble             et;
         SpiceDouble             f;
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             lt;
         SpiceDouble             phase;
         static SpiceDouble      points  [MAXN][3];
         static SpiceDouble      svpnts  [MAXN][3];
         SpiceDouble             pos     [3];
         SpiceDouble             radii   [3];
         SpiceDouble             re;
         SpiceDouble             roll;
         SpiceDouble             rp;
         SpiceDouble             schstp;
         SpiceDouble             solar;
         SpiceDouble             soltol;
         SpiceDouble             srfvec  [3];
         static SpiceDouble      tangts  [MAXN][3];
         SpiceDouble             trgepc;
         static SpiceDouble      trgeps  [MAXN];
         SpiceDouble             z       [3] = { 0.0, 0.0, 1.0 };

         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                k;
         SpiceInt                m;
         SpiceInt                n;
         SpiceInt                ncuts;
         static SpiceInt         npts    [MAXN];
         SpiceInt                start;

         /.
         Load kernel files via the meta-kernel.
         ./
         furnsh_c ( META );

         /.
         Set target, observer, and target body-fixed,
         body-centered reference frame.
         ./
         obsrvr = "EARTH";
         target = "MARS";
         fixref = "IAU_MARS";

         /.
         Set aberration correction. We'll set the correction
         locus below.
         ./
         abcorr = "CN+S";

         /.
         Convert the UTC request time string seconds past
         J2000, TDB.
         ./
         str2et_c ( "2008 AUG 11 00:00:00", &et );

         /.
         Look up the target body's radii. We'll use these to
         convert Cartesian to planetographic coordinates. Use
         the radii to compute the flattening coefficient of
         the reference ellipsoid.
         ./
         bodvrd_c ( target, "RADII", 3, &n, radii );

         /.
         Compute the flattening coefficient for planetodetic
         coordinates
         ./
         re = radii[0];
         rp = radii[2];
         f  = ( re - rp ) / re;

         /.
         Compute a set of limb points using light time and
         stellar aberration corrections. Use both ellipsoid
         and DSK shape models.

         Obtain the observer-target distance at ET.
         ./
         spkpos_c ( target, et,  "J2000", abcorr,
                    obsrvr, pos, &lt              );

         dist = vnorm_c( pos );

         /.
         Set the angular step size so that a single step will
         be taken in the root bracketing process; that's all
         that is needed since we don't expect to have multiple
         limb points in any cutting half-plane.
         ./
         schstp = 4.0;

         /.
         Set the convergence tolerance to minimize the height
         error. We can't achieve the 1 millimeter precision
         suggested by the formula because the earth-Mars
         distance is about 3.5e8 km. Compute 3 limb points
         for each computation method.
         ./
         soltol = 1.0e-6/dist;

         /.
         Set the number of cutting half-planes and roll step.
         ./
         ncuts  = 3;
         delrol = twopi_c() / ncuts;

         printf ( "\n"
                  "Observer:       %s\n"
                  "Target:         %s\n"
                  "Frame:          %s\n"
                  "\n"
                  "Number of cuts: %d\n",
                  obsrvr,
                  target,
                  fixref,
                  (int)ncuts            );

         for ( i = 0;  i < NMETH;  i++ )
         {
            limbpt_c ( method[i], target,    et,     fixref,
                       abcorr,    corloc[i], obsrvr, z,
                       delrol,    ncuts,     schstp, soltol,
                       MAXN,      npts,      points, trgeps,
                       tangts                               );
            /.
            Write the results.
            ./
            printf ( "\n"
                     "Computation method = %s\n"
                     "Locus              = %s\n",
                     method[i],
                     corloc[i]                   );

            start = 0;

            for ( j = 0;  j < ncuts;  j++ )
            {
               roll = j * delrol;

               printf ( "\n"
                        "  Roll angle (deg) = %21.9f\n"
                        "     Target epoch  = %21.9f\n"
                        "     Number of limb points at this "
                        "roll angle: %d\n",
                        roll * dpr_c(),
                        trgeps[j],
                        npts[j]                            );

               for ( k = 0;  k < npts[j];  k++ )
               {
                  printf ( "      Limb point planetodetic "
                           "coordinates:\n"                );

                  recgeo_c ( points[start+k], re,   f,
                             &lon,            &lat, &alt );

                  printf ( "       Longitude      (deg): %21.9f\n"
                           "       Latitude       (deg): %21.9f\n"
                           "       altitude        (km): %21.9f\n",
                           lon*dpr_c(), lat*dpr_c(), alt           );

                  /.
                  Get illumination angles for this limb point.
                  ./
                  m = start + k;

                  ilumin_c ( ilumth[i], target,  et,
                             fixref,    abcorr,  obsrvr,
                             points[m], &trgepc, srfvec,
                             &phase,    &solar,  &emissn );

                  printf ( "       Emission angle (deg): %21.9f\n",
                           emissn*dpr_c()                          );

                  if ( i == 1 )
                  {
                     vequ_c ( points[m], svpnts[m] );
                  }
                  else if ( i == 2 )
                  {
                     dist = vdist_c ( points[m], svpnts[m] );

                     printf ( "       Distance error  (km): "
                              "%21.9f\n",
                     dist                                    );
                  }
               }
               start += npts[j];
            }
            printf( "\n" );
         }
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Observer:       EARTH
      Target:         MARS
      Frame:          IAU_MARS

      Number of cuts: 3

      Computation method = TANGENT/ELLIPSOID
      Locus              = ELLIPSOID LIMB

        Roll angle (deg) =           0.000000000
           Target epoch  =   271683700.368869901
           Number of limb points at this roll angle: 1
            Limb point planetodetic coordinates:
             Longitude      (deg):         -19.302258950
             Latitude       (deg):          64.005620446
             altitude        (km):          -0.000000000
             Emission angle (deg):          90.000000000

        Roll angle (deg) =         120.000000000
           Target epoch  =   271683700.368948162
           Number of limb points at this roll angle: 1
            Limb point planetodetic coordinates:
             Longitude      (deg):          85.029135674
             Latitude       (deg):         -26.912378799
             altitude        (km):           0.000000000
             Emission angle (deg):          90.000000000

        Roll angle (deg) =         240.000000000
           Target epoch  =   271683700.368949771
           Number of limb points at this roll angle: 1
            Limb point planetodetic coordinates:
             Longitude      (deg):        -123.633654215
             Latitude       (deg):         -26.912378799
             altitude        (km):          -0.000000000
             Emission angle (deg):          90.000000000


      Computation method = TANGENT/DSK/UNPRIORITIZED
      Locus              = ELLIPSOID LIMB

        Roll angle (deg) =           0.000000000
           Target epoch  =   271683700.368869901
           Number of limb points at this roll angle: 1
            Limb point planetodetic coordinates:
             Longitude      (deg):         -19.302258949
             Latitude       (deg):          63.893637432
             altitude        (km):          -3.667553958
             Emission angle (deg):          89.979580513

        Roll angle (deg) =         120.000000000
           Target epoch  =   271683700.368948162
           Number of limb points at this roll angle: 1
            Limb point planetodetic coordinates:
             Longitude      (deg):          85.434644181
             Latitude       (deg):         -26.705411232
             altitude        (km):          -0.044832382
             Emission angle (deg):          88.089500425

        Roll angle (deg) =         240.000000000
           Target epoch  =   271683700.368949771
           Number of limb points at this roll angle: 1
            Limb point planetodetic coordinates:
             Longitude      (deg):        -123.375003592
             Latitude       (deg):         -27.043096738
             altitude        (km):           3.695628489
             Emission angle (deg):          89.875890611


      Computation method = GUIDED/DSK/UNPRIORITIZED
      Locus              = CENTER

        Roll angle (deg) =           0.000000000
           Target epoch  =   271683700.368922532
           Number of limb points at this roll angle: 1
            Limb point planetodetic coordinates:
             Longitude      (deg):         -19.302259163
             Latitude       (deg):          64.005910146
             altitude        (km):          -3.676424552
             Emission angle (deg):          89.979580513
             Distance error  (km):           6.664208540

        Roll angle (deg) =         120.000000000
           Target epoch  =   271683700.368922532
           Number of limb points at this roll angle: 1
            Limb point planetodetic coordinates:
             Longitude      (deg):          85.029135792
             Latitude       (deg):         -26.912405352
             altitude        (km):          -0.328988915
             Emission angle (deg):          91.525256314
             Distance error  (km):          24.686472888

        Roll angle (deg) =         240.000000000
           Target epoch  =   271683700.368922532
           Number of limb points at this roll angle: 1
            Limb point planetodetic coordinates:
             Longitude      (deg):        -123.633653487
             Latitude       (deg):         -26.912086524
             altitude        (km):           3.626058850
             Emission angle (deg):          89.809897171
             Distance error  (km):          15.716056568


   3) Find apparent limb points on comet Churyumov-Gerasimenko
      as seen from the Rosetta orbiter.

      This computation is an example of a case for which some
      of the cutting half-planes contain multiple limb points.

      Use the "TANGENT" limb definition, since the target shape
      is not well approximated by its reference ellipsoid.
      Use the "CENTER" aberration correction locus since the
      light time difference across the object is small.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: limbpt_ex3.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         paths of the kernels referenced here must be adjusted to
         be compatible with the user's host computer directory
         structure.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

           File name                          Contents
           ---------                          --------
           DE405.BSP                          Planetary ephemeris
           NAIF0011.TLS                       Leapseconds
           ROS_CG_M004_NSPCESA_N_V1.BDS       DSK plate model based on
                                              Rosetta NAVCAM data
           RORB_DV_145_01_______00216.BSP     Rosetta orbiter
                                              ephemeris
           CORB_DV_145_01_______00216.BSP     Comet Churyumov-
                                              Gerasimenko ephemeris
           ROS_CG_RAD_V10.TPC                 Comet Churyumov-
                                              Gerasimenko radii
           ROS_V25.TF                         Comet C-G frame kernel
                                              (includes SCLK
                                              parameters)
           CATT_DV_145_01_______00216.BC      Comet C-G C-kernel


         \begindata

           KERNELS_TO_LOAD = ( 'DE405.BSP'
                               'NAIF0011.TLS'
                               'RORB_DV_145_01_______00216.BSP'
                               'CORB_DV_145_01_______00216.BSP'
                               'ROS_CG_RAD_V10.TPC'
                               'ROS_V25.TF'
                               'CATT_DV_145_01_______00216.BC'
                               'ROS_CG_M004_NSPCESA_N_V1.BDS'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program limbpt_ex1


         Find limb points on comet Churyumov-Gerasimenko
         as seen from the Rosetta orbiter.

         Compute limb points using the tangent definition.
         Perform aberration corrections for the target center.
         Use both ellipsoid and DSK shape models.

         Display only limb points lying in half-planes that
         contain multiple limb points.
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define META            "limbpt_ex3.tm"
         #define CORLEN          21
         #define MTHLEN          51
         #define MAXN         10000

         /.
         Local variables
         ./
         SpiceChar             * abcorr;
         SpiceChar             * corloc;
         SpiceChar             * fixref;

         ConstSpiceChar        * method =

                           { "TANGENT/DSK/UNPRIORITIZED" };

         SpiceChar             * obsrvr;
         SpiceChar             * target;

         SpiceDouble             angle;
         SpiceDouble             axis   [3];
         SpiceDouble             delrol;
         SpiceDouble             et;
         SpiceDouble             lt;
         static SpiceDouble      points  [MAXN][3];
         SpiceDouble             refvec  [3];
         SpiceDouble             roll;
         SpiceDouble             schstp;
         SpiceDouble             soltol;
         static SpiceDouble      tangts  [MAXN][3];
         static SpiceDouble      trgeps  [MAXN];
         SpiceDouble             trgpos  [3];
         SpiceDouble             xvec    [3] = { 1.0, 0.0, 0.0 };

         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                ncuts;
         static SpiceInt         npts    [MAXN];
         SpiceInt                start;

         /.
         Load kernel files via the meta-kernel.
         ./
         furnsh_c ( META );

         /.
         Set target, observer, and target body-fixed,
         body-centered reference frame.
         ./
         obsrvr = "ROSETTA";
         target = "CHURYUMOV-GERASIMENKO";
         fixref = "67P/C-G_CK";

         /.
         Set aberration correction and correction locus.
         ./
         abcorr = "CN+S";
         corloc = "CENTER";

         /.
         Convert the UTC request time string seconds past
         J2000, TDB.
         ./
         str2et_c ( "2015 MAY 10 00:00:00", &et );

         /.
         Compute a set of limb points using light time and
         stellar aberration corrections. Use a step size
         corresponding to a 1 meter height error to ensure
         we don't miss the limb. Set the convergence tolerance
         to 1/100 of this amount, which will limit the height
         convergence error to about 1 cm.
         ./
         spkpos_c ( target, et,     fixref, abcorr,
                    obsrvr, trgpos, &lt            );

         schstp = 1.e-2  / vnorm_c( trgpos );
         soltol = schstp / 100.0;

         /.
         Set the reference vector to the start of a
         region of the roll domain on which we know
         (from an external computation) that we'll
         find multiple limb points in some half planes.
         Compute 6 limb points, starting with the
         half-plane containing the reference vector.
         ./

         vminus_c ( trgpos, axis );

         angle = 310.0 * rpd_c();

         vrotv_c  ( xvec, axis, angle, refvec );

         ncuts  = 6;
         delrol = twopi_c() / 1000.0;

         printf ( "\n"
                  "Observer:       %s\n"
                  "Target:         %s\n"
                  "Frame:          %s\n"
                  "\n"
                  "Number of cuts: %d\n",
                  obsrvr,
                  target,
                  fixref,
                  (int)ncuts            );

         limbpt_c ( method, target, et,     fixref,
                    abcorr, corloc, obsrvr, refvec,
                    delrol, ncuts,  schstp, soltol,
                    MAXN,   npts,   points, trgeps,
                    tangts                          );
         /.
         Write the results.
         ./
         printf ( "\n\n"
                  "Computation method = %s\n"
                  "Locus              = %s\n",
                  method,
                  corloc                   );

         start = 0;

         for ( i = 0;  i < ncuts;  i++ )
         {
            roll = i * delrol;

            printf ( "\n"
                     "  Roll angle (deg) = %21.9f\n"
                     "     Target epoch  = %21.9f\n"
                     "     Number of limb points at this "
                     "roll angle: %d\n",
                     roll * dpr_c(),
                     trgeps[i],
                     npts[i]                            );

            if ( npts[i] > 1 )
            {
               printf ( "      Limb points\n" );

               for ( j = 0;  j < npts[i];  j++ )
               {
                  printf( "%21.9f  %21.9f  %21.9f\n",
                          points[start+j][0],
                          points[start+j][1],
                          points[start+j][2]         );
               }
            }
            start += npts[i];
         }
         printf( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Observer:       ROSETTA
      Target:         CHURYUMOV-GERASIMENKO
      Frame:          67P/C-G_CK

      Number of cuts: 6


      Computation method = TANGENT/DSK/UNPRIORITIZED
      Locus              = CENTER

        Roll angle (deg) =           0.000000000
           Target epoch  =   484488067.184933782
           Number of limb points at this roll angle: 3
            Limb points
                1.320416231           -0.347379011            1.445260615
                0.970350318            0.201685071            0.961996205
                0.436720618            0.048224590            0.442280714

        Roll angle (deg) =           0.360000000
           Target epoch  =   484488067.184933782
           Number of limb points at this roll angle: 3
            Limb points
                1.330290293           -0.352340416            1.438802587
                0.965481808            0.202131806            0.946190003
                0.453917030            0.082062880            0.447624224

        Roll angle (deg) =           0.720000000
           Target epoch  =   484488067.184933782
           Number of limb points at this roll angle: 3
            Limb points
                1.339037339           -0.357848188            1.431256926
                0.962159098            0.192370269            0.934342086
                0.459160821            0.082273840            0.447880429

        Roll angle (deg) =           1.080000000
           Target epoch  =   484488067.184933782
           Number of limb points at this roll angle: 3
            Limb points
                1.346729151           -0.365488231            1.423051540
                0.960760394            0.183652804            0.924323093
                0.464582286            0.084076587            0.447930141

        Roll angle (deg) =           1.440000000
           Target epoch  =   484488067.184933782
           Number of limb points at this roll angle: 3
            Limb points
                1.351235771           -0.380664224            1.413164272
                0.960268777            0.176953543            0.914876859
                0.466284590            0.079312729            0.445564308

        Roll angle (deg) =           1.800000000
           Target epoch  =   484488067.184933782
           Number of limb points at this roll angle: 3
            Limb points
                1.358042184           -0.390349186            1.404421386
                0.959495690            0.170340551            0.905212642
                0.370611049           -0.167047205            0.395076979


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 2.0.0, 01-NOV-2021 (NJB) (JDR)

       Added support for transmission aberration corrections.

       Corrected description of iteration count for non-converged
       corrections.

       Edited the header to comply with NAIF standard. Reduced
       the number of cuts to present in the output in Example #3.

   -CSPICE Version 1.0.0, 05-APR-2017 (NJB)

-Index_Entries

   find limb points on target body

-&
*/

{ /* Begin limbpt_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "limbpt_c" );

   /*
   Check the input string arguments:

      method
      target
      fixref
      abcorr
      corloc
      obsrvr

   Make sure each pointer is non-null and each string contains
   at least one data character: that is, one character
   preceding the null terminator.
   */
   CHKFSTR ( CHK_STANDARD, "limbpt_c", method );
   CHKFSTR ( CHK_STANDARD, "limbpt_c", target );
   CHKFSTR ( CHK_STANDARD, "limbpt_c", fixref );
   CHKFSTR ( CHK_STANDARD, "limbpt_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "limbpt_c", corloc );
   CHKFSTR ( CHK_STANDARD, "limbpt_c", obsrvr );


   /*
   Call the f2c'd SPICELIB function.
   */

   limbpt_ ( ( char         * ) method,
             ( char         * ) target,
             ( doublereal   * ) &et,
             ( char         * ) fixref,
             ( char         * ) abcorr,
             ( char         * ) corloc,
             ( char         * ) obsrvr,
             ( doublereal   * ) refvec,
             ( doublereal   * ) &rolstp,
             ( integer      * ) &ncuts,
             ( doublereal   * ) &schstp,
             ( doublereal   * ) &soltol,
             ( integer      * ) &maxn,
             ( integer      * ) npts,
             ( doublereal   * ) points,
             ( doublereal   * ) epochs,
             ( doublereal   * ) tangts,
             ( ftnlen         ) strlen(method),
             ( ftnlen         ) strlen(target),
             ( ftnlen         ) strlen(fixref),
             ( ftnlen         ) strlen(abcorr),
             ( ftnlen         ) strlen(corloc),
             ( ftnlen         ) strlen(obsrvr)  );

   chkout_c ( "limbpt_c" );

} /* End limbpt_c */
