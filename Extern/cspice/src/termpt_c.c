/*

-Procedure termpt_c ( Terminator points on an extended object )

-Abstract

   Find terminator points on a target body. The caller specifies
   half-planes, bounded by the illumination source center-target
   center vector, in which to search for terminator points.

   The terminator can be either umbral or penumbral. The umbral
   terminator is the boundary of the region on the target surface
   where no light from the source is visible. The penumbral
   terminator is the boundary of the region on the target surface
   where none of the light from the source is blocked by the target
   itself.

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

   DSK
   GEOMETRY
   SHADOW
   TERMINATOR

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef termpt_c


   void termpt_c ( ConstSpiceChar      * method,
                   ConstSpiceChar      * ilusrc,
                   ConstSpiceChar      * target,
                   SpiceDouble           et,
                   ConstSpiceChar      * fixref,
                   ConstSpiceChar      * abcorr,
                   ConstSpiceChar      * corloc,
                   ConstSpiceChar      * obsrvr,
                   ConstSpiceDouble      refvec[3],
                   SpiceDouble           rolstp,
                   SpiceInt              ncuts,
                   SpiceDouble           schstp,
                   SpiceDouble           soltol,
                   SpiceInt              maxn,
                   SpiceInt              npts  [],
                   SpiceDouble           points[][3],
                   SpiceDouble           epochs[],
                   SpiceDouble           trmvcs[][3]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   method     I   Computation method.
   ilusrc     I   Illumination source.
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
   npts       O   Counts of terminator points corresponding to cuts.
   points     O   Terminator points.
   epochs     O   Times associated with terminator points.
   trmvcs     O   Terminator vectors emanating from the observer.

-Detailed_Input

   method      is a short string providing parameters defining
               the computation method to be used. In the syntax
               descriptions below, items delimited by angle brackets
               "<>" are to be replaced by actual values. Items
               delimited by brackets "[]" are optional.

               `method' may be assigned the following values:

                  "<shadow>/<curve type>/<shape specification>"

               An example of such a string is

                  "UMBRAL/TANGENT/DSK/UNPRIORITIZED"

               In the `method' string

                  <shadow> may be either of the strings

                     "UMBRAL"    indicates the terminator is the
                                 boundary of the portion of the surface
                                 that receives no light from the
                                 illumination source. The shape of the
                                 source is modeled as a sphere. See the
                                 -Particulars section below for details.

                     "PENUMBRAL" indicates the terminator is the
                                 boundary of the portion of the surface
                                 that receives all possible light from
                                 the illumination source. The shape of
                                 the source is modeled as a sphere.

                                 The penumbral terminator bounds the
                                 portion of the surface that is not
                                 subject to self-occultation of light
                                 from the illumination source. Given
                                 that the light source is modeled as a
                                 sphere, from any target surface point
                                 nearer to the source than the
                                 penumbral terminator, the source
                                 appears to be a lit disc. See the
                                 -Particulars section below for details.


                  <curve type> may be either of the strings

                     "TANGENT"   for topographic (DSK) target models
                                 indicates that a terminator point is
                                 defined as the point of tangency, on
                                 the surface represented by the
                                 specified data, of a line also tangent
                                 to the illumination source.

                                 For ellipsoidal target models, a
                                 terminator point is a point of
                                 tangency of a plane that is also
                                 tangent to the illumination source.
                                 See the -Particulars section below for
                                 details.

                                 Terminator points are generated within
                                 a specified set of "cutting"
                                 half-planes that have as an edge the
                                 line containing the illumination
                                 source center-target center vector.
                                 Multiple terminator points may be
                                 found within a given half-plane, if
                                 the target body shape allows for this.

                                 This is the highest-accuracy method
                                 supported by this routine. It
                                 generally executes much more slowly
                                 than the GUIDED method described
                                 below.

                     "GUIDED"    indicates that terminator points are
                                 "guided" so as to lie on rays
                                 emanating from the target body's
                                 center and passing through the
                                 terminator on the target body's
                                 reference ellipsoid. The terminator
                                 points are constrained to lie on the
                                 target body's surface. As with the
                                 "TANGENT" method (see above), cutting
                                 half-planes are used to generate
                                 terminator points.

                                 The GUIDED method produces a unique
                                 terminator point for each cutting
                                 half-plane. If multiple terminator
                                 point candidates lie in a given
                                 cutting half-plane, the outermost one
                                 is chosen.

                                 This method may be used only with the
                                 CENTER aberration correction locus
                                 (see the description of `corloc' below).

                                 Terminator points generated by this
                                 method are approximations; they are
                                 generally not true ray-surface tangent
                                 points. However, these approximations
                                 can be generated much more quickly
                                 than tangent points.


                  <shape specification> may be either of the strings

                     "DSK/UNPRIORITIZED[/SURFACES = <surface list>]"

                        The DSK option indicates that terminator point
                        computation is to use topographic data provided
                        by DSK files (abbreviated as "DSK data" below)
                        to model the surface of the target body.

                        The surface list specification is optional. The
                        syntax of the list is

                           <surface 1> [, <surface 2>...]

                        If present, it indicates that data only for the
                        listed surfaces are to be used; however, data
                        need not be available for all surfaces in the
                        list. If the list is absent, loaded DSK data
                        for any surface associated with the target body
                        are used.

                        The surface list may contain surface names or
                        surface ID codes. Names containing blanks must
                        be delimited by double quotes, for example

                           "SURFACES = \"Mars MEGDR 128 PIXEL/DEG\""

                        If multiple surfaces are specified, their names
                        or IDs must be separated by commas.

                        See the -Particulars section below for details
                        concerning use of DSK data.


                     "ELLIPSOID"

                        The ELLIPSOID shape option generates terminator
                        points on the target body's reference
                        ellipsoid. When the ELLIPSOID shape is
                        selected, The TANGENT curve option may be used
                        with any aberration correction locus, while the
                        GUIDED option may be used only with the CENTER
                        locus (see the description of `corloc' below).

                        When the locus is set to "CENTER", the
                        "TANGENT" and "GUIDED" curve options produce
                        the same results.

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


   ilusrc      is the name of the illumination source. This source
               may be any ephemeris object. Case, blanks, and
               numeric values are treated in the same way as for the
               input `target'.

               The shape of the illumination source is considered
               to be spherical. The radius of the sphere is the
               largest radius of the source's reference ellipsoid.


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

               The output terminator points in the array `points' and
               the output observer-terminator vectors in the array
               `trmvcs' are expressed relative to this reference
               frame.


   abcorr      indicates the aberration corrections to be applied
               when computing the target's position and orientation.
               Corrections are applied at the location specified by
               the aberration correction locus argument `corloc',
               which is described below.

               For remote sensing applications, where apparent
               terminator points seen by the observer are desired,
               normally either of the corrections

                  "LT+S"
                  "CN+S"

               should be used. These and the other supported options
               are described below. `abcorr' may be any of the
               following:

                  "NONE"     Apply no correction. Return the
                             geometric terminator points on the
                             target body.

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
                             "LT" option uses one iteration.

                             Both the target position as seen by the
                             observer, and rotation of the target
                             body, are corrected for light time. The
                             position of the illumination source as
                             seen from the target is corrected as
                             well.

                  "LT+S"     Correct for one-way light time and
                             stellar aberration using a Newtonian
                             formulation. This option modifies the
                             locus obtained with the "LT" option to
                             account for the observer's velocity
                             relative to the solar system
                             barycenter. These corrections yield
                             points on the apparent terminator.

                  "CN"       Converged Newtonian light time
                             correction. In solving the light time
                             equation, the "CN" correction iterates
                             until the solution converges. Both the
                             position and rotation of the target
                             body are corrected for light time. The
                             position of the illumination source as
                             seen from the target is corrected as
                             well.

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
                      than for the ELLIPSOID TERMINATOR option.

                  "ELLIPSOID TERMINATOR"

                      Light time and stellar aberration corrections
                      are applied to individual terminator points on
                      the reference ellipsoid. For a terminator
                      point on the surface described by topographic
                      data, lying in a specified cutting half-plane,
                      the unique reference ellipsoid terminator
                      point in the same half-plane is used as the
                      locus of the aberration corrections.

                      This choice is appropriate for large target
                      objects for which the light time from the
                      terminator to the observer is significantly
                      different from the light time from the target
                      center to the observer.

                      Because aberration corrections are repeated
                      for individual terminator points,
                      computational speed for this option is
                      relatively slow.


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
               half-planes in which terminator points are to be
               found. Each cutting half-plane has as its edge the
               line containing the illumination source center-target
               center vector; the first half-plane contains `refvec'.

               `refvec' is expressed in the body-fixed reference frame
               designated by `fixref'.

               `rolstp' is an angular step by which to roll the cutting
               half-planes about the target-illumination source vector,
               which we'll call the "axis." The ith half-plane is
               rotated from `refvec' about the axis in the
               counter-clockwise direction by i*rolstp. Units are
               radians. `rolstp' should be set to

                  2*pi/ncuts

               to generate an approximately uniform distribution of
               points along the terminator.

               `ncuts' is the number of cutting half-planes used to
               find terminator points; the angular positions of
               consecutive half-planes increase in the positive
               (counterclockwise) sense about the axis and are
               distributed roughly equally about that vector: each
               half-plane has angular separation of approximately

                  `rolstp' radians

               from each of its neighbors. When the aberration
               correction locus is set to "CENTER", the angular
               separation is the value above, up to round-off.
               When the locus is "TANGENT", the separations are
               less uniform due to differences in the aberration
               corrections used for the respective terminator points.


   schstp,
   soltol      are used only for DSK-based surfaces. These inputs
               are, respectively, the search angular step size and
               solution convergence tolerance used to find tangent
               rays and associated terminator points within each
               cutting half plane. These values are used when the
               `method' argument includes the TANGENT option. In this
               case, terminator points are found by a two-step
               search process:

                  1) Bracketing: starting with a direction having
                     sufficiently small angular separation from the
                     axis, rays emanating from the surface of the
                     illumination source are generated within the
                     half-plane at successively greater angular
                     separations from the axis, where the increment
                     of angular separation is `schstp'. The rays are
                     tested for intersection with the target
                     surface. When a transition from
                     non-intersection to intersection is found, the
                     angular separation of a tangent ray has been
                     bracketed.

                  2) Root finding: each time a tangent ray is
                     bracketed, a search is done to find the angular
                     separation from the starting direction at which
                     a tangent ray exists. The search terminates
                     when successive rays are separated by no more
                     than `soltol'. When the search converges, the
                     last ray-surface intersection point found in
                     the convergence process is considered to be a
                     terminator point.


               `schstp' and `soltol' have units of radians.

               Target bodies with simple surfaces---for example,
               convex shapes---will have a single terminator point
               within each cutting half-plane. For such surfaces,
               `schstp' can be set large enough so that only one
               bracketing step is taken. A value greater than pi,
               for example 4.0, is recommended.

               Target bodies with complex surfaces can have
               multiple terminator points within a given cutting
               half-plane. To find all terminator points, `schstp'
               must be set to a value smaller than the angular
               separation of any two terminator points in any
               cutting half-plane, where the vertex of the angle is
               near a point on the surface of the illumination
               source. `schstp' must not be too small, or the search
               will be excessively slow.

               For both kinds of surfaces, `soltol' must be chosen so
               that the results will have the desired precision.
               Note that the choice of `soltol' required to meet a
               specified bound on terminator point height errors
               depends on the illumination source-target distance.


   maxn        is the maximum number of terminator points that can
               be stored in the output array `points'.

-Detailed_Output

   npts        is an array of counts of terminator points within
               the specified set of cutting half-planes. The Ith
               element of `npts' is the terminator point count in the
               Ith half-plane. `npts' should be declared with length
               at least `ncuts'.


   points      is an array containing the terminator points found
               by this routine. Terminator points are ordered by
               the indices of the half-planes in which they're
               found. The terminator points in a given half-plane
               are ordered by decreasing angular separation from
               the illumination source-target direction; the
               outermost terminator point in a given half-plane is
               the first of that set.

               The terminator points for the half-plane containing
               `refvec' occupy array elements

                   points[ 0         ][0]          through
                   points[ npts[0]-1 ][2]

               Terminator points for the second half plane occupy
               elements

                  points[ npts[0]           ][0]  through
                  points[ npts[0]+npts[1]-1 ][2]

               and so on.

               `points' should be declared with dimensions

                  [maxn][3]

               Terminator points are expressed in the reference
               frame designated by `fixref'. For each terminator
               point, the orientation of the frame is evaluated at
               the epoch corresponding to the terminator point; the
               epoch is provided in the output array `epochs'
               (described below).

               Units of the terminator points are km.


   epochs      is an array of epochs associated with the terminator
               points, accounting for light time if aberration
               corrections are used. `epochs' contains one element
               for each terminator point. `epochs' should be declared
               with length

                  maxn

               The element

                  epochs[i]

               is associated with the terminator point

                  points[i][j], j = 0 to 2

               If `corloc' is set to "CENTER", all values of `epochs'
               will be the epoch associated with the target body
               center. That is, if aberration corrections are used,
               and if `lt' is the one-way light time from the target
               center to the observer, the elements of `epochs' will
               all be set to

                  et - lt

               If `corloc' is set to "ELLIPSOID TERMINATOR", all
               values of `epochs' for the terminator points in a
               given half plane will be those for the reference
               ellipsoid terminator point in that half plane. That
               is, if aberration corrections are used, and if lt(i)
               is the one-way light time to the observer from the
               reference ellipsoid terminator point in the ith half
               plane, the elements of `epochs' for that half plane
               will all be set to

                  et - lt[i]


   trmvcs      is an array of vectors connecting the observer to
               the terminator points. The terminator vectors are
               expressed in the frame designated by `fixref'. For the
               Ith vector, the orientation of the frame is
               evaluated at the Ith epoch provided in the output
               array `epochs' (described above).

               `trmvcs' should be declared with dimensions

                  [maxn][3]

               The elements

                  trmvcs[i][j], j = 0 to 2

               are associated with the terminator point

                  points[i][j], j = 0 to 2

               Units of the terminator vectors are km.

-Parameters

   None.

-Exceptions

   1)  If the specified aberration correction is unrecognized, an
       error is signaled by a routine in the call tree of this
       routine.

   2)  If transmission corrections are commanded, the error
       SPICE(INVALIDOPTION) is signaled by a routine in the call tree
       of this routine.

   3)  If either the target or observer input strings cannot be
       converted to an integer ID code, the error
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

   7)  If the input argument `method' is not recognized, the error
       SPICE(INVALIDMETHOD) is signaled by either this routine or a
       routine in the call tree of this routine.

   8)  If `method' contains an invalid terminator type, the error
       SPICE(INVALIDTERMTYPE) is signaled by a routine in the call
       tree of this routine.

   9)  If the target and observer have distinct identities but are
       at the same location, the error SPICE(NOSEPARATION) is
       signaled by a routine in the call tree of this routine.

   10) If insufficient ephemeris data have been loaded prior to
       calling termpt_c, an error is signaled by a routine in
       the call tree of this routine. When light time correction is
       used, sufficient ephemeris data must be available to
       propagate the states of both observer and target to the solar
       system barycenter.

   11) If the computation method requires an ellipsoidal target shape
       and triaxial radii of the target body have not been loaded
       into the kernel pool prior to calling termpt_c, an error is
       signaled by a routine in the call tree of this routine.

       When the target shape is modeled by topographic data, radii
       of the reference triaxial ellipsoid are still required if
       the aberration correction locus is ELLIPSOID TERMINATOR or if
       the terminator point generation method is GUIDED.

   12) If the target body's shape is modeled as an ellipsoid, and if
       any of the radii of the target body are non-positive, an error
       is signaled by a routine in the call tree of this routine. The
       target must be an extended body.

   13) If PCK data specifying the target body-fixed frame orientation
       have not been loaded prior to calling termpt_c, an error is
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

   18) If the GUIDED terminator type is used with the ELLIPSOID
       TERMINATOR aberration correction locus, the error
       SPICE(BADTERMLOCUSMIX) is signaled by a routine in the call
       tree of this routine.

   19) If the reference vector `refvec' is the zero vector, the error
       SPICE(ZEROVECTOR) is signaled by a routine in the call tree of
       this routine.

   20) If the reference vector `refvec' and the observer target vector
       are linearly dependent, the error SPICE(DEGENERATECASE) is
       signaled by a routine in the call tree of this routine.

   21) If the terminator points cannot all be stored in the output
       `points' array, the error SPICE(OUTOFROOM) is signaled by a
       routine in the call tree of this routine.

   22) If `ncuts' is greater than 1, the roll step `rolstp' must be
       positive. Otherwise, the error SPICE(INVALIDROLLSTEP) is
       signaled by a routine in the call tree of this routine.

   23) If any of the `method', `ilusrc', `target', `fixref',
       `abcorr', `corloc' or `obsrvr' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   24) If any of the `method', `ilusrc', `target', `fixref',
       `abcorr', `corloc' or `obsrvr' input strings has zero length,
       the error SPICE(EMPTYSTRING) is signaled.

-Files

   Appropriate kernels must be loaded by the calling program before
   this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for the target, observer, and
      illumination source must be loaded. If aberration
      corrections are used, the states of target and observer
      relative to the solar system barycenter must be calculable
      from the available ephemeris data. Typically ephemeris data
      are made available by loading one or more SPK files via
      furnsh_c.

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
           modeled by DSK data but one or both of the GUIDED
           terminator definition method or the ELLIPSOID
           TERMINATOR aberration correction locus are selected.

        DSK data:

           If the target shape is modeled by DSK data, DSK files
           containing topographic data for the target body must be
           loaded. If a surface list is specified, data for at
           least one of the listed surfaces must be loaded.

   -  Shape data for the illumination source:

        PCK data:

           Triaxial radii for the illumination source must be
           loaded into the kernel pool. Typically this is done by
           loading a text PCK file via furnsh_c.

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

   Terminator definition
   =====================

   The definitions of terminators used by this routine vary
   depending on the target surface model.

   In all cases, the surface of the illumination source is
   modeled as a sphere.


   Ellipsoidal target surface model
   --------------------------------

   The umbral terminator is the boundary of the set of target
   surface points at which the illumination source is completely
   below the local tangent plane: the entire illumination source is
   below the horizon as seen from any surface point on the far side,
   relative to the source, of the umbral terminator. At an umbral
   terminator point, the target surface tangent plane containing
   that point is tangent to the surface of the light source as well,
   and the outward normal vectors at the two points of tangency are
   parallel.

   The penumbral terminator is the boundary of the set of target
   surface points at which the illumination source is completely
   above the local tangent plane: the entire illumination source is
   above the horizon as seen from any surface point on the near
   side, relative to the source, of the penumbral terminator. At a
   penumbral terminator point, the target surface tangent plane
   containing that point is tangent to the surface of the light
   source as well, and the outward normal vectors at the two points
   of tangency are anti-parallel.


   Topographic target surface model (DSK case)
   -------------------------------------------

   The concept of a plane tangent to both a topographic target
   surface and an illumination source is problematic. If the target
   tangent point is required to lie in a given cutting half-plane
   bounded by the line containing the target-source vector, the
   desired plane may not exist. In general, planes tangent to both
   the illumination source and the target will rest upon the high
   points of the target surface.

   For topographic target surface models, this routine uses a
   modified terminator definition: terminator points are target
   surface points at which a line is tangent to both the target and
   the illumination source. The line is constrained to lie in the
   plane containing the specified cutting half-plane. The concepts
   of umbral and penumbral terminators still apply. For umbral
   terminator points, the common tangent line does not cross the
   target-source line; for penumbral points, it does.

   Note that for ellipsoids, the terminator definitions based on
   tangent lines are not equivalent to the definitions based on
   tangent planes. Typically, a plane tangent to the target
   ellipsoid at a point found by the method described above will not
   be tangent to the illumination source: it will be rotated about
   the common tangent line and "cut into" the sphere representing
   the light source. This implies that some of the source will be
   visible at umbral terminator points and some will be blocked at
   penumbral terminator points: both umbral and penumbral terminator
   points found by this method will lie in a region bounded by the
   true terminators.

   The two definitions are equivalent for spherical targets.


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

      UMBRAL/TANGENT/DSK/UNPRIORITIZED/<surface list>
      DSK/UMBRAL/TANGENT/<surface list>/UNPRIORITIZED
      UNPRIORITIZED/<surface list>/DSK/TANGENT/UMBRAL

   The simplest form of the `method' argument specifying use of
   DSK data is one that lacks a surface list, for example:

      "PENUMBRAL/TANGENT/DSK/UNPRIORITIZED"
      "UMBRAL/GUIDED/DSK/UNPRIORITIZED"

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

     "UMBRAL/TANGENT/DSK/UNPRIORITIZED/SURFACES= \"Mars MEGDR 64 PIXEL/DEG\",3"

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Find apparent terminator points on Phobos as seen from Mars.
      Use the "umbral" shadow definition.

      Due to Phobos' irregular shape, the TANGENT terminator point
      definition will be used. It suffices to compute light time and
      stellar aberration corrections for the center of Phobos, so
      the CENTER aberration correction locus will be used. Use
      converged Newtonian light time and stellar aberration
      corrections in order to model the apparent position and
      orientation of Phobos.

      For comparison, compute terminator points using both ellipsoid
      and topographic shape models.

      Use the target body-fixed +Z axis as the reference direction
      for generating cutting half-planes. This choice enables the
      user to see whether the first terminator point is near the
      target's north pole.

      For each option, use just three cutting half-planes in order
      to keep the volume of output manageable. In most applications,
      the number of cuts and the number of resulting terminator
      points would be much greater.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: termpt_ex1.tm

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
         Program termpt_ex1

         Find terminator points on Phobos as seen from Mars.

         Compute terminator points using the tangent
         definition, using the "umbral" shadow type.
         The sun is the illumination source. Perform
         aberration corrections for the target center.
         Use both ellipsoid and DSK shape models.
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define META            "termpt_ex1.tm"
         #define MTHLEN          51
         #define NMETH            2
         #define MAXN         10000

         /.
         Local variables
         ./
         SpiceChar             * abcorr;
         SpiceChar             * corloc;
         SpiceChar             * fixref;
         SpiceChar             * ilusrc;
         SpiceChar             * obsrvr;
         SpiceChar             * target;

         SpiceChar               method [NMETH][MTHLEN] =

                           { "UMBRAL/TANGENT/ELLIPSOID",
                             "UMBRAL/TANGENT/DSK/UNPRIORITIZED" };

         SpiceDouble             delrol;
         SpiceDouble             dist;
         SpiceDouble             et;
         SpiceDouble             lt;
         SpiceDouble             points  [MAXN][3];
         SpiceDouble             pos     [3];
         SpiceDouble             roll;
         SpiceDouble             schstp;
         SpiceDouble             soltol;
         SpiceDouble             trmvcs  [MAXN][3];
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
         Set illumination source, target, observer,
         and target body-fixed, body-centered reference frame.
         ./
         ilusrc = "SUN";
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
         Compute a set of terminator points using light
         time and stellar aberration corrections. Use
         both ellipsoid and DSK shape models. Use an
         angular step size corresponding to a height of
         about 100 meters to ensure we don't miss the
         terminator. Set the convergence tolerance to limit
         the height convergence error to about 1 meter.
         Compute 3 terminator points for each computation
         method.

         Get the approximate light source-target distance
         at ET. We'll ignore the observer-target light
         time for this approximation.
         ./

         spkpos_c ( ilusrc, et, "J2000", abcorr,
                    target, pos, &lt            );

         dist   = vnorm_c( pos );

         schstp = 1.0e-1 / dist;
         soltol = 1.0e-3 / dist;
         ncuts  = 3;

         printf ( "\n"
                  "Light source:   %s\n"
                  "Observer:       %s\n"
                  "Target:         %s\n"
                  "Frame:          %s\n"
                  "\n"
                  "Number of cuts: %d\n",
                  ilusrc,
                  obsrvr,
                  target,
                  fixref,
                  (int)ncuts            );

         delrol = twopi_c() / ncuts;


         for ( i = 0;  i < NMETH;  i++ )
         {
            termpt_c ( method[i], ilusrc, target, et,
                       fixref,    abcorr, corloc, obsrvr,
                       z,         delrol, ncuts,  schstp,
                       soltol,    MAXN,   npts,   points,
                       trgeps,    trmvcs                  );
            /.
            Write the results.
            ./
            printf ( "\n"
                     "Computation method = %s\n"
                     "Locus              = %s\n"
                     "\n",
                     method[i],
                     corloc                     );

            start = 0;

            for ( j = 0;  j < ncuts;  j++ )
            {
               roll = j * delrol;

               printf ( "\n"
                        "  Roll angle (deg) = %21.9f\n"
                        "     Target epoch  = %21.9f\n"
                        "     Number of terminator points at "
                        "this roll angle: %d\n",
                        roll * dpr_c(),
                        trgeps[j],
                        npts[j]                            );

               printf ( "      Terminator points:\n" );

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


      Light source:   SUN
      Observer:       MARS
      Target:         PHOBOS
      Frame:          IAU_PHOBOS

      Number of cuts: 3

      Computation method = UMBRAL/TANGENT/ELLIPSOID
      Locus              = CENTER


        Roll angle (deg) =           0.000000000
           Target epoch  =   271684865.152078211
           Number of terminator points at this roll angle: 1
            Terminator points:
                2.040498332          5.012722925          8.047281838

        Roll angle (deg) =         120.000000000
           Target epoch  =   271684865.152078211
           Number of terminator points at this roll angle: 1
            Terminator points:
              -11.058054707          0.167672089         -4.782740292

        Roll angle (deg) =         240.000000000
           Target epoch  =   271684865.152078211
           Number of terminator points at this roll angle: 1
            Terminator points:
                8.195238564         -6.093889437         -5.122310498

      Computation method = UMBRAL/TANGENT/DSK/UNPRIORITIZED
      Locus              = CENTER


        Roll angle (deg) =           0.000000000
           Target epoch  =   271684865.152078211
           Number of terminator points at this roll angle: 1
            Terminator points:
                1.626396122          3.995432317          8.853689531

        Roll angle (deg) =         120.000000000
           Target epoch  =   271684865.152078211
           Number of terminator points at this roll angle: 1
            Terminator points:
              -11.186659739         -0.142366278         -4.646137201

        Roll angle (deg) =         240.000000000
           Target epoch  =   271684865.152078211
           Number of terminator points at this roll angle: 1
            Terminator points:
                9.338447077         -6.091352469         -5.960849305


   2) Find apparent terminator points on Mars as seen from the
      earth.

      Use both the "umbral" and "penumbral" shadow definitions. Use
      only ellipsoid shape models for easier comparison. Find
      distances between corresponding terminator points on the
      umbral and penumbral terminators.

      Use the ELLIPSOID TERMINATOR aberration correction locus
      in order to perform separate aberration corrections for
      each terminator point. Because of the large size of Mars,
      corrections for the target center are less accurate.

      For each option, use just three cutting half-planes, in order
      to keep the volume of output manageable. In most applications,
      the number of cuts and the number of resulting terminator
      points would be much greater.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: termpt_ex2.tm

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
         Program termpt_ex2

         Find terminator points on Mars as seen from the
         earth.

         Use only ellipsoid shape models. Use the
         ELLIPSOID TERMINATOR aberration correction
         locus.

         Use both UMBRAL and PENUMBRAL shadow definitions.
         Compute the distances between corresponding
         umbral and penumbral terminator points.

         Check terminator points by computing solar
         incidence angles at each point.
      ./



      #include <stdio.h>
      #include <math.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define CORLEN          21
         #define META            "termpt_ex2.tm"
         #define MTHLEN          51
         #define NMETH            2
         #define MAXN         10000

         /.
         Local variables
         ./
         SpiceChar             * abcorr;

         SpiceChar               corloc [NMETH][CORLEN] =

                           { "ELLIPSOID TERMINATOR",
                             "ELLIPSOID TERMINATOR" };

         SpiceChar             * fixref;

         SpiceChar               ilumth [NMETH][MTHLEN] =

                           { "ELLIPSOID",
                             "ELLIPSOID" };

         SpiceChar             * ilusrc;
         SpiceChar             * obsrvr;
         SpiceChar             * target;


         SpiceChar               method [NMETH][MTHLEN] =

                        { "UMBRAL/TANGENT/ELLIPSOID",
                          "PENUMBRAL/TANGENT/ELLIPSOID" };

         SpiceDouble             adjang;
         SpiceDouble             alt;
         SpiceDouble             angsrc;
         SpiceDouble             delrol;
         SpiceDouble             dist;
         SpiceDouble             emissn;
         SpiceDouble             et;
         SpiceDouble             f;
         SpiceDouble             ilupos  [3];
         SpiceDouble             lat;
         SpiceDouble             lon;
         SpiceDouble             lt;
         SpiceDouble             phase;
         SpiceDouble             points  [MAXN][3];
         SpiceDouble             svpnts  [MAXN][3];
         SpiceDouble             tptilu  [3];
         SpiceDouble             radii   [3];
         SpiceDouble             re;
         SpiceDouble             roll;
         SpiceDouble             rp;
         SpiceDouble             schstp;
         SpiceDouble             solar;
         SpiceDouble             soltol;
         SpiceDouble             srcrad  [3];
         SpiceDouble             srfvec  [3];
         SpiceDouble             trmvcs  [MAXN][3];
         SpiceDouble             trgepc;
         SpiceDouble             trgeps  [MAXN];
         SpiceDouble             z       [3] = { 0.0, 0.0, 1.0 };

         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                k;
         SpiceInt                m;
         SpiceInt                n;
         SpiceInt                ncuts;
         SpiceInt                npts    [MAXN];
         SpiceInt                start;

         /.
         Load kernel files via the meta-kernel.
         ./
         furnsh_c ( META );

         /.
         Set illumination source, target, observer,
         and target body-fixed, body-centered reference frame.
         ./
         ilusrc = "SUN";
         obsrvr = "EARTH";
         target = "MARS";
         fixref = "IAU_MARS";

         /.
         Set the aberration correction.
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
         Get the radii of the illumination source as well.
         We'll use these radii to compute the angular radius
         of the source as seen from the terminator points.
         ./
         bodvrd_c ( ilusrc, "RADII", 3, &n, srcrad );

         /.
         Compute a set of terminator points using light time and
         stellar aberration corrections. Use both ellipsoid
         and DSK shape models.

         Get the approximate light source-target distance
         at ET. We'll ignore the observer-target light
         time for this approximation.
         ./
         spkpos_c ( ilusrc, et,     fixref, abcorr,
                    target, ilupos, &lt            );

         dist = vnorm_c( ilupos );

         /.
         Set the angular step size so that a single step will
         be taken in the root bracketing process; that's all
         that is needed since we don't expect to have multiple
         terminator points in any cutting half-plane.
         ./
         schstp = 4.0;

         /.
         Set the convergence tolerance to minimize the
         height error. We can't achieve the precision
         suggested by the formula because the sun-Mars
         distance is about 2.4e8 km. Compute 3 terminator
         points for each computation method.
         ./
         soltol = 1.e-7/dist;

         /.
         Set the number of cutting half-planes and roll step.
         ./
         ncuts  = 3;
         delrol = twopi_c() / ncuts;


         printf ( "\n"
                  "Light source:   %s\n"
                  "Observer:       %s\n"
                  "Target:         %s\n"
                  "Frame:          %s\n"
                  "\n"
                  "Number of cuts: %d\n",
                  ilusrc,
                  obsrvr,
                  target,
                  fixref,
                  (int)ncuts            );

         delrol = twopi_c() / ncuts;


         for ( i = 0;  i < NMETH;  i++ )
         {
            termpt_c ( method[i], ilusrc, target, et,
                       fixref,    abcorr, corloc, obsrvr,
                       z,         delrol, ncuts,  schstp,
                       soltol,    MAXN,   npts,   points,
                       trgeps,    trmvcs                  );
            /.
            Write the results.
            ./
            printf ( "\n\n"
                     "Computation method = %s\n"
                     "Locus              = %s\n",
                     method[i],
                     corloc[i]                  );

            start = 0;

            for ( j = 0;  j < ncuts;  j++ )
            {
               roll = j * delrol;

               printf ( "\n"
                        "   Roll angle (deg) = %21.9f\n"
                        "    Target epoch    = %21.9f\n"
                        "    Number of terminator points at "
                        "this roll angle: %d\n",
                        roll * dpr_c(),
                        trgeps[j],
                        npts[j]                            );

               for ( k = 0;  k < npts[j];  k++ )
               {
                  printf ( "    Terminator point planetodetic "
                           "coordinates:\n" );

                  m = k+start;

                  recgeo_c ( points[m], re, f,
                             &lon,          &lat, &alt );

                  printf ( "      Longitude        (deg): %20.9f\n"
                           "      Latitude         (deg): %20.9f\n"
                           "      Altitude          (km): %20.9f\n",
                           lon * dpr_c(),
                           lat * dpr_c(),
                           alt                                     );

                  /.
                  Get illumination angles for this terminator point.
                  ./

                  illumg_c ( ilumth[i], target,  ilusrc, et,
                             fixref,    abcorr,  obsrvr,
                             points[m], &trgepc, srfvec,
                             &phase,    &solar,  &emissn    );

                  printf ( "      Incidence angle  (deg): "
                           "%20.9f\n",   solar * dpr_c()    );


                  /.
                  Adjust the incidence angle for the angular
                  radius of the illumination source. Use the
                  epoch associated with the terminator point
                  for this lookup.
                  ./
                  spkpos_c ( ilusrc, trgeps[m], fixref,
                             abcorr, target,    tptilu, &lt );

                  dist   = vnorm_c( tptilu );

                  angsrc = asin ( maxd_c( 3, srcrad[0],
                                             srcrad[1],
                                             srcrad[2] ) / dist );

                  if ( i == 0 )
                  {
                     /.
                     For points on the umbral terminator,
                     the ellipsoid outward normal is tilted
                     away from the terminator-source center
                     direction by the angular radius of the
                     source. Subtract this radius from the
                     illumination incidence angle to get the
                     angle between the local normal and the
                     direction to the corresponding tangent
                     point on the source.
                     ./
                     adjang = solar - angsrc;
                  }
                  else
                  {
                     /.
                     For the penumbral case, the outward
                     normal is tilted toward the illumination
                     source by the angular radius of the
                     source. Adjust the illumination
                     incidence angle for this.
                     ./
                     adjang = solar + angsrc;
                  }

                  printf ( "      Adjusted angle   (deg): "
                           "%20.9f\n",   adjang * dpr_c() );

                  if ( i == 0 )
                  {
                     /.
                     Save terminator points for comparison.
                     ./
                     vequ_c ( points[m], svpnts[m] );
                  }
                  else
                  {
                     /.
                     Compare terminator points with last
                     saved values.
                     ./
                     dist = vdist_c( points[m], svpnts[m] );

                     printf ( "      Distance offset  (km):  "
                              "%20.9f\n",   dist             );
                  }
               }
               start += npts[j];
            }
         }
         printf ( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Light source:   SUN
      Observer:       EARTH
      Target:         MARS
      Frame:          IAU_MARS

      Number of cuts: 3


      Computation method = UMBRAL/TANGENT/ELLIPSOID
      Locus              = ELLIPSOID TERMINATOR

         Roll angle (deg) =           0.000000000
          Target epoch    =   271683700.369686902
          Number of terminator points at this roll angle: 1
          Terminator point planetodetic coordinates:
            Longitude        (deg):          4.189318082
            Latitude         (deg):         66.416132677
            Altitude          (km):          0.000000000
            Incidence angle  (deg):         90.163842885
            Adjusted angle   (deg):         89.999999980

         Roll angle (deg) =         120.000000000
          Target epoch    =   271683700.372003794
          Number of terminator points at this roll angle: 1
          Terminator point planetodetic coordinates:
            Longitude        (deg):        107.074551917
            Latitude         (deg):        -27.604435701
            Altitude          (km):          0.000000000
            Incidence angle  (deg):         90.163842793
            Adjusted angle   (deg):         89.999999888

         Roll angle (deg) =         240.000000000
          Target epoch    =   271683700.364983618
          Number of terminator points at this roll angle: 1
          Terminator point planetodetic coordinates:
            Longitude        (deg):        -98.695906077
            Latitude         (deg):        -27.604435700
            Altitude          (km):          0.000000000
            Incidence angle  (deg):         90.163843001
            Adjusted angle   (deg):         90.000000096


      Computation method = PENUMBRAL/TANGENT/ELLIPSOID
      Locus              = ELLIPSOID TERMINATOR

         Roll angle (deg) =           0.000000000
          Target epoch    =   271683700.369747400
          Number of terminator points at this roll angle: 1
          Terminator point planetodetic coordinates:
            Longitude        (deg):          4.189317837
            Latitude         (deg):         66.743818467
            Altitude          (km):          0.000000000
            Incidence angle  (deg):         89.836157094
            Adjusted angle   (deg):         89.999999999
            Distance offset  (km):          19.483590936

         Roll angle (deg) =         120.000000000
          Target epoch    =   271683700.372064054
          Number of terminator points at this roll angle: 1
          Terminator point planetodetic coordinates:
            Longitude        (deg):        107.404259674
            Latitude         (deg):        -27.456458359
            Altitude          (km):          0.000000000
            Incidence angle  (deg):         89.836157182
            Adjusted angle   (deg):         90.000000087
            Distance offset  (km):          19.411414247

         Roll angle (deg) =         240.000000000
          Target epoch    =   271683700.365043879
          Number of terminator points at this roll angle: 1
          Terminator point planetodetic coordinates:
            Longitude        (deg):        -99.025614323
            Latitude         (deg):        -27.456458357
            Altitude          (km):          0.000000000
            Incidence angle  (deg):         89.836156972
            Adjusted angle   (deg):         89.999999877
            Distance offset  (km):          19.411437239


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Added missing exceptions.

   -CSPICE Version 1.0.0, 05-APR-2017 (NJB)

-Index_Entries

   find terminator points on target body

-&
*/

{ /* Begin termpt_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "termpt_c" );


   /*
   Check the input string arguments:

      method
      ilusrc
      target
      fixref
      abcorr
      corloc
      obsrvr

   Make sure each pointer is non-null and each string contains
   at least one data character: that is, one character
   preceding the null terminator.
   */
   CHKFSTR ( CHK_STANDARD, "termpt_c", method );
   CHKFSTR ( CHK_STANDARD, "termpt_c", ilusrc );
   CHKFSTR ( CHK_STANDARD, "termpt_c", target );
   CHKFSTR ( CHK_STANDARD, "termpt_c", fixref );
   CHKFSTR ( CHK_STANDARD, "termpt_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "termpt_c", corloc );
   CHKFSTR ( CHK_STANDARD, "termpt_c", obsrvr );


   /*
   Call the f2c'd SPICELIB function.
   */

   termpt_ ( ( char         * ) method,
             ( char         * ) ilusrc,
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
             ( doublereal   * ) trmvcs,
             ( ftnlen         ) strlen(method),
             ( ftnlen         ) strlen(ilusrc),
             ( ftnlen         ) strlen(target),
             ( ftnlen         ) strlen(fixref),
             ( ftnlen         ) strlen(abcorr),
             ( ftnlen         ) strlen(corloc),
             ( ftnlen         ) strlen(obsrvr)  );


   chkout_c ( "termpt_c" );

} /* End termpt_c */
