/*

-Procedure gfilum_c ( GF, illumination angle search )

-Abstract

   Return the time window over which a specified constraint on
   the observed phase, solar incidence, or emission angle at
   a specified target body surface point is met.

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

   GF
   FRAMES
   NAIF_IDS
   PCK
   SPK
   TIME

-Keywords

   ANGLE
   EPHEMERIS
   ILLUMINATION
   LIGHTING
   SEARCH

*/

   #include <stdlib.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef gfilum_c


   void gfilum_c ( ConstSpiceChar     * method,
                   ConstSpiceChar     * angtyp,
                   ConstSpiceChar     * target,
                   ConstSpiceChar     * illmn,
                   ConstSpiceChar     * fixref,
                   ConstSpiceChar     * abcorr,
                   ConstSpiceChar     * obsrvr,
                   ConstSpiceDouble     spoint [3],
                   ConstSpiceChar     * relate,
                   SpiceDouble          refval,
                   SpiceDouble          adjust,
                   SpiceDouble          step,
                   SpiceInt             nintvls,
                   SpiceCell          * cnfine,
                   SpiceCell          * result     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   SPICE_GF_CNVTOL
              P   Convergence tolerance.
   SPICE_GF_NWILUM
              P   Number of workspace windows for angle search.
   method     I   Computation method.
   angtyp     I   Type of illumination angle.
   target     I   Name of the target body.
   illmn      I   Name of the illumination source.
   fixref     I   Body-fixed, body-centered target body frame.
   abcorr     I   Aberration correction flag.
   obsrvr     I   Name of the observing body.
   spoint     I   Body-fixed coordinates of a target surface point.
   relate     I   Relational operator.
   refval     I   Reference value.
   adjust     I   Adjustment value for absolute extrema searches.
   step       I   Step size used for locating extrema and roots.
   nintvls    I   Workspace window interval count.
   cnfine    I-O  SPICE window to which the search is confined.
   result     O   SPICE window containing results.

-Detailed_Input

   method      is a short string providing parameters defining the
               computation method to be used. Parameters include, but
               are not limited to, the shape model used to represent the
               surface of the target body.

               The only choice currently supported is

                  "Ellipsoid"        The illumination angle
                                     computation uses a triaxial
                                     ellipsoid to model the surface
                                     of the target body. The
                                     ellipsoid's radii must be
                                     available in the kernel pool.

               Neither case nor whitespaces are significant in `method'.
               For example, the string " eLLipsoid " is valid.

   angtyp      is a string specifying the type of illumination angle for
               which a search is to be performed. The possible values of
               `angtyp' are

                  "PHASE"
                  "INCIDENCE"
                  "EMISSION"

               When the illumination source is the sun, the incidence
               angle is commonly called the "solar incidence angle."

               See the -Particulars section below for a detailed
               description of these angles.

               Neither case nor whitespaces are significant in `angtyp'.
               For example, the string " Incidence " is valid.

   target      is the name of a target body. The point at which the
               illumination angles are defined is located on the surface
               of this body.

               Optionally, you may supply the integer ID code for the
               object as an integer string. For example both "MOON" and
               "301" are legitimate strings that indicate the moon is
               the target body.

               Neither case nor leading and trailing blanks are
               significant in `target'. For example, the string
               " Incidence " is valid. Sequences of embedded blanks
               are treated as a single blank.

   illmn       is the name of the illumination source. This source may
               be any ephemeris object. Case, blanks, and numeric values
               are treated in the same way as for the input `target'.

   fixref      is the name of the body-fixed, body-centered reference
               frame associated with the target body. The input surface
               point `spoint' is expressed relative to this reference
               frame, and this frame is used to define the orientation
               of the target body as a function of time.

               The string `fixref' is case-insensitive, and leading
               and trailing blanks in `fixref' are not significant.

   abcorr      indicates the aberration corrections to be applied to the
               observer-surface point vector, the surface point-
               illumination source vector, and the target body
               orientation to account for one-way light time and stellar
               aberration.

               Any "reception" correction accepted by spkezr_c can be used
               here. See the header of spkezr_c for a detailed description
               of the aberration correction options. For convenience,
               the options are listed below:

                  "NONE"     Apply no correction.

                  "LT"       "Reception" case: correct for
                             one-way light time using a Newtonian
                             formulation.

                  "LT+S"     "Reception" case: correct for
                             one-way light time and stellar
                             aberration using a Newtonian
                             formulation.

                  "CN"       "Reception" case: converged
                             Newtonian light time correction.

                  "CN+S"     "Reception" case: converged
                             Newtonian light time and stellar
                             aberration corrections.

               Case and blanks are not significant in the string `abcorr'.

   obsrvr      is the name of an observing body. Case, blanks, and
               numeric values are treated in the same way as for the
               input `target'.

   spoint      is a surface point on the target body, expressed in
               Cartesian coordinates, relative to the body-fixed target
               frame designated by `fixref'.

               `spoint' need not be visible from the observer's location
               in order for the constraint specified by `relate' and
               `refval' (see descriptions below) to be satisfied.

               The components of `spoint' have units of km.

   relate      is a relational operator used to define a constraint on a
               specified illumination angle. The result window found by
               this routine indicates the time intervals where the
               constraint is satisfied. Supported values of `relate' and
               corresponding meanings are shown below:

                  ">"      The angle is greater than the reference
                           value `refval'.

                  "="      The angle is equal to the reference
                           value `refval'.

                  "<"      The angle is less than the reference
                           value `refval'.


                 "ABSMAX"  The angle is at an absolute maximum.

                 "ABSMIN"  The angle is at an absolute minimum.

                 "LOCMAX"  The angle is at a local maximum.

                 "LOCMIN"  The angle is at a local minimum.

               The caller may indicate that the window of interest is
               the set of time intervals where the angle is within a
               specified separation from an absolute extremum. The
               argument `adjust' (described below) is used to specify this
               separation.

               Local extrema are considered to exist only in the
               interiors of the intervals comprising the confinement
               window: a local extremum cannot exist at a boundary point
               of the confinement window.

               Case is not significant in the string `relate'.

   refval      is the reference value used together with the argument
               `relate' to define an equality or inequality to be
               satisfied by the specified illumination angle. See the
               discussion of `relate' above for further information.

               The units of `refval' are radians.

   adjust      is a parameter used to modify searches for absolute
               extrema: when `relate' is set to "ABSMAX" or "ABSMIN" and
               `adjust' is set to a positive value, gfilum_c will find times
               when the specified illumination angle is within `adjust'
               radians of the specified extreme value.

               If `adjust' is non-zero and a search for an absolute
               minimum is performed, the result window contains time
               intervals when the specified illumination angle has
               values between the absolute minimum `absmin' and
               absmin + adjust radians.

               If `adjust' is non-zero and the search is for an absolute
               maximum, the corresponding angle is between the absolute
               maximum `absmax' and absmax - adjust radians.

               `adjust' is not used for searches for local extrema,
               equality or inequality conditions.

   step        is the step size to be used in the search. `step' must be
               short enough for a search using this step size to locate
               the time intervals where the specified illumination angle
               is monotone increasing or decreasing. However, `step' must
               not be *too* short, or the search will take an
               unreasonable amount of time.

               The choice of `step' affects the completeness but not the
               precision of solutions found by this routine; the
               precision is controlled by the convergence tolerance. See
               the discussion of the parameter SPICE_GF_CNVTOL for details.

               `step' has units of seconds.

   nintvls     is an integer parameter specifying the number of intervals
               that can be accommodated by each of the dynamically allocated
               workspace windows used internally by this routine.

               In many cases, it's not necessary to compute an accurate
               estimate of how many intervals are needed; rather, the user
               can pick a size considerably larger than what's really
               required.

               However, since excessively large arrays can prevent
               applications from compiling, linking, or running properly,
               sometimes `nintvls' must be set according to the actual
               workspace requirement. A rule of thumb for the number of
               intervals needed is

                  nintvls  =  2*n  +  ( m / step )

               where

                  n      is the number of intervals in the confinement
                         window.

                  m      is the measure of the confinement window, in units
                         of seconds.

                  step   is the search step size in seconds.

   cnfine      is a SPICE window that confines the time period over
               which the specified search is conducted. `cnfine' may
               consist of a single interval or a collection of
               intervals.

               The endpoints of the time intervals comprising `cnfine' are
               interpreted as seconds past J2000 TDB.

               In some cases the confinement window can be used to
               greatly reduce the time window that must be searched for
               the desired solution. See the -Particulars section below
               for further discussion.

               See the -Examples section below for a code example that
               shows how to create a confinement window.

               In some cases the observer's state may be computed at
               times outside of `cnfine' by as much as 2 seconds. See
               -Particulars for details.

               `cnfine' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( cnfine, CNFINESZ );

               where CNFINESZ is the maximum capacity of `cnfine'.

-Detailed_Output

   cnfine      is the input confinement window, updated if necessary so the
               control area of its data array indicates the window's size
               and cardinality. The window data are unchanged.

   result      is the SPICE window of intervals, contained within the
               confinement window `cnfine', on which the specified
               constraint is satisfied.

               `result' must be declared and initialized with sufficient
               size to capture the full set of time intervals within the
               search region on which the specified condition is satisfied.

               If `result' is non-empty on input, its contents will be
               discarded before gfilum_c conducts its search.

               The endpoints of the time intervals comprising `result' are
               interpreted as seconds past J2000 TDB.

               If the search is for local extrema, or for absolute
               extrema with `adjust' set to zero, then normally each
               interval of `result' will be a singleton: the left and
               right endpoints of each interval will be identical.

               If no times within the confinement window satisfy the
               search criteria, `result' will be returned with a
               cardinality of zero.

               `result' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( result, RESULTSZ );

               where RESULTSZ is the maximum capacity of `result'.

-Parameters

   SPICE_GF_CNVTOL   is the default convergence tolerance used for finding
                     endpoints of the intervals comprising the result
                     window. SPICE_GF_CNVTOL is also used for finding
                     intermediate results; in particular, SPICE_GF_CNVTOL is
                     used for finding the windows on which the specified
                     illumination angle is increasing or decreasing.
                     SPICE_GF_CNVTOL is used to determine when binary
                     searches for roots should terminate: when a root is
                     bracketed within an interval of length SPICE_GF_CNVTOL,
                     the root is considered to have been found.

                     The accuracy, as opposed to precision, of roots found
                     by this routine depends on the accuracy of the input
                     data. In most cases, the accuracy of solutions will be
                     inferior to their precision.

                     The calling program can reset the convergence
                     tolerance; see the -Particulars section below for
                     further information.

   SPICE_GF_NWILUM   is the number of workspace windows required by
                     this routine.

   See header file SpiceGF.h for declarations and descriptions of
   parameters used throughout the GF subsystem.

-Exceptions

   1)  In order for this routine to produce correct results,
       the step size must be appropriate for the problem at hand.
       Step sizes that are too large may cause this routine to miss
       roots; step sizes that are too small may cause this routine
       to run unacceptably slowly and in some cases, find spurious
       roots.

       This routine does not diagnose invalid step sizes, except that
       if the step size is non-positive, the error SPICE(INVALIDSTEP)
       is signaled by a routine in the call tree of this routine.

   2)  Due to numerical errors, in particular,

          - Truncation error in time values
          - Finite tolerance value
          - Errors in computed geometric quantities

       it is *normal* for the condition of interest to not always be
       satisfied near the endpoints of the intervals comprising the
       result window.

       The result window may need to be contracted slightly by the
       caller to achieve desired results. The SPICE window routine
       wncond_c can be used to contract the result window.

   3)  If the number of intervals `nintvls' is less than 1, the error
       SPICE(VALUEOUTOFRANGE) is signaled.

   4)  If an error (typically cell overflow) occurs while performing
       window arithmetic, the error is signaled by a routine
       in the call tree of this routine.

   5)  If the output SPICE window `result' has size less than 2, the
       error SPICE(INVALIDDIMENSION) is signaled by a routine in the
       call tree of this routine.

   6)  If the output SPICE window `result' has insufficient capacity to
       hold the set of intervals on which the specified illumination
       angle condition is met, an error is signaled by a routine in
       the call tree of this routine.

   7)  If the input target body-fixed frame `fixref' is not
       recognized, an error is signaled by a routine in the call
       tree of this routine. A frame name may fail to be recognized
       because a required frame specification kernel has not been
       loaded; another cause is a misspelling of the frame name.

   8)  If the input frame `fixref' is not centered at the target body,
       an error is signaled by a routine in the call tree of this
       routine.

   9)  If the input argument `method' is not recognized, an error is
       signaled by a routine in the call tree of this routine.

   10) If the illumination angle type `angtyp' is not recognized,
       an error is signaled by a routine in the call tree
       of this routine.

   11) If the relational operator `relate' is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   12) If the aberration correction specifier contains an
       unrecognized value, an error is signaled by a routine in the
       call tree of this routine.

   13) If `adjust' is negative, an error is signaled by a routine in
       the call tree of this routine.

   14) If any of the input body names do not map to NAIF ID
       codes, an error is signaled by a routine in the call tree of
       this routine.

   15) If the target coincides with the observer or the illumination
       source, an error is signaled by a routine in the call tree
       of this routine.

   16) If required ephemerides or other kernel data are not
       available, an error is signaled by a routine in the call tree
       of this routine.

   17) If any of the `method', `angtyp', `target', `illmn', `fixref',
       `abcorr', `obsrvr' or `relate' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   18) If any of the `method', `angtyp', `target', `illmn', `fixref',
       `abcorr', `obsrvr' or `relate' input strings has zero length,
       the error SPICE(EMPTYSTRING) is signaled.

   19) If any the `cnfine' or `result' cell arguments has a type
       other than SpiceDouble, the error SPICE(TYPEMISMATCH) is
       signaled.

   20) If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled.

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

   -  PCK data: if the target body shape is modeled as an
      ellipsoid (currently no other shapes are supported),
      triaxial radii for the target body must be loaded
      into the kernel pool. Typically this is done by loading a
      text PCK file via furnsh_c.

   -  Further PCK data: rotation data for the target body must be
      loaded. These may be provided in a text or binary PCK file.

   -  Frame data: if a frame definition not built into SPICE
      is required to convert the observer and target states to the
      body-fixed frame of the target, that definition must be
      available in the kernel pool. Typically the definition is
      supplied by loading a frame kernel via furnsh_c.

   -  In some cases the observer's state may be computed at times
      outside of `cnfine' by as much as 2 seconds; data required to
      compute this state must be provided by loaded kernels. See
      -Particulars for details.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   This routine determines a set of one or more time intervals
   within the confinement window when the specified illumination
   angle satisfies a caller-specified constraint. The resulting set
   of intervals is returned as a SPICE window.

   The term "illumination angles" refers to the following set of
   angles:


      phase angle              Angle between the vectors from the
                               surface point to the observer and
                               from the surface point to the
                               illumination source.

      incidence angle          Angle between the surface normal at
                               the specified surface point and the
                               vector from the surface point to the
                               illumination source. When the sun is
                               the illumination source, this angle is
                               commonly called the "solar incidence
                               angle."

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
         sensing measurement is made, and let et - lt (`lt' stands
         for "light time") be the epoch at which the photons
         received at `et' were emitted from the surface point `spoint'.
         Note that the light time between the surface point and
         observer will generally differ from the light time between
         the target body's center and the observer.


         Target body's orientation
         -------------------------

         Using the definitions of `et' and `lt' above, the target body's
         orientation at et - lt is used. The surface normal is
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
         observer-to-surface point direction vector, which we'll
         call SRFVEC, is adjusted so as to point to the apparent
         position of `spoint': considering `spoint' to be an ephemeris
         object, SRFVEC points from the observer's position at `et' to
         the light time and stellar aberration
         corrected position of `spoint'.

         Target body-illumination source vector
         --------------------------------------

         The target body-illumination source vector is the apparent
         position of the illumination source, corrected for light
         time and stellar aberration, as seen from the surface point
         `spoint' at time et-lt.


   Below we discuss in greater detail aspects of this routine's
   solution process that are relevant to correct and efficient
   use of this routine in user applications.


   The Search Process
   ==================

   Regardless of the type of constraint selected by the caller, this
   routine starts the search for solutions by determining the time
   periods, within the confinement window, over which the specified
   illumination angle is monotone increasing and monotone decreasing.
   Each of these time periods is represented by a SPICE window.
   Having found these windows, all of the illumination angle's local
   extrema within the confinement window are known. Absolute extrema
   then can be found very easily.

   Within any interval of these "monotone" windows, there will be at
   most one solution of any equality constraint. Since the boundary
   of the solution set for any inequality constraint is contained in
   the union of

   -  the set of points where an equality constraint is met

   -  the boundary points of the confinement window

   the solutions of both equality and inequality constraints can be
   found easily once the monotone windows have been found.


   Step Size
   =========

   The monotone windows (described above) are found via a two-step
   search process. Each interval of the confinement window is
   searched as follows: first, the input step size is used to
   determine the time separation at which the sign of the rate of
   change of the illumination angle will be sampled. Starting at the
   left endpoint of an interval, samples will be taken at each step.
   If a change of sign is found, a root has been bracketed; at that
   point, the time at which the rate of change of the selected
   illumination angle is zero can be found by a refinement process,
   for example, via binary search.

   Note that the optimal choice of step size depends on the lengths
   of the intervals over which the illumination angle is monotone:
   the step size should be shorter than the shortest of these
   intervals (within the confinement window).

   The optimal step size is *not* necessarily related to the lengths
   of the intervals comprising the result window. For example, if
   the shortest monotone interval has length 10 days, and if the
   shortest result window interval has length 5 minutes, a step size
   of 9.9 days is still adequate to find all of the intervals in the
   result window. In situations like this, the technique of using
   monotone windows yields a dramatic efficiency improvement over a
   state-based search that simply tests at each step whether the
   specified constraint is satisfied. The latter type of search can
   miss solution intervals if the step size is longer than the
   shortest solution interval.

   Having some knowledge of the relative geometry of the target,
   observer, and illumination source can be a valuable aid in
   picking a reasonable step size. In general, the user can
   compensate for lack of such knowledge by picking a very short
   step size; the cost is increased computation time.

   Note that the step size is not related to the precision with which
   the endpoints of the intervals of the result window are computed.
   That precision level is controlled by the convergence tolerance.


   Convergence Tolerance
   =====================

   As described above, the root-finding process used by this routine
   involves first bracketing roots and then using a search process
   to locate them. "Roots" are both times when local extrema are
   attained and times when the illumination angle is equal to a
   reference value. All endpoints of the intervals comprising the
   result window are either endpoints of intervals of the
   confinement window or roots.

   Once a root has been bracketed, a refinement process is used to
   narrow down the time interval within which the root must lie.
   This refinement process terminates when the location of the root
   has been determined to within an error margin called the
   "convergence tolerance." The convergence tolerance used by this
   routine is set via the parameter SPICE_GF_CNVTOL.

   The value of SPICE_GF_CNVTOL is set to a "tight" value so that the
   tolerance doesn't become the limiting factor in the accuracy of
   solutions found by this routine. In general the accuracy of input
   data will be the limiting factor.

   The user may change the convergence tolerance from the default
   SPICE_GF_CNVTOL value by calling the routine gfstol_c, e.g.

      gfstol_c ( tolerance value in seconds );

   Call gfstol_c prior to calling this routine. All subsequent
   searches will use the updated tolerance value.

   Searches over time windows of long duration may require use of
   larger tolerance values than the default: the tolerance must be
   large enough so that it, when added to or subtracted from the
   confinement window's lower and upper bounds, yields distinct time
   values.

   Setting the tolerance tighter than SPICE_GF_CNVTOL is unlikely to be
   useful, since the results are unlikely to be more accurate.
   Making the tolerance looser will speed up searches somewhat,
   since a few convergence steps will be omitted.


   The Confinement Window
   ======================

   The simplest use of the confinement window is to specify a time
   interval within which a solution is sought. However, the
   confinement window can, in some cases, be used to make searches
   more efficient. Sometimes it's possible to do an efficient search
   to reduce the size of the time period over which a relatively
   slow search of interest must be performed.

   Certain types of searches require the state of the observer,
   relative to the solar system barycenter, to be computed at times
   slightly outside the confinement window `cnfine'. The time window
   that is actually used is the result of "expanding" `cnfine' by a
   specified amount "T": each time interval of `cnfine' is expanded by
   shifting the interval's left endpoint to the left and the right
   endpoint to the right by T seconds. Any overlapping intervals are
   merged. (The input argument `cnfine' is not modified.)

   The window expansions listed below are additive: if both
   conditions apply, the window expansion amount is the sum of the
   individual amounts.

   -  If a search uses an equality constraint, the time window
      over which the state of the observer is computed is expanded
      by 1 second at both ends of all of the time intervals
      comprising the window over which the search is conducted.

   -  If a search uses stellar aberration corrections, the time
      window over which the state of the observer is computed is
      expanded as described above.

   When light time corrections are used, expansion of the search
   window also affects the set of times at which the light time-
   corrected state of the target is computed.

   In addition to the possible 2 second expansion of the search
   window that occurs when both an equality constraint and stellar
   aberration corrections are used, round-off error should be taken
   into account when the need for data availability is analyzed.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Determine time intervals over which the MER-1 ("Opportunity")
      rover's location satisfies certain constraints on its
      illumination and visibility as seen from the Mars
      Reconnaissance Orbiter (MRO) spacecraft.

      In this case we require the emission angle to be less than
      20 degrees and the solar incidence angle to be less than
      60 degrees.

      The reader can verify that the observation start times of the
      MRO HIRISE images

         Product ID              Image start time
         ----------              ----------------
         TRA_000873_1780_RED     2006-10-03T12:44:13.425
         PSP_001414_1780_RED     2006-11-14T15:39:55.373
         PSP_001612_1780_RED     2006-11-30T01:38:34.390

      are contained within the result window found by the
      example program shown below.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: gfilum_ex1.tm

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
            de421.bsp                     Planetary ephemeris
            pck00010.tpc                  Planet orientation
                                          and radii
            naif0010.tls                  Leapseconds
            mer1_surf_rover_ext10_v1.bsp  MER-1 ephemeris
            mer1_surf_rover_ext11_v1.bsp  MER-1 ephemeris
            mer1_ls_040128_iau2000_v1.bsp MER-1 landing site
                                          ephemeris
            mro_psp1.bsp                  MRO ephemeris
            mer1_v10.tf                   MER-1 frame kernel


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00010.tpc',
                                'naif0010.tls',
                                'mro_psp1.bsp',
                                'mer1_surf_rover_ext10_v1.bsp',
                                'mer1_surf_rover_ext11_v1.bsp',
                                'mer1_ls_040128_iau2000_v1.bsp',
                                'mro_psp1.bsp',
                                'mer1_v10.tf'                    )
         \begintext


      Example code begins here.


      /.
         Program gfilum_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"


      int main()
      {
         /.
         Output time format:
         ./
         #define  TIMFMT  "YYYY MON DD HR:MN:SC.### UTC"

         /.
         Meta-kernel name:
         ./
         #define  META  "gfilum_ex1.tm"

         /.
         Maximum number of intervals in the windows
         used in this program:
         ./
         #define  MAXIVL  1000
         #define  MAXWIN  ( 2 * MAXIVL )

         /.
         Maximum length of time string:
         ./
         #define  TIMLEN  41


         /.
         Local variables
         ./
         SPICEDOUBLE_CELL      ( cnfine, MAXWIN );
         SPICEDOUBLE_CELL      ( result, MAXWIN );
         SPICEDOUBLE_CELL      ( wnsolr, MAXWIN );

         SpiceChar             * abcorr;
         SpiceChar             * fixref;
         SpiceChar             * illmn;
         SpiceChar             * method;
         SpiceChar             * obsrvr;
         SpiceChar             * target;
         SpiceChar               timstr [ TIMLEN ];
         SpiceChar             * utcbeg;
         SpiceChar             * utcend;

         SpiceDouble             adjust;
         SpiceDouble             emissn;
         SpiceDouble             et0;
         SpiceDouble             et1;
         SpiceDouble             finish;
         SpiceDouble             phase;
         SpiceDouble             refval;
         SpiceDouble             rovlt;
         SpiceDouble             rovpos [ 3 ];
         SpiceDouble             solar;
         SpiceDouble             srfvec [ 3 ];
         SpiceDouble             start;
         SpiceDouble             step;
         SpiceDouble             trgepc;

         SpiceInt                i;


         /.
         Load kernels:
         ./
         furnsh_c ( META );

         /.
         Set the search interval:
         ./
         utcbeg = "2006 OCT 02 00:00:00 UTC";
         str2et_c ( utcbeg, &et0 );

         utcend = "2006 NOV 30 12:00:00 UTC";
         str2et_c ( utcend, &et1 );

         wninsd_c ( et0, et1, &cnfine );

         /.
         Set observer, target, aberration correction, and the
         Mars body-fixed, body-centered reference frame. The
         lighting source is the sun.

         Aberration corrections are set for remote observations.
         ./
         illmn  = "sun";
         obsrvr = "mro";
         target = "mars";
         abcorr = "cn+s";
         fixref = "iau_mars";

         /.
         Use the rover position at the start of
         the search interval as the surface point.
         ./
         spkpos_c ( "MER-1", et0,    fixref,
                    "NONE",  target, rovpos, &rovlt );

         /.
         Initialize the adjustment value for absolute
         extremum searches. We're not performing
         such searches in this example, but this input
         to GFILUM must still be set.
         ./
         adjust = 0.0;

         /.
         The computation uses an ellipsoidal model for the
         target body shape.
         ./
         method = "Ellipsoid";

         /.
         Set the reference value to use for the solar
         incidence angle search.
         ./
         refval = 60.0 * rpd_c();

         /.
         Since the period of the solar incidence angle
         is about one Martian day, we can safely use 6 hours
         as the search step.
         ./
         step   = 21600.0;

         /.
         Search over the confinement window for times
         when the solar incidence angle is less than
         the reference value.
         ./
         gfilum_c ( method,  "INCIDENCE", target, illmn,
                    fixref,  abcorr,      obsrvr, rovpos,
                    "<",     refval,      adjust, step,
                     MAXIVL, &cnfine,     &wnsolr       );
         /.
         Set the reference value for the emission angle search.
         ./
         refval = 20.0 * rpd_c();

         /.
         We'll use 15 minutes as the search step. This step
         is small enough to be suitable for Mars orbiters.
         Units are seconds.
         ./
         step   = 900.0;

         /.
         Search over the previous result window for times when the
         emission angle is less than the reference value.
         ./
         gfilum_c ( method,  "EMISSION", target, illmn,
                    fixref,  abcorr,     obsrvr, rovpos,
                    "<",     refval,     adjust, step,
                    MAXIVL,  &wnsolr,    &result        );

         /.
         Display the result window. Show the solar incidence
         and emission angles at the window's interval
         boundaries.
         ./
         printf( "\n" );

         if ( wncard_c( &result ) == 0 )
         {
            printf( "     Window is empty: condition "
                    "is not met.\n"                    );
         }
         else
         {
            printf (   "                                 "
                       "   Solar Incidence   Emission\n"
                       "                                 "
                       "         (deg)         (deg)\n"
                       "\n"                                   );

            for ( i = 0;  i < wncard_c( &result );  i++ )
            {

               wnfetd_c ( &result, i, &start, &finish );

               /.
               Compute the angles of interest at the boundary
               epochs.
               ./
               timout_c ( start, TIMFMT, TIMLEN, timstr );

               ilumin_c ( method, target, start,  fixref,
                          abcorr, obsrvr, rovpos, &trgepc,
                          srfvec, &phase, &solar, &emissn );

               printf ( "Start: %s %13.8f %13.8f\n",
                        timstr,  solar*dpr_c(),  emissn*dpr_c() );


               timout_c ( finish, TIMFMT, TIMLEN, timstr );

               ilumin_c ( method, target, finish, fixref,
                          abcorr, obsrvr, rovpos, &trgepc,
                          srfvec, &phase, &solar, &emissn );


               printf ( "Stop:  %s %13.8f %13.8f\n",
                        timstr,  solar*dpr_c(),  emissn*dpr_c() );

               printf ( "\n" );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


                                          Solar Incidence   Emission
                                                (deg)         (deg)

      Start: 2006 OCT 03 12:43:46.949 UTC   56.10415019   20.00000019
      Stop:  2006 OCT 03 12:44:42.288 UTC   56.29996181   20.00000015

      Start: 2006 OCT 08 16:03:33.956 UTC   56.48955485   20.00000021
      Stop:  2006 OCT 08 16:04:29.495 UTC   56.68754510   19.99999997

      Start: 2006 OCT 13 19:23:24.634 UTC   56.88741059   19.99999988
      Stop:  2006 OCT 13 19:24:12.492 UTC   57.05931857   20.00000017

      Start: 2006 OCT 18 22:43:21.631 UTC   57.30924467   20.00000012
      Stop:  2006 OCT 18 22:43:47.966 UTC   57.40457272   20.00000004

      Start: 2006 NOV 14 15:39:44.153 UTC   54.32875839   19.99999994
      Stop:  2006 NOV 14 15:40:10.446 UTC   54.42668077   19.99999990

      Start: 2006 NOV 19 18:59:10.190 UTC   54.63096111   20.00000007
      Stop:  2006 NOV 19 18:59:54.776 UTC   54.79840753   19.99999985

      Start: 2006 NOV 24 22:18:38.342 UTC   54.94960000   19.99999982
      Stop:  2006 NOV 24 22:19:30.964 UTC   55.14883883   20.00000003

      Start: 2006 NOV 30 01:38:07.309 UTC   55.28054784   19.99999983
      Stop:  2006 NOV 30 01:39:03.296 UTC   55.49418925   19.99999999


-Restrictions

   1)  The kernel files to be used by this routine must be loaded
       (normally using the CSPICE routine furnsh_c) before this
       routine is called.

   2)  This routine has the side effect of re-initializing the
       illumination angle utility package. Callers may
       need to re-initialize the package after calling this routine.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 01-NOV-2021 (JDR)

       Updated short error messages for consistency within CSPICE wrapper
       interface: MALLOCFAILURE -> MALLOCFAILED, and INVALIDDIMENSION ->
       VALUEOUTOFRANGE.

       Updated header to describe use of expanded confinement window.

       Edited the header to comply with NAIF standard.

       Changed code example for the solution to fit within the -Examples
       section without modifications.

       Updated the description of "nintvls", "cnfine" and "result" arguments.

       Added entry #20 in -Exceptions section.

   -CSPICE Version 1.0.0, 27-FEB-2014 (NJB) (BVS) (EDW)

-Index_Entries

   solve for illumination_angle constraints
   solve for phase_angle constraints
   solve for solar_incidence_angle constraints
   solve for incidence_angle constraints
   solve for emission_angle constraints
   search using illumination_angle constraints
   search using lighting_angle constraints

-&
*/

{ /* Begin gfilum_c */


   /*
   Static local variables
   */
   static SpiceInt         nw  =  SPICE_GF_NWILUM;

   /*
   Local variables
   */
   doublereal            * work;

   SpiceInt                nBytes;
   SpiceInt                worksz;

   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "gfilum_c" );


   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfilum_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary.
   */
   CELLINIT2 ( cnfine, result );

   /*
   Check the input strings to make sure each pointer is non-null
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfilum_c", method );
   CHKFSTR ( CHK_STANDARD, "gfilum_c", angtyp );
   CHKFSTR ( CHK_STANDARD, "gfilum_c", target );
   CHKFSTR ( CHK_STANDARD, "gfilum_c", illmn  );
   CHKFSTR ( CHK_STANDARD, "gfilum_c", fixref );
   CHKFSTR ( CHK_STANDARD, "gfilum_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "gfilum_c", obsrvr );
   CHKFSTR ( CHK_STANDARD, "gfilum_c", relate );

   /*
   Check the workspace size; some mallocs have a violent
   dislike for negative allocation amounts. To be safe,
   rule out a count of zero intervals as well.
   */
   if ( nintvls < 1 )
   {
      setmsg_c ( "The specified workspace interval count # was "
                 "less than the minimum allowed value (1)."     );
      errint_c ( "#",  nintvls                                  );
      sigerr_c ( "SPICE(VALUEOUTOFRANGE)"                       );
      chkout_c ( "gfilum_c"                                     );
      return;
   }

   /*
   Allocate the workspace.

   We have `nw' "doublereal" cells, each having cell size 2*nintvls.
   Each cell also has a control area containing SPICE_CELL_CTRLSZ
   double precision values.
   */

   worksz = nintvls * 2;

   nBytes = ( worksz + SPICE_CELL_CTRLSZ ) * nw * sizeof(SpiceDouble);

   work   = (doublereal *) malloc ( nBytes );

   if ( !work )
   {
      setmsg_c ( "Workspace allocation of # bytes failed due to "
                 "malloc failure"                                 );
      errint_c ( "#",  nBytes                                     );
      sigerr_c ( "SPICE(MALLOCFAILED)"                            );
      chkout_c ( "gfilum_c"                                       );
      return;
   }

   /*
   Let the f2'd routine do the work.
   */
   gfilum_ ( ( char          * ) method,
             ( char          * ) angtyp,
             ( char          * ) target,
             ( char          * ) illmn,
             ( char          * ) fixref,
             ( char          * ) abcorr,
             ( char          * ) obsrvr,
             ( doublereal    * ) spoint,
             ( char          * ) relate,
             ( doublereal    * ) &refval,
             ( doublereal    * ) &adjust,
             ( doublereal    * ) &step,
             ( doublereal    * ) (cnfine->base),
             ( integer       * ) &worksz,
             ( integer       * ) &nw,
             ( doublereal    * ) work,
             ( doublereal    * ) (result->base),
             ( ftnlen          ) strlen(method),
             ( ftnlen          ) strlen(angtyp),
             ( ftnlen          ) strlen(target),
             ( ftnlen          ) strlen(illmn),
             ( ftnlen          ) strlen(fixref),
             ( ftnlen          ) strlen(abcorr),
             ( ftnlen          ) strlen(obsrvr),
             ( ftnlen          ) strlen(relate) );

   /*
   De-allocate the workspace.
   */
   free ( work );

   /*
   Sync the output cell.
   */
   if ( !failed_c() )
   {
     zzsynccl_c ( F2C, result ) ;
   }

   chkout_c ( "gfilum_c" );

} /* End gfilum_c */
