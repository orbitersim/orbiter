/*

-Procedure gfsubc_c (GF, subpoint vector coordinate search)

-Abstract

   Determine time intervals for which a coordinate of an
   subpoint position vector satisfies a numerical constraint.

-Disclaimer

   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
   PUBLICL Y AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
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
   SPK
   CK
   TIME
   WINDOWS

-Keywords

   COORDINATE
   EVENT
   GEOMETRY
   SEARCH

*/

   #include <stdlib.h>
   #include "SpiceUsr.h"
   #include "SpiceGF.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "zzalloc.h"

   void gfsubc_c ( ConstSpiceChar     * target,
                   ConstSpiceChar     * fixref,
                   ConstSpiceChar     * method,
                   ConstSpiceChar     * abcorr,
                   ConstSpiceChar     * obsrvr,
                   ConstSpiceChar     * crdsys,
                   ConstSpiceChar     * coord,
                   ConstSpiceChar     * relate,
                   SpiceDouble          refval,
                   SpiceDouble          adjust,
                   SpiceDouble          step,
                   SpiceInt             nintvls,
                   SpiceCell          * cnfine,
                   SpiceCell          * result  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   SPICE_GF_CNVTOL
              P   Convergence tolerance.
   target     I   Name of the target body.
   fixref     I   Body fixed frame associated with `target'.
   method     I   Name of method type for subpoint calculation.
   abcorr     I   Aberration correction flag.
   obsrvr     I   Name of the observing body.
   crdsys     I   Name of the coordinate system containing `coord'.
   coord      I   Name of the coordinate of interest.
   relate     I   Relational operator.
   refval     I   Reference value.
   adjust     I   Adjustment value for absolute extrema searches.
   step       I   Step size used for locating extrema and roots.
   nintvls    I   Workspace window interval count.
   cnfine    I-O  SPICE window to which the search is confined.
   result     O   SPICE window containing results.

-Detailed_Input

   target      is the string name of a target body. Optionally, you may
               supply the integer ID code for the object as an
               integer string. For example both "MOON" and "301"
               are legitimate strings that indicate the moon is the
               target body.

               The target and observer define a position vector
               that points from the observer to the target.

   fixref      is the string name of the body-fixed, body-centered
               reference frame associated with the target body `target'.

               The SPICE frame subsystem must recognize the "fixref"
               name.

   method      is the string name of the method to use for the subpoint
               calculation. The accepted values for `method':

                  "Near point: ellipsoid"   The sub-observer point
                                            computation uses a
                                            triaxial ellipsoid to
                                            model the surface of the
                                            target body. The
                                            sub-observer point is
                                            defined as the nearest
                                            point on the target
                                            relative to the
                                            observer.

                  "Intercept: ellipsoid"    The sub-observer point
                                            computation uses a
                                            triaxial ellipsoid to
                                            model the surface of the
                                            target body. The
                                            sub-observer point is
                                            defined as the target
                                            surface intercept of the
                                            line containing the
                                            observer and the
                                            target's center.

               The `method' string lacks sensitivity to case, embedded,
               leading and trailing blanks.

   abcorr      is the string description of the aberration corrections
               to apply to the state evaluations to account for one-way
               light time and stellar aberration.

               This routine accepts the same aberration corrections
               as does the SPICE routine spkezr_c. See the header of
               spkezr_c for a detailed description of the aberration
               correction options. For convenience, the options are
               listed below:

                  "NONE"     Apply no correction. Returns the "true"
                             geometric state.

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

                  "XLT"      "Transmission" case: correct for
                             one-way light time using a Newtonian
                             formulation.

                  "XLT+S"    "Transmission" case: correct for
                             one-way light time and stellar
                             aberration using a Newtonian
                             formulation.

                  "XCN"      "Transmission" case: converged
                             Newtonian light time correction.

                  "XCN+S"    "Transmission" case: converged
                             Newtonian light time and stellar
                             aberration corrections.

               The `abcorr' string lacks sensitivity to case, leading
               and trailing blanks.

   obsrvr      is the string name of an observing body. Optionally, you
               may supply the ID code of the object as an integer
               string. For example, both "EARTH" and "399" are
               legitimate strings to indicate that the observer is the
               Earth.

   crdsys      is the string name of the coordinate system for which the
               coordinate of interest is a member.

   coord       is the string name of the coordinate of interest in
               `crdsys'.

               The supported coordinate systems and coordinate names:

                  crdsys             coord               Range
                  ----------------   -----------------   ------------

                  "RECTANGULAR"      "X"
                                     "Y"
                                     "Z"

                  "LATITUDINAL"      "RADIUS"
                                     "LONGITUDE"         (-Pi,Pi]
                                     "LATITUDE"          [-Pi/2,Pi/2]

                  "RA/DEC"           "RANGE"
                                     "RIGHT ASCENSION"   [0,2Pi)
                                     "DECLINATION"       [-Pi/2,Pi/2]

                  "SPHERICAL"        "RADIUS"
                                     "COLATITUDE"        [0,Pi]
                                     "LONGITUDE"         (-Pi,Pi]

                  "CYLINDRICAL"      "RADIUS"
                                     "LONGITUDE"         [0,2Pi)
                                     "Z"

                  "GEODETIC"         "LONGITUDE"         (-Pi,Pi]
                                     "LATITUDE"          [-Pi/2,Pi/2]
                                     "ALTITUDE"

                  "PLANETOGRAPHIC"   "LONGITUDE"         [0,2Pi)
                                     "LATITUDE"          [-Pi/2,Pi/2]
                                     "ALTITUDE"

               The "ALTITUDE" coordinates have a constant value of
               zero +/- roundoff for ellipsoid targets.

               Limit searches for coordinate events in the "GEODETIC"
               and "PLANETOGRAPHIC" coordinate systems to `target' bodies
               with axial symmetry in the equatorial plane, i.e.
               equality of the body X and Y radii (oblate or prolate
               spheroids).

               Searches on "GEODETIC" or "PLANETOGRAPHIC" coordinates
               requires body shape data, and in the case of
               "PLANETOGRAPHIC" coordinates, body rotation data.

               The body associated with "GEODETIC" or "PLANETOGRAPHIC"
               coordinates is the center of the frame `fixref'.

   relate      is the string or character describing the relational
               operator used to define a constraint on the selected
               coordinate of the subpoint vector. The result
               window found by this routine indicates the time intervals
               where the constraint is satisfied. Supported values of
               `relate' and corresponding meanings are shown below:

                  ">"        The coordinate value is greater than the
                             reference value `refval'.

                  "="        The coordinate value is equal to the
                             reference value `refval'.

                  "<"        The coordinate value is less than the
                             reference value `refval'.

                  "ABSMAX"   The coordinate value is at an absolute
                             maximum.

                  "ABSMIN"   The coordinate value is at an absolute
                             minimum.

                  "LOCMAX"   The coordinate value is at a local
                             maximum.

                  "LOCMIN"   The coordinate value is at a local
                             minimum.

               The caller may indicate that the region of interest
               is the set of time intervals where the quantity is
               within a specified measure of an absolute extremum.
               The argument `adjust' (described below) is used to
               specify this measure.

               Local extrema are considered to exist only in the
               interiors of the intervals comprising the confinement
               window:  a local extremum cannot exist at a boundary
               point of the confinement window.

               The `relate' string lacks sensitivity to case, leading
               and trailing blanks.

   refval      is the double precision reference value used together
               with the argument `relate' to define an equality or
               inequality to satisfy by the selected coordinate of the
               subpoint vector. See the discussion of `relate' above for
               further information.

               The units of `refval' correspond to the type as defined
               by `coord', radians for angular measures, kilometers for
               distance measures.

   adjust      is a double precision value used to modify searches for
               absolute extrema: when `relate' is set to "ABSMAX" or
               "ABSMIN" and `adjust' is set to a positive value, gfsubc_c
               finds times when the subpoint position vector coordinate
               is within `adjust' radians/kilometers of the specified
               extreme value.

               For `relate' set to "ABSMAX", the `result' window contains
               time intervals when the position vector coordinate has
               values between absmax - adjust and `absmax'.

               For `relate' set to "ABSMIN", the `result' window contains
               time intervals when the position vector coordinate has
               values between `absmin' and absmin + adjust.

               `adjust' is not used for searches for local extrema,
               equality or inequality conditions.

   step        is the double precision time step size to use in the
               search.

               `step' must be short enough to for a search using this step
               size to locate the time intervals where coordinate
               function of the subpoint vector is monotone increasing or
               decreasing. However, `step' must not be *too* short, or
               the search will take an unreasonable amount of time.

               For coordinates other than "LONGITUDE" and 'RIGHT
               ASCENSION', the step size must be shorter than the
               shortest interval, within the confinement window, over
               which the coordinate is monotone increasing or
               decreasing.

               For "LONGITUDE" and "RIGHT ASCENSION", the step size must
               be shorter than the shortest interval, within the
               confinement window, over which either the sin or cos
               of the coordinate is monotone increasing or decreasing.

               The choice of `step' affects the completeness but not
               the precision of solutions found by this routine; the
               precision is controlled by the convergence tolerance.
               See the discussion of the parameter SPICE_GF_CNVTOL for
               details.

               `step' has units of TDB seconds.

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

   cnfine      is a double precision SPICE window that confines the time
               period over which the specified search is conducted.
               `cnfine' may consist of a single interval or a collection
               of intervals.

               In some cases the confinement window can be used to
               greatly reduce the time period that must be searched
               for the desired solution. See the -Particulars section
               below for further discussion.

               See the -Examples section below for a code example
               that shows how to create a confinement window.

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
               discarded before gfsubc_c conducts its search.

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

   SPICE_GF_CNVTOL

              is the convergence tolerance used for finding endpoints
              of the intervals comprising the result window.
              SPICE_GF_CNVTOL is used to determine when binary searches
              for roots should terminate: when a root is bracketed
              within an interval of length SPICE_GF_CNVTOL; the root is
              considered to have been found.

              The accuracy, as opposed to precision, of roots found by
              this routine depends on the accuracy of the input data.
              In most cases, the accuracy of solutions will be inferior
              to their precision.

              SPICE_GF_CNVTOL has the value 1.0e-6. Units are TDB
              seconds.

-Exceptions

   1)  In order for this routine to produce correct results,
       the step size must be appropriate for the problem at hand.
       Step sizes that are too large may cause this routine to miss
       roots; step sizes that are too small may cause this routine
       to run unacceptably slowly and in some cases, find spurious
       roots.

       This routine does not diagnose invalid step sizes, except
       that if the step size is non-positive, an error is signaled
       by a routine in the call tree of this routine.

   2)  Due to numerical errors, in particular,

          - truncation error in time values
          - finite tolerance value
          - errors in computed geometric quantities

       it is *normal* for the condition of interest to not always be
       satisfied near the endpoints of the intervals comprising the
       `result' window. One technique to handle such a situation,
       slightly contract `result' using the window routine wncond_c.

   3)  If the number of intervals `nintvls' is less than 1, the error
       SPICE(VALUEOUTOFRANGE) is signaled.

   4)  If the window size of `result' is less than 2, the error
       SPICE(INVALIDDIMENSION) is signaled by a routine in the call
       tree of this routine.

   5)  If the output SPICE window `result' has insufficient capacity
       to contain the number of intervals on which the specified
       distance condition is met, an error is signaled
       by a routine in the call tree of this routine.

   6)  If an error (typically cell overflow) occurs during
       window arithmetic, the error is signaled by a routine
       in the call tree of this routine.

   7)  If the relational operator `relate' is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   8)  If the aberration correction specifier contains an
       unrecognized value, an error is signaled by a routine in the
       call tree of this routine.

   9)  If `adjust' is negative, an error is signaled by a routine in
       the call tree of this routine.

   10) If either of the input body names do not map to NAIF ID
       codes, an error is signaled by a routine in the call tree of
       this routine.

   11) If required ephemerides or other kernel data are not
       available, an error is signaled by a routine in the call tree
       of this routine.

   12) If the search uses GEODETIC or PLANETOGRAPHIC coordinates, and
       the center body of the reference frame has unequal equatorial
       radii, an error is signaled by a routine in the call tree of
       this routine.

   13) If any of the `target', `fixref', `method', `abcorr',
       `obsrvr', `crdsys', `coord' or `relate' input string pointers
       is null, the error SPICE(NULLPOINTER) is signaled.

   14) If any of the `target', `fixref', `method', `abcorr',
       `obsrvr', `crdsys', `coord' or `relate' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

   15) If any the `cnfine' or `result' cell arguments has a type
       other than SpiceDouble, the error SPICE(TYPEMISMATCH) is
       signaled.

   16) If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled.

-Files

   Appropriate SPK and PCK kernels must be loaded by the calling
   program before this routine is called.

   The following data are required:

   -  SPK data: the calling application must load ephemeris data
      for the targets, observer, and any intermediate objects in
      a chain connecting the targets and observer that cover the
      time period specified by the window `cnfine'. If aberration
      corrections are used, the states of target and observer
      relative to the solar system barycenter must be calculable
      from the available ephemeris data. Typically ephemeris data
      are made available by loading one or more SPK files using
      furnsh_c.

   -  If non-inertial reference frames are used, then PCK
      files, frame kernels, C-kernels, and SCLK kernels may be
      needed.

   -  In some cases the observer's state may be computed at times
      outside of `cnfine' by as much as 2 seconds; data required to
      compute this state must be provided by loaded kernels. See
      -Particulars for details.

   Such kernel data are normally loaded once per program run, NOT
   every time this routine is called.

-Particulars

   This routine provides a simpler, but less flexible interface
   than does the routine gfevnt_c for conducting searches for
   subpoint position vector coordinate value events.
   Applications that require support for progress reporting,
   interrupt handling, non-default step or refinement functions, or
   non-default convergence tolerance should call gfevnt_c rather than
   this routine.

   This routine determines a set of one or more time intervals
   within the confinement window when the selected coordinate of
   the subpoint position vector satisfies a caller-specified
   constraint. The resulting set of intervals is returned as a SPICE
   window.

   Below we discuss in greater detail aspects of this routine's
   solution process that are relevant to correct and efficient
   use of this routine in user applications.


   The Search Process
   ==================

   Regardless of the type of constraint selected by the caller, this
   routine starts the search for solutions by determining the time
   periods, within the confinement window, over which the specified
   coordinate function is monotone increasing and monotone
   decreasing. Each of these time periods is represented by a SPICE
   window. Having found these windows, all of the coordinate
   function's local extrema within the confinement window are known.
   Absolute extrema then can be found very easily.

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

   The monotone windows (described above) are found using a two-step
   search process. Each interval of the confinement window is
   searched as follows: first, the input step size is used to
   determine the time separation at which the sign of the rate of
   change of coordinate will be sampled. Starting at
   the left endpoint of an interval, samples will be taken at each
   step. If a change of sign is found, a root has been bracketed; at
   that point, the time at which the time derivative of the
   coordinate is zero can be found by a refinement process, for
   example, using a binary search.

   Note that the optimal choice of step size depends on the lengths
   of the intervals over which the coordinate function is monotone:
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

   Having some knowledge of the relative geometry of the target and
   observer can be a valuable aid in picking a reasonable step size.
   In general, the user can compensate for lack of such knowledge by
   picking a very short step size; the cost is increased computation
   time.

   Note that the step size is not related to the precision with which
   the endpoints of the intervals of the result window are computed.
   That precision level is controlled by the convergence tolerance.


   Convergence Tolerance
   =====================

   As described above, the root-finding process used by this routine
   involves first bracketing roots and then using a search process
   to locate them. "Roots" are both times when local extrema are
   attained and times when the coordinate function is equal to a
   reference value. All endpoints of the intervals comprising the
   result window are either endpoints of intervals of the
   confinement window or roots.

   Once a root has been bracketed, a refinement process is used to
   narrow down the time interval within which the root must lie.
   This refinement process terminates when the location of the root
   has been determined to within an error margin called the
   "convergence tolerance." The default convergence tolerance
   used by this routine is set by the parameter SPICE_GF_CNVTOL (defined
   in SpiceGF.h).

   The value of SPICE_GF_CNVTOL is set to a "tight" value so that the
   tolerance doesn't become the limiting factor in the accuracy of
   solutions found by this routine. In general the accuracy of input
   data will be the limiting factor.

   The user may change the convergence tolerance from the default
   SPICE_GF_CNVTOL value by calling the routine gfstol_c, e.g.

      gfstol_c ( tolerance value );

   Call gfstol_c prior to calling this routine. All subsequent
   searches will use the updated tolerance value.

   Setting the tolerance tighter than SPICE_GF_CNVTOL is unlikely to be
   useful, since the results are unlikely to be more accurate.
   Making the tolerance looser will speed up searches somewhat,
   since a few convergence steps will be omitted. However, in most
   cases, the step size is likely to have a much greater effect
   on processing time than would the convergence tolerance.


   The Confinement Window
   ======================

   The simplest use of the confinement window is to specify a time
   interval within which a solution is sought. However, the
   confinement window can, in some cases, be used to make searches
   more efficient. Sometimes it's possible to do an efficient search
   to reduce the size of the time period over which a relatively
   slow search of interest must be performed.

   Practical use of the coordinate search capability would likely
   consist of searches over multiple coordinate constraints to find
   time intervals that satisfies the constraints. An
   effective technique to accomplish such a search is
   to use the result window from one search as the confinement window
   of the next.

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

   Longitude and Right Ascension
   =============================

   The cyclic nature of the longitude and right ascension coordinates
   produces branch cuts at +/- 180 degrees longitude and 0-360
   longitude. Round-off error may cause solutions near these branches
   to cross the branch. Use of the SPICE routine wncond_c will contract
   solution windows by some epsilon, reducing the measure of the
   windows and eliminating the branch crossing. A one millisecond
   contraction will in most cases eliminate numerical round-off
   caused branch crossings.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the time during 2007 for which the subpoint position vector
      of the sun on earth in the IAU_EARTH frame lies within a geodetic
      latitude-longitude "box" defined as

         16 degrees <= latitude  <= 17 degrees
         85 degrees <= longitude <= 86 degrees

      This problem requires four searches, each search on one of the
      box restrictions. The user needs also realize the temporal
      behavior of latitude greatly differs from that of the longitude. The
      sub-observer point latitude varies between approximately 23.44 degrees
      and -23.44 degrees during the year. The sub-observer point longitude
      varies between -180 degrees and 180 degrees in one day.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: gfsubc_ex1.tm

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
            de414.bsp                     Planetary ephemeris
            pck00008.tpc                  Planet orientation and
                                          radii
            naif0008.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de414.bsp',
                                'pck00008.tpc',
                                'naif0008.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program gfsubc_ex1
      ./
      #include <stdio.h>
      #include <stdlib.h>
      #include <string.h>
      #include "SpiceUsr.h"

      #define   MAXWIN   100
      #define   TIMFMT   "YYYY-MON-DD HR:MN:SC.###### (TDB) ::TDB ::RND"
      #define   STRLEN   64

      int main( )
         {

         /.
         Create the needed windows. Note, one window
         consists of two values, so the total number
         of cell values to allocate equals twice
         the number of intervals.
         ./
         SPICEDOUBLE_CELL ( result1, 2*MAXWIN );
         SPICEDOUBLE_CELL ( result2, 2*MAXWIN );
         SPICEDOUBLE_CELL ( result3, 2*MAXWIN );
         SPICEDOUBLE_CELL ( result4, 2*MAXWIN );
         SPICEDOUBLE_CELL ( cnfine, 2       );

         SpiceDouble       begtim;
         SpiceDouble       endtim;
         SpiceDouble       step;
         SpiceDouble       adjust;
         SpiceDouble       refval;
         SpiceDouble       beg;
         SpiceDouble       end;

         SpiceChar         begstr [ STRLEN ];
         SpiceChar         endstr [ STRLEN ];
         SpiceChar       * target = "EARTH";
         SpiceChar       * obsrvr = "SUN";
         SpiceChar       * fixref = "IAU_EARTH";
         SpiceChar       * method = "Near point: ellipsoid";
         SpiceChar       * crdsys = "GEODETIC";
         SpiceChar       * abcorr = "NONE";

         SpiceInt          count;
         SpiceInt          i;

         /.
         Load kernels.
         ./
         furnsh_c( "gfsubc_ex1.tm" );

         /.
         Store the time bounds of our search interval in
         the cnfine confinement window.
         ./
         str2et_c( "2007 JAN 01", &begtim );
         str2et_c( "2008 JAN 01", &endtim );

         wninsd_c ( begtim, endtim, &cnfine );

         /.
         Perform four searches to determine the times when the
         latitude-longitude box restriction conditions apply to
         the subpoint vector.

         Perform the searches such that the result window of a search
         serves as the confinement window of the subsequent search.

         Since the latitude coordinate varies slowly and is well behaved
         over the time of the confinement window, search first for the
         windows satisfying the latitude requirements, then use that result
         as confinement for the longitude search.
         ./

         /.
         The latitude varies relatively slowly, ~46 degrees during the
         year. The extrema occur approximately every six months.
         Search using a step size less than half that value (180 days).
         For this example use ninety days (in units of seconds).
         ./

         step   = (90.)*spd_c();
         adjust = 0.;

         {
         SpiceChar       * coord  = "LATITUDE";
         SpiceChar       * relate = ">";

         refval = 16. *rpd_c();

         gfsubc_c (  target,  fixref,
                     method,  abcorr, obsrvr,
                     crdsys,  coord,
                     relate,  refval,
                     adjust,  step,
                     MAXWIN,
                     &cnfine, &result1 );
         }


         {
         SpiceChar       * coord  = "LATITUDE";
         SpiceChar       * relate = "<";

         refval = 17. *rpd_c();

         gfsubc_c (  target,  fixref,
                     method,  abcorr, obsrvr,
                     crdsys,  coord,
                     relate,  refval,
                     adjust,  step,
                     MAXWIN,
                     &result1, &result2 );
         }


         /.
         Now the longitude search.
         ./

         /.
         Reset the stepsize to something appropriate for the 360
         degrees in 24 hours domain. The longitude shows near
         linear behavior so use a stepsize less than half the period
         of twelve hours. Ten hours will suffice in this case.
         ./
         step   = (10./24.)*spd_c();

         {
         SpiceChar       * coord  = "LONGITUDE";
         SpiceChar       * relate = ">";

         refval = 85. *rpd_c();

         gfsubc_c (  target,  fixref,
                     method,  abcorr, obsrvr,
                     crdsys,  coord,
                     relate,  refval,
                     adjust,  step,
                     MAXWIN,
                     &result2, &result3 );

         /.
         Contract the endpoints of each window to account
         for possible round-off error at the -180/180 degree branch.

         A contraction value of a millisecond should eliminate
         any round-off caused branch crossing.
         ./

         wncond_c( 1e-3, 1e-3, &result3 );
         }


         {
         SpiceChar       * coord  = "LONGITUDE";
         SpiceChar       * relate = "<";

         refval = 86. *rpd_c();

         gfsubc_c (  target,  fixref,
                     method,  abcorr, obsrvr,
                     crdsys,  coord,
                     relate,  refval,
                     adjust,  step,
                     MAXWIN,
                     &result3, &result4 );
         }


         /.
         List the beginning and ending points in each interval
         if result contains data.
         ./
         count = wncard_c( &result4 );

         /.
         Display the results.
         ./
         if (count == 0 )
            {
            printf ( "Result window is empty.\n\n" );
            }
         else
            {
            for ( i = 0;  i < count;  i++ )
               {

               /.
               Fetch the endpoints of the Ith interval
               of the result window.
               ./
               wnfetd_c ( &result4, i, &beg, &end );

               timout_c ( beg, TIMFMT, STRLEN, begstr );
               timout_c ( end, TIMFMT, STRLEN, endstr );

               printf ( "Interval %d\n", i + 1);
               printf ( "Beginning TDB %s \n",   begstr );
               printf ( "Ending TDB    %s \n\n", endstr );

               }
            }

         kclear_c();
         return( 0 );
         }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Interval 1
      Beginning TDB 2007-MAY-05 06:14:04.637735 (TDB)
      Ending TDB    2007-MAY-05 06:18:04.621907 (TDB)

      Interval 2
      Beginning TDB 2007-MAY-06 06:13:59.583483 (TDB)
      Ending TDB    2007-MAY-06 06:17:59.569240 (TDB)

      Interval 3
      Beginning TDB 2007-MAY-07 06:13:55.102940 (TDB)
      Ending TDB    2007-MAY-07 06:17:55.090299 (TDB)

      Interval 4
      Beginning TDB 2007-MAY-08 06:13:51.202604 (TDB)
      Ending TDB    2007-MAY-08 06:17:51.191583 (TDB)

      Interval 5
      Beginning TDB 2007-AUG-06 06:23:17.282927 (TDB)
      Ending TDB    2007-AUG-06 06:27:17.264010 (TDB)

      Interval 6
      Beginning TDB 2007-AUG-07 06:23:10.545441 (TDB)
      Ending TDB    2007-AUG-07 06:27:10.524925 (TDB)

      Interval 7
      Beginning TDB 2007-AUG-08 06:23:03.233996 (TDB)
      Ending TDB    2007-AUG-08 06:27:03.211889 (TDB)


-Restrictions

   1)  The kernel files to be used by this routine must be loaded
       (normally via the CSPICE routine furnsh_c) before this routine
       is called.

   2)  This routine has the side effect of re-initializing the
       coordinate quantity utility package. Callers may
       need to re-initialize the package after calling this routine.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 01-NOV-2021 (JDR) (EDW)

       Added use of ALLOC_CHECK_INTRA to check net null effect on
       alloc count.

       Updated header to describe use of expanded confinement window.

       Edited the header to comply with NAIF standard.

       Added entries #3, #4, #12 and #15 in -Exceptions section.

       Updated the description of "nintvls", "cnfine" and "result" arguments.

   -CSPICE Version 1.0.2, 29-AUG-2014 (EDW)

       Edit to header, replaced ' character with character " to indicate
       C strings. Removed extraneous line from header.

   -CSPICE Version 1.0.1, 28-FEB-2013 (NJB) (EDW)

       Header was updated to discuss use of gfstol_c.

       Edit to comments to correct search description.

       Edit to Example description, replaced "intercept" with
       "sub-observer point."

       Correction of several typos.

   -CSPICE Version 1.0.0, 10-FEB-2009 (NJB) (EDW)

-Index_Entries

   GF subpoint coordinate search

-&
*/

   { /* Begin gfsubc_c */

   /*
   Local variables
   */
   doublereal            * work;

   SpiceInt                nBytes;

   int                     nowalloc;

   static SpiceInt         nw = SPICE_GF_NWMAX;



   /*
   Participate in error tracing.
   */
   if ( return_c() )
      {
      return;
      }
   chkin_c ( "gfsubc_c" );


   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfsubc_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary.
   */
   CELLINIT2 ( cnfine, result );

   /*
   Check the input strings to make sure each pointer is non-null
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfsubc_c", target );
   CHKFSTR ( CHK_STANDARD, "gfsubc_c", fixref );
   CHKFSTR ( CHK_STANDARD, "gfsubc_c", method );
   CHKFSTR ( CHK_STANDARD, "gfsubc_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "gfsubc_c", obsrvr );
   CHKFSTR ( CHK_STANDARD, "gfsubc_c", crdsys );
   CHKFSTR ( CHK_STANDARD, "gfsubc_c", coord  );
   CHKFSTR ( CHK_STANDARD, "gfsubc_c", relate );

   /*
   Check the workspace size; some mallocs have a violent
   dislike for negative allocation amounts. To be safe,
   rule out a count of zero intervals as well.
   */

   if ( nintvls < 1 )
      {
      setmsg_c ( "The specified workspace interval count # was "
                 "less than the minimum allowed value of one (1)." );
      errint_c ( "#",  nintvls                              );
      sigerr_c ( "SPICE(VALUEOUTOFRANGE)"                   );
      chkout_c ( "gfposc_c"                                 );
      return;
      }

   /*
   Allocate the workspace. 'nintvls' indicates the maximum number of
   intervals returned in 'result'. An interval consists of
   two values.
   */

   nowalloc = alloc_count();

   nintvls = 2 * nintvls;

   nBytes = ( nintvls + SPICE_CELL_CTRLSZ ) * nw * sizeof(SpiceDouble);

   work   = (doublereal *) alloc_SpiceMemory( nBytes );

   if ( !work )
      {
      setmsg_c ( "Workspace allocation of # bytes failed due to "
                 "malloc failure"                               );
      errint_c ( "#",  nBytes                                   );
      sigerr_c ( "SPICE(MALLOCFAILED)"                          );
      chkout_c ( "gfsubc_c"                                     );
      return;
      }


   /*
   Let the f2'd routine do the work.
   */

   gfsubc_ ( ( char          * ) target,
             ( char          * ) fixref,
             ( char          * ) method,
             ( char          * ) abcorr,
             ( char          * ) obsrvr,
             ( char          * ) crdsys,
             ( char          * ) coord,
             ( char          * ) relate,
             ( doublereal    * ) &refval,
             ( doublereal    * ) &adjust,
             ( doublereal    * ) &step,
             ( doublereal    * ) (cnfine->base),
             ( integer       * ) &nintvls,
             ( integer       * ) &nw,
             ( doublereal    * ) work,
             ( doublereal    * ) (result->base),
             ( ftnlen          ) strlen(target),
             ( ftnlen          ) strlen(fixref),
             ( ftnlen          ) strlen(method),
             ( ftnlen          ) strlen(abcorr),
             ( ftnlen          ) strlen(obsrvr),
             ( ftnlen          ) strlen(crdsys),
             ( ftnlen          ) strlen(coord),
             ( ftnlen          ) strlen(relate) );

   /*
   De-allocate the workspace.
   */
   free_SpiceMemory( work );

   /*
   Sync the output cell.
   */
   if ( !failed_c() )
      {
      zzsynccl_c ( F2C, result ) ;
      }

   ALLOC_CHECK_INTRA(nowalloc);

   chkout_c ( "gfsubc_c" );

   } /* End gfsubc_c */
