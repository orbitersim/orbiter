/*

-Procedure gfdist_c ( GF, distance search )

-Abstract

   Return the time window over which a specified constraint on
   observer-target distance is met.

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
   NAIF_IDS
   SPK
   TIME
   WINDOWS

-Keywords

   EPHEMERIS
   EVENT
   GEOMETRY
   SEARCH
   WINDOW

*/

   #include <stdlib.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void gfdist_c ( ConstSpiceChar     * target,
                   ConstSpiceChar     * abcorr,
                   ConstSpiceChar     * obsrvr,
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
   SPICE_GF_NWDIST
              P   Number of workspace windows for distance search.
   target     I   Name of the target body.
   abcorr     I   Aberration correction flag.
   obsrvr     I   Name of the observing body.
   relate     I   Relational operator.
   refval     I   Reference value.
   adjust     I   Adjustment value for absolute extrema searches.
   step       I   Step size used for locating extrema and roots.
   nintvls    I   Workspace window interval count.
   cnfine    I-O  SPICE window to which the search is confined.
   result     O   SPICE window containing results.

-Detailed_Input

   target      is the name of a target body. Optionally, you may supply
               the integer ID code for the object as an integer string.
               For example both "MOON" and "301" are legitimate strings
               that indicate the Moon is the target body.

               The target and observer define a position vector which
               points from the observer to the target; the length of
               this vector is the "distance" that serves as the subject
               of the search performed by this routine.

               Case and leading or trailing blanks are not significant
               in the string `target'.

   abcorr      indicates the aberration corrections to be applied to the
               observer-target position vector to account for one-way
               light time and stellar aberration.

               Any aberration correction accepted by the SPICE routine
               spkezr_c is accepted here. See the header of spkezr_c for a
               detailed description of the aberration correction
               options. For convenience, the options are listed below:

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

               Case and leading or trailing blanks are not significant
               in the string `abcorr'.

   obsrvr      is the name of an observing body. Optionally, you may
               supply the ID code of the object as an integer string.
               For example, both "EARTH" and "399" are legitimate
               strings to supply to indicate the observer is Earth.

               Case and leading or trailing blanks are not significant
               in the string `obsrvr'.

   relate      is a relational operator used to define a constraint on
               the observer-target distance. The result window found by
               this routine indicates the time intervals where the
               constraint is satisfied.

               Supported values of `relate' and corresponding meanings are
               shown below:

                  ">"        Distance is greater than the reference
                             value `refval'.

                  "="        Distance is equal to the reference
                             value `refval'.

                  "<"        Distance is less than the reference
                             value `refval'.

                  "ABSMAX"   Distance is at an absolute maximum.

                  "ABSMIN"   Distance is at an absolute  minimum.

                  "LOCMAX"   Distance is at a local maximum.

                  "LOCMIN"   Distance is at a local minimum.

               The caller may indicate that the region of interest is
               the set of time intervals where the distance is within a
               specified offset relative to an absolute extremum. The
               argument `adjust' (described below) is used to specify this
               offset.

               Local extrema are considered to exist only in the
               interiors of the intervals comprising the confinement
               window:  a local extremum cannot exist at a boundary
               point of the confinement window.

               Case and leading or trailing blanks are not significant
               in the string `relate'.

   refval      is the reference value used together with the argument
               `relate' to define an equality or inequality to be
               satisfied by the distance between the specified target
               and observer. See the discussion of `relate' above for
               further information.

               The units of `refval' are km.

   adjust      is a parameter used to modify searches for absolute
               extrema: when `relate' is set to "ABSMAX" or "ABSMIN" and
               `adjust' is set to a positive value, gfdist_c will find times
               when the observer-target distance is within `adjust' km of
               the specified extreme value.

               If `adjust' is non-zero and a search for an absolute
               minimum `amin' is performed, the result window contains
               time intervals when the observer-target distance has
               values between `amin' and amin + adjust.

               If the search is for an absolute maximum `amax', the
               corresponding range is  between amax - adjust and `amax'.

               `adjust' is not used for searches for local extrema,
               equality or inequality conditions.

   step        is the step size to be used in the search. `step' must be
               shorter than any maximal time interval on which the
               specified distance function is monotone increasing or
               decreasing. That is, if the confinement window is
               partitioned into alternating intervals on which the
               distance function is either monotone increasing or
               decreasing, `step' must be shorter than any of these
               intervals.

               However, `step' must not be *too* short, or the search will
               take an unreasonable amount of time.

               The choice of `step' affects the completeness but not the
               precision of solutions found by this routine; the
               precision is controlled by the convergence tolerance. See
               the discussion of the parameter SPICE_GF_CNVTOL for details.

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

   cnfine      is a SPICE window that confines the time period over
               which the specified search is conducted. `cnfine' may
               consist of a single interval or a collection of
               intervals.

               The endpoints of the time intervals comprising `cnfine' are
               interpreted as seconds past J2000 TDB.

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
               distance constraint is satisfied.

               `result' must be declared and initialized with sufficient
               size to capture the full set of time intervals within the
               search region on which the specified condition is satisfied.

               If `result' is non-empty on input, its contents will be
               discarded before gfdist_c conducts its search.

               The endpoints of the time intervals comprising `result'
               are interpreted as seconds past J2000 TDB.

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

               is the convergence tolerance used for finding endpoints of the
               intervals comprising the result window. SPICE_GF_CNVTOL is also
               used for finding intermediate results; in particular,
               SPICE_GF_CNVTOL is used for finding the windows on which the
               specified distance is increasing or decreasing. SPICE_GF_CNVTOL
               is used to determine when binary searches for roots should
               terminate: when a root is bracketed within an interval of length
               SPICE_GF_CNVTOL; the root is considered to have been found.

               The accuracy, as opposed to precision, of roots found by this
               routine depends on the accuracy of the input data. In most
               cases, the accuracy of solutions will be inferior to their
               precision.

   SPICE_GF_NWDIST

               is the number of workspace windows required by this routine.

   See header file SpiceGF.h for declarations and descriptions of
   parameters used throughout the GF system.

-Exceptions

   1)  In order for this routine to produce correct results,
       the step size must be appropriate for the problem at hand.
       Step sizes that are too large may cause this routine to miss
       roots; step sizes that are too small may cause this routine
       to run unacceptably slowly and in some cases, find spurious
       roots.

       This routine does not diagnose invalid step sizes, except that
       if the step size is non-positive, an error is signaled by a
       routine in the call tree of this routine.

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

   3)  If an error (typically cell overflow) occurs while performing
       window arithmetic, the error is signaled by a routine
       in the call tree of this routine.

   4)  If the relational operator `relate' is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   5)  If the aberration correction specifier contains an
       unrecognized value, an error is signaled by a routine in the
       call tree of this routine.

   6)  If `adjust' is negative, an error is signaled by a routine in
       the call tree of this routine.

   7)  If either of the input body names do not map to NAIF ID
       codes, an error is signaled by a routine in the call tree of
       this routine.

   8)  If required ephemerides or other kernel data are not
       available, an error is signaled by a routine in the call tree
       of this routine.

   9)  If the number of intervals `nintvls' is less than 1, the error
       SPICE(VALUEOUTOFRANGE) is signaled.

   10) If the result window has size less than 2, the error
       SPICE(INVALIDDIMENSION) is signaled by a routine in the call
       tree of this routine.

   11) If the output SPICE window `result' has insufficient capacity
       to contain the number of intervals on which the specified
       distance condition is met, an error is signaled
       by a routine in the call tree of this routine.

   12) If any of the `target', `abcorr', `obsrvr' or `relate' input
       string pointers is null, the error SPICE(NULLPOINTER) is
       signaled.

   13) If any of the `target', `abcorr', `obsrvr' or `relate' input
       strings has zero length, the error SPICE(EMPTYSTRING) is
       signaled.

   14) If any the `cnfine' or `result' cell arguments has a type
       other than SpiceDouble, the error SPICE(TYPEMISMATCH) is
       signaled.

   15) If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled.

-Files

   Appropriate kernels must be loaded by the calling program before
   this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for target and observer for the
      time period defined by the confinement window must be
      loaded. If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data.
      Typically ephemeris data are made available by loading one
      or more SPK files via furnsh_c.

   -  If non-inertial reference frames are used, then PCK
      files, frame kernels, C-kernels, and SCLK kernels may be
      needed.

   -  In some cases the observer's state may be computed at times
      outside of `cnfine' by as much as 2 seconds; data required to
      compute this state must be provided by loaded kernels. See
      -Particulars for details.

   Kernel data are normally loaded once per program run, NOT every
   time this routine is called.

-Particulars

   This routine determines a set of one or more time intervals
   within the confinement window when the distance between the
   specified target and observer satisfies a caller-specified
   constraint. The resulting set of intervals is returned as a SPICE
   window.

   Below we discuss in greater detail aspects of this routine's
   solution process that are relevant to correct and efficient
   use of this routine in user applications.


   The Search Process
   ==================

   Regardless of the type of constraint selected by the caller, this
   routine starts the search for solutions by determining the time
   periods, within the confinement window, over which the
   distance function is monotone increasing and monotone
   decreasing. Each of these time periods is represented by a SPICE
   window. Having found these windows, all of the range rate
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

   The monotone windows (described above) are found via a two-step
   search process. Each interval of the confinement window is
   searched as follows: first, the input step size is the time
   separation at which the sign of the rate of change of distance
   ("range rate") is sampled. Starting at the left endpoint of the
   interval, samples will be taken at each step. If a change of sign
   is found, a root has been bracketed; at that point, the time at
   which the range rate is zero can be found by a refinement
   process, for example, via binary search.

   Note that the optimal choice of step size depends on the lengths
   of the intervals over which the distance function is monotone:
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
   to locate them. "Roots" include times when extrema are attained
   and times when the distance function is equal to a reference
   value or adjusted extremum. All endpoints of the intervals
   comprising the result window are either endpoints of intervals of
   the confinement window or roots.

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
   slow search of interest must be performed. See the "CASCADE"
   example program in gf.req for a demonstration.

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

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Find times during the first three months of the year 2007
      when the Earth-Moon distance is greater than 400000 km.
      Display the start and stop times of the time intervals
      over which this constraint is met, along with the Earth-Moon
      distance at each interval endpoint.

      We expect the Earth-Moon distance to be an oscillatory function
      with extrema roughly two weeks apart. Using a step size of one
      day will guarantee that the GF system will find all distance
      extrema. (Recall that a search for distance extrema is an
      intermediate step in the GF search process.)

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: gfdist_ex1.tm

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
            pck00008.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00008.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program gfdist_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Constants
         ./
         #define  TIMFMT  "YYYY MON DD HR:MN:SC.###"
         #define  MAXWIN  200
         #define  NINTVL  100
         #define  TIMLEN  41

         /.
         Local variables
         ./
         SpiceChar               begstr [ TIMLEN ];
         SpiceChar               endstr [ TIMLEN ];

         SPICEDOUBLE_CELL      ( cnfine, MAXWIN );
         SPICEDOUBLE_CELL      ( result, MAXWIN );

         SpiceDouble             adjust;
         SpiceDouble             dist;
         SpiceDouble             et0;
         SpiceDouble             et1;
         SpiceDouble             lt;
         SpiceDouble             pos    [3];
         SpiceDouble             refval;
         SpiceDouble             start;
         SpiceDouble             step;
         SpiceDouble             stop;

         SpiceInt                i;

         /.
         Load kernels.
         ./
         furnsh_c ( "gfdist_ex1.tm" );

         /.
         Store the time bounds of our search interval in
         the confinement window.
         ./
         str2et_c ( "2007 JAN 1", &et0 );
         str2et_c ( "2007 APR 1", &et1 );

         wninsd_c ( et0, et1, &cnfine );

         /.
         Search using a step size of 1 day (in units of
         seconds).  The reference value is 400000 km.
         We're not using the adjustment feature, so
         we set `adjust' to zero.
         ./
         step   = spd_c();
         refval = 4.e5;
         adjust = 0.0;

         /.
         Perform the search. The set of times when the
         constraint is met will be stored in the SPICE
         window `result'.
         ./
         gfdist_c ( "MOON", "NONE", "EARTH", ">",     refval,
                    adjust, step,   NINTVL,  &cnfine, &result );

         /.
         Display the results.
         ./
         if ( wncard_c(&result) == 0 )
         {
            printf ( "Result window is empty.\n\n" );
         }
         else
         {
            for ( i = 0;  i < wncard_c(&result);  i++ )
            {
               /.
               Fetch the endpoints of the Ith interval
               of the result window.
               ./
               wnfetd_c ( &result, i, &start, &stop );

               /.
               Check the distance at the interval's
               start and stop times.
               ./
               spkpos_c ( "MOON",  start, "J2000", "NONE",
                          "EARTH", pos,   &lt             );

               dist = vnorm_c(pos);

               timout_c ( start, TIMFMT, TIMLEN, begstr );

               printf ( "Start time, distance = %s %17.9f\n",
                        begstr, dist                          );

               spkpos_c ( "MOON",  stop, "J2000", "NONE",
                          "EARTH", pos,  &lt             );

               dist = vnorm_c(pos);

               timout_c ( stop, TIMFMT, TIMLEN, endstr );

               printf ( "Stop time,  distance = %s %17.9f\n",
                        endstr, dist                          );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Start time, distance = 2007 JAN 08 00:10:02.439  399999.999999989
      Stop time,  distance = 2007 JAN 13 06:36:42.770  400000.000000010
      Start time, distance = 2007 FEB 04 07:01:30.094  399999.999999990
      Stop time,  distance = 2007 FEB 10 09:29:56.659  399999.999999998
      Start time, distance = 2007 MAR 03 00:19:19.998  400000.000000006
      Stop time,  distance = 2007 MAR 10 14:03:33.312  400000.000000007
      Start time, distance = 2007 MAR 29 22:52:52.961  399999.999999995
      Stop time,  distance = 2007 APR 01 00:00:00.000  404531.955232216


      Note that the distance at the final solutions interval's stop
      time is not close to the reference value of 400000 km. This is
      because the interval's stop time was determined by the stop time
      of the confinement window.


   2) Extend the first example to demonstrate use of all supported
      relational operators. Find times when

         Earth-Moon distance is = 400000 km
         Earth-Moon distance is < 400000 km
         Earth-Moon distance is > 400000 km
         Earth-Moon distance is at a local minimum
         Earth-Moon distance is at the absolute minimum
         Earth-Moon distance is > the absolute minimum + 100 km
         Earth-Moon distance is at a local maximum
         Earth-Moon distance is at the absolute maximum
         Earth-Moon distance is > the absolute maximum - 100 km

      To shorten the search time and output, use the
      shorter search interval

         2007 JAN 15 00:00:00 UTC  to
         2007 MAR 15 00:00:00 UTC

      As before, use geometric (uncorrected) positions, so
      set the aberration correction flag to 'NONE'.

      Use the meta-kernel from the first example.

      Example code begins here.


      /.
         Program gfdist_ex2
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Constants
         ./
         #define  TIMFMT  "YYYY MON DD HR:MN:SC.###"
         #define  LNSIZE  81
         #define  MAXWIN  200
         #define  NINTVL  100
         #define  TIMLEN  41
         #define  NRELOP  9

         /.
         Local variables
         ./
         SpiceChar               begstr [ TIMLEN ];
         SpiceChar               endstr [ TIMLEN ];

         static ConstSpiceChar * relate [NRELOP] =
                                 {
                                    "=",
                                    "<",
                                    ">",
                                    "LOCMIN",
                                    "ABSMIN",
                                    "ABSMIN",
                                    "LOCMAX",
                                    "ABSMAX",
                                    "ABSMAX"
                                 };

         static ConstSpiceChar * templt [NRELOP] =
            {
               "Condition: distance = # km",
               "Condition: distance < # km",
               "Condition: distance > # km",
               "Condition: distance is a local minimum",
               "Condition: distance is the absolute minimum",
               "Condition: distance < the absolute minimum + * km",
               "Condition: distance is a local maximum",
               "Condition: distance is the absolute maximum",
               "Condition: distance > the absolute maximum - * km"
            };

         SpiceChar               title [ LNSIZE ];

         SPICEDOUBLE_CELL      ( cnfine, MAXWIN );
         SPICEDOUBLE_CELL      ( result, MAXWIN );

         static SpiceDouble      adjust [NRELOP] =
                                 {
                                    0.0,
                                    0.0,
                                    0.0,
                                    0.0,
                                    0.0,
                                    100.0,
                                    0.0,
                                    0.0,
                                    100.0
                                 };

         SpiceDouble             dist;
         SpiceDouble             et0;
         SpiceDouble             et1;
         SpiceDouble             lt;
         SpiceDouble             pos    [3];
         SpiceDouble             refval;
         SpiceDouble             start;
         SpiceDouble             step;
         SpiceDouble             stop;

         SpiceInt                i;
         SpiceInt                j;

         /.
         Load kernels.
         ./
         furnsh_c ( "gfdist_ex1.tm" );

         /.
         Store the time bounds of our search interval in
         the confinement window.
         ./
         str2et_c ( "2007 JAN 15", &et0 );
         str2et_c ( "2007 MAR 15", &et1 );

         wninsd_c ( et0, et1, &cnfine );

         /.
         Search using a step size of 1 day (in units of
         seconds). Use a reference value of 400000 km.
         ./
         refval = 400000.0;
         step   = spd_c();

         for ( i = 0;  i < NRELOP;  i++ )
         {
            gfdist_c ( "MOON",    "NONE", "EARTH", relate[i], refval,
                       adjust[i], step,   NINTVL,  &cnfine,   &result );

            /.
            Display the results.
            ./
            printf ( "\n" );

            /.
            Substitute the reference and adjustment values,
            where applicable, into the title string:
            ./
            repmd_c ( templt[i], "#", refval,    6, LNSIZE, title );
            repmd_c ( title,     "*", adjust[i], 6, LNSIZE, title );

            printf ( "%s\n", title );

            if ( wncard_c(&result) == 0 )
            {
               printf ( " Result window is empty.\n" );
            }
            else
            {
               printf ( " Result window:\n" );

               for ( j = 0;  j < wncard_c(&result);  j++ )
               {
                  /.
                  Fetch the endpoints of the jth interval
                  of the result window.
                  ./
                  wnfetd_c ( &result, j, &start, &stop );

                  /.
                  Check the distance at the interval's
                  start and stop times.
                  ./
                  spkpos_c ( "MOON",  start, "J2000", "NONE",
                             "EARTH", pos,   &lt             );

                  dist = vnorm_c(pos);

                  timout_c ( start, TIMFMT, TIMLEN, begstr );

                  printf ( "  Start time, distance = %s  %12.5f\n",
                           begstr, dist                          );

                  spkpos_c ( "MOON",  stop, "J2000", "NONE",
                             "EARTH", pos,  &lt             );

                  dist = vnorm_c(pos);

                  timout_c ( stop, TIMFMT, TIMLEN, endstr );

                  printf ( "  Stop time,  distance = %s  %12.5f\n",
                           endstr, dist                          );
               }
            }
         }
         printf ( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Condition: distance = 4.00000E+05 km
       Result window:
        Start time, distance = 2007 FEB 04 07:01:30.094  400000.00000
        Stop time,  distance = 2007 FEB 04 07:01:30.094  400000.00000
        Start time, distance = 2007 FEB 10 09:29:56.659  400000.00000
        Stop time,  distance = 2007 FEB 10 09:29:56.659  400000.00000
        Start time, distance = 2007 MAR 03 00:19:19.998  400000.00000
        Stop time,  distance = 2007 MAR 03 00:19:19.998  400000.00000
        Start time, distance = 2007 MAR 10 14:03:33.312  400000.00000
        Stop time,  distance = 2007 MAR 10 14:03:33.312  400000.00000

      Condition: distance < 4.00000E+05 km
       Result window:
        Start time, distance = 2007 JAN 15 00:00:00.000  393018.60991
        Stop time,  distance = 2007 FEB 04 07:01:30.094  400000.00000
        Start time, distance = 2007 FEB 10 09:29:56.659  400000.00000
        Stop time,  distance = 2007 MAR 03 00:19:19.998  400000.00000
        Start time, distance = 2007 MAR 10 14:03:33.312  400000.00000
        Stop time,  distance = 2007 MAR 15 00:00:00.000  376255.45393

      Condition: distance > 4.00000E+05 km
       Result window:
        Start time, distance = 2007 FEB 04 07:01:30.094  400000.00000
        Stop time,  distance = 2007 FEB 10 09:29:56.659  400000.00000
        Start time, distance = 2007 MAR 03 00:19:19.998  400000.00000
        Stop time,  distance = 2007 MAR 10 14:03:33.312  400000.00000

      Condition: distance is a local minimum
       Result window:
        Start time, distance = 2007 JAN 22 12:30:49.458  366925.80411
        Stop time,  distance = 2007 JAN 22 12:30:49.458  366925.80411
        Start time, distance = 2007 FEB 19 09:36:29.968  361435.64681
        Stop time,  distance = 2007 FEB 19 09:36:29.968  361435.64681

      Condition: distance is the absolute minimum
       Result window:
        Start time, distance = 2007 FEB 19 09:36:29.968  361435.64681
        Stop time,  distance = 2007 FEB 19 09:36:29.968  361435.64681

      Condition: distance < the absolute minimum + 1.00000E+02 km
       Result window:
        Start time, distance = 2007 FEB 19 01:09:52.706  361535.64681
        Stop time,  distance = 2007 FEB 19 18:07:45.136  361535.64681

      Condition: distance is a local maximum
       Result window:
        Start time, distance = 2007 FEB 07 12:38:29.870  404992.42429
        Stop time,  distance = 2007 FEB 07 12:38:29.870  404992.42429
        Start time, distance = 2007 MAR 07 03:37:02.122  405853.45213
        Stop time,  distance = 2007 MAR 07 03:37:02.122  405853.45213

      Condition: distance is the absolute maximum
       Result window:
        Start time, distance = 2007 MAR 07 03:37:02.122  405853.45213
        Stop time,  distance = 2007 MAR 07 03:37:02.122  405853.45213

      Condition: distance > the absolute maximum - 1.00000E+02 km
       Result window:
        Start time, distance = 2007 MAR 06 15:56:00.957  405753.45213
        Stop time,  distance = 2007 MAR 07 15:00:38.674  405753.45213


-Restrictions

   1)  The kernel files to be used by this routine must be loaded
       (normally via the CSPICE routine furnsh_c) before this routine
       is called.

   2)  This routine has the side effect of re-initializing the
       distance quantity utility package.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 01-NOV-2021 (JDR)

       Updated short error message for consistency within CSPICE wrapper
       interface: MALLOCFAILURE -> MALLOCFAILED.

       Updated header to describe use of expanded confinement window.

       Edited the header to comply with NAIF standard.

       Modified the output resolution for the distances in code example #2 to
       fit in the -Examples section without modifications. Renamed example's
       meta-kernel.

       Updated the description of "nintvls", "cnfine" and "result" arguments.

   -CSPICE Version 1.0.1, 28-FEB-2013 (NJB) (EDW)

       Header was updated to discuss use of gfstol_c. A
       header typo was corrected.

   -CSPICE Version 1.0.0, 15-APR-2009 (NJB) (EDW)

-Index_Entries

   GF distance search

-&
*/

{ /* Begin gfdist_c */


   /*
   Static local variables
   */
   static SpiceInt         nw  =  SPICE_GF_NWDIST;

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
   chkin_c ( "gfdist_c" );


   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfdist_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary.
   */
   CELLINIT2 ( cnfine, result );

   /*
   Check the input strings to make sure each pointer is non-null
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfdist_c", target );
   CHKFSTR ( CHK_STANDARD, "gfdist_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "gfdist_c", obsrvr );
   CHKFSTR ( CHK_STANDARD, "gfdist_c", relate );

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
      chkout_c ( "gfdist_c"                                     );
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
      chkout_c ( "gfdist_c"                                       );
      return;
   }

   /*
   Let the f2'd routine do the work.
   */
   gfdist_ ( ( char          * ) target,
             ( char          * ) abcorr,
             ( char          * ) obsrvr,
             ( char          * ) relate,
             ( doublereal    * ) &refval,
             ( doublereal    * ) &adjust,
             ( doublereal    * ) &step,
             ( doublereal    * ) (cnfine->base),
             ( integer       * ) &worksz,
             ( integer       * ) &nw,
             ( doublereal    * ) work,
             ( doublereal    * ) (result->base),
             ( ftnlen          ) strlen(target),
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


   chkout_c ( "gfdist_c" );

} /* End gfdist_c */
