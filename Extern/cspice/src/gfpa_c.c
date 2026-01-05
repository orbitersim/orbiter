/*

-Procedure gfpa_c ( GF, phase angle search )

-Abstract

   Determine time intervals for which a specified constraint
   on the phase angle between an illumination source, a target,
   and observer body centers is met.

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
   #include "SpiceGF.h"
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "zzalloc.h"

   void gfpa_c ( ConstSpiceChar     * target,
                 ConstSpiceChar     * illmn,
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
              P   Default convergence tolerance.
   target     I   Name of the target body.
   illmn      I   Name of the illuminating body.
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
               a string containing the integer ID code for the object.
               For example both "MOON" and "301" are legitimate strings
               that indicate the Moon is the target body.

               Case and leading or trailing blanks are not significant
               in the string `target'.

   illmn       is the name of the illuminating body. This will normally
               be "SUN" but the algorithm can use any ephemeris object.

               Case and leading or trailing blanks are not significant
               in the string `illmn'.

   abcorr      is the description of the aberration corrections to apply
               to the state evaluations to account for one-way light
               time and stellar aberration.

               This routine accepts only reception mode aberration
               corrections. See the header of spkezr_c for a detailed
               description of the aberration correction options. For
               convenience, the allowed aberration options are listed
               below:

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

                 "CN+S"      "Reception" case: converged
                             Newtonian light time and stellar
                             aberration corrections.

               Case and leading or trailing blanks are not significant
               in the string `abcorr'.

   obsrvr      is the name of an observing body. Optionally, you may
               supply a string containing the integer ID code for the
               object. For example both "MOON" and "301" are legitimate
               strings that indicate the Moon is the observer.

               Case and leading or trailing blanks are not significant
               in the string `obsrvr'.

   relate      is a relational operator used to define a constraint on a
               specified phase angle. The result window found by this
               routine indicates the time intervals where the constraint
               is satisfied. Supported values of `relate' and
               corresponding meanings are shown below:

                  ">"        The phase angle value is greater than the
                             reference value `refval'.

                  "="        The phase angle value is equal to the
                             reference value `refval'.

                  "<"        The phase angle value is less than the
                             reference value `refval'.

                  "ABSMAX"   The phase angle value is at an absolute
                             maximum.

                  "ABSMIN"   The phase angle value is at an absolute
                             minimum.

                  "LOCMAX"   The phase angle value is at a local
                             maximum.

                  "LOCMIN"   The phase angle value is at a local
                             minimum.

               `relate' may be used to specify an "adjusted" absolute
               extremum constraint: this requires the phase angle to be
               within a specified offset relative to an absolute
               extremum. The argument `adjust' (described below) is used
               to specify this offset.

               Local extrema are considered to exist only in the
               interiors of the intervals comprising the confinement
               window:  a local extremum cannot exist at a boundary
               point of the confinement window.

               Case and leading or trailing blanks are not
               significant in the string `relate'.

   refval      is the double precision reference value used together
               with the argument `relate' to define an equality or
               inequality to be satisfied by the phase angle. See the
               discussion of `relate' above for further information.

               The units of `refval' are radians.

   adjust      is a double precision value used to modify searches for
               absolute extrema: when `relate' is set to "ABSMAX" or
               "ABSMIN" and `adjust' is set to a positive value, gfpa_c
               finds times when the phase angle is within `adjust' radians
               of the specified extreme value.

               If `adjust' is non-zero and a search for an absolute
               minimum `min' is performed, the result window contains
               time intervals when the phase angle has values between
                `min' and min+adjust.

               If the search is for an absolute maximum `max', the
               corresponding range is from max-adjust to `max'.

               `adjust' is not used for searches for local extrema,
               equality or inequality conditions.

   step        is the step size to be used in the search. `step' must
               be shorter than any maximal time interval on which the
               specified phase angle function is monotone increasing or
               decreasing. That is, if the confinement window is
               partitioned into alternating intervals on which the
               phase angle function is either monotone increasing or
               decreasing, `step' must be shorter than any of these
               intervals.

               However, `step' must not be *too* short, or the search
               will take an unreasonable amount of time.

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

               The endpoints of the time intervals comprising `cnfine' are
               interpreted as seconds past J2000 TDB.

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
               confinement window `cnfine', on which the specified phase
               angle constraint is satisfied.

               `result' must be declared and initialized with sufficient
               size to capture the full set of time intervals within the
               search region on which the specified condition is satisfied.

               If `result' is non-empty on input, its contents will be
               discarded before gfpa_c conducts its search.

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
               SPICE_GF_CNVTOL is used to determine when binary
               searches for roots should terminate: when a root is
               bracketed within an interval of length SPICE_GF_CNVTOL,
               the root is considered to have been found.

               The accuracy, as opposed to precision, of roots found by
               this routine depends on the accuracy of the input data.
               In most cases, the accuracy of solutions will be
               inferior to their precision.

               SPICE_GF_CNVTOL is declared in the header file
               SpiceGF.h.

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

          - truncation error in time values
          - finite tolerance value
          - errors in computed geometric quantities

       it is *normal* for the condition of interest to not always be
       satisfied near the endpoints of the intervals comprising the
       `result' window. One technique to handle such a situation,
       slightly contract `result' using the window routine wncond_c.

   3)  If the number of intervals `nintvls' is less than 1, the error
       SPICE(VALUEOUTOFRANGE) is signaled.

   4)  If result window, `result', is not at least 2 and an even value,
       the error SPICE(INVALIDDIMENSION) is signaled by a routine in
       the call tree of this routine is signaled.

   5)  If `result' has insufficient capacity to contain the
       number of intervals on which the specified angle condition
       is met, an error is signaled by a routine in the call
       tree of this routine.

   6)  If an error (typically cell overflow) occurs during
       window arithmetic, the error is signaled by a routine
       in the call tree of this routine.

   7)  If the relational operator `relate' is not recognized, an
       error is signaled by a routine in the call tree of this
       routine.

   8)  If `adjust' is negative, an error is signaled by a routine in
       the call tree of this routine.

   9)  If `adjust' has a non-zero value when `relate' has any value other
       than "ABSMIN" or "ABSMAX", an error is signaled by a routine
       in the call tree of this routine.

   10) If any of the input body names, `target', `illmn', `obsrvr', do
       not map to NAIF ID codes, an error is signaled by a routine
       in the call tree of this routine.

   11) If the input body names, `target', `illmn', `obsrvr', are not
       distinct, an error is signaled by a routine in the call
       tree of this routine.

   12) If required ephemerides or other kernel data are not
       available, an error is signaled by a routine in the call tree
       of this routine.

   13) If the aberration correction specifier contains an
       unrecognized value, an error is signaled by a routine in the
       call tree of this routine.

   14) If a transmit mode aberration correction is requested, an
       error is signaled by a routine in the call tree of this
       routine.

   15) If any of the `target', `illmn', `abcorr', `obsrvr' or
       `relate' input string pointers is null, the error
       SPICE(NULLPOINTER) is signaled.

   16) If any of the `target', `illmn', `abcorr', `obsrvr' or
       `relate' input strings has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   17) If any the `cnfine' or `result' cell arguments has a type
       other than SpiceDouble, the error SPICE(TYPEMISMATCH) is
       signaled.

   18) If memory cannot be allocated to create the temporary variable
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

   -  In some cases the observer's state may be computed at times
      outside of `cnfine' by as much as 2 seconds; data required to
      compute this state must be provided by loaded kernels. See
      -Particulars for details.

   Kernel data are normally loaded once per program run, NOT every
   time this routine is called.

-Particulars

                      illmn      OBS
      illmn as seen      *       /
      from TARG at       |      /
      et - lt.           |     /
                        >|..../< phase angle
                         |   /
                       . |  /
                     .   | /
                    .     *     TARG as seen from OBS
              SEP   .   TARG    at `et'
                     .  /
                       /
                      *

   This routine determines if the caller-specified constraint
   condition on the geometric event (phase angle) is satisfied for
   any time intervals within the confinement window `cnfine'. If one
   or more such time intervals exist, those intervals are added
   to the `result' window.

   This routine provides a simpler, but less flexible interface
   than does the routine gfevnt_c for conducting searches for
   illuminator-target-observer phase angle value events.
   Applications that require support for progress reporting,
   interrupt handling, non-default step or refinement functions
   should call gfevnt_c rather than this routine.

   Below we discuss in greater detail aspects of this routine's
   solution process that are relevant to correct and efficient
   use of this routine in user applications.


   The Search Process
   ==================

   Regardless of the type of constraint selected by the caller, this
   routine starts the search for solutions by determining the time
   periods, within the confinement window, over which the
   phase angle function is monotone increasing and monotone
   decreasing. Each of these time periods is represented by a SPICE
   window. Having found these windows, all of the phase angle
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
   change of phase angle will be sampled. Starting at
   the left endpoint of an interval, samples will be taken at each
   step. If a change of sign is found, a root has been bracketed; at
   that point, the time at which the time derivative of the
   phase angle is zero can be found by a refinement process, for
   example, using a binary search.

   Note that the optimal choice of step size depends on the lengths
   of the intervals over which the phase angle function is monotone:
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
   illumination source, and observer can be a valuable aid in
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
   attained and times when the geometric quantity function is equal
   to a reference value. All endpoints of the intervals comprising
   the result window are either endpoints of intervals of the
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

   1) Determine the time windows from December 1, 2006 UTC to
      January 31, 2007 UTC for which the sun-moon-earth configuration
      phase angle satisfies the relation conditions with respect to a
      reference value of .57598845 radians (the phase angle at
      January 1, 2007 00:00:00.000 UTC, 33.001707 degrees). Also
      determine the time windows corresponding to the local maximum and
      minimum phase angles, and the absolute maximum and minimum phase
      angles during the search interval. The configuration defines the
      sun as the illuminator, the moon as the target, and the earth as
      the observer.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: gfpa_ex1.tm

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
            pck00009.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00009.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program gfpa_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      #define  TIMFMT  "YYYY MON DD HR:MN:SC.###"
      #define  NINTVL  5000
      #define  TIMLEN  41
      #define  NLOOPS  7

      int main()
         {

         /.
         Local variables
         ./
         SpiceChar               begstr [ TIMLEN ];
         SpiceChar               endstr [ TIMLEN ];

         SPICEDOUBLE_CELL      ( cnfine, 2 );
         SPICEDOUBLE_CELL      ( result, NINTVL*2 );

         SpiceDouble             adjust;
         SpiceDouble             et0;
         SpiceDouble             et1;
         SpiceDouble             phaseq;
         SpiceDouble             refval;
         SpiceDouble             start;
         SpiceDouble             step;
         SpiceDouble             stop;
         SpiceInt                i;
         SpiceInt                j;

         /.
         Define the values for target, observer, illuminator, and
         aberration correction.
         ./

         ConstSpiceChar * target = "moon";
         ConstSpiceChar * illmn  = "sun";
         ConstSpiceChar * abcorr = "lt+s";
         ConstSpiceChar * obsrvr = "earth";

         ConstSpiceChar * relate [NLOOPS] = { "=",
                                              "<",
                                              ">",
                                              "LOCMIN",
                                              "ABSMIN",
                                              "LOCMAX",
                                              "ABSMAX",
                                            };

         /.
         Load kernels.
         ./
         furnsh_c ( "gfpa_ex1.tm" );

         /.
         Store the time bounds of our search interval in
         the confinement window.
         ./
         str2et_c ( "2006 DEC 01", &et0 );
         str2et_c ( "2007 JAN 31", &et1 );

         wninsd_c ( et0, et1, &cnfine );

         /.
         Search using a step size of 1 day (in units of seconds).
         The reference value is 0.57598845 radians. We're not using the
         adjustment feature, so we set ADJUST to zero.
         ./
         step   = spd_c();
         refval = 0.57598845;
         adjust = 0.0;

         for ( j = 0;  j < NLOOPS;  j++ )
            {

            printf ( "Relation condition: %s\n",  relate[j] );

            /.
            Perform the search. The SPICE window `result' contains
            the set of times when the condition is met.
            ./
            gfpa_c ( target,    illmn,   abcorr, obsrvr,
                     relate[j], refval,  adjust, step,
                     NINTVL,    &cnfine, &result        );

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

                  phaseq = phaseq_c ( start, target, illmn, obsrvr, abcorr );

                  timout_c ( start, TIMFMT, TIMLEN, begstr );
                  printf ( "Start time = %s %16.9f\n", begstr, phaseq );

                  phaseq = phaseq_c ( stop, target, illmn, obsrvr, abcorr );

                  timout_c ( stop, TIMFMT, TIMLEN, endstr );
                  printf ( "Stop time  = %s %16.9f\n", endstr, phaseq );
                  }

               printf("\n");

               }

            }

         return ( 0 );
         }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Relation condition: =
      Start time = 2006 DEC 02 13:31:34.414      0.575988450
      Stop time  = 2006 DEC 02 13:31:34.414      0.575988450
      Start time = 2006 DEC 07 14:07:55.470      0.575988450
      Stop time  = 2006 DEC 07 14:07:55.470      0.575988450
      Start time = 2006 DEC 31 23:59:59.997      0.575988450
      Stop time  = 2006 DEC 31 23:59:59.997      0.575988450
      Start time = 2007 JAN 06 08:16:25.512      0.575988450
      Stop time  = 2007 JAN 06 08:16:25.512      0.575988450
      Start time = 2007 JAN 30 11:41:32.557      0.575988450
      Stop time  = 2007 JAN 30 11:41:32.557      0.575988450

      Relation condition: <
      Start time = 2006 DEC 02 13:31:34.414      0.575988450
      Stop time  = 2006 DEC 07 14:07:55.470      0.575988450
      Start time = 2006 DEC 31 23:59:59.997      0.575988450
      Stop time  = 2007 JAN 06 08:16:25.512      0.575988450
      Start time = 2007 JAN 30 11:41:32.557      0.575988450
      Stop time  = 2007 JAN 31 00:00:00.000      0.468279091

      Relation condition: >
      Start time = 2006 DEC 01 00:00:00.000      0.940714974
      Stop time  = 2006 DEC 02 13:31:34.414      0.575988450
      Start time = 2006 DEC 07 14:07:55.470      0.575988450
      Stop time  = 2006 DEC 31 23:59:59.997      0.575988450
      Start time = 2007 JAN 06 08:16:25.512      0.575988450
      Stop time  = 2007 JAN 30 11:41:32.557      0.575988450

      Relation condition: LOCMIN
      Start time = 2006 DEC 05 00:16:50.317      0.086121423
      Stop time  = 2006 DEC 05 00:16:50.317      0.086121423
      Start time = 2007 JAN 03 14:18:31.977      0.079899769
      Stop time  = 2007 JAN 03 14:18:31.977      0.079899769

      Relation condition: ABSMIN
      Start time = 2007 JAN 03 14:18:31.977      0.079899769
      Stop time  = 2007 JAN 03 14:18:31.977      0.079899769

      Relation condition: LOCMAX
      Start time = 2006 DEC 20 14:09:10.392      3.055062862
      Stop time  = 2006 DEC 20 14:09:10.392      3.055062862
      Start time = 2007 JAN 19 04:27:54.600      3.074603891
      Stop time  = 2007 JAN 19 04:27:54.600      3.074603891

      Relation condition: ABSMAX
      Start time = 2007 JAN 19 04:27:54.600      3.074603891
      Stop time  = 2007 JAN 19 04:27:54.600      3.074603891


-Restrictions

   1)  The kernel files to be used by this routine must be loaded
       (normally using the CSPICE routine furnsh_c) before this
       routine is called.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 01-NOV-2021 (JDR) (EDW)

       Updated short error message for consistency within CSPICE wrapper
       interface: MALLOCFAILURE -> MALLOCFAILED.

       Added use of ALLOC_CHECK_INTRA to check net null effect on
       alloc count.

       Updated header to describe use of expanded confinement window.

       Edited the header to comply with NAIF standard.

       Updated the description of "nintvls", "cnfine" and "result" arguments.

       Replaced entry #8 by new entries #8 and #9, and added entry #11
       in -Exceptions section.

   -CSPICE Version 1.0.0, 15-JUL-2014 (EDW) (NJB)

-Index_Entries

   GF phase angle search

-&
*/

{ /* Begin gfpa_c */

   /*
   Static local variables
   */
   static SpiceInt         nw  =  SPICE_GF_NWPA;

   /*
   Local variables
   */
   doublereal            * work;

   int                     nowalloc;

   SpiceInt                nBytes;


   /*
   Participate in error tracing.
   */
   if ( return_c() )
      {
      return;
      }
   chkin_c ( "gfpa_c" );


   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfpa_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary.
   */
   CELLINIT2 ( cnfine, result );

   /*
   Check the input strings to make sure each pointer is non-null
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfpa_c", target );
   CHKFSTR ( CHK_STANDARD, "gfpa_c", illmn  );
   CHKFSTR ( CHK_STANDARD, "gfpa_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "gfpa_c", obsrvr );
   CHKFSTR ( CHK_STANDARD, "gfpa_c", relate );

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
      chkout_c ( "gfpa_c"                                       );
      return;
      }

   /*
   Allocate the workspace.

   We have `nw' "doublereal" cells, each having cell size 2*nintvls.
   Each cell also has a control area containing SPICE_CELL_CTRLSZ
   double precision values.
   */

   nowalloc = alloc_count();

   nintvls = nintvls * 2;

   nBytes  = ( nintvls + SPICE_CELL_CTRLSZ ) * nw * sizeof(SpiceDouble);

   work    = (doublereal *) alloc_SpiceMemory( nBytes );

   if ( !work )
      {
      setmsg_c ( "Workspace allocation of # bytes failed due to "
                 "malloc failure"                                 );
      errint_c ( "#",  nBytes                                     );
      sigerr_c ( "SPICE(MALLOCFAILED)"                            );
      chkout_c ( "gfpa_c"                                         );
      return;
      }

   /*
   Let the f2'd routine do the work.
   */
   gfpa_ ( ( char          * ) target,
           ( char          * ) illmn,
           ( char          * ) abcorr,
           ( char          * ) obsrvr,
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
           ( ftnlen          ) strlen(illmn),
           ( ftnlen          ) strlen(abcorr),
           ( ftnlen          ) strlen(obsrvr),
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

   chkout_c ( "gfpa_c" );

} /* End gfpa_c */
