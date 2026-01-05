/*

-Procedure gfrr_c (GF, range rate search )

-Abstract

   Determine time intervals for which a specified constraint
   on the observer-target range rate is met.

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

   void gfrr_c ( ConstSpiceChar     * target,
                 ConstSpiceChar     * abcorr,
                 ConstSpiceChar     * obsrvr,
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
               that indicate the moon is the target body.

               The target and observer define a position vector that
               points from the observer to the target. The derivative
               with respect to time of the length of this vector is the
               "range rate" used by this routine as the geometric
               quantity of interest.

               Case and leading or trailing blanks are not significant
               in the string `target'.

   abcorr      is the description of the aberration corrections to apply
               to the state evaluations to account for one-way light
               time and stellar aberration.

               This routine accepts the same aberration corrections as
               does the SPICE routine spkezr_c. See the header of spkezr_c
               for a detailed description of the aberration correction
               options. For convenience, the options are listed below:

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

               Case and leading or trailing blanks are not significant
               in the string `abcorr'.

   obsrvr      is the name of an observing body. Optionally, you may
               supply the ID code of the object as an integer string.
               For example, both "EARTH" and "399" are legitimate
               strings to indicate that the observer is the Earth.

               Case and leading or trailing blanks are not significant
               in the string `obsrvr'.

   relate      is the relational operator that defines the constraint on
               the range rate of the observer-target vector. The result
               window found by this routine indicates the time intervals
               where the constraint is satisfied. Supported values of
               `relate' and corresponding meanings are shown below:

                  ">"        The range rate value is greater than the
                             reference value `refval'.

                  "="        The range rate value is equal to the
                             reference value `refval'.

                  "<"        The range rate value is less than the
                             reference value `refval'.

                  "ABSMAX"   The range rate value is at an absolute
                             maximum.

                  "ABSMIN"   The range rate value is at an absolute
                             minimum.

                  "LOCMAX"   The range rate value is at a local
                             maximum.

                  "LOCMIN"   The range rate value is at a local
                             minimum.

               `relate' may be used to specify an "adjusted" absolute
               extremum constraint: this requires the range rate to be
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
               inequality to satisfy by the range rate of the
               observer-target vector. See the discussion of `relate'
               above for further information.

               The units of `refval' are km/s.

   adjust      is a double precision value used to modify searches for
               absolute extrema: when `relate' is set to "ABSMAX" or
               "ABSMIN" and `adjust' is set to a positive value, gfrr_c
               finds times when the range rate is within `adjust'
               kilometers/second of the specified extreme value.

               For `relate' set to "ABSMAX", the `result' window contains
               time intervals when the range rate has
               values between absmax - adjust and `absmax'.

               For `relate' set to "ABSMIN", the `result' window contains
               time intervals when the range rate has
               values between `absmin' and absmin + adjust.

               `adjust' is not used for searches for local extrema,
               equality or inequality conditions.

   step        is the double precision time step size to use in the
               search.

               `step' must be short enough for a search using this step
               size to locate the time intervals where the range rate
               function is monotone increasing or decreasing. However,
               `step' must not be *too* short, or the search will take an
               unreasonable amount of time.

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
               discarded before gfrr_c conducts its search.

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
              within an interval of length SPICE_GF_CNVTOL, the root is
              considered to have been found.

              The accuracy, as opposed to precision, of roots found
              by this routine depends on the accuracy of the input
              data. In most cases, the accuracy of solutions will be
              inferior to their precision.

              SPICE_GF_CNVTOL is declared in the header file SpiceGF.h.

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

   4)  If the size of the SPICE window `result' is less than 2 or not
       an even value, the error SPICE(INVALIDDIMENSION) is signaled
       by a routine in the call tree of this routine.

   5)  If the SPICE window `result' has insufficient capacity to
       contain the number of intervals on which the specified
       distance condition is met, an error is signaled by a routine
       in the call tree of this routine.

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

   10) If `adjust' has a non-zero value when `relate' has any value other
       than "ABSMIN" or "ABSMAX", an error is signaled by a routine
       in the call tree of this routine.

   11) If either of the input body names do not map to NAIF ID
       codes, an error is signaled by a routine in the call tree of
       this routine.

   12) If required ephemerides or other kernel data are not
       available, an error is signaled by a routine in the call tree
       of this routine.

   13) If any of the `target', `abcorr', `obsrvr' or `relate' input
       string pointers is null, the error SPICE(NULLPOINTER) is
       signaled.

   14) If any of the `target', `abcorr', `obsrvr' or `relate' input
       strings has zero length, the error SPICE(EMPTYSTRING) is
       signaled.

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

   -  In some cases the observer's state may be computed at times
      outside of `cnfine' by as much as 2 seconds; data required to
      compute this state must be provided by loaded kernels. See
      -Particulars for details.

   Kernel data are normally loaded once per program run, NOT every
   time this routine is called.

-Particulars

   This routine determines if the caller-specified constraint
   condition on the geometric event (range rate) is satisfied for
   any time intervals within the confinement window `cnfine'. If one
   or more such time intervals exist, those intervals are added
   to the `result' window.

   This routine provides a simpler, but less flexible interface
   than does the routine gfevnt_c for conducting searches for
   observer-target range rate value events. Applications that
   require support for progress reporting, interrupt handling,
   non-default step or refinement functions, or non-default
   convergence tolerance should call gfevnt_c rather than this routine.

   Below we discuss in greater detail aspects of this routine's
   solution process that are relevant to correct and efficient
   use of this routine in user applications.


   The Search Process
   ==================

   Regardless of the type of constraint selected by the caller, this
   routine starts the search for solutions by determining the time
   periods, within the confinement window, over which the
   range rate function is monotone increasing and monotone
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

   The monotone windows (described above) are found using a two-step
   search process. Each interval of the confinement window is
   searched as follows: first, the input step size is used to
   determine the time separation at which the sign of the rate of
   change of range rate will be sampled. Starting at
   the left endpoint of an interval, samples will be taken at each
   step. If a change of sign is found, a root has been bracketed; at
   that point, the time at which the time derivative of the
   range rate is zero can be found by a refinement process, for
   example, using a binary search.

   Note that the optimal choice of step size depends on the lengths
   of the intervals over which the range rate function is monotone:
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
   attained and times when the range rate function is equal to a
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

   1) Determine the time windows from January 1, 2007 UTC to
      April 1, 2007 UTC for which the sun-moon range rate satisfies the
      relation conditions with respect to a reference value of
      0.3365 km/s radians (this range rate known to occur within the
      search interval). Also determine the time windows corresponding
      to the local maximum and minimum range rate, and the absolute
      maximum and minimum range rate during the search interval.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: gfrr_ex1.tm

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
         Program gfrr_ex1
      ./
      #include <stdio.h>
      #include <stdlib.h>
      #include <string.h>
      #include "SpiceUsr.h"

      #define       MAXWIN    20000
      #define       TIMFMT    "YYYY-MON-DD HR:MN:SC.###"
      #define       TIMLEN    41
      #define       NLOOPS    7

      int main( )
         {

         /.
         Create the needed windows. Note, one window
         consists of two values, so the total number
         of cell values to allocate is twice
         the number of intervals.
         ./
         SPICEDOUBLE_CELL ( result, 2*MAXWIN );
         SPICEDOUBLE_CELL ( cnfine, 2        );

         SpiceDouble       begtim;
         SpiceDouble       endtim;
         SpiceDouble       step;
         SpiceDouble       adjust;
         SpiceDouble       refval;
         SpiceDouble       beg;
         SpiceDouble       end;

         SpiceChar         begstr [ TIMLEN ];
         SpiceChar         endstr [ TIMLEN ];

         SpiceChar       * target = "MOON";
         SpiceChar       * abcorr = "NONE";
         SpiceChar       * obsrvr = "SUN";

         SpiceInt          count;
         SpiceInt          i;
         SpiceInt          j;

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
         furnsh_c( "gfrr_ex1.tm" );

         /.
         Store the time bounds of our search interval in
         the cnfine confinement window.
         ./
         str2et_c( "2007 JAN 01", &begtim );
         str2et_c( "2007 APR 01", &endtim );

         wninsd_c ( begtim, endtim, &cnfine );

         /.
         Search using a step size of 1 day (in units of seconds).
         The reference value is .3365 km/s. We're not using the
         adjustment feature, so we set 'adjust' to zero.
         ./
         step   = spd_c();
         adjust = 0.;
         refval = .3365;

         for ( j = 0;  j < NLOOPS;  j++ )
            {

            printf ( "Relation condition: %s \n",  relate[j] );

            /.
            Perform the search. The SPICE window 'result' contains
            the set of times when the condition is met.
            ./
            gfrr_c ( target,
                     abcorr,
                     obsrvr,
                     relate[j],
                     refval,
                     adjust,
                     step,
                     MAXWIN,
                     &cnfine,
                     &result );

            count = wncard_c( &result );

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
                  wnfetd_c ( &result, i, &beg, &end );

                  timout_c ( beg, TIMFMT, TIMLEN, begstr );
                  timout_c ( end, TIMFMT, TIMLEN, endstr );

                  printf ( "Start time, drdt = %s \n", begstr );
                  printf ( "Stop time,  drdt = %s \n", endstr );

                  }

               }

            printf("\n");

            }

         return( 0 );
         }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Relation condition: =
      Start time, drdt = 2007-JAN-02 00:35:19.571
      Stop time,  drdt = 2007-JAN-02 00:35:19.571
      Start time, drdt = 2007-JAN-19 22:04:54.897
      Stop time,  drdt = 2007-JAN-19 22:04:54.897
      Start time, drdt = 2007-FEB-01 23:30:13.427
      Stop time,  drdt = 2007-FEB-01 23:30:13.427
      Start time, drdt = 2007-FEB-17 11:10:46.538
      Stop time,  drdt = 2007-FEB-17 11:10:46.538
      Start time, drdt = 2007-MAR-04 15:50:19.929
      Stop time,  drdt = 2007-MAR-04 15:50:19.929
      Start time, drdt = 2007-MAR-18 09:59:05.957
      Stop time,  drdt = 2007-MAR-18 09:59:05.957

      Relation condition: <
      Start time, drdt = 2007-JAN-02 00:35:19.571
      Stop time,  drdt = 2007-JAN-19 22:04:54.897
      Start time, drdt = 2007-FEB-01 23:30:13.427
      Stop time,  drdt = 2007-FEB-17 11:10:46.538
      Start time, drdt = 2007-MAR-04 15:50:19.929
      Stop time,  drdt = 2007-MAR-18 09:59:05.957

      Relation condition: >
      Start time, drdt = 2007-JAN-01 00:00:00.000
      Stop time,  drdt = 2007-JAN-02 00:35:19.571
      Start time, drdt = 2007-JAN-19 22:04:54.897
      Stop time,  drdt = 2007-FEB-01 23:30:13.427
      Start time, drdt = 2007-FEB-17 11:10:46.538
      Stop time,  drdt = 2007-MAR-04 15:50:19.929
      Start time, drdt = 2007-MAR-18 09:59:05.957
      Stop time,  drdt = 2007-APR-01 00:00:00.000

      Relation condition: LOCMIN
      Start time, drdt = 2007-JAN-11 07:03:58.991
      Stop time,  drdt = 2007-JAN-11 07:03:58.991
      Start time, drdt = 2007-FEB-10 06:26:15.441
      Stop time,  drdt = 2007-FEB-10 06:26:15.441
      Start time, drdt = 2007-MAR-12 03:28:36.404
      Stop time,  drdt = 2007-MAR-12 03:28:36.404

      Relation condition: ABSMIN
      Start time, drdt = 2007-JAN-11 07:03:58.991
      Stop time,  drdt = 2007-JAN-11 07:03:58.991

      Relation condition: LOCMAX
      Start time, drdt = 2007-JAN-26 02:27:33.762
      Stop time,  drdt = 2007-JAN-26 02:27:33.762
      Start time, drdt = 2007-FEB-24 09:35:07.812
      Stop time,  drdt = 2007-FEB-24 09:35:07.812
      Start time, drdt = 2007-MAR-25 17:26:56.148
      Stop time,  drdt = 2007-MAR-25 17:26:56.148

      Relation condition: ABSMAX
      Start time, drdt = 2007-MAR-25 17:26:56.148
      Stop time,  drdt = 2007-MAR-25 17:26:56.148


-Restrictions

   1)  The kernel files to be used by this routine must be loaded
       (normally using the CSPICE routine furnsh_c) before this
       routine is called.

   2)  This routine has the side effect of re-initializing the
       range rate quantity utility package. Callers may themselves
       need to re-initialize the range rate quantity utility
       package after calling this routine.

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

       Updated the description of "nintvls", "cnfine" and "result"
       arguments.

       Added entries #4 and #5 and replaced former entry #6 by
       new entries #9 and #10 in -Exceptions section.

   -CSPICE Version 1.0.2, 31-JUL-2014 (EDW)

       Edit to header, replaced ' character with character " to indicate
       C strings.

       Edit to header, correct Required Reading entry eliminating ".REQ"
       suffix.

   -CSPICE Version 1.0.1, 28-FEB-2013 (NJB) (EDW)

       Header was updated to discuss use of gfstol_c.

       Edit to comments to correct search description.

       Edits to Example section, proper description of "standard.tm"
       meta kernel.

   -CSPICE Version 1.0.0, 26-AUG-2009 (EDW) (NJB)

-Index_Entries

   GF range rate search

-&
*/

{ /* Begin gfrr_c */

   /*
   Local variables
   */
   doublereal            * work;

   static SpiceInt         nw = SPICE_GF_NWRR;

   int                     nowalloc;

   SpiceInt                nBytes;

   /*
   Participate in error tracing.
   */

   chkin_c ( "gfrr_c" );

   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfrr_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary.
   */
   CELLINIT2 ( cnfine, result );

   /*
   Check the input strings to make sure each pointer is non-null
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfrr_c", target );
   CHKFSTR ( CHK_STANDARD, "gfrr_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "gfrr_c", obsrvr );
   CHKFSTR ( CHK_STANDARD, "gfrr_c", relate );

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
      chkout_c ( "gfrr_c"                                   );
      return;
      }

   /*
   Allocate the workspace. 'nintvls' indicates the maximum number of
   intervals returned in 'result'. An interval consists of
   two values.
   */

   nowalloc = alloc_count();

   nintvls = 2 * nintvls;

   nBytes  = ( nintvls + SPICE_CELL_CTRLSZ ) * nw * sizeof(SpiceDouble);

   work    = (doublereal *) alloc_SpiceMemory( nBytes );

   if ( !work )
      {
      setmsg_c ( "Workspace allocation of # bytes failed due to "
                 "malloc failure"                               );
      errint_c ( "#",  nBytes                                   );
      sigerr_c ( "SPICE(MALLOCFAILED)"                          );
      chkout_c ( "gfrr_c"                                       );
      return;
      }

   /*
   Let the f2'd routine do the work.
   */

   gfrr_( ( char          * ) target,
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

   chkout_c ( "gfrr_c" );

} /* End gfrr_c */
