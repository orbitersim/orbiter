/*

-Procedure gfuds_c ( GF, user defined scalar )

-Abstract

   Perform a GF search on a user defined scalar quantity.

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
   WINDOWS

-Keywords

   EVENT
   GEOMETRY
   SEARCH
   WINDOW

*/

   #include <signal.h>
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #include "SpiceZfc.h"
   #include "SpiceZad.h"
   #include "SpiceZst.h"
   #include "zzalloc.h"
   #undef   gfuds_c

   void gfuds_c ( void             ( * udfuns ) ( SpiceDouble       et,
                                                  SpiceDouble     * value ),

                  void             ( * udqdec ) ( void ( * udfuns )
                                                       ( SpiceDouble   et,
                                                         SpiceDouble * value ),

                                                  SpiceDouble       et,
                                                  SpiceBoolean    * isdecr ),

                  ConstSpiceChar     * relate,
                  SpiceDouble          refval,
                  SpiceDouble          adjust,
                  SpiceDouble          step,
                  SpiceInt             nintvls,
                  SpiceCell          * cnfine,
                  SpiceCell          * result )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   SPICE_GF_CNVTOL
              P   Convergence tolerance.
   udfuns     I   Name of the routine that computes a scalar
                  quantity corresponding to an `et'.
   udqdec     I   Name of the routine that computes whether the
                  scalar quantity is decreasing.
   relate     I   Operator that either looks for an extreme value
                  (max, min, local, absolute) or compares the
                  geometric quantity value and a number.
   refval     I   Value used as reference for scalar quantity
                  condition.
   adjust     I   Allowed variation for absolute extremal
                  geometric conditions.
   step       I   Step size used for locating extrema and roots.
   nintvls    I   Workspace window interval count.
   cnfine    I-O  SPICE window to which the search is confined.
   result     O   SPICE window containing results.

-Detailed_Input

   udfuns      is the routine that returns the value of the scalar
               quantity of interest at time `et'. The prototype of
               `udfuns' is:

                  void   ( * udfuns ) ( SpiceDouble       et,
                                        SpiceDouble     * value )

               where:

                  et      is a double precision value representing
                          ephemeris time, expressed as seconds past
                          J2000 TDB, at which to determine the scalar
                          value.

                  value   is the value of the scalar quantity at `et'.

   udqdec      is the name of the routine that determines if the scalar
               quantity calculated by `udfuns' is decreasing. The prototype
               of `udqdec' is:

                 void    ( * udqdec ) ( void          ( * udfuns )
                                                    ( SpiceDouble   et,
                                                      SpiceDouble * value ),
                                        SpiceDouble       et,
                                        SpiceBoolean    * isdecr )

              where:

                  udfuns   is the name of the scalar function as defined
                           above.

                  et       is a double precision value representing
                           ephemeris time, expressed as seconds past
                           J2000 TDB, at which to determine the time
                           derivative of `udfuns'.

                  isdecr   is a logical output variable indicating whether
                           or not the scalar value returned by `udfuns' is
                           decreasing. `isdecr' returns SPICETRUE if the
                           time derivative of `udfuns' at `et' is negative.

   relate      is the scalar string comparison operator indicating
               the numeric constraint of interest. Values are:

                  ">"        value of scalar quantity greater than some
                             reference (refval).

                  "="        value of scalar quantity equal to some
                             reference (refval).

                  "<"        value of scalar quantity less than some
                             reference (refval).

                  "ABSMAX"   The scalar quantity is at an absolute
                             maximum.

                  "ABSMIN"   The scalar quantity is at an absolute
                             minimum.

                  "LOCMAX"   The scalar quantity is at a local
                             maximum.

                  "LOCMIN"   The scalar quantity is at a local
                             minimum.

               The caller may indicate that the region of interest
               is the set of time intervals where the quantity is
               within a specified distance of an absolute extremum.
               The argument `adjust' (described below) is used to
               specified this distance.

               Local extrema are considered to exist only in the
               interiors of the intervals comprising the confinement
               window:  a local extremum cannot exist at a boundary
               point of the confinement window.

               `relate' is insensitive to case, leading and
               trailing blanks.

   refval      is the reference value used to define an equality or
               inequality to  satisfied by the scalar quantity.
               The units of `refval' are those of the scalar quantity.

   adjust      is the amount by which the quantity is allowed to vary
               from an absolute extremum.

               If the search is for an absolute minimum is performed,
               the resulting window contains time intervals when the
               geometric quantity value has values between `absmin' and
               absmin + adjust.

               If the search is for an absolute maximum, the
               corresponding range is between absmax - adjust and
               `absmax'.

               `adjust' is not used for searches for local extrema,
               equality or inequality conditions and must have value
               zero for such searches.

   step        is the double precision time step size to use in
               the search.

               `step' must be short enough to for a search using this
               step size to locate the time intervals where the
               scalar quantity function is monotone increasing or
               decreasing. However, `step' must not be *too* short,
               or the search will take an unreasonable amount of time.

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

   cnfine      is a SPICE window that confines the time period over
               which the specified search is conducted. `cnfine' may
               consist of a single interval or a collection of
               intervals.

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

   result      is a SPICE window containing the time intervals within
               the confinement window, during which the specified
               condition on the scalar quantity is met.

               `result' must be declared and initialized with sufficient
               size to capture the full set of time intervals within the
               search region on which the specified condition is satisfied.

               If `result' is non-empty on input, its contents will be
               discarded before gfuds_c conducts its search.

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

   None.

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

   5)  If `result' has insufficient capacity to contain the
       number of intervals on which the specified condition
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

   9)  If a non-zero value is provided for `adjust' when `relate' has any
       value other than "ABSMIN" or "ABSMAX", an error is signaled by
       a routine in the call tree of this routine.

   10) If required ephemerides or other kernel data are not
       available, an error is signaled by a routine in the call tree
       of this routine.

   11) If the `relate' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   12) If the `relate' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   13) If any the `cnfine' or `result' cell arguments has a type
       other than SpiceDouble, the error SPICE(TYPEMISMATCH) is
       signaled.

   14) If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled.

-Files

   Appropriate kernels must be loaded by the calling program before
   this routine is called.

   If the scalar function requires access to ephemeris data:

   -  SPK data: ephemeris data for any body over the
      time period defined by the confinement window must be
      loaded. If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data.
      Typically ephemeris data are made available by loading one
      or more SPK files via furnsh_c.

   -  If non-inertial reference frames are used, then PCK
      files, frame kernels, C-kernels, and SCLK kernels may be
      needed.

   -  Certain computations can expand the time window over which
      `udfuns' and `udqdec' require data; such data must be provided by
      loaded kernels. See -Particulars for details.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   This routine determines a set of one or more time intervals
   within the confinement window when the scalar function
   satisfies a caller-specified constraint. The resulting set of
   intervals is returned as a SPICE window.

   udqdec Default Template
   =======================

   The user must supply a routine to determine whether sign of the
   time derivative of `udfuns' is positive or negative at `et'. For
   cases where `udfuns' is numerically well behaved, the user
   may find it convenient to use a routine based on the below
   template. uddc_c determines the truth of the expression

      d (udfuns)
      --         < 0
      dt

   using the library routine uddf_c to numerically calculate the
   derivative of `udfuns' using a three-point estimation.
   Please see the -Examples section for an example of gfdecr use.

      void gfdecr ( SpiceDouble et, SpiceBoolean * isdecr )
      {

         SpiceDouble         dt = h, double precision interval size;

         uddc_c( udfuns, et, dt, isdecr );

         return;
      }

   The Search Process
   ==================

   Regardless of the type of constraint selected by the caller, this
   routine starts the search for solutions by determining the time
   periods, within the confinement window, over which the specified
   scalar function is monotone increasing and monotone
   decreasing. Each of these time periods is represented by a SPICE
   window. Having found these windows, all of the quantity
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
   change of quantity function will be sampled. Starting at
   the left endpoint of an interval, samples will be taken at each
   step. If a change of sign is found, a root has been bracketed; at
   that point, the time at which the time derivative of the quantity
   function is zero can be found by a refinement process, for
   example, using a binary search.

   Note that the optimal choice of step size depends on the lengths
   of the intervals over which the quantity function is monotone:
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

   Having some knowledge of the relative geometry of the targets and
   observer can be a valuable aid in picking a reasonable step size.
   In general, the user can compensate for lack of such knowledge by
   picking a very short step size; the cost is increased computation
   time.

   Note that the step size is not related to the precision with which
   the endpoints of the intervals of the result window are computed.
   That precision level is controlled by the convergence tolerance.


   Convergence Tolerance
   =====================

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

   Certain user-defined computations may expand the window over
   which computations are performed. Here "expansion" of a window by
   an amount "T" means that the left endpoint of each interval
   comprising the window is shifted left by T, the right endpoint of
   each interval is shifted right by T, and any overlapping
   intervals are merged. Note that the input window `cnfine' itself is
   not modified.

   If a search uses an equality constraint, the time window over
   which the functions `udfuns' and `udqdec' are called is expanded by 1
   second.

   Computation of observer-target states by spkezr_c or spkez_c, using
   stellar aberration corrections, requires the state of the
   observer, relative to the solar system barycenter, to be computed
   at times offset from the input time by +/- 1 second. If the input
   time `et' is used by `udfuns' or `udqdec' to compute such a state, the
   window over which the observer state is computed is expanded by
   1 second.

   The window expansions described above are additive: if both
   conditions apply, the window expansion amount is the sum of the
   individual amounts.

   When light time corrections are used in the computation of
   observer-target states, expansion of the search window also
   affects the set of times at which the light time-corrected states
   of the targets are computed.

   In addition to the possible expansion of the search window that
   occurs when both an equality constraint and stellar aberration
   corrections are used, round-off error should be taken into
   account when the need for data availability is analyzed.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Conduct a search on the range rate of the vector from the Sun
      to the Moon. Define a function to calculate the value.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: gfuds_ex1.tm

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
            naif0009.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de414.bsp',
                                'pck00008.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program gfuds_ex1
      ./
      #include <stdio.h>
      #include <stdlib.h>
      #include <string.h>

      #include "SpiceUsr.h"
      #include "SpiceZfc.h"


      #define       MAXWIN    20000
      #define       TIMFMT    "YYYY-MON-DD HR:MN:SC.###"
      #define       TIMLEN    41
      #define       NLOOPS    7

      void    gfq     ( SpiceDouble et, SpiceDouble * value );
      void    gfdecrx ( void ( * udfuns ) ( SpiceDouble    et,
                                            SpiceDouble  * value ),
                        SpiceDouble    et,
                        SpiceBoolean * isdecr );

      doublereal dvnorm_(doublereal *state);


      int main( )
         {

         /.
         Create the needed windows. Note, one interval
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

         SpiceInt          count;
         SpiceInt          i;
         SpiceInt          j;

         ConstSpiceChar * relate [NLOOPS] = { "=",
                                              "<",
                                              ">",
                                              "LOCMIN",
                                              "ABSMIN",
                                              "LOCMAX",
                                              "ABSMAX"
                                            };

         printf( "Compile date %s, %s\n\n", __DATE__, __TIME__ );

         /.
         Load kernels.
         ./
         furnsh_c( "gfuds_ex1.tm" );

         /.
         Store the time bounds of our search interval in the `cnfine'
         confinement window.
         ./
         str2et_c( "2007 JAN 01", &begtim );
         str2et_c( "2007 APR 01", &endtim );

         wninsd_c ( begtim, endtim, &cnfine );

         /.
         Search using a step size of 1 day (in units of seconds). The reference
         value is .3365 km/s. We're not using the adjustment feature, so
         we set `adjust' to zero.
         ./
         step   = spd_c();
         adjust = 0.;
         refval = .3365;

         for ( j = 0;  j < NLOOPS;  j++ )
            {

            printf ( "Relation condition: %s \n",  relate[j] );

            /.
            Perform the search. The SPICE window `result' contains
            the set of times when the condition is met.
            ./

            gfuds_c ( gfq,
                      gfdecrx,
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

         kclear_c();
         return( 0 );
         }



      /.
      The user defined functions required by GFUDS.

         gfq     for udfuns
         gfdecrx for udqdec
      ./



      /.
      -Procedure Procedure gfq
      ./

      void gfq ( SpiceDouble et, SpiceDouble * value )

      /.
      -Abstract

         User defined geometric quantity function. In this case,
         the range rate from the sun to the Moon at TDB time `et'.

      ./
         {

         /. Initialization ./
         SpiceInt             targ   = 301;
         SpiceInt             obs    = 10;

         SpiceChar          * ref    = "J2000";
         SpiceChar          * abcorr = "NONE";

         SpiceDouble          state [6];
         SpiceDouble          lt;

         /.
         Retrieve the vector from the Sun to the Moon in the J2000
         frame, without aberration correction.
         ./
         spkez_c ( targ, et, ref, abcorr, obs, state, &lt );

         /.
         Calculate the scalar range rate corresponding the
        `state' vector.
         ./

         *value = dvnorm_( state );

         return;
         }



      /.
      -Procedure gfdecrx
      ./

      void gfdecrx ( void ( * udfuns ) ( SpiceDouble    et,
                                         SpiceDouble  * value ),
                     SpiceDouble    et,
                     SpiceBoolean * isdecr )

      /.
      -Abstract

         User defined function to detect if the function derivative
         is negative (the function is decreasing) at TDB time `et'.
      ./
         {

         SpiceDouble         dt = 10.;

         /.
         Determine if "udfuns" is decreasing at `et'.

         uddc_c   - the GF function to determine if
                    the derivative of the user defined
                    function is negative at `et'.
         ./

         uddc_c( udfuns, et, dt, isdecr );

         return;
         }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Compile date Mar 20 2018, 17:48:10

      Relation condition: =
      Start time, drdt = 2007-JAN-02 00:35:19.574
      Stop time,  drdt = 2007-JAN-02 00:35:19.574
      Start time, drdt = 2007-JAN-19 22:04:54.899
      Stop time,  drdt = 2007-JAN-19 22:04:54.899
      Start time, drdt = 2007-FEB-01 23:30:13.428
      Stop time,  drdt = 2007-FEB-01 23:30:13.428
      Start time, drdt = 2007-FEB-17 11:10:46.540
      Stop time,  drdt = 2007-FEB-17 11:10:46.540
      Start time, drdt = 2007-MAR-04 15:50:19.929
      Stop time,  drdt = 2007-MAR-04 15:50:19.929
      Start time, drdt = 2007-MAR-18 09:59:05.959
      Stop time,  drdt = 2007-MAR-18 09:59:05.959

      Relation condition: <
      Start time, drdt = 2007-JAN-02 00:35:19.574
      Stop time,  drdt = 2007-JAN-19 22:04:54.899
      Start time, drdt = 2007-FEB-01 23:30:13.428
      Stop time,  drdt = 2007-FEB-17 11:10:46.540
      Start time, drdt = 2007-MAR-04 15:50:19.929
      Stop time,  drdt = 2007-MAR-18 09:59:05.959

      Relation condition: >
      Start time, drdt = 2007-JAN-01 00:00:00.000
      Stop time,  drdt = 2007-JAN-02 00:35:19.574
      Start time, drdt = 2007-JAN-19 22:04:54.899
      Stop time,  drdt = 2007-FEB-01 23:30:13.428
      Start time, drdt = 2007-FEB-17 11:10:46.540
      Stop time,  drdt = 2007-MAR-04 15:50:19.929
      Start time, drdt = 2007-MAR-18 09:59:05.959
      Stop time,  drdt = 2007-APR-01 00:00:00.000

      Relation condition: LOCMIN
      Start time, drdt = 2007-JAN-11 07:03:58.988
      Stop time,  drdt = 2007-JAN-11 07:03:58.988
      Start time, drdt = 2007-FEB-10 06:26:15.439
      Stop time,  drdt = 2007-FEB-10 06:26:15.439
      Start time, drdt = 2007-MAR-12 03:28:36.404
      Stop time,  drdt = 2007-MAR-12 03:28:36.404

      Relation condition: ABSMIN
      Start time, drdt = 2007-JAN-11 07:03:58.988
      Stop time,  drdt = 2007-JAN-11 07:03:58.988

      Relation condition: LOCMAX
      Start time, drdt = 2007-JAN-26 02:27:33.766
      Stop time,  drdt = 2007-JAN-26 02:27:33.766
      Start time, drdt = 2007-FEB-24 09:35:07.816
      Stop time,  drdt = 2007-FEB-24 09:35:07.816
      Start time, drdt = 2007-MAR-25 17:26:56.150
      Stop time,  drdt = 2007-MAR-25 17:26:56.150

      Relation condition: ABSMAX
      Start time, drdt = 2007-MAR-25 17:26:56.150
      Stop time,  drdt = 2007-MAR-25 17:26:56.150


-Restrictions

   1)  Any kernel files required by this routine must be loaded
       before this routine is called.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 03-NOV-2021 (JDR) (EDW) (NJB)

       Added use of ALLOC_CHECK_INTRA to check net null effect on
       alloc count.

       Updated header to describe use of expanded confinement window.

       Edited the header to comply with NAIF standard.

       Updated the description of "nintvls", "cnfine" and "result"
       arguments.

       Added entries #4, #5 and #14 in -Exceptions sections, and
       replaced old entry #5 by new entries #8 and #9.

   -CSPICE Version 1.0.1, 21-OCT-2013 (NJB) (EDW)

       Correction to description of UDQDEC to show UDFUNC as
       an argument.

       Header was updated to discuss use of gfstol_c.

       Edit to comments to correct search description; eliminate
       typo in gfq -Abstract, "range rate" instead of "range."

       Improved header detail describing convergence tolerance.

   -CSPICE Version 1.0.0, 22-FEB-2010 (EDW)

-Index_Entries

   GF user defined scalar function search

-&
*/

  { /* Begin gfuds_c */

   /*
   Local variables
   */

   doublereal              * work;

   static SpiceInt           nw = SPICE_GF_NWMAX;

   int                       nowalloc;

   SpiceInt                  nBytes;


   /*
   Participate in error tracing.
   */
   if ( return_c() )
     {
      return;
      }
   chkin_c ( "gfuds_c" );


   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfuds_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary.
   */
   CELLINIT2 ( cnfine, result );

   /*
   Check the input strings to make sure each pointer is non-null
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfuds_c", relate );

   /*
   Store the input function pointers so these functions can be
   called by the GF adapters.
   */
   zzadsave_c ( UDFUNC,  (void *)(udfuns)  );
   zzadsave_c ( UDQDEC,  (void *)(udqdec)  );

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
      chkout_c ( "gfuds_c"                                  );
      return;
      }


   /*
   Allocate the workspace. `nintvls' indicates the maximum number of
   intervals returned in `result'. An interval consists of
   two values.
   */

   nowalloc = alloc_count();

   nintvls = 2 * nintvls;

   nBytes = (nintvls + SPICE_CELL_CTRLSZ ) * nw * sizeof(SpiceDouble);

   work   = (doublereal *) alloc_SpiceMemory( nBytes );

   if ( !work )
      {
      setmsg_c ( "Workspace allocation of # bytes failed due to "
                 "malloc failure"                               );
      errint_c ( "#",  nBytes                                   );
      sigerr_c ( "SPICE(MALLOCFAILED)"                          );
      chkout_c ( "gfuds_c"                                      );
      return;
      }


   /*
   Let the f2c'd routine do the work.

   We pass the adapter functions, not those provided as inputs,
   to the f2c'd routine:

      zzadfunc_c  adapter for  udfuns
      zzadqdec_c     ''        udqdec

   */

   (void) gfuds_( ( U_fp            ) zzadfunc_c,
                  ( U_fp            ) zzadqdec_c,
                  ( char          * ) relate,
                  ( doublereal    * ) &refval,
                  ( doublereal    * ) &adjust,
                  ( doublereal    * ) &step,
                  ( doublereal    * ) (cnfine->base),
                  ( integer       * ) &nintvls,
                  ( integer       * ) &nw,
                  ( doublereal    * ) work,
                  ( doublereal    * ) (result->base),
                  ( ftnlen          ) strlen(relate) );


   /*
   Always free dynamically allocated memory.
   */
   free_SpiceMemory( work );

   /*
   Sync the output cell.
   */
   if ( !failed_c() )
     {
     zzsynccl_c ( F2C, result );
     }

   ALLOC_CHECK_INTRA(nowalloc);

   chkout_c ( "gfuds_c" );

   } /* End gfuds_c */
