/*

-Procedure gfrfov_c ( GF, is ray in FOV? )

-Abstract

   Determine time intervals when a specified ray intersects the
   space bounded by the field-of-view (FOV) of a specified
   instrument.

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
   FRAMES
   GF
   KERNEL
   NAIF_IDS
   PCK
   SPK
   TIME
   WINDOWS

-Keywords

   EVENT
   FOV
   GEOMETRY
   INSTRUMENT
   SEARCH
   WINDOW

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef gfrfov_c

   void gfrfov_c ( ConstSpiceChar     * inst,
                   ConstSpiceDouble     raydir [3],
                   ConstSpiceChar     * rframe,
                   ConstSpiceChar     * abcorr,
                   ConstSpiceChar     * obsrvr,
                   SpiceDouble          step,
                   SpiceCell          * cnfine,
                   SpiceCell          * result  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   SPICE_GF_MARGIN
              P   Minimum complement of FOV cone angle.
   SPICE_GF_CNVTOL
              P   Convergence tolerance.
   SPICE_GF_MAXVRT
              P   Maximum number of FOV boundary vertices.
   inst       I   Name of the instrument.
   raydir     I   Ray's direction vector.
   rframe     I   Reference frame of ray's direction vector.
   abcorr     I   Aberration correction flag.
   obsrvr     I   Name of the observing body.
   step       I   Step size in seconds for finding FOV events.
   cnfine    I-O  SPICE window to which the search is restricted.
   result     O   SPICE window containing results.

-Detailed_Input

   inst        indicates the name of an instrument, such as a
               spacecraft-mounted framing camera, the field of view
               (FOV) of which is to be used for a target intersection
               search: the direction from the observer to a target
               is represented by a ray, and times when the specified
               ray intersects the region of space bounded by the FOV
               are sought.

               The position of the instrument designated by `inst' is
               considered to coincide with that of the ephemeris
               object designated by the input argument `obsrvr' (see
               description below).

               `inst' must have a corresponding NAIF ID and a frame
               defined, as is normally done in a frame kernel. It
               must also have an associated reference frame and a FOV
               shape, boresight and boundary vertices (or reference
               vector and reference angles) defined, as is usually
               done in an instrument kernel.

               See the header of the CSPICE routine getfov_c for a
               description of the required parameters associated with
               an instrument.

   raydir      is the direction vector associated with a ray
               representing a target. The ray emanates from the
               location of the ephemeris object designated by the
               input argument `obsrvr' and is expressed relative to the
               reference frame designated by `rframe' (see descriptions
               below).

   rframe      is the name of the reference frame associated with
               the input ray's direction vector `raydir'.

               Since light time corrections are not supported for
               rays, the orientation of the frame is always evaluated
               at the epoch associated with the observer, as opposed
               to the epoch associated with the light-time corrected
               position of the frame center.

               Case and leading or trailing blanks bracketing a
               non-blank frame name are not significant in the string
               `rframe'.

   abcorr      indicates the aberration corrections to be applied
               when computing the ray's direction.

               The supported aberration correction options are

                  "NONE"          No correction.

                  "S"             Stellar aberration correction,
                                  reception case.

                  "XS"            Stellar aberration correction,
                                  transmission case.

               For detailed information, see the geometry finder
               required reading, gf.req.

               Case, leading and trailing blanks are not significant
               in the string `abcorr'.

   obsrvr      is the name of the body from which the target
               represented by `raydir' is observed. The instrument
               designated by `inst' is treated as if it were co-located
               with the observer.

               Optionally, you may supply the integer NAIF ID code
               for the body as a string.

               Case and leading or trailing blanks are not
               significant in the string `obsrvr'.

   step        is the step size to be used in the search. `step' must
               be shorter than any interval, within the confinement
               window, over which the specified condition is met. In
               other words, `step' must be shorter than the shortest
               visibility event that the user wishes to detect. `step'
               also must be shorter than the minimum duration
               separating any two visibility events. However, `step'
               must not be *too* short, or the search will take an
               unreasonable amount of time.

               The choice of `step' affects the completeness but not
               the precision of solutions found by this routine; the
               precision is controlled by the convergence tolerance.
               See the discussion of the parameter SPICE_GF_CNVTOL for
               details.

               `step' has units of seconds.

   cnfine      is a SPICE window that confines the time period over
               which the specified search is conducted. `cnfine' may
               consist of a single interval or a collection of
               intervals.

               The endpoints of the time intervals comprising `cnfine'
               are interpreted as seconds past J2000 TDB.

               See the -Examples section below for a code example
               that shows how to create a confinement window.

               `cnfine' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( cnfine, CNFINESZ );

               where CNFINESZ is the maximum capacity of `cnfine'.

-Detailed_Output

   cnfine      is the input confinement window, updated if necessary so the
               control area of its data array indicates the window's size
               and cardinality. The window data are unchanged.

   result      is a SPICE window representing the set of time intervals,
               within the confinement period, when the input ray is
               "visible"; that is, when the ray is contained in the
               space bounded by the specified instrument's field of
               view.

               `result' must be declared and initialized with sufficient
               size to capture the full set of time intervals within the
               search region on which the specified condition is satisfied.

               If `result' is non-empty on input, its contents will be
               discarded before gfrfov_c conducts its search.

               The endpoints of the time intervals comprising `result' are
               interpreted as seconds past J2000 TDB.

               If no times within the confinement window satisfy the
               search criteria, `result' will be returned with a
               cardinality of zero.

               `result' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( result, RESULTSZ );

               where RESULTSZ is the maximum capacity of `result'.

-Parameters

   All parameters described here are declared in the header file
   SpiceGF.h. See that file for parameter values.

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


   SPICE_GF_MAXVRT

              is the maximum number of vertices that may be used
              to define the boundary of the specified instrument's
              field of view.


   SPICE_GF_MARGIN

              is a small positive number used to constrain the
              orientation of the boundary vectors of polygonal
              FOVs. Such FOVs must satisfy the following constraints:

                 1)  The boundary vectors must be contained within
                     a right circular cone of angular radius less
                     than than (pi/2) - SPICE_GF_MARGIN radians; in other
                     words, there must be a vector A such that all
                     boundary vectors have angular separation from
                     A of less than (pi/2)-SPICE_GF_MARGIN radians.

                 2)  There must be a pair of boundary vectors U, V
                     such that all other boundary vectors lie in the
                     same half space bounded by the plane containing U
                     and V. Furthermore, all other boundary vectors
                     must have orthogonal projections onto a specific
                     plane normal to this plane (the normal plane
                     contains the angle bisector defined by U and V)
                     such that the projections have angular separation
                     of at least 2*SPICE_GF_MARGIN radians from the
                     plane spanned by U and V.

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

   3)  If the observer's name cannot be mapped to an ID code, an
       error is signaled by a routine in the call tree of this
       routine.

   4)  If the aberration correction flag calls for light time
       correction, an error is signaled by a routine in the call tree
       of this routine.

   5)  If the ray's direction vector is zero, an error is signaled by
       a routine in the call tree of this routine.

   6)  If the instrument name `inst' does not have corresponding NAIF
       ID code, an error is signaled by a routine in the call
       tree of this routine.

   7)  If the FOV parameters of the instrument are not present in
       the kernel pool, an error is signaled by a routine
       in the call tree of this routine.

   8)  If the FOV boundary has more than SPICE_GF_MAXVRT vertices, an error
       is signaled by a routine in the call tree of this
       routine.

   9)  If the instrument FOV is polygonal, and this routine cannot
       find a ray R emanating from the FOV vertex such that maximum
       angular separation of R and any FOV boundary vector is within
       the limit (pi/2)-SPICE_GF_MARGIN radians, an error is signaled
       by a routine in the call tree of this routine. If the FOV
       is any other shape, the same error check will be applied with
       the instrument boresight vector serving the role of R.

   10) If the loaded kernels provide insufficient data to compute a
       requested state vector, an error is signaled by a
       routine in the call tree of this routine.

   11) If an error occurs while reading an SPK or other kernel file,
       the error is signaled by a routine in the call tree
       of this routine.

   12) If the output SPICE window `result' has size less than 2, the
       error SPICE(WINDOWTOOSMALL) is signaled by a routine in the
       call tree of this routine.

   13) If the output SPICE window `result' has insufficient capacity
       to contain the number of intervals on which the specified
       visibility condition is met, an error is signaled
       by a routine in the call tree of this routine.

   14) If any of the `inst', `rframe', `abcorr' or `obsrvr' input
       string pointers is null, the error SPICE(NULLPOINTER) is
       signaled.

   15) If any of the `inst', `rframe' or `abcorr' input strings has
       zero length, the error SPICE(EMPTYSTRING) is signaled.

   16) If any the `cnfine' or `result' cell arguments has a type
       other than SpiceDouble, the error SPICE(TYPEMISMATCH) is
       signaled.

-Files

   Appropriate SPICE kernels must be loaded by the calling program
   before this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for the observer for the period
      defined by the confinement window `cnfine' must be loaded.
      If aberration corrections are used, the state of the
      observer relative to the solar system barycenter must be
      calculable from the available ephemeris data. Typically
      ephemeris data are made available by loading one or more SPK
      files via furnsh_c.

   -  Data defining the reference frame associated with the
      instrument designated by `inst' must be available in the kernel
      pool. Additionally the name `inst' must be associated with an
      ID code. Normally these data are  made available by loading a
      frame kernel via furnsh_c.

   -  IK data: the kernel pool must contain data such that
      the CSPICE routine getfov_c may be called to obtain
      parameters for `inst'. Normally such data are provided by
      an IK via furnsh_c.

   The following data may be required:

   -  CK data: if the instrument frame is fixed to a spacecraft,
      at least one CK file will be needed to permit transformation
      of vectors between that frame and the J2000 frame.

   -  SCLK data: if a CK file is needed, an associated SCLK
      kernel is required to enable conversion between encoded SCLK
      (used to time-tag CK data) and barycentric dynamical time
      (TDB).

   -  Since the input ray direction may be expressed in any
      frame, FKs, CKs, SCLK kernels, PCKs, and SPKs may be
      required to map the direction to the J2000 frame.

   Kernel data are normally loaded once per program run, NOT every
   time this routine is called.

-Particulars

   This routine determines a set of one or more time intervals when
   the specified ray in contained within the field of view of a
   specified instrument. We'll use the term "visibility event" to
   designate such an appearance. The set of time intervals resulting
   from the search is returned as a SPICE window.

   This routine provides a simpler, but less flexible, interface
   than does the CSPICE routine gffove_c for conducting searches for
   visibility events. Applications that require support for progress
   reporting, interrupt handling, non-default step or refinement
   functions, or non-default convergence tolerance should call
   gffove_c rather than this routine.

   Below we discuss in greater detail aspects of this routine's
   solution process that are relevant to correct and efficient use
   of this routine in user applications.


   The Search Process
   ==================

   The search for visibility events is treated as a search for state
   transitions: times are sought when the state of the ray
   changes from "not visible" to "visible" or vice versa.

   Step Size
   =========

   Each interval of the confinement window is searched as follows:
   first, the input step size is used to determine the time
   separation at which the visibility state will be sampled.
   Starting at the left endpoint of an interval, samples will be
   taken at each step. If a state change is detected, a root has
   been bracketed; at that point, the "root"--the time at which the
   state change occurs---is found by a refinement process, for
   example, via binary search.

   Note that the optimal choice of step size depends on the lengths
   of the intervals over which the visibility state is constant:
   the step size should be shorter than the shortest visibility event
   duration and the shortest period between visibility events, within
   the confinement window.

   Having some knowledge of the relative geometry of the ray and
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
   "convergence tolerance." The convergence tolerance used by this
   routine is set via the parameter SPICE_GF_CNVTOL.

   The value of SPICE_GF_CNVTOL is set to a "tight" value so that the
   tolerance doesn't become the limiting factor in the accuracy of
   solutions found by this routine. In general the accuracy of input
   data will be the limiting factor.

   To use a different tolerance value, a lower-level GF routine such
   as gffove_c  must be called. Making the tolerance tighter than
   SPICE_GF_CNVTOL is unlikely to be useful, since the results are unlikely
   to be more accurate. Making the tolerance looser will speed up
   searches somewhat, since a few convergence steps will be omitted.
   However, in most cases, the step size is likely to have a much
   greater effect on processing time than would the convergence
   tolerance.


   The Confinement Window
   ======================

   The simplest use of the confinement window is to specify a time
   interval within which a solution is sought. However, the confinement
   window can, in some cases, be used to make searches more efficient.
   Sometimes it's possible to do an efficient search to reduce the size
   of the time period over which a relatively slow search of interest
   must be performed. For an example, see the program CASCADE in the GF
   Example Programs chapter of the GF Required Reading, gf.req.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example is an extension of example #1 in the
      header of

         gftfov_c

      The problem statement for that example is

         Search for times when Saturn's satellite Phoebe is within the
         FOV of the Cassini narrow angle camera (CASSINI_ISS_NAC). To
         simplify the problem, restrict the search to a short time
         period where continuous Cassini bus attitude data are
         available.

         Use a step size of 10 seconds to reduce chances of missing
         short visibility events.

      Here we search the same confinement window for times when a
      selected background star is visible. We use the FOV of the
      Cassini ISS wide angle camera (CASSINI_ISS_WAC) to enhance the
      probability of viewing the star.

      The star we'll use has catalog number 6000 in the Hipparcos
      Catalog. The star's J2000 right ascension and declination, proper
      motion, and parallax are taken from that catalog.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: gfrfov_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                       Contents
            -----------------------------   ----------------------
            naif0012.tls                    Leapseconds
            041014R_SCPSE_01066_04199.bsp   CASSINI, planetary and
                                            Saturn satellite
                                            ephemeris
            cas_v40.tf                      Cassini FK
            04161_04164ra.bc                Cassini bus CK
            cas00071.tsc                    Cassini SCLK kernel
            cas_iss_v10.ti                  Cassini IK


         \begindata

            KERNELS_TO_LOAD = ( 'naif0012.tls',
                                '041014R_SCPSE_01066_04199.bsp',
                                'cas_v40.tf',
                                '04161_04164ra.bc',
                                'cas00071.tsc',
                                'cas_iss_v10.ti'            )
         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program gfrfov_ex1
      ./
      #include <stdio.h>
      #include <stdlib.h>
      #include <math.h>
      #include "SpiceUsr.h"
      #include "SpiceZmc.h"

      int main()
      {
         /.
         Local constants
         ./
         #define  AU             149597870.693
         #define  META           "gfrfov_ex1.tm"
         #define  TIMFMT         "YYYY-MON-DD HR:MN:SC.######::TDB (TDB)"
         #define  TIMLEN         41
         #define  MAXWIN         10000

         /.
         Local variables
         ./
         SPICEDOUBLE_CELL ( cnfine, MAXWIN );
         SPICEDOUBLE_CELL ( result, MAXWIN );

         SpiceChar             * abcorr;
         SpiceChar             * inst;
         SpiceChar             * obsrvr;
         SpiceChar             * rframe;
         SpiceChar               timstr  [2][ TIMLEN ];

         SpiceDouble             dec;
         SpiceDouble             dec_deg;
         SpiceDouble             dec_deg_0;
         SpiceDouble             dec_epoch;
         SpiceDouble             dec_pm;
         SpiceDouble             dtdec;
         SpiceDouble             dtra;
         SpiceDouble             endpt   [2];
         SpiceDouble             et0;
         SpiceDouble             et1;
         SpiceDouble             lt;
         SpiceDouble             parallax;
         SpiceDouble             parallax_deg;
         SpiceDouble             pos     [3];
         SpiceDouble             ra;
         SpiceDouble             ra_deg;
         SpiceDouble             ra_deg_0;
         SpiceDouble             ra_epoch;
         SpiceDouble             ra_pm;
         SpiceDouble             raydir  [3];
         SpiceDouble             stardist;
         SpiceDouble             starpos [3];
         SpiceDouble             stepsz;
         SpiceDouble             t;

         SpiceInt                catno;
         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                n;

         /.
         Load kernels.
         ./
         furnsh_c ( META );

         /.
         Insert search time interval bounds into the
         confinement window.
         ./
         str2et_c ( "2004 JUN 11 06:30:00 TDB", &et0 );
         str2et_c ( "2004 JUN 11 12:00:00 TDB", &et1 );

         wninsd_c ( et0, et1, &cnfine );

         /.
         Initialize inputs for the search.
         ./
         inst   = "CASSINI_ISS_WAC";

         /.
         Create a unit direction vector pointing from observer to star.
         We'll assume the direction is constant during the confinement
         window, and we'll use et0 as the epoch at which to compute the
         direction from the spacecraft to the star.

         The data below are for the star with catalog number 6000
         in the Hipparcos catalog. Angular units are degrees; epochs
         have units of Julian years and have a reference epoch of J1950.
         The reference frame is J2000.
         ./
         catno        = 6000;

         parallax_deg = 0.000001056;

         ra_deg_0     = 19.290789927;
         ra_pm        = -0.000000720;
         ra_epoch     = 41.2000;

         dec_deg_0    =  2.015271007;
         dec_pm       =  0.000001814;
         dec_epoch    = 41.1300;

         rframe       = "J2000";

         /.
         Correct the star's direction for proper motion.

         The argument t represents et0 as Julian years past J1950.
         ./
         t         = et0/jyear_c()  +  ( j2000_c()- j1950_c() )/365.25;

         dtra      = t - ra_epoch;
         dtdec     = t - dec_epoch;

         ra_deg    = ra_deg_0  +  dtra  * ra_pm;
         dec_deg   = dec_deg_0 +  dtdec * dec_pm;

         ra        = ra_deg  * rpd_c();
         dec       = dec_deg * rpd_c();

         radrec_c ( 1.0, ra, dec, starpos );

         /.
         Correct star position for parallax applicable at
         the Cassini orbiter's position. (The parallax effect
         is negligible in this case; we're simply demonstrating
         the computation.)
         ./
         parallax = parallax_deg * rpd_c();
         stardist = AU / tan(parallax);

         /.
         Scale the star's direction vector by its distance from
         the solar system barycenter. Subtract off the position
         of the spacecraft relative to the solar system barycenter;
         the result is the ray's direction vector.
         ./
         vscl_c   ( stardist, starpos, starpos );

         spkpos_c ( "cassini", et0, "J2000",  "NONE",
                    "solar system barycenter", pos,  &lt );

         vsub_c   ( starpos, pos, raydir );

         /.
         Correct the star direction for stellar aberration when
         we conduct the search.
         ./
         abcorr = "S";
         obsrvr = "CASSINI";
         stepsz = 10.0;

         printf ( "\n"
                  " Instrument:            %s\n"
                  " Star's catalog number: %d\n"
                  "\n",
                  inst,
                  (int)catno            );

         /.
         Perform the search.
         ./
         gfrfov_c ( inst,   raydir, rframe,  abcorr,
                    obsrvr, stepsz, &cnfine, &result );


         n = wncard_c ( &result );

         if ( n == 0 )
         {
            printf (  "No FOV intersection found.\n" );
         }
         else
         {
            printf ( "  Visibility start time              Stop time\n" );

            for ( i = 0;  i < n;  i++ )
            {
               wnfetd_c ( &result, i, endpt, endpt+1 );

               for ( j = 0;  j < 2;  j++ )
               {
                  timout_c ( endpt[j], TIMFMT, TIMLEN, timstr[j] );
               }

               printf ( "  %s  %s\n",
                        timstr[0],
                        timstr[1]                                   );
            }
         }

         printf ( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       Instrument:            CASSINI_ISS_WAC
       Star's catalog number: 6000

        Visibility start time              Stop time
        2004-JUN-11 06:30:00.000000 (TDB)  2004-JUN-11 12:00:00.000000 (TDB)


      Note that the star is visible throughout the confinement window.

-Restrictions

   1)  The kernel files to be used by gfrfov_c must be loaded (normally via
       the CSPICE routine furnsh_c) before gfrfov_c is called.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   L.S. Elson          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 06-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Updated Example's kernels set to use PDS archived data.

       Updated the description of "obsrvr", "cnfine" and "result"
       arguments.

       Added entry #16 in -Exceptions section.

   -CSPICE Version 1.0.1, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.0, 12-FEB-2009 (NJB) (LSE) (EDW)

-Index_Entries

   GF ray in instrument FOV search

-&
*/

{ /* Begin gfrfov_c */


   /*
   Local variables
   */
   SpiceChar               * obsrvrStr;

   /*
   Static variables
   */
   static const SpiceChar  * blankStr = " ";


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "gfrfov_c" );

   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfrfov_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary.
   */
   CELLINIT2 ( cnfine, result );

   /*
   The input observer name is a special case because we allow the
   caller to pass in an empty string. If this string is empty, we pass
   a null-terminated string containing one blank character to the
   underlying f2c'd routine.

   First make sure the observer name pointer is non-null.
   */
   CHKPTR ( CHK_STANDARD, "gfrfov_c", obsrvr );

   /*
   Use the input observer name string if it's non-empty; otherwise
   use a blank string for the instr name.
   */

   if ( obsrvr[0] )
   {
      obsrvrStr = (SpiceChar *) obsrvr;
   }
   else
   {
      obsrvrStr = (SpiceChar *) blankStr;
   }

   /*
   Check the other input strings to make sure each pointer is non-null
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfrfov_c", inst   );
   CHKFSTR ( CHK_STANDARD, "gfrfov_c", rframe );
   CHKFSTR ( CHK_STANDARD, "gfrfov_c", abcorr );

   /*
   Let the f2c'd routine do the work.
   */
   gfrfov_ ( (char         *) inst,
             (doublereal   *) raydir,
             (char         *) rframe,
             (char         *) abcorr,
             (char         *) obsrvrStr,
             (doublereal   *) &step,
             (doublereal   *) cnfine->base,
             (doublereal   *) result->base,
             (ftnlen        ) strlen(inst),
             (ftnlen        ) strlen(rframe),
             (ftnlen        ) strlen(abcorr),
             (ftnlen        ) strlen(obsrvrStr)  );

   /*
   Sync the output result cell.
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, result );
   }

   chkout_c ( "gfrfov_c" );

} /* End gfrfov_c */
