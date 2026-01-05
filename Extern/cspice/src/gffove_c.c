/*

-Procedure gffove_c ( GF, is target in FOV? )

-Abstract

   Determine time intervals when a specified target body or ray
   intersects the space bounded by the field-of-view (FOV) of a
   specified instrument. Report progress and handle interrupts if so
   commanded.

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

   #include <signal.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZad.h"
   #include "SpiceZmc.h"
   #undef gffove_c

   void gffove_c ( ConstSpiceChar     * inst,
                   ConstSpiceChar     * tshape,
                   ConstSpiceDouble     raydir [3],
                   ConstSpiceChar     * target,
                   ConstSpiceChar     * tframe,
                   ConstSpiceChar     * abcorr,
                   ConstSpiceChar     * obsrvr,
                   SpiceDouble          tol,
                   void             ( * udstep ) ( SpiceDouble       et,
                                                   SpiceDouble     * step ),

                   void             ( * udrefn ) ( SpiceDouble       t1,
                                                   SpiceDouble       t2,
                                                   SpiceBoolean      s1,
                                                   SpiceBoolean      s2,
                                                   SpiceDouble     * t    ),
                   SpiceBoolean         rpt,

                   void             ( * udrepi ) ( SpiceCell       * cnfine,
                                                   ConstSpiceChar  * srcpre,
                                                   ConstSpiceChar  * srcsuf ),

                   void             ( * udrepu ) ( SpiceDouble       ivbeg,
                                                   SpiceDouble       ivend,
                                                   SpiceDouble       et      ),

                   void             ( * udrepf ) ( void ),
                   SpiceBoolean         bail,
                   SpiceBoolean     ( * udbail ) ( void ),
                   SpiceCell          * cnfine,
                   SpiceCell          * result                                )

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
   tshape     I   Type of shape model used for target body.
   raydir     I   Ray's direction vector.
   target     I   Name of the target body.
   tframe     I   Body-fixed, body-centered frame for target body.
   abcorr     I   Aberration correction flag.
   obsrvr     I   Name of the observing body.
   tol        I   Convergence tolerance in seconds.
   udstep     I   Name of routine that returns a time step.
   udrefn     I   Name of the routine that computes a refined time.
   rpt        I   Progress report flag.
   udrepi     I   Function that initializes progress reporting.
   udrepu     I   Function that updates the progress report.
   udrepf     I   Function that finalizes progress reporting.
   bail       I   Logical indicating program interrupt monitoring.
   udbail     I   Name of a routine that signals a program interrupt.
   cnfine    I-O  SPICE window to which the search is restricted.
   result     O   SPICE window containing results.

-Detailed_Input

   inst        is a string indicates the name of an instrument, such as
               a spacecraft-mounted framing camera, the field of view
               (FOV) of which is to be used for a target intersection
               search: times when the specified target intersects the
               region of space corresponding to the FOV are sought.

               `inst' must have a corresponding NAIF ID and a frame
               defined, as is normally done in a frame kernel. It must
               also have an associated reference frame and a FOV shape,
               boresight and boundary vertices (or reference vector and
               reference angles) defined, as is usually done in an
               instrument kernel.

               See the header of the CSPICE routine getfov_c for a
               description of the required parameters associated with an
               instrument.

   tshape      is a string indicating the geometric model used to
               represent the location and shape of the target body. The
               target body may be represented by either an ephemeris
               object or a ray emanating from the observer.

               The supported values of `tshape' are:

                  "ELLIPSOID"     The target is an ephemeris object.

                                  The target's shape is represented
                                  using triaxial ellipsoid model,
                                  with radius values provided via the
                                  kernel pool. A kernel variable
                                  having a name of the form

                                     "BODYnnn_RADII"

                                  where nnn represents the NAIF
                                  integer code associated with the
                                  body, must be present in the kernel
                                  pool. This variable must be
                                  associated with three numeric
                                  values giving the lengths of the
                                  ellipsoid's X, Y, and Z semi-axes.

                  "POINT"         The target is an ephemeris object.
                                  The body is treated as a single
                                  point.

                  "RAY"           The target is NOT an ephemeris
                                  object. Instead, the target is
                                  represented by the ray emanating
                                  from the observer's location and
                                  having direction vector `raydir'. The
                                  target is considered to be visible
                                  if and only if the ray is contained
                                  within the space bounded by the
                                  instrument FOV.

               Case and leading or trailing blanks are not
               significant in the string `tshape'.

   raydir      is the direction vector associated with a ray
               representing the target. `raydir' is used if and only if
               `tshape' (see description above) indicates the target is
               modeled as a ray.

   target      is the name of the target body, the appearances of which
               in the specified instrument's field of view are sought.
               The body must be an ephemeris object.

               Optionally, you may supply the integer NAIF ID code for
               the body as a string. For example both "MOON" and "301"
               are legitimate strings that designate the Moon.

               Case and leading or trailing blanks are not significant
               in the string `target'.

               The input argument `target' is used if and only if the
               target is NOT modeled as ray, as indicated by the input
               argument `tshape'.

               `target' may be set to a blank string if the target is
               modeled as a ray.

   tframe      is the name of the reference frame associated with the
               target. Examples of such names are "IAU_SATURN" (for
               Saturn) and "ITRF93" (for the Earth).

               If the target is an ephemeris object modeled as an
               ellipsoid, `tframe' must designate a body-fixed reference
               frame centered on the target body.

               If the target is an ephemeris object modeled as a point,
               `tframe' is ignored; `tframe' should be left blank.

               If the target is modeled as a ray, `tframe' may designate
               any reference frame. Since light time corrections are not
               supported for rays, the orientation of the frame is
               always evaluated at the epoch associated with the
               observer, as opposed to the epoch associated with the
               light-time corrected position of the frame center.

               Case and leading or trailing blanks bracketing a
               non-blank frame name are not significant in the string
               `tframe'.

   abcorr      is a string indicating the aberration corrections to be
               applied when computing the target's position and
               orientation. The supported values of `abcorr' depend on the
               target representation.

               If the target is represented by a ray, the aberration
               correction options are

                  "NONE"       No correction.
                  "S"          Stellar aberration correction, reception
                               case.
                  "XS"         Stellar aberration correction,
                               transmission case.

               If the target is an ephemeris object, the aberration
               correction options are those supported by the SPICE SPK
               system. For remote sensing applications, where the
               apparent position and orientation of the target seen by
               the observer are desired, normally either of the
               corrections

                  "LT+S"
                  "CN+S"

               should be used. These and the other supported options are
               described below.

               Supported aberration correction options for observation
               (the case where radiation is received by observer at ET)
               are:

                  "NONE"       No correction.
                  "LT"         Light time only
                  "LT+S"       Light time and stellar aberration.
                  "CN"         Converged Newtonian (CN) light time.
                  "CN+S"       CN light time and stellar aberration.

               Supported aberration correction options for transmission
               (the case where radiation is emitted from observer at ET)
               are:

                  "XLT"        Light time only.
                  "XLT+S"      Light time and stellar aberration.
                  "XCN"        Converged Newtonian (CN) light time.
                  "XCN+S"      CN light time and stellar aberration.

               For detailed information, see the geometry finder
               required reading, gf.req.

               Case, leading and trailing blanks are not significant
               in the string `abcorr'.

   obsrvr      is the name of the body from which the target is
               observed. The instrument designated by `inst' is treated as
               if it were co-located with the observer.

               Optionally, you may supply the integer NAIF ID code
               for the body as a string.

               Case and leading or trailing blanks are not
               significant in the string `obsrvr'.

   tol         is a tolerance value used to determine convergence of
               root-finding operations. `tol' is measured in TDB seconds
               and must be greater than zero.

   udstep      is an externally specified routine that computes a time
               step used to find transitions of the state being
               considered. A state transition occurs where the state
               changes from being "visible" to being "not visible" or
               vice versa.

               This routine relies on `udstep' returning step sizes small
               enough so that state transitions within the confinement
               window are not overlooked.

               The prototype for `udstep' is:

                  void   ( * udstep ) ( SpiceDouble       et,
                                        SpiceDouble     * step )

               where:

                  et      is the input start time from which the
                          algorithm is to search forward for a state
                          transition. `et' is expressed as seconds past
                          J2000 TDB.

                  step    is the output step size. `step' indicates
                          how far to advance `et' so that `et' and
                          et+step may bracket a state transition and
                          definitely do not bracket more than one
                          state transition. Units are TDB seconds.

               If a constant step size is desired, the CSPICE routine

                  gfstep_c

               may be used as the step size function. If gfstep_c is used,
               the step size must be set by calling gfsstp_c prior to
               calling this routine.

   udrefn      is the name of the externally specified routine that
               refines the times that bracket a transition point. In
               other words, once a pair of times, `t1' and `t2', that
               bracket a state transition have been found, `udrefn'
               computes an intermediate time `t' such that either [t1, t]
               or [t, t2] contains the time of the state transition.
               The prototype for `udrefn' is:

                  void   ( * udrefn ) ( SpiceDouble       t1,
                                        SpiceDouble       t2,
                                        SpiceBoolean      s1,
                                        SpiceBoolean      s2,
                                        SpiceDouble     * t   )

               where the inputs are:

                  t1    is a time when the visibility state is `s1'. `t1'
                        is expressed as seconds past J2000 TDB.

                  t2    is a time when the visibility state is `s2'. `t2' is
                        expressed as seconds past J2000 TDB and is
                        assumed to be larger than `t1'.

                  s1    is the visibility state at time `t1'.

                  s2    is the visibility state at time `t2'.

               The output is:

                  t     is next time to check for a state transition.
                        `t' is a number between `t1' and `t2'. `t' is
                        expressed as seconds past J2000 TDB.

               If a simple bisection method is desired, the CSPICE
               routine gfrefn_c may be used as the refinement function.

   rpt         is a logical variable that controls whether progress
               reporting is enabled. When `rpt' is SPICETRUE, progress
               reporting is enabled and the routines `udrepi', `udrepu', and
               `udrepf' (see descriptions below) are used to report
               progress.

   udrepi      is a user-defined routine that initializes a progress
               report. When progress reporting is enabled, `udrepi' is
               called at the start of a search. The prototype for
               `udrepi' is:

                 void   ( * udrepi ) ( SpiceCell       * cnfine,
                                       ConstSpiceChar  * srcpre,
                                       ConstSpiceChar  * srcsuf )

               where

                  cnfine

               is the confinement window specifying the time period
               over which a search is conducted, and

                  srcpre
                  srcsuf

               are prefix and suffix strings used in the progress
               report: these strings are intended to bracket a
               representation of the fraction of work done. For example,
               when the CSPICE progress reporting functions are used,
               if `srcpre' and `srcsuf' are, respectively,

                  "Target visibility search"
                  "done."

               the progress report display at the end of the search will
               be:

                  Target visibility search 100.00% done.

               The CSPICE routine gfrepi_c may be used as the actual
               argument corresponding to `udrepi'. If so, the CSPICE
               routines gfrepu_c and gfrepf_c must be the actual arguments
               corresponding to `udrepu' and `udrepf'.

   udrepu      is a user-defined routine that updates the progress
               report for a search. The prototype of `udrepu' is:

                 void   ( * udrepu ) ( SpiceDouble       ivbeg,
                                       SpiceDouble       ivend,
                                       SpiceDouble       et      )

               Here `ivbeg', `ivend' are the bounds of an interval that is
               contained in some interval belonging to the confinement
               window. The confinement window is associated with some
               root finding activity. It is used to determine how much
               total time is being searched in order to find the events
               of interest.

               `et' is an epoch belonging to the interval [ivbeg, ivend].

               In order for a meaningful progress report to be
               displayed, `ivbeg' and `ivend' must satisfy the following
               constraints:

               -  `ivbeg' must be less than or equal to `ivend'.

               -  The interval [ `ivbeg', `ivend' ] must be contained in
                  some interval of the confinement window. It can be
                  a proper subset of the containing interval; that
                  is, it can be smaller than the interval of the
                  confinement window that contains it.

               -  Over a search, the sum of the differences

                     ivend - ivbeg

                  for all calls to this routine made during the search
                  must equal the measure of the confinement window.

               The CSPICE routine gfrepu_c may be used as the actual
               argument corresponding to `udrepu'. If so, the CSPICE
               routines gfrepi_c and gfrepf_c must be the actual arguments
               corresponding to `udrepi' and `udrepf'.

   udrepf      is a user-defined routine that finalizes a progress
               report. `udrepf' has no arguments.

               The CSPICE routine gfrepf_c may be used as the actual
               argument corresponding to `udrepf'. If so, the CSPICE
               routines gfrepi_c and gfrepu_c must be the actual arguments
               corresponding to `udrepi' and `udrepu'.

   bail        is a logical variable indicating whether or not interrupt
               handling is enabled. When `bail' is set to SPICETRUE, the
               input function `udbail' (see description below) is used to
               determine whether an interrupt has been issued.

   udbail      is the name of a user defined logical function that
               indicates whether an interrupt signal has been issued
               (for example, from the keyboard). The prototype of `udbail'
               is:

                  SpiceBoolean   ( * udbail ) ( void )

               The return value is SPICETRUE if an interrupt has
               been issued; otherwise the value is SPICEFALSE.

               gffove_c uses `udbail' only when `bail' (see above) is set to
               SPICETRUE, indicating that interrupt handling is enabled.
               When interrupt handling is enabled, gffove_c and routines
               in its call tree will call `udbail' to determine whether to
               terminate processing and return immediately.

               If interrupt handing is not enabled, a logical function
               must still be passed to gffove_c as an input argument. The
               CSPICE routine

                  gfbail_c

               may be used for this purpose.

               The function `udbail' will be usually be tested
               multiple times by the GF system between the time
               an interrupt is issued and the time when
               control is returned to the calling program, so
               `udbail' must continue to return SPICETRUE
               until explicitly reset by the calling application.
               So `udbail' must provide a "reset" mechanism."
               In the case of gfbail_c, the reset function is

                  gfclrh_c

               If interrupt handing is not enabled, a logical
               function must still be passed to gffove_c as
               an input argument. The CSPICE function

                  gfbail_c

               may be used for this purpose.

               See the -Examples header section below for a complete code
               example demonstrating use of the CSPICE interrupt
               handling capability.

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

   result      is a SPICE window representing the set of time
               intervals, within the confinement period, when image
               of the target body is partially or completely within
               the specified instrument field of view.

               `result' must be declared and initialized with sufficient
               size to capture the full set of time intervals within the
               search region on which the specified condition is satisfied.

               If `result' is non-empty on input, its contents will be
               discarded before gffove_c conducts its search.

               The endpoints of the time intervals comprising `result'
               are interpreted as seconds past J2000 TDB.

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
              within an interval of length SPICE_GF_CNVTOL; the root is
              considered to have been found.

              The accuracy, as opposed to precision, of roots found by
              this routine depends on the accuracy of the input data.
              In most cases, the accuracy of solutions will be inferior
              to their precision.


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

   3)  If the name of either the target or observer cannot be
       translated to a NAIF ID code, an error is signaled by
       a routine in the call tree of this routine.

   4)  If the specified aberration correction is not a supported
       value for the target type (ephemeris object or ray), an error
       is signaled by a routine in the call tree of this routine.

   5)  If the radii of a target body modeled as an ellipsoid cannot
       be determined by searching the kernel pool for a kernel
       variable having a name of the form

          "BODYnnn_RADII"

       where nnn represents the NAIF integer code associated with
       the body, an error is signaled by a routine in the
       call tree of this routine.

   6)  If the target body coincides with the observer body `obsrvr', an
       error is signaled by a routine in the call tree of this
       routine.

   7)  If the body model specifier `tshape' is not recognized, an error
       is signaled by a routine in the call tree of this routine.

   8)  If a target body-fixed reference frame associated with a
       non-point target is not recognized, an error is signaled by a
       routine in the call tree of this routine.

   9)  If a target body-fixed reference frame is not centered at the
       corresponding target body, an error is signaled by a routine
       in the call tree of this routine.

   10) If the instrument name `inst' does not have corresponding NAIF
       ID code, an error is signaled by a routine in the call
       tree of this routine.

   11) If the FOV parameters of the instrument are not present in
       the kernel pool, an error is signaled by a routine
       in the call tree of this routine.

   12) If the FOV boundary has more than SPICE_GF_MAXVRT vertices, an error
       is signaled by a routine in the call tree of this
       routine.

   13) If the instrument FOV is polygonal, and this routine cannot
       find a ray R emanating from the FOV vertex such that maximum
       angular separation of R and any FOV boundary vector is within
       the limit (pi/2)-SPICE_GF_MARGIN radians, an error is signaled
       by a routine in the call tree of this routine. If the FOV is
       any other shape, the same error check will be applied with the
       instrument boresight vector serving the role of R.

   14) If the loaded kernels provide insufficient data to compute a
       requested state vector, an error is signaled by a
       routine in the call tree of this routine.

   15) If an error occurs while reading an SPK or other kernel file,
       the error is signaled by a routine in the call tree
       of this routine.

   16) If the output SPICE window `result' has insufficient capacity
       to contain the number of intervals on which the specified
       visibility condition is met, an error is signaled
       by a routine in the call tree of this routine.

   17) If the result window has size less than 2, the error
       SPICE(WINDOWTOOSMALL) is signaled by a routine in the call
       tree of this routine.

   18) If the convergence tolerance size is non-positive, the error
       SPICE(INVALIDTOLERANCE) is signaled by a routine in the call
       tree of this routine.

   19) If the step size is non-positive, an error is signaled by a
       routine in the call tree of this routine.

   20) If the ray's direction vector is zero, an error is signaled by
       a routine in the call tree of this routine.

   21) If operation of this routine is interrupted, the output result
       window will be invalid.

   22) If any of the `inst', `tshape', `abcorr', `tframe', `target'
       or `obsrvr' input string pointers is null, the error
       SPICE(NULLPOINTER) is signaled.

   23) If any of the `inst', `tshape' or `abcorr' input strings has
       zero length, the error SPICE(EMPTYSTRING) is signaled.

   24) If any the `cnfine' or `result' cell arguments has a type
       other than SpiceDouble, the error SPICE(TYPEMISMATCH) is
       signaled.

   25) If any attempt to change the handler for the interrupt signal
       SIGINT fails, the error SPICE(SIGNALFAILED) is signaled.

-Files

   Appropriate SPICE kernels must be loaded by the
   calling program before this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for target and observer that
      describes the ephemeris of these objects for the period
      defined by the confinement window, `cnfine' must be
      loaded. If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data.
      Typically ephemeris data are made available by loading one
      or more SPK files via furnsh_c.

   -  Frame data: if a frame definition is required to convert
      the observer and target states to the body-fixed frame of
      the target, that definition must be available in the kernel
      pool. Typically the definitions of frames not already
      built-in to SPICE are supplied by loading a frame kernel.

      Data defining the reference frame associated with the
      instrument designated by `inst' must be available in the kernel
      pool. Additionally the name `inst' must be associated with an
      ID code. Normally these data are  made available by loading
      a frame kernel via furnsh_c.

   -  IK data: the kernel pool must contain data such that
      the CSPICE routine getfov_c may be called to obtain
      parameters for `inst'. Normally such data are provided by
      an IK via furnsh_c.

   The following data may be required:

   -  PCK data: bodies modeled as triaxial ellipsoids must have
      orientation data provided by variables in the kernel pool.
      Typically these data are made available by loading a text
      PCK file via furnsh_c.

      Bodies modeled as triaxial ellipsoids must have semi-axis
      lengths provided by variables in the kernel pool. Typically
      these data are made available by loading a text PCK file via
      furnsh_c.

   -  CK data: if the instrument frame is fixed to a spacecraft,
      at least one CK file will be needed to permit transformation
      of vectors between that frame and both J2000 and the target
      body-fixed frame.

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

   This routine determines a set of one or more time intervals within
   the confinement window when a specified ray or any portion of a
   specified target body appears within the field of view of a
   specified instrument. We'll use the term "visibility event" to
   designate such an appearance. The set of time intervals resulting
   from the search is returned as a SPICE window.

   This routine provides the SPICE GF system's most flexible
   interface for searching for FOV intersection events.

   Applications that require do not require support for progress
   reporting, interrupt handling, non-default step or refinement
   functions, or non-default convergence tolerance normally should
   call either gftfov_c or gfrfov_c rather than this routine.

   Below we discuss in greater detail aspects of this routine's
   solution process that are relevant to correct and efficient use
   of this routine in user applications.


   The Search Process
   ==================

   The search for visibility events is treated as a search for state
   transitions: times are sought when the state of the target ray or
   body changes from "not visible" to "visible" or vice versa.

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

   The times of state transitions are called ``roots.''

   Once a root has been bracketed, a refinement process is used to
   narrow down the time interval within which the root must lie. This
   refinement process terminates when the location of the root has been
   determined to within an error margin called the "convergence
   tolerance."

   The convergence tolerance used by high-level GF routines that call
   this routine is set via the parameter SPICE_GF_CNVTOL, which is
   declared in the header file SpiceGF.h. The value of SPICE_GF_CNVTOL
   is set to a "tight" value so that the tolerance doesn't become the
   limiting factor in the accuracy of solutions found by this routine.
   In general the accuracy of input data will be the limiting factor.

   Setting the input tolerance `tol' tighter than SPICE_GF_CNVTOL is
   unlikely to be useful, since the results are unlikely to be more
   accurate. Making the tolerance looser will speed up searches
   somewhat, since a few convergence steps will be omitted. However, in
   most cases, the step size is likely to have a much greater effect on
   processing time than would the convergence tolerance.


   The Confinement Window
   ======================

   The simplest use of the confinement window is to specify a time
   interval within which a solution is sought. However, the confinement
   window can, in some cases, be used to make searches more efficient.
   Sometimes it's possible to do an efficient search to reduce the size
   of the time period over which a relatively slow search of interest
   must be performed. For an example, see the program CASCADE in the
   GF Example Programs chapter of the GF Required Reading, gf.req.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Conduct a search using default GF progress reporting
      and interrupt handling capabilities.

      The program will use console I/O to display a simple
      ASCII-based progress report.

      The program will trap keyboard interrupts (on most systems,
      generated by typing the "control C" key combination). This
      feature can be used in non-trivial applications to allow
      the application to continue after a search as been interrupted.

      Search for times when Saturn's satellite Phoebe is within
      the FOV of the Cassini narrow angle camera (CASSINI_ISS_NAC).
      To simplify the problem, restrict the search to a short time
      period where continuous Cassini bus attitude data are
      available.

      Use a step size of 1 second to reduce chances of missing
      short visibility events.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: gffove_ex1.tm

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
            pck00010.tpc                    Satellite orientation
                                            and radii
            041014R_SCPSE_01066_04199.bsp   CASSINI, planetary and
                                            Saturn satellite
                                            ephemeris
            cas_v40.tf                      Cassini FK
            04161_04164ra.bc                Cassini bus CK
            cas00071.tsc                    Cassini SCLK kernel
            cas_iss_v10.ti                  Cassini IK


         \begindata

            KERNELS_TO_LOAD = ( 'naif0012.tls',
                                'pck00010.tpc',
                                '041014R_SCPSE_01066_04199.bsp',
                                'cas_v40.tf',
                                '04161_04164ra.bc',
                                'cas00071.tsc',
                                'cas_iss_v10.ti'            )
         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program gffove_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local constants
         ./
         #define  META           "gffove_ex1.tm"
         #define  TIMFMT         "YYYY-MON-DD HR:MN:SC.######::TDB (TDB)"
         #define  TIMLEN         41
         #define  MAXWIN         10000
         #define  TIMTOL         1.e-6

         /.
         Local variables
         ./
         SPICEDOUBLE_CELL ( cnfine, MAXWIN );
         SPICEDOUBLE_CELL ( result, MAXWIN );

         SpiceBoolean            bail;
         SpiceBoolean            rpt;

         SpiceChar             * abcorr;
         SpiceChar             * inst;
         SpiceChar             * obsrvr;
         SpiceChar             * target;
         SpiceChar             * tframe;
         SpiceChar               timstr  [2][ TIMLEN ];
         SpiceChar             * tshape;

         SpiceDouble             endpt   [2];
         SpiceDouble             et0;
         SpiceDouble             et1;
         SpiceDouble             raydir [3];

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
         inst   = "CASSINI_ISS_NAC";
         target = "PHOEBE";
         tshape = "ELLIPSOID";
         tframe = "IAU_PHOEBE";
         abcorr = "LT+S";
         obsrvr = "CASSINI";

         /.
         Select a 1-second step. We'll ignore any target
         appearances lasting less than 1 second.
         ./
         gfsstp_c ( 1.0 );

         printf ( "\n"
                  "Instrument: %s\n"
                  "Target:     %s\n",
                  inst,
                  target            );

         /.
         Turn on interrupt handling and progress reporting.
         ./
         bail = SPICETRUE;
         rpt  = SPICETRUE;

         /.
         Perform the search.
         ./
         gffove_c ( inst,      tshape,    raydir,    target,    tframe,
                    abcorr,    obsrvr,    TIMTOL,    gfstep_c,  gfrefn_c,
                    rpt,       gfrepi_c,  gfrepu_c,  gfrepf_c,  bail,
                    gfbail_c,  &cnfine,   &result                        );

         if ( gfbail_c() )
         {
            /.
            Clear the CSPICE interrupt indication. This is
            an essential step for programs that continue
            running after an interrupt; gfbail_c will
            continue to return SPICETRUE until this step
            has been performed.
            ./
            gfclrh_c();

            /.
            We've trapped an interrupt signal. In a realistic
            application, the program would continue operation
            from this point. In this simple example, we simply
            display a message and quit.
            ./
            printf ( "\nSearch was interrupted.\n\nThis message "
                     "was written after an interrupt signal\n"
                     "was trapped. By default, the program "
                     "would have terminated \nbefore this message "
                     "could be written.\n\n"                       );
         }
         else
         {

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

                  printf ( "  %s  %s\n", timstr[0], timstr[1] );
               }
            }

            printf ( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Instrument: CASSINI_ISS_NAC
      Target:     PHOEBE

      Target visibility search 100.00% done.

        Visibility start time              Stop time
        2004-JUN-11 07:35:27.066980 (TDB)  2004-JUN-11 08:48:03.954696 (TDB)
        2004-JUN-11 09:02:56.580045 (TDB)  2004-JUN-11 09:35:04.038509 (TDB)
        2004-JUN-11 09:49:56.476397 (TDB)  2004-JUN-11 10:22:04.242879 (TDB)
        2004-JUN-11 10:36:56.283771 (TDB)  2004-JUN-11 11:09:04.397165 (TDB)
        2004-JUN-11 11:23:56.020645 (TDB)  2004-JUN-11 11:56:04.733536 (TDB)


      Note that the progress report has the format shown below:

         Target visibility search   2.66% done.

      The completion percentage was updated approximately once per
      second.

   2) A variation of example (1): search the same confinement
      window for times when a selected background star is visible.
      We use the FOV of the Cassini ISS wide angle camera
      (CASSINI_ISS_WAC) to enhance the probability of viewing the
      star.

      The star we'll use has catalog number 6000 in the Hipparcos
      Catalog. The star's J2000 right ascension and declination,
      proper motion, and parallax are taken from that catalog.

      Use the meta-kernel from the first example.

      Example code begins here.


      /.
         Program gffove_ex2
      ./
      #include <stdio.h>
      #include <math.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local constants
         ./
         #define  META           "gffove_ex1.tm"
         #define  TIMFMT         "YYYY-MON-DD HR:MN:SC.######::TDB (TDB)"
         #define  TIMLEN         41
         #define  MAXWIN         10000
         #define  TIMTOL         1.e-6
         #define  AU             149597870.693

         /.
         Local variables
         ./
         SPICEDOUBLE_CELL ( cnfine, MAXWIN );
         SPICEDOUBLE_CELL ( result, MAXWIN );

         SpiceBoolean            bail;
         SpiceBoolean            rpt;

         SpiceChar             * abcorr;
         SpiceChar             * inst;
         SpiceChar             * obsrvr;
         SpiceChar             * rframe;
         SpiceChar             * target;
         SpiceChar               timstr  [2][ TIMLEN ];
         SpiceChar             * tshape;

         SpiceDouble             dec;
         SpiceDouble             decdeg;
         SpiceDouble             decdg0;
         SpiceDouble             decepc;
         SpiceDouble             decpm;
         SpiceDouble             dtdec;
         SpiceDouble             dtra;
         SpiceDouble             endpt   [2];
         SpiceDouble             et0;
         SpiceDouble             et1;
         SpiceDouble             lt;
         SpiceDouble             parlax;
         SpiceDouble             plxdeg;
         SpiceDouble             pos     [3];
         SpiceDouble             pstar   [3];
         SpiceDouble             ra;
         SpiceDouble             radeg0;
         SpiceDouble             radeg;
         SpiceDouble             raepc;
         SpiceDouble             rapm;
         SpiceDouble             raydir  [3];
         SpiceDouble             rstar;
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
         target = " ";
         tshape = "RAY";

         /.
         Create a unit direction vector pointing from
         observer to star. We'll assume the direction
         is constant during the confinement window, and
         we'll use et0 as the epoch at which to compute the
         direction from the spacecraft to the star.

         The data below are for the star with catalog
         number 6000 in the Hipparcos catalog. Angular
         units are degrees; epochs have units of Julian
         years and have a reference epoch of J1950.
         The reference frame is J2000.
         ./
         catno  = 6000;

         plxdeg = 0.000001056;

         radeg0 = 19.290789927;
         rapm   = -0.000000720;
         raepc  = 41.2000;

         decdg0 =  2.015271007;
         decpm  =  0.000001814;
         decepc = 41.1300;

         rframe = "j2000";

         /.
         Correct the star's direction for proper motion.

         The argument t represents et0 as Julian years
         past J1950.
         ./
         t      =    ( et0      / jyear_c() )
                  +  ( j2000_c()- j1950_c() ) / 365.25;

         dtra   = t - raepc;
         dtdec  = t - decepc;

         radeg  = radeg0  +  dtra  * rapm;
         decdeg = decdg0  +  dtdec * decpm;

         ra     = radeg  * rpd_c();
         dec    = decdeg * rpd_c();

         radrec_c ( 1.0, ra, dec, pstar );

         /.
         Correct star position for parallax applicable at
         the Cassini orbiter's position. (The parallax effect
         is negligible in this case; we're simply demonstrating
         the computation.)
         ./
         parlax = plxdeg * rpd_c();
         rstar  = AU / tan(parlax);

         /.
         Scale the star's direction vector by its distance from
         the solar system barycenter. Subtract off the position
         of the spacecraft relative to the solar system barycenter;
         the result is the ray's direction vector.
         ./
         vscl_c ( rstar, pstar, pstar );

         spkpos_c ( "cassini", et0, "j2000",  "none",
                    "solar system barycenter", pos,  &lt );

         vsub_c   ( pstar, pos, raydir );

         /.
         Correct the star direction for stellar aberration when
         we conduct the search.
         ./
         abcorr = "S";
         obsrvr = "CASSINI";

         /.
         Select a 1-second step. We'll ignore any target
         appearances lasting less than 1 second.
         ./
         gfsstp_c ( 1.0 );

         /.
         Turn on interrupt handling and progress reporting.
         ./
         bail = SPICETRUE;
         rpt  = SPICETRUE;


         printf ( "\n"
                  "Instrument:            %s\n"
                  "Star's catalog number: %d\n",
                  inst,
                  (int)catno                        );

         /.
         Perform the search.
         ./
         gffove_c ( inst,      tshape,    raydir,    target,    rframe,
                    abcorr,    obsrvr,    TIMTOL,    gfstep_c,  gfrefn_c,
                    rpt,       gfrepi_c,  gfrepu_c,  gfrepf_c,  bail,
                    gfbail_c,  &cnfine,   &result                        );

         if ( gfbail_c() )
         {
            /.
            Clear the CSPICE interrupt indication. This is
            an essential step for programs that continue
            running after an interrupt; gfbail_c will
            continue to return SPICETRUE until this step
            has been performed.
            ./
            gfclrh_c();

            /.
            We've trapped an interrupt signal. In a realistic
            application, the program would continue operation
            from this point. In this simple example, we simply
            display a message and quit.
            ./
            printf ( "\nSearch was interrupted.\n\nThis message "
                     "was written after an interrupt signal\n"
                     "was trapped. By default, the program "
                     "would have terminated \nbefore this message "
                     "could be written.\n\n"                       );
         }
         else
         {

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

                  printf ( "  %s  %s\n", timstr[0], timstr[1] );
               }
            }

            printf ( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Instrument:            CASSINI_ISS_WAC
      Star's catalog number: 6000

      Target visibility search 100.00% done.

        Visibility start time              Stop time
        2004-JUN-11 06:30:00.000000 (TDB)  2004-JUN-11 12:00:00.000000 (TDB)


-Restrictions

   1)  The kernel files to be used by gffove_c must be loaded (normally via
       the CSPICE routine furnsh_c) before gffove_c is called.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 06-AUG-2021 (JDR)

       Edited the header to comply to comply with NAIF standard.

       Updated Examples' kernels set to use PDS archived data.

       Updated the description of "cnfine" and "result" arguments.

       Added entries #17 and #24 in -Exceptions section.

   -CSPICE Version 1.0.1, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.0, 15-APR-2009 (NJB) (EDW)

-Index_Entries

   GF low-level target in instrument FOV search

-&
*/

{ /* Begin gffove_c */


   /*
   Prototypes
   */
   void                  ( * defSigHandler ) (int);
   void                  ( * sigPtr        ) (int);

   /*
   Local variables
   */
   logical                   interrupt;
   logical                   rep;

   SpiceBoolean              newHandler;

   SpiceChar               * obsStr;
   SpiceChar               * targStr;
   SpiceChar               * tFrameStr;

   /*
   Static variables
   */
   static const SpiceChar  * blankStr = " ";



   /*
   Participate in error tracing.
   */
   chkin_c ( "gffove_c" );

   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gffove_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary.
   */
   CELLINIT2 ( cnfine, result );

   /*
   Make sure the frame name, target, and observer pointers
   are non-null.
   */
   CHKPTR ( CHK_STANDARD, "gffove_c", tframe );
   CHKPTR ( CHK_STANDARD, "gffove_c", target );
   CHKPTR ( CHK_STANDARD, "gffove_c", obsrvr );

   /*
   The input frame name, observer name, and target name are special
   cases because we allow the caller to pass in an empty strings for
   any of these. If any of one of these strings is empty, we pass in
   its place a null-terminated string containing one blank character to
   the underlying f2c'd routine.
   */
   if ( tframe[0] )
   {
      tFrameStr = (SpiceChar *) tframe;
   }
   else
   {
      tFrameStr = (SpiceChar *) blankStr;
   }

   if ( target[0] )
   {
      targStr   = (SpiceChar *) target;
   }
   else
   {
      targStr   = (SpiceChar *) blankStr;
   }

   if ( obsrvr[0] )
   {
      obsStr    = (SpiceChar *) obsrvr;
   }
   else
   {
      obsStr    = (SpiceChar *) blankStr;
   }


   /*
   Check the other input strings to make sure each pointer is non-null
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gffove_c", inst   );
   CHKFSTR ( CHK_STANDARD, "gffove_c", tshape );
   CHKFSTR ( CHK_STANDARD, "gffove_c", abcorr );

   rep       = (logical) rpt;
   interrupt = (logical) bail;


   /*
   Store the input function pointers so these functions can be
   called by the GF adapters.
   */
   zzadsave_c ( UDSTEP,  (void *)(udstep)  );
   zzadsave_c ( UDREFN,  (void *)(udrefn)  );
   zzadsave_c ( UDREPF,  (void *)(udrepf)  );
   zzadsave_c ( UDREPI,  (void *)(udrepi)  );
   zzadsave_c ( UDREPU,  (void *)(udrepu)  );
   zzadsave_c ( UDBAIL,  (void *)(udbail)  );


   /*
   If interrupt handling is enabled, and if the default bail-out
   routine gfbail_c is being used, set the SPICE interrupt
   handler.
   */

   newHandler = SPICEFALSE;

   if ( bail )
   {
      newHandler = (  (void *)udbail == (void *)gfbail_c );

      if ( newHandler )
      {
         defSigHandler = signal ( SIGINT, gfinth_c );

         if ( defSigHandler == SIG_ERR )
         {
            setmsg_c ( "Attempt to establish the CSPICE routine "
                       "gfinth_c as the handler for the interrupt "
                       "signal SIGINT failed."                     );
            sigerr_c ( "SPICE(SIGNALFAILED)"                       );
            chkout_c ( "gffove_c"                                  );
            return;
         }
      }
   }

   gffove_ ( ( char       * ) inst,
             ( char       * ) tshape,
             ( doublereal * ) raydir,
             ( char       * ) target,
             ( char       * ) tFrameStr,
             ( char       * ) abcorr,
             ( char       * ) obsrvr,
             ( doublereal * ) &tol,
             ( U_fp         ) zzadstep_c,
             ( U_fp         ) zzadrefn_c,
             ( logical    * ) &rep,
             ( S_fp         ) zzadrepi_c,
             ( U_fp         ) zzadrepu_c,
             ( S_fp         ) zzadrepf_c,
             ( logical    * ) &interrupt,
             ( L_fp         ) zzadbail_c,
             ( doublereal * ) (cnfine->base),
             ( doublereal * ) (result->base),
             ( ftnlen       ) strlen(inst),
             ( ftnlen       ) strlen(tshape),
             ( ftnlen       ) strlen(target),
             ( ftnlen       ) strlen(tframe),
             ( ftnlen       ) strlen(abcorr),
             ( ftnlen       ) strlen(obsrvr)  );

   /*
   If we've changed the signal handler, restore the previous one.
   */
   if ( newHandler )
   {
      sigPtr = signal ( SIGINT, defSigHandler );

      if ( sigPtr == SIG_ERR )
      {
         setmsg_c ( "Attempt to restore the previous handler "
                    "for the interrupt signal SIGINT failed."  );
         sigerr_c ( "SPICE(SIGNALFAILED)"                      );
         chkout_c ( "gffove_c"                                 );
         return;
      }
   }

   /*
   Sync the output result cell.
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, result );
   }

   chkout_c ( "gffove_c" );

} /* End gffove_c */
