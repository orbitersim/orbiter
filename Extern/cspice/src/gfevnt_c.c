/*

-Procedure gfevnt_c (GF, geometric event finder )

-Abstract

   Determine time intervals when a specified geometric quantity
   satisfies a specified mathematical condition.

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
   #undef gfevnt_c

   void gfevnt_c ( void             ( * udstep ) ( SpiceDouble       et,
                                                   SpiceDouble     * step ),

                   void             ( * udrefn ) ( SpiceDouble       t1,
                                                   SpiceDouble       t2,
                                                   SpiceBoolean      s1,
                                                   SpiceBoolean      s2,
                                                   SpiceDouble     * t    ),
                   ConstSpiceChar     * gquant,
                   SpiceInt             qnpars,
                   SpiceInt             lenvals,
                   const void         * qpnams,
                   const void         * qcpars,
                   ConstSpiceDouble   * qdpars,
                   ConstSpiceInt      * qipars,
                   ConstSpiceBoolean  * qlpars,
                   ConstSpiceChar     * op,
                   SpiceDouble          refval,
                   SpiceDouble          tol,
                   SpiceDouble          adjust,
                   SpiceBoolean         rpt,

                   void             ( * udrepi ) ( SpiceCell       * cnfine,
                                                   ConstSpiceChar  * srcpre,
                                                   ConstSpiceChar  * srcsuf ),

                   void             ( * udrepu ) ( SpiceDouble       ivbeg,
                                                   SpiceDouble       ivend,
                                                   SpiceDouble       et      ),

                   void             ( * udrepf ) ( void ),
                   SpiceInt             nintvls,
                   SpiceBoolean         bail,
                   SpiceBoolean     ( * udbail ) ( void ),
                   SpiceCell          * cnfine,
                   SpiceCell          * result )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   SPICE_GFEVNT_MAXPAR
              P   Maximum number of parameters required to define
                  any quantity.
   SPICE_GF_CNVTOL
              P   Default convergence tolerance.
   udstep     I   Name of the routine that computes and returns a
                  time step.
   udrefn     I   Name of the routine that computes a refined time.
   gquant     I   Type of geometric quantity.
   qnpars     I   Number of quantity definition parameters.
   lenvals    I   Length of strings in 'qpnams' and 'qcpars'.
   qpnams     I   Names of quantity definition parameters.
   qcpars     I   Array of character quantity definition parameters.
   qdpars     I   Array of double precision quantity definition
                  parameters.
   qipars     I   Array of integer quantity definition parameters.
   qlpars     I   Array of logical quantity definition parameters.
   op         I   Operator that either looks for an extreme value
                  (max, min, local, absolute) or compares the
                  geometric quantity value and a number.
   refval     I   Reference value.
   tol        I   Convergence tolerance in seconds
   adjust     I   Absolute extremum adjustment value.
   rpt        I   Progress reporter on (SPICETRUE) or off (SPICEFALSE).
   udrepi     I   Function that initializes progress reporting.
   udrepu     I   Function that updates the progress report.
   udrepf     I   Function that finalizes progress reporting.
   nintvls    I   Workspace window interval count
   bail       I   Logical indicating program interrupt monitoring.
   udbail     I   Name of a routine that signals a program interrupt.
   cnfine    I-O  SPICE window to which the search is restricted.
   result     O   SPICE window containing results.

-Detailed_Input

   udstep      is an externally specified routine that computes a
               time step in an attempt to find a transition of the
               state being considered. In the context of this
               routine's algorithm, a "state transition" occurs where
               the geometric state changes from being in the desired
               geometric condition event to not, or vice versa.

               This routine relies on `udstep' returning step sizes small
               enough so that state transitions within the confinement
               window are not overlooked. There must never be two roots
               A and B separated by less than `step', where `step' is the
               minimum step size returned by `udstep' for any value of `et'
               in the interval [A, B].

               The prototype for `udstep' is

                  void   ( * udstep ) ( SpiceDouble       et,
                                        SpiceDouble     * step )

               where:

                  et      is the input start time from which the
                          algorithm is to search forward for a state
                          transition. `et' is expressed as seconds past
                          J2000 TDB.

                  step    is the output step size. `step' indicates how
                          far to advance `et' so that `et' and et+step may
                          bracket a state transition and definitely do
                          not bracket more than one state transition.
                          Units are TDB seconds.

               If a constant step size is desired, the CSPICE routine

                  gfstep_c

               may be used as the step size function. This is the
               default option. If gfstep_c is used, the step size must be
               set by calling

                  gfsstp_c ( step );

               prior to calling this routine.

   udrefn      is the name of the externally specified routine that computes
               a refinement in the times that bracket a transition point.
               In other words, once a pair of times have been detected
               such that the system is in different states at each of
               the two times, `udrefn' selects an intermediate time which
               should be closer to the transition state than one of the
               two known times.

               The prototype for `udrefn' is:

                  void   ( * udrefn ) ( SpiceDouble       t1,
                                        SpiceDouble       t2,
                                        SpiceBoolean      s1,
                                        SpiceBoolean      s2,
                                        SpiceDouble     * t   )

               where the inputs are:

                  t1    is a time when the system is in state `s1'. `t1' is
                        expressed as seconds past J2000 TDB.

                  t2    is a time when the system is in state `s2'. `t2' is
                        expressed as seconds past J2000 TDB. `t2' is
                        assumed to be larger than `t1'.

                  s1    is the state of the system at time `t1'.

                  s2    is the state of the system at time `t2'.

               `udrefn' may use or ignore the `s1' and `s2' values.

               The output is:

                  t     is next time to check for a state transition.
                        `t' has value between `t1' and `t2'. `t' is
                        expressed as seconds past J2000 TDB.

               If a simple bisection method is desired, the CSPICE
               routine

                  gfrefn_c

               may be used as the refinement function. This is the
               default option.

   gquant      is a string containing the name of a geometric quantity.
               The times when this quantity satisfies a condition
               specified by the arguments `op' and `adjust' (described
               below) are to be found.

               Each quantity is specified by the quantity name given in
               argument `gquant', and by a set of parameters specified by
               the arguments

                  qnpars
                  qpnams
                  qcpars
                  qdpars
                  qipars
                  qlpars

               For each quantity listed here, we also show how to set up
               these input arguments to define the quantity. See the
               detailed discussion of these arguments below for further
               information.

               `gquant' may be any of the strings:

                  "ANGULAR SEPARATION"
                  "COORDINATE"
                  "DISTANCE"
                  "ILLUMINATION ANGLE"
                  "PHASE ANGLE"
                  "RANGE RATE"

               `gquant' strings are case insensitive. Values, meanings,
               and associated parameters are discussed below.

               The aberration correction parameter indicates the
               aberration corrections to be applied to the state of the
               target body to account for one-way light time and stellar
               aberration. If relevant, it applies to the rotation of
               the target body as well.

               Supported aberration correction options for observation
               (case where radiation is received by observer at `et') are:

                  "NONE"          No correction.
                  "LT"            Light time only.
                  "LT+S"          Light time and stellar aberration.
                  "CN"            Converged Newtonian (CN) light time.
                  "CN+S"          CN light time and stellar aberration.

               Supported aberration correction options for transmission
               (case where radiation is emitted from observer at `et')
               are:

                  "XLT"           Light time only.
                  "XLT+S"         Light time and stellar aberration.
                  "XCN"           Converged Newtonian (CN) light time.
                  "XCN+S"         CN light time and stellar aberration.

               For detailed information, see the geometry finder
               required reading, gf.req.

               Case, leading and trailing blanks are not significant in
               aberration correction parameter strings.


               ANGULAR SEPARATION

                  is the apparent angular separation of two target
                  bodies as seen from an observing body.

                  Quantity Parameters:

                     qnpars    = 8;
                     SpiceChar qpnams[qnpars][LNSIZE] =
                                     { "TARGET1",
                                       "FRAME1",
                                       "SHAPE1",
                                       "TARGET2",
                                       "FRAME2",
                                       "SHAPE2",
                                       "OBSERVER",
                                       "ABCORR"    };

                     SpiceChar qcpars[qnpars][LNSIZE] =
                                     { <name of first target>,
                                       <name of body-fixed frame
                                                 of first target>,
                                       <shape of first target>,
                                       <name of second target>,
                                       <name of body-fixed frame
                                                of second target>,
                                       <shape of second target>,
                                       <name of observer>,
                                       <aberration correction>     };

                  The target shape model specifiers may be set to either
                  of the values

                     "POINT"
                     "SPHERE"

                  The shape models for the two bodies need not match.

                  Spherical models have radii equal to the longest
                  equatorial radius of the PCK-based tri-axial
                  ellipsoids used to model the respective bodies. When
                  both target bodies are modeled as spheres, the angular
                  separation between the bodies is the angle between the
                  closest points on the limbs of the spheres, as viewed
                  from the vantage point of the observer. If the limbs
                  overlap, the angular separation is negative.

                  (In this case, the angular separation is the angle
                  between the centers of the spheres minus the sum of
                  the apparent angular radii of the spheres.)


               COORDINATE

                  is a coordinate of a specified vector in a specified
                  reference frame and coordinate system. For example, a
                  coordinate can be the Z component of the earth-sun
                  vector in the J2000 reference frame, or the latitude
                  of the nearest point on Mars to an orbiting
                  spacecraft, expressed relative to the IAU_MARS
                  reference frame.

                  The method by which the vector is defined is indicated
                  by the

                     "VECTOR DEFINITION"

                  parameter. Allowed values and meanings of this
                  parameter are:

                     "POSITION"

                        The vector is defined by the position of a
                        target relative to an observer.

                     "SUB-OBSERVER POINT"

                        The vector is the sub-observer point on a
                        specified target body.

                     "SURFACE INTERCEPT POINT"

                        The vector is defined as the intercept point of
                        a vector from the observer to the target body.

                  Some vector definitions, such as the sub-observer
                  point, may be specified by a variety of methods, so a
                  parameter is provided to select the computation
                  method. The computation method parameter name is

                     "METHOD"

                  If the vector definition is

                     "POSITION"

                  the "METHOD" parameter must be set to blank:

                     " "

                  If the vector definition is

                     "SUB-OBSERVER POINT"

                  the "METHOD" parameter must be set to either:

                     "Near point: ellipsoid"
                     "Intercept: ellipsoid"

                  If the vector definition is

                     "SURFACE INTERCEPT POINT"

                  the "METHOD" parameter must be set to:

                     "Ellipsoid"

                        The intercept computation uses a triaxial
                        ellipsoid to model the surface of the target
                        body. The ellipsoid's radii must be available in
                        the kernel pool.

                  The supported coordinate systems and coordinate names:

                     Coordinate System  Coordinates        Range
                     -----------------  -----------------  ------------

                     "RECTANGULAR"      "X"
                                        "Y"
                                        "Z"

                     "LATITUDINAL"      "RADIUS"
                                        "LONGITUDE"        (-Pi,Pi]
                                        "LATITUDE"         [-Pi/2,Pi/2]

                     "RA/DEC"           "RANGE"
                                        "RIGHT ASCENSION"  [0,2Pi)
                                        "DECLINATION"      [-Pi/2,Pi/2]

                     "SPHERICAL"        "RADIUS"
                                        "COLATITUDE"       [0,Pi]
                                        "LONGITUDE"        (-Pi,Pi]

                     "CYLINDRICAL"      "RADIUS"
                                        "LONGITUDE"        [0,2Pi)
                                        "Z"

                     "GEODETIC"         "LONGITUDE"        (-Pi,Pi]
                                        "LATITUDE"         [-Pi/2,Pi/2]
                                        "ALTITUDE"

                     "PLANETOGRAPHIC"   "LONGITUDE"        [0,2Pi)
                                        "LATITUDE"         [-Pi/2,Pi/2]
                                        "ALTITUDE"

                  When geodetic coordinates are selected, the radii used
                  are those of the central body associated with the
                  reference frame. For example, if IAU_MARS is the
                  reference frame, then geodetic coordinates are
                  calculated using the radii of Mars taken from a SPICE
                  planetary constants kernel. One cannot ask for
                  geodetic coordinates for a frame which doesn't have an
                  extended body as its center.

                  Reference frame names must be recognized by the SPICE
                  frame subsystem.

                  Quantity Parameters:

                     qnpars = 10;
                     SpiceChar qpnams[qnpars][LNSIZE] =
                                     { "TARGET",
                                       "OBSERVER",
                                       "ABCORR",
                                       "COORDINATE SYSTEM",
                                       "COORDINATE",
                                       "REFERENCE FRAME",
                                       "VECTOR DEFINITION",
                                       "METHOD",
                                       "DREF",
                                       "DVEC"               };

                  Only "SURFACE INTERCEPT POINT" searches make use of
                  the "DREF" and "DVEC" parameters.

                     SpiceChar qcpars[qnpars][LNSIZE] =
                                     { <name of first target>,
                                       <name of observer>,
                                       <aberration correction>,
                                       <coordinate system name>,
                                       <coordinate name>,
                                       <target reference frame name>,
                                       <vector definition>,
                                       <computation method>,
                                       <reference frame of DVEC pointing
                                               vector, defined in qdpars> };

                     qdpars[0] = <DVEC pointing vector x component
                                                       from observer>
                     qdpars[1] = <DVEC pointing vector y component
                                                       from observer>
                     qdpars[2] = <DVEC pointing vector z component
                                                       from observer>

               DISTANCE

                  is the apparent distance between a target body and an
                  observing body. Distances are always measured between
                  centers of mass.

                  Quantity Parameters:

                     qnpars    = 3;
                     SpiceChar qpnams[qnpars][LNSIZE] =
                                     { "TARGET",
                                       "OBSERVER",
                                       "ABCORR"    };

                     SpiceChar qcpars[qnpars][LNSIZE] =
                                     { <name of target>,
                                       <name of observer>,
                                       <aberration correction> };


               ILLUMINATION ANGLE

                  is any of the illumination angles

                     emission
                     phase
                     solar incidence

                  defined at a surface point on a target body. These
                  angles are defined as in the CSPICE routine ilumin_c.

                  Quantity Parameters:

                     qnpars    = 8;
                     SpiceChar qpnams[qnpars][LNSIZE] =
                                     { "TARGET",
                                       "ILLUM",
                                       "OBSERVER",
                                       "ABCORR",
                                       "FRAME",
                                       "ANGTYP",
                                       "METHOD",
                                       "SPOINT"    };

                     SpiceChar qcpars[qnpars][LNSIZE] =
                                     { <name of target>,
                                       <name of illumination source>,
                                       <name of observer>,
                                       <aberration correction>,
                                       <target body-fixed frame>,
                                       <type of illumination angle>,
                                       <computation method>          };

                  The surface point is specified using rectangular
                  coordinates in the specified body-fixed frame.

                     qdpars[0] =  <X coordinate of surface point>
                     qdpars[1] =  <Y coordinate of surface point>
                     qdpars[2] =  <Z coordinate of surface point>

               PHASE ANGLE

                  is the apparent phase angle between a target body
                  center and an illuminating body center as seen from an
                  observer.

                  Quantity Parameters:

                     qnpars    = 4;
                     SpiceChar qpnams[qnpars][LNSIZE] =
                                     { "TARGET",
                                       "OBSERVER",
                                       "ABCORR",
                                       "ILLUM"    };

                     SpiceChar qcpars[qnpars][LNSIZE] =
                                     { <name of target>,
                                       <name of observer>,
                                       <aberration correction>,
                                       <name of illuminating body> };


               RANGE RATE

                  is the apparent range rate between a target body and
                  an observing body.

                  Quantity Parameters:

                     qnpars    = 3;
                     SpiceChar qpnams[qnpars][LNSIZE] =
                                     { "TARGET",
                                       "OBSERVER",
                                       "ABCORR"   };

                     SpiceChar qcpars[qnpars][LNSIZE] =
                                     { <name of target>,
                                       <name of observer>,
                                       <aberration correction> };

   qnpars      is the count of quantity parameter definition parameters.
               These parameters supply the quantity-specific information
               needed to fully define the quantity used in the search
               performed by this routine.

   lenvals     is the length of the string in arrays `qpnames' and `qcpars',
               including the null terminators.

   qpnams      is an array of names of quantity definition parameters.
               The names occupy elements 0:qnpars-1 of this array. The
               value associated with the ith element of `qpnams' is
               located in element `i' of the parameter value argument
               having data type appropriate for the parameter:

                  Data Type                      Argument
                  ---------                      --------
                  Character strings              qcpars
                  Double precision numbers       qdpars
                  Integers                       qipars
                  Logicals                       qlpars

               The order in which the parameter names are listed is
               unimportant, as long as the corresponding parameter
               values are listed in the same order.

               The names in `qpnams' are case-insensitive.

               See the description of the input argument `gquant' for a
               discussion of the parameter names and values associated
               with a given quantity.

   qcpars,
   qdpars,
   qipars,
   qlpars      are, respectively, parameter value arrays of types

                   const void         * qcpars;
                   ConstSpiceDouble   * qdpars;
                   ConstSpiceInt      * qipars;
                   ConstSpiceBoolean  * qlpars;

               The value associated with the ith name in the array
               `qpnams' resides in the ith element of whichever of these
               arrays has the appropriate data type.

               All of these arrays should be declared with dimension at
               least `qnpars'. `qcpars' should have the same dimension
               and shape as `qpnams'.

               The names in the array `qcpars' are case-insensitive.

               Note that there is no required order for qpnams/q*pars
               pairs.

               See the description of the input argument `gquant' for a
               discussion of the parameter names and values associated
               with a given quantity.

   op          is a scalar string comparison operator indicating the
               numeric constraint of interest. Values are:

                  ">"        value of geometric quantity greater than
                             some reference (refval).

                  "="        value of geometric quantity equal to some
                             reference (refval).

                  "<"        value of geometric quantity less than some
                             reference (refval).

                  "ABSMAX"   The geometric quantity is at an absolute
                             maximum.

                  "ABSMIN"   The geometric quantity is at an absolute
                             minimum.

                  "LOCMAX"   The geometric quantity is at a local
                             maximum.

                  "LOCMIN"   The geometric quantity is at a local
                             minimum.

               The caller may indicate that the region of interest is
               the set of time intervals where the quantity is within a
               specified distance of an absolute extremum. The argument
               `adjust' (described below) is used to specified this
               distance.

               Local extrema are considered to exist only in the
               interiors of the intervals comprising the confinement
               window: a local extremum cannot exist at a boundary point
               of the confinement window.

               Case is not significant in the string `op'.

   refval      is the reference value used to define an equality or
               inequality to be satisfied by the geometric quantity. The
               units of `refval' are radians, radians/sec, km, or km/sec
               as appropriate.

   tol         is a tolerance value used to determine convergence of
               root-finding operations. `tol' is measured in ephemeris
               seconds and must be greater than zero.

   adjust      is the amount by which the quantity is allowed to vary
               from an absolute extremum.

               If the search is for an absolute minimum is performed,
               the resulting window contains time intervals when the
               geometric quantity `gquant' has values between `absmin' and
               absmin + adjust.

               If the search is for an absolute maximum, the
               corresponding range is  between absmax - adjust and
               `absmax'.

               `adjust' is not used for searches for local extrema,
               equality or inequality conditions and must have value
               zero for such searches. `adjust' must not be negative.

   rpt         is a logical variable which controls whether the progress
               reporter is enabled. When `rpt' is SPICETRUE, progress
               reporting is enabled and the routines `udrepi', `udrepu', and
               `udrepf' (see descriptions below) are used to report
               progress.

   udrepi      is the name of the user specified routine that
               initializes a progress report. When progress reporting is
               enabled, `udrepi' is called at the start of a search. The
               prototype for `udrepi' is

                   void   ( * udrepi ) ( SpiceCell       * cnfine,
                                         ConstSpiceChar  * srcpre,
                                         ConstSpiceChar  * srcsuf )

               where

                  cnfine

               is a confinement window specifying the time period over
               which a search is conducted, and

                  srcpre
                  srcsuf

               are prefix and suffix strings used in the progress
               report: these strings are intended to bracket a
               representation of the fraction of work done. For example,
               when the CSPICE progress reporting functions are used, if
               if `srcpre' and `srcsuf' are, respectively,

                  "Occultation/transit search"
                  "done."

               the progress report display at the end of the search will
               be:

                  Occultation/transit search 100.00% done.

               If the user doesn't wish to provide a custom set of
               progress reporting functions, the CSPICE routine

                  gfrepi_c

               may be used.

   udrepu      is the name of the user specified routine that updates
               the progress report for a search. The prototype of
               `udrepu' is

                  void   ( * udrepu ) ( SpiceDouble       ivbeg,
                                        SpiceDouble       ivend,
                                        SpiceDouble       et      )

               where `et' is an epoch belonging to the confinement window,
               `ivbeg' and `ivend' are the start and stop times,
               respectively of the current confinement window interval.
               The ratio of the measure of the portion of `cnfine' that
               precedes `et' to the measure of `cnfine' would be a logical
               candidate for the searches completion percentage; however
               the method of measurement is up to the user.

               If the user doesn't wish to provide a custom set of
               progress reporting functions, the CSPICE routine

                  gfrepu_c

               may be used.

   udrepf      is the name of the user specified routine that finalizes
               a progress report. `udrepf' has no arguments.

               If the user doesn't wish to provide a custom set of
               progress reporting functions, the CSPICE routine

                  gfrepf_c

               may be used.

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

   bail        is a logical flag indicating whether or not interrupt
               signaling handling is enabled. When `bail' is set to
               SPICETRUE, the input function `udbail' (see description
               below) is used to determine whether an interrupt has been
               issued.

   udbail      is the name of the user specified routine that indicates
               whether an interrupt signal has been issued (for example,
               from the keyboard). The prototype of `udbail' is

                  SpiceBoolean   ( * udbail ) ( void )

               The return value is SPICETRUE if an interrupt has been
               issued; otherwise the value is SPICEFALSE.

               gfevnt_c uses `udbail' only when `bail' (see above) is set
               to SPICETRUE, indicating that interrupt handling is
               enabled. When interrupt handling is enabled, gfevnt_c
               and routines in its call tree will call `udbail' to
               determine whether to terminate processing and return
               immediately.

               If the user doesn't wish to provide a custom interrupt
               handling function, the CSPICE routine

                  gfbail_c

               may be used.

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
               function must still be passed as an input argument.
               The CSPICE function

                  gfbail_c

               may be used for this purpose.

               See the -Examples header section below for a complete code
               example demonstrating use of the CSPICE interrupt
               handling capability.

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

   result      is a SPICE window representing the set of time intervals,
               within the confinement period, when the specified
               geometric event occurs.

               `result' must be declared and initialized with sufficient
               size to capture the full set of time intervals within the
               search region on which the specified condition is satisfied.

               If `result' is non-empty on input, its contents will be
               discarded before gfevnt_c conducts its search.

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

   All parameters described here are declared in the header file
   SpiceGF.h. See that file for parameter values.

   SPICE_GFEVNT_MAXPAR

               is the maximum number of parameters required to define
               any quantity. SPICE_GFEVNT_MAXPAR may grow if new quantities
               require more parameters.

   SPICE_GF_CNVTOL

               is the default convergence tolerance used by the high-level
               GF search API routines. This tolerance is used to terminate
               searches for binary state transitions: when the time at which
               a transition occurs is bracketed by two times that differ by
               no more than SPICE_GF_CNVTOL, the transition time is considered
               to have been found.

-Exceptions

   1)  There are varying requirements on how distinct the three
       objects, `qcpars', must be. If the requirements are not met, an,
       an error is signaled by a routine in the call tree of this
       routine.

       When `gquant' has value "ANGULAR SEPARATION" then all three
       must be distinct.

       When `gquant' has value of either

          "DISTANCE"
          "COORDINATE"
          "RANGE RATE"

       the qcpars[0] and qcpars[1] objects must be distinct.

   2)  If any of the bodies involved do not have NAIF ID codes, an
       error is signaled by a routine in the call tree of this
       routine.

   3)  If the value of `gquant' is not recognized as a valid value, the
       error SPICE(NOTRECOGNIZED) is signaled by a routine in the
       call tree of this routine.

   4)  If the number of quantity definition parameters, `qnpars' is
       greater than the maximum allowed value, SPICE_GFEVNT_MAXPAR,
       the error SPICE(INVALIDCOUNT) is signaled.

   5)  If the proper required parameters are not supplied in `qnpars',
       the error SPICE(MISSINGVALUE) is signaled by a routine in the
       call tree of this routine.

   6)  If the comparison operator, `op', is not recognized, the error
       SPICE(NOTRECOGNIZED) is signaled by a routine in the call tree
       of this routine.

   7)  If the number of intervals `nintvls' is less than 1, the error
       SPICE(VALUEOUTOFRANGE) is signaled.

   8)  If `tol' is not greater than zero, an error is signaled by a
       routine in the call tree of this routine.

   9)  If `adjust' is negative, an error is signaled by a routine in
       the call tree of this routine.

   10) If `adjust' has a non-zero value when `op' has any value other
       than "ABSMIN" or "ABSMAX", an error is signaled by a routine
       in the call tree of this routine.

   11) The user must take care when searching for an extremum
       ("ABSMAX", "ABSMIN", "LOCMAX", "LOCMIN") of an angular
       quantity. Problems are most common when using the "COORDINATE"
       value of `gquant' with "LONGITUDE" or "RIGHT ASCENSION" values
       for the coordinate name. Since these quantities are cyclical,
       rather than monotonically increasing or decreasing, an
       extremum may be hard to interpret. In particular, if an
       extremum is found near the cycle boundary (-Pi for
       "LONGITUDE", 2*Pi for "RIGHT ASCENSION") it may not be
       numerically reasonable. For example, the search for times when
       a longitude coordinate is at its absolute maximum may result
       in a time when the longitude value is -Pi, due to roundoff
       error.

   12) If operation of this routine is interrupted, the output result
       window will be invalid.

   13) If any of the `qpnams', `qcpars', `gquant' or `op' input
       string pointers is null, the error SPICE(NULLPOINTER) is
       signaled.

   14) If any of the `qpnams', `qcpars', `gquant' or `op' input
       strings has zero length, the error SPICE(EMPTYSTRING) is
       signaled.

   15) If any the `cnfine' or `result' cell arguments has a type
       other than SpiceDouble, the error SPICE(TYPEMISMATCH) is
       signaled.

   16) If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled.

   17) If any attempt to change the handler for the interrupt signal
       SIGINT fails, the error SPICE(SIGNALFAILED) is signaled.

-Files

   Appropriate SPK and PCK kernels must be loaded by the
   calling program before this routine is called.

   The following data are required:

   -  SPK data: ephemeris data for target, source and observer that
      describes the ephemeris of these objects for the period
      defined by the confinement window, `udbail' must be
      loaded. If aberration corrections are used, the states of
      target and observer relative to the solar system barycenter
      must be calculable from the available ephemeris data.
      Typically ephemeris data are made available by loading one
      or more SPK files via furnsh_c.

   -  PCK data: bodies are assumed to be spherical and must have a
      radius loaded from the kernel pool. Typically this is done by
      loading a text PCK file via furnsh_c. If the bodies are
      triaxial, the largest radius is chosen as that of the
      equivalent spherical body.

   -  In some cases the observer's state may be computed at times
      outside of `udbail' by as much as 2 seconds; data required to
      compute this state must be provided by loaded kernels. See
      -Particulars for details.

   In all cases, kernel data are normally loaded once per program
   run, NOT every time this routine is called.

-Particulars

   This routine provides the SPICE GF subsystem's general interface
   to determines time intervals when the value of some
   geometric quantity related to one or more objects and an observer
   satisfies a user specified constraint. It puts these times in a
   result window called `cnfine'. It does this by first finding
   windows when the quantity of interest is either monotonically
   increasing or decreasing. These windows are then manipulated to
   give the final result.

   Applications that require do not require support for progress
   reporting, interrupt handling, non-default step or refinement
   functions, or non-default convergence tolerance normally should
   call a high level geometry quantity routine rather than
   this routine.

   The Search Process
   ==================

   Regardless of the type of constraint selected by the caller, this
   routine starts the search for solutions by determining the time
   periods, within the confinement window, over which the specified
   geometric quantity function is monotone increasing and monotone
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
   "convergence tolerance," passed to this routine as "tol".

   The GF subsystem defines a parameter, SPICE_GF_CNVTOL (from SpiceGF.h),
   as a default tolerance. This represents a "tight" tolerance value
   so that the tolerance doesn't become the limiting factor in the
   accuracy of solutions found by this routine. In general the
   accuracy of input data will be the limiting factor.

   Making the tolerance tighter than SPICE_GF_CNVTOL is unlikely to
   be useful, since the results are unlikely to be more accurate.
   Making the tolerance looser will speed up searches somewhat,
   since a few convergence steps will be omitted. However, in most
   cases, the step size is likely to have a much greater affect
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
   slightly outside the confinement window `udbail'. The time window
   that is actually used is the result of "expanding" `udbail' by a
   specified amount "T": each time interval of `udbail' is expanded by
   shifting the interval's left endpoint to the left and the right
   endpoint to the right by T seconds. Any overlapping intervals are
   merged. (The input argument `udbail' is not modified.)

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

   1) Conduct a DISTANCE search using the default GF progress reporting
      capability.

      The program will use console I/O to display a simple
      ASCII-based progress report.

      The program will find local maximums of the distance from earth to
      Moon with light time and stellar aberration corrections to model
      the apparent positions of the Moon.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: gfevnt_ex1.tm

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
         Program gfevnt_ex1
      ./
      #include "SpiceUsr.h"
      #include <stdio.h>
      #include <signal.h>

      int main()
         {

         /.
         Constants
         ./
         #define  TIMFMT  "YYYY-MON-DD HR:MN:SC.###### (TDB) ::TDB ::RND"
         #define  MAXVAL  10000
         #define  STRSIZ  41
         #define  LNSIZE  81

         /.
         Local variables
         ./
         SpiceBoolean            bail;
         SpiceBoolean            rpt;

         /.
         Confining window beginning and ending time strings.
         ./
         SpiceChar               begstr [LNSIZE] = "2001 jan 01 00:00:00.000";
         SpiceChar               endstr [LNSIZE] = "2001 dec 31 00:00:00.000";
         SpiceChar               event  []       = "DISTANCE";
         SpiceChar               relate []       = "LOCMAX";


         /.
         Declare qpnams and qcpars with the same dimensions.
         SPICE_GFEVNT_MAXPAR is defined in SpiceGF.h.
         ./
         SpiceChar  qpnams[SPICE_GFEVNT_MAXPAR][LNSIZE] = { "TARGET",
                                                            "OBSERVER",
                                                            "ABCORR" };

         SpiceChar  qcpars[SPICE_GFEVNT_MAXPAR][LNSIZE] = { "MOON",
                                                            "EARTH",
                                                            "LT+S"  };

         SpiceDouble             qdpars[SPICE_GFEVNT_MAXPAR];
         SpiceInt                qipars[SPICE_GFEVNT_MAXPAR];
         SpiceBoolean            qlpars[SPICE_GFEVNT_MAXPAR];


         SPICEDOUBLE_CELL      ( cnfine, MAXVAL );
         SPICEDOUBLE_CELL      ( result, MAXVAL );

         SpiceDouble             begtim;
         SpiceDouble             endtim;
         SpiceDouble             step;
         SpiceDouble             refval;
         SpiceDouble             adjust;
         SpiceDouble             tol;
         SpiceDouble             beg;
         SpiceDouble             end;


         SpiceInt                lenvals;
         SpiceInt                nintvls;
         SpiceInt                count;
         SpiceInt                qnpars;
         SpiceInt                i;


         /.
         Load leapsecond and spk kernels. The name of the
         meta kernel file shown here is fictitious; you
         must supply the name of a file available
         on your own computer system.
         ./
         furnsh_c ( "gfevnt_ex1.tm" );

         /.
         Set a beginning and end time for confining window.
         ./

         str2et_c ( begstr, &begtim );
         str2et_c ( endstr, &endtim );


         /.
         Add 2 points to the confinement interval window.
         ./
         wninsd_c ( begtim, endtim, &cnfine );


         /.
         Check the number of intervals in confining window.
         ./
         count = wncard_c( &cnfine );
         printf( "Found %d intervals in cnfine\n", (int)count );


         /.
         Set the step size to 1/1000 day and convert to seconds.
         One day would be a reasonable step size for this
         search, but the run would not last long enough to issue
         an interrupt.
         ./
         step = 0.001 * spd_c();
         gfsstp_c ( step );


         /.
         Set interrupt handling and progress reporting.
         ./
         bail = SPICETRUE;
         rpt  = SPICETRUE;

         lenvals= LNSIZE;
         qnpars = 3;
         tol    = SPICE_GF_CNVTOL;
         refval = 0.;
         adjust = 0.;
         nintvls= MAXVAL;

         /.
         Perform the search.
         ./
         gfevnt_c ( gfstep_c,
                    gfrefn_c,
                    event,
                    qnpars,
                    lenvals,
                    qpnams,
                    qcpars,
                    qdpars,
                    qipars,
                    qlpars,
                    relate,
                    refval,
                    tol,
                    adjust,
                    rpt,
                    &gfrepi_c,
                    gfrepu_c,
                    gfrepf_c,
                    nintvls,
                    bail,
                    gfbail_c,
                    &cnfine,
                    &result );

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
            count = wncard_c( &result);
            printf( "Found %d intervals in result\n", (int)count );

            /.
            List the beginning and ending points in each interval.
            ./
            for( i=0; i<count; i++ )
               {
               wnfetd_c( &result, i, &beg, &end );

               timout_c ( beg, TIMFMT, LNSIZE, begstr );
               timout_c ( end, TIMFMT, LNSIZE, endstr );

               printf( "Interval %d\n", (int)i );
               printf( "Beginning TDB %s\n", begstr );
               printf( "Ending TDB    %s\n", endstr );
               }

            }

         return ( 0 );
         }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Found 1 intervals in cnfine

      Distance pass 1 of 1 100.00% done.

      Found 13 intervals in result
      Interval 0
      Beginning TDB 2001-JAN-24 19:22:01.436672 (TDB)
      Ending TDB    2001-JAN-24 19:22:01.436672 (TDB)
      Interval 1
      Beginning TDB 2001-FEB-20 21:52:07.914964 (TDB)
      Ending TDB    2001-FEB-20 21:52:07.914964 (TDB)
      Interval 2
      Beginning TDB 2001-MAR-20 11:32:03.182345 (TDB)
      Ending TDB    2001-MAR-20 11:32:03.182345 (TDB)
      Interval 3
      Beginning TDB 2001-APR-17 06:09:00.877038 (TDB)
      Ending TDB    2001-APR-17 06:09:00.877038 (TDB)
      Interval 4
      Beginning TDB 2001-MAY-15 01:29:28.532819 (TDB)
      Ending TDB    2001-MAY-15 01:29:28.532819 (TDB)
      Interval 5
      Beginning TDB 2001-JUN-11 19:44:10.855458 (TDB)
      Ending TDB    2001-JUN-11 19:44:10.855458 (TDB)
      Interval 6
      Beginning TDB 2001-JUL-09 11:14:45.082568 (TDB)
      Ending TDB    2001-JUL-09 11:14:45.082568 (TDB)
      Interval 7
      Beginning TDB 2001-AUG-05 20:51:11.781173 (TDB)
      Ending TDB    2001-AUG-05 20:51:11.781173 (TDB)
      Interval 8
      Beginning TDB 2001-SEP-01 23:10:57.488901 (TDB)
      Ending TDB    2001-SEP-01 23:10:57.488901 (TDB)
      Interval 9
      Beginning TDB 2001-SEP-29 05:23:55.442354 (TDB)
      Ending TDB    2001-SEP-29 05:23:55.442354 (TDB)
      Interval 10
      Beginning TDB 2001-OCT-26 20:08:15.029871 (TDB)
      Ending TDB    2001-OCT-26 20:08:15.029871 (TDB)
      Interval 11
      Beginning TDB 2001-NOV-23 15:45:23.027511 (TDB)
      Ending TDB    2001-NOV-23 15:45:23.027511 (TDB)
      Interval 12
      Beginning TDB 2001-DEC-21 13:04:47.124241 (TDB)
      Ending TDB    2001-DEC-21 13:04:47.124241 (TDB)


      Note that the progress report has the format shown below:

         Distance pass 1 of 1   6.02% done.

      The completion percentage was updated approximately once per
      second.

      When the program was interrupted at an arbitrary time,
      the output was:

         Distance pass 1 of 1  26.74% done.
         Search was interrupted.

      This message was written after an interrupt signal
      was trapped. By default, the program would have terminated
      before this message could be written.

-Restrictions

   1)  The kernel files to be used by gfevnt_c must be loaded (normally
       via the CSPICE routine furnsh_c) before calling gfevnt_c.

   2)  If using the default, constant step size routine, gfstep_c, the
       the caller must set the step size by calling the entry point
       gfsstp_c before calling gfevnt_c. The call syntax for gfsstp_c:

          gfsstp_c ( step );

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 01-NOV-2021 (JDR) (EDW) (NJB)

       Added check for error condition "nintvls" less than one.

       Added check for `qnpars' out of range.

       Bug fix: moved creation of Fortran-style arrays for parameter
       names and string parameter values to right before the call to
       gfevnt_. This prevents a memory leak that could occur due to
       the prior placement of this code before checking macros that
       can execute return statements.

       Bug fix: changed input void array checks from using CHKFSTR to
       CHKOSTR. The previous checks did not inspect the input `lenvals'.

       Added use of ALLOC_CHECK_INTRA to check net null effect on
       alloc count.

       Updated header to describe use of expanded confinement window.

       Edited the header to comply with NAIF standard.

       Updated the description of "nintvls", "cnfine" and "result" arguments.

       Updates in -Exceptions section: Added entries #13 to #15, fixed
       short error message in entries #16 and #17, replaced entry #9 by new
       entries #9 and #10.

       Parameter MAXPAR has been replaced with parameter SPICE_GFEVNT_MAXPAR.

       Added descriptions of SPICE_GFEVNT_MAXPAR and SPICE_GF_CNVTOL
       to the -Brief_I/O and -Parameters sections.

   -CSPICE Version 1.0.2, 12-JUL-2016 (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

   -CSPICE Version 1.0.1, 24-APR-2010 (EDW)

       Minor edit to code comments eliminating typo.

   -CSPICE Version 1.0.0, 11-MAR-2009 (EDW) (NJB)

-Index_Entries

   determine when a geometric quantity satisfies a condition

-&
*/

  { /* Begin gfevnt_c */

   /*
   Prototypes
   */
   void                  ( * defSigHandler ) (int);
   void                  ( * sigPtr        ) (int);


   /*
   Local variables
   */
   doublereal              * work;

   int                       nowalloc;

   static SpiceInt           nw = SPICE_GF_NWMAX;

   SpiceInt                  nBytes;
   SpiceInt                  fstr_Len_qpnams;
   SpiceInt                  fstr_Len_qcpars;

   SpiceChar               * fstr_qpnams;
   SpiceChar               * fstr_qcpars;

   SpiceBoolean              newHandler;


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "gfevnt_c" );

   /*
   Check the parameter count.
   */
   if (  ( qnpars < 0 ) || ( qnpars > SPICE_GFEVNT_MAXPAR ) )
   {
      setmsg_c ( "Parameter count must be in the range 0:# but was #." );
      errint_c ( "#", SPICE_GFEVNT_MAXPAR );
      errint_c ( "#", qnpars              );
      sigerr_c ( "SPICE(INVALIDCOUNT)"    );
      chkout_c ( "gfevnt_c"               );
      return;
   }

   /*
   Make sure the input string pointers are non-null and that the
   length 'lenvals' is sufficient.
   */
   CHKOSTR ( CHK_STANDARD, "gfevnt_c", qpnams, lenvals );
   CHKOSTR ( CHK_STANDARD, "gfevnt_c", qcpars, lenvals );

   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfevnt_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary.
   */
   CELLINIT2 ( cnfine, result );

   /*
   Check the other input strings to make sure each pointer is non-null
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfevnt_c", gquant );
   CHKFSTR ( CHK_STANDARD, "gfevnt_c", op );


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
      chkout_c ( "gfevnt_c"                                     );
      return;
   }

   /*
   Allocate the workspace. 'nintvls' indicates the maximum number of
   intervals returned in 'result'. An interval consists of
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
      chkout_c ( "gfevnt_c"                                     );
      return;
   }


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
            chkout_c ( "gfevnt_c"                                  );
            return;
         }
      }
   }

   /*
   Create Fortran-style string arrays for the quantity's parameter
   names and any character string parameter values.

   Use of the 'qnpars' argument for both 'qpnams' and 'qcpars' is deliberate.
   The calling routine should declare the 'qpnams' and 'qcpars' string arrays
   with the same dimensions (size and shape).
   */
   C2F_MapStrArr ( "gfevnt_c", qnpars, lenvals, qpnams, &fstr_Len_qpnams,
                                                        &fstr_qpnams );
   C2F_MapStrArr ( "gfevnt_c", qnpars, lenvals, qcpars, &fstr_Len_qcpars,
                                                        &fstr_qcpars );


   /*
   Let the f2c'd routine do the work.

   We pass the adapter functions, not those provided as inputs,
   to the f2c'd routine:

      zzadstep_c  adapter for  udstep
      zzadrefn_c     ''        udrefn
      zzadrepi_c     ''        udrepi
      zzadrepu_c     ''        udrepu
      zzadrepf_c     ''        udrepf
      zzadbail_c     ''        udbail

   */

   gfevnt_(  ( U_fp            )  zzadstep_c,
             ( U_fp            )  zzadrefn_c,
             ( char          * )  gquant,
             ( integer       * ) &qnpars,
             ( char          * )  fstr_qpnams,
             ( char          * )  fstr_qcpars,
             ( doublereal    * )  qdpars,
             ( integer       * )  qipars,
             ( logical       * )  qlpars,
             ( char          * )  op,
             ( doublereal    * ) &refval,
             ( doublereal    * ) &tol,
             ( doublereal    * ) &adjust,
             ( doublereal    * )  (cnfine->base),
             ( logical       * ) &rpt,
             ( S_fp            )  zzadrepi_c,
             ( U_fp            )  zzadrepu_c,
             ( S_fp            )  zzadrepf_c,
             ( integer       * ) &nintvls,
             ( integer       * ) &nw,
             ( doublereal    * )  work,
             ( logical      *  ) &bail,
             ( L_fp            )  zzadbail_c,
             ( doublereal    * )  (result->base),
             ( ftnlen          )  strlen(gquant),
             ( ftnlen          )  fstr_Len_qpnams,
             ( ftnlen          )  fstr_Len_qcpars,
             ( ftnlen          )  strlen(op) );

   /*
   Always restore the previous signal handler and free dynamically
   allocated memory.
   */
   free_SpiceMemory( work );
   free ( fstr_qpnams );
   free ( fstr_qcpars );

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
         chkout_c ( "gfevnt_c"                                 );
         return;
      }
   }

   /*
   Sync the output cell.
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, result ) ;
   }

   ALLOC_CHECK_INTRA(nowalloc);

   chkout_c ( "gfevnt_c" );

}
