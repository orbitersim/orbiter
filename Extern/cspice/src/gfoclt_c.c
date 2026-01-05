/*

-Procedure gfoclt_c ( GF, find occultation )

-Abstract

   Determine time intervals when an observer sees one target occulted
   by, or in transit across, another.

   The surfaces of the target bodies may be represented by triaxial
   ellipsoids or by topographic data provided by DSK files.

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

   FRAMES
   GF
   KERNEL
   NAIF_IDS
   SPK
   TIME
   WINDOWS

-Keywords

   EVENT
   GEOMETRY
   SEARCH
   WINDOW

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void gfoclt_c ( ConstSpiceChar   * occtyp,
                   ConstSpiceChar   * front,
                   ConstSpiceChar   * fshape,
                   ConstSpiceChar   * fframe,
                   ConstSpiceChar   * back,
                   ConstSpiceChar   * bshape,
                   ConstSpiceChar   * bframe,
                   ConstSpiceChar   * abcorr,
                   ConstSpiceChar   * obsrvr,
                   SpiceDouble        step,
                   SpiceCell        * cnfine,
                   SpiceCell        * result )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   SPICE_GF_CNVTOL
              P   Convergence tolerance.
   occtyp     I   Type of occultation.
   front      I   Name of body occulting the other.
   fshape     I   Type of shape model used for front body.
   fframe     I   Body-fixed, body-centered frame for front body.
   back       I   Name of body occulted by the other.
   bshape     I   Type of shape model used for back body.
   bframe     I   Body-fixed, body-centered frame for back body.
   abcorr     I   Aberration correction flag.
   obsrvr     I   Name of the observing body.
   step       I   Step size in seconds for finding occultation
                  events.
   cnfine    I-O  SPICE window to which the search is restricted.
   result     O   SPICE window containing results.

-Detailed_Input

   occtyp      indicates the type of occultation that is to be found.
               Note that transits are considered to be a type of
               occultation.

               Supported values and corresponding definitions are:

                  "FULL"      denotes the full occultation of the
                              body designated by `back' by the body
                              designated by `front', as seen from the
                              location of the observer. In other
                              words, the occulted body is completely
                              invisible as seen from the observer's
                              location.

                  "ANNULAR"   denotes an annular occultation: the
                              body designated by `front' blocks part
                              of, but not the limb of, the body
                              designated by `back', as seen from the
                              location of the observer.

                  "PARTIAL"   denotes a partial, non-annular
                              occultation: the body designated by
                              `front' blocks part, but not all, of the
                              limb of the body designated by `back', as
                              seen from the location of the observer.

                  "ANY"       denotes any of the above three types of
                              occultations: "PARTIAL", "ANNULAR", or
                              "FULL".

                              "ANY" should be used to search for
                              times when the body designated by `front'
                              blocks any part of the body designated
                              by `back'.

                              The option "ANY" must be used if either
                              the front or back target body is
                              modeled as a point.

               Case and leading or trailing blanks are not
               significant in the string `occtyp'.

   front       is the name of the target body that occults---that is,
               passes in front of---the other. Optionally, you may
               supply the integer NAIF ID code for the body as a
               string. For example both "MOON" and "301" are
               legitimate strings that designate the Moon.

               Case and leading or trailing blanks are not
               significant in the string `front'.

   fshape      is a string indicating the geometric model used to
               represent the shape of the front target body. The
               supported options are:

                  "ELLIPSOID"

                      Use a triaxial ellipsoid model with radius
                      values provided via the kernel pool. A kernel
                      variable having a name of the form

                         BODYnnn_RADII

                      where nnn represents the NAIF integer code
                      associated with the body, must be present in
                      the kernel pool. This variable must be
                      associated with three numeric values giving the
                      lengths of the ellipsoid's X, Y, and Z
                      semi-axes.

                  "POINT"

                      Treat the body as a single point. When a point
                      target is specified, the occultation type must
                      be set to "ANY".

                  "DSK/UNPRIORITIZED[/SURFACES = <surface list>]"

                      Use topographic data provided by DSK files to
                      model the body's shape. These data must be
                      provided by loaded DSK files.

                      The surface list specification is optional. The
                      syntax of the list is

                         <surface 1> [, <surface 2>...]

                      If present, it indicates that data only for the
                      listed surfaces are to be used; however, data
                      need not be available for all surfaces in the
                      list. If absent, loaded DSK data for any surface
                      associated with the target body are used.

                      The surface list may contain surface names or
                      surface ID codes. Names containing blanks must
                      be delimited by double quotes, for example

                         SURFACES = "Mars MEGDR 128 PIXEL/DEG"

                      If multiple surfaces are specified, their names
                      or IDs must be separated by commas.

                      See the -Particulars section below for details
                      concerning use of DSK data.

               The combinations of the shapes of the target bodies
               `front' and `back' must be one of:

                  One ELLIPSOID, one POINT
                  Two ELLIPSOIDs
                  One DSK, one POINT

               Case and leading or trailing blanks are not
               significant in the string `fshape'.

   fframe      is the name of the body-fixed, body-centered reference
               frame associated with the front target body. Examples of
               such names are "IAU_SATURN" (for Saturn) and "ITRF93"
               (for the Earth).

               If the front target body is modeled as a point, `fframe'
               should be left empty or blank.

               Case and leading or trailing blanks bracketing a
               non-blank frame name are not significant in the string
               `fframe'.

   back        is the name of the target body that is occulted
               by---that is, passes in back of---the other.
               Optionally, you may supply the integer NAIF ID code
               for the body as a string. For example both "MOON" and
               "301" are legitimate strings that designate the Moon.

               Case and leading or trailing blanks are not
               significant in the string `back'.

   bshape      is the shape specification for the body designated
               by `back'. The supported options are those for
               `fshape'. See the description of `fshape' above for
               details.

   bframe      is the name of the body-fixed, body-centered reference
               frame associated with the ``back'' target body. Examples
               of such names are "IAU_SATURN" (for Saturn) and "ITRF93"
               (for the Earth).

               If the back target body is modeled as a point, `bframe'
               should be left empty or blank.

               Case and leading or trailing blanks bracketing a
               non-blank frame name are not significant in the string
               `bframe'.

   abcorr      indicates the aberration corrections to be applied to
               the state of each target body to account for one-way
               light time. Stellar aberration corrections are
               ignored if specified, since these corrections don't
               improve the accuracy of the occultation determination.

               See the header of the SPICE routine spkezr_c for a
               detailed description of the aberration correction
               options. For convenience, the options supported by
               this routine are listed below:

                  "NONE"     Apply no correction.

                  "LT"       "Reception" case: correct for
                             one-way light time using a Newtonian
                             formulation.

                  "CN"       "Reception" case: converged
                             Newtonian light time correction.

                  "XLT"      "Transmission" case: correct for
                             one-way light time using a Newtonian
                             formulation.

                  "XCN"      "Transmission" case: converged
                             Newtonian light time correction.

               Case and blanks are not significant in the string
               `abcorr'.

   obsrvr      is the name of the body from which the occultation is
               observed. Optionally, you may supply the integer NAIF
               ID code for the body as a string.

               Case and leading or trailing blanks are not
               significant in the string `obsrvr'.

   step        is the step size to be used in the search. `step' must
               be shorter than any interval, within the confinement
               window, over which the specified occultation condition
               is met. In other words, `step' must be shorter than the
               shortest occultation event that the user wishes to
               detect; `step' must also be shorter than the shortest
               time interval between two occultation events that
               occur within the confinement window (see below).
               However, `step' must not be *too* short, or the search
               will take an unreasonable amount of time.

               The choice of `step' affects the completeness but not
               the precision of solutions found by this routine; the
               precision is controlled by the convergence tolerance.
               See the discussion of the parameter SPICE_GF_CNVTOL for
               details.

               `step' has units of TDB seconds.

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
               within the confinement window, when the specified
               occultation occurs.

               `result' must be declared and initialized with sufficient
               size to capture the full set of time intervals within the
               search region on which the specified condition is satisfied.

               If `result' is non-empty on input, its contents will be
               discarded before gfoclt_c conducts its search.

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

              SPICE_GF_CNVTOL is declared in the header file

                 SpiceGF.h

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

   3)  If name of either target or the observer cannot be translated
       to a NAIF ID code, an error is signaled by a routine
       in the call tree of this routine.

   4)  If the radii of a target body modeled as an ellipsoid cannot
       be determined by searching the kernel pool for a kernel
       variable having a name of the form

          "BODYnnn_RADII"

       where nnn represents the NAIF integer code associated with
       the body, an error is signaled by a routine in the
       call tree of this routine.

   5)  If either of the target bodies `front' or `back' coincides with
       the observer body `obsrvr', an error is signaled by a
       routine in the call tree of this routine.

   6)  If the body designated by `front' coincides with that
       designated by `back', an error is signaled by a routine
       in the call tree of this routine.

   7)  If either of the body model specifiers `fshape' or `bshape'
       is not recognized, an error is signaled by a routine
       in the call tree of this routine.

   8)  If both of the body model specifiers `fshape' and `bshape'
       specify point targets, an error is signaled by a
       routine in the call tree of this routine.

   9)  If one of the body model specifiers `fshape' and `bshape'
       specifies a DSK model, and the other argument does not
       specify a point target, an error is signaled by a routine in
       the call tree of this routine.

   10) If a target body-fixed reference frame associated with a
       non-point target is not recognized, an error is signaled by a
       routine in the call tree of this routine.

   11) If a target body-fixed reference frame is not centered at the
       corresponding target body, an error is signaled by a routine
       in the call tree of this routine.

   12) If the loaded kernels provide insufficient data to compute any
       required state vector, an error is signaled by a routine in
       the call tree of this routine.

   13) If an error occurs while reading an SPK or other kernel file,
       the error is signaled by a routine in the call tree
       of this routine.

   14) If a point target is specified and the occultation type is set
       to a valid value other than "ANY", an error is signaled by a
       routine in the call tree of this routine.

   15) If the output SPICE window `result' has size less than 2, the
       error SPICE(WINDOWTOOSMALL) is signaled by a routine in the
       call tree of this routine.

   16) If the output SPICE window `result' has insufficient capacity
       to contain the number of intervals on which the specified
       occultation condition is met, an error is signaled
       by a routine in the call tree of this routine.

   17) If the occultation type `occtyp' is invalid, an error is
       signaled by a routine in the call tree of this routine.

   18) If the aberration correction specification `abcorr' is invalid,
       an error is signaled by a routine in the call tree of this
       routine.

   19) If either `fshape' or `bshape' specifies that the target surface
       is represented by DSK data, and no DSK files are loaded for
       the specified target, an error is signaled by a routine in
       the call tree of this routine.

   20) If either `fshape' or `bshape' specifies that the target surface
       is represented by DSK data, but the shape specification is
       invalid, an error is signaled by a routine in the call tree
       of this routine.

   21) If any of the `occtyp', `front', `fshape', `back', `bshape',
       `abcorr', `obsrvr', `bframe' or `fframe' input string pointers
       is null, the error SPICE(NULLPOINTER) is signaled.

   22) If any of the `occtyp', `front', `fshape', `back', `bshape',
       `abcorr' or `obsrvr' input strings has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   23) If any the `cnfine' or `result' cell arguments has a type
       other than SpiceDouble, the error SPICE(TYPEMISMATCH) is
       signaled.

-Files

   Appropriate SPICE kernels must be loaded by the calling program
   before this routine is called.

   The following data are required:

   -  SPK data: the calling application must load ephemeris data
      for the targets, source and observer that cover the time
      period specified by the window `cnfine'. If aberration
      corrections are used, the states of the target bodies and of
      the observer relative to the solar system barycenter must be
      calculable from the available ephemeris data. Typically
      ephemeris data are made available by loading one or more SPK
      files via furnsh_c.

   -  PCK data: bodies modeled as triaxial ellipsoids must have
      semi-axis lengths provided by variables in the kernel pool.
      Typically these data are made available by loading a text
      PCK file via furnsh_c.

   -  FK data: if either of the reference frames designated by
      `bframe' or `fframe' are not built in to the SPICE system,
      one or more FKs specifying these frames must be loaded.

   The following data may be required:

   -  DSK data: if either `fshape' or `bshape' indicates that DSK
      data are to be used, DSK files containing topographic data
      for the target body must be loaded. If a surface list is
      specified, data for at least one of the listed surfaces must
      be loaded.

   -  Surface name-ID associations: if surface names are specified
      in `fshape' or `bshape', the association of these names with
      their corresponding surface ID codes must be established by
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

   -  CK data: either of the body-fixed frames to which `fframe' or
      `bframe' refer might be a CK frame. If so, at least one CK
      file will be needed to permit transformation of vectors
      between that frame and the J2000 frame.

   -  SCLK data: if a CK file is needed, an associated SCLK
      kernel is required to enable conversion between encoded SCLK
      (used to time-tag CK data) and barycentric dynamical time
      (TDB).

   Kernel data are normally loaded once per program run, NOT every
   time this routine is called.

-Particulars

   This routine provides a simpler, but less flexible, interface
   than does the CSPICE routine gfocce_c for conducting searches for
   occultation events. Applications that require support for
   progress reporting, interrupt handling, non-default step or
   refinement functions, or non-default convergence tolerance should
   call gfocce_c rather than this routine.

   This routine determines a set of one or more time intervals
   within the confinement window when a specified type of
   occultation occurs. The resulting set of intervals is returned as
   a SPICE window.

   Below we discuss in greater detail aspects of this routine's
   solution process that are relevant to correct and efficient
   use of this routine in user applications.


   The Search Process
   ==================

   The search for occultations is treated as a search for state
   transitions: times are sought when the state of the `back' body
   changes from "not occulted" to "occulted" or vice versa.

   Step Size
   =========

   Each interval of the confinement window is searched as follows:
   first, the input step size is used to determine the time separation
   at which the occultation state will be sampled. Starting at the left
   endpoint of the interval, samples of the occultation state will be
   taken at each step. If a state change is detected, a root has been
   bracketed; at that point, the "root"--the time at which the state
   change occurs---is found by a refinement process, for example, via
   binary search.

   Note that the optimal choice of step size depends on the lengths
   of the intervals over which the occultation state is constant:
   the step size should be shorter than the shortest occultation
   duration and the shortest period between occultations, within
   the confinement window.

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
   narrow down the time interval within which the root must lie. This
   refinement process terminates when the location of the root has been
   determined to within an error margin called the "convergence
   tolerance." The convergence tolerance used by this routine is set
   via the parameter SPICE_GF_CNVTOL.

   The value of SPICE_GF_CNVTOL is set to a "tight" value so that the
   tolerance doesn't limit the accuracy of solutions found by this
   routine. In general the accuracy of input data will be the limiting
   factor.

   To use a different tolerance value, a lower-level GF routine such as
   gfocce_c must be called. Making the tolerance tighter than
   SPICE_GF_CNVTOL is unlikely to be useful, since the results are
   unlikely to be more accurate. Making the tolerance looser will speed
   up searches somewhat, since a few convergence steps will be omitted.
   However, in most cases, the step size is likely to have a much
   greater effect on processing time than would the convergence
   tolerance.


   The Confinement Window
   ======================

   The simplest use of the confinement window is to specify a time
   interval within which a solution is sought.

   The confinement window also can be used to restrict a search to
   a time window over which required data (typically ephemeris
   data, in the case of occultation searches) are known to be
   available.

   In some cases, the confinement window be used to make searches
   more efficient. Sometimes it's possible to do an efficient search
   to reduce the size of the time period over which a relatively
   slow search of interest must be performed. See the "CASCADE"
   example program in gf.req for a demonstration.


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
      both represent a surface as a set of triangular plates, the
      union of those sets of plates is considered to represent the
      surface.

      Currently only unprioritized data selection is supported.
      Because prioritized data selection may be the default behavior
      in a later version of the routine, the UNPRIORITIZED keyword is
      required in the `fshape' and `bshape' arguments.


      Syntax of the shape input arguments for the DSK case
      ----------------------------------------------------

      The keywords and surface list in the target shape arguments
      `bshape' and `fshape' are called "clauses." The clauses may
      appear in any order, for example

         "DSK/<surface list>/UNPRIORITIZED"
         "DSK/UNPRIORITIZED/<surface list>"
         "UNPRIORITIZED/<surface list>/DSK"

      The simplest form of the `method' argument specifying use of
      DSK data is one that lacks a surface list, for example:

         "DSK/UNPRIORITIZED"

      For applications in which all loaded DSK data for the target
      body are for a single surface, and there are no competing
      segments, the above string suffices. This is expected to be
      the usual case.

      When, for the specified target body, there are loaded DSK
      files providing data for multiple surfaces for that body, the
      surfaces to be used by this routine for a given call must be
      specified in a surface list, unless data from all of the
      surfaces are to be used together.

      The surface list consists of the string

         "SURFACES = "

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

      Escaped double quotes are used to delimit the surface name because
      it contains blank characters.

      To use data for surfaces 2 and 3 together, any
      of the following surface lists could be used:

         "SURFACES = 2, 3"

         "SURFACES = \"Mars MEGDR  64 PIXEL/DEG\", 3"

         "SURFACES = 2, Mars_MRO_HIRISE"

         "SURFACES = \"Mars MEGDR 64 PIXEL/DEG\", Mars_MRO_HIRISE"

      An example of a shape argument that could be constructed
      using one of the surface lists above is

         "DSK/UNPRIORITIZED/SURFACES = \"Mars MEGDR 64 PIXEL/DEG\", 3"

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Find occultations of the Sun by the Moon (that is, solar
      eclipses) as seen from the center of the Earth over the month
      December, 2001.

      Use light time corrections to model apparent positions of Sun
      and Moon. Stellar aberration corrections are not specified
      because they don't affect occultation computations.

      We select a step size of 3 minutes, which means we
      ignore occultation events lasting less than 3 minutes,
      if any exist.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: gfoclt_ex1.tm

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
         Program gfoclt_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./

         #define TIMFMT          "YYYY MON DD HR:MN:SC.###### (TDB)::TDB"
         #define MAXWIN          200
         #define TIMLEN          41

         /.
         Local variables
         ./
         SPICEDOUBLE_CELL      ( cnfine, MAXWIN );
         SPICEDOUBLE_CELL      ( result, MAXWIN );

         SpiceChar             * win0;
         SpiceChar             * win1;
         SpiceChar               begstr [ TIMLEN ];
         SpiceChar               endstr [ TIMLEN ];

         SpiceDouble             et0;
         SpiceDouble             et1;
         SpiceDouble             left;
         SpiceDouble             right;
         SpiceDouble             step;

         SpiceInt                i;

         /.
         Load kernels.
         ./
         furnsh_c ( "gfoclt_ex1.tm" );

         /.
         Obtain the TDB time bounds of the confinement
         window, which is a single interval in this case.
         ./
         win0 = "2001 DEC 01 00:00:00 TDB";
         win1 = "2002 JAN 01 00:00:00 TDB";

         str2et_c ( win0, &et0 );
         str2et_c ( win1, &et1 );

         /.
         Insert the time bounds into the confinement
         window.
         ./
         wninsd_c ( et0, et1, &cnfine );

         /.
         Select a 3-minute step. We'll ignore any occultations
         lasting less than 3 minutes.  Units are TDB seconds.
         ./
         step = 180.0;

         /.
         Perform the search.
         ./
         gfoclt_c ( "any",
                    "moon",    "ellipsoid",  "iau_moon",
                    "sun",     "ellipsoid",  "iau_sun",
                    "lt",      "earth",      step,
                    &cnfine,   &result                 );

         if ( wncard_c(&result) == 0 )
         {
            printf ( "No occultation was found.\n" );
         }
         else
         {
            for ( i = 0;  i < wncard_c(&result); i++ )
            {
               /.
               Fetch and display each occultation interval.
               ./
               wnfetd_c ( &result, i, &left, &right );

               timout_c ( left,  TIMFMT, TIMLEN, begstr );
               timout_c ( right, TIMFMT, TIMLEN, endstr );

               printf ( "Interval %d\n"
                        "   Start time: %s\n"
                        "   Stop time:  %s\n",
                        (int)i, begstr, endstr      );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Interval 0
         Start time: 2001 DEC 14 20:10:14.195952 (TDB)
         Stop time:  2001 DEC 14 21:35:50.317994 (TDB)


   2) Find occultations of Titan by Saturn or of Saturn by
      Titan as seen from the center of the Earth over the
      last four months of 2008. Model both target bodies as
      ellipsoids. Search for every type of occultation.

      Use light time corrections to model apparent positions of
      Saturn and Titan. Stellar aberration corrections are not
      specified because they don't affect occultation computations.

      We select a step size of 15 minutes, which means we
      ignore occultation events lasting less than 15 minutes,
      if any exist.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: gfoclt_ex2.tm

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
            sat427.bsp                    Satellite ephemeris for
                                          Saturn
            pck00008.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'sat427.bsp',
                                'pck00008.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program gfoclt_ex2
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define TIMFMT          "YYYY MON DD HR:MN:SC.###### (TDB)::TDB"
         #define MAXWIN          200
         #define TIMLEN          41
         #define LNSIZE          81
         #define NTYPES          4

         /.
         Local variables
         ./
         SPICEDOUBLE_CELL      ( cnfine, MAXWIN );
         SPICEDOUBLE_CELL      ( result, MAXWIN );

         SpiceChar             * back;
         SpiceChar             * bframe;
         SpiceChar             * front;
         SpiceChar             * fframe;
         SpiceChar               line   [ LNSIZE ];
         SpiceChar             * obsrvr;

         SpiceChar             * occtyp [ NTYPES ] =
                                 {
                                    "FULL",
                                    "ANNULAR",
                                    "PARTIAL",
                                    "ANY"
                                 };

         SpiceChar             * templt [ NTYPES ] =
                                 {
                                    "Condition: # occultation of # by #",
                                    "Condition: # occultation of # by #",
                                    "Condition: # occultation of # by #",
                                    "Condition: # occultation of # by #"
                                 };

         SpiceChar               timstr [ TIMLEN ];
         SpiceChar               title  [ LNSIZE ];
         SpiceChar             * win0;
         SpiceChar             * win1;

         SpiceDouble             et0;
         SpiceDouble             et1;
         SpiceDouble             finish;
         SpiceDouble             start;
         SpiceDouble             step;

         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                k;

         /.
         Load kernels.
         ./
         furnsh_c ( "gfoclt_ex2.tm" );

         /.
         Obtain the TDB time bounds of the confinement
         window, which is a single interval in this case.
         ./
         win0 = "2008 SEP 01 00:00:00 TDB";
         win1 = "2009 JAN 01 00:00:00 TDB";

         str2et_c ( win0, &et0 );
         str2et_c ( win1, &et1 );

         /.
         Insert the time bounds into the confinement
         window.
         ./
         wninsd_c ( et0, et1, &cnfine );

         /.
         Select a 15-minute step. We'll ignore any occultations
         lasting less than 15 minutes. Units are TDB seconds.
         ./
         step = 900.0;

         /.
         The observation location is the Earth.
         ./
         obsrvr = "Earth";

         /.
         Loop over the occultation types.
         ./
         for ( i = 0;  i < NTYPES;  i++ )
         {
            /.
            For each type, do a search for both transits of
            Titan across Saturn and occultations of Titan by
            Saturn.
            ./
            for ( j = 0;  j < 2;  j++ )
            {
               if ( j == 0 )
               {
                  front  = "TITAN";
                  fframe = "IAU_TITAN";
                  back   = "SATURN";
                  bframe = "IAU_SATURN";
               }
               else
               {
                  front  = "SATURN";
                  fframe = "IAU_SATURN";
                  back   = "TITAN";
                  bframe = "IAU_TITAN";
               }

               /.
               Perform the search. The target body shapes
               are modeled as ellipsoids.
               ./
               gfoclt_c ( occtyp[i],
                          front,    "ellipsoid",  fframe,
                          back,     "ellipsoid",  bframe,
                          "lt",     obsrvr,       step,
                          &cnfine,  &result               );

               /.
               Display the results.
               ./
               printf ( "\n" );

               /.
               Substitute the occultation type and target
               body names into the title string:
               ./
               repmc_c ( templt[i], "#", occtyp[i], LNSIZE, title );
               repmc_c ( title,     "#", back,      LNSIZE, title );
               repmc_c ( title,     "#", front,     LNSIZE, title );

               printf ( "%s\n", title );

               if ( wncard_c(&result) == 0 )
               {
                  printf ( " Result window is empty: "
                           "no occultation was found.\n" );
               }
               else
               {
                  printf ( " Result window start, stop times:\n" );

                  for ( k = 0;  k < wncard_c(&result);  k++ )
                  {
                     /.
                     Fetch the endpoints of the kth interval
                     of the result window.
                     ./
                     wnfetd_c ( &result, k, &start, &finish );

                     /.
                     Call strncpy with a length of 7 to include
                     a terminating null.
                     ./
                     strncpy ( line, "  #  #", 7 );

                     timout_c ( start,  TIMFMT, TIMLEN, timstr );

                     repmc_c  ( line, "#", timstr, LNSIZE, line );

                     timout_c ( finish, TIMFMT, TIMLEN, timstr );

                     repmc_c  ( line, "#", timstr, LNSIZE, line );

                     printf ( "%s\n", line );
                  }
               }
               /.
               We've finished displaying the results of the
               current search.
               ./
            }
            /.
            We've finished displaying the results of the
            searches using the current occultation type.
            ./
         }
         printf ( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Condition: FULL occultation of SATURN by TITAN
       Result window is empty: no occultation was found.

      Condition: FULL occultation of TITAN by SATURN
       Result window start, stop times:
        2008 OCT 27 22:08:01.672540 (TDB)  2008 OCT 28 01:05:03.332576 (TDB)
        2008 NOV 12 21:21:59.270691 (TDB)  2008 NOV 13 02:06:05.034713 (TDB)
        2008 NOV 28 20:49:02.415745 (TDB)  2008 NOV 29 02:13:58.978005 (TDB)
        2008 DEC 14 20:05:09.258916 (TDB)  2008 DEC 15 01:44:53.517960 (TDB)
        2008 DEC 30 19:00:56.586894 (TDB)  2008 DEC 31 00:42:43.219311 (TDB)

      Condition: ANNULAR occultation of SATURN by TITAN
       Result window start, stop times:
        2008 OCT 19 21:29:20.694709 (TDB)  2008 OCT 19 22:53:34.442728 (TDB)
        2008 NOV 04 20:15:38.652650 (TDB)  2008 NOV 05 00:18:59.130645 (TDB)
        2008 NOV 20 19:38:59.674043 (TDB)  2008 NOV 21 00:35:26.726756 (TDB)
        2008 DEC 06 18:58:34.093679 (TDB)  2008 DEC 07 00:16:17.653066 (TDB)
        2008 DEC 22 18:02:46.308375 (TDB)  2008 DEC 22 23:26:52.721881 (TDB)

      Condition: ANNULAR occultation of TITAN by SATURN
       Result window is empty: no occultation was found.

      Condition: PARTIAL occultation of SATURN by TITAN
       Result window start, stop times:
        2008 OCT 19 20:44:30.377189 (TDB)  2008 OCT 19 21:29:20.694709 (TDB)
        2008 OCT 19 22:53:34.442728 (TDB)  2008 OCT 19 23:38:26.219865 (TDB)
        2008 NOV 04 19:54:40.368045 (TDB)  2008 NOV 04 20:15:38.652650 (TDB)
        2008 NOV 05 00:18:59.130645 (TDB)  2008 NOV 05 00:39:58.607159 (TDB)
        2008 NOV 20 19:21:46.714396 (TDB)  2008 NOV 20 19:38:59.674043 (TDB)
        2008 NOV 21 00:35:26.726756 (TDB)  2008 NOV 21 00:52:40.606954 (TDB)
        2008 DEC 06 18:42:36.120122 (TDB)  2008 DEC 06 18:58:34.093679 (TDB)
        2008 DEC 07 00:16:17.653066 (TDB)  2008 DEC 07 00:32:16.331199 (TDB)
        2008 DEC 22 17:47:10.796147 (TDB)  2008 DEC 22 18:02:46.308375 (TDB)
        2008 DEC 22 23:26:52.721881 (TDB)  2008 DEC 22 23:42:28.860689 (TDB)

      Condition: PARTIAL occultation of TITAN by SATURN
       Result window start, stop times:
        2008 OCT 27 21:37:17.003993 (TDB)  2008 OCT 27 22:08:01.672540 (TDB)
        2008 OCT 28 01:05:03.332576 (TDB)  2008 OCT 28 01:35:49.235670 (TDB)
        2008 NOV 12 21:01:47.121213 (TDB)  2008 NOV 12 21:21:59.270691 (TDB)
        2008 NOV 13 02:06:05.034713 (TDB)  2008 NOV 13 02:26:18.211753 (TDB)
        2008 NOV 28 20:31:28.534248 (TDB)  2008 NOV 28 20:49:02.415745 (TDB)
        2008 NOV 29 02:13:58.978005 (TDB)  2008 NOV 29 02:31:33.684575 (TDB)
        2008 DEC 14 19:48:27.106157 (TDB)  2008 DEC 14 20:05:09.258916 (TDB)
        2008 DEC 15 01:44:53.517960 (TDB)  2008 DEC 15 02:01:36.356012 (TDB)
        2008 DEC 30 18:44:23.495003 (TDB)  2008 DEC 30 19:00:56.586894 (TDB)
        2008 DEC 31 00:42:43.219311 (TDB)  2008 DEC 31 00:59:17.027816 (TDB)

      Condition: ANY occultation of SATURN by TITAN
       Result window start, stop times:
        2008 OCT 19 20:44:30.377189 (TDB)  2008 OCT 19 23:38:26.219865 (TDB)
        2008 NOV 04 19:54:40.368045 (TDB)  2008 NOV 05 00:39:58.607159 (TDB)
        2008 NOV 20 19:21:46.714396 (TDB)  2008 NOV 21 00:52:40.606954 (TDB)
        2008 DEC 06 18:42:36.120122 (TDB)  2008 DEC 07 00:32:16.331199 (TDB)
        2008 DEC 22 17:47:10.796147 (TDB)  2008 DEC 22 23:42:28.860689 (TDB)

      Condition: ANY occultation of TITAN by SATURN
       Result window start, stop times:
        2008 OCT 27 21:37:17.003993 (TDB)  2008 OCT 28 01:35:49.235670 (TDB)
        2008 NOV 12 21:01:47.121213 (TDB)  2008 NOV 13 02:26:18.211753 (TDB)
        2008 NOV 28 20:31:28.534248 (TDB)  2008 NOV 29 02:31:33.684575 (TDB)
        2008 DEC 14 19:48:27.106157 (TDB)  2008 DEC 15 02:01:36.356012 (TDB)
        2008 DEC 30 18:44:23.495003 (TDB)  2008 DEC 31 00:59:17.027816 (TDB)


   3) Find occultations of the Mars Reconnaissance Orbiter (MRO)
      by Mars, or transits of the MRO spacecraft across Mars,
      as seen from the DSN station DSS-14 over a period of a
      few hours on FEB 28 2015.

      Use both ellipsoid and DSK shape models for Mars.

      Use light time corrections to model apparent positions of Mars
      and MRO. Stellar aberration corrections are not specified
      because they don't affect occultation computations.

      We select a step size of 3 minutes, which means we ignore
      occultation events lasting less than 3 minutes, if any exist.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File: gfoclt_ex3.tm

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
            de410.bsp                        Planetary ephemeris
            mar063.bsp                       Mars satellite ephemeris
            pck00010.tpc                     Planet orientation and
                                             radii
            naif0011.tls                     Leapseconds
            earthstns_itrf93_050714.bsp      DSN station ephemeris
            earth_latest_high_prec.bpc       Earth orientation
            mro_psp34.bsp                    MRO ephemeris
            megr90n000cb_plate.bds           Plate model based on
                                             MEGDR DEM, resolution
                                             4 pixels/degree.

         \begindata

            KERNELS_TO_LOAD = ( 'de410.bsp',
                                'mar063.bsp',
                                'mro_psp34.bsp',
                                'earthstns_itrf93_050714.bsp',
                                'earth_latest_high_prec.bpc',
                                'pck00010.tpc',
                                'naif0011.tls',
                                'megr90n000cb_plate.bds'       )
         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program gfoclt_ex3
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./

         #define META            "gfoclt_ex3.tm"
         #define TIMFMT          "YYYY MON DD HR:MN:SC" \
                                 ".###### (TDB)::TDB"
         #define MAXWIN          200
         #define TIMLEN          41

         /.
         Local variables
         ./
         SPICEDOUBLE_CELL      ( cnfine, MAXWIN );
         SPICEDOUBLE_CELL      ( result, MAXWIN );

         SpiceChar             * abcorr;
         SpiceChar             * back;
         SpiceChar               begstr [ TIMLEN ];
         SpiceChar             * bframe;
         SpiceChar             * bshape;
         SpiceChar               endstr [ TIMLEN ];
         SpiceChar             * fframe;
         SpiceChar             * front;
         SpiceChar             * fshape;
         SpiceChar             * obsrvr;
         SpiceChar             * occtyp;
         SpiceChar             * win0;
         SpiceChar             * win1;

         SpiceDouble             et0;
         SpiceDouble             et1;
         SpiceDouble             left;
         SpiceDouble             right;
         SpiceDouble             step;

         SpiceInt                i;
         SpiceInt                j;
         SpiceInt                k;


         /.
         Load kernels.
         ./
         furnsh_c ( META );


         /.
         Set the observer and aberration correction.
         ./
         obsrvr = "DSS-14";
         abcorr = "CN";

         /.
         Set the occultation type.
         ./
         occtyp = "ANY";

         /.
         Set the TDB time bounds of the confinement
         window, which is a single interval in this case.
         ./
         win0 = "2015 FEB 28 07:00:00 TDB";
         win1 = "2015 FEB 28 12:00:00 TDB";

         str2et_c ( win0, &et0 );
         str2et_c ( win1, &et1 );

         /.
         Insert the time bounds into the confinement
         window.
         ./
         wninsd_c ( et0, et1, &cnfine );


         /.
         Select a 3-minute step. We'll ignore any occultations
         lasting less than 3 minutes. Units are TDB seconds.
         ./
         step = 180.0;

         /.
         Perform both spacecraft occultation and spacecraft
         transit searches.
         ./
         for ( i = 0;  i < 2;  i++ )
         {
            if ( i == 0 )
            {
               /.
               Perform a spacecraft occultation search.
               ./
               front  = "MARS";
               fframe = "IAU_MARS";

               back   = "MRO";
               bshape = "POINT";
               bframe = " ";
            }
            else
            {
               /.
               Perform a spacecraft transit search.
               ./
               front  = "MRO";
               fframe = " ";
               fshape = "POINT";

               back   = "MARS";
               bframe = "IAU_MARS";
            }

            for ( j = 0;  j < 2;  j++ )
            {
               if ( j == 0 )
               {
                  /.
                  Model the planet shape as an ellipsoid.
                  ./
                  if ( i == 0 )
                  {
                     fshape = "ELLIPSOID";
                  }
                  else
                  {
                     bshape = "ELLIPSOID";
                  }
               }
               else
               {
                  /.
                  Model the planet shape using DSK data.
                  ./
                  if ( i == 0 )
                  {
                     fshape = "DSK/UNPRIORITIZED";
                  }
                  else
                  {
                     bshape = "DSK/UNPRIORITIZED";
                  }
               }
               /.
               Perform the spacecraft occultation or
               transit search.
               ./
               printf ( "\n" );

               if ( i == 0 )
               {
                  printf ( "Using shape model %s\n"
                           "Starting occultation search...\n",
                           fshape                             );
               }
               else
               {
                  printf ( "Using shape model %s\n"
                           "Starting transit search...\n",
                           bshape                             );
               }

               gfoclt_c ( occtyp,
                          front,   fshape,  fframe,
                          back,    bshape,  bframe,
                          abcorr,  obsrvr,  step,
                          &cnfine, &result          );

               if ( wncard_c(&result) == 0 )
               {
                  printf ( "No event was found.\n" );
               }
               else
               {
                  for ( k = 0;  k < wncard_c(&result); k++ )
                  {
                     /.
                     Fetch and display each occultation interval.
                     ./
                     wnfetd_c ( &result, k, &left, &right );

                     timout_c ( left,  TIMFMT, TIMLEN, begstr );
                     timout_c ( right, TIMFMT, TIMLEN, endstr );

                     printf ( "Interval %d\n"
                              "   Start time: %s\n"
                              "   Stop time:  %s\n",
                              (int)k, begstr, endstr );
                  }
               }
            }
            /.
            End of the target shape loop.
            ./
         }
         /.
         End of the occultation vs transit loop.
         ./
         printf ( "\n" );
         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Using shape model ELLIPSOID
      Starting occultation search...
      Interval 0
         Start time: 2015 FEB 28 07:17:35.379879 (TDB)
         Stop time:  2015 FEB 28 07:50:37.710284 (TDB)
      Interval 1
         Start time: 2015 FEB 28 09:09:46.920140 (TDB)
         Stop time:  2015 FEB 28 09:42:50.497193 (TDB)
      Interval 2
         Start time: 2015 FEB 28 11:01:57.845730 (TDB)
         Stop time:  2015 FEB 28 11:35:01.489716 (TDB)

      Using shape model DSK/UNPRIORITIZED
      Starting occultation search...
      Interval 0
         Start time: 2015 FEB 28 07:17:38.130608 (TDB)
         Stop time:  2015 FEB 28 07:50:38.310802 (TDB)
      Interval 1
         Start time: 2015 FEB 28 09:09:50.314903 (TDB)
         Stop time:  2015 FEB 28 09:42:55.369626 (TDB)
      Interval 2
         Start time: 2015 FEB 28 11:02:01.756296 (TDB)
         Stop time:  2015 FEB 28 11:35:08.368384 (TDB)

      Using shape model ELLIPSOID
      Starting transit search...
      Interval 0
         Start time: 2015 FEB 28 08:12:21.112018 (TDB)
         Stop time:  2015 FEB 28 08:45:48.401746 (TDB)
      Interval 1
         Start time: 2015 FEB 28 10:04:32.682324 (TDB)
         Stop time:  2015 FEB 28 10:37:59.920302 (TDB)
      Interval 2
         Start time: 2015 FEB 28 11:56:39.757564 (TDB)
         Stop time:  2015 FEB 28 12:00:00.000000 (TDB)

      Using shape model DSK/UNPRIORITIZED
      Starting transit search...
      Interval 0
         Start time: 2015 FEB 28 08:12:15.750020 (TDB)
         Stop time:  2015 FEB 28 08:45:43.406870 (TDB)
      Interval 1
         Start time: 2015 FEB 28 10:04:29.031706 (TDB)
         Stop time:  2015 FEB 28 10:37:55.565509 (TDB)
      Interval 2
         Start time: 2015 FEB 28 11:56:34.634642 (TDB)
         Stop time:  2015 FEB 28 12:00:00.000000 (TDB)


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   L.S. Elson          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 25-NOV-2021 (JDR) (NJB)

       Edited the header to comply with NAIF standard.

       Updated the description of "cnfine" and "result" arguments.

       Added entries #9, #15 and #23 in -Exceptions section.

   -CSPICE Version 1.0.1, 12-JUL-2016 (NJB) (EDW)

       Edit to example program to use "%d" with explicit casts
       to int for printing SpiceInts with printf.

       Updated to support use of DSKs.

   -CSPICE Version 1.0.0, 07-APR-2009 (NJB) (LSE) (EDW)

-Index_Entries

   GF occultation search

-&
*/

{ /* Begin gfoclt_c */


   /*
   Local variables
   */
   static const SpiceChar  * blankStr = " ";

   SpiceChar               * bFrameStr;
   SpiceChar               * fFrameStr;


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "gfoclt_c" );


   /*
   Make sure cell data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "gfoclt_c", SPICE_DP, cnfine, result );

   /*
   Initialize the input cells if necessary.
   */
   CELLINIT2 ( cnfine, result );

   /*
   The input frame names are special cases because we allow the caller
   to pass in empty strings. If either of these strings are empty,
   we pass a null-terminated string containing one blank character to
   the underlying f2c'd routine.

   First make sure the frame name pointers are non-null.
   */
   CHKPTR ( CHK_STANDARD, "gfoclt_c", bframe );
   CHKPTR ( CHK_STANDARD, "gfoclt_c", fframe );

   /*
   Use the input frame strings if they're non-empty; otherwise
   use blank strings for the frame names.
   */

   if ( bframe[0] )
   {
      bFrameStr = (SpiceChar *) bframe;
   }
   else
   {
      bFrameStr = (SpiceChar *) blankStr;
   }

   if ( fframe[0] )
   {
      fFrameStr = (SpiceChar *) fframe;
   }
   else
   {
      fFrameStr = (SpiceChar *) blankStr;
   }


   /*
   Check the other input strings to make sure each pointer is non-null
   and each string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", occtyp );
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", front  );
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", fshape );
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", back   );
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", bshape );
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", abcorr );
   CHKFSTR ( CHK_STANDARD, "gfoclt_c", obsrvr );


   /*
   Let the f2c'd routine do the work.
   */
   gfoclt_ ( (char         *) occtyp,
             (char         *) front,
             (char         *) fshape,
             (char         *) fFrameStr,
             (char         *) back,
             (char         *) bshape,
             (char         *) bFrameStr,
             (char         *) abcorr,
             (char         *) obsrvr,
             (doublereal   *) &step,
             (doublereal   *) cnfine->base,
             (doublereal   *) result->base,
             (ftnlen        ) strlen(occtyp),
             (ftnlen        ) strlen(front),
             (ftnlen        ) strlen(fshape),
             (ftnlen        ) strlen(fframe),
             (ftnlen        ) strlen(back),
             (ftnlen        ) strlen(bshape),
             (ftnlen        ) strlen(bframe),
             (ftnlen        ) strlen(abcorr),
             (ftnlen        ) strlen(obsrvr)  );

   /*
   Sync the output result cell.
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, result );
   }


   chkout_c ( "gfoclt_c" );

} /* End gfoclt_c */
