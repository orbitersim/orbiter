/*

-Procedure spkw12_c ( Write SPK segment, type 12 )

-Abstract

   Write a type 12 segment to an SPK file.

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

   NAIF_IDS
   SPC
   SPK
   TIME

-Keywords

   EPHEMERIS
   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef    spkw12_c


   void spkw12_c ( SpiceInt             handle,
                   SpiceInt             body,
                   SpiceInt             center,
                   ConstSpiceChar     * frame,
                   SpiceDouble          first,
                   SpiceDouble          last,
                   ConstSpiceChar     * segid,
                   SpiceInt             degree,
                   SpiceInt             n,
                   ConstSpiceDouble     states[][6],
                   SpiceDouble          begtim,
                   SpiceDouble          step        )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   MAXDEG     P   Maximum degree of interpolating polynomials.
   TOLSCL     P   Scale factor used to compute time bound tolerance.
   handle     I   Handle of an SPK file open for writing.
   body       I   NAIF code for an ephemeris object.
   center     I   NAIF code for center of motion of body.
   frame      I   Reference frame name.
   first      I   Start time of interval covered by segment.
   last       I   End time of interval covered by segment.
   segid      I   Segment identifier.
   degree     I   Degree of interpolating polynomials.
   n          I   Number of states.
   states     I   Array of states.
   begtim     I   Epoch of first state in states array.
   step       I   Time step separating epochs of states.
   MAXDEG     P   Maximum allowed degree of interpolating polynomial.

-Detailed_Input

   handle      is the file handle of an SPK file that has been
               opened for writing.

   body        is the NAIF integer code for an ephemeris object
               whose state relative to another body is described
               by the segment to be created.

   center      is the NAIF integer code for the center of motion
               of the object identified by body.

   frame       is the NAIF name for a reference frame
               relative to which the state information for `body'
               is specified.

   first,
   last        are, respectively, the start and stop times of
               the time interval over which the segment defines
               the state of `body'.

   segid       is the segment identifier. An SPK segment
               identifier may contain up to 40 characters.

   degree      is the degree of the Hermite polynomials used to
               interpolate the states. All components of the
               state vectors are interpolated by polynomials of
               fixed degree.

   n           is the number of states in the input state vector
               array.

   states      contains a time-ordered array of geometric states
               ( x, y, z, dx/dt, dy/dt, dz/dt, in kilometers and
               kilometers per second ) of body relative to `center',
               specified relative to `frame'.

   begtim      is the epoch corresponding to the first state in
               the state array. Because extra states are needed
               at the beginning and end of the segment in order
               for the interpolation method to work, `begtim' will
               normally precede first.

   step        is the time step separating the epochs of adjacent
               states in the input state array. `step' is specified
               in seconds.

-Detailed_Output

   None.

   See -Particulars for a description of the effect of this routine.

-Parameters

   The parameters below are declared in the Fortran include file
   spk12.inc, which is part of the Fortran SPICE Toolkit (aka
   SPICELIB). The values of those parameters are used in CSPICE code
   generated by running f2c on SPICELIB source code. They are not
   directly referenced by code in this module.

      MAXDEG         is the maximum allowed degree of the interpolating
                     polynomial.

                     The value of MAXDEG is 27.


      TOLSCL         is a tolerance scale factor (also called a
                     "relative tolerance") used for time coverage bound
                     checking. TOLSCL is unitless. TOLSCL produces a
                     tolerance value via the formula

                        TOL = TOLSCL * max( abs(first), abs(last) )

                     where `first' and `last' are the coverage time
                     bounds of a type 12 segment, expressed as seconds
                     past J2000 TDB.

                     The resulting parameter TOL is used as a tolerance
                     for comparing the input segment descriptor time
                     bounds to the first and last epoch covered by the
                     sequence of time intervals defined by the inputs
                     to spkw12_c:

                        begtim
                        step
                        n

                     The value of TOLSCL is 1.e-13.

-Exceptions

   If any of the following exceptions occur, this routine will return
   without creating a new segment.

   1)  If `frame' is not a recognized name, the error
       SPICE(INVALIDREFFRAME) is signaled by a routine in the call
       tree of this routine.

   2)  If the last non-blank character of `segid' occurs past index 40,
       the error SPICE(SEGIDTOOLONG) is signaled by a routine in the
       call tree of this routine.

   3)  If `segid' contains any nonprintable characters, the error
       SPICE(NONPRINTABLECHARS) is signaled by a routine in the call
       tree of this routine.

   4)  If `degree' is not at least 1 or is greater than MAXDEG, the
       error SPICE(INVALIDDEGREE) is signaled by a routine in the
       call tree of this routine.

   5)  If `degree' is not odd, the error SPICE(INVALIDDEGREE) is
       signaled by a routine in the call tree of this routine.

   6)  If the number of states `n' is not at least (degree+1)/2, the
       error SPICE(TOOFEWSTATES) is signaled by a routine in the call
       tree of this routine.

   7)  If `first' is greater than `last', the error SPICE(BADDESCRTIMES)
       is signaled by a routine in the call tree of this routine.

   8)  If `step' is non-positive, the error SPICE(INVALIDSTEPSIZE) is
       signaled by a routine in the call tree of this routine.

   9)  If the first epoch `begtim' is greater than `first', the error
       SPICE(BADDESCRTIMES) is signaled by a routine in the call tree
       of this routine.

   10) If the start time of the first record exceeds the descriptor
       begin time by more than a computed tolerance, or if the end
       time of the last record precedes the descriptor end time by
       more than a computed tolerance, the error SPICE(COVERAGEGAP)
       is signaled by a routine in the call tree of this routine. See
       the -Parameters section for a description of the tolerance.

   11) If any of the `frame' or `segid' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   12) If any of the `frame' or `segid' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   A new type 12 SPK segment is written to the SPK file attached
   to `handle'.

-Particulars

   This routine writes an SPK type 12 data segment to the open SPK
   file according to the format described in the type 12 section of
   the SPK Required Reading. The SPK file must have been opened with
   write access.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example demonstrates how to create an SPK type 12 kernel
      containing only one segment, given an evenly-spaced
      time-ordered set of discrete states.


      Example code begins here.


      /.
         Program spkw12_ex1
      ./
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Define the segment identifier parameters.
         ./
         #define SPK12        "spkw12_ex1.bsp"
         #define REF          "J2000"
         #define BODY         3
         #define CENTER       10
         #define DEGREE       3
         #define NSTATS       9

         /.
         Local variables.
         ./
         SpiceChar          * ifname;
         SpiceChar          * segid;

         SpiceDouble          first;
         SpiceDouble          last;
         SpiceDouble          step;

         SpiceInt             handle;
         SpiceInt             ncomch;

         /.
         Define the states and epochs.
         ./
         SpiceDouble          states [NSTATS][6] = {
                         {101.0, 201.0, 301.0, 401.0, 501.0, 601.0},
                         {102.0, 202.0, 302.0, 402.0, 502.0, 602.0},
                         {103.0, 203.0, 303.0, 403.0, 503.0, 603.0},
                         {104.0, 204.0, 304.0, 404.0, 504.0, 604.0},
                         {105.0, 205.0, 305.0, 405.0, 505.0, 605.0},
                         {106.0, 206.0, 306.0, 406.0, 506.0, 606.0},
                         {107.0, 207.0, 307.0, 407.0, 507.0, 607.0},
                         {108.0, 208.0, 308.0, 408.0, 508.0, 608.0},
                         {109.0, 209.0, 309.0, 409.0, 509.0, 609.0} };

         /.
         Set the start and end times of interval covered by
         segment, and the time step.
         ./
         first  = 100.0;
         last   = 900.0;
         step   = 100.0;

         /.
         `ncomch' is the number of characters to reserve for the
         kernel's comment area. This example doesn't write
         comments, so set to zero.
         ./
         ncomch = 0;

         /.
         Internal file name and segment ID.
         ./
         ifname = "Type 12 SPK internal file name.";
         segid  = "SPK type 12 test segment";

         /.
         Open a new SPK file.
         ./
         spkopn_c ( SPK12, ifname, ncomch, &handle );

         /.
         Write the segment.
         ./
         spkw12_c ( handle, BODY,   CENTER, REF,    first, last,
                    segid,  DEGREE, NSTATS, states, first, step );

         /.
         Close the SPK file.
         ./
         spkcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new SPK type 12 exists in
      the output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 2.1.0, 10-AUG-2021 (JDR)

       Changed the input argument name "epoch1" to "begtim" for
       consistency with other routines.

       Edited the header to comply with NAIF standard. Added
       complete example code from existing fragment.

   -CSPICE Version 2.0.0, 11-JAN-2014 (NJB)

       Relaxed test on relationship between the time bounds of the
       input record set (determined by `begtim', `step', and `n') and
       the descriptor bounds `first' and `last'. Now the descriptor
       bounds may extend beyond the time bounds of the record set by a
       ratio computed using the parameter TOLSCL (see -Parameters above
       for details). Added checks on input polynomial degree.
       Renamed argument `epoch0' to `epoch1' for consistency with other
       CSPICE and SPICELIB interfaces.

   -CSPICE Version 1.0.0, 23-FEB-2000 (NJB)

-Index_Entries

   write SPK type_12 ephemeris data segment

-&
*/

{ /* Begin spkw12_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "spkw12_c" );

   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw12_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw12_c", segid );


   /*
   Write the segment.
   */
   spkw12_ ( ( integer    * ) &handle,
             ( integer    * ) &body,
             ( integer    * ) &center,
             ( char       * ) frame,
             ( doublereal * ) &first,
             ( doublereal * ) &last,
             ( char       * ) segid,
             ( integer    * ) &degree,
             ( integer    * ) &n,
             ( doublereal * ) states,
             ( doublereal * ) &begtim,
             ( doublereal * ) &step,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );


   chkout_c ( "spkw12_c" );

} /* End spkw12_c */
