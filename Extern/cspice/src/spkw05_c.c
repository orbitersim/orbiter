/*

-Procedure spkw05_c ( Write SPK segment, type 5 )

-Abstract

   Write an SPK segment of type 5 given a time-ordered set of
   discrete states and epochs, and the gravitational parameter
   of a central body.

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

   SPK
   SPC
   NAIF_IDS

-Keywords

   EPHEMERIS

*/
   #include <string.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef    spkw05_c


   void spkw05_c ( SpiceInt                handle,
                   SpiceInt                body,
                   SpiceInt                center,
                   ConstSpiceChar        * frame,
                   SpiceDouble             first,
                   SpiceDouble             last,
                   ConstSpiceChar        * segid,
                   SpiceDouble             gm,
                   SpiceInt                n,
                   ConstSpiceDouble        states [][6],
                   ConstSpiceDouble        epochs []      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of an SPK file open for writing.
   body       I   Body code for ephemeris object.
   center     I   Body code for the center of motion of the body.
   frame      I   The reference frame of the states.
   first      I   First valid time for which states can be computed.
   last       I   Last valid time for which states can be computed.
   segid      I   Segment identifier.
   gm         I   Gravitational parameter of central body.
   n          I   Number of states and epochs.
   states     I   States.
   epochs     I   Epochs.

-Detailed_Input

   handle      is the file handle of an SPK file
               opened for writing.

   body        is the NAIF ID for the body whose states are
               to be recorded in an SPK file.

   center      is the NAIF ID for the center of motion associated
               with `body'.

   frame       is the reference frame that states are referenced to,
               for example "J2000".

   first,
   last        are the bounds on the ephemeris times, expressed as
               seconds past J2000, for which the states can be used
               to interpolate a state for `body'.

   segid       is the segment identifier. An SPK segment identifier
               may contain up to 40 characters.

   gm          is the gravitational parameter of the central body
               ( in units of kilometers **3 / seconds **2 ).

   n           is the number of states and epochs to be stored
               in the segment.

   states      contains a time-ordered array of geometric states
               ( x, y, z, dx/dt, dy/dt, dz/dt, in kilometers and
               kilometers per second ) of the target body with
               respect to the central body specified in the segment
               descriptor.

   epochs      contains the epochs (ephemeris seconds past J2000)
               corresponding to the states in `states'. Epochs must
               form a strictly increasing sequence.

-Detailed_Output

   None. A type 5 segment is written to the file attached to handle.

-Parameters

   None.

-Exceptions

   1)  If `gm' is not positive, the error SPICE(NONPOSITIVEMASS)
       is signaled by a routine in the call tree of this routine.

   2)  If the input epochs do not form an increasing sequence, the
       error SPICE(UNORDEREDTIMES) is signaled by a routine in the
       call tree of this routine.

   3)  If the number of states and epochs is not positive, the error
       SPICE(NUMSTATESNOTPOS) is signaled by a routine in the call
       tree of this routine.

   4)  If `first' is greater than `last', the error SPICE(BADDESCRTIMES)
       is signaled by a routine in the call tree of this routine.

   5)  If `segid' is more than 40 characters long, the error
       SPICE(SEGIDTOOLONG) is signaled by a routine in the call tree
       of this routine.

   6)  If `segid' contains any nonprintable characters, the error
       SPICE(NONPRINTABLECHARS) is signaled by a routine in the call
       tree of this routine.

   7)  If a file I/O problem occurs, an error is signaled by a
       routine in the call tree of this routine.

   8)  If any of the `frame' or `segid' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   9)  If any of the `frame' or `segid' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   A new type 05 SPK segment is written to the SPK file attached
   to handle.

-Particulars

   This routine writes an SPK type 05 data segment to the open SPK
   file according to the format described in the type 05 section of
   the SPK Required Reading. The SPK file must have been opened with
   write access.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example demonstrates how to create an SPK type 5 kernel
      containing only one segment, given a time-ordered set of
      discrete states and epochs, and the gravitational parameter
      of a central body.


      Example code begins here.


      /.
         Program spkw05_ex1
      ./
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Define the segment identifier parameters.
         ./
         #define SPK5         "spkw05_ex1.bsp"
         #define REF          "J2000"
         #define GMSUN        132712440023.310
         #define BODY         3
         #define CENTER       10
         #define NSTATS       9

         /.
         Local variables.
         ./
         SpiceChar          * ifname;
         SpiceChar          * segid;

         SpiceDouble          first;
         SpiceDouble          last;

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

         SpiceDouble          epochs [NSTATS] = { 100.0, 200.0, 300.0,
                                                  400.0, 500.0, 600.0,
                                                  700.0, 800.0, 900.0 };

         /.
         Set the start and end times of interval covered by
         segment.
         ./
         first  = epochs[0];
         last   = epochs[NSTATS-1];

         /.
         `ncomch' is the number of characters to reserve for the
         kernel's comment area. This example doesn't write
         comments, so set to zero.
         ./
         ncomch = 0;

         /.
         Internal file name and segment ID.
         ./
         ifname = "Type 5 SPK internal file name.";
         segid  = "SPK type 5 test segment";

         /.
         Open a new SPK file.
         ./
         spkopn_c ( SPK5, ifname, ncomch, &handle );

         /.
         Write the segment.
         ./
         spkw05_c ( handle, BODY,  CENTER, REF,    first, last,
                    segid,  GMSUN, NSTATS, states, epochs      );

         /.
         Close the SPK file.
         ./
         spkcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new SPK type 5 exists in
      the output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   J.M. Lynch          (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete example code from existing fragment.

       Added entry #1 in -Exceptions section.

   -CSPICE Version 1.0.0, 23-JUN-1999 (NJB) (KRG) (JML) (WLT) (IMU)

-Index_Entries

   write SPK type_5 ephemeris data segment

-&
*/

{ /* Begin spkw05_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "spkw05_c" );


   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkw05_c", frame );
   CHKFSTR ( CHK_STANDARD, "spkw05_c", segid );


   /*
   Write the segment.  Note that the state array DOES NOT require
   transposition!
   */

   spkw05_ ( ( integer    * ) &handle,
             ( integer    * ) &body,
             ( integer    * ) &center,
             ( char       * ) frame,
             ( doublereal * ) &first,
             ( doublereal * ) &last,
             ( char       * ) segid,
             ( doublereal * ) &gm,
             ( integer    * ) &n,
             ( doublereal * ) states,
             ( doublereal * ) epochs,
             ( ftnlen       ) strlen(frame),
             ( ftnlen       ) strlen(segid)  );

   chkout_c ( "spkw05_c" );

} /* End spkw05_c */
