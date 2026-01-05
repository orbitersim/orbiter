/*

-Procedure spkopa_c ( SPK open for addition )

-Abstract

   Open an existing SPK file for subsequent write.

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

-Keywords

   SPK

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void spkopa_c ( ConstSpiceChar * file,
                   SpiceInt       * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   file       I   The name of an existing SPK file.
   handle     O   Handle attached to the SPK file opened to append.

-Detailed_Input

   file        is the name of an existing SPK file to which
               you wish to append additional SPK segments.

-Detailed_Output

   handle      is the DAF integer handle that refers to the SPK file
               opened for appending. `handle' is required by any of the
               SPK writing routines.

-Parameters

   None.

-Exceptions

   If any of the following exceptions occur, `handle' will be returned
   with the value 0.

   1)  If the file specified does not exist, the error
       SPICE(FILENOTFOUND) is signaled by a routine in the call tree
       of this routine.

   2)  If the file specified is not an SPK file, the error
       SPICE(FILEISNOTSPK) is signaled by a routine in the call tree
       of this routine.

   3)  If the specified SPK file cannot be opened for writing, an
       error is signaled by a routine in the call tree of this
       routine.

   4)  If the specified SPK file uses a non-native binary file
       format, an error is signaled by a routine in the call tree of
       this routine.

   5)  If the specified SPK file is corrupted or otherwise invalid,
       an error is signaled by a routine in the call tree of this
       routine.

   6)  If the `file' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   7)  If the `file' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This file provides an interface for opening existing SPK
   files for the addition of SPK segments. If you need
   to open an new SPK file for writing, call the routine spkopn_c.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example demonstrates how to add a new segment to an
      SPK type 5 kernel.

      For this example, we will first create an SPK type 5 kernel
      containing only one segment, given a time-ordered set of
      discrete states and epochs, and the gravitational parameter
      of a central body.

      Then, we will reopen the SPK and add a second segment. The
      example below shows one set of calls that you could perform
      to make the addition. Obviously, there is no need to close
      and re-open the file in order to add multiple segments. It is
      done in this example to demonstrate the use of spkopa_c.

      Note that you could add segments of other data types by
      replacing the call to spkw05_c with a suitably modified call to
      another SPK writing routine.


      Example code begins here.


      /.
         Program spkopa_ex1
      ./
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Define the segment identifier parameters.
         ./
         #define SPK5         "spkopa_ex1.bsp"
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

         SpiceDouble          epoch1 [NSTATS] = { 100.0, 200.0, 300.0,
                                                  400.0, 500.0, 600.0,
                                                  700.0, 800.0, 900.0 };

         SpiceDouble          epoch2 [NSTATS] = { 1100.0, 1200.0, 1300.0,
                                                  1400.0, 1500.0, 1600.0,
                                                  1700.0, 1800.0, 1900.0 };

         /.
         Set the start and stop times of interval covered by
         the first segment.
         ./
         first  = epoch1[0];
         last   = epoch1[NSTATS-1];

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
         segid  = "SPK type 5 test segment #1";

         /.
         Open a new SPK file.
         ./
         spkopn_c ( SPK5, ifname, ncomch, &handle );

         /.
         Write the segment.
         ./
         spkw05_c ( handle, BODY,  CENTER, REF,    first,  last,
                    segid,  GMSUN, NSTATS, states, epoch1 );

         /.
         Close the SPK file.
         ./
         spkcls_c ( handle );

         /.
         At this point we have an existing SPK type 5 kernel
         that contains a single segment. Let's now demonstrate
         the use of spkopa_c.

         Open the an existing SPK file for subsequent write.
         ./
         spkopa_c ( SPK5, &handle );

         /.
         Set the start and stop times of interval covered by
         the second segment, and the segment ID.
         ./
         first  = epoch2[0];
         last   = epoch2[NSTATS-1];

         segid  = "SPK type 5 test segment #2";

         /.
         Now write the second segment. Use the same set of
         states time-ordered set of discrete states and the
         gravitational parameter. Set the epochs to be `epoch2'.
         ./
         spkw05_c ( handle, BODY,  CENTER, REF,    first,  last,
                    segid,  GMSUN, NSTATS, states, epoch2 );

         /.
         Finally, close the file.
         ./
         spkcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new SPK type 5, with two
      segments, exists in the output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   F.S. Turner         (JPL)

-Version

   -CSPICE Version 1.0.1, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Added entries #3 to #5 in -Exceptions section.

   -CSPICE Version 1.0.0, 16-MAR-1999 (FST)

-Index_Entries

   Open an existing SPK file for adding segments

-&
*/

{  /* Begin spkopa_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "spkopa_c" );

   /*
   Check the input string file to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkopa_c", file );

   /*
   Call the f2c'd Fortran routine.
   */
   spkopa_ ( ( char     * )  file,
             ( integer  * )  handle,
             ( ftnlen     )  strlen(file) );

   chkout_c ( "spkopa_c" );

} /* End spkopa_c */
