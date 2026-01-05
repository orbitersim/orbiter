/*

-Procedure spkopn_c ( SPK, open new file. )

-Abstract

   Create a new SPK file, returning the handle of the opened file.

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

   void spkopn_c ( ConstSpiceChar * fname,
                   ConstSpiceChar * ifname,
                   SpiceInt         ncomch,
                   SpiceInt       * handle  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   The name of the new SPK file to be created.
   ifname     I   The internal filename for the SPK file.
   ncomch     I   The number of characters to reserve for comments.
   handle     O   The handle of the opened SPK file.

-Detailed_Input

   fname       is the name of the new SPK file to be created.

   ifname      is the internal filename for the SPK file that is being
               created. The internal filename may be up to 60 characters
               long. If you do not have any conventions for tagging your
               files, an internal filename of "SPK_file" is perfectly
               acceptable. You may also leave it blank if you like.

   ncomch      is the space, measured in characters, to be
               initially set aside for the comment area when a new SPK
               file is opened. The amount of space actually set aside
               may be greater than the amount requested, due to the
               manner in which comment records are allocated in an SPK
               file. However, the amount of space set aside for comments
               will always be at least the amount that was requested.

               The value of ncomch should be greater than or equal to
               zero, i.e., 0 <= ncomch. A negative value, should one
               occur, will be assumed to be zero.

-Detailed_Output

   handle      is the handle of the opened SPK file. If an error occurs
               when opening the file, the value of this variable should
               not be used, as it will not represent a valid handle.

-Parameters

   None.

-Exceptions

   1)  If the value of `ncomch' is negative, a value of zero (0) will
       be used for the number of comment characters to be set aside
       for comments.

   2)  If an error occurs while attempting to open the SPK file, the
       value of `handle' will not represent a valid file handle.

   3)  If any of the `fname' or `ifname' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   4)  If any of the `fname' or `ifname' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

-Files

   See arguments `fname' and `handle'.

-Particulars

   Open a new SPK file, reserving room for comments if requested.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example demonstrates how to create an SPK type 8 kernel
      containing only one segment, given a time-ordered set of
      discrete states and epochs.


      Example code begins here.


      /.
         Program spkopn_ex1
      ./
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Define the segment identifier parameters.
         ./
         #define SPK8         "spkopn_ex1.bsp"
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

         SpiceDouble          begtim;
         SpiceDouble          first;
         SpiceDouble          last;
         SpiceDouble          step;

         SpiceInt             handle;
         SpiceInt             ncomch;

         /.
         Set the array of discrete states to write to the SPK
         segment.
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
         segment, and the time step separating epochs of states.
         ./
         first = 100.0;
         last  = 900.0;
         step  = 100.0;

         /.
         `ncomch' is the number of characters to reserve for the
         kernel's comment area. This example doesn't write
         comments, so set to zero.
         ./
         ncomch = 0;

         /.
         Internal file name and segment ID.
         ./
         ifname = "Type 8 SPK internal file name.";
         segid  = "SPK type 8 test segment";

         /.
         Open a new SPK file.
         ./
         spkopn_c ( SPK8, ifname, ncomch, &handle );

         /.
         Set the epoch of first state in `states' array to be
         the start time of the interval covered by the segment.
         ./
         begtim = first;

         /.
         Create a type 8 segment.
         ./
         spkw08_c ( handle, BODY,   CENTER, REF,    first,  last,
                    segid,  DEGREE, NSTATS, states, begtim, step );

         /.
         Close the SPK file.
         ./
         spkcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new SPK type 8 exists in
      the output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   F.S. Turner         (JPL)

-Version

   -CSPICE Version 1.2.0, 05-AUG-2021 (JDR)

       Changed input argument name "name" to "fname" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added
       complete code example based on spkw08_c example.

   -CSPICE Version 1.1.0, 20-APR-2005 (NJB)

       Bug fix: address, rather than value, of `ncomch' is now
       passed to spkopn_.

       Header comments indicating that `ncomch' is not used have
       been deleted.

   -CSPICE Version 1.0.0, 16-MAR-1999 (FST)

-Index_Entries

   open a new SPK file

-&
*/

{ /* Begin spkopn_c */

   /*
   Participate in error tracing.
   */

   chkin_c ( "spkopn_c" );

   /*
   Check the input string `fname' to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkopn_c", fname );

   /*
   Check the input string ifname to make sure the pointer is
   non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spkopn_c", ifname );

   /*
   Call the f2c'd Fortran routine.
   */
   spkopn_ ( ( char    * )  fname,
             ( char    * )  ifname,
             ( integer * )  &ncomch,
             ( integer * )  handle,
             ( ftnlen    )  strlen(fname),
             ( ftnlen    )  strlen(ifname) );

   chkout_c ( "spkopn_c" );

} /* End spkopn_c */
