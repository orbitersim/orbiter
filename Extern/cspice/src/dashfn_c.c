/*

-Procedure dashfn_c ( DAS, handle to file name )

-Abstract

   Return the name of the DAS file associated with a handle.

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

   DAS

-Keywords

   CONVERSION
   DAS
   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"
   #undef dashfn_c


   void dashfn_c ( SpiceInt     handle,
                   SpiceInt     namlen,
                   SpiceChar  * fname  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of a DAS file.
   namlen     I   Length of output file name string.
   fname      O   Corresponding file name.

-Detailed_Input

   handle      is the handle of a previously opened DAS file.

   namlen      is the count of characters in the output string,
               including room for the terminating null character.

-Detailed_Output

   fname       is the name of the DAS file associated with the input
               file handle.

-Parameters

   None.

-Exceptions

   1)  If the specified handle does not belong to any file that is
       currently known to be open, the error SPICE(DASNOSUCHHANDLE)
       is signaled by a routine in the call tree of this routine.

   2)  If `fname' string is too short to contain the output string,
       the name is truncated on the right.

   3)  If the `fname' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `fname' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   See the description of the argument `handle' in -Detailed_Input.

-Particulars

   It may be desirable to recover the names of one or more DAS
   files in a different part of the program from the one in which
   they were opened. Note that the names returned by dashfn_c may
   not be identical to the names used to open the files. Under
   most operating systems, a particular file can be accessed using
   many different names. dashfn_c returns one of them.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) In the following program, the name of a DAS file is
      recovered using the handle associated with the file.


      Use the DSK kernel below as input DAS file for the example.

         phobos512.bds


      Example code begins here.


      /.
         Program dashfn_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define DAS             "phobos512.bds"
         #define FILSIZ          256

         /.
         Local variables
         ./
         SpiceChar               fname [ FILSIZ ];


         SpiceInt                handle;

         /.
         Open the DAS file for read access.
         ./
         dasopr_c ( DAS, &handle );

         /.
         Map the handle to a file name.
         ./
         dashfn_c ( handle, FILSIZ, fname );

         printf ( "DAS file name = <%s>.\n", fname );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      DAS file name = <phobos512.bds>.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 10-DEC-2016 (NJB) (KRG) (WLT) (IMU)

-Index_Entries

   map DAS handle to file name

-&
*/

{ /* Begin dashfn_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "dashfn_c" );

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "dashfn_c", fname, namlen );


   dashfn_ ( (SpiceInt   *) &handle,
             (SpiceChar  *) fname,
             (ftnlen      ) namlen   );

   /*
   The output file name is a Fortranish type string.
   Convert the string to C type.
   */
   F2C_ConvertStr ( namlen, fname );


   chkout_c ( "dashfn_c" );

} /* End dashfn_c */
