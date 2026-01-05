/*

-Procedure kdata_c ( Kernel Data )

-Abstract

   Return data for the nth kernel that is among a list of specified
   kernel types.

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

   KERNEL

-Keywords

   KERNEL

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void kdata_c ( SpiceInt          which,
                  ConstSpiceChar  * kind,
                  SpiceInt          fileln,
                  SpiceInt          filtln,
                  SpiceInt          srclen,
                  SpiceChar       * file,
                  SpiceChar       * filtyp,
                  SpiceChar       * srcfil,
                  SpiceInt        * handle,
                  SpiceBoolean    * found  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   which      I   Index of kernel to fetch from the list of kernels.
   kind       I   The kind of kernel to which fetches are limited.
   fileln     I   Maximum length of output string `fileln'.
   filtln     I   Maximum length of output string `filtln'.
   srclen     I   Maximum length of output string `srcfil'.
   file       O   The name of the kernel file.
   filtyp     O   The type of the kernel.
   srcfil     O   Name of the source file used to load `file'.
   handle     O   The handle attached to `file'.
   found      O   SPICETRUE if the specified file could be located.

-Detailed_Input

   which       is the number of the kernel to fetch (matching the type
               specified by `kind') from the list of kernels that have
               been loaded through the routine furnsh_c but that have not
               been unloaded through the routine unload_c.

               The range of `which' is 0 to count-1, where `count' is the
               number of kernels loaded via furnsh_c of type `kind'. This
               count may be obtained by calling ktotal_c. See the
               -Examples section for an illustrative example.

   kind        is a list of types of kernels to be considered when
               fetching kernels from the list of loaded kernels. `kind'
               should consist of words from list of kernel types
               given below.

                  SPK  --- All SPK files are counted in the total.
                  CK   --- All CK files are counted in the total.
                  DSK  --- All DSK files are counted in the total.
                  PCK  --- All binary PCK files are counted in the
                           total.
                  EK   --- All EK files are counted in the total.
                  TEXT --- All text kernels that are not meta-text
                           kernels are included in the total.
                  META --- All meta-text kernels are counted in the
                           total.
                  ALL  --- Every type of kernel is counted in the
                           total.

               `kind' is case insensitive. If a word appears in `kind'
               that is not one of those listed above, it is ignored.

               When `kind' consists of multiple words, the words must
               be separated by blanks. Examples of valid lists are the
               strings

                  "SPK CK TEXT"
                  "SPK CK text"
                  "PCK DSK"
                  "CK"
                  "ALL"

               See the routine ktotal_c for examples of the use of `kind'.

   fileln      is the maximum allowed length of the output string `file'.
               This length must be large enough to hold the output string
               plus the null-terminator character. If the output string
               is expected to have N characters, `fileln' should be at
               least N+1.

   filtln      is the maximum allowed length of the output string
               `filtyp'. This length must be large enough to hold the
               output string plus the null-terminator character. If the
               output string is expected to have N characters, `filtln'
               should be at least N+1.

   srclen      is the maximum allowed length of the output string
               `srcfil'. This length must be large enough to hold the
               output string plus the null-terminator character. If the
               output string is expected to have N characters, `srclen'
               should be at least N+1.

-Detailed_Output

   file        is the name of the file having index `which' in the
               sequence of files of type `kind' that is currently loaded
               via furnsh_c. `file' will be empty if there is not such
               kernel loaded.

   filtyp      is the type of the kernel specified by `file'. `file'
               will be empty if there is no file matching the
               specification of `which' and `kind'.

   srcfil      is the name of the source file that was used to
               specify `file' as one to load. If `file' was loaded
               directly via a call to furnsh_c, `srcfil' will be empty.
               If there is no file matching the specification of
               `which' and `kind', `srcfil' will be empty.

   handle      is the handle attached to `file' if it is a binary
               kernel. If `file' is a text kernel or meta-text kernel
               `handle' will be zero. If there is no file matching
               the specification of `which' and `kind', `handle' will be
               set to zero.

   found       is returned SPICETRUE if a `file' matching the specification
               of `which' and `kind' exists. If there is no such file,
               `found' will be set to SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If a file is not loaded matching the specification of `which' and
       `kind', `found' will be SPICEFALSE, `file', `filtyp', and `srcfil'
       will be empty and `handle' will be set to zero.

   2)  If any of `file', `filtyp' or `srcfil' output strings has length
       too short to contain the corresponding output string, the
       string is truncated on the right.

   3)  If the `kind' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `kind' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   5)  If any of the `file', `filtyp' or `srcfil' output string
       pointers is null, the error SPICE(NULLPOINTER) is signaled.

   6)  If any of the `file', `filtyp' or `srcfil' output strings has
       length less than two characters, the error
       SPICE(STRINGTOOSHORT) is signaled, since the output string is
       too short to contain one character of output data plus a null
       terminator.

-Files

   None.

-Particulars

   This routine allows you to determine which kernels have been
   loaded via furnsh_c and to obtain information sufficient to directly
   query those files.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Load a meta-kernel with a PCK, an LSK and an SPK and loop over
      the loaded kernels, outputting file information for each of
      them.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: kdata_ex1.tm

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
            pck00009.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00009.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program kdata_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants.
         ./
         #define  FNAMLN   256
         #define  FTYPLN   33
         #define  SRCLEN   256

         /.
         Local variables.
         ./
         SpiceBoolean    found;

         SpiceInt        which;
         SpiceInt        count;
         SpiceInt        handle;

         SpiceChar       file [FNAMLN];
         SpiceChar       filtyp [FTYPLN];
         SpiceChar       srcfil[SRCLEN];

         /.
         Load several kernel files.
         ./
         furnsh_c( "kdata_ex1.tm" );

         /.
         Count the number of loaded kernel files.
         ./
         ktotal_c ( "ALL", &count );

         for ( which = 0;  which <= count;  which++ )
         {
            kdata_c ( which,  "ALL",  FNAMLN,   FTYPLN, SRCLEN,
                      file,  filtyp,  srcfil,  &handle,  &found );

            if ( found )
            {
               printf ( "Index : %d\n", which  );
               printf ( "File  : %s\n", file  );
               printf ( "Type  : %s\n", filtyp  );
               printf ( "Source: %s\n", srcfil );
               printf ( "Handle: %d\n", handle );
               printf ( "\n"                   );
            }
            else
            {
               printf ( "No kernel found with index: %d\n", which );
            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Index : 0
      File  : kdata_ex1.tm
      Type  : META
      Source:
      Handle: 0

      Index : 1
      File  : de421.bsp
      Type  : SPK
      Source: kdata_ex1.tm
      Handle: 1

      Index : 2
      File  : pck00009.tpc
      Type  : TEXT
      Source: kdata_ex1.tm
      Handle: 0

      Index : 3
      File  : naif0009.tls
      Type  : TEXT
      Source: kdata_ex1.tm
      Handle: 0

      No kernel found with index: 4


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   L.S. Elson          (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.0, 10-AUG-2021 (JDR) (NJB)

       Changed argument names "fillen", "typlen" and "source" to
       "fileln", "filtln" and "srcfil" for consistency with other
       routines.

       Edited the header to comply with NAIF standard. Added
       example's problem statement and meta-kernel, and updated
       example to provide information about each of the loaded files.

       Updated -Detailed_Input description of input argument "kind" to
       illustrate use of multi-word lists. Added KERNEL to the list
       of required readings.

   -CSPICE Version 1.1.4, 20-JAN-2016 (NJB)

      Updated header to reflect support for use of DSKs.
      Made minor updates to example program. Added
      Ed Wright as an author of this routine.

   -CSPICE Version 1.1.3, 02-MAY-2008 (EDW)

      standard.ker renamed standard.tm

   -CSPICE Version 1.1.2, 05-SEP-2007 (EDW)

      Expanded -Examples section to a full, compilable program.

   -CSPICE Version 1.1.1, 29-DEC-2004 (LSE)

      Corrected example code to match routine's argument list.
      (2 arguments reversed)

   -CSPICE Version 1.1.0, 02-FEB-2003 (EDW)

      Corrected example code to match routine's argument list.

   -CSPICE Version 1.0.0, 12-SEP-1999 (NJB) (WLT)

-Index_Entries

   Retrieve information on loaded SPICE kernels

-&
*/

{ /* Begin kdata_c */


   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "kdata_c" );


   /*
   Check the input string kind to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "kdata_c", kind );


   /*
   Make sure the output string file has at least enough room for one
   output character and a null terminator.  Also check for a null
   pointer.
   */
   CHKOSTR ( CHK_STANDARD, "kdata_c", file, fileln );


   /*
   Make sure the output string filtyp has at least enough room for one
   output character and a null terminator.  Also check for a null
   pointer.
   */
   CHKOSTR ( CHK_STANDARD, "kdata_c", filtyp, filtln );


   /*
   Make sure the output string source has at least enough room for one
   output character and a null terminator.  Also check for a null
   pointer.
   */
   CHKOSTR ( CHK_STANDARD, "kdata_c", srcfil, srclen );


   /*
   Map the input index from C to Fortran style.
   */

   which++;


   /*
   Call the f2c'd routine.
   */
   kdata_ (  ( integer   * ) &which,
             ( char      * ) kind,
             ( char      * ) file,
             ( char      * ) filtyp,
             ( char      * ) srcfil,
             ( integer   * ) handle,
             ( logical   * ) &fnd,
             ( ftnlen      ) strlen(kind),
             ( ftnlen      ) fileln-1,
             ( ftnlen      ) filtln-1,
             ( ftnlen      ) srclen-1     );


   /*
   Convert the output strings from Fortran style to C style.  Set
   the SpiceBoolean output found flag.
   */
   F2C_ConvertStr( fileln, file  );
   F2C_ConvertStr( filtln, filtyp  );
   F2C_ConvertStr( srclen, srcfil );

   *found = fnd;


   chkout_c ( "kdata_c" );

} /* End kdata_c */
