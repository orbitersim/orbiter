/*

-Procedure kinfo_c ( Kernel Information )

-Abstract

   Return information about a loaded kernel specified by name.

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

   None.

-Keywords

   KERNEL

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void kinfo_c ( ConstSpiceChar  * file,
                  SpiceInt          filtln,
                  SpiceInt          srclen,
                  SpiceChar       * filtyp,
                  SpiceChar       * srcfil,
                  SpiceInt        * handle,
                  SpiceBoolean    * found  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   file       I   Name of a kernel to fetch information for
   filtln     I   Available space in output kernel type string.
   srclen     I   Available space in output `srcfil' string.
   filtyp     O   The type of the kernel.
   srcfil     O   Name of the source file used to load `file'.
   handle     O   The handle attached to `file'.
   found      O   SPICETRUE if the specified file could be located.

-Detailed_Input

   file        is the name of a kernel file for which descriptive
               information is desired.

   filtln      is the amount of available space in the output kernel
               type string.

   srclen      is the amount of available space in the output kernel
               `srcfil' string.

-Detailed_Output

   filtyp      is the type of the kernel specified by file. filtyp
               will be empty if `file' is not on the list of kernels
               loaded via furnsh_c.

   srcfil      is the name of the source file that was used to
               specify file as one to load. If `file' was loaded
               directly via a call to furnsh_c, `srcfil' will be empty.
               If `file' is not on the list of kernels loaded via
               furnsh_c, `srcfil' will be empty.

   handle      is the handle attached to `file' if it is a binary
               kernel. If `file' is a text kernel or meta-text kernel
               handle will be zero. If `file' is not on the list of
               kernels loaded via furnsh_c, handle will be set to zero.

   found       is returned SPICETRUE if the specified file exists.
               If there is no such file, found will be set to
               SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If the specified file is not on the list of files that are currently
       loaded via the interface furnsh_c, `found' will be SPICEFALSE,
       `handle' will be set to zero and `filtyp' and `srcfil' will be set to
       empty.

   2)  If any of  `filtyp' or `srcfil' output strings has length too
       short to contain the corresponding output string, the string
       is truncated on the right.

   3)  If the `file' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `file' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   5)  If any of the `filtyp' or `srcfil' output string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   6)  If any of the `filtyp' or `srcfil' output strings has length
       less than two characters, the error SPICE(STRINGTOOSHORT) is
       signaled, since the output string is too short to contain one
       character of output data plus a null terminator.

-Files

   None.

-Particulars

   This routine allows you to request information directly
   for a specific SPICE kernel.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose you wish to determine the types of kernels loaded
      by a given meta-kernel. The following code example shows
      how you might use this routine to do this.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: kinfo_ex1.tm

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
         Program kinfo_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants.
         ./
         #define  FILLEN   256
         #define  TYPLEN   33
         #define  SRCLEN   256

         /.
         Local variables.
         ./
         SpiceInt        which;
         SpiceInt        count;
         SpiceInt        handle;

         SpiceChar       file  [FILLEN];
         SpiceChar       filtyp[TYPLEN];
         SpiceChar       srcfil[SRCLEN];

         SpiceBoolean    found;

         /.
         Load the meta-kernel.
         ./
         furnsh_c( "kinfo_ex1.tm" );

         /.
         Find out the total number of kernels in the kernel pool.
         ./
         ktotal_c ( "all", &count );

         if ( count == 0 )
         {
            printf ( "No files loaded at this time.\n" );
         }
         else
         {
            printf ( "The loaded files files are: \n\n" );
         }

         /.
         Find the file name, type and source for each of the
         kernels in the kernel pool and print its type.
         ./
         for ( which = 0;  which < count;  which++ )
         {

            kdata_c ( which,  "all",    FILLEN,   TYPLEN, SRCLEN,
                      file,   filtyp,  srcfil,  &handle,  &found );

            kinfo_c ( file,   TYPLEN,  SRCLEN,
                      filtyp, srcfil, &handle, &found );

            if (  eqstr_c ( filtyp, "SPK" )  )
            {
               printf ( "%s is an SPK file.\n", file );
            }
            else if (  eqstr_c ( filtyp, "CK" )  )
            {
               printf ( "%s is a CK file.\n", file );
            }
            else if (  eqstr_c ( filtyp, "PCK" )  )
            {
               printf ( "%s is a PCK file.\n", file );
            }
            else if (  eqstr_c ( filtyp, "DSK" )  )
            {
               printf ( "%s is a DSK file.\n", file );
            }
            else if (  eqstr_c ( filtyp, "EK" )  )
            {
               printf ( "%s is an EK file.\n", file );
            }
            else if (  eqstr_c ( filtyp, "META" )  )
            {
               printf ( "%s is a meta-text kernel.\n", file );
            }
            else
            {
               printf ( "%s is a text kernel.\n", file );
            }
         }

         return(0);
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      The loaded files files are:

      kinfo_ex1.tm is a meta-text kernel.
      de421.bsp is an SPK file.
      pck00008.tpc is a text kernel.
      naif0009.tls is a text kernel.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.3.0, 10-AUG-2021 (JDR)

       Changed argument names "typlen" and "source" to "filtln" and
       "srcfil" for consistency with other routines.

       Edited the header to comply with NAIF standard. Added
       meta-kernel and output solution to the example.

   -CSPICE Version 1.2.0, 20-JAN-2016 (NJB)

       Updated header example program to support DSKs.
       Updated description of example program. Removed
       references to the term "entry point." Added
       Ed Wright as an author of this routine.

   -CSPICE Version 1.1.2, 02-MAY-2008 (EDW)

       standard.ker renamed standard.tm

   -CSPICE Version 1.1.1, 05-SEP-2007 (EDW)

       Expanded -Examples section to a full, compilable program.

   -CSPICE Version 1.1.0, 02-FEB-2003 (EDW)

       Corrected example code to match routine's argument list.

   -CSPICE Version 1.0.0, 01-SEP-1999 (NJB) (WLT)

-Index_Entries

   Fetch information about a loaded SPICE kernel

-&
*/

{ /* Begin kinfo_c */

   /*
   Local variables
   */
   logical                 fnd;



   /*
   Participate in error tracing.
   */
   chkin_c ( "kinfo_c" );


   /*
   Check the input string file to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "kinfo_c", file );


   /*
   Make sure the output string `filtyp' has at least enough room for one
   output character and a null terminator.  Also check for a null
   pointer.
   */
   CHKOSTR ( CHK_STANDARD, "kinfo_c", filtyp, filtln );


   /*
   Make sure the output string `srcfil' has at least enough room for one
   output character and a null terminator.  Also check for a null
   pointer.
   */
   CHKOSTR ( CHK_STANDARD, "kinfo_c", srcfil, srclen );

   /*
   Call the f2c'd routine.
   */
   kinfo_ (  ( char      * ) file,
             ( char      * ) filtyp,
             ( char      * ) srcfil,
             ( integer   * ) handle,
             ( logical   * ) &fnd,
             ( ftnlen      ) strlen(file),
             ( ftnlen      ) filtln-1,
             ( ftnlen      ) srclen-1     );


   /*
   Convert the output strings from Fortran style to C style.  Set
   the SpiceBoolean output found flag.
   */
   F2C_ConvertStr( filtln, filtyp );
   F2C_ConvertStr( srclen, srcfil );

   *found = fnd;


   chkout_c ( "kinfo_c" );

} /* End kinfo_c */
