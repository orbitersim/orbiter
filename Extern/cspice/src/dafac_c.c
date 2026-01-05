/*

-Procedure dafac_c ( DAF add comments )

-Abstract

   Add comments from a buffer of character strings to the comment
   area of a binary DAF file, appending them to any comments which
   are already present in the file's comment area.

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

   DAF

-Keywords

   FILES
   UTILITY

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"
   #undef   dafac_c

   void dafac_c ( SpiceInt      handle,
                  SpiceInt      n,
                  SpiceInt      buflen,
                  const void  * buffer  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I    handle of a DAF opened with write access.
   n          I    Number of comments to put into the comment area.
   buflen     I    Length of elements
   buffer     I    Buffer of comments to put into the comment area.

-Detailed_Input

   handle      is the file handle of a binary DAF which has been opened
               with write access.

   n           is the number of rows in the array `buffer'. This is
               also the number of comment lines in `buffer' that are to
               be added to the comment area of the binary DAF attached
               to `handle'.

   buffer      is a string buffer containing comments which are to be
               added to the comment area of the binary DAF attached to
               `handle'.  `buffer' should be declared by the caller has
               follows:

                  SpiceChar    buffer[n][buflen];

               Each row of the buffer should contain one comment line.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If the number of comments to be added is not positive, the
       error SPICE(INVALIDARGUMENT) is signaled by a routine in the
       call tree of this routine.

   2)  If a non printing ASCII character is encountered in the
       comments, the error SPICE(ILLEGALCHARACTER) is signaled by a
       routine in the call tree of this routine.

   3)  If the binary DAF file attached to `handle' is not open with
       write access, an error is signaled by a routine in the call
       tree of this routine.

   4)  If the end of the comments cannot be found, i.e., the end of
       comments marker is missing on the last comment record, the
       error SPICE(BADCOMMENTAREA) is signaled by a routine in the
       call tree of this routine.

   5)  If the `buffer' input array pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `buffer' input array strings have length less than two
       characters, the error SPICE(EMPTYSTRING) is signaled.

-Files

   See argument `handle' in -Detailed_Input.

-Particulars

   A binary DAF contains a data area which is reserved for storing
   annotations or descriptive textual information about the data
   contained in a file. This area is referred to as the "comment
   area" of the file. The comment area of a DAF is a line oriented
   medium for storing textual information. The comment area preserves
   leading or embedded white space in the line(s) of text which are
   stored so that the appearance of the information will be unchanged
   when it is retrieved (extracted) at some other time. Trailing
   blanks, however, are NOT preserved, due to the way that character
   strings are represented in standard Fortran 77.

   This routine will take a buffer of text lines and add (append) them
   to the comment area of a binary DAF. If there are no comments in the
   comment area of the file, then space will be allocated and the text
   lines in `buffer' will be placed into the comment area. The text lines
   may contain only printable ASCII characters (decimal values 32 -
   126).

   There is NO maximum length imposed on the significant portion of a
   text line that may be placed into the comment area of a DAF. The
   maximum length of a line stored in the comment area should be
   reasonable, however, so that they may be easily extracted. A good
   maximum value for this would be 255 characters, as this can easily
   accommodate "screen width" lines as well as long lines which may
   contain some other form of information.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This example demonstrates how to append new comments to the
      comment area of a DAF file.

      Use the SPK kernel below as input DAF file for the program.

         earthstns_itrf93_201023.bsp


      Example code begins here.


      /.
         Program dafac_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters
         ./
         #define KERNEL       "earthstns_itrf93_201023.bsp"
         #define BUFFSZ       25
         #define CMTSIZ       7
         #define LINLEN       1000

         /.
         Local variables.
         ./
         SpiceChar            buffer [BUFFSZ][LINLEN];

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             n;

         SpiceBoolean         done;

         /.
         Set the new comments to be added to the DAF file.
         ./
         SpiceChar            newcmt [CMTSIZ][LINLEN] = {
            "================== NEW COMMENTS ==================",
            "",
            "   New comments can be appended to the end of the",
            "   comment area of a DAF file, with a single",
            "   operation.",
            "",
            "================ END NEW COMMENTS ================" };

         /.
         Open a DAF for write. Return a `handle' referring to the
         file.
         ./
         dafopw_c ( KERNEL, &handle );

         /.
         Print the end of comment area from the DAF file.
         (Maximum 15 lines.)
         ./
         done = SPICEFALSE;

         while ( ! done )
         {
            dafec_c ( handle, 15, LINLEN, &n, buffer, &done );

            if ( done )
            {

               printf( "End of comment area of input DAF file "
                       "(max. 15 lines):\n" );
               printf( "-------------------------------"
                       "-------------------------------\n" );

               for ( i = 0; i < n; i++ )
               {
                  printf( "%s\n", buffer[i] );
               }

               printf( "-------------------------------"
                       "-------------------------------\n" );

            }
         }

         /.
         Append the new comments to the DAF file.
         ./
         dafac_c ( handle, CMTSIZ, LINLEN, newcmt );

         /.
         Safely close the DAF.
         ./
         dafcls_c ( handle );

         /.
         Check if the comments have indeed appended.

         Open a DAF for read.
         ./
         dafopr_c ( KERNEL, &handle );
         done = SPICEFALSE;

         while ( ! done )
         {
            dafec_c ( handle, BUFFSZ, LINLEN, &n, buffer, &done );

            if ( done )
            {

               printf( "End of comment area of input DAF file "
                       "(max. 25 lines):\n" );
               printf( "-------------------------------"
                       "-------------------------------\n" );

               for ( i = 0; i < n; i++ )
               {
                  printf( "%s\n", buffer[i] );
               }

               printf( "-------------------------------"
                       "-------------------------------\n" );

            }
         }

         /.
         Safely close the DAF.
         ./
         dafcls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      End of comment area of input DAF file (max. 15 lines):
      --------------------------------------------------------------
         DSS-65_DXYZ       =    (    -0.0100          0.0242          0.0156***
         DSS-65_TOPO_EPOCH =       @2020-OCT-23/00:00
         DSS-65_UP         =       'Z'
         DSS-65_NORTH      =       'X'

      \begintext
      --------------------------------------------------------------
      End of comment area of input DAF file (max. 25 lines):
      --------------------------------------------------------------
         DSS-65_DXYZ       =    (    -0.0100          0.0242          0.0156***
         DSS-65_TOPO_EPOCH =       @2020-OCT-23/00:00
         DSS-65_UP         =       'Z'
         DSS-65_NORTH      =       'X'

      \begintext
      ================== NEW COMMENTS ==================

         New comments can be appended to the end of the
         comment area of a DAF file, with a single
         operation.

      ================ END NEW COMMENTS ================
      --------------------------------------------------------------


      Warning: incomplete output. 2 lines extended past the right
      margin of the header and have been truncated. These lines are
      marked by "***" at the end of each line.


-Restrictions

   1)  This routine uses constants that are specific to the ASCII
       character sequence. The results of using this routine with
       a different character sequence are unpredictable.

   2)  This routine is only used to extract records on environments
       whose characters are a single byte in size. Updates to this
       routine and routines in its call tree may be required to
       properly handle other cases.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)

-Version

   -CSPICE Version 1.1.0, 25-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete code
       example.

       Changed input argument name "lenvals" to "buflen" for
       consistency with other routines.

   -CSPICE Version 1.0.0, 16-NOV-2006 (NJB) (KRG)

-Index_Entries

   add comments to a binary DAF file
   append comments to a DAF file comment area

-&
*/

{ /* Begin dafac_c */


   /*
   Local variables
   */
   SpiceChar             * fCvalsArr;

   SpiceInt                fCvalsLen;


   /*
   Participate in error tracing.
   */
   chkin_c ( "dafac_c" );


   /*
   Make sure the input string pointer for the `buffer' array is non-null
   and that the length buflen is sufficient.
   */
   CHKOSTR ( CHK_STANDARD, "dafac_c", buffer, buflen );

   /*
   The input buffer contains C-style strings; we must pass a
   Fortran-style buffer to dafac_.
   */
   C2F_MapStrArr ( "dafac_c",
                   n, buflen, buffer, &fCvalsLen, &fCvalsArr );

   if ( failed_c() )
   {
      chkout_c ( "dafac_c" );
      return;
   }


   /*
   Call the f2c'd routine.
   */
   dafac_ ( ( integer * ) &handle,
            ( integer * ) &n,
            ( char    * ) fCvalsArr,
            ( ftnlen    ) fCvalsLen );

   /*
   Free the dynamically allocated array.
   */
   free ( fCvalsArr );


   chkout_c ( "dafac_c" );

} /* End dafac_c */
