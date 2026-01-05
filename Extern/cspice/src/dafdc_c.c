/*

-Procedure dafdc_c ( DAF delete comments )

-Abstract

   Delete the entire comment area of a specified DAF file.

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

   None.

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void dafdc_c ( SpiceInt handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   The handle of a binary DAF opened for writing.

-Detailed_Input

   handle      is the handle of a binary DAF that is to have its entire
               comment area deleted. The DAF must have been opened
               with write access.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If the binary DAF attached to `handle' is not open with write
       access, an error is signaled by a routine in the call tree of
       this routine.

-Files

   See argument `handle' in -Detailed_Input.

-Particulars

   A binary DAF contains an area which is reserved for storing
   annotations or descriptive textual information about the data
   contained in a file. This area is referred to as the "comment
   area" of the file. The comment area of a DAF is a line oriented
   medium for storing textual information. The comment area preserves
   any leading or embedded white space in the line(s) of text which are
   stored, so that the appearance of the of information will be
   unchanged when it is retrieved (extracted) at some other time.
   Trailing blanks, however, are NOT preserved, due to the way that
   character strings are represented in standard Fortran 77.

   This routine will delete the entire comment area from the binary DAF
   attached to `handle'. The size of the binary DAF will remain
   unchanged. The space that was used by the comment records is
   reclaimed: the data area of the DAF is shifted toward the beginning

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Delete the entire comment area of a DAF file. Note that this
      action should only be performed if fresh new comments are to
      be placed within the DAF file.

      Use the SPK kernel below as input DAF file for the program.

         earthstns_itrf93_201023.bsp


      Example code begins here.


      /.
         Program dafdc_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local parameters
         ./
         #define KERNEL       "earthstns_itrf93_201023.bsp"
         #define BUFFSZ       10
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
         Open a DAF for write. Return a `handle' referring to the
         file.
         ./
         dafopw_c ( KERNEL, &handle );

         /.
         Print the first 10 lines of comments from the DAF file.
         ./
         printf( "Comment area of input DAF file (max. 10 lines): \n" );
         printf( "--------------------------------"
                 "------------------------------\n" );

         dafec_c ( handle, BUFFSZ, LINLEN, &n, buffer, &done );

         for ( i = 0; i < n; i++ )
         {
            printf( "%s\n", buffer[i] );
         }

         printf( "--------------------------------"
                 "------------------------------\n" );
         printf( " \n" );
         printf( "Deleting entire comment area...\n" );

         /.
         Delete all the comments from the DAF file.
         ./
         dafdc_c ( handle );

         /.
         Close the DAF file and re-open it for read
         access to work around the dafec_c restriction
         on comments not to be modified while they are
         being extracted.
         ./
         dafcls_c ( handle );

         dafopr_c ( KERNEL, &handle );

         /.
         Check if the comments have indeed been deleted.
         ./
         dafec_c ( handle, BUFFSZ, LINLEN, &n, buffer, &done );

         if ( done  &&  ( n == 0 ) )
         {
            printf( " \n" );
            printf( "   Successful operation.\n" );
         }
         else
         {
            printf( " \n" );
            printf( "   Operation failed.\n" );
         }

         /.
         Safely close the DAF.
         ./
         dafcls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Comment area of input DAF file (max. 10 lines):
      --------------------------------------------------------------

         SPK for DSN Station Locations
         =====================================================================

         Original file name:                   earthstns_itrf93_201023.bsp
         Creation date:                        2020 October 28 12:30
         Created by:                           Nat Bachman  (NAIF/JPL)


         Introduction
      --------------------------------------------------------------

      Deleting entire comment area...

         Successful operation.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)

-Version

   -CSPICE Version 1.0.1, 25-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

   -CSPICE Version 1.0.0, 16-NOV-2006 (NJB) (KRG)

-Index_Entries

   delete DAF comment area

-&
*/

{ /* Begin dafdc_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dafdc_c" );


   /*
   Hand off the task to the f2c'd routine.
   */
   dafdc_ ( (integer *) &handle );



   chkout_c ( "dafdc_c" );

} /* End dafdc_c */
