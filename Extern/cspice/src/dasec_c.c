/*

-Procedure dasec_c  ( DAS extract comments )

-Abstract

   Extract comments from the comment area of a binary DAS file.

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

   FILES
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef    dasec_c

   void dasec_c ( SpiceInt         handle,
                  SpiceInt         bufsiz,
                  SpiceInt         buffln,
                  SpiceInt       * n,
                  void           * buffer,
                  SpiceBoolean   * done   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of binary DAS file open with read access.
   bufsiz     I   Maximum size, in lines, of `buffer'.
   buffln     I   Line length associated with `buffer'.
   n          O   Number of comments extracted from the DAS file.
   buffer     O   Buffer in which extracted comments are placed.
   done       O   Indicates whether all comments have been extracted.

-Detailed_Input

   handle      is the file handle of a binary DAS file which has been
               opened with read access.

   bufsiz      is the maximum number of comments that may be placed into
               `buffer'. This would typically be the declared array size
               for the C character string array passed into this
               routine.

   buffln      is the common length of the strings in `buffer', including the
               terminating nulls.

-Detailed_Output

   n           is the number of comment lines extracted from the comment area
               of the binary DAS file attached to handle. This number will
               be <= bufsiz on output. If n == bufsiz and done !=
               SPICETRUE then there are more comments left to extract. If
               n == 0, then done == SPICETRUE, i.e., there were no
               comments in the comment area. If there are comments in the
               comment area, or comments remaining after the extraction
               process has begun, n > 0, always.

   buffer      is a list of at most `bufsiz' comments which have been
               extracted from the comment area of the binary DAS
               file attached to handle. `buffer' should be declared as
               follows:

                  ConstSpiceChar   buffer [bufsiz][buffln]

               Each string in buffer is null-terminated.

   done        is a boolean flag indicating whether or not all of the
               comment lines from the comment area of the DAS file have
               been read. This variable has the value SPICETRUE after the
               last comment line has been read. It will have the value
               SPICEFALSE otherwise.

               If there are no comments in the comment area, this
               variable will have the value SPICETRUE, and n == 0.

-Parameters

   None.

-Exceptions

   1)  If the size of the output line buffer is is not positive, the
       error SPICE(INVALIDARGUMENT) is signaled by a routine in the
       call tree of this routine.

   2)  If a comment line in a DAS file is longer than the length of a
       character string array element of `buffer', the error
       SPICE(COMMENTTOOLONG) is signaled by a routine in the call
       tree of this routine.

   3)  If there is a mismatch between the number of comment
       characters found and the number of comment characters
       expected, the error SPICE(BADDASCOMMENTAREA) is signaled by a
       routine in the call tree of this routine.

   4)  If the binary DAS file attached to `handle' is not open for
       reading, an error is signaled by a routine in the call tree of
       this routine.

   5)  If the `buffer' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `buffer' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   See argument handle in -Detailed_Input.

-Particulars

   Binary DAS files contain an area which is reserved for storing
   annotations or descriptive textual information describing the data
   contained in a file. This area is referred to as the "comment
   area" of the file. The comment area of a DAS file is a line
   oriented medium for storing textual information. The comment
   area preserves any leading or embedded white space in the line(s)
   of text which are stored, so that the appearance of the of
   information will be unchanged when it is retrieved (extracted) at
   some other time. Trailing blanks, however, are NOT preserved,
   due to the way that character strings are represented in
   standard Fortran 77.

   This routine will read the comments from the comment area of
   a binary DAS file, placing them into a line buffer. If the line
   buffer is not large enough to hold the entire comment area,
   the portion read will be returned to the caller, and the done
   flag will be set to SPICEFALSE. This allows the comment area to be
   read in "chunks," a buffer at a time. After all of the comment
   lines have been read, the done flag will be set to SPICETRUE.

   After all of the comments in DAS file have been read, the next
   call to this routine will start reading comments at the start
   of the comment area.

   This routine can be used to "simultaneously" extract comments
   from the comment areas of multiple binary DAS files.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following example will extract the entire comment area of a
      binary DAS file attached to `handle', displaying the comments on
      the terminal screen.


      Use the DSK kernel below as input DAS file for the example.

         phobos512.bds


      Example code begins here.


      /.
         Program dasec_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         #define LNSIZE          81
         #define MAXBUF          25

         SpiceBoolean            done;

         SpiceChar               buffer [MAXBUF][LNSIZE];
         SpiceChar             * filename;

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                n;


         filename = "phobos512.bds";

         dasopr_c ( filename, &handle );

         done = SPICEFALSE;

         while ( !done )
         {
            dasec_c( handle, MAXBUF, LNSIZE, &n, buffer, &done );

            for ( i = 0;  i < n;  i++ )
            {
               printf ( "%s\n", buffer[i] );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      *************************************************************************
      MKDSK RUN DATE/TIME: 2010-06-30T16:52:12
      MKDSK SETUP FILE:    phobos512.cmd
      MKDSK INPUT FILE:    phobos_q512.txt
      MKDSK OUTPUT FILE:   phobos512.bds
      OUTPUT FILE STATUS:    NEW FILE
      *************************************************************************

                 \begindata

                 INPUT_SHAPE_FILE    = 'phobos_q512.txt'
                 OUTPUT_DSK_FILE     = 'phobos512.bds'
                 COMMENT_FILE        = ' '
                 LEAPSECONDS_FILE    = 'naif0009.tls'
                 SURFACE_NAME        = 'phobos'
                 CENTER_NAME         = 'phobos'
                 REF_FRAME_NAME      = 'IAU_PHOBOS'
                 START_TIME          = '1950-JAN-1/00:00:00'
                 STOP_TIME           = '2050-JAN-1/00:00:00'
                 DATA_CLASS          = 1
                 INPUT_DATA_UNITS    = ( 'ANGLES    = DEGREES'
                                         'DISTANCES = KILOMETERS' )
                 COORDINATE_SYSTEM   = 'LATITUDINAL'
                 MINIMUM_LATITUDE    = -90
                 MAXIMUM_LATITUDE    =  90
                 MINIMUM_LONGITUDE   = -180
                 MAXIMUM_LONGITUDE   =  180
                 DATA_TYPE           = 2
                 PLATE_TYPE          = 2
                 FINE_VOXEL_SCALE    = 4.0
                 COARSE_VOXEL_SCALE  = 5

                 \begintext

      *************************************************************************


      Warning: incomplete output. 3 lines extended past the right
      margin of the header and have been truncated. These lines are
      marked by "***" at the end of each line.


-Restrictions

   1)  The comment area may consist only of printing ASCII characters,
       decimal values 32 - 126.

   2)  There is NO maximum length imposed on the significant portion
       of a text line that may be placed into the comment area of a
       DAS file. The maximum length of a line stored in the comment
       area should be kept reasonable, so that they may be easily
       extracted. A good value for this would be 255 characters, as
       this can easily accommodate "screen width" lines as well as
       long lines which may contain some other form of information.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)

-Version

   -CSPICE Version 1.1.1, 02-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Updated the example to use CSPICE dasopr_c routine and
       a hardcoded value for the DAS file name.

   -CSPICE Version 1.1.0, 29-JUL-2015 (NJB)

       Bug fix: removed semi-colon at end of the "if"
       statement controlling execution of the call
       to F2C_ConvertStrTrArr. This semi-colon turned
       out to have no effect on the behavior of the
       routine.

   -CSPICE Version 1.0.0, 24-FEB-2003 (NJB) (KRG)

-Index_Entries

   extract comments from a DAS file

-&
*/

{ /* Begin dasec_c */


   /*
   Local variables
   */
   logical                 locDone;


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "dasec_c" );


   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "dasec_c", buffer, buffln );


   /*
   Call the f2c'd routine.
   */
   dasec_  ( (integer *) &handle,
             (integer *) &bufsiz,
             (integer *) n,
             (char    *) buffer,
             (logical *) &locDone,
             (ftnlen   ) buffln-1  );

   /*
   Convert the output array from Fortran to C style.
   */
   if ( *n > 0 )
   {
      F2C_ConvertTrStrArr ( *n,  buffln,  (SpiceChar *)buffer );
   }


   /*
   Set the "done" flag.
   */

   *done = (SpiceBoolean) locDone;


   chkout_c ( "dasec_c" );

} /* End dasec_c */
