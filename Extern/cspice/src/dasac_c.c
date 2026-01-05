/*

-Procedure dasac_c ( DAS add comments )

-Abstract

   Add comments from a buffer of character strings to the comment
   area of a binary DAS file, appending them to any comments which
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

   DAS

-Keywords

   FILES
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef    dasac_c


   void dasac_c ( SpiceInt       handle,
                  SpiceInt       n,
                  SpiceInt       buflen,
                  const void   * buffer  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   DAS handle of a file opened with write access.
   n          I   Number of comments to put into the comment area.
   buflen     I   Line length associated with buffer.
   buffer     I   Buffer of lines to be put into the comment area.

-Detailed_Input

   handle      The file handle of a binary DAS file which has been
               opened with write access.

   n           The number of strings in buffer that are to be
               appended to the comment area of the binary DAS file
               attached to handle.

   buflen      is the common length of the strings in buffer, including the
               terminating nulls.

   buffer      A buffer containing comments which are to be added
               to the comment area of the binary DAS file attached
               to handle. buffer should be declared as follows:

                  ConstSpiceChar   buffer [n][buflen]

               Each string in buffer is null-terminated.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If the number of comments to be added is not positive, the
       error SPICE(INVALIDARGUMENT) is signaled.

   2)  If a non printing ASCII character is encountered in the
       comments, the error SPICE(ILLEGALCHARACTER) is signaled by a
       routine in the call tree of this routine.

   3)  If the binary DAS file attached to `handle' is not open with
       write access, an error is signaled by a routine in the call
       tree of this routine.

   4)  If the `buffer' input array pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the `buffer' input array strings have length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled.

-Files

   See argument handle in -Detailed_Input.

-Particulars

   Binary DAS files contain a data area which is reserved for storing
   annotations or descriptive textual information about the data
   contained in a file. This area is referred to as the "comment
   area" of the file. The comment area of a DAS file is a line
   oriented medium for storing textual information. The comment
   area preserves any leading or embedded white space in the line(s)
   of text which are stored so that the appearance of the
   information will be unchanged when it is retrieved (extracted) at
   some other time. Trailing blanks, however, are NOT preserved,
   due to the way that character strings are represented in
   standard Fortran 77.

   This routine will take a buffer of text lines and add (append)
   them to the comment area of a binary DAS file. If there are no
   comments in the comment area of the file, then space will be
   allocated and the text lines in buffer will then placed into the
   comment area. The text lines may contain only printable ASCII
   characters (decimal values 32 - 126).

   There is no maximum length imposed on the significant portion
   of a text line that may be placed into the comment area of a
   DAS file. The maximum length of a line stored in the comment
   area should be reasonable, however, so that they may be easily
   extracted. A good value for this would be 255 characters, as
   this can easily accommodate "screen width" lines as well as
   long lines which may contain some other form of information.

-Examples

   Let

      handle   be the handle for a DAS file which has been opened
               with write access.

      n        be the number of lines of text to be added to the
               comment area of the binary DAS file attached to
               handle.

      BUFLEN   be the declared line length of the buffer.

      buffer   is a list of text lines to be added to the comment
               area of the binary DAS file attached to handle.

   The call

      dasac_c ( handle, n, BUFLEN, buffer );

   will append the first n line(s) in buffer to the comment area
   of the binary DAS file attached to handle.

-Restrictions

   1)  This routine uses constants that are specific to the ASCII
       character sequence. The results of using this routine with
       a different character sequence are unpredictable.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)

-Version

   -CSPICE Version 1.1.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.1.0, 02-MAR-2003 (NJB)

       Added error check in wrapper for non-positive
       buffer line count.

   -CSPICE Version 1.0.0, 25-FEB-2003 (NJB) (KRG)

-Index_Entries

   add comments to a binary DAS file
   append comments to a DAS file comment area

-&
*/

{ /* Begin dasac_c */


   /*
   Local variables
   */

   SpiceChar             * fCvalsArr;

   SpiceInt                fCvalsLen;


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "dasac_c" );

   /*
   Check the line count of the input buffer.
   */
   if ( n < 1 )
   {
      setmsg_c ( "Comment buffer line count n = #; must be positive." );
      errint_c ( "#", n                                               );
      sigerr_c ( "SPICE(INVALIDARGUMENT)"                             );
      chkout_c ( "dasac_c"                                            );
      return;
   }

   /*
   Check the input buffer for null pointer or short lines.
   */
   CHKOSTR ( CHK_STANDARD, "dasac_c", buffer, buflen );


   /*
   Map the input buffer to a Fortran-style buffer.
   */
   C2F_MapStrArr ( "dasac_c", n, buflen, buffer, &fCvalsLen, &fCvalsArr );

   if ( failed_c() )
   {
      chkout_c ( "dasac_c" );
      return;
   }


   /*
   Call the f2c'd routine.
   */
   dasac_ (  ( integer    * ) &handle,
             ( integer    * ) &n,
             ( char       * ) fCvalsArr,
             ( ftnlen       ) fCvalsLen );


   /*
   Free the dynamically allocated array.
   */
   free ( fCvalsArr );


   chkout_c ( "dasac_c" );

} /* End dasac_c */
