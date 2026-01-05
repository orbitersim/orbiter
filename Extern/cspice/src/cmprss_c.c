/*

-Procedure cmprss_c ( Compress a character string )

-Abstract

   Compress a character string by removing occurrences of
   more than N consecutive occurrences of a specified
   character.

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

   ASCII
   CHARACTER
   STRING

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"


   void cmprss_c ( SpiceChar          delim,
                   SpiceInt           n,
                   ConstSpiceChar   * input,
                   SpiceInt           outlen,
                   SpiceChar        * output  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   delim      I   Delimiter to be compressed.
   n          I   Maximum consecutive occurrences of delim.
   input      I   Input string.
   outlen     I   Available space in output string.
   output     O   Compressed string.

-Detailed_Input

   delim       is the delimiter to be compressed out of the string.
               This may be any ASCII character.

   n           is the maximum number of consecutive occurrences
               of delim that will be allowed to remain in the
               output string.

   input       is the input string.

   outlen      is the available space in the output string, counting
               the terminating null. `outlen' is typically the
               declared length of the output string.

-Detailed_Output

   output      is the output string. This is the input string with all
               occurrences of more than N consecutive delimiters
               removed. A null terminator will follow the last
               character of the string resulting from the compression.

               If `output' is not large enough to hold the compressed
               string, it is truncated on the right. The output string
               will always be null-terminated.

               `output' may overwrite `input'.

-Parameters

   None.

-Exceptions

   1)  If the output string length is too short to contain the result
       of compressing the input string, the result is truncated on
       the right. The output string is still null-terminated.

   2)  If the `input' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `output' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `output' output string has length less than one
       character, the error SPICE(STRINGTOOSHORT) is signaled.

-Files

   None.

-Particulars

   Occurrences of more than `n' consecutive delimiters are removed
   from the input string as it is copied to the output string.
   If the output string is not large enough to hold the compressed
   string, it is truncated on the right.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Remove multiple occurrences of a character in different strings.
      As example, compress the occurrences of '.' down to two periods
      (..), three periods (...) or just one period (.). Also, as a
      practical example, remove trailing, leading and embedded spaces
      from an input string.

      Example code begins here.


      /.
         Program cmprss_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants.
         ./
         #define   STRLEN        40

         /.
         Local variables.
         ./
         SpiceChar               output [ STRLEN ];

         /.
         Assign an array of input character strings to be compressed.
         ./
         SpiceChar             * strings[] = {
                                   "ABC...DE.F...",
                                   "...........",
                                   ".. ..AB....CD",
                                   " Embe dde d -sp   a c  es   " };

         /.
         Compress multiple occurrences of '.'
         in the strings array. Compress to
         two periods...
         ./
         cmprss_c ( '.', 2, strings[0], STRLEN, output );
         printf ( "Input : '%s'\n",   strings[0] );
         printf ( "Output: '%s'\n\n", output     );

         /.
         ...three periods...
         ./
         cmprss_c ( '.', 3, strings[1], STRLEN, output );
         printf ( "Input : '%s'\n",   strings[1] );
         printf ( "Output: '%s'\n\n", output     );

         /.
         ...one period.
         ./
         cmprss_c ( '.', 1, strings[2], STRLEN, output );
         printf ( "Input : '%s'\n",   strings[2] );
         printf ( "Output: '%s'\n\n", output     );

         /.
         Use the call to remove trailing, leading, and
         embedded spaces.
         ./
         cmprss_c ( ' ', 0, strings[3], STRLEN, output );
         printf ( "Input : '%s'\n",   strings[3] );
         printf ( "Output: '%s'\n", output     );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input : 'ABC...DE.F...'
      Output: 'ABC..DE.F..'

      Input : '...........'
      Output: '...'

      Input : '.. ..AB....CD'
      Output: '. .AB.CD'

      Input : ' Embe dde d -sp   a c  es   '
      Output: 'Embedded-spaces'


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.1.0, 09-JUL-2021 (JDR)

       Changed the input argument name "lenout" to "outlen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added
       complete code example.

   -CSPICE Version 1.0.0, 20-AUG-1999 (WLT) (IMU) (NJB)

-Index_Entries

   compress a character_string

-&
*/

{ /* Begin cmprss_c */


   /*
   Local variables
   */
   SpiceBoolean            isDelim;

   SpiceInt                iPtr;
   SpiceInt                inLen;
   SpiceInt                nConsec;
   SpiceInt                oPtr;
   SpiceInt                outLen;


   /*
   Use discovery check-in.
   */


   /*
   Check for null input and output pointers.
   */
   CHKPTR( CHK_DISCOVER, "cmprss_c", input  );
   CHKPTR( CHK_DISCOVER, "cmprss_c", output );


   /*
   Make sure there is at least room for a null terminator in the output
   string.
   */

   if ( outlen < 1 )
   {
      chkin_c  ( "cmprss_c"                                           );
      setmsg_c ( "Output string must have length at least 1; outlen "
                 "= #"                                                );
      errint_c ( "#",  outlen                                         );
      sigerr_c ( "SPICE(STRINGTOOSHORT)"                              );
      chkout_c ( "cmprss_c"                                           );
      return;
   }


   /*
   Save the lengths of the input and output strings.  Initialize the
   input and output string pointers.  Initialize the consecutive
   delimiter count.
   */
   inLen   =  strlen ( input );
   outLen  =  outlen - 1;

   iPtr    =  0;
   oPtr    =  0;
   nConsec =  0;


   while (  ( iPtr < inLen )  &&  ( oPtr < outLen )  )
   {

      isDelim   =   ( input[iPtr] == delim );


      if (  ( !isDelim )  ||  ( nConsec < n )  )
      {
         /*
         As long as we're not looking at the nth (or greater)
         consecutive delimiter character, we transfer the character.
         */
         output[oPtr] = input[iPtr];
         oPtr ++;


         /*
         If we ARE looking at a delimiter, increment the delimiter
         count.  Otherwise set the count to zero.  This results in
         zeroing out the count for every non-delimiter, but that's as
         efficient as anything else.
         */
         if ( isDelim )
         {
            nConsec ++;
         }
         else
         {
            nConsec = 0;
         }
      }


      /*
      We don't bother incrementing the delimiter count once we've seen
      n consecutive delimiters.  The exact number seen doesn't matter
      after this point.

      Look at the next input character.
      */
      iPtr ++;
   }


   /*
   We're done transferring characters.  Whether we ran out of input or
   output room, we're ready to null-terminate the output string.  We've
   saved a space for the null character.
   */
   output[oPtr] = NULLCHAR;


} /* End cmprss_c */
