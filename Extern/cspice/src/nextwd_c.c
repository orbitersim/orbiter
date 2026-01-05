/*

-Procedure nextwd_c ( Next word in a character string )

-Abstract

   Return the next word in a given character string, and
   left justify the rest of the string.

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

   CHARACTER
   PARSING
   WORD

*/
   #include <stdio.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"

   void nextwd_c ( ConstSpiceChar    * string,
                   SpiceInt            nexlen,
                   SpiceInt            reslen,
                   SpiceChar         * next,
                   SpiceChar         * rest   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   string     I   Input character string.
   nexlen     I   Maximum length of output string `next'.
   reslen     I   Maximum length of output string `rest'.
   next       O   The next word in the string.
   rest       O   The remaining part of `string', left-justified.

-Detailed_Input

   string      is a null-terminated string to be parsed. Each word of this
               string is a maximal sequence of consecutive non-blank, non-null
               characters.

   nexlen      is the maximum allowed length of the output string `next'.
               This length must be large enough to hold the output string
               plus the null-terminator character. If the output string
               is expected to have M characters, `nexlen' should be at
               least M+1.

   reslen      is the maximum allowed length of the output string `rest'.
               This length must be large enough to hold the output string
               plus the null-terminator character. If the output string
               is expected to have N characters, `reslen' should be at
               least N+1.

-Detailed_Output

   next        is the first word in `string'. It is called the "next" word
               because nextwd_c is typically called repeatedly to find the
               words of the input string in left-to-right order. A word
               is a maximal sequence of consecutive non-blank, non-null
               characters. `next' is always returned left-justified.

               If `string' is blank or empty, `next' is empty.

               If the first word in `string' is longer than nexlen-1
               characters, it will be truncated on the right.

               `next' may NOT overwrite `string'.

   rest        is the remaining part of `string', left-justified after the
               removal of `next'.

               If `string' is empty, `rest' is empty.

               If the remaining part of `string' is longer than reslen-1
               characters, it will be truncated on the right.

               `rest' may overwrite `string'.

-Parameters

   None.

-Exceptions

   1)  If the declared lengths of `next' and `rest' are not large enough
       to hold the output strings, they are truncated on the right.

   2)  If the `string' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If any of the `next' or `rest' output string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   4)  If any of the `next' or `rest' output strings has length less
       than two characters, the error SPICE(STRINGTOOSHORT) is
       signaled, since the output string is too short to contain one
       character of output data plus a null terminator.

-Files

   None.

-Particulars

   nextwd_c is used primarily for parsing input commands consisting
   of one or more words, where a word is defined to be any sequence
   of consecutive non-blank characters. Successive calls to nextwd_c,
   each using the previous value of `rest' as the input string, allow
   the calling routine to neatly parse and process one word at a
   time.

   nextwd_c cuts the input string into two pieces, and returns them
   separately. The first piece is the first word in the string.
   (Leading blanks are ignored. The first word, which is returned in
   the output argument `next', runs from the first non-blank character
   in the string up to the first blank that follows it.) The second
   piece is whatever is left after the first word is removed. The
   second piece is left justified, to simplify later calls to nextwd_c.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given a character string, get the sequence of words within.


      Example code begins here.


      /.
         Program nextwd_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define LINZS        48
         #define WRDSZ        6

         /.
         Local variables.
         ./
         SpiceChar            next   [WRDSZ];
         SpiceChar            rest   [LINZS];
         SpiceChar            string [LINZS];

         strncpy( rest, "  Now is the time,  for all good men   to come.",
                  48 );

         printf( "Next   Rest of the string\n" );
         printf( "-----  ------------------------------------------\n" );

         while ( ! eqstr_c ( rest, "" ) )
         {

            strncpy( string, rest, LINZS );
            nextwd_c ( string, WRDSZ, LINZS, next, rest );

            printf( "%-5s  %s\n", next, rest );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Next   Rest of the string
      -----  ------------------------------------------
      Now    is the time,  for all good men   to come.
      is     the time,  for all good men   to come.
      the    time,  for all good men   to come.
      time,  for all good men   to come.
      for    all good men   to come.
      all    good men   to come.
      good   men   to come.
      men    to come.
      to     come.
      come.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 04-AUG-2021 (JDR)

-Index_Entries

   next word in a character_string

-&
*/

{ /* Begin nextwd_c */

   /*
   Discovery error tracing. No check-in required.
   */

   /*
   Check the output string arguments:

      next
      rest

   Make sure each pointer is non-null and each string has enough room
   for one output character and a null terminator. Unlike most CSPICE
   wrappers, nextwd_c must check this before checking the inputs because
   there's a special case that results in returning right after then input
   checks are completed.
   */
   CHKOSTR ( CHK_DISCOVER, "nextwd_c", next, nexlen );
   CHKOSTR ( CHK_DISCOVER, "nextwd_c", rest, reslen );

   /*
   Special case: `string' is empty. Verify that `string' is not a null
   pointer first.
   */
   CHKPTR ( CHK_DISCOVER, "nextwd_c", string );

   if ( string[0] == NULLCHAR )
   {
      next[0] = NULLCHAR;
      rest[0] = NULLCHAR;

      return;
   }

   /*
   Call the f2c'd Fortran routine.
   */
   nextwd_ (  ( char       * )  string,
              ( char       * )  next,
              ( char       * )  rest,
              ( ftnlen       )  strlen(string),
              ( ftnlen       )  nexlen - 1,
              ( ftnlen       )  reslen - 1     );

   /*
   Convert

      next
      rest

   to C-style strings.
   */
   F2C_ConvertStr ( nexlen, next );
   F2C_ConvertStr ( reslen, rest );

} /* End nextwd_c */
