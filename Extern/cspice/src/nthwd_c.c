/*

-Procedure nthwd_c ( n'th word in a character string )

-Abstract

   Return the nth word in a character string, and its location
   in the string.

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
   SEARCH
   WORD

*/
   #include <stdio.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"

   void nthwd_c  ( ConstSpiceChar    * string,
                   SpiceInt            nth,
                   SpiceInt            worlen,
                   SpiceChar         * word,
                   SpiceInt          * loc    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   string     I   Input character string.
   nth        I   Index of the word to be returned.
   worlen     I   Maximum length of output string `word'.
   word       O   The `nth' word in `string'.
   loc        O   Location of `word' in `string'.

-Detailed_Input

   string      is a null-terminated string to be parsed. Each word of this
               string is a maximal sequence of consecutive non-blank, non-null
               characters.

   nth         is the index of the word to be returned. (Zero for the first
               word, one for the second, and so on.)

   worlen      is the maximum allowed length of the output string `word'.
               This length must be large enough to hold the output string
               plus the null-terminator character. If the output string
               is expected to have N characters, `worlen' should be at
               least N+1.

-Detailed_Output

   word        is the n'th word in `string'. If `string' is blank or empty,
               or `nth' is non-positive or too large, `word' is empty.

               If the n'th word is longer than worlen-1 characters, it will be
               truncated on the right.

               `word' may overwrite `string'.

   loc         is the location of `word' in `string'. (That is, `word'
               begins at string[loc]). If `string' is blank or empty, or
               `nth' is nonpositive or too large, `loc' is -1.

-Parameters

   None.

-Exceptions

   1)  If the declared length of `word' is not large enough to contain
       the `nth' word in `string', the word will be truncated on the
       right.

   2)  If the `string' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `word' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `word' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   nthwd_c, like nextwd_c, is useful primarily for parsing input commands
   consisting of one or more words, where a word is defined to be a
   maximal sequence of consecutive non-blank, non-null characters. Each word is
   bounded on both sides by a blank character, or by the start or end
   of the input string. Successive calls to nextwd_c allow the calling
   routine to neatly parse and process one word at a time.

   The chief difference between the two routines is that
   nthwd_c allows the calling routine to access the words making
   up the input string in random order. (nextwd_c allows only
   sequential access.)

   nthwd_c may be more efficient than nextwd_c, since nthwd_c doesn't
   update an output string consisting of the remaining, unparsed
   string.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given a character string, get the n'th word within, and the
      word's location.


      Example code begins here.


      /.
         Program nthwd_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define STRING       " Now is the time,   for all good men     to " \
                              "come."
         #define WRDSZ        6

         /.
         Local variables.
         ./
         SpiceChar            word   [WRDSZ];
         SpiceInt             loc;
         SpiceInt             nth;

         for ( nth = -1; nth <= 11; nth++ )
         {
            nthwd_c ( STRING, nth-1, WRDSZ, word, &loc );
            printf( "Word # %2d  is <%s>, starting at position %2d\n",
                                             (int)nth, word, (int)loc );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Word # -1  is <>, starting at position -1
      Word #  0  is <>, starting at position -1
      Word #  1  is <Now>, starting at position  1
      Word #  2  is <is>, starting at position  5
      Word #  3  is <the>, starting at position  8
      Word #  4  is <time,>, starting at position 12
      Word #  5  is <for>, starting at position 20
      Word #  6  is <all>, starting at position 24
      Word #  7  is <good>, starting at position 28
      Word #  8  is <men>, starting at position 33
      Word #  9  is <to>, starting at position 41
      Word # 10  is <come.>, starting at position 44
      Word # 11  is <>, starting at position -1


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 01-NOV-2021 (JDR)

-Index_Entries

   n'th word in a character_string

-&
*/

{ /* Begin nthwd_c */

   /*
   Local variables.
   */
   SpiceInt           nthFidx;

   /*
   Discovery error tracing. No check-in required.
   */

   /*
   Check the output string `word' to make sure the pointer is non-null
   and the string has enough room for one output character and a null
   terminator.  Unlike most CSPICE wrappers, nthwd_c must check this
   before checking the inputs because there's a special case that results
   in returning right after the input checks are completed.
   */
   CHKOSTR ( CHK_DISCOVER, "nthwd_c", word, worlen );

   /*
   Special case: `string' is empty. Verify that `string' is not a null
   pointer first.
   */
   CHKPTR ( CHK_DISCOVER, "nthwd_c", string );

   if ( string[0] == NULLCHAR )
   {
      word[0] = NULLCHAR;
      *loc    = -1;

      return;
   }

   /*
   Convert `nth' to a Fortran-style index.
   */
   nthFidx = nth + 1;

   /*
   Call the f2c'd Fortran routine.
   */
   nthwd_  (  ( char       * )  string,
              ( integer    * ) &nthFidx,
              ( char       * )  word,
              ( integer    * )  loc,
              ( ftnlen       )  strlen(string),
              ( ftnlen       )  worlen - 1     );

   /*
   Convert `loc' to a C-style index.
   */
   (*loc)--;

   /*
   Convert `word' to a C-style string.
   */
   F2C_ConvertStr ( worlen, word );

} /* End nthwd_c */
