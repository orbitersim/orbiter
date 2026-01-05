/*

-Procedure lxqstr_c ( Lex quoted string )

-Abstract

   Scan (lex) a quoted string.

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
   SCANNING
   STRING
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void lxqstr_c ( ConstSpiceChar    * string,
                   SpiceChar           qchar,
                   SpiceInt            first,
                   SpiceInt          * last,
                   SpiceInt          * nchar  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   string     I   String to be scanned.
   qchar      I   Quote delimiter character.
   first      I   Character position at which to start scanning.
   last       O   Character position of end of token.
   nchar      O   Number of characters in token.

-Detailed_Input

   string      is a character string that may contain a "string
               token" starting at the character position
               indicated by the input argument first (see below).
               String tokens are sequences of characters that
               represent literal strings. Syntactically, a string
               token is a sequence of characters that begins and
               ends with a designated "quote character".  Within
               the token, any occurrence of the quote character
               is indicated by an adjacent pair of quote
               characters: for example, if the quote character is

                  "

               then the token representing one instance of this
               character is

                  """"

               Here the first quote indicates the beginning of the
               token, the next two quotes together indicate a
               single quote character that constitutes the
               "contents" of the token, and the final quote
               indicates the end of the token.

   qchar       is the quote character. This is always a single
               character. The characters

                  "  and '

               are common choices, but any non-blank character is
               accepted. Case *is* significant in qchar.

   first       is the character position at which the routine
               is to start scanning a quoted string token. Note
               that the character string[first] must equal
               `qchar' if a string token is to be found; this
               routine does *not* attempt to locate the first
               quoted string following the position first.

-Detailed_Output

   last        is the last character position such that the subtring
               ranging from string[first] to string[last] is a
               quoted string token, if such a substring exists.
               Otherwise, the returned value of `last' is first-1.

   nchar       is the length of the string token found by this
               routine, if such a token exists. This length
               includes the starting and ending bracketing quotes.
               If a string token is not found, the returned value
               of `nchar' is zero.

-Parameters

   None.

-Exceptions

   1)  If the input argument `first' is less than 0 or greater than
       strlen(string)-1, the returned value of `last' is first-1, and the
       returned value of `nchar' is zero.

   2)  It is not an error for a quoted string token to consist of
       two consecutive quote characters with no intervening
       characters. Calling routines that require special treatment
       of null tokens must handle this case.

   3)  If the input argument `qchar' is blank, the returned value
       of `last' is first-1, and the returned value of `nchar' is zero.

   4)  If the `string' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the input string has length zero, `last' will be set to first-1
       and `nchar' will be set to zero. This case is not considered an
       error.

-Files

   None.

-Particulars

   Quote characters may be ANY non-blank character. For example, the
   ampersand

      &

   is a perfectly valid quote character. If we were using the
   ampersand as the quote character, then the term "doubled quote"
   in the following discussion would refer to the sequence

      &&

   not the character

      "

   The string tokens identified by this routine are Fortran-style
   quoted strings: they start and end with quote characters. In the
   interior of any such token, any quote characters are represented
   by doubled quote characters. These rules imply that the number of
   quote characters in a quoted string token is always even. The end
   of a quoted string token is located at the first even-numbered
   quote character, counting from the initial quote character, that
   is  not the first member of a pair of quotes indicating an
   embedded quote character.

   To map the token to the string of characters it represents, use
   the CSPICE subroutine parsqs_c (String parse, quoted).  parsqs_c
   removes the bracketing quotes from a quoted string token and
   converts each doubled quote between the bracketing quotes to a
   single quote. For example, the token

      """"

   identified by this routine would be mapped by parsqs_c to a string
   variable containing the single character

      "

-Examples

   1)  The table below illustrates the action of this routine.


       STRING CONTENTS               qchar   first   last   nchar
       ==========================================================
       The "SPICE" system            "       4       10      7
       The "SPICE" system            "       0       -1      0
       The "SPICE" system            '       4        3      0
       The """SPICE"" system"        "       4       12      9
       The """SPICE"""" system       "       4       14     11
       The &&&SPICE system           &       4        5      2
       ' '                           '       0        2      3
       ''                            '       0        1      2
       ==========================================================
       01234567890123456789012

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 04-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 19-AUG-2002 (NJB)

-Index_Entries

   scan quoted string token
   lex quoted string token
   recognize quoted string token

-&
*/

{ /* Begin lxqstr_c */

   /*
   Local variables
   */
   SpiceInt                locFirst;
   SpiceInt                len;


   /*
   Use discovery check-in.

   Check the input string argument for a null pointer.
   */
   CHKPTR ( CHK_DISCOVER, "lxqstr_c", string );


   /*
   We're done if the input string has zero length.
   */
   len = strlen(string);

   if ( len == 0 )
   {
      *last  = first - 1;
      *nchar = 0;

      return;
   }


   /*
   Map first to a Fortran-style index.
   */
   locFirst = first + 1;


   /*
   Call the f2c'd routine.
   */
   lxqstr_ ( ( char    * ) string,
             ( char    * ) &qchar,
             ( integer * ) &locFirst,
             ( integer * ) last,
             ( integer * ) nchar,
             ( ftnlen    ) len,
             ( ftnlen    ) 1         );

   /*
   Map last to a C-style index.
   */

   (*last)--;


} /* End lxqstr_c */
