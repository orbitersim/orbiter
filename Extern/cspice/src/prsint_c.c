/*

-Procedure prsint_c   ( Parse integer with error checking )

-Abstract

   Parse a string as an integer, encapsulating error handling.

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

   INTEGER
   PARSING

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void prsint_c ( ConstSpiceChar  * string,
                   SpiceInt        * intval )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   string     I   String representing a numeric value.
   intval     O   Integer value obtained by parsing `string'.

-Detailed_Input

   string      is a string representing a numeric value. Commas and
               spaces may be used in this string for ease of reading
               and writing the number. They are treated as
               insignificant but non-error-producing characters.

               For exponential representation any of the characters
               "E","D","e","d" may be used.

               The following are legitimate numeric expressions

                  +12.2 e-1
                  -3. 1415 9276
                  1e6
                  E8

               The program also recognizes the following  mnemonics

                  "PI",  "pi",  "Pi",  "pI"
                  "+PI", "+pi", "+Pi", "+pI"
                  "-PI", "-pi", "-Pi", "-pI"

               and returns the value ( + OR - ) 3 as appropriate.

-Detailed_Output

   intval      is the integer obtained by parsing `string'. If an error is
               encountered, `intval' is not changed from whatever the
               input value was. If the input string has a fractional
               part, the fractional part will be truncated. Thus
               3.18 is interpreted as 3. -4.98 is interpreted as -4.

-Parameters

   None.

-Exceptions

   1)  If the input string cannot be parsed or if the string
       represents a number that is outside the range of representable
       integers, as defined by intmin_c and intmax_c, the error
       SPICE(NOTANINTEGER) is signaled by a routine in the call tree
       of this routine. The value of `intval' is not changed from
       whatever the input value was.

   2)  If the `string' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `string' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   The purpose of this routine is to enable safe parsing of numeric
   values into a SpiceInt variable without the necessity of in-line
   error checking.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Parse into a SpiceInt variable a set of strings representing
      numeric values.


      Example code begins here.


      /.
         Program prsint_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define SETSIZ       10
         #define STRLEN       12

         /.
         Local variables.
         ./
         SpiceInt             i;
         SpiceInt             intval;

         /.
         Initialize the array of strings.
         ./
         SpiceChar            strval [SETSIZ][STRLEN] = {
                                   "100,000,000", " -2 690 192",
                                   "  +12.2 e-1", "-3. 141 592",
                                   "      1.2e8", "         E6",
                                   "         Pi", "        -PI",
                                   "-2147483648", " 2147483647" };

         /.
         Parse each string into an INTEGER variable.
         ./
         printf( "   STRVAL       INTVAL\n"    );
         printf( "-----------  ------------\n" );
         for ( i = 0; i < SETSIZ; i++ )
         {
            prsint_c ( strval[i], &intval );

            printf( "%-11s   %11d\n", strval[i], (int)intval );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


         STRVAL       INTVAL
      -----------  ------------
      100,000,000     100000000
       -2 690 192      -2690192
        +12.2 e-1             1
      -3. 141 592            -3
            1.2e8     120000000
               E6       1000000
               Pi             3
              -PI            -3
      -2147483648   -2147483648
       2147483647    2147483647


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.2, 04-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Updated the header to properly describe its input, output,
       exceptions and particulars.

   -CSPICE Version 1.1.1, 26-AUG-1999 (NJB)

       Header was updated to list string exceptions.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

-Index_Entries

   parse integer with encapsulated error handling

-&
*/

{ /* Begin prsint_c */

   /*
   Participate in error handling.
   */
   chkin_c ( "prsint_c");


   /*
   Check the input string to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "prsint_c", string );


   prsint_ ( ( char      * ) string,
             ( integer   * ) intval,
             ( ftnlen      ) strlen(string) );


   chkout_c ( "prsint_c");

} /* End prsint_c */
