/*

-Procedure hx2dp_c ( Hexadecimal string to d.p. number )

-Abstract

   Convert a string representing a double precision number in a
   base 16 "scientific notation" into its equivalent double
   precision number.

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

   ALPHANUMERIC
   CONVERSION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"
   #undef    hx2dp_c

   void hx2dp_c  ( ConstSpiceChar    * string,
                   SpiceInt            errmln,
                   SpiceDouble       * number,
                   SpiceBoolean      * error,
                   SpiceChar           errmsg [] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   MAXMAN     P   Maximum number of digits in a hex mantissa.
   string     I   Hex form string to convert to double precision.
   errmln     I   Available space for output string `errmsg'.
   number     O   Double precision value to be returned.
   error      O   A logical flag which is SPICETRUE on error.
   errmsg     O   A descriptive error message.

-Detailed_Input

   string      is a character string containing a base 16 "scientific
               notation" representation of a double precision number
               which is to be converted to a double precision number.
               Examples of such a string are:

                  "2A^3" = ( 2/16 + 10/( 16^2 ) ) * 16^3 = 672.0

               and

                  "-B^1" = - ( 11/16 ) * 16^1            = -11.0

               The following table describes the character set used to
               represent the hexadecimal digits and their corresponding
               values.

                  Character     Value         Character     Value
                  ---------    -------        ---------    -------
                     "0"         0.0e0           "8"         8.0e0
                     "1"         1.0e0           "9"         9.0e0
                     "2"         2.0e0         "A","a"      10.0e0
                     "3"         3.0e0         "B","b"      11.0e0
                     "4"         4.0e0         "C","c"      12.0e0
                     "5"         5.0e0         "D","d"      13.0e0
                     "6"         6.0e0         "E","e"      14.0e0
                     "7"         7.0e0         "F","f"      15.0e0

               The caret, or hat, character, "^", is used to
               distinguish the exponent.

               The plus sign, "+", and the minus sign, "-", are used,
               and they have their usual meanings.

               A base 16 "scientific notation" character string which
               is to be parsed by this routine should consist of a sign,
               "+" or "-" (the plus sign is optional for nonnegative
               numbers), followed immediately by a contiguous sequence
               of hexadecimal digits, the exponent character, and a
               signed hexadecimal exponent. The exponent is required,
               but the sign is optional for a nonnegative exponent.

               A number in base 16 "scientific notation" consists of
               a contiguous sequence of characters with one of the
               following formats:

                  (1)   h h h h  ... h ^H H  ... H
                         1 2 3 4      n  1 2      m

                  (2)   +h h h h  ... h ^H H  ... H
                          1 2 3 4      n  1 2      m

                  (3)   -h h h h  ... h ^H H  ... H
                          1 2 3 4      n  1 2      m

                  (4)    h h h h  ... h ^+H H  ... H
                          1 2 3 4      n   1 2      m

                  (5)   +h h h h  ... h ^+H H  ... H
                          1 2 3 4      n   1 2      m

                  (6)   -h h h h  ... h ^+H H  ... H
                          1 2 3 4      n   1 2      m

                  (7)   h h h h  ... h ^-H H  ... H
                         1 2 3 4      n   1 2      m

                  (8)   +h h h h  ... h ^-H H  ... H
                          1 2 3 4      n   1 2      m

                  (9)   -h h h h  ... h ^-H H  ... H
                          1 2 3 4      n   1 2      m

               where

                  h  and H  denote hexadecimal digits;
                   i      j

                  ^         denotes exponentiation;

               and

                  + and - have their usual interpretations.

               `string' may have leading and trailing blanks, but blanks
               embedded within the significant portion of the input
               string are not allowed.

   errmln      is the maximum length of the output `errmsg'. The value
               defined by `errmln' should be one plus the value large
               enough to hold any possible output.

-Detailed_Output

   number      is the double precision value to be returned. The value
               of this argument is not changed if an error occurs while
               parsing the input string.

   error       is a logical flag which indicates whether an error
               occurred while attempting to parse `number' from the input
               character string `string'. `error' will have the value
               SPICETRUE if an error occurs. It will have the value
               SPICEFALSE otherwise.

   errmsg      is a descriptive error message if an error occurs while
               attempting to parse the number `number' from the
               hexadecimal character string `string', blank otherwise.

-Parameters

   MAXMAN      is the maximum number of digits in a hexadecimal
               mantissa. The value of MAXMAN is 31.

               The current value of MAXMAN is more than sufficient for
               most double precision implementations, providing almost
               twice as many digits as can actually be produced. This
               value may be changed when a greater precision is known
               to exist among all of the supported platforms.

-Exceptions

   1)  If an unexpected character is encountered, an appropriate
       error message will be set, and the routine will exit. The
       value of `number' will be unchanged.

   2)  If the input string represents a number that is larger in
       absolute magnitude than the maximum representable
       double precision number an appropriate error message
       will be set, and the routine will exit. The value of
       `number' will be unchanged.

   3)  If the input string is blank, an appropriate error message
       will be set, and the routine will exit. The value of
       `number' will be unchanged.

   4)  If the string has too many digits in the mantissa, then an
       appropriate error message will be set, and the routine will
       exit. The value of `number' will be unchanged.

   5)  If the output error message string is not long enough to
       contain the entire error message, the error message will be
       truncated on the right.

   6)  This routine does NOT check for underflow errors when
       constructing a double precision number.

   7)  If the `string' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   8)  If the `string' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   9)  If the `errmsg' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   10) If the `errmsg' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   This routine will convert a character string containing a number
   in base 16 "scientific notation" into its equivalent double
   precision number.

   This routine is one of a pair of routines which are used to
   perform conversions between double precision numbers and
   an equivalent base 16 "scientific notation" character string
   representation:

         dp2hx_c  -- Convert a double precision number into a base 16
                     "scientific notation" character string.

         hx2dp_c  -- Convert a base 16 "scientific notation"
                     character string into a double precision number.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Convert a set of character strings containing a base 16
      "scientific notation" representation of a double precision
      number, to their double precision values.


      Example code begins here.


      /.
         Program hx2dp_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants.
         ./
         #define   ERRLEN        81

         /.
         Local variables.
         ./
         SpiceBoolean            error;

         SpiceChar               errmsg [ERRLEN];

         SpiceDouble             value;

         SpiceInt                i;

         /.
         Assign an array of strings representing, in base 16
         "scientific notation", double precision numbers.  Not all of
         them are valid representations.
         ./
         SpiceChar             * number[] = {
                                   "89705F4136B4A6^-7", "12357898765X34",
                                   "1^1",               "-1^1",
                                   "4^3",               "-4^3",
                                   "7F5EB^5",           "7F5eb^5",
                                   "1B^2",              "+1B^2",
                                   "+1B^+2",            "0^0",
                                   " ",                 "-AB238Z^2",
                                   "234ABC",            "234ABC^"    };

         /.
         Loop over the `number' array, call hx2dp_c for each
         element of `number'.
         ./
         printf( "string             number\n" );
         printf( "-----------------  ----------------\n" );

         for ( i = 0; i < 16; i++ )
         {
            hx2dp_c ( number[i], ERRLEN, &value, &error, errmsg );

            if ( error )
            {
               printf( "%17s  %s\n", number[i], errmsg );
            }
            else
            {
               printf( "%17s  %16.9e\n", number[i], value );
            }
         }

         /.
         Finally, try with a number that has too many digits in the
         mantissa.
         ./
         hx2dp_c ( "4ABC123AB346523BDC568798C2473678^1",
                   ERRLEN, &value, &error, errmsg );

         printf( "\n" );
         printf( "String 4ABC123AB346523BDC568798C2473678^1 produces:\n" );
         printf( "   %s\n", errmsg );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      string             number
      -----------------  ----------------
      89705F4136B4A6^-7   2.000000000e-09
         12357898765X34  ERROR: Illegal character 'X' encountered.
                    1^1   1.000000000e+00
                   -1^1  -1.000000000e+00
                    4^3   1.024000000e+03
                   -4^3  -1.024000000e+03
                7F5EB^5   5.217070000e+05
                7F5eb^5   5.217070000e+05
                   1B^2   2.700000000e+01
                  +1B^2   2.700000000e+01
                 +1B^+2   2.700000000e+01
                    0^0   0.000000000e+00
                         ERROR: A blank input string is not allowed.
              -AB238Z^2  ERROR: Illegal character 'Z' encountered.
                 234ABC  ERROR: Missing exponent.
                234ABC^  ERROR: Missing exponent.

      String 4ABC123AB346523BDC568798C2473678^1 produces:
         ERROR: Too many digits in the mantissa (> 31).


      Note: The hat or caret, '^', signals an exponent.

      Note that some errors are machine dependent. For example,
      for a VAX using D_floating arithmetic we get:

         string = "23BCE^30"
         number = ( Not defined )
         error  = SPICETRUE
         errmsg = "ERROR: Number is too large to be represented."

         string = "-2abc3^22"
         number = ( Not defined )
         error  = SPICETRUE
         errmsg = "ERROR: Number is too small to be represented."

-Restrictions

   1)  The current value of MAXMAN is more than sufficient for most
       double precision implementations, providing almost twice as
       many digits as can actually be produced.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 10-AUG-2021 (JDR)

       Changed the input argument name "lenout" to "errmln" for
       consistency with other routines.

       Updated wrapper code to remove unnecessary chkin_c/chkout_c
       calls and cast output `error' flag to SpiceBoolean.

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example. Added description of
       MAXMAN parameter to the -Brief_I/O and -Parameters sections.

   -CSPICE Version 1.0.0, 10-APR-2010 (EDW)

-Index_Entries

   convert signed normalized hexadecimal string to d.p.
   convert encoded d.p. number to d.p. number
   convert base 16 scientific notation d.p. number

-&
*/

{ /* Begin hx2dp_c */

   /*
   Local variables.
   */
   logical            logError;

   /*
   Error free:  no error tracing required.
   */

   /*
   Check the input string `string' to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_DISCOVER, "hx2dp_c", string );

   /*
   Check the output string `errmsg' to make sure the pointer is non-null
   and the string has enough room for one output character and a null
   terminator.
   */
   CHKOSTR ( CHK_DISCOVER, "hx2dp_c", errmsg, errmln );

   /*
   Call the f2c'd Fortran routine.
   */
   hx2dp_  (  ( char       * )  string,
              ( doublereal * )  number,
              ( logical    * ) &logError,
              ( char       * )  errmsg,
              ( ftnlen       )  strlen(string),
              ( ftnlen       )  errmln - 1  );

   /*
   Set the output SpiceBoolean `error' flag.
   */
   *error = (SpiceBoolean)logError;

   /*
   Convert `errmsg' to a C-style string.
   */
   F2C_ConvertStr ( errmln, errmsg );

} /* End hx2dp_c */
