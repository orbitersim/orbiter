/*

-Procedure dp2hx_c ( D.p. number to hexadecimal string )

-Abstract

   Convert a double precision number to an equivalent character
   string using a base 16 "scientific notation."

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

   void dp2hx_c  ( SpiceDouble         number,
                   SpiceInt            hxslen,
                   SpiceChar           hxstr  [],
                   SpiceInt          * hxssiz    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   STRLEN     P   Max number of characters allowed in output string.
   number     I   D.p. number to be converted.
   hxslen     I   Available space for output string `hxstr'.
   hxstr      O   Equivalent character string, left justified.
   hxssiz     O   Length of the character string produced.

-Detailed_Input

   number      is the double precision number to be converted to a
               character string representation.

   hxslen      is the maximum length of the output `hxstr'. The value
               defined by `hxslen' should be one plus the value large
               enough to hold any possible output.

-Detailed_Output

   hxstr       is the character string produced by this routine that
               represents `number' in a base 16 "scientific notation,"
               e.g.:

                  672.0 = "2A^3" = ( 2/16 + 10/( 16^2 ) ) * 16^3

               and

                  -11.0 = "-B^1" = - ( 11/16 ) * 16^1.

               The following table describes the character set used to
               represent the hexadecimal digits and their corresponding
               values.

                  Character    Value         Character    Value
                  ---------    ------        ---------    ------
                    "0"         0.0e0          "8"         8.0e0
                    "1"         1.0e0          "9"         9.0e0
                    "2"         2.0e0          "A"        10.0e0
                    "3"         3.0e0          "B"        11.0e0
                    "4"         4.0e0          "C"        12.0e0
                    "5"         5.0e0          "D"        13.0e0
                    "6"         6.0e0          "E"        14.0e0
                    "7"         7.0e0          "F"        15.0e0

               The caret, or hat, character, "^", is used to
               distinguish the exponent.

               The plus sign, "+", and the minus sign, "-", are used,
               and they have their usual meanings.

               In order to obtain the entire character string produced
               by this routine, the output character string should be
               at least `n' characters long, where


                           # of bits per double precision mantissa + 3
                  n = 3 + ---------------------------------------------
                                                4

                           # of bits per double precision exponent + 3
                        + ---------------------------------------------
                                                4

               There should be one character position for the sign of
               the mantissa, one for the sign of the exponent, one for
               the exponentiation character, and one for each
               hexadecimal digit that could be produced from a mantissa
               and an exponent.

               The following table contains minimum output string
               lengths necessary to obtain the complete character
               string produced by this routine for some typical
               implementations of double precision numbers.

                  Double precision number
                  Size Mantissa Exponent   Minimum output string
                  bits   bits     bits     length
                  ---- -------- --------   ----------------------
                  64   48       15         3 + 12 + 4 = 19
                  64   55+1     8          3 + 14 + 2 = 19 (VAX)
                  64   52       11         3 + 13 + 3 = 19 (IEEE)

               The base 16 "scientific notation" character string
               produced by this routine will be left justified and
               consist of a contiguous sequence of characters with one
               of the following formats:

                  (1)   h h h h  ... h ^H H  ... H
                         1 2 3 4      n  1 2      m

                  (2)   -h h h h  ... h ^H H  ... H
                          1 2 3 4      n  1 2      m

                  (3)   h h h h  ... h ^-H H  ... H
                         1 2 3 4      n   1 2      m

                  (4)   -h h h h  ... h ^-H H  ... H
                          1 2 3 4      n   1 2      m

               where

                  h   and  H   denote hexadecimal digits
                   i        j

                  "^"          denotes exponentiation ( base 16 )

               and

                  "+" and "-"  have their usual interpretations.

               The character string produced will be blank padded on
               the right if hxssiz < hxslen.

   hxssiz      is the length of the base 16 "scientific notation"
               character string produced by this routine.

-Parameters

   STRLEN      is the maximum number of characters permitted in the
               output string, excluding the null terminator character.
               The value of STRLEN is 255.

-Exceptions

   1)  If the output character string is not long enough to
       contain the entire character string that was produced,
       the string will be truncated on the right.

   2)  If hxslen > hxssiz, the output character string will be blank
       padded on the right.

   3)  If the `hxstr' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `hxstr' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   This routine converts a double precision number into an equivalent
   character string using a base 16 "scientific notation." This
   representation allows the full precision of a number to be placed
   in a format that is suitable for porting or archival storage.

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

   1) Convert a set of double precision numbers to their equivalent
      character string using a base 16 "scientific notation."


      Example code begins here.


      /.
         Program dp2hx_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants.
         ./
         #define   HXSLEN        40

         /.
         Local variables.
         ./
         SpiceChar               strval [HXSLEN];

         SpiceInt                i;
         SpiceInt                len;

         /.
         Assign an array of double precision numbers.
         ./
         SpiceDouble             number[] = { 2.0e-9,     1.0,     -1.0,
                                              1024.0, -1024.0, 521707.0,
                                                27.0,     0.0        };

         /.
         Loop over the `number' array, call dp2hx_c for each
         element of `number'.
         ./
         printf( "number       string             length\n" );
         printf( "-----------  -----------------  ------\n" );

         for ( i = 0; i < 8; i++ )
         {
            dp2hx_c ( number[i], HXSLEN, strval, &len );
            printf( "%11.4e  %17s  %d\n", number[i], strval, (int)len );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      number       string             length
      -----------  -----------------  ------
       2.0000e-09  89705F4136B4A8^-7  17
       1.0000e+00                1^1  3
      -1.0000e+00               -1^1  4
       1.0240e+03                4^3  3
      -1.0240e+03               -4^3  4
       5.2171e+05            7F5EB^5  7
       2.7000e+01               1B^2  4
       0.0000e+00                0^0  3


      Note: the hat or caret, "^", signals an exponent.

-Restrictions

   1)  The maximum number of characters permitted in the output
       string is specified by the parameter STRLEN.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 09-JUL-2021 (JDR)

       Changed the argument names "lenout", "string" and "length" to
       "hxslen", "hxstr" and "hxssiz" for consistency with other
       routines.

       Updated wrapper code to remove unnecessary chkin_c/chkout_c
       calls. Modified error tracing strategy in the "CHKOSTR" call to
       CHK_DISCOVER.

       Added the exceptions present in SPICELIB version and those derived
       from the use of "CHKOSTR".

       Edited the header to comply with NAIF standard. Added
       complete code example.

   -CSPICE Version 1.0.0, 10-APR-2010 (EDW)

-Index_Entries

   convert d.p. to signed normalized hexadecimal string
   convert d.p. number to encoded d.p. number
   convert d.p. to base 16 scientific notation

-&
*/

{ /* Begin dp2hx_c */

   /*
   Error free:  no error tracing required.
   */

   /*
   Check the output string `hxstr' to make sure the pointer is non-null
   and the string has enough room for one output character and a null
   terminator.
   */
   CHKOSTR ( CHK_DISCOVER, "dp2hx_c", hxstr, hxslen );

   /*
   Call the f2c'd Fortran routine.
   */
   dp2hx_  (  ( doublereal * ) &number,
              ( char       * )  hxstr,
              ( integer    * )  hxssiz,
              ( ftnlen       )  hxslen - 1  );

   /*
   Convert `hxstr' to a C-style string.
   */
   F2C_ConvertStr ( hxslen, hxstr );

} /* End dp2hx_c */
