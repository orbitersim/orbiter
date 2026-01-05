/*

-Procedure repmct_c  ( Replace marker with cardinal text )

-Abstract

   Replace a marker with the text representation of a
   cardinal number.

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
   CONVERSION
   STRING

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void repmct_c ( ConstSpiceChar   * in,
                   ConstSpiceChar   * marker,
                   SpiceInt           value,
                   SpiceChar          rtcase,
                   SpiceInt           outlen,
                   SpiceChar        * out      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   in         I   Input string.
   marker     I   Marker to be replaced.
   value      I   Replacement value.
   rtcase     I   Case of replacement text.
   outlen     I   Available space in output string.
   out        O   Output string.
   MAXLCN     P   is the maximum expected length of any cardinal text.

-Detailed_Input

   in          is an arbitrary character string.

   marker      is an arbitrary character string. The first occurrence of
               `marker' in the input string is to be replaced by the text
               representation of the cardinal number `value'.

               Leading and trailing blanks in `marker' are NOT
               significant. In particular, no substitution is performed
               if `marker' is blank or empty.

   value       is an arbitrary integer.

   rtcase      indicates the case of the replacement text. `rtcase' may be
               any of the following:

                  rtcase   Meaning        Example
                  ------   -----------    -----------------------
                  U, u     Uppercase      ONE HUNDRED FIFTY-THREE

                  L, l     Lowercase      one hundred fifty-three

                  C, c     Capitalized    One hundred fifty-three

   outlen      is the maximum allowed length of the output string `out'.
               This length must be large enough to hold the output string
               plus the null-terminator character. If the output string
               is expected to have N characters, `outlen' should be at
               least N+1.

-Detailed_Output

   out         is the string obtained by substituting the text
               representation of the cardinal number `value' for the first
               occurrence of `marker' in the input string.

               `out' and `in' must be identical or disjoint.

-Parameters

   MAXLCN      is the maximum expected length of any cardinal text. 145
               characters are sufficient to hold the text representing
               any value in the range

                 ( -10**12, 10**12 )

               An example of a number whose text representation is of
               maximum length is

                  - 777 777 777 777

-Exceptions

   1)  If `out' does not have sufficient length to accommodate the
       result of the substitution, the result will be truncated on
       the right.

   2)  If `marker' is blank or empty, or if `marker' is not a substring of
       `in', this routine leaves the input string unchanged, except that
       trailing blanks will be trimmed. This case is not considered an error.

   3)  If the value of `rtcase' is not recognized, the error
       SPICE(INVALIDCASE) is signaled by a routine in the call tree
       of this routine. `out' is not changed.

   4)  If any of the `in' or `marker' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   5)  If the `out' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `out' output string has length less than one character,
       the error SPICE(STRINGTOOSHORT) is signaled.

-Files

   None.

-Particulars

   This is one of a family of related routines for inserting values
   into strings. They are typically used to construct messages that
   are partly fixed, and partly determined at run time. For example,
   a message like

      "Fifty-one pictures were found in directory [USER.DATA]."

   might be constructed from the fixed string

      "#1 pictures were found in directory #2."

   by the calls

      repmct_c ( string, "#1",  51,  'c',     lenstr, string );
      repmc_c  ( string, "#2", "[USER.DATA]", lenstr, string );

   which substitute the cardinal text "Fifty-one" and the character
   string "[USER.DATA]" for the markers "#1" and "#2" respectively.

   The complete list of routines is shown below.

      repmc_c    ( Replace marker with character string value )
      repmd_c    ( Replace marker with double precision value )
      repmf_c    ( Replace marker with formatted d.p. value   )
      repmi_c    ( Replace marker with integer value          )
      repml_c    ( Replace marker with logical value          )
      repmct_c   ( Replace marker with cardinal text          )
      repmot_c   ( Replace marker with ordinal text           )

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following example illustrate the use of repmct_c to
      replace a marker within a string with the cardinal text
      corresponding to an integer.


      Example code begins here.


      /.
         Program repmct_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define STRLEN       81

         /.
         Local variables.
         ./
         SpiceChar          * instr;
         SpiceChar          * marker;
         SpiceChar            outstr [STRLEN];

         /.
         1. Uppercase
         ./
         marker = "#";
         instr  = "INVALID COMMAND. WORD # NOT RECOGNIZED.";

         repmct_c ( instr, marker, 5, 'U', STRLEN, outstr );

         printf( "Case 1: Replacement text in uppercase.\n" );
         printf( "   Input string :  %s\n", instr );
         printf( "   Output string:  %s\n", outstr );
         printf( " \n" );

         /.
         2. Lowercase
         ./
         marker = " XX ";
         instr  = "Word XX of the XX sentence was ...";

         repmct_c ( instr, marker, 5, 'L', STRLEN, outstr );

         printf( "Case 2: Replacement text in lowercase.\n" );
         printf( "   Input string :  %s\n", instr );
         printf( "   Output string:  %s\n", outstr );
         printf( " \n" );

         /.
         2. Capitalized
         ./
         marker = " XX ";
         instr  = "Name:  YY.  Rank:  XX.";

         repmc_c ( instr, "YY", "Moriarty", STRLEN, outstr );
         repmct_c ( outstr, marker, 5, 'C', STRLEN, outstr );

         printf( "Case 3: Replacement text in capitalized.\n" );
         printf( "   Input string :  %s\n", instr );
         printf( "   Output string:  %s\n", outstr );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Case 1: Replacement text in uppercase.
         Input string :  INVALID COMMAND. WORD # NOT RECOGNIZED.
         Output string:  INVALID COMMAND. WORD FIVE NOT RECOGNIZED.

      Case 2: Replacement text in lowercase.
         Input string :  Word XX of the XX sentence was ...
         Output string:  Word five of the XX sentence was ...

      Case 3: Replacement text in capitalized.
         Input string :  Name:  YY.  Rank:  XX.
         Output string:  Name:  Moriarty.  Rank:  Five.


-Restrictions

   1)  `value' must be in the range accepted by the SPICELIB routine
       INTTXT. This range is currently

          ( -10**12, 10**12 )

       Note that the endpoints of the interval are excluded.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.1.0, 04-AUG-2021 (JDR)

       Changed the input argument names "repcase" and "lenout" to
       "rtcase" and "outlen" for consistency with other routines.

       Updated error handling strategy to Standard, as the f2c'ed
       routine might signal errors.

       Edited the header to comply with NAIF standard. Added complete
       code example from existing fragments.

   -CSPICE Version 1.0.0, 14-AUG-2002 (NJB) (IMU)

-Index_Entries

   replace marker with cardinal text

-&
*/

{ /* Begin repmct_c */

   /*
   Local variables
   */
   ConstSpiceChar        * markPtr;


   /*
   Participate in error tracing.
   */
   chkin_c ( "repmct_c" );

   /*
   Make sure no string argument pointers are null.
   */
   CHKPTR( CHK_STANDARD, "repmct_c", in     );
   CHKPTR( CHK_STANDARD, "repmct_c", marker );
   CHKPTR( CHK_STANDARD, "repmct_c", out    );


   /*
   If the output string can't hold a terminating null character,
   we can't proceed.
   */
   if ( outlen < 1 )
   {
      setmsg_c ( "String length outlen must be >= 1; actual "
                 "value = #."                                 );
      errint_c ( "#", outlen                                  );
      sigerr_c ( "SPICE(STRINGTOOSHORT)"                      );
      chkout_c ( "repmct_c"                                   );
      return;
   }


   /*
   If the output string has no room for data characters, we simply
   terminate the string.
   */
   if ( outlen == 1 )
   {
      out[0] = NULLCHAR;
      chkout_c ( "repmct_c" );

      return;
   }


   /*
   If the input string has zero length, the output is empty as well.
   */
   if ( in[0] == NULLCHAR )
   {
      out[0] = NULLCHAR;
      chkout_c ( "repmct_c" );

      return;
   }


   /*
   If the marker is empty, pass a blank marker to the f2c'd routine.
   Otherwise, pass in the marker.
   */
   if ( marker[0] == NULLCHAR )
   {
      markPtr = " ";
   }
   else
   {
      markPtr = marker;
   }

   /*
   Simply call the f2c'd routine.
   */
   repmct_ ( ( char     * ) in,
             ( char     * ) markPtr,
             ( integer  * ) &value,
             ( char     * ) &rtcase,
             ( char     * ) out,
             ( ftnlen     ) strlen(in),
             ( ftnlen     ) strlen(markPtr),
             ( ftnlen     ) 1,
             ( ftnlen     ) outlen-1         );

   /*
   Convert the output string from Fortran to C style.
   */
   F2C_ConvertStr ( outlen, out );

   chkout_c ( "repmct_c" );

} /* End repmct_c */
