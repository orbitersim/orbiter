/*

-Procedure repml_c ( Replace marker with logical value text )

-Abstract

   Replace a marker with the text representation of a logical value.

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
   #include <stdio.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"

   void repml_c  ( ConstSpiceChar    * in,
                   ConstSpiceChar    * marker,
                   SpiceBoolean        value,
                   SpiceChar           rtcase,
                   SpiceInt            outlen,
                   SpiceChar         * out    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   in         I   Input string.
   marker     I   Marker to be replaced.
   value      I   Replacement logical value.
   rtcase     I   Case of replacement text.
   outlen     I   Maximum length of output string `out'.
   out        O   Output string.

-Detailed_Input

   in          is an arbitrary character string.

   marker      is an arbitrary character string. The first occurrence
               of `marker' in the input string is to be replaced by
               `value'.

               `marker' is case-sensitive.

               Leading and trailing blanks in `marker' are NOT
               significant. In particular, no substitution is
               performed if `marker' is blank or empty.

   value       is an arbitrary logical value, either SPICETRUE or
               SPICEFALSE.

   rtcase      indicates the case of the replacement text. `rtcase' may
               be any of the following:

                  rtcase    Meaning        Output values
                  ------    -----------    ---------------
                  U, u      Uppercase      "TRUE", "FALSE"

                  L, l      Lowercase      "true", "false"

                  C, c      Capitalized    "True", "False"

   outlen      is the maximum allowed length of the output string `out'.
               This length must be large enough to hold the output string
               plus the null-terminator character. If the output string
               is expected to have N characters, `outlen' should be at
               least N+1.

-Detailed_Output

   out         is the string obtained by substituting the text
               representation of `value' for the first occurrence of
               `marker' in the input string.

               `out' and `in' must be disjoint.

-Parameters

   None.

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

   6)  If the `out' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

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

      repmct_c ( string, "#1",       51, "C", LENTMP, tmpstr );
      repmc_c  ( tmpstr, "#2", "[USER.DATA]", LENSTR, string );

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

   1) The following example illustrates the use of repml_c to replace
      a marker within a string with the text representation of a
      logical value.


      Example code begins here.


      /.
         Program repml_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define STRLEN       81

         /.
         Local variables.
         ./
         SpiceChar            instr  [STRLEN];
         SpiceChar            marker [STRLEN];
         SpiceChar            outstr [STRLEN];

         /.
         1. Uppercase
         ./
         strncpy( marker, "#", 2 );
         strncpy( instr, "Invalid value. The value was:  #.", 34 );

         repml_c ( instr, marker, SPICEFALSE, 'U', STRLEN, outstr );

         printf( "Case 1: Replacement text in uppercase.\n" );
         printf( "   Input : %s\n", instr );
         printf( "   Output: %s\n", outstr );
         printf( "\n" );

         /.
         2. Lowercase
         ./
         strncpy( marker, " XX ", 5 );
         strncpy( instr, "Invalid value. The value was:  XX.", 35 );

         repml_c ( instr, marker, SPICETRUE, 'l', STRLEN, outstr );

         printf( "Case 2: Replacement text in lowercase.\n" );
         printf( "   Input : %s\n", instr );
         printf( "   Output: %s\n", outstr );
         printf( "\n" );

         /.
         2. Capitalized
         ./
         strncpy( marker, "#", 2 );
         strncpy( instr, "Invalid value. The value was:  #.", 34 );

         repml_c ( instr, marker, SPICEFALSE, 'c', STRLEN, outstr );

         printf( "Case 3: Replacement text capitalized.\n" );
         printf( "   Input : %s\n", instr );
         printf( "   Output: %s\n", outstr );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Case 1: Replacement text in uppercase.
         Input : Invalid value. The value was:  #.
         Output: Invalid value. The value was:  FALSE.

      Case 2: Replacement text in lowercase.
         Input : Invalid value. The value was:  XX.
         Output: Invalid value. The value was:  true.

      Case 3: Replacement text capitalized.
         Input : Invalid value. The value was:  #.
         Output: Invalid value. The value was:  False.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 04-AUG-2021 (JDR)

-Index_Entries

   replace marker with logical value

-&
*/

{ /* Begin repml_c */

   /*
   Local variables.
   */
   logical            logCvalue;
   ConstSpiceChar   * markerPtr;

   /*
   Participate in error tracing.
   */
   chkin_c ( "repml_c" );

   /*
   Check the output string `out' to make sure the pointer is non-null
   and the string has enough room for one output character and a null
   terminator.  Unlike most CSPICE wrappers, repml_c must check this
   before checking the inputs because there's a special case that results
   in returning right after the input checks are completed.
   */
   CHKOSTR ( CHK_STANDARD, "repml_c", out, outlen );

   /*
   Special case: `in' is empty. Verify that `in' is not a null
   pointer first.
   */
   CHKPTR ( CHK_STANDARD, "repml_c", in );

   if ( in[0] == NULLCHAR )
   {
      out[0] = NULLCHAR;
      chkout_c ( "repml_c" );

      return;
   }

   /*
   Special case. If `marker' is empty, pass a blank string to the f2c'd
   routine. Otherwise, pass in `marker'. Verify that `marker' is not a
   null pointer first.
   */
   CHKPTR ( CHK_STANDARD, "repml_c", marker );

   if ( marker[0] == NULLCHAR )
   {
      markerPtr = " ";
   }
   else
   {
      markerPtr = marker;
   }

   /*
   Get a type logical copy of the `value' flag.
   */
   logCvalue = value;

   /*
   Call the f2c'd Fortran routine.
   */
   repml_  (  ( char       * )  in,
              ( char       * )  markerPtr,
              ( logical    * ) &logCvalue,
              ( char       * ) &rtcase,
              ( char       * )  out,
              ( ftnlen       )  strlen(in),
              ( ftnlen       )  strlen(markerPtr),
              ( ftnlen       )  1,
              ( ftnlen       )  outlen - 1        );

   /*
   Convert `out' to a C-style string.
   */
   F2C_ConvertStr ( outlen, out );

   chkout_c ( "repml_c" );

} /* End repml_c */
