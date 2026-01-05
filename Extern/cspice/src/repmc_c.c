/*

-Procedure repmc_c  ( Replace marker with character string )

-Abstract

   Replace a marker with a character string.

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


   void repmc_c ( ConstSpiceChar    * in,
                  ConstSpiceChar    * marker,
                  ConstSpiceChar    * value,
                  SpiceInt            outlen,
                  SpiceChar         * out    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   in         I   Input string.
   marker     I   Marker to be replaced.
   value      I   Replacement string.
   outlen     I   Available space in output string.
   out        O   Output string.

-Detailed_Input

   in          is an arbitrary character string.

   marker      is an arbitrary character string. The first occurrence of
               `marker' in the input string is to be replaced by `value'.

               Leading and trailing blanks in `marker' are NOT
               significant. In particular, no substitution is performed
               if `marker' is blank or empty.

   value       is the replacement character string.

               Leading and trailing blanks in `value' are NOT significant:
               the portion of `value' that is substituted for `marker'
               extends from its first non-blank character to its last
               non-blank character.

               However, if `value' is blank or empty, a single blank is
               substituted for the first occurrence of `marker'.

   outlen      is the maximum allowed length of the output string `out'.
               This length must be large enough to hold the output string
               plus the null-terminator character. If the output string
               is expected to have N characters, `outlen' should be at
               least N+1.

-Detailed_Output

   out         is the string obtained by substituting `value' (leading and
               trailing blanks excepted) for the first occurrence of
               `marker' in the input string.

               `out' and `in' must be identical or disjoint.

-Parameters

   None.

-Exceptions

   1)  If `out' does not have sufficient length to accommodate the
       result of the substitution, the result will be truncated on
       the right.

   2)  If `marker' is blank or empty, or if `marker' is not a substring of
       `in', this routine leaves the input string unchanged, except that
       trailing blanks will be trimmed. This case is not considered an error.

   3)  If `value' is blank or empty, a single blank is substituted for the
       first occurrence of `marker'.

   4)  If any of the `in', `marker' or `value' input string pointers
       is null, the error SPICE(NULLPOINTER) is signaled.

   5)  If the `out' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `out' output string has length less than one character,
       the error SPICE(STRINGTOOSHORT) is signaled.

-Files

   None.

-Particulars

   This is one of a family of related routines for inserting values
   into strings. They are typically to construct messages that
   are partly fixed, and partly determined at run time. For example,
   a message like

      "Fifty-one pictures were found in directory [USER.DATA]."

   might be constructed from the fixed string

      "#1 pictures were found in directory #2."

   by the calls

      #include "SpiceUsr.h"
           .
           .
           .
      #define   STRLEN                  81
           .
           .
           .
      repmct_c ( string, "#1",  51,  'c',     STRLEN, string );
      repmc_c  ( string, "#2", "[USER.DATA]", STRLEN, string );


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

   1) The following example illustrate the use of repmc_c to
      replace a marker within a string with a character string
      value.


      Example code begins here.


      /.
         Program repmc_ex1
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
         1. Single marker
         ./
         strncpy( marker, "#", 2 );
         strncpy( instr, "Invalid value. The value was:  #", 33 );

         repmc_c ( instr, marker, "append", STRLEN, outstr );

         printf( "Case 1: Single marker.\n" );
         printf( "   Input : %s\n", instr );
         printf( "   Output: %s\n", outstr );
         printf( "\n" );

         /.
         2. Multiple markers
         ./
         strncpy( marker, " XX ", 5 );
         strncpy( instr, "The token XX was not recognized. Was it XX?", 44 );

         repmc_c ( instr, marker, "  FND  ", STRLEN, outstr );

         printf( "Case 2: Multiple markers.\n" );
         printf( "   Input : %s\n", instr );
         printf( "   Output: %s\n", outstr );
         printf( "\n" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Case 1: Single marker.
         Input : Invalid value. The value was:  #
         Output: Invalid value. The value was:  append

      Case 2: Multiple markers.
         Input : The token XX was not recognized. Was it XX?
         Output: The token FND was not recognized. Was it XX?


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.1.0, 04-AUG-2021 (JDR)

       Changed the input argument name "lenout" to "outlen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing code fragment.

   -CSPICE Version 1.0.0, 14-AUG-2002 (NJB) (IMU)

-Index_Entries

   replace marker with character_string

-&
*/

{ /* Begin repmc_c */


   /*
   Local variables
   */
   ConstSpiceChar        * markPtr;
   ConstSpiceChar        * valPtr;


   /*
   Use discovery check-in.

   Make sure no string argument pointers are null.
   */
   CHKPTR( CHK_DISCOVER, "repmc_c", in     );
   CHKPTR( CHK_DISCOVER, "repmc_c", marker );
   CHKPTR( CHK_DISCOVER, "repmc_c", value  );
   CHKPTR( CHK_DISCOVER, "repmc_c", out    );


   /*
   If the output string can't hold a terminating null character,
   we can't proceed.
   */
   if ( outlen < 1 )
   {
      chkin_c  ( "repmc_c"                                    );
      setmsg_c ( "String length outlen must be >= 1; actual "
                 "value = #."                                 );
      errint_c ( "#", outlen                                  );
      sigerr_c ( "SPICE(STRINGTOOSHORT)"                      );
      chkout_c ( "repmc_c"                                    );
      return;
   }


   /*
   If the output string has no room for data characters, we simply
   terminate the string.
   */
   if ( outlen == 1 )
   {
      out[0] = NULLCHAR;
      return;
   }


   /*
   If the input string has zero length, the output is empty as well.
   */
   if ( in[0] == NULLCHAR )
   {
      out[0] = NULLCHAR;

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
   If the value is empty, pass a blank value to the f2c'd routine.
   Otherwise, pass in the marker.
   */
   if ( value[0] == NULLCHAR )
   {
      valPtr = " ";
   }
   else
   {
      valPtr = value;
   }


   /*
   Simply call the f2c'd routine.
   */
   repmc_ ( ( char     * ) in,
            ( char     * ) markPtr,
            ( char     * ) valPtr,
            ( char     * ) out,
            ( ftnlen     ) strlen(in),
            ( ftnlen     ) strlen(markPtr),
            ( ftnlen     ) strlen(valPtr),
            ( ftnlen     ) outlen-1         );

   /*
   Convert the output string from Fortran to C style.
   */
   F2C_ConvertStr ( outlen, out );


} /* End repmc_c */
