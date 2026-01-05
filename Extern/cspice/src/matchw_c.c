/*

-Procedure matchw_c ( Match string against wildcard template )

-Abstract

   Determine whether a string is matched by a template containing
   wild cards. The comparison is case-sensitive.

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
   COMPARE

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   SpiceBoolean matchw_c ( ConstSpiceChar      * string,
                           ConstSpiceChar      * templ,
                           SpiceChar             wstr,
                           SpiceChar             wchr   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   string     I   String to be tested.
   templ      I   Template (with wild cards) to test against string.
   wstr       I   Wild string token.
   wchr       I   Wild character token.

   The function returns the value SPICETRUE if string matches templ,
   SPICEFALSE if not.

-Detailed_Input

   string      is the input character string to be tested for
               a match against the input template. Leading and
               trailing blanks are ignored.

   templ       is the input template to be tested for a match
               against the input string. `templ' may contain wild
               cards. Leading and trailing blanks are ignored.

   wstr        is the wild string token used in the input template.
               The wild string token may represent from zero to
               any number of characters.

   wchr        is the wild character token used in the input
               template. The wild character token represents
               exactly one character.

-Detailed_Output

   The function returns SPICETRUE when the input string matches the
   input template, and SPICEFALSE otherwise. The string and template
   match whenever the template can expand (through replacement of its
   wild cards) to become the input string.

-Parameters

   None.

-Exceptions

   1)  If any of the `string' or `templ' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled. The function
       returns the value SPICEFALSE.

   2)  If any of the `string' or `templ' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled. The function
       returns the value SPICEFALSE.

-Files

   None.

-Particulars

   matchw_c ignores leading and trailing blanks in both the string
   and the template. All of the following are equivalent: they
   all return SPICETRUE.

      #include "SpiceUsr.h"
            .
            .
            .
      matchw_c ( "ALCATRAZ",       "A*Z",      '*', '%' );
      matchw_c ( "  ALCATRAZ  ",   "A*Z",      '*', '%' );
      matchw_c ( "ALCATRAZ",       "  A*Z  ",  '*', '%' );
      matchw_c ( "  ALCATRAZ  ",   "  A*Z  ",  '*', '%' );

   matchw_c is case-sensitive: uppercase characters do not match
   lowercase characters, and vice versa. Wild characters match
   characters of both cases.

-Examples

   Let
         string  = "  ABCDEFGHIJKLMNOPQRSTUVWXYZ  "
         wstr    = '*'
         wchr    = '%'

   Then
         if TEMPL is  "*A*"        matchw_c is SPICETRUE
                      "A%D*"                     SPICEFALSE
                      "A%C*"                   SPICETRUE
                      "%A*"                      SPICEFALSE
                      "%%CD*Z"                 SPICETRUE
                      "%%CD"                     SPICEFALSE
                      "A*MN*Y*Z"               SPICETRUE
                      "A*MN*Y*%Z"                SPICEFALSE
                      "*BCD*Z*"                SPICETRUE
                      "*bcd*z*"                  SPICEFALSE
                      " *BCD*Z*  "             SPICETRUE

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

   -CSPICE Version 1.0.1, 04-AUG-2021 (NJB) (JDR)

       Edited the header to comply with NAIF standard.

       Corrected header example to show case-sensitive comparison.

   -CSPICE Version 1.0.0, 17-AUG-1999 (NJB) (WLT) (IMU)

-Index_Entries

   match string against wildcard template
   test whether a string matches a wildcard template

-&
*/

{ /* Begin matchw_c */

   /*
   Use discovery check-in.
   */


   /*
   Check the input strings string and templ to make sure the pointers
   are non-null and the strings are non-empty.
   */
   CHKFSTR_VAL ( CHK_DISCOVER, "matchw_c", string, SPICEFALSE );
   CHKFSTR_VAL ( CHK_DISCOVER, "matchw_c", templ,  SPICEFALSE );

   /*
   Call the f2c'd routine if we got this far.
   */

   return (   matchw_ (  ( char    * ) string,
                         ( char    * ) templ,
                         ( char    * ) &wstr,
                         ( char    * ) &wchr,
                         ( ftnlen    ) strlen(string),
                         ( ftnlen    ) strlen(templ),
                         ( ftnlen    ) 1,
                         ( ftnlen    ) 1              )   );


} /* End matchw_c */
