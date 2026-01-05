/*

-Procedure lastnb_c ( Last non-blank character )

-Abstract

   Return the zero based index of the last non-blank character in
   a character string.

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

   ASCII
   CHARACTER
   SEARCH

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"

   SpiceInt lastnb_c ( ConstSpiceChar * string )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   string     I   Input character string.

   The function returns the zero-based index of the last non-blank
   character in a character string.

-Detailed_Input

   string      is the input character string.

-Detailed_Output

   The function returns the zero-based index of the last non-blank
   character in a character string. If the string is entirely blank
   or is empty, the value -1 is returned.

-Parameters

   None.

-Exceptions

   1)  If the `string' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the value
       -1.

-Files

   None.

-Particulars

   If the string is blank or null, return -1. Otherwise, step through
   the string one character at a time until something other than
   a blank is found. Return the zero based index of that something
   within the string.

   Note that if the length of the string to the last non-blank
   character is of interest, that value is the returned value plus one.

-Examples

   The following examples illustrate the use of lastnb_c.

         last = lastnb_c ( "ABCDE"              );
         last is 4

         last = lastnb_c ( "AN EXAMPLE"         );
         last is 9

         last = lastnb_c ( "AN EXAMPLE        " );
         last is 9

         last = lastnb_c ( "                  " )
         last is -1

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.1.0, 27-AUG-1999 (NJB)

       Added check for null input string. Added some further comments
       to the -Brief_I/O and -Detailed_Output header sections.

   -CSPICE Version 1.0.0, 08-FEB-1998 (KRG) (IMU) (EDW)

-Index_Entries

   last non-blank character

-&
*/

{ /* Begin lastnb_c */

   /*
   Local variables
   */
   SpiceInt                 i;



   /*
   Check the input string pointer to make sure it's non-null.
   */
   CHKPTR_VAL ( CHK_DISCOVER, "lastnb_c", string, -1 );


   i = strlen(string) - 1;


   /*
   Start at the end of the string, moving backwards until a non blank
   character is found.  Once found return the index value.
   */

   while ( ( i >= 0 ) && ( string[i] == BLANK ) )
      {
      i--;
      }


   return i;


} /* End lastnb_c */
