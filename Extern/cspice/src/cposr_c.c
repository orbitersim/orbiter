/*

-Procedure cposr_c ( Character position, reverse )

-Abstract

   Find the first occurrence in a string of a character belonging
   to a collection of characters, starting at a specified location,
   searching in reverse.

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

   SCANNING

-Keywords

   CHARACTER
   SEARCH
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   SpiceInt cposr_c ( ConstSpiceChar    * str,
                      ConstSpiceChar    * chars,
                      SpiceInt            start  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   str        I   Any character string.
   chars      I   A collection of characters.
   start      I   Position to begin looking for one of chars.

   The function returns the index of the last character of str
   at or before index start that is in the collection chars.

-Detailed_Input

   str         is any character string.

   chars       is a character string containing a collection
               of characters. Spaces in chars are significant,
               including trailing blanks. The order in which
               characters are listed is not significant.

   start       is the position in str to begin looking for one of
               the characters in chars. start may range from 0
               to n-1, where n is the number of characters in str.

-Detailed_Output

   The function returns the index of the last character of str (at or
   before index start) that is one of the characters in the string
   chars. The returned value normally ranges from 0 to n-1, where n is
   the number of characters in str. If none of the characters is found,
   the function returns -1.

-Parameters

   None.

-Exceptions

   1)  If `start' is less than 1, cposr_c returns -1.

   2)  If `start' is greater than strlen(string), the search begins
       at the last character of the string.

   3)  If any of the `str' or `chars' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled. The function returns
       the value -1.

-Files

   None.

-Particulars

   cposr_c is case sensitive.

   An entire family of related CSPICE routines

      cpos_c
      cposr_c
      ncpos_c
      ncposr_c
      pos_c
      posr_c

   is described in the Required Reading.

-Examples

   Let string == "BOB, JOHN, TED, AND MARTIN...."
                  012345678901234567890123456789


   Normal (sequential) searching:
   ------------------------------

     cposr_c( string, ' ,',    29 ) = 29
     cposr_c( string, ' ,',    28 ) = 28
     cposr_c( string, ' ,',    27 ) = 27
     cposr_c( string, ' ,',    26 ) = 26
     cposr_c( string, ' ,',    25 ) = 19
     cposr_c( string, ' ,',    18 ) = 15
     cposr_c( string, ' ,',    14 ) = 14
     cposr_c( string, ' ,',    13 ) = 10
     cposr_c( string, ' ,',     9 ) =  9
     cposr_c( string, ' ,',     8 ) =  4
     cposr_c( string, ' ,',     3 ) =  3
     cposr_c( string, ' ,',     2 ) = -1


   start out of bounds:
   --------------------

     cposr_c( string, ' ,',   230 ) = 29
     cposr_c( string, ' ,',    30 ) = 29
     cposr_c( string, ' ,',    -1 ) = -1
     cposr_c( string, ' ,',   -10 ) = -1

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 08-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 27-AUG-2002 (NJB) (WLT)

-Index_Entries

   backward search for position of character

-&
*/

{ /* Begin cposr_c */

   /*
   Local variables
   */
   SpiceInt                fstart;
   SpiceInt                retval;



   /*
   Use discovery check-in.

   Check for null pointers.
   */
   CHKPTR_VAL ( CHK_DISCOVER, "cposr_c",  str,   -1 );
   CHKPTR_VAL ( CHK_DISCOVER, "cposr_c",  chars, -1 );


   /*
   Check for empty strings.
   */
   if (  ( strlen(str) == 0 ) || ( strlen(chars) == 0 )  )
   {
     return ( -1 );
   }


   /*
   The rest can be handled by the f2c'd SPICELIB routine. Adjust
   the start index to account for Fortran indexing.
   */

   fstart = start + 1;

   retval  =  cposr_ ( (char     *) str,
                       (char     *) chars,
                       (integer  *) &fstart,
                       (ftnlen    ) strlen(str),
                       (ftnlen    ) strlen(chars)  );

   /*
   Adjust the return value to account for C indexing.
   */
   return ( retval-1 );


} /* End cposr_c */
