/*

-Procedure posr_c ( Position of substring, reverse search )

-Abstract

   Find the first occurrence in a string of a substring, starting at
   a specified location, searching backward.

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


   SpiceInt posr_c ( ConstSpiceChar    * str,
                     ConstSpiceChar    * substr,
                     SpiceInt            start  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   str        I   Any character string.
   substr     I   Substring to locate in the character string.
   start      I   Position to begin looking for substr in str.

   The function returns the index of the last occurrence of substr in
   str at or preceding index start.

-Detailed_Input

   str         is any character string.

   substr      is a substring to look for in str. Spaces in substr are
               significant, including trailing blanks.

   start       is the position in str to begin looking for substr. start
               may range from 0 to n-1, where n is the number of
               characters in str.

-Detailed_Output

   The function returns the index of the beginning of the last
   substring of str that begins at or before index start and is equal
   to substr. If the substring cannot be found after start, the
   function is returns -1.

-Parameters

   None.

-Exceptions

   1)  If `start' is less than 0, posr_c returns -1.

   2)  If `start' is greater than strlen(str), the search begins
       at the last character of the string.

   3)  If any of the `str' or `substr' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled. The function returns
       the value -1.

   4)  If any of the `str' or `substr' input strings has zero length,
       the function returns the value -1. This case is not considered
       an error.

-Files

   None.

-Particulars

   posr_c is case sensitive.

   An entire family of related CSPICE routines

      cpos_c
      cposr_c
      ncpos_c
      ncposr_c
      pos_c
      posr_c

   is described in the Required Reading.

-Examples

   Let string == "AN ANT AND AN ELEPHANT        "
                  012345678901234567890123456789

   Normal (Sequential) Searching:
   ------------------------------

      posr_c ( STRING, "AN",  29 ) == 19
      posr_c ( STRING, "AN",  18 ) == 11
      posr_c ( STRING, "AN",  10 ) ==  7
      posr_c ( STRING, "AN",   6 ) ==  3
      posr_c ( STRING, "AN",   2 ) ==  0

   start out of bounds:
   --------------------

      posr_c ( STRING, "AN", -6 ) == -1
      posr_c ( STRING, "AN", -1 ) == -1
      posr_c ( STRING, "AN", 30 ) == 19
      posr_c ( STRING, "AN", 43 ) == 19

   Significance of Spaces:
   -----------------------

      posr_c ( STRING, "AN",    29 ) ==  19
      posr_c ( STRING, " AN",   29 ) ==  10
      posr_c ( STRING, " AN ",  29 ) ==  10
      posr_c ( STRING, " AN ",   9 ) ==  -1
      posr_c ( STRING, " AN  ", 29 ) ==  -1

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 04-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 15-AUG-2002 (NJB) (WLT)

-Index_Entries

   position of substring reverse search

-&
*/

{ /* Begin posr_c */


   /*
   Local variables
   */
   SpiceInt                fstart;
   SpiceInt                retval;



   /*
   Use discovery check-in.

   Check for null pointers.
   */
   CHKPTR_VAL ( CHK_DISCOVER, "posr_c",  str,    -1 );
   CHKPTR_VAL ( CHK_DISCOVER, "posr_c",  substr, -1 );


   /*
   Check for empty strings.
   */
   if (  ( strlen(str) == 0 ) || ( strlen(substr) == 0 )  )
   {
     return ( -1 );
   }


   /*
   The rest can be handled by the f2c'd SPICELIB routine.  Adjust
   the start index to account for Fortran indexing.
   */

   fstart = start + 1;

   retval  =  posr_ ( (char     *) str,
                      (char     *) substr,
                      (integer  *) &fstart,
                      (ftnlen    ) strlen(str),
                      (ftnlen    ) strlen(substr)  );

   /*
   Adjust the return value to account for C indexing.
   */
   return ( retval-1 );


} /* End posr_c */
