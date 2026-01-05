/*

-Procedure esrchc_c ( Equivalence search, character )

-Abstract

   Search for a given value within a character string array.
   Return the index of the first equivalent array entry, or -1
   if no equivalent element is found.

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

   ARRAY
   SEARCH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef    esrchc_c

   SpiceInt esrchc_c ( ConstSpiceChar  * value,
                       SpiceInt          ndim,
                       SpiceInt          arrlen,
                       const void      * array    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   value      I   Key value to be found in array.
   ndim       I   Dimension of array.
   arrlen     I   String length.
   array      I   Character string array to search.

   The function returns the index of the first array entry
   equivalent to value, or -1 if none is found.

-Detailed_Input

   value       is the key value to be found in the array. Trailing
               blanks in this key are not significant: string matches
               found by this routine do not require trailing blanks in
               value to match those in the corresponding element of
               `array'.

               `value' may be an empty string.

   ndim        is the dimension of the array.

   arrlen      is the declared length of the strings in the input
               string `array', including null terminators. The input
               array should be declared with dimension

                  [ndim][arrlen]

   array       is the array of character strings to be searched. Trailing
               blanks in the strings in this array are not significant.

-Detailed_Output

   The function returns the index of the first element of the
   input array equivalent to the input value, or -1 if the
   array contains no such elements.

   Two strings are equivalent if they contain the same characters
   in the same order, when blanks are ignored and uppercase and
   lowercase characters are considered equal.

-Parameters

   None.

-Exceptions

   1)  If ndim < 1 the function value is -1. This is not considered
       an error.

   2)  If the `value' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the value
       -1.

   3)  If the `array' input array pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the value
       -1.

   4)  If the `array' input array strings have length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled. The
       function returns the value -1.

-Files

   None.

-Particulars

   esrchc_c is identical to isrchc_c, except that it looks for
   the first equivalent string (as defined by eqstr_c) instead
   of the first identical one.

-Examples

   Let array be declared with dimension

      [NDIM][STRLEN]

   and contain the following elements:

      array[0] == "This"
      array[1] == "little"
      array[2] == "piggy"
      array[3] == "went"
      array[4] == "to"
      array[5] == "market"

   Then

      esrchc_c ( "PIGGY",      NDIM,  STRLEN,  array )  ==  2
      esrchc_c ( " LiTtLe  ",  NDIM,  STRLEN,  array )  ==  1
      esrchc_c ( "W e n t",    NDIM,  STRLEN,  array )  ==  3
      esrchc_c ( "mall",       NDIM,  STRLEN,  array )  == -1

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.1.0, 24-AUG-2021 (JDR)

       Changed the input argument name "lenvals" to "arrlen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard.

       Removed entry #3 from -Exceptions section and updated the argument
       "value" description to indicate that empty strings are allowed.

   -CSPICE Version 1.0.0, 22-JUL-2002 (NJB) (IMU)

-Index_Entries

   search array for equivalent character_string

-&
*/

{ /* Begin esrchc_c */


   /*
   Local macros
   */
   #define ARRAY( i )     (  ( (SpiceChar *)array ) + (i)*arrlen  )

   /*
   Local variables
   */
   SpiceInt                i;


   /*
   Use discovery check-in.

   Return immediately if the array dimension is non-positive.
   */
   if ( ndim < 1 )
   {
      return ( -1 );
   }


   /*
   Make sure the input pointer for the key value is non-null
   and that the length is adequate.
   */
   CHKPTR_VAL ( CHK_DISCOVER, "esrchc_c", value, -1 );


   /*
   Make sure the input pointer for the string array is non-null
   and that the length arrlen is sufficient.
   */
   CHKOSTR_VAL ( CHK_DISCOVER, "esrchc_c", array, arrlen, -1 );


   for ( i = 0;  i < ndim;  i++ )
   {
      if (  eqstr_c( value, ARRAY(i) )  )
      {
         return ( i );
      }
   }

   /*
   Indicate no match was found.
   */
   return ( -1 );



} /* End esrchc_c */
