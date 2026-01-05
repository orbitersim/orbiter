/*

-Procedure bsrchc_c ( Binary search for a character string )

-Abstract

   Do a binary search for a given value within a character string array,
   assumed to be in nondecreasing order. Return the index of the
   matching array entry, or -1 if the key value is not found.

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
   #include "SpiceZmc.h"
   #include "f2cMang.h"
   #undef    bsrchc_c


   SpiceInt bsrchc_c ( ConstSpiceChar  * value,
                       SpiceInt          ndim,
                       SpiceInt          arrlen,
                       const void      * array   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   value      I   Key value to be found in `array'.
   ndim       I   Dimension of `array'.
   arrlen     I   Declared length of the strings in `array'.
   array      I   Character string array to search.

   The function returns the index of the first matching array element
   or -1 if the value is not found.

-Detailed_Input

   value       is the key value to be found in the array. Trailing
               blanks in this key are not significant: string matches
               found by this routine do not require trailing blanks in
               value to match those in the corresponding element of
               array.

   ndim        is the number of elements in the input array.

   arrlen      is the declared length of the strings in the input
               string array, including null terminators. The input
               array should be declared with dimension

                  [ndim][arrlen]

   array       is the array of character strings to be searched. Trailing
               blanks in the strings in this array are not significant.
               The elements in `array' are assumed to sorted according to
               the ASCII collating sequence..

-Detailed_Output

   The function returns the index of the specified value in the input
   array. Indices range from 0 to ndim-1.

   If the input array does not contain the specified value, the
   function returns -1.

   If the input array contains more than one occurrence of the
   specified value, the returned index may point to any of the
   occurrences.

-Parameters

   None.

-Exceptions

   1)  If ndim < 1, the value of the function is -1. This is not
       considered an error.

   2)  If the `value' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the
       value -1.

   3)  If the `array' input array pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the
       value -1.

   4)  If the `array' input array strings have length less than two
       characters, the error SPICE(EMPTYSTRING) is signaled. The
       function returns the value -1.

-Files

   None.

-Particulars

   A binary search is performed on the input array. If an element of
   the array is found to match the input value, the index of that
   element is returned. If no matching element is found, -1 is
   returned.

-Examples

   Let array be a character array of dimension

     [5][arrlen]

   which contains the following elements:

      "BOHR"
      "EINSTEIN"
      "FEYNMAN"
      "GALILEO"
      "NEWTON"

   Then

      bsrchc_c ( "NEWTON",   5, arrlen, array )    ==   4
      bsrchc_c ( "EINSTEIN", 5, arrlen, array )    ==   1
      bsrchc_c ( "GALILEO",  5, arrlen, array )    ==   3
      bsrchc_c ( "Galileo",  5, arrlen, array )    ==  -1
      bsrchc_c ( "BETHE",    5, arrlen, array )    ==  -1

-Restrictions

   1)  `array' is assumed to be sorted in increasing order according to
       the ASCII collating sequence. If this condition is not met,
       the results of bsrchc_c are unpredictable.

   2)  String comparisons performed by this routine are Fortran-style:
       trailing blanks in the input array or key value are ignored.
       This gives consistent behavior with CSPICE code generated by
       the f2c translator, as well as with the Fortran SPICE Toolkit.

       Note that this behavior is not identical to that of the ANSI
       C library functions strcmp and strncmp.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)

-Version

   -CSPICE Version 1.2.0, 01-NOV-2021 (JDR)

       Changed the input argument name "lenvals" to "arrlen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.1.0, 07-MAR-2009 (NJB)

       This file now includes the header file f2cMang.h.
       This header supports name mangling of f2c library
       functions.

       Header sections were re-ordered.

   -CSPICE Version 1.0.0, 26-AUG-2002 (NJB) (WMO)

-Index_Entries

   search in a character array

-&
*/

{ /* Begin bsrchc_c */

   /*
   f2c library utility prototypes
   */
   logical          l_lt   (char *a, char *b, ftnlen la, ftnlen lb );
   extern integer   s_cmp  (char *a, char *b, ftnlen la, ftnlen lb );

   /*
   Local macros
   */
   #define ARRAY( i )     (  ( (SpiceChar *)array ) + (i)*arrlen  )


   /*
   Local variables
   */
   SpiceInt                i;
   SpiceInt                keylen;
   SpiceInt                left;
   SpiceInt                order;
   SpiceInt                right;


   /*
   Use discovery check-in.

   Return immediately if the array dimension is non-positive.
   */
   if ( ndim < 1 )
   {
      return ( -1 );
   }


   /*
   Make sure the pointer for the key value is non-null
   and that the length is adequate.
   */
   CHKPTR_VAL ( CHK_DISCOVER, "bsrchc_c", value, -1 );


   /*
   Make sure the pointer for the string array is non-null
   and that the length arrlen is sufficient.
   */
   CHKOSTR_VAL ( CHK_DISCOVER, "bsrchc_c", array, arrlen, -1 );


   /*
   Do a binary search for the specified key value.
   */
   keylen = strlen(value);

   left   = 0;
   right  = ndim - 1;

   while ( left <= right )
   {
      /*
      Check the middle element.
      */
      i  =  ( left + right ) / 2;

      /*
      The f2c library function s_cmp performs a Fortran-style
      lexical order comparison.  A negative return value indicates
      the first argument is less than the second, a return value
      of zero indicates equality, and a positive value indicates
      the second argument is greater.
      */
      order =  (SpiceInt) s_cmp ( (char    * ) value,
                                  (char    * ) ARRAY(i),
                                  (ftnlen    ) keylen,
                                  (ftnlen    ) strlen(ARRAY(i)) );

      /*
      If the middle element matches, return its location.
      */
      if ( order == 0 )
      {
         return ( i );
      }

      /*
      Otherwise, narrow the search area.
      */
      else if ( order < 0 )
      {
         /*
         value is less than the middle element.
         */
         right = i - 1;
      }

      else
      {
         left  = i + 1;
      }

   }

   /*
   If the search area is empty, indicate the value was not found.
   */
   return ( -1 );


} /* End bsrchc_c */
