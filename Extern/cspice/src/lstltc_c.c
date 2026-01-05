/*

-Procedure lstltc_c ( Last character element less than )

-Abstract

   Find the index of the largest array element less than a given
   character string in an ordered array of character strings.

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
   #include "f2cMang.h"
   #undef    lstltc_c

   SpiceInt lstltc_c ( ConstSpiceChar  * string,
                       SpiceInt          n,
                       SpiceInt          arrlen,
                       const void      * array   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   string     I   Upper bound value to search against.
   n          I   Number of elements in `array'.
   arrlen     I   Declared length of the strings in `array'.
   array      I   Array of possible lower bounds.

   The function returns the index of the last element of `array' that
   is lexically less than `string'.

-Detailed_Input

   string      is a string acting as an upper bound: the element of
               `array' that is lexically the greatest element less than
               `string' is to be found. Trailing blanks in this bound
               value are not significant.

   n           is the total number of elements in `array'.

   arrlen      is the declared length of the strings in the input
               string `array', including null terminators. The input
               array should be declared with dimension

                  [n][arrlen]

   array       is an array of character strings to be searched. Trailing
               blanks in the strings in this array are not significant.
               The strings in `array' must be sorted in non-decreasing
               order. The elements of `array' need not be distinct.

-Detailed_Output

   The function returns the index of the highest-indexed element in
   the input array that is lexically less than `string'. The routine
   assumes the array elements are sorted in non-decreasing order.

   Indices range from 0 to n-1.

   If all elements of `array' are lexically greater than or equal to
   `string', the routine returns the value -1. If `n' is less than or
   equal to zero, the routine returns the value -1.

-Parameters

   None.

-Exceptions

   1)  If `n' is less than or equal to zero, the function returns -1.
       This case is not treated as an error.

   2)  If the input array is not sorted in non-decreasing order, the
       output of this routine is undefined. No error is signaled.

   3)  If the `string' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the value
       -1.

   4)  If the `array' input array pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the value
       -1.

   5)  If the `array' input array strings have length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled. The
       function returns the value -1.

-Files

   None.

-Particulars

   This routine uses a binary search algorithm and so requires
   at most on the order of

      log (n)
         2

   steps to compute the value of lstltc_c.

   Note: If you need to find the first element of the array that is
   lexically greater than or equal to `string', simply add 1 to the
   result returned by this function and check to see if the result is
   within the array bounds given by `n'.

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

      lstltc_c ( "NEWTON",   5, arrlen, array )    ==   3
      lstltc_c ( "EINSTEIN", 5, arrlen, array )    ==   0
      lstltc_c ( "GALILEO",  5, arrlen, array )    ==   2
      lstltc_c ( "Galileo",  5, arrlen, array )    ==   3
      lstltc_c ( "BETHE",    5, arrlen, array )    ==  -1

-Restrictions

   1)  If the sequence of character strings in the input array `array'
       is not non-decreasing, the program will run to completion but
       the index found will not mean anything.

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
   H.A. Neilan         (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.2.0, 01-NOV-2021 (JDR)

       Changed the input argument name "lenvals" to "arrlen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Improved
       -Detailed_Input, -Detailed_Output, -Particulars, -Exceptions and
       -Restrictions sections.

   -CSPICE Version 1.1.0, 07-MAR-2009 (NJB)

       This file now includes the header file f2cMang.h.
       This header supports name mangling of f2c library
       functions.

   -CSPICE Version 1.0.0, 22-JUL-2002 (NJB) (HAN) (WLT)

-Index_Entries

   last character element less_than

-&
*/

{ /* Begin lstltc_c */

   /*
   f2c library utility prototypes
   */
   logical          l_gt   (char *a, char *b, ftnlen la, ftnlen lb );
   logical          l_le   (char *a, char *b, ftnlen la, ftnlen lb );
   logical          l_lt   (char *a, char *b, ftnlen la, ftnlen lb );

   /*
   Local macros
   */
   #define ARRAY( i )     (  ( (SpiceChar *)array ) + (i)*arrlen  )


   /*
   Local variables
   */
   SpiceInt                begin;
   SpiceInt                end;
   SpiceInt                items;
   SpiceInt                j;
   SpiceInt                keylen;
   SpiceInt                middle;



   /*
   Use discovery check-in.

   Return immediately if the array dimension is non-positive.
   */
   if ( n < 1 )
   {
      return ( -1 );
   }

   /*
   Make sure the pointer for the key value is non-null
   and that the length is adequate.
   */
   CHKPTR_VAL ( CHK_DISCOVER, "lstltc_c", string, -1 );


   /*
   Make sure the pointer for the string array is non-null
   and that the length arrlen is sufficient.
   */
   CHKOSTR_VAL ( CHK_DISCOVER, "lstltc_c", array, arrlen, -1 );


   /*
   Return if none of the array's elements are less than the key value.
   */
   keylen = strlen(string);

   begin  = 0;
   end    = n - 1;

   if (  l_le( ( char * )string,
               ( char * )ARRAY(begin),
               ( ftnlen )keylen,
               ( ftnlen )strlen(ARRAY(begin)) )  )
   {
      return ( -1 );
   }


   /*
   Return if the key string is greater than all of the array's elements.
   */
   if (  l_gt( ( char * )string,
               ( char * )ARRAY(end),
               ( ftnlen )keylen,
               ( ftnlen )strlen(ARRAY(end)) )  )
   {
      return ( end );
   }


   /*
   Do a binary search for the specified key value.

   At this point, string is greater than the first element of array and
   less than or equal to the last element of array.
   */
   items  = n;

   while ( items > 2 )
   {
      /*
      Check the middle element.
      */
      j      = items / 2;
      middle = begin + j;


      /*
      Narrow the search area.
      */
      if (  l_lt ( (char    * ) ARRAY(middle),
                   (char    * ) string,
                   (ftnlen    ) strlen( ARRAY(middle) ),
                   (ftnlen    ) keylen                   )  )
      {
         /*
         The middle element is less than string.
         */
         begin = middle;
      }
      else
      {
         end   = middle;
      }

      items = end - begin + 1;

      /*
      At this point, string is greater than the array element at index
      begin and is less than or equal to the element at index end.
      */
   }

   /*
   The element at index begin is the winner.
   */
   return ( begin );


} /* End lstltc_c */
