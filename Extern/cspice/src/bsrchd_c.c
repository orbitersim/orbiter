/*

-Procedure bsrchd_c ( Binary search for a double precision value )

-Abstract

   Do a binary search for a given value within a double precision
   array, assumed to be in nondecreasing order. Return the index of
   the matching array entry, or -1 if the key value is not found.

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
   #include "SpiceZim.h"
   #undef    bsrchd_c

   SpiceInt bsrchd_c ( SpiceDouble          value,
                       SpiceInt             ndim,
                       ConstSpiceDouble   * array )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   value      I   Value to find in `array'.
   ndim       I   Dimension of `array'.
   array      I   Array to be searched.

   The function returns the index of the input key value in the
   input array, or -1 if the value is not found.

-Detailed_Input

   value       is the double precision value to be found in the input
               array.

   ndim        is the number of elements in the input array.

   array       is the double precision array to be searched. The
               elements in `array' are assumed to sorted in increasing
               order.

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

   Error free.

   1)  If ndim < 1, the value of the function is -1.

-Files

   None.

-Particulars

   A binary search is performed on the input array. If an element of
   the array is found to match the input value, the index of that
   element is returned. If no matching element is found, -1 is
   returned.

-Examples

   Let `array' contain the following elements:

      -11.0
        0.0
       22.0
      750.0

   Then

      bsrchd_c ( -11.0, 4, array )   ==  0
      bsrchd_c (  22.0, 4, array )   ==  2
      bsrchd_c ( 751.0, 4, array )   == -1

-Restrictions

   1)  `array' is assumed to be sorted in increasing order. If this
       condition is not met, the results of bsrchd_c are unpredictable.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 22-AUG-2002 (NJB) (IMU)

-Index_Entries

   binary search for a double precision value

-&
*/

{ /* Begin bsrchd_c */


   /*
   Note that we adjust the return value to make it a C-style index.
   */

   return (   bsrchd_ ( (doublereal *) &value,
                        (integer    *) &ndim,
                        (doublereal *) array  )   - 1  );

} /* End bsrchd_c */
