/*

-Procedure bschoi_c ( Binary search with order vector, integer )

-Abstract

   Do a binary search for a given value within an integer array,
   accompanied by an order vector. Return the index of the
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

   #include <stdlib.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZim.h"
   #undef    bschoi_c


   SpiceInt bschoi_c ( SpiceInt          value,
                       SpiceInt          ndim,
                       ConstSpiceInt   * array,
                       ConstSpiceInt   * order  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   value      I   Value to find in `array'.
   ndim       I   Dimension of `array'.
   array      I   Array to be searched.
   order      I   Order vector.

   The function returns the index of the first matching array element
   or -1 if the value is not found.

-Detailed_Input

   value       is the value to be found in the input array.

   ndim        is the number of elements in the input array.

   array       is the array to be searched.

   order       is an order vector which can be used to access the
               elements of `array' in order. The contents of order are a
               permutation of the sequence of integers ranging from 0 to
               ndim-1.

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

   1)  If ndim < 1, the value of the function is -1.

   2)  If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled. The function
       returns the value -1.

-Files

   None.

-Particulars

   A binary search is performed on the input array, whose order is
   given by an associated order vector. If an element of the array is
   found to match the input value, the index of that element is
   returned. If no matching element is found, -1 is returned.

-Examples

   Let `array' and `order' contain the following elements:

      array         order
      -----------   -----
        100             1
          1             2
         10             0
      10000             4
       1000             3

   Then

      bschoi_c (  1000, 5, array, order )  ==   4
      bschoi_c (     1, 5, array, order )  ==   1
      bschoi_c ( 10000, 5, array, order )  ==   3
      bschoi_c (    -1, 5, array, order )  ==  -1
      bschoi_c (    17, 5, array, order )  ==  -1

   That is,

      array[4] ==  1000
      array[1] ==     1
      array[3] == 10000

-Restrictions

   1)  `order' is assumed to give the order of the elements of `array'
       in increasing order. If this condition is not met, the results
       of bschoi_c are unpredictable.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 08-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Updated
       -Index_Entries to better explain the objective of this function.

   -CSPICE Version 1.0.0, 10-JUL-2002 (NJB) (WLT) (IMU)

-Index_Entries

   binary search for an integer using an order vector

-&
*/

{ /* Begin bschoi_c */


   /*
   Local variables
   */
   SpiceInt                i ;
   SpiceInt                loc ;
   SpiceInt              * ordvec;
   SpiceInt                vSize;



   /*
   Use discovery check-in.

   Return immediately if the array dimension is non-positive.
   */
   if ( ndim < 1 )
   {
      return ( -1 );
   }


   /*
   Get a local copy of the input order vector; map the vector's contents
   to the range 1:ndim.
   */
   vSize  = ndim * sizeof(SpiceInt);

   ordvec = (SpiceInt *) malloc( vSize );

   if ( ordvec == 0 )
   {
      chkin_c  ( "bschoi_c"                                );
      setmsg_c ( "Failure on malloc call to create array "
                 "for Fortran-style order vector.  Tried "
                 "to allocate # bytes."                    );
      errint_c ( "#",  vSize                               );
      sigerr_c ( "SPICE(MALLOCFAILED)"                     );
      chkout_c ( "bschoi_c"                                );

      return   ( -1 );
   }

   for ( i = 0;  i < ndim;  i++ )
   {
      ordvec[i] = order[i] + 1;
   }

   loc  =  bschoi_ ( (integer *) &value,
                     (integer *) &ndim,
                     (integer *) array,
                     (integer *) ordvec   )   -  1;

   free   ( ordvec );

   return ( loc );


} /* End bschoi_c */
