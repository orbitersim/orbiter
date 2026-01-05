/*

-Procedure sumai_c ( Sum of an integer array )

-Abstract

   Return the sum of the elements of an integer array.

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
   MATH
   UTILITY

*/

   #include "SpiceUsr.h"
   #undef    sumai_c


   SpiceInt sumai_c ( ConstSpiceInt   * array,
                      SpiceInt          n     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   array      I   Input array.
   n          I   Number of elements in `array'.

   The function returns the sum of the elements of `array'.

-Detailed_Input

   array       is the input integer array.

   n           is the number of elements in the array.

-Detailed_Output

   The function returns the sum of the elements of the input array.
   That is,

      sumai_c( array, n ) = array[0] + array[1] + ... + array[n-1]

   If `n' is zero or negative, sumai_c is zero.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The value of the function is initially set to zero. The elements
   of the array are then added. If the number of elements is zero or
   negative, sumai_c is zero.

-Examples

   Let `array' contain the following elements.

         array[0] = 12
         array[1] =  1
         array[2] =  4
         array[3] = 75
         array[4] = 18

   Then

         sumai_c ( array,   -3 )       =   0
         sumai_c ( array,    0 )       =   0
         sumai_c ( array,    1 )       =  12
         sumai_c ( array,    2 )       =  13
         sumai_c ( array,    5 )       = 110
         sumai_c ( array+2,  3 )       =  97

-Restrictions

   1)  sumai_c does not check for overflow.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 09-APR-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 24-MAR-1999 (IMU) (NJB)

-Index_Entries

   sum of an integer array

-&
*/

{ /* Begin sumai_c */

   /*
   Local variables
   */

   SpiceInt                retval;
   SpiceInt                i;



   retval = 0;

   for ( i = 0;  i < n;  i++  )
   {
      retval += array[i];
   }

   return ( retval );


} /* End sumai_c */
