/*

-Procedure orderi_c ( Order of an integer array )

-Abstract

   Determine the order of elements in an integer array.

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
   SORT

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZim.h"
   #undef    orderi_c


   void orderi_c ( ConstSpiceInt  * array,
                   SpiceInt         ndim,
                   SpiceInt       * iorder )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   array      I    Input array.
   ndim       I    Dimension of array.
   iorder     O    Order vector for array.

-Detailed_Input

   array       is the input array.

   ndim        is the number of elements in the input array.

-Detailed_Output

   iorder      is the order vector for the input array.
               iorder[0] is the index of the smallest element
               of array; iorder[1] is the index of the next
               smallest; and so on.

               The elements of iorder range from zero to ndim-1.

-Parameters

   None.

-Exceptions

   Error free.

   1)  A negative input dimension causes this routine to leave the
       output order vector unchanged.

-Files

   None.

-Particulars

   orderi_c finds the index of the smallest element of the input
   array. This becomes the first element of the order vector.
   The process is repeated for the rest of the elements.

   The order vector returned by orderi_c may be used by any of
   the reord*_c routines to sort sets of related arrays, as shown
   in the example below.

-Examples

   In the following example, the order*_c and reord*_c routines are
   used to sort four related arrays (containing the names,
   masses, integer ID codes, and visual magnitudes for a group
   of satellites). This is representative of the typical use of
   these routines.

      #include "SpiceUsr.h"
            .
            .
            .
      /.
      Sort the object arrays by ID code.
      ./

      orderi_c ( codes,  n,         iorder );

      reordc_c ( iorder, n, namlen, names  );
      reordd_c ( iorder, n,         masses );
      reordi_c ( iorder, n,         codes  );
      reordd_c ( iorder, n,         vmags  );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.2, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 23-MAR-2010 (NJB)

       Header example was updated to show use of this routine.
       -Exceptions section was updated. Header sections were
       re-ordered.

   -CSPICE Version 1.0.0, 08-JUL-2002 (NJB) (IMU)

-Index_Entries

   order of an integer array

-&
*/

{ /* Begin orderi_c */

   /*
   Local variables
   */
   SpiceInt                i;


   /*
   Call the f2c'd routine.
   */
   orderi_ ( ( integer * ) array,
             ( integer * ) &ndim,
             ( integer * ) iorder );

   /*
   Map the order vector elements to the range 0 : ndim-1.
   */
   for ( i = 0;  i < ndim;  i++ )
   {
      --iorder[i];
   }


} /* End orderi_c */
