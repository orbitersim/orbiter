/*

-Procedure vtmvg_c  ( Vector transpose times matrix times vector )

-Abstract

   Multiply the transpose of a n-dimensional column vector,
   a nxm matrix, and a m-dimensional column vector.

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

   MATRIX
   VECTOR

*/

   #include "SpiceUsr.h"
   #undef vtmvg_c

   SpiceDouble vtmvg_c ( const void          * v1,
                         const void          * matrix,
                         const void          * v2,
                         SpiceInt              nrow,
                         SpiceInt              ncol    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   n-dimensional double precision column vector.
   matrix     I   nxm double precision matrix.
   v2         I   m-dimensional double precision column vector.
   nrow       I   Number of rows in matrix (number of rows in `v1'.)
   ncol       I   Number of columns in matrix (number of rows in `v2'.)

   The function returns the result of (v1**t * matrix * v2 ).

-Detailed_Input

   v1          is an n-dimensional double precision vector.

   matrix      is an n x m double precision matrix.

   v2          is an m-dimensional double precision vector.

   nrow        is the number of rows in matrix. this is also
               equivalent to the number of rows in the vector `v1'.

   ncol        is the number of columns in matrix. this is also
               equivalent to the number of rows in the vector `v2'.

-Detailed_Output

   The function returns the double precision value of the equation
   (v1**t * matrix * v2 ).

   Notice that vtmvg_c is actually the dot product of the vector
   resulting from multiplying the transpose of `v1' and `matrix' and the
   vector `v2'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This routine implements the following vector/matrix/vector
   multiplication:

                          T |          |  |  |
      vtmvg_c = [   v1   ]  |  matrix  |  |v2|
                            |          |  |  |

   by calculating over all values of the indices `k' and `l' from 0 to
   nrow-1 and 0 to ncol-1, respectively, the expression

      vtmvg_c = Summation of ( v1(k)*matrix(k,l)*v2(l) ) .

   `v1' is a column vector which becomes a row vector when transposed.
   `v2' is a column vector.

   No check performed to determine whether floating point
   overflow has occurred.

-Examples

   If  v1 = | 1.0 |  matrix = | 2.0  0.0 |  v2 = | 1.0 |
            |     |           |          |       |     |
            | 2.0 |           | 1.0  2.0 |       | 2.0 |
            |     |           |          |
            | 3.0 |           | 1.0  1.0 |

   nrow = 3
   ncol = 2

   Then the value of the function is  21.0.

-Restrictions

   1)  Since no error detection or recovery is implemented, the
       programmer is required to insure that the inputs to this routine
       are both valid and within the proper range.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 01-JUL-1999 (EDW) (WMO)

-Index_Entries

   n-dimensional vector_transpose times matrix times vector

-&
*/

{ /* Begin vtmvg_c */


   /*
   Local macros

   We'd like to be able to refer to the elements of the input and output
   matrices using normal subscripts, for example, m1[2][3].  Since the
   compiler doesn't know how to compute index offsets for the array
   arguments, which have user-adjustable size, we must compute the
   offsets ourselves.  To make syntax a little easier to read (we hope),
   we'll use macros to do the computations.

   The macro INDEX(width, i,j) computes the index offset from the array
   base of the element at position [i][j] in a 2-dimensional matrix
   having the number of columns indicated by width.  For example, if
   the input matrix m1 has 2 rows and 3 columns, the element at position
   [0][1] would be indicated by

      m1[ INDEX(3,0,1) ]

   */

   #define INDEX( width, row, col )     ( (row)*(width) + (col) )


   /*
   Local variables
   */
   ConstSpiceDouble      * loc_v1;
   ConstSpiceDouble      * loc_m1;
   ConstSpiceDouble      * loc_v2;


   SpiceInt                k;
   SpiceInt                l;
   SpiceDouble             val = 0.;


   loc_v1 = ( ConstSpiceDouble * ) v1;
   loc_v2 = ( ConstSpiceDouble * ) v2;
   loc_m1 = ( ConstSpiceDouble * ) matrix;


   for ( k = 0; k < nrow; k++ )
      {
      for ( l = 0; l < ncol; l++ )
         {
         val += loc_v1[k] * loc_m1[ INDEX(ncol,k,l) ] * loc_v2[l];
         }
      }

   return val;

} /* End vtmvg_c */
