/*

-Procedure xposeg_c ( Transpose a matrix, general )

-Abstract

   Transpose a matrix of arbitrary size (in place, the matrix
   need not be square).

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

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZim.h"
   #undef    xposeg_c


   void xposeg_c ( const void   * matrix,
                   SpiceInt       nrow,
                   SpiceInt       ncol,
                   void         * xposem )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   matrix     I   Matrix to be transposed.
   nrow       I   Number of rows of input matrix `matrix'.
   ncol       I   Number of columns of input matrix `matrix'.
   xposem     O   Transposed matrix.

-Detailed_Input

   matrix      is a matrix to be transposed.

   nrow        is the number of rows of input matrix `matrix'.

   ncol        is the number of columns of input matrix `matrix'.

-Detailed_Output

   xposem      is the transposed matrix.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If either `nrow' or `ncol' is less than or equal to zero, no
       action is taken. The routine simply returns.

-Files

   None.

-Particulars

   This routine transposes the input matrix and writes the
   result to the matrix `xposem'. This algorithm is performed in
   such a way that the transpose can be performed in place. That
   is, `matrix' and `xposem' can use the same storage area in memory.

   NOTE:  The matrices `matrix' and `xposem' are declared
          one-dimensional for computational purposes only. The
          calling program should declare them as matrix[nrow][ncol]
          and xposem[ncol][nrow].

          This routine works on the assumption that the input and
          output matrices are defined as described above. More
          specifically it assumes that the elements of the matrix
          to be transformed is stored in contiguous memory locations
          as shown here. On output these elements will be
          rearranged in consecutive memory locations as shown.


             matrix                xposem

             m[0][0]               m[0][0]
             m[0][1]               m[1][0]
             m[0][2]               m[2][0]
             .                     .
             .                     .
             m[0][ncol-1]          .
             m[1][0]               m[nrow-1][0]
             m[1][1]               m[0][1]
             m[1][2]               m[1][1]
             .                     m[2][1]
             .                     .
             m[1][ncol-1]          .
             .                     .
                                   m[nrow-1][1]
             .                     .

             .                     .

             .                     .
                                   m[0][ncol-1]
             m[nrow-1][0]          m[1][ncol-1]
             m[nrow-1][1]          m[2][ncol-1]
             m[nrow-1][2]          .
             .                     .
             .                     .
             m[nrow-1][ncol-1]     m[nrow-1][ncol-1]


   For those familiar with permutations, this algorithm relies upon the
   fact that the transposition of a matrix, which has been stored as a
   string, is simply the action of a permutation applied to that
   string. Since any permutation can be decomposed as a product of
   disjoint cycles, it is possible to transpose the matrix with only
   one additional storage register. However, once a cycle has been
   computed it is necessary to find the next entry in the string that
   has not been moved by the permutation. For this reason the
   algorithm is slower than would be necessary if the numbers of rows
   and columns were known in advance.

-Examples

   This routine is primarily useful when attempting to transpose large
   matrices, where inplace transposition is important. For example
   suppose you have the following declarations

      SpiceDouble           matrix [1003][800];

   If the transpose of the matrix is needed, it may not be possible to
   fit a second matrix requiring the same storage into memory. Instead
   declare xposem as below so that no additional memory is allocated.

      SpiceDouble        (* xposem) [1003]  =  matrix;

   To obtain the transpose simply execute

      xposeg_c ( matrix, 1003, 800, xposem );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.1.1, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.1.0, 24-JUL-2001 (NJB)

       Changed prototype: input matrix is now type (const void *).
       Implemented interface macro for casting input matrix to const.

   -CSPICE Version 1.0.0, 31-MAY-1999 (NJB) (WLT)

-Index_Entries

   transpose a matrix general

-&
*/

{ /* Begin xposeg_c */


   /*
   Error free.
   */

   /*
   The matrix looks to the f2c'd routine xposeg_ as though it has
   ncol rows and nrow columns.  xposeg_ will do a perfectly good job
   of transposing it if told that these are the dimensions of the input
   matrix.
   */

   xposeg_  (  ( doublereal  * ) matrix,
               ( integer     * ) &ncol,
               ( integer     * ) &nrow,
               ( doublereal  * ) xposem );


} /* End xposeg_c */
