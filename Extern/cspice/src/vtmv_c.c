/*

-Procedure vtmv_c ( Vector transpose times matrix times vector, 3 dim )

-Abstract

   Multiply the transpose of a 3-dimensional column vector,
   a 3x3 matrix, and a 3-dimensional column vector.

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
   #undef    vtmv_c

   SpiceDouble vtmv_c ( ConstSpiceDouble v1     [3],
                        ConstSpiceDouble matrix [3][3],
                        ConstSpiceDouble v2     [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   3-dimensional double precision column vector.
   matrix     I   3x3 double precision matrix.
   v2         I   3-dimensional double precision column vector.

   The function returns the result of multiplying the transpose of
   `v1' by `matrix' by `v2'.

-Detailed_Input

   v1          is any double precision 3-dimensional column vector.

   matrix      is any double precision 3x3 matrix.

   v2          is any double precision 3-dimensional column vector.

-Detailed_Output

   The function returns the double precision value of the equation

        T
      v1  *  matrix * v2

   Notice that vtmv_c is actually the dot product of the vector
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

                 T
      vtmv_c = v1  * matrix * v2;

   `v1' is a column vector which becomes a row vector when transposed.
   `v2' is a column vector.

   No checking is performed to determine whether floating point
   overflow has occurred.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute the multiplication of the transpose of a 3-dimensional
      column vector, a 3x3 matrix, and a second 3-dimensional column
      vector.


      Example code begins here.


      /.
         Program vtmv_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceInt             i;

         /.
         Define `v1', `matrix' and `v2'.
         ./
         SpiceDouble          v1     [3]    = {   2.0,  4.0, 6.0 };
         SpiceDouble          matrix [3][3] = { { 0.0,  1.0, 0.0 },
                                                {-1.0,  0.0, 0.0 },
                                                { 0.0,  0.0, 1.0 } };
         SpiceDouble          v2     [3]    = {   1.0,  1.0, 1.0 };

         printf( "V1:\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf( "%6.1f\n", v1[i] );
         }

         printf( "\n" );
         printf( "MATRIX:\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf( "%6.1f %5.1f %5.1f\n",
                    matrix[i][0], matrix[i][1], matrix[i][2] );
         }

         printf( "\n" );
         printf( "V2:\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf( "%6.1f\n", v2[i] );
         }

         /.
         Compute the transpose of `v1' times `matrix' times `v2'.
         ./
         printf( "\n" );
         printf( "Transpose of V1 times MATRIX times V2: %5.1f\n",
                                               vtmv_c ( v1, matrix, v2 ) );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      V1:
         2.0
         4.0
         6.0

      MATRIX:
         0.0   1.0   0.0
        -1.0   0.0   0.0
         0.0   0.0   1.0

      V2:
         1.0
         1.0
         1.0

      Transpose of V1 times MATRIX times V2:   4.0


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 19-MAY-2020 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.0.0, 01-JUL-1999 (EDW) (WMO)

-Index_Entries

   3-dimensional vector_transpose times matrix times vector

-&
*/

{ /* Begin vtmv_c */


  /*
   Local variables
   */
   SpiceInt          k;
   SpiceInt          l;
   SpiceDouble       val = 0.;

   for ( k = 0; k < 3; k++ )
      {
      for ( l = 0; l < 3; l++ )
         {
         val += v1[k] * matrix[k][l] * v2[l];
         }
      }

   return val;

} /* End vtmv_c */
