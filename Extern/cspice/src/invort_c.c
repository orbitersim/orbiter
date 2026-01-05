/*

-Procedure invort_c ( Invert nearly orthogonal matrices )

-Abstract

   Construct the inverse of a 3x3 matrix with orthogonal columns and
   non-zero column norms using a numerically stable algorithm. The
   rows of the output matrix are the columns of the input matrix
   divided by the length squared of the corresponding columns.

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
   #undef    invort_c


   void invort_c ( ConstSpiceDouble   m  [3][3],
                   SpiceDouble        mit[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m          I   A 3x3 matrix.
   mit        O   m after transposition and scaling of rows.

-Detailed_Input

   m           is a 3x3 matrix.

-Detailed_Output

   mit         is the matrix obtained by transposing m and dividing
               the rows by squares of their norms.

-Parameters

   None.

-Exceptions

   1)  If any of the columns of `m' have zero length, the error
       SPICE(ZEROLENGTHCOLUMN) is signaled by a routine in the call
       tree of this routine.

   2)  If any column is too short to allow computation of the
       reciprocal of its length without causing a floating point
       overflow, the error SPICE(COLUMNTOOSMALL) is signaled by a
       routine in the call tree of this routine.

-Files

   None.

-Particulars

   Suppose that m is the matrix

          .-                      -.
          |   A*u    B*v     C*w   |
          |      1      1       1  |
          |                        |
          |   A*u    B*v     C*w   |
          |      2      2       2  |
          |                        |
          |   A*u    B*v     C*w   |
          |      3      3       3  |
          `-                      -'

   where the vectors (u , u , u ),  (v , v , v ),  and (w , w , w )
                       1   2   3      1   2   3          1   2   3

   are unit vectors. This routine produces the matrix:


          .-                      -.
          |   a*u    a*u     a*u   |
          |      1      2       3  |
          |                        |
          |   b*v    b*v     b*v   |
          |      1      2       3  |
          |                        |
          |   c*w    c*w     c*w   |
          |      1      2       3  |
          `-                      -'

   where a = 1/A, b = 1/B, and c = 1/C.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given a double precision 3x3 matrix with mutually orthogonal
      rows of arbitrary length, compute its inverse. Check that the
      original matrix times the computed inverse produces the
      identity matrix.

      Example code begins here.


      /.
         Program invort_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          imat   [3][3];
         SpiceDouble          mout   [3][3];

         SpiceInt             i;

         /.
         Define a matrix to invert.
         ./
         SpiceDouble          m      [3][3] = { {0.0, -1.0, 0.0},
                                                {0.5,  0.0, 0.0},
                                                {0.0,  0.0, 1.0} };

         printf( "Original Matrix:\n" );
         for ( i = 0; i < 3; i++ )
         {

            printf( "%16.7f %15.7f %15.7f\n", m[i][0], m[i][1], m[i][2] );

         }

         /.
         Invert the matrix, then output.
         ./
         invort_c ( m, mout );

         printf( " \n" );
         printf( "Inverse Matrix:\n" );
         for ( i = 0; i < 3; i++ )
         {

            printf( "%16.7f %15.7f %15.7f\n",
                    mout[i][0], mout[i][1], mout[i][2] );

         }

         /.
         Check the `m' times `mout' produces the identity matrix.
         ./
         mxm_c ( m, mout, imat );

         printf( " \n" );
         printf( "Original times inverse:\n" );
         for ( i = 0; i < 3; i++ )
         {

            printf( "%16.7f %15.7f %15.7f\n",
                    imat[i][0], imat[i][1], imat[i][2] );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Original Matrix:
             0.0000000      -1.0000000       0.0000000
             0.5000000       0.0000000       0.0000000
             0.0000000       0.0000000       1.0000000

      Inverse Matrix:
             0.0000000       2.0000000       0.0000000
            -1.0000000       0.0000000       0.0000000
             0.0000000       0.0000000       1.0000000

      Original times inverse:
             1.0000000       0.0000000       0.0000000
             0.0000000       1.0000000       0.0000000
             0.0000000       0.0000000       1.0000000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Fixed I/O type
       of argument "mit" in -Brief_I/O table. Extended -Abstract
       section.

       Added complete code example.

   -CSPICE Version 1.0.0, 02-JAN-2002 (WLT) (NJB)

-Index_Entries

   Transpose a matrix and invert the lengths of the rows
   Invert a pseudo orthogonal matrix

-&
*/

{ /* Begin invort_c */

   /*
   Local variables
   */
   SpiceDouble             temp[3][3];


   /*
   Participate in error tracing.
   */
   chkin_c ( "invort_c" );

   /*
   Transpose the input matrix to obtain a Fortran-style matrix.
   */
   xpose_c ( m, temp );

   invort_ ( (SpiceDouble * )temp,
             (SpiceDouble * )mit  );

   /*
   Transpose the output matrix to obtain a C-style matrix.
   */
   xpose_c ( mit, mit );


   chkout_c ( "invort_c" );

} /* End invort_c */
