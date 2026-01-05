/*

-Procedure invert_c ( Invert a 3x3 matrix )

-Abstract

   Generate the inverse of a 3x3 matrix.

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

   MATH
   MATRIX

*/
   #include <math.h>
   #include "SpiceUsr.h"
   #undef    invert_c


   void invert_c ( ConstSpiceDouble  m   [3][3],
                   SpiceDouble       mout[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m          I   Matrix to be inverted.
   mout       O   Inverted matrix (m)^-1.

-Detailed_Input

   m           is an arbitrary 3x3 matrix. The limits on the size of
               elements of `m' are determined by the process of calculating
               the cofactors of each element of the matrix. For a 3x3
               matrix this amounts to the differencing of two terms, each
               of which consists of the multiplication of two matrix
               elements. This multiplication must not exceed the range
               of double precision numbers or else an overflow error will
               occur.

-Detailed_Output

   mout        is the inverse of `m' and is calculated explicitly using
               the matrix of cofactors. `mout' is set to be the zero matrix
               if `m' is singular.

               `mout' can overwrite `m'.

-Parameters

   None.

-Exceptions

   1)  No internal checking on the input matrix `m' is performed except
       on the size of its determinant. Thus it is possible to generate a
       floating point overflow or underflow in the process of
       calculating the matrix of cofactors.

   2)  If the determinant is less than 10**-16, the matrix is deemed to
       be singular and the output matrix is filled with zeros.

-Files

   None.

-Particulars

   A temporary matrix is used to compute the result, so the output
   matrix may overwrite the input matrix.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given a double precision 3x3 matrix, compute its inverse. Check
      that the original matrix times the computed inverse produces
      the identity matrix.


      Example code begins here.


      /.
         Program invert_ex1
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
         invert_c ( m, mout );

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
             0.0000000       2.0000000      -0.0000000
            -1.0000000       0.0000000      -0.0000000
             0.0000000      -0.0000000       1.0000000

      Original times inverse:
             1.0000000       0.0000000       0.0000000
             0.0000000       1.0000000       0.0000000
             0.0000000       0.0000000       1.0000000


-Restrictions

   1)  The input matrix must be such that generating the cofactors will
       not cause a floating point overflow or underflow. The
       strictness of this condition depends, of course, on the computer
       installation and the resultant maximum and minimum values of
       double precision numbers.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)

-Version

   -CSPICE Version 1.1.0, 06-JUL-2021 (JDR)

       Changed input argument name "m1" to "m" for consistency with
       other routines.

       Updated the header to comply with NAIF standard. Added
       complete code example.

   -CSPICE Version 1.0.0, 13-SEP-1999 (NJB) (WMO)

-Index_Entries

   invert a 3x3_matrix

-&
*/

{ /* Begin invert_c */

   /*
   Local constants
   */

   #define  SINGULAR_DET     1.e-16


   /*
   Local variables
   */
   SpiceInt                i;

   SpiceDouble             invdet;
   SpiceDouble             mdet;
   SpiceDouble             mtemp[3][3];


   /*
   Find the determinant of m and check for singularity.
   */

   mdet = det_c(m);

   if ( fabs(mdet) < SINGULAR_DET )
   {

      /*
      The matrix is considered to be singular.
      */

      for ( i = 0;  i < 9;  i++ )
      {
         *( (SpiceDouble*)mout+i ) = 0.;
      }

      return;
   }


   /*
   Get the cofactors of each element of m.
   */
   mtemp[0][0] =  ( m[1][1]*m[2][2] - m[2][1]*m[1][2] );
   mtemp[0][1] = -( m[0][1]*m[2][2] - m[2][1]*m[0][2] );
   mtemp[0][2] =  ( m[0][1]*m[1][2] - m[1][1]*m[0][2] );
   mtemp[1][0] = -( m[1][0]*m[2][2] - m[2][0]*m[1][2] );
   mtemp[1][1] =  ( m[0][0]*m[2][2] - m[2][0]*m[0][2] );
   mtemp[1][2] = -( m[0][0]*m[1][2] - m[1][0]*m[0][2] );
   mtemp[2][0] =  ( m[1][0]*m[2][1] - m[2][0]*m[1][1] );
   mtemp[2][1] = -( m[0][0]*m[2][1] - m[2][0]*m[0][1] );
   mtemp[2][2] =  ( m[0][0]*m[1][1] - m[1][0]*m[0][1] );

   /*
   Multiply the cofactor matrix by 1/mdet to obtain the inverse matrix.
   */

   invdet = 1. / mdet;

   vsclg_c ( invdet, (SpiceDouble *)mtemp, 9, (SpiceDouble *)mout );


} /* End invert_c */
