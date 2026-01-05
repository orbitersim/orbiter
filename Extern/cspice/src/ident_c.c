/*

-Procedure ident_c ( Return the 3x3 identity matrix )

-Abstract

   Return the 3x3 identity matrix.

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
   #include "SpiceZmc.h"

   void ident_c ( SpiceDouble    matrix[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   matrix     O   The 3x3 identity matrix.

-Detailed_Input

   None.

-Detailed_Output

   matrix      is the 3x3 Identity matrix. That `matrix' is
               the following

                  .-                 -.
                  |  1.0   0.0   0.0  |
                  |  0.0   1.0   0.0  |
                  |  0.0   0.0   1.0  |
                  `-                 -'

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This is a utility routine for obtaining the 3x3 identity matrix
   so that you may avoid having to write the loop or assignments
   needed to get the matrix.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define a 3x3 matrix and compute its inverse using the CSPICE
      routine invert_c. Verify the accuracy of the computed inverse
      using the mathematical identity

              -1
         m x m   - i = 0

      where `i' is the 3x3 identity matrix.


      Example code begins here.


      /.
         Program ident_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          idmat  [3][3];
         SpiceDouble          imat   [3][3];
         SpiceDouble          mout   [3][3];
         SpiceDouble          mzero  [3][3];

         SpiceInt             i;

         /.
         Define a matrix to invert.
         ./
         SpiceDouble          m      [3][3] = { {0.0,  -1.0, 0.0},
                                                {0.5,   0.0, 0.0},
                                                {0.0,   0.0, 1.0} };

         printf( "Original Matrix:\n" );
         for ( i = 0; i < 3; i++ )
         {

            printf( "%16.7f %15.7f %15.7f\n", m[i][0], m[i][1], m[i][2] );

         }

         /.
         Invert the matrix, then output.
         ./
         invert_c ( m, mout );

         printf( "\n" );
         printf( "Inverse Matrix:\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf( "%16.7f %15.7f %15.7f\n",
                    mout[i][0], mout[i][1], mout[i][2] );
         }

         /.
         Check the `m' times `mout' produces the identity matrix.
         ./
         ident_c ( idmat );
         mxm_c   ( m, mout, imat );

         vsubg_c ( (SpiceDouble *)imat, (SpiceDouble *)idmat,
                    9,                  (SpiceDouble *)mzero );

         printf( "\n" );
         printf( "Original times inverse minus identity:\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf( "%16.7f %15.7f %15.7f\n",
                    mzero[i][0], mzero[i][1], mzero[i][2] );
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

      Original times inverse minus identity:
             0.0000000       0.0000000       0.0000000
             0.0000000       0.0000000       0.0000000
             0.0000000       0.0000000       0.0000000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 02-JUN-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

   -CSPICE Version 1.0.0, 01-JUN-1999 (NJB) (WLT)

-Index_Entries

   Get the 3x3 identity matrix

-&
*/

{ /* Begin ident_c */


   matrix[0][0] =  1.0;
   matrix[0][1] =  0.0;
   matrix[0][2] =  0.0;
   matrix[1][0] =  0.0;
   matrix[1][1] =  1.0;
   matrix[1][2] =  0.0;
   matrix[2][0] =  0.0;
   matrix[2][1] =  0.0;
   matrix[2][2] =  1.0;

} /* End ident_c */
