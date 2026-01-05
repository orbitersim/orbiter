/*

-Procedure mxm_c ( Matrix times matrix, 3x3 )

-Abstract

   Multiply two 3x3 matrices.

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
   #undef    mxm_c


   void mxm_c ( ConstSpiceDouble   m1  [3][3],
                ConstSpiceDouble   m2  [3][3],
                SpiceDouble        mout[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m1         I   3x3 double precision matrix.
   m2         I   3x3 double precision matrix.
   mout       O   The 3x3 double precision matrix product m1*m2.

-Detailed_Input

   m1          is an arbitrary 3x3 double precision matrix.

   m2          is an arbitrary 3x3 double precision matrix.

-Detailed_Output

   mout        is a 3x3 double precision matrix. `mout' is the product
               m1*m2.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The code reflects precisely the following mathematical expression

      For each value of the subscripts `i' and `j' from 0 to 2:

                         2
                      .-----
                       \
         mout[i][j] =   )  m1[i][k] * m2[k][j]
                       /
                      '-----
                        k=0

   The intermediate results of the operation above are buffered in a
   temporary matrix which is later moved to the output matrix.
   Thus, to save space in the calling program, `mout' can be actually
   be `m1' or `m2' if desired without interfering with the computations.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given two 3x3 double precision matrices, compute their
      product.


      Example code begins here.


      /.
         Program mxm_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          mout   [3][3];

         SpiceInt             i;

         /.
         Define `m1' and `m2'.
         ./
         SpiceDouble          m1     [3][3] = { { 1.0,  1.0,  0.0},
                                                {-1.0,  1.0,  0.0},
                                                { 0.0,  0.0,  1.0} };

         SpiceDouble          m2     [3][3] = { { 1.0,  0.0,  0.0},
                                                { 0.0,  1.0,  1.0},
                                                { 0.0, -1.0,  1.0} };

         /.
         Compute `m1' times `m2'.
         ./
         mxm_c ( m1, m2, mout );

         printf( "M1:\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf( "%16.7f %15.7f %15.7f\n", m1[i][0], m1[i][1], m1[i][2] );
         }

         printf( "\n" );
         printf( "M2:\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf( "%16.7f %15.7f %15.7f\n", m2[i][0], m2[i][1], m2[i][2] );
         }

         printf( "\n" );
         printf( "M1 times M2:\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf( "%16.7f %15.7f %15.7f\n",
                    mout[i][0], mout[i][1], mout[i][2] );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      M1:
             1.0000000       1.0000000       0.0000000
            -1.0000000       1.0000000       0.0000000
             0.0000000       0.0000000       1.0000000

      M2:
             1.0000000       0.0000000       0.0000000
             0.0000000       1.0000000       1.0000000
             0.0000000      -1.0000000       1.0000000

      M1 times M2:
             1.0000000       1.0000000       1.0000000
            -1.0000000       1.0000000       1.0000000
             0.0000000      -1.0000000       1.0000000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 05-JUN-2020 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW)

-Index_Entries

   matrix times matrix 3x3_case

-&
*/

{ /* Begin mxm_c */

   /*
   Local variables
   */

   SpiceInt     i;
   SpiceInt     j;
   SpiceDouble  mtemp[3][3];


   for ( i = 0; i <= 2; ++i)
      {

      for ( j = 0; j <= 2; ++j)
         {
         mtemp[i][j] = m1[i][0] * m2[0][j] +
                       m1[i][1] * m2[1][j] +
                       m1[i][2] * m2[2][j];
         }

      }


   /*
   Copy the results from the temporary matrix to the return matrix.
   */
   MOVED ( mtemp, 9, mout );


} /* End mxm_c */
