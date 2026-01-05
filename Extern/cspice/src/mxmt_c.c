/*

-Procedure mxmt_c ( Matrix times matrix transpose, 3x3 )

-Abstract

   Multiply a 3x3 matrix and the transpose of another 3x3 matrix.

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
   #undef    mxmt_c


   void mxmt_c ( ConstSpiceDouble    m1  [3][3],
                 ConstSpiceDouble    m2  [3][3],
                 SpiceDouble         mout[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m1         I   3x3 double precision matrix.
   m2         I   3x3 double precision matrix.
   mout       O   The product `m1' times transpose of `m2'.

-Detailed_Input

   m1          is an arbitrary 3x3 double precision matrix.

   m2          is an arbitrary 3x3 double precision matrix.
               Typically, `m2' will be a rotation matrix since
               then its transpose is its inverse (but this is
               NOT a requirement).

-Detailed_Output

   mout        is a 3x3 double precision matrix. `mout' is the product

                                T
                  mout = m1 x m2

               `mout' may overwrite either `m1' or `m2'.

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
         mout[i][j] =   )  m1[i][k] * m2[j][k]
                       /
                      '-----
                        k=0

   Note that the reversal of the `k' and `j' subscripts in the right-
   hand matrix `m2' is what makes `mout' the product of the TRANSPOSE of
   `m2' and not simply of `m2' itself. Also, the intermediate results of
   the operation above are buffered in a temporary matrix which is
   later moved to the output matrix. Thus mout can be actually be
   m1 or m2 if desired without interfering with the computations.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given two 3x3 double precision matrices, multiply the first
      matrix by the transpose of the second one.


      Example code begins here.


      /.
         Program mxmt_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          m2     [3][3];
         SpiceDouble          mout   [3][3];

         SpiceInt             i;

         /.
         Define `m1'.
         ./
         SpiceDouble          m1     [3][3] = { { 0.0,  1.0,  0.0},
                                                {-1.0,  0.0,  0.0},
                                                { 0.0,  0.0,  1.0} };

         /.
         Make `m2' equal to `m1'.
         ./
         mequ_c ( m1, m2 );

         /.
         Multiply `m1' by the transpose of `m2'.
         ./
         mxmt_c ( m1, m2, mout );

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
         printf( "M1 times transpose of M2:\n" );
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
             0.0000000       1.0000000       0.0000000
            -1.0000000       0.0000000       0.0000000
             0.0000000       0.0000000       1.0000000

      M2:
             0.0000000       1.0000000       0.0000000
            -1.0000000       0.0000000       0.0000000
             0.0000000       0.0000000       1.0000000

      M1 times transpose of M2:
             1.0000000       0.0000000       0.0000000
             0.0000000       1.0000000       0.0000000
             0.0000000       0.0000000       1.0000000


-Restrictions

   1)  The user is responsible for checking the magnitudes of the
       elements of `m1' and `m2' so that a floating point overflow does
       not occur. (In the typical use where `m1' and `m2' are rotation
       matrices, this not a risk at all.)

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 04-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code examples based on existing code fragments.

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW) (WMO)

-Index_Entries

   matrix times matrix_transpose 3x3_case

-&
*/

{ /* Begin mxmt_c */

   /*
   Local variables
   */

   SpiceInt                i;
   SpiceInt                j;

   SpiceDouble             mtemp[3][3];


   for ( i = 0; i < 3;  i++ )
      {

      for ( j = 0; j < 3;  j++ )
         {
         mtemp[i][j]  =     m1[i][0] * m2[j][0]
                         +  m1[i][1] * m2[j][1]
                         +  m1[i][2] * m2[j][2];
         }
      }


   /*
   Copy the results from the temporary matrix to the return matrix.
   */

   MOVED ( mtemp, 9, mout );


} /* End mxmt_c */
