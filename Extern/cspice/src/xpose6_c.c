/*

-Procedure xpose6_c ( Transpose a matrix, 6x6 )

-Abstract

   Transpose a 6x6 matrix.

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
   #undef    xpose6_c


   void xpose6_c ( ConstSpiceDouble  m1     [6][6],
                   SpiceDouble       mout   [6][6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m1         I   6x6 matrix to be transposed.
   mout       O   Transpose of `m1'.

-Detailed_Input

   m1          is any double precision 6x6 matrix.

-Detailed_Output

   mout        is a double precision, 6x6 matrix which contains the
               transpose of `m1'.  `mout' may overwrite `m1'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This is a utility routine intended to facilitate passing state
   transformation matrices between C and Fortran.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given a 6x6 double precision matrix, find its transpose.


      Example code begins here.


      /.
         Program xpose6_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          mout   [6][6];

         SpiceInt             i;

         /.
         Define the input matrix.
         ./
         SpiceDouble          m1     [6][6] = {
                                    { 1.0,  2.0,  3.0,  4.0,  5.0,  6.0},
                                    { 0.0,  7.0,  8.0,  9.0, 10.0, 11.0},
                                    { 0.0,  0.0, 12.0, 13.0, 14.0, 15.0},
                                    { 0.0,  0.0,  0.0, 16.0, 17.0, 18.0},
                                    { 0.0,  0.0,  0.0,  0.0, 19.0, 20.0},
                                    { 0.0,  0.0,  0.0,  0.0,  0.0, 21.0}  };


         /.
         Compute the transpose of `m1'.
         ./
         xpose6_c ( m1, mout );


         /.
         Display the results.
         ./
         printf( "Input matrix (M1):\n" );
         printf( "\n" );
         for ( i = 0; i < 6; i++ )
         {
            printf( "%6.1f %5.1f %5.1f %5.1f %5.1f %5.1f\n",
                               m1[i][0], m1[i][1], m1[i][2],
                               m1[i][3], m1[i][4], m1[i][5] );
         }
         printf( "\n" );
         printf( "Transpose of M1:\n" );
         printf( "\n" );
         for ( i = 0; i < 6; i++ )
         {
            printf( "%6.1f %5.1f %5.1f %5.1f %5.1f %5.1f\n",
                         mout[i][0], mout[i][1], mout[i][2],
                         mout[i][3], mout[i][4], mout[i][5] );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input matrix (M1):

         1.0   2.0   3.0   4.0   5.0   6.0
         0.0   7.0   8.0   9.0  10.0  11.0
         0.0   0.0  12.0  13.0  14.0  15.0
         0.0   0.0   0.0  16.0  17.0  18.0
         0.0   0.0   0.0   0.0  19.0  20.0
         0.0   0.0   0.0   0.0   0.0  21.0

      Transpose of M1:

         1.0   0.0   0.0   0.0   0.0   0.0
         2.0   7.0   0.0   0.0   0.0   0.0
         3.0   8.0  12.0   0.0   0.0   0.0
         4.0   9.0  13.0  16.0   0.0   0.0
         5.0  10.0  14.0  17.0  19.0   0.0
         6.0  11.0  15.0  18.0  20.0  21.0


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.4, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.0.3, 08-JAN-2014 (BVS)

       Corrected a minor typo in the header.

   -CSPICE Version 1.0.2, 16-JAN-2008 (EDW)

       Corrected typos in header titles:

       Detailed Input to -Detailed_Input
       Detailed Output to -Detailed_Output

   -CSPICE Version 1.0.1, 10-NOV-2006 (EDW)

       Added -Keywords and -Parameters section headers.
       Reordered section headers.

   -CSPICE Version 1.0.0, 16-APR-1999 (NJB)

-Index_Entries

   transpose a 6x6_matrix

-&
*/

{  /* Begin xpose6_c */


   /*
   Local constants
   */
   #define   SIZE          6
   #define   SIZESQ        36

   /*
   Local variables
   */
   SpiceInt                col;
   SpiceInt                row;

   SpiceDouble             temp[SIZE][SIZE];


   /*
   Capture a temporary copy of the input matrix.
   */
   MOVED ( m1, SIZESQ, temp );

   /*
   Move the temporary matrix to the output matrix, transposing as
   we go.
   */
   for ( row = 0;  row < SIZE;  row++ )
   {
      for ( col = 0;  col < SIZE;  col++ )
      {
         mout[row][col] = temp[col][row];
      }
   }

} /* End xpose6_c */
