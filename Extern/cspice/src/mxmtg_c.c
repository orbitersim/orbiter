/*

-Procedure mxmtg_c ( Matrix times matrix transpose, general dimension )

-Abstract

   Multiply a matrix and the transpose of a matrix, both of
   arbitrary size.

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
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    mxmtg_c


   void mxmtg_c ( const void   * m1,
                  const void   * m2,
                  SpiceInt       nr1,
                  SpiceInt       nc1c2,
                  SpiceInt       nr2,
                  void         * mout  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m1         I   Left-hand matrix to be multiplied.
   m2         I   Right-hand matrix whose transpose is to be multiplied.
   nr1        I   Row dimension of `m1' and row dimension of `mout'.
   nc1c2      I   Column dimension of `m1' and column dimension of `m2'.
   nr2        I   Row dimension of `m2' and column dimension of `mout'.
   mout       O   Product matrix.

-Detailed_Input

   m1          is a double precision matrix of arbitrary size.

   m2          is a double precision matrix of arbitrary size.
               The number of columns in `m2' must match the number of
               columns in `m1'.

   nr1         is the number of rows in both `m1' and `mout'.

   nc1c2       is the number of columns in `m1' and (by necessity) the
               number of columns of `m2'.

   nr2         is the number of rows in both `m2' and the number of
               columns in `mout'.

-Detailed_Output

   mout        is the product matrix given by

                                    t
                  mout = (m1) x (m2)


               where the superscript `t' denotes the transpose matrix.
               This is a double precision matrix of dimension nr1 x nr2.

               `mout' may overwrite `m1' or `m2'.

-Parameters

   None.

-Exceptions

   1)  If memory cannot be allocated to create the temporary matrix
       required for the execution of the routine, the error
       SPICE(MALLOCFAILED) is signaled.

-Files

   None.

-Particulars

   The code reflects precisely the following mathematical expression:

   For each value of the subscript `i' from 1 to `nr1', and `j' from 1
   to `nr2':

      mout(i,j) = summation from k=1 to nc1c2 of  ( m1(i,k) * m2(j,k) )

   Notice that the order of the subscripts of `m2' are reversed from
   what they would be if this routine merely multiplied `m1' and `m2'.
   It is this transposition of subscripts that makes this routine
   multiply `m1' and the TRANPOSE of `m2'.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given a 2x3 and a 3x4 matrices, multiply the first matrix by
      the transpose of the second one.


      Example code begins here.


      /.
         Program mxmtg_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          mout   [2][4];

         SpiceInt             i;

         /.
         Define `m1' and `m2'.
         ./
         SpiceDouble          m1     [2][3] = { {1.0,  2.0,  3.0},
                                                {3.0,  2.0,  1.0} };

         SpiceDouble          m2     [4][3] = { { 1.0,  2.0,  0.0},
                                                { 2.0,  1.0,  2.0},
                                                { 1.0,  2.0,  0.0},
                                                { 2.0,  1.0,  2.0} };

         /.
         Multiply `m1' by the transpose of `m2'.
         ./
         mxmtg_c ( m1, m2, 2, 3, 4, mout );

         printf( "M1 times transpose of M2:\n" );
         for ( i = 0; i < 2; i++ )
         {
            printf( "%10.3f %9.3f %9.3f %9.3f\n",
                    mout[i][0], mout[i][1], mout[i][2], mout[i][3] );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      M1 times transpose of M2:
           5.000    10.000     5.000    10.000
           7.000    10.000     7.000    10.000


-Restrictions

   1)  No error checking is performed to prevent numeric overflow or
       underflow.

       The user is responsible for checking the magnitudes of the
       elements of `m1' and `m2' so that a floating point overflow does
       not occur.

   2)  No error checking is performed to determine if the input and
       output matrices have, in fact, been correctly dimensioned.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.3.0, 06-AUG-2021 (JDR)

       Changed the input argument names "nrow1" and "nrow2" to "nr1"
       and "nr2" for consistency with other routines.

       Updated short error message for consistency within CSPICE wrapper
       interface: MEMALLOCFAILED -> MALLOCFAILED.

       Edited the header to comply with NAIF standard.
       Added complete code example based on the existing example.

   -CSPICE Version 1.2.0, 28-AUG-2001 (NJB)

       Const-qualified input arrays.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       Corrected a comment describing the local macro INDEX. Made
       miscellaneous code format corrections.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB) (WMO) (WLT)

       Based on SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)

-Index_Entries

   matrix times matrix_transpose n-dimensional_case

-&
*/

{ /* Begin mxmtg_c */

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
   SpiceDouble            innerProduct;
   SpiceDouble            *tmpmat;
   SpiceDouble            *loc_m1;
   SpiceDouble            *loc_m2;

   SpiceInt                col;
   SpiceInt                nelts;

   SpiceInt                row;
   SpiceInt                i;

   size_t                  size;


   /*
   Allocate space for a temporary copy of the output matrix, which
   has nr1 rows and nc1c2 columns.
   */
   nelts    =  nr1 * nr2;

   size     =  (size_t) ( nelts * sizeof(SpiceDouble) );

   tmpmat   =  (SpiceDouble *) malloc ( size );

   if ( tmpmat == (SpiceDouble *)0 )
   {
      chkin_c  ( "mxmtg_c"                                         );
      setmsg_c ( "An attempt to create a temporary matrix failed." );
      sigerr_c ( "SPICE(MALLOCFAILED)"                             );
      chkout_c ( "mxmtg_c"                                         );
      return;
   }

   /*
   Cast the input pointers to pointers to SpiceDoubles.  Note:  the
   original variables are pointers to void so that callers may
   supply the array names as arguments without casting them to
   SpiceDoubles.  The naked array name is considered by the compiler
   to be an incompatible pointer type with (SpiceDouble *), so we
   can't simply declare the arguments to be (SpiceDouble *).  On the
   other hand, every pointer type can be cast to (void *).
   */

   loc_m1 = (SpiceDouble *) m1;
   loc_m2 = (SpiceDouble *) m2;


   /*
   Compute the product.  The matrix element at position (row,col) is
   the inner product of the row of m1 having index row and the
   row of m2 having index col.  We compute index offsets using
   the macro INDEX.
   */

   for ( row = 0;  row < nr1;  row++ )
   {

      for ( col = 0;  col < nr2;  col++ )
      {
         innerProduct = 0.0;

         for ( i = 0;  i < nc1c2;  i++ )
         {
            innerProduct  +=    loc_m1[ INDEX(nc1c2, row, i) ]
                              * loc_m2[ INDEX(nc1c2, col, i) ];
         }

         tmpmat [ INDEX( nr2, row, col ) ]  =  innerProduct;
      }
   }

   /*
   Move the result from tmpmat into mout.
   */
   MOVED ( tmpmat, nelts, mout );

   /*
   Free the temporary matrix.
   */
   free ( tmpmat );


} /* End mxmtg_c */
