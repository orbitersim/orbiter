/*

-Procedure mtxv_c ( Matrix transpose times vector, 3x3 )

-Abstract

   Multiply the transpose of a 3x3 matrix on the left with a vector
   on the right.

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
   #include "SpiceZmc.h"
   #undef    mtxv_c


   void mtxv_c ( ConstSpiceDouble     m   [3][3],
                 ConstSpiceDouble     vin [3],
                 SpiceDouble          vout[3]   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m          I   3x3 double precision matrix.
   vin        I   3-dimensional double precision vector.
   vout       O   3-dimensional double precision vector. `vout' is
                  the product m**t * vin.

-Detailed_Input

   m           is an arbitrary 3x3 double precision matrix.
               Typically, `m' will be a rotation matrix since
               then its transpose is its inverse (but this is NOT
               a requirement).

   vin         is an arbitrary 3-dimensional double precision
               vector.

-Detailed_Output

   vout        is a 3-dimensional double precision vector. `vout' is
               the product vout = (m**t)  x (vin). `vout' can
               overwrite `vin'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The code reflects precisely the following mathematical expression

      For each value of the subscript `i' from 0 to 2:

                      2
                   .-----
                    \
         vout(i) =   )  m[k][i] * vin[k]
                    /
                   '-----
                     k=0

   Note that the reversal of the `k' and `i' subscripts in the left-hand
   matrix `m' is what makes `vout' the product of the TRANSPOSE of
   and not simply of `m' itself.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given a 3x3 matrix and a 3-vector, multiply the transpose of
      the matrix by the vector.


      Example code begins here.


      /.
         Program mtxv_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          vout   [3];

         /.
         Define `m' and `vin'.
         ./
         SpiceDouble          m      [3][3] = { { 1.0,  1.0,  0.0},
                                                {-1.0,  1.0,  0.0},
                                                { 0.0,  0.0,  1.0} };

         SpiceDouble          vin    [3] = { 5.0,  10.0,  15.0 };

         /.
         Multiply the transpose of `m' by `vin'.
         ./
         mtxv_c ( m, vin, vout );

         printf( "Transpose of M times VIN:\n" );
         printf( "%10.3f %9.3f %9.3f\n", vout[0], vout[1], vout[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Transpose of M times VIN:
          -5.000    15.000    15.000


      Note that typically the matrix `m' will be a rotation matrix.
      Because the transpose of an orthogonal matrix is equivalent to
      its inverse, applying the rotation to the vector is
      accomplished by multiplying the vector by the transpose of the
      matrix.

      Let

             -1
            m   * vin = vout

      If `m' is an orthogonal matrix, then (m**T) * vin = vout.

-Restrictions

   1)  The user is responsible for checking the magnitudes of the
       elements of m and vin so that a floating point overflow does
       not occur.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 25-AUG-2021 (JDR)

       Changed input argument name "m1" to "m" for consistency with
       other routines.

       Edited the header to comply with NAIF standard. Added complete code
       example based on the existing example.

   -CSPICE Version 1.0.1, 10-NOV-2006 (EDW)

       Added -Parameters section header.

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW) (WMO)

-Index_Entries

   matrix_transpose times 3-dimensional vector

-&
*/

{ /* Begin mtxv_c */


   /*
   Local variables
   */

   SpiceInt                 i;
   SpiceDouble              vtemp[3];


   for ( i = 0; i <= 2; i++ )
      {
      vtemp[i] = m[0][i]*vin[0] + m[1][i]*vin[1] + m[2][i]*vin[2];
      }


   /* Move the computed result to the output array. */

   MOVED ( vtemp, 3, vout );


} /* End mtxv_c */
