/*

-Procedure unormg_c ( Unit vector and norm, general dimension )

-Abstract

   Normalize a double precision vector of arbitrary dimension and
   return its magnitude.

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

   VECTOR

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef    unormg_c

   void unormg_c ( ConstSpiceDouble    v1     [],
                   SpiceInt            ndim,
                   SpiceDouble         vout   [],
                   SpiceDouble       * vmag    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   Vector to be normalized.
   ndim       I   Dimension of `v1' (and also `vout').
   vout       O   Unit vector v1 / ||v1||.
   vmag       O   Magnitude of `v1', i.e. ||v1||.

-Detailed_Input

   v1          is an arbitrary double precision n-dimensional vector,
               including the zero vector.

   ndim        is the dimension of `v1' and `vout'.

-Detailed_Output

   vout        is the double precision n-dimensional unit vector in the
               direction of `v1'. If `v1' is the zero vector, then `vout'
               will also be the zero vector.

   vmag        is the magnitude of `v1'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   unormg_c references a function called vnormg_c (which itself is
   numerically stable) to calculate the norm of the input vector `v1'.
   If the norm is equal to zero, then each component of the output
   vector `vout' is set to zero. Otherwise, `vout' is calculated by
   dividing `v1' by the norm. No error detection or correction is
   implemented.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define a set of n-dimensional vectors and compute their
      corresponding unit vectors and magnitudes.


      Example code begins here.


      /.
         Program unormg_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NDIM         4
         #define SETSIZ       2

         /.
         Local variables.
         ./
         SpiceDouble          vmag;
         SpiceDouble          vout   [NDIM];

         SpiceInt             i;

         /.
         Define the vector set.
         ./
         SpiceDouble          v1     [SETSIZ][NDIM] = {
                                   { 5.0,   12.0,   0.0,   4.0 },
                                   { 1.e-6,  2.e-6, 2.e-6, 0.0 } };

         /.
         Calculate the unit vectors and magnitudes.
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {
            unormg_c ( v1[i], NDIM, vout, &vmag );

            printf( "Vector     : %11.7f %11.7f %11.7f %11.7f\n",
                           v1[i][0], v1[i][1], v1[i][2], v1[i][3] );
            printf( "Unit vector: %11.7f %11.7f %11.7f %11.7f\n",
                               vout[0], vout[1], vout[2], vout[3] );
            printf( "Magnitude  : %11.7f\n", vmag );
            printf( "\n" );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Vector     :   5.0000000  12.0000000   0.0000000   4.0000000
      Unit vector:   0.3676073   0.8822575   0.0000000   0.2940858
      Magnitude  :  13.6014705

      Vector     :   0.0000010   0.0000020   0.0000020   0.0000000
      Unit vector:   0.3333333   0.6666667   0.6666667   0.0000000
      Magnitude  :   0.0000030


-Restrictions

   1)  No error checking is implemented in this function to guard
       against numeric overflow.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.3.0, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

       Updated wrapper to call f2c'ed SPICELIB version.

   -CSPICE Version 1.2.0, 06-FEB-2017 (EDW)

       Bug fix: eliminated spurious semi-colon on "for(...)" line.
       This caused the output vector not to be set when the input
       argument `v1' was the zero vector.

       Corrected section order.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input vector const. Converted check-in style to discovery.

   -CSPICE Version 1.0.0, 31-MAR-1998 (EDW)

-Index_Entries

   n-dimensional unit vector and norm

-&
*/

{ /* Begin unormg_c */

   /*
   Error free:  no error tracing required.
   */

   /*
   Call the f2c'd Fortran routine.
   */
   unormg_ (  ( doublereal * )  v1,
              ( integer    * ) &ndim,
              ( doublereal * )  vout,
              ( doublereal * )  vmag   );

} /* End unormg_c */
