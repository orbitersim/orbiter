/*

-Procedure mxv_c ( Matrix times vector, 3x3 )

-Abstract

   Multiply a 3x3 double precision matrix with a 3-dimensional
   double precision vector.

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
   #undef    mxv_c


   void mxv_c ( ConstSpiceDouble    m   [3][3],
                ConstSpiceDouble    vin [3],
                SpiceDouble         vout[3]    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m          I   3x3 double precision matrix.
   vin        I   3-dimensional double precision vector.
   vout       O   3-dimensional double precision vector. `vout' is
                  the product m*vin.

-Detailed_Input

   m           is an arbitrary 3x3 double precision matrix.

   vin         is an arbitrary 3-dimensional double precision vector.

-Detailed_Output

   vout        is a 3-dimensional double precision vector. `vout' is
               the product m * v. `vout' may overwrite `vin'.

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
         vout[i] =   )  m[i][k] * vin[k]
                    /
                   '-----
                     k=0

   The intermediate results of the operation performed by this routine
   are buffered in a temporary vector which is later moved to the output
   vector.  Thus `vout' can be actually be `vin' if desired without
   interfering with the computation.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given a 3x3 matrix and a 3-vector, multiply the matrix by
      the vector.


      Example code begins here.


      /.
         Program mxv_ex1
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
         SpiceDouble          m      [3][3] = { { 0.0,  1.0,  0.0},
                                                {-1.0,  0.0,  0.0},
                                                { 0.0,  0.0,  1.0} };

         SpiceDouble          vin    [3] = { 1.0,  2.0,  3.0 };

         /.
         Multiply `m' by `vin'.
         ./
         mxv_c ( m, vin, vout );

         printf( "M times VIN:\n" );
         printf( "%10.3f %9.3f %9.3f\n", vout[0], vout[1], vout[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      M times VIN:
           2.000    -1.000     3.000


-Restrictions

   1)  The user is responsible for checking the magnitudes of the
       elements of `m' and `vin' so that a floating point overflow does
       not occur.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 13-AUG-2021 (JDR)

       Changed input argument name "m1" to "m" for consistency with
       other routines.

       Edited the header to comply with NAIF standard. Added complete code
       example based on the existing example. Updated -Particulars section.

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW)

-Index_Entries

   matrix times 3-dimensional vector

-&
*/

{ /* Begin mxv_c */

   /*
   Local variables
   */

   SpiceInt                 i;
   SpiceDouble              vtemp[3];


   for ( i = 0; i <= 2; i++ )
      {
      vtemp[i] = m[i][0]*vin[0] + m[i][1]*vin[1] + m[i][2]*vin[2];
      }


   /* Move the computed result to the output array. */

   MOVED ( vtemp, 3, vout );


} /* End of mxv_c */
