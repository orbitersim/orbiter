/*

-Procedure vscl_c ( Vector scaling, 3 dimensions )

-Abstract

   Multiply a scalar and a double precision 3-dimensional vector.

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
   #undef    vscl_c


   void vscl_c ( SpiceDouble        s,
                 ConstSpiceDouble   v1[3],
                 SpiceDouble        vout[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   s          I   Scalar to multiply a vector.
   v1         I   Vector to be multiplied.
   vout       O   Product vector, s * v1.

-Detailed_Input

   s           is a double precision scalar used to multiply the vector
               `v1'.

   v1          is a double precision 3-dimensional vector, which is to
               be scaled by `s'.

-Detailed_Output

   vout        is a double precision 3-dimensional vector containing
               the product of the scalar with the vector `v1'. `vout' may
               overwrite `v1'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   For each value of the index `i' from 0 to 2, this function
   performs the following multiplication

      vout[i] = s * v1[i];

   No error checking is performed to guard against numeric overflow
   or underflow.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define a sets of scalar double precision values and use them
      to scale a given 3-dimensional vector.


      Example code begins here.


      /.
         Program vscl_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define SETSIZ       3

         /.
         Local variables.
         ./
         SpiceDouble          vout   [3];

         SpiceInt             i;

         /.
         Define the set of scalars and the input vector.
         ./
         SpiceDouble          s      [SETSIZ] = { 3.0, 0.0, -1.0 };

         SpiceDouble          v1     [3]      = { 1.0, 2.0, -3.0 };

         printf( "Input vector :  %5.1f %5.1f %5.1f\n",
                                   v1[0], v1[1], v1[2] );
         printf( "\n" );

         /.
         Calculate product of each scalar and `v1'.
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            vscl_c ( s[i], v1, vout );

            printf( "Scale factor :  %5.1f\n", s[i] );
            printf( "Output vector:  %5.1f %5.1f %5.1f\n",
                              vout[0], vout[1], vout[2] );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input vector :    1.0   2.0  -3.0

      Scale factor :    3.0
      Output vector:    3.0   6.0  -9.0

      Scale factor :    0.0
      Output vector:    0.0   0.0  -0.0

      Scale factor :   -1.0
      Output vector:   -1.0  -2.0   3.0


-Restrictions

   1)  The user is responsible for insuring that no floating point
       overflow occurs from multiplying `s' by any component of `v1'. No
       error recovery or reporting scheme is incorporated in this
       function.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.1, 23-JUL-2020 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input vector const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (WMO)

-Index_Entries

   3-dimensional vector scaling

-&
*/

{ /* Begin vscl_c */

   vout[0] = s * v1[0];
   vout[1] = s * v1[1];
   vout[2] = s * v1[2];


} /* End vscl_c */
