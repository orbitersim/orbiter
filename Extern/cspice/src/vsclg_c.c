/*

-Procedure vsclg_c ( Vector scaling, general dimension )

-Abstract

   Multiply a scalar and a double precision vector of arbitrary
   dimension.

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
   #undef    vsclg_c


   void vsclg_c ( SpiceDouble          s,
                  ConstSpiceDouble   * v1,
                  SpiceInt             ndim,
                  SpiceDouble        * vout )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   s          I   Scalar to multiply a vector.
   v1         I   Vector to be multiplied.
   ndim       I   Dimension of `v1' (and also `vout').
   vout       O   Product vector, s * v1.

-Detailed_Input

   s           is a double precision scalar.

   v1          is a double precision n-dimensional vector.

   ndim        is the dimension of `v1' (and `vout').

-Detailed_Output

   vout        is a double precision n-dimensional vector containing
               the product of the scalar with the vector `v1'.
               `vout' may overwrite `v1'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   For each value of the index `i' from 0 to ndim-1, this function
   performs the following multiplication

      vout[i] = s * v1[i];

   No error checking is performed to guard against numeric overflow
   or underflow. `vout' may overwrite `v1'.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define a sets of scalar double precision values and use them
      to scale a given n-dimensional vector.


      Example code begins here.


      /.
         Program vsclg_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NDIM         4
         #define SETSIZ       3

         /.
         Local variables.
         ./
         SpiceDouble          vout   [NDIM];

         SpiceInt             i;

         /.
         Define the set of scalars and the input vector.
         ./
         SpiceDouble          s      [SETSIZ] = { 3.0, 0.0, -1.0 };

         SpiceDouble          v1     [NDIM]   = { 1.0, 2.0, -3.0, 4.0 };

         printf( "Input vector :  %5.1f %5.1f %5.1f %5.1f\n",
                                   v1[0], v1[1], v1[2], v1[3] );
         printf( "\n" );

         /.
         Calculate product of each scalar and `v1'.
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            vsclg_c ( s[i], v1, NDIM, vout );

            printf( "Scale factor :  %5.1f\n", s[i] );
            printf( "Output vector:  %5.1f %5.1f %5.1f %5.1f\n",
                              vout[0], vout[1], vout[2], vout[3] );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input vector :    1.0   2.0  -3.0   4.0

      Scale factor :    3.0
      Output vector:    3.0   6.0  -9.0  12.0

      Scale factor :    0.0
      Output vector:    0.0   0.0  -0.0   0.0

      Scale factor :   -1.0
      Output vector:   -1.0  -2.0   3.0  -4.0


-Restrictions

   1)  No error checking is performed to guard against numeric
       overflow. The programmer is thus required to insure that the
       values in `v1' and `s' are reasonable and will not cause overflow.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)

-Version

   -CSPICE Version 1.1.1, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input vector const. Removed #includes of SpiceZfc.h and
       SpiceZst.h.

   -CSPICE Version 1.0.0, 13-JUL-1998 (NJB) (WMO)

-Index_Entries

   n-dimensional vector scaling

-&
*/

{ /* Begin vsclg_c */


   /*
   Local variables
   */

   SpiceInt                i;


   for ( i = 0;  i < ndim;  i++ )
   {
      vout[i] = s * v1[i];
   }


} /* End vsclg_c */
