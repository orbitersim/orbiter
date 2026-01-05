/*

-Procedure vdotg_c ( Vector dot product, general dimension )

-Abstract

   Compute the dot product of two vectors of arbitrary dimension.

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
   #include "SpiceZmc.h"
   #undef    vdotg_c


   SpiceDouble vdotg_c ( ConstSpiceDouble   * v1,
                         ConstSpiceDouble   * v2,
                         SpiceInt             ndim )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   First vector in the dot product.
   v2         I   Second vector in the dot product.
   ndim       I   Dimension of `v1' and `v2'.

   The function returns the value of the dot product of `v1' and `v2'.

-Detailed_Input

   v1,
   v2          are two arbitrary double precision n-dimensional
               vectors.

   ndim        is the dimension of `v1' and `v2'.

-Detailed_Output

   The function returns the value of the dot product (inner product)
   of `v1' and `v2':

      < v1, v2 >

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   vdotg_c calculates the dot product of `v1' and `v2' by a simple
   application of the definition:

                  ndim-1
                 .------
                  \
      vdotg_c  =   )  v1[i] * v2[i]
                  /
                 '------
                    i=0

   No error checking is performed to prevent or recover from numeric
   overflow.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose that you have a set of double precision n-dimensional
      vectors. Check if they are orthogonal to the Z-axis in
      n-dimensional space.


      Example code begins here.


      /.
         Program vdotg_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NDIM         4
         #define SETSIZ       5

         /.
         Local variables.
         ./
         SpiceInt             i;

         /.
         Define the vector set.
         ./
         SpiceDouble          v1     [SETSIZ][NDIM] = {
                                   { 1.0,  0.0,  0.0, 0.0 },
                                   { 0.0,  1.0,  0.0, 3.0 },
                                   { 0.0,  0.0, -6.0, 0.0 },
                                   {10.0,  0.0, -1.0, 0.0 },
                                   { 0.0,  0.0,  0.0, 1.0 } };

         SpiceDouble          z      [NDIM] = { 0.0,  0.0,  1.0, 0.0 };

         /.
         Check the orthogonality with respect to `z' of each
         vector in `v1'.
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            printf( "\n" );
            printf( "Input vector (V1):  %5.1f %5.1f %5.1f %5.1f\n",
                              v1[i][0], v1[i][1], v1[i][2], v1[i][3] );

            if ( vdotg_c ( v1[i], z, NDIM ) == 0.0 )
            {
               printf( "V1 and Z are orthogonal.\n" );
            }
            else
            {
               printf( "V1 and Z are NOT orthogonal.\n" );
            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input vector (V1):    1.0   0.0   0.0   0.0
      V1 and Z are orthogonal.

      Input vector (V1):    0.0   1.0   0.0   3.0
      V1 and Z are orthogonal.

      Input vector (V1):    0.0   0.0  -6.0   0.0
      V1 and Z are NOT orthogonal.

      Input vector (V1):   10.0   0.0  -1.0   0.0
      V1 and Z are NOT orthogonal.

      Input vector (V1):    0.0   0.0   0.0   1.0
      V1 and Z are orthogonal.


-Restrictions

   1)  The user is responsible for determining that the vectors `v1'
       and `v2' are not so large as to cause numeric overflow. In
       most cases this will not present a problem.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.0, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example. Improved -Particulars section.

       Removed check for "ndim" being positive in order to replicate
       behavior of SPICELIB equivalent routine.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input vectors const. Converted check-in style to discovery.

   -CSPICE Version 1.0.0, 31-MAR-1998 (EDW) (WMO)

-Index_Entries

   dot product of n-dimensional vectors

-&
*/

{ /* Begin vdotg_c */

   /*
   Local variables
   */
   SpiceInt                i;
   SpiceDouble             dot;


   /*
   Error free:  no error tracing required.
   */


   /*
   Initialize dot to zero.
   */
   dot  = 0.;

   /*
   Do the calculation.  Not very involved.
   */
   for ( i = 0; i < ndim; i++ )
   {
      dot += v1[i] * v2[i];
   }

   /*
   Return the value.
   */
   return dot;


} /* End vdotg_c */
