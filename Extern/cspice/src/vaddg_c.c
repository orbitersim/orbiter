/*

-Procedure vaddg_c ( Vector addition, general dimension )

-Abstract

   Add two vectors of arbitrary dimension.

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
   #undef   vaddg_c

   void vaddg_c ( ConstSpiceDouble  * v1,
                  ConstSpiceDouble  * v2,
                  SpiceInt            ndim,
                  SpiceDouble       * vout )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   First vector to be added.
   v2         I   Second vector to be added.
   ndim       I   Dimension of `v1', `v2', and `vout'.
   vout       O   Sum vector, v1 + v2.

-Detailed_Input

   v1,
   v2          are two arbitrary double precision n-dimensional
               vectors.

   ndim        is the dimension of `v1', `v2' and `vout'.

-Detailed_Output

   vout        is the double precision n-dimensional vector sum of `v1'
               and `v2'. `vout' may overwrite either `v1' or `v2'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This routine simply performs addition between components of `v1'
   and `v2'. No checking is performed to determine whether floating
   point overflow has occurred.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define two sets of n-dimensional vectors and compute the sum
      of each vector in first set with the corresponding vector in
      the second set.


      Example code begins here.


      /.
         Program vaddg_ex1
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
         SpiceDouble          vout   [NDIM];

         SpiceInt             i;

         /.
         Define the two vector sets.
         ./
         SpiceDouble          seta   [SETSIZ][NDIM] = {
                                   {1.0,   2.0,   3.0,    4.0},
                                   {1.e-7, 1.e23, 1.e-9,  0.0} };

         SpiceDouble          setb   [SETSIZ][NDIM] = {
                                   {4.0,   5.0,    6.0,  7.0},
                                   {1.e24, 1.e23,  0.0,  3.e-23} };

         /.
         Calculate the sum of each pair of vectors
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            vaddg_c ( seta[i], setb[i], NDIM, vout );

            printf( "Vector A  :  %10.2e %10.2e %10.2e %10.2e\n",
                    seta[i][0], seta[i][1], seta[i][2], seta[i][3] );
            printf( "Vector B  :  %10.2e %10.2e %10.2e %10.2e\n",
                    setb[i][0], setb[i][1], setb[i][2], setb[i][3] );
            printf( "Sum vector:  %10.2e %10.2e %10.2e %10.2e\n",
                           vout[0], vout[1], vout[2], vout[3] );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Vector A  :    1.00e+00   2.00e+00   3.00e+00   4.00e+00
      Vector B  :    4.00e+00   5.00e+00   6.00e+00   7.00e+00
      Sum vector:    5.00e+00   7.00e+00   9.00e+00   1.10e+01

      Vector A  :    1.00e-07   1.00e+23   1.00e-09   0.00e+00
      Vector B  :    1.00e+24   1.00e+23   0.00e+00   3.00e-23
      Sum vector:    1.00e+24   2.00e+23   1.00e-09   3.00e-23


-Restrictions

   1)  The user is required to determine that the magnitude each
       component of the vectors is within the appropriate range so as
       not to cause floating point overflow.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.0.1, 07-NOV-2003 (EDW)

       Corrected a mistake in the second example's value
       for VOUT, i.e. replaced [1D24, 2D23, 0.0] with
       [1e24, 2e23].

   -CSPICE Version 1.0.0, 29-JUN-1999 (EDW) (WMO)

-Index_Entries

   n-dimensional vector addition

-&
*/

{ /* Begin vaddg_c */

   /*
   Local variables
   */
   SpiceInt       i;


   /*
   Do it.  This isn't rocket science.
   */
   for ( i = 0; i < ndim; i++ )
      {
      vout[i] = v1[i] + v2[i];
      }


} /* End vaddg_c */
