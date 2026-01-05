/*

-Procedure vsubg_c ( Vector subtraction, general dimension )

-Abstract

   Compute the difference between two double precision vectors of
   arbitrary dimension.

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
   #undef    vsubg_c


   void vsubg_c ( ConstSpiceDouble  * v1,
                  ConstSpiceDouble  * v2,
                  SpiceInt            ndim,
                  SpiceDouble       * vout )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   First vector (minuend).
   v2         I   Second vector (subtrahend).
   ndim       I   Dimension of `v1', `v2', and `vout'.
   vout       O   Difference vector, v1 - v2.

-Detailed_Input

   v1          is a double precision vector of arbitrary dimension which
               is the minuend (i.e. first or left-hand member) in the
               vector subtraction.

   v2          is a double precision vector of arbitrary dimension which
               is the subtrahend (i.e. second or right-hand member) in
               the vector subtraction.

   ndim        is the dimension of `v1' and `v2' (and `vout').

-Detailed_Output

   vout        is a double precision n-dimensional vector which
               represents the vector difference, v1 - v2. `vout' may
               overwrite `v1' or `v2'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   For each value of the index `i' from 0 to ndim-1, this function
   performs the following subtraction:

      vout[i] = v1[i] - v2[i]

   No error checking is performed to guard against numeric overflow
   or underflow. `vout' may overwrite `v1' or `v2'.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define two sets of n-dimensional vectors and compute the
      difference from each vector in first set with the
      corresponding vector in the second set.


      Example code begins here.


      /.
         Program vsubg_ex1
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
         Define the two vector sets.
         ./
         SpiceDouble          v1     [SETSIZ][NDIM] = {
                                   {1.0,  2.0,  3.0,  4.0},
                                   {1.0,  2.0,  3.0,  4.0},
                                   {1.0,  2.0,  3.0,  4.0} };

         SpiceDouble          v2     [SETSIZ][NDIM] = {
                                   { 1.0,  1.0,  1.0,  1.0},
                                   {-1.0, -2.0, -3.0, -4.0},
                                   {-1.0,  2.0, -3.0,  4.0} };

         /.
         Calculate the difference between each pair of vectors
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            vsubg_c ( v1[i], v2[i], NDIM, vout );

            printf( "First vector :  %5.1f %5.1f %5.1f %5.1f\n",
                          v1[i][0], v1[i][1], v1[i][2], v1[i][3] );
            printf( "Second vector:  %5.1f %5.1f %5.1f %5.1f\n",
                          v2[i][0], v2[i][1], v2[i][2], v2[i][3] );
            printf( "Difference   :  %5.1f %5.1f %5.1f %5.1f\n",
                              vout[0], vout[1], vout[2], vout[3] );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      First vector :    1.0   2.0   3.0   4.0
      Second vector:    1.0   1.0   1.0   1.0
      Difference   :    0.0   1.0   2.0   3.0

      First vector :    1.0   2.0   3.0   4.0
      Second vector:   -1.0  -2.0  -3.0  -4.0
      Difference   :    2.0   4.0   6.0   8.0

      First vector :    1.0   2.0   3.0   4.0
      Second vector:   -1.0   2.0  -3.0   4.0
      Difference   :    2.0   0.0   6.0   0.0


-Restrictions

   1)  No error checking is performed to guard against numeric
       overflow. The programmer is thus required to insure that the
       values in `v1' and `v2' are reasonable and will not cause
       overflow. No error recovery or reporting scheme is
       incorporated in this routine.

   2)  It is assumed the proper amount of memory has been allocated for
       `v1', `v2' and `vout'.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.1, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input vectors const.

   -CSPICE Version 1.0.0, 05-MAR-1998 (EDW) (WMO)

-Index_Entries

   n-dimensional vector subtraction

-&
*/

{ /* Begin vsubg_c */

   /*
   Local variables
   */

   SpiceInt                i;


   /* Do the calculation.  Not very involved. */

   for ( i = 0; i < ndim; i++ )
      {
      vout[i] = v1[i] - v2[i];
      }


} /* End vsubg_c */
