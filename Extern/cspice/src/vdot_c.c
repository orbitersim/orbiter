/*

-Procedure vdot_c ( Vector dot product, 3 dimensions )

-Abstract

   Compute the dot product of two double precision, 3-dimensional
   vectors.

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

   #include <math.h>
   #include "SpiceUsr.h"
   #undef    vdot_c


   SpiceDouble vdot_c ( ConstSpiceDouble   v1[3],
                        ConstSpiceDouble   v2[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   First vector in the dot product.
   v2         I   Second vector in the dot product.

   The function returns the value of the dot product of `v1' and `v2'.

-Detailed_Input

   v1,
   v2          are two arbitrary double precision 3-dimensional
               vectors.

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

   vdot_c calculates the dot product of `v1' and `v2' by a simple
   application of the definition:

                   1
                .-----
                 \
      vdot_c  =   )  v1[i] * v2[i]
                 /
                '-----
                  i=0

   No error checking is performed to prevent or recover from numeric
   overflow.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose that you have a set of double precision 3-dimensional
      vectors. Check if they are orthogonal to the Z-axis.


      Example code begins here.


      /.
         Program vdot_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define SETSIZ       4

         /.
         Local variables.
         ./
         SpiceInt             i;

         /.
         Define the vector set.
         ./
         SpiceDouble          v1     [SETSIZ][3] = {
                                              { 1.0,  0.0,  0.0 },
                                              { 0.0, -6.0,  0.0 },
                                              {10.0,  0.0, -1.0 },
                                              { 0.0,  0.0,  1.0 } };

         SpiceDouble          z      [3]    = { 0.0,  0.0,  1.0 };

         /.
         Check the orthogonality with respect to `z' of each
         vector in `v1'.
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            printf( "\n" );
            printf( "Input vector (V1):  %5.1f %5.1f %5.1f\n",
                              v1[i][0], v1[i][1], v1[i][2] );

            if ( vdot_c ( v1[i], z ) == 0.0 )
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


      Input vector (V1):    1.0   0.0   0.0
      V1 and Z are orthogonal.

      Input vector (V1):    0.0  -6.0   0.0
      V1 and Z are orthogonal.

      Input vector (V1):   10.0   0.0  -1.0
      V1 and Z are NOT orthogonal.

      Input vector (V1):    0.0   0.0   1.0
      V1 and Z are NOT orthogonal.


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

   -CSPICE Version 1.0.3, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example. Improved -Particulars section.

   -CSPICE Version 1.0.2, 16-JAN-2008 (EDW)

       Corrected typos in header titles:

          Detailed Input to -Detailed_Input
          Detailed Output to -Detailed_Output

   -CSPICE Version 1.0.1, 12-NOV-2006 (EDW)

       Added -Parameters section header.

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW) (WMO) (NJB)

-Index_Entries

   dot product 3-dimensional vectors

-&
*/

{  /* Begin vdot_c */


   return ( v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2] );


} /* End vdot_c */
