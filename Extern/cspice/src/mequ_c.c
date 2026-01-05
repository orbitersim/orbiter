/*

-Procedure mequ_c ( Matrix equal to another, 3x3 )

-Abstract

   Set one double precision 3x3 matrix equal to another.

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

   ASSIGNMENT
   MATRIX

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #undef    mequ_c

   void mequ_c ( ConstSpiceDouble  m1  [3][3],
                 SpiceDouble       mout[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m1         I   Input matrix.
   mout       O   Output matrix equal to `m1'.

-Detailed_Input

   m1          is an arbitrary input 3x3 matrix. There are no
               restrictions on what it may contain.

-Detailed_Output

   mout        is a 3x3 matrix set to be equal to `m1'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   None.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This trivial example demonstrates how to use mequ_c to assign
      one matrix to another.


      Example code begins here.


      /.
         Program mequ_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          mout   [3][3];

         SpiceInt             i;

         /.
         Define `m1'.
         ./
         SpiceDouble          m1     [3][3] = { {0.0, -1.0,  0.0},
                                                {1.0,  0.0,  0.0},
                                                {0.0,  0.0,  1.0} };

         /.
         Assign `m1' to `mout' and print `mout'.
         ./
         mequ_c ( m1, mout );

         printf( "MOUT:\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf( "%16.7f %15.7f %15.7f\n",
                    mout[i][0], mout[i][1], mout[i][2] );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      MOUT:
             0.0000000      -1.0000000       0.0000000
             1.0000000       0.0000000       0.0000000
             0.0000000       0.0000000       1.0000000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 04-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.0.0, 29-JUN-1999 (EDW) (WMO)

-Index_Entries

   equal to another 3x3_matrix

-&
*/

{ /* Begin mequ_c */


   MOVED (m1, 9, mout);


} /* End mequ_c */
