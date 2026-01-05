/*

-Procedure vminus_c ( Negate vector, "-v", 3 dimensions )

-Abstract

   Negate a double precision 3-dimensional vector.

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
   #undef    vminus_c


   void vminus_c ( ConstSpiceDouble    v1     [3],
                   SpiceDouble         vout   [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   Vector to be negated.
   vout       O   Negated vector -v1.

-Detailed_Input

   v1          is any double precision 3-dimensional vector.

-Detailed_Output

   vout        is the negation (additive inverse) of `v1'. It is a
               double precision 3-dimensional vector. `vout' may
               overwrite `v1'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   For each value of the index `i' from 0 to 2, vminus_c negates `v1'
   by the expression:

      vout[i] = -v1[i];

   No error checking is performed since overflow can occur ONLY if
   the dynamic range of positive floating point numbers is not the
   same size as the dynamic range of negative floating point numbers
   AND at least one component of `v1' falls outside the common range.
   The likelihood of this occurring is so small as to be of no
   concern.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define a set of 3-dimensional vectors and negate each of them.


      Example code begins here.


      /.
         Program vminus_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define SETSIZ       2

         /.
         Local variables.
         ./
         SpiceDouble          vout   [3];

         SpiceInt             i;

         /.
         Define a set of 3-dimensional vectors.
         ./
         SpiceDouble          v1     [SETSIZ][3] = { { 1.0, -2.0, 0.0 },
                                                     { 0.0,  0.0, 0.0 } };

         /.
         Negate each vector
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            vminus_c ( v1[i], vout );

            printf( "Input vector  :  %5.1f %5.1f %5.1f\n",
                               v1[i][0], v1[i][1], v1[i][2] );
            printf( "Negated vector:  %5.1f %5.1f %5.1f\n",
                                  vout[0], vout[1], vout[2] );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input vector  :    1.0  -2.0   0.0
      Negated vector:   -1.0   2.0  -0.0

      Input vector  :    0.0   0.0   0.0
      Negated vector:   -0.0  -0.0  -0.0


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.1, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input vector const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (WMO)

-Index_Entries

   negate a 3-dimensional vector

-&
*/

{ /* Begin vminus_c */


   vout[0] = -v1[0];
   vout[1] = -v1[1];
   vout[2] = -v1[2];


} /* End vminus_c */
