/*

-Procedure trace_c ( Trace of a 3x3 matrix )

-Abstract

   Return the trace of a 3x3 matrix.

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

*/

   #include "SpiceUsr.h"
   #undef    trace_c

   SpiceDouble trace_c ( ConstSpiceDouble  matrix[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   matrix     I   3x3 matrix of double precision numbers.

   The function returns the trace of `matrix'.

-Detailed_Input

   matrix      is a double precision 3x3 matrix.

-Detailed_Output

   The function returns the trace of `matrix', i.e. it is the sum of
   the diagonal elements of `matrix'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The code reflects precisely the following mathematical
   expression:

                  2
                .----
                 \
      trace_c =   )  matrix[i][i]
                 /
                '----
                 i=0

   No error detection or correction is implemented within this
   function.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given a 3x3 double precision matrix, compute its trace.


      Example code begins here.


      /.
         Program trace_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceInt             i;

         /.
         Define `matrix'.
         ./
         SpiceDouble          matrix [3][3] = { {3.0,  5.0,  7.0},
                                                {0.0, -2.0,  8.0},
                                                {4.0,  0.0, -1.0} };

         printf( "MATRIX:\n" );
         for ( i = 0; i < 3; i++ )
         {
            printf( "%6.1f %5.1f %5.1f\n",
                    matrix[i][0], matrix[i][1], matrix[i][2] );
         }

         /.
         Compute the trace of `matrix' and display the result.
         ./
         printf( "\n" );
         printf( "Trace:  %3.1f\n", trace_c ( matrix ) );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      MATRIX:
         3.0   5.0   7.0
         0.0  -2.0   8.0
         4.0   0.0  -1.0

      Trace:  0.0


-Restrictions

   1)  No checking is performed to guard against floating point
       overflow or underflow. This routine should probably not be
       used if the input matrix is expected to have large double
       precision numbers along the diagonal.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.0.0, 29-JUN-1999 (EDW) (WLT)

-Index_Entries

   trace of a 3x3_matrix

-&
*/

{ /* Begin trace_c */

   /*
   Local variables
   */
   SpiceInt          i;
   SpiceDouble       trace = 0.;


   /* Do it.  This isn't rocket science. */
   for ( i = 0; i < 3; i++ )
      {
      trace += matrix[i][i];
      }

   return trace;


} /* End trace_c */
