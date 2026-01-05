/*

-Procedure vnorm_c ( Vector norm, 3 dimensions )

-Abstract

   Compute the magnitude of a double precision 3-dimensional
   vector.

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
   #include "SpiceZmc.h"
   #undef    vnorm_c


   SpiceDouble vnorm_c ( ConstSpiceDouble v1[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   Vector whose magnitude is to be found.

   The function returns the magnitude of `v1'.

-Detailed_Input

   v1          is any double precision 3-dimensional vector.

-Detailed_Output

   The function returns the magnitude of `v1' calculated in a
   numerically stable way.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   vnorm_c takes care to avoid overflow while computing the norm of the
   input vector `v1'. vnorm_c finds the component of `v1' whose magnitude
   is the largest. Calling this magnitude `v1max', the norm is computed
   using the formula:

                        ||    1         ||
      vnorm_c = v1max * || ------- * v1 ||
                        ||  v1max       ||

   where the notation ||x|| indicates the norm of the vector `x'.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define a set of 3-dimensional vectors and compute the
      magnitude of each vector within.


      Example code begins here.


      /.
         Program vnorm_ex1
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
         SpiceInt             i;

         /.
         Define a set of 3-dimensional vectors.
         ./
         SpiceDouble          v1     [SETSIZ][3] = {
                                   { 1.0,    2.0,  2.0   },
                                   { 5.0,   12.0,  0.0   },
                                   {-5.e-17, 0.0, 12.e-17}  };

         /.
         Calculate the magnitude of each vector
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            printf( "Input vector:  %9.2e %9.2e %9.2e\n",
                                   v1[i][0], v1[i][1], v1[i][2] );
            printf( "Magnitude   :  %23.20f\n", vnorm_c ( v1[i] ) );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input vector:   1.00e+00  2.00e+00  2.00e+00
      Magnitude   :   3.00000000000000000000

      Input vector:   5.00e+00  1.20e+01  0.00e+00
      Magnitude   :  13.00000000000000000000

      Input vector:  -5.00e-17  0.00e+00  1.20e-16
      Magnitude   :   0.00000000000000013000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.3, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.0.2, 16-JAN-2008 (EDW)

       Corrected typos in header titles:

       Detailed Input to -Detailed_Input
       Detailed Output to -Detailed_Output

   -CSPICE Version 1.0.1, 12-NOV-2006 (EDW)

       Added -Parameters section header.

   -CSPICE Version 1.0.0, 16-APR-1999 (NJB) (WMO) (WLT)

-Index_Entries

   norm of 3-dimensional vector

-&
*/

{  /* Begin vnorm_c */

   /*
   Local variables
   */
   SpiceDouble                        normSqr;
   SpiceDouble                        tmp0;
   SpiceDouble                        tmp1;
   SpiceDouble                        tmp2;
   SpiceDouble                        v1max;


   /*
   Determine the maximum component of the vector.
   */
   v1max = MaxAbs(  v1[0],   MaxAbs( v1[1], v1[2] )   );


   /*
   If the vector is zero, return zero; otherwise normalize first.
   Normalizing helps in the cases where squaring would cause overflow
   or underflow.  In the cases where such is not a problem it not worth
   it to optimize further.
   */

   if ( v1max == 0.0 )
   {
      return ( 0.0 );
   }
   else
   {
      tmp0     =  v1[0]/v1max;
      tmp1     =  v1[1]/v1max;
      tmp2     =  v1[2]/v1max;

      normSqr  =  tmp0*tmp0 + tmp1*tmp1 + tmp2*tmp2;

      return (  v1max * sqrt( normSqr )  );
   }


} /* End vnorm_c */
