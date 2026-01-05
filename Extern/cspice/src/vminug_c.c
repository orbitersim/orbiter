/*

-Procedure vminug_c ( Negate vector, "-v", general dimension )

-Abstract

   Negate a double precision vector of arbitrary dimension.

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
   #undef   vminug_c

   void vminug_c ( ConstSpiceDouble  * vin,
                   SpiceInt            ndim,
                   SpiceDouble       * vout )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   vin        I   n-dimensional vector to be negated.
   ndim       I   Dimension of `vin' (and also `vout').
   vout       O   Negated vector -v1.

-Detailed_Input

   vin         is any double precision vector of arbitrary size.

   ndim        is the dimension of `vin' and `vout'.

-Detailed_Output

   vout        is a n-dimensional double precision vector which
               contains the negation of `vin'. `vout' may overwrite `vin'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   For each value of the index `i' from 0 to ndim-1, vminug_c negates `vin'
   by the expression:

      vout[i] = -vin[i];

   No error checking is performed since overflow can occur ONLY if
   the dynamic range of positive floating point numbers is not the
   same size as the dynamic range of negative floating point numbers
   AND at least one component of `vin' falls outside the common range.
   The likelihood of this occurring is so small as to be of no
   concern.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define a set of n-dimensional vectors and negate each of them.


      Example code begins here.


      /.
         Program vminug_ex1
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
         Define a set of n-dimensional vectors.
         ./
         SpiceDouble          vin    [SETSIZ][NDIM] = {
                                   { -10.0, 15.0, -5.0, 20.0 },
                                   {   0.0,  0.0,  0.0,  0.0 } };

         /.
         Negate each vector
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            vminug_c ( vin[i], NDIM, vout );

            printf( "Input vector  :  %6.1f %6.1f %6.1f %6.1f\n",
                       vin[i][0], vin[i][1], vin[i][2], vin[i][3] );
            printf( "Negated vector:  %6.1f %6.1f %6.1f %6.1f\n",
                               vout[0], vout[1], vout[2], vout[3] );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input vector  :   -10.0   15.0   -5.0   20.0
      Negated vector:    10.0  -15.0    5.0  -20.0

      Input vector  :     0.0    0.0    0.0    0.0
      Negated vector:    -0.0   -0.0   -0.0   -0.0


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

       Extended -Particulars section.

   -CSPICE Version 1.0.0, 29-JUN-1999 (EDW) (WMO)

-Index_Entries

   negate an n-dimensional vector

-&
*/

{ /* Begin vminug_c */

   /*
   Local variables
   */
   SpiceInt       i;


   /* Do it.  This isn't rocket science. */
   for ( i = 0; i < ndim; i++ )
      {
      vout[i] = -vin[i];
      }


} /* End vminug_c */
