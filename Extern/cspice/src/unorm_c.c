/*

-Procedure unorm_c ( Unit vector and norm, 3 dimensional )

-Abstract

   Normalize a double precision 3-vector and return its magnitude.

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
   #undef    unorm_c


   void unorm_c ( ConstSpiceDouble     v1[3],
                  SpiceDouble          vout[3],
                  SpiceDouble        * vmag    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I     Vector to be normalized.
   vout       O     Unit vector v1 / |v1|.
   vmag       O     Magnitude of v1, i.e. |v1|.

-Detailed_Input

   v1          is an arbitrary 3-vector, including the
               zero vector.

-Detailed_Output

   vout        is the unit vector in the direction  of `v1'. If `v1' is
               the zero vector, then `vout' will also be the zero
               vector.

   vmag        is the magnitude of `v1'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   unorm_c references a function called vnorm_c (which itself is
   numerically stable) to calculate the norm of the input vector `v1'.
   If the norm is equal to zero, then each component of the output
   vector `vout' is set to zero. Otherwise, `vout' is calculated by
   dividing `v1' by the norm.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define a set of vectors and compute their corresponding unit
      vector and magnitude.


      Example code begins here.


      /.
         Program unorm_ex1
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
         SpiceDouble          vmag;
         SpiceDouble          vout   [3];

         SpiceInt             i;

         /.
         Define the vector set.
         ./
         SpiceDouble          seta   [SETSIZ][3] = {
                                   {5.0,  12.0,  0.0},
                                   {1.e-7,  2.e-7, 2.e-7} };

         /.
         Calculate the unit vectors and magnitudes.
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            unorm_c ( seta[i], vout, &vmag );

            printf( "Vector     :  %12.8f %12.8f %12.8f\n",
                         seta[i][0], seta[i][1], seta[i][2] );
            printf( "Unit vector:  %12.8f %12.8f %12.8f\n",
                                  vout[0], vout[1], vout[2] );
            printf( "Magnitude  :  %12.8f\n", vmag );
            printf( " \n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Vector     :    5.00000000  12.00000000   0.00000000
      Unit vector:    0.38461538   0.92307692   0.00000000
      Magnitude  :   13.00000000

      Vector     :    0.00000010   0.00000020   0.00000020
      Unit vector:    0.33333333   0.66666667   0.66666667
      Magnitude  :    0.00000030


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

   -CSPICE Version 1.1.1, 10-AUG-2021 (JDR)

       Updated the header to comply with NAIF standard. Added
       complete code example to -Examples section.

       Header sections have been reordered.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input vector const.

   -CSPICE Version 1.0.0, 17-OCT-1997 (EDW) (WMO) (WLT)

-Index_Entries

   3-dimensional unit vector and norm

-&
*/

{ /* Begin unorm_c */


   /*
   Obtain the magnitude of v1.  Note:  since vmage is a pointer, the
   value of what vmag is pointing at is *vmag.
   */

   *vmag = vnorm_c( v1 );



   /*
   If *vmag is nonzero, then normalize.  Note that this process is
   numerically stable: overflow could only happen if vmag were small,
   but this could only happen if each component of v1 were small.
   In fact, the magnitude of any vector is never less than the
   magnitude of any component.
   */

   if ( *vmag > 0.0 )
      {
      vout[0] = v1[0] / *vmag;
      vout[1] = v1[1] / *vmag;
      vout[2] = v1[2] / *vmag;
      }
   else
      {
      vout[0] = 0.;
      vout[1] = 0.;
      vout[2] = 0.;
      }

} /* End unorm_c */
