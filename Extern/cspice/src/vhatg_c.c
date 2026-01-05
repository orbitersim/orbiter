/*

-Procedure vhatg_c ( "V-Hat", unit vector along V, general dimension )

-Abstract

   Find the unit vector along a double precision vector of
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
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #undef    vhatg_c


   void vhatg_c ( ConstSpiceDouble   * v1,
                  SpiceInt             ndim,
                  SpiceDouble        * vout )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   Vector to be normalized.
   ndim       I   Dimension of `v1' (and also `vout').
   vout       O   Unit vector along `v1'.

-Detailed_Input

   v1          is any double precision vector of arbitrary dimension.

   ndim        is the dimension of `v1' (and also `vout').

-Detailed_Output

   vout        is the unit vector in the direction of `v1':

                            v1
                  vout = --------
                          ||v1||

               If `v1' represents the zero vector, then `vout' will also be
               the zero vector. `vout' may overwrite `v1'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   vhatg_c determines the magnitude of `v1' and then divides each
   component of `v1' by the magnitude. This process is highly stable
   over the whole range of multi-dimensional vectors.

   This routine will detect if `v1' the zero vector, and will not
   attempt to divide by zero.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define a set of n-dimensional vectors and find the unit vector
      along each of them.


      Example code begins here.


      /.
         Program vhatg_ex1
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
         Define the vector set.
         ./
         SpiceDouble          v1     [SETSIZ][NDIM] = {
                                   { 5.0,    12.0,  0.0,   0.0 },
                                   { 1.e-7,  2.e-7, 2.e-7, 0.0 } };

         /.
         Calculate the unit vectors.
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            vhatg_c ( v1[i], NDIM, vout );

            printf( "Input vector:  %11.7f %11.7f %11.7f %11.7f\n",
                             v1[i][0], v1[i][1], v1[i][2], v1[i][3] );
            printf( "Unit vector :  %11.7f %11.7f %11.7f %11.7f\n",
                                 vout[0], vout[1], vout[2], vout[3] );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input vector:    5.0000000  12.0000000   0.0000000   0.0000000
      Unit vector :    0.3846154   0.9230769   0.0000000   0.0000000

      Input vector:    0.0000001   0.0000002   0.0000002   0.0000000
      Unit vector :    0.3333333   0.6666667   0.6666667   0.0000000


-Restrictions

   1)  The relative number of cases whereby floating point overflow
       may occur is negligible. Thus, no error recovery or reporting
       scheme is incorporated into this function.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)

-Version

   -CSPICE Version 1.0.1, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.0.0, 13-JUL-1999 (NJB) (WMO)

-Index_Entries

   unitize a n-dimensional vector

-&
*/

{ /* Begin vhatg_c */


   /*
   Local variables
   */
   SpiceDouble             vmag;
   SpiceInt                i;



   /*
   Obtain the magnitude of v1.
   */
   vmag = vnormg_c ( v1, ndim );

   /*
   If vmag is nonzero, then normalize.  Note that this process is
   numerically stable: overflow could only happen if vmag were small,
   but this could only happen if each component of v1 were small.
   In fact, the magnitude of any vector is never less than the
   magnitude of any component.
   */

   if ( vmag > 0.0 )
   {
      for (  i = 0;  i < ndim;  i++  )
      {
         vout[i] = v1[i] / vmag;
      }
   }
   else
   {
      for (  i = 0;  i < ndim;  i++  )
      {
         vout[i] = 0.;
      }
   }

} /* End vhatg_c */
