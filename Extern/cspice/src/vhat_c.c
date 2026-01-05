/*

-Procedure vhat_c ( "V-Hat", unit vector along V, 3 dimensions )

-Abstract

   Find the unit vector along a double precision 3-dimensional vector.

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
   #undef    vhat_c


   void vhat_c ( ConstSpiceDouble  v1  [3],
                 SpiceDouble       vout[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   Vector to be unitized.
   vout       O   Unit vector v1 / |v1|.

-Detailed_Input

   v1          is any double precision, 3-dimensional vector.

-Detailed_Output

   vout        is the unit vector in the direction of `v1'.

               If `v1' represents the zero vector, then `vout' will also
               be the zero vector.  `vout' may overwrite `v1'.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If `v1' represents the zero vector, then `vout' will also be the
       zero vector.

-Files

   None.

-Particulars

   vhat_c determines the magnitude of `v1' and then divides each
   component of `v1' by the magnitude. This process is highly stable
   over the whole range of 3-dimensional vectors.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define a set of vectors and compute their corresponding unit
      vector.


      Example code begins here.


      /.
         Program vhat_ex1
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
         Define the vector set.
         ./
         SpiceDouble          seta   [SETSIZ][3] = {
                                   {5.0,  12.0,  0.0},
                                   {1.e-7,  2.e-7, 2.e-7} };

         /.
         Calculate the unit vectors.
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            vhat_c ( seta[i], vout );

            printf( "Vector     :  %12.8f %12.8f %12.8f\n",
                         seta[i][0], seta[i][1], seta[i][2] );
            printf( "Unit vector:  %12.8f %12.8f %12.8f\n",
                                  vout[0], vout[1], vout[2] );
            printf( " \n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Vector     :    5.00000000  12.00000000   0.00000000
      Unit vector:    0.38461538   0.92307692   0.00000000

      Vector     :    0.00000010   0.00000020   0.00000020
      Unit vector:    0.33333333   0.66666667   0.66666667


-Restrictions

   1)  There is no known case whereby floating point overflow may
       occur. Thus, no error recovery or reporting scheme is
       incorporated into this routine.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.3, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code example to -Examples section.

   -CSPICE Version 1.0.2, 16-JAN-2008 (EDW)

       Corrected typos in header titles:

       Detailed Input to -Detailed_Input
       Detailed Output to -Detailed_Output

   -CSPICE Version 1.0.1, 12-NOV-2006 (EDW)

       Added -Parameters section header.

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW) (WMO) (WLT) (NJB)

-Index_Entries

   unitize a 3-dimensional vector

-&
*/

{  /* Begin vhat_c */


   /*
   Local variables
   */
   SpiceDouble                        vmag;


   /*
   Obtain the magnitude of v1.
   */
   vmag = vnorm_c(v1);

   /*
   If vmag is nonzero, then unitize.  Note that this process is
   numerically stable: overflow could only happen if vmag were small,
   but this could only happen if each component of v1 were small.
   In fact, the magnitude of any vector is never less than the
   magnitude of any component.
   */

   if ( vmag > 0.0 )
      {
      vout[0] = v1[0] / vmag;
      vout[1] = v1[1] / vmag;
      vout[2] = v1[2] / vmag;
      }
   else
      {
      vout[0] = 0.0;
      vout[1] = 0.0;
      vout[2] = 0.0;
      }


} /* End vhat_c */
