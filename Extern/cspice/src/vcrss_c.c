/*

-Procedure vcrss_c ( Vector cross product, 3 dimensions )

-Abstract

   Compute the cross product of two 3-dimensional vectors.

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
   #include "SpiceZmc.h"
   #undef    vcrss_c


   void vcrss_c ( ConstSpiceDouble   v1[3],
                  ConstSpiceDouble   v2[3],
                  SpiceDouble        vout[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   Left hand vector for cross product.
   v2         I   Right hand vector for cross product.
   vout       O   Cross product v1 x v2.

-Detailed_Input

   v1,
   v2          are two 3-dimensional vectors. Typically, these might
               represent the (possibly unit) vector to a planet, Sun,
               or a star which defines the orientation of axes of some
               reference frame.

-Detailed_Output

   vout        is the cross product of `v1' and `v2'. `vout' may overwrite
               `v1' or `v2'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   vcrss_c calculates the three dimensional cross product of two
   vectors according to the definition. The cross product is stored
   in a buffer vector until the calculation is complete. Thus `vout'
   may overwrite `v1' or `v2' without interfering with intermediate
   computations.

   If `v1' and `v2' are large in magnitude (taken together, their
   magnitude surpasses the limit allowed by the computer) then it
   may be possible to generate a floating point overflow from an
   intermediate computation even though the actual cross product may
   be well within the range of double precision numbers. vcrss_c does
   NOT check the magnitude of `v1' or `v2' to insure that overflow will
   not occur.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define two sets of vectors and compute the cross product of
      each vector in first set and the corresponding vector in
      the second set.


      Example code begins here.


      /.
         Program vcrss_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NDIM         3
         #define SETSIZ       2

         /.
         Local variables.
         ./
         SpiceDouble          vout   [NDIM];

         SpiceInt             i;

         /.
         Define the two vector sets.
         ./
         SpiceDouble          seta   [SETSIZ][NDIM] = {
                                   {0.0,  1.0,  0.0},
                                   {5.0,  5.0,  5.0} };

         SpiceDouble          setb   [SETSIZ][NDIM] = {
                                   { 1.0,  0.0,  0.0},
                                   {-1.0, -1.0, -1.0} };

         /.
         Calculate the cross product of each pair of vectors
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            vcrss_c ( seta[i], setb[i], vout );

            printf( "Vector A     :  %4.1f %4.1f %4.1f\n",
                        seta[i][0], seta[i][1], seta[i][2] );
            printf( "Vector B     :  %4.1f %4.1f %4.1f\n",
                        setb[i][0], setb[i][1], setb[i][2] );
            printf( "Cross product:  %4.1f %4.1f %4.1f\n",
                                 vout[0], vout[1], vout[2] );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Vector A     :   0.0  1.0  0.0
      Vector B     :   1.0  0.0  0.0
      Cross product:   0.0  0.0 -1.0

      Vector A     :   5.0  5.0  5.0
      Vector B     :  -1.0 -1.0 -1.0
      Cross product:   0.0  0.0  0.0


-Restrictions

   1)  No checking of `v1' or `v2' is done to prevent floating point
       overflow. The user is required to determine that the
       magnitude of each component of the vectors is within an
       appropriate range so as not to cause floating point overflow.
       In almost every case there will be no problem and no checking
       actually needs to be done.

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

   -CSPICE Version 1.0.1, 06-MAR-1998 (EDW)

       Minor header correction. Added use of MOVED.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (WMO)

-Index_Entries

   vector cross product

-&
*/

{ /* Begin vcrss_c */

   /*
   Local variables
   */

   SpiceDouble  vtemp[3];


   /*
   Calculate the cross product of v1 and v2, store in vtemp.
   */

   vtemp[0] = v1[1]*v2[2] - v1[2]*v2[1];
   vtemp[1] = v1[2]*v2[0] - v1[0]*v2[2];
   vtemp[2] = v1[0]*v2[1] - v1[1]*v2[0];


   /*
   Now move the result into vout.
   */
   MOVED ( vtemp, 3, vout );


} /* End vcrss_c */
