/*

-Procedure vadd_c ( Vector addition, 3 dimensional )

-Abstract

   Add two double precision 3-dimensional vectors.

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
   #undef    vadd_c


   void vadd_c ( ConstSpiceDouble   v1[3],
                 ConstSpiceDouble   v2[3],
                 SpiceDouble        vout[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1         I   First vector to be added.
   v2         I   Second vector to be added.
   vout       O   Sum vector, v1 + v2.

-Detailed_Input

   v1,
   v2          are two arbitrary double precision 3-dimensional
               vectors.

-Detailed_Output

   vout        is the double precision 3-dimensional vector sum of `v1'
               and `v2'. `vout' may overwrite either `v1' or `v2'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This routine simply performs addition between components of `v1'
   and `v2'. No checking is performed to determine whether floating
   point overflow has occurred.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Define two sets of 3-dimensional vectors and compute the sum
      of each vector in first set with the corresponding vector in
      the second set.


      Example code begins here.


      /.
         Program vadd_ex1
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
         Define the two vector sets.
         ./
         SpiceDouble          seta   [SETSIZ][3] = { {1.0,   2.0,   3.0  },
                                                     {1.e-7, 1.e23, 1.e-9} };

         SpiceDouble          setb   [SETSIZ][3] = { {4.0,   5.0,   6.0},
                                                     {1.e24, 1.e23, 0.0} };

         /.
         Calculate the sum of each pair of vectors
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            vadd_c ( seta[i], setb[i], vout );

            printf( "Vector A  :  %10.2e %10.2e %10.2e\n",
                    seta[i][0], seta[i][1], seta[i][2] );
            printf( "Vector B  :  %10.2e %10.2e %10.2e\n",
                    setb[i][0], setb[i][1], setb[i][2] );
            printf( "Sum vector:  %10.2e %10.2e %10.2e\n",
                               vout[0], vout[1], vout[2] );
            printf( "\n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Vector A  :    1.00e+00   2.00e+00   3.00e+00
      Vector B  :    4.00e+00   5.00e+00   6.00e+00
      Sum vector:    5.00e+00   7.00e+00   9.00e+00

      Vector A  :    1.00e-07   1.00e+23   1.00e-09
      Vector B  :    1.00e+24   1.00e+23   0.00e+00
      Sum vector:    1.00e+24   2.00e+23   1.00e-09


-Restrictions

   1)  The user is required to determine that the magnitude each
       component of the vectors is within the appropriate range so as
       not to cause floating point overflow.

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

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (WMO)

-Index_Entries

   3-dimensional vector addition

-&
*/

{ /* Begin vadd_c */


   vout[0] = v1[0] + v2[0];
   vout[1] = v1[1] + v2[1];
   vout[2] = v1[2] + v2[2];


} /* End vadd_c */
