/*

-Procedure vzerog_c ( Is a vector the zero vector? -- general dim. )

-Abstract

   Indicate whether an n-dimensional vector is the zero vector.

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

   MATH
   VECTOR

*/

   #include "SpiceUsr.h"
   #undef    vzerog_c

   SpiceBoolean vzerog_c ( ConstSpiceDouble * v, SpiceInt ndim )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v          I   Vector to be tested.
   ndim       I   Dimension of `v'.

   The function returns the value SPICETRUE if and only if `v' is the
   zero vector.

-Detailed_Input

   v,
   ndim        are, respectively, an n-dimensional vector and its
               dimension.

-Detailed_Output

   The function returns the value SPICETRUE if and only if `v' is the
   zero vector.

-Parameters

   None.

-Exceptions

   Error free.

   1)  When `ndim' is non-positive, this function returns the value
       SPICEFALSE (A vector of non-positive dimension cannot be the
       zero vector.)

-Files

   None.

-Particulars

   This function has the same truth value as the logical expression

      ( vnormg_c ( v, ndim )  ==  0. )

   Replacing the above expression by

      vzerog_c ( v, ndim );

   has several advantages: the latter expresses the test more
   clearly, looks better, and doesn't go through the work of scaling,
   squaring, taking a square root, and re-scaling (all of which
   vnormg_c must do) just to find out that a vector is non-zero.

   A related function is vzero_c, which accepts three-dimensional
   vectors.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given a set of n-dimensional vectors, check which ones are
      the zero vector.


      Example code begins here.


      /.
         Program vzerog_ex1
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
         SpiceInt             i;

         /.
         Define the vector set.
         ./
         SpiceDouble          v      [SETSIZ][NDIM] = {
                                   {0.0,  0.0,  0.0,  2.e-7},
                                   {0.0,  0.0,  0.0,  0.0  } };

         /.
         Check each n-dimensional vector within the set.
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            /.
            Check if the i'th vector is the zero vector.
            ./
            printf( "\n" );
            printf( "Input vector:  %10.7f %10.7f %10.7f %10.7f\n",
                                 v[i][0], v[i][1], v[i][2], v[i][3] );

            if ( vzerog_c ( v[i], NDIM ) )
            {
               printf( "   The zero vector.\n" );
            }
            else
            {
               printf( "   Not all elements of the vector are zero.\n" );
            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input vector:   0.0000000  0.0000000  0.0000000  0.0000002
         Not all elements of the vector are zero.

      Input vector:   0.0000000  0.0000000  0.0000000  0.0000000
         The zero vector.


   2) Define a unit quaternion and confirm that it is non-zero
      before converting it to a rotation matrix.


      Example code begins here.


      /.
         Program vzerog_ex2
      ./
      #include <math.h>
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          q      [4];
         SpiceDouble          m      [3][3];
         SpiceDouble          s;

         SpiceInt             i;

         /.
         Define a unit quaternion.
         ./
         s = sqrt( 2.0 ) / 2.0;

         q[0] = s;
         q[1] = 0.0;
         q[2] = 0.0;
         q[3] = -s;

         printf( "Quaternion : %11.7f %11.7f %11.7f %11.7f\n",
                                        q[0], q[1], q[2], q[3] );

         /.
         Confirm that it is non-zero and
         ./
         if ( vzerog_c ( q, 4 ) )
         {
            printf( "   Quaternion is the zero vector.\n" );
         }
         else
         {

            /.
            Confirm q satisfies ||q|| = 1.
            ./
            printf( "Norm       : %11.7f\n", vnormg_c ( q, 4 ) );

            /.
            Convert the quaternion to a matrix form.
            ./
            q2m_c ( q, m );

            printf( "Matrix form:\n" );
            for ( i = 0; i < 3; i++ )
            {
               printf( "%12.7f %11.7f %11.7f\n", m[i][0], m[i][1], m[i][2] );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Quaternion :   0.7071068   0.0000000   0.0000000  -0.7071068
      Norm       :   1.0000000
      Matrix form:
         0.0000000   1.0000000   0.0000000
        -1.0000000   0.0000000  -0.0000000
        -0.0000000   0.0000000   1.0000000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

   -CSPICE Version 1.0.0, 29-JUN-1999 (EDW) (NJB) (IMU)

-Index_Entries

   test whether an n-dimensional vector is the zero vector

-&
*/

{ /* Begin vzerog_c */


   /*
   Local variables.
   */
   SpiceInt       i;

   /* ndim must be at least 1. */
   if ( ndim < 1 )
      {
      return SPICEFALSE;
      }


   /* Check for any non-zero entries.  If they exist, test fails. */
   for ( i=0; i < ndim; i++ )
      {
      if ( v[i] != 0. )
         {
         return SPICEFALSE;
         }
      }


   /* If we are here, the vector is zero. */
   return SPICETRUE;



} /* End vzerog_c */
