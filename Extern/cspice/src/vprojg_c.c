/*

-Procedure vprojg_c ( Vector projection, general dimension )

-Abstract

   Compute the projection of one vector onto another vector. All
   vectors are of arbitrary dimension.

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

   void vprojg_c ( ConstSpiceDouble    a      [],
                   ConstSpiceDouble    b      [],
                   SpiceInt            ndim,
                   SpiceDouble         p      [] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   a          I   The vector to be projected.
   b          I   The vector onto which `a' is to be projected.
   ndim       I   Dimension of `a', `b', and `p'.
   p          O   The projection of `a' onto `b'.

-Detailed_Input

   a           is a double precision vector of arbitrary dimension.
               This vector is to be projected onto the vector `b'.

   b           is a double precision vector of arbitrary dimension.
               This vector is the vector which receives the
               projection.

   ndim        is the dimension of `a', `b' and `p'.

-Detailed_Output

   p           is a double precision vector of arbitrary dimension
               containing the projection of `a' onto `b'. (`p' is
               necessarily parallel to `b'.)

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The projection of a vector `a' onto a vector `b' is, by definition,
   that component of `a' which is parallel to `b'. To find this
   component it is enough to find the scalar ratio of the length of
   `b' to the projection of `a' onto `b', and then use this number to
   scale the length of `b'. This ratio is given by

       ratio = < a, b > / < b, b >

   where <,> denotes the general vector dot product. This routine
   does not attempt to divide by zero in the event that `b' is the
   zero vector.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Define two sets of vectors and compute the projection of
      each vector of the first set on the corresponding vector of
      the second set.

      Example code begins here.


      /.
         Program vprojg_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NDIM         4
         #define SETSIZ       4

         /.
         Local variables.
         ./
         SpiceDouble          pvec   [NDIM];

         SpiceInt             i;

         /.
         Define the two vector sets.
         ./
         SpiceDouble          seta   [SETSIZ][NDIM] = {
                                   {6.0,  6.0,  6.0,  0.0},
                                   {6.0,  6.0,  6.0,  0.0},
                                   {6.0,  6.0,  0.0,  0.0},
                                   {6.0,  0.0,  0.0,  0.0} };

         SpiceDouble          setb   [SETSIZ][NDIM] = {
                                   {2.0,  0.0,  0.0,  0.0},
                                   {-3.0,  0.0,  0.0,  0.0},
                                   {0.0,  7.0,  0.0,  0.0},
                                   {0.0,  0.0,  9.0,  0.0} };

         /.
         Calculate the projection
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            vprojg_c ( seta[i], setb[i], NDIM, pvec );
            printf( "Vector A  :  %4.1f %4.1f %4.1f %4.1f\n",
                    seta[i][0], seta[i][1], seta[i][2], seta[i][3] );
            printf( "Vector B  :  %4.1f %4.1f %4.1f %4.1f\n",
                    setb[i][0], setb[i][1], setb[i][2], setb[i][3] );
            printf( "Projection:  %4.1f %4.1f %4.1f %4.1f\n",
                           pvec[0], pvec[1], pvec[2], pvec[3] );
            printf( " \n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Vector A  :   6.0  6.0  6.0  0.0
      Vector B  :   2.0  0.0  0.0  0.0
      Projection:   6.0  0.0  0.0  0.0

      Vector A  :   6.0  6.0  6.0  0.0
      Vector B  :  -3.0  0.0  0.0  0.0
      Projection:   6.0 -0.0 -0.0 -0.0

      Vector A  :   6.0  6.0  0.0  0.0
      Vector B  :   0.0  7.0  0.0  0.0
      Projection:   0.0  6.0  0.0  0.0

      Vector A  :   6.0  0.0  0.0  0.0
      Vector B  :   0.0  0.0  9.0  0.0
      Projection:   0.0  0.0  0.0  0.0


-Restrictions

   1)  No error detection or recovery schemes are incorporated into
       this routine except to insure that no attempt is made to
       divide by zero. Thus, the user is required to make sure that
       the vectors `a' and `b' are such that no floating point overflow
       will occur when the dot products are calculated.

-Literature_References

   [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
        7th Edition, Addison Wesley, 1988.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 01-NOV-2021 (JDR)

-Index_Entries

   n-dimensional vector projection

-&
*/

{ /* Begin vprojg_c */

   /*
   Error free:  no error tracing required.
   */

   /*
   Call the f2c'd Fortran routine.
   */
   vprojg_ (  ( doublereal * )  a,
              ( doublereal * )  b,
              ( integer    * ) &ndim,
              ( doublereal * )  p      );

} /* End vprojg_c */
