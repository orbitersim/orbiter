/*

-Procedure vproj_c ( Vector projection, 3 dimensions )

-Abstract

   Compute the projection of one 3-dimensional vector onto another
   3-dimensional vector.

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
   #undef  vproj_c

   void vproj_c ( ConstSpiceDouble   a[3],
                  ConstSpiceDouble   b[3],
                  SpiceDouble        p[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   a          I   The vector to be projected.
   b          I   The vector onto which `a' is to be projected.
   p          O   The projection of `a' onto `b'.

-Detailed_Input

   a           is a double precision, 3-dimensional vector. This
               vector is to be projected onto the vector `b'.

   b           is a double precision, 3-dimensional vector. This
               vector is the vector which receives the projection.

-Detailed_Output

   p           is a double precision, 3-dimensional vector containing
               the projection of `a' onto `b'. (`p' is necessarily parallel
               to `b'.) If `b' is the zero vector then `p' will be returned
               as the zero vector.

               `p' may overwrite either `a' or `b'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   Given any vectors `a' and `b', there is a unique decomposition of
   `a' as a sum v + p such that `v', the dot product of `v' and `b', is zero,
   and the dot product of `p' with `b' is equal the product of the
   lengths of `p' and `b'. `p' is called the projection of `a' onto `b'. It
   can be expressed mathematically as

      dot(a,b)
      -------- * b
      dot(b,b)

   (This is not necessarily the prescription used to compute the
   projection. It is intended only for descriptive purposes.)

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
         Program vproj_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define NDIM         3
         #define SETSIZ       4

         /.
         Local variables.
         ./
         SpiceDouble          pvec   [NDIM];

         SpiceInt             i;

         /.
         Define the two vector sets.
         ./
         SpiceDouble          seta   [SETSIZ][NDIM] =

                                     { {6.0,  6.0,  6.0},
                                       {6.0,  6.0,  6.0},
                                       {6.0,  6.0,  0.0},
                                       {6.0,  0.0,  0.0} };

         SpiceDouble          setb   [SETSIZ][NDIM] =

                                     { { 2.0,  0.0,  0.0},
                                       {-3.0,  0.0,  0.0},
                                       { 0.0,  7.0,  0.0},
                                       { 0.0,  0.0,  9.0} };

         /.
         Calculate the projection
         ./
         for ( i = 0; i < SETSIZ; i++ )
         {

            vproj_c ( seta[i], setb[i], pvec );

            printf( "Vector A  :  %4.1f %4.1f %4.1f\n",
                    seta[i][0], seta[i][1], seta[i][2] );
            printf( "Vector B  :  %4.1f %4.1f %4.1f\n",
                    setb[i][0], setb[i][1], setb[i][2] );
            printf( "Projection:  %4.1f %4.1f %4.1f\n",
                    pvec[0], pvec[1], pvec[2]          );
            printf( " \n" );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Vector A  :   6.0  6.0  6.0
      Vector B  :   2.0  0.0  0.0
      Projection:   6.0  0.0  0.0

      Vector A  :   6.0  6.0  6.0
      Vector B  :  -3.0  0.0  0.0
      Projection:   6.0 -0.0 -0.0

      Vector A  :   6.0  6.0  0.0
      Vector B  :   0.0  7.0  0.0
      Projection:   0.0  6.0  0.0

      Vector A  :   6.0  0.0  0.0
      Vector B  :   0.0  0.0  9.0
      Projection:   0.0  0.0  0.0


-Restrictions

   1)  An implicit assumption exists that `a' and `b' are specified in
       the same reference frame. If this is not the case, the
       numerical result has no meaning.

-Literature_References

   [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
        7th Edition, Addison Wesley, 1988.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete example
       to the -Examples section. Added entry to -Restrictions section.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (WLT)

-Index_Entries

   3-dimensional vector projection

-&
*/

{ /* Begin vproj_c */


   /*
   Local variables
   */

   SpiceDouble     biga;
   SpiceDouble     bigb;
   SpiceDouble     r[3];
   SpiceDouble     t[3];
   SpiceDouble     scale;


   biga = MaxAbs ( a[0] ,MaxAbs ( a[1], a[2] ) );
   bigb = MaxAbs ( b[0] ,MaxAbs ( b[1], b[2] ) );


   /*
   If a or b is zero, return the zero vector.
   */

   if ( biga == 0 || bigb == 0 )
      {
      p[0] = 0.0;
      p[1] = 0.0;
      p[2] = 0.0;
      return;
      }


   vscl_c ( 1./biga, a, t );
   vscl_c ( 1./bigb, b, r );

   scale = vdot_c (t,r) * biga  / vdot_c (r,r);

   vscl_c ( scale, r, p );


} /* End vproj_c */
