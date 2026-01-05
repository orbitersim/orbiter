/*

-Procedure vdistg_c ( Vector distance, general dimension )

-Abstract

   Return the distance between two vectors of arbitrary dimension.

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
   #undef    vdistg_c


   SpiceDouble vdistg_c ( ConstSpiceDouble   * v1,
                          ConstSpiceDouble   * v2,
                          SpiceInt             ndim )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v1,
   v2         I   Two vectors of arbitrary dimension.
   ndim       I   The common dimension of v1 and v2

   The function returns the distance between v1 and v2.

-Detailed_Input

   v1,
   v2          are two vectors of arbitrary dimension, the
               distance between which is desired.

   ndim        is the common dimension of v1 and v2. ndim must be
               non-negative and must not exceed the minimum of the
               declared sizes of the actual arguments corresponding
               to v1 and v2.

-Detailed_Output

   The function returns the distance between v1 and v2. This is
   defined as

            ||  v1 - v2  ||,

   where || x || indicates the Euclidean norm of the vector x.

   If ndim is less than 1, the function value is set to 0..

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The Euclidean norm of an n-dimensional vector

      (x ,  x , ... , x )
        1    2         n

   is defined as

                                              1/2
            2        2                  2
      (   x    +   x    +  . . .  +   x     ).
           1        2                  n

   This number is the distance of the point (x, y, z) from the
   origin. If n = 3, and A and B are two vectors whose components
   are

      ( a[0], a[1], a[2] )    and    ( b[0], b[1], b[2] ),

   then the distance between A and B is the norm of the difference
   A - B, which has components

      (  a[0] - b[0],  a[1] - b[1],  a[2] - b[2]  ).

   A related routine is vdist_c, which computes the distance between
   two 3-vectors.

-Examples

   1)  If v1 is

          [ 2.0,  3.0 ]

       and v2 is

          [ 5.0,  7.0 ],

       and ndim is 2, then

          vdistg_c ( v1, v2, ndim );

       will be 5.0.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.1, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input vectors const.

   -CSPICE Version 1.0.0, 23-MAR-1998 (EDW)

-Index_Entries

   distance between n-dimensional vectors

-&
*/

{ /* Begin vdistg_c */

   /*
   Local variables
   */

   SpiceInt                i;
   SpiceDouble             dist;
   SpiceDouble             scale;


   /* Initialize dist and scale to zero. */

   dist  = 0.;
   scale = 0.;


   /* Check ndim makes sense. */

   if ( ndim <= 0 )
      {
      return 0.;
      }


   /*
   Determine an appropriate scale factor to prevent numerical
   overflow.  Overflow would be bad!
   */

   for ( i = 0; i < ndim; i++ )
      {
      scale = MaxAbs( scale, v1[i] - v2[i] );
      }


   /* If the vectors are equal, return zero. */

   if ( scale == 0. )
      {
      return 0.;
      }


   /* Do the calculation.  Not very involved. */

   for ( i = 0; i < ndim; i++ )
      {
      dist += pow( ( v1[i] - v2[i] ) / scale, 2 );
      }

   return ( scale * sqrt( dist ) );


} /* End vdistg_c */
