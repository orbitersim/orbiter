/*

-Procedure frame_c ( Build a right handed coordinate frame )

-Abstract

   Build a right handed orthonormal frame (x,y,z) from a
   3-dimensional input vector, where the X-axis of the resulting
   frame is parallel to the original input vector.

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

   AXES
   FRAME

*/
   #include <math.h>
   #include "SpiceUsr.h"


   void frame_c ( SpiceDouble x[3],
                  SpiceDouble y[3],
                  SpiceDouble z[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   x         I-O  Input vector. A parallel unit vector on output.
   y          O   Unit vector in the plane orthogonal to `x'.
   z          O   Unit vector given by the cross product <x,y>.

-Detailed_Input

   x           is a 3-dimensional vector used to form the first vector
               of a right-handed orthonormal triple.

-Detailed_Output

   x,
   y,
   z           are the 3-dimensional unit vectors that form a right
               handed orthonormal frame, where `x' is now a unit vector
               parallel to the original input vector in `x'. There are no
               special geometric properties connected to `y' and `z' (other
               than that they complete the right handed frame).

-Parameters

   None.

-Exceptions

   Error free.

   1)  If `x' on input is the zero vector the "standard" frame (ijk)
       is returned.

-Files

   None.

-Particulars

   Given an input vector x, this routine returns unit vectors x,
   y, and z such that xyz forms a right-handed orthonormal frame
   where the output x is parallel to the input x.

   This routine is intended primarily to provide a basis for
   the plane orthogonal to x. There are no special properties
   associated with y and z other than that the resulting xyz frame
   is right handed and orthonormal. There are an infinite
   collection of pairs (y,z) that could be used to this end.
   Even though for a given x, y and z are uniquely determined, users
   should regard the pair (y,z) as a random selection from this
   infinite collection.

   For instance, when attempting to determine the locus of points
   that make up the limb of a triaxial body, it is a straightforward
   matter to determine the normal to the limb plane. To find
   the actual parametric equation of the limb one needs to have
   a basis of the plane. This routine can be used to get a basis
   in which one can describe the curve and from which one can
   then determine the principal axes of the limb ellipse.

-Examples

   In addition to using a vector to construct a right handed frame
   with the x-axis aligned with the input vector, one can construct
   right handed frames with any of the axes aligned with the input
   vector.

   For example suppose we want a right hand frame xyz with the
   z-axis aligned with some vector v. Assign v to z

         z[0] = v[0];
         z[1] = v[1];
         z[2] = v[2];

   Then call frame_c with the arguments x,y,z cycled so that z
   appears first.

         frame_c (z, x, y);

   The resulting xyz frame will be orthonormal with z parallel
   to the vector v.

   To get an xyz frame with y parallel to v perform the following

         y[0] = v[0];
         y[1] = v[1];
         y[2] = v[2];

         frame_c (y, z, x);

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 03-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 26-MAR-1999 (NJB)

-Index_Entries

   build a right handed coordinate frame

-&
*/

{ /* Begin frame_c */


   SpiceDouble             a;
   SpiceDouble             b;
   SpiceDouble             c;
   SpiceDouble             f;

   SpiceInt                s0;
   SpiceInt                s1;
   SpiceInt                s2;


   /*
   First make x into a unit vector.
   */
   vhat_c ( x, x );


   /*
   We'll need the squares of the components of x in a bit.
   */
   a  =  x[0] * x[0];
   b  =  x[1] * x[1];
   c  =  x[2] * x[2];


   /*
   If X is zero, then just return the ijk frame.
   */
   if ( a+b+c == 0.0  )
   {
      x[0] = 1.0;
      x[1] = 0.0;
      x[2] = 0.0;

      y[0] = 0.0;
      y[1] = 1.0;
      y[2] = 0.0;

      z[0] = 0.0;
      z[1] = 0.0;
      z[2] = 1.0;

      return;
   }


   /*
   If we make it this far, determine which component of x has the
   smallest magnitude.  This component will be zero in y. The other
   two components of x will put into y swapped with the sign of
   the first changed.  From there, z can have only one possible
   set of values which it gets from the smallest component
   of x, the non-zero components of y and the length of y.
   */

   if (  ( a <= b ) && ( a <= c )  )
   {
      f  = sqrt ( b + c );
      s0 = 0;
      s1 = 1;
      s2 = 2;
   }

   else if (  ( b <= a ) && ( b <= c )  )
   {
      f  = sqrt ( a + c );
      s0 = 1;
      s1 = 2;
      s2 = 0;
   }

   else
   {
      f  = sqrt ( a + b );
      s0 = 2;
      s1 = 0;
      s2 = 1;
   }

   /*
   Note: by construction, f is the magnitude of the large components
   of x.  With this in mind, one can verify by inspection that x, y
   and z yield an orthonormal frame.  The right handedness follows
   from the assignment of values to s0, s1 and s2 (they are merely
   cycled from one case to the next).
   */

   y[s0]  =   0.0;
   y[s1]  = - x[s2] / f;
   y[s2]  =   x[s1] / f;

   z[s0]  =   f;
   z[s1]  = - x[s0] * y[s2];
   z[s2]  =   x[s0] * y[s1];


} /* End frame_c */
