/*

-Procedure twovec_c ( Two vectors defining an orthonormal frame )

-Abstract

   Find the transformation to the right-handed frame having a
   given vector as a specified axis and having a second given
   vector lying in a specified coordinate plane.

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
   ROTATION
   TRANSFORMATION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef    twovec_c


   void twovec_c ( ConstSpiceDouble    axdef  [3],
                   SpiceInt            indexa,
                   ConstSpiceDouble    plndef [3],
                   SpiceInt            indexp,
                   SpiceDouble         mout   [3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   axdef      I   Vector defining a principal axis.
   indexa     I   Principal axis number of `axdef' (x=1, y=2, z=3).
   plndef     I   Vector defining (with `axdef') a principal plane.
   indexp     I   Second axis number (with `indexa') of principal
                  plane.
   mout       O   Output rotation matrix.

-Detailed_Input

   axdef       is a vector defining one of the principal axes of a
               coordinate frame.

   indexa      is a number that determines which of the three
               coordinate axes contains `axdef'.

               If `indexa' is 1 then `axdef' defines the X axis of the
               coordinate frame.

               If `indexa' is 2 then `axdef' defines the Y axis of the
               coordinate frame.

               If `indexa' is 3 then `axdef' defines the Z axis of the
               coordinate frame.

   plndef      is a vector defining (with `axdef') a principal plane of
               the coordinate frame. `axdef' and `plndef' must be
               linearly independent.

   indexp      is the second axis of the principal frame determined by
               `axdef' and `plndef'.  `indexa', `indexp' must be different
               and be integers from 1 to 3.

               If `indexp' is 1, the second axis of the principal
               plane is the X-axis.

               If `indexp' is 2, the second axis of the principal
               plane is the Y-axis.

               If `indexp' is 3, the second axis of the principal plane
               is the Z-axis.

-Detailed_Output

   mout        is a rotation matrix that transforms coordinates given
               in the input frame to the frame determined by `axdef',
               `plndef', `indexa' and `indexp'.

-Parameters

   None.

-Exceptions

   1)  If `indexa' or `indexp' is not in the set {1,2,3}, the error
       SPICE(BADINDEX) is signaled by a routine in the call tree of
       this routine.

   2)  If `indexa' and `indexp' are the same, the error
       SPICE(UNDEFINEDFRAME) is signaled by a routine in the call
       tree of this routine.

   3)  If the cross product of the vectors `axdef' and `plndef' is zero,
       the error SPICE(DEPENDENTVECTORS) is signaled by a routine in
       the call tree of this routine.

-Files

   None.

-Particulars

   Given two linearly independent vectors there is a unique
   right-handed coordinate frame having:

      `axdef' lying along the `indexa' axis.

      `plndef' lying in the indexa-indexp coordinate plane.

   This routine determines the transformation matrix that transforms
   from coordinates used to represent the input vectors to the
   the system determined by `axdef' and `plndef'. Thus a vector
   (x,y,z) in the input coordinate system will have coordinates

                   t
      mout* (x,y,z)

   in the frame determined by `axdef' and `plndef'.

-Examples

   The rotation matrix ticc from inertial to Sun-Canopus
   (celestial) coordinates is found by the call

      twovec_c ( Sun_vector, 3, Canopus_vector, 1, ticc );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 22-OCT-1998 (NJB)

       Made input matrices const.

   -CSPICE Version 1.0.0, 02-MAR-1998 (WLT) (WMO)

-Index_Entries

   define an orthonormal frame from two vectors

-&
*/

{ /* Begin twovec_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "twovec_c" );

   /*
   Call the f2c'd routine.
   */
   twovec_ ( ( doublereal * )  axdef,
             ( integer    * )  &indexa,
             ( doublereal * )  plndef,
             ( integer    * )  &indexp,
             ( doublereal * )  mout     );

   /*
   Transpose the output matrix to put it in row-major
   order.
   */
   xpose_c ( mout, mout );


   chkout_c ( "twovec_c" );

} /* End twovec_c */
