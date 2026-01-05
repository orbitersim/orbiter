/*

-Procedure rotmat_c ( Rotate a matrix )

-Abstract

   Apply a rotation of `angle' radians about axis `iaxis' to a matrix.
   This rotation is thought of as rotating the coordinate system.

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

   MATRIX
   ROTATION

*/

   #include "SpiceZfc.h"
   #include "SpiceUsr.h"
   #undef    rotmat_c


   void rotmat_c ( ConstSpiceDouble   m1[3][3],
                   SpiceDouble        angle,
                   SpiceInt           iaxis,
                   SpiceDouble        mout[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m1        I     Matrix to be rotated.
   angle     I     Angle of rotation (radians).
   iaxis     I     Axis of rotation (X=1, Y=2, Z=3).
   mout      O     Resulting rotated matrix.

-Detailed_Input

   m1          is a matrix to which a rotation is to be applied.
               In matrix algebra, the components of the matrix are
               relative to one particular coordinate system. Applying
               rotmat_c changes the components of m1 so that they are
               relative to a rotated coordinate system.

   angle       is the angle in radians through which the original
               coordinate system is to be rotated.

   iaxis       is an index for the axis of the original coordinate system
               about which the rotation by angle is to be performed.
               iaxis = 1,2 or 3 designates the x-, y- or z-axis,
               respectively.

-Detailed_Output

   mout        is the matrix resulting from the application of the
               specified rotation to the input matrix m1. If

                  [angle]
                        iaxis

               denotes the rotation matrix by angle radians about iaxis,
               (see the Rotations Required Reading document) then mout is
               given by the following matrix equation:

                  mout = [angle]      * m1
                                iaxis

               mout can overwrite m1.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If the axis index is not in the range 1 to 3, it will be
       treated the same as that integer 1, 2, or 3 that is congruent
       to it mod 3.

-Files

   None.

-Particulars

   None.

-Examples

   Suppose that to rotate the a set of inertial axes to body fixed
   axes, one must first roll the coordinate axes about the x-axis by
   angle r to get x', y', z'. From this one must pitch about the
   y' axis by angle o to get x'', y'', z''.  And finally yaw the
   x'', y'', z'' about the z'' axis by angle y to obtain the
   transformation to bodyfixed coordinates. If id is the identity
   matrix, then the following code fragment generates the
   transformation from inertial to body fixed.

      rotmat_c ( id, r, 1, m1   );
      rotmat_c ( m1, p, 2, m2   );
      rotmat_c ( m2, y, 3, tibf );

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

   -CSPICE Version 1.1.1, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.1.0, 22-OCT-1998 (NJB)

       Made input matrix const.

   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB) (WMO) (WLT)

       Based on SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)

-Index_Entries

   rotate a matrix

-&
*/

{ /* Begin rotmat_c */

   /*
   Local variables
   */
   SpiceDouble             loc_mat[3][3];


   /*
   Transpose the input matrix to put it in column-major order.
   */
   xpose_c ( m1, loc_mat );

   /*
   Call the f2c'd version of the routine. Note that the Fortran version
   ROTMAT works in place.
   */
   rotmat_ ( (doublereal *)  loc_mat,
             (doublereal *) &angle,
             (integer    *) &iaxis,
             (doublereal *)  loc_mat    );

   /*
   Transpose the output matrix to put it in row-major order.
   */
   xpose_c ( loc_mat, mout );


} /* End rotmat_c */
