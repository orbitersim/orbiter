/*

-Procedure edpnt_c ( Ellipsoid point  )

-Abstract

   Scale a point so that it lies on the surface of a specified
   triaxial ellipsoid that is centered at the origin and aligned
   with the Cartesian coordinate axes.

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

   ELLIPSOID
   GEOMETRY
   MATH

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void edpnt_c  ( ConstSpiceDouble    p      [3],
                   SpiceDouble         a,
                   SpiceDouble         b,
                   SpiceDouble         c,
                   SpiceDouble         ep     [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   p          I   A point in three-dimensional space.
   a          I   Semi-axis length in the X direction.
   b          I   Semi-axis length in the Y direction.
   c          I   Semi-axis length in the Z direction.
   ep         O   Point on ellipsoid.

-Detailed_Input

   p           is a non-zero point in three-dimensional space.

   a,
   b,
   c           are, respectively, the semi-axis lengths of a triaxial
               ellipsoid in the X, Y, and Z directions. The axes of
               the ellipsoid are aligned with the axes of the
               Cartesian coordinate system.

-Detailed_Output

   ep          is the result of scaling the input point `p' so that
               it lies on the surface of the triaxial ellipsoid
               defined by the input semi-axis lengths.

-Parameters

   None.

-Exceptions

   1)  If any of the target ellipsoid's semi-axis lengths is non-positive,
       the error SPICE(INVALIDAXES) is signaled by a routine in the call
       tree of this routine.

   2)  If `p' is the zero vector, the error SPICE(ZEROVECTOR) is
       signaled by a routine in the call tree of this routine.

   3)  If the level surface parameter of the input point underflows, the
       error SPICE(POINTTOOSMALL) is signaled by a routine in the call tree
       of this routine.

-Files

   None.

-Particulars

   This routine efficiently computes the ellipsoid surface point
   corresponding to a specified ray emanating from the origin.
   Practical examples of this computation occur in the CSPICE
   routines latsrf_c and srfrec_c.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Find the surface intercept point on an ellipsoid having radii

          ( 3, 2, 1 )

      of the ray emanating from the origin and having direction
      vector

          ( 1, 1, 1 )


      Example code begins here.


      /.
         Program edpnt_ex1
      ./
      #include <math.h>
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         SpiceDouble          a;
         SpiceDouble          b;
         SpiceDouble          c;
         SpiceDouble          v      [3];
         SpiceDouble          ep     [3];
         SpiceDouble          level;

         a = 3.0;
         b = 2.0;
         c = 1.0;

         vpack_c ( 1.0, 1.0, 1.0, v );

         edpnt_c ( v, a, b, c, ep );

         printf( "EP    =  %17.14f %17.14f %17.14f\n", ep[0], ep[1], ep[2] );

         /.
         Verify that `ep' is on the ellipsoid.
         ./
         level =   pow( (ep[0]/a), 2 ) + pow( (ep[1]/b), 2 )
                 + pow( (ep[2]/c), 2 );

         printf( "LEVEL =  %17.14f\n", level );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      EP    =   0.85714285714286  0.85714285714286  0.85714285714286
      LEVEL =   1.00000000000000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 08-FEB-2021 (JDR)

-Index_Entries

   scale point to lie on ellipsoid

-&
*/

{ /* Begin edpnt_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "edpnt_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   edpnt_  (  ( doublereal * )  p,
              ( doublereal * ) &a,
              ( doublereal * ) &b,
              ( doublereal * ) &c,
              ( doublereal * )  ep     );

   chkout_c ( "edpnt_c" );

} /* End edpnt_c */
