/*

-Procedure invstm_c ( Inverse of state transformation matrix )

-Abstract

   Return the inverse of a state transformation matrix.

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

   ROTATION

-Keywords

   MATH
   MATRIX
   TRANSFORMATION

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef invstm_c
   

   void invstm_c ( ConstSpiceDouble    mat    [6][6],
                   SpiceDouble         invmat [6][6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   mat        I   A state transformation matrix.
   invmat     O   The inverse of `mat'.

-Detailed_Input

   mat         is a state transformation matrix for converting states
               relative to one frame to states relative to another.
               The state transformation of a state vector, `s', is
               performed by the matrix-vector product.

                  mat * s.

               For `mat' to be a "true" state transformation matrix
               it must have the form

                   .-            -.
                   |       :      |
                   |   r   :   0  |
                   |.......:......|
                   |       :      |
                   |  w*r  :   r  |
                   |       :      |
                   `-            -'

               where `r' is a 3x3 rotation matrix, 0 is the 3x3 zero
               matrix and `w' is a 3x3 skew-symmetric matrix.

               NOTE: no checks are performed on `mat' to ensure that it
                     does indeed have the form described above.

-Detailed_Output

   invmat      is the inverse of `mat' under the operation of matrix
               multiplication.

               If `mat' has the form described above, then `invmat' has
               the form shown below.

                  .-             -.
                  |     t  :      |
                  |    r   :   0  |
                  |........:......|
                  |      t :    t |
                  | (w*r)  :   r  |
                  |        :      |
                  `-             -'

               (The superscript "t" denotes the matrix transpose
               operation.)

-Parameters

   None.

-Exceptions

   Error free.

   1)  No checks are performed to ensure that the input matrix is
       indeed a state transformation matrix.

-Files

   None.

-Particulars

   Given a matrix for transforming states relative frame 1 to
   states relative frame 2, the routine produces the inverse
   matrix. That is, it returns the matrix for transforming states
   relative to frame 2 to states relative to frame 1.

   This special routine exists because unlike the inverse of a
   rotation matrix, the inverse of a state transformation matrix,
   is NOT simply the transpose of the matrix.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose you have a geometric state of a spacecraft in Earth
      body-fixed reference frame and wish to express this state
      relative to an Earth centered J2000 frame. The following
      example code illustrates how to carry out this computation.

      Use the PCK kernel below to load the required high precision
      orientation of the ITRF93 Earth body-fixed reference frame.
      Note that the body ID code used in this file for the Earth is
      3000.

         earth_720101_070426.bpc


      Example code begins here.


      /.
         Program invstm_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          invmat [6][6];
         SpiceDouble          istat1 [6];
         SpiceDouble          istat2 [6];
         SpiceDouble          mat    [6][6];
         SpiceDouble          xmat   [6][6];

         SpiceInt             earth;

         /.
         Define the state of the spacecraft, in km and
         km/s, and the `et' epoch, in seconds past J2000.
         ./
         SpiceDouble          et = 0.0;
         SpiceDouble          state  [6] = { 175625246.29100420,
                                             164189388.12540060,
                                             -62935198.26067264,
                                                 11946.73372264,
                                                -12771.29732556,
                                                    13.84902914 };

         /.
         Load the required high precision Earth PCK.
         ./
         furnsh_c ( "earth_720101_070426.bpc" );

         /.
         First get the state transformation from J2000 frame
         to Earth body-fixed frame at the time of interest `et'.
         The body ID code used in high precision PCK files for
         the Earth is 3000; this number indicates that the
         terrestrial frame used is ITRF93.
         ./
         earth = 3000;
         tisbod_c ( "J2000", earth, et, mat );

         /.
         Get the inverse of `mat'.
         ./
         invstm_c ( mat, invmat );

         /.
         Transform from bodyfixed state to inertial state.
         ./
         mxvg_c ( invmat, state, 6, 6, istat1 );

         /.
         Print the resulting state.
         ./
         printf( "Input state in Earth centered J2000 frame, using "
                 "invstm_c:\n" );
         printf( "   Position: %15.3f %15.3f %15.3f\n",
                        istat1[0], istat1[1], istat1[2] );
         printf( "   Velocity: %15.3f %15.3f %15.3f\n",
                        istat1[3], istat1[4], istat1[5] );

         /.
         Compute the same state using sxform_c.
         ./
         sxform_c ( "ITRF93", "J2000", et, xmat );
         mxvg_c ( xmat, state, 6, 6, istat2 );

         printf( "\n" );
         printf( "Input state in Earth centered J2000 frame, using "
                 "sxform_c:\n" );
         printf( "   Position: %15.3f %15.3f %15.3f\n",
                        istat2[0], istat2[1], istat2[2] );
         printf( "   Velocity: %15.3f %15.3f %15.3f\n",
                        istat2[3], istat2[4], istat2[5] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Input state in Earth centered J2000 frame, using invstm_c:
         Position:   192681395.921  -143792821.383   -62934296.473
         Velocity:          30.312          32.007          13.876

      Input state in Earth centered J2000 frame, using sxform_c:
         Position:   192681395.921  -143792821.383   -62934296.473
         Velocity:          30.312          32.007          13.876


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 25-NOV-2021 (JDR)

-Index_Entries

   inverse of state transformation matrix

-&
*/

{ /* Begin invstm_c */

   /*
   Local variables.
   */
   SpiceDouble        matTmp     [6][6];

   /*
   Error free: no error tracing required.
   */

   /*
   Transpose the input `mat' to put it in column-major order.
   */
   xpose6_c ( mat, matTmp );

   /*
   Call the f2c'd Fortran routine.
   */
   invstm_ (  ( doublereal * )  matTmp,
              ( doublereal * )  invmat  );

   /*
   Transpose the output matrix to put it in row-major order.
   */
   xpose6_c ( invmat, invmat );

} /* End invstm_c */
