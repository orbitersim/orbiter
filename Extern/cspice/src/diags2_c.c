/*

-Procedure diags2_c   ( Diagonalize symmetric 2x2 matrix )

-Abstract

   Diagonalize a symmetric 2x2 matrix.

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

   ELLIPSE
   MATRIX
   ROTATION
   TRANSFORMATION

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef   diags2_c


   void diags2_c ( ConstSpiceDouble    symmat [2][2],
                   SpiceDouble         diag   [2][2],
                   SpiceDouble         rotate [2][2]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   symmat     I   A symmetric 2x2 matrix.
   diag       O   A diagonal matrix similar to `symmat'.
   rotate     O   A rotation used as the similarity transformation.

-Detailed_Input

   symmat      is a symmetric 2x2 matrix. That is, `symmat' has the
               form

                  .-        -.
                  |  a    b  |
                  |          |
                  |  b    C  |
                  `-        -'

               This routine uses only the upper-triangular
               elements of `symmat', that is, the elements

                  symmat[0][0]
                  symmat[0][1]
                  symmat[1][1]

               to determine the outputs `diag' and `rotate'.

-Detailed_Output

   diag,
   rotate      are, respectively, a diagonal matrix and a 2x2
               rotation matrix that satisfy the equation

                                   T
                  diag   =   rotate   *  symmat  *  rotate.

               In other words, `diag' is similar to `symmat', and `rotate'
               is a change-of-basis matrix that diagonalizes `symmat'.
               diags2_c chooses `rotate' so that its angle of rotation has
               the smallest possible magnitude. If there are two rotations
               that meet these criteria (they will be inverses of one
               another), either rotation may be chosen.

-Parameters

   None.

-Exceptions

   Error free.

   1)  The matrix element symmat[1][0] is not used in this routine's
       computations, so the condition

          symmat[0][1]  !=  symmat[1][0]

       has no effect on this routine's outputs.

-Files

   None.

-Particulars

   The capability of diagonalizing a 2x2 symmetric matrix is
   especially useful in a number of geometric applications
   involving quadratic curves such as ellipses. Such curves are
   described by expressions of the form

         2                    2
      A x   +   B xy   +   C y   +   D x    +    E y   +   F   =   0.

   Diagonalization of the matrix

      .-         -.
      | A     B/2 |
      |           |
      | B/2     C |
      `-         -'

   allows us to perform a coordinate transformation (a rotation,
   specifically) such that the equation of the curve becomes

         2         2
      P u   +   Q v   +   R u    +    S v   +   T   =   0

   in the transformed coordinates. This form is much easier to
   handle. If the quadratic curve in question is an ellipse,
   we can easily find its center, semi-major axis, and semi-minor
   axis from the second equation.

   Ellipses turn up frequently in navigation geometry problems;
   for example, the limb and terminator (if we treat the Sun as a
   point source) of a body modeled as a tri-axial ellipsoid are
   ellipses.

   A mathematical note: because `symmat' is symmetric, we can ALWAYS
   find an orthogonal similarity transformation that diagonalizes
   `symmat', and we can choose the similarity transformation to be a
   rotation matrix. By `orthogonal' we mean that if the `rotate' is
   the matrix in question, then

            T                         T
      rotate  rotate  =  rotate rotate  =  I.

   The reasons this routine handles only the 2x2 case are: first,
   the 2x2 case is much simpler than the general case, in which
   iterative diagonalization methods must be used, and second, the
   2x2 case is adequate for solving problems involving ellipses in
   3 dimensional space. Finally, this routine can be used to
   support a routine that solves the general-dimension diagonalization
   problem for symmetric matrices.

   Another feature of the routine that might provoke curiosity is
   its insistence on choosing the diagonalization matrix that
   rotates the original basis vectors by the smallest amount. The
   rotation angle of `rotate' is of no concern for most applications,
   but can be important if this routine is used as part of an
   iterative diagonalization method for higher-dimensional matrices.
   In that case, it is most undesirable to interchange diagonal
   matrix elements willy-nilly; the matrix to be diagonalized could
   get ever closer to being diagonal without converging. Choosing
   the smallest rotation angle precludes this possibility.

-Examples

   1)  A case that can be verified by hand computation:
       Suppose symmat is

          .-            -.
          |  1.0    4.0  |
          |              |
          |  4.0   -5.0  |
          `-            -'

       Then symmat is similar to the diagonal matrix

          .-            -.
          |  3.0    0.0  |
          |              |
          |  0.0   -7.0  |
          `-            -'

       so

          diag[0][0] =  3.
          diag[1][0] =  0.
          diag[0][1] =  0.
          diag[1][1] = -7.

       and rotate is

          .-                                             -.
          |  0.89442719099991588    -0.44721359549995794  |
          |                                               |
          |  0.44721359549995794     0.89442719099991588  |
          `-                                             -'

      which is an approximation to

          .-                                   -.
          |  .4 * 5**(1/2)     -.2 * 5**(1/2)   |
          |                                     |
          |  .2 * 5**(1/2)      .4 * 5**(1/2)   |
          `-                                   -'


   2)  Suppose we want to find the semi-axes of the ellipse defined
       by
              2                 2
          27 x  +  10 xy  +  3 y   =  1

       We can write the above equation as the matrix equation

          .-     -.  .-         -.  .- -.
          | x   y |  | 27     5  |  | x |    =   1
          `-     -'  |           |  |   |
                     |  5     3  |  | y |
                     `-         -'  `- -'

       Let symmat be the symmetric matrix on the left. The code
       fragment

          symmat[0][0]  =  27.0;
          symmat[1][0]  =   5.0;
          symmat[0][1]  =   5.0;
          symmat[1][1]  =   3.0;

          diags2_c ( symmat, diag, rotate );

       will return diag, an array containing the eigenvalues of
       symmat, and rotate, the coordinate transformation required
       to diagonalize symmat. In this case,

          diag[0][0]   =  28.
          diag[1][0]   =   0.
          diag[0][1]   =   0.
          diag[1][1]   =   2.

        and

          rotate[0][0] =   0.980580675690920
          rotate[1][0] =   0.196116135138184
          rotate[0][1] =  -0.196116135138184
          rotate[1][1] =   0.980580675690920

       The columns of rotate give the ellipse's axes, after scaling
       them by

                 1                            1
          ----------------     and     ---------------
            ____________                 ____________
          \/  diag[0][0]               \/  diag[1][1]

       respectively.

       If smajor and sminor are semi-major and semi-minor axes,
       we can find them as shown below. For brevity, we omit the
       check for zero or negative eigenvalues.

          for ( i = 0;  i < 2;  i++ )
          {
             smajor[i] = rotate[i][0]  /  sqrt( diag[0][0] );
             sminor[i] = rotate[i][1]  /  sqrt( diag[1][1] );
          }

-Restrictions

   None.

-Literature_References

   [1]  T. Apostol, "Calculus, Vol. II," chapter 5, "Eigenvalues of
        Operators Acting on Euclidean Spaces," John Wiley & Sons,
        1969.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 17-JUN-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 13-JUL-1999 (NJB)

-Index_Entries

   diagonalize symmetric 2x2_matrix

-&
*/

{ /* Begin diags2_c */

   /*
   Local constants
   */


   /*
   Static variables
   */
   static SpiceDouble      ident [2][2] = { {1., 0.}, {0., 1.} };


   /*
   Local variables
   */
   SpiceDouble             a;
   SpiceDouble             b;
   SpiceDouble             c;
   SpiceDouble             eigvec [2];
   SpiceDouble             root1  [2];
   SpiceDouble             root2  [2];
   SpiceDouble             scale;


   /*
   Error free.
   */

   /*
   We check for the case of a diagonal input matrix, since
   eigenvector determination is simplified by ruling out this
   case.
   */

   if ( symmat [0][1] == 0. )
   {
      MOVED  ( ident,  4, rotate );
      MOVED  ( symmat, 4, diag   );

      /*
      Explicitly zero out the [1][0] entry of diag, since diag is
      guaranteed to be diagonal.
      */
      diag[1][0] = 0.0;

      return;
   }


   /*
   Getting here means there's some actual work to do.  We start out
   by scaling our matrix, in order to reduce the chance of overflow.
   We divide everything by the largest magnitude of any element of
   symmat.  We're guaranteed that scale is non-zero, since the 0
   matrix is diagonal.
   */

   scale  =  MaxAbs ( symmat[0][0], symmat[0][1] );
   scale  =  MaxAbs ( scale,        symmat[1][1] );

   a      =  symmat[0][0] / scale;
   b      =  symmat[0][1] / scale;
   c      =  symmat[1][1] / scale;


   /*
   Compute the eigenvalues of the scaled version of symmat.  The
   eigenvalues are roots of the equation

        det (  (1 / scale) * symmat  -  x * identity  ) = 0,

   or equivalently,

         2                             2
        x   -  ( a + c ) x  +  ( ac - b )  =   0.

   */

   rquad_c (  1.0,  -(a + c),   a*c - b*b,   root1,  root2 );


   /*
   root1 is the root corresponding to the positive discriminant term;
   this is guaranteed by rquad_c.
   */
   diag[0][0] = root1[0];
   diag[1][0] = 0.;
   diag[0][1] = 0.;
   diag[1][1] = root2[0];


   /*
   Our next job is to find an eigenvector corresponding to the
   eigenvalue of smaller magnitude.  We can unitize it and choose
   an orthogonal unit vector so as to create the desired rotation
   matrix.

      If our original matrix is

         +-        -+
         |  a    b  |
         |          |,
         |  b    c  |
         +-        -+

      then the matrix

         +-                                  -+
         |  a - diag[x][x]    b               |
         |                                    |
         |  b                 c - diag[x][x]  |
         +-                                  -+

      maps to zero all elements of the eigenspace corresponding to
      diag[x][x], where x is either 0 or 1.

      So

         +-                -+           +-                -+
         |  b               |           |  diag[x][x] - c  |
         |                  |   and     |                  |
         |  diag[x][x] - a  |           |  b               |
         +-                -+           +-                -+

      are candidates for eigenvectors for diag[x][x].  To minimize
      loss of accuracy in our eigenvector due to subtraction of
      nearly equal quantities, we choose the vector in which the
      term involving the eigenvalue has the larger magnitude.

      Note that there is nothing to be gained as far as accuracy is
      concerned by working with one eigenvalue as opposed to the
      other:  the magnitudes of the quantities diag[x][x] - a and
      diag[x][x] - c would be interchanged by taking x = 1 instead
      of x = 0.
   */

   if (  fabs( diag[0][0] - a )  >=  fabs( diag[0][0] - c )  )
   {

      /*
      In this case, the second eigenvector component eigvec[1]
      should be larger than |b|; we explain why in detail below.
      We use the MaxVal macro below to guard against reversal of the
      inequality due to round-off error.
      */

      eigvec[0]  =  b;
      eigvec[1]  =  MaxVal (   diag[0][0] - a,   fabs(b)   );

      /*
      Recall that diag[0][0] is an eigenvalue of the scaled version
      of symmat

         +-      -+
         | a    b |
         |        |.
         | b    c |
         +-      -+

      diag[0][0] is the positive-discriminant root of this matrix's
      characteristic equation.  eigvec's y-component

         diag[0][0] - a

      is positive and of magnitude at least as large as that of B,
      since it is the larger of
                                               ______________________
                                              /         2
                            c - a            / ( a - c )           2
         diag[0][0] - a  =  -----   +    \  /  ----------    +    b
                              2           \/       4

      and
                                               ______________________
                                              /         2
                            a - c            / ( a - c )           2
         diag[0][0] - c  =  -----   +    \  /  ----------    +    b
                              2           \/       4

      Equality between these expressions can occur only when a is
      equal to c, in which case both expressions are equal (except
      for round-off error) to |b|.

      So the argument of eigvec is in the interval [pi/4, 3*pi/4].
      The second eigenvector is eigvec, and the first
      eigenvector is found by rotating eigvec by -pi/2.  Since
      diag[0][0] is the eigenvalue for the SECOND eigenvector, we
      must swap the eigenvalues.
      */

      /*
      Unitize the eigenvector.
      */
      vhatg_c ( eigvec, 2, eigvec );

      rotate[0][0]  =  eigvec[1];
      rotate[1][0]  = -eigvec[0];
      rotate[0][1]  =  eigvec[0];
      rotate[1][1]  =  eigvec[1];

      swapd_ (  &(diag[0][0]),  &(diag[1][1])  );

   }

   else
   {

      eigvec[0]  =  MaxVal (  diag[0][0] - c,   fabs(b)  );
      eigvec[1]  =  b;

      /*
      The x-component of eigvec is positive and has magnitude
      greater than or equal to that of the y-component of eigvec.
      The argument of eigvec is in [-pi/4, pi/4], and the second
      eigenvector is found by rotating eigvec by pi/2.
      */

      /*
      Unitize the eigenvector.
      */
      vhatg_c ( eigvec, 2, eigvec );

      rotate[0][0]  =  eigvec[0];
      rotate[1][0]  =  eigvec[1];
      rotate[0][1]  = -eigvec[1];
      rotate[1][1]  =  eigvec[0];
   }

   /*
   We must scale the eigenvalues.
   */
   diag[0][0]  *=  scale;
   diag[1][1]  *=  scale;


} /* End diags2_c */
