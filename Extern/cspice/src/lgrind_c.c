/*

-Procedure lgrind_c (Lagrange polynomial interpolation with derivative)

-Abstract

   Evaluate a Lagrange interpolating polynomial, for a specified
   set of coordinate pairs, at a specified abscissa value. Return
   both the value of the polynomial and its derivative.

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

   INTERPOLATION
   POLYNOMIAL

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef    lgrind_c

   void lgrind_c ( SpiceInt            n,
                   ConstSpiceDouble  * xvals,
                   ConstSpiceDouble  * yvals,
                   SpiceDouble       * work,
                   SpiceDouble         x,
                   SpiceDouble       * p,
                   SpiceDouble       * dp )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   n          I   Number of points defining the polynomial.
   xvals      I   Abscissa values.
   yvals      I   Ordinate values.
   work      I-O  Work space array.
   x          I   Point at which to interpolate the polynomial.
   p          O   Polynomial value at `x'.
   dp         O   Polynomial derivative at `x'.

-Detailed_Input

   n           is the number of points defining the polynomial.
               The arrays `xvals' and `yvals' contain `n' elements.

   xvals,
   yvals       are arrays of abscissa and ordinate values that
               together define `n' ordered pairs. The set of points

                  ( xvals[i], yvals[i] )

               define the Lagrange polynomial used for
               interpolation. The elements of `xvals' must be
               distinct and in increasing order.

   work        is an n * 2 work space array, where `n' is the same
               dimension as that of `xvals' and `yvals'. It is used
               by this routine as a scratch area to hold
               intermediate results.

   x           is the abscissa value at which the interpolating
               polynomial is to be evaluated.

-Detailed_Output

   p           is the value at `x' of the unique polynomial of
               degree n-1 that fits the points in the plane
               defined by `xvals' and `yvals'.

   dp          is the derivative at `x' of the interpolating
               polynomial described above.

-Parameters

   None.

-Exceptions

   1)  If any two elements of the array `xvals' are equal, the error
       SPICE(DIVIDEBYZERO) is signaled by a routine in the call tree of this
       routine.

   2)  If `n' is less than 1, the error SPICE(INVALIDSIZE) is
       signaled by a routine in the call tree of this routine.

   3)  This routine does not attempt to ward off or diagnose
       arithmetic overflows.

-Files

   None.

-Particulars

   Given a set of `n' distinct abscissa values and corresponding
   ordinate values, there is a unique polynomial of degree n-1, often
   called the "Lagrange polynomial", that fits the graph defined by
   these values. The Lagrange polynomial can be used to interpolate
   the value of a function at a specified point, given a discrete
   set of values of the function.

   Users of this routine must choose the number of points to use
   in their interpolation method. The authors of Reference [1] have
   this to say on the topic:

      Unless there is solid evidence that the interpolating function
      is close in form to the true function `f', it is a good idea to
      be cautious about high-order interpolation. We
      enthusiastically endorse interpolations with 3 or 4 points, we
      are perhaps tolerant of 5 or 6; but we rarely go higher than
      that unless there is quite rigorous monitoring of estimated
      errors.

   The same authors offer this warning on the use of the
   interpolating function for extrapolation:

      ...the dangers of extrapolation cannot be overemphasized:
      An interpolating function, which is perforce an extrapolating
      function, will typically go berserk when the argument `x' is
      outside the range of tabulated values by more than the typical
      spacing of tabulated points.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Fit a cubic polynomial through the points

          ( -1, -2 )
          (  0, -7 )
          (  1, -8 )
          (  3, 26 )

      and evaluate this polynomial at x = 2.

      The returned value of `p' should be 1.0, since the
      unique cubic polynomial that fits these points is

                     3      2
         f(x)   =   x  + 2*x  - 4*x - 7

      The returned value of `dp' should be 16.0, since the
      derivative of f(x) is

          '           2
         f (x)  =  3*x  + 4*x - 4


      Example code begins here.


      /.
         Program lgrind_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble      p;
         SpiceDouble      dp;
         SpiceDouble      xvals [] = { -1., 0., 1., 3. };
         SpiceDouble      yvals [] = { -2., -7., -8., 26. };
         SpiceDouble      work  [4*2];
         SpiceInt         n = 4;

         lgrind_c ( n, xvals, yvals, work, 2., &p, &dp );

         printf( "P, DP = %f %f\n", p, dp );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      P, DP = 1.000000 16.000000


      Note that we could also have lgrind_c with the reference

         lgrind_c ( n, xvals, yvals, yvals, 2., &p, &dp );

      if we wished to; in this case `yvals' would have been
      modified on output.

-Restrictions

   None.

-Literature_References

   [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
        "Numerical Recipes -- The Art of Scientific Computing,"
        chapters 3.0 and 3.1, Cambridge University Press, 1986.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 24-AUG-2015 (EDW) (NJB)

-Index_Entries

   interpolate function using Lagrange polynomial
   Lagrange interpolation

-&
*/

{ /* Begin lgrind_c */

   /*
   Local constants
   */


   /*
   Local macros
   */


   /*
   Local variables
   */


   /*
   Static variables
   */


   /*
   Participate in error tracing.
   */

   chkin_c ( "lgrind_c" );

   /*
   The f2c'd routine does the work.
   */
   lgrind_( (integer    *) &n,
            (doublereal *) xvals,
            (doublereal *) yvals,
            (doublereal *) work,
            (doublereal *) &x,
            (doublereal *) p,
            (doublereal *) dp);

   chkout_c ( "lgrind_c" );

} /* End lgrind_c */
