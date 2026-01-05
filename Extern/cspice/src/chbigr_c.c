/*

-Procedure chbigr_c ( Chebyshev expansion integral )

-Abstract

   Evaluate an indefinite integral of a Chebyshev expansion at a
   specified point `x' and return the value of the input expansion at
   `x' as well. The constant of integration is selected to make the
   integral zero when `x' equals the abscissa value x2s[0].

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

   CHEBYSHEV
   EPHEMERIS
   INTEGRAL
   MATH

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void chbigr_c ( SpiceInt            degp,
                   ConstSpiceDouble    cp     [],
                   ConstSpiceDouble    x2s    [2],
                   SpiceDouble         x,
                   SpiceDouble       * p,
                   SpiceDouble       * itgrlp     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   degp       I   Degree of input Chebyshev expansion.
   cp         I   Chebyshev coefficients of input expansion.
   x2s        I   Transformation parameters.
   x          I   Abscissa value of evaluation.
   p          O   Input expansion evaluated at `x'.
   itgrlp     O   Integral evaluated at `x'.

-Detailed_Input

   degp        is the degree of the input Chebyshev expansion.

   cp          is an array containing the coefficients of the input
               Chebyshev expansion. The coefficient of the i'th
               Chebyshev polynomial is contained in element cp[i],
               for i = 0 : degp.

   x2s         is an array containing the "transformation parameters"
               of the domain of the expansion. Element x2s[0]
               contains the midpoint of the interval on which the
               input expansion is defined; x2s[1] is one-half of the
               length of this interval; this value is called the
               interval's "radius."

               The input expansion defines a function f(x) on the
               interval

                  [ x2s[0]-x2s[1],  x2s[0]+x2s[1] ]

               as follows:

                  Define s = ( x - x2s[0] ) / x2s[1]


                                    degp
                                    __
                                    \
                     f(x) = g(s)  = /  cp[k]  T (s)
                                    --         k
                                    k=0


   x           is the abscissa value at which the function defined by
               the input expansion and its integral are to be
               evaluated. Normally `x' should lie in the closed
               interval

                  [ x2s[0]-x2s[1],  x2s[0]+x2s[1] ]

               See the -Restrictions section below.

-Detailed_Output

   p,
   itgrlp      Define `s' and f(x) as above in the description of the
               input argument `x2s'. Then `p' is f(x), and `itgrlp' is
               an indefinite integral of f(x), evaluated at `x'.

               The indefinite integral satisfies

                  d(itgrlp)/dx     = f(x)

               and

                  itgrlp( x2s[0] ) = 0

-Parameters

   None.

-Exceptions

   1)  If the input degree is negative, the error SPICE(INVALDDEGREE) is
       signaled by a routine in the call tree of this routine.

   2)  If the input interval radius is non-positive, the error
       SPICE(INVALIDRADIUS) is signaled by a routine in the call tree of
       this routine.

-Files

   None.

-Particulars

   Let

      T ,  n = 0, ...
       n

   represent the nth Chebyshev polynomial of the first kind:

      T (x) = cos( n*arccos(x) )
       n

   The input coefficients represent the Chebyshev expansion

                     degp
                     __
                     \
      f(x) = g(s)  = /  cp[k]  T (s)
                     --         k
                     k=0

   where

      s = ( x - x2s[0] ) / x2s[1]

   This routine evaluates and returns the value at `x' of an
   indefinite integral F(x), where

      dF(x)/dx    = f(x)  for all `x' in
                          [x2s[0]-x2s[0], x2s[0]+x2s[1]]

      F( x2s[0] ) = 0

   The value at `x' of the input expansion

      f(x)

   is returned as well.

   Note that numerical problems may result from applying this
   routine to abscissa values outside of the interval defined
   by the input parameters x2s[*]. See the -Restrictions section.

   To evaluate Chebyshev expansions and their derivatives, use the
   CSPICE routines chbint_c or chbder_c.

   This routine supports the SPICELIB SPK type 20 and PCK type 20
   evaluators SPKE20 and PCKE20.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Let the domain of a polynomial to be evaluated be the
      closed interval

         [20, 30]

      Let the input expansion represent the polynomial

                           6
         f(x)  = g(s) = 5*s

      where

         s     = (x - 20)/10

      Let F(x) be an indefinite integral of f(x) such that

         F(20) = 0

      Evaluate

         f(30) and F(30)


      Example code begins here.


      /.
         Program chbigr_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          cp     [6];
         SpiceDouble          x;
         SpiceDouble          x2s    [2];
         SpiceDouble          p;
         SpiceDouble          itgrlp;

         SpiceInt             degp;

         /.
         Let our domain be the interval [10, 30].
         ./
         x2s[0] = 20.0;
         x2s[1] = 10.0;

         /.
         Assign the expansion coefficients.
         ./
         degp  = 5;

         cp[0] = 0.0;
         cp[1] = 3.75;
         cp[2] = 0.0;
         cp[3] = 1.875;
         cp[4] = 0.0;
         cp[5] = 0.375;

         /.
         Evaluate the function and its integral at x = 30.
         ./
         x = 30.0;

         chbigr_c ( degp, cp, x2s, x, &p, &itgrlp );

         /.
         We make the change of variables

            s(x) = (1/10) * ( x - 20 )

         The expansion represents the polynomial

                             5
            f(x) = g(s) = 6*s

         An indefinite integral of the expansion is

                                        6
            F(x) = G(s) * dx/ds = 10 * s

         where `G' is defined on the interval [-1, 1]. The result
         should be (due to the change of variables)

              (G(1)  - G(0) ) * dx/ds

            = (F(30) - F(20)) * 10

            = 10

         The value of the expansion at `x' should be

            f(30) = g(1) = 6
         ./
         printf( "ITGRLP = %f\n", itgrlp );
         printf( "P      = %f\n", p );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      ITGRLP = 10.000000
      P      = 6.000000


-Restrictions

   1)  The value (x-x2s[0]) / x2s[1] normally should lie within the
       interval -1:1 inclusive, that is, the closed interval
       [-1, 1]. Chebyshev polynomials increase rapidly in magnitude
       as a function of distance of abscissa values from this
       interval.

       In typical SPICE applications, where the input expansion
       represents position, velocity, or orientation, abscissa
       values that map to points outside of [-1, 1] due to round-off
       error will not cause numeric exceptions.

   2)  No checks for floating point overflow are performed.

   3)  Significant accumulated round-off error can occur for input
       expansions of excessively high degree. This routine imposes
       no limits on the degree of the input expansion; users must
       verify that the requested computation provides appropriate
       accuracy.

-Literature_References

   [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
        "Numerical Recipes -- The Art of Scientific Computing,"
        chapter 5.4, "Recurrence Relations and Clenshaw's Recurrence
        Formula," p 161, Cambridge University Press, 1986.

   [2]  "Chebyshev polynomials," Wikipedia, The Free Encyclopedia.
        Retrieved 01:23, November 23, 2013, from
        http://en.wikipedia.org/w/index.php?title=
        Chebyshev_polynomials&oldid=574881046

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 19-JUL-2021 (JDR)

-Index_Entries

   integral of chebyshev_polynomial_expansion
   integrate chebyshev_polynomial_expansion

-&
*/

{ /* Begin chbigr_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "chbigr_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   chbigr_ (  ( integer    * ) &degp,
              ( doublereal * )  cp,
              ( doublereal * )  x2s,
              ( doublereal * ) &x,
              ( doublereal * )  p,
              ( doublereal * )  itgrlp  );

   chkout_c ( "chbigr_c" );

} /* End chbigr_c */
