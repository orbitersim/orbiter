/*

-Procedure hrmesp_c ( Hermite polynomial interpolation, equal spacing )

-Abstract

   Evaluate, at a specified point, a Hermite interpolating polynomial
   for a specified set of equally spaced abscissa values and
   corresponding pairs of function and function derivative values.

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
   #include <stddef.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "zzalloc.h"

   void hrmesp_c ( SpiceInt            n,
                   SpiceDouble         first,
                   SpiceDouble         step,
                   ConstSpiceDouble    yvals  [],
                   SpiceDouble         x,
                   SpiceDouble       * f,
                   SpiceDouble       * df        )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   n          I   Number of points defining the polynomial.
   first      I   First abscissa value.
   step       I   Step size.
   yvals      I   Ordinate and derivative values.
   x          I   Point at which to interpolate the polynomial.
   f          O   Interpolated function value at `x'.
   df         O   Interpolated function's derivative at `x'.

-Detailed_Input

   n           is the number of points defining the polynomial.
               The array `yvals' contains 2*n elements.

   first,
   step        are, respectively, a starting abscissa value and a
               step size that define the set of abscissa values

                  first   +   i * step,     i = 0, ..., n-1

               `step' must be non-zero.

   yvals       is an array of length 2*n containing ordinate and
               derivative values for each point in the domain
               defined by `first', `step', and `n'. The elements

                  yvals[ 2*i     ]
                  yvals[ 2*i + 1 ]

               give the value and first derivative of the output
               polynomial at the abscissa value

                  first   +   i * step

               where `i' ranges from 0 to n-1.

   x           is the abscissa value at which the interpolating
               polynomial and its derivative are to be evaluated.

-Detailed_Output

   f,
   df          are the value and derivative at `x' of the unique
               polynomial of degree 2*n-1 that fits the points and
               derivatives defined by `first', `step', and `yvals'.

-Parameters

   None.

-Exceptions

   1)  If `step' is zero, the error SPICE(INVALIDSTEPSIZE) is
       signaled by a routine in the call tree of this routine.

   2)  If `n' is less than 1, the error SPICE(INVALIDSIZE) is
       signaled.

   3)  This routine does not attempt to ward off or diagnose
       arithmetic overflows.

   4)  If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled.

-Files

   None.

-Particulars

   Users of this routine must choose the number of points to use
   in their interpolation method. The authors of Reference [1] have
   this to say on the topic:

      Unless there is solid evidence that the interpolating function
      is close in form to the true function f, it is a good idea to
      be cautious about high-order interpolation. We
      enthusiastically endorse interpolations with 3 or 4 points, we
      are perhaps tolerant of 5 or 6; but we rarely go higher than
      that unless there is quite rigorous monitoring of estimated
      errors.

   The same authors offer this warning on the use of the
   interpolating function for extrapolation:

      ...the dangers of extrapolation cannot be overemphasized:
      An interpolating function, which is perforce an extrapolating
      function, will typically go berserk when the argument x is
      outside the range of tabulated values by more than the typical
      spacing of tabulated points.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Fit a 7th degree polynomial through the points ( x, y, y' )

         ( -1,      6,       3 )
         (  1,      8,      11 )
         (  3,   2210,    5115 )
         (  5,  78180,  109395 )

      and evaluate this polynomial at x = 2.

      The returned value of ANSWER should be 141.0, and the
      returned derivative value should be 456.0, since the unique
      7th degree polynomial that fits these constraints is

                   7       2
         f(x)  =  x   +  2x  + 5


      Example code begins here.


      /.
         Program hrmesp_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         SpiceDouble          answer;
         SpiceDouble          deriv;
         SpiceDouble          first;
         SpiceDouble          step;
         SpiceDouble          yvals  [8];
         SpiceInt             n;

         n         =       4;

         yvals[0]  =       6.0;
         yvals[1]  =       3.0;
         yvals[2]  =       8.0;
         yvals[3]  =      11.0;
         yvals[4]  =    2210.0;
         yvals[5]  =    5115.0;
         yvals[6]  =   78180.0;
         yvals[7]  =  109395.0;

         first     =  -1.0;
         step      =   2.0;

         hrmesp_c ( n, first, step, yvals, 2.0, &answer, &deriv );

         printf( "ANSWER = %f\n", answer );
         printf( "DERIV  = %f\n", deriv );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      ANSWER = 141.000000
      DERIV  = 456.000000


-Restrictions

   None.

-Literature_References

   [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
        "Numerical Recipes -- The Art of Scientific Computing,"
        chapters 3.0 and 3.1, Cambridge University Press, 1986.

   [2]  S. Conte and C. de Boor, "Elementary Numerical Analysis -- An
        Algorithmic Approach," 3rd Edition, p 64, McGraw-Hill, 1980.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 03-AUG-2021 (JDR)

-Index_Entries

   interpolate function using Hermite polynomial
   Hermite interpolation

-&
*/

{ /* Begin hrmesp_c */

   /*
   Local variables.
   */
   SpiceInt           nBytesWork;

   doublereal       * work;

   /*
   Participate in error tracing.
   */
   chkin_c ( "hrmesp_c" );

   /*
   No data, no interpolation.
   */
   if ( n < 1 )
   {
      setmsg_c ( "Array size must be positive; was #." );
      errint_c ( "#", n );
      sigerr_c ( "SPICE(INVALIDSIZE)" );
      chkout_c ( "hrmesp_c" );
      return;
   }

   /*
   Allocate the workspace.
   */
   nBytesWork = 4 * n * sizeof(SpiceDouble);

   work       = (doublereal *) alloc_SpiceMemory( (size_t)nBytesWork );

   if ( work == NULL )
   {
      setmsg_c ( "Workspace allocation of # bytes failed due to "
                 "malloc failure."                              );
      errint_c ( "#", nBytesWork                                );
      sigerr_c ( "SPICE(MALLOCFAILED)"                          );
      chkout_c ( "hrmesp_c" );
      return;
   }

   /*
   Call the f2c'd Fortran routine.
   */
   hrmesp_ (  ( integer    * ) &n,
              ( doublereal * ) &first,
              ( doublereal * ) &step,
              ( doublereal * )  yvals,
              ( doublereal * ) &x,
              ( doublereal * )  work,
              ( doublereal * )  f,
              ( doublereal * )  df    );

   /*
   De-allocate the workspace.
   */
   free_SpiceMemory( work );

   ALLOC_CHECK;

   chkout_c ( "hrmesp_c" );

} /* End hrmesp_c */
