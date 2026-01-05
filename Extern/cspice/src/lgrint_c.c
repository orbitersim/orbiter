/*

-Procedure lgrint_c ( Lagrange polynomial interpolation )

-Abstract

   Evaluate a Lagrange interpolating polynomial for a specified
   set of coordinate pairs, at a specified abscissa value.

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

   SpiceDouble lgrint_c ( SpiceInt            n,
                          ConstSpiceDouble    xvals  [],
                          ConstSpiceDouble    yvals  [],
                          SpiceDouble         x         )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   n          I   Number of points defining the polynomial.
   xvals      I   Abscissa values.
   yvals      I   Ordinate values.
   x          I   Point at which to interpolate the polynomial.

   The function returns the value at `x' of the unique polynomial of
   degree n-1 that fits the points in the plane defined by `xvals' and
   `yvals'.

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

   x           is the abscissa value at which the interpolating
               polynomial is to be evaluated.

-Detailed_Output

   The function returns the value at `x' of the unique polynomial of
   degree n-1 that fits the points in the plane defined by `xvals' and
   `yvals'.

-Parameters

   None.

-Exceptions

   1)  If any two elements of the array `xvals' are equal, the error
       SPICE(DIVIDEBYZERO) is signaled by a routine in the call tree
       of this routine. The function will return the value 0.0.

   2)  If `n' is less than 1, the error SPICE(INVALIDSIZE) is signaled. The
       function will return the value 0.0.

   3)  This routine does not attempt to ward off or diagnose
       arithmetic overflows.

   4)  If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled. The function
       returns the value result.

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
      is close in form to the true function F, it is a good idea to
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

      The returned value of lgrint_c should be 1.0, since the
      unique cubic polynomial that fits these points is

                   3      2
         f(x)  =  x  + 2*x  - 4*x - 7


      Example code begins here.


      /.
         Program lgrint_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          answer;
         SpiceDouble          xvals  [4];
         SpiceDouble          yvals  [4];
         SpiceInt             n;

         n         =   4;

         xvals[0]  =  -1.0;
         xvals[1]  =   0.0;
         xvals[2]  =   1.0;
         xvals[3]  =   3.0;

         yvals[0]  =  -2.0;
         yvals[1]  =  -7.0;
         yvals[2]  =  -8.0;
         yvals[3]  =  26.0;

         answer    =   lgrint_c ( n, xvals, yvals, 2.0 );

         printf( "ANSWER = %f\n", answer );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      ANSWER = 1.000000


-Restrictions

   None.

-Literature_References

   [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
        "Numerical Recipes -- The Art of Scientific Computing,"
        chapters 3.0 and 3.1, Cambridge University Press, 1986.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 04-AUG-2021 (JDR)

-Index_Entries

   interpolate function using Lagrange polynomial
   Lagrange interpolation

-&
*/

{ /* Begin lgrint_c */

   /*
   Local variables.
   */
   SpiceDouble        result;
   SpiceInt           nBytesWork;

   doublereal       * work;

   /*
   Participate in error tracing.
   */
   chkin_c ( "lgrint_c" );

   /*
   Set the return value in case of failure.
   */
   result = 0.0;

   /*
   No data, no interpolation.
   */
   if ( n < 1 )
   {
      setmsg_c ( "Array size must be positive; was #." );
      errint_c ( "#", n );
      sigerr_c ( "SPICE(INVALIDSIZE)" );
      chkout_c ( "lgrint_c" );
      return ( result );
   }

   /*
   Allocate the workspace.
   */
   nBytesWork = n * sizeof(SpiceDouble);

   work       = (doublereal *) alloc_SpiceMemory( (size_t)nBytesWork );

   if ( work == NULL )
   {
      setmsg_c ( "Workspace allocation of # bytes failed due to "
                 "malloc failure."                              );
      errint_c ( "#", nBytesWork                                );
      sigerr_c ( "SPICE(MALLOCFAILED)"                          );
      chkout_c ( "lgrint_c" );
      return ( result );
   }

   /*
   Call the f2c'd Fortran routine.
   */
   result = (SpiceDouble) lgrint_ (  ( integer    * ) &n,
                                     ( doublereal * )  xvals,
                                     ( doublereal * )  yvals,
                                     ( doublereal * )  work,
                                     ( doublereal * ) &x     );

   /*
   De-allocate the workspace.
   */
   free_SpiceMemory( work );

   ALLOC_CHECK;

   chkout_c ( "lgrint_c" );

   /*
   Tell the caller the result.
   */
   return ( result );

} /* End lgrint_c */
