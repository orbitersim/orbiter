/*

-Procedure qderiv_c ( Quadratic derivative )

-Abstract

   Estimate the derivative of a function by finding the derivative
   of a quadratic approximating function. This derivative estimate
   is equivalent to that found by computing the average of forward
   and backward differences.

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

   MATH
   UTILITY

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void qderiv_c ( SpiceInt            ndim,
                   ConstSpiceDouble    f0     [],
                   ConstSpiceDouble    f2     [],
                   SpiceDouble         delta,
                   SpiceDouble         dfdt   [] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   ndim       I   Dimension of function to be differentiated.
   f0         I   Function values at left endpoint.
   f2         I   Function values at right endpoint.
   delta      I   Separation of abscissa points.
   dfdt       O   Derivative vector.

-Detailed_Input

   ndim        is the dimension of the function to be
               differentiated. The derivative of each
               function component will be found.

   f0          is an array of `ndim' function values at a point on
               the real line; we'll refer to this point as `x0'.

   f2          is an array of `ndim' function values at a second
               point on the real line; we'll refer to this point
               as `x2'. The points `x0' and `x2' must satisfy

                  x2 = x0 + 2 * delta


   delta       is one half of the difference between `x2' and `x0':

                  delta = ( x2 - x0 ) / 2

               `delta' may be negative but must be non-zero.

-Detailed_Output

   dfdt        is an N-dimensional vector representing an estimate
               of the derivative of the input function at the
               midpoint `x1' of the interval between `x0' and `x2'.

               The ith component of `dfdt' is

                  ( 1 / (2*delta) ) * ( f2(i) - f0(i) )

               We may regard this estimate as the derivative
               at `x1' of a parabola fitted to the points

                   ( x0, f0(i) ),  ( x2, f2(i) )

               We may also regard this derivative as the average
               of the forward and backward first-order
               differences of the input function defined by
               f0(i), f2(i), and `delta'.

-Parameters

   None.

-Exceptions

   1)  If `delta' is zero, the error SPICE(DIVIDEBYZERO) is signaled by
       a routine in the call tree of this routine.

   2)  If `ndim' is less than 1, this routine will fail in a
       system-dependent manner.

-Files

   None.

-Particulars

   This routine estimates the derivative of a vector-valued function
   using the average of forward and backward differences.

   The derivative estimate computed by this routine is equivalent to
   that obtained by fitting each component of the function with a
   parabola at the points

      (x0, f(x0)), (x1, f(x1)), (x2, f(x2))

   where

       x0  =  x1 - delta
       x2  =  x1 + delta

   and finding the derivative of the parabolas at `x1'.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Estimate the derivative of x**2 at x = 2.

      Example code begins here.


      /.
         Program qderiv_ex1
      ./
      #include <math.h>
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         SpiceDouble          delta;
         SpiceDouble          dfdt   [1];
         SpiceDouble          f0     [1];
         SpiceDouble          f2     [1];
         SpiceInt             n;

         n     = 1;
         delta = 1.e-3;
         f0[0] = pow( ( 2.0 - delta ), 2.0 );
         f2[0] = pow( ( 2.0 + delta ), 2.0 );

         qderiv_c ( n, f0, f2, delta, dfdt );

         printf( " 4 - DFDT(1) =  %24.16e\n", 4 - dfdt[0] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       4 - DFDT(1) =    4.5474735088646412e-13


      Note that the difference displayed is platform-dependent, but
      should be on the order of 1.E-12.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 04-AUG-2021 (JDR)

-Index_Entries

   Estimate function derivative using quadratic fit

-&
*/

{ /* Begin qderiv_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "qderiv_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   qderiv_ (  ( integer    * ) &ndim,
              ( doublereal * )  f0,
              ( doublereal * )  f2,
              ( doublereal * ) &delta,
              ( doublereal * )  dfdt   );

   chkout_c ( "qderiv_c" );

} /* End qderiv_c */
