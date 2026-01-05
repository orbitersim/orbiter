/*

-Procedure polyds_c ( Compute a Polynomial and its Derivatives )

-Abstract

   Compute the value of a polynomial and its first
   `nderiv' derivatives at the value `t'.

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
   MATH
   POLYNOMIAL

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef    polyds_c

   void polyds_c ( ConstSpiceDouble    * coeffs,
                   SpiceInt              deg,
                   SpiceInt              nderiv,
                   SpiceDouble           t,
                   SpiceDouble         * p )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   coeffs     I   Coefficients of the polynomial to be evaluated.
   deg        I   Degree of the polynomial to be evaluated.
   nderiv     I   Number of derivatives to compute.
   t          I   Point to evaluate the polynomial and derivatives
   p          O   Value of polynomial and derivatives.

-Detailed_Input

   coeffs      are the coefficients of the polynomial that is
               to be evaluated. The first element of this array
               should be the constant term, the second element the
               linear coefficient, the third term the quadratic
               coefficient, and so on. The number of coefficients
               supplied should be one more than `deg'.

                  f(x) =   coeffs[0] + coeffs[1]*x + coeffs[2]*x^2

                         + coeffs[3]*x^4 + ... + coeffs[deg]*x^deg

   deg         is the degree of the polynomial to be evaluated. `deg'
               should be one less than the number of coefficients
               supplied.

   nderiv      is the number of derivatives to compute. If `nderiv'
               is zero, only the polynomial will be evaluated. If
               nderiv = 1, then the polynomial and its first
               derivative will be evaluated, and so on. If the value
               of `nderiv' is negative, the routine returns
               immediately.

   t           is the point at which the polynomial and its
               derivatives should be evaluated.

-Detailed_Output

   p           is an array containing the value of the polynomial and
               its derivatives evaluated at `t'. The first element of
               the array contains the value of `p' at `t'. The second
               element of the array contains the value of the first
               derivative of `p' at `t' and so on. The nderiv + 1'st
               element of the array contains the nderiv'th derivative
               of `p' evaluated at `t'.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If `nderiv' is less than zero, the routine simply returns.

   2)  If the degree of the polynomial is less than 0, the routine
       returns the first nderiv+1 elements of `p' set to 0.

-Files

   None.

-Particulars

   This routine uses the user supplied coefficients (coeffs)
   to evaluate a polynomial (having these coefficients) and its
   derivatives at the point `t'. The zero'th derivative of the
   polynomial is regarded as the polynomial itself.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) For the polynomial

         f(x) = 1 + 3*x + 0.5*x^2 + x^3 + 0.5*x^4 - x^5 + x^6

      the coefficient set

         Degree  coeffs
         ------  ------
         0       1
         1       3
         2       0.5
         3       1
         4       0.5
         5      -1
         6       1

      Compute the value of the polynomial and it's first
      3 derivatives at the value t = 1.0. We expect:

         Derivative Number     t = 1
         ------------------    -----
         f(x)         0        6
         f'(x)        1        10
         f''(x)       2        23
         f'''(x)      3        78


      Example code begins here.


      /.
         Program polyds_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define NDERIV       3

         /.
         Local variables.
         ./
         SpiceDouble      p      [ NDERIV + 1 ];
         SpiceInt         i;

         SpiceDouble      coeffs [] = { 1., 3., 0.5, 1., 0.5, -1., 1. };
         SpiceDouble      t         = 1.;

         SpiceInt         deg       = 6;

         polyds_c ( coeffs, deg, NDERIV, t, p );

         for ( i = 0; i <= NDERIV; i++ )
         {
            printf( "P = %f\n", p[i] );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      P = 6.000000
      P = 10.000000
      P = 23.000000
      P = 78.000000


-Restrictions

   1)  Depending on the coefficients the user should be careful when
       taking high order derivatives. As the example shows, these
       can get big in a hurry. In general the coefficients of the
       derivatives of a polynomial grow at a rate greater
       than N! (N factorial).

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 04-AUG-2021 (JDR)

       Removed unnecessary Standard SPICE error handling calls to
       register/unregister this routine in the error handling
       subsystem; this routine is Error free.

       Updated the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 24-AUG-2015 (EDW) (WLT)

-Index_Entries

   compute a polynomial and its derivatives

-&
*/

{ /* Begin polyds_c */

   /*
   Error free: no error tracing required.
   */

   /*
   Call the f2c'd Fortran routine.
   */
   polyds_ (  ( doublereal * )  coeffs,
              ( integer    * ) &deg,
              ( integer    * ) &nderiv,
              ( doublereal * ) &t,
              ( doublereal * )  p      );

} /* End polyds_c */
