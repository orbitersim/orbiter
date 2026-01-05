/*

-Procedure rquad_c ( Roots of a quadratic equation )

-Abstract

   Find the roots of a quadratic equation.

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
   POLYNOMIAL
   ROOT

*/
   #include <math.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void rquad_c ( SpiceDouble  a,
                  SpiceDouble  b,
                  SpiceDouble  c,
                  SpiceDouble  root1[2],
                  SpiceDouble  root2[2] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   a          I   Coefficient of quadratic term.
   b          I   Coefficient of linear term.
   c          I   Constant.
   root1      O   Root built from positive discriminant term.
   root2      O   Root built from negative discriminant term.

-Detailed_Input

   a,
   b,
   c           are the coefficients of a quadratic polynomial

                    2
                  ax   +   bx   +   c.

-Detailed_Output

   root1,
   root2       are the roots of the equation,

                     2
                   ax   +   bx   +   c   =  0.


               root1 and root2 are both arrays of length 2. The
               first element of each array is the real part of a
               root; the second element contains the complex part
               of the same root.

               When a is non-zero, root1 represents the root

                                _____________
                               /  2
                  - b   +    \/  b    -   4ac
                  ---------------------------
                                2a


               and root2 represents the root

                                _____________
                               /  2
                  - b   -    \/  b    -   4ac
                  --------------------------- .
                                2a


               When a is zero and b is non-zero, root1 and root2
               both represent the root

                  - c / b.

-Parameters

   None.

-Exceptions

   1)  If the input coefficients `a' and `b' are both zero, the error
       SPICE(DEGENERATECASE) is signaled. The output arguments are not
       modified.

-Files

   None.

-Particulars

   None.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Humor us and suppose we want to compute the "golden ratio."

      The quantity `r' is defined by the equation

         1/r = r/(1-r),

      which is equivalent to

          2
         r   +  r  -  1  =  0.

      The following code example does the job.


      Example code begins here.


      /.
         Program rquad_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local variables.
         ./
         SpiceDouble             root1 [ 2 ];
         SpiceDouble             root2 [ 2 ];

         /.
         Compute "golden ratio."  The root we want,

                    ___
                   /
            -1 + \/  5
            -----------,
                 2


         is contained in root1.
         ./
         rquad_c ( 1., 1., -1., root1, root2 );

         /.
         Print the results.
         ./
         printf ( "The \"golden ratio\" is %f\n", root1[0] );

         return ( 0 );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      The "golden ratio" is 0.618034


   2) Calculate the roots of the following quadratic equation:

          2
         x  + 2x + 3 = 0

      Example code begins here.


      /.
         Program rquad_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local variables.
         ./
         SpiceDouble             root1 [ 2 ];
         SpiceDouble             root2 [ 2 ];

         /.
         Let's do one with imaginary roots just for fun.
         ./

         rquad_c ( 1.,  2.,  3.,  root1,  root2 );

         printf ( "Root #1: %12.7f %12.7f\n", root1[0], root1[1] );
         printf ( "Root #2: %12.7f %12.7f\n", root2[0], root2[1] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Root #1:   -1.0000000    1.4142136
      Root #2:   -1.0000000   -1.4142136


-Restrictions

   1)  No checks for overflow of the roots are performed.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 04-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Created complete code examples from existing code fragments.

   -CSPICE Version 1.0.0, 13-JUN-1999 (NJB)

-Index_Entries

   roots of a quadratic equation

-&
*/

{ /* Begin rquad_c */


   /*
   Local variables
   */

   SpiceBoolean            zeroed;

   SpiceDouble             con;
   SpiceDouble             discrm;
   SpiceDouble             lin;
   SpiceDouble             scale;
   SpiceDouble             sqr;


   /*
   Use discovery check-in.
   */


   /*
   The degree of the equation is zero unless at least one of the
   second or first degree coefficients is non-zero.
   */

   if (  ( a == 0.0 )  &&  ( b == 0.0 )  )
   {
      chkin_c  ( "rquad_c"                                        );
      setmsg_c ( "Both 1st and 2nd degree coefficients are zero." );
      sigerr_c ( "SPICE(DEGENERATECASE)"                          );
      chkout_c ( "rquad"                                          );
      return;
   }


   /*
   If we can scale the coefficients without zeroing any of them out,
   we will do so, to help prevent overflow.
   */

   scale  =  MaxAbs ( a, b     );
   scale  =  MaxAbs ( c, scale );

   zeroed =      (  ( a != 0. ) && ( a / scale == 0. )  )
             ||  (  ( b != 0. ) && ( b / scale == 0. )  )
             ||  (  ( c != 0. ) && ( c / scale == 0. )  );


   if ( !zeroed )
   {
      sqr = a / scale;
      lin = b / scale;
      con = c / scale;
   }
   else
   {
      sqr = a;
      lin = b;
      con = c;
   }


   /*
   If the second-degree coefficient is non-zero, we have a bona fide
   quadratic equation, as opposed to a linear equation.
   */

   if ( sqr != 0. )
   {
      /*
      Compute the discriminant.
      */
      discrm  =  lin*lin  -  4.0 * sqr * con;


      /*
      A non-negative discriminant indicates that the roots are
      real.
      */

      if ( discrm >= 0.0 )
      {
         /*
         The imaginary parts of both roots are zero.
         */
         root1[1] = 0.;
         root2[1] = 0.;

         /*
         We can take advantage of the fact that con/sqr is the
         product of the roots to improve the accuracy of the root
         having the smaller magnitude.  We compute the larger root
         first and then divide con/sqr by it to obtain the smaller
         root.
         */

         if ( lin < 0. )
         {
            /*
            root1 will contain the root of larger magnitude.
            */

            root1[0] =  ( - lin + sqrt(discrm) )  /  ( 2. * sqr );

            root2[0] =  ( con / sqr )  /   root1[0];
         }

         else if ( lin > 0. )
         {
            /*
            ROOT2 will contain the root of larger magnitude.
            */
            root2[0] =  ( - lin - sqrt(discrm) )  /  ( 2. * sqr );

            root1[0] =  ( con / sqr )  /   root2[0];
         }

         else
         {
            /*
            The roots have the same magnitude.
            */
            root1[0]  =    sqrt( discrm )  /  ( 2. * sqr );
            root2[0]  =  - root1[0];
         }

      }

      else
      {
         /*
         The only other possibility is that the roots are complex.

         The roots are complex conjugates, so they have equal
         magnitudes.
         */
         root1[0]  =  -lin               /   ( 2. * sqr );
         root1[1]  =   sqrt( -discrm )   /   ( 2. * sqr );

         root2[0]  =   root1[0];
         root2[1]  =  -root1[1];
      }

   }

   else
   {
      /*
      If the second-degree coefficient is zero, we actually have a
      linear equation.
      */

      root1[0] =  - con / lin;
      root1[1] =    0.;

      /*
      We set the second root equal to the first, rather than
      leaving it undefined.
      */
      MOVED ( root1, 2, root2 );
   }


} /* End rquad_c */
