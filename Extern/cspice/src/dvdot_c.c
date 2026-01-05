/*

-Procedure dvdot_c ( Derivative of Vector Dot Product, 3-D )

-Abstract

   Compute the derivative of the dot product of two double
   precision position vectors.

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

   DERIVATIVE
   VECTOR

*/

   #include "SpiceUsr.h"
   #undef   dvdot_c

   SpiceDouble dvdot_c ( ConstSpiceDouble s1[6],
                         ConstSpiceDouble s2[6] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   s1         I   First state vector in the dot product.
   s2         I   Second state vector in the dot product.

   The function returns the derivative of the dot product <s1,s2>

-Detailed_Input

   s1          is any state vector. The components are in order
               (x, y, z, dx/dt, dy/dt, dz/dt )

   s2          is any state vector.

-Detailed_Output

   The function returns the derivative of the dot product of the
   position portions of the two state vectors `s1' and `s2'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   Given two state vectors `s1' and `s2' made up of position and
   velocity components (p1,v1) and (p2,v2) respectively,
   dvdot_c calculates the derivative of the dot product of `p1' and `p2',
   i.e. the time derivative

         d
         -- < p1, p2 > = < v1, p2 > + < p1, v2 >
         dt

   where <,> denotes the dot product operation.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose that given two state vectors whose position components
      are unit vectors, and that we need to compute the rate of
      change of the angle between the two vectors.

      Example code begins here.


      /.
         Program dvdot_ex1
      ./
      #include <math.h>
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          dtheta;

         /.
         Define the two state vectors whose position
         components are unit vectors.
         ./
         SpiceDouble          s1     [6] = {
                                      7.2459e-01,  6.6274e-01,  1.8910e-01,
                                     -1.5990e-06,  1.6551e-06,  7.4873e-07 };

         SpiceDouble          s2     [6] = {
                                      8.4841e-01, -4.7790e-01, -2.2764e-01,
                                      1.0951e-07,  1.0695e-07,  4.8468e-08 };

         /.
         We know that the Cosine of the angle `theta' between them
         is given by

            cos(theta) = vdot_c(s1,s2)

         Thus by the chain rule, the derivative of the angle is
         given by:

            sin(theta) dtheta/dt = dvdot_c(s1,s2)

         Thus for values of `theta' away from zero we can compute
         dtheta/dt as:
         ./
         dtheta = dvdot_c(s1,s2) / sqrt( 1 - pow( vdot_c(s1,s2 ), 2 ) );

         printf( "Rate of change of angle between S1 and S2: %17.12f\n",
                                                                  dtheta );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Rate of change of angle between S1 and S2:   -0.000002232415


      Note that if the position components of `s1' and `s2' are parallel,
      the derivative of the  angle between the positions does not
      exist. Any code that computes the derivative of the angle
      between two position vectors should account for the case
      when the position components are parallel.

-Restrictions

   1)  The user is responsible for determining that the states `s1' and
       `s2' are not so large as to cause numeric overflow. In most
       cases this won't present a problem.

   2)  An implicit assumption exists that `s1' and `s2' are specified in
       the same reference frame. If this is not the case, the
       numerical result has no meaning.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 26-MAY-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code examples. Added entry #2 to -Restrictions section.

       Re-ordered header sections.

   -CSPICE Version 1.0.0, 07-JUL-1999 (EDW) (WLT)

-Index_Entries

   Compute the derivative of a dot product

-&
*/

{ /* Begin dvdot_c */

   return (  s1[0]*s2[3] + s1[1]*s2[4] + s1[2]*s2[5]
           + s1[3]*s2[0] + s1[4]*s2[1] + s1[5]*s2[2] );

} /* End dvdot_c */
