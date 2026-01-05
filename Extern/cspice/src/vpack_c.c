/*

-Procedure vpack_c ( Pack three scalar components into a vector )

-Abstract

   Pack three scalar components into a vector.

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

   VECTOR

*/

   #include "SpiceUsr.h"


   void vpack_c ( SpiceDouble   x,
                  SpiceDouble   y,
                  SpiceDouble   z,
                  SpiceDouble   v[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   x,
   y,
   z          I   Scalar components of a vector.
   v          O   Equivalent vector.

-Detailed_Input

   x,
   y,
   z           are the scalar components of a 3-dimensional vector.

-Detailed_Output

   v           is the equivalent vector, such that

                  v[0] = x
                  v[1] = y
                  v[2] = z

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   Basically, this is just shorthand notation for the common
   sequence

      v[0] = x;
      v[1] = y;
      v[2] = z;

   The routine is useful largely for two reasons. First, it
   reduces the chance that the programmer will make a "cut and
   paste" mistake, like

      v[0] = x;
      v[0] = y;
      v[0] = z;

   Second, it makes conversions between equivalent units simpler,
   and clearer. For instance, the sequence

      v[0] = x * rpd_c();
      v[1] = y * rpd_c();
      v[2] = z * rpd_c();

   can be replaced by the (nearly) equivalent sequence

      vpack_c ( x, y, z,    v );
      vscl_c  ( rpd_c(), v, v );

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input
   (if any), the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Compute an upward normal of an equilateral triangle lying
      in the x-y plane and centered at the origin.


      Example code begins here.


      /.
         Program vpack_ex1
      ./
      #include <math.h>
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          normal [3];
         SpiceDouble          s;
         SpiceDouble          v1     [3];
         SpiceDouble          v2     [3];
         SpiceDouble          v3     [3];

         s = sqrt(3.0)/2;

         /.
         Define the three corners of the triangle.
         ./
         vpack_c (   s, -0.5, 0.0, v1 );
         vpack_c ( 0.0,  1.0, 0.0, v2 );
         vpack_c (  -s, -0.5, 0.0, v3 );

         /.
         Compute an upward normal of the triangle.
         ./
         pltnrm_c ( v1, v2, v3, normal );

         printf( "NORMAL =  %16.13f %16.13f %16.13f\n",
                        normal[0], normal[1], normal[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      NORMAL =   0.0000000000000  0.0000000000000  2.5980762113533


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 16-JUL-2020 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

   -CSPICE Version 1.0.0, 24-MAY-1999 (IMU) (NJB)

-Index_Entries

   pack three scalar components into a vector

-&
*/

{ /* Begin vpack_c */


   v[0] = x;
   v[1] = y;
   v[2] = z;


} /* End vpack_c */
