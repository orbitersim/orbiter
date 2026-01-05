/*

-Procedure pltnp_c ( Nearest point on triangular plate )

-Abstract

   Find the nearest point on a triangular plate to a given point.

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

   DSK

-Keywords

   GEOMETRY
   MATH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #undef pltnp_c


   void pltnp_c ( ConstSpiceDouble    point[3],
                  ConstSpiceDouble    v1   [3],
                  ConstSpiceDouble    v2   [3],
                  ConstSpiceDouble    v3   [3],
                  SpiceDouble         pnear[3],
                  SpiceDouble       * dist      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   point      I   A point in 3-dimensional space.
   v1,
   v2,
   v3         I   Vertices of a triangular plate.
   pnear      O   Nearest point on the plate to `point'.
   dist       O   Distance between `pnear' and `point'.

-Detailed_Input

   point       is an arbitrary point in 3-dimensional space.

   v1,
   v2,
   v3          are 3-vectors constituting the vertices of
               a triangular plate.

               The plate is allowed to be degenerate: it may
               consist of a line segment or of a single point.

-Detailed_Output

   pnear       is the closest point on the plate to `point'.
               `pnear' is unique, since the plate is convex.

   dist        is the distance between `point' and `pnear'.

-Parameters

   None.

-Exceptions

   1)  The input plate is allowed to be degenerate: it may be
       a line segment or a single point.

-Files

   None.

-Particulars

   None.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input
   (if any), the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Find the nearest point to the point (2,2,2) on a plate having
      vertices at the unit basis vectors that lie along the positive
      X, Y, and Z coordinate axes.


      Example code begins here.


      /.
         Program pltnp_ex1
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local variables
         ./
         SpiceDouble             dist;
         SpiceDouble             pnear[3];
         SpiceDouble             point[3] = {2.0, 2.0, 2.0};
         SpiceDouble             v1   [3] = {1.0, 0.0, 0.0};
         SpiceDouble             v2   [3] = {0.0, 1.0, 0.0};
         SpiceDouble             v3   [3] = {0.0, 0.0, 1.0};

         pltnp_c ( point, v1, v2, v3, pnear, &dist );

         printf ( "\n"
                  "Plate vertex 1 = %14.7e %14.7e %14.7e\n"
                  "Plate vertex 2 = %14.7e %14.7e %14.7e\n"
                  "Plate vertex 3 = %14.7e %14.7e %14.7e\n"
                  "Input point    = %14.7e %14.7e %14.7e\n"
                  "\n"
                  "Near point     = %14.7e %14.7e %14.7e\n"
                  "Distance       = %14.7e\n"
                  "\n",
                  v1[0],    v1[1],    v1[2],
                  v2[0],    v2[1],    v2[2],
                  v3[0],    v3[1],    v3[2],
                  point[0], point[1], point[2],
                  pnear[0], pnear[1], pnear[2],
                  dist                                    );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Plate vertex 1 =  1.0000000e+00  0.0000000e+00  0.0000000e+00
      Plate vertex 2 =  0.0000000e+00  1.0000000e+00  0.0000000e+00
      Plate vertex 3 =  0.0000000e+00  0.0000000e+00  1.0000000e+00
      Input point    =  2.0000000e+00  2.0000000e+00  2.0000000e+00

      Near point     =  3.3333333e-01  3.3333333e-01  3.3333333e-01
      Distance       =  2.8867513e+00


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 04-MAR-2019 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 01-FEB-2016 (NJB)

-Index_Entries

   nearest point on triangular plate

-&
*/

{ /* Begin pltnp_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "pltnp_c" );


   pltnp_ ( (doublereal  *) point,
            (doublereal  *) v1,
            (doublereal  *) v2,
            (doublereal  *) v3,
            (doublereal  *) pnear,
            (doublereal  *) dist   );


   chkout_c ( "pltnp_c" );

} /* End pltnp_c */
