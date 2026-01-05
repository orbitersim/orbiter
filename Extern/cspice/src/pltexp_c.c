/*

-Procedure pltexp_c ( Plate expander )

-Abstract

   Expand a triangular plate by a specified amount. The expanded
   plate is co-planar with, and has the same orientation as, the
   original. The centroids of the two plates coincide.

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
   TOPOGRAPHY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef pltexp_c

   void pltexp_c ( ConstSpiceDouble   iverts[3][3],
                   SpiceDouble        delta,
                   SpiceDouble        overts[3][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   iverts     I   Vertices of the plate to be expanded.
   delta      I   Fraction by which the plate is to be expanded.
   overts     O   Vertices of the expanded plate.

-Detailed_Input

   iverts      is an array containing three vertices of a triangular
               plate. Each vertex is a three-dimensional vector. The
               elements

                 iverts[i][j], j = 0 ... 2

               are, respectively, the X, Y, and Z components of the
               ith vertex.


   delta       is a fraction by which the plate is to be scaled.
               Scaling is done so that the scaled plate has the
               following properties:

                  -  it is co-planar with the input plate

                  -  its centroid coincides with that of the input
                     plate

                  -  its sides remain parallel to the corresponding
                     sides of the input plate

                  -  the distance of each vertex from the centroid is
                     (1+delta) times the corresponding distance for
                     the input plate

-Detailed_Output

   overts      is an array containing three vertices of the triangular
               plate resulting from scaling the input plate.

               If `ctroid' is the centroid (the average of the vertices)
               of the input plate, then the ith vertex of `overts'

                  overts[i][j], j = 0 ... 2

               is equal to

                  ctroid[j] + (1+delta)*( iverts[i][j] - ctroid[j] ),

                  j = 0 ... 2

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This routine supports "greedy" ray-plate intercept algorithms.
   Such algorithms attempt to ensure that false negatives---in which
   an intersection is not found due to round-off error---do not
   occur. In such an algorithm, the plate of interest is expanded
   slightly before the intersection test is performed.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input
   (if any), the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Expand an equilateral triangle that lies in the plane

         { (x,y,z) : z = 7 }

      Use an expansion fraction of 1.0; this doubles the size of
      the plate.

      Example code begins here.


      /.
         Program pltexp_ex1
      ./

      #include <stdio.h>
      #include <math.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local variables
         ./
         SpiceDouble             delta;
         SpiceDouble             iverts[3][3];
         SpiceDouble             overts[3][3];
         SpiceDouble             s;

         s     = sqrt( 3.0 ) / 2.0;

         vpack_c (   s,  -0.5,  7.0, iverts[0] );
         vpack_c ( 0.0,   1.0,  7.0, iverts[1] );
         vpack_c (  -s,  -0.5,  7.0, iverts[2] );

         delta = 1.0;

         pltexp_c ( iverts, delta, overts );

         printf ( "\n"
                  "Vertices of input plate: \n"
                  " I1 = %18.10f %18.10f %18.10f\n"
                  " I2 = %18.10f %18.10f %18.10f\n"
                  " I3 = %18.10f %18.10f %18.10f\n",
                  iverts[0][0], iverts[0][1], iverts[0][2],
                  iverts[1][0], iverts[1][1], iverts[1][2],
                  iverts[2][0], iverts[2][1], iverts[2][2]  );

         printf ( "\n"
                  "Vertices of output plate: \n"
                  " O1 = %18.10f %18.10f %18.10f\n"
                  " O2 = %18.10f %18.10f %18.10f\n"
                  " O3 = %18.10f %18.10f %18.10f\n\n",
                  overts[0][0], overts[0][1], overts[0][2],
                  overts[1][0], overts[1][1], overts[1][2],
                  overts[2][0], overts[2][1], overts[2][2]  );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Vertices of input plate:
       I1 =       0.8660254038      -0.5000000000       7.0000000000
       I2 =       0.0000000000       1.0000000000       7.0000000000
       I3 =      -0.8660254038      -0.5000000000       7.0000000000

      Vertices of output plate:
       O1 =       1.7320508076      -1.0000000000       7.0000000000
       O2 =       0.0000000000       2.0000000000       7.0000000000
       O3 =      -1.7320508076      -1.0000000000       7.0000000000


      Note that the height of the plate is unchanged, but the vectors
      from the centroid to the vertices have doubled in length.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 13-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Changed code example output format.

   -CSPICE Version 1.0.0, 29-FEB-2016 (NJB)

-Index_Entries

   expand triangular plate

-&
*/

{ /* Begin pltexp_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "pltexp_c" );


   pltexp_ ( ( doublereal * ) iverts,
             ( doublereal * ) &delta,
             ( doublereal * ) overts  );


   chkout_c ( "pltexp_c" );

} /* End pltexp_c */
