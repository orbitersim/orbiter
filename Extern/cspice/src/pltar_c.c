/*

-Procedure pltar_c ( Compute area of plate set )

-Abstract

   Compute the total area of a collection of triangular plates.

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

   DSK
   GEOMETRY
   MATH
   TOPOGRAPHY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #undef pltar_c


   SpiceDouble pltar_c ( SpiceInt           nv,
                         ConstSpiceDouble   vrtces [][3],
                         SpiceInt           np,
                         ConstSpiceInt      plates [][3]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   nv         I   Number of vertices.
   vrtces     I   Array of vertices.
   np         I   Number of triangular plates.
   plates     I   Array of plates.

   The function returns the total area of the set of plates.

-Detailed_Input

   nv          is the number of vertices comprising the plate
               set.

   vrtces      is an array containing the plate model's vertices.
               Elements

                  vrtces[i-1][0]
                  vrtces[i-1][1]
                  vrtces[i-1][2]

               are, respectively, the X, Y, and Z components of
               the ith vertex, where i ranges from 1 to `nv'.

               This routine doesn't associate units with the
               vertices.

   np          is the number of triangular plates comprising the
               plate set.

   plates      is an array containing 3-tuples of integers
               representing the set of plates. The elements of
               `plates' are vertex indices. The vertex indices are
               1-based: vertices have indices ranging from 1 to
               `nv'. The elements

                  plates[i-1][0]
                  plates[i-1][1]
                  plates[i-1][2]

               are, respectively, the indices of the vertices
               comprising the ith plate.

               Note that the order of the vertices of a plate is
               significant: the vertices must be ordered in the
               positive (counterclockwise) sense with respect to
               the outward normal direction associated with the
               plate. In other words, if V1, V2, V3 are the
               vertices of a plate, then

                  ( V2 - V1 )  x  ( V3 - V2 )

               points in the outward normal direction. Here
               "x" denotes the vector cross product operator.

-Detailed_Output

   The function returns the total area of the input set of plates.
   Each plate contributes the area of the triangle defined by the
   plate's vertices.

   If the components of the vertex array have length unit L, then the
   output area has units

       2
      L

-Parameters

   None.

-Exceptions

   1)  If the number of plates is less than 0, the error
       SPICE(BADPLATECOUNT) is signaled by a routine in the call tree
       of this routine.

   2)  If the number of plates is positive and the number of vertices
       is less than 3, the error SPICE(TOOFEWVERTICES) is signaled by
       a routine in the call tree of this routine.

   3)  If any plate contains a vertex index outside of the range

          [1, nv]

       the error SPICE(INDEXOUTOFRANGE) is signaled by a routine in
       the call tree of this routine.

-Files

   None.

-Particulars

   This routine computes the total area of a set of triangular
   plates. The plates need not define a closed surface.

   Examples of valid plate sets:

      Tetrahedron
      Box
      Tiled ellipsoid
      Tiled ellipsoid with one plate removed
      Two disjoint boxes
      Two boxes with intersection having positive volume
      Single plate
      Empty plate set

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input
   (if any), the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Compute the area of the pyramid defined by the four
      triangular plates whose vertices are the 3-element
      subsets of the set of vectors:

         ( 0, 0, 0 )
         ( 1, 0, 0 )
         ( 0, 1, 0 )
         ( 0, 0, 1 )


      Example code begins here.


      /.
         Program pltar_ex1

         Compute the area of a plate model representing the pyramid
         with one vertex at the origin and the other vertices
         coinciding with the standard basis vectors.
      ./

      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local constants
         ./
         #define NVERT           4
         #define NPLATE          4

         /.
         Local variables
         ./
         SpiceDouble             area;

         /.
         Let the notation

            < A, B >

         denote the dot product of vectors A and B.

         The plates defined below lie in the following planes,
         respectively:

            Plate 1:    { P :  < P, (-1,  0,  0) > = 0 }
            Plate 2:    { P :  < P, ( 0, -1,  0) > = 0 }
            Plate 3:    { P :  < P, ( 0,  0, -1) > = 0 }
            Plate 4:    { P :  < P, ( 1,  1,  1) > = 1 }

         ./
         SpiceDouble             vrtces[NVERT ][3] =

                                 { { 0.0, 0.0, 0.0 },
                                   { 1.0, 0.0, 0.0 },
                                   { 0.0, 1.0, 0.0 },
                                   { 0.0, 0.0, 1.0 }  };

         SpiceInt                plates[NPLATE][3] =

                                 { { 1, 4, 3 },
                                   { 1, 2, 4 },
                                   { 1, 3, 2 },
                                   { 2, 3, 4 }  };

         area = pltar_c( NVERT, vrtces, NPLATE, plates );

         printf ( "Expected area  =    (3 + sqrt(3)) / 2\n"
                  "               =    0.23660254037844384e+01\n" );
         printf ( "Computed area  =   %24.17e\n", area            );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Expected area  =    (3 + sqrt(3)) / 2
                     =    0.23660254037844384e+01
      Computed area  =    2.36602540378443837e+00


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 04-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 24-OCT-2016 (NJB)

-Index_Entries

   compute plate model area

-&
*/

{ /* Begin pltar_c */



   /*
   Local variables
   */
   SpiceDouble             retval;


   /*
   Participate in error tracing.
   */
   chkin_c ( "pltar_c" );


   retval = (SpiceDouble) pltar_ ( ( integer    * ) &nv,
                                   ( doublereal * ) vrtces,
                                   ( integer    * ) &np,
                                   ( integer    * ) plates  );
   chkout_c ( "pltar_c" );

   return ( retval );


} /* End pltar_c */
