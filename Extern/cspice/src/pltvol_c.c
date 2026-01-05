/*

-Procedure pltvol_c ( Compute volume of plate model )

-Abstract

   Compute the volume of a three-dimensional region bounded by a
   collection of triangular plates.

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
   #include "SpiceZmc.h"
   #undef pltvol_c


   SpiceDouble pltvol_c ( SpiceInt           nv,
                          ConstSpiceDouble   vrtces[][3],
                          SpiceInt           np,
                          ConstSpiceInt      plates[][3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   nv         I   Number of vertices.
   vrtces     I   Array of vertices.
   np         I   Number of triangular plates.
   plates     I   Array of plates.

   The function returns the volume of the spatial region bounded
   by the plates.

-Detailed_Input

   nv          is the number of vertices comprising the plate
               model.

   vrtces      is an array containing the plate model's vertices.
               Elements

                  vrtces[i-1][0]
                  vrtces[i-1][1]
                  vrtces[i-1][2]

               are, respectively, the X, Y, and Z components of
               the ith vertex, where `i' ranges from 1 to `nv'.

               This routine doesn't associate units with the
               vertices.


   np          is the number of triangular plates comprising the
               plate model.

   plates      is an array containing 3-tuples of integers
               representing the model's plates. The elements of
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
               plate. In other words, if `v1', `v2', `v3' are the
               vertices of a plate, then

                  ( v2 - v1 )  x  ( v3 - v2 )

               points in the outward normal direction. Here
               "x" denotes the vector cross product operator.

-Detailed_Output

   The function returns the volume of the spatial region bounded
   by the plates.

   If the components of the vertex array have length unit L, then the
   output volume has units

       3
      L

-Parameters

   None.

-Exceptions

   1)  The input plate model must define a spatial region with
       a boundary. This routine does not check the inputs to
       verify this condition. See the -Restrictions section below.

   2)  If the number of vertices is less than 4, the error
       SPICE(TOOFEWVERTICES) is signaled by a routine in the call tree of
       this routine.

   3)  If the number of plates is less than 4, the error SPICE(TOOFEWPLATES)
       is signaled by a routine in the call tree of this routine.

   4)  If any plate contains a vertex index outside of the range

          [1, nv]

       the error SPICE(INDEXOUTOFRANGE) is signaled by a routine in the call
       tree of this routine.

-Files

   None.

-Particulars

   This routine computes the volume of a spatial region bounded by
   a set of triangular plates. If the plate set does not actually
   form the boundary of a spatial region, the result of this routine
   is invalid.

   Examples:

      Valid inputs
      ------------
      Tetrahedron
      Box
      Tiled ellipsoid
      Two disjoint boxes

      Invalid inputs
      --------------
      Single plate
      Tiled ellipsoid with one plate removed
      Two boxes with intersection having positive volume

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input
   (if any), the compiler and supporting libraries, and the machine
   specific arithmetic implementation.


   1) Compute the volume of the pyramid defined by the four
      triangular plates whose vertices are the 3-element
      subsets of the set of vectors

         ( 0, 0, 0 )
         ( 1, 0, 0 )
         ( 0, 1, 0 )
         ( 0, 0, 1 )


      Example code begins here.


      /.
         Program pltvol_ex1

         Compute the volume of a plate model representing the pyramid
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
         SpiceDouble             vol;

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



         vol = pltvol_c( NVERT, vrtces, NPLATE, plates );

         printf ( "Expected volume =      1/6\n"        );
         printf ( "Computed volume  =   %24.17e\n", vol );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Expected volume =      1/6
      Computed volume  =    1.66666666666666657e-01


-Restrictions

   1)  The plate collection must describe a surface and enclose a
       volume such that the divergence theorem (see [1]) is
       applicable.

-Literature_References

   [1]  T. Apostol, "Calculus, Vol. II," John Wiley & Sons, 1969.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 13-APR-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 24-OCT-2016 (NJB)

-Index_Entries

   compute plate model volume

-&
*/

{ /* Begin pltvol_c */



   /*
   Local variables
   */
   SpiceDouble             retval;


   /*
   Participate in error tracing.
   */
   chkin_c ( "pltvol_c" );


   retval = (SpiceDouble) pltvol_ ( (SpiceInt     *) &nv,
                                    (SpiceDouble  *) vrtces,
                                    (SpiceInt     *) &np,
                                    (SpiceInt     *) plates  );
   chkout_c ( "pltvol_c" );

   return ( retval );

} /* End pltvol_c */
