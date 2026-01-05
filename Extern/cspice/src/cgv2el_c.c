/*

-Procedure cgv2el_c ( Center and generating vectors to ellipse )

-Abstract

   Form a SPICE ellipse from a center vector and two generating
   vectors.

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

   ELLIPSES

-Keywords

   ELLIPSE
   GEOMETRY

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #undef    cgv2el_c


   void cgv2el_c ( ConstSpiceDouble    center[3],
                   ConstSpiceDouble    vec1  [3],
                   ConstSpiceDouble    vec2  [3],
                   SpiceEllipse      * ellips    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   center,
   vec1,
   vec2       I   Center and two generating vectors for an ellipse.
   ellips     O   The SPICE ellipse defined by the input vectors.

-Detailed_Input

   center,
   vec1,
   vec2        are a center and two generating vectors defining
               an ellipse in three-dimensional space. The
               ellipse is the set of points

                  center  +  cos(theta) * vec1  +  sin(theta) * vec2

               where theta ranges over the interval (-pi, pi].
               `vec1' and `vec2' need not be linearly independent.

-Detailed_Output

   ellips      is the SPICE ellipse defined by the input
               vectors.

-Parameters

   None.

-Exceptions

   1)  If `vec1' and `vec2' are linearly dependent, `ellips' will be
       degenerate. SPICE ellipses are allowed to represent
       degenerate geometric ellipses.

-Files

   None.

-Particulars

   SPICE ellipses serve to simplify calling sequences and reduce
   the chance for error in declaring and describing argument lists
   involving ellipses.

   The set of ellipse conversion routines is

      cgv2el_c ( Center and generating vectors to ellipse )
      el2cgv_c ( Ellipse to center and generating vectors )

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a SPICE ellipse given its center and two linearly
      independent generating vectors of the ellipse.


      Example code begins here.


      /.
         Program cgv2el_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          ecentr [3];
         SpiceDouble          smajor [3];
         SpiceDouble          sminor [3];

         SpiceEllipse         ellips;

         /.
         Define the center and two linearly independent
         generating vectors of an ellipse (the vectors need not
         be linearly independent).
         ./
         SpiceDouble          center [3] = { -1.0,  1.0, -1.0 };
         SpiceDouble          vec1   [3] = {  1.0,  1.0,  1.0 };
         SpiceDouble          vec2   [3] = {  1.0, -1.0,  1.0 };

         /.
         Create the `ellips'.
         ./
         cgv2el_c ( center, vec1, vec2, &ellips );

         /.
         In a real application, please use CSPICE API el2cgv_c
         to retrieve the center and generating vectors from the
         ellipse structure (see next block).
         ./
         printf( "SPICE ellipse:\n" );
         printf( " Semi-minor axis: %9.6f %9.6f %9.6f\n",
                                     ellips.semiMinor[0],
                                     ellips.semiMinor[1],
                                     ellips.semiMinor[2] );
         printf( " Semi-major axis: %9.6f %9.6f %9.6f\n",
                                     ellips.semiMajor[0],
                                     ellips.semiMajor[1],
                                     ellips.semiMajor[2] );
         printf( " Center         : %9.6f %9.6f %9.6f\n",
                                        ellips.center[0],
                                        ellips.center[1],
                                        ellips.center[2] );
         printf( " \n" );

         /.
         Obtain the center and generating vectors from the
         `ellips'.
         ./
         el2cgv_c ( &ellips, ecentr, smajor, sminor );

         printf( "SPICE ellipse (using el2cgv_c):\n" );
         printf( " Semi-minor axis: %9.6f %9.6f %9.6f\n",
                          sminor[0], sminor[1], sminor[2] );
         printf( " Semi-major axis: %9.6f %9.6f %9.6f\n",
                          smajor[0], smajor[1], smajor[2] );
         printf( " Center         : %9.6f %9.6f %9.6f\n",
                          ecentr[0], ecentr[1], ecentr[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      SPICE ellipse:
       Semi-minor axis:  0.000000  1.414214  0.000000
       Semi-major axis:  1.414214 -0.000000  1.414214
       Center         : -1.000000  1.000000 -1.000000

      SPICE ellipse (using el2cgv_c):
       Semi-minor axis:  0.000000  1.414214  0.000000
       Semi-major axis:  1.414214 -0.000000  1.414214
       Center         : -1.000000  1.000000 -1.000000


   2) Find the intersection of an ellipse with a plane.


      Example code begins here.


      /.
         Program cgv2el_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceEllipse         ellips;
         SpicePlane           plane;
         SpiceDouble          xpts   [2][3];

         SpiceInt             i;
         SpiceInt             nxpts;

         /.
         The ellipse is defined by the vectors `center', `vec1', and
         `vec2'. The plane is defined by the normal vector `normal'
         and the `center'.
         ./
         SpiceDouble          center [3] = {  0.0,  0.0,  0.0 };
         SpiceDouble          vec1   [3] = {  1.0,  7.0,  2.0 };
         SpiceDouble          vec2   [3] = { -1.0,  1.0,  3.0 };

         SpiceDouble          normal [3] = {  0.0,  1.0,  0.0 };

         /.
         Make a SPICE ellipse and a plane.
         ./
         cgv2el_c ( center, vec1, vec2, &ellips );
         nvp2pl_c ( normal, center,     &plane  );

         /.
         Find the intersection of the ellipse and plane.
         `nxpts' is the number of intersection points; `xpts'
         are the points themselves.
         ./
         inelpl_c ( &ellips, &plane, &nxpts, xpts[0], xpts[1] );

         printf( "Number of intercept points: %2d\n", nxpts );

         for ( i = 0; i < nxpts; i++ )
         {
            printf( " Point %1d: %9.6f %9.6f %9.6f\n", i,
                                 xpts[i][0], xpts[i][1], xpts[i][2] );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Number of intercept points:  2
       Point 0:  1.131371  0.000000 -2.687006
       Point 1: -1.131371 -0.000000  2.687006


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.0, 24-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Added complete code examples.

       Changed the output argument name "ellipse" to "ellips" for
       consistency with other routines.

   -CSPICE Version 1.0.0, 05-MAR-1999 (NJB)

-Index_Entries

   center and generating vectors to ellipse

-&
*/

{ /* Begin cgv2el_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "cgv2el_c" );

   /*
   The center of the ellipse is held in the first three elements.
   */
   MOVED ( center, 3, ellips->center );

   /*
   Find the semi-axes of the ellipse.  These may be degenerate.
   */
   saelgv_c ( vec1, vec2, ellips->semiMajor, ellips->semiMinor );


   chkout_c ( "cgv2el_c" );

} /* End cgv2el_c */
