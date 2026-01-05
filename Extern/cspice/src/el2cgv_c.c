/*

-Procedure el2cgv_c ( Ellipse to center and generating vectors )

-Abstract

   Convert a SPICE ellipse to a center vector and two generating
   vectors. The selected generating vectors are semi-axes of the
   ellipse.

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
   #undef    el2cgv_c


   void el2cgv_c ( ConstSpiceEllipse   * ellips,
                   SpiceDouble           center[3],
                   SpiceDouble           smajor[3],
                   SpiceDouble           sminor[3]  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   ellips     I   A SPICE ellipse.
   center,
   smajor,
   sminor     O   Center and semi-axes of `ellips'.

-Detailed_Input

   ellips      is a SPICE ellipse.

-Detailed_Output

   center,
   smajor,
   sminor      are, respectively, a center vector, a semi-major
               axis vector, and a semi-minor axis vector that
               generate the input ellipse. This ellipse is the
               set of points

                  center + cos(theta) * smajor + sin(theta) * sminor

               where theta ranges over the interval (-pi, pi].

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   SPICE ellipses serve to simplify calling sequences and reduce
   the chance for error in declaring and describing argument lists
   involving ellipses.

   The set of ellipse conversion routines is

      cgv2el_c ( Center and generating vectors to ellipse )
      el2cgv_c ( Ellipse to center and generating vectors )

   A word about the output of this routine: the semi-major axis of
   an ellipse is a vector of largest possible magnitude in the set

      cos(theta) * vec1  +  sin(theta) * vec2,

   where `theta' is in the interval (-pi, pi].  There are two such
   vectors; they are additive inverses of each other. The semi-minor
   axis is an analogous vector of smallest possible magnitude. The
   semi-major and semi-minor axes are orthogonal to each other. If
   smajor and sminor are choices of semi-major and semi-minor axes,
   then the input ellipse can also be represented as the set of
   points

      center  +  cos(theta) * smajor  +  sin(theta) * sminor

   where theta ranges over the interval (-pi, pi].

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Given a SPICE ellipse structure, extract its components into
      independent variables.


      Example code begins here.


      /.
         Program el2cgv_ex1
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
         printf( "   Semi-minor axis: %9.6f %9.6f %9.6f\n",
                                       ellips.semiMinor[0],
                                       ellips.semiMinor[1],
                                       ellips.semiMinor[2] );
         printf( "   Semi-major axis: %9.6f %9.6f %9.6f\n",
                                       ellips.semiMajor[0],
                                       ellips.semiMajor[1],
                                       ellips.semiMajor[2] );
         printf( "   Center         : %9.6f %9.6f %9.6f\n",
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
         printf( "   Semi-minor axis: %9.6f %9.6f %9.6f\n",
                            sminor[0], sminor[1], sminor[2] );
         printf( "   Semi-major axis: %9.6f %9.6f %9.6f\n",
                          smajor[0], smajor[1], smajor[2] );
         printf( "   Center         : %9.6f %9.6f %9.6f\n",
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


   2) Given an ellipsoid and a viewpoint exterior to it, calculate
      the limb ellipse as seen from that viewpoint.


      Example code begins here.


      /.
         Program el2cgv_ex2
      ./
      #include <math.h>
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define UBEL         9

         /.
         Local variables.
         ./
         SpiceDouble          a;
         SpiceDouble          b;
         SpiceDouble          c;
         SpiceDouble          ecentr [3];
         SpiceEllipse         limb;
         SpiceDouble          smajor [3];
         SpiceDouble          sminor [3];

         /.
         Define a viewpoint exterior to the ellipsoid.
         ./
         SpiceDouble          viewpt [3] = { 2.0,  0.0,  0.0 };

         /.
         Define an ellipsoid.
         ./
         a = sqrt( 2.0 );
         b = 2.0 * sqrt( 2.0 );
         c = sqrt( 2.0 );

         /.
         Calculate the limb ellipse as seen by from the
         viewpoint.
         ./
         edlimb_c ( a, b, c, viewpt, &limb );

         /.
         Output the structure components.
         ./
         el2cgv_c ( &limb, ecentr, smajor, sminor );

         printf( "Limb ellipse as seen from viewpoint:\n" );
         printf( "   Semi-minor axis: %10.6f %10.6f %10.6f\n",
                              sminor[0], sminor[1], sminor[2] );
         printf( "   Semi-major axis: %10.6f %10.6f %10.6f\n",
                              smajor[0], smajor[1], smajor[2] );
         printf( "   Center         : %10.6f %10.6f %10.6f\n",
                              ecentr[0], ecentr[1], ecentr[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Limb ellipse as seen from viewpoint:
         Semi-minor axis:   0.000000   0.000000  -1.000000
         Semi-major axis:   0.000000   2.000000  -0.000000
         Center         :   1.000000   0.000000   0.000000


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

   -CSPICE Version 1.0.0, 12-JUN-1999 (NJB)

-Index_Entries

   ellipse to center and generating vectors

-&
*/

{ /* Begin el2cgv_c */

   /*
   Error free.
   */


   MOVED ( ellips->center,    3, center );
   MOVED ( ellips->semiMajor, 3, smajor );
   MOVED ( ellips->semiMinor, 3, sminor );


} /* End el2cgv_c */
