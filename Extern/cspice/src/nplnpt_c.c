/*

-Procedure nplnpt_c ( Nearest point on line to point )

-Abstract

   Find the nearest point on a line to a specified point, and find
   the distance between the two points.

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

   GEOMETRY
   MATH
   VECTOR

*/

   #include "SpiceUsr.h"
   #undef    nplnpt_c


   void nplnpt_c ( ConstSpiceDouble    linpt  [3],
                   ConstSpiceDouble    lindir [3],
                   ConstSpiceDouble    point  [3],
                   SpiceDouble         pnear  [3],
                   SpiceDouble       * dist       )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   linpt,
   lindir     I   Point on a line and the line's direction vector.
   point      I   A second point.
   pnear      O   Nearest point on the line to point.
   dist       O   Distance between point and pnear.

-Detailed_Input

   linpt,
   lindir      are, respectively, a point and a direction vector
               that define a line in 3-dimensional space. The
               line is the set of points

                  linpt   +   t * lindir

               where t is any real number.

   point       is a point in 3-dimensional space.

-Detailed_Output

   pnear       is the nearest point on the input line to the input
               point.

   dist        is the distance between the input line and input
               point.

-Parameters

   None.

-Exceptions

   1)  If the line direction vector `lindir' is the zero vector, the
       error SPICE(ZEROVECTOR) is signaled.

-Files

   None.

-Particulars

   For every line L and point P, there is a unique closest point
   on L to P. Call this closest point C. It is always true that
   P - C  is perpendicular to L, and the length of P - C is called
   the "distance" between P and L.

-Examples

   1)  Suppose a line passes through the point ( 1, 2, 3 ) and
       has direction vector ( 0, 1, 1 ).  We wish to find the
       closest point on the line to the point ( -6, 9, 10 ).  We
       can use the code fragment

          #include "SpiceUsr.h"
               .
               .
               .
          LINPT[0]   =  1.0;
          LINPT[1]   =  2.0;
          LINPT[2]   =  3.0;

          LINDIR[0]  =  0.0;
          LINDIR[1]  =  1.0;
          LINDIR[2]  =  1.0;

          POINT[0]   = -6.0;
          POINT[1]   =  9.0;
          POINT[2]   = 10.0;

          nplnpt_c ( linpt, lindir, point, pnear, &dist );


       After the call, pnear will take the value

          ( 1., 9., 10. );

       dist will be 7.0.

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

   -CSPICE Version 1.0.0, 16-AUG-1999 (NJB)

-Index_Entries

   distance between point and line
   nearest point on line to point

-&
*/

{ /* Begin nplnpt_c */


   /*
   Local variables
   */
   SpiceDouble             trans [3];



   /*
   We need a real direction vector to work with.
   */
   if (  vzero_c (lindir)  )
   {
      chkin_c  ( "nplnpt_c"                           );
      setmsg_c ( "Direction vector must be non-zero." );
      sigerr_c ( "SPICE(ZEROVECTOR)"                  );
      chkout_c ( "nplnpt_c"                           );
      return;
   }


   /*
   We translate line and input point so as to put the line through
   the origin.  Then the nearest point on the translated line to the
   translated point TRANS is the projection of TRANS onto the line.
   */

   vsub_c  ( point,  linpt,  trans );
   vproj_c ( trans,  lindir, pnear );
   vadd_c  ( pnear,  linpt,  pnear );

   *dist = vdist_c ( pnear,  point );


} /* End nplnpt_c */
