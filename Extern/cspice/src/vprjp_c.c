/*

-Procedure vprjp_c ( Vector projection onto plane )

-Abstract

   Project a vector onto a specified plane, orthogonally.

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

   PLANES

-Keywords

   GEOMETRY
   MATH
   PLANE
   VECTOR

*/

   #include "SpiceUsr.h"
   #undef    vprjp_c


   void vprjp_c ( ConstSpiceDouble    vin   [3],
                  ConstSpicePlane   * plane,
                  SpiceDouble         vout  [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   vin        I   Vector to be projected.
   plane      I   A SPICE plane onto which `vin' is projected.
   vout       O   Vector resulting from projection.

-Detailed_Input

   vin         is a 3-vector that is to be orthogonally projected
               onto a specified plane.

   plane       is a SPICE plane that represents the geometric
               plane onto which `vin' is to be projected.

               The normal vector component of a SPICE plane has
               unit length.

-Detailed_Output

   vout        is the vector resulting from the orthogonal
               projection of `vin' onto `plane'. `vout' is the closest
               point in the specified plane to `vin'.

-Parameters

   None.

-Exceptions

   1)  If the normal vector of the input plane does not have unit
       length (allowing for round-off error), the error
       SPICE(NONUNITNORMAL) is signaled by a routine in the call tree
       of this routine.

-Files

   None.

-Particulars

   Projecting a vector `vin' orthogonally onto a plane can be thought
   of as finding the closest vector in the plane to `vin'. This
   "closest vector" always exists; it may be coincident with the
   original vector.

   Two related routines are vprjpi_c, which inverts an orthogonal
   projection of a vector onto a plane, and vproj_c, which projects
   a vector orthogonally onto another vector.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the closest point in the ring plane of a planet to a
      spacecraft located at a point (in body-fixed coordinates).


      Example code begins here.


      /.
         Program vprjp_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local variables.
         ./
         SpiceDouble          proj   [3];
         SpicePlane           ringpl;

         /.
         Set the spacecraft location and define the normal
         vector as the normal to the equatorial plane, and
         the origin at the body/ring center.
         ./
         SpiceDouble          scpos  [3] = { -29703.16955,
                                             879765.72163,
                                             -137280.21757 };

         SpiceDouble          norm   [3] = { 0.0, 0.0, 1.0 };

         SpiceDouble          orig   [3] = { 0.0, 0.0, 0.0 };

         /.
         Create the plane structure.
         ./
         nvp2pl_c ( norm, orig, &ringpl );

         /.
         Project the position vector onto the ring plane.
         ./
         vprjp_c ( scpos, &ringpl, proj );

         printf( "Projection of S/C position onto ring plane:\n" );
         printf( "%17.5f %16.5f %16.5f\n", proj[0], proj[1], proj[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Projection of S/C position onto ring plane:
           -29703.16955     879765.72163          0.00000


-Restrictions

   1)  It is recommended that the input plane be created by one of
       the CSPICE routines

          nvc2pl_c ( Normal vector and constant to plane )
          nvp2pl_c ( Normal vector and point to plane    )
          psv2pl_c ( Point and spanning vectors to plane )

       In any case the input plane must have a unit length normal
       vector and a plane constant consistent with the normal
       vector.

-Literature_References

   [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
        7th Edition, Addison Wesley, 1988.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)

-Version

   -CSPICE Version 1.0.2, 24-AUG-2021 (JDR) (NJB)

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Added entry #1 to -Exceptions section, and entry #1 to -Restrictions.

   -CSPICE Version 1.0.1, 01-FEB-2017 (BVS)

       Typo fix: pnv2pl_c -> nvp2pl_c.

   -CSPICE Version 1.0.0, 05-MAR-1999 (NJB)

-Index_Entries

   vector projection onto plane

-&
*/

{ /* Begin vprjp_c */


   /*
   Local variables
   */
   SpiceDouble             constant;
   SpiceDouble             normal    [3];


   /*
   Participate in error tracing.
   */

   if ( return_c() )
   {
      return;
   }

   chkin_c ( "vprjp_c" );


   /*
   Obtain a unit vector normal to the input plane, and a constant
   for the plane.
   */
   pl2nvc_c ( plane, normal, &constant );


   /*
   Let the notation < a, b > indicate the inner product of vectors
   a and b.

   vin differs from its projection onto plane by some multiple of
   normal.  That multiple is


             < vin - vout, normal >                 *  normal

      =   (  < vin, normal > - < vout, normal >  )  *  normal

      =   (  < vin, normal > - const             )  *  normal


   Subtracting this multiple of normal from vin yields vout.
   */

   vlcom_c (  1.0,
              vin,
              constant - vdot_c ( vin, normal ),
              normal,
              vout                              );


   chkout_c ( "vprjp_c" );

} /* End vprjp_c */
