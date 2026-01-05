/*

-Procedure surfnm_c ( Surface normal vector on an ellipsoid )

-Abstract

   Compute the outward-pointing, unit normal vector at a point on
   the surface of an ellipsoid.

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

   ELLIPSOID
   GEOMETRY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #undef    surfnm_c


   void surfnm_c ( SpiceDouble        a,
                   SpiceDouble        b,
                   SpiceDouble        c,
                   ConstSpiceDouble   point[3],
                   SpiceDouble        normal[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   a          I   Length of the ellipsoid semi-axis along the X-axis.
   b          I   Length of the ellipsoid semi-axis along the Y-axis.
   c          I   Length of the ellipsoid semi-axis along the Z-axis.
   point      I   Body-fixed coordinates of a point on the ellipsoid.
   normal     O   Outward pointing unit normal to ellipsoid at `point'.

-Detailed_Input

   a           is the length of the semi-axis of the ellipsoid that is
               parallel to the X-axis of the body-fixed reference frame.

   b           is the length of the semi-axis of the ellipsoid that is
               parallel to the Y-axis of the body-fixed reference frame.

   c           is the length of the semi-axis of the ellipsoid that is
               parallel to the Z-axis of the body-fixed reference frame.

   point       is a 3-vector giving the bodyfixed coordinates of a point
               on the ellipsoid. In bodyfixed coordinates, the semi-axes
               of the ellipsoid are aligned with the X, Y, and Z-axes of
               the reference frame.

-Detailed_Output

   normal      is the unit vector pointing away from the ellipsoid and
               normal to the ellipsoid at `point'.

-Parameters

   None.

-Exceptions

   1)  If any of the axes are non-positive, the error
       SPICE(BADAXISLENGTH) is signaled by a routine in the call tree
       of this routine.

-Files

   None.

-Particulars

   This routine computes the outward pointing unit normal vector to
   the ellipsoid having semi-axes of length `a', `b', and `c' from the
   point `point'.

-Examples

   A typical use of surfnm_c would be to find the angle of incidence
   of the light from the sun at a point on the surface of an
   ellipsoid.

   Let q be a 3-vector representing the rectangular body-fixed
   coordinates of a point on the ellipsoid (we are assuming that
   the axes of the ellipsoid are aligned with the axes of the
   body fixed frame.) Let v be the vector from q to the sun in
   bodyfixed coordinates. Then the following code fragment could
   be used to compute angle of incidence of sunlight at q.

      surfnm_c   ( a, b, c, q, nrml );

      incidn = vsep_c ( v, nrml );

-Restrictions

   1)  It is assumed that the input point is indeed on the ellipsoid.
       No checking for this is done.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)

-Version

   -CSPICE Version 1.3.2, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Updated the documentation to refer to "reference frame" instead
       of "coordinate system" as per NAIF conventions.

   -CSPICE Version 1.3.1, 31-JAN-2008 (BVS)

       Removed '-Revisions' from the header.

   -CSPICE Version 1.3.0, 22-OCT-1998 (NJB)

       Made input vector const.

   -CSPICE Version 1.2.0, 08-FEB-1998 (NJB)

       Removed local variables used for temporary capture of outputs.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

        Based on SPICELIB Version 1.2.0, 07-AUG-1996 (WLT)

-Index_Entries

   surface normal vector on an ellipsoid

-&
*/

{ /* Begin surfnm_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "surfnm_c");


   /*
   Call the f2c'd surfpt.
   */
   surfnm_( (doublereal *)  &a,
            (doublereal *)  &b,
            (doublereal *)  &c,
            (doublereal *)  point,
            (doublereal *)  normal );


   chkout_c ( "surfnm_c" );


} /* End surfnm_c */
