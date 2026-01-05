/*

-Procedure vprjpi_c ( Vector projection onto plane, inverted )

-Abstract

   Find the vector in a specified plane that maps to a specified
   vector in another plane under orthogonal projection.

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
   #include <math.h>
   #include "SpiceUsr.h"
   #undef    vprjpi_c


   void vprjpi_c ( ConstSpiceDouble    vin    [3],
                   ConstSpicePlane   * projpl,
                   ConstSpicePlane   * invpl,
                   SpiceDouble         vout   [3],
                   SpiceBoolean      * found       )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   vin        I   The projected vector.
   projpl     I   Plane containing `vin'.
   invpl      I   Plane containing inverse image of `vin'.
   vout       O   Inverse projection of `vin'.
   found      O   Flag indicating whether `vout' could be calculated.

-Detailed_Input

   vin,
   projpl,
   invpl       are, respectively, a 3-vector, a SPICE plane
               containing the vector, and a SPICE plane
               containing the inverse image of the vector under
               orthogonal projection onto `projpl'.

-Detailed_Output

   vout        is the inverse orthogonal projection of `vin'. This
               is the vector lying in the plane `invpl' whose
               orthogonal projection onto the plane `projpl' is
               `vin'. `vout' is valid only when `found' (defined below)
               is SPICETRUE. Otherwise, `vout' is undefined.

   found       indicates whether the inverse orthogonal projection
               of `vin' could be computed. `found' is SPICETRUE if so,
               SPICEFALSE otherwise.

-Parameters

   None.

-Exceptions

   1)  If the normal vector of either input plane does not have unit
       length (allowing for round-off error), the error
       SPICE(NONUNITNORMAL) is signaled by a routine in the call tree
       of this routine.

   2)  If the geometric planes defined by `projpl' and `invpl' are
       orthogonal, or nearly so, the inverse orthogonal projection
       of `vin' may be undefined or have magnitude too large to
       represent with double precision numbers. In either such
       case, `found' will be set to SPICEFALSE.

   3)  Even when `found' is SPICETRUE, `vout' may be a vector of extremely
       large magnitude, perhaps so large that it is impractical to
       compute with it. It's up to you to make sure that this
       situation does not occur in your application of this routine.

-Files

   None.

-Particulars

   Projecting a vector orthogonally onto a plane can be thought of
   as finding the closest vector in the plane to the original vector.
   This "closest vector" always exists; it may be coincident with the
   original vector. Inverting an orthogonal projection means finding
   the vector in a specified plane whose orthogonal projection onto
   a second specified plane is a specified vector. The vector whose
   projection is the specified vector is the inverse projection of
   the specified vector, also called the "inverse image under
   orthogonal projection" of the specified vector. This routine
   finds the inverse orthogonal projection of a vector onto a plane.

   Related routines are vprjp_c, which projects a vector onto a plane
   orthogonally, and vproj_c, which projects a vector onto another
   vector orthogonally.

-Examples

   1)   Suppose

           vin    =  ( 0.0, 1.0, 0.0 ),

        and that projpl has normal vector

           projn  =  ( 0.0, 0.0, 1.0 ).

        Also, let's suppose that invpl has normal vector and constant

           invn   =  ( 0.0, 2.0, 2.0 )
           invc   =    4.0.

        Then vin lies on the y-axis in the x-y plane, and we want to
        find the vector vout lying in invpl such that the orthogonal
        projection of vout the x-y plane is vin. Let the notation
        < a, b > indicate the inner product of vectors a and b.
        Since every point x in invpl satisfies the equation

           <  x,  (0.0, 2.0, 2.0)  >  =  4.0,

        we can verify by inspection that the vector

           ( 0.0, 1.0, 1.0 )

        is in invpl and differs from vin by a multiple of projn. So

           ( 0.0, 1.0, 1.0 )

        must be vout.

        To find this result using CSPICE, we can create the
        SPICE planes projpl and invpl using the code fragment

           nvp2pl_c  ( projn,  vin,  &projpl );
           nvc2pl_c  ( invn,   invc, &invpl  );

        and then perform the inverse projection using the call

           vprjpi_c ( vin, &projpl, &invpl, vout );

        vprjpi_c will return the value

           vout = ( 0.0, 1.0, 1.0 );

-Restrictions

   1)  It is recommended that the input planes be created by one of
       the CSPICE routines

          nvc2pl_c ( Normal vector and constant to plane )
          nvp2pl_c ( Normal vector and point to plane    )
          psv2pl_c ( Point and spanning vectors to plane )

       In any case each input plane must have a unit length normal
       vector and a plane constant consistent with the normal
       vector.

-Literature_References

   [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
        7th Edition, Addison Wesley, 1988.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.1, 25-AUG-2021 (JDR) (NJB)

       Edited the header to comply with NAIF standard.

       Added entry #1 to -Exceptions section, and entry #1 to -Restrictions.

   -CSPICE Version 1.1.0, 05-APR-2004 (NJB)

       Computation of LIMIT was re-structured to avoid
       run-time underflow warnings on some platforms.

   -CSPICE Version 1.0.0, 05-MAR-1999 (NJB)

-Index_Entries

   vector projection onto plane inverted

-&
*/

{ /* Begin vprjpi_c */

   /*
   Local constants
   */

   /*
   BOUND is used to bound the magnitudes of the numbers that we
   try to take the reciprocal of, since we can't necessarily invert
   any non-zero number.  We won't try to invert any numbers with
   magnitude less than

      BOUND / dpmax_c()

   BOUND is chosen somewhat arbitrarily....
   */

   #define BOUND      10.0



   /*
   Local variables
   */
   SpiceDouble             denom;
   SpiceDouble             invc;
   SpiceDouble             invn   [3];
   SpiceDouble             limit;
   SpiceDouble             mult;
   SpiceDouble             numer;
   SpiceDouble             projc;
   SpiceDouble             projn  [3];



   /*
   Participate in error tracing.
   */

   if ( return_c() )
   {
      return;
   }

   chkin_c ( "vprjpi_c" );


   /*
   Unpack the planes.
   */
   pl2nvc_c ( projpl, projn, &projc );
   pl2nvc_c ( invpl,  invn,  &invc  );

   /*
   We'll first discuss the computation of VOUT in the nominal case,
   and then deal with the exceptional cases.

   When projpl and invpl are not orthogonal to each other, the
   inverse projection of vin will differ from vin by a multiple of
   projn, the unit normal vector to projpl.  We find this multiple
   by using the fact that the inverse projection vout satisfies the
   plane equation for the inverse projection plane invpl.

      We have

         vout = vin  +  mult * projn;                           (1)

      since vout satisfies

         < vout, invn >  =  invc

      we must have

         <  vin  +  mult * projn,  invn  > = invc

      which in turn implies


                   invc  -  < vin, invn >
         mult  =  ------------------------.                     (2)
                      < projn, invn >

      Having mult, we can compute vout according to equation (1).

   Now, if the denominator in the above expression for mult is zero
   or just too small, performing the division would cause a
   divide-by-zero error or an overflow of mult.  In either case, we
   will avoid carrying out the division, and we'll set found to
   SPICEFALSE.


   Compute the numerator and denominator of the right side of (2).
   */

   numer  =  invc - vdot_c ( vin,   invn );
   denom  =         vdot_c ( projn, invn );


   /*
   If the magnitude of the denominator is greater than

                         BOUND
      limit  =  abs (  ---------- * numer  ),
                        dpmax_c()

   we can safely divide the numerator by the denominator, and the
   magnitude of the result will be no greater than

       dpmax_c()
      ----------- .
        BOUND

   Note that we have ruled out the case where numer and denom are
   both zero by insisting on strict inequality in the comparison of
   denom and limit:
   */

   if ( fabs(numer) < 1.0 )
   {
      limit  =  fabs ( BOUND / dpmax_c() );
   }
   else
   {
      limit  =  fabs (  ( BOUND / dpmax_c() ) * numer  );
   }

   *found  =  ( fabs (denom) > limit );


   if ( *found )
   {
      /*
      We'll compute vout after all.
      */
      mult = numer / denom;

      vlcom_c ( 1.0, vin, mult, projn, vout );
   }


   chkout_c ( "vprjpi_c" );

} /* End vprjpi_c */
