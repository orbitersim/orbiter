/*

-Procedure vperp_c ( Perpendicular component of a 3-vector )

-Abstract

   Find the component of a vector that is perpendicular to a second
   vector. All vectors are 3-dimensional.

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

   VECTOR

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #undef    vperp_c


   void vperp_c ( ConstSpiceDouble   a[3],
                  ConstSpiceDouble   b[3],
                  SpiceDouble        p[3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   a          I   The vector whose orthogonal component is sought.
   b          I   The vector used as the orthogonal reference.
   p          O   The component of `a' orthogonal to `b'.

-Detailed_Input

   a           is a double precision, 3-dimensional vector. It the
               vector whose component orthogonal to `b' is sought. (There
               is a unique decomposition of `a' into a sum v + p, where `v'
               is parallel to `b' and `p' is orthogonal to `b'. We want the
               component `p'.)

   b           is a double precision, 3-dimensional vector. This
               vector is the vector used as a reference for the
               decomposition of `a'.

-Detailed_Output

   p           is a double precision, 3-dimensional vector containing
               the component of `a' that is orthogonal to `b'.
               `p' may overwrite either `a' or `b'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   Given and non-zero vector `b' and a vector `a', there is a unique
   decomposition of `a' as a sum v + p such that `p' is orthogonal
   to `b' and `v' is parallel to `b'. This routine finds the vector `p'.

   If `b' is a zero vector, `p' will be identical to `a'.

-Examples

   The following table gives sample inputs and results from calling
   vperp_c.

      a                  b                 p
      ------------------------------------------
      (6, 6, 6)      ( 2, 0, 0)        (0, 6, 6)
      (6, 6, 6)      (-3, 0, 0)        (0, 6, 6)
      (6, 6, 0)      ( 0, 7, 0)        (6, 0, 0)
      (6, 0, 0)      ( 0, 0, 9)        (6, 0, 0)

-Restrictions

   None.

-Literature_References

   [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
        7th Edition, Addison Wesley, 1988.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.2, 13-APR-2021 (JDR)

       Updated the header to comply with NAIF standard.

   -CSPICE Version 1.2.1, 24-APR-2010 (EDW)

       Minor edit to code comments eliminating typo.

   -CSPICE Version 1.2.0, 22-OCT-1998 (NJB)

       Made input vectors const.

   -CSPICE Version 1.1.0, 06-MAR-1998 (EDW)

       Removed non printing character.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (WLT)

-Index_Entries

   perpendicular component of a 3-vector

-&
*/

{ /* Begin vperp_c */

   /*
   Local variables
   */

   SpiceDouble     biga;
   SpiceDouble     bigb;
   SpiceDouble     r[3];
   SpiceDouble     t[3];
   SpiceDouble     v[3];


   biga = MaxAbs( a[0] , MaxAbs( a[1], a[2] ) );
   bigb = MaxAbs( b[0] , MaxAbs( b[1], b[2] ) );


   /*
   If a or b is zero, set p to zero and return.
   */

   if ( biga == 0. || bigb == 0. )
      {
      p[0] = 0.;
      p[1] = 0.;
      p[2] = 0.;
      return;
      }


   vscl_c ( 1./biga, a, t );
   vscl_c ( 1./bigb, b, r );

   vproj_c ( t,    r, v );
   vsub_c  ( t,    v, p );
   vscl_c  ( biga, p, p );


} /* End vperp_c */
