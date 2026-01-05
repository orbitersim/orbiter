/*

-Procedure vrotv_c ( Vector rotation about an axis )

-Abstract

   Rotate a vector about a specified axis vector by a specified
   angle and return the rotated vector.

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

   ROTATION

-Keywords

   ROTATION
   VECTOR

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #undef    vrotv_c


   void vrotv_c ( ConstSpiceDouble  v     [3],
                  ConstSpiceDouble  axis  [3],
                  SpiceDouble       theta,
                  SpiceDouble       r     [3] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v          I   Vector to be rotated.
   axis       I   Axis of the rotation.
   theta      I   Angle of rotation (radians).
   r          O   Result of rotating `v' about axis by `theta'.

-Detailed_Input

   v           is a 3-dimensional vector to be rotated.

   axis        is the axis about which the rotation is to be
               performed.

   theta       is the angle through which `v' is to be rotated about
               axis.

-Detailed_Output

   r           is the result of rotating `v' about `axis' by `theta'.
               If `axis' is the zero vector, r = v.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If the input axis is the zero vector, `r' will be returned
       as `v'.

-Files

   None.

-Particulars

   This routine computes the result of rotating (in a right handed
   sense) the vector v about the axis represented by axis through
   an angle of theta radians.

   If w is a unit vector parallel to axis, then r is given by:

       r = v + ( 1 - cos(theta) ) (w X(w X v)) + sin(theta) (w X v)

   where "X" above denotes the vector cross product.

-Examples

   If axis = ( 0, 0, 1 ) and theta = pi/2 then the following results
   for r will be obtained

           v                           r
      -------------             ----------------
      ( 1, 2, 3 )                ( -2, 1, 3 )
      ( 1, 0, 0 )                (  0, 1, 0 )
      ( 0, 1, 0 )                ( -1, 0, 0 )


   If axis = ( 0, 1, 0 ) and theta = pi/2 then the following results
   for r will be obtained

           v                           r
      -------------             ----------------
      ( 1, 2, 3 )                (  3, 2, -1 )
      ( 1, 0, 0 )                (  0, 0, -1 )
      ( 0, 1, 0 )                (  0, 1,  0 )


   If axis = ( 1, 1, 1 ) and theta = pi/2 then the following results
   for r will be obtained

           v                                     r
      -----------------------------      -----------------------------
      ( 1.0,     2.0,     3.0     )      ( 2.577.., 0.845.., 2.577.. )
      ( 2.577.., 0.845.., 2.577.. )      ( 3.0      2.0,     1.0     )
      ( 3.0      2.0,     1.0     )      ( 1.422.., 3.154.., 1.422.. )
      ( 1.422.., 3.154.., 1.422.. )      ( 1.0      2.0,     3.0     )

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   H.A. Neilan         (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.2, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 05-FEB-2003 (NJB)

       Header examples were corrected. -Exceptions section filled in.
       Miscellaneous header corrections were made.

   -CSPICE Version 1.0.0, 22-OCT-1998 (NJB) (HAN) (WLT)

-Index_Entries

   vector rotation about an axis

-&
*/

{ /* Begin vrotv_c */


   vrotv_ ( ( doublereal * ) v,
            ( doublereal * ) axis,
            ( doublereal * ) &theta,
            ( doublereal * ) r      );


} /* End vrotv_c */
