/*

-Procedure mequg_c ( Matrix equal to another, general dimension )

-Abstract

   Set one double precision matrix of arbitrary size equal to
   another.

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

   ASSIGNMENT
   MATRIX

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    mequg_c


   void mequg_c ( const void   * m1,
                  SpiceInt       nr,
                  SpiceInt       nc,
                  void         * mout )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   m1         I   Input matrix.
   nr         I   Row dimension of m1 (and also mout).
   nc         I   Column dimension of m1 (and also mout).
   mout       O   Output matrix equal to m1.

-Detailed_Input

   m1          is an arbitrary-sized double precision matrix.
               There are no restrictions on what it may contain.

   nr          is the number of rows in the input matrix.

   nc          is the number of columns in the input matrix.

-Detailed_Output

   mout        This matrix is set to be equal to m1.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If nr < 1 or nc < 1, the elements of the matrix `mout' are not
       assigned any values.

-Files

   None.

-Particulars

   None.

-Examples

   If  m1 = | 1.0   2.0 |
            |           |
            | 2.0   4.0 |
            |           |
            | 4.0   6.0 |

   the call

     mequg_c ( m1, 3, 2, mout )

   produces the matrix

     mout = | 1.0   2.0 |
            |           |
            | 2.0   4.0 |
            |           |
            | 4.0   6.0 |

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.1, 04-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.2.0, 28-AUG-2001 (NJB)

       Const-qualified input array.

   -CSPICE Version 1.0.0, 31-MAR-1998 (EDW)

-Index_Entries

   equal to another n-dimensional matrix

-&
*/

{ /* Begin mequg_c */


   /* Not really that complicated. */

   MOVED ( m1, nr * nc, mout );


} /* End mequg_c */
