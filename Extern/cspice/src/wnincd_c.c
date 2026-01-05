/*

-Procedure wnincd_c ( Included in a double precision window )

-Abstract

   Determine whether an interval is included in a double precision
   window.

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

   WINDOWS

-Keywords

   WINDOWS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   SpiceBoolean wnincd_c ( SpiceDouble     left,
                           SpiceDouble     right,
                           SpiceCell     * window )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   left,
   right      I   Input interval.
   window     I   Input window.

   The function returns SPICETRUE if the input interval is included in
   ---is a subset of some interval in---window.

-Detailed_Input

   left,
   right       are the endpoints of an interval, which may or may not be
               contained in one of the intervals in `window'.

   window      is a window containing zero or more intervals.

               `window' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( window, WINDOWSZ );

               where WINDOWSZ is the maximum capacity of `window'.

-Detailed_Output

   The function returns SPICETRUE if the input interval is included in
   the input window --- that is, if

      a(i)  <  left  <  right  <  b(i)
            -        -         -

   for some interval [ a(i), b(i) ] in `window' --- and is SPICEFALSE
   otherwise.

-Parameters

   None.

-Exceptions

   1)  The cardinality of the input `window' must be even. Left
       endpoints of stored intervals must be strictly greater than
       preceding right endpoints. Right endpoints must be greater
       than or equal to corresponding left endpoints. Invalid window
       data are not diagnosed by this routine and may lead to
       unpredictable results.

   2)  The order of the input interval's endpoints, `left' and `right',
       is not checked, and that this does not affect the result.

   3)  If the `window' cell argument has a type other than
       SpiceDouble, the error SPICE(TYPEMISMATCH) is signaled. The
       function returns the value SPICEFALSE.

-Files

   None.

-Particulars

   None.

-Examples

   Let window contain the intervals

      [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]

   Then the following expressions are SPICETRUE

      wnincd_c ( 1.0,  3.0, &window );
      wnincd_c ( 9.0, 10.0, &window );

   and the following expressions are SPICEFALSE.

      wnincd_c (  0.0,  2.0, &window );
      wnincd_c ( 13.0, 15.0, &window );
      wnincd_c ( 29.0, 30.0, &window );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   H.A. Neilan         (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Extended description of argument "window" in -Detailed_Input to include
       type and preferred declaration method.

       Added entries #1 and #2 in -Exceptions section.

   -CSPICE Version 1.0.0, 29-JUL-2002 (NJB) (HAN) (WLT) (IMU)

-Index_Entries

   included in a d.p. window

-&
*/

{ /* Begin wnincd_c */


   /*
   Use discovery check-in.

   Make sure cell data type is d.p.
   */
   CELLTYPECHK_VAL ( CHK_DISCOVER, "wnincd_c", SPICE_DP, window, SPICEFALSE );


   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( window );


   /*
   Let the f2c'd routine do the work.
   */
   return ( (SpiceBoolean) wnincd_ ( (doublereal *) &left,
                                     (doublereal *) &right,
                                     (doublereal *) (window->base) )  );

} /* End wnincd_c */
