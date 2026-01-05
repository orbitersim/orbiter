/*

-Procedure wncomd_c ( Complement a DP window )

-Abstract

   Determine the complement of a double precision window with
   respect to a specified interval.

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


   void wncomd_c ( SpiceDouble    left,
                   SpiceDouble    right,
                   SpiceCell    * window,
                   SpiceCell    * result )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   left,
   right      I   Left, right endpoints of complement interval.
   window     I   Input window.
   result     O   Complement of window with respect to [left,right].

-Detailed_Input

   left,
   right       are the left and right endpoints of the complement
               interval.

   window      is the window to be complemented.

               `window' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( window, WINDOWSZ );

               where WINDOWSZ is the maximum capacity of `window'.

-Detailed_Output

   result      is the output window, containing the complement
               of `window' with respect to the interval from `left'
               to `right'. If the output window is not large enough
               to contain the result, as many intervals as will
               fit are returned.

               `result' must be distinct from `window'.

               `result' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( result, RESULTSZ );

               where RESULTSZ is the maximum capacity of `result'.

-Parameters

   None.

-Exceptions

   1)  If `left' is greater than `right', the error SPICE(BADENDPOINTS)
       is signaled by a routine in the call tree of this routine.

   2)  The cardinality of the input `window' must be even. Left
       endpoints of stored intervals must be strictly greater than
       preceding right endpoints. Right endpoints must be greater
       than or equal to corresponding left endpoints. Invalid window
       data are not diagnosed by this routine and may lead to
       unpredictable results.

   3)  If any the `window' or `result' cell arguments has a type
       other than SpiceDouble, the error SPICE(TYPEMISMATCH) is
       signaled.

-Files

   None.

-Particulars

   Mathematically, the complement of a window contains those
   points that are not contained in the window. That is, the
   complement of the set of closed intervals

      [ a(0), b(0) ], [ a(1), b(1) ], ..., [ a(n), b(n) ]

   is the set of open intervals

      ( -inf, a(0) ), ( b(0), a(1) ), ..., ( b(n), +inf )

   Because ANSI C offers no satisfactory representation of
   infinity, we must take the complement with respect to a
   finite interval.

   In addition, ANSI C offers no satisfactory floating point
   representation of open intervals. Therefore, the complement
   of a floating point window is closure of the set theoretical
   complement. In short, the floating point complement of the
   window

      [ a(0), b(0) ], [ a(1), b(1) ], ..., [ a(n), b(n) ]

   with respect to the interval from left to right is the
   intersection of the windows

      ( -inf, a(0) ), ( b(0), a(1) ), ..., ( b(n), +inf )

   and

      [ left, right ]

   Note that floating point intervals of measure zero (singleton
   intervals) in the original window are replaced by gaps of
   measure zero, which are filled. Thus, complementing a floating
   point window twice does not necessarily yield the original window.

-Examples

   Let window contain the intervals

      [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]

   Then the floating point complement of window with respect
   to [2,20] contains the intervals

      [ 3, 7 ]  [ 11, 20 ]

   and the complement with respect to [ 0, 100 ] contains

      [ 0, 1 ]  [ 3, 7 ]  [ 11, 23 ]  [ 27, 100 ]

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

       Extended description of argument "window" in -Detailed_Input and
       "result" in -Detailed_Output to include type and preferred declaration
       method.

       Added entry #2 in -Exceptions section.

   -CSPICE Version 1.0.0, 27-AUG-2002 (NJB) (HAN) (WLT) (IMU)

-Index_Entries

   complement a d.p. window

-&
*/

{ /* Begin wncomd_c */



   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "wncomd_c" );


   /*
   Make sure data types are d.p.
   */
   CELLTYPECHK2 ( CHK_STANDARD, "wncomd_c", SPICE_DP, window, result );


   /*
   Initialize the cells if necessary.
   */
   CELLINIT2 ( window, result );

   /*
   Let the f2c'd routine do the work.
   */
   wncomd_ ( (doublereal * )  &left,
             (doublereal * )  &right,
             (doublereal * )  (window->base),
             (doublereal * )  (result->base)  );

   /*
   Sync the output cell.
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, result );
   }

   chkout_c ( "wncomd_c" );

} /* End wncomd_c */
