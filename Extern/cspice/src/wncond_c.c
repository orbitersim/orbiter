/*

-Procedure wncond_c ( Contract the intervals of a DP window )

-Abstract

   Contract each of the intervals of a double precision window.

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

   void wncond_c ( SpiceDouble     left,
                   SpiceDouble     right,
                   SpiceCell     * window )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   left       I   Amount added to each left endpoint.
   right      I   Amount subtracted from each right endpoint.
   window    I-O  Window to be contracted.

-Detailed_Input

   left        is the amount to be added to the left endpoint of each
               interval in the input window. The amount `left' is signed.

   right       is the amount to be subtracted from the right endpoint of
               each interval in the window. The amount `right' is signed.

   window      on input, is a window containing zero or more
               intervals.

               `window' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( window, WINDOWSZ );

               where WINDOWSZ is the maximum capacity of `window'.

-Detailed_Output

   window      on output, is the original window with each of its
               intervals contracted by `left' units on the left and
               `right' units on the right.

-Parameters

   None.

-Exceptions

   1)  The cardinality of the input `window' must be even. Left
       endpoints of stored intervals must be strictly greater than
       preceding right endpoints. Right endpoints must be greater
       than or equal to corresponding left endpoints. Invalid window
       data are not diagnosed by this routine and may lead to
       unpredictable results.

   2)  If the `window' cell argument has a type other than
       SpiceDouble, the error SPICE(TYPEMISMATCH) is signaled.

-Files

   None.

-Particulars

   This routine contracts (shortens) each of the intervals in
   the input window. The adjustments are not necessarily symmetric.
   That is, left units are added to the left endpoint of each
   interval, and right units are subtracted from the right endpoint
   of each interval, where left and right may be different.

   Intervals are dropped when they are contracted by amounts
   greater than their measures.

-Examples

   Let window contain the intervals

      [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]

   Then the following series of calls

      wncond_c (  2,  1, &window );              (1)
      wncond_c ( -2,  2, &window );              (2)
      wncond_c ( -2, -1, &window );              (3)

   produces the following series of windows

      [ 9, 10 ]  [ 25, 26 ]                      (1)
      [ 7,  8 ]  [ 23, 24 ]                      (2)
      [ 5,  9 ]  [ 21, 25 ]                      (3)

   Note that intervals may be "contracted" by negative amounts.
   In the example above, the second call shifts each interval to
   the left, while the third call undoes the effect of the first
   call (without restoring the destroyed intervals).

   Note also that the third call is exactly equivalent to the
   call

      wnexpd_c ( 2, 1, window );

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

       Added entry #1 in -Exceptions section.

   -CSPICE Version 1.0.0, 21-JUL-2002 (NJB) (HAN) (WLT) (IMU)

-Index_Entries

   contract the intervals of a d.p. window

-&
*/

{ /* Begin wncond_c */

   /*
   Use discovery check-in.

   Make sure cell data type is d.p.
   */
   CELLTYPECHK ( CHK_DISCOVER, "wncond_c", SPICE_DP, window );


   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( window );


   /*
   Let the f2c'd routine do the work.
   */
   wncond_ ( (doublereal * )  &left,
             (doublereal * )  &right,
             (doublereal * )  (window->base) );

   /*
   Sync the output cell.
   */
   zzsynccl_c ( F2C, window );


} /* End wncond_c */
