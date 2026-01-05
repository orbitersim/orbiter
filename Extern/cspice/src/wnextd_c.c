/*

-Procedure wnextd_c ( Extract the endpoints from a DP window )

-Abstract

   Extract the left or right endpoints from a double precision
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


   void wnextd_c ( SpiceChar     side,
                   SpiceCell   * window )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   side       I   Extract left ('L') or right ('R') endpoints.
   window    I-O  Window to be extracted.

-Detailed_Input

   side        indicates whether the left or right endpoints of the
               intervals in the window are to be extracted.

                  'L', 'l'       Left endpoints.
                  'R', 'r'       Right endpoints.

               If `side' is not recognized, the input window is not
               changed.

   window      on input, is a window containing zero or more intervals.

               `window' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( window, WINDOWSZ );

               where WINDOWSZ is the maximum capacity of `window'.

-Detailed_Output

   window      on output, is the collection of singleton intervals
               containing either the left or the right endpoints of the
               intervals in the original window.

-Parameters

   None.

-Exceptions

   1)  If the endpoint specification, `side', is not recognized, the
       error SPICE(INVALIDENDPNTSPEC) is signaled by a routine in the
       call tree of this routine.

   2)  The cardinality of the input `window' must be even. Left
       endpoints of stored intervals must be strictly greater than
       preceding right endpoints. Right endpoints must be greater
       than or equal to corresponding left endpoints. Invalid window
       data are not diagnosed by this routine and may lead to
       unpredictable results.

   3)  If the `window' cell argument has a type other than
       SpiceDouble, the error SPICE(TYPEMISMATCH) is signaled.

-Files

   None.

-Particulars

   This routine replaces every interval in the input window with
   the singleton interval containing one of the endpoints of the
   interval.

-Examples

   Let window contain the intervals

      [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]

   Then the call

      wnextd_c (  'L', &window );

   produces the window

      [ 1, 1 ]  [ 7, 7 ]  [ 23, 23 ]  [ 29, 29 ]

   And the call

      wnextd_c ( 'R', &window );

   produces the window

      [ 3, 3 ]  [ 11, 11 ]  [ 27, 27 ]  [ 29, 29 ]

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

       Added entry #2 in -Exceptions section.

   -CSPICE Version 1.0.0, 29-JUL-2002 (NJB) (HAN) (WLT) (IMU)

-Index_Entries

   extract the endpoints from a d.p. window

-&
*/

{ /* Begin wnextd_c */


   /*
   Participate in error tracing.
   */
   if ( failed_c() )
   {
      return;
   }
   chkin_c ( "wnextd_c" );


   /*
   Make sure data type is d.p.
   */
   CELLTYPECHK ( CHK_STANDARD, "wnextd_c", SPICE_DP, window );


   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( window );


   wnextd_ ( ( char       * ) &side,
             ( doublereal * ) window->base,
             ( ftnlen       ) 1             );


   /*
   Note:  we don't sync the cell because the size and cardinality
   are unchanged.
   */

   chkout_c ( "wnextd_c" );

} /* End wnextd_c */
