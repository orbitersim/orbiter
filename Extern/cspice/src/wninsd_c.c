/*

-Procedure wninsd_c ( Insert an interval into a DP window )

-Abstract

   Insert an interval into a double precision window.

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

   void wninsd_c ( SpiceDouble     left,
                   SpiceDouble     right,
                   SpiceCell     * window )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   left,
   right      I   Left, right endpoints of new interval.
   window    I-O  Input, output window.

-Detailed_Input

   left,
   right       are the left and right endpoints of the interval to be
               inserted.

   window      on input, is a SPICE window containing zero or more
               intervals.

               `window' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( window, WINDOWSZ );

               where WINDOWSZ is the maximum capacity of `window'.

-Detailed_Output

   window      on output, is the original window following the insertion
               of the interval from `left' to `right'.

-Parameters

   None.

-Exceptions

   1)  If `left' is greater than `right', the error SPICE(BADENDPOINTS)
       is signaled by a routine in the call tree of this routine.

   2)  If the insertion of the interval causes an excess of elements,
       the error SPICE(WINDOWEXCESS) is signaled by a routine in the
       call tree of this routine.

   3)  The cardinality of the input `window' must be even. Left
       endpoints of stored intervals must be strictly greater than
       preceding right endpoints. Right endpoints must be greater
       than or equal to corresponding left endpoints. Invalid window
       data are not diagnosed by this routine and may lead to
       unpredictable results.

   4)  If the `window' cell argument has a type other than
       SpiceDouble, the error SPICE(TYPEMISMATCH) is signaled.

-Files

   None.

-Particulars

   This routine inserts the interval from `left' to `right' into the
   input window. If the new interval overlaps any of the intervals
   in the window, the intervals are merged. Thus, the cardinality
   of the input window can actually decrease as the result of an
   insertion. However, because inserting an interval that is
   disjoint from the other intervals in the window can increase the
   cardinality of the window, the routine signals an error.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) The following code example demonstrates how to insert an
      interval into an existing double precision SPICE window, and
      how to loop over all its intervals to extract their left and
      right points.


      Example code begins here.


      /.
         Program wninsd_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local parameters.
         ./
         #define WNSIZE          10

         /.
         Local variables.
         ./
         SpiceDouble              left;
         SpiceDouble              right;

         SPICEDOUBLE_CELL ( window,  WNSIZE );

         SpiceInt                 i;

         /.
         Validate the window with size WNSIZE and zero elements.
         ./
         wnvald_c ( WNSIZE, 0, &window );

         /.
         Insert the intervals

            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]

         into `window'.
         ./
         wninsd_c (  1.0,   3.0, &window );
         wninsd_c (  7.0,  11.0, &window );
         wninsd_c ( 23.0,  27.0, &window );

         /.
         Loop over the number of intervals in `window', output
         the `left' and `right' endpoints for each interval.
         ./
         for ( i=0; i<wncard_c(&window); i++)
         {
            wnfetd_c( &window, i, &left, &right );
            printf("Interval %d [%8.3f, %8.3f]\n", i, left, right );
         }

         return ( 0 );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Interval 0 [   1.000,    3.000]
      Interval 1 [   7.000,   11.000]
      Interval 2 [  23.000,   27.000]


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   H.A. Neilan         (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 25-AUG-2021 (JDR)

       Edited the header to comply to NAIF standard. Added complete code
       example, problem statement and solution.

       Extended description of argument "window" in -Detailed_Input to include
       type and preferred declaration method.

       Added entry #3 in -Exceptions section.

       Removed irrelevant information related to other unary window
       routines from -Particulars section.

   -CSPICE Version 1.0.0, 29-JUL-2002 (NJB) (KRG) (HAN) (WLT) (IMU)

-Index_Entries

   insert an interval into a d.p. window

-&
*/

{ /* Begin wninsd_c */


   /*
   Standard SPICE error handling.
   */

   if ( return_c() )
   {
      return;
   }
   chkin_c ( "wninsd_c" );


   /*
   Make sure cell data type is d.p.
   */
   CELLTYPECHK ( CHK_STANDARD, "wninsd_c", SPICE_DP, window );


   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( window );


   /*
   Let the f2c'd routine do the work.
   */
   wninsd_ ( (doublereal * )  &left,
             (doublereal * )  &right,
             (doublereal * )  (window->base) );

   /*
   Sync the output cell.
   */
   if ( !failed_c() )
   {
      zzsynccl_c ( F2C, window );
   }


   chkout_c ( "wninsd_c" );

} /* End wninsd_c */
