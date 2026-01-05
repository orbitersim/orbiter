/*

-Procedure wnfild_c ( Fill small gaps in a DP window )

-Abstract

   Fill small gaps between adjacent intervals of a double precision
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


   void wnfild_c ( SpiceDouble     smlgap,
                   SpiceCell     * window )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   smlgap     I   Limiting measure of small gaps.
   window    I-O  Window to be filled.

-Detailed_Input

   smlgap      is the limiting measure of the small gaps to be filled.
               Adjacent intervals separated by gaps of measure less than
               or equal to `smlgap' are merged. The measure `smlgap' is
               signed, and is used as is---the absolute value of `smlgap'
               is not used for in place of negative input values.

   window      on input, is a window containing zero or more
               intervals.

               `window' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( window, WINDOWSZ );

               where WINDOWSZ is the maximum capacity of `window'.

-Detailed_Output

   window      on output, is the original window, after adjacent
               intervals separated by small gaps have been merged.

-Parameters

   None.

-Exceptions

   1)  The cardinality of the input `window' must be even. Left
       endpoints of stored intervals must be strictly greater than
       preceding right endpoints. Right endpoints must be greater
       than or equal to corresponding left endpoints. Invalid window
       data are not diagnosed by this routine and may lead to
       unpredictable results.

   2)  If `smlgap' is less than or equal to zero, this routine has
       no effect on the window.

   3)  If the `window' cell argument has a type other than
       SpiceDouble, the error SPICE(TYPEMISMATCH) is signaled.

-Files

   None.

-Particulars

   This routine removes small gaps between adjacent intervals
   by merging intervals separated by gaps of measure less than
   or equal to the limiting measure `smlgap'.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) Given a double precision window, containing the following four
      intervals:

         [ 1.0, 3.0 ], [ 7.0, 11.0 ], [ 23.0, 27.0 ], [ 29.0, 29.0 ]

      merge any adjacent intervals separated by a gap equal to or less
      than 3.0.


      Example code begins here.


      /.
         Program wnfild_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local parameters.
         ./
         #define WNSIZE           10

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

            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]

         into `window'.
         ./
         wninsd_c (  1.0,   3.0, &window );
         wninsd_c (  7.0,  11.0, &window );
         wninsd_c ( 23.0,  27.0, &window );
         wninsd_c ( 29.0,  29.0, &window );

         /.
         Loop over the number of intervals in `window', output
         the `left' and `right' endpoints for each interval.
         ./
         printf( "Initial window:\n" );
         for ( i=0; i<wncard_c(&window); i++)
         {
            wnfetd_c( &window, i, &left, &right );
            printf("   Interval %d [%8.3f, %8.3f]\n", i, left, right );
         }

         /.
         Fill the gaps smaller than or equal to 3.0
         ./
         wnfild_c ( 3.0, &window );

         /.
         Output the intervals.
         ./
         printf( "\nWindow after filling gaps <= 3.0:\n" );
         for ( i=0; i<wncard_c(&window); i++)
         {
            wnfetd_c( &window, i, &left, &right );
            printf("   Interval %d [%8.3f, %8.3f]\n", i, left, right );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Initial window:
         Interval 0 [   1.000,    3.000]
         Interval 1 [   7.000,   11.000]
         Interval 2 [  23.000,   27.000]
         Interval 3 [  29.000,   29.000]

      Window after filling gaps <= 3.0:
         Interval 0 [   1.000,    3.000]
         Interval 1 [   7.000,   11.000]
         Interval 2 [  23.000,   29.000]


   2) Using the same window from the first example:

         [ 1.0, 3.0 ]  [ 7.0, 11.0 ]  [ 23.0, 27.0 ]  [ 29.0, 29.0 ]

      Then the following series of calls

         wnfild_c (  1.0, &window );                             (1)
         wnfild_c (  2.0, &window );                             (2)
         wnfild_c (  3.0, &window );                             (3)
         wnfild_c ( 12.0, &window );                             (4)

      produces the following series of windows

         [1.0,  3.0]  [7.0, 11.0]  [23.0, 27.0]  [29.0, 29.0]    (1)
         [1.0,  3.0]  [7.0, 11.0]  [23.0, 29.0]                  (2)
         [1.0,  3.0]  [7.0, 11.0]  [23.0, 29.0]                  (3)
         [1.0, 29.0]                                             (4)

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
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 10-AUG-2021 (JDR)

       Changed input argument name "small" to "smlgap" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added complete code
       example based on existing example.

       Extended description of argument "window" in -Detailed_Input to include
       type and preferred declaration method.

       Added entry #1 in -Exceptions section.

   -CSPICE Version 1.0.1, 27-JUL-2007 (EDW)

       Changed gap size in -Examples (4) from 10 to 12 to correct
       erroneous example.

   -CSPICE Version 1.0.0, 29-JUL-2002 (NJB) (HAN) (WLT) (IMU)

-Index_Entries

   fill small gaps in a d.p. window

-&
*/

{ /* Begin wnfild_c */


   /*
   Use discovery check-in.

   Make sure cell data type is d.p.
   */
   CELLTYPECHK ( CHK_DISCOVER, "wnfild_c", SPICE_DP, window );


   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( window );


   /*
   Let the f2c'd routine do the work.
   */
   wnfild_ ( (doublereal * )  &smlgap,
             (doublereal * )  (window->base) );

   /*
   Sync the output cell.
   */
   zzsynccl_c ( F2C, window );


} /* End wnfild_c */
