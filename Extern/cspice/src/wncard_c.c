/*

-Procedure wncard_c ( Cardinality of a double precision window )

-Abstract

   Return the cardinality (number of intervals) of a double
   precision window.

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

   SpiceInt wncard_c ( SpiceCell  * window )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   window     I   Input window

   The function returns the window cardinality of the window.

-Detailed_Input

   window      is a window containing zero or more intervals.

               `window' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( window, WINDOWSZ );

               where WINDOWSZ is the maximum capacity of `window'.

-Detailed_Output

   The function returns the cardinality of (number of intervals in)
   the input window.

-Parameters

   None.

-Exceptions

   1)  If the number of elements in `window' is not even, the error
       SPICE(INVALIDSIZE) is signaled by a routine in the call tree
       of this routine.

   2)  If the `window' cell argument has a type other than
       SpiceDouble, the error SPICE(TYPEMISMATCH) is signaled. The
       function returns the value 0.

-Files

   None.

-Particulars

   This function returns the value of card_c(window)/2.

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
         Program wncard_ex1
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

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 05-AUG-2021 (JDR)

       Edited the header to comply to NAIF standard. Created complete code
       example from code fragment and added example's problem statement.

       Extended description of argument "window" in -Detailed_Input to include
       type and preferred declaration method.

       Completed -Exceptions section.

   -CSPICE Version 1.0.0, 21-AUG-2007 (EDW)

-Index_Entries

   cardinality of a d.p. window

-&
*/

{ /* Begin wncard_c */

   SpiceInt      retval;

   /*
   Use discovery check-in.

   Make sure cell data type is d.p.
   */
   CELLTYPECHK_VAL( CHK_DISCOVER, "wncard_c", SPICE_DP, window, 0 );
   /*
   Initialize the cell if necessary.
   */
   CELLINIT( window );

   retval = wncard_( (doublereal * ) (window->base) );

   return( retval );

} /* End wncard_c */
