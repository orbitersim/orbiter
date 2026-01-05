/*

-Procedure appndd_c ( Append an item to a double precision cell )

-Abstract

   Append an item to a double precision cell.

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

   CELLS

-Keywords

   CELLS

*/

#include "SpiceUsr.h"
#include "SpiceZmc.h"


   void appndd_c ( SpiceDouble     item,
                   SpiceCell     * cell )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   item       I   The item to append.
   cell      I-O  The cell to which item will be appended.

-Detailed_Input

   item        is a double precision value which is to be appended to
               `cell'.

   cell        is a double precision SPICE cell to which `item' will be
               appended.

               `cell' must be declared as a double precision SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICEDOUBLE_CELL        ( cell, CELLSZ );

               where CELLSZ is the maximum capacity of `cell'.

-Detailed_Output

   cell        is the input cell with `item' appended. `item' is the last
               member of `cell'.

               If `cell' is actually a SPICE set on input and ceases to
               qualify as a set as result of the requested append
               operation, the `isSet' member of `cell' will be set to
               SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If the input cell argument doesn't have double precision data type,
       the error SPICE(TYPEMISMATCH) is signaled.

   2)  If the cell is not big enough to accommodate the addition
       of a new element, the error SPICE(CELLTOOSMALL) is signaled.

-Files

   None.

-Particulars

   None.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a cell for fifteen elements, add a first element to
      it and then append several more integer numbers. Validate
      the cell into a set and print the result. Finally, append
      another integer number. After each operation, check if the
      cell constitutes a set or not.


      Example code begins here.


      /.
         Program appndd_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define LISTSZ       9
         #define SETDIM       15

         /.
         Local variables.
         ./
         SPICEDOUBLE_CELL   ( a     , SETDIM );
         SpiceInt             i;

         /.
         Set the list of double precision numbers to be appended
         to the cell.
         ./
         SpiceDouble          items  [LISTSZ] = { 3.0,  1.0,  1.0,
                                                  2.0,  5.0,  8.0,
                                                 21.0, 13.0, 34.0  };

         /.
         Add a single item to the new cell.
         ./
         appndd_c ( 0, &a );

         /.
         Now insert a list of items.
         ./
         for ( i = 0; i < LISTSZ; i++ )
         {
            appndd_c ( items[i], &a );
         }

         /.
         Output the original contents of cell A.
         ./
         printf( "Items in original set A:\n" );

         for ( i = 0; i < card_c( &a ); i++ )
         {
            printf( "%6.1f", SPICE_CELL_ELEM_D( &a, i ) );
         }

         printf( " \n" );

         /.
         After the append, does the cell key as a set?
         ./
         if ( a.isSet )
         {
            printf( "Cell is a set after first append\n" );
         }
         else
         {
            printf ( "Cell is not a set after first append\n" );
         }

         /.
         Initialize the set by validating the cell: remove duplicates
         and sort the elements in increasing order.
         ./
         valid_c ( SETDIM, card_c( &a ), &a );

         printf( " \n" );
         printf( "Items in cell A after valid_c:\n" );

         for ( i = 0; i < card_c( &a ); i++ )
         {
            printf( "%6.1f", SPICE_CELL_ELEM_D( &a, i ) );
         }

         printf( " \n" );

         /.
         After the append, does the cell key as a set?
         ./
         if ( a.isSet )
         {
            printf( "Cell is a set after valid_c\n" );
         }
         else
         {
            printf ( "Cell is not a set after valid_c\n" );
         }

         /.
         Append a new value to the cell, the value being less than all
         other set values.
         ./
         appndd_c ( -22, &a );

         printf( " \n" );
         printf( "Items in cell A after second append:\n" );

         for ( i = 0; i < card_c( &a ); i++ )
         {
            printf( "%6.1f", SPICE_CELL_ELEM_D( &a, i ) );
         }

         printf( " \n" );

         /.
         After the append, does the cell key as a set?
         ./
         if ( a.isSet )
         {
            printf( "Cell is a set after second append\n" );
         }
         else
         {
            printf ( "Cell is not a set after second append\n" );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Items in original set A:
         0.0   3.0   1.0   1.0   2.0   5.0   8.0  21.0  13.0  34.0
      Cell is not a set after first append

      Items in cell A after valid_c:
         0.0   1.0   2.0   3.0   5.0   8.0  13.0  21.0  34.0
      Cell is a set after valid_c

      Items in cell A after second append:
         0.0   1.0   2.0   3.0   5.0   8.0  13.0  21.0  34.0 -22.0
      Cell is not a set after second append


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   H.A. Neilan         (JPL)

-Version

   -CSPICE Version 1.0.1, 27-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete code
       example.

       Extended description of argument "cell" in -Detailed_Input to include
       type and preferred declaration method.

   -CSPICE Version 1.0.0, 01-AUG-2002 (NJB) (HAN)

-Index_Entries

   append an item to a d.p. cell

-&
*/

{ /* Begin appndd_c */


   /*
   Use discovery check-in.
   */

   /*
   Make sure we're working with a DP cell.
   */
   CELLTYPECHK ( CHK_DISCOVER, "appndd_c", SPICE_DP, cell );


   if ( cell->card == cell->size )
   {
      chkin_c  ( "appndd_c"                                        );
      setmsg_c ( "The cell cannot accommodate the addition of the "
                 "element *"                                       );
      errdp_c  ( "*", item                                         );
      sigerr_c ( "SPICE(CELLTOOSMALL)"                             );
      chkout_c ( "appndd_c"                                        );
      return;
   }


   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( cell );


   /*
   The item must be strictly greater than its predecessor, or
   the input cell is no longer a set.
   */
   if (  ( cell->isSet ) && ( cell->card > 0 )  )
   {
      if (  item  <=  SPICE_CELL_ELEM_D(cell, cell->card-1)  )
      {
         cell->isSet = SPICEFALSE;
      }
   }


   /*
   Append the item to the cell and increment the cell's cardinality.
   */
   SPICE_CELL_SET_D ( item, cell->card, cell );

   (cell->card) ++;


   /*
   Sync the cell.
   */
   zzsynccl_c ( C2F, cell );


} /* End appndd_c */
