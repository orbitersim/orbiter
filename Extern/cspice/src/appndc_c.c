/*

-Procedure appndc_c ( Append an item to a character cell )

-Abstract

   Append an item to a character cell.

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
#include "SpiceZfc.h"
#include "SpiceZmc.h"
#include "f2cMang.h"


   void appndc_c ( ConstSpiceChar   * item,
                   SpiceCell        * cell  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   item       I   The item to append.
   cell      I-O  The cell to which item will be appended.

-Detailed_Input

   item        is a character string which is to be appended to `cell'.

   cell        is a character SPICE cell to which `item' will be
               appended.

               `cell' must be declared as a character SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICECHAR_CELL          ( cell, CELLSZ, CELLMLEN );

               where CELLSZ is the maximum capacity of `cell' and CELLMLEN is
               the maximum length of any member in the character cell.

-Detailed_Output

   cell        is the input cell with `item' appended. `item' is the last
               member of `cell'.

               If `cell' is actually a SPICE set on input and ceases to
               qualify as a set as result of the append operation,
               the `isSet' member of `cell' will be set to SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If the input cell argument is a SpiceCell of type other than
       character, the error SPICE(TYPEMISMATCH) is signaled.

   2)  If the cell is not large enough to accommodate the addition
       of a new element, the error SPICE(CELLTOOSMALL) is signaled.

   3)  If the length of the item is longer than the length of the
       cell, `item' is truncated on the right.

   4)  If on input cell is actually a SPICE set, that is, it contains sorted
       elements with no duplicates, and if item is not strictly greater than
       the last element, on output the `isSet' member of cell will be set to
       SPICEFALSE. This case is not considered an error.

   5)  If the input string pointer is null, the error SPICE(NULLPOINTER)
       is signaled.

-Files

   None.

-Particulars

   None.

-Examples

   1) In the following example, the item "PLUTO" is appended to
      the character cell planets. planets is declared with
      string length NAMLEN.

         #include "SpiceUsr.h"
                .
                .
                .
         /.
         Declare the cell with string length NAMLEN and with maximum
         number of elements MAXSIZ.
         ./
         SPICECHAR_CELL ( planets, MAXSIZ, NAMLEN );
                .
                .
                .
         /.
         Before appending "PLUTO", suppose the cell planets' data array
         contains:

            Element 0: == "MERCURY"
            Element 1: == "VENUS"
            Element 2: == "EARTH"
            Element 3: == "MARS"
            Element 4: == "JUPITER"
            Element 5: == "SATURN"
            Element 6: == "URANUS"
            Element 7: == "NEPTUNE"

         Append the string "PLUTO" at index 8, and update the
         cell's cardinality.
         ./

         appndc_c ( "PLUTO", &planets );

         /.
         The cell's data array now has the contents

            Element 0: == "MERCURY"
            Element 1: == "VENUS"
            Element 2: == "EARTH"
            Element 3: == "MARS"
            Element 4: == "JUPITER"
            Element 5: == "SATURN"
            Element 6: == "URANUS"
            Element 7: == "NEPTUNE"
            Element 8: == "PLUTO"
         ./

-Restrictions

   1)  String comparisons performed by this routine are Fortran-style:
       trailing blanks in the input array or key value are ignored.
       This gives consistent behavior with CSPICE code generated by
       the f2c translator, as well as with the Fortran SPICE Toolkit.

       Note that this behavior is not identical to that of the ANSI
       C library functions strcmp and strncmp.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   H.A. Neilan         (JPL)

-Version

   -CSPICE Version 1.1.1, 27-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Extended description of argument "cell" in -Detailed_Input to include
       type and preferred declaration method.

   -CSPICE Version 1.1.0, 07-MAR-2009 (NJB)

       This file now includes the header file f2cMang.h.
       This header supports name mangling of f2c library
       functions.

       Header sections were re-ordered.

   -CSPICE Version 1.0.0, 21-AUG-2002 (NJB) (HAN)

-Index_Entries

   append an item to a character cell

-&
*/

{ /* Begin appndc_c */


   /*
   f2c library utility prototypes
   */
   extern integer   s_cmp  (char *a, char *b, ftnlen la, ftnlen lb );


   /*
   Local variables
   */
   SpiceChar             * sPtr;

   SpiceInt                card;
   SpiceInt                diff;


   /*
   Use discovery check-in.
   */
   if ( return_c() )
   {
      return;
   }


   /*
   Check the input string pointer to make sure it's not null.
   */
   CHKPTR ( CHK_DISCOVER, "appndc_c", item );


   /*
   Make sure we're working with a character cell.
   */
   CELLTYPECHK ( CHK_DISCOVER, "appndc_c", SPICE_CHR, cell );


   /*
   Initialize the cell if necessary.
   */
   CELLINIT ( cell );


   card = cell->card;

   if ( card == cell->size )
   {
      chkin_c  ( "appndc_c"                                        );
      setmsg_c ( "The cell cannot accommodate the addition of the "
                 "element *"                                       );
      errch_c  ( "*", item                                         );
      sigerr_c ( "SPICE(CELLTOOSMALL)"                             );
      chkout_c ( "appndc_c"                                        );
      return;
   }


   if (  ( cell->isSet ) && ( card > 0 )  )
   {
      /*
      The item must be strictly greater than its predecessor, or
      the input cell is no longer a set.
      */
      sPtr = SPICE_CELL_ELEM_C(cell, card-1 );

      diff = s_cmp ( (char     *) item,
                     (char     *) sPtr,
                     (ftnlen    ) strlen(item),
                     (ftnlen    ) strlen(sPtr)  );

      if ( diff < 1 )
      {
         cell->isSet = SPICEFALSE;
      }
   }


   /*
   Append the item to the cell and increment the cell's cardinality.
   */
   SPICE_CELL_SET_C ( item, card, cell );

   (cell->card) ++;


} /* End appndc_c */
