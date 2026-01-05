/*

-Procedure insrtc_c ( Insert an item into a character set )

-Abstract

   Insert an item into a character set.

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

   SETS

-Keywords

   CELLS
   SETS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "f2cMang.h"

   void insrtc_c ( ConstSpiceChar  * item,
                   SpiceCell       * a    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   item       I   Item to be inserted.
   a         I-O  Insertion set.

-Detailed_Input

   item        is an item which is to be inserted into the specified
               set. `item' may or may not already be an element of the
               set.

               If `item' is longer than the declared maximum length of the
               set's elements, the string will be truncated on the right
               when it is inserted. Trailing blanks in `item' are not
               significant.

   a           is a SPICE set.

               On input, `a' may or may not contain the input item as an
               element.

               `a' must be declared as a character SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICECHAR_CELL          ( a, ASZ, AMLEN );

               where ASZ is the maximum capacity of `a' and AMLEN is the
               maximum length of any member in the character cell.

-Detailed_Output

   a           on output, contains the union of the input set and the
               singleton set containing the input item, unless there was
               not sufficient room in the set for the item to be
               included, in which case the set is not changed and an
               error is signaled.

-Parameters

   None.

-Exceptions

   1)  If the insertion of the item into the set causes an excess of
       elements, the error SPICE(SETEXCESS) is signaled.

   2)  If the item to be inserted has greater length than the string
       length of the elements of the set, the item will be truncated
       on the right when it is inserted. The insertion point of the
       element will be determined by the comparison of the truncated
       item to members of the set. If, after truncation, the item to
       be inserted matches an element already present in the set, no
       insertion occurs.

   3)  If the `item' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `a' cell argument has a type other than SpiceChar, the
       error SPICE(TYPEMISMATCH) is signaled.

   5)  If the `a' cell argument does not qualify as a SPICE set, the
       error SPICE(NOTASET) is signaled. SPICE sets have their data
       elements stored in increasing order and contain no duplicate
       elements.

-Files

   None.

-Particulars

   None.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a set with all the original planets of the Solar
      System and then remove Pluto from that set.

      Example code begins here.


      /.
         Program insrtc_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define PNAMSZ       8
         #define SETDIM       9

         /.
         Local variables.
         ./
         SPICECHAR_CELL     ( plnets, SETDIM, PNAMSZ );
         SpiceInt             i;

         /.
         Create the original planets list.
         ./
         SpiceChar            list   [SETDIM][PNAMSZ] = {
                                          "MERCURY", "VENUS",   "EARTH",
                                          "MARS",    "JUPITER", "SATURN",
                                          "URANUS",  "NEPTUNE", "PLUTO"  };

         /.
         Initialize the empty set.
         ./
         valid_c ( SETDIM, 0, &plnets );

         /.
         Insert the list of planets into the set. If the item is
         an element of the set, the set is not changed.
         ./
         for ( i = 0; i < SETDIM; i++ )
         {
            insrtc_c ( list[i], &plnets );
         }

         /.
         Remove the Pluto from the set. If the Pluto is not an
         element of the set, the set is not changed.
         ./
         removc_c ( "PLUTO", &plnets );

         /.
         Output the contents of `plnets'.
         ./
         printf( "Planets of the Solar System:\n" );

         for ( i = 0; i < card_c( &plnets ); i++ )
         {
            printf( "   %s\n", SPICE_CELL_ELEM_C( &plnets, i ) );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Planets of the Solar System:
         EARTH
         JUPITER
         MARS
         MERCURY
         NEPTUNE
         SATURN
         URANUS
         VENUS


-Restrictions

   1)  String comparisons performed by this routine are Fortran-style:
       trailing blanks in the input set or key value are ignored.
       This gives consistent behavior with CSPICE code generated by
       the f2c translator, as well as with the Fortran SPICE Toolkit.

       Note that this behavior is not identical to that of the ANSI
       C library functions strcmp and strncmp.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   C.A. Curzon         (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 2.2.0, 24-AUG-2021 (JDR)

       Changed the argument name "set" to "a" for consistency with other
       routines.

       Edited the header to comply with NAIF standard. Added complete code
       example.

       Extended description of argument "a" in -Detailed_Input to include
       type and preferred declaration method.

       Added entry #5 to -Exceptions section.

   -CSPICE Version 2.1.0, 07-MAR-2009 (NJB)

       This file now includes the header file f2cMang.h.
       This header supports name mangling of f2c library
       functions.

   -CSPICE Version 2.0.0, 01-NOV-2005 (NJB)

       Bug fix: when the item to be inserted would, after
       truncation to the set's string length, match an item
       already in the set, no insertion is performed. Previously
       the truncated string was inserted, corrupting the set.

       Long error message was updated to include size of
       set into which insertion was attempted.

   -CSPICE Version 1.0.0, 21-AUG-2002 (NJB) (CAC) (WLT) (IMU)

-Index_Entries

   insert an item into a character set

-&
*/

{
   /*
   f2c library utility prototypes
   */
   extern integer   s_cmp  (char *sa, char *sb, ftnlen lsa, ftnlen lsb );


   /*
   Local macros
   */
   #define ARRAY( i )    (  (SpiceChar *)(a->data) + (i)*(a->length)  )


   /*
   local variables
   */
   SpiceBoolean            inSet;

   SpiceChar             * cdata;

   SpiceInt                i;
   SpiceInt                loc;
   SpiceInt                slen;



   /*
   Use discovery check-in.

   Check the input string pointer to make sure it's not null.
   */
   CHKPTR ( CHK_DISCOVER, "insrtc_c", item );


   /*
   Make sure we're working with a character cell.
   */
   CELLTYPECHK ( CHK_DISCOVER, "insrtc_c", SPICE_CHR, a );


   /*
   Make sure the input cell is a set.
   */
   CELLISSETCHK ( CHK_DISCOVER, "insrtc_c", a );


   /*
   Initialize the set if it's not already initialized.
   */
   CELLINIT ( a );


   /*
   Let slen be the effective string length of the input item.
   Characters beyond the string length of the set are ignored.
   */
   slen = mini_c ( 2, a->length, strlen(item) );


   /*
   Is the item already in the set? If not, it needs to be inserted.
   */
   cdata =  (SpiceChar *) (a->data);

   /*
   The following call will give the location of the last element
   less than or equal to the item to be inserted.  If the item
   differs from an element of the set only in characters that would
   be truncated, no insertion will occur.  Even in this case, the
   insertion point `loc' returned by lstlec_c will be correct.
   */
   loc   =  lstlec_c ( item,  a->card,  a->length,  cdata );

   inSet =     (  loc  >  -1  )

            && (  s_cmp( (SpiceChar *)item,  ARRAY(loc),
                          slen,              strlen(ARRAY(loc)) ) == 0  );

   if ( inSet )
   {
      return;
   }


   /*
   It's an error if the set has no room left.
   */
   if ( a->card == a->size )
   {
      chkin_c  ( "insrtc_c"                                       );
      setmsg_c ( "An element could not be inserted into the set "
                 "due to lack of space; set size is #."           );
      errint_c ( "#", a->size                                     );
      sigerr_c ( "SPICE(SETEXCESS)"                               );
      chkout_c ( "insrtc_c"                                       );
      return;
   }


   /*
   Make room by moving the items that come after index loc in the set.
   Insert the item after index loc.
   */
   for (  i = (a->card);   i > (loc+1);   i--  )
   {
      SPICE_CELL_SET_C( ARRAY(i-1), i, a );
   }

   /*
   This insertion macro will truncate the item to be inserted, if
   necessary.  The input item will be null-terminated.
   */
   SPICE_CELL_SET_C( item, loc+1, a );


   /*
   Increment the set's cardinality.
   */
   (a->card) ++;

}
