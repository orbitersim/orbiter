/*

-Procedure elemc_c ( Element of a character set )

-Abstract

   Determine whether an item is an element of a character set.

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

   SpiceBoolean elemc_c ( ConstSpiceChar  * item,
                          SpiceCell       * a    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   item       I   Item to be tested.
   a          I   Set to be tested.

   The function returns SPICETRUE if `item' is an element of `a'.

-Detailed_Input

   item        is an item which may or may not be an element of the
               input set. Trailing blanks in `item' are not significant.

   a           is a SPICE set. Trailing blanks in the members of `a'
               are not significant.

               `a' must be declared as a character SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICECHAR_CELL          ( a, ASZ, AMLEN );

               where ASZ is the maximum capacity of `a' and AMLEN is the
               maximum length of any member in the character cell.

-Detailed_Output

   The function returns SPICETRUE if `item' is a member of the set `a',
   and returns SPICEFALSE otherwise.

   The comparison between `item' and members of `a' is case-sensitive.
   Trailing blanks are ignored.

-Parameters

   None.

-Exceptions

   1)  If the `item' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the value
       SPICEFALSE.

   2)  If the `a' cell argument has a type other than SpiceChar, the
       error SPICE(TYPEMISMATCH) is signaled. The function returns
       the value SPICEFALSE.

   3)  If the `a' cell argument does not qualify as a SPICE set, the
       error SPICE(NOTASET) is signaled. SPICE sets have their data
       elements stored in increasing order and contain no duplicate
       elements. The function returns the value SPICEFALSE.

-Files

   None.

-Particulars

   The functions

      elemc_c
      elemd_c
      elemi_c

   provide a convenient shorthand notation for a binary search
   on a set's data array for the item of interest.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1)Check if the elements of a list of body names belong to the
     Solar System planets set.


      Example code begins here.


      /.
         Program elemc_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define LISTSZ       6
         #define PNAMSZ       8
         #define SETDIM       9

         /.
         Local variables.
         ./
         SPICECHAR_CELL     ( plnets, SETDIM, PNAMSZ );
         SpiceInt             i;

         /.
         Create the original planets set and a list of body
         names.
         ./
         SpiceChar            values [SETDIM][PNAMSZ] = {
                                             "MERCURY", "VENUS",   "EARTH",
                                             "MARS",    "JUPITER", "SATURN",
                                             "URANUS",  "NEPTUNE", "PLUTO"  };

         SpiceChar            items  [LISTSZ][PNAMSZ] = {
                                                  "EARTH", "APOLLO", "MARS",
                                                  "PLUTO", "VENUS", "CERES" };

         /.
         Add the values to the integer set using appndc_c. The
         results of these calls results in a cell which does not
         qualify as a set.
         ./
         for ( i = 0; i < SETDIM; i++ )
         {
            appndc_c ( values[i], &plnets );
         }

         /.
         Initialize the set by validating the cell: remove duplicates
         and sort the elements in increasing order.
         ./
         valid_c ( SETDIM, SETDIM, &plnets );

         /.
         Check if the items in the list belong to the set.
         ./
         for ( i = 0; i < LISTSZ; i++ )
         {
            if ( elemc_c ( items[i], &plnets ) )
            {
               printf( "Item %-8s is in the set.\n", items[i] );
            }
            else
            {
               printf( "Item %-8s is NOT in the set.\n", items[i] );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Item EARTH    is in the set.
      Item APOLLO   is NOT in the set.
      Item MARS     is in the set.
      Item PLUTO    is in the set.
      Item VENUS    is in the set.
      Item CERES    is NOT in the set.


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
   C.A. Curzon         (JPL)
   J. Diaz del Rio     (ODC Space)
   H.A. Neilan         (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.1.0, 24-AUG-2021 (JDR)

       Changed the argument name "set" to "a" for consistency with other
       routines.

       Edited the header to comply with NAIF standard. Added complete code
       example.

       Cast returned value type to SpiceBoolean.

       Extended description of argument "a" in -Detailed_Input to include
       type and preferred declaration method.

   -CSPICE Version 1.0.0, 21-AUG-2002 (NJB) (CAC) (HAN) (WLT) (IMU)

-Index_Entries

   element of a character set

-&
*/

{ /* Begin elemc_c */


   /*
   Use discovery check-in.

   Check the input string pointer to make sure it's not null.
   */
   CHKPTR_VAL ( CHK_DISCOVER, "elemc_c", item, SPICEFALSE );


   /*
   Make sure we're working with a character cell.
   */
   CELLTYPECHK_VAL ( CHK_DISCOVER, "elemc_c", SPICE_CHR, a, SPICEFALSE );


   /*
   Make sure the cell is really a set.
   */
   CELLISSETCHK_VAL ( CHK_DISCOVER, "elemc_c", a, SPICEFALSE );


   /*
   Initialize the set if necessary.
   */
   CELLINIT ( a );


   /*
   The routine bsrchc_c returns the index of the item in the set,
   or -1 if the item is not present.
   */
   return (  ( SpiceBoolean )
             ( bsrchc_c ( item,       a->card,
                          a->length,  a->data )   !=   -1   )  );

} /* End elemc_c */
