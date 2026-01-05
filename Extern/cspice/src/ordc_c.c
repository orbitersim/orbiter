/*

-Procedure ordc_c ( The ordinal position of an element in a set )

-Abstract

   Return the ordinal position of a given item in a set. If the
   item does not appear in the set, return -1.

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

   SEARCH
   SETS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   SpiceInt ordc_c ( ConstSpiceChar  * item,
                     SpiceCell       * set   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   item       I   An item to locate within a set.
   set        I   A set to search for a given item.

   The function returns the ordinal position of item within the set.

-Detailed_Input

   item        is a string to be located within a character set.
               Trailing blanks are not significant in the comparison.

   set         is a properly validated SPICE set that is to be
               searched for the occurrence of `item'. Trailing blanks are
               not significant in the comparison.

               `set' must be declared as a character SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICECHAR_CELL          ( set, SETSZ, SETMLEN );

               where SETSZ is the maximum capacity of `set' and SETMLEN is the
               maximum length of any member in the character cell.

-Detailed_Output

   The function returns the ordinal position of `item' within `set'.
   Ordinal positions range from 0 to N-1, where N is the cardinality
   of the set.

   If `item' is not an element of `set', the function returns -1.

-Parameters

   None.

-Exceptions

   1)  If the input set has invalid cardinality, an error is signaled
       by a routine in the call tree of this routine. ordc_c returns an
       unspecified value in this case.

   2)  If the input set has invalid size, an error is signaled by a
       routine in the call tree of this routine. ordc_c returns an
       unspecified value in this case.

   3)  If the `item' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the value
       -1.

   4)  If the `set' cell argument has a type other than SpiceChar,
       the error SPICE(TYPEMISMATCH) is signaled. The function
       returns the value -1.

   5)  If the `set' cell argument does not qualify as a SPICE set,
       the error SPICE(NOTASET) is signaled. SPICE sets have their
       data elements stored in increasing order and contain no
       duplicate elements. The function returns the value -1.

-Files

   None.

-Particulars

   A natural ordering can be imposed upon the elements of any
   SPICE set, be it integer, character or double precision. For
   character strings the ASCII collating sequence serves as the
   ordering relation, for double precision and integer variables
   the arithmetic ordering is used.

   Given any element of a set, its location within this ordered
   sequence of elements is called its ordinal position within
   the set.

   In common mathematical usage, ordinal positions of elements
   in a set of cardinality N range from 1 to N. In C programs,
   it is much more convenient to use the range 0 to N-1; this is
   the convention used in CSPICE.

   For illustrative purposes suppose that set represents the set

      { "8", "1", "2", "9", "7", "4", "10" }

   The ordinal position of:

       "8" is 5
       "1" is 0
       "2" is 2
       "9" is 6
       "7" is 4
       "4" is 3
      "10" is 1

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Obtain the ordinal positions shown in the table of the -Particulars
      section above.


      Example code begins here.


      /.
         Program ordc_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Declare an integer set and populate it with the elements
         shown above.
         ./
         #define MAXSIZ         7
         #define ITMLEN         10

         SPICECHAR_CELL ( set, MAXSIZ, ITMLEN );

         SpiceChar            * cElt;

         SpiceChar              inputs [MAXSIZ][ITMLEN] =
                                {
                                   "8", "1", "2", "9", "7", "4", "10"
                                };

         SpiceInt               expected [MAXSIZ] =
                                {
                                   5, 0, 2, 6, 4, 3, 1
                                };

         SpiceInt               i;


         /.
         Create the set.
         ./

         for ( i = 0;  i < MAXSIZ;  i++ )
         {
            insrtc_c ( inputs[i], &set );
         }

         /.
         Examine the ordinal positions of the set's elements.
         Extract each element and verify that ordc_c gives the
         index at which the element is located.
         ./
         for ( i = 0;  i < card_c(&set);  i++ )
         {
            cElt = inputs[i];

            if (  ordc_c(cElt, &set)  !=  expected[i]  )
            {
               setmsg_c ( "Position of # was expected to be # "
                          "but was actually #."                 );
               errch_c  ( "#",  cElt                            );
               errint_c ( "#",  expected[i]                     );
               errint_c ( "#",  ordc_c(cElt,&set)               );
               sigerr_c ( "INVALID LOCATION"                    );
            }
            printf( "%2d  is the ordinal position of \"%s\"\n",
                     (int)expected[i], cElt                     );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


       5  is the ordinal position of "8"
       0  is the ordinal position of "1"
       2  is the ordinal position of "2"
       6  is the ordinal position of "9"
       4  is the ordinal position of "7"
       3  is the ordinal position of "4"
       1  is the ordinal position of "10"


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

   -CSPICE Version 1.0.1, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard. Updated the example
       code to generate outputs and provided example's solution.

       Extended description of argument "set" in -Detailed_input to include
       type and preferred declaration method.

       Added entries #1 and #2 to -Exceptions section.

   -CSPICE Version 1.0.0, 21-AUG-2002 (NJB) (CAC) (HAN) (WLT) (IMU)

-Index_Entries

   the ordinal position of an element in a set

-&
*/

{
   /*
   Use discovery check-in.

   Check the input string pointer to make sure it's not null.
   */
   CHKPTR_VAL ( CHK_DISCOVER, "ordc_c", item, -1 );


   /*
   Make sure we're working with a character cell.
   */
   CELLTYPECHK_VAL ( CHK_DISCOVER, "ordc_c", SPICE_CHR, set, -1 );


   /*
   Initialize the set if necessary.
   */
   CELLINIT ( set );

   /*
   Make sure the cell is really a set.
   */
   CELLISSETCHK_VAL ( CHK_DISCOVER, "ordc_c", set, -1 );

   /*
   The routine bsrchc_c returns the index of the item in the set,
   or -1 if the item is not present.
   */
   return (  bsrchc_c ( item,  set->card,  set->length,  set->data )  );
}
