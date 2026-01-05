/*

-Procedure lparss_c (Parse a list of items; return a set)

-Abstract

   Parse a list of items separated by multiple delimiters, placing the
   resulting items into a set.

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

   None.

-Keywords

   CHARACTER
   LIST
   PARSING
   STRING

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void lparss_c ( ConstSpiceChar   * list,
                   ConstSpiceChar   * delims,
                   SpiceCell        * set     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   list       I    List of items delimited by delims.
   delims     I    Single characters which delimit items.
   set        O    Set containing items in the list, left justified.

-Detailed_Input

   list        is a list of items delimited by any one of the characters
               in the string `delims'. Consecutive delimiters, and
               delimiters at the beginning and end of the list, are
               considered to delimit empty items. A blank or empty list
               is considered to contain a single, empty item. Leading and
               trailing blanks in list are ignored.

   delims      contains the individual characters which delimit the
               items in the list. These may be any ASCII characters,
               including blanks.

               However, by definition, consecutive blanks are NOT
               considered to be consecutive delimiters. Nor are a blank
               and any other delimiter considered to be consecutive
               delimiters. In addition, leading and trailing blanks are
               ignored.

-Detailed_Output

   set         is a SPICE set containing the items in the list, left
               justified. Any item in the list too long to fit into an
               element of `set' is truncated on the right. Empty or blank
               items in the input string are mapped to empty strings on
               output.

               The strings in `set' will be sorted in increasing order,
               and duplicates will be removed. Trailing blanks are
               ignored in string comparisons.

               The size of the set must be initialized prior to calling
               lparss_c.

               `set' must be declared as a character SpiceCell.

               CSPICE provides the following macro, which declares and
               initializes the cell

                  SPICECHAR_CELL          ( set, SETSZ, SETMLEN );

               where SETSZ is the maximum capacity of `set' and SETMLEN is the
               maximum length of any member in the character cell.

-Parameters

   None.

-Exceptions

   1)  If the size of the set is not large enough to accommodate all
       of the items in the set, an error is signaled by a routine in
       the call tree of this routine.

   2)  If the string length of `set' is too short to accommodate an
       item, the item will be truncated on the right.

   3)  If the string length of `set' is too short to permit encoding of
       integers via the SPICELIB routine ENCHAR, an error is signaled
       by a routine in the call tree of this routine.

   4)  If any of the `list', `delims' or `list' input string pointers
       is null, the error SPICE(NULLPOINTER) is signaled.

   5)  If any of the `list' or `delims' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

   6)  If the `set' cell argument has a type other than SpiceChar,
       the error SPICE(TYPEMISMATCH) is signaled.

   7)  If the string length associated with the argument `set' is
       non-positive or too short to be usable when constructing the
       equivalent SPICE character cell required by the wrapped
       SPICELIB routine, an error is signaled by a routine in the
       call tree of this routine.

   8)  An empty input `list' will result in a single, empty string item
       added to `set'. This case is not an error.

-Files

   None.

-Particulars

   None.

-Examples

   The following examples illustrate the operation of lparss_c.

   1) Let

         list   == "  A number of words   separated   by spaces.   "
         delims == " ,."

      Let set be declared with size 20.

      Then

         Element 0 of set == " "
         Element 1 of set == "A"
         Element 2 of set == "by"
         Element 3 of set == "number"
         Element 4 of set == "of"
         Element 5 of set == "separated"
         Element 6 of set == "spaces"
         Element 7 of set == "words"

    2) Let

          list   == "  1986-187// 13:15:12.184 "
          delims == " ,/-:"
          nmax   == 20

       Then

          Element 0 of set == ""
          Element 1 of set == "12.184"
          Element 2 of set == "13"
          Element 3 of set == "15"
          Element 4 of set == "187"
          Element 5 of set == "1986"

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
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 29-OCT-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Extended description of argument "set" in -Detailed_Output to include
       type and preferred declaration method.

       Added entries #2 and #3 in -Exceptions section.

   -CSPICE Version 1.0.0, 27-AUG-2002 (NJB) (IMU)

-Index_Entries

   parse a list of items

-&
*/

{ /* Begin lparss_c */

   /*
   Local variables
   */
   SpiceChar             * fCell;

   SpiceInt                fLen;


   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "lparss_c" );


   /*
   Special case:  if the input string is empty, return a set
   containing a single empty string.

   We must know that list is not a null pointer first.
   */
   CHKPTR ( CHK_STANDARD, "lparss_c", list  );

   if ( list[0] == NULLCHAR )
   {
      insrtc_c ( "", set );

      chkout_c ( "lparss_c" );
      return;
   }


   /*
   Check the input delimiter string to make sure the pointers are
   non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "lparss_c", list   );
   CHKFSTR ( CHK_STANDARD, "lparss_c", delims );


   /*
   Make sure we've been handed a character set.
   */
   CELLTYPECHK ( CHK_STANDARD, "lparss_c", SPICE_CHR, set );


   /*
   Initialize the set if necessary.
   */
   CELLINIT ( set );


   /*
   Create a Fortran-style character set for the f2c'd routine to
   write to.  The first argument (caller) is empty because we
   don't want to use delegated check-in.
   */
   C2F_MAP_CELL ( "",  set,  &fCell,  &fLen );

   if ( failed_c() )
   {
      chkout_c ( "lparss_c" );
      return;
   }


   /*
   Call the f2c'd routine.
   */
   lparss_ ( ( char       * ) list,
             ( char       * ) delims,
             ( char       * ) fCell,
             ( ftnlen       ) strlen(list),
             ( ftnlen       ) strlen(delims),
             ( ftnlen       ) fLen           );

   /*
   Map the Fortran set to a CSPICE set.
   */
   F2C_MAP_CELL ( fCell, fLen, set );


   /*
   We're done with the dynamically allocated Fortran-style array.
   */
   free ( fCell );


   chkout_c ( "lparss_c" );

} /* End lparss_c */
