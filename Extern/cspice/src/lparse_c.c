/*

-Procedure lparse_c ( Parse items from a list )

-Abstract

   Parse a list of items delimited by a single character.

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
   #include <stdio.h>

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void lparse_c ( ConstSpiceChar   * list,
                   ConstSpiceChar   * delim,
                   SpiceInt           nmax,
                   SpiceInt           itemln,
                   SpiceInt         * n,
                   void             * items   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   list       I    List of items delimited by delim.
   delim      I    Single character used to delimit items.
   nmax       I    Maximum number of items to return.
   itemln     I    Length of strings in item array.
   n          O    Number of items in the list.
   items      O    Items in the list, left justified.

-Detailed_Input

   list        is a string containing a list of items delimited by the
               single character delim. Consecutive delimiters, and
               delimiters at the beginning and end of the list, are
               considered to delimit empty items. A blank or empty
               list is considered to contain a single (empty) item.

   delim       is the character delimiting the items in the list.
               This may be any ASCII character, including a blank.
               However, by definition, consecutive blanks are NOT
               considered to be consecutive delimiters. In addition,
               leading and trailing blanks are ignored.

   nmax        is the maximum number of items to be returned from
               the list. This allows the user to guard against
               overflow from a list containing more items than
               expected.

   itemln      is the declared length of the strings in the string
               array items. This length must include room for the
               terminating null character in each string.

-Detailed_Output

   n           is the number of items in the list. `n' may be
               any number between one and `nmax'. `n' is always the
               number of delimiters plus one.

   items       is an array of strings containing the items in the list,
               left justified. Any item in the list too long to fit into
               an element of items is truncated on the right. Empty
               (null) or blank items in the input string are mapped to
               empty strings on output.

               items should be declared by the caller as:

                  SpiceCharitem [nmax][itemln]

-Parameters

   None.

-Exceptions

   1)  If the string length of `items' is too short to accommodate
       an item, the item will be truncated on the right.

   2)  If `nmax' is less than one, then `n' will be set to zero, and no
       items will be returned. This case is not an error.

   3)  If the input `list' string is empty, the routine will return a
       single, empty output string in `items'. This case is not an error.

   4)  If any of the `list', `delim' or `list' input string pointers
       is null, the error SPICE(NULLPOINTER) is signaled.

   5)  If any of the `list' or `delim' input strings has zero length,
       the error SPICE(EMPTYSTRING) is signaled.

   6)  If the `items' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   7)  If the `items' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   None.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Parse a character string to retrieve the words contained
      within.

      Example code begins here.


      /.
         Program lparse_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define NMAX         25
         #define STRLEN       256

         /.
         Local variables.
         ./
         SpiceChar          * delim;
         SpiceChar            items  [NMAX][STRLEN];
         SpiceChar          * list;

         SpiceInt             i;
         SpiceInt             n;

         /.
         Define the list of delimited items.

         Think of a sentence as a list delimited by a space.
         `delim' is assigned to a space.
         ./
         list  = "Run and find out.";
         delim = " ";

         /.
         Parse the items from `list'.
         ./
         lparse_c ( list, delim, NMAX, STRLEN, &n, items );

         /.
         Output the `items'.
         ./
         for ( i = 0; i < n; i++ )
         {

            printf( "Item %2d: %s\n", i, items[i] );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Item  0: Run
      Item  1: and
      Item  2: find
      Item  3: out.


   2) Repeat the previous example with different character
      delimiting the items in the list and different maximum number
      of items to return.

      Example code begins here.


      /.
         Program lparse_ex2
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local constants.
         ./
         #define NCASES       2
         #define NMAXT        25
         #define STRLEN       255

         /.
         Local variables.
         ./
         SpiceChar            delim  [NCASES][2];
         SpiceChar            items  [NMAXT][STRLEN];
         SpiceChar            list   [NCASES][STRLEN];

         SpiceInt             i;
         SpiceInt             j;
         SpiceInt             n;
         SpiceInt             nmax   [NCASES];

         /.
         Define the lists of delimited items, the delimiting
         character and the maximum number of items to return.
         ./
         strcpy( list[0],  "//option1//option2/ //" );
         strcpy( delim[0], "/" );
         nmax[0] = 20;

         strcpy( list[1],  " ,bob,   carol,, ted,  alice" );
         strcpy( delim[1], "," );
         nmax[1] = 4;

         for ( i = 0; i < NCASES; i++ )
         {

            printf( "Case %d:\n", i );
            printf( "  String: \"%s\"\n", list[i]  );
            printf( "  DELIM : \"%s\"\n", delim[i] );
            printf( "  NMAX  : %2d\n", nmax[i] );
            printf( "  Output items:\n" );

            /.
            Parse the items from `list'.
            ./
            lparse_c ( list[i], delim[i], nmax[i], STRLEN, &n, items );

            /.
            Output the `items'.
            ./
            for ( j = 0; j < n; j++ )
            {
               printf( "     Item %2d: \"%s\"\n", j, items[j] );
            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Case 0:
        String: "//option1//option2/ //"
        DELIM : "/"
        NMAX  : 20
        Output items:
           Item  0: ""
           Item  1: ""
           Item  2: "option1"
           Item  3: ""
           Item  4: "option2"
           Item  5: ""
           Item  6: ""
           Item  7: ""
      Case 1:
        String: " ,bob,   carol,, ted,  alice"
        DELIM : ","
        NMAX  :  4
        Output items:
           Item  0: ""
           Item  1: "bob"
           Item  2: "carol"
           Item  3: ""


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 2.3.0, 04-AUG-2021 (JDR)

       Changed input argument name "lenout" to "itemln" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Added complete code
       example.

   -CSPICE Version 2.2.0, 18-MAY-2001 (WLT)

       Added a cast to (char *) in the call to  F2C_ConvertTrStrArr

   -CSPICE Version 2.1.0, 20-APR-2000 (NJB)

       Bug fix: set n to zero for nmax < 1.

   -CSPICE Version 2.0.0, 25-MAR-2000 (NJB)

       Updated header to accurately describe treatment of null tokens.
       Updated code to handle the case of an empty input string or
       nmax < 1.

       Changed typedef SpiceVoid to void.

   -CSPICE Version 1.0.0, 09-FEB-1998 (NJB)

-Index_Entries

   parse items from a list

-&
*/

{ /* Begin lparse_c */


   /*
   Participate in error handling.
   */
   chkin_c ( "lparse_c" );


   /*
   If there's no room for output tokens, just return.
   */
   if ( nmax < 1 )
   {
      *n = 0;
      chkout_c ( "lparse_c" );
      return;
   }


   /*
   Make sure the output string array contains at least enough room
   for a null character in each string.  Unlike most CSPICE wrappers,
   lparse_c must check the output array before checking the inputs
   because there's a special case that results in returning before
   the input checks are performed.
   */
   CHKOSTR ( CHK_STANDARD, "lparse_c", items, itemln );


   /*
   Special case:  if the input string is empty, return a single blank
   string.

   We must know that list is not a null pointer first.
   */
   CHKPTR ( CHK_STANDARD, "lparse_c", list  );

   if ( list[0] == NULLCHAR )
   {
      *n                   =  1;
      *(SpiceChar *)items  =  NULLCHAR;

      chkout_c ( "lparse_c" );
      return;
   }


   /*
   Check the input delimiter string to make sure the pointers are
   non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "lparse_c", list  );
   CHKFSTR ( CHK_STANDARD, "lparse_c", delim );


   /*
   Call the f2c'd routine.
   */
   lparse_ ( ( char       * ) list,
             ( char       * ) delim,
             ( integer    * ) &nmax,
             ( integer    * ) n,
             ( char       * ) items,
             ( ftnlen       ) strlen(list),
             ( ftnlen       ) strlen(delim),
             ( ftnlen       ) itemln-1      );

   /*
   Reformat the output item array from Fortran to C style.  Trim
   trailing blanks from output tokens.
   */

   F2C_ConvertTrStrArr ( *n, itemln, (char *) items );


   chkout_c ( "lparse_c" );


} /* End lparse_c */
