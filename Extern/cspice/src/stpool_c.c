/*

-Procedure stpool_c ( String from pool )

-Abstract

   Retrieve the nth string from a kernel pool variable, where the
   string may be continued across several components of the kernel
   pool variable.

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

   POOL

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void stpool_c ( ConstSpiceChar    * item,
                   SpiceInt            nth,
                   ConstSpiceChar    * contin,
                   SpiceInt            nthlen,
                   SpiceChar         * nthstr,
                   SpiceInt          * size,
                   SpiceBoolean      * found  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   item       I   Name of the kernel pool variable.
   nth        I   Index of the full string to retrieve.
   contin     I   Character sequence used to indicate continuation.
   nthlen     I   Maximum length of output string `nth'.
   nthstr     O   A full string concatenated across continuations.
   size       O   The number of characters in the full string value.
   found      O   Flag indicating success or failure of request.

-Detailed_Input

   item        is the name of a kernel pool variable for which
               the caller wants to retrieve a full (potentially
               continued) string component.

   nth         is the number of the string to retrieve from the kernel
               pool. The range of `nth' is 0 to one less than the
               number of full strings that are present.

   contin      is a sequence of characters which (if they appear as the
               last non-blank sequence of characters in a component of a
               value of a kernel pool variable) act as a continuation
               marker: the marker indicates that the string associated
               with the component is continued into the next literal
               component of the kernel pool variable.

               If `contin' is blank or empty, all of the components of
               `item' will be retrieved as a single string.

   nthlen      is the available space in the output `nthstr', counting
               room for the terminating null. Up to nthlen-1 "data"
               characters will be assigned to the output string.

-Detailed_Output

   nthstr      is the `nth' full string associated with the kernel pool
               variable specified by `item'.

               Note that if `nthstr' is not sufficiently long to hold the fully
               continued string, the value will be truncated. You can determine
               if `nthstr' has been truncated by examining the variable `size'.
               `nthstr' will always be null-terminated, even if truncation of
               the data occurs.

   size        is the index of last non-blank character of the continued
               string as it is represented in the kernel pool. This is
               the actual number of characters needed to hold the
               requested string. If `nthstr' contains a truncated portion
               of the full string, strlen(nthstr) will be less than `size'.

               If the value of `nthstr' should be a blank, then `size' will
               be set to 1.

   found       is a logical variable indicating success of the request
               to retrieve the `nth' string associated with `item'. If an
               N'th string exists, `found' will be set to SPICETRUE;
               otherwise SPICEFALSE will be set to SPICEFALSE

-Parameters

   None.

-Exceptions

   1)  If the variable specified by `item' is not present in the kernel
       pool or is present but is not character valued, `nthstr' will be
       returned as an empty string, `size' will be returned with the value 0
       and `found' will be set to SPICEFALSE. In particular if `nth' is less
       than 1, `nthstr' will be returned as an empty string, `size' will be
       zero and `found' will be SPICEFALSE.

   2)  If the variable specified has a blank string associated
       with its `nth' full string, `nthstr' will be blank, `size'
       will be 1 and `found' will be set to SPICETRUE.

   3)  If `nthstr' is not long enough to hold all of the characters
       associated with the `nth' string, it will be truncated on the
       right. `nthstr' will still be null terminated.

   4)  If the continuation character is a blank, every component
       of the variable specified by `item' will be inserted into
       the output string.

   5)  If the continuation character is blank, then a blank component
       of a variable is treated as a component with no letters. For
       example:

          STRINGS = ( 'This is a variable'
                      'with a blank'
                      ' '
                      'component.' )

       Is equivalent to

          STRINGS = ( 'This is a variable'
                      'with a blank'
                      'component.' )

       from the point of view of stpool_c if `contin' is set to the
       blank character.

   6)  If any of the `item' or `contin' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled.

   7)  If any of the `item' or `contin' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled.

   8)  If the `nthstr' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   9)  If the `nthstr' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   The SPICE Kernel Pool provides a very convenient interface for
   supplying both numeric and textual data to user application
   programs. However, any particular component of a character valued
   component of a kernel pool variable is limited to 80 or fewer
   characters in length.

   This routine allows you to overcome this limitation by "continuing"
   a character component of a kernel pool variable. To do this you need
   to select a continuation sequence of characters and then insert this
   sequence as the last non-blank set of characters that make up the
   portion of the component that should be continued.

   For example, you may decide to use the sequence "//" to indicate
   that a string should be continued to the next component of a kernel
   pool variable. Then set up the kernel pool variable as shown below:

      LONG_STRINGS = ( 'This is part of the first component //'
                       'that needs more than one line when //'
                       'inserting it into the kernel pool.'
                       'This is the second string that is split //'
                       'up as several components of a kernel pool //'
                       'variable.' )

   When loaded into the kernel pool, the variable LONG_STRINGS
   will have six literal components:

      component[0] == "This is part of the first component //"
      component[1] == "that needs more than one line when //"
      component[2] == "inserting it into the kernel pool."
      component[3] == "This is the second string that is split //"
      component[4] == "up as several components of a kernel pool //"
      component[5] == "variable."

   These are the components that would be retrieved by the call

      gcpool_c ( "LONG_STRINGS", 1, 6, 81, &n, component, &found );

   However, using the routine stpool_c you can view the variable
   LONG_STRINGS as having two long components.

      strgna == "This is part of the first component that "
                "needs more than one line when inserting "
                "it into the kernel pool. "

      strgnb == "This is the second string that is split "
                "up as several components of a kernel pool "
                "variable. "


   These string components would be retrieved by the following two
   calls.

      stpool_c( "LONG_STRINGS", 0, "//", 200, strgna, &size, &found );
      stpool_c( "LONG_STRINGS", 1, "//", 200, strgnb, &size, &found );

-Examples

   Example 1. Retrieving file names.

      Suppose a you have used the kernel pool as a mechanism for
      specifying SPK files to load at startup but that the full names
      of the files are too long to be contained in a single text line
      of a kernel pool assignment.

      By selecting an appropriate continuation character ("*" for
      example)  you can insert the full names of the SPK files into the
      kernel pool and then retrieve them using this routine.

      First set up the kernel pool specification of the strings
      as shown here:

            SPK_FILES = ( 'this_is_the_full_path_specification_*'
                          'of_a_file_with_a_long_name'
                          'this_is_the_full_path_specification_*'
                          'of_a_second_file_with_a_very_long_*'
                          'name' )

      Now to retrieve and load the SPK_FILES one at a time,
      exercise the following loop.

         #include <stdio.h>
         #include <string.h>
         #include "SpiceUsr.h"
             .
             .
             .
         #define FILSIZ          255

         SpiceBoolean            found;

         SpiceChar               file [ FILSIZ ];

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                size;


         i = 0;

         stpool_c ( "SPK_FILES", i, "*", FILSIZ, file, &size, &found );

         while (  found  &&  ( strlen(file) == size )  )
         {
            spklef_c ( file, &handle );

            i++;

            stpool_c ( "SPK_FILES",  i,      "*",    FILSIZ,
                       file,         &size,  &found          );
         }

         if (  found  &&  ( strlen(file) != size )  )
         {
            printf ( "The %d th file name was too long\n", i );
         }



   Example 2. Retrieving all components as a string.


   Occasionally, it may be useful to retrieve the entire
   contents of a kernel pool variable as a single string. To
   do this you can use the blank character as the
   continuation character. For example if you place the
   following assignment in a text kernel

       COMMENT = (  'This is a long note '
                    ' about the intended '
                    ' use of this text kernel that '
                    ' can be retrieved at run time.' )

   you can retrieve COMMENT as single string via the call below. Here
   LENOUT is the declared length of commnt.

      stpool_c ( "COMMENT", 1, " ", commnt, LENOUT, &size, &found );

   The result will be that commnt will have the following value.

      commnt == "This is a long note about the intended use of "
                "this text kernel that can be retrieved at run "
                "time. "

   Note that the leading blanks of each component of COMMENT are
   significant; trailing blanks are not significant.

   If COMMENT had been set as

       COMMENT = (  'This is a long note '
                    'about the intended '
                    'use of this text kernel that '
                    'can be retrieved at run time.' )

   Then the call to stpool_c above would have resulted in several
   words being run together as shown below.


      commnt == "This is a long noteabout the intendeduse of "
                "this text kernel thatcan be retrieved at run "
                "time. "


   resulted in several words being run together as shown above.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.3.0, 01-NOV-2021 (JDR)

       Changed the arguments "lenout" and "string" to "nthlen" and "nthstr"
       for consistency with other routines.

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.2.0, 06-SEP-2004 (NJB)

       Bug fix: added CHKOSTR call to check output string pointer
       and length. Made some minor header updates.

   -CSPICE Version 1.1.0, 06-MAY-2003 (NJB)

       Bug fix: removed extra #include statement referencing
       SpiceZfc.h.

   -CSPICE Version 1.0.1, 10-JUN-2001 (NJB)

       Header was corrected: kernel variable assignments were shown
       using double quotes; these have been changed to single quotes.

       Various instances of the '=' operator were replaced with the
       '==' operator in comments where the intent was to indicate
       equality of two items.

   -CSPICE Version 1.0.0, 10-JUN-1999 (NJB) (WLT)

-Index_Entries

   Retrieve a continued string value from the kernel pool

-&
*/

{ /* Begin stpool_c */


   /*
   Local variables
   */
   logical                 fnd;
   SpiceInt                ncomp;


   /*
   Participate in error tracing.
   */
   chkin_c ( "stpool_c" );


   /*
   Initialize the found flag in case we get kicked out on an error
   condition.
   */

   *found = SPICEFALSE;


   /*
   Check the input strings item and contin to make sure the pointers
   are non-null and the strings are non-empty.
   */
   CHKFSTR ( CHK_STANDARD, "stpool_c", item   );
   CHKFSTR ( CHK_STANDARD, "stpool_c", contin );


   /*
   Check the output string to make sure the pointer is non-null and that
   there is room for at least one character plus a null terminator.
   */
   CHKOSTR ( CHK_STANDARD, "stpool_c", nthstr, nthlen );

   /*
   Call the f2c'd routine.  First map the number of components to
   the Fortran style range of 1 : #of components.
   */

   ncomp = nth + 1;

   stpool_ ( ( char         * ) item,
             ( integer      * ) &ncomp,
             ( char         * ) contin,
             ( char         * ) nthstr,
             ( integer      * ) size,
             ( logical      * ) &fnd,
             ( ftnlen         ) strlen(item),
             ( ftnlen         ) strlen(contin),
             ( ftnlen         ) nthlen-1        );

   /*
   Convert the output string from C style to Fortran style.
   */
   F2C_ConvertStr ( nthlen, nthstr );


   /*
   If the output string is supposed to consist of a single blank, make
   sure that's what `nthstr' contains.  This condition holds when fnd
   is SPICETRUE, `nthstr' is empty, and size is 1.  The Fortran to C
   conversion wipes out trailing white space; that's why `nthstr' would
   be empty in this case.
   */

   if (  fnd   &&  ( strlen(nthstr) == 0 )  &&  ( *size == 1 )  )
   {
      nthstr[0] = BLANK;
      nthstr[1] = NULLCHAR;
   }


   /*
   Set the SpiceBoolean found flag.
   */
   *found = fnd;


   chkout_c ( "stpool_c" );

} /* End stpool_c */
