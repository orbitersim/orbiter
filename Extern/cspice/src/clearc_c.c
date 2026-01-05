/*

-Procedure clearc_c ( Clear a two-dimensional character array )

-Abstract

   Fill a two-dimensional character array with blank strings.

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

   ARRAY
   ASSIGNMENT

*/
   #include <string.h>
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"


   void clearc_c ( SpiceInt            ndim,
                   SpiceInt            arrlen,
                   void              * array   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   ndim       I   Number of rows of `array' to be set to blank.
   arrlen     I   Common length of the strings in `array'.
   array      O   Two-dimensional character array to be filled.

-Detailed_Input

   ndim        is the number of rows in `array' which are to be set to
               blank.

   arrlen      is the common length of the strings in `array', including
               the terminating null character.

-Detailed_Output

   array       a two-dimensional character array having as minimum dimensions

                  [ndim][arrlen]

               Each of the first `ndim' rows of the two-dimensional character
               array is to be filled with blank characters up to index arrlen-2
               and null-terminated.

-Parameters

   None.

-Exceptions

   1)  If ndim < 1, the array is not modified.

   2)  If the `array' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `array' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   None.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Initialize a two dimensional character array and then clear
      the first two rows.


      Example code begins here.


      /.
         Program clearc_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define ARRSZ        22
         #define NDIM         4

         /.
         Local variables.
         ./
         SpiceChar            array  [NDIM][ARRSZ];
         SpiceInt             i;

         /.
         Initialize `array'.
         ./
         strncpy( array[0], "Element #1", 11 );
         strncpy( array[1], "Element #2", 11 );
         strncpy( array[2], "Element #3", 11 );
         strncpy( array[3], "Element #4", 11 );

         printf( "Contents of ARRAY before clearc_c:\n" );
         printf( "\n" );
         for ( i = 0; i < NDIM; i++ )
         {
            printf( "Position #%d: %s\n", (int)i, array[i] );
         }

         /.
         Clear the first 2 elements.
         ./
         clearc_c ( 2, ARRSZ, array );

         printf( "\n" );
         printf( "Contents of ARRAY after clearc_c:\n" );
         printf( "\n" );
         for ( i = 0; i < NDIM; i++ )
         {
            printf( "Position #%d: %s\n", (int)i, array[i] );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Contents of ARRAY before clearc_c:

      Position #0: Element #1
      Position #1: Element #2
      Position #2: Element #3
      Position #3: Element #4

      Contents of ARRAY after clearc_c:

      Position #0:
      Position #1:
      Position #2: Element #3
      Position #3: Element #4


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 20-SEP-2021 (JDR)

-Index_Entries

   clear a character array

-&
*/

{ /* Begin clearc_c */

   /*
   Local variables
   */
   SpiceInt            i;
   SpiceInt            j;

   /*
   If ndim < 1, the array is not modified. Return immediately.
   */
   if ( ndim < 1 )
   {
      return;
   }

   /*
   Check the output string `array' to make sure the pointer is non-null
   and the string has enough room for one output character and a null
   terminator.
   */
   CHKOSTR ( CHK_DISCOVER, "clearc_c", array, arrlen );

   /*
   Fill the two-dimensional array of characters with blank strings.
   */
   for ( i = 0;  i < ndim;  i++ )
   {
      j = i * arrlen;
      memset( (char*)array + j, ' ', arrlen-1 );
      memset( (char*)array + arrlen + j - 1, '\0', 1 );
   }

} /* End clearc_c */
