/*

-Procedure zzalloc ( Umbrella routine for CSPICE amemory allocation cals )

-Abstract

   Set of routines to manage allocation and deallocation of memory
   for variables used by CSPICE calls. primary usage intended for
   interfaces to external languages and applications (IDL, MATLAB, etc. )

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

   error

*/


   /*
   Prevent the redefinition of malloc and free in these routines.
   Note, this line must preceed all #includes.
   */
#define NO_NEW_ALLOC

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include "SpiceUsr.h"
#include "zzalloc.h"

   /*
   Define 'op' tags for zzalloc_count control.
   */
enum{ ALLOC_INC,    /* Increment the count value by +1.   */
      ALLOC_DEC,    /* Decrement the count value by -1.   */
      ALLOC_EQU };  /* Return the current value of count. */


/*

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   Routines coded in this file:

      Private:

         zzalloc_count

      Public:

         alloc_SpiceBoolean_C_array
         alloc_SpiceDouble_C_array
         alloc_SpiceInt_C_array
         alloc_SpiceMemory
         alloc_SpiceString
         alloc_SpiceString_C_Copy_array
         alloc_SpiceString_C_array
         alloc_SpiceString_Pointer_array
         alloc_count
         free_SpiceMemory
         free_SpiceString_C_array

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   E.D. Wright (JPL)

-Version

   CSPICE 1.3.1 24-JUL-2021 (EDW)

      Corrected spelling mistake "must excceed" in error message 
      with "must exceed".

   CSPICE 1.3.0 26-AUG-2016 (EDW)

      Added routine alloc_SpiceBoolean_C_array.

   CSPICE 1.2.0 02-MAY-2008 (EDW)

      Implemented use of enums as input flags to zzalloc_count.
      Added a routine alloc_count function as an accessor to
      the allocation count stored in zzalloc_count.

   CSPICE 1.1.0 10-MAY-2007 (EDW)

      Added additional error checks on 'row' and 'cols' arguments in
      alloc_SpiceInt_C_array and alloc_SpiceDouble_C_array.

   CSPICE 1.0.10 10-MAY-2007 (EDW)

      Minor edits to clarify declarations and remove unneeded casts.

   Icy 1.0.9 23-JUN-2005 (EDW)

      Added alloc_SpiceString_Pointer_array routine to allocate
      an array of pointers to SpiceChars - a more conventional
      manner to define an array of strings.

      Edited alloc_SpiceMemory to pass an unsigned int rather than
      an int. Added error check for 'op' value in zzalloc_count.
      Cast zzalloc_count calls to void when ignoring the return value.

      Defined NO_NEW_ALLOC preprocessor flag to prevent the memory
      test malloc/free macros from redefining the calls to C malloc/free
      in this routine. Implement the malloc/free macros with:

         #ifndef NO_NEW_ALLOC

         #define malloc(x) alloc_SpiceMemory(x)
         #define free(x)   free_SpiceMemory(x)

         #endif

      placed as the first directives in SpiceUsr.h.

   Icy 1.0.7 13-JUL-2004 (EDW)

      Added proper header documentation.

-Index_Entries

   None.

-&
*/




/*

-Procedure zzalloc_count ( Track number of allocations/deallocations)

-Abstract

   The count increments when allocating memory, the count
   decrements when deallocating memory. The routine can also
   return the current allocation count.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   None.

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   None.

-Version

   None.

-Index_Entries

   None.

-&
*/
int zzalloc_count ( int op )
   {

   /*
   Initialize the count to zero. Save the value
   between calls.
   */
   static int            count = 0;

   /*
   Respond according to the op variable.
   */
   switch (op)
      {
      case ALLOC_INC:

         /*
         An allocation, increment the count.
         */
         ++count;

         return count;
         break;


      case ALLOC_DEC:

         /*
         A free, decrement the count.
         */
         --count;

         return count;
         break;

      case ALLOC_EQU:

          /*
         Return the current count. Should equal zero at end of
         program run and NEVER have a negative value.
         */
         return count;
         break;

      default:

         setmsg_c ( "Unknown op in zzalloc_count: #");
         errint_c  ( "#", op           );
         sigerr_c ( "SPICE(UNKNOWNOP)" );
         return 0;
         break;

      }

   }



/*

-Procedure alloc_SpiceString ( Allocate a string )

-Abstract

   Allocate a block of memory for a SpiceChar string. Signal an
   error if the malloc fails.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   None.

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   None.

-Version

   None.

-Index_Entries

   None.

-&
*/

SpiceChar * alloc_SpiceString ( int length )
   {

   SpiceChar           * str;

   chkin_c ( "alloc_SpiceString" );

   /* Allocate the needed memory for the double array. Check for errors. */
   str = (SpiceChar *) alloc_SpiceMemory ( length * sizeof(SpiceChar) );

   /*
   Check for a malloc failure. Signal a SPICE error if error found.
   */
   if (str == NULL )
      {

      /* Malloc failed; signal an error; return a NULL. */
      setmsg_c ( "Malloc failed to allocate space for a string of length #. ");
      errint_c ( "#", (SpiceInt) length   );
      sigerr_c ( "SPICE(MALLOCFAILED)"    );
      chkout_c ( "alloc_SpiceString" );
      return NULL;
      }

   chkout_c ( "alloc_SpiceString" );
   return str;
   }



/*

-Procedure alloc_SpiceInt_C_array ( Allocate an array of SpiceInts)

-Abstract

   Allocate a block of memory for an array of SpiceInts. Signal an
   error if the malloc fails.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   None.

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   None.

-Version

   None.

-Index_Entries

   None.

-&
*/
SpiceInt * alloc_SpiceInt_C_array ( int rows, int cols )
   {

   SpiceInt            * mat;

   chkin_c ( "alloc_SpiceInt_C_array" );

   if ( rows*cols < 1 )
      {
      setmsg_c ( "The specified total workspace size #1 was "
                 "less than the minimum allowed value (1). "
                 "The value for both rows, #2, and cols, #3, "
                 "must exceed zero."                       );
      errint_c ( "#1", (SpiceInt) (rows*cols)               );
      errint_c ( "#2", (SpiceInt) rows                      );
      errint_c ( "#3", (SpiceInt) cols                      );
      sigerr_c ( "SPICE(VALUEOUTOFRANGE)"                   );
      chkout_c ( "alloc_SpiceInt_C_array"                   );
      return NULL;
      }

   /*
   Allocate the needed memory for the double array. Check for errors.
   */
   mat = (SpiceInt *) alloc_SpiceMemory ( rows * cols * sizeof(SpiceInt) );

   /*
   Check for a malloc failure. Signal a SPICE error if error found.
   */
   if ( mat == NULL )
      {

      /* Malloc failed; signal an error; return a NULL. */
      setmsg_c ( "Malloc failed to allocate space for an array of "
                 "$1 * $2 SpiceInts. ");
      errint_c ( "#", (SpiceInt) rows     );
      errint_c ( "#", (SpiceInt) cols     );
      sigerr_c ( "SPICE(MALLOCFAILED)"    );
      chkout_c ( "alloc_SpiceInt_C_array" );
      return NULL;
      }

   chkout_c ( "alloc_SpiceInt_C_array" );
   return mat;
   }



/*

-Procedure alloc_SpiceBoolean_C_array ( Allocate an array of SpiceBooleans)

-Abstract

   Allocate a block of memory for an array of SpiceBooleans. Signal an
   error if the malloc fails.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   None.

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   None.

-Version

   None.

-Index_Entries

   None.

-&
*/
SpiceBoolean * alloc_SpiceBoolean_C_array ( int rows, int cols )
   {

   SpiceBoolean            * mat;

   chkin_c ( "alloc_SpiceBoolean_C_array" );

   if ( rows*cols < 1 )
      {
      setmsg_c ( "The specified total workspace size #1 was "
                 "less than the minimum allowed value (1). "
                 "The value for both rows, #2, and cols, #3, "
                 "must exceed zero."                       );
      errint_c ( "#1", (SpiceInt) (rows*cols)               );
      errint_c ( "#2", (SpiceInt) rows                      );
      errint_c ( "#3", (SpiceInt) cols                      );
      sigerr_c ( "SPICE(VALUEOUTOFRANGE)"                   );
      chkout_c ( "alloc_SpiceBoolean_C_array"                   );
      return NULL;
      }

   /*
   Allocate the needed memory for the double array. Check for errors.
   */
   mat = (SpiceBoolean *) alloc_SpiceMemory ( rows * cols * 
                                              sizeof(SpiceBoolean) );

   /*
   Check for a malloc failure. Signal a SPICE error if error found.
   */
   if ( mat == NULL )
      {

      /* Malloc failed; signal an error; return a NULL. */
      setmsg_c ( "Malloc failed to allocate space for an array of "
                 "$1 * $2 SpiceBooleans. ");
      errint_c ( "#", (SpiceInt) rows     );
      errint_c ( "#", (SpiceInt) cols     );
      sigerr_c ( "SPICE(MALLOCFAILED)"    );
      chkout_c ( "alloc_SpiceBoolean_C_array" );
      return NULL;
      }

   chkout_c ( "alloc_SpiceBoolean_C_array" );
   return mat;
   }



/*

-Procedure alloc_SpiceDouble_C_array ( Allocate an array of SpiceDoubles)

-Abstract

   Allocate a block of memory for an array of SpiceDoubles. Signal an
   error if the malloc fails.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   The routine allocates a block of contiguous memory then returns a
   pointer the block. It does not return an array of pointers or
   a pointer to an array of pointers.

-Examples

   SpiceInt         n      = 2;
   SpiceChar      * utc[] = { "Jan 1 2006",
                              "Jan 1 2007" };

   et = (SpiceDouble*)alloc_SpiceDouble_C_array( 1, n );

   for( i=0; i<n; i++ )
      {
      puts(utc[i] );
      str2et_c( utc[i],  et + i );
      }

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   None.

-Version

   None.

-Index_Entries

   None.

-&
*/
SpiceDouble * alloc_SpiceDouble_C_array ( int rows, int cols )
   {

   SpiceDouble         * mat;

   chkin_c ( "alloc_SpiceDouble_C_array" );

   if ( (rows < 1) || (cols < 1) )
      {
      setmsg_c ( "The specified total workspace size #1 was "
                 "less than the minimum allowed value (1). "
                 "The value for both rows, #2, and cols, #3, "
                 "must exceed zero."                       );
      errint_c ( "#1", (SpiceInt) (rows*cols)               );
      errint_c ( "#2", (SpiceInt) rows                      );
      errint_c ( "#3", (SpiceInt) cols                      );
      sigerr_c ( "SPICE(VALUEOUTOFRANGE)"                   );
      chkout_c ( "alloc_SpiceDouble_C_array"                );
      return NULL;
      }

   /*
   Allocate the needed memory for the double array. Check for errors.
   */
   mat = (SpiceDouble*) alloc_SpiceMemory( rows *cols *sizeof(SpiceDouble));

   /*
   Check for a malloc failure. Signal a SPICE error if error found.
   */
   if ( mat == NULL )
      {

      /* Malloc failed; signal an error; return a NULL. */
      setmsg_c ( "Malloc failed to allocate space for an array of "
                 "$1 * $2 SpiceDoubles. ");
      errint_c ( "#", (SpiceInt) rows        );
      errint_c ( "#", (SpiceInt) cols        );
      sigerr_c ( "SPICE(MALLOCFAILED)"       );
      chkout_c ( "alloc_SpiceDouble_C_array" );
      return NULL;
      }

   chkout_c ( "alloc_SpiceDouble_C_array" );
   return mat;
   }



/*

-Procedure alloc_SpiceString_C_array ( Allocate an array of SpiceChar strings)

-Abstract

   Allocate memory for a contiguous array of strings, each string of length
   'string_length' with a total of 'string_count' strings.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1) If the input number of strings has value less than 1, the error
      SPICE(NOTPOSITIVE) signals.

   2) If the length of each string has value less than 2 (one character plus
      a line terminator), the error SPICE(STRINGTOOSMALL) signals.

   3) If a malloc fails, the error SPICE(MALLOCFAILED) signals.

-Files

   None.

-Particulars

   This routine produces a memory block functionally similar to a
   declaration of:

      SpiceChar    X[string_count][string_length]

-Examples

   CSPICE wrappers using arrays of strings declare the arrays as:

      SpiceChar         ** cvals;

   Allocate memory:

      cvals = alloc_SpiceString_C_array ( cvals_len, cvals_size );

   Where 'cvals_len' represents the length of each string, 'cvals[i]',
   and 'cvals_size' represents the number of strings in array 'cvals'.

   Use of the allocated array by the gcpool_c routine:

      SpiceInt              start;
      SpiceInt              cvals_size;
      SpiceInt              cvals_len;
      SpiceInt              cvals_dim;
      SpiceBoolean        * found;

      gcpool_c( name, start, cvals_size, cvals_len, &cvals_dim, *cvals, found);

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   None.

-Version

   None.

-Index_Entries

   None.

-&
*/
SpiceChar ** alloc_SpiceString_C_array ( int string_length, int string_count )
   {

   SpiceChar          ** cvals;

   chkin_c ( "alloc_SpiceString_C_array" );

   /*
   Sanity checks.
   */
   if( string_count < 1 )
      {
      setmsg_c ( "The user defined a non-positive value for string count: #");
      errint_c ( "#", string_count           );
      sigerr_c ( "SPICE(NOTPOSITIVE)"        );
      chkout_c ( "alloc_SpiceString_C_array" );
      return NULL;
      }
   else if( string_length < 2 )
      {
      setmsg_c ( "The user defined a value less than 2 for string length: #");
      errint_c ( "#", string_length          );
      sigerr_c ( "SPICE(STRINGTOOSMALL)"     );
      chkout_c ( "alloc_SpiceString_C_array" );
      return NULL;
      }


   /*
   Allocate the needed memory for the strings array. Check for errors.
   */
   cvals    = (SpiceChar**)
               alloc_SpiceMemory ( string_count*sizeof(SpiceChar*) );
   if ( cvals == NULL )
      {

      /* Malloc failed; signal an error; return a NULL. */
      setmsg_c ( "Malloc failed to allocate space for # SpiceChar pointers. ");
      errint_c ( "#", string_count           );
      sigerr_c ( "SPICE(MALLOCFAILED)"       );
      chkout_c ( "alloc_SpiceString_C_array" );
      return NULL;
      }

   /*
   Now allocate enough memory for the string_length * string_count block.
   Assign the memory to cvals[0], i.e. *cvals.
   */
   cvals[0] = (SpiceChar* ) alloc_SpiceMemory ( string_length
                                                * string_count
                                                * sizeof(SpiceChar) );
   if ( cvals[0] == NULL )
      {

      /*
      Malloc failed; free the allocated memory; signal an error; return
      a NULL .
      */
      free_SpiceMemory( cvals );

      setmsg_c ( "Malloc failed to allocate space for $1 * $2 "
                 "SpiceChar values. ");
      errint_c ( "$1", string_count          );
      errint_c ( "$2", string_length         );
      sigerr_c ( "SPICE(MALLOCFAILED)"       );
      chkout_c ( "alloc_SpiceString_C_array" );
      return NULL;
      }

   /*
   Note: this routine allocated the needed memory for the string array,
   nothing more. The user must explicitly assign the cvals pointers to
   the appropriate values.
   */
   chkout_c ( "alloc_SpiceString_C_array" );
   return cvals;
   }



/*

-Procedure alloc_SpiceString_C_Copy_array ( Copy array to pointers)

-Abstract

   Copy a string array of the form SpiceChar X[num][len] to a string array
   consisting of an array of SpiceChar pointers.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   None.

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   None.

-Version

   None.

-Index_Entries

   None.

-&
*/
SpiceChar ** alloc_SpiceString_C_Copy_array ( int array_len,
                                              int string_len,
                                              SpiceChar ** array )
   {

   SpiceChar          ** str_array;
   int                   i;

   chkin_c ( "alloc_SpiceString_C_Copy_array" );

   /*
   Sanity checks.
   */
   if( array_len < 1 )
      {
      setmsg_c ( "The user defined a non-positive value for array length: #");
      errint_c ( "#", array_len                   );
      sigerr_c ( "SPICE(NOTPOSITIVE)"             );
      chkout_c ( "alloc_SpiceString_C_Copy_array" );
      return NULL;
      }
   else if( string_len < 2 )
      {
      setmsg_c ( "The user defined a value less than 2 for string length: #");
      errint_c ( "#", string_len                  );
      sigerr_c ( "SPICE(NOTPOSITIVE)"             );
      chkout_c ( "alloc_SpiceString_C_Copy_array" );
      return NULL;
      }


   /*
   Create a string array for passing to the new array.
   */
   str_array = (SpiceChar**) alloc_SpiceMemory( sizeof(SpiceChar*) *array_len);
   if ( str_array == NULL )
      {
      /* Malloc failed; signal an error; return a NULL. */
      setmsg_c ( "Malloc failed to allocate space for # SpiceChar pointers. ");
      errint_c ( "#", array_len                   );
      sigerr_c ( "SPICE(MALLOCFAILED)"            );
      chkout_c ( "alloc_SpiceString_C_Copy_array" );
      return NULL;
      }


   /*
   Copy the data from items to the string array for the copy.
   */
   for ( i=0; i < array_len; i++)
         {

         str_array[i] = (SpiceChar *)
                        alloc_SpiceMemory(sizeof(SpiceChar) * string_len );

         if ( str_array[i] == NULL )
            {
            /*
            Malloc failed; free the memory; signal an error; return a NULL.
            */
            free_SpiceString_C_array ( i-1, str_array );

            setmsg_c ( "Malloc failed to allocate space for array "
                       "$1 of $2 SpiceChars. "          );
            errint_c ( "$1", (SpiceInt) i               );
            errint_c ( "$2", string_len                 );
            sigerr_c ( "SPICE(MALLOCFAILED)"            );
            chkout_c ( "alloc_SpiceString_C_Copy_array" );
            return NULL;
            }
         else
            {
            strcpy( (char*) str_array[i], *array + i*string_len );
            }

         }

   chkout_c ( "alloc_SpiceString_C_Copy_array" );
   return str_array;
   }



/*

-Procedure alloc_SpiceString_Pointer_array ( Allocate an array of
                                            pointers to SpiceChar )

-Abstract

   None.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   None.

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   None.

-Version

   None.

-Index_Entries

   None.

-&
*/
SpiceChar ** alloc_SpiceString_Pointer_array( int array_len )
   {

   SpiceChar          ** ptr_array;

   chkin_c ( "alloc_SpiceString_Pointer_array" );

   /*
   Sanity checks.
   */
   if( array_len < 1 )
      {
      setmsg_c ( "The user defined a non-positive value for array length: #");
      errint_c ( "#", array_len                    );
      sigerr_c ( "SPICE(NOTPOSITIVE)"              );
      chkout_c ( "alloc_SpiceString_Pointer_array" );
      return NULL;
      }


   /*
   Create a string array for passing to the new array.
   */
   ptr_array = alloc_SpiceMemory( array_len * sizeof(SpiceChar*) );
   if ( ptr_array == NULL )
      {
      /* Malloc failed; signal an error; return a NULL. */
      setmsg_c ( "Malloc failed to allocate space for # SpiceChar pointers. ");
      errint_c ( "#", array_len                    );
      sigerr_c ( "SPICE(MALLOCFAILED)"             );
      chkout_c ( "alloc_SpiceString_Pointer_array" );
      return NULL;
      }

   chkout_c ( "alloc_SpiceString_Pointer_array" );
   return ptr_array;
   }



/*

-Procedure free_SpiceString_C_array (Free string memory)

-Abstract

   Free string array memory allocated by alloc_SpiceString* routines.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   None.

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   None.

-Version

   None.

-Index_Entries

   None.

-&
*/
void free_SpiceString_C_array ( int dim, SpiceChar ** array )
   {

   int                   i;

   /*
   Now free the allocated memory, first each component
   then the pointer array.
   */
   for (i=0; i< dim; i++)
      {
      free_SpiceMemory( array[i]);
      }

   free_SpiceMemory ( array );
   }



/*

-Procedure free_SpiceMemory (Free allocated memory)

-Abstract

   Free memory allocated by alloc_Spice* routines.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   This function serves only as a wrapper to free with
   an error check for non NULL pointers and a corresponding
   alloc count decrement.

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   None.

-Version

   None.

-Index_Entries

   None.

-&
*/
void free_SpiceMemory( void * ptr )
   {

   /*
   Free the allocated memory.
   */
   free( ptr);

   /*
   Decrement the allocation count.
   */
   (void) zzalloc_count ( ALLOC_DEC );

   }



/*

-Procedure alloc_SpiceMemory (Allocate memory)

-Abstract

   Allocate memory for alloc_Spice* routines.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   This function serves only as a wrapper to malloc with a
   corresponding zzalloc_count increment.

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   None.

-Version

   None.

-Index_Entries

   None.

-&
*/
void * alloc_SpiceMemory ( size_t size )
   {

   /*
   The malloc return value.
   */
   void                * mem;

   /*
   Allocate the memory.
   */
   mem = malloc(size);

   /*
   Increment the alloc count if allocation succeeded.
   */
   if ( mem != NULL )
      {
      (void) zzalloc_count ( ALLOC_INC );
      }

   /*
   Return 'mem' regardless of the value.
   */
   return mem;

   }


/*

-Procedure alloc_count (Return the value of count)

-Abstract

   This function returns the value of the zzalloc variable 'count'.

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

   None.

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   None.

-Files

   None.

-Particulars

   This function serves only as an accessor function to the
   zzalloc_count call that returns the value of 'count'.

-Examples

   None.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   None.

-Version

   None.

-Index_Entries

   None.

-&
*/
int alloc_count()
   {

   /*
   Nothing to do except return the 'count' value.
   */
   return zzalloc_count( ALLOC_EQU );
   }

