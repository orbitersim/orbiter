/*

-Procedure gdpool_c (Get d.p. values from the kernel pool)

-Abstract

   Return the d.p. value of a kernel variable from the kernel pool.

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

   KERNEL

-Keywords

   CONSTANTS
   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void gdpool_c ( ConstSpiceChar * name,
                   SpiceInt         start,
                   SpiceInt         room,
                   SpiceInt       * n,
                   SpiceDouble    * values,
                   SpiceBoolean   * found )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   name       I   Name of the variable whose value is to be returned.
   start      I   Which component to start retrieving for `name'.
   room       I   The largest number of values to return.
   n          O   Number of values returned for `name'.
   values     O   Values associated with `name'.
   found      O   SPICETRUE if variable is in pool.

-Detailed_Input

   name        is the name of the variable whose values are to be
               returned. If the variable is not in the pool with
               numeric type, found will be SPICEFALSE.

   start       is the index of the first component of name to return.
               The index follows the C convention of being 0 based.
               If start is less than 0, it will be treated as 0. If
               start is greater than the total number of components
               available for name, no values will be returned (n will
               be set to zero).  However, found will still be set to
               SPICETRUE

   room        is the maximum number of components that should be
               returned for this variable. (Usually it is the amount
               of room available in the array values). If room is
               less than 1 the error SPICE(BADARRAYSIZE) will be
               signaled.

-Detailed_Output

   n           is the number of values associated with name that
               are returned. It will always be less than or equal
               to room.

               If name is not in the pool with numeric type, no value
               is given to n.

   values      is the array of values associated with name.
               If name is not in the pool with numeric type, no
               values are given to the elements of values.

   found       is SPICETRUE if the variable is in the pool and has
               numeric type, SPICEFALSE if it is not.

-Parameters

   None.

-Exceptions

   1)  If the value of `room' is less than one, the error
       SPICE(BADARRAYSIZE) is signaled by a routine in the call tree
       of this routine.

   2)  If the `name' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `name' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This routine provides the user interface to retrieving
   numeric data stored in the kernel pool. This interface
   allows you to retrieve the data associated with a variable
   in multiple accesses. Under some circumstances this alleviates
   the problem of having to know in advance the maximum amount
   of space needed to accommodate all kernel variables.

   However, this method of access does come with a price. It is
   always more efficient to retrieve all of the data associated
   with a kernel pool data in one call than it is to retrieve
   it in sections.

   This routine should be used in place of rtpool_c when possible
   as it avoids errors associated with writing data past the
   end of an array.

   See also the entry points gipool_c and gcpool_c.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following code example demonstrates how the data stored
      in a kernel pool variable can be retrieved in pieces.

      Use the kernel shown below to load the kernel pool with the
      variables used within the example.


         KPL/MK

         File name: gdpool_ex1.tm

         This kernel is intended to support operation of SPICE
         example programs.

         \begindata

            CTEST_VAL = ('LARRY', 'MOE', 'CURLY' )

            ITEST_VAL = ( 3141, 186, 282 )

            DTEST_VAL = ( 3.1415, 186. , 282.397 )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program gdpool_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local parameters.
         ./
         #define   NUMVALS 2

         /.
         Local variables.
         ./
         SpiceBoolean      found;

         SpiceDouble       vals[NUMVALS];

         SpiceInt          n;
         SpiceInt          i;

         /.
         Load the test data.
         ./
         furnsh_c ( "gdpool_ex1.tm" );

         /.
         Is data available by that name?
         ./
         gdpool_c ( "DTEST_VAL", 1, NUMVALS, &n, vals, &found );

         /.
         If so, show me the values.
         ./
         if ( !found )
         {
            printf ( "No dp data available for DTEST_VAL.\n" );
         }
         else
         {
            for ( i=0; i < NUMVALS; i++ )
            {
               printf ( "%f \n", vals[i] );
            }

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      186.000000
      282.397000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.1.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.
       Removed unnecessary includes, replaced "ldpool_c" by
       "furnsh_c" and fixed bug in the code example.

   -CSPICE Version 2.1.0, 22-JUN-1999 (EDW)

       Re-implemented routine without dynamically allocated, temporary
       strings.

       Added local variable to return boolean/logical values. This
       fix allows the routine to function if int and long are different
       sizes.

   -CSPICE Version 2.0.1, 08-FEB-1998 (EDW)

       The start parameter is now zero based as per C convention.

   -CSPICE Version 1.0.1, 06-JAN-1998 (EDW)

       Replaced example routine. Included the data for a test kernel.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   RETURN the d.p. value of a pooled kernel variable
   RETURN the numeric value of a pooled kernel variable

-&
*/

{ /* Begin gdpool_c */

   /*
   Local variables.
   */
   logical         yes;


   /* The index is zero based here but not in gdpool_. */
   start = start + 1;


   /*
   Participate in error handling
   */
   chkin_c ( "gdpool_c");


   /*
   Check the input string name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "gdpool_c", name );


   /*
   Call the f2c'd routine
   */
   gdpool_( ( char       * ) name,
            ( integer    * ) &start,
            ( integer    * ) &room,
            ( integer    * ) n,
            ( doublereal * ) values,
            ( logical    * ) &yes,
            ( ftnlen       ) strlen(name) );



   /* Cast back to a SpiceBoolean. */
   *found = yes;


   /* Done.  Checkout. */
   chkout_c ( "gdpool_c");


} /* End gdpool_c */
