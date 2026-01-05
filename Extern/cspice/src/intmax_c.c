/*

-Procedure intmax_c ( Largest integer number )

-Abstract

   Return the value of the largest (positive) number representable
   in a SpiceInt variable.

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

   CONSTANTS

*/

   #include "SpiceUsr.h"

   SpiceInt intmax_c ()

/*

-Brief_I/O

   The function returns the value of the largest (positive) number
   that can be represented in a SpiceInt variable.

-Detailed_Input

   None.

-Detailed_Output

   The function returns the value of the largest (positive) number
   that can be represented in an SpiceInt variable, where SpiceInt
   is a typedef defined in SpiceZdf.h.

   The returned value will be greater than or equal to 2147483647.
   See the -Particulars section for details.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The typedef SpiceInt is used throughout the CSPICE API to refer to
   integers; the precise type of integer is platform-dependent.  A
   SpiceInt always maps to the same type as does the f2c typedef
   integer.

   When translating Fortran code, f2c maps Fortran variables of type
   INTEGER to C variables of type "integer," where integer is a typedef
   defined in the f2c header file f2c.h. On all supported platforms,
   Fortran INTEGERS occupy at least 32 bits. On most platforms, this
   means that the typedef integer translates to type long. There are
   some exceptional platforms on which an integer translates to type
   int. The mapping must provide compatibility with the f2c typedef
   doublereal: integers must occupy half the storage of doublereals in
   order for these types to correctly represent the Fortran types
   INTEGER and DOUBLE PRECISION.

   On systems where the typedef integer maps to type long, the return
   value is defined by the macro LONG_MAX from the ANSI standard header
   file limits.h. According to the ANSI standard, LONG_MAX must be at
   least

      2147483647

   This is

        31
       2   - 1

   On systems where the typedef integer maps to type int, the value is
   defined by the macro INT_MAX from the ANSI standard header file
   limits.h. According to the ANSI standard, INT_MAX must be at least

      32767

   This is

       15
      2   - 1

   In practice however, the typedef integer will map to type int only
   if ints occupy at least four bytes, so the value of INT_MAX will
   actually be at least 2147483647.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Obtain the integer part of a double precision number. If the
      integer component of the number is out of range, avoid
      overflow by making it as large or small as possible.


      Example code begins here.


      /.
         Program intmax_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceInt             i;
         SpiceInt             ivalue;

         /.
         Define a set of three numbers, two of them having an
         integer component that is out of range.
         ./
         SpiceDouble          number [3] = { 2.e40, -1.5e35, 1.6 };

         for ( i = 0; i < 3; i++ )
         {
            printf( "Double precision number: %e\n", number[i] );

            /.
            If the integer component is out of range, avoid
            overflow by making it as large as possible.
            ./
            if ( number[i] > (SpiceDouble)( intmax_c() ) )
            {
               printf( "   Overflow! Greater than intmax_c.\n" );
               ivalue = intmax_c();
            }
            else if ( number[i] < (SpiceDouble)( intmin_c() ) )
            {
               printf( "   Overflow! Smaller than intmin_c.\n" );
               ivalue = intmin_c();
            }
            else
            {
               ivalue = (SpiceInt)( number[i] );
            }

            printf( "   Integer part        : %d\n", (int)ivalue );
            printf( "\n" );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Double precision number: 2.000000e+40
         Overflow! Greater than intmax_c.
         Integer part        : 2147483647

      Double precision number: -1.500000e+35
         Overflow! Smaller than intmin_c.
         Integer part        : -2147483648

      Double precision number: 1.600000e+00
         Integer part        : 1


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.1.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example from existing code fragment.

   -CSPICE Version 1.1.0, 29-JAN-1999 (NJB)

       Header has been updated to describe in more detail the
       choice of return value, and the dependency of the value on the
       host environment.

   -CSPICE Version 1.0.0, 16-OCT-1998 (NJB)

-Index_Entries

   largest integer number

-&
*/

{ /* Begin intmax_c */


   /*
   Static variables
   */

   static SpiceBoolean            first = SPICETRUE;
   static SpiceInt                value;



   if ( first )
   {
      value = intmax_();
      first = SPICEFALSE;
   }

   return ( value );


} /* End intmax_c */
