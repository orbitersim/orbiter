/*

-Procedure maxi_c ( Maximum of a set of integers )

-Abstract

   Find the maximum of a set of integers.

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

   PLANES

-Keywords

   GEOMETRY
   MATH
   PLANE

*/
   #include <stdarg.h>
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"


   SpiceInt maxi_c ( SpiceInt n,  ... )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   n          I   The number of integer values to compare.
   ...        I   The numbers to be compared, separated by commas.

-Detailed_Input

   n           is the number of integer values in the set whose maximum is to
               be determined.

   ...         represents a variable argument list. The number of integer
               values supplied must be that indicated by `n'. The values
               are separated by commas.

               Section 5.2.4.1 of the ANSI C Standard, titled "Translation
               Limits," specifies that argument lists containing at least 31
               items must be supported. In the interest of portability, no
               more than 30 integer values should be supplied.

-Detailed_Output

   The function returns the maximum of the set of input integers.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If `n' is less than 1, the value 0 is returned.

   2)  If the number of integer values supplied does not match the
       argument `n', the action of this routine is not defined.

   3)  If the number of integer values supplied exceeds 30,
       the action of this routine is not defined.

-Files

   None.

-Particulars

   None.

-Examples

   1) Find the maximum of four integer values.

      #include "SpiceUsr.h"
           .
           .
           .

      SpiceInt                max;
      SpiceInt                a;
      SpiceInt                b;
      SpiceInt                c;
      SpiceInt                d;
           .
           .
           .

      max = maxi_c ( 4, a, b, c, d );

-Restrictions

   1)  The ANSI C Standard specifies that argument lists containing 31
       actual arguments must be supported. Larger sets of values may
       not be handled properly by this routine.

-Literature_References

   [1]  "American National Standard for Programming Languages -- C,"
        Section 5.4.2.1, "Translation Limits," p.13, American National
        Standards Institute, 1990.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.1, 11-NOV-2006 (EDW)

       Added "None." text to -Particulars section, required for
       API doc script (cspicehtml.pl) integrity checks.

   -CSPICE Version 1.0.0, 29-MAR-1999 (NJB)

-Index_Entries

   maximum of integer values

-&
*/

{ /* Begin maxi_c */

   /*
   Local variables
   */

   SpiceInt                next;
   SpiceInt                retval;

   SpiceInt                i;


   /*
   ap is the argument pointer.  Its type va_list is declared in the
   header stdarg.h.
   */

   va_list                 ap;



   /*
   If there are no values to compare, return zero.
   */

   if ( n < 1 )
   {
      return ( 0 );
   }

   /*
   Initialize the argument pointer with the last named argument, namely
   n.
   */

   va_start ( ap, n );


   /*
   Initialize the maximum with the first value.
   */

   retval = va_arg ( ap, int );


   /*
   Now compute a running maximum of the values, if there are more.

   By the way, we capture the argument in the variable next rather than
   make the va_arg call as a MaxVal argument, because the MaxVal macro
   would make the va_arg call twice.
   */

   for ( i = 1;  i < n;  i++ )
   {
      next   =  va_arg ( ap,     int  );
      retval =  MaxVal ( retval, next );
   }


   /*
   Terminate the argument fetching process.
   */

   va_end ( ap );


   /*
   Return the value we've found.
   */

   return ( retval );


} /* End maxi_c */
