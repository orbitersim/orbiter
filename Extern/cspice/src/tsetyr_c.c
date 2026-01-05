/*

-Procedure tsetyr_c ( Time --- set year expansion boundaries )

-Abstract

   Set the lower bound on the 100 year range

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

   TIME

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void tsetyr_c ( SpiceInt            year )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   year       I   Lower bound on the 100 year interval of expansion

-Detailed_Input

   year        is the year associated with the lower bound on all year
               expansions computed by the SPICELIB routine TEXPYR. For
               example if `year' is 1980, then the range of years that can
               be abbreviated is from 1980 to 2079.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If `year' is less than 1, no action is taken.

-Files

   None.

-Particulars

   This routine allows you to set the range to which years
   abbreviated to the last two digits will be expanded. The input
   supplied to this routine represents the lower bound of the
   expansion interval. The upper bound of the expansion interval
   is year + 99.

   The default expansion interval is from 1969 to 2068.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose that you need to manipulate time strings and that
      you want to treat years components in the range from 0 to 99
      as being abbreviations for years in the range from
      1980 to 2079 (provided that the years are not modified by
      an ERA substring). The example code below shows how you
      could go about this.

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program tsetyr_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define DATELN       12
         #define NTSTRS       7

         /.
         Local variables.
         ./
         SpiceChar            timstr [DATELN];

         SpiceDouble          et;

         SpiceInt             i;

         /.
         Assign an array of calendar dates.
         ./
         SpiceChar            date   [NTSTRS][DATELN] = {
                                   "00 JAN 21", "01 FEB 22", "48 MAR 23",
                                   "49 APR 24", "79 JUL 14", "80 FEB 02",
                                   "99 DEC 31" };

         /.
         Load the required LSK.
         ./
         furnsh_c ( "naif0012.tls" );

         /.
         Set up the lower bound for the
         expansion of abbreviated years.
         ./
         tsetyr_c ( 1980 );

         /.
         Expand the years in input time strings.
         ./
         printf( "Time string    Expansion\n" );
         printf( "-----------    -----------\n" );

         for ( i = 0; i < NTSTRS; i++ )
         {
            str2et_c ( date[i], &et );
            timout_c ( et, "YYYY MON DD", DATELN, timstr );

            printf( "%s      %s\n", date[i], timstr );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Time string    Expansion
      -----------    -----------
      00 JAN 21      2000 JAN 21
      01 FEB 22      2001 FEB 22
      48 MAR 23      2048 MAR 23
      49 APR 24      2049 APR 24
      79 JUL 14      2079 JUL 14
      80 FEB 02      1980 FEB 02
      99 DEC 31      1999 DEC 31


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 05-JUL-2021 (JDR)

       Fixed bug: Added check in underlying code for "year" to be positive in
       order to update the lower bound for the expansion.

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Removed information related to SPICELIB routine TEXPYR behavior
       from -Particulars.

   -CSPICE Version 1.0.0, 11-FEB-1998 (EDW) (WLT)

-Index_Entries

   Set the interval of expansion for abbreviated years

-&
*/

{ /* Begin tsetyr_c */

   /*
   Error free: no error tracing required.
   */

   /*
   Call the f2c'd Fortran routine.
   */
   tsetyr_ (  ( integer    * ) &year  );

} /* End tsetyr_c */
