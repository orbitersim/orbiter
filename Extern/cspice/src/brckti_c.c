/*

-Procedure brckti_c ( Bracket an integer value within an interval )

-Abstract

   Bracket an integer number. That is, given a number and an
   acceptable interval, make sure that the number is contained in the
   interval. (If the number is already in the interval, leave it
   alone. If not, set it to the nearest endpoint of the interval.)

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

   INTERVALS
   NUMBERS
   UTILITY

*/

   #include "SpiceUsr.h"


   SpiceInt brckti_c ( SpiceInt  number,
                       SpiceInt  end1,
                       SpiceInt  end2   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   number     I   Number to be bracketed.
   end1       I   One of the bracketing endpoints for `number'.
   end2       I   The other bracketing endpoint for `number'.

   The function returns the bracketed number.

-Detailed_Input

   number      is the number to be bracketed. That is, the
               value of `number' is constrained to lie in the
               interval bounded by `end1' and `end2'.

   end1,
   end2        are the lower and upper bounds for `number'. The
               order is not important.

-Detailed_Output

   The function returns the bracketed number. That is `number', if it
   was already in the interval provided. Otherwise the returned
   value is the nearest bound of the interval.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This routine provides a shorthand notation for code fragments
   like the following:

      if ( end1 < end2 )
      {
         if ( number < end1 )
         {
            number = end1;
         }
         else if ( number > end2 )
         {
            number = end2;
         }
      }
      else
      {
         if ( number < end2 )
         {
            number = end2;
         }
         else if ( number > end1 )
         {
            number = end1;
         }
      }

   which occur frequently during the processing of program inputs.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following code example illustrates the operation of
      brckti_c.

      Example code begins here.


      /.
         Program brckti_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define LISTSZ       4

         /.
         Local variables.
         ./
         SpiceInt             i;

         /.
         Set the values for the example.
         ./
         SpiceInt             end1   [LISTSZ] = {  1,  1,  10, -10 };
         SpiceInt             end2   [LISTSZ] = { 10, 10, -10,  -1 };
         SpiceInt             number [LISTSZ] = { -1, 29,   3,   3 };

         printf( "Number  End1  End2  Bracketed\n" );
         printf( "------  ----  ----  ---------\n" );

         for ( i = 0; i < LISTSZ; i++ )
         {

            printf( "%6d %5d %5d %10d\n",
                    number[i], end1[i], end2[i],
                    brckti_c ( number[i], end1[i], end2[i] ) );

         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Number  End1  End2  Bracketed
      ------  ----  ----  ---------
          -1     1    10          1
          29     1    10         10
           3    10   -10          3
           3   -10    -1         -1


   2) The following code example illustrates a typical use for
      brckti_c: force an identifier to be within a range. Note that
      this code assumes that the user provided value is a valid
      integer number.


      Example code begins here.


      /.
         Program brckti_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define KWDSZ        31

         /.
         Local variables.
         ./
         SpiceChar            usrin  [KWDSZ];

         SpiceInt             codein;
         SpiceInt             codeok;

         /.
         Prompt the user for the code identifier.
         ./
         prompt_c ( "Enter object code: ", KWDSZ, usrin );

         /.
         Convert the user input to integer.
         ./
         prsint_c ( usrin, &codein );

         /.
         Object code must be in the range 701-705.
         ./
         codeok = brckti_c ( codein, 701, 705 );

         /.
         Display confirmation message.
         ./
         if ( codein != codeok )
         {
            printf( "Provided object code %3d is out of range (701-705).\n",
                                                               (int)codein );
         }
         else
         {
            printf( "Provided object code %3d is in range (701-705).\n",
                                                           (int)codein );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using "710" as user provided input, the output was:


      Enter object code: 710
      Provided object code 710 is out of range (701-705).


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 10-AUG-2021 (JDR) (BVS)

       BUG FIX: corrected to make the order of endpoints not important
       as stated in the description.

       Edited the header to comply with NAIF standard. Added complete
       code examples based on existing fragments.

       Updated code fragment in -Particulars to show that the
       order of endpoints is not important.

   -CSPICE Version 1.0.1, 11-NOV-2006 (EDW)

      Added "None." text to -Keywords section, required for
      API doc script (cspicehtml.pl) integrity checks.

   -CSPICE Version 1.0.0, 16-AUG-1999 (NJB) (WLT) (IMU)

-Index_Entries

   bracket an integer value within an interval

-&
*/

{ /* Begin brckti_c */

   if ( end1 < end2 )
   {
      if ( number < end1 )
      {
         return ( end1 );
      }
      else if ( number > end2 )
      {
         return ( end2 );
      }
   }
   else
   {
      if ( number < end2 )
      {
         return ( end2 );
      }
      else if ( number > end1 )
      {
         return ( end1 );
      }
   }

   return ( number );

} /* End brckti_c */
