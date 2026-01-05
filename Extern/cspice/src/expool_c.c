/*

-Procedure expool_c ( Confirm the existence of a pooled kernel variable )

-Abstract

   Confirm the existence of a numeric kernel variable in the kernel
   pool.

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

   void expool_c ( ConstSpiceChar  * name,
                   SpiceBoolean    * found )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   name       I   Name of a numeric kernel variable.
   found      O   SPICETRUE when the variable is in the pool.

-Detailed_Input

   name        is the name of the numeric kernel variable whose
               existence in the kernel pool is to be checked.

-Detailed_Output

   found       is SPICETRUE whenever the specified variable is included
               in the pool.

-Parameters

   None.

-Exceptions

   1)  If the `name' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   2)  If the `name' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This routine determines whether or not a numeric kernel pool
   variable exists. It does not detect the existence of
   string valued kernel pool variables.

   A better routine for determining the existence of numeric kernel
   pool variables is the routine dtpool_c which determines the
   existence, size and type of kernel pool variables.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following code example demonstrates how to use expool_c
      to confirm the existence of numeric kernel pool variables.
      In the example, we will look for different variables;
      some of them numeric, some string valued and some not
      present in the kernel pool.

      Use the kernel shown below; an IK defining two keywords
      used to provide data for an instrument with NAIF ID -999001.


         KPL/IK

         File name: expool_ex1.ti

         The keyword below define the three frequencies used by a
         hypothetical instrument (NAIF ID -999001). They correspond
         to three filters: red, green and blue. Frequencies are
         given in micrometers.

         \begindata

            INS-999001_FREQ_RGB   = (  0.65,  0.55, 0.475 )
            INS-999001_FREQ_UNITS = ( 'MICROMETERS'       )

         \begintext


         End of IK


      Example code begins here.


      /.
         Program expool_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define IKNAME       "expool_ex1.ti"
         #define KPVNLN       33
         #define NKPVNM       3

         /.
         Local variables.
         ./
         SpiceInt             i;

         SpiceBoolean         found;

         /.
         Define the variable names
         ./
         SpiceChar            keywrd [NKPVNM][KPVNLN] = {
                                                    "INS-999001_FREQ_RGB",
                                                    "NOT_IN_THE_POOL",
                                                    "INS-999001_FREQ_UNITS" };

         /.
         Load the instrument kernel.
         ./
         furnsh_c ( IKNAME );

         for ( i = 0; i < NKPVNM; i++ )
         {

            /.
            Check if the variable is numeric and present
            in the kernel pool.
            ./
            expool_c ( keywrd[i], &found );

            printf( "Variable name: %s\n", keywrd[i] );

            if ( found )
            {
               printf( "   It is numeric and exists in the kernel pool.\n" );
            }
            else
            {
               printf( "   Either it is not numeric or it is not in the "
                       "kernel pool.\n" );
            }
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Variable name: INS-999001_FREQ_RGB
         It is numeric and exists in the kernel pool.
      Variable name: NOT_IN_THE_POOL
         Either it is not numeric or it is not in the kernel pool.
      Variable name: INS-999001_FREQ_UNITS
         Either it is not numeric or it is not in the kernel pool.


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.1, 04-AUG-2021 (JDR)

       Edited header to comply with NAIF standard. Added complete code
       example.

   -CSPICE Version 1.2.0, 22-JUN-1999 (EDW)

       Added local variable to return boolean/logical values. This
       fix allows the routine to function if int and long are different
       sizes.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       Re-implemented routine without dynamically allocated, temporary
       strings.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   CONFIRM the existence of a pooled numeric kernel variable

-&
*/

{ /* Begin expool_c */

   /*
   Local variables.
   */
   logical         yes;


   /*
   Participate in error tracing.
   */
   chkin_c ( "expool_c" );


   /*
   Check the input string name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "expool_c", name );


   /*
   Call the f2c'd routine.
   */
   expool_( ( char    * ) name,
            ( logical * ) &yes,
            ( ftnlen    ) strlen(name) );


   /* Cast back to a SpiceBoolean. */
   *found = yes;


   /* Done.  Checkout. */
   chkout_c ( "expool_c" );


} /* End expool_c */
